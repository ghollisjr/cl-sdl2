(in-package :cl-sdl2)

;;;; This is an exploration of the abstract controller idea as an
;;;; improved interface to SDL event polling.  An abstract controller
;;;; supplies a set of values for specified control aspects.  Each
;;;; aspect has a keyword naming the aspect, and each aspect can have
;;;; input defined from many SDL sources, including any combination of
;;;; keyboard, mouse, and joystick input.
;;;;
;;;; Arbitrary user code can be added to the polling loop through a
;;;; body form that is evaluated in the context of an available SDL
;;;; event object, so that the body is evaluated once every time an
;;;; SDL event is detected.  I am thinking to have all controller code
;;;; execute first, followed by the body code.  This would make it
;;;; convenient to add a few custom user checks at the end of the poll
;;;; loop to partition input into different categories, such as
;;;; detecting inputs that would require recomputing something
;;;; expensive.

(defclass sdl-controller ()
  ;; hash-table from name to current value
  ((names
    :documentation "Symbols denoting aspects."
    :accessor sdl-controller-names
    :initform NIL
    :initarg :names)
   (aspects
    :documentation "Map from name to aspect value."
    :accessor sdl-controller-aspects
    :initform (make-hash-table :test 'eq)
    :initarg :aspects)
   (inputs
    :documentation "Map from input designator to input value."
    :accessor sdl-controller-inputs
    :initform (make-hash-table :test 'equal)
    :initarg :aspects)
   (handler
    :documentation "Event handler for SDL events.  Accepts SDL event
   and a hash table denoting relevant raw inputs."
    :accessor sdl-controller-handler
    :initform (make-hash-table :test 'eq)
    :initarg :handler)
   (joysticks
    :documentation "List of joystick IDs used by this controller.
    Useful for opening and closing SDL joysticks."
    :accessor sdl-controller-joysticks
    :initform NIL
    :initarg :joysticks)
   (joystick-handles
    :documentation "List of SDL_Joystick pointers used by this
    controller.  Should be managed by sdl-controller-init and
    sdl-controller-close."
    :accessor sdl-controller-joystick-handles
    :initform NIL
    :initarg :joystick-handles)
   (axis-min
    :documentation "Minimum value for any SDL joystick axis"
    :accessor sdl-controller-axis-min
    :initform -32767
    :initarg :axis-min)
   (axis-max
    :documentation "Maximum value for any SDL joystick axis"
    :accessor sdl-controller-axis-max
    :initform 32767
    :initarg :axis-max)
   (initial-axis-value-map
    :documentation "Map from (joystick-id axis) to initial value for
    that axis (between -1 and 1 for reasonable behavior)."
    :accessor sdl-controller-initial-axis-value-map
    :initform (make-hash-table :test 'equal)
    :initarg :initial-axis-value-map)
   (deadzone-map
    :documentation "Map from (joystick-id axis) to deadzone for that axis"
    :accessor sdl-controller-deadzone-map
    :initform (make-hash-table :test 'equal)
    :initarg :deadzone-map)
   (response
    :documentation "function accepting SDL event and possibly modifying
   aspects supplied as second argument, returning T if aspects were
   modified in response to the event."
    :accessor sdl-controller-response
    :initform (make-hash-table :test 'eq)
    :initarg :response)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-keys (form)
    "Finds terms (key X) in the form and returns the list of symbols found."
    (let* ((ht (make-hash-table :test 'eq)))
      (labels ((rec (f)
                 (cond
                   ((NULL f)
                    nil)
                   ((atom f)
                    NIL)
                   ((consp f)
                    (if (and (eq (car f)
                                 'key)
                             (consp (cdr f))
                             (keywordp (cadr f)))
                        (setf (gethash (cadr f) ht)
                              T)
                        (progn
                          (rec (car f))
                          (rec (cdr f))))))))
        (rec form)
        (loop
           for k being the hash-keys in ht
           collecting k))))

  (defun find-quit (form)
    "Returns T if (quit) is found in the form."
    (let* ((result NIL))
      (labels ((rec (f)
                 (cond
                   ((NULL f)
                    nil)
                   ((atom f)
                    NIL)
                   ((consp f)
                    (if (and (eq (car f)
                                 'quit)
                             (null (cdr f)))
                        (setf result T)
                        (progn
                          (rec (car f))
                          (rec (cdr f))))))))
        (rec form)
        result)))

  (defun actual-listp (x)
    "Returns length of list if x is an actual list, i.e. not just a cons
pair.  Returns 0 for NIL.  Useful because #'cl:listp will return
non-NIL for e.g. (cons 1 2), while #'actual-listp does not."
    (labels ((rec (f &optional (length 0))
               (cond
                 ((null f)
                  length)
                 ((atom f)
                  NIL)
                 (T
                  (rec (cdr f) (1+ length))))))
      (rec x)))

  (defun find-joysticks (form)
    "Finds terms (joystick X Y Z) in the form and returns the list of symbols found."
    (let* ((ht (make-hash-table :test 'equal)))
      (labels ((rec (f)
                 (cond
                   ((NULL f)
                    nil)
                   ((atom f)
                    NIL)
                   ((consp f)
                    (let* ((len NIL))
                      (if (and (eq (car f)
                                   'joystick)
                               (setf len (actual-listp f))
                               (equal len 4))
                          (setf (gethash (rest f) ht)
                                T)
                          (progn
                            (rec (car f))
                            (rec (cdr f)))))))))
        (rec form)
        (loop
           for k being the hash-keys in ht
           collecting k))))

  (defun joystick-buttons (joysticks)
    "Takes input from find-joysticks and returns list of (X button Y)
present in joysticks."
    (remove-if-not (lambda (x)
                     (eq (second x)
                         'button))
                   joysticks))

  (defun joystick-axes (joysticks)
    "Takes input from find-joysticks and returns list of (X axis Y)
present in joysticks."
    (remove-if-not (lambda (x)
                     (eq (second x)
                         'axis))
                   joysticks))

  (defun joystick-ids (joysticks)
    "Returns list of numerical IDs used in joystick forms."
    (let* ((ht (make-hash-table :test 'eql)))
      (loop
         for js in joysticks
         for id = (first js)
         do (setf (gethash id ht)
                  T))
      (sort (loop
               for k being the hash-keys in ht
               collecting k)
            #'<))))

(defmacro sdl-controller-poll (controllers &optional event &body body)
  "Polls SDL events and hands event to each of the supplied
controllers as well as executing body once per event after the events
have been handled and the controller responses have been processed.
controllers is an evaluated argument.  event is a symbol used to
designate the SDL event handle, possibly useful when supplying non-NIL
body.

Note that body is executed once per event This provides the ability to
respond to each event in ways that aren't specified by controllers."
  (let* ((cs (gensym "CONTROLLERS"))
         (c (gensym "C"))
         (event (if event event (gensym "EVENT"))))
    `(with-foreign-object (,event '(:union sdl-event))
       (let* ((,cs ,controllers))
         ;; handlers
         (loop
            while (not (zerop (sdl-pollevent ,event)))
            do
              (loop
                 for ,c in ,cs
                 do
                   (funcall (sdl-controller-handler ,c) ,event))
              ,@body)
         ;; responses
         (loop
            for ,c in ,cs
            do (funcall (sdl-controller-response ,c)))))))

(defun norm-axis (val min max)
  (/ (float val)
     (max (abs min)
          (abs max))))

(defun undeadzone-axis (js-axis val deadzone-map)
  (let* ((v (gethash js-axis deadzone-map)))
    (if v
        (if (> (abs val) v)
            val
            0)
        val)))

(defun clip-axis (x min max)
  (max (min x max) min))

(define-condition sdl-controller-njoysticks-error (error)
  ((navailable :initarg :navailable :reader sdl-controller-njoysticks-navailable)))

(defun sdl-controller-init (controller)
  "Initializes controller.  At the moment, opens SDL joysticks and
sets initial axis values."
  (let* ((njs (sdl-numjoysticks))
         (joysticks (sdl-controller-joysticks controller))
         (n (reduce #'max joysticks))
         (axismin (sdl-controller-axis-min controller))
         (axismax (sdl-controller-axis-max controller))
         (initial-map (sdl-controller-initial-axis-value-map controller))
         (deadzone-map (sdl-controller-deadzone-map controller)))
    (when (>= n njs)
      (error 'sdl-controller-njoysticks-error
             :navailable njs))
    ;; "Too many joysticks requested by controller"))
    (setf (sdl-controller-joystick-handles controller)
          (loop
             for js in joysticks
             collecting (sdl-joystickopen js)))
    (loop
       for (js axis) being the hash-keys in initial-map
       for v being the hash-values in initial-map
       do (setf (gethash (list 'joystick js 'axis axis)
                         (sdl-controller-inputs controller))
                v))))

(defun sdl-controller-close (controller)
  (let* ((joystick-handles
          (sdl-controller-joystick-handles controller)))
    (loop
       for h in joystick-handles
       when (equal (foreign-enum-value 'sdl-bool :true)
                   (sdl-joystickgetattached h))
       do (sdl-joystickclose h))
    (setf (sdl-controller-joystick-handles controller) NIL)))

(defmacro make-sdl-controller ((&rest specs)
                               &key
                                 initial-axis-values
                                 deadzones
                                 (axis-min -32767) ; clipped minimum
                                 (axis-max 32767))
  "Returns a controller object which can be supplied to poll and
controller functions to parse user input in a useful way.

joysticks must be non-NIL if joysticks are used in the controller
specifications.  If non-NIL, joysticks must be a list of SDL_Joystick
CFFI foreign objects.  Each joystick can be referenced using (joystick
index ...) forms in the supplied specs, where index is the index into
the joysticks list referencing the specific joystick object being
used.

initial-axis-values can be a list of elements of the form (joystick-id
axis value) to designate non-zero default axis values.  This is a
workaround for SDL not being able to retrieve accurate initial axis
readings.  There was a stackoverflow reference to this problem here:

 https://stackoverflow.com/questions/63707867/with-sdl2-how-do-i-reliably-get-the-initial-positions-of-the-joystick-axes

deadzones can be a list of elements of the form (joystick-id axis
deadzone).  deadzones is an evaluated argument.

Each spec is a list of the form (:name &rest exprs).  Each expr
denotes a computation using various inputs.  Supported fundamental
forms are:

* (quit) for window close event from the window manager/X window
  system.  This needs to be part of the controller interface to enable
  graceful window close handling.  As an exception, the value
  of (quit) is T or NIL, not a numerical value as the other inputs
  are.

* (key sym) for keyboard input with SDL keyword sym for the key.  On
  key-down, the key takes value 1.0, and on key-up, the key takes
  value 0.0.

* (joystick X axis Y) for the Y axis of joystick X.  Axis is clipped
  into range (axis-min axis-max) and normalized to not exceed (-1.0
  1.0).

* (joystick X button Y) for the Y button of joystick X.  On
  joybuttondown, button has 1.0 value, and on joybuttonup, button has
  0.0 value.

* (mouse axis A) for the A axis of the mouse
* (mouse button B) for the B button of the mouse"
  (let* ((result (gensym "RESULT"))
         (names (gensym "NAMES"))
         (aspects (gensym "ASPECTS"))
         (inputs (gensym "INPUTS"))
         (handler (gensym "HANDLER"))
         (response (gensym "RESPONSE"))
         (event (gensym "EVENT"))
         (event-type (gensym "EVENT-TYPE"))
         (sym (gensym "SYM"))
         (js (gensym "JS"))
         (jb (gensym "JB"))
         (ja (gensym "JA"))
         (axismin (gensym "AXISMIN"))
         (axismax (gensym "AXISMAX"))
         (deadzone-map (gensym "DEADZONE-MAP"))
         (initial-map (gensym "INITIAL-MAP"))
         (undeadzone-axis (gensym "UNDEADZONE-AXIS"))
         (clipaxis (gensym "CLIPAXIS"))
         (normaxis (gensym "NORMAXIS"))
         (keys (gensym "KEYS"))
         (joystick-axes (gensym "JOYSTICK-AXES"))
         (joystick-buttons (gensym "JOYSTICK-BUTTONS")))
    `(symbol-macrolet ((,aspects (sdl-controller-aspects ,result))
                       (,inputs (sdl-controller-inputs ,result))
                       (,handler (sdl-controller-handler ,result))
                       (,response (sdl-controller-response ,result)))
       ;; Set joystick-ids
       (let* ((,names (list ,@(mapcar #'first specs)))
              (,result (make-instance 'sdl-controller
                                      :names ,names))
              (,keys (let* ((ht (make-hash-table :test 'eq)))
                       (loop
                          for k in (list ,@(find-keys specs))
                          do (setf (gethash k ht) T))
                       ht))
              (,joystick-axes (let* ((ht (make-hash-table :test 'equal)))
                                (loop
                                   for ja in
                                     (list ,@(map 'list
                                                  (lambda (x)
                                                    (list 'list
                                                          (first x)
                                                          `',(second x)
                                                          (third x)))
                                                  (joystick-axes
                                                   (find-joysticks specs))))
                                   do (setf (gethash (list (first ja)
                                                           (third ja))
                                                     ht)
                                            T))
                                ht))
              (,joystick-buttons (let* ((ht (make-hash-table :test 'equal)))
                                   (loop
                                      for jb in
                                        (list ,@(map 'list
                                                     (lambda (x)
                                                       (list 'list
                                                             (first x)
                                                             `',(second x)
                                                             (third x)))
                                                     (joystick-buttons
                                                      (find-joysticks specs))))
                                      do (setf (gethash (list (first jb)
                                                              (third jb))
                                                        ht)
                                               T))
                                   ht))
              (,axismax ,axis-max)
              (,axismin ,axis-min)
              (,initial-map (let* ((ivs ,initial-axis-values)
                                   (ht (make-hash-table :test 'equal)))
                              (loop
                                 for iv in ivs
                                 do (destructuring-bind (js axis val)
                                        iv
                                      (setf (gethash (list js axis) ht)
                                            val)))
                              ht))
              (,deadzone-map (let* ((dzs ,deadzones)
                                    (ht (make-hash-table :test 'equal)))
                               (loop
                                  for dz in dzs
                                  do (destructuring-bind (js axis val)
                                         dz
                                       (setf (gethash (list js axis) ht)
                                             val)))
                               ht)))
         (setf (sdl-controller-axis-min ,result)
               ,axismin)
         (setf (sdl-controller-axis-max ,result)
               ,axismax)
         (setf (sdl-controller-initial-axis-value-map ,result)
               ,initial-map)
         (setf (sdl-controller-deadzone-map ,result)
               ,deadzone-map)
         (setf (sdl-controller-joysticks ,result)
               (list ,@(joystick-ids (find-joysticks specs))))
         (setf
          ,handler
          (lambda (,event)
            (let* ((,event-type (foreign-enum-keyword
                                 'sdl-event-type
                                 (foreign-slot-value ,event '(:union sdl-event)
                                                     :type)))
                   )
              (labels ((,normaxis (val)
                         (cl-sdl2::norm-axis val ,axismin ,axismax))
                       (,undeadzone-axis (js-axis val)
                         (cl-sdl2::undeadzone-axis js-axis val ,deadzone-map))
                       (,clipaxis (x)
                         (cl-sdl2::clip-axis x ,axismin ,axismax)))
                (cond
                  ((equal ,event-type :quit)
                   (setf (gethash :quit ,inputs)
                         T))
                  ((or (equal ,event-type
                              :keydown)
                       (equal ,event-type
                              :keyup))
                   (let* ((,sym (foreign-enum-keyword
                                 'sdl-keycode
                                 (foreign-slot-value
                                  (foreign-slot-pointer ,event
                                                        '(:struct sdl-keyboard-event)
                                                        :keysym)
                                  '(:struct sdl-keysym)
                                  :sym))))
                     (when (gethash ,sym ,keys)
                       (setf (gethash (list 'key ,sym)
                                      ,inputs)
                             (if (equal ,event-type :keydown)
                                 1.0
                                 0.0)))))
                  ((equal ,event-type :joyaxismotion)
                   (let* ((,js (foreign-slot-value ,event
                                                   '(:struct sdl-joy-axis-event)
                                                   :which))
                          (,ja
                           (foreign-slot-value ,event
                                               '(:struct sdl-joy-axis-event)
                                               :axis)))
                     (when (gethash (list ,js ,ja)
                                    ,joystick-axes)
                       (setf (gethash (list 'joystick ,js 'axis ,ja)
                                      ,inputs)
                             (,normaxis
                              (,undeadzone-axis
                               (list ,js ,ja)
                               (,clipaxis
                                (foreign-slot-value ,event
                                                    '(:struct sdl-joy-axis-event)
                                                    :value)
                                ;; (sdl-joystickgetaxis ,js ,ja)
                                )))))))
                  ((or (equal ,event-type :joybuttondown)
                       (equal ,event-type :joybuttonup))
                   (let* ((,js (foreign-slot-value ,event
                                                   '(:struct sdl-joy-axis-event)
                                                   :which))
                          (,jb
                           (foreign-slot-value ,event
                                               '(:struct sdl-joy-button-event)
                                               :button)))
                     (when (gethash (list ,js ,jb)
                                    ,joystick-buttons)
                       (setf (gethash (list 'joystick ,js 'button ,jb)
                                      ,inputs)
                             (if (equal ,event-type :joybuttondown)
                                 1.0
                                 0.0))))))))))
         (setf
          ,response
          (lambda ()
            (macrolet ((quit ()
                         `(gethash :quit ,',inputs))
                       (key (sym)
                         `(or (gethash (list 'key ,sym)
                                       ,',inputs)
                              0.0))
                       (joystick (id op val)
                         `(or (gethash (list 'joystick ,id ',op ,val)
                                       ,',inputs)
                              0.0)))
              ,@(loop
                   for spec in specs
                   collecting
                     `(setf (gethash ,(first spec) ,aspects)
                            ,(second spec))))))
         ,result))))

(defun make-sdl-controller-fn (specs
                               &key
                                 initial-axis-values
                                 deadzones
                                 (axis-min -32767) ; clipped minimum
                                 (axis-max 32767))
  "Function version of make-sdl-controller."
  (eval `(make-sdl-controller ,specs
                              :initial-axis-values ,initial-axis-values
                              :deadzones ,deadzones
                              :axis-min ,axis-min
                              :axis-max ,axis-max)))

(defmacro with-sdl-controller-aspects
    (controller (&rest aspect-bindings) &body body)
  "Binds symbols to aspect values for given controller for body.

Each aspect-binding is of the form (symbol aspect-name)."
  (let* ((control (gensym "CONTROLLER")))
    `(let ((,control ,controller))
       (symbol-macrolet
           ,(loop
               for (symbol aspect-name) in aspect-bindings
               collecting `(,symbol
                            (gethash ,aspect-name
                                     (sdl-controller-aspects ,control))))
         ,@body))))

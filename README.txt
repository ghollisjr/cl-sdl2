cl-sdl2 is a combination of an SDL2 CFFI and a few extra systems
defined on top of SDL2 to exploit Lisp's capabilities.  There is
already a suite of SDL2 libraries available via quicklisp, but the
design of this system is different in that it

1. Exposes a more or less 1-to-1 CFFI to the SDL2 API.
2. Builds Lispy utilities in layers on the basic CFFI.

The goal of this design is to maintain direct compatibility with the
SDL2 API so that e.g. features that aren't yet supported in a Lispy
way can still be accessed and can be easily found by examining widely
available SDL2 documentation.

At the moment, these are the extra systems added to the CFFI:

----------------------------------------------------------------------
sdl-controller

An sdl-controller object is best created by the make-sdl-controller
macro.  This macro provides a simple DSL for creating abstract
controller representations that can handle SDL events and present a
simple interface to the programmer.  I.e. rather than directly
handling key press and joystick events, you define an abstract
controller that reacts to those events and presents an interface more
suitable to the needs of your application.

For example, if you imagine a simple 2-D video game where the
character can move left, move right, and then jump, then you might
define a controller as follows:

(defparameter *control*
  (make-sdl-controller
   ((:quit (or (quit)
               (not (zerop
                     (max (key :escape)
                          (key :q))))))
    (:move (- (key :right) (key :left)))
    (:jump (key :space)))))

This control object denotes a controller where the left and right
arrow keys on the keyboard will move the player left or right, with
both being pressed meaning no movement, and where the space bar causes
the player to jump.  The first control element denotes the ability of
the player to exit the game by either closing the game window or
pressing the escape or "Q" key on the keyboard.

The special form (key ...) evaluates to floating point 0 or 1
depending on whether a key is up (not pressed) or down (pressed)
respectively.

The special form (quit) returns T when the SDL window quit event has
been observed, and NIL otherwise.

It would be easy to add joystick support to the above as follows:

(defparameter *STARTBUTTON* 5
  "start button for given controller")

(defparameter *ABUTTON* 0
  "A button for given controller")

(defparameter *LEFTXAXIS* 0
  "left x axis for given controller")

(defparameter *control*
  (make-sdl-controller
   ((:quit (or (quit)
               (not (zerop
                     (max (key :escape)
                          (key :q)
                          (joystick 0 button *STARTBUTTON*))))))
    (:move (+ (- (key :right) (key :left))
              (joystick 0 axis *LEFTXAXIS*)))
    (:jump (+ (key :space)
              (joystick 0 button *ABUTTON*))))))

This would allow both joystick 0 and keyboard to simultaneously
control movement and quit events.  The axis and button symbols are not
evaluated and are used to determine the kind of joystick input the
joystick special form denotes.  For buttons, the allowed values are
floating point 0 and 1 as with keyboard input.  For axes, the allowed
values are between floating point -1 and 1, with options to control
deadzones and axis extremes as keyword arguments to
make-sdl-controller.  For example, a deadzone of 3500 could be added
to the x-axis of the controller as follows:

(defparameter *control*
  (make-sdl-controller
   ((:quit (or (quit)
               (not (zerop
                     (max (key :escape)
                          (key :q)
                          (joystick 0 button *STARTBUTTON*))))))
    (:move (+ (- (key :right) (key :left))
              (joystick 0 axis *LEFTXAXIS*)))
    (:jump (+ (key :space)
              (joystick 0 button *ABUTTON*))))
   :deadzones (list (list 0 0 3500))))

:deadzones accepts a list of (joystick-id axis-index deadzone)
elements, where each deadzone is an integer as SDL2 treats joystick
axes as integers.  This is convenient for controlling deadzones, but
not very convenient for most applications, which is why the (joystick
...) form evaluates to floating point numbers between -1 and 1 rather
than integers between -32768 and 32767 as returned by SDL2.

To make use of this controller object, you could do the following:

(let* ((window (init-sdl 400 400)))
  (sdl-controller-init *control*) ; call after init-sdl but before event loop
  (with-sdl-controller-aspects *control*
      ((quit :quit)
       (move :move)
       (jump :jump))
    (loop
       while (not quit)
       do
         (sdl-controller-poll (list *control*)) 
         (move-player move jump)))) ; assuming this is defined somewhere

where move-player is assumed to be an interesting function to control
game mechanics.

sdl-controller-poll processes SDL2 events using a list of supplied
control objects and optionally accepting a body to execute for each
SDL event.  This allows directly reacting to events that might not be
handled by the supplied controllers.  The first argument is the list
of controllers, while the second can be the name of a symbol denoting
the foreign sdl-event object in the context of a supplied body.  For
example,

(sdl-controller-poll (list *control*)
    sdlevent
  (let* ((type (foreign-enum-keyword
                'sdl-event-type
                (foreign-slot-value sdlevent '(:union sdl-event)
                                    :type))))
    (print type)))

would print the type keyword of each event handled by the
sdl-controller-poll loop.

----------------------------------------------------------------------
make-sdl-timer

This function returns a timer object that returns the floating point
amount of seconds that have passed between calls to the timer
function.  For example,

(let* ((timer (make-sdl-timer))
       (x (+ 1 1)))
  (funcall timer))

would return the amount of time it took to evaluate (+ 1 1).

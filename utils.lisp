;; File of useful C -> Lisp code conversion utilities
(require 'cl-ana)
(in-package :cl-ana)

(defun typeconv (type)
  (cond
    ((eq type 'void) :void)
    ((eq type 'int) :int)
    ((eq type 'long) :long)
    ((eq type 'float) :float)
    ((eq type 'double) :double)
    ((eq type 'glenum) 'gl-enum)
    ((eq type 'glvoid) 'gl-void)
    ((eq type 'glboolean) 'gl-bool)
    ((eq type 'glsizei) 'gl-sizei)
    ((eq type 'glclampf) 'gl-clampf)
    ((eq type 'glclampd) 'gl-clampd)
    ((eq type 'glbyte) 'gl-byte)
    ((eq type 'glubyte) 'gl-ubyte)
    ((eq type 'glchar) 'gl-char)
    ((eq type 'glshort) 'gl-short)
    ((eq type 'glushort) 'gl-ushort)
    ((eq type 'glint) 'gl-int)
    ((eq type 'gluint) 'gl-uint)
    ((eq type 'gllong) 'gl-long)
    ((eq type 'glulong) 'gl-ulong)
    ((eq type 'glfloat) 'gl-float)
    ((eq type 'gldouble) 'gl-double)
    ((eq type 'glsizeiptr) 'gl-sizei-ptr)
    ((eq type 'glintptr) 'gl-int-ptr)
    ((eq type 'glsync) :pointer)
    ((eq type 'glbitfield) 'gl-bitfield)
    ((eq type 'glint64) 'gl-int64)
    ((eq type 'gluint64) 'gl-uint64)
    ((eq type 'gldebugproc) :pointer)
    (t type)))

(defun vartrans (var)
  (cond
    ((eq var 'T)
     'TT)
    (T var)))

(defun nesttypes (types)
  (let* ((result NIL))
    (loop
       for r in types
       do (setf result
                (if (null result)
                    (typeconv (vartrans r))
                    (list (typeconv (vartrans r)) result))))
    result))

(defun argtrans (arg)
  `(,(first (last arg))
     ,(nesttypes (butlast arg))))

(defun functrans (func)
  (destructuring-bind (defop rettype name &rest args)
      func
    `(,defop ,name ,(typeconv rettype)
       ,@(mapcar #'argtrans args))))

(defun constantify (sym)
  (let* ((str (string sym))
         (sym
          (intern
           (concatenate 'string
                        "+"
                        (map 'string (lambda (c)
                                       (if (char= c #\_)
                                           #\-
                                           c))
                             str)
                        "+"))))
    `(constant (,sym ,(format nil "~a" str)))))

(defun find-constants (code)
  (let* ((result NIL))
    (labels ((rec (c)
               (cond
                 ((null c) NIL)
                 ((atom c) NIL)
                 (T
                  (destructuring-bind (op &rest rest) c
                    (cond
                      ((equal op 'constant)
                       (push (first (first rest))
                             result))
                      (T
                       (mapcar #'rec c))))))))
      (rec code)
      (reverse result))))

(defun find-defctypes (code)
  (let* ((result NIL))
    (labels ((rec (c)
               (cond
                 ((null c) NIL)
                 ((atom c) NIL)
                 (T
                  (destructuring-bind (op &rest rest) c
                    (cond
                      ((equal op 'defctype)
                       (push (first rest)
                             result))
                      (T
                       (mapcar #'rec c))))))))
      (rec code)
      (reverse result))))

(defun find-grovel-types (code)
  (let* ((result NIL))
    (labels ((rec (c)
               (cond
                 ((null c) NIL)
                 ((atom c) NIL)
                 (T
                  (destructuring-bind (op &rest rest) c
                    (cond
                      ((or (equal op 'ctype))
                       (push (first rest)
                             result))
                      (T
                       (mapcar #'rec c))))))))
      (rec code)
      (reverse result))))

(defun find-defcfun (code)
  (let* ((result NIL))
    (labels ((rec (c)
               (cond
                 ((null c) NIL)
                 ((atom c) NIL)
                 (T
                  (destructuring-bind (op &rest rest) c
                    (cond
                      ((or (equal op 'defcfun))
                       (push (intern (string-upcase (first rest)))
                             result))
                      (T
                       (mapcar #'rec c))))))))
      (rec code)
      (reverse result))))

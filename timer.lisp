(in-package :cl-sdl2)

(defun make-sdl-timer ()
  (let* ((lasttime (sdl-getticks)))
    (lambda ()
      (let* ((currenttime (sdl-getticks))
             (dt (* 1f-3 (- currenttime lasttime))))
        (setf lasttime currenttime)
        dt))))

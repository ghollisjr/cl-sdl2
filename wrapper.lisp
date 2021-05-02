;; (pkg-config-cflags "sdl2" :optional t)
;; (flags "-I/usr/include/SDL2" "-D_REENTRANT")
(include "SDL2/SDL.h")

(in-package :cl-sdl2)

;; SDL_rect.h

;; Can't figure out how to get the CFFI wrapper system to allow
;; enumeration types, so these return ints

(defwrapper "SDL_PointInRect" :int
  (point :pointer)
  (rect :pointer))

(defwrapper "SDL_RectEmpty" :int
  (rect :pointer))

(defwrapper "SDL_RectEquals" :int
  (a :pointer)
  (b :pointer))

;; SDL_surface.h
;; Tried getting this to work but fails for some reason
;; (defwrapper "SDL_MUSTLOCK" :int
;;   (surface :pointer))

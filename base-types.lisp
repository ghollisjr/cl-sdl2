(in-package :cl-sdl2-cffi)

(define-foreign-library cl-sdl2
  (:unix "libSDL2.so")
  (t (:default "libSDL2")))

(use-foreign-library cl-sdl2)

(defctype Uint64 :ulong)
(defctype Uint32 :uint)
(defctype Uint16 :ushort)
(defctype Uint8 :uchar)
(defctype Sint64 :long)
(defctype Sint32 :int)
(defctype Sint16 :short)
(defctype Sint8 :char)

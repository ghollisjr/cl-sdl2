(asdf:defsystem cl-sdl2
  :serial t
  :description "Simple CFFI for SDL2"
  :license ""
  :author "Gary Hollis"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :components
  ((:file "package")
   (:file "base-types")
   (:cffi-grovel-file "grovel")
   (:cffi-wrapper-file "wrapper")
   (:file "sdl2-cffi")))

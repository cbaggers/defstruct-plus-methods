;;;; defstruct-plus-methods.asd

(asdf:defsystem #:defstruct-plus-methods
  :description "A macro that lets you define a struct with generic function accessors like a class"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause (see the license file)"
  :serial t
  :components ((:file "package")
               (:file "defstruct-plus-methods")))

;;;; cl-xkb.asd

(asdf:defsystem #:cl-xkb
  :description "Common Lisp wrapper for libxkb"
  :author "Malcolm Still"
  :license "BSD3"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-xkb")))


;;;; cl-xkb.asd

(asdf:defsystem #:cl-xkb
  :description "Common Lisp wrapper for libxkb"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-xkb")))


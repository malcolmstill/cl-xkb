;;;; cl-xkb.asd

(asdf:defsystem #:cl-xkb
  :description "Common Lisp wrapper for libxkb"
  :author "Malcolm Still"
  :license "BSD 3-Clause"

  :homepage "https://github.com/malcolmstill/cl-xkb"
  :source-control (:git "https://github.com/malcolmstill/cl-xkb.git")
  :bug-tracker "https://github.com/malcolmstill/cl-xkb/issues"

  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-xkb")))

;;;; cl-xkb.asd

(asdf:defsystem #:cl-xkb
  :description "Describe cl-xkb here"
  :author "Malcolm Still"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-xkb")))


(in-package :common-lisp-user)
(asdf:defsystem #:euclidean
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Your name"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components ((:file "euclidean")))

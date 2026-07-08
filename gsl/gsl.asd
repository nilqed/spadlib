(in-package :common-lisp-user)
(asdf:defsystem #:gsl
  :serial t
  :description "GNU Scientific Library in FriCAS"
  :version "1.0.0"
  :author "Your name"
  :license "MIT"
  :depends-on ("gsll")
  :pathname "src/"
  :components ((:file "gsl")))

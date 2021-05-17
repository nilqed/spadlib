(in-package :common-lisp-user)
(asdf:defsystem #:maxima
  :serial t
  :description "Short description"
  :version "5.44.0"
  :author "Your name"
  :license "GPL"
  :depends-on ()
  :pathname "src/"
  :components ((:file "maxima")))

(in-package :common-lisp-user)
(asdf:defsystem #:fmtmjax
  :serial t
  :description "Short description"
  :version "1.0.1"
  :author "Ralf Hemmecke"
  :license "TBD"
  :depends-on ()
  :pathname "src/"
  :components ((:file "fmtmjax")))

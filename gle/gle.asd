(in-package :common-lisp-user)
(asdf:defsystem #:gle
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ("pltspt")
  :pathname "src/"
  :components ((:file "gle")))

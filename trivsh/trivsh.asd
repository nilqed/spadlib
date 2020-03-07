(in-package :common-lisp-user)
(asdf:defsystem #:trivsh
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ("trivial-shell")
  :pathname "src/"
  :components ((:file "trivsh")))

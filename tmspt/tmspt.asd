(in-package :common-lisp-user)
(asdf:defsystem #:tmspt
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components ((:file "tmspt")))

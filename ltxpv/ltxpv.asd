(in-package :common-lisp-user)
(asdf:defsystem #:ltxpv
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ("pipe" "tex")
  :pathname "src/"
  :components ((:file "ltxpv")))

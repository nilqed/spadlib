(in-package :common-lisp-user)
(asdf:defsystem #:ltxspt
  :serial t
  :description "LaTeXFormat support"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components ((:file "ltxspt")))

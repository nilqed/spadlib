(in-package :common-lisp-user)

(asdf:defsystem #:atp
  :serial t
  :description "FriCAS Automated Theorem Prover - based on SNARK"
  :version "1.0.1"
  :author "Kurt Pagani, <nilqed@gmail.com>"
  :license "MPL 1.1, see file LICENSE"
  :depends-on (#:snark)
  :pathname "src/"
  :components ((:file "snark")))
  

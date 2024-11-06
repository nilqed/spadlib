(in-package :common-lisp-user)
(asdf:defsystem #:fsqlite
  :serial t
  :description "SQLite/CL-SQLITE"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components ((:file "fsqlite")))

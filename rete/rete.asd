(in-package :common-lisp-user)
(asdf:defsystem #:rete
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Your name"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components ((:file "rete")
               (:file "package")
               (:file "ops-globals")
               (:file "ops-util")
               (:file "ops-compile")
               (:file "ops-rhs")
               (:file "ops-match")
               (:file "ops-main")
               (:file "ops-backup")
               (:file "ops-init")
               (:file "ops-io")
               (:file "ops")))


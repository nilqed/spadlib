(in-package :common-lisp-user)
(asdf:defsystem #:reduce
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Your name"
  :license "MIT"
  :depends-on ()
  :pathname "src/"
  :components (
    ;(:file "bootstrap")
    ;(:file "sl-on-cl")
    (:file "reduce")))

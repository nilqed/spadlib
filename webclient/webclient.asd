(in-package :common-lisp-user)
(asdf:defsystem #:webclient
  :serial t
  :description "Short description"
  :version "1.0.0"
  :author "Kurt Pagani <nilqed@gmail.com>"
  :license "MIT"
  :depends-on (#:drakma)
  :pathname "src/"
  :components ((:file "webclient")))

; -*- Lisp -*-
(in-package :maxima)

(defparameter *autoconf-prefix* "C:/Users/kfp/quicklisp/local-projects/spadlib/maxima/src/maxima/.")
(defparameter *autoconf-exec_prefix* "@expanded_exec_prefix@")
(defparameter *autoconf-package* "maxima")
(defparameter *autoconf-version* "5.44.0")
(defparameter *autoconf-libdir* "@expanded_libdir@")
(defparameter *autoconf-libexecdir* "@expanded_libexecdir@")
(defparameter *autoconf-datadir* "@expanded_datadir@")
(defparameter *autoconf-infodir* "@expanded_infodir@")
(defparameter *autoconf-host* "unknown")
;; This variable is kept for backwards compatibiliy reasons:
;; We seem to be in the fortunate position that we sometimes need to check for windows.
;; But at least until dec 2015 we didn't need to check for a specific windows flavour.
(defparameter *autoconf-win32* "false")
(defparameter *autoconf-windows* "false")
(defparameter *autoconf-ld-flags* "@LDFLAGS@")

;; This will be T if this was a lisp-only build
(defparameter *autoconf-lisp-only-build* (eq t 't))
 
(defparameter *maxima-source-root* "C:/Users/kfp/quicklisp/local-projects/spadlib/maxima/src/maxima/.")
(defparameter *maxima-default-layout-autotools* "false")

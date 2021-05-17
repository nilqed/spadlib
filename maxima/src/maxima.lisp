;;;
;;; ASDF/QuickLisp
;;;
(defparameter *maxima* (asdf:system-source-directory :maxima))


;;;
;;; special configure/compile/loading procedures for MAXIMA itself
;;;
(defun |configureMAXIMA| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Asrc/maxima" *maxima*))
  (|doSystemCommand| (format nil "lisp (load \"~A\")" "configure.lisp"))
  (|doSystemCommand| (format nil "lisp (configure :interactive nil)"))))


(defun |compileMAXIMA| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Asrc/maxima/src" *maxima*))
  (|doSystemCommand| (format nil "lisp (load \"~A\")" "maxima-build.lisp"))
  (|doSystemCommand| (format nil "lisp (maxima-compile)"))))


(defun |loadMAXIMA| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Asrc/maxima/src" *maxima*))
  (|doSystemCommand| (format nil "lisp (load \"~A\")" "maxima-build.lisp"))
  (|doSystemCommand| (format nil "lisp (maxima-load)"))))


(defun |compile_maxima| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *maxima*))
  (|doSystemCommand| (format nil "compile ../src/maxima.spad )quiet"))))

(defun |load_maxima| ()
  (if (probe-file (format nil "~Alib/maxima.NRLIB/maxima.lsp" *maxima*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *maxima*))
     (|compile_maxima|)))

(defun |test_maxima| ()
  (if (probe-file (format nil "~Atest/test_maxima.input" *maxima*))
    (|doSystemCommand| (format nil "read ~Atest/test_maxima )quiet" *maxima*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_maxima|))


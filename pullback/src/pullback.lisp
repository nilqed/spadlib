;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pullback* (asdf:system-source-directory :pullback))

(defun |compile_pullback| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pullback*))
  (|doSystemCommand| (format nil "compile ../src/pullback.spad )quiet"))))

(defun |load_pullback| ()
  (if (probe-file (format nil "~Alib/pullback.NRLIB/pullback.lsp" *pullback*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pullback*))
     (|compile_pullback|)))

(defun |test_pullback| ()
  (if (probe-file (format nil "~Atest/test_pullback.input" *pullback*))
    (|doSystemCommand| (format nil "read ~Atest/test_pullback )quiet" *pullback*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pullback|))


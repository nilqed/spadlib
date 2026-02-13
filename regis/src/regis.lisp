;;;
;;; ASDF/QuickLisp
;;;
(defparameter *regis* (asdf:system-source-directory :regis))

(defun |compile_regis| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *regis*))
  (|doSystemCommand| (format nil "compile ../src/regis.spad )quiet"))))

(defun |load_regis| ()
  (if (probe-file (format nil "~Alib/regis.NRLIB/regis.lsp" *regis*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *regis*))
     (|compile_regis|)))

(defun |test_regis| ()
  (if (probe-file (format nil "~Atest/test_regis.input" *regis*))
    (|doSystemCommand| (format nil "read ~Atest/test_regis )quiet" *regis*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_regis|))


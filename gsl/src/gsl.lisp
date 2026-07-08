;;;
;;; ASDF/QuickLisp
;;;
(defparameter *gsl* (asdf:system-source-directory :gsl))

(defun |compile_gsl| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *gsl*))
  (|doSystemCommand| (format nil "compile ../src/gsl.spad )quiet"))))

(defun |load_gsl| ()
  (if (probe-file (format nil "~Alib/gsl.NRLIB/gsl.lsp" *gsl*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *gsl*))
     (|compile_gsl|)))

(defun |test_gsl| ()
  (if (probe-file (format nil "~Atest/test_gsl.input" *gsl*))
    (|doSystemCommand| (format nil "read ~Atest/test_gsl )quiet" *gsl*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_gsl|))


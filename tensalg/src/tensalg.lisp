;;;
;;; ASDF/QuickLisp
;;;
(defparameter *tensalg* (asdf:system-source-directory :tensalg))

(defun |compile_tensalg| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *tensalg*))
  (|doSystemCommand| (format nil "compile ../src/tensalg.spad )quiet"))))

(defun |load_tensalg| ()
  (if (probe-file (format nil "~Alib/tensalg.NRLIB/tensalg.lsp" *tensalg*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *tensalg*))
     (|compile_tensalg|)))

(defun |test_tensalg| ()
  (if (probe-file (format nil "~Atest/test_tensalg.input" *tensalg*))
    (|doSystemCommand| (format nil "read ~Atest/test_tensalg )quiet" *tensalg*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_tensalg|))


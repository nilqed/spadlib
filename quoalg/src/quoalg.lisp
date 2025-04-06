;;;
;;; ASDF/QuickLisp
;;;
(defparameter *quoalg* (asdf:system-source-directory :quoalg))

(defun |compile_quoalg| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *quoalg*))
  (|doSystemCommand| (format nil "compile ../src/quoalg.spad )quiet"))))

(defun |load_quoalg| ()
  (if (probe-file (format nil "~Alib/quoalg.NRLIB/quoalg.lsp" *quoalg*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *quoalg*))
     (|compile_quoalg|)))

(defun |test_quoalg| ()
  (if (probe-file (format nil "~Atest/test_quoalg.input" *quoalg*))
    (|doSystemCommand| (format nil "read ~Atest/test_quoalg )quiet" *quoalg*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_quoalg|))


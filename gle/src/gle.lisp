;;;
;;; ASDF/QuickLisp
;;;
(defparameter *gle* (asdf:system-source-directory :gle))

(defun |compile_gle| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *gle*))
  (|doSystemCommand| (format nil "compile ../src/gle.spad )quiet"))))

(defun |load_gle| ()
  (if (probe-file (format nil "~Alib/gle.NRLIB/gle.lsp" *gle*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *gle*))
     (|compile_gle|)))

(defun |test_gle| ()
  (if (probe-file (format nil "~Atest/test_gle.input" *gle*))
    (|doSystemCommand| (format nil "read ~Atest/test_gle )quiet" *gle*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_gle|))


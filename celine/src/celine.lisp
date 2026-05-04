;;;
;;; ASDF/QuickLisp
;;;
(defparameter *celine* (asdf:system-source-directory :celine))

(defun |compile_celine| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *celine*))
  (|doSystemCommand| (format nil "compile ../src/celine.spad )quiet"))))

(defun |load_celine| ()
  (if (probe-file (format nil "~Alib/celine.NRLIB/celine.lsp" *celine*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *celine*))
     (|compile_celine|)))

(defun |test_celine| ()
  (if (probe-file (format nil "~Atest/test_celine.input" *celine*))
    (|doSystemCommand| (format nil "read ~Atest/test_celine )quiet" *celine*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_celine|))


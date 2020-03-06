;;;
;;; ASDF/QuickLisp
;;;
(defparameter *dform* (asdf:system-source-directory :dform))

(defun |compile_dform| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *dform*))
  (|doSystemCommand| (format nil "compile ../src/dform.spad )quiet"))))

(defun |load_dform| ()
  (if (probe-file (format nil "~Alib/dform.NRLIB/dform.lsp" *dform*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *dform*))
     (|compile_dform|)))

(defun |test_dform| ()
  (if (probe-file (format nil "~Atest/test_dform.input" *dform*))
    (|doSystemCommand| (format nil "read ~Atest/test_dform )quiet" *dform*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_dform|))


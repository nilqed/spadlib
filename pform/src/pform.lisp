;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pform* (asdf:system-source-directory :pform))

(defun |compile_pform| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pform*))
  (|doSystemCommand| (format nil "compile ../src/pform.spad )quiet"))))

(defun |load_pform| ()
  (if (probe-file (format nil "~Alib/pform.NRLIB/pform.lsp" *pform*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pform*))
     (|compile_pform|)))

(defun |test_pform| ()
  (if (probe-file (format nil "~Atest/test_pform.input" *pform*))
    (|doSystemCommand| (format nil "read ~Atest/test_pform )quiet" *pform*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pform|))


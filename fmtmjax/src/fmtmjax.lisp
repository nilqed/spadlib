;;;
;;; ASDF/QuickLisp
;;;
(defparameter *fmtmjax* (asdf:system-source-directory :fmtmjax))

(defun |compile_fmtmjax| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *fmtmjax*))
  (|doSystemCommand| (format nil "compile ../src/fmtmjax.spad )quiet"))))

(defun |load_fmtmjax| ()
  (if (probe-file (format nil "~Alib/fmtmjax.NRLIB/fmtmjax.lsp" *fmtmjax*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *fmtmjax*))
     (|compile_fmtmjax|)))

(defun |test_fmtmjax| ()
  (if (probe-file (format nil "~Atest/test_fmtmjax.input" *fmtmjax*))
    (|doSystemCommand| (format nil "read ~Atest/test_fmtmjax )quiet" *fmtmjax*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_fmtmjax|))


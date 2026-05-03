;;;
;;; ASDF/QuickLisp
;;;
(defparameter *htoot* (asdf:system-source-directory :htoot))

(defun |compile_htoot| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *htoot*))
  (|doSystemCommand| (format nil "compile ../src/htoot.spad )quiet"))))

(defun |load_htoot| ()
  (if (probe-file (format nil "~Alib/htoot.NRLIB/htoot.lsp" *htoot*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *htoot*))
     (|compile_htoot|)))

(defun |test_htoot| ()
  (if (probe-file (format nil "~Atest/test_htoot.input" *htoot*))
    (|doSystemCommand| (format nil "read ~Atest/test_htoot )quiet" *htoot*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_htoot|))


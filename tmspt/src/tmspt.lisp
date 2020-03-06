;;;
;;; ASDF/QuickLisp
;;;
(defparameter *tmspt* (asdf:system-source-directory :tmspt))

(defun |compile_tmspt| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *tmspt*))
  (|doSystemCommand| (format nil "compile ../src/tmspt.spad )quiet"))))

(defun |load_tmspt| ()
  (if (probe-file (format nil "~Alib/tmspt.NRLIB/tmspt.lsp" *tmspt*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *tmspt*))
     (|compile_tmspt|)))

(defun |test_tmspt| ()
  (if (probe-file (format nil "~Atest/test_tmspt.input" *tmspt*))
    (|doSystemCommand| (format nil "read ~Atest/test_tmspt )quiet" *tmspt*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_tmspt|))


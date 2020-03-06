;;;
;;; ASDF/QuickLisp
;;;
(defparameter *strspt* (asdf:system-source-directory :strspt))

(defun |compile_strspt| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *strspt*))
  (|doSystemCommand| (format nil "compile ../src/strspt.spad )quiet"))))

(defun |load_strspt| ()
  (if (probe-file (format nil "~Alib/strspt.NRLIB/strspt.lsp" *strspt*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *strspt*))
     (|compile_strspt|)))

(defun |test_strspt| ()
  (if (probe-file (format nil "~Atest/test_strspt.input" *strspt*))
    (|doSystemCommand| (format nil "read ~Atest/test_strspt )quiet" *strspt*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_strspt|))


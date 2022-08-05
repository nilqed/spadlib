;;;
;;; ASDF/QuickLisp
;;;
(defparameter *ltxspt* (asdf:system-source-directory :ltxspt))

(defun |compile_ltxspt| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *ltxspt*))
  (|doSystemCommand| (format nil "compile ../src/ltxspt.spad )quiet"))))

(defun |load_ltxspt| ()
  (if (probe-file (format nil "~Alib/ltxspt.NRLIB/ltxspt.lsp" *ltxspt*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *ltxspt*))
     (|compile_ltxspt|)))

(defun |test_ltxspt| ()
  (if (probe-file (format nil "~Atest/test_ltxspt.input" *ltxspt*))
    (|doSystemCommand| (format nil "read ~Atest/test_ltxspt )quiet" *ltxspt*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_ltxspt|))


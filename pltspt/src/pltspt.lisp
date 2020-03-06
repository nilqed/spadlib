;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pltspt* (asdf:system-source-directory :pltspt))

(defun |compile_pltspt| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pltspt*))
  (|doSystemCommand| (format nil "compile ../src/pltspt.spad )quiet"))))

(defun |load_pltspt| ()
  (if (probe-file (format nil "~Alib/pltspt.NRLIB/pltspt.lsp" *pltspt*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pltspt*))
     (|compile_pltspt|)))

(defun |test_pltspt| ()
  (if (probe-file (format nil "~Atest/test_pltspt.input" *pltspt*))
    (|doSystemCommand| (format nil "read ~Atest/test_pltspt )quiet" *pltspt*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pltspt|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *idxobj* (asdf:system-source-directory :idxobj))

(defun |compile_idxobj| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *idxobj*))
  (|doSystemCommand| (format nil "compile ../src/idxobj.spad )quiet"))))

(defun |load_idxobj| ()
  (if (probe-file (format nil "~Alib/idxobj.NRLIB/idxobj.lsp" *idxobj*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *idxobj*))
     (|compile_idxobj|)))

(defun |test_idxobj| ()
  (if (probe-file (format nil "~Atest/test_idxobj.input" *idxobj*))
    (|doSystemCommand| (format nil "read ~Atest/test_idxobj )quiet" *idxobj*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_idxobj|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *oobj* (asdf:system-source-directory :oobj))

(defun |compile_oobj| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *oobj*))
  (|doSystemCommand| (format nil "compile ../src/oobj.spad )quiet"))))

(defun |load_oobj| ()
  (if (probe-file (format nil "~Alib/oobj.NRLIB/oobj.lsp" *oobj*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *oobj*))
     (|compile_oobj|)))

(defun |test_oobj| ()
  (if (probe-file (format nil "~Atest/test_oobj.input" *oobj*))
    (|doSystemCommand| (format nil "read ~Atest/test_oobj )quiet" *oobj*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_oobj|))


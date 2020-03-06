;;;
;;; ASDF/QuickLisp
;;;
(defparameter *quicklsp* (asdf:system-source-directory :quicklsp))

(defun |compile_quicklsp| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *quicklsp*))
  (|doSystemCommand| (format nil "compile ../src/quicklsp.spad )quiet"))))

(defun |load_quicklsp| ()
  (if (probe-file (format nil "~Alib/quicklsp.NRLIB/quicklsp.lsp" *quicklsp*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *quicklsp*))
     (|compile_quicklsp|)))

(defun |test_quicklsp| ()
  (if (probe-file (format nil "~Atest/test_quicklsp.input" *quicklsp*))
    (|doSystemCommand| (format nil "read ~Atest/test_quicklsp )quiet" *quicklsp*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_quicklsp|))


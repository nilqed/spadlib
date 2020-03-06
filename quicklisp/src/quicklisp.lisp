;;;
;;; ASDF/QuickLisp
;;;
(defparameter *quicklisp* (asdf:system-source-directory :quicklisp))

(defun |compile_quicklisp| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *quicklisp*))
  (|doSystemCommand| (format nil "compile ../src/quicklisp.spad )quiet"))))

(defun |load_quicklisp| ()
  (if (probe-file (format nil "~Alib/quicklisp.NRLIB/quicklisp.lsp" *quicklisp*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *quicklisp*))
     (|compile_quicklisp|)))

(defun |test_quicklisp| ()
  (if (probe-file (format nil "~Atest/test_quicklisp.input" *quicklisp*))
    (|doSystemCommand| (format nil "read ~Atest/test_quicklisp )quiet" *quicklisp*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_quicklisp|))


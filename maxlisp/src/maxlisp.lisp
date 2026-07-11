;;;
;;; ASDF/QuickLisp
;;;
(defparameter *maxlisp* (asdf:system-source-directory :maxlisp))

(defun |compile_maxlisp| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *maxlisp*))
  (|doSystemCommand| (format nil "compile ../src/maxlisp.spad )quiet"))))

(defun |load_maxlisp| ()
  (if (probe-file (format nil "~Alib/maxlisp.NRLIB/maxlisp.lsp" *maxlisp*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *maxlisp*))
     (|compile_maxlisp|)))

(defun |test_maxlisp| ()
  (if (probe-file (format nil "~Atest/test_maxlisp.input" *maxlisp*))
    (|doSystemCommand| (format nil "read ~Atest/test_maxlisp )quiet" *maxlisp*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_maxlisp|))


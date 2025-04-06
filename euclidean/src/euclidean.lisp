;;;
;;; ASDF/QuickLisp
;;;
(defparameter *euclidean* (asdf:system-source-directory :euclidean))

(defun |compile_euclidean| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *euclidean*))
  (|doSystemCommand| (format nil "compile ../src/euclidean.spad )quiet"))))

(defun |load_euclidean| ()
  (if (probe-file (format nil "~Alib/euclidean.NRLIB/euclidean.lsp" *euclidean*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *euclidean*))
     (|compile_euclidean|)))

(defun |test_euclidean| ()
  (if (probe-file (format nil "~Atest/test_euclidean.input" *euclidean*))
    (|doSystemCommand| (format nil "read ~Atest/test_euclidean )quiet" *euclidean*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_euclidean|))


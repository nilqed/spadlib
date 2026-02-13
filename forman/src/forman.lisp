;;;
;;; ASDF/QuickLisp
;;;
(defparameter *forman* (asdf:system-source-directory :forman))

(defun |compile_forman| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *forman*))
  (|doSystemCommand| (format nil "compile ../src/forman.spad )quiet"))))

(defun |load_forman| ()
  (if (probe-file (format nil "~Alib/forman.NRLIB/forman.lsp" *forman*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *forman*))
     (|compile_forman|)))

(defun |test_forman| ()
  (if (probe-file (format nil "~Atest/test_forman.input" *forman*))
    (|doSystemCommand| (format nil "read ~Atest/test_forman )quiet" *forman*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_forman|))


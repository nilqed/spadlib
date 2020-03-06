;;;
;;; ASDF/QuickLisp
;;;
(defparameter *awaic* (asdf:system-source-directory :awaic))

(defun |compile_awaic| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *awaic*))
  (|doSystemCommand| (format nil "compile ../src/awaic.spad )quiet"))))

(defun |load_awaic| ()
  (if (probe-file (format nil "~Alib/awaic.NRLIB/awaic.lsp" *awaic*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *awaic*))
     (|compile_awaic|)))

(defun |test_awaic| ()
  (if (probe-file (format nil "~Atest/test_awaic.input" *awaic*))
    (|doSystemCommand| (format nil "read ~Atest/test_awaic )quiet" *awaic*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_awaic|))


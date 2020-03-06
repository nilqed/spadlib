;;;
;;; ASDF/QuickLisp
;;;
(defparameter *codata* (asdf:system-source-directory :codata))

(defun |compile_codata| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *codata*))
  (|doSystemCommand| (format nil "compile ../src/codata.spad )quiet"))))

(defun |load_codata| ()
  (if (probe-file (format nil "~Alib/codata.NRLIB/codata.lsp" *codata*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *codata*))
     (|compile_codata|)))

(defun |test_codata| ()
  (if (probe-file (format nil "~Atest/test_codata.input" *codata*))
    (|doSystemCommand| (format nil "read ~Atest/test_codata )quiet" *codata*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_codata|))


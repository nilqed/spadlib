;;;
;;; ASDF/QuickLisp
;;;
(defparameter *aosc* (asdf:system-source-directory :aosc))

(defun |compile_aosc| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *aosc*))
  (|doSystemCommand| (format nil "compile ../src/aosc.spad )quiet"))))

(defun |load_aosc| ()
  (if (probe-file (format nil "~Alib/aosc.NRLIB/aosc.lsp" *aosc*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *aosc*))
     (|compile_aosc|)))

(defun |test_aosc| ()
  (if (probe-file (format nil "~Atest/test_aosc.input" *aosc*))
    (|doSystemCommand| (format nil "read ~Atest/test_aosc )quiet" *aosc*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_aosc|))


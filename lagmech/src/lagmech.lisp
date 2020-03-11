;;;
;;; ASDF/QuickLisp
;;;
(defparameter *lagmech* (asdf:system-source-directory :lagmech))

(defun |compile_lagmech| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *lagmech*))
  (|doSystemCommand| (format nil "compile ../src/lagmech.spad )quiet"))))

(defun |load_lagmech| ()
  (if (probe-file (format nil "~Alib/lagmech.NRLIB/lagmech.lsp" *lagmech*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *lagmech*))
     (|compile_lagmech|)))

(defun |test_lagmech| ()
  (if (probe-file (format nil "~Atest/test_lagmech.input" *lagmech*))
    (|doSystemCommand| (format nil "read ~Atest/test_lagmech )quiet" *lagmech*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_lagmech|))


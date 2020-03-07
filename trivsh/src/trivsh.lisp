;;;
;;; ASDF/QuickLisp
;;;
(defparameter *trivsh* (asdf:system-source-directory :trivsh))

(defun |compile_trivsh| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *trivsh*))
  (|doSystemCommand| (format nil "compile ../src/trivsh.spad )quiet"))))

(defun |load_trivsh| ()
  (if (probe-file (format nil "~Alib/trivsh.NRLIB/trivsh.lsp" *trivsh*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *trivsh*))
     (|compile_trivsh|)))

(defun |test_trivsh| ()
  (if (probe-file (format nil "~Atest/test_trivsh.input" *trivsh*))
    (|doSystemCommand| (format nil "read ~Atest/test_trivsh )quiet" *trivsh*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_trivsh|))


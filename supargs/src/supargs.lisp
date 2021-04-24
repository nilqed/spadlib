;;;
;;; ASDF/QuickLisp
;;;
(defparameter *supargs* (asdf:system-source-directory :supargs))

(defun |compile_supargs| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *supargs*))
  (|doSystemCommand| (format nil "compile ../src/supargs.spad )quiet"))))

(defun |load_supargs| ()
  (if (probe-file (format nil "~Alib/supargs.NRLIB/supargs.lsp" *supargs*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *supargs*))
     (|compile_supargs|)))

(defun |test_supargs| ()
  (if (probe-file (format nil "~Atest/test_supargs.input" *supargs*))
    (|doSystemCommand| (format nil "read ~Atest/test_supargs )quiet" *supargs*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_supargs|))


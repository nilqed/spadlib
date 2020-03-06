;;;
;;; ASDF/QuickLisp
;;;
(defparameter *ltxpv* (asdf:system-source-directory :ltxpv))

(defun |compile_ltxpv| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *ltxpv*))
  (|doSystemCommand| (format nil "compile ../src/ltxpv.spad )quiet"))))

(defun |load_ltxpv| ()
  (if (probe-file (format nil "~Alib/ltxpv.NRLIB/ltxpv.lsp" *ltxpv*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *ltxpv*))
     (|compile_ltxpv|)))

(defun |test_ltxpv| ()
  (if (probe-file (format nil "~Atest/test_ltxpv.input" *ltxpv*))
    (|doSystemCommand| (format nil "read ~Atest/test_ltxpv )quiet" *ltxpv*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_ltxpv|))


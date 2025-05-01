;;;
;;; ASDF/QuickLisp
;;;
(defparameter *unify* (asdf:system-source-directory :unify))

(defun |compile_unify| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *unify*))
  (|doSystemCommand| (format nil "compile ../src/unify.spad )quiet"))))

(defun |load_unify| ()
  (if (probe-file (format nil "~Alib/unify.NRLIB/unify.lsp" *unify*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *unify*))
     (|compile_unify|)))

(defun |test_unify| ()
  (if (probe-file (format nil "~Atest/test_unify.input" *unify*))
    (|doSystemCommand| (format nil "read ~Atest/test_unify )quiet" *unify*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_unify|))


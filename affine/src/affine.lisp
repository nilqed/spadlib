;;;
;;; ASDF/QuickLisp
;;;
(defparameter *affine* (asdf:system-source-directory :affine))

(defun |compile_affine| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *affine*))
  (|doSystemCommand| (format nil "compile ../src/affine.spad )quiet"))))

(defun |load_affine| ()
  (if (probe-file (format nil "~Alib/affine.NRLIB/affine.lsp" *affine*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *affine*))
     (|compile_affine|)))

(defun |test_affine| ()
  (if (probe-file (format nil "~Atest/test_affine.input" *affine*))
    (|doSystemCommand| (format nil "read ~Atest/test_affine )quiet" *affine*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_affine|))


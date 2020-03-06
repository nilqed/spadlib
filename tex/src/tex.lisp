;;;
;;; ASDF/QuickLisp
;;;
(defparameter *tex* (asdf:system-source-directory :tex))

(defun |compile_tex| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *tex*))
  (|doSystemCommand| (format nil "compile ../src/tex.spad )quiet"))))

(defun |load_tex| ()
  (if (probe-file (format nil "~Alib/tex.NRLIB/tex.lsp" *tex*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *tex*))
     (|compile_tex|)))

(defun |test_tex| ()
  (if (probe-file (format nil "~Atest/test_tex.input" *tex*))
    (|doSystemCommand| (format nil "read ~Atest/test_tex )quiet" *tex*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_tex|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *hypgeom* (asdf:system-source-directory :hypgeom))

(defun |compile_hypgeom| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *hypgeom*))
  (|doSystemCommand| (format nil "compile ../src/hypgeom.spad )quiet"))))

(defun |load_hypgeom| ()
  (if (probe-file (format nil "~Alib/hypgeom.NRLIB/hypgeom.lsp" *hypgeom*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *hypgeom*))
     (|compile_hypgeom|)))

(defun |test_hypgeom| ()
  (if (probe-file (format nil "~Atest/test_hypgeom.input" *hypgeom*))
    (|doSystemCommand| (format nil "read ~Atest/test_hypgeom )quiet" *hypgeom*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_hypgeom|))


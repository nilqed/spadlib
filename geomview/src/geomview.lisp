;;;
;;; ASDF/QuickLisp
;;;
(defparameter *geomview* (asdf:system-source-directory :geomview))

(defun |compile_geomview| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *geomview*))
  (|doSystemCommand| (format nil "compile ../src/geomview.spad )quiet"))))

(defun |load_geomview| ()
  (if (probe-file (format nil "~Alib/geomview.NRLIB/geomview.lsp" *geomview*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *geomview*))
     (|compile_geomview|)))

(defun |test_geomview| ()
  (if (probe-file (format nil "~Atest/test_geomview.input" *geomview*))
    (|doSystemCommand| (format nil "read ~Atest/test_geomview )quiet" *geomview*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_geomview|))


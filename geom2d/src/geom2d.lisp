;;;
;;; ASDF/QuickLisp
;;;
(defparameter *geom2d* (asdf:system-source-directory :geom2d))

(defun |compile_geom2d| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *geom2d*))
  (|doSystemCommand| (format nil "compile ../src/geom2d.spad )quiet"))))

(defun |load_geom2d| ()
  (if (probe-file (format nil "~Alib/geom2d.NRLIB/geom2d.lsp" *geom2d*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *geom2d*))
     (|compile_geom2d|)))

(defun |test_geom2d| ()
  (if (probe-file (format nil "~Atest/test_geom2d.input" *geom2d*))
    (|doSystemCommand| (format nil "read ~Atest/test_geom2d )quiet" *geom2d*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_geom2d|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *frustum* (asdf:system-source-directory :frustum))

(defun |compile_frustum| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *frustum*))
  (|doSystemCommand| (format nil "compile ../src/frustum.spad )quiet"))))

(defun |load_frustum| ()
  (if (probe-file (format nil "~Alib/frustum.NRLIB/frustum.lsp" *frustum*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *frustum*))
     (|compile_frustum|)))

(defun |test_frustum| ()
  (if (probe-file (format nil "~Atest/test_frustum.input" *frustum*))
    (|doSystemCommand| (format nil "read ~Atest/test_frustum )quiet" *frustum*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_frustum|))


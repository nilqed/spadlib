;;;
;;; ASDF/QuickLisp
;;;
(defparameter *webclient* (asdf:system-source-directory :webclient))

(defun |compile_webclient| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *webclient*))
  (|doSystemCommand| (format nil "compile ../src/webclient.spad )quiet"))))

(defun |load_webclient| ()
  (if (probe-file (format nil "~Alib/webclient.NRLIB/webclient.lsp" *webclient*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *webclient*))
     (|compile_webclient|)))

(defun |test_webclient| ()
  (if (probe-file (format nil "~Atest/test_webclient.input" *webclient*))
    (|doSystemCommand| (format nil "read ~Atest/test_webclient )quiet" *webclient*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_webclient|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *space3* (asdf:system-source-directory :space3))

(defun |compile_space3| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *space3*))
  (|doSystemCommand| (format nil "compile ../src/space3.spad )quiet"))))

(defun |load_space3| ()
  (if (probe-file (format nil "~Alib/space3.NRLIB/space3.lsp" *space3*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *space3*))
     (|compile_space3|)))

(defun |test_space3| ()
  (if (probe-file (format nil "~Atest/test_space3.input" *space3*))
    (|doSystemCommand| (format nil "read ~Atest/test_space3 )quiet" *space3*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_space3|))


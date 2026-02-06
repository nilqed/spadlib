;;;
;;; ASDF/QuickLisp
;;;
(defparameter *dialog* (asdf:system-source-directory :dialog))

(defun |compile_dialog| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *dialog*))
  (|doSystemCommand| (format nil "compile ../src/dialog.spad )quiet"))))

(defun |load_dialog| ()
  (if (probe-file (format nil "~Alib/dialog.NRLIB/dialog.lsp" *dialog*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *dialog*))
     (|compile_dialog|)))

(defun |test_dialog| ()
  (if (probe-file (format nil "~Atest/test_dialog.input" *dialog*))
    (|doSystemCommand| (format nil "read ~Atest/test_dialog )quiet" *dialog*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_dialog|))


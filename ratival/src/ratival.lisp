;;;
;;; ASDF/QuickLisp
;;;
(defparameter *ratival* (asdf:system-source-directory :ratival))

(defun |compile_ratival| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *ratival*))
  (|doSystemCommand| (format nil "compile ../src/ratival.spad )quiet"))))

(defun |load_ratival| ()
  (if (probe-file (format nil "~Alib/ratival.NRLIB/ratival.lsp" *ratival*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *ratival*))
     (|compile_ratival|)))

(defun |test_ratival| ()
  (if (probe-file (format nil "~Atest/test_ratival.input" *ratival*))
    (|doSystemCommand| (format nil "read ~Atest/test_ratival )quiet" *ratival*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_ratival|))


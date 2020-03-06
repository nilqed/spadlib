;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pipe* (asdf:system-source-directory :pipe))

(defun |compile_pipe| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pipe*))
  (|doSystemCommand| (format nil "compile ../src/pipe.spad )quiet"))))

(defun |load_pipe| ()
  (if (probe-file (format nil "~Alib/pipe.NRLIB/pipe.lsp" *pipe*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pipe*))
     (|compile_pipe|)))

(defun |test_pipe| ()
  (if (probe-file (format nil "~Atest/test_pipe.input" *pipe*))
    (|doSystemCommand| (format nil "read ~Atest/test_pipe )quiet" *pipe*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pipe|))


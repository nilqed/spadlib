;;;
;;; ASDF/QuickLisp
;;;
(defparameter *rete* (asdf:system-source-directory :rete))

(defun |compile_rete| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *rete*))
  (|doSystemCommand| (format nil "compile ../src/rete.spad )quiet"))))

(defun |load_rete| ()
  (if (probe-file (format nil "~Alib/rete.NRLIB/rete.lsp" *rete*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *rete*))
     (|compile_rete|)))

(defun |test_rete| ()
  (if (probe-file (format nil "~Atest/test_rete.input" *rete*))
    (|doSystemCommand| (format nil "read ~Atest/test_rete )quiet" *rete*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_rete|))


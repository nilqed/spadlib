;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pchain* (asdf:system-source-directory :pchain))

(defun |compile_pchain| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pchain*))
  (|doSystemCommand| (format nil "compile ../src/pchain.spad )quiet"))
  (|doSystemCommand| (format nil "compile ../src/rpc.spad )quiet"))))

(defun |load_pchain| ()
  (if (probe-file (format nil "~Alib/pchain.NRLIB/pchain.lsp" *pchain*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pchain*))
     (|compile_pchain|)))

(defun |test_pchain| ()
  (if (probe-file (format nil "~Atest/test_pchain.input" *pchain*))
    (|doSystemCommand| (format nil "read ~Atest/test_pchain )quiet" *pchain*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pchain|))


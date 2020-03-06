;;;
;;; ASDF/QuickLisp
;;;
(defparameter *pqty* (asdf:system-source-directory :pqty))

(defun |compile_pqty| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *pqty*))
  (|doSystemCommand| (format nil "compile ../src/pqty.spad )quiet"))))

(defun |load_pqty| ()
  (if (probe-file (format nil "~Alib/pqty.NRLIB/pqty.lsp" *pqty*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *pqty*))
     (|compile_pqty|)))

(defun |test_pqty| ()
  (if (probe-file (format nil "~Atest/test_pqty.input" *pqty*))
    (|doSystemCommand| (format nil "read ~Atest/test_pqty )quiet" *pqty*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_pqty|))


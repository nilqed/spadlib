;;;
;;; ASDF/QuickLisp
;;;
(defparameter *sqlite* (asdf:system-source-directory :sqlite))

(defun |compile_sqlite| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *sqlite*))
  (|doSystemCommand| (format nil "compile ../src/sqlite.spad )quiet"))))

(defun |load_sqlite| ()
  (if (probe-file (format nil "~Alib/sqlite.NRLIB/sqlite.lsp" *sqlite*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *sqlite*))
     (|compile_sqlite|)))

(defun |test_sqlite| ()
  (if (probe-file (format nil "~Atest/test_sqlite.input" *sqlite*))
    (|doSystemCommand| (format nil "read ~Atest/test_sqlite )quiet" *sqlite*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_sqlite|))


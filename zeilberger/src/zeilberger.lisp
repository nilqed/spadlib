;;;
;;; ASDF/QuickLisp
;;;
(defparameter *zeilberger* (asdf:system-source-directory :zeilberger))

(defun |compile_zeilberger| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *zeilberger*))
  (|doSystemCommand| (format nil "compile ../src/zeilberger.spad )quiet"))))

(defun |load_zeilberger| ()
  (if (probe-file (format nil "~Alib/zeilberger.NRLIB/zeilberger.lsp" *zeilberger*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *zeilberger*))
     (|compile_zeilberger|)))

(defun |test_zeilberger| ()
  (if (probe-file (format nil "~Atest/test_zeilberger.input" *zeilberger*))
    (|doSystemCommand| (format nil "read ~Atest/test_zeilberger )quiet" *zeilberger*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_zeilberger|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *jupy* (asdf:system-source-directory :jupy))

(defun |compile_jupy| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *jupy*))
  (|doSystemCommand| (format nil "compile ../src/jupy.spad )quiet"))))

(defun |load_jupy| ()
  (if (probe-file (format nil "~Alib/jupy.NRLIB/jupy.lsp" *jupy*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *jupy*))
     (|compile_jupy|)))

(defun |test_jupy| ()
  (if (probe-file (format nil "~Atest/test_jupy.input" *jupy*))
    (|doSystemCommand| (format nil "read ~Atest/test_jupy )quiet" *jupy*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_jupy|))


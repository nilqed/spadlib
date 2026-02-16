;;;
;;; ASDF/QuickLisp
;;;
(defparameter *sixel* (asdf:system-source-directory :sixel))

(defun |compile_sixel| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *sixel*))
  (|doSystemCommand| (format nil "compile ../src/sixel.spad )quiet"))))

(defun |load_sixel| ()
  (if (probe-file (format nil "~Alib/sixel.NRLIB/sixel.lsp" *sixel*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *sixel*))
     (|compile_sixel|)))

(defun |test_sixel| ()
  (if (probe-file (format nil "~Atest/test_sixel.input" *sixel*))
    (|doSystemCommand| (format nil "read ~Atest/test_sixel )quiet" *sixel*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_sixel|))


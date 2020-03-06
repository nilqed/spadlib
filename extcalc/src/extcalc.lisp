;;;
;;; ASDF/QuickLisp
;;;
(defparameter *extcalc* (asdf:system-source-directory :extcalc))

(defun |compile_extcalc| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *extcalc*))
  (|doSystemCommand| (format nil "compile ../src/extcalc.spad )quiet"))))

(defun |load_extcalc| ()
  (if (probe-file (format nil "~Alib/extcalc.NRLIB/extcalc.lsp" *extcalc*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *extcalc*))
     (|compile_extcalc|)))

(defun |test_extcalc| ()
  (if (probe-file (format nil "~Atest/test_extcalc.input" *extcalc*))
    (|doSystemCommand| (format nil "read ~Atest/test_extcalc )quiet" *extcalc*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_extcalc|))


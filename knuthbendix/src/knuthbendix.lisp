;;;
;;; ASDF/QuickLisp
;;;
(defparameter *knuthbendix* (asdf:system-source-directory :knuthbendix))

(defun |compile_knuthbendix| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *knuthbendix*))
  (|doSystemCommand| (format nil "compile ../src/knuthbendix.spad )quiet"))))

(defun |load_knuthbendix| ()
  (if (probe-file (format nil "~Alib/knuthbendix.NRLIB/knuthbendix.lsp" *knuthbendix*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *knuthbendix*))
     (|compile_knuthbendix|)))

(defun |test_knuthbendix| ()
  (if (probe-file (format nil "~Atest/test_knuthbendix.input" *knuthbendix*))
    (|doSystemCommand| (format nil "read ~Atest/test_knuthbendix )quiet" *knuthbendix*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_knuthbendix|))


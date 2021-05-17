;;;
;;; ASDF/QuickLisp
;;;
(defparameter *drawfe* (asdf:system-source-directory :drawfe))

(defun |compile_drawfe| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *drawfe*))
  (|doSystemCommand| (format nil "compile ../src/drawfe.spad )quiet"))))

(defun |load_drawfe| ()
  (if (probe-file (format nil "~Alib/drawfe.NRLIB/drawfe.lsp" *drawfe*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *drawfe*))
     (|compile_drawfe|)))

(defun |test_drawfe| ()
  (if (probe-file (format nil "~Atest/test_drawfe.input" *drawfe*))
    (|doSystemCommand| (format nil "read ~Atest/test_drawfe )quiet" *drawfe*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_drawfe|))


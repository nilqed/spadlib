;;;
;;; ASDF/QuickLisp
;;;
(defparameter *canvas* (asdf:system-source-directory :canvas))

(defun |compile_canvas| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *canvas*))
  (|doSystemCommand| (format nil "compile ../src/canvas.spad )quiet"))))

(defun |load_canvas| ()
  (if (probe-file (format nil "~Alib/canvas.NRLIB/canvas.lsp" *canvas*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *canvas*))
     (|compile_canvas|)))

(defun |test_canvas| ()
  (if (probe-file (format nil "~Atest/test_canvas.input" *canvas*))
    (|doSystemCommand| (format nil "read ~Atest/test_canvas )quiet" *canvas*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_canvas|))


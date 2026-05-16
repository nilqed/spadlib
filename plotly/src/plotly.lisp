;;;
;;; ASDF/QuickLisp
;;;
(defparameter *plotly* (asdf:system-source-directory :plotly))

(defun |compile_plotly| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *plotly*))
  (|doSystemCommand| (format nil "compile ../src/plotly.spad )quiet"))))

(defun |load_plotly| ()
  (if (probe-file (format nil "~Alib/plotly.NRLIB/plotly.lsp" *plotly*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *plotly*))
     (|compile_plotly|)))

(defun |test_plotly| ()
  (if (probe-file (format nil "~Atest/test_plotly.input" *plotly*))
    (|doSystemCommand| (format nil "read ~Atest/test_plotly )quiet" *plotly*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_plotly|))


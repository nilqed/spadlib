;;;
;;; ASDF/QuickLisp
;;;
(defparameter *tmux* (asdf:system-source-directory :tmux))

(defun |compile_tmux| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *tmux*))
  (|doSystemCommand| (format nil "compile ../src/tmux.spad )quiet"))))

(defun |load_tmux| ()
  (if (probe-file (format nil "~Alib/tmux.NRLIB/tmux.lsp" *tmux*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *tmux*))
     (|compile_tmux|)))

(defun |test_tmux| ()
  (if (probe-file (format nil "~Atest/test_tmux.input" *tmux*))
    (|doSystemCommand| (format nil "read ~Atest/test_tmux )quiet" *tmux*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_tmux|))


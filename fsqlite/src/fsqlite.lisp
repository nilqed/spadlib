;;;
;;; ASDF/QuickLisp
;;;
(defparameter *fsqlite* (asdf:system-source-directory :fsqlite))

(defun |compile_fsqlite| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *fsqlite*))
  (|doSystemCommand| (format nil "compile ../src/fsqlite.spad )quiet"))))

(defun |load_fsqlite| ()
  (if (probe-file (format nil "~Alib/fsqlite.NRLIB/fsqlite.lsp" *fsqlite*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *fsqlite*))
     (|compile_fsqlite|)))

(defun |test_fsqlite| ()
  (if (probe-file (format nil "~Atest/test_fsqlite.input" *fsqlite*))
    (|doSystemCommand| (format nil "read ~Atest/test_fsqlite )quiet" *fsqlite*))
    (print "Test file not found ...")))

(catch 'spad_reader (|load_fsqlite|))


;;;
;;; ASDF/QuickLisp
;;;
(defparameter *reduce* (asdf:system-source-directory :reduce))

(defun |compile_reduce| ()
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *reduce*))
  (|doSystemCommand| (format nil "compile ../src/reduce.spad )quiet"))))

(defun |load_reduce| ()
  (if (probe-file (format nil "~Alib/reduce.NRLIB/reduce.lsp" *reduce*))
     (|doSystemCommand| (format nil "lib )dir ~Alib/" *reduce*))
     (|compile_reduce|)))

(defun |test_reduce| ()
  (if (probe-file (format nil "~Atest/test_reduce.input" *reduce*))
    (|doSystemCommand| (format nil "read ~Atest/test_reduce )quiet" *reduce*))
    (print "Test file not found ...")))
   
;;; added manually Thu 17 Apr 23:20:52 CEST 2025/kfp
;;; call load_bootstrap_reduce()$Lisp from FriCAS or via CL.
(defun |load_bootstrap_reduce| ()
(progn
  (|doSystemCommand| (format nil "cd ~Asrc" *reduce*))
  (load "bootstrap.lisp")))
  
(defun |load_sl_on_cl| ()
(progn
  (|doSystemCommand| (format nil "cd ~Asrc" *reduce*))
  (load "sl_on_cl.lisp")))


(defun |compile_spad_in_src_folder| (s)
  (progn
  (|doSystemCommand| (format nil "cd ~Alib" *reduce*))
  (|doSystemCommand| (format nil "compile ../src/~A.spad )quiet" s))))


(catch 'spad_reader (|load_reduce|))


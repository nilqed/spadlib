;;; This file can be loaded into FriCAS-TeXmacs by the command
;;;   )read path/ioHook.lisp
;;; and serves to redefine the Lisp funtion |$ioHook| which 
;;; handles the output of TeXmacs to FriCAS. It will use the 
;;; "latex:" message instead of "verbatim:" in order to display the
;;; FriCAS Types more nicely. One may also adjust the startup shell file 
;;; "fricas" where $ioHook is defined in the first place.
;;; Mo 21 Okt 2024 17:33:22 CEST/kfp


(in-package :boot)

(defun tm-start-prompt () 
    (princ (concat (code-char 2) "prompt#")))

(defun tm-end-of-output ()
    (princ (concat (code-char 5) (code-char 10))))

(defun tm-start-output ()
    (princ (code-char 2)))


(defparameter font-size "tiny")
(defparameter font-color "darkgrey")

(defun scm-for-keyed-msg (fc fs)
   (format 'nil "latex:\\textcolor{~A}{\\~A " fc fs))

(defun tm-start-keyed-msg ()          
     (princ (concat (code-char 2) (scm-for-keyed-msg font-color font-size) ))) 


(defun tm-end-of-keyed-msg () 
    (princ (concat "}" (code-char 5))))

(defun tm-end-of-prompt ()
    (princ (code-char 5) ))

(setf |$ioHook| (lambda (x &optional args)  
  (cond 
    ((eq x '|startPrompt|) (tm-start-prompt))        
    ((eq x '|endOfTeXmacsOutput|) (tm-end-of-output))      
    ((eq x '|startTeXmacsOutput|) (tm-start-output))        
    ((eq x '|startKeyedMsg|) (tm-start-keyed-msg))        
    ((eq x '|endOfKeyedMsg|) (tm-end-of-keyed-msg))        
    ((eq x '|endOfPrompt|)  (tm-end-of-prompt)))))





;;; NOTES:
;;; Test with: fricas -nosman  -eval ")read ioHook.lisp" to see raw output.
;;;  "font-series" "bold" works.
;;; --> concat exists in SCM as well as in BOOT ...
;;; )r /home/kfp/Desktop/work/spadlib/tmspt/src/ioHook.lisp
;;; Alternatives:
;;; (format 'nil "scheme:(with \"color\" \"~A\" \"font-size\" \"~A\" (concat \"" fc fs))
;;; (format 'nil "latex:\\textcolor{darkgreen}{\\tiny \\href{http://www.fricas.org}{"))

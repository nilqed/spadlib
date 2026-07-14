(defvar n 1000)  ;;; to vary

; Source - https://stackoverflow.com/q/25866292
; Retrieved 2026-07-12, License - CC BY-SA 3.0
(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

            
; Source - https://stackoverflow.com/a/13937652
; Retrieved 2026-07-12, License - CC BY-SA 3.0
(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))


(defvar r (range (1+ n) :min (- n) :step 1))

(defvar rl 
(loop for x in r
  collect
   (loop for y in r
     collect (+ (* x x) (* x y) (* y y)))))

;;; list only
(defvar frl (flatten rl))
(format t "~%Created list of ~a frustums. ~%~%" (length frl))

;;; sorted, duplicates removed
;(defvar frl (sort (remove-duplicates (flatten rl)) #'<))
;(format t "~%There are ~a frustums for [-~a...~a] ~%~%" (length frl) n n)



#+clasp (ext:quit)  
#-clasp (quit)




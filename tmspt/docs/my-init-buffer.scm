;;; My keyboard map (kfp/22-04-2014)
;;; Updated: 26-OCT-2024





;;; Any mode      

(kbd-map
        ;; CTRL-$ toogle math input 
        ("C-$" (toggle-session-math-input))
	
	;; insert -> fold -> executable -> maxima
	("m a x i m a ."  
	  (begin (insert-go-to `(script-input ,"maxima" "default" "" "") '(2 0))))

	;; insert -> fold -> executable -> Pure
	("p u r e ."  
	  (begin (insert-go-to `(script-input ,"pure" "default" "" "")   '(2 0))))
	
	;; insert -> fold -> executable -> FriCas
	("f r i c a s ."  
	  (begin (insert-go-to `(script-input ,"fricas" "default" "" "") '(2 0))))
	;; insert -> fold -> executable -> FriCas
	("r e d u c e ."  
	  (begin (insert-go-to `(script-input ,"fricas" "default" "" "") '(2 0))))	

        ("p u r e s e s s i o n ."
           (make-session "pure" "default"))
        ("r e d s e s s i o n ."
           (make-session "reduce" "default"))
        ("m a x s e s s i o n ."
           (make-session "maxima" "default"))
        ("f r i c a s s e s s i o n ."
           (make-session "fricas" "default"))

         ("f r i $"
           (make-session "fricas" "default"))

)



;;; Text mode only 

(kbd-map
	(:mode in-text?)
	("e q u ."    (begin (insert "Eq. (" ) 
			      (make 'reference) 
			      (insert "equ:")))
	
	("r e f ."    (begin (make 'cite) 
			      (insert "bib:")))
	
	("e q ."    (make-equation*))
	
	("e q n ."  (make-equation))
	
)	


;;; Math mode only 


(kbd-map
	(:mode in-math?)
	("n a b ."  (insert "<nabla>"))
	("h b a ."  (insert "<hbar>"))
	("e q u ."  (begin (make 'label) (insert "equ:")))
	
)
	



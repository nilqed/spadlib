(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'XCRIT)) 
(FLUID '(XVARLIST* ZERODIVS* XTRUNCATE* *TWOSIDED)) 
(PUT 'CRITICAL_PAIRS 'NUMBER-OF-ARGS 3) 
(PUT 'CRITICAL_PAIRS 'DEFINED-ON-LINE '44) 
(PUT 'CRITICAL_PAIRS 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'CRITICAL_PAIRS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CRITICAL_PAIRS (Q P C)
    (PROG (F)
      (PROG (L)
        (SETQ L Q)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        (PROG ()
          (SETQ F (CAR L))
          (PROG (G)
            (SETQ G (CDR L))
           LAB
            (COND ((NULL G) (RETURN NIL)))
            ((LAMBDA (G)
               ((LAMBDA (PR) (COND (PR (ADD_ITEM PR C))))
                (MAKE_SPOLY_PAIR F G)))
             (CAR G))
            (SETQ G (CDR G))
            (GO LAB))
          (PROG (G)
            (SETQ G P)
           LAB
            (COND ((NULL G) (RETURN NIL)))
            ((LAMBDA (G)
               ((LAMBDA (PR) (COND (PR (ADD_ITEM PR C))))
                (MAKE_SPOLY_PAIR F G)))
             (CAR G))
            (SETQ G (CDR G))
            (GO LAB))
          (PROG (X)
            (SETQ X ZERODIVS*)
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               ((LAMBDA (PR) (COND (PR (ADD_ITEM PR C))))
                (MAKE_WEDGE_PAIR X F)))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (PROG (X)
            (SETQ X (COND (*TWOSIDED XVARLIST*)))
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               ((LAMBDA (PR) (COND (PR (ADD_ITEM PR C))))
                (MAKE_XCOMM_PAIR X F)))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB)))
        (SETQ L (CDR L))
        (GO LAB))
      (RETURN C))) 
(PUT 'REMOVE_CRITICAL_PAIRS 'NUMBER-OF-ARGS 2) 
(PUT 'REMOVE_CRITICAL_PAIRS 'DEFINED-ON-LINE '65) 
(PUT 'REMOVE_CRITICAL_PAIRS 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'REMOVE_CRITICAL_PAIRS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_CRITICAL_PAIRS (G P) (PROGN (COND (G (REMOVE_ITEMS P G))) P)) 
(PUT 'MAKE_SPOLY_PAIR 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_SPOLY_PAIR 'DEFINED-ON-LINE '71) 
(PUT 'MAKE_SPOLY_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'MAKE_SPOLY_PAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_SPOLY_PAIR (F G)
    (COND ((PFORDP G F) (MAKE_SPOLY_PAIR G F))
          (T
           ((LAMBDA (L)
              (AND T (OR (CDR F) (CDR G))
                   (NOT (TRIVIALLCM L (XVAL F) (XVAL G)))
                   (NOT (XDEGREECHECK (MKNWEDGE L))) (LIST L 'SPOLY_PAIR F G)))
            (XLCM (XVAL F) (XVAL G)))))) 
(PUT 'TRIVIALLCM 'NUMBER-OF-ARGS 3) 
(PUT 'TRIVIALLCM 'DEFINED-ON-LINE '84) 
(PUT 'TRIVIALLCM 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'TRIVIALLCM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIVIALLCM (L P Q) (EQUAL (XDIV P L) Q)) 
(PUT 'XDEGREECHECK 'NUMBER-OF-ARGS 1) 
(PUT 'XDEGREECHECK 'DEFINED-ON-LINE '90) 
(PUT 'XDEGREECHECK 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'XDEGREECHECK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XDEGREECHECK (U) (AND XTRUNCATE* (GREATERP (XDEGREE U) XTRUNCATE*))) 
(PUT 'MAKE_WEDGE_PAIR 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_WEDGE_PAIR 'DEFINED-ON-LINE '97) 
(PUT 'MAKE_WEDGE_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'MAKE_WEDGE_PAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_WEDGE_PAIR (X F)
    ((LAMBDA (L)
       (AND (OR (AND *TWOSIDED (NOT XTRUNCATE*)) (MEMQ X (XVAL F)))
            (NOT (OVERALL_FACTOR X F)) (NOT (XDEGREECHECK (MKNWEDGE L)))
            (LIST L 'WEDGE_PAIR (CONS (CONS X (CONS 1 1)) NIL) F)))
     (XLCM (LIST X X) (XVAL F)))) 
(PUT 'OVERALL_FACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'OVERALL_FACTOR 'DEFINED-ON-LINE '108) 
(PUT 'OVERALL_FACTOR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'OVERALL_FACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OVERALL_FACTOR (X F)
    (OR (NULL F) (AND (MEMQ X (XVAL F)) (OVERALL_FACTOR X (CDR F))))) 
(PUT 'MAKE_XCOMM_PAIR 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_XCOMM_PAIR 'DEFINED-ON-LINE '113) 
(PUT 'MAKE_XCOMM_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'MAKE_XCOMM_PAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_XCOMM_PAIR (X F)
    (AND *TWOSIDED (NOT XTRUNCATE*)
         (LIST (XVAL F) 'XCOMM_PAIR (CONS (CONS X (CONS 1 1)) NIL) F))) 
(PUT 'CRITICAL_ELEMENT 'NUMBER-OF-ARGS 1) 
(PUT 'CRITICAL_ELEMENT 'DEFINED-ON-LINE '122) 
(PUT 'CRITICAL_ELEMENT 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'CRITICAL_ELEMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CRITICAL_ELEMENT (PR) (APPLY1 (CADR PR) PR)) 
(PUT 'SPOLY_PAIR 'NUMBER-OF-ARGS 1) 
(PUT 'SPOLY_PAIR 'DEFINED-ON-LINE '128) 
(PUT 'SPOLY_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'SPOLY_PAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPOLY_PAIR (PR)
    (PROG (L F G)
      (SETQ F (CADDR PR))
      (SETQ G (CADDR (CDR PR)))
      (SETQ L (CAR PR))
      (SETQ F
              (WEDGEPF
               (CONS (CONS (MKNWEDGE (XDIV (XVAL F) L)) (CONS 1 1)) NIL) F))
      (SETQ G
              (WEDGEPF
               (CONS (CONS (MKNWEDGE (XDIV (XVAL G) L)) (CONS 1 1)) NIL) G))
      (RETURN
       (ADDPF (MULTPFSQ F (CDAR G))
              (MULTPFSQ (MULTPFSQ G (CDAR F)) (CONS (MINUS 1) 1)))))) 
(PUT 'WEDGE_PAIR 'NUMBER-OF-ARGS 1) 
(PUT 'WEDGE_PAIR 'DEFINED-ON-LINE '140) 
(PUT 'WEDGE_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'WEDGE_PAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WEDGE_PAIR (PR)
    (COND
     ((AND *TWOSIDED (NOT (XDIV (XVAL (CADDR PR)) (XVAL (CADDR (CDR PR))))))
      (WEDGEPF (WEDGEPF (CADDR PR) (CADDR (CDR PR))) (CADDR PR)))
     (T (WEDGEPF (CADDR PR) (CADDR (CDR PR)))))) 
(PUT 'XCOMM_PAIR 'NUMBER-OF-ARGS 1) 
(PUT 'XCOMM_PAIR 'DEFINED-ON-LINE '148) 
(PUT 'XCOMM_PAIR 'DEFINED-IN-FILE 'XIDEAL/XCRIT.RED) 
(PUT 'XCOMM_PAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XCOMM_PAIR (PR)
    (ADDPF (WEDGEPF (CADDR PR) (CADDR (CDR PR)))
           (COND
            ((EVENP (XDEGREEMON (XVAL (CADDR (CDR PR)))))
             (WEDGEPF (CADDR (CDR PR))
              (MULTPFSQ (CADDR PR) (CONS (MINUS 1) 1))))
            (T (WEDGEPF (CADDR (CDR PR)) (CADDR PR)))))) 
(ENDMODULE) 
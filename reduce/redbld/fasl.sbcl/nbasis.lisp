(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NBASIS)) 
(FLUID '(*TRA NESTEDSQRTS SQRT-INTVAR TAYLORASSLIST)) 
(EXPORTS (LIST 'NORMALBASIS)) 
(IMPORTS
 (LIST 'SUBSTITUTESQ 'TAYLORFORM 'PRINTSQ 'NEWPLACE 'SQRTSINSQ 'UNION 'SQRTSIGN
       'INTERR 'VECSORT 'MAPVEC 'FIRSTLINEARRELATION 'MKSP 'MULTSQ '*MULTSQ
       'ADDSQ 'REMOVECMSQ 'ANTISUBS 'INVOLVESQ)) 
(PUT 'NORMALBASIS 'NUMBER-OF-ARGS 3) 
(PUT 'NORMALBASIS 'DEFINED-ON-LINE '38) 
(PUT 'NORMALBASIS 'DEFINED-IN-FILE 'ALGINT/NBASIS.RED) 
(PUT 'NORMALBASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NORMALBASIS (ZBASIS X INFDEGREE)
    (PROG (N NESTEDSQRTS SQRTS U V W LI M LAM I INF BASIS SAVE)
      (SETQ SAVE TAYLORASSLIST)
      (SETQ INF (LIST (LIST X 'QUOTIENT 1 X)))
      (SETQ N (UPBV ZBASIS))
      (SETQ BASIS (MKVECT N))
      (SETQ LAM (MKVECT N))
      (SETQ M (MKVECT N))
      (GO A)
     SQUARE
      (SETQ SQRTS NIL)
      (SETQ INF (APPEND INF (LIST (LIST X 'EXPT X 2))))
     A
      (NEWPLACE INF)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ V (SUBSTITUTESQ (GETV ZBASIS I) INF))
         (PUTV BASIS I V)
         (SETQ SQRTS (UNION SQRTS (SQRTSINSQ V X))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       (*TRA
        (PROGN
         (PRINC "Normal integral basis reduction with the")
         (PRIN2T " following sqrts lying over infinity:")
         (SUPERPRINT SQRTS))))
      (COND ((MEMBER (LIST 'SQRT X) SQRTS) (GO SQUARE)))
      (SETQ SQRTS (SQRTSIGN SQRTS X))
      (COND
       ((NEQ (IADD1 N) (LENGTH SQRTS))
        (INTERR "Length mismatch in normalbasis")))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ V (CL8ROWEVAL (GETV BASIS I) SQRTS))
         (PUTV M I (CDR V))
         (PUTV LAM I (CAR V)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
     REDUCTIONLOOP
      (VECSORT LAM (LIST BASIS M))
      (COND
       (*TRA
        (PROGN
         (PRIN2T "Matrix before a reduction step at infinity is:")
         (MAPVEC M (FUNCTION PRIN2T)))))
      (SETQ V (FIRSTLINEARRELATION M (IADD1 N)))
      (COND ((NULL V) (GO RET)))
      (SETQ I N)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NULL (CAR (GETV V I)))) (RETURN NIL)))
        (SETQ I (ISUB1 I))
        (GO WHILELABEL))
      (SETQ LI (GETV LAM I))
      (SETQ W (CONS NIL 1))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((MINUSP (DIFFERENCE I J)) (RETURN NIL)))
        (SETQ W
                (*ADDSQ W
                        (*MULTSQ (GETV BASIS J)
                                 (MULTSQ (GETV V J)
                                         (CONS 1
                                               (*FMKSP X
                                                (PLUS (MINUS LI)
                                                      (GETV LAM J))))))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (COND
       (*TRA
        (PROGN
         (PRINC "Element ")
         (PRINC I)
         (PRIN2T " replaced by the function printed below:"))))
      (SETQ W (REMOVECMSQ W))
      (PUTV BASIS I W)
      (SETQ W (CL8ROWEVAL W SQRTS))
      (COND ((LEQ (CAR W) LI) (INTERR "Normal basis reduction did not work")))
      (PUTV LAM I (CAR W))
      (PUTV M I (CDR W))
      (GO REDUCTIONLOOP)
     RET
      (NEWPLACE (LIST (CONS X X)))
      (SETQ U (CONS 1 (LIST (CONS (GETPOWER (FKERN X) 1) 1))))
      (SETQ INF (ANTISUBS INF X))
      (SETQ U (SUBSTITUTESQ U INF))
      (SETQ M NIL)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG ()
          (SETQ V (DIFFERENCE (GETV LAM I) INFDEGREE))
          (COND ((LESSP V 0) (GO NEXT)))
          (SETQ W (SUBSTITUTESQ (GETV BASIS I) INF))
          (PROG (J)
            (SETQ J 0)
           LAB
            (COND ((MINUSP (DIFFERENCE V J)) (RETURN NIL)))
            (PROGN
             (COND ((NOT (INVOLVESQ W SQRT-INTVAR)) (SETQ M (CONS W M))))
             (SETQ W (*MULTSQ W U)))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
         NEXT)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
     NIL
      (RETURN M))) 
(PUT '*FMKSP 'NUMBER-OF-ARGS 2) 
(PUT '*FMKSP 'DEFINED-ON-LINE '122) 
(PUT '*FMKSP 'DEFINED-IN-FILE 'ALGINT/NBASIS.RED) 
(PUT '*FMKSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *FMKSP (X I)
    (COND ((IEQUAL I 0) 1) (T (LIST (CONS (GETPOWER (FKERN X) I) 1))))) 
(PUT 'CL8ROWEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'CL8ROWEVAL 'DEFINED-ON-LINE '129) 
(PUT 'CL8ROWEVAL 'DEFINED-IN-FILE 'ALGINT/NBASIS.RED) 
(PUT 'CL8ROWEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL8ROWEVAL (BASISELEMENT SQRTS)
    (PROG (LAM ROW I V MINIMUM N)
      (SETQ N (ISUB1 (LENGTH SQRTS)))
      (SETQ LAM (MKVECT N))
      (SETQ ROW (MKVECT N))
      (SETQ I 0)
      (SETQ MINIMUM 1000000)
      (PROG ()
       WHILELABEL
        (COND ((NOT SQRTS) (RETURN NIL)))
        (PROGN
         (SETQ V (TAYLORFORM (SUBSTITUTESQ BASISELEMENT (CAR SQRTS))))
         (SETQ V (ASSOC (CAADR V) (CDDR V)))
         (PUTV ROW I (CDR V))
         (SETQ V (CAR V))
         (PUTV LAM I V)
         (COND ((LESSP V MINIMUM) (SETQ MINIMUM V)))
         (SETQ I (IADD1 I))
         (SETQ SQRTS (CDR SQRTS)))
        (GO WHILELABEL))
      (COND
       (*TRA
        (PROGN
         (PRINC "Evaluating ")
         (PRINTSQ BASISELEMENT)
         (PRIN2T LAM)
         (PRIN2T ROW))))
      (SETQ V 1000000)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ V (GETV LAM I))
         (COND ((GREATERP V MINIMUM) (PUTV ROW I (CONS NIL 1)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS MINIMUM ROW)))) 
(ENDMODULE) 
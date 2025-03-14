(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'POLYOP)) 
(FLUID '(*RATARG GDMODE*)) 
(PUT 'DEG 'NUMBER-OF-ARGS 2) 
(PUT 'DEG 'DEFINED-ON-LINE '41) 
(PUT 'DEG 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'DEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEG (U KERN)
    ((LAMBDA (DMODE*)
       (PROGN (SETQ U (SIMP* U)) (TSTPOLYARG2 U KERN) (NUMRDEG (CAR U) KERN)))
     GDMODE*)) 
(PUT 'TOTALDEG 'NUMBER-OF-ARGS 2) 
(PUT 'TOTALDEG 'DEFINED-ON-LINE '60) 
(PUT 'TOTALDEG 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'TOTALDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTALDEG (U KERNLIST)
    (PROG (N)
      (SETQ U (CAR (SIMP* U)))
      (SETQ KERNLIST (PREPSQ (SIMP* KERNLIST)))
      (COND ((EQCAR KERNLIST 'LIST) (SETQ KERNLIST (CDR KERNLIST)))
            (T (SETQ KERNLIST (LIST KERNLIST))))
      (SETQ N (TOTALDEG1 U KERNLIST 0))
      (RETURN N))) 
(PUT 'TOTALDEG1 'NUMBER-OF-ARGS 3) 
(PUT 'TOTALDEG1 'DEFINED-ON-LINE '71) 
(PUT 'TOTALDEG1 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'TOTALDEG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TOTALDEG1 (U KERNLIST ABOVE)
    (PROG (R)
      (SETQ R ABOVE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM U) (ATOM (CAR U))))) (RETURN NIL)))
        (PROGN
         (COND
          ((MEMBER (CAAAR U) KERNLIST)
           (SETQ R
                   (MAX2 R
                         (TOTALDEG1 (CDAR U) KERNLIST
                                    (PLUS ABOVE (CDAAR U))))))
          (T (SETQ R (MAX2 R (TOTALDEG1 (CDAR U) KERNLIST ABOVE)))))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'TSTPOLYARG2 'NUMBER-OF-ARGS 2) 
(PUT 'TSTPOLYARG2 'DEFINED-ON-LINE '83) 
(PUT 'TSTPOLYARG2 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'TSTPOLYARG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TSTPOLYARG2 (U KERN)
    (PROGN
     (PROG (J)
       (SETQ J (KERNELS (CAR U)))
      LAB
       (COND ((NULL J) (RETURN NIL)))
       ((LAMBDA (J)
          (COND
           ((AND (NEQ J KERN) (DEPENDS J KERN))
            (TYPERR (PREPSQ U) "polynomial"))))
        (CAR J))
       (SETQ J (CDR J))
       (GO LAB))
     (PROG (J)
       (SETQ J (KERNELS (CDR U)))
      LAB
       (COND ((NULL J) (RETURN NIL)))
       ((LAMBDA (J) (COND ((DEPENDS J KERN) (TYPERR (PREPSQ U) "polynomial"))))
        (CAR J))
       (SETQ J (CDR J))
       (GO LAB)))) 
(PUT 'NUMRDEG 'NUMBER-OF-ARGS 2) 
(PUT 'NUMRDEG 'DEFINED-ON-LINE '90) 
(PUT 'NUMRDEG 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'NUMRDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUMRDEG (U KERN)
    (PROG (X)
      (SETQ KERN (*A2K KERN))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN 0))
            ((EQ (CAAAR U) KERN) (RETURN (PREPF (CDAAR U)))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((NOT (EQ (CAAAR U) KERN)) (SETQ U 0)) (T (SETQ U (CDAAR U))))
      (SETKORDER X)
      (RETURN U))) 
(PUT 'LCOFEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LCOFEVAL 'DEFINED-ON-LINE '103) 
(PUT 'LCOFEVAL 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'LCOFEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LCOFEVAL (U)
    (PROG (KERN X Y)
      (COND
       ((OR (NULL U) (NULL (CDR U)) (NOT (NULL (CDDR U))))
        (RERROR 'POLY 280 "LCOF called with wrong number of arguments")))
      (SETQ KERN (*A2K (CADR U)))
      (SETQ U (SIMP* (CAR U)))
      (SETQ Y (CDR U))
      (TSTPOLYARG Y U)
      (SETQ U (CAR U))
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (RETURN (COND ((NULL U) 0) (T (*FF2A U Y)))))
       ((EQ (CAAAR U) KERN) (RETURN (*FF2A (CDAR U) Y))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((EQ (CAAAR U) KERN) (SETQ U (CDAR U))))
      (SETKORDER X)
      (RETURN (COND ((NULL U) 0) (T (*FF2A U Y)))))) 
(PUT 'LCOF 'PSOPFN 'LCOFEVAL) 
(PUT 'LCOF 'NUMBER-OF-ARGS 2) 
(PUT 'LCOF 'DEFINED-ON-LINE '127) 
(PUT 'LCOF 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'LCOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LCOF (U KERN)
    (PROG (X Y)
      (SETQ U (SIMP* U))
      (SETQ Y (CDR U))
      (TSTPOLYARG Y U)
      (SETQ U (CAR U))
      (SETQ KERN (*A2K KERN))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN 0))
            ((EQ (CAAAR U) KERN) (RETURN (*FF2A (CDAR U) Y))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((EQ (CAAAR U) KERN) (SETQ U (CDAR U))))
      (SETKORDER X)
      (RETURN (COND ((NULL U) 0) (T (*FF2A U Y)))))) 
(PUT 'LPOWER 'NUMBER-OF-ARGS 2) 
(PUT 'LPOWER 'DEFINED-ON-LINE '143) 
(PUT 'LPOWER 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'LPOWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPOWER (U KERN)
    (PROG (X Y)
      (SETQ U (SIMP* U))
      (SETQ Y (CDR U))
      (TSTPOLYARG Y U)
      (SETQ U (CAR U))
      (SETQ KERN (*A2K KERN))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (*FF2A 1 Y)))
            ((EQ (CAAAR U) KERN)
             (RETURN (*FF2A (CONS (CONS (CAAR U) 1) NIL) Y))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((EQ (CAAAR U) KERN) (SETQ U (CONS (CONS (CAAR U) 1) NIL)))
            (T (SETQ U 1)))
      (SETKORDER X)
      (RETURN (*FF2A U Y)))) 
(PUT 'LTERM 'NUMBER-OF-ARGS 2) 
(PUT 'LTERM 'DEFINED-ON-LINE '160) 
(PUT 'LTERM 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'LTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LTERM (U KERN)
    (PROG (X Y)
      (SETQ U (SIMP* U))
      (SETQ Y (CDR U))
      (TSTPOLYARG Y U)
      (SETQ U (CAR U))
      (SETQ KERN (*A2K KERN))
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (RETURN (COND ((NULL U) 0) (T (*FF2A U Y)))))
       ((EQ (CAAAR U) KERN) (RETURN (*FF2A (CONS (CAR U) NIL) Y))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((EQ (CAAAR U) KERN) (SETQ U (CONS (CAR U) NIL))))
      (SETKORDER X)
      (SETQ U (REORDER U))
      (RETURN (*FF2A U Y)))) 
(PUT 'MAINVAR 'NUMBER-OF-ARGS 1) 
(PUT 'MAINVAR 'DEFINED-ON-LINE '181) 
(PUT 'MAINVAR 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'MAINVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAINVAR (U)
    (COND
     (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ U (CAR (SIMP* U)))) 0)
     (T (SFCHK (SETQ U (CAAAR U)))))) 
(PUT 'SFCHK 'NUMBER-OF-ARGS 1) 
(PUT 'SFCHK 'DEFINED-ON-LINE '185) 
(PUT 'SFCHK 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'SFCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFCHK (U) (COND ((SFP U) (PREPF U)) (T U))) 
(PUT 'REDUCT 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCT 'DEFINED-ON-LINE '187) 
(PUT 'REDUCT 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'REDUCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCT (U KERN)
    (PROG (X Y)
      (SETQ U (SIMP* U))
      (SETQ Y (CDR U))
      (TSTPOLYARG Y U)
      (SETQ U (CAR U))
      (SETQ KERN (*A2K KERN))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN 0))
            ((EQ (CAAAR U) KERN) (RETURN (*FF2A (CDR U) Y))))
      (SETQ X (UPDKORDER KERN))
      (SETQ U (REORDER U))
      (COND ((EQ (CAAAR U) KERN) (SETQ U (CDR U))) (T (SETQ U NIL)))
      (SETKORDER X)
      (SETQ U (REORDER U))
      (RETURN (*FF2A U Y)))) 
(PUT 'TSTPOLYARG 'NUMBER-OF-ARGS 2) 
(PUT 'TSTPOLYARG 'DEFINED-ON-LINE '206) 
(PUT 'TSTPOLYARG 'DEFINED-IN-FILE 'POLY/POLYOP.RED) 
(PUT 'TSTPOLYARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TSTPOLYARG (Y U)
    (AND (NULL *RATARG) (NEQ Y 1) (TYPERR (PREPSQ U) "polynomial"))) 
(FLAG '(DEG TOTALDEG LPOWER LTERM MAINVAR REDUCT) 'OPFN) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NAGELL)) 
(FLUID '(*TRA *TRMIN INTVAR)) 
(EXPORTS (LIST 'LUTZ-NAGELL)) 
(PUT 'LUTZ-NAGELL 'NUMBER-OF-ARGS 1) 
(PUT 'LUTZ-NAGELL 'DEFINED-ON-LINE '34) 
(PUT 'LUTZ-NAGELL 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'LUTZ-NAGELL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LUTZ-NAGELL (DIVISOR)
    (PROG (ANS PLACES MULTS SAVE*TRA)
      (PROG (U)
        (SETQ U DIVISOR)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (SETQ PLACES (CONS (CAR U) PLACES))
            (SETQ MULTS (CONS (CDR U) MULTS))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (SETQ ANS (LUTZ-NAGELL-2 PLACES MULTS))
      (COND ((EQ ANS 'INFINITE) (RETURN 'PROVABLY-IMPOSSIBLE)))
      (SETQ SAVE*TRA *TRA)
      (COND (*TRMIN (SETQ *TRA NIL)))
      (SETQ ANS (COATES-MULTIPLE PLACES MULTS ANS))
      (SETQ *TRA SAVE*TRA)
      (RETURN ANS))) 
(PUT 'LUTZ-NAGELL-2 'NUMBER-OF-ARGS 2) 
(PUT 'LUTZ-NAGELL-2 'DEFINED-ON-LINE '52) 
(PUT 'LUTZ-NAGELL-2 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'LUTZ-NAGELL-2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LUTZ-NAGELL-2 (PLACES MULTS)
    (PROG (WST X Y EQUATION POINT A)
      (SETQ WST (WEIERSTRASS-FORM (GETSQRTSFROMPLACES PLACES)))
      (SETQ X (CAR WST))
      (SETQ Y (CADR WST))
      (SETQ EQUATION (CADDR WST))
      (SETQ EQUATION (*Q2F (*MULTSQ EQUATION EQUATION)))
      (SETQ EQUATION (MAKEMAINVAR EQUATION INTVAR))
      (COND ((EQUAL (CDAAR EQUATION) 3) (SETQ EQUATION (CDR EQUATION)))
            (T (INTERR "Equation not of correct form")))
      (COND
       ((EQ (CAAAR EQUATION) INTVAR)
        (COND
         ((EQUAL (CDAAR EQUATION) 1)
          (PROGN
           (SETQ A (CONS (CDAR EQUATION) 1))
           (SETQ EQUATION (CDR EQUATION))))
         (T (INTERR "Equation should not have a x**2 term"))))
       (T (SETQ A (CONS NIL 1))))
      (SETQ EQUATION (CONS A (CONS EQUATION 1)))
      (SETQ PLACES
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U PLACES)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (WST-CONVERT U X Y)) (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (WST-CONVERT U X Y)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ POINT (ELLIPTIC-SUM PLACES MULTS EQUATION))
      (SETQ A (LUTZ-NAGELL-BOUND POINT EQUATION))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRINC "Point actually is of order ")
         (PROGN (PRIN2 A) (TERPRI) A))))
      (RETURN A))) 
(PUT 'WST-CONVERT 'NUMBER-OF-ARGS 3) 
(PUT 'WST-CONVERT 'DEFINED-ON-LINE '83) 
(PUT 'WST-CONVERT 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'WST-CONVERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WST-CONVERT (PLACE X Y)
    (PROG ()
      (SETQ X (SUBZERO (XSUBSTITUTESQ X PLACE) INTVAR))
      (SETQ Y (SUBZERO (XSUBSTITUTESQ Y PLACE) INTVAR))
      (RETURN (CONS X Y)))) 
(PUT 'ELLIPTIC-SUM 'NUMBER-OF-ARGS 3) 
(PUT 'ELLIPTIC-SUM 'DEFINED-ON-LINE '91) 
(PUT 'ELLIPTIC-SUM 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'ELLIPTIC-SUM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC-SUM (PLACES MULTS EQUATION)
    (PROG (POINT)
      (SETQ POINT (ELLIPTIC-MULTIPLY (CAR PLACES) (CAR MULTS) EQUATION))
      (SETQ PLACES (CDR PLACES))
      (SETQ MULTS (CDR MULTS))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (SETQ POINT
                 (ELLIPTIC-ADD POINT
                  (ELLIPTIC-MULTIPLY (CAR PLACES) (CAR MULTS) EQUATION)
                  EQUATION))
         (SETQ PLACES (CDR PLACES))
         (SETQ MULTS (CDR MULTS)))
        (GO WHILELABEL))
      (RETURN POINT))) 
(PUT 'ELLIPTIC-MULTIPLY 'NUMBER-OF-ARGS 3) 
(PUT 'ELLIPTIC-MULTIPLY 'DEFINED-ON-LINE '108) 
(PUT 'ELLIPTIC-MULTIPLY 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'ELLIPTIC-MULTIPLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC-MULTIPLY (POINT N EQUATION)
    (COND
     ((LESSP N 0)
      (ELLIPTIC-MULTIPLY (CONS (CAR POINT) (NEGSQ (CDR POINT))) (MINUS N)
       EQUATION))
     ((EQUAL N 0) (INTERR "N=0 in elliptic!-multiply")) ((EQUAL N 1) POINT)
     (T
      (PROG (Q R)
        (SETQ Q (DIVIDE N 2))
        (SETQ R (CDR Q))
        (SETQ Q (CAR Q))
        (SETQ Q
                (ELLIPTIC-MULTIPLY (ELLIPTIC-ADD POINT POINT EQUATION) Q
                 EQUATION))
        (COND ((EQUAL R 0) (RETURN Q))
              (T (RETURN (ELLIPTIC-ADD POINT Q EQUATION)))))))) 
(PUT 'ELLIPTIC-ADD 'NUMBER-OF-ARGS 3) 
(PUT 'ELLIPTIC-ADD 'DEFINED-ON-LINE '130) 
(PUT 'ELLIPTIC-ADD 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'ELLIPTIC-ADD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC-ADD (P1 P2 EQUATION)
    (PROG (X1 X2 Y1 Y2 X3 Y3 INF A B LHS RHS)
      (SETQ A (CAR EQUATION))
      (SETQ B (CDR EQUATION))
      (SETQ INF (CONS (LIST (CONS (GETPOWER (FKERN 'INFINITE) 1) 1)) 1))
      (SETQ X1 (CAR P1))
      (SETQ Y1 (CDR P1))
      (SETQ X2 (CAR P2))
      (SETQ Y2 (CDR P2))
      (COND
       ((EQUAL X1 X2)
        (COND
         ((EQUAL Y1 Y2)
          (PROGN
           (SETQ X3
                   (MULTSQ (CONS 4 1)
                           (*ADDSQ B (*MULTSQ X1 (*ADDSQ A (*EXPTSQ X1 2))))))
           (COND ((NULL (CAR X3)) (RETURN (CONS INF INF))))
           (SETQ X3
                   (*MULTSQ
                    (*ADDSQ (*ADDSQ (*MULTSQ A A) (*EXPTSQ X1 4))
                            (*ADDSQ (MULTSQ (CONS (MINUS 8) 1) (*MULTSQ X1 B))
                                    (*MULTSQ (*MULTSQ X1 X1)
                                             (MULTSQ (CONS (MINUS 2) 1) A))))
                    (*INVSQ X3)))
           (SETQ Y3
                   (*ADDSQ Y1
                           (*MULTSQ
                            (*MULTSQ (*ADDSQ X3 (NEGSQ X1))
                                     (*ADDSQ A
                                             (MULTSQ (CONS 3 1)
                                                     (*MULTSQ X1 X1))))
                            (*INVSQ (MULTSQ (CONS 2 1) Y1)))))))
         (T (SETQ X3 (SETQ Y3 INF)))))
       ((EQUAL X1 INF) (PROGN (SETQ X3 X2) (SETQ Y3 Y2)))
       ((EQUAL X2 INF) (PROGN (SETQ X3 X1) (SETQ Y3 Y1)))
       (T
        (PROGN
         (SETQ X3
                 (*MULTSQ
                  (*ADDSQ (*MULTSQ A (*ADDSQ X1 X2))
                          (*ADDSQ (MULTSQ (CONS 2 1) B)
                                  (*ADDSQ
                                   (*MULTSQ (*MULTSQ X1 X2) (*ADDSQ X1 X2))
                                   (MULTSQ (CONS (MINUS 2) 1)
                                           (*MULTSQ Y1 Y2)))))
                  (*INVSQ (*EXPTSQ (*ADDSQ X1 (NEGSQ X2)) 2))))
         (SETQ Y3
                 (*MULTSQ
                  (*ADDSQ (*MULTSQ (*ADDSQ Y2 (NEGSQ Y1)) X3)
                          (*ADDSQ (*MULTSQ X2 Y1) (*MULTSQ X1 (NEGSQ Y2))))
                  (*INVSQ (*ADDSQ X1 (NEGSQ X2))))))))
      (COND ((EQUAL X3 INF) (RETURN (CONS X3 Y3))))
      (SETQ LHS (*MULTSQ Y3 Y3))
      (SETQ RHS (*ADDSQ B (*MULTSQ X3 (*ADDSQ A (*MULTSQ X3 X3)))))
      (COND
       ((CAR (*ADDSQ LHS (NEGSQ RHS)))
        (PROGN
         (PRIN2T "Point defined by X and Y as follows:")
         (PRINTSQ X3)
         (PRINTSQ Y3)
         (PRIN2T "on the curve defined by A and B as follows:")
         (PRINTSQ A)
         (PRINTSQ B)
         (PRIN2T "gives a consistency check between:")
         (PRINTSQ LHS)
         (PRINTSQ RHS)
         (INTERR "Consistency check failed in elliptic!-add"))))
      (RETURN (CONS X3 Y3)))) 
(PUT 'INFINITEP 'NUMBER-OF-ARGS 1) 
(PUT 'INFINITEP 'DEFINED-ON-LINE '201) 
(PUT 'INFINITEP 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'INFINITEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INFINITEP (U) (AND (KERNP U) (EQ (CAAAR (CAR U)) 'INFINITE))) 
(PUT 'LUTZ-NAGELL-BOUND 'NUMBER-OF-ARGS 2) 
(PUT 'LUTZ-NAGELL-BOUND 'DEFINED-ON-LINE '204) 
(PUT 'LUTZ-NAGELL-BOUND 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'LUTZ-NAGELL-BOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LUTZ-NAGELL-BOUND (POINT EQUATION)
    (PROG (X Y A B LUTZ-ALIST N POINT2 P L ANS)
      (SETQ X (CAR POINT))
      (SETQ Y (CDR POINT))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2 "Point to have torsion investigated is")
          (TERPRI)
          "Point to have torsion investigated is")
         (PRINTSQ X)
         (PRINTSQ Y))))
      (SETQ A (CAR EQUATION))
      (SETQ B (CDR EQUATION))
      (COND
       ((NEQ (CDR Y) 1)
        (PROGN
         (SETQ L (CDR Y))
         (SETQ Y (*MULTSQ Y (CONS (*EXPTF L 3) 1)))
         (SETQ X (*MULTSQ X (CONS (*EXPTF L 2) 1)))
         (SETQ A (*MULTSQ A (CONS (*EXPTF L 4) 1)))
         (SETQ B (*MULTSQ B (CONS (*EXPTF L 6) 1))))))
      (COND
       ((NEQ (CDR X) 1)
        (PROGN
         (SETQ L (CDR X))
         (SETQ Y (*MULTSQ Y (CONS (*EXPTF L 3) 1)))
         (SETQ X (*MULTSQ X (CONS (*EXPTF L 2) 1)))
         (SETQ A (*MULTSQ A (CONS (*EXPTF L 4) 1)))
         (SETQ B (*MULTSQ B (CONS (*EXPTF L 6) 1))))))
      (SETQ LUTZ-ALIST (LIST (CONS X (CONS Y 0))))
      (COND
       ((AND (NEQ X (CAR POINT)) (OR *TRA *TRMIN))
        (PROGN
         (PROGN
          (PRIN2 "Point made integral as ")
          (TERPRI)
          "Point made integral as ")
         (PRINTSQ X)
         (PRINTSQ Y)
         (PROGN
          (PRIN2 "on the curve with coefficients")
          (TERPRI)
          "on the curve with coefficients")
         (PRINTSQ A)
         (PRINTSQ B))))
      (SETQ POINT (CONS X Y))
      (SETQ EQUATION (CONS A B))
      (SETQ N 0)
     LOOP
      (SETQ N (PLUS N 1))
      (SETQ POINT2 (ELLIPTIC-MULTIPLY (CONS X Y) 2 EQUATION))
      (SETQ X (CAR POINT2))
      (SETQ Y (CDR POINT2))
      (COND ((INFINITEP X) (RETURN (EXPT 2 N))))
      (COND ((NEQ (CDR X) 1) (GO SPECIAL-DENR)))
      (COND
       ((SETQ A (ASSOC X LUTZ-ALIST))
        (COND
         ((EQUAL Y (CADR A))
          (RETURN
           (SETQ ANS
                   (LUTZ-REDUCE POINT EQUATION
                    (DIFFERENCE (EXPT 2 N) (EXPT 2 (CDDR A)))))))
         ((NULL (CAR (*ADDSQ Y (CADR A))))
          (RETURN
           (SETQ ANS
                   (LUTZ-REDUCE POINT EQUATION
                    (PLUS (EXPT 2 N) (EXPT 2 (CDDR A)))))))
         (T (INTERR "Cannot have 3 points here")))))
      (SETQ LUTZ-ALIST (CONS (CONS X (CONS Y N)) LUTZ-ALIST))
      (COND (ANS (RETURN ANS)))
      (GO LOOP)
     SPECIAL-DENR
      (SETQ P (CDR X))
      (COND ((NOT (PRIMEP P)) (RETURN 'INFINITE)))
      (SETQ N 1)
      (SETQ N 1)
     LOOP2
      (SETQ POINT (ELLIPTIC-MULTIPLY POINT P EQUATION))
      (SETQ N (TIMES N P))
      (COND ((INFINITEP (CAR POINT)) (RETURN N)))
      (COND (((LAMBDA (*EXP) (QUOTF1 P (CDR (CAR POINT)))) T) (GO LOOP2)))
      (RETURN 'INFINITE))) 
(PUT 'LUTZ-REDUCE 'NUMBER-OF-ARGS 3) 
(PUT 'LUTZ-REDUCE 'DEFINED-ON-LINE '280) 
(PUT 'LUTZ-REDUCE 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'LUTZ-REDUCE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LUTZ-REDUCE (POINT EQUATION POWER)
    (PROG (N)
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRINC "Point is of order dividing ")
         (PROGN (PRIN2 POWER) (TERPRI) POWER))))
      (SETQ N 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EVENP POWER)) (RETURN NIL)))
        (PROGN
         (SETQ POWER (QUOTIENT POWER 2))
         (SETQ N (TIMES N 2))
         (SETQ POINT (ELLIPTIC-ADD POINT POINT EQUATION)))
        (GO WHILELABEL))
      (COND ((EQUAL POWER 1) (RETURN N)))
      (COND ((PRIMEP POWER) (RETURN (TIMES N POWER))))
      (RETURN (TIMES N (LUTZ-REDUCE2 POINT EQUATION POWER 3))))) 
(PUT 'LUTZ-REDUCE2 'NUMBER-OF-ARGS 4) 
(PUT 'LUTZ-REDUCE2 'DEFINED-ON-LINE '301) 
(PUT 'LUTZ-REDUCE2 'DEFINED-IN-FILE 'ALGINT/NAGELL.RED) 
(PUT 'LUTZ-REDUCE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LUTZ-REDUCE2 (POINT EQUATION POWER PRIME)
    (COND ((EQUAL POWER 1) (COND ((INFINITEP (CAR POINT)) 1) (T NIL)))
          ((INFINITEP (CAR POINT)) POWER)
          (T
           (PROG (N PRIME2 U ANS)
             (SETQ N 0)
             (PROG ()
              WHILELABEL
               (COND ((NOT (EQUAL (CDR (DIVIDE POWER PRIME)) 0)) (RETURN NIL)))
               (PROGN (SETQ N (PLUS N 1)) (SETQ POWER (QUOTIENT POWER PRIME)))
               (GO WHILELABEL))
             (SETQ PRIME2 (NEXTPRIME PRIME))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (PROGN
                (SETQ U (LUTZ-REDUCE2 POINT EQUATION POWER PRIME2))
                (COND
                 (U (PROGN (SETQ ANS (TIMES U (EXPT PRIME I))) (SETQ I N)))
                 (T
                  (PROGN
                   (SETQ POWER (TIMES POWER PRIME))
                   (SETQ POINT (ELLIPTIC-MULTIPLY POINT PRIME EQUATION))))))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (COND (ANS (RETURN ANS)) (T (RETURN NIL))))))) 
(ENDMODULE) 
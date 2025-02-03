(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPECFAC)) 
(FLUID '(*KEEPSQRTS *SUB2 *SURDS KNOWNDISCRIMSIGN KORD* ZLIST)) 
(EXPORTS (LIST 'CUBICF 'QUADRATICF 'QUARTICF)) 
(PUT 'COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'COEFFS 'DEFINED-ON-LINE '38) 
(PUT 'COEFFS 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COEFFS (POL)
    (PROG (DEGREE DEG1 COFS MV)
      (SETQ DEGREE 0)
      (SETQ DEG1 0)
      (SETQ MV (CAAAR POL))
      (SETQ DEGREE (CDAAR POL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM POL) (ATOM (CAR POL)))) (EQ (CAAAR POL) MV)))
          (RETURN NIL)))
        (PROGN
         (SETQ DEG1 (CDAAR POL))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((MINUSP (DIFFERENCE (DIFFERENCE (DIFFERENCE DEGREE DEG1) 1) I))
             (RETURN NIL)))
           (SETQ COFS (CONS NIL COFS))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ COFS (CONS (CDAR POL) COFS))
         (SETQ POL (CDR POL))
         (SETQ DEGREE DEG1))
        (GO WHILELABEL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE DEGREE 1) I)) (RETURN NIL)))
        (SETQ COFS (CONS NIL COFS))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (REVERSIP (CONS POL COFS))))) 
(PUT 'SHIFT-POL 'NUMBER-OF-ARGS 1) 
(PUT 'SHIFT-POL 'DEFINED-ON-LINE '54) 
(PUT 'SHIFT-POL 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'SHIFT-POL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHIFT-POL (POL)
    (PROG (LC1 LD MV POL1 REDP SHIFT X)
      (SETQ MV (CAAAR POL))
      (SETQ LD (CDAAR POL))
      (SETQ REDP (CDR POL))
      (COND
       ((OR (OR (ATOM REDP) (ATOM (CAR REDP))) (NOT (EQ (CAAAR REDP) MV))
            (LESSP (CDAAR REDP) (DIFFERENCE LD 1)))
        (RETURN (LIST POL 1 (CONS NIL 1)))))
      (SETQ LC1 (CDAR POL))
      (SETQ X (CDAR REDP))
      (SETQ SHIFT (MULTSQ (CONS X 1) (INVSQ (CONS (MULTD LD LC1) 1))))
      (SETQ POL1
              (SUBF1 POL
                     (LIST
                      (CONS MV
                            (MK*SQ
                             (ADDSQ (CONS (LIST (CONS (CONS MV 1) 1)) 1)
                                    (NEGSQ SHIFT)))))))
      (RETURN (LIST (CAR POL1) (CDR POL1) SHIFT)))) 
(PUT 'QUADRATICF* 'NUMBER-OF-ARGS 2) 
(PUT 'QUADRATICF* 'DEFINED-ON-LINE '71) 
(PUT 'QUADRATICF* 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'QUADRATICF* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUADRATICF* (POL VAR)
    (COND
     ((OR (ATOM POL) (ATOM (CAR POL))) (ERRACH "invalid quadratic to factr"))
     ((EQUAL (CAAAR POL) VAR) (QUADRATICF POL))
     (T
      (PROG (KORD W)
        (SETQ KORD KORD*)
        (SETQ KORD* (LIST VAR))
        (SETQ W (COEFFS (*Q2F (RESIMP (CONS POL 1)))))
        (SETQ KORD* KORD)
        (SETQ W (QUADRATICF1 (CAR W) (CADR W) (CADDR W)))
        (COND ((EQ W 'FAILED) (RETURN (LIST 1 POL))))
        (SETQ VAR (LIST (CONS (CONS VAR 1) 1)))
        (RETURN
         (LIST (COND ((NEQ (CAR W) 1) (MKRN 1 (CAR W))) (T 1))
               (ADDF
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF VAR (CADR W)))
                      (T (POLY-MULTF VAR (CADR W))))
                (CADDR W))
               (ADDF
                ((LAMBDA (G618)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF VAR G618))
                         (T (POLY-MULTF VAR G618))))
                 (CADDDR W))
                (CAR (CDDDDR W))))))))) 
(PUT 'QUADRATICF 'NUMBER-OF-ARGS 1) 
(PUT 'QUADRATICF 'DEFINED-ON-LINE '87) 
(PUT 'QUADRATICF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'QUADRATICF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUADRATICF (POL)
    ((LAMBDA (W)
       ((LAMBDA (X)
          (COND ((EQ X 'FAILED) (LIST 1 POL))
                ((NOT (OR (ATOM (CAR X)) (ATOM (CAR (CAR X))))) (LIST 1 POL))
                (T
                 ((LAMBDA (Y)
                    (LIST (COND ((NEQ (CAR X) 1) (MKRN 1 (CAR X))) (T 1))
                          (CONS (CONS Y (CADR X)) (CADDR X))
                          (CONS (CONS Y (CADDDR X)) (CAR (CDDDDR X)))))
                  (CONS (CAAAR POL) 1)))))
        (QUADRATICF1 (CAR W) (CADR W) (CADDR W))))
     (COEFFS POL))) 
(PUT 'QUADRATICF1 'NUMBER-OF-ARGS 3) 
(PUT 'QUADRATICF1 'DEFINED-ON-LINE '98) 
(PUT 'QUADRATICF1 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'QUADRATICF1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUADRATICF1 (A B C)
    (PROG (A1 DENOM DISCRIM W)
      (COND
       ((AND (NULL B) (MINUSF C) (NOT (MINUSF A)))
        (PROGN
         (SETQ A (ROOTXF A 2))
         (SETQ C (ROOTXF (NEGF C) 2))
         (RETURN
          (COND ((OR (EQ A 'FAILED) (EQ C 'FAILED)) 'FAILED)
                (T (LIST 1 A C A (NEGF C))))))))
      (SETQ DISCRIM
              (POWSUBSF
               (ADDF (EXPTF B 2)
                     (MULTD (MINUS 4)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF A C))
                                  (T (POLY-MULTF A C)))))))
      (COND ((NULL DISCRIM) NIL)
            (T
             (PROGN
              (COND
               (KNOWNDISCRIMSIGN
                (PROGN
                 (COND ((EQ KNOWNDISCRIMSIGN 'NEGATIVE) (RETURN 'FAILED)))))
               ((MINUSF DISCRIM) (RETURN 'FAILED)))
              (SETQ DISCRIM (ROOTXF DISCRIM 2))
              (COND ((EQUAL DISCRIM 'FAILED) (RETURN DISCRIM))))))
      (SETQ DENOM (MULTD 4 A))
      (SETQ A (SETQ A1 (MULTD 2 A)))
      (SETQ W (ADDF B DISCRIM))
      (SETQ C (ADDF B (NEGF DISCRIM)))
      (SETQ B W)
      (COND
       ((NEQ (SETQ W (GCDF A B)) 1)
        (PROGN
         (SETQ A1 (QUOTF-FAIL A W))
         (SETQ B (QUOTF-FAIL B W))
         (SETQ DENOM (QUOTF-FAIL DENOM W)))))
      (COND
       ((AND (NEQ (SETQ W (GCDF A DENOM)) 1) (SETQ W (GCDF C DENOM)))
        (PROGN
         (SETQ A (QUOTF-FAIL A W))
         (SETQ C (QUOTF-FAIL C W))
         (SETQ DENOM (QUOTF-FAIL DENOM W)))))
      (RETURN (LIST DENOM A1 B A C)))) 
(PUT 'ROOTXF 'NUMBER-OF-ARGS 2) 
(PUT 'ROOTXF 'DEFINED-ON-LINE '132) 
(PUT 'ROOTXF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'ROOTXF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROOTXF (U N)
    (PROG (X Y Z W)
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (RETURN
         (COND ((MINUSF U) 'FAILED)
               ((AND (ATOM U) (EQUAL (EXPT (SETQ Y (IROOTN U N)) N) U)) Y)
               ((AND (NOT (ATOM U)) (SETQ X (GET (CAR U) 'ROOTFN)))
                (APPLY2 X U N))
               ((AND *SURDS (NOT (MEMBER U ZLIST))) (NROOTN* U N))
               (T 'FAILED)))))
      (SETQ X (COMFAC U))
      (SETQ U ((LAMBDA (*EXP) (QUOTF1 U (COMFAC-TO-POLY X))) T))
      (SETQ Z 1)
      (COND
       ((CAR X)
        (COND
         ((EQUAL (CDR (SETQ Y (DIVIDE (CDAR X) N))) 0)
          (SETQ Z
                  ((LAMBDA (G544)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 Z))
                           (T (POLY-MULTF G544 Z))))
                   (LIST (CONS (CONS (CAAR X) (CAR Y)) 1)))))
         (*SURDS
          (PROGN
           (SETQ Z
                   ((LAMBDA (G621)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G621 Z))
                            (T (POLY-MULTF G621 Z))))
                    (MKROOTF (CAAR X) N (CDR Y))))
           (COND
            ((NEQ (CAR Y) 0)
             (SETQ Z
                     ((LAMBDA (G544)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 Z))
                              (T (POLY-MULTF G544 Z))))
                      (LIST (CONS (CONS (CAAR X) (CAR Y)) 1))))))))
         (T (RETURN 'FAILED)))))
      (SETQ X (CDR X))
      (COND
       ((OR (ATOM X) (ATOM (CAR X)))
        (COND ((MINUSF X) (RETURN 'FAILED))
              ((AND (FIXP X) (EQUAL (EXPT (SETQ Y (IROOTN X N)) N) X))
               (SETQ Z (MULTD Y Z)))
              ((AND *SURDS (FIXP X))
               (SETQ Z
                       ((LAMBDA (G625)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G625 Z))
                                (T (POLY-MULTF G625 Z))))
                        (NROOTN* X N))))
              ((AND (NOT (ATOM X)) (SETQ W (GET (CAR X) 'ROOTFN)))
               (APPLY2 W X N))
              (T (RETURN 'FAILED))))
       ((EQ (SETQ Y (ROOTXF X N)) 'FAILED) (RETURN Y))
       (T
        (SETQ Z
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y Z))
                      (T (POLY-MULTF Y Z))))))
      (COND ((EQUAL U 1) (RETURN Z)))
      (SETQ X (SQFRF U))
     C
      (COND ((NULL X) (RETURN Z))
            ((EQUAL (CDR (SETQ Y (DIVIDE (CDAR X) N))) 0)
             (PROGN
              (SETQ Z
                      ((LAMBDA (G627)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G627 Z))
                               (T (POLY-MULTF G627 Z))))
                       (EXPTF (CAAR X) (CAR Y))))
              (SETQ X (CDR X))))
            (*SURDS
             (PROGN
              (SETQ Z
                      ((LAMBDA (G631 G632)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G631 G632))
                               (T (POLY-MULTF G631 G632))))
                       (MKROOTF (PREPF (CAAR X)) N (CDR Y))
                       ((LAMBDA (G629)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G629 Z))
                                (T (POLY-MULTF G629 Z))))
                        (EXPTF (CAAR X) (CAR Y)))))
              (SETQ X (CDR X))))
            (T (RETURN 'FAILED)))
      (GO C))) 
(PUT 'MKROOTF 'NUMBER-OF-ARGS 3) 
(PUT 'MKROOTF 'DEFINED-ON-LINE '180) 
(PUT 'MKROOTF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'MKROOTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKROOTF (U M N)
    (COND
     ((OR (NEQ M 2) (NULL *KEEPSQRTS))
      (LIST (CONS (GETPOWER (FKERN (LIST 'EXPT U (LIST 'QUOTIENT 1 M))) N) 1)))
     ((NEQ N 1) (ERRACH 'MKROOTF)) (T (*Q2F (SIMPSQRT (LIST U)))))) 
(PUT 'NROOTN* 'NUMBER-OF-ARGS 2) 
(PUT 'NROOTN* 'DEFINED-ON-LINE '186) 
(PUT 'NROOTN* 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'NROOTN* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NROOTN* (U N)
    (PROG (X)
      (COND ((NULL U) (RETURN NIL)))
      (SETQ U (NROOTN U N))
      (SETQ X (CDR U))
      (SETQ U (CAR U))
      (COND ((EQUAL X 1) (RETURN X)))
      (SETQ X (MKROOTF (PREPF X) N 1))
      (RETURN
       (POWSUBSF
        (COND (*PHYSOP-LOADED (PHYSOP-MULTF U X)) (T (POLY-MULTF U X))))))) 
(PUT 'CUBICF 'NUMBER-OF-ARGS 1) 
(PUT 'CUBICF 'DEFINED-ON-LINE '198) 
(PUT 'CUBICF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'CUBICF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CUBICF (POL)
    (PROG (A A0 A1 B NEG P)
      (SETQ P (SHIFT-POL POL))
      (SETQ A (COEFFS (CAR P)))
      (COND ((CADR A) (RETURN (LIST 1 POL))) ((CADDR A) (RETURN (LIST 1 POL))))
      (SETQ A0 (CADDDR A))
      (SETQ A (CAR A))
      (COND ((MINUSF A0) (PROGN (SETQ NEG T) (SETQ A0 (NEGF A0)))))
      (COND
       ((OR (EQ (SETQ A (ROOTXF A 3)) 'FAILED)
            (EQ (SETQ A0 (ROOTXF A0 3)) 'FAILED))
        (RETURN (LIST 1 POL))))
      (COND (NEG (SETQ A0 (NEGF A0))))
      (SETQ A (CONS A 1))
      (SETQ A0 (CONS A0 1))
      (SETQ P (ADDSQ (CONS (LIST (CONS (CONS (CAAAR POL) 1) 1)) 1) (CADDR P)))
      (SETQ A1 (CAR (ADDSQ (MULTSQ A P) A0)))
      (SETQ B (MULTSQ A0 A0))
      (SETQ B (ADDSQ B (MULTSQ (NEGSQ (MULTSQ A A0)) P)))
      (SETQ B (CAR (ADDSQ B (MULTSQ (MULTSQ A A) (EXPTSQ P 2)))))
      (RETURN (ACONC* (QUADRATICF B) A1)))) 
(PUT 'POWSUBSF 'NUMBER-OF-ARGS 1) 
(PUT 'POWSUBSF 'DEFINED-ON-LINE '229) 
(PUT 'POWSUBSF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'POWSUBSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POWSUBSF (U)
    (PROG (*SUB2)
      (SETQ U (SUBS2Q (CONS U 1)))
      (COND
       ((NEQ (CDR U) 1)
        (PROGN
         (SETQ U (RATIONALIZESQ U))
         (COND ((NEQ (CDR U) 1) (ERRACH (LIST 'POWSUBSF U)))))))
      (RETURN (CAR U)))) 
(PUT 'QUARTICF 'NUMBER-OF-ARGS 1) 
(PUT 'QUARTICF 'DEFINED-ON-LINE '241) 
(PUT 'QUARTICF 'DEFINED-IN-FILE 'POLY/SPECFAC.RED) 
(PUT 'QUARTICF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUARTICF (POL)
    (PROG (*SUB2 A A2 A0 B DSC P P1 P2 Q SHIFT VAR)
      (SETQ VAR (CAAAR POL))
      (SETQ P (SHIFT-POL POL))
      (SETQ A (COEFFS (CAR P)))
      (SETQ SHIFT (CADDR P))
      (COND ((OR (CADR A) (CADDDR A)) (RETURN (LIST 1 POL))))
      (SETQ A2 (CDDR A))
      (SETQ A0 (CADDR A2))
      (SETQ A2 (CAR A2))
      (SETQ A (CAR A))
      (SETQ Q (QUADRATICF1 A A2 A0))
      (COND
       ((NOT (EQ Q 'FAILED))
        (PROGN
         (SETQ A2 (CAR Q))
         (SETQ Q (CDR Q))
         (SETQ A
                 (EXPTSQ
                  (ADDSQ (CONS (LIST (CONS (CONS (CAAAR POL) 1) 1)) 1) SHIFT)
                  2))
         (SETQ B
                 (CAR
                  (SUBS2Q
                   (MULTSQ
                    (ADDSQ (MULTSQ (CONS (CAR Q) 1) A) (CONS (CADR Q) 1))
                    (INVSQ (CONS (CADR P) 1))))))
         (SETQ A
                 (CAR
                  (SUBS2Q
                   (MULTSQ
                    (ADDSQ (MULTSQ (CONS (CADDR Q) 1) A) (CONS (CADDDR Q) 1))
                    (INVSQ (CONS (CADR P) 1))))))
         (SETQ A (QUADRATICF* A VAR))
         (SETQ B (QUADRATICF* B VAR))
         (RETURN
          (CONS
           ((LAMBDA (G634)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF A2 G634))
                    (T (POLY-MULTF A2 G634))))
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR A) (CAR B)))
                  (T (POLY-MULTF (CAR A) (CAR B)))))
           (NCONC* (CDR A) (CDR B))))))
       ((OR (NULL *SURDS) (NEQ (CDR SHIFT) 1)) (RETURN (LIST 1 POL))))
      (SETQ SHIFT (CAR SHIFT))
      (COND ((EQ KNOWNDISCRIMSIGN 'NEGATIVE) (GO COMPLEX)))
      (SETQ DSC
              (POWSUBSF
               (ADDF (EXPTF A2 2)
                     (MULTD (MINUS 4)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF A A0))
                                  (T (POLY-MULTF A A0)))))))
      (SETQ P2 (MINUSF A0))
      (COND ((AND (NOT P2) (MINUSF DSC)) (GO COMPLEX)))
      (SETQ P1 (OR (NOT A2) (MINUSF A2)))
      (COND ((NOT P1) (COND (P2 (SETQ P1 T)) (T (SETQ P2 T)))))
      (SETQ P1 (COND (P1 'POSITIVE) (T 'NEGATIVE)))
      (SETQ P2 (COND (P2 'NEGATIVE) (T 'POSITIVE)))
      (SETQ A (ROOTXF A 2))
      (COND ((EQ A 'FAILED) (RETURN (LIST 1 POL))))
      (SETQ DSC (ROOTXF DSC 2))
      (COND ((EQ DSC 'FAILED) (RETURN (LIST 1 POL))))
      (SETQ P (INVSQ (CONS (ADDF A A) 1)))
      (SETQ Q (MULTSQ (CONS (ADDF A2 (NEGF DSC)) 1) P))
      (SETQ P (MULTSQ (CONS (ADDF A2 DSC) 1) P))
      (SETQ B
              ((LAMBDA (G636)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G636))
                       (T (POLY-MULTF A G636))))
               (EXPTF (ADDF (LIST (CONS (CONS (CAAAR POL) 1) 1)) SHIFT) 2)))
      (SETQ A (POWSUBSF (ADDF B Q)))
      (SETQ B (POWSUBSF (ADDF B P)))
      (SETQ KNOWNDISCRIMSIGN P1)
      (SETQ A (QUADRATICF* A VAR))
      (SETQ KNOWNDISCRIMSIGN P2)
      (SETQ B (QUADRATICF* B VAR))
      (SETQ KNOWNDISCRIMSIGN NIL)
      (RETURN
       (CONS
        (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR A) (CAR B)))
              (T (POLY-MULTF (CAR A) (CAR B))))
        (NCONC* (CDR A) (CDR B))))
     COMPLEX
      (SETQ A (ROOTXF A 2))
      (COND ((EQ A 'FAILED) (RETURN (LIST 1 POL))))
      (SETQ A0 (ROOTXF A0 2))
      (COND ((EQ A0 'FAILED) (RETURN (LIST 1 POL))))
      (SETQ A2
              (POWSUBSF
               (ADDF
                ((LAMBDA (G638)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 G638))
                         (T (POLY-MULTF 2 G638))))
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF A A0))
                       (T (POLY-MULTF A A0))))
                (NEGF A2))))
      (SETQ A2 (ROOTXF A2 2))
      (COND ((EQ A2 'FAILED) (RETURN (LIST 1 POL))))
      (SETQ P (ADDF (LIST (CONS (CONS (CAAAR POL) 1) 1)) SHIFT))
      (SETQ Q
              (ADDF
               ((LAMBDA (G640)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G640))
                        (T (POLY-MULTF A G640))))
                (EXPTF P 2))
               A0))
      (SETQ P
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF A2 P))
                    (T (POLY-MULTF A2 P))))
      (SETQ A (POWSUBSF (ADDF Q P)))
      (SETQ B (POWSUBSF (ADDF Q (NEGF P))))
      (SETQ KNOWNDISCRIMSIGN 'NEGATIVE)
      (SETQ A (QUADRATICF* A VAR))
      (SETQ B (QUADRATICF* B VAR))
      (SETQ KNOWNDISCRIMSIGN NIL)
      (RETURN
       (CONS
        (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR A) (CAR B)))
              (T (POLY-MULTF (CAR A) (CAR B))))
        (NCONC* (CDR A) (CDR B)))))) 
(ENDMODULE) 
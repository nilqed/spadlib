(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CHARPOL)) 
(FLUID '(*EXP *GCD *PRFOURMAT)) 
(SWITCH (LIST 'PRFOURMAT)) 
(SETQ *PRFOURMAT T) 
(PUT 'COEFC1 'NUMBER-OF-ARGS 1) 
(PUT 'COEFC1 'DEFINED-ON-LINE '37) 
(PUT 'COEFC1 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'COEFC1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COEFC1 (UU)
    (PROG (LCO L U V A)
      (SETQ U (CAR UU))
      (SETQ V (CADR UU))
      (SETQ A (CADDR UU))
      (SETQ LCO (REVAL1 (LIST 'COEFF U V) NIL))
      (SETQ LCO (CDR LCO))
      (SETQ L (DIFFERENCE (LENGTH LCO) 1))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE L I)) (RETURN NIL)))
        (PROGN (SETEL (LIST A I) (CAR LCO)) (SETQ LCO (CDR LCO)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS L 1)))) 
(DEFLIST '((COEFC1 COEFC1)) 'SIMPFN) 
(GLOBAL '(CURSYM* COORDS* ICOORDS* UNVARS*)) 
(SETQ ICOORDS* '(I J K L M N I1 J1 K1 L1 M1 N1)) 
(FLAG '(TCON UNIT CHARMAT AMPMAT DENOTEMAT) 'MATFLG) 
(PUT 'UNIT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'CHARMAT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'AMPMAT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'DENOTEMAT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'UNIT 'NUMBER-OF-ARGS 1) 
(PUT 'UNIT 'DEFINED-ON-LINE '65) 
(PUT 'UNIT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'UNIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIT (U) (GENERATEIDENT (LENGTH (MATSM U)))) 
(PUT 'CHARMAT 'NUMBER-OF-ARGS 1) 
(PUT 'CHARMAT 'DEFINED-ON-LINE '68) 
(PUT 'CHARMAT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'CHARMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHARMAT (U) (MATSM (LIST 'DIFFERENCE (LIST 'TIMES 'LAM (LIST 'UNIT U)) U))) 
(PUT 'CHARPOL 'NUMBER-OF-ARGS 1) 
(PUT 'CHARPOL 'DEFINED-ON-LINE '71) 
(PUT 'CHARPOL 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'CHARPOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHARPOL (U)
    (PROG (X COMPLEXX)
      (SETQ COMPLEXX *COMPLEX)
      (AEVAL (ON (LIST 'COMPLEX)))
      (SETQ X (SIMP (LIST 'DET (LIST 'CHARMAT (CARX U 'CHARPOL)))))
      (COND ((NULL COMPLEXX) (AEVAL (OFF (LIST 'COMPLEX)))))
      (RETURN X))) 
(PUT 'CHARPOL 'SIMPFN 'CHARPOL) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(KORDER (LIST 'LAM)) 
(PUT 'RE 'NUMBER-OF-ARGS 1) 
(FLAG '(RE) 'OPFN) 
(PUT 'RE 'DEFINED-ON-LINE '86) 
(PUT 'RE 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'RE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RE (X) (LIST 'SUB (LIST 'EQUAL 'I 0) X)) 
(PUT 'IM 'NUMBER-OF-ARGS 1) 
(FLAG '(IM) 'OPFN) 
(PUT 'IM 'DEFINED-ON-LINE '89) 
(PUT 'IM 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'IM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IM (X) (LIST 'QUOTIENT (LIST 'DIFFERENCE X (LIST 'RE X)) 'I)) 
(PUT 'CON 'NUMBER-OF-ARGS 1) 
(FLAG '(CON) 'OPFN) 
(PUT 'CON 'DEFINED-ON-LINE '92) 
(PUT 'CON 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'CON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CON (X) (LIST 'SUB (LIST 'EQUAL 'I (LIST 'MINUS 'I)) X)) 
(PUT 'COMPLEXPOL 'NUMBER-OF-ARGS 1) 
(FLAG '(COMPLEXPOL) 'OPFN) 
(PUT 'COMPLEXPOL 'DEFINED-ON-LINE '95) 
(PUT 'COMPLEXPOL 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'COMPLEXPOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPLEXPOL (X)
    (PROG (Y)
      (SETQ Y (AEVAL (LIST 'TROOT1 X)))
      (RETURN
       (COND ((EVALEQUAL (AEVAL (LIST 'IM Y)) 0) (AEVAL Y))
             (T (AEVAL (LIST 'TIMES Y (LIST 'CON Y)))))))) 
(PUT 'TROOT1 'NUMBER-OF-ARGS 1) 
(FLAG '(TROOT1) 'OPFN) 
(PUT 'TROOT1 'DEFINED-ON-LINE '103) 
(PUT 'TROOT1 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'TROOT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TROOT1 (X)
    (PROG (Y)
      (SETQ Y (AEVAL X))
      (WHILE
       (AND
        (NOT
         (EVALEQUAL (AEVAL* (LIST 'SUB (LIST 'EQUAL 'LAM 0) Y)) (AEVAL* Y)))
        (EVALEQUAL (AEVAL* (LIST 'SUB (LIST 'EQUAL 'LAM 1) Y)) 0))
       (SETQ Y (AEVAL* (LIST 'QUOTIENT Y (LIST 'DIFFERENCE 'LAM 1)))))
      (SETQ X (AEVAL (LIST 'SUB (LIST 'EQUAL 'LAM 1) Y)))
      (COND
       ((NOT
         (OR (EVALNUMBERP (AEVAL Y))
             (AND (EVALNUMBERP (AEVAL (LIST 'NUM Y)))
                  (EVALNUMBERP (AEVAL (LIST 'DEN Y))))))
        (PROGN
         (ASSGNPRI (AEVAL " If  ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'RE X)) NIL NIL)
         (ASSGNPRI (AEVAL "  =  0  and  ") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'IM X)) NIL NIL)
         (ASSGNPRI (AEVAL "  =  0  , a root of the polynomial is equal to 1.")
                   NIL 'LAST))))
      (RETURN (AEVAL Y)))) 
(PUT 'HURW 'NUMBER-OF-ARGS 1) 
(FLAG '(HURW) 'OPFN) 
(PUT 'HURW 'DEFINED-ON-LINE '115) 
(PUT 'HURW 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'HURW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HURW (X)
    (LIST 'TIMES
          (LIST 'EXPT (LIST 'DIFFERENCE 'LAM 1) (LIST 'DEG (LIST 'NUM X) 'LAM))
          (LIST 'SUB
                (LIST 'EQUAL 'LAM
                      (LIST 'QUOTIENT (LIST 'PLUS 'LAM 1)
                            (LIST 'DIFFERENCE 'LAM 1)))
                X))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'UNFUNC 'NUMBER-OF-ARGS 1) 
(PUT 'UNFUNC 'DEFINED-ON-LINE '122) 
(PUT 'UNFUNC 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'UNFUNC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNFUNC (U)
    (PROGN
     (SETQ UNVARS* U)
     (PROG (A)
       (SETQ A U)
      LAB
       (COND ((NULL A) (RETURN NIL)))
       ((LAMBDA (A) (PUT A 'SIMPFN 'SIMPIDEN)) (CAR A))
       (SETQ A (CDR A))
       (GO LAB)))) 
(PUT 'UNFUNC 'STAT 'RLIS) 
(GLOBAL '(DENOTATION* DENOTID*)) 
(SETQ DENOTATION* NIL) 
(SETQ DENOTID* 'A) 
(PUT 'DENOTID 'NUMBER-OF-ARGS 1) 
(PUT 'DENOTID 'DEFINED-ON-LINE '132) 
(PUT 'DENOTID 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'DENOTID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DENOTID (U) (PROGN (SETQ DENOTID* (CAR U)) NIL)) 
(PUT 'DENOTID 'STAT 'RLIS) 
(PUT 'CLEARDENOT 'NUMBER-OF-ARGS 0) 
(PUT 'CLEARDENOT 'DEFINED-ON-LINE '137) 
(PUT 'CLEARDENOT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'CLEARDENOT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEARDENOT NIL (SETQ DENOTATION* NIL)) 
(PUT 'CLEARDENOT 'STAT 'ENDSTAT) 
(FLAG '(CLEARDENOT) 'EVAL) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(ARRAYFN 'ALGEBRAIC (LIST (LIST 'COFPOL* 20))) 
(PUT 'DENOTEPOL 'NUMBER-OF-ARGS 1) 
(FLAG '(DENOTEPOL) 'OPFN) 
(PUT 'DENOTEPOL 'DEFINED-ON-LINE '146) 
(PUT 'DENOTEPOL 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'DENOTEPOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DENOTEPOL (U)
    (PROG (NCO DCO)
      (SETQ DCO (AEVAL (LIST 'DEN U)))
      (SETQ U (AEVAL (LIST 'NUM U)))
      (SETQ NCO (AEVAL (LIST 'COEFC1 U 'LAM 'COFPOL*)))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NCO) J)) (RETURN NIL)))
        (SETEL (LIST 'COFPOL* J)
               (AEVAL* (LIST 'QUOTIENT (LIST 'COFPOL* J) DCO)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (AEVAL (LIST 'DENOTEAR NCO))
      (SETQ U
              (PROG (J FORALL-RESULT)
                (SETQ J 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NCO) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES (LIST 'EXPT 'LAM J)
                                      (LIST 'COFPOL* J)))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1)))
      (RETURN (AEVAL U)))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'DENOTEAR 'SIMPFN 'DENOTEAR) 
(PUT 'DENOTEAR 'NUMBER-OF-ARGS 1) 
(PUT 'DENOTEAR 'DEFINED-ON-LINE '162) 
(PUT 'DENOTEAR 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'DENOTEAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DENOTEAR (U)
    (PROG (NCO X)
      (SETQ NCO (CAR U))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE NCO I)) (RETURN NIL)))
        (PROGN
         (SETQ X (LIST 'COFPOL* I))
         (SETEL X (MK*SQ (DENOTE (GETEL X) 0 I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS NIL 1)))) 
(PUT 'DENOTEMAT 'NUMBER-OF-ARGS 1) 
(PUT 'DENOTEMAT 'DEFINED-ON-LINE '172) 
(PUT 'DENOTEMAT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'DENOTEMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DENOTEMAT (U)
    (PROG (I J X)
      (SETQ I 0)
      (SETQ X
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (MATSM U))
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (PROGN
                                     (SETQ I (PLUS I 1))
                                     (SETQ J 0)
                                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ B A)
                                       (COND ((NULL B) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (B)
                                                           (PROGN
                                                            (SETQ J (PLUS J 1))
                                                            (DENOTE (MK*SQ B) I
                                                             J)))
                                                         (CAR B))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ B (CDR B))
                                       (COND ((NULL B) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (PROGN
                                                    (SETQ J (PLUS J 1))
                                                    (DENOTE (MK*SQ B) I J)))
                                                 (CAR B))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (PROGN
                             (SETQ I (PLUS I 1))
                             (SETQ J 0)
                             (PROG (B FORALL-RESULT FORALL-ENDPTR)
                               (SETQ B A)
                               (COND ((NULL B) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (PROGN
                                                    (SETQ J (PLUS J 1))
                                                    (DENOTE (MK*SQ B) I J)))
                                                 (CAR B))
                                                NIL)))
                              LOOPLABEL
                               (SETQ B (CDR B))
                               (COND ((NULL B) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (B)
                                           (PROGN
                                            (SETQ J (PLUS J 1))
                                            (DENOTE (MK*SQ B) I J)))
                                         (CAR B))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN X))) 
(PUT 'DENOTE 'NUMBER-OF-ARGS 3) 
(PUT 'DENOTE 'DEFINED-ON-LINE '185) 
(PUT 'DENOTE 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'DENOTE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DENOTE (U I J)
    (PROG (REU IMU IREU IIMU EIJ FGCD)
      (COND ((ATOM U) (RETURN (SIMP U))))
      (SETQ FGCD *GCD)
      (SETQ *GCD T)
      (SETQ REU (SIMP* (LIST 'RE U)))
      (SETQ IMU (SIMP* (LIST 'IM U)))
      (SETQ *GCD FGCD)
      (SETQ EIJ (APPEND (EXPLODE I) (EXPLODE J)))
      (SETQ IREU
              (INTERN
               (COMPRESS (APPEND (APPEND (EXPLODE DENOTID*) '(R)) EIJ))))
      (SETQ IIMU
              (INTERN
               (COMPRESS (APPEND (APPEND (EXPLODE DENOTID*) '(I)) EIJ))))
      (COND ((CAR REU) (INSDENOT IREU REU)))
      (COND ((CAR IMU) (INSDENOT IIMU IMU)))
      (RETURN
       (SIMP
        (LIST 'PLUS (COND ((CAR REU) IREU) (T 0))
              (LIST 'TIMES 'I (COND ((CAR IMU) IIMU) (T 0)))))))) 
(PUT 'INSDENOT 'NUMBER-OF-ARGS 2) 
(PUT 'INSDENOT 'DEFINED-ON-LINE '209) 
(PUT 'INSDENOT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'INSDENOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSDENOT (IDEN U) (SETQ DENOTATION* (CONS (CONS U IDEN) DENOTATION*))) 
(PUT 'PRDENOT 'NUMBER-OF-ARGS 0) 
(PUT 'PRDENOT 'DEFINED-ON-LINE '212) 
(PUT 'PRDENOT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'PRDENOT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRDENOT NIL
    (PROG (A)
      (SETQ A (REVERSE DENOTATION*))
     LAB
      (COND ((NULL A) (RETURN NIL)))
      ((LAMBDA (A) (ASSGNPRI (LIST '*SQ (CAR A) T) (LIST (CDR A)) 'ONLY))
       (CAR A))
      (SETQ A (CDR A))
      (GO LAB))) 
(PUT 'PRDENOT 'STAT 'ENDSTAT) 
(FLAG '(PRDENOT) 'EVAL) 
(PUT 'AMPMAT 'NUMBER-OF-ARGS 1) 
(PUT 'AMPMAT 'DEFINED-ON-LINE '219) 
(PUT 'AMPMAT 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'AMPMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AMPMAT (U)
    (PROG (X I H1 H0 UN RH1 RH0 RU PH1 PH0 *EXP *GCD COMPLEXX)
      (SETQ COMPLEXX *COMPLEX)
      (SETQ *EXP T)
      (FOURIERSUBS)
      (SETQ U (CAR (MATSM U)))
      (SETQ X
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A COORDS*)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (COND ((EQUAL A 'T) 0)
                                          (T
                                           (LIST 'TIMES (TCAR (GET A 'INDEX))
                                                 (GET A 'WAVE)
                                                 (GET A 'STEP)))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (COND ((EQUAL A 'T) 0)
                                  (T
                                   (LIST 'TIMES (TCAR (GET A 'INDEX))
                                         (GET A 'WAVE) (GET A 'STEP)))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (LIST 'EXPP (CONS 'PLUS X)))
      (SETQ X (SIMP X))
      (SETQ U
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A U)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A) (RESIMP (MULTSQ A (INVSQ X))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A) (RESIMP (MULTSQ A (INVSQ X)))) (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GONSUBS)
      (AEVAL (ON (LIST 'COMPLEX)))
      (SETQ U
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A U)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (A) (RESIMP A)) (CAR A)) NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (RESIMP A)) (CAR A)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (REMFOURIER)
     A
      (COND ((NULL U) (GO D)))
      (SETQ RU (CAAR U))
      (SETQ UN UNVARS*)
      (SETQ I 1)
     B
      (COND (UN (GO C)))
      (SETQ RH1 (REVERSE RH1))
      (SETQ RH0 (REVERSE RH0))
      (SETQ H1 (CONS RH1 H1))
      (SETQ H0 (CONS RH0 H0))
      (SETQ RH0 (SETQ RH1 NIL))
      (SETQ U (CDR U))
      (GO A)
     C
      (SETQ RH1 (CONS (COEFCK RU (LIST 'U1* I)) RH1))
      (SETQ RH0 (CONS (NEGSQ (COEFCK RU (LIST 'U0* I))) RH0))
      (SETQ UN (CDR UN))
      (SETQ I (PLUS I 1))
      (GO B)
     D
      (SETQ H1 (REVERSE H1))
      (SETQ H0 (REVERSE H0))
      (COND
       (*PRFOURMAT
        (PROGN
         (SETQ PH1
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A H1)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A)
                                       (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ B A)
                                         (COND ((NULL B) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (B)
                                                             (PREPSQ* B))
                                                           (CAR B))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ B (CDR B))
                                         (COND
                                          ((NULL B) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (B) (PREPSQ* B))
                                                   (CAR B))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (A)
                               (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ B A)
                                 (COND ((NULL B) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (B) (PREPSQ* B))
                                                   (CAR B))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ B (CDR B))
                                 (COND ((NULL B) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (B) (PREPSQ* B)) (CAR B))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR A))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETMATPRI 'H1 (CONS NIL PH1))
         (SETQ PH1 NIL)
         (SETQ PH0
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A H0)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A)
                                       (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ B A)
                                         (COND ((NULL B) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (B)
                                                             (PREPSQ* B))
                                                           (CAR B))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ B (CDR B))
                                         (COND
                                          ((NULL B) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (B) (PREPSQ* B))
                                                   (CAR B))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (A)
                               (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ B A)
                                 (COND ((NULL B) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (B) (PREPSQ* B))
                                                   (CAR B))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ B (CDR B))
                                 (COND ((NULL B) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (B) (PREPSQ* B)) (CAR B))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR A))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETMATPRI 'H0 (CONS NIL PH0))
         (SETQ PH0 NIL))))
      (SETQ *GCD T)
      (SETQ X
              (COND
               ((EQUAL (LENGTH H1) 1)
                (LIST (LIST (MULTSQ (CAAR H0) (INVSQ (CAAR H1))))))
               (T (LNRSOLVE H1 H0))))
      (COND ((NULL COMPLEXX) (AEVAL (OFF (LIST 'COMPLEX)))))
      (RETURN X))) 
(PUT 'COEFCK 'NUMBER-OF-ARGS 2) 
(PUT 'COEFCK 'DEFINED-ON-LINE '273) 
(PUT 'COEFCK 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'COEFCK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFCK (X Y)
    (PROG (KY XS)
      (SETQ KY (*A2K Y))
      (SETQ XS (CAR (SUBF X (LIST (CONS KY 0)))))
      (SETQ XS (ADDF X (NEGF XS)))
      (COND ((NULL XS) (RETURN (CONS NIL 1))))
      (SETQ XS (QUOTF1 XS (LIST (CONS (CONS KY 1) 1))))
      (RETURN
       (COND
        ((NULL XS)
         (PROGN
          (MSGPRI "COEFCK failed on " (LIST 'SQ* (CONS X 1) NIL) " with " Y
                  'HOLD)
          (XREAD NIL)
          (CONS XS 1)))
        (T (CONS XS 1)))))) 
(PUT 'SIMPFOUR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPFOUR 'DEFINED-ON-LINE '290) 
(PUT 'SIMPFOUR 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'SIMPFOUR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPFOUR (U)
    (PROG (NRUNV X EX ARG MV COR INCR LCOR)
      (SETQ NRUNV (GET (CAR U) 'NRUNVAR))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (GO R)))
      (SETQ ARG (SIMP (CAR U)))
      (SETQ MV (CAAAR (CAR ARG)))
      (COND
       ((OR (NOT (ATOM MV)) (NOT (NUMBERP (CDR ARG))))
        (RETURN (MSGPRI "Bad index " (CAR U) NIL NIL 'HOLD))))
      (SETQ COR (TCAR (GET MV 'COORD)))
      (COND
       ((NOT (MEMBER COR COORDS*))
        (RETURN
         (MSGPRI "Term " (CAR U) " contains non-coordinate " MV 'HOLD))))
      (COND
       ((MEMBER COR LCOR)
        (RETURN
         (MSGPRI "Term " (CAR U) " means second appearance of coordinate " COR
                 'HOLD))))
      (COND
       ((AND (NOT (EQUAL COR 'T)) (GREATERP (CDR ARG) (GET COR 'MAXDEN)))
        (PUT COR 'MAXDEN (CDR ARG))))
      (SETQ LCOR (CONS COR LCOR))
      (SETQ INCR (ADDSQ ARG (NEGSQ (CONS (LIST (CONS (CONS MV 1) 1)) 1))))
      (COND
       ((NOT (FLAGP COR 'UNIFORM))
        (RETURN (LPRIE "Non-uniform grids not yet supported"))))
      (COND ((EQUAL COR 'T) (GO TI)))
      (SETQ EX (CONS (LIST 'TIMES (CAR U) (GET COR 'STEP) (GET COR 'WAVE)) EX))
      (GO A)
     TI
      (COND ((NULL (CAR INCR)) (SETQ X (LIST 'U0* NRUNV)))
            ((EQUAL INCR (CONS 1 1)) (SETQ X (LIST 'U1* NRUNV)))
            (T (RETURN (LPRIE "Scheme is not twostep in time"))))
      (GO A)
     R
      (PROG (A)
        (SETQ A (SETDIFF COORDS* LCOR))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND ((EQUAL A 'T) (SETQ X (LIST 'U0* NRUNV)))
                 (T
                  (SETQ EX
                          (CONS
                           (LIST 'TIMES (TCAR (GET A 'INDEX)) (GET A 'STEP)
                                 (GET A 'WAVE))
                           EX)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (RETURN (SIMP (LIST 'TIMES X (LIST 'EXPP (CONS 'PLUS EX))))))) 
(PUT 'FOURIERSUBS 'NUMBER-OF-ARGS 0) 
(PUT 'FOURIERSUBS 'DEFINED-ON-LINE '326) 
(PUT 'FOURIERSUBS 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'FOURIERSUBS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FOURIERSUBS NIL
    (PROG (X I)
      (PROG (A)
        (SETQ A '(EXPP U1* U0*))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (PUT A 'SIMPFN 'SIMPIDEN)) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ X UNVARS*)
      (SETQ I 1)
     A
      (COND ((NULL X) (GO B)))
      (PUT (CAR X) 'NRUNVAR I)
      (SETQ I (PLUS I 1))
      (SETQ X (CDR X))
      (GO A)
     B
      (FLAG UNVARS* 'FULL)
      (PROG (A)
        (SETQ A UNVARS*)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (PUT A 'SIMPFN 'SIMPFOUR)) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A COORDS*)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NOT (EQUAL A 'T))
             (PROGN
              (SETQ X (INTERN (COMPRESS (APPEND (EXPLODE 'H) (EXPLODE A)))))
              (PUT A 'STEP X)
              (COND ((NOT (FLAGP A 'UNIFORM)) (PUT X 'SIMPFN 'SIMPIDEN)))
              (SETQ X (INTERN (COMPRESS (APPEND (EXPLODE 'K) (EXPLODE A)))))
              (PUT A 'WAVE X)
              (SETQ X (INTERN (COMPRESS (APPEND (EXPLODE 'A) (EXPLODE A)))))
              (PUT A 'ANGLE X)
              (PUT A 'MAXDEN 1)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (AEVAL
       (FORALL
        (LIST '(Z Y V) 'T
              '(LET00
                '((EQUAL (EXPP (QUOTIENT (PLUS Z Y) V))
                         (TIMES (EXPP (QUOTIENT Z V)) (EXPP (QUOTIENT Y V))))
                  (EQUAL (EXPP (PLUS Z Y)) (TIMES (EXPP Z) (EXPP Y)))))))))) 
(PUT 'GONSUBS 'NUMBER-OF-ARGS 0) 
(PUT 'GONSUBS 'DEFINED-ON-LINE '354) 
(PUT 'GONSUBS 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'GONSUBS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GONSUBS NIL
    (PROG (XX)
      (AEVAL
       (FORALL
        (LIST '(Z Y V) 'T
              '(CLEAR
                (LIST (LIST 'EXPP (LIST 'QUOTIENT (LIST 'PLUS 'Z 'Y) 'V))
                      (LIST 'EXPP (LIST 'PLUS 'Z 'Y)))))))
      (PROG (A)
        (SETQ A COORDS*)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NOT (EQUAL A 'T))
             (PROGN
              (SETQ XX
                      (LIST 'QUOTIENT
                            (LIST 'TIMES (GET A 'MAXDEN) (GET A 'ANGLE))
                            (GET A 'STEP)))
              (SETK (GET A 'WAVE) (REVAL1 XX NIL))
              (SETQ XX (LIST 'TIMES (GET A 'WAVE) (GET A 'STEP)))
              (MATHPRINT
               (LIST 'SETQ (GET A 'ANGLE)
                     (COND ((EQUAL (GET A 'MAXDEN) 1) XX)
                           (T (LIST 'QUOTIENT XX (GET A 'MAXDEN))))))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (AEVAL
       (FORALL
        (LIST '(X) 'T
              '(LET00 '((EQUAL (EXPP X) (PLUS (COS X) (TIMES I (SIN X)))))))))
      (AEVAL
       (FORALL
        (LIST '(X N)
              '(AND (EVALNUMBERP (AEVAL 'N)) (EVALGREATERP (AEVAL 'N) 1))
              '(LET00
                '((EQUAL (SIN (TIMES N X))
                         (PLUS (TIMES (SIN X) (COS (TIMES (DIFFERENCE N 1) X)))
                               (TIMES (COS X)
                                      (SIN (TIMES (DIFFERENCE N 1) X)))))
                  (EQUAL (COS (TIMES N X))
                         (DIFFERENCE
                          (TIMES (COS X) (COS (TIMES (DIFFERENCE N 1) X)))
                          (TIMES (SIN X)
                                 (SIN (TIMES (DIFFERENCE N 1) X))))))))))
      (PROG (A)
        (SETQ A UNVARS*)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (PROGN (PUT A 'SIMPFN 'SIMPIDEN) (REMPROP A 'NRUNVAR)))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB)))) 
(PUT 'REMFOURIER 'NUMBER-OF-ARGS 0) 
(PUT 'REMFOURIER 'DEFINED-ON-LINE '384) 
(PUT 'REMFOURIER 'DEFINED-IN-FILE 'FIDE/CHARPOL.RED) 
(PUT 'REMFOURIER 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REMFOURIER NIL
    (PROGN
     (AEVAL (FORALL (LIST '(X) 'T '(CLEAR (LIST (LIST 'EXPP 'X))))))
     (AEVAL
      (FORALL
       (LIST '(X N) '(AND (EVALNUMBERP (AEVAL 'N)) (EVALGREATERP (AEVAL 'N) 1))
             '(CLEAR
               (LIST (LIST 'SIN (LIST 'TIMES 'N 'X))
                     (LIST 'COS (LIST 'TIMES 'N 'X)))))))
     (PROG (A)
       (SETQ A COORDS*)
      LAB
       (COND ((NULL A) (RETURN NIL)))
       ((LAMBDA (A)
          (COND
           ((NOT (EQUAL A 'T))
            (PROGN
             (REMPROP A 'STEP)
             (REMPROP (REMPROP A 'WAVE) 'AVALUE)
             (REMPROP A 'MAXDEN)))))
        (CAR A))
       (SETQ A (CDR A))
       (GO LAB)))) 
(FLAG '(NUMBERP) 'OPFN) 
(ENDMODULE) 
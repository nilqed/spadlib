(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FIXSUBF)) 
(FLUID '(*NOSUBS ASYMPLIS* DMODE* NCMP*)) 
(PUT 'ALGINT-SUBF 'NUMBER-OF-ARGS 2) 
(PUT 'ALGINT-SUBF 'DEFINED-ON-LINE '35) 
(PUT 'ALGINT-SUBF 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGINT-SUBF (A B) (ALGINT-SUBF1 A B)) 
(PUT 'ALGINT-SUBSQ 'NUMBER-OF-ARGS 2) 
(PUT 'ALGINT-SUBSQ 'DEFINED-ON-LINE '37) 
(PUT 'ALGINT-SUBSQ 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SUBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGINT-SUBSQ (U V)
    (*MULTSQ (ALGINT-SUBF (CAR U) V) (*INVSQ (ALGINT-SUBF (CDR U) V)))) 
(PUT 'ALGINT-SUBF1 'NUMBER-OF-ARGS 2) 
(PUT 'ALGINT-SUBF1 'DEFINED-ON-LINE '40) 
(PUT 'ALGINT-SUBF1 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SUBF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGINT-SUBF1 (U L)
    (COND
     ((OR (ATOM U) (ATOM (CAR U)))
      (COND ((ATOM U) (COND ((NULL DMODE*) (CONS U 1)) (T (SIMPATOM U))))
            ((EQ DMODE* (CAR U)) (*D2Q U)) (T (SIMP (PREPF U)))))
     (T
      (PROG (N KERN M W X XEXP Y Y1 Z)
        (SETQ N 0)
        (SETQ Z (CONS NIL 1))
       A0
        (SETQ KERN (CAAAR U))
        (COND ((SETQ M (ASSOC KERN ASYMPLIS*)) (SETQ M (CDR M))))
       A
        (COND ((OR (NULL U) (EQUAL (SETQ N (DEGR U KERN)) 0)) (GO B))
              ((OR (NULL M) (LESSP N M)) (SETQ Y (CONS (CAR U) Y))))
        (SETQ U (CDR U))
        (GO A)
       B
        (COND
         ((AND (NOT (ATOM KERN)) (NOT (ATOM (CAR KERN))))
          (SETQ KERN (PREPF KERN))))
        (COND ((NULL L) (SETQ XEXP (COND ((EQ KERN 'K*) 1) (T KERN))))
              ((AND (EQUAL (SETQ XEXP (ALGINT-SUBSUBLIS L KERN)) KERN)
                    (NOT (ASSOC KERN ASYMPLIS*)))
               (GO F)))
       C
        (SETQ W (CONS 1 1))
        (SETQ N 0)
        (COND ((AND Y (LESSP (CDAAR Y) 0)) (GO H)))
        (COND ((SETQ X (GETRTYPE XEXP)) (TYPERR X "substituted expression")))
        (SETQ X (SIMP* XEXP))
        (SETQ X (CONS (REORDER (CAR X)) (REORDER (CDR X))))
        (COND ((AND (NULL L) (KERNP X) (EQ (CAAAR (CAR X)) KERN)) (GO F))
              ((NULL (CAR X)) (GO E)))
        (PROG (J)
          (SETQ J Y)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J)
             (PROGN
              (SETQ M (CDAR J))
              (SETQ W (*MULTSQ (*EXPTSQ X (DIFFERENCE M N)) W))
              (SETQ N M)
              (SETQ Z (*ADDSQ (*MULTSQ W (ALGINT-SUBF1 (CDR J) L)) Z))))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB))
       E
        (SETQ Y NIL)
        (COND ((NULL U) (RETURN Z))
              ((OR (ATOM U) (ATOM (CAR U)))
               (RETURN (*ADDSQ (ALGINT-SUBF1 U L) Z))))
        (GO A0)
       F
        (SUB2CHK KERN)
        (PROG (J)
          (SETQ J Y)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J)
             (SETQ Z
                     (*ADDSQ
                      (*MULTSQ (CONS (LIST (CONS (CAR J) 1)) 1)
                               (ALGINT-SUBF1 (CDR J) L))
                      Z)))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB))
        (GO E)
       H
        (SETQ X (SIMPRECIP (LIST XEXP)))
       J
        (SETQ Y1 (CONS (CAR Y) Y1))
        (SETQ Y (CDR Y))
        (COND ((AND Y (LESSP (CDAAR Y) 0)) (GO J)))
       K
        (SETQ M (MINUS (CDAAR Y1)))
        (SETQ W (*MULTSQ (*EXPTSQ X (DIFFERENCE M N)) W))
        (SETQ N M)
        (SETQ Z (*ADDSQ (*MULTSQ W (ALGINT-SUBF1 (CDAR Y1) L)) Z))
        (SETQ Y1 (CDR Y1))
        (COND (Y1 (GO K)) (Y (GO C)) (T (GO E))))))) 
(PUT 'ALGINT-SUBSUBLIS 'NUMBER-OF-ARGS 2) 
(PUT 'ALGINT-SUBSUBLIS 'DEFINED-ON-LINE '102) 
(PUT 'ALGINT-SUBSUBLIS 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SUBSUBLIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGINT-SUBSUBLIS (U V)
    (PROG (X)
      (RETURN
       (COND ((SETQ X (ASSOC V U)) (CDR X)) ((ATOM V) V)
             ((EQ (CAR V) '*SQ)
              (LIST '*SQ (ALGINT-SUBSQ (CADR V) U) (CADDR V)))
             ((SETQ X (GET (CAR V) 'SUBFUNC)) (APPLY2 X U V))
             (T
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J V)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (ALGINT-SUBSUBLIS U J)) (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (ALGINT-SUBSUBLIS U J)) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'INT 'SUBFUNC 'ALGINT-SUBSUBF) 
(PUT 'ALGINT-SUBSUBF 'NUMBER-OF-ARGS 2) 
(PUT 'ALGINT-SUBSUBF 'DEFINED-ON-LINE '117) 
(PUT 'ALGINT-SUBSUBF 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SUBSUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGINT-SUBSUBF (L EXPN)
    (PROG (X Y)
      (PROG (J)
        (SETQ J (CDDR EXPN))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((SETQ X (ASSOC J L))
             (PROGN (SETQ Y (CONS X Y)) (SETQ L (DELETE X L))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ EXPN
              (CONS (SUBLIS L (CAR EXPN))
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDR EXPN))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (ALGINT-SUBSUBLIS L J))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (ALGINT-SUBSUBLIS L J)) (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (COND ((NULL Y) (RETURN EXPN)))
      (SETQ EXPN
              (ACONC*
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (REVERSIP* Y))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (LIST 'EQUAL (CAR J)
                                           (REVAL1 (CDR J) NIL)))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J)
                             (LIST 'EQUAL (CAR J) (REVAL1 (CDR J) NIL)))
                           (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               EXPN))
      (RETURN
       (MK*SQ
        (COND (L (ALGINT-SIMPSUB EXPN))
              (T
               (CONS (LIST (CONS (GETPOWER (FKERN (CONS 'SUB EXPN)) 1) 1))
                     1))))))) 
(PUT 'ALGINT-SIMPSUB 'NUMBER-OF-ARGS 1) 
(PUT 'ALGINT-SIMPSUB 'DEFINED-ON-LINE '133) 
(PUT 'ALGINT-SIMPSUB 'DEFINED-IN-FILE 'ALGINT/FIXSUBF.RED) 
(PUT 'ALGINT-SIMPSUB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGINT-SIMPSUB (U)
    (PROG (*NOSUBS W X Z)
     A
      (COND
       ((NULL (CDR U))
        (PROGN
         (COND
          ((OR (GETRTYPE (CAR U)) (EQCAR (CAR U) 'EQUAL))
           (TYPERR (CAR U) "scalar")))
         (SETQ U (SIMP* (CAR U)))
         (SETQ Z (REVERSIP* Z))
         (RETURN
          (MULTSQ (ALGINT-SUBF (CAR U) Z) (INVSQ (ALGINT-SUBF (CDR U) Z)))))))
      (SETQ *NOSUBS T)
      (SETQ W (REVAL1 (CAR U) T))
      (SETQ *NOSUBS NIL)
      (COND
       ((EQ (GETRTYPE W) 'LIST)
        (PROGN (SETQ U (APPEND (CDR W) (CDR U))) (GO A)))
       ((NOT (EQEXPR W)) (ERRPRI2 (CAR U) T)))
      (SETQ X (CADR W))
      (COND ((NULL (GETRTYPE X)) (SETQ X (*A2K X))))
      (SETQ Z (CONS (CONS X (CADDR W)) Z))
      (SETQ U (CDR U))
      (GO A))) 
(ENDMODULE) 
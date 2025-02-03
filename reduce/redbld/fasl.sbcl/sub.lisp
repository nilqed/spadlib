(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUB)) 
(FLUID '(*NOSQRTS ASYMPLIS* DMODE* ERRMSG* NCMP* SUBLIST* WTL*)) 
(PUT 'SUBEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SUBEVAL 'DEFINED-ON-LINE '36) 
(PUT 'SUBEVAL 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBEVAL (U)
    (PROG (SUBLIST* X)
      (PUT 'SUB 'PSOPFN 'SUBEVAL0)
      (SETQ X (ERRORSET2 (LIST 'SUBEVAL0 (MKQUOTE U))))
      (PUT 'SUB 'PSOPFN 'SUBEVAL)
      (COND
       ((ERRORP X)
        (COND (ERRMSG* (REDERR ERRMSG*))
              (T (REDERR "Error in sub operator")))))
      (RETURN (CAR X)))) 
(PUT 'SUBEVAL0 'NUMBER-OF-ARGS 1) 
(PUT 'SUBEVAL0 'DEFINED-ON-LINE '53) 
(PUT 'SUBEVAL0 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBEVAL0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBEVAL0 (U)
    (PROG (X Y Z NS)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR U)) (RETURN NIL)))
        (PROGN
         (COND ((NOT (EQCAR (CAR U) 'EQUAL)) (SETQ X (CONS (CAR U) X)))
               ((NOT (EQUAL (CADAR U) (SETQ Y (REVAL1 (CADDAR U) T))))
                (SETQ X (CONS (LIST (CAAR U) (CADAR U) Y) X))))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (COND ((NULL X) (RETURN (CAR U))) (T (SETQ U (REVERSIP2 X U))))
      (COND
       ((SETQ X (ASSOC U SUBLIST*))
        (RETURN
         (COND
          ((NULL (CDR X))
           (MK*SQ (CONS (LIST (CONS (GETPOWER (FKERN (CONS 'SUB U)) 1) 1)) 1)))
          (T (CDR X)))))
       (T (SETQ SUBLIST* (CONS (CONS U NIL) SUBLIST*))))
      (COND
       ((NULL (AND U (CDR U))) (REDERR "SUB requires at least 2 arguments")))
      ((LAMBDA (*EVALLHSEQP)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR U)) (RETURN NIL)))
           (PROGN
            (SETQ X (REVAL1 (CAR U) T))
            (COND ((EQ (GETRTYPE X) 'LIST) (SETQ U (APPEND (CDR X) (CDR U))))
                  (T
                   (PROGN
                    (COND ((NOT (EQEXPR X)) (ERRPRI2 (CAR U) T)))
                    (SETQ Y (CADR X))
                    (COND ((NULL (GETRTYPE Y)) (SETQ Y (*A2KWOWEIGHT Y))))
                    (COND
                     ((GETRTYPE (CADDR X))
                      (SETQ NS (CONS (CONS Y (CADDR X)) NS)))
                     (T (SETQ Z (CONS (CONS Y (CADDR X)) Z))))
                    (SETQ U (CDR U))))))
           (GO WHILELABEL)))
       NIL)
      (SETQ X (REVAL1 (CAR U) NIL))
      (SETQ X (SUBEVAL1 (APPEND NS Z) X))
      (COND ((NULL (CDAR SUBLIST*)) (RPLACD (CAR SUBLIST*) X)))
      (RETURN X))) 
(PUT 'SUBEVAL1 'NUMBER-OF-ARGS 2) 
(PUT 'SUBEVAL1 'DEFINED-ON-LINE '91) 
(PUT 'SUBEVAL1 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBEVAL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBEVAL1 (U V)
    (PROG (Y Z)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (EQUAL (CAAR U) (CDAR U)))) (RETURN NIL)))
        (SETQ U (CDR U))
        (GO WHILELABEL))
      (COND ((NULL U) (RETURN V))
            ((SETQ Y (GETRTYPE V))
             (COND ((SETQ Z (GET Y 'SUBFN)) (RETURN (APPLY2 Z U V)))
                   (T
                    (RERROR 'ALG 23
                            (LIST "No substitution defined for type" Y))))))
      (SETQ U (SUBSQ (SIMP V) U))
      ((LAMBDA (*SUB2) (SETQ U (SUBS2 U))) T)
      (RETURN (MK*SQ U)))) 
(PUT 'SUB 'PSOPFN 'SUBEVAL) 
(PUT 'SUBSQ 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSQ 'DEFINED-ON-LINE '127) 
(PUT 'SUBSQ 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSQ (U V)
    (PROG (X)
      (SETQ X (SUBF (CAR U) V))
      (SETQ U (SUBF (CDR U) V))
      (COND
       ((NULL (CAR (SUBS2* U)))
        (COND ((NULL (CAR (SUBS2* X))) (RERROR 'ALG 201 "0/0 formed"))
              (T (RERROR 'ALG 201 "Zero divisor")))))
      (RETURN (MULTSQ X (INVSQ U))))) 
(PUT 'SUBS2* 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS2* 'DEFINED-ON-LINE '139) 
(PUT 'SUBS2* 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBS2* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS2* (U) ((LAMBDA (*SUB2) (SUBS2 U)) *SUB2)) 
(PUT 'SUBF 'NUMBER-OF-ARGS 2) 
(PUT 'SUBF 'DEFINED-ON-LINE '142) 
(PUT 'SUBF 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBF (U L)
    (PROG (ALGLIST* X Y Z)
      (SETQ ALGLIST* (CONS NIL NIL))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (*D2Q U)))
            ((AND NCMP* (NONCOMEXPF U)) (RETURN (SUBF1 U L))))
      (SETQ X
              (REVERSE
               (INTERSECTION
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y L)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))
                (KERNORD U NIL))))
      (SETQ X (SETKORDER X))
      (SETQ U (SUBF1 (REORDER U) L))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (MEMBER U Z))
                (OR (ATSOC 'EXPT (KERNELS (CAR U)))
                    (ATSOC 'EXPT (KERNELS (CDR U))))
                (NOT (MEMBER (SETQ Y (PREPSQ U)) VARSTACK*))))
          (RETURN NIL)))
        (PROGN (SETQ Z (CONS U Z)) (SETQ U (SIMP Y)))
        (GO WHILELABEL))
      (SETKORDER X)
      (RETURN (CONS (REORDER (CAR U)) (REORDER (CDR U)))))) 
(PUT 'NONCOMEXPF 'NUMBER-OF-ARGS 1) 
(PUT 'NONCOMEXPF 'DEFINED-ON-LINE '165) 
(PUT 'NONCOMEXPF 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'NONCOMEXPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NONCOMEXPF (U)
    (AND (NOT (OR (ATOM U) (ATOM (CAR U))))
         (OR (AND *NCMP (NONCOMP1 (CAAAR U))) (NONCOMEXPF (CDAR U))
             (NONCOMEXPF (CDR U))))) 
(PUT 'SUBF1 'NUMBER-OF-ARGS 2) 
(PUT 'SUBF1 'DEFINED-ON-LINE '173) 
(PUT 'SUBF1 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBF1 (U L)
    (COND ((NULL U) (CONS NIL 1))
          ((OR (ATOM U) (ATOM (CAR U)))
           (COND ((ATOM U) (COND ((NULL DMODE*) (CONS U 1)) (T (SIMPATOM U))))
                 ((AND (EQ DMODE* (CAR U)) (NOT (FLAGP DMODE* 'RESIMPLIFY)))
                  (*D2Q U))
                 (T (SIMP (PREPF U)))))
          (T
           (PROG (N KERN L1 M VARSTACK* V W X X1 XEXP Y Y1 Z)
             (SETQ N 0)
             (SETQ Z (CONS NIL 1))
            A0
             (SETQ KERN (CAAAR U))
             (SETQ V NIL)
             (COND
              ((AND (ASSOC KERN L) (SETQ V (ASSOC KERN WTL*)))
               (SETQ V (CDR V))))
             (COND ((SETQ M (ASSOC KERN ASYMPLIS*)) (SETQ M (CDR M))))
            A
             (COND ((OR (NULL U) (EQUAL (SETQ N (DEGR U KERN)) 0)) (GO B))
                   ((OR (NULL M) (LESSP N M))
                    (SETQ Y (CONS (WTCHK (CAR U) V) Y))))
             (SETQ U (CDR U))
             (GO A)
            B
             (SETQ L1 NIL)
             (PROG ()
              WHILELABEL
               (COND ((NOT L) (RETURN NIL)))
               (PROGN
                (COND ((NEQ (CAAR L) (CDAR L)) (SETQ L1 (CONS (CAR L) L1))))
                (SETQ L (CDR L)))
               (GO WHILELABEL))
             (SETQ L (REVERSIP L1))
             (COND
              ((AND (NOT (ATOM KERN)) (NOT (ATOM (CAR KERN))))
               (SETQ KERN (PREPF KERN))))
             (COND ((NULL L) (SETQ XEXP (COND ((EQ KERN 'K*) 1) (T KERN))))
                   ((AND (EQUAL (SETQ XEXP (SUBSUBLIS L KERN)) KERN)
                         (NOT (ASSOC KERN ASYMPLIS*)))
                    (GO F)))
            C
             (SETQ W (CONS 1 1))
             (SETQ N 0)
             (COND ((AND Y (MINUSP (CDAAR Y))) (GO H)))
             (COND
              ((EQ (SETQ X (GETRTYPE XEXP)) 'YETUNKNOWNTYPE)
               (SETQ X
                       (GETRTYPE
                        (SETQ XEXP (EVAL-YETUNKNOWNTYPEEXPR XEXP NIL))))))
             (COND
              ((AND X (NOT (EQ X 'LIST)))
               (TYPERR (LIST X XEXP) "substituted expression")))
             (SETQ V (SETKORDER NIL))
             (SETQ X (SIMP XEXP))
             (SETKORDER V)
             (SETQ X (REORDSQ X))
             (COND ((AND (NULL L) (KERNP X) (EQ (CAAAR (CAR X)) KERN)) (GO F))
                   ((NULL (CAR X)) (GO E)))
             (SETQ X1 X)
             (PROG (J)
               (SETQ J Y)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J)
                  (PROGN
                   (SETQ M (CDAR J))
                   (COND
                    ((MEMQ M FRLIS*)
                     (PROGN
                      (SETQ X
                              ((LAMBDA (U)
                                 (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U))
                                       (T (BASIC-SIMPEXPT U))))
                               (LIST (PREPSQ X1) M)))
                      (SETQ M 1))))
                   (SETQ W (MULTSQ (SUBS2 (EXPTSQ X (DIFFERENCE M N))) W))
                   (SETQ N M)
                   (SETQ Z (ADDSQ (MULTSQ W (SUBF1 (CDR J) L)) Z))))
                (CAR J))
               (SETQ J (CDR J))
               (GO LAB))
            E
             (PROG ()
              WHILELABEL
               (COND ((NOT Y) (RETURN NIL)))
               (PROGN (SUBF1 (CDR (CAR Y)) L) (SETQ Y (CDR Y)))
               (GO WHILELABEL))
             (COND ((NULL U) (RETURN Z))
                   ((OR (ATOM U) (ATOM (CAR U)))
                    (RETURN (ADDSQ (SUBF1 U L) Z))))
             (GO A0)
            F
             (SUB2CHK KERN)
             (PROG (J)
               (SETQ J Y)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J)
                  (SETQ Z
                          (ADDSQ
                           (MULTSQ (CONS (LIST (CONS (CAR J) 1)) 1)
                                   (SUBF1 (CDR J) L))
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
             (SETQ W (MULTSQ (SUBS2 (EXPTSQ X (DIFFERENCE M N))) W))
             (SETQ N M)
             (SETQ Z (ADDSQ (MULTSQ W (SUBF1 (CDAR Y1) L)) Z))
             (SETQ Y1 (CDR Y1))
             (COND (Y1 (GO K)) (Y (GO C)) (T (GO E))))))) 
(PUT 'WTCHK 'NUMBER-OF-ARGS 2) 
(PUT 'WTCHK 'DEFINED-ON-LINE '271) 
(PUT 'WTCHK 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'WTCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WTCHK (U WT)
    (COND ((NULL WT) U)
          (T
           ((LAMBDA (X)
              (COND ((NULL X) (ERRACH (LIST "weight confusion" U WT)))
                    (T (CAR X))))
            ((LAMBDA (*EXP)
               (QUOTF1 (CONS U NIL)
                       (LIST (CONS (CONS 'K* (TIMES WT (CDAR U))) 1))))
             T))))) 
(PUT 'SUBSUBLIS 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSUBLIS 'DEFINED-ON-LINE '279) 
(PUT 'SUBSUBLIS 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBSUBLIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSUBLIS (U V)
    (PROG (X)
      (SETQ X
              (COND ((SETQ X (ASSOC V U)) (CDR X))
                    ((AND (EQCAR V 'SQRT)
                          (SETQ X
                                  (ASSOC (LIST 'EXPT (CADR V) '(QUOTIENT 1 2))
                                         U)))
                     (CDR X))
                    ((ATOM V) V)
                    ((NOT (IDP (CAR V)))
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J V)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J) (SUBSUBLIS U J)) (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (SUBSUBLIS U J)) (CAR J))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
                    ((SETQ X (GET (CAR V) 'SUBFUNC)) (APPLY2 X U V))
                    ((GET (CAR V) 'DNAME) V)
                    ((EQ (CAR V) '*SQ) (SUBSUBLIS U (PREPSQ (CADR V))))
                    (T
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J V)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J) (SUBSUBLIS U J)) (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (SUBSUBLIS U J)) (CAR J))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (RETURN X))) 
(PUT 'SUBSUBF 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSUBF 'DEFINED-ON-LINE '302) 
(PUT 'SUBSUBF 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'SUBSUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSUBF (L EXPN)
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
                                       ((LAMBDA (J) (SUBSUBLIS L J)) (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (SUBSUBLIS L J)) (CAR J))
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
       (COND (L (SUBEVAL EXPN))
             (T
              (MK*SQ
               (CONS (LIST (CONS (GETPOWER (FKERN (CONS 'SUB EXPN)) 1) 1))
                     1))))))) 
(PUT 'LISTSUB 'NUMBER-OF-ARGS 2) 
(PUT 'LISTSUB 'DEFINED-ON-LINE '327) 
(PUT 'LISTSUB 'DEFINED-IN-FILE 'ALG/SUB.RED) 
(PUT 'LISTSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTSUB (U V)
    (CONS 'LIST
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (CDR V))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (SUBEVAL1 U X)) (CAR X)) NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (X) (SUBEVAL1 U X)) (CAR X)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'LIST 'SUBFN 'LISTSUB) 
(PUT 'INT 'SUBFUNC 'SUBSUBF) 
(PUT 'DF 'SUBFUNC 'SUBSUBF) 
(ENDMODULE) 
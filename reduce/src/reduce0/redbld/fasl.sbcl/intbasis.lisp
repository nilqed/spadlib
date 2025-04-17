(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTBASIS)) 
(FLUID
 '(*TRA *TRMIN EXCOATESPOLES INTVAR PREVIOUSBASIS TAYLORASSLIST TAYLORVARIABLE)) 
(EXPORTS (LIST 'COMPLETEPLACES 'COMPLETEPLACES2 'INTEGRALBASIS)) 
(PUT 'DELETEPLACE 'NUMBER-OF-ARGS 2) 
(PUT 'DELETEPLACE 'DEFINED-ON-LINE '36) 
(PUT 'DELETEPLACE 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'DELETEPLACE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DELETEPLACE (A B)
    (COND ((NULL B) NIL) ((EQUALPLACE A (CAR B)) (CDR B))
          (T (CONS (CAR B) (DELETEPLACE A (CDR B)))))) 
(PUT 'COMPLETEPLACES 'NUMBER-OF-ARGS 2) 
(PUT 'COMPLETEPLACES 'DEFINED-ON-LINE '44) 
(PUT 'COMPLETEPLACES 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'COMPLETEPLACES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPLETEPLACES (PLACES MULTS)
    (PROG (CURRENT CP CM OP OM ANSP ANSM)
      (COND ((NULL PLACES) (RETURN NIL)))
     LOOP
      (SETQ CURRENT (BASICPLACE (CAR PLACES)))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL CURRENT (BASICPLACE (CAR PLACES)))
           (PROGN
            (SETQ CP (CONS (CAR PLACES) CP))
            (SETQ CM (CONS (CAR MULTS) CM))))
          (T
           (PROGN
            (SETQ OP (CONS (CAR PLACES) OP))
            (SETQ OM (CONS (CAR MULTS) OM)))))
         (SETQ PLACES (CDR PLACES))
         (SETQ MULTS (CDR MULTS)))
        (GO WHILELABEL))
      (SETQ CP (COMPLETEPLACES2 CP CM (SQRTSINPLACES CP)))
      (SETQ ANSP (APPEND (CAR CP) ANSP))
      (SETQ ANSM (APPEND (CDR CP) ANSM))
      (SETQ PLACES OP)
      (SETQ MULTS OM)
      (SETQ CP (SETQ OP (SETQ CM (SETQ OM NIL))))
      (COND (PLACES (GO LOOP)) (T (RETURN (CONS ANSP ANSM)))))) 
(PUT 'COMPLETEPLACES2 'NUMBER-OF-ARGS 3) 
(PUT 'COMPLETEPLACES2 'DEFINED-ON-LINE '72) 
(PUT 'COMPLETEPLACES2 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'COMPLETEPLACES2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPLETEPLACES2 (PLACES MULTS SQRTS)
    (PROG (B P)
      (SETQ SQRTS (SQRTSIGN SQRTS INTVAR))
      (SETQ B (BASICPLACE (CAR PLACES)))
      (SETQ P PLACES)
      (PROG ()
       WHILELABEL
        (COND ((NOT P) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (EQUAL B (BASICPLACE (CAR P))))
           (INTERR "Multiple places not supported")))
         (SETQ SQRTS (DELETEPLACE (EXTENPLACE (CAR P)) SQRTS))
         (SETQ P (CDR P)))
        (GO WHILELABEL))
      (SETQ MULTS (NCONC (NLIST 0 (LENGTH SQRTS)) MULTS))
      (SETQ PLACES (NCONC (MAPPEND SQRTS B) PLACES))
      (RETURN (CONS PLACES MULTS)))) 
(PUT 'INTBASISREDUCTION 'NUMBER-OF-ARGS 3) 
(PUT 'INTBASISREDUCTION 'DEFINED-ON-LINE '89) 
(PUT 'INTBASISREDUCTION 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'INTBASISREDUCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTBASISREDUCTION (ZBASIS PLACES MULTS)
    (PROG (I M N V W SUBSTN BASIS)
      (SETQ SUBSTN (LIST (CONS INTVAR INTVAR)))
      (SETQ N (UPBV ZBASIS))
      (SETQ BASIS (COPYVEC ZBASIS N))
      (SETQ TAYLORVARIABLE INTVAR)
      (SETQ V (SQRTSINPLACES PLACES))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ W (UNION W (SQRTSINSQ (GETV BASIS I) INTVAR)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ M (INTERSECTION V W))
      (SETQ V (SETDIFF V M))
      (SETQ W (SETDIFF W M))
      (PROG (U)
        (SETQ U V)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (COND
             ((OR *TRA *TRMIN)
              (PROGN
               (PRIN2T U)
               (PRIN2T "does not occur in the functions")
               (MAPVEC BASIS (FUNCTION PRINTSQ)))))
            (SETQ M (*Q2F (SIMP (CADR U))))
            (SETQ I W)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND I
                      (NOT
                       ((LAMBDA (*EXP) (QUOTF1 M (*Q2F (SIMP (CADR (CAR I))))))
                        T))))
                (RETURN NIL)))
              (SETQ I (CDR I))
              (GO WHILELABEL))
            (COND
             ((NULL I)
              (INTERR "Unable to find equivalent representation of branches")))
            (SETQ I (CAR I))
            (SETQ W (DELETE I W))
            (SETQ PLACES (SUBST I U PLACES))
            (COND
             ((OR *TRA *TRMIN) (PROGN (PRIN2T "replaced by") (PRIN2T I))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (COND
       ((NEQ (LENGTH PLACES) (IADD1 N))
        (PROGN
         (COND (*TRA (PRIN2T "Too many functions")))
         (SETQ BASIS (SHORTEN-BASIS BASIS))
         (SETQ N (UPBV BASIS)))))
      (SETQ M (MKVECT N))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV M I (CL6ROWEVAL (CONS BASIS I) PLACES MULTS SUBSTN))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
     REDUCTIONLOOP
      (COND
       (*TRA
        (PROGN
         (PRIN2T "Matrix before a reduction step:")
         (MAPVEC M (FUNCTION PRIN2T)))))
      (SETQ V (FIRSTLINEARRELATION M (IADD1 N)))
      (COND
       ((NULL V)
        (RETURN
         (REPLICATEBASIS BASIS (QUOTIENT (IADD1 (UPBV ZBASIS)) (PLUS N 1))))))
      (SETQ I N)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NULL (CAR (GETV V I)))) (RETURN NIL)))
        (SETQ I (ISUB1 I))
        (GO WHILELABEL))
      (SETQ W (CONS NIL 1))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((MINUSP (DIFFERENCE I J)) (RETURN NIL)))
        (SETQ W (*ADDSQ W (*MULTSQ (GETV BASIS J) (GETV V J))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ W
              (REMOVECMSQ
               (MULTSQ W
                       (CONS 1 (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1))))))
      (COND
       ((NULL (CAR W))
        (PROGN
         (MAPVEC BASIS (FUNCTION PRINTSQ))
         (PRIN2T (IADD1 I))
         (INTERR "Basis collapses"))))
      (COND
       (*TRA
        (PROGN
         (PRINC "Element ")
         (PRINC (IADD1 I))
         (PRIN2T " of the basis replaced by ")
         (COND (*TRA (PRINTSQ W))))))
      (PUTV BASIS I W)
      (PUTV M I (CL6ROWEVAL (CONS BASIS I) PLACES MULTS SUBSTN))
      (GO REDUCTIONLOOP))) 
(PUT 'INTEGRALBASIS 'NUMBER-OF-ARGS 4) 
(PUT 'INTEGRALBASIS 'DEFINED-ON-LINE '160) 
(PUT 'INTEGRALBASIS 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'INTEGRALBASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRALBASIS (BASIS PLACES MULTS X)
    (PROG (Z SAVE POINTS P M PRINCILAP-PART M1)
      (COND ((NULL PLACES) (RETURN BASIS)))
      (SETQ MULTS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U MULTS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (MIN U 0)) (CAR U)) NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (MIN U 0)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ POINTS
              (REMOVEDUPLICATES
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J PLACES)
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (BASICPLACE J)) (CAR J))
                                       NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (BASICPLACE J)) (CAR J)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       ((EQUAL POINTS (LIST (CONS X X)))
        (SETQ BASIS (INTBASISREDUCTION BASIS PLACES MULTS)))
       ((CDR POINTS) (GO COMPLEX))
       (T
        (PROGN
         (SUBSTITUTEVEC BASIS (CAR POINTS))
         (COND
          (*TRA
           (PROGN
            (PRIN2T "Integral basis reduction at")
            (PRIN2T (CAR POINTS)))))
         (SETQ BASIS
                 (INTBASISREDUCTION BASIS
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J PLACES)
                    (COND ((NULL J) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (EXTENPLACE J)) (CAR J))
                                          NIL)))
                   LOOPLABEL
                    (SETQ J (CDR J))
                    (COND ((NULL J) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (J) (EXTENPLACE J)) (CAR J)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  MULTS))
         (SUBSTITUTEVEC BASIS (ANTISUBS (CAR POINTS) X)))))
     CONC
      (SETQ SAVE TAYLORASSLIST)
      (SETQ Z (GENSYM))
      (SETQ PLACES (MAPCONS PLACES (CONS X (LIST 'DIFFERENCE X Z))))
      (SETQ Z (LIST (CONS X Z)))
      (SETQ TAYLORASSLIST SAVE)
      (COND
       ((NOT EXCOATESPOLES) (SETQ PREVIOUSBASIS (COPYVEC BASIS (UPBV BASIS)))))
      (RETURN BASIS)
     COMPLEX
      (PROG ()
       WHILELABEL
        (COND ((NOT POINTS) (RETURN NIL)))
        (PROGN
         (SETQ P PLACES)
         (SETQ M MULTS)
         (SETQ PRINCILAP-PART (SETQ M1 NIL))
         (PROG ()
          WHILELABEL
           (COND ((NOT P) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (CAR POINTS) (BASICPLACE (CAR P)))
              (PROGN
               (SETQ PRINCILAP-PART (CONS (EXTENPLACE (CAR P)) PRINCILAP-PART))
               (SETQ M1 (CONS (CAR M) M1)))))
            (SETQ P (CDR P))
            (SETQ M (CDR M)))
           (GO WHILELABEL))
         (SUBSTITUTEVEC BASIS (CAR POINTS))
         (COND
          (*TRA
           (PROGN
            (PRIN2T "Integral basis reduction at")
            (PRIN2T (CAR POINTS)))))
         (SETQ BASIS (INTBASISREDUCTION BASIS PRINCILAP-PART M1))
         (SUBSTITUTEVEC BASIS (ANTISUBS (CAR POINTS) X))
         (SETQ POINTS (CDR POINTS)))
        (GO WHILELABEL))
      (GO CONC))) 
(PUT 'CL6ROWEVAL 'NUMBER-OF-ARGS 4) 
(PUT 'CL6ROWEVAL 'DEFINED-ON-LINE '221) 
(PUT 'CL6ROWEVAL 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'CL6ROWEVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL6ROWEVAL (BASISLOC PLACES MULTS X-ALPHA)
    (PROG (I V W SAVE BASISELEMENT TAYSAVE MMULTS FLG)
      (SETQ I (ISUB1 (LENGTH PLACES)))
      (SETQ V (MKVECT I))
      (SETQ TAYSAVE (MKVECT I))
      (SETQ I 0)
      (SETQ BASISELEMENT (GETV (CAR BASISLOC) (CDR BASISLOC)))
      (SETQ MMULTS MULTS)
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (SETQ W (SUBSTITUTESQ BASISELEMENT (CAR PLACES)))
         (SETQ W (TAYLORFORM (SUBSTITUTESQ W X-ALPHA)))
         (SETQ SAVE TAYLORASSLIST)
         (COND ((NOT FLG) (PUTV TAYSAVE I W)))
         (SETQ W (TAYLOREVALUATE W (CAR MMULTS)))
         NIL
         (PUTV V I W)
         (SETQ I (IADD1 I))
         (SETQ FLG (OR FLG (CAR W)))
         (SETQ MMULTS (CDR MMULTS))
         (SETQ PLACES (CDR PLACES)))
        (GO WHILELABEL))
      (COND (FLG (RETURN V)))
      (SETQ SAVE 0)
     LOOP
      (SETQ SAVE (IADD1 SAVE))
      (SETQ MMULTS MULTS)
      (SETQ I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT MMULTS) (RETURN NIL)))
        (PROGN
         (SETQ W (TAYLOREVALUATE (GETV TAYSAVE I) (PLUS SAVE (CAR MMULTS))))
         (SETQ FLG (OR FLG (CAR W)))
         (SETQ MMULTS (CDR MMULTS))
         (PUTV V I W)
         (SETQ I (IADD1 I)))
        (GO WHILELABEL))
      (COND ((NOT FLG) (GO LOOP)))
      (PUTV (CAR BASISLOC) (CDR BASISLOC)
            (MULTSQ BASISELEMENT
                    (CONS 1 (LIST (CONS (GETPOWER (FKERN INTVAR) SAVE) 1)))))
      (RETURN V))) 
(PUT 'REPLICATEBASIS 'NUMBER-OF-ARGS 2) 
(PUT 'REPLICATEBASIS 'DEFINED-ON-LINE '269) 
(PUT 'REPLICATEBASIS 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'REPLICATEBASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPLICATEBASIS (BASIS N)
    (COND ((EQUAL N 1) BASIS)
          ((EQUAL N 2)
           (PROG (B SQINTVAR LEN)
             (SETQ LEN (UPBV BASIS))
             (SETQ SQINTVAR
                     (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)) 1))
             (SETQ B (MKVECT (PLUS (TIMES 2 LEN) 1)))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
               (PROGN
                (PUTV B I (GETV BASIS I))
                (PUTV B (PLUS I LEN 1) (MULTSQ SQINTVAR (GETV BASIS I))))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN B)))
          (T (INTERR "Unexpected replication request")))) 
(PUT 'SHORTEN-BASIS 'NUMBER-OF-ARGS 1) 
(PUT 'SHORTEN-BASIS 'DEFINED-ON-LINE '286) 
(PUT 'SHORTEN-BASIS 'DEFINED-IN-FILE 'ALGINT/INTBASIS.RED) 
(PUT 'SHORTEN-BASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHORTEN-BASIS (V)
    (PROG (U N SFINTVAR)
      (SETQ SFINTVAR (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)))
      (SETQ N (UPBV V))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG (UU)
          (SETQ UU (GETV V I))
          (COND
           ((NOT ((LAMBDA (*EXP) (QUOTF1 (CAR UU) SFINTVAR)) T))
            (SETQ U (CONS UU U)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (MKVEC U)))) 
(ENDMODULE) 
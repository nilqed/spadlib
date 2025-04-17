(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTH)) 
(FLUID '(MELLIN-TRANSFORMS* MELLIN-COEFFICIENTS*)) 
(DE LISTSQ (U)
    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
      (SETQ UU U)
      (COND ((NULL UU) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL)))
     LOOPLABEL
      (SETQ UU (CDR UU))
      (COND ((NULL UU) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'LISTSQ 'NUMBER-OF-ARGS 1) 
(PUT 'LISTSQ 'DEFINED-ON-LINE '32) 
(PUT 'LISTSQ 'DEFINED-IN-FILE 'DEFINT/DEFINTH.RED) 
(PUT 'LISTSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LISTSQ 'INLINE
      '(LAMBDA (U)
         (PROG (UU FORALL-RESULT FORALL-ENDPTR)
           (SETQ UU U)
           (COND ((NULL UU) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL)))
          LOOPLABEL
           (SETQ UU (CDR UU))
           (COND ((NULL UU) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))) 
(AEVAL
 (OPERATOR
  (LIST 'INDEFINT2 '|DEFINT:SUBTRACT| '|DEFINT:ADDX| '|DEFINT:MULTIPLYX|))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (|DEFINT:SUBTRACT| (~ U) (~ V))
      (WHEN (DIFFERENCE U V) (AND (FREEOF U UNKNOWN) (FREEOF V UNKNOWN))))
     (REPLACEBY (|DEFINT:SUBTRACT| (~ U) (~ V)) UNKNOWN))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (|DEFINT:ADDX| (~ U) (~ V))
      (WHEN (PLUS U V) (AND (FREEOF U UNKNOWN) (FREEOF V UNKNOWN))))
     (REPLACEBY (|DEFINT:ADDX| (~ U) (~ V)) UNKNOWN))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (|DEFINT:MULTIPLYX| (~ U) (~ V))
      (WHEN (TIMES U V) (AND (FREEOF U UNKNOWN) (FREEOF V UNKNOWN))))
     (REPLACEBY (|DEFINT:MULTIPLYX| (~ U) (~ V)) UNKNOWN))))) 
(SETK 'INDEFINT2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F1)
                                     (LIST '~ (LIST '~ 'F2)))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'INDEFINT2 (LIST 'QUOTIENT 'F1 'F3) 'X 'Y)
                               (LIST 'INDEFINT2 (LIST 'QUOTIENT 'F2 'F3) 'X
                                     'Y))
                         (LIST 'NOT (LIST 'EQUAL 'F2 0))))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST '~ 'N)
                         (LIST 'DIFFERENCE (LIST '~ 'F1) (LIST '~ 'F2))
                         (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST '|DEFINT:SUBTRACT| (LIST 'INDEFINT2 'N 'F1 'X 'Y)
                         (LIST 'INDEFINT2 'N 'F2 'X 'Y)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST '~ 'N)
                         (LIST 'PLUS (LIST '~ 'F1) (LIST '~ 'F2)) (LIST '~ 'X)
                         (LIST '~ 'Y))
                   (LIST '|DEFINT:ADDX| (LIST 'INDEFINT2 'N 'F1 'X 'Y)
                         (LIST 'INDEFINT2 'N 'F2 'X 'Y)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X)
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'TRANSF (LIST 'DEFINT_CHOOSE 'F 'X) (LIST 'MINUS 'A)
                         'Y 'X))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'B)))
                               (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'TRANSF (LIST 'DEFINT_CHOOSE 'F 'X)
                         (LIST 'PLUS 'B (LIST 'QUOTIENT 1 2)) 'Y 'X))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F)
                         (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'TRANSF (LIST 'DEFINT_CHOOSE 'F 'X)
                         (LIST 'QUOTIENT 1 2) 'Y 'X))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2
                         (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'A)))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'TRANSF (LIST 'DEFINT_CHOOSE 'F 'X) 'A 'Y 'X))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST 'TIMES (LIST '~ 'B) (LIST '~ 'FF))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST '|DEFINT:MULTIPLYX| 'B
                               (LIST 'INDEFINT2 'FF 'F 'X 'Y))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2
                         (LIST 'QUOTIENT (LIST '~ 'B)
                               (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                     (LIST '~ 'FF)))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST '|DEFINT:MULTIPLYX| (LIST 'QUOTIENT 'B 'C)
                               (LIST 'INDEFINT2 (LIST 'QUOTIENT 1 'FF) 'F 'X
                                     'Y))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST 'QUOTIENT (LIST '~ 'FF) (LIST '~ 'B))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'B)
                               (LIST 'INDEFINT2 'FF 'F 'X 'Y))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST 'TIMES (LIST '~ 'B) (LIST '~ 'FF))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST '|DEFINT:MULTIPLYX| 'B
                               (LIST 'INDEFINT2 'FF 'F 'X 'Y))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST 'QUOTIENT (LIST '~ 'FF) (LIST '~ 'B))
                         (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'B)
                               (LIST 'INDEFINT2 'FF 'F 'X 'Y))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST '~ 'B) (LIST '~ 'F) (LIST '~ 'X)
                         (LIST '~ 'Y))
                   (LIST 'WHEN
                         (LIST '|DEFINT:MULTIPLYX| 'B
                               (LIST 'INDEFINT2 'F 'X 'Y))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INDEFINT2 (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'Y))
                   (LIST 'TRANSF (LIST 'DEFINT_CHOOSE 'F 'X) 0 'Y 'X))))) 
(AEVAL (LET '(INDEFINT2_RULES))) 
(PUT 'SIMPINTEG 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINTEG 'DEFINED-ON-LINE '84) 
(PUT 'SIMPINTEG 'DEFINED-IN-FILE 'DEFINT/DEFINTH.RED) 
(PUT 'SIMPINTEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINTEG (U)
    (PROG (FF1 ALPHA Y VAR CHOSEN_NUM COEF *UNCACHED)
      (SETQ *UNCACHED T)
      (SETQ FF1 (PREPSQ (SIMP (CAR U))))
      (COND ((EQUAL FF1 'UNKNOWN) (RETURN (SIMP 'UNKNOWN))))
      (SETQ ALPHA (CADR U))
      (SETQ Y (CADDR U))
      (COND
       ((OR (SMEMBER 'MINUS Y) (AND (FIXP Y) (LESSP Y 0)))
        (RETURN (SIMP 'UNKNOWN))))
      (SETQ VAR (CADDDR U))
      (SETQ CHOSEN_NUM (CADR FF1))
      (COND
       ((EQUAL CHOSEN_NUM 0)
        (PROGN
         (SETQ COEF (CADDR FF1))
         (RETURN
          (SIMP
           (REVAL1
            (AEVAL
             (LIST 'TIMES COEF
                   (LIST 'QUOTIENT (LIST 'EXPT Y (LIST 'PLUS ALPHA 1))
                         (LIST 'PLUS ALPHA 1))))
            T)))))
       (T
        (PROGN
         (PUT '|DEFINT:OPF1| 'G (GETV MELLIN-TRANSFORMS* CHOSEN_NUM))
         (SETQ COEF (GETV MELLIN-COEFFICIENTS* CHOSEN_NUM))
         (COND
          (COEF (SETQ MELLINCOEF (PROGN (SETQ ALGLIST* (CONS NIL NIL)) COEF)))
          (T (SETQ MELLINCOEF (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 1))))
         (RETURN
          (SIMP
           (LIST 'NEW_MEI (CONS '|DEFINT:OPF1| (CDDR FF1)) ALPHA Y VAR)))))))) 
(AEVAL (PUT 'NEW_MEI 'SIMPFN 'NEW_MEIJER)) 
(PUT 'NEW_MEIJER 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_MEIJER 'DEFINED-ON-LINE '113) 
(PUT 'NEW_MEIJER 'DEFINED-IN-FILE 'DEFINT/DEFINTH.RED) 
(PUT 'NEW_MEIJER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_MEIJER (U)
    (PROG (F Y MELLIN NEW_MELLIN M N P Q OLD_NUM OLD_DENOM TEMP A1 B1 A2 B2
           ALPHA NUM DENOM N1 TEMP1 TEMP2 COEFF V VAR NEW_VAR NEW_Y NEW_V K)
      (SETQ F (PREPSQ (SIMP (CAR U))))
      (SETQ Y (CADDR U))
      (SETQ MELLIN (BASTAB (CAR F) (CDDR F)))
      (SETQ TEMP (CAR (CDDDDR MELLIN)))
      (SETQ VAR (CADR F))
      (COND ((NOT (IDP VAR)) (RETURN (ERROR 99 'FAIL))))
      (SETQ TEMP (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X VAR) TEMP)) T))
      (SETQ MELLIN
              (LIST (CAR MELLIN) (CADR MELLIN) (CADDR MELLIN) (CADDDR MELLIN)
                    TEMP))
      (SETQ TEMP (REDUCE_VAR (CADR U) MELLIN VAR))
      (SETQ ALPHA (SIMP* (CAR TEMP)))
      (SETQ NEW_MELLIN (CDR TEMP))
      (COND
       ((NEQ (CAR (CDDDDR NEW_MELLIN)) (CAR (CDDDDR MELLIN)))
        (PROGN
         (SETQ K (CAR (CDDDDR MELLIN)))
         (SETQ Y (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL VAR Y) K)) T))
         (SETQ NEW_Y (SIMP Y))))
       (T
        (PROGN
         (SETQ NEW_VAR (CAR (CDDDDR NEW_MELLIN)))
         (SETQ NEW_Y
                 (SIMP
                  (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X Y) NEW_VAR))
                          T))))))
      (SETQ N1 (ADDSQ ALPHA '(1 . 1)))
      (SETQ TEMP1 (LIST 'EXPT Y (PREPSQ N1)))
      (SETQ TEMP2 (CADDDR NEW_MELLIN))
      (SETQ COEFF (SIMP* (REVAL1 (AEVAL (LIST 'TIMES TEMP1 TEMP2)) T)))
      (SETQ M (CAAR NEW_MELLIN))
      (SETQ N (CADAR NEW_MELLIN))
      (SETQ P (CADDAR NEW_MELLIN))
      (SETQ Q (CAR (CDDDAR NEW_MELLIN)))
      (SETQ OLD_NUM (CADR NEW_MELLIN))
      (SETQ OLD_DENOM (CADDR NEW_MELLIN))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL OLD_NUM NIL) (SETQ A1 (APPEND A1 (LIST (SIMP* OLD_NUM)))))
          (T
           (PROGN
            (SETQ A1 (APPEND A1 (LIST (SIMP* (CAR OLD_NUM)))))
            (SETQ OLD_NUM (CDR OLD_NUM)))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL OLD_DENOM NIL)
           (SETQ B1 (APPEND B1 (LIST (SIMP* OLD_DENOM)))))
          (T
           (PROGN
            (SETQ B1 (APPEND B1 (LIST (SIMP* (CAR OLD_DENOM)))))
            (SETQ OLD_DENOM (CDR OLD_DENOM)))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ A2
              (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                (SETQ UU OLD_NUM)
                (COND ((NULL UU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                      NIL)))
               LOOPLABEL
                (SETQ UU (CDR UU))
                (COND ((NULL UU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ B2
              (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                (SETQ UU OLD_DENOM)
                (COND ((NULL UU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                      NIL)))
               LOOPLABEL
                (SETQ UU (CDR UU))
                (COND ((NULL UU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND (EQUAL A1 NIL) (EQUAL A2 NIL))
        (SETQ NUM (LIST (LIST (NEGSQ ALPHA)))))
       ((EQUAL A2 NIL) (SETQ NUM (LIST (APPEND A1 (LIST (NEGSQ ALPHA))))))
       (T
        (PROGN
         (SETQ NUM (APPEND A1 (LIST (NEGSQ ALPHA))))
         (SETQ NUM (APPEND (LIST NUM) A2)))))
      (COND
       ((AND (EQUAL B1 NIL) (EQUAL B2 NIL))
        (SETQ DENOM (LIST (LIST (ADDSQ (NEGSQ ALPHA) (NEGSQ '(1 . 1)))))))
       ((EQUAL B2 NIL)
        (SETQ DENOM (LIST B1 (ADDSQ (NEGSQ ALPHA) (NEGSQ '(1 . 1))))))
       (T
        (PROGN
         (SETQ DENOM (LIST B1 (ADDSQ (NEGSQ ALPHA) (NEGSQ '(1 . 1)))))
         (SETQ DENOM (APPEND DENOM B2)))))
      (SETQ V (GFMSQ NUM DENOM NEW_Y))
      (COND ((EQUAL V 'FAIL) (RETURN (SIMP 'FAIL)))
            (T (SETQ V (PREPSQ (SUBSQ V (LIST (CONS (PREPSQ NEW_Y) Y)))))))
      (COND ((EQCAR V 'MEIJERG) (SETQ NEW_V V)) (T (SETQ NEW_V (SIMP V))))
      (RETURN (MULTSQ NEW_V COEFF)))) 
(PUT 'REDUCE_VAR 'NUMBER-OF-ARGS 3) 
(PUT 'REDUCE_VAR 'DEFINED-ON-LINE '203) 
(PUT 'REDUCE_VAR 'DEFINED-IN-FILE 'DEFINT/DEFINTH.RED) 
(PUT 'REDUCE_VAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDUCE_VAR (U V VAR1)
    (PROG (VAR M N COEF ALPHA BETA ALPHA1 ALPHA2 EXPT_FLAG K TEMP1 TEMP2 CONST
           NEW_K)
      (SETQ VAR (CAR (CDDDDR V)))
      (SETQ BETA 1)
      (COND ((EQUAL (LENGTH VAR) 0) (RETURN (CONS U V)))
            (T
             (PROGN
              (SETQ K U)
              (SETQ COEF (CADDDR V))
              (PROG (I)
                (SETQ I VAR)
               LAB
                (COND ((NULL I) (RETURN NIL)))
                ((LAMBDA (I)
                   (PROGN
                    (COND
                     ((LISTP I)
                      (PROGN
                       (COND
                        ((EQUAL (CAR I) 'EXPT)
                         (PROGN (SETQ ALPHA (CADDR I)) (SETQ EXPT_FLAG 'T)))
                        ((EQUAL (CAR I) 'SQRT)
                         (PROGN
                          (SETQ BETA 2)
                          (SETQ ALPHA 1)
                          (SETQ EXPT_FLAG 'T)))
                        ((EQUAL (CAR I) 'TIMES)
                         (PROGN
                          (SETQ TEMP1 (CADR I))
                          (SETQ TEMP2 (CADDR I))
                          (COND
                           ((LISTP TEMP1)
                            (PROGN
                             (COND
                              ((EQUAL (CAR TEMP1) 'SQRT)
                               (PROGN
                                (SETQ BETA 2)
                                (SETQ ALPHA1 1)
                                (SETQ EXPT_FLAG 'T)))
                              ((AND (EQUAL (CAR TEMP1) 'EXPT)
                                    (LISTP (CADDR TEMP1)))
                               (PROGN
                                (SETQ BETA (CADR (CDADDR TEMP1)))
                                (SETQ ALPHA1 (CAR (CDADDR TEMP1)))
                                (SETQ EXPT_FLAG 'T))))
                             NIL)))
                          (COND
                           ((AND (LISTP TEMP2) (EQUAL (CAR TEMP2) 'EXPT))
                            (PROGN
                             (SETQ ALPHA2 (CADDR TEMP2))
                             (SETQ EXPT_FLAG 'T))))
                          (COND
                           ((NEQ ALPHA1 'NIL)
                            (SETQ ALPHA
                                    (REVAL1
                                     (AEVAL
                                      (LIST 'PLUS ALPHA1
                                            (LIST 'TIMES BETA ALPHA2)))
                                     T)))
                           (T (SETQ ALPHA ALPHA2)))
                          NIL)))
                       NIL))
                     (T
                      (PROGN
                       (COND
                        ((EQUAL I 'EXPT)
                         (PROGN (SETQ ALPHA (CADDR VAR)) (SETQ EXPT_FLAG 'T))))
                       NIL)))
                    NIL))
                 (CAR I))
                (SETQ I (CDR I))
                (GO LAB))
              (COND ((EQUAL EXPT_FLAG NIL) (RETURN (CONS U V)))
                    (T
                     (PROGN
                      (COND
                       ((LISTP ALPHA)
                        (PROGN
                         (SETQ M (CADR ALPHA))
                         (SETQ N (CADDR ALPHA))
                         (SETQ N (REVAL1 (AEVAL (LIST 'TIMES BETA N)) T))))
                       (T (PROGN (SETQ M ALPHA) (SETQ N BETA))))
                      (SETQ CONST
                              (REVAL1
                               (AEVAL (LIST 'SUB (LIST 'EQUAL VAR1 1) VAR)) T))
                      (SETQ CONST
                              (REVAL1
                               (AEVAL
                                (LIST 'QUOTIENT 1
                                      (LIST 'EXPT CONST (LIST 'QUOTIENT N M))))
                               T))
                      (SETQ NEW_K
                              (REVAL1
                               (AEVAL
                                (LIST 'QUOTIENT
                                      (LIST 'DIFFERENCE
                                            (LIST 'TIMES (LIST 'PLUS K 1) N) M)
                                      M))
                               T))
                      (SETQ COEF
                              (REVAL1
                               (AEVAL
                                (LIST 'TIMES (LIST 'QUOTIENT N M) COEF
                                      (LIST 'EXPT CONST (LIST 'PLUS K 1))))
                               T))
                      (SETQ VAR
                              (REVAL1
                               (AEVAL (LIST 'EXPT VAR (LIST 'QUOTIENT N M)))
                               T))
                      (RETURN
                       (LIST NEW_K (CAR V) (CADR V) (CADDR V) COEF VAR)))))
              NIL))))) 
(AEVAL 'NIL) 
(ENDMODULE) 
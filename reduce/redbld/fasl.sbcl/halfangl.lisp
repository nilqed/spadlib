(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HALFANGL)) 
(FLUID '(*GCD)) 
(EXPORTS (LIST 'HALFANGLE 'UNTAN)) 
(PUT 'TRANSFORM 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFORM 'DEFINED-ON-LINE '35) 
(PUT 'TRANSFORM 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'TRANSFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFORM (U X)
    (PROG (ZL TNARG SUBSTLIST)
      (SETQ ZL ZLIST)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (CAR (SETQ TNARG (TAN-FUNCTION-IN ZL)))
                (HALFANGLE-CONFUSION ZLIST (CADAR TNARG))))
          (RETURN NIL)))
        (PROGN
         (SETQ ZL (CDR TNARG))
         (SETQ TNARG (CAR TNARG))
         (COND
          ((EQCAR TNARG 'TAN)
           (SETQ SUBSTLIST (CONS (CONS (GENSYM) TNARG) SUBSTLIST)))
          (T
           (SETQ SUBSTLIST
                   (CONS
                    (CONS (GENSYM) (LIST 'QUOTIENT 1 (CONS 'TAN (CDR TNARG))))
                    SUBSTLIST))))
         (SETQ U (SUBST (CAAR SUBSTLIST) TNARG U)))
        (GO WHILELABEL))
      (RETURN
       (COND (SUBSTLIST (SIMP (SUBLIS SUBSTLIST (PREPSQ (HALFANGLE U X)))))
             (T (SIMP (PREPSQ (HALFANGLE U X)))))))) 
(PUT 'TAN-FUNCTION-IN 'NUMBER-OF-ARGS 1) 
(PUT 'TAN-FUNCTION-IN 'DEFINED-ON-LINE '59) 
(PUT 'TAN-FUNCTION-IN 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'TAN-FUNCTION-IN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAN-FUNCTION-IN (ZZ)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT (AND ZZ (NOT (EQCAR (CAR ZZ) 'TAN)) (NOT (EQCAR (CAR ZZ) 'COT))))
         (RETURN NIL)))
       (SETQ ZZ (CDR ZZ))
       (GO WHILELABEL))
     (COND ((NULL ZZ) (CONS NIL NIL)) (T ZZ)))) 
(PUT 'HALFANGLE-CONFUSION 'NUMBER-OF-ARGS 2) 
(PUT 'HALFANGLE-CONFUSION 'DEFINED-ON-LINE '65) 
(PUT 'HALFANGLE-CONFUSION 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'HALFANGLE-CONFUSION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HALFANGLE-CONFUSION (ZZ TNARG)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT
          (AND ZZ
               (OR (ATOM (CAR ZZ))
                   (NOT
                    (EQUAL TNARG
                           (PREPSQ (SIMP (LIST 'QUOTIENT (CADAR ZZ) 2))))))))
         (RETURN NIL)))
       (SETQ ZZ (CDR ZZ))
       (GO WHILELABEL))
     ZZ)) 
(PUT 'QUOTQQ 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTQQ 'DEFINED-ON-LINE '72) 
(PUT 'QUOTQQ 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'QUOTQQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTQQ (U1 V1) (MULTSQ U1 (INVSQ V1))) 
(PUT '*SUBTRQ 'NUMBER-OF-ARGS 2) 
(PUT '*SUBTRQ 'DEFINED-ON-LINE '75) 
(PUT '*SUBTRQ 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT '*SUBTRQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *SUBTRQ (U1 V1) (ADDSQ U1 (NEGSQ V1))) 
(PUT '*INT2QM 'NUMBER-OF-ARGS 1) 
(PUT '*INT2QM 'DEFINED-ON-LINE '78) 
(PUT '*INT2QM 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT '*INT2QM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *INT2QM (U1) (COND ((EQUAL U1 0) (CONS NIL 1)) (T (CONS U1 1)))) 
(PUT 'HALFANGLE 'NUMBER-OF-ARGS 2) 
(PUT 'HALFANGLE 'DEFINED-ON-LINE '81) 
(PUT 'HALFANGLE 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'HALFANGLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HALFANGLE (R X) (QUOTQQ (HFAGLF (CAR R) X) (HFAGLF (CDR R) X))) 
(PUT 'HFAGLF 'NUMBER-OF-ARGS 2) 
(PUT 'HFAGLF 'DEFINED-ON-LINE '88) 
(PUT 'HFAGLF 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'HFAGLF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HFAGLF (P X)
    (COND ((OR (ATOM P) (ATOM (CAR P))) (CONS P 1))
          (T
           (SUBS2Q
            (ADDSQ
             (MULTSQ (EXPTSQ (HFAGLK (CAAAR P) X) (CDAAR P))
                     (HFAGLF (CDAR P) X))
             (HFAGLF (CDR P) X)))))) 
(PUT 'HFAGLK 'NUMBER-OF-ARGS 2) 
(PUT 'HFAGLK 'DEFINED-ON-LINE '95) 
(PUT 'HFAGLK 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'HFAGLK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HFAGLK (K X)
    (PROG (KT)
      (COND
       ((OR (ATOM K) (NOT (MEMBER X (FLATTEN (CDR K)))))
        (RETURN (CONS (LIST (CONS (CONS K 1) 1)) 1))))
      (SETQ K (CONS (CAR K) (HFAGLARGS (CDR K) X)))
      (COND ((EQ (CADR K) 'PI) (RETURN (CONS (LIST (CONS (CONS K 1) 1)) 1))))
      (SETQ KT (SIMP (LIST 'TAN (LIST 'QUOTIENT (CADR K) 2))))
      (RETURN
       (COND
        ((EQUAL (CAR K) 'SIN)
         (QUOTQQ (MULTSQ (*INT2QM 2) KT) (ADDSQ (*INT2QM 1) (EXPTSQ KT 2))))
        ((EQUAL (CAR K) 'COS)
         (QUOTQQ (*SUBTRQ (*INT2QM 1) (EXPTSQ KT 2))
          (ADDSQ (*INT2QM 1) (EXPTSQ KT 2))))
        ((EQUAL (CAR K) 'TAN)
         (QUOTQQ (MULTSQ (*INT2QM 2) KT) (*SUBTRQ (*INT2QM 1) (EXPTSQ KT 2))))
        ((EQUAL (CAR K) 'COT)
         (QUOTQQ (*SUBTRQ (*INT2QM 1) (EXPTSQ KT 2)) (MULTSQ (*INT2QM 2) KT)))
        ((EQUAL (CAR K) 'SEC)
         (QUOTQQ (ADDSQ (*INT2QM 1) (EXPTSQ KT 2))
          (*SUBTRQ (*INT2QM 1) (EXPTSQ KT 2))))
        ((EQUAL (CAR K) 'CSC)
         (QUOTQQ (ADDSQ (*INT2QM 1) (EXPTSQ KT 2)) (MULTSQ (*INT2QM 2) KT)))
        ((EQUAL (CAR K) 'SINH)
         (QUOTQQ
          (*SUBTRQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))
          (MULTSQ (*INT2QM 2)
                  (CONS
                   (LIST
                    (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 1)
                          1))
                   1))))
        ((EQUAL (CAR K) 'COSH)
         (QUOTQQ
          (ADDSQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))
          (MULTSQ (*INT2QM 2)
                  (CONS
                   (LIST
                    (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 1)
                          1))
                   1))))
        ((EQUAL (CAR K) 'TANH)
         (QUOTQQ
          (*SUBTRQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))
          (ADDSQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))))
        ((EQUAL (CAR K) 'COTH)
         (QUOTQQ
          (ADDSQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))
          (*SUBTRQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))))
        ((EQUAL (CAR K) 'SECH)
         (QUOTQQ
          (MULTSQ (*INT2QM 2)
                  (CONS
                   (LIST
                    (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 1)
                          1))
                   1))
          (ADDSQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))))
        ((EQUAL (CAR K) 'CSCH)
         (QUOTQQ
          (MULTSQ (*INT2QM 2)
                  (CONS
                   (LIST
                    (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 1)
                          1))
                   1))
          (*SUBTRQ
           (CONS
            (LIST (CONS (GETPOWER (FKERN (CONS 'EXPT (CONS 'E (CDR K)))) 2) 1))
            1)
           (*INT2QM 1))))
        ((EQUAL (CAR K) 'ACOT)
         (CONS
          (LIST
           (CONS (GETPOWER (FKERN (LIST 'ATAN (LIST 'QUOTIENT 1 (CADR K)))) 1)
                 1))
          1))
        (T (CONS (LIST (CONS (CONS K 1) 1)) 1)))))) 
(PUT 'HFAGLARGS 'NUMBER-OF-ARGS 2) 
(PUT 'HFAGLARGS 'DEFINED-ON-LINE '149) 
(PUT 'HFAGLARGS 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'HFAGLARGS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HFAGLARGS (L X)
    (COND ((NULL L) NIL)
          (T (CONS (PREPSQ (HFAGLK (CAR L) X)) (HFAGLARGS (CDR L) X))))) 
(PUT 'UNTANF 'NUMBER-OF-ARGS 1) 
(PUT 'UNTANF 'DEFINED-ON-LINE '154) 
(PUT 'UNTANF 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'UNTANF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNTANF (X)
    (PROG (*GCD Y Z W)
      (COND ((OR (ATOM X) (ATOM (CAR X))) (RETURN (CONS X 1))))
      (SETQ Y (CAAAR X))
      (COND ((EQCAR Y 'INT) (ERROR1)))
      (SETQ Z (CDAAR X))
      (SETQ W (CONS 1 1))
      (SETQ Y
              (COND ((ATOM Y) (CONS (LIST (CONS (CONS Y 1) 1)) 1))
                    ((EQ (CAR Y) 'TAN)
                     (PROG (YY)
                       (SETQ YY (CONS (PREPSQ (UNTAN (SIMP (CADR Y)))) NIL))
                       (COND
                        ((EVENP Z)
                         (PROGN
                          (SETQ Z (QUOTIENT Z 2))
                          (RETURN
                           (SIMP
                            (LIST 'QUOTIENT
                                  (LIST 'PLUS
                                        (LIST 'MINUS
                                              (LIST 'COS
                                                    (CONS 'TIMES (CONS 2 YY))))
                                        1)
                                  (LIST 'PLUS
                                        (LIST 'COS (CONS 'TIMES (CONS 2 YY)))
                                        1))))))
                        ((EQUAL Z 1)
                         (RETURN
                          (SIMP
                           (LIST 'QUOTIENT
                                 (LIST 'PLUS
                                       (LIST 'MINUS
                                             (LIST 'COS
                                                   (CONS 'TIMES (CONS 2 YY))))
                                       1)
                                 (LIST 'SIN (CONS 'TIMES (CONS 2 YY)))))))
                        (T
                         (PROGN
                          (SETQ Z (QUOTIENT (DIFFERENCE Z 1) 2))
                          (SETQ W
                                  (SIMP
                                   (LIST 'QUOTIENT
                                         (LIST 'PLUS
                                               (LIST 'MINUS
                                                     (LIST 'COS
                                                           (CONS 'TIMES
                                                                 (CONS 2 YY))))
                                               1)
                                         (LIST 'SIN
                                               (CONS 'TIMES (CONS 2 YY))))))
                          (RETURN
                           (SIMP
                            (LIST 'QUOTIENT
                                  (LIST 'PLUS
                                        (LIST 'MINUS
                                              (LIST 'COS
                                                    (CONS 'TIMES (CONS 2 YY))))
                                        1)
                                  (LIST 'PLUS
                                        (LIST 'COS (CONS 'TIMES (CONS 2 YY)))
                                        1)))))))))
                    (T (SIMP Y))))
      (RETURN
       (ADDSQ (MULTSQ (MULTSQ (EXPTSQ Y Z) (UNTANF (CDAR X))) W)
              (UNTANF (CDR X)))))) 
(PUT 'UNTAN 'NUMBER-OF-ARGS 1) 
(PUT 'UNTAN 'DEFINED-ON-LINE '224) 
(PUT 'UNTAN 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'UNTAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNTAN (X)
    (PROG (Y)
      (SETQ Y
              (COSSQCHK
               (SINSQRDCHK
                (MULTSQ (UNTANF (CAR X)) (INVSQ (UNTANF (CDR X)))))))
      (RETURN
       (COND ((GREATERP (LENGTH (FLATTEN Y)) (LENGTH (FLATTEN X))) X) (T Y))))) 
(PUT 'SINSQRDCHK 'NUMBER-OF-ARGS 1) 
(PUT 'SINSQRDCHK 'DEFINED-ON-LINE '232) 
(PUT 'SINSQRDCHK 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'SINSQRDCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SINSQRDCHK (X) (MULTSQ (SINSQCHKF (CAR X)) (INVSQ (SINSQCHKF (CDR X))))) 
(PUT 'SINSQCHKF 'NUMBER-OF-ARGS 1) 
(PUT 'SINSQCHKF 'DEFINED-ON-LINE '235) 
(PUT 'SINSQCHKF 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'SINSQCHKF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SINSQCHKF (X)
    (PROG (Y Z W)
      (COND ((OR (ATOM X) (ATOM (CAR X))) (RETURN (CONS X 1))))
      (SETQ Y (CAAAR X))
      (SETQ Z (CDAAR X))
      (SETQ W (CONS 1 1))
      (SETQ Y
              (COND
               ((EQCAR Y 'SIN)
                (COND
                 ((EVENP Z)
                  (PROGN
                   (SETQ Z (QUOTIENT Z 2))
                   (SIMP
                    (LIST 'PLUS 1
                          (LIST 'MINUS (LIST 'EXPT (CONS 'COS (CDR Y)) 2))))))
                 ((EQUAL Z 1) (CONS (LIST (CONS (CONS Y 1) 1)) 1))
                 (T
                  (PROGN
                   (SETQ Z (QUOTIENT (DIFFERENCE Z 1) 2))
                   (SETQ W (CONS (LIST (CONS (CONS Y 1) 1)) 1))
                   (SIMP
                    (LIST 'PLUS 1
                          (LIST 'MINUS
                                (LIST 'EXPT (CONS 'COS (CDR Y)) 2))))))))
               (T (CONS (LIST (CONS (CONS Y 1) 1)) 1))))
      (RETURN
       (ADDSQ (MULTSQ (MULTSQ (EXPTSQ Y Z) (SINSQCHKF (CDAR X))) W)
              (SINSQCHKF (CDR X)))))) 
(PUT 'COSSQCHKF 'NUMBER-OF-ARGS 1) 
(PUT 'COSSQCHKF 'DEFINED-ON-LINE '255) 
(PUT 'COSSQCHKF 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'COSSQCHKF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COSSQCHKF (X)
    (PROG (Y Z W X1 X2)
      (COND ((OR (ATOM X) (ATOM (CAR X))) (RETURN (CONS X 1))))
      (SETQ Y (CAAAR X))
      (SETQ Z (CDAAR X))
      (SETQ W (CONS 1 1))
      (SETQ X1 (COSSQCHKF (CDAR X)))
      (SETQ X2 (COSSQCHKF (CDR X)))
      (SETQ X (ADDSQ (MULTSQ (CONS (LIST (CONS (CAAR X) 1)) 1) X1) X2))
      (SETQ Y
              (COND
               ((EQCAR Y 'COS)
                (COND
                 ((EVENP Z)
                  (PROGN
                   (SETQ Z (QUOTIENT Z 2))
                   (SIMP
                    (LIST 'PLUS 1
                          (LIST 'MINUS (LIST 'EXPT (CONS 'SIN (CDR Y)) 2))))))
                 ((EQUAL Z 1) (CONS (LIST (CONS (CONS Y 1) 1)) 1))
                 (T
                  (PROGN
                   (SETQ Z (QUOTIENT (DIFFERENCE Z 1) 2))
                   (SETQ W (CONS (LIST (CONS (CONS Y 1) 1)) 1))
                   (SIMP
                    (LIST 'PLUS 1
                          (LIST 'MINUS
                                (LIST 'EXPT (CONS 'SIN (CDR Y)) 2))))))))
               (T (CONS (LIST (CONS (CONS Y 1) 1)) 1))))
      (SETQ Y (ADDSQ (MULTSQ (MULTSQ (EXPTSQ Y Z) W) X1) X2))
      (RETURN (COND ((GREATERP (LENGTH Y) (LENGTH X)) X) (T Y))))) 
(PUT 'COSSQCHK 'NUMBER-OF-ARGS 1) 
(PUT 'COSSQCHK 'DEFINED-ON-LINE '278) 
(PUT 'COSSQCHK 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'COSSQCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COSSQCHK (X)
    (PROG (*GCD)
      (SETQ *GCD T)
      (RETURN (MULTSQ (COSSQCHKF (CAR X)) (INVSQ (COSSQCHKF (CDR X))))))) 
(PUT 'LROOTCHK 'NUMBER-OF-ARGS 2) 
(PUT 'LROOTCHK 'DEFINED-ON-LINE '284) 
(PUT 'LROOTCHK 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'LROOTCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LROOTCHK (L X)
    (COND ((NULL L) NIL) (T (OR (KROOTCHK (CAR L) X) (LROOTCHK (CDR L) X))))) 
(PUT 'KROOTCHK 'NUMBER-OF-ARGS 2) 
(PUT 'KROOTCHK 'DEFINED-ON-LINE '288) 
(PUT 'KROOTCHK 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'KROOTCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KROOTCHK (F X)
    (COND ((ATOM F) NIL)
          ((AND (EQUAL (CAR F) 'SQRT) (MEMBER X (FLATTEN (CDR F)))) T)
          ((AND (EQUAL (CAR F) 'EXPT) (NOT (ATOM (CADDR F)))
                (EQUAL (CAADDR F) 'QUOTIENT) (MEMBER X (FLATTEN (CADR F))))
           T)
          (T (LROOTCHK (CDR F) X)))) 
(PUT 'ROOTCHK1P 'NUMBER-OF-ARGS 2) 
(PUT 'ROOTCHK1P 'DEFINED-ON-LINE '298) 
(PUT 'ROOTCHK1P 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'ROOTCHK1P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROOTCHK1P (F X)
    (COND ((OR (ATOM F) (ATOM (CAR F))) NIL)
          (T
           (OR (KROOTCHK (CAAAR F) X) (ROOTCHK1P (CDAR F) X)
               (ROOTCHK1P (CDR F) X))))) 
(PUT 'ROOTCHECKP 'NUMBER-OF-ARGS 2) 
(PUT 'ROOTCHECKP 'DEFINED-ON-LINE '303) 
(PUT 'ROOTCHECKP 'DEFINED-IN-FILE 'INT/HALFANGL.RED) 
(PUT 'ROOTCHECKP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROOTCHECKP (F X) (OR (ROOTCHK1P (CAR F) X) (ROOTCHK1P (CDR F) X))) 
(ENDMODULE) 
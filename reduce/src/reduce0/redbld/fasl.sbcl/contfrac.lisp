(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CONTFRAC)) 
(PUT 'POLYNOMQQQ 'NUMBER-OF-ARGS 1) 
(PUT 'POLYNOMQQQ 'DEFINED-ON-LINE '38) 
(PUT 'POLYNOMQQQ 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'POLYNOMQQQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLYNOMQQQ (X)
    ((LAMBDA (XX)
       (COND ((FIXP XX) 1) ((NOT (ONEP (CDR (SETQ XX (CADR XX))))) NIL)
             (T
              (PROG (KERNS KERN AA VAR FFORM MVV DEGG)
                (SETQ FFORM (SFP (CAAAR (CAR XX))))
                (SETQ VAR (REVAL1 (CADR X) T))
                (COND
                 (FFORM
                  (PROGN
                   (SETQ XX (CAR XX))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (NEQ XX 1)) (RETURN NIL)))
                     (PROGN
                      (SETQ MVV (CAAAR XX))
                      (SETQ DEGG (CDAAR XX))
                      (SETQ XX (CDAR XX))
                      (COND
                       ((OR (ATOM MVV) (ATOM (CAR MVV)))
                        (PROGN
                         (COND
                          ((NOT (FREEOF MVV VAR))
                           (PROGN
                            (SETQ XX 1)
                            (SETQ KERNS (LIST (LIST 'SIN VAR))))))))
                       (T
                        (SETQ KERNS
                                (APPEND (APPEND (KERNELS MVV) (KERNELS DEGG))
                                        KERNS)))))
                     (GO WHILELABEL))))
                 (T (SETQ KERNS (KERNELS (*Q2F XX)))))
               AA
                (COND ((NULL KERNS) (RETURN 1)))
                (SETQ KERN (FIRST KERNS))
                (SETQ KERNS (CDR KERNS))
                (COND
                 ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                 (T (GO AA)))))))
     (REVAL1 (CAR X) NIL))) 
(PUT 'POLYNOMQQ 'PSOPFN 'POLYNOMQQQ) 
(PUT 'TTTTYPE_RATPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-ON-LINE '65) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'TTTTYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TTTTYPE_RATPOLY (U)
    ((LAMBDA (XX)
       (COND ((FIXP XX) 1) ((NOT (EQCAR XX '*SQ)) NIL)
             (T
              (AND
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CAR (CADR XX)) 1)) (REVAL1 (CADR U) T)))
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CDR (CADR XX)) 1))
                      (REVAL1 (CADR U) T)))))))
     (REVAL1 (CAR U) NIL))) 
(FLAG '(TYPE_RATPOLY) 'BOOLEAN) 
(PUT 'TYPE_RATPOLY 'PSOPFN 'TTTTYPE_RATPOLY) 
(PUT 'TYPE_RATPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'TYPE_RATPOLY 'DEFINED-ON-LINE '78) 
(PUT 'TYPE_RATPOLY 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'TYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPE_RATPOLY (F Z) (TTTTYPE_RATPOLY (LIST F Z))) 
(FLAG '(VARI) 'BOOLEAN) 
(PUT 'VARI 'NUMBER-OF-ARGS 1) 
(PUT 'VARI 'DEFINED-ON-LINE '88) 
(PUT 'VARI 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'VARI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARI (X) (IDP X)) 
(PUT 'POLYNOMIALP 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMIALP 'DEFINED-ON-LINE '91) 
(PUT 'POLYNOMIALP 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'POLYNOMIALP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMIALP (U X)
    (COND ((AND (EQUAL (DEN U) 1) (OR (FREEOF U X) (GEQ (DEG U X) 1))) T)
          (T NIL))) 
(FLAG '(POLYNOMIALP) 'BOOLEAN) 
(SWITCH (LIST 'CF_TAYLOR)) 
(PUT 'CF_TAYLOR 'SIMPFG '((T (ON1 'TAYLORAUTOEXPAND)))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'CFRAC)) 
(PUT 'A_CONSTANT 'NUMBER-OF-ARGS 1) 
(FLAG '(A_CONSTANT) 'OPFN) 
(PUT 'A_CONSTANT 'DEFINED-ON-LINE '104) 
(PUT 'A_CONSTANT 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'A_CONSTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE A_CONSTANT (X) (CONSTANT_EXPRP X)) 
(SETK 'CFRACRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'CFRAC (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'CONTINUED_FRACTION 'X)
                         (LIST 'A_CONSTANT 'X)))
             (LIST 'REPLACEBY (LIST 'CFRAC (LIST '~ 'X) (LIST '~ 'S))
                   (LIST 'WHEN (LIST 'CONTINUED_FRACTION 'X 0 'S)
                         (LIST 'AND (LIST 'A_CONSTANT 'X) (LIST 'NUMBERP 'S))))
             (LIST 'REPLACEBY (LIST 'CFRAC (LIST '~ 'X) (LIST '~ 'S))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'CF 'PT2 'RES 'PT2R)
                               (LIST 'SETQ 'CF
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CFRACALL ''X ''S)))
                               (LIST 'SETQ 'PT2
                                     (LIST 'AEVAL (LIST 'LIST ''PART 'CF 2)))
                               (LIST 'SETQ 'RES
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'TYPE_RATPOLY
                                                  (LIST 'REVALX ''X)
                                                  (LIST 'REVALX ''S))
                                            (LIST 'PROG
                                                  (LIST 'R 'FORALL-RESULT
                                                        'FORALL-ENDPTR)
                                                  (LIST 'SETQ 'R 2)
                                                  (LIST 'COND
                                                        (LIST
                                                         (LIST '|AMINUSP:|
                                                               (LIST 'LIST
                                                                     ''DIFFERENCE
                                                                     (LIST
                                                                      'AEVAL*
                                                                      (LIST
                                                                       'LIST
                                                                       ''LENGTH
                                                                       'PT2))
                                                                     'R))
                                                         (LIST 'RETURN
                                                               (LIST 'MAKELIST
                                                                     NIL))))
                                                  (LIST 'SETQ 'FORALL-RESULT
                                                        (LIST 'SETQ
                                                              'FORALL-ENDPTR
                                                              (LIST 'CONS
                                                                    (LIST
                                                                     'AEVAL*
                                                                     (LIST
                                                                      'LIST
                                                                      ''LIST 1
                                                                      (LIST
                                                                       'LIST
                                                                       ''PART
                                                                       'PT2
                                                                       'R)))
                                                                    NIL)))
                                                  'LOOPLABEL
                                                  (LIST 'SETQ 'R
                                                        (LIST
                                                         (LIST 'LAMBDA
                                                               (LIST
                                                                'FORALL-RESULT)
                                                               (LIST 'AEVAL*
                                                                     (LIST
                                                                      'LIST
                                                                      ''PLUS
                                                                      'FORALL-RESULT
                                                                      1)))
                                                         'R))
                                                  (LIST 'COND
                                                        (LIST
                                                         (LIST '|AMINUSP:|
                                                               (LIST 'LIST
                                                                     ''DIFFERENCE
                                                                     (LIST
                                                                      'AEVAL*
                                                                      (LIST
                                                                       'LIST
                                                                       ''LENGTH
                                                                       'PT2))
                                                                     'R))
                                                         (LIST 'RETURN
                                                               (LIST 'CONS
                                                                     ''LIST
                                                                     'FORALL-RESULT))))
                                                  (LIST 'RPLACD 'FORALL-ENDPTR
                                                        (LIST 'CONS
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LIST
                                                                          1
                                                                          (LIST
                                                                           'LIST
                                                                           ''PART
                                                                           'PT2
                                                                           'R)))
                                                              NIL))
                                                  (LIST 'SETQ 'FORALL-ENDPTR
                                                        (LIST 'CDR
                                                              'FORALL-ENDPTR))
                                                  (LIST 'GO 'LOOPLABEL)))
                                           (LIST 'T
                                                 (LIST 'PROG
                                                       (LIST 'R 'FORALL-RESULT
                                                             'FORALL-ENDPTR)
                                                       (LIST 'SETQ 'R 2)
                                                       (LIST 'COND
                                                             (LIST
                                                              (LIST '|AMINUSP:|
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'AEVAL*
                                                                           (LIST
                                                                            'LIST
                                                                            ''LENGTH
                                                                            'PT2))
                                                                          'R))
                                                              (LIST 'RETURN
                                                                    (LIST
                                                                     'MAKELIST
                                                                     NIL))))
                                                       (LIST 'SETQ
                                                             'FORALL-RESULT
                                                             (LIST 'SETQ
                                                                   'FORALL-ENDPTR
                                                                   (LIST 'CONS
                                                                         (LIST
                                                                          'PROGN
                                                                          (LIST
                                                                           'SETQ
                                                                           'PT2R
                                                                           (LIST
                                                                            'AEVAL*
                                                                            (LIST
                                                                             'LIST
                                                                             ''PART
                                                                             'PT2
                                                                             'R)))
                                                                          (LIST
                                                                           'AEVAL*
                                                                           (LIST
                                                                            'LIST
                                                                            ''LIST
                                                                            (LIST
                                                                             'LIST
                                                                             ''NUM
                                                                             'PT2R)
                                                                            (LIST
                                                                             'LIST
                                                                             ''DEN
                                                                             'PT2R))))
                                                                         NIL)))
                                                       'LOOPLABEL
                                                       (LIST 'SETQ 'R
                                                             (LIST
                                                              (LIST 'LAMBDA
                                                                    (LIST
                                                                     'FORALL-RESULT)
                                                                    (LIST
                                                                     'AEVAL*
                                                                     (LIST
                                                                      'LIST
                                                                      ''PLUS
                                                                      'FORALL-RESULT
                                                                      1)))
                                                              'R))
                                                       (LIST 'COND
                                                             (LIST
                                                              (LIST '|AMINUSP:|
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'AEVAL*
                                                                           (LIST
                                                                            'LIST
                                                                            ''LENGTH
                                                                            'PT2))
                                                                          'R))
                                                              (LIST 'RETURN
                                                                    (LIST 'CONS
                                                                          ''LIST
                                                                          'FORALL-RESULT))))
                                                       (LIST 'RPLACD
                                                             'FORALL-ENDPTR
                                                             (LIST 'CONS
                                                                   (LIST 'PROGN
                                                                         (LIST
                                                                          'SETQ
                                                                          'PT2R
                                                                          (LIST
                                                                           'AEVAL*
                                                                           (LIST
                                                                            'LIST
                                                                            ''PART
                                                                            'PT2
                                                                            'R)))
                                                                         (LIST
                                                                          'AEVAL*
                                                                          (LIST
                                                                           'LIST
                                                                           ''LIST
                                                                           (LIST
                                                                            'LIST
                                                                            ''NUM
                                                                            'PT2R)
                                                                           (LIST
                                                                            'LIST
                                                                            ''DEN
                                                                            'PT2R))))
                                                                   NIL))
                                                       (LIST 'SETQ
                                                             'FORALL-ENDPTR
                                                             (LIST 'CDR
                                                                   'FORALL-ENDPTR))
                                                       (LIST 'GO
                                                             'LOOPLABEL)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CONTFRAC ''X
                                                 (LIST 'LIST ''PART 'CF 1)
                                                 (LIST 'LIST ''CONS
                                                       (LIST 'LIST ''PART 'PT2
                                                             1)
                                                       'RES)))))
                         (LIST 'AND (LIST 'NOT (LIST 'NUMBERP 'X))
                               (LIST 'VARI 'S))))
             (LIST 'REPLACEBY
                   (LIST 'CFRAC (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'C))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'CF 'PT2 'RES)
                               (LIST 'SETQ 'CF
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CFRAC_RATPOLY ''A ''B
                                                 ''C)))
                               (LIST 'SETQ 'PT2
                                     (LIST 'AEVAL (LIST 'LIST ''PART 'CF 2)))
                               (LIST 'SETQ 'RES
                                     (LIST 'PROG
                                           (LIST 'Q 'FORALL-RESULT
                                                 'FORALL-ENDPTR)
                                           (LIST 'SETQ 'Q 2)
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'PT2))
                                                              'Q))
                                                  (LIST 'RETURN
                                                        (LIST 'MAKELIST NIL))))
                                           (LIST 'SETQ 'FORALL-RESULT
                                                 (LIST 'SETQ 'FORALL-ENDPTR
                                                       (LIST 'CONS
                                                             (LIST 'AEVAL*
                                                                   (LIST 'LIST
                                                                         ''LIST
                                                                         1
                                                                         (LIST
                                                                          'LIST
                                                                          ''PART
                                                                          'PT2
                                                                          'Q)))
                                                             NIL)))
                                           'LOOPLABEL
                                           (LIST 'SETQ 'Q
                                                 (LIST
                                                  (LIST 'LAMBDA
                                                        (LIST 'FORALL-RESULT)
                                                        (LIST 'AEVAL*
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    'FORALL-RESULT
                                                                    1)))
                                                  'Q))
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'PT2))
                                                              'Q))
                                                  (LIST 'RETURN
                                                        (LIST 'CONS ''LIST
                                                              'FORALL-RESULT))))
                                           (LIST 'RPLACD 'FORALL-ENDPTR
                                                 (LIST 'CONS
                                                       (LIST 'AEVAL*
                                                             (LIST 'LIST ''LIST
                                                                   1
                                                                   (LIST 'LIST
                                                                         ''PART
                                                                         'PT2
                                                                         'Q)))
                                                       NIL))
                                           (LIST 'SETQ 'FORALL-ENDPTR
                                                 (LIST 'CDR 'FORALL-ENDPTR))
                                           (LIST 'GO 'LOOPLABEL)))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CONTFRAC ''A
                                                 (LIST 'LIST ''PART 'CF 1)
                                                 (LIST 'LIST ''CONS
                                                       (LIST 'LIST ''PART 'PT2
                                                             1)
                                                       'RES)))))
                         (LIST 'AND (LIST 'NUMBERP 'C) (LIST 'VARI 'B)
                               (LIST 'TYPE_RATPOLY 'A 'B))))
             (LIST 'REPLACEBY
                   (LIST 'CFRAC (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'C))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'CF 'PT2 'PT2R 'RES)
                               (LIST 'SETQ 'CF
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CFRAC_NONRATPOLY ''A
                                                 ''B ''C)))
                               (LIST 'SETQ 'PT2
                                     (LIST 'AEVAL (LIST 'LIST ''PART 'CF 2)))
                               (LIST 'SETQ 'RES
                                     (LIST 'PROG
                                           (LIST 'R 'FORALL-RESULT
                                                 'FORALL-ENDPTR)
                                           (LIST 'SETQ 'R 2)
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'PT2))
                                                              'R))
                                                  (LIST 'RETURN
                                                        (LIST 'MAKELIST NIL))))
                                           (LIST 'SETQ 'FORALL-RESULT
                                                 (LIST 'SETQ 'FORALL-ENDPTR
                                                       (LIST 'CONS
                                                             (LIST 'PROGN
                                                                   (LIST 'SETQ
                                                                         'PT2R
                                                                         (LIST
                                                                          'AEVAL*
                                                                          (LIST
                                                                           'LIST
                                                                           ''PART
                                                                           'PT2
                                                                           'R)))
                                                                   (LIST
                                                                    'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LIST
                                                                          (LIST
                                                                           'LIST
                                                                           ''NUM
                                                                           'PT2R)
                                                                          (LIST
                                                                           'LIST
                                                                           ''DEN
                                                                           'PT2R))))
                                                             NIL)))
                                           'LOOPLABEL
                                           (LIST 'SETQ 'R
                                                 (LIST
                                                  (LIST 'LAMBDA
                                                        (LIST 'FORALL-RESULT)
                                                        (LIST 'AEVAL*
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    'FORALL-RESULT
                                                                    1)))
                                                  'R))
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'PT2))
                                                              'R))
                                                  (LIST 'RETURN
                                                        (LIST 'CONS ''LIST
                                                              'FORALL-RESULT))))
                                           (LIST 'RPLACD 'FORALL-ENDPTR
                                                 (LIST 'CONS
                                                       (LIST 'PROGN
                                                             (LIST 'SETQ 'PT2R
                                                                   (LIST
                                                                    'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''PART
                                                                          'PT2
                                                                          'R)))
                                                             (LIST 'AEVAL*
                                                                   (LIST 'LIST
                                                                         ''LIST
                                                                         (LIST
                                                                          'LIST
                                                                          ''NUM
                                                                          'PT2R)
                                                                         (LIST
                                                                          'LIST
                                                                          ''DEN
                                                                          'PT2R))))
                                                       NIL))
                                           (LIST 'SETQ 'FORALL-ENDPTR
                                                 (LIST 'CDR 'FORALL-ENDPTR))
                                           (LIST 'GO 'LOOPLABEL)))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''CONTFRAC ''A
                                                 (LIST 'LIST ''PART 'CF 1)
                                                 (LIST 'LIST ''CONS
                                                       (LIST 'LIST ''PART 'PT2
                                                             1)
                                                       'RES)))))
                         (LIST 'AND (LIST 'NUMBERP 'C) (LIST 'VARI 'B)
                               (LIST 'NOT (LIST 'TYPE_RATPOLY 'A 'B)))))))) 
(LET '(CFRACRULES)) 
(LOAD_PACKAGE (LIST 'TAYLOR)) 
(LOAD_PACKAGE (LIST 'TPS)) 
(PUT 'LONGDIV 'NUMBER-OF-ARGS 3) 
(FLAG '(LONGDIV) 'OPFN) 
(PUT 'LONGDIV 'DEFINED-ON-LINE '160) 
(PUT 'LONGDIV 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'LONGDIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LONGDIV (POLY1 POLY2 X)
    (PROG (NUMER DENOM DIV DIV_LIST REM ANSWER)
      (SETQ NUMER (AEVAL POLY1))
      (SETQ DENOM (AEVAL POLY2))
      (SETQ DIV_LIST (AEVAL (LIST 'LIST)))
      (SETQ DIV (AEVAL 0))
      (SETQ REM (AEVAL 1))
      (SETQ ANSWER (AEVAL 0))
      (COND
       ((EVALLESSP (AEVAL (LIST 'LONGDIVDEG NUMER X))
                   (AEVAL (LIST 'LONGDIVDEG DENOM X)))
        (SETQ REM (AEVAL NUMER)))
       (T
        (PROGN
         (WHILE
          (AND
           (EVALGEQ (AEVAL* (LIST 'LONGDIVDEG NUMER X))
                    (AEVAL* (LIST 'LONGDIVDEG DENOM X)))
           (EVALNEQ (AEVAL* REM) 0))
          (PROGN
           (COND
            ((EVALEQUAL (AEVAL* (LIST 'LONGDIVLTERM NUMER X)) 0)
             (PROGN
              (SETQ DIV (AEVAL* (LIST 'QUOTIENT NUMER DENOM)))
              (SETQ REM (AEVAL* 0))
              (AEVAL* 'NIL)))
            (T
             (PROGN
              (SETQ DIV
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'LONGDIVLTERM NUMER X)
                             (LIST 'LONGDIVLTERM DENOM X))))
              (SETQ NUMER
                      (AEVAL*
                       (LIST 'DIFFERENCE NUMER (LIST 'TIMES DENOM DIV))))
              (SETQ REM (AEVAL* NUMER))
              (AEVAL* 'NIL))))
           (SETQ DIV_LIST (AEVAL* (LIST 'CONS DIV DIV_LIST)))
           (AEVAL* 'NIL)))
         (SETQ ANSWER
                 (PROG (ELMT FORALL-RESULT)
                   (SETQ ELMT (GETRLIST (AEVAL DIV_LIST)))
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((NULL ELMT) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  ((LAMBDA (ELMT) (AEVAL ELMT)) (CAR ELMT))
                                  FORALL-RESULT)))
                   (SETQ ELMT (CDR ELMT))
                   (GO LAB1)))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'LIST ANSWER REM))))) 
(PUT 'LONGDIVDEG 'NUMBER-OF-ARGS 2) 
(FLAG '(LONGDIVDEG) 'OPFN) 
(PUT 'LONGDIVDEG 'DEFINED-ON-LINE '190) 
(PUT 'LONGDIVDEG 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'LONGDIVDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LONGDIVDEG (I_P I_P_VAR)
    (PROG (A)
      (SETQ A
              (COND
               ((EVALNUMBERP (AEVAL (LIST 'DEN I_P)))
                (AEVAL
                 (LIST 'DEG (LIST 'TIMES I_P (LIST 'DEN I_P)) I_P_VAR)))))
      (RETURN (AEVAL A)))) 
(PUT 'LONGDIVLTERM 'NUMBER-OF-ARGS 2) 
(FLAG '(LONGDIVLTERM) 'OPFN) 
(PUT 'LONGDIVLTERM 'DEFINED-ON-LINE '197) 
(PUT 'LONGDIVLTERM 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'LONGDIVLTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LONGDIVLTERM (I_P I_P_VAR)
    (PROG (B)
      (SETQ B
              (COND
               ((EVALNUMBERP (AEVAL (LIST 'DEN I_P)))
                (AEVAL
                 (LIST 'QUOTIENT
                       (LIST 'LTERM (LIST 'TIMES (LIST 'DEN I_P) I_P) I_P_VAR)
                       (LIST 'DEN I_P))))))
      (RETURN (AEVAL B)))) 
(PUT 'CFRACALL 'NUMBER-OF-ARGS 2) 
(FLAG '(CFRACALL) 'OPFN) 
(PUT 'CFRACALL 'DEFINED-ON-LINE '204) 
(PUT 'CFRACALL 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'CFRACALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRACALL (RAT_POLY VAR)
    (PROG (TOP_POLY BOT_POLY EUCLIDSLIST LD_RETURN)
      (COND
       ((TYPE_RATPOLY (REVALX RAT_POLY) (REVALX VAR))
        (PROGN
         (SETQ TOP_POLY (AEVAL (LIST 'NUM RAT_POLY)))
         (SETQ BOT_POLY (AEVAL (LIST 'DEN RAT_POLY)))
         (SETQ EUCLIDSLIST (AEVAL (LIST 'LIST)))
         (SETQ LD_RETURN (AEVAL (LIST 'LONGDIV TOP_POLY BOT_POLY VAR)))
         (WHILE (EVALNEQ (AEVAL* (LIST 'PART LD_RETURN 2)) 0)
                (PROGN
                 (SETQ TOP_POLY (AEVAL* BOT_POLY))
                 (SETQ BOT_POLY (AEVAL* (LIST 'PART LD_RETURN 2)))
                 (SETQ EUCLIDSLIST
                         (AEVAL*
                          (LIST 'CONS (LIST 'PART LD_RETURN 1) EUCLIDSLIST)))
                 (SETQ LD_RETURN
                         (AEVAL* (LIST 'LONGDIV TOP_POLY BOT_POLY VAR)))
                 (AEVAL* 'NIL)))
         (SETQ EUCLIDSLIST
                 (AEVAL (LIST 'CONS (LIST 'PART LD_RETURN 1) EUCLIDSLIST)))
         (SETQ EUCLIDSLIST (AEVAL (LIST 'REVERSE EUCLIDSLIST)))
         (RETURN
          (AEVAL (LIST 'LIST (LIST 'INV_CFRACALL EUCLIDSLIST) EUCLIDSLIST)))
         (AEVAL 'NIL)))
       (T (RETURN (AEVAL (LIST 'CFRAC_NONRATPOLY RAT_POLY VAR 5))))))) 
(PUT 'CFRAC_RATPOLY 'NUMBER-OF-ARGS 3) 
(FLAG '(CFRAC_RATPOLY) 'OPFN) 
(PUT 'CFRAC_RATPOLY 'DEFINED-ON-LINE '228) 
(PUT 'CFRAC_RATPOLY 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'CFRAC_RATPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CFRAC_RATPOLY (RAT_POLY VAR NUMBER)
    (PROG (TOP_POLY BOT_POLY EUCLIDSLIST LD_RETURN K)
      (COND
       ((TYPE_RATPOLY (REVALX RAT_POLY) (REVALX VAR))
        (PROGN
         (SETQ TOP_POLY (AEVAL (LIST 'NUM RAT_POLY)))
         (SETQ BOT_POLY (AEVAL (LIST 'DEN RAT_POLY)))
         (SETQ EUCLIDSLIST (AEVAL (LIST 'LIST)))
         (SETQ K (AEVAL NUMBER))
         (SETQ LD_RETURN (AEVAL (LIST 'LONGDIV TOP_POLY BOT_POLY VAR)))
         (WHILE
          (AND (EVALNEQ (AEVAL* (LIST 'PART LD_RETURN 2)) 0)
               (EVALNEQ (AEVAL* K) 0))
          (PROGN
           (SETQ TOP_POLY (AEVAL* BOT_POLY))
           (SETQ BOT_POLY (AEVAL* (LIST 'PART LD_RETURN 2)))
           (SETQ EUCLIDSLIST
                   (AEVAL* (LIST 'CONS (LIST 'PART LD_RETURN 1) EUCLIDSLIST)))
           (SETQ LD_RETURN (AEVAL* (LIST 'LONGDIV TOP_POLY BOT_POLY VAR)))
           (SETQ K (AEVAL* (LIST 'DIFFERENCE K 1)))
           (AEVAL* 'NIL)))
         (SETQ EUCLIDSLIST
                 (AEVAL (LIST 'CONS (LIST 'PART LD_RETURN 1) EUCLIDSLIST)))
         (SETQ EUCLIDSLIST (AEVAL (LIST 'REVERSE EUCLIDSLIST)))
         (RETURN
          (AEVAL (LIST 'LIST (LIST 'INV_CFRACALL EUCLIDSLIST) EUCLIDSLIST)))
         (AEVAL 'NIL)))
       (T (RETURN (AEVAL (LIST 'CFRAC_NONRATPOLY RAT_POLY VAR NUMBER))))))) 
(PUT 'CFRAC_NONRATPOLY 'NUMBER-OF-ARGS 3) 
(FLAG '(CFRAC_NONRATPOLY) 'OPFN) 
(PUT 'CFRAC_NONRATPOLY 'DEFINED-ON-LINE '254) 
(PUT 'CFRAC_NONRATPOLY 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'CFRAC_NONRATPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CFRAC_NONRATPOLY (NONRAT X N)
    (COND
     ((BOOLVALUE* (REVALX *CF_TAYLOR))
      (AEVAL (LIST 'CFRAC_NONRATPOLY_TAY NONRAT X N)))
     (T (AEVAL (LIST 'CFRAC_NONRATPOLY_TPS NONRAT X N))))) 
(PUT 'CFRAC_NONRATPOLY_TPS 'NUMBER-OF-ARGS 3) 
(FLAG '(CFRAC_NONRATPOLY_TPS) 'OPFN) 
(PUT 'CFRAC_NONRATPOLY_TPS 'DEFINED-ON-LINE '260) 
(PUT 'CFRAC_NONRATPOLY_TPS 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'CFRAC_NONRATPOLY_TPS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CFRAC_NONRATPOLY_TPS (NONRAT X N)
    (PROG (G A_0 A COEFF_LIST K H SEARCH_LIM PSORD)
      (SETQ G (AEVAL (LIST 'PS NONRAT X 0)))
      (SETQ K (AEVAL N))
      (SETQ PSORD (AEVAL (LIST 'PSORDER G)))
      (COND
       ((EVALLESSP (AEVAL PSORD) 0)
        (PROGN
         (SETQ A_0
                 (AEVAL
                  (LIST 'TIMES (LIST 'PSTERM G PSORD) (LIST 'EXPT X PSORD))))
         (SETQ H (AEVAL PSORD))))
       (T (PROGN (SETQ A_0 (AEVAL (LIST 'PSTERM G 0))) (SETQ H (AEVAL 0)))))
      (SETQ COEFF_LIST (AEVAL (LIST 'LIST A_0)))
      (WHILE (AND (EVALNEQ (AEVAL* G) 0) (EVALGREATERP (AEVAL* K) 0))
             (PROGN
              (SETQ A (AEVAL* 0))
              (SETQ SEARCH_LIM (AEVAL* (LIST 'PLUS H (LIST 'PSORDLIM))))
              (WHILE
               (AND (EVALEQUAL (AEVAL* A) 0)
                    (EVALLEQ (AEVAL* H) (AEVAL* SEARCH_LIM)))
               (PROGN
                (SETQ H (AEVAL* (LIST 'PLUS H 1)))
                (SETQ A (AEVAL* (LIST 'PSTERM G H)))
                (AEVAL* 'NIL)))
              (COND
               ((EVALNEQ (AEVAL* A) 0)
                (PROGN
                 (SETQ COEFF_LIST
                         (AEVAL*
                          (LIST 'CONS (LIST 'TIMES A (LIST 'EXPT X H))
                                COEFF_LIST)))
                 (SETQ G
                         (AEVAL*
                          (LIST 'PS
                                (LIST 'TIMES A
                                      (LIST 'QUOTIENT (LIST 'EXPT X H)
                                            (LIST 'DIFFERENCE G A_0)))
                                X 0)))
                 (SETQ A_0 (AEVAL* (LIST 'PSTERM G 0)))
                 (SETQ H (AEVAL* 0))
                 (AEVAL* 'NIL)))
               (T (SETQ G (AEVAL* 0))))
              (SETQ K (AEVAL* (LIST 'DIFFERENCE K 1)))
              (AEVAL* 'NIL)))
      (SETQ COEFF_LIST (AEVAL (LIST 'ADAPTCFRAC (LIST 'REVERSE COEFF_LIST))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'INV_CFRAC_NONRATPOLY2 COEFF_LIST) COEFF_LIST))))) 
(PUT 'CFRAC_NONRATPOLY_TAY 'NUMBER-OF-ARGS 3) 
(FLAG '(CFRAC_NONRATPOLY_TAY) 'OPFN) 
(PUT 'CFRAC_NONRATPOLY_TAY 'DEFINED-ON-LINE '300) 
(PUT 'CFRAC_NONRATPOLY_TAY 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'CFRAC_NONRATPOLY_TAY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CFRAC_NONRATPOLY_TAY (NONRAT X N)
    (PROG (G GG A_0 A CLIST COEFF_LIST K H DEGD)
      (SETQ G (AEVAL (LIST 'TAYLOR NONRAT X 0 (LIST 'TIMES 2 N))))
      (SETQ K (AEVAL N))
      (SETQ GG (AEVAL (LIST 'TAYLORTOSTANDARD G)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'TAYLORP GG)))
        (AEVAL (REDERR (REVALX "not yet implemented")))))
      (COND
       ((NOT (TYPE_RATPOLY (REVALX GG) (REVALX X)))
        (AEVAL (REDERR (REVALX "not yet implemented"))))
       (T
        (PROGN
         (SETQ DEGD (AEVAL (LIST 'DEG (LIST 'DEN GG) X)))
         (SETQ GG (AEVAL (LIST 'TIMES GG (LIST 'EXPT X DEGD)))))))
      (SETQ CLIST (AEVAL (LIST 'COEFF GG X)))
      (SETQ A_0
              (AEVAL (LIST 'QUOTIENT (LIST 'FIRST CLIST) (LIST 'EXPT X DEGD))))
      (SETQ CLIST (AEVAL (LIST 'REST CLIST)))
      (SETQ COEFF_LIST (AEVAL (LIST 'LIST A_0)))
      (WHILE (AND (EVALNEQ (AEVAL* G) 0) (EVALGREATERP (AEVAL* K) 0))
             (PROGN
              (SETQ A (AEVAL* 0))
              (SETQ H (AEVAL* (LIST 'MINUS DEGD)))
              (WHILE
               (AND (EVALEQUAL (AEVAL* A) 0)
                    (EVALGEQ (AEVAL* (LIST 'LENGTH CLIST)) 1))
               (PROGN
                (SETQ A (AEVAL* (LIST 'FIRST CLIST)))
                (SETQ CLIST (AEVAL* (LIST 'REST CLIST)))
                (SETQ H (AEVAL* (LIST 'PLUS H 1)))
                (AEVAL* 'NIL)))
              (COND
               ((EVALNEQ (AEVAL* A) 0)
                (PROGN
                 (SETQ COEFF_LIST
                         (AEVAL*
                          (LIST 'CONS (LIST 'TIMES A (LIST 'EXPT X H))
                                COEFF_LIST)))
                 (SETQ G
                         (AEVAL*
                          (LIST 'TAYLORCOMBINE
                                (LIST 'QUOTIENT
                                      (LIST 'TAYLOR
                                            (LIST 'TIMES A (LIST 'EXPT X H)) X
                                            0 (LIST 'TIMES 2 N))
                                      (LIST 'DIFFERENCE G A_0)))))
                 (SETQ GG (AEVAL* (LIST 'TAYLORTOSTANDARD G)))
                 (SETQ CLIST (AEVAL* (LIST 'COEFF GG X)))
                 (SETQ A_0 (AEVAL* (LIST 'FIRST CLIST)))
                 (SETQ CLIST (AEVAL* (LIST 'REST CLIST)))
                 (SETQ DEGD (AEVAL* 0))))
               (T (SETQ G (AEVAL* 0))))
              (SETQ K (AEVAL* (LIST 'DIFFERENCE K 1)))
              (AEVAL* 'NIL)))
      (SETQ COEFF_LIST (AEVAL (LIST 'ADAPTCFRAC (LIST 'REVERSE COEFF_LIST))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'INV_CFRAC_NONRATPOLY2 COEFF_LIST) COEFF_LIST))))) 
(PUT 'ADAPTCFRAC 'NUMBER-OF-ARGS 1) 
(FLAG '(ADAPTCFRAC) 'OPFN) 
(PUT 'ADAPTCFRAC 'DEFINED-ON-LINE '353) 
(PUT 'ADAPTCFRAC 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'ADAPTCFRAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADAPTCFRAC (L_LIST)
    (PROG (H L K N M NEW_LIST)
      (SETQ NEW_LIST (AEVAL (LIST 'LIST)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'LENGTH L_LIST)) 3) (RETURN (AEVAL L_LIST)))
       (T
        (PROGN
         (SETQ H (AEVAL (LIST 'FIRST L_LIST)))
         (SETQ L (AEVAL (LIST 'SECOND L_LIST)))
         (SETQ K (AEVAL 2))
         (WHILE (EVALGEQ (AEVAL* (LIST 'LENGTH L_LIST)) (AEVAL* K))
                (PROGN
                 (SETQ N (AEVAL* (LIST 'NUM L)))
                 (SETK 'D (AEVAL* (LIST 'DEN L)))
                 (SETQ NEW_LIST
                         (AEVAL* (LIST 'CONS (LIST 'QUOTIENT N 'D) NEW_LIST)))
                 (SETQ K (AEVAL* (LIST 'PLUS K 1)))
                 (COND
                  ((EVALGEQ (AEVAL* (LIST 'LENGTH L_LIST)) (AEVAL* K))
                   (PROGN
                    (SETQ L (AEVAL* (LIST 'PART L_LIST K)))
                    (SETQ L (AEVAL* (LIST 'TIMES 'D L))))))
                 (AEVAL* 'NIL)))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'CONS H (LIST 'REVERSE NEW_LIST)))))) 
(PUT 'INV_CFRAC_NONRATPOLY1 'NUMBER-OF-ARGS 1) 
(FLAG '(INV_CFRAC_NONRATPOLY1) 'OPFN) 
(PUT 'INV_CFRAC_NONRATPOLY1 'DEFINED-ON-LINE '377) 
(PUT 'INV_CFRAC_NONRATPOLY1 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'INV_CFRAC_NONRATPOLY1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INV_CFRAC_NONRATPOLY1 (C_LIST)
    (PROG (N J EXPAN)
      (SETQ J (AEVAL (LIST 'LENGTH C_LIST)))
      (COND
       ((EVALLESSP (AEVAL J) 3)
        (RETURN
         (PROG (M FORALL-RESULT)
           (SETQ M (GETRLIST (AEVAL C_LIST)))
           (SETQ FORALL-RESULT 0)
          LAB1
           (COND ((NULL M) (RETURN FORALL-RESULT)))
           (SETQ FORALL-RESULT
                   (AEVAL*
                    (LIST 'PLUS ((LAMBDA (M) (AEVAL M)) (CAR M))
                          FORALL-RESULT)))
           (SETQ M (CDR M))
           (GO LAB1))))
       (T
        (PROGN
         (PROG (K)
           (SETQ K (AEVAL* J))
          LAB
           (COND
            ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 2 K)))
             (RETURN NIL)))
           (PROGN
            (SETQ N (AEVAL* (LIST 'PART C_LIST K)))
            (COND ((EVALEQUAL (AEVAL* K) (AEVAL* J)) (SETQ EXPAN (AEVAL* N)))
                  (T
                   (SETQ EXPAN
                           (AEVAL* (LIST 'QUOTIENT N (LIST 'PLUS 1 EXPAN))))))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                    K))
           (GO LAB))
         (RETURN (AEVAL (LIST 'PLUS (LIST 'PART C_LIST 1) EXPAN)))
         (AEVAL 'NIL)))))) 
(PUT 'INV_CFRAC_NONRATPOLY2 'NUMBER-OF-ARGS 1) 
(FLAG '(INV_CFRAC_NONRATPOLY2) 'OPFN) 
(PUT 'INV_CFRAC_NONRATPOLY2 'DEFINED-ON-LINE '395) 
(PUT 'INV_CFRAC_NONRATPOLY2 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'INV_CFRAC_NONRATPOLY2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INV_CFRAC_NONRATPOLY2 (C_LIST)
    (PROG (N J EXPAN)
      (SETQ J (AEVAL (LIST 'LENGTH C_LIST)))
      (COND
       ((EVALLESSP (AEVAL J) 3)
        (RETURN
         (PROG (M FORALL-RESULT)
           (SETQ M (GETRLIST (AEVAL C_LIST)))
           (SETQ FORALL-RESULT 0)
          LAB1
           (COND ((NULL M) (RETURN FORALL-RESULT)))
           (SETQ FORALL-RESULT
                   (AEVAL*
                    (LIST 'PLUS ((LAMBDA (M) (AEVAL M)) (CAR M))
                          FORALL-RESULT)))
           (SETQ M (CDR M))
           (GO LAB1))))
       (T
        (PROGN
         (PROG (K)
           (SETQ K (AEVAL* J))
          LAB
           (COND
            ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 2 K)))
             (RETURN NIL)))
           (PROGN
            (SETQ N (AEVAL* (LIST 'PART C_LIST K)))
            (COND ((EVALEQUAL (AEVAL* K) (AEVAL* J)) (SETQ EXPAN (AEVAL* N)))
                  (T
                   (SETQ EXPAN
                           (AEVAL*
                            (LIST 'QUOTIENT (LIST 'NUM N)
                                  (LIST 'PLUS (LIST 'DEN N) EXPAN))))))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                    K))
           (GO LAB))
         (RETURN (AEVAL (LIST 'PLUS (LIST 'PART C_LIST 1) EXPAN)))
         (AEVAL 'NIL)))))) 
(PUT 'INV_CFRACALL 'NUMBER-OF-ARGS 1) 
(FLAG '(INV_CFRACALL) 'OPFN) 
(PUT 'INV_CFRACALL 'DEFINED-ON-LINE '413) 
(PUT 'INV_CFRACALL 'DEFINED-IN-FILE 'RATAPRX/CONTFRAC.RED) 
(PUT 'INV_CFRACALL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INV_CFRACALL (C_LIST)
    (PROG (ANS J)
      (SETQ J (AEVAL (LIST 'LENGTH C_LIST)))
      (COND ((EVALEQUAL (AEVAL J) 0) (RETURN (AEVAL (LIST 'LIST))))
            ((EVALEQUAL (AEVAL J) 1) (SETQ ANS (AEVAL (LIST 'PART C_LIST 1))))
            (T
             (PROGN
              (SETQ ANS (AEVAL (LIST 'PART C_LIST J)))
              (PROG (K)
                (SETQ K (AEVAL* (LIST 'DIFFERENCE J 1)))
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 1 K)))
                  (RETURN NIL)))
                (SETQ ANS
                        (AEVAL*
                         (LIST 'PLUS (LIST 'PART C_LIST K)
                               (LIST 'QUOTIENT 1 ANS))))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                         K))
                (GO LAB))
              (AEVAL 'NIL))))
      (RETURN (AEVAL ANS)))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RATALGO)) 
(PUT 'COMPLEXAPART 'NUMBER-OF-ARGS 2) 
(FLAG '(COMPLEXAPART) 'OPFN) 
(PUT 'COMPLEXAPART 'DEFINED-ON-LINE '30) 
(PUT 'COMPLEXAPART 'DEFINED-IN-FILE 'SPECFN/RATALGO.RED) 
(PUT 'COMPLEXAPART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPLEXAPART (F X)
    (PROG (*FACTOR *COMPLEX)
      (AEVAL (ON (LIST 'FACTOR 'COMPLEX)))
      (SETQ X (AEVAL (LIST 'PF F X)))
      (AEVAL (OFF (LIST 'FACTOR 'COMPLEX)))
      (RETURN (AEVAL X)))) 
(PUT 'RATALGO 'NUMBER-OF-ARGS 2) 
(FLAG '(RATALGO) 'OPFN) 
(PUT 'RATALGO 'DEFINED-ON-LINE '39) 
(PUT 'RATALGO 'DEFINED-IN-FILE 'SPECFN/RATALGO.RED) 
(PUT 'RATALGO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATALGO (P X)
    (PROG (AFP TT S AK D C J SS)
      (SETQ AFP (AEVAL (LIST 'COMPLEXAPART P X)))
      (SETQ S (AEVAL 0))
      (SETQ AK (AEVAL 0))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (ASSGNPRI (AEVAL " Rational Algorithm applied") NIL 'ONLY)))
      (PROG (TT)
        (SETQ TT (GETRLIST (AEVAL AFP)))
       LAB
        (COND ((NULL TT) (RETURN NIL)))
        ((LAMBDA (TT)
           (PROGN
            (COND
             ((FREEOF (REVALX TT) (REVALX X))
              (SETQ S (AEVAL (LIST 'PLUS S TT))))
             (T
              (PROGN
               (SETQ D (AEVAL (LIST 'QUOTIENT 1 TT)))
               (COND
                ((NOT (POLYNOMQ (REVALX D) (REVALX X)))
                 (PROGN
                  (COND
                   ((BOOLVALUE* (REVALX *TRACEFPS))
                    (ASSGNPRI (AEVAL " Rational Algorithm  failed") NIL
                              'ONLY)))
                  (SETQ S (AEVAL (MINUS 1))))))
               (COND
                ((NOT (EVALEQUAL (AEVAL S) (MINUS 1)))
                 (PROGN
                  (SETQ D (AEVAL (LIST 'QUOTIENT D (LIST 'LCOF D X))))
                  (SETQ C (AEVAL (LIST 'TIMES D TT)))
                  (SETQ J (AEVAL (LIST 'DEG D X)))
                  (SETQ D (AEVAL (LIST 'EXPT D (LIST 'QUOTIENT 1 J))))
                  (COND
                   ((NOT (POLYNOMQ (REVALX D) (REVALX X)))
                    (PROGN
                     (COND
                      ((BOOLVALUE* (REVALX *TRACEFPS))
                       (ASSGNPRI (AEVAL " Rational Algorithm  failed") NIL
                                 'ONLY)))
                     (SETQ AFP (AEVAL (LIST 'LIST)))
                     (SETQ D (AEVAL 12))
                     (SETQ S (AEVAL (MINUS 1))))))
                  (COND
                   ((EVALEQUAL (AEVAL D) (AEVAL X))
                    (SETQ S
                            (AEVAL
                             (LIST 'PLUS S
                                   (LIST 'QUOTIENT C (LIST 'EXPT D J))))))
                   (T
                    (PROGN
                     (SETQ SS (AEVAL (LIST 'LCOF D X)))
                     (SETQ D (AEVAL (LIST 'QUOTIENT D SS)))
                     (SETQ C (AEVAL (LIST 'QUOTIENT C SS)))
                     (SETK 'XK (AEVAL (LIST 'DIFFERENCE X D)))
                     (SETQ C
                             (AEVAL
                              (LIST 'TIMES C
                                    (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) J)
                                          (LIST 'EXPT 'XK J)))))
                     (SETQ AK
                             (AEVAL
                              (LIST 'PLUS AK
                                    (LIST 'TIMES C
                                          (LIST 'QUOTIENT
                                                (LIST 'SIMPLIFY_FACTORIAL
                                                      (LIST 'QUOTIENT
                                                            (LIST 'QUOTIENT
                                                                  (LIST
                                                                   'FACTORIAL
                                                                   (LIST 'PLUS
                                                                         J
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          'K
                                                                          1)))
                                                                  (LIST
                                                                   'FACTORIAL
                                                                   'K))
                                                            (LIST 'FACTORIAL
                                                                  (LIST
                                                                   'DIFFERENCE
                                                                   J 1))))
                                                (LIST 'EXPT 'XK 'K))))))
                     (AEVAL 'NIL))))))))))))
         (CAR TT))
        (SETQ TT (CDR TT))
        (GO LAB))
      (COND ((EVALEQUAL (AEVAL S) (MINUS 1)) (RETURN (MINUS 1))))
      (RETURN
       (SETQ S
               (AEVAL
                (LIST 'PLUS S
                      (LIST 'INFSUM (LIST 'TIMES AK (LIST 'EXPT X 'K)) 'K 0
                            'INFINITY))))))) 
(PUT 'FASTPART 'NUMBER-OF-ARGS 2) 
(PUT 'FASTPART 'DEFINED-ON-LINE '78) 
(PUT 'FASTPART 'DEFINED-IN-FILE 'SPECFN/RATALGO.RED) 
(PUT 'FASTPART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FASTPART (X N) (REVAL1 (NTH X (PLUS N 1)) T)) 
(FLAG '(FASTPART FASTLENGTH) 'OPFN) 
(PUT 'FASTLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'FASTLENGTH 'DEFINED-ON-LINE '83) 
(PUT 'FASTLENGTH 'DEFINED-IN-FILE 'SPECFN/RATALGO.RED) 
(PUT 'FASTLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FASTLENGTH (X) (DIFFERENCE (LENGTH X) 1)) 
(PUT 'AUXCOPY 'NUMBER-OF-ARGS 1) 
(PUT 'AUXCOPY 'DEFINED-ON-LINE '86) 
(PUT 'AUXCOPY 'DEFINED-IN-FILE 'SPECFN/RATALGO.RED) 
(PUT 'AUXCOPY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AUXCOPY (U)
    (COND ((PAIRP U) (CONS (AUXCOPY (CAR U)) (AUXCOPY (CDR U)))) (T U))) 
(COND ((GETD 'PRINT_FORMAT) (PRINT_FORMAT '(POCHHAMMER U V) '(|(| U |)| _ V)))) 
(ENDMODULE) 
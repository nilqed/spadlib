(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRIGINT)) 
(CREATE-PACKAGE '(TRIGINT) NIL) 
(GLOBAL '(*TRACETRIG)) 
(SWITCH (LIST 'TRACETRIG)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(LOAD_PACKAGE (LIST 'LIMITS)) 
(LOAD_PACKAGE (LIST 'MISC)) 
(PUT 'SUB_A 'NUMBER-OF-ARGS 2) 
(FLAG '(SUB_A) 'OPFN) 
(PUT 'SUB_A 'DEFINED-ON-LINE '52) 
(PUT 'SUB_A 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'SUB_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUB_A (EXP VAR)
    (LIST 'SUB
          (LIST 'LIST
                (LIST 'EQUAL (LIST 'SIN VAR)
                      (LIST 'TIMES 2
                            (LIST 'QUOTIENT 'U
                                  (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                (LIST 'EQUAL (LIST 'COS VAR)
                      (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2))
                            (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
          EXP)) 
(PUT 'SUB_B 'NUMBER-OF-ARGS 2) 
(FLAG '(SUB_B) 'OPFN) 
(PUT 'SUB_B 'DEFINED-ON-LINE '55) 
(PUT 'SUB_B 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'SUB_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUB_B (EXP VAR)
    (LIST 'SUB
          (LIST 'LIST
                (LIST 'EQUAL (LIST 'SIN VAR)
                      (LIST 'QUOTIENT (LIST 'DIFFERENCE (LIST 'EXPT 'U 2) 1)
                            (LIST 'PLUS (LIST 'EXPT 'U 2) 1)))
                (LIST 'EQUAL (LIST 'COS VAR)
                      (LIST 'TIMES 2
                            (LIST 'QUOTIENT 'U
                                  (LIST 'PLUS (LIST 'EXPT 'U 2) 1)))))
          EXP)) 
(PUT 'SUB_C 'NUMBER-OF-ARGS 2) 
(FLAG '(SUB_C) 'OPFN) 
(PUT 'SUB_C 'DEFINED-ON-LINE '59) 
(PUT 'SUB_C 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'SUB_C 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUB_C (EXP VAR)
    (LIST 'SUB
          (LIST 'LIST
                (LIST 'EQUAL (LIST 'SIN VAR)
                      (LIST 'TIMES 2
                            (LIST 'QUOTIENT 'U
                                  (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                (LIST 'EQUAL (LIST 'COS VAR)
                      (LIST 'QUOTIENT (LIST 'DIFFERENCE (LIST 'EXPT 'U 2) 1)
                            (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
          EXP)) 
(PUT 'SUB_D 'NUMBER-OF-ARGS 2) 
(FLAG '(SUB_D) 'OPFN) 
(PUT 'SUB_D 'DEFINED-ON-LINE '62) 
(PUT 'SUB_D 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'SUB_D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUB_D (EXP VAR)
    (LIST 'SUB
          (LIST 'LIST
                (LIST 'EQUAL (LIST 'SIN VAR)
                      (LIST 'QUOTIENT 'U
                            (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                (LIST 'EQUAL (LIST 'COS VAR)
                      (LIST 'QUOTIENT 1
                            (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))))
          EXP)) 
(PUT 'APPLY_A 'NUMBER-OF-ARGS 2) 
(FLAG '(APPLY_A) 'OPFN) 
(PUT 'APPLY_A 'DEFINED-ON-LINE '67) 
(PUT 'APPLY_A 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'APPLY_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLY_A (EXP VAR)
    (PROG (ANSWER RESULT)
      (SETQ ANSWER (AEVAL (LIST 'SUB_A EXP VAR)))
      (SETQ ANSWER
              (AEVAL
               (LIST 'TIMES ANSWER
                     (LIST 'QUOTIENT 2 (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))))
      (SETQ RESULT (AEVAL (LIST 'INT ANSWER 'U)))
      (SETQ RESULT
              (AEVAL
               (LIST 'SUB
                     (LIST 'LIST
                           (LIST 'EQUAL 'U (LIST 'TAN (LIST 'QUOTIENT VAR 2))))
                     RESULT)))
      (SETQ RESULT
              (AEVAL
               (LIST 'PLUS RESULT
                     (LIST 'TIMES (LIST 'TRIGINT_K RESULT VAR 'PI)
                           (LIST 'FLOOR
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE VAR 'PI)
                                       (LIST 'TIMES 2 'PI)))))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'APPLY_B 'NUMBER-OF-ARGS 2) 
(FLAG '(APPLY_B) 'OPFN) 
(PUT 'APPLY_B 'DEFINED-ON-LINE '77) 
(PUT 'APPLY_B 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'APPLY_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLY_B (EXP VAR)
    (PROG (ANSWER RESULT)
      (SETQ ANSWER (AEVAL (LIST 'SUB_B EXP VAR)))
      (SETQ ANSWER
              (AEVAL
               (LIST 'TIMES ANSWER
                     (LIST 'QUOTIENT 2 (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))))
      (SETQ RESULT (AEVAL (LIST 'INT ANSWER 'U)))
      (SETQ RESULT
              (AEVAL
               (LIST 'SUB
                     (LIST 'LIST
                           (LIST 'EQUAL 'U
                                 (LIST 'TAN
                                       (LIST 'PLUS (LIST 'QUOTIENT VAR 2)
                                             (LIST 'QUOTIENT 'PI 4)))))
                     RESULT)))
      (SETQ RESULT
              (AEVAL
               (LIST 'PLUS RESULT
                     (LIST 'TIMES
                           (LIST 'TRIGINT_K RESULT VAR (LIST 'QUOTIENT 'PI 2))
                           (LIST 'FLOOR
                                 (LIST 'QUOTIENT
                                       (LIST 'DIFFERENCE VAR
                                             (LIST 'QUOTIENT 'PI 2))
                                       (LIST 'TIMES 2 'PI)))))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'APPLY_C 'NUMBER-OF-ARGS 2) 
(FLAG '(APPLY_C) 'OPFN) 
(PUT 'APPLY_C 'DEFINED-ON-LINE '85) 
(PUT 'APPLY_C 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'APPLY_C 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLY_C (EXP VAR)
    (PROG (ANSWER RESULT)
      (SETQ ANSWER (AEVAL (LIST 'SUB_C EXP VAR)))
      (SETQ ANSWER
              (AEVAL
               (LIST 'TIMES ANSWER
                     (LIST 'MINUS
                           (LIST 'QUOTIENT 2
                                 (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))))
      (SETQ RESULT (AEVAL (LIST 'INT ANSWER 'U)))
      (SETQ RESULT
              (AEVAL
               (LIST 'SUB
                     (LIST 'LIST
                           (LIST 'EQUAL 'U
                                 (LIST 'QUOTIENT 1
                                       (LIST 'TAN (LIST 'QUOTIENT VAR 2)))))
                     RESULT)))
      (SETQ RESULT
              (AEVAL
               (LIST 'PLUS RESULT
                     (LIST 'TIMES (LIST 'TRIGINT_K RESULT VAR 0)
                           (LIST 'FLOOR
                                 (LIST 'QUOTIENT VAR (LIST 'TIMES 2 'PI)))))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'APPLY_D 'NUMBER-OF-ARGS 2) 
(FLAG '(APPLY_D) 'OPFN) 
(PUT 'APPLY_D 'DEFINED-ON-LINE '93) 
(PUT 'APPLY_D 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'APPLY_D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLY_D (EXP VAR)
    (PROG (ANSWER RESULT)
      (SETQ ANSWER (AEVAL (LIST 'SUB_D EXP VAR)))
      (SETQ ANSWER
              (AEVAL
               (LIST 'TIMES ANSWER
                     (LIST 'QUOTIENT 1 (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))))
      (SETQ RESULT (AEVAL (LIST 'INT ANSWER 'U)))
      (SETQ RESULT
              (AEVAL
               (LIST 'SUB (LIST 'LIST (LIST 'EQUAL 'U (LIST 'TAN VAR)))
                     RESULT)))
      (SETQ RESULT
              (AEVAL
               (LIST 'PLUS RESULT
                     (LIST 'TIMES
                           (LIST 'TRIGINT_K RESULT VAR (LIST 'QUOTIENT 'PI 2))
                           (LIST 'FLOOR
                                 (LIST 'QUOTIENT
                                       (LIST 'DIFFERENCE VAR
                                             (LIST 'QUOTIENT 'PI 2))
                                       'PI))))))
      (RETURN (AEVAL RESULT)))) 
(SETK 'TRIG_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SEC (LIST '~ 'X)) 2)
                   (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST 'COS 'X) 2)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'TAN (LIST '~ 'X)) 2)
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'SEC 'X) 2) 1))))) 
(PUT 'UNEVALP 'NUMBER-OF-ARGS 1) 
(FLAG '(UNEVALP) 'OPFN) 
(PUT 'UNEVALP 'DEFINED-ON-LINE '118) 
(PUT 'UNEVALP 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'UNEVALP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNEVALP (EXP)
    (PROG (FINISHED K)
      (SETQ K (AEVAL 0))
      (SETQ FINISHED (AEVAL 0))
      (WHILE
       (AND (EVALEQUAL (AEVAL* FINISHED) 0)
            (EVALLEQ (AEVAL* K) (AEVAL* (LIST 'ARGLENGTH EXP))))
       (PROGN
        (COND
         ((EVALEQUAL (AEVAL* (LIST 'PART EXP K)) (AEVAL* 'INT))
          (SETQ FINISHED (AEVAL* 1)))
         (T (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
        (AEVAL* 'NIL)))
      (COND ((EVALEQUAL (AEVAL FINISHED) 1) (RETURN (AEVAL 'T)))
            (T (AEVAL 'NIL))))) 
(PUT 'TRIGINT_K 'NUMBER-OF-ARGS 3) 
(FLAG '(TRIGINT_K) 'OPFN) 
(PUT 'TRIGINT_K 'DEFINED-ON-LINE '130) 
(PUT 'TRIGINT_K 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'TRIGINT_K 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIGINT_K (EXP VAR VAL)
    (LIST 'DIFFERENCE (LIST 'LIMIT- EXP VAR VAL) (LIST 'LIMIT+ EXP VAR VAL))) 
(PUT 'UNEVAL_INT 'NUMBER-OF-ARGS 1) 
(FLAG '(UNEVAL_INT) 'OPFN) 
(PUT 'UNEVAL_INT 'DEFINED-ON-LINE '136) 
(PUT 'UNEVAL_INT 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'UNEVAL_INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNEVAL_INT (EXP)
    (PROG (TEMP)
      (COND
       ((NOT (FREEOF (REVALX EXP) (REVALX 'INT)))
        (RETURN (SETQ TEMP (AEVAL 'T))))
       (T (SETQ TEMP (AEVAL 'NIL))))
      (RETURN (AEVAL TEMP)))) 
(PUT 'UNEVAL_LIM 'NUMBER-OF-ARGS 1) 
(FLAG '(UNEVAL_LIM) 'OPFN) 
(PUT 'UNEVAL_LIM 'DEFINED-ON-LINE '142) 
(PUT 'UNEVAL_LIM 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'UNEVAL_LIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNEVAL_LIM (EXP)
    (PROG (TEMP)
      (COND ((NOT (FREEOF (REVALX EXP) (REVALX 'LIMIT-))) (RETURN (AEVAL 'T)))
            (T
             (PROGN
              (COND
               ((NOT (FREEOF (REVALX EXP) (REVALX 'LIMIT+)))
                (RETURN (AEVAL 'T)))
               (T (AEVAL 'NIL)))
              (AEVAL 'NIL)))))) 
(PUT 'FAIL_A 'NUMBER-OF-ARGS 2) 
(FLAG '(FAIL_A) 'OPFN) 
(PUT 'FAIL_A 'DEFINED-ON-LINE '150) 
(PUT 'FAIL_A 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'FAIL_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAIL_A (EXP VAR)
    (PROG (TEMP)
      (SETQ TEMP (AEVAL (LIST 'APPLY_A EXP VAR)))
      (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_LIM TEMP))) (RETURN (AEVAL 'T)))
            (T
             (PROGN
              (COND
               ((BOOLVALUE* (REVALX (LIST 'UNEVAL_INT TEMP)))
                (RETURN (AEVAL 'T)))
               (T (RETURN (AEVAL 'NIL))))
              (AEVAL 'NIL)))))) 
(PUT 'FAIL_B 'NUMBER-OF-ARGS 2) 
(FLAG '(FAIL_B) 'OPFN) 
(PUT 'FAIL_B 'DEFINED-ON-LINE '160) 
(PUT 'FAIL_B 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'FAIL_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAIL_B (EXP VAR)
    (PROG (TEMP)
      (SETQ TEMP (AEVAL (LIST 'APPLY_B EXP VAR)))
      (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_LIM TEMP))) (RETURN (AEVAL 'T)))
            (T
             (PROGN
              (COND
               ((BOOLVALUE* (REVALX (LIST 'UNEVAL_INT TEMP)))
                (RETURN (AEVAL 'T)))
               (T (RETURN (AEVAL 'NIL))))
              (AEVAL 'NIL)))))) 
(PUT 'FAIL_C 'NUMBER-OF-ARGS 2) 
(FLAG '(FAIL_C) 'OPFN) 
(PUT 'FAIL_C 'DEFINED-ON-LINE '170) 
(PUT 'FAIL_C 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'FAIL_C 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAIL_C (EXP VAR)
    (PROG (TEMP)
      (SETQ TEMP (AEVAL (LIST 'APPLY_C EXP VAR)))
      (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_LIM TEMP))) (RETURN (AEVAL 'T)))
            (T
             (PROGN
              (COND
               ((BOOLVALUE* (REVALX (LIST 'UNEVAL_INT TEMP)))
                (RETURN (AEVAL 'T)))
               (T (RETURN (AEVAL 'NIL))))
              (AEVAL 'NIL)))))) 
(PUT 'FAIL_D 'NUMBER-OF-ARGS 2) 
(FLAG '(FAIL_D) 'OPFN) 
(PUT 'FAIL_D 'DEFINED-ON-LINE '180) 
(PUT 'FAIL_D 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'FAIL_D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAIL_D (EXP VAR)
    (PROG (TEMP)
      (SETQ TEMP (AEVAL (LIST 'APPLY_D EXP VAR)))
      (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_LIM TEMP))) (RETURN (AEVAL 'T)))
            (T
             (PROGN
              (COND
               ((BOOLVALUE* (REVALX (LIST 'UNEVAL_INT TEMP)))
                (RETURN (AEVAL 'T)))
               (T (RETURN (AEVAL 'NIL))))
              (AEVAL 'NIL)))))) 
(PUT 'FAIL 'NUMBER-OF-ARGS 1) 
(FLAG '(FAIL) 'OPFN) 
(PUT 'FAIL 'DEFINED-ON-LINE '190) 
(PUT 'FAIL 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'FAIL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FAIL (EXP)
    (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_LIM EXP))) (AEVAL 'T))
          (T
           (PROGN
            (COND ((BOOLVALUE* (REVALX (LIST 'UNEVAL_INT EXP))) (AEVAL 'T))
                  (T (AEVAL 'NIL)))
            (AEVAL 'NIL))))) 
(LET (LIST (LIST 'REPLACEBY (LIST 'LOG (MINUS 1)) (LIST 'TIMES 'I 'PI)))) 
(PUT 'TRIGINT 'NUMBER-OF-ARGS 2) 
(FLAG '(TRIGINT) 'OPFN) 
(PUT 'TRIGINT 'DEFINED-ON-LINE '200) 
(PUT 'TRIGINT 'DEFINED-IN-FILE 'TRIGINT/TRIGINT.RED) 
(PUT 'TRIGINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGINT (EXP VAR)
    (PROG (ANSWER ANSWER_1 ANSWER_2 ANSWER_3 ANSWER_4 RESULT)
      (COND
       ((AND (FREEOF (REVALX EXP) (REVALX 'SIN))
             (FREEOF (REVALX EXP) (REVALX 'COS))
             (FREEOF (REVALX EXP) (REVALX 'TAN)))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRACETRIG))
           (ASSGNPRI
            (AEVAL
             "expression free of sin, cos tan, proceeding with standard integration")
            NIL 'ONLY)))
         (RETURN (AEVAL (LIST 'INT EXP 'X)))
         (AEVAL 'NIL))))
      (AEVAL (ON (LIST 'USETAYLOR)))
      (COND
       ((FREEOF (REVALX EXP) (REVALX (LIST 'SIN VAR)))
        (PROGN
         (SETQ ANSWER (AEVAL (LIST 'APPLY_A EXP VAR)))
         (COND
          ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER)))
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *TRACETRIG))
              (ASSGNPRI
               (AEVAL "system can't integrate after substitution A,
              trying again")
               NIL 'ONLY)))
            (SETQ ANSWER_2 (AEVAL (LIST 'APPLY_B EXP VAR)))
            (COND
             ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_2)))
              (PROGN
               (COND
                ((BOOLVALUE* (REVALX *TRACETRIG))
                 (ASSGNPRI (AEVAL "trying again with substitution B") NIL
                           'ONLY)))
               (SETQ ANSWER_3 (AEVAL (LIST 'APPLY_C EXP VAR)))
               (COND
                ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_3)))
                 (PROGN
                  (COND
                   ((BOOLVALUE* (REVALX *TRACETRIG))
                    (ASSGNPRI (AEVAL "and again with substitution C") NIL
                              'ONLY)))
                  (SETQ ANSWER_4 (AEVAL (LIST 'APPLY_D EXP VAR)))
                  (COND
                   ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_4)))
                    (PROGN
                     (COND
                      ((BOOLVALUE* (REVALX *TRACETRIG))
                       (ASSGNPRI
                        (AEVAL
                         "failed in all attempts, system cannot integrate")
                        NIL 'ONLY)))
                     (RETURN (AEVAL ANSWER))
                     (AEVAL 'NIL)))
                   (T (RETURN (AEVAL ANSWER_4))))
                  (AEVAL 'NIL)))
                (T (RETURN (AEVAL ANSWER_3))))
               (AEVAL 'NIL)))
             (T (RETURN (AEVAL ANSWER_2))))
            (AEVAL 'NIL)))
          (T (RETURN (AEVAL ANSWER))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (COND
          ((FREEOF (REVALX EXP) (REVALX (LIST 'COS VAR)))
           (PROGN
            (SETQ ANSWER (AEVAL (LIST 'APPLY_B EXP VAR)))
            (COND
             ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER)))
              (PROGN
               (COND
                ((BOOLVALUE* (REVALX *TRACETRIG))
                 (ASSGNPRI
                  (AEVAL "failed with substitution B: system could not
                                integrate after subs, trying A")
                  NIL 'ONLY)))
               (SETQ ANSWER_2 (AEVAL (LIST 'APPLY_A EXP VAR)))
               (COND
                ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_2)))
                 (PROGN
                  (COND
                   ((BOOLVALUE* (REVALX *TRACETRIG))
                    (ASSGNPRI (AEVAL "failed with A: trying C now") NIL
                              'ONLY)))
                  (SETQ ANSWER_3 (AEVAL (LIST 'APPLY_C EXP VAR)))
                  (COND
                   ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_3)))
                    (PROGN
                     (COND
                      ((BOOLVALUE* (REVALX *TRACETRIG))
                       (ASSGNPRI (AEVAL "failed with C: trying D now") NIL
                                 'ONLY)))
                     (SETQ ANSWER_4 (AEVAL (LIST 'APPLY_D EXP VAR)))
                     (COND
                      ((BOOLVALUE* (REVALX *TRACETRIG))
                       (ASSGNPRI (AEVAL "trying all possible substitutions")
                                 NIL 'ONLY)))
                     (COND
                      ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_4)))
                       (AEVAL
                        (REDERR
                         (REVALX "system can't integrate after
                                                 subs"))))
                      (T (RETURN (AEVAL ANSWER_4))))
                     (AEVAL 'NIL)))
                   (T (RETURN (AEVAL ANSWER_3))))
                  (AEVAL 'NIL)))
                (T (RETURN (AEVAL ANSWER_2))))
               (AEVAL 'NIL)))
             (T (RETURN (AEVAL ANSWER))))
            (AEVAL 'NIL)))
          (T
           (PROGN
            (COND
             ((EVALEQUAL
               (AEVAL
                (LIST 'SUB
                      (LIST 'LIST
                            (LIST 'EQUAL (LIST 'SIN VAR)
                                  (LIST 'MINUS (LIST 'SIN VAR)))
                            (LIST 'EQUAL (LIST 'COS VAR)
                                  (LIST 'MINUS (LIST 'COS VAR))))
                      EXP))
               (AEVAL EXP))
              (PROGN
               (COND
                ((BOOLVALUE* (REVALX *TRACETRIG))
                 (ASSGNPRI
                  (AEVAL "using heuristics: G & R section 2.504 to integrate ")
                  NIL 'ONLY)))
               (SETQ ANSWER (AEVAL (LIST 'APPLY_D EXP VAR)))
               (COND
                ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER)))
                 (PROGN
                  (COND
                   ((BOOLVALUE* (REVALX *TRACETRIG))
                    (ASSGNPRI (AEVAL "subs D failed, trying now with A") NIL
                              'ONLY)))
                  (SETQ ANSWER_2 (AEVAL (LIST 'APPLY_A EXP VAR)))
                  (COND
                   ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_2)))
                    (PROGN
                     (COND
                      ((BOOLVALUE* (REVALX *TRACETRIG))
                       (ASSGNPRI (AEVAL "subs B falied, trying with sub C") NIL
                                 'ONLY)))
                     (SETQ ANSWER_3 (AEVAL (LIST 'APPLY_B EXP VAR)))
                     (COND
                      ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_3)))
                       (PROGN
                        (COND
                         ((BOOLVALUE* (REVALX *TRACETRIG))
                          (ASSGNPRI (AEVAL "sub C falied, trying sub D") NIL
                                    'ONLY)))
                        (SETQ ANSWER_4 (AEVAL (LIST 'APPLY_C EXP VAR)))
                        (COND
                         ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_4)))
                          (AEVAL
                           (REDERR (REVALX "can't integrate after subs"))))
                         (T (RETURN (AEVAL ANSWER_4))))
                        (AEVAL 'NIL)))
                      (T (RETURN (AEVAL ANSWER_3))))
                     (AEVAL 'NIL)))
                   (T (RETURN (AEVAL ANSWER_2))))
                  (AEVAL 'NIL)))
                (T (RETURN (AEVAL ANSWER))))
               (AEVAL 'NIL)))
             (T
              (PROGN
               (SETQ ANSWER (AEVAL (LIST 'APPLY_A EXP VAR)))
               (COND
                ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER)))
                 (PROGN
                  (COND
                   ((BOOLVALUE* (REVALX *TRACETRIG))
                    (ASSGNPRI
                     (AEVAL "not using heuristics,
         attempting subs in order: trying A")
                     NIL 'ONLY)))
                  (SETQ ANSWER_2 (AEVAL (LIST 'APPLY_B EXP VAR)))
                  (COND
                   ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_2)))
                    (PROGN
                     (COND
                      ((BOOLVALUE* (REVALX *TRACETRIG))
                       (ASSGNPRI (AEVAL "A failed, trying B") NIL 'ONLY)))
                     (SETQ ANSWER_3 (AEVAL (LIST 'APPLY_C EXP VAR)))
                     (COND
                      ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_3)))
                       (PROGN
                        (SETQ ANSWER_4 (AEVAL (LIST 'APPLY_D EXP VAR)))
                        (COND
                         ((BOOLVALUE* (REVALX (LIST 'FAIL ANSWER_4)))
                          (AEVAL (REDERR (REVALX "can't do it"))))
                         (T (RETURN (AEVAL ANSWER_4))))
                        (AEVAL 'NIL)))
                      (T (RETURN (AEVAL ANSWER_3))))
                     (AEVAL 'NIL)))
                   (T (RETURN (AEVAL ANSWER_2))))
                  (AEVAL 'NIL)))
                (T (RETURN (AEVAL ANSWER))))
               (AEVAL 'NIL))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (RETURN (AEVAL ANSWER)))) 
(ENDMODULE) 
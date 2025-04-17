(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HARMONIC)) 
(FLAG '(SOLIDHARMONIC SPHERICALHARMONIC) 'SPECFN) 
(DEFLIST '((SOLIDHARMONIC 6) (SPHERICALHARMONIC 4)) 'NUMBER-OF-ARGS) 
(PUT 'SOLIDHARMONICY 'NUMBER-OF-ARGS 6) 
(FLAG '(SOLIDHARMONICY) 'OPFN) 
(PUT 'SOLIDHARMONICY 'DEFINED-ON-LINE '106) 
(PUT 'SOLIDHARMONICY 'DEFINED-IN-FILE 'SPECFN/HARMONIC.RED) 
(PUT 'SOLIDHARMONICY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLIDHARMONICY (N M X Y Z R2)
    (PROG (MP V Y0 Y1 Y2)
      (COND
       ((NOT (AND (FIXP (REVALX N)) (FIXP (REVALX M))))
        (RETURN
         (AEVAL
          (REDERR (REVALX " SolidHarmonicY : n and m must be integers"))))))
      (COND ((EVALLESSP (AEVAL N) 0) (RETURN 0)))
      (SETQ MP (AEVAL (LIST 'ABS M)))
      (COND ((EVALLESSP (AEVAL N) (AEVAL MP)) (RETURN 0)))
      (SETQ Y0 (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'TIMES 4 'PI)))))
      (COND ((EVALEQUAL (AEVAL N) 0) (RETURN (AEVAL Y0))))
      (COND
       ((EVALGREATERP (AEVAL MP) 0)
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL M) 0)
           (SETQ V (AEVAL (LIST 'PLUS X (LIST 'TIMES 'I Y)))))
          (T (SETQ V (AEVAL (LIST 'DIFFERENCE X (LIST 'TIMES 'I Y))))))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MP) K)) (RETURN NIL)))
           (SETQ Y0
                   (AEVAL*
                    (LIST 'MINUS
                          (LIST 'TIMES
                                (LIST 'SQRT
                                      (LIST 'QUOTIENT (PLUS (TIMES 2 K) 1)
                                            (TIMES 2 K)))
                                V Y0))))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (COND
          ((EVALGREATERP (AEVAL N) (AEVAL MP))
           (PROGN
            (SETK 'K (AEVAL (LIST 'PLUS MP 1)))
            (SETQ Y1 (AEVAL Y0))
            (SETQ Y0
                    (AEVAL
                     (LIST 'TIMES Z
                           (LIST 'SQRT (LIST 'PLUS (LIST 'TIMES 2 'K) 1)) Y1)))
            (COND
             ((EVALGREATERP (AEVAL N) (AEVAL (LIST 'PLUS MP 1)))
              (PROG (K)
                (SETQ K (AEVAL* (LIST 'PLUS MP 2)))
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
                (PROGN
                 (SETQ Y2 (AEVAL* Y1))
                 (SETQ Y1 (AEVAL* Y0))
                 (SETQ Y0
                         (AEVAL*
                          (LIST 'DIFFERENCE
                                (LIST 'TIMES Z
                                      (LIST 'SQRT
                                            (LIST 'QUOTIENT
                                                  (LIST 'DIFFERENCE
                                                        (LIST 'TIMES 4 K K) 1)
                                                  (LIST 'DIFFERENCE
                                                        (LIST 'TIMES K K)
                                                        (LIST 'TIMES MP MP))))
                                      Y1)
                                (LIST 'TIMES R2
                                      (LIST 'SQRT
                                            (LIST 'QUOTIENT
                                                  (LIST 'TIMES
                                                        (LIST 'PLUS
                                                              (LIST 'TIMES 2 K)
                                                              1)
                                                        (LIST 'DIFFERENCE
                                                              (LIST 'DIFFERENCE
                                                                    K MP)
                                                              1)
                                                        (LIST 'PLUS K
                                                              (LIST 'DIFFERENCE
                                                                    MP 1)))
                                                  (LIST 'TIMES
                                                        (LIST 'DIFFERENCE
                                                              (LIST 'TIMES 2 K)
                                                              3)
                                                        (LIST 'DIFFERENCE
                                                              (LIST 'TIMES K K)
                                                              (LIST 'TIMES MP
                                                                    MP)))))
                                      Y2)))))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ Y1 (AEVAL Y0))
         (SETQ Y0 (AEVAL (LIST 'TIMES Z (LIST 'SQRT 3) Y1)))
         (COND
          ((EVALGREATERP (AEVAL N) 1)
           (PROG (K)
             (SETQ K 2)
            LAB
             (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
             (PROGN
              (SETQ Y2 (AEVAL* Y1))
              (SETQ Y1 (AEVAL* Y0))
              (SETQ Y0
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'DIFFERENCE
                                   (LIST 'TIMES Z
                                         (LIST 'SQRT
                                               (DIFFERENCE (TIMES 4 K K) 1))
                                         Y1)
                                   (LIST 'TIMES R2 (DIFFERENCE K 1)
                                         (LIST 'SQRT
                                               (LIST 'QUOTIENT
                                                     (PLUS (TIMES 2 K) 1)
                                                     (DIFFERENCE (TIMES 2 K)
                                                                 3)))
                                         Y2))
                             K))))
             (SETQ K
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      K))
             (GO LAB))))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALLESSP (AEVAL M) 0)
             (NOT (BOOLVALUE* (REVALX (LIST 'EVENP MP)))))
        (SETQ Y0 (AEVAL (LIST 'MINUS Y0)))))
      (RETURN (AEVAL Y0)))) 
(PUT 'SPHERICALHARMONICY 'NUMBER-OF-ARGS 4) 
(FLAG '(SPHERICALHARMONICY) 'OPFN) 
(PUT 'SPHERICALHARMONICY 'DEFINED-ON-LINE '142) 
(PUT 'SPHERICALHARMONICY 'DEFINED-IN-FILE 'SPECFN/HARMONIC.RED) 
(PUT 'SPHERICALHARMONICY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPHERICALHARMONICY (N M THETA PHI)
    (LIST 'SOLIDHARMONICY N M (LIST 'TIMES (LIST 'SIN THETA) (LIST 'COS PHI))
          (LIST 'TIMES (LIST 'SIN THETA) (LIST 'SIN PHI)) (LIST 'COS THETA) 1)) 
(ENDMODULE) 
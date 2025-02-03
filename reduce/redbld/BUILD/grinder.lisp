(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT '|g| 'NUMBER-OF-ARGS 2) 
(FLAG '(|g|) 'OPFN) 
(PUT '|g| 'DEFINED-ON-LINE '38) 
(PUT '|g| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |g| (X N)
    (COND
     ((EVALGREATERP (AEVAL N) 0)
      (PROG (I FORALL-RESULT)
        (SETQ I 0)
        (SETQ FORALL-RESULT 1)
       LAB1
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT
                (AEVAL* (LIST 'TIMES (AEVAL* (LIST 'PLUS X I)) FORALL-RESULT)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB1)))
     ((EVALLESSP (AEVAL N) 0)
      (AEVAL
       (LIST 'QUOTIENT 1
             (PROG (I FORALL-RESULT)
               (SETQ I 1)
               (SETQ FORALL-RESULT 1)
              LAB1
               (COND
                ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'MINUS N)) I))
                 (RETURN FORALL-RESULT)))
               (SETQ FORALL-RESULT
                       (AEVAL*
                        (LIST 'TIMES (AEVAL* (LIST 'DIFFERENCE X I))
                              FORALL-RESULT)))
               (SETQ I
                       ((LAMBDA (FORALL-RESULT)
                          (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                        I))
               (GO LAB1)))))
     (T 1))) 
(PUT '|g1| 'NUMBER-OF-ARGS 2) 
(FLAG '(|g1|) 'OPFN) 
(PUT '|g1| 'DEFINED-ON-LINE '44) 
(PUT '|g1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g1| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |g1| (N1 N2)
    (COND ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0)) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) 2)
                        (LIST 'PLUS N1 (LIST 'DIFFERENCE N2 2)))
                  (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                        (LIST 'DIFFERENCE 1 N1))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'DIFFERENCE 1 N2))
                        (LIST 'TIMES
                              (LIST '|g| (LIST 'DIFFERENCE 'D 2)
                                    (LIST 'DIFFERENCE (LIST 'DIFFERENCE 2 N1)
                                          N2))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|gg1| 'NUMBER-OF-ARGS 4) 
(FLAG '(|gg1|) 'OPFN) 
(PUT '|gg1| 'DEFINED-ON-LINE '50) 
(PUT '|gg1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|gg1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |gg1| (N1 N2 N M)
    (COND ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0)) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) 2)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'DIFFERENCE N2 M) 2)))
                  (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                        (LIST 'PLUS (LIST 'DIFFERENCE 1 N1)
                              (LIST 'DIFFERENCE N M)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'PLUS (LIST 'DIFFERENCE 1 N2) M))
                        (LIST 'TIMES
                              (LIST '|g| (LIST 'DIFFERENCE 'D 2)
                                    (LIST 'PLUS
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 2 N1) N2)
                                          N))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|g1A| 'NUMBER-OF-ARGS 2) 
(FLAG '(|g1A|) 'OPFN) 
(PUT '|g1A| 'DEFINED-ON-LINE '56) 
(PUT '|g1A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g1A| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |g1A| (N1 N2)
    (COND ((EVALLEQ (AEVAL N2) 0) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                        (LIST 'PLUS N1 (LIST 'DIFFERENCE N2 3)))
                  (LIST '|g| (LIST 'DIFFERENCE 'D 2) (LIST 'DIFFERENCE 2 N1))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'DIFFERENCE 1 N2))
                        (LIST 'TIMES
                              (LIST '|g|
                                    (LIST 'DIFFERENCE
                                          (LIST 'TIMES (LIST 'QUOTIENT 3 2) 'D)
                                          3)
                                    (LIST 'DIFFERENCE (LIST 'DIFFERENCE 3 N1)
                                          N2))
                              (LIST '|g|
                                    (LIST 'PLUS
                                          (LIST 'MINUS (LIST 'QUOTIENT 'D 2))
                                          2)
                                    (LIST 'DIFFERENCE N1 2))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|i1| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1|) 'OPFN) 
(PUT '|i1| 'DEFINED-ON-LINE '62) 
(PUT '|i1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1| (N1 N2)
    (COND ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0)) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 3)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'DIFFERENCE 1 N2))
                        (LIST 'TIMES (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|ii1| 'NUMBER-OF-ARGS 4) 
(FLAG '(|ii1|) 'OPFN) 
(PUT '|ii1| 'DEFINED-ON-LINE '67) 
(PUT '|ii1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii1| (N1 N2 N M)
    (COND ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0)) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE
                                    (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) N)
                                    3)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'PLUS (LIST 'DIFFERENCE 1 N2)
                                    (LIST 'DIFFERENCE N M)))
                        (LIST 'TIMES (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|i1A| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1A|) 'OPFN) 
(PUT '|i1A| 'DEFINED-ON-LINE '72) 
(PUT '|i1A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1A| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1A| (N1 N2)
    (COND ((EVALLEQ (AEVAL N2) 0) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 2 'D)) 5)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 5)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'DIFFERENCE 1 N2))
                        (LIST 'TIMES
                              (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                                    (LIST 'DIFFERENCE N1 3))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|i1B| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1B|) 'OPFN) 
(PUT '|i1B| 'DEFINED-ON-LINE '77) 
(PUT '|i1B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1B| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1B| (N1 N2)
    (COND ((EVALLEQ (AEVAL N1) 0) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 2 'D)) 5)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 5)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE 'D 2)
                              (LIST 'DIFFERENCE 2 N2))
                        (LIST 'TIMES
                              (LIST '|g|
                                    (LIST 'PLUS
                                          (LIST 'MINUS (LIST 'QUOTIENT 'D 2))
                                          2)
                                    (LIST 'DIFFERENCE N2 2))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))))))))) 
(PUT '|i1C| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1C|) 'OPFN) 
(PUT '|i1C| 'DEFINED-ON-LINE '82) 
(PUT '|i1C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1C| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1C| (N1 N2)
    (COND ((EVALLEQ (AEVAL N2) 0) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 3 'D)) 7)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 7)))
                  (LIST 'QUOTIENT
                        (LIST '|g| (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) 1)
                              (LIST 'DIFFERENCE 1 N2))
                        (LIST 'TIMES
                              (LIST '|g|
                                    (LIST 'PLUS
                                          (LIST 'MINUS (LIST 'TIMES 2 'D)) 5)
                                    (LIST 'DIFFERENCE N1 5))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N2 1))))))))) 
(PUT '|i1D| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1D|) 'OPFN) 
(PUT '|i1D| 'DEFINED-ON-LINE '87) 
(PUT '|i1D| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1D| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1D| (N1 N2)
    (LIST 'TIMES
          (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 3 'D)) 7)
                (LIST 'PLUS N1 (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 7)))
          (LIST 'QUOTIENT
                (LIST '|g| (LIST 'DIFFERENCE 'D 2) (LIST 'DIFFERENCE 2 N2))
                (LIST 'TIMES
                      (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                            (LIST 'DIFFERENCE N1 3))
                      (LIST '|g|
                            (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) 2)
                            (LIST 'DIFFERENCE N2 2)))))) 
(PUT '|i1E| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i1E|) 'OPFN) 
(PUT '|i1E| 'DEFINED-ON-LINE '91) 
(PUT '|i1E| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i1E| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i1E| (N1 N2)
    (COND ((EVALLEQ (AEVAL N1) 0) 0)
          (T
           (AEVAL
            (LIST 'TIMES
                  (LIST '|g| (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 3 'D)) 7)
                        (LIST 'PLUS N1
                              (LIST 'DIFFERENCE (LIST 'TIMES 2 N2) 7)))
                  (LIST 'QUOTIENT
                        (LIST '|g|
                              (LIST 'DIFFERENCE
                                    (LIST 'TIMES (LIST 'QUOTIENT 3 2) 'D) 3)
                              (LIST 'DIFFERENCE 3 N2))
                        (LIST 'TIMES
                              (LIST '|g| (LIST 'PLUS (LIST 'MINUS 'D) 3)
                                    (LIST 'DIFFERENCE N2 3))
                              (LIST 'FACTORIAL (LIST 'DIFFERENCE N1 1))))))))) 
(OPERATOR (LIST '|b|)) 
(PUT '|g2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|g2|) 'OPFN) 
(PUT '|g2| 'DEFINED-ON-LINE '134) 
(PUT '|g2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |g2| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N1 N3) (LIST '|g1| N2 N4) (LIST '|b| 1))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N3 N5) (LIST '|g1A| (LIST 'PLUS N4 N3 N5) N2)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N5) (LIST '|g1A| (LIST 'PLUS N3 N4 N5) N1)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N1 N5) (LIST '|g1A| (LIST 'PLUS N2 N1 N5) N4)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N2 N5) (LIST '|g1A| (LIST 'PLUS N1 N2 N5) N3)
             (LIST '|b| 2))))
     ((EVALLESSP (AEVAL N5) 0)
      (COND ((EVALGREATERP (AEVAL N1) 1) (AEVAL (LIST '|g2R5| N1 N2 N3 N4 N5)))
            ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|g2R5| N2 N1 N4 N3 N5)))
            ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|g2R5| N3 N4 N1 N2 N5)))
            ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|g2R5| N4 N3 N2 N1 N5)))
            (T (AEVAL (LIST '|g2R| N5)))))
     ((EVALLESSP (AEVAL N1) 0)
      (COND
       ((AND (EVALLESSP (AEVAL N4) 0) (EVALGREATERP (AEVAL N4) (AEVAL N1)))
        (AEVAL (LIST '|g2R1| N4 N3 N2 N1 N5)))
       (T (AEVAL (LIST '|g2R1| N1 N2 N3 N4 N5)))))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|g2R1| N4 N3 N2 N1 N5)))
     ((EVALLESSP (AEVAL N2) 0)
      (COND
       ((AND (EVALLESSP (AEVAL N3) 0) (EVALGREATERP (AEVAL N3) (AEVAL N2)))
        (AEVAL (LIST '|g2R1| N3 N4 N1 N2 N5)))
       (T (AEVAL (LIST '|g2R1| N2 N1 N4 N3 N5)))))
     ((EVALLESSP (AEVAL N3) 0) (AEVAL (LIST '|g2R1| N3 N4 N1 N2 N5)))
     ((EVALLESSP (AEVAL (LIST 'MAX N1 N3)) (AEVAL (LIST 'MAX N2 N4)))
      (AEVAL (LIST '|g2L| N2 N1 N4 N3 N5)))
     (T (AEVAL (LIST '|g2L| N1 N2 N3 N4 N5))))) 
(REMEMBER (LIST '|g2|)) 
(PUT '|g2L| 'NUMBER-OF-ARGS 5) 
(FLAG '(|g2L|) 'OPFN) 
(PUT '|g2L| 'DEFINED-ON-LINE '163) 
(PUT '|g2L| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g2L| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |g2L| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N1
                      (LIST 'DIFFERENCE
                            (LIST '|g2| (LIST 'PLUS N1 1) N2 N3 N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|g2| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 N4 N5)))
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|g2| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|g2| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N3)
                (LIST 'TIMES 2 N5)))) 
(PUT '|g2R5| 'NUMBER-OF-ARGS 5) 
(FLAG '(|g2R5|) 'OPFN) 
(PUT '|g2R5| 'DEFINED-ON-LINE '169) 
(PUT '|g2R5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g2R5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |g2R5| (N1 N2 N3 N4 N5)
    (LIST 'PLUS (LIST '|g2| N1 (LIST 'DIFFERENCE N2 1) N3 N4 (LIST 'PLUS N5 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS (LIST 'QUOTIENT 'D 2)
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE N2 N3)
                                                    N4)
                                              N5)
                                        1))
                            (LIST '|g2| (LIST 'DIFFERENCE N1 1) N2 N3 N4
                                  (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES
                                                                (LIST 'QUOTIENT
                                                                      3 2)
                                                                'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        N4)
                                  N5)
                            (LIST 'DIFFERENCE
                                  (LIST '|g2| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 N4
                                        (LIST 'PLUS N5 1))
                                  (LIST '|g2| (LIST 'DIFFERENCE N1 1) N2 N3
                                        (LIST 'DIFFERENCE N4 1)
                                        (LIST 'PLUS N5 1)))))
                (LIST 'DIFFERENCE N1 1)))) 
(PUT '|g2R| 'NUMBER-OF-ARGS 1) 
(FLAG '(|g2R|) 'OPFN) 
(PUT '|g2R| 'DEFINED-ON-LINE '176) 
(PUT '|g2R| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g2R| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |g2R| (N5)
    (LIST 'MINUS
          (LIST 'TIMES
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'QUOTIENT 'D 2) N5) 2)
                      (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 3))
                (LIST '|g2| 1 1 1 1 (LIST 'PLUS N5 1))))) 
(PUT '|g2R1| 'NUMBER-OF-ARGS 5) 
(FLAG '(|g2R1|) 'OPFN) 
(PUT '|g2R1| 'DEFINED-ON-LINE '180) 
(PUT '|g2R1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|g2R1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |g2R1| (N1 N2 N3 N4 N5)
    (LIST 'PLUS (LIST '|g2| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS (LIST 'QUOTIENT 'D 2)
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE N3 N1)
                                                    N2)
                                              N5)
                                        1))
                            (LIST '|g2| (LIST 'PLUS N1 1) N2 N3 N4 N5))
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|g2| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1)
                                        (LIST 'PLUS N4 1) N5)
                                  (LIST '|g2| (LIST 'PLUS N1 1) N2 N3
                                        (LIST 'PLUS N4 1)
                                        (LIST 'DIFFERENCE N5 1)))))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES
                                                          (LIST 'QUOTIENT 3 2)
                                                          'D)
                                                    N1)
                                              N2)
                                        N3)
                                  N4)
                            N5)
                      1)))) 
(PUT '|i2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2|) 'OPFN) 
(PUT '|i2| 'DEFINED-ON-LINE '213) 
(PUT '|i2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N3) (LIST '|i1| N2 N4) (LIST '|b| 1))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N3 N5) (LIST '|i1B| N2 (LIST 'PLUS N4 N3 N5))
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N5) (LIST '|i1B| N1 (LIST 'PLUS N3 N4 N5))
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N5)
             (LIST '|i1A| (LIST 'PLUS N2 N1 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5)
             (LIST '|i1A| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N5)) N3)
             (LIST '|b| 2))))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i2R1| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i2R1| N2 N1 N4 N3 N5)))
     ((EVALLESSP (AEVAL N3) 0) (AEVAL (LIST '|i2R3| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i2R3| N2 N1 N4 N3 N5)))
     ((EVALLESSP (AEVAL N5) 0)
      (COND
       ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|i2R53| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|i2R53| N2 N1 N4 N3 N5)))
       ((EVALGREATERP (AEVAL N1) 1) (AEVAL (LIST '|i2R51| N1 N2 N5)))
       ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|i2R51| N2 N1 N5)))
       (T (AEVAL (LIST '|i2R5| N5)))))
     ((EVALLESSP (AEVAL (LIST 'MAX N1 N3)) (AEVAL (LIST 'MAX N2 N4)))
      (AEVAL (LIST '|i2L| N2 N1 N4 N3 N5)))
     (T (AEVAL (LIST '|i2L| N1 N2 N3 N4 N5))))) 
(REMEMBER (LIST '|i2|)) 
(PUT '|i2L| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2L|) 'OPFN) 
(PUT '|i2L| 'DEFINED-ON-LINE '238) 
(PUT '|i2L| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2L| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2L| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES 2
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE 'D
                                                                N3)
                                                          N4)
                                                    N5))
                                        N1)
                                  N2)
                            1)
                      (LIST '|i2| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5))
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|i2| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|i2| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5))))
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N2) N3)
                      (LIST 'TIMES 2 N5))
                1))) 
(PUT '|i2R1| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2R1|) 'OPFN) 
(PUT '|i2R1| 'DEFINED-ON-LINE '244) 
(PUT '|i2R1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2R1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2R1| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1)
                                        N2)
                                  N4)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i2| (LIST 'PLUS N1 1) N2 N3 N4 N5))
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i2| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5)
                            (LIST '|i2| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3)
                                        N4)
                                  N5))
                      N1)
                N2))) 
(PUT '|i2R3| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2R3|) 'OPFN) 
(PUT '|i2R3| 'DEFINED-ON-LINE '250) 
(PUT '|i2R3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2R3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2R3| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES 2
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE 'D
                                                                N3)
                                                          N4)
                                                    N5))
                                        N1)
                                  N2)
                            1)
                      (LIST '|i2| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5))
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|i2| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|i2| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5))))
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N2) N3)
                      (LIST 'TIMES 2 N5))
                1))) 
(PUT '|i2R53| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2R53|) 'OPFN) 
(PUT '|i2R53| 'DEFINED-ON-LINE '256) 
(PUT '|i2R53| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2R53| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2R53| (N1 N2 N3 N4 N5)
    (LIST 'PLUS (LIST '|i2| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N2)
                                        N3)
                                  (LIST 'TIMES 2 N5))
                            (LIST '|i2| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'TIMES 2
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE 'D
                                                                 N3)
                                                                N4)
                                                          N5))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1))))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i2R51| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i2R51|) 'OPFN) 
(PUT '|i2R51| 'DEFINED-ON-LINE '263) 
(PUT '|i2R51| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2R51| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2R51| (N1 N2 N5)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|i2| (LIST 'DIFFERENCE N1 1) N2 0 1
                                  (LIST 'PLUS N5 1))
                            (LIST '|i2| (LIST 'DIFFERENCE N1 1) N2 1 0
                                  (LIST 'PLUS N5 1))))
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N5) 1)
                      (LIST '|i2| (LIST 'DIFFERENCE N1 1) N2 1 1 N5)))
          (LIST 'DIFFERENCE N1 1))) 
(PUT '|i2R5| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i2R5|) 'OPFN) 
(PUT '|i2R5| 'DEFINED-ON-LINE '269) 
(PUT '|i2R5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2R5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i2R5| (N5)
    (LIST 'TIMES
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D (LIST 'TIMES 2 N5)) 4)
                (LIST 'TIMES 2 (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 3)))
          (LIST '|i2| 1 1 1 1 (LIST 'PLUS N5 1)))) 
(PUT '|i2A| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2A|) 'OPFN) 
(PUT '|i2A| 'DEFINED-ON-LINE '273) 
(PUT '|i2A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2A| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1B| N1 N3) (LIST '|i1| N2 N4) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1A| N3 N5) (LIST '|i1E| N2 (LIST 'PLUS N4 N3 N5))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N5) (LIST '|i1E| N1 (LIST 'PLUS N3 N4 N5))
             (LIST '|b| 5))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5)
             (LIST '|i1D| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N5)) N3)
             (LIST '|b| 3))))
     ((EVALLESSP (AEVAL N5) 0) (AEVAL (LIST '|i2AR5| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i2AR2| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i2AR1| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i2AR4| N1 N2 N3 N4 N5)))
     (T (AEVAL (LIST '|i2AL| N1 N2 N3 N4 N5))))) 
(REMEMBER (LIST '|i2A|)) 
(PUT '|i2AL| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2AL|) 'OPFN) 
(PUT '|i2AL| 'DEFINED-ON-LINE '289) 
(PUT '|i2AL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2AL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2AL| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) N3)
                      (LIST 'DIFFERENCE
                            (LIST '|i2A| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|i2A| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5)))
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                              (LIST 'TIMES 2
                                                    (LIST 'PLUS N3 N4 N5)))
                                        N1)
                                  N2)
                            1)
                      (LIST '|i2A| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5)))
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES (LIST 'QUOTIENT 3 2) 'D)
                                        N1)
                                  N2)
                            N3)
                      (LIST 'TIMES 2 N5))
                1))) 
(PUT '|i2AR5| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2AR5|) 'OPFN) 
(PUT '|i2AR5| 'DEFINED-ON-LINE '295) 
(PUT '|i2AR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2AR5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2AR5| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2A| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES
                                                          (LIST 'QUOTIENT 3 2)
                                                          'D)
                                                    N1)
                                              N2)
                                        N3)
                                  (LIST 'TIMES 2 N5))
                            (LIST '|i2A| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2A| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1))))
                (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2))
                      (LIST 'DIFFERENCE N3 1))))) 
(PUT '|i2AR2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2AR2|) 'OPFN) 
(PUT '|i2AR2| 'DEFINED-ON-LINE '302) 
(PUT '|i2AR2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2AR2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2AR2| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                                    'D)
                                              N1)
                                        N2)
                                  N3)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i2A| N1 (LIST 'PLUS N2 1) N3 N4 N5))
                (LIST 'TIMES
                      (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) N3)
                      (LIST 'DIFFERENCE
                            (LIST '|i2A| N1 (LIST 'PLUS N2 1) (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5)
                            (LIST '|i2A| N1 (LIST 'PLUS N2 1) (LIST 'PLUS N3 1)
                                  N4 (LIST 'DIFFERENCE N5 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                            (LIST 'TIMES 2 (LIST 'PLUS N3 N4 N5)))
                      N1)
                N2))) 
(PUT '|i2AR1| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2AR1|) 'OPFN) 
(PUT '|i2AR1| 'DEFINED-ON-LINE '308) 
(PUT '|i2AR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2AR1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2AR1| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1)
                                        N2)
                                  N4)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i2A| (LIST 'PLUS N1 1) N2 N3 N4 N5))
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i2A| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5)
                            (LIST '|i2A| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                            (LIST 'TIMES 2 (LIST 'PLUS N3 N4 N5)))
                      N1)
                N2))) 
(PUT '|i2AR4| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2AR4|) 'OPFN) 
(PUT '|i2AR4| 'DEFINED-ON-LINE '314) 
(PUT '|i2AR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2AR4| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2AR4| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2A| N1 N2 N3 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2A| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1)
                                  N5))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES
                                                                (LIST 'QUOTIENT
                                                                      3 2)
                                                                'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        (LIST 'TIMES 2 N5))
                                  2)
                            (LIST '|i2A| N1 N2 (LIST 'DIFFERENCE N3 1)
                                  (LIST 'PLUS N4 1) N5)))
                (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2))
                      (LIST 'DIFFERENCE N3 1))))) 
(PUT '|i2B| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2B|) 'OPFN) 
(PUT '|i2B| 'DEFINED-ON-LINE '321) 
(PUT '|i2B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2B| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2B| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1A| N1 N3) (LIST '|i1| N2 N4) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N5) (LIST '|i1D| N1 (LIST 'PLUS N3 N4 N5))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1A| N1 N5)
             (LIST '|i1C| (LIST 'PLUS N2 N1 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5)
             (LIST '|i1C| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N5)) N3)
             (LIST '|b| 4))))
     ((EVALLESSP (AEVAL N5) 0)
      (COND
       ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|i2BR53| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|i2BR54| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|i2BR52| N1 N2 N5)))
       (T (AEVAL (LIST '|i2BR5| N1 N5)))))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i2BR2| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|i2BR34| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N5) 1) (AEVAL (LIST '|i2BR35| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|i2BR32| N1 N2 N3)))
       (T (AEVAL (LIST '|i2BR3| N1 N3)))))
     ((EVALLESSP (AEVAL N4) 0)
      (COND
       ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|i2BR43| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N5) 1) (AEVAL (LIST '|i2BR45| N1 N2 N3 N4 N5)))
       ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|i2BR42| N1 N2 N4)))
       (T (AEVAL (LIST '|i2BR4| N1 N4)))))
     (T (AEVAL (LIST '|i2BL| N1 N2 N3 N4 N5))))) 
(REMEMBER (LIST '|i2B|)) 
(PUT '|i2BL| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BL|) 'OPFN) 
(PUT '|i2BL| 'DEFINED-ON-LINE '349) 
(PUT '|i2BL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BL| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1))
                            (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5)))
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                              (LIST 'TIMES 2
                                                    (LIST 'PLUS N3 N4 N5)))
                                        N1)
                                  N2)
                            1)
                      (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5)))
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'TIMES 2 'D) N1) N2)
                            N3)
                      (LIST 'TIMES 2 N5))
                1))) 
(PUT '|i2BR53| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR53|) 'OPFN) 
(PUT '|i2BR53| 'DEFINED-ON-LINE '355) 
(PUT '|i2BR53| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR53| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR53| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2B| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2 'D) N1)
                                              N2)
                                        N3)
                                  (LIST 'TIMES 2 N5))
                            (LIST '|i2B| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'DIFFERENCE N3 1) N4
                                  (LIST 'PLUS N5 1))))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i2BR54| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR54|) 'OPFN) 
(PUT '|i2BR54| 'DEFINED-ON-LINE '362) 
(PUT '|i2BR54| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR54| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR54| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2B| N1 N2 (LIST 'DIFFERENCE N3 1) N4 (LIST 'PLUS N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2 'D) N1)
                                              N2)
                                        N4)
                                  (LIST 'TIMES 2 N5))
                            (LIST '|i2B| N1 N2 N3 (LIST 'DIFFERENCE N4 1)
                                  (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2B| (LIST 'DIFFERENCE N1 1) N2 N3
                                  (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i2BR52| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i2BR52|) 'OPFN) 
(PUT '|i2BR52| 'DEFINED-ON-LINE '369) 
(PUT '|i2BR52| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR52| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR52| (N1 N2 N5)
    (LIST 'MINUS
          (LIST 'TIMES
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N2) N5) 1)
                      (LIST 'DIFFERENCE N2 1))
                (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1) 1 1 N5)))) 
(PUT '|i2BR5| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i2BR5|) 'OPFN) 
(PUT '|i2BR5| 'DEFINED-ON-LINE '373) 
(PUT '|i2BR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR5| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i2BR5| (N1 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS (LIST '|i2B| N1 1 1 2 N5)
                (LIST 'TIMES N5
                      (LIST '|i2B| (LIST 'DIFFERENCE N1 1) 1 1 1
                            (LIST 'PLUS N5 1))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 3)))) 
(PUT '|i2BR2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR2|) 'OPFN) 
(PUT '|i2BR2| 'DEFINED-ON-LINE '377) 
(PUT '|i2BR2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR2| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 2 'D)
                                              N1)
                                        N2)
                                  N3)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i2B| N1 (LIST 'PLUS N2 1) N3 N4 N5))
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|i2B| N1 (LIST 'PLUS N2 1) (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5)
                            (LIST '|i2B| N1 (LIST 'PLUS N2 1) (LIST 'PLUS N3 1)
                                  N4 (LIST 'DIFFERENCE N5 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                            (LIST 'TIMES 2 (LIST 'PLUS N3 N4 N5)))
                      N1)
                N2))) 
(PUT '|i2BR34| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR34|) 'OPFN) 
(PUT '|i2BR34| 'DEFINED-ON-LINE '383) 
(PUT '|i2BR34| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR34| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR34| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1) N4 (LIST 'DIFFERENCE N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2B| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1)
                                  N5))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 2 'D)
                                                          N1)
                                                    N2)
                                              N4)
                                        (LIST 'TIMES 2 N5))
                                  2)
                            (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5)))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i2BR35| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR35|) 'OPFN) 
(PUT '|i2BR35| 'DEFINED-ON-LINE '390) 
(PUT '|i2BR35| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR35| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR35| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1) N5)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2 'D) N1)
                                              N5)
                                        (LIST 'TIMES 2 N3))
                                  1)
                            (LIST '|i2B| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1)))
                      (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N1)
                            (LIST '|i2B| (LIST 'PLUS N1 1) N2 (LIST 'PLUS N3 1)
                                  N4 (LIST 'DIFFERENCE N5 1))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|i2BR32| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i2BR32|) 'OPFN) 
(PUT '|i2BR32| 'DEFINED-ON-LINE '397) 
(PUT '|i2BR32| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR32| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR32| (N1 N2 N3)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D) N1)
                                              N2)
                                        (LIST 'TIMES 2 N3))
                                  3)
                            (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1) N3 1 1))
                      (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N1)
                            (LIST '|i2B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 1 1)))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|i2BR3| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i2BR3|) 'OPFN) 
(PUT '|i2BR3| 'DEFINED-ON-LINE '403) 
(PUT '|i2BR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR3| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i2BR3| (N1 N3)
    (LIST 'TIMES
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1)
                            (LIST 'TIMES 2 N3))
                      4)
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 2 'D) N1) N3)
                      2))
          (LIST '|i2B| N1 0 N3 1 1))) 
(PUT '|i2BR43| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR43|) 'OPFN) 
(PUT '|i2BR43| 'DEFINED-ON-LINE '407) 
(PUT '|i2BR43| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR43| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR43| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2B| N1 N2 N3 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N3 N4
                                                                N5)))
                                              N1)
                                        N2)
                                  1)
                            (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1)
                                  N5))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 2 'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        (LIST 'TIMES 2 N5))
                                  2)
                            (LIST '|i2B| N1 N2 (LIST 'DIFFERENCE N3 1)
                                  (LIST 'PLUS N4 1) N5)))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i2BR45| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2BR45|) 'OPFN) 
(PUT '|i2BR45| 'DEFINED-ON-LINE '414) 
(PUT '|i2BR45| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR45| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR45| (N1 N2 N3 N4 N5)
    (LIST 'DIFFERENCE
          (LIST '|i2B| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2 'D) N1)
                                              N5)
                                        (LIST 'TIMES 2 N3))
                                  1)
                            (LIST '|i2B| N1 N2 N3 (LIST 'PLUS N4 1)
                                  (LIST 'DIFFERENCE N5 1)))
                      (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N1)
                            (LIST '|i2B| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|i2BR42| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i2BR42|) 'OPFN) 
(PUT '|i2BR42| 'DEFINED-ON-LINE '421) 
(PUT '|i2BR42| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR42| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2BR42| (N1 N2 N4)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D) N1)
                                              N2)
                                        (LIST 'TIMES 2 N4))
                                  3)
                            (LIST '|i2B| N1 (LIST 'DIFFERENCE N2 1) 1 N4 1))
                      (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N1)
                            (LIST '|i2B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) 1 N4 1)))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|i2BR4| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i2BR4|) 'OPFN) 
(PUT '|i2BR4| 'DEFINED-ON-LINE '427) 
(PUT '|i2BR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2BR4| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i2BR4| (N1 N4)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i2B| (LIST 'DIFFERENCE N1 2) 1 2 N4 1)
                      (LIST '|i2B| (LIST 'DIFFERENCE N1 1) 1 2 N4 1))
                (LIST '|i2B| (LIST 'DIFFERENCE N1 2) 1 1 N4 2))
          (LIST 'TIMES 2
                (LIST 'PLUS (LIST 'MINUS 'D) (LIST 'DIFFERENCE N1 1))))) 
(PUT '|i2C| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2C|) 'OPFN) 
(PUT '|i2C| 'DEFINED-ON-LINE '432) 
(PUT '|i2C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2C| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2C| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1A| N5 N3) (LIST '|i1E| N2 (LIST 'PLUS N4 N3 N5))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1A| N5 N4) (LIST '|i1E| N1 (LIST 'PLUS N3 N4 N5))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1B| N1 N5)
             (LIST '|i1C| (LIST 'PLUS N2 N1 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1B| N2 N5)
             (LIST '|i1C| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N5)) N3)
             (LIST '|b| 3))))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i2CR1| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i2CR1| N2 N1 N4 N3 N5)))
     ((EVALLESSP (AEVAL N3) 0) (AEVAL (LIST '|i2CR3| N1 N2 N3 N4 N5)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i2CR3| N2 N1 N4 N3 N5)))
     ((EVALGREATERP (AEVAL N1) 1) (AEVAL (LIST '|i2CL1| N1 N2 N3 N4 N5)))
     ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|i2CL1| N2 N1 N4 N3 N5)))
     ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|i2CL3| N3 N4 N5)))
     ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|i2CL3| N4 N3 N5)))
     ((EVALEQUAL (AEVAL N5) 2) (AEVAL (LIST '|b| 6)))
     ((EVALGREATERP (AEVAL N5) 2) (AEVAL (LIST '|i2CL5| N5)))
     (T (AEVAL (LIST '|i2CR5| N5))))) 
(REMEMBER (LIST '|i2C|)) 
(PUT '|i2CR1| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2CR1|) 'OPFN) 
(PUT '|i2CR1| 'DEFINED-ON-LINE '454) 
(PUT '|i2CR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CR1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2CR1| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 2 'D)
                                              N1)
                                        N2)
                                  N4)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i2C| (LIST 'PLUS N1 1) N2 N3 N4 N5))
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i2C| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5)
                            (LIST '|i2C| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                            (LIST 'TIMES 2 (LIST 'PLUS N3 N4 N5)))
                      N1)
                N2))) 
(PUT '|i2CR3| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2CR3|) 'OPFN) 
(PUT '|i2CR3| 'DEFINED-ON-LINE '460) 
(PUT '|i2CR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CR3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2CR3| (N1 N2 N3 N4 N5)
    (LIST 'PLUS
          (LIST '|i2C| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1) N5)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES
                                                          (LIST 'QUOTIENT 3 2)
                                                          'D)
                                                    N1)
                                              N5)
                                        (LIST 'TIMES 2 N3))
                                  1)
                            (LIST '|i2C| N1 N2 (LIST 'PLUS N3 1) N4
                                  (LIST 'DIFFERENCE N5 1)))
                      (LIST 'TIMES N1
                            (LIST '|i2C| (LIST 'PLUS N1 1) N2 (LIST 'PLUS N3 1)
                                  N4 (LIST 'DIFFERENCE N5 1))))
                (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2))
                      (LIST 'DIFFERENCE N5 1))))) 
(PUT '|i2CL1| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i2CL1|) 'OPFN) 
(PUT '|i2CL1| 'DEFINED-ON-LINE '467) 
(PUT '|i2CL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CL1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2CL1| (N1 N2 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES
                      (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) N5)
                      (LIST 'DIFFERENCE
                            (LIST '|i2C| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 (LIST 'PLUS N5 1))
                            (LIST '|i2C| (LIST 'DIFFERENCE N1 1) N2 N3
                                  (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1))))
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                                    'D)
                                              N1)
                                        N5)
                                  (LIST 'TIMES 2 N3))
                            1)
                      (LIST '|i2C| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5)))
          (LIST 'DIFFERENCE N1 1))) 
(PUT '|i2CL3| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i2CL3|) 'OPFN) 
(PUT '|i2CL3| 'DEFINED-ON-LINE '473) 
(PUT '|i2CL3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CL3| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i2CL3| (N3 N4 N5)
    (LIST 'PLUS (LIST '|i2C| 0 1 N3 N4 N5)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 'D 2)) N5)
                            (LIST 'DIFFERENCE
                                  (LIST '|i2C| 0 1 (LIST 'DIFFERENCE N3 1) N4
                                        (LIST 'PLUS N5 1))
                                  (LIST '|i2C| 1 0 (LIST 'DIFFERENCE N3 1) N4
                                        (LIST 'PLUS N5 1))))
                      (LIST 'TIMES 2
                            (LIST '|i2C| 2 1 (LIST 'DIFFERENCE N3 1) N4 N5)))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i2CL5| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i2CL5|) 'OPFN) 
(PUT '|i2CL5| 'DEFINED-ON-LINE '480) 
(PUT '|i2CL5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CL5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i2CL5| (N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                  (LIST 'TIMES 2 N5))
                            4)
                      (LIST '|i2C| 1 1 1 1 (LIST 'DIFFERENCE N5 1)))
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                  (LIST 'TIMES 2 N5))
                            5)
                      (LIST '|i2C| 1 0 1 1 N5))
                (LIST 'DIFFERENCE (LIST '|i2C| 0 1 2 1 (LIST 'DIFFERENCE N5 1))
                      (LIST '|i2C| 1 1 2 0 N5)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 1)))) 
(PUT '|i2CR5| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i2CR5|) 'OPFN) 
(PUT '|i2CR5| 'DEFINED-ON-LINE '486) 
(PUT '|i2CR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i2CR5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i2CR5| (N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 2)
                            (LIST '|i2C| 1 1 1 1 (LIST 'PLUS N5 1)))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                        (LIST 'TIMES 2 N5))
                                  7)
                            (LIST '|i2C| 1 0 1 1 (LIST 'PLUS N5 1))))
                (LIST 'DIFFERENCE (LIST '|i2C| 1 1 2 0 (LIST 'PLUS N5 1))
                      (LIST '|i2C| 0 1 2 1 N5)))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) (LIST 'TIMES 2 N5)) 6))) 
(PUT '|j2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j2|) 'OPFN) 
(PUT '|j2| 'DEFINED-ON-LINE '514) 
(PUT '|j2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N5) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N4) (LIST '|i1| N2 N5) (LIST '|b| 1))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N4)
             (LIST '|i1A| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N4)) N5)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N5)
             (LIST '|i1A| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 2))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j2| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5)
             (LIST 'DIFFERENCE
                   (LIST '|j2| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4
                         N5)
                   (LIST '|j2| N1 N2 (LIST 'PLUS N3 1) N4 N5)))))
     ((AND (EVALLESSP (AEVAL N1) 0)
           (NOT
            (AND (EVALLESSP (AEVAL N2) 0)
                 (EVALGREATERP (AEVAL N2) (AEVAL N1)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|j2| (LIST 'PLUS N1 1) N2 N3 N4 N5)
                   (LIST '|j2| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5))
             (LIST '|j2| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|j2| N1 (LIST 'PLUS N2 1) N3 N4 N5)
                   (LIST '|j2| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4
                         N5))
             (LIST '|j2| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4 N5))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|j2| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5)
             (LIST 'DIFFERENCE (LIST '|j2| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5)
                   (LIST '|j2| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5))))))) 
(REMEMBER (LIST '|j2|)) 
(PUT '|j2A| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j2A|) 'OPFN) 
(PUT '|j2A| 'DEFINED-ON-LINE '528) 
(PUT '|j2A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2A| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1B| N1 N4) (LIST '|i1| N2 N5) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1B| N3 N4)
             (LIST '|i1C| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N4)) N5)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N5)
             (LIST '|i1D| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 3))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j2A| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5)
             (LIST 'DIFFERENCE
                   (LIST '|j2A| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4
                         N5)
                   (LIST '|j2A| N1 N2 (LIST 'PLUS N3 1) N4 N5)))))
     ((AND (EVALLESSP (AEVAL N1) 0)
           (NOT
            (AND (EVALLESSP (AEVAL N2) 0)
                 (EVALGREATERP (AEVAL N2) (AEVAL N1)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|j2A| (LIST 'PLUS N1 1) N2 N3 N4 N5)
                   (LIST '|j2A| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5))
             (LIST '|j2A| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                   N5))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|j2A| N1 (LIST 'PLUS N2 1) N3 N4 N5)
                   (LIST '|j2A| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4
                         N5))
             (LIST '|j2A| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                   N5))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|j2A| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5)
             (LIST 'DIFFERENCE
                   (LIST '|j2A| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5)
                   (LIST '|j2A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5))))))) 
(REMEMBER (LIST '|j2A|)) 
(PUT '|j2B| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j2B|) 'OPFN) 
(PUT '|j2B| 'DEFINED-ON-LINE '542) 
(PUT '|j2B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2B| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2B| (N1 N2 N3 N4 N5)
    (COND
     ((OR (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N5) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1A| N1 N4) (LIST '|i1| N2 N5) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N5)
             (LIST '|i1C| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N5)) N4)
             (LIST '|b| 4))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j2B| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5)
             (LIST 'DIFFERENCE
                   (LIST '|j2B| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4
                         N5)
                   (LIST '|j2B| N1 N2 (LIST 'PLUS N3 1) N4 N5)))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|j2B| N1 (LIST 'PLUS N2 1) N3 N4 N5)
                   (LIST '|j2B| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4
                         N5))
             (LIST '|j2B| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                   N5))))
     (T
      (AEVAL
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N1)
                         (LIST 'DIFFERENCE
                               (LIST '|j2B| (LIST 'PLUS N1 1) N2
                                     (LIST 'DIFFERENCE N3 1) N4 N5)
                               (LIST '|j2B| (LIST 'PLUS N1 1)
                                     (LIST 'DIFFERENCE N2 1) N3 N4 N5)))
                   (LIST 'TIMES N3
                         (LIST '|j2B| N1 (LIST 'DIFFERENCE N2 1)
                               (LIST 'PLUS N3 1) N4 N5)))
             (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3) (LIST 'TIMES 2 N4))))))) 
(REMEMBER (LIST '|j2B|)) 
(PUT '|j2C| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j2C|) 'OPFN) 
(PUT '|j2C| 'DEFINED-ON-LINE '556) 
(PUT '|j2C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2C| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2C| (N1 N2 N3 N4 N5)
    (COND ((OR (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N5) 0)) 0)
          ((EVALEQUAL (AEVAL N1) 0)
           (AEVAL
            (LIST 'TIMES (LIST '|i1A| N3 N4)
                  (LIST '|i1C| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N4)) N5)
                  (LIST '|b| 3))))
          ((EVALEQUAL (AEVAL N2) 0)
           (AEVAL
            (LIST 'TIMES (LIST '|i1A| N3 N5)
                  (LIST '|i1C| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N5)) N4)
                  (LIST '|b| 3))))
          ((AND (EVALLESSP (AEVAL N1) 0)
                (NOT
                 (AND (EVALLESSP (AEVAL N2) 0)
                      (EVALGREATERP (AEVAL N2) (AEVAL N1)))))
           (AEVAL
            (LIST 'PLUS
                  (LIST 'DIFFERENCE (LIST '|j2C| (LIST 'PLUS N1 1) N2 N3 N4 N5)
                        (LIST '|j2C| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1)
                              N3 N4 N5))
                  (LIST '|j2C| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                        N5))))
          ((EVALLESSP (AEVAL N2) 0)
           (AEVAL
            (LIST 'PLUS
                  (LIST 'DIFFERENCE (LIST '|j2C| N1 (LIST 'PLUS N2 1) N3 N4 N5)
                        (LIST '|j2C| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1)
                              N3 N4 N5))
                  (LIST '|j2C| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                        N5))))
          ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|j2CL4| N1 N2 N3 N4 N5)))
          ((EVALGREATERP (AEVAL N5) 1) (AEVAL (LIST '|j2CL4| N2 N1 N3 N5 N4)))
          ((EVALGREATERP (AEVAL N1) 1) (AEVAL (LIST '|j2CL1| N1 N2 N3)))
          ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|j2CL1| N2 N1 N3)))
          ((EVALEQUAL (AEVAL N3) 3) (AEVAL (LIST '|b| 7)))
          ((EVALGREATERP (AEVAL N3) 3)
           (AEVAL
            (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST '|j2C| 0 1 N3 1 1))
                  (LIST '|j2C| 1 1 (LIST 'DIFFERENCE N3 1) 1 1))))
          (T
           (AEVAL
            (LIST 'DIFFERENCE
                  (LIST 'TIMES 2 (LIST '|j2C| 0 1 (LIST 'PLUS N3 1) 1 1))
                  (LIST '|j2C| 1 1 (LIST 'PLUS N3 1) 1 1)))))) 
(REMEMBER (LIST '|j2C|)) 
(PUT '|j2CL4| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j2CL4|) 'OPFN) 
(PUT '|j2CL4| 'DEFINED-ON-LINE '574) 
(PUT '|j2CL4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2CL4| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2CL4| (N1 N2 N3 N4 N5)
    (LIST 'DIFFERENCE (LIST '|j2C| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5)
          (LIST 'TIMES 2
                (LIST 'QUOTIENT
                      (LIST 'PLUS
                            (LIST 'TIMES N1
                                  (LIST '|j2C| (LIST 'PLUS N1 1) N2 N3
                                        (LIST 'DIFFERENCE N4 1) N5))
                            (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N3)
                                  (LIST '|j2C| N1 N2 (LIST 'PLUS N3 1)
                                        (LIST 'DIFFERENCE N4 1) N5)))
                      (LIST 'DIFFERENCE N4 1))))) 
(PUT '|j2CL1| 'NUMBER-OF-ARGS 3) 
(FLAG '(|j2CL1|) 'OPFN) 
(PUT '|j2CL1| 'DEFINED-ON-LINE '579) 
(PUT '|j2CL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j2CL1| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j2CL1| (N1 N2 N3)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 2 'D)
                                              N1)
                                        N3)
                                  1)
                            (LIST '|j2C| (LIST 'DIFFERENCE N1 1) N2 N3 1 1))
                      (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'D) N3)
                            (LIST '|j2C| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) 1
                                  1)))
                (LIST 'DIFFERENCE N1 1)))) 
(PUT '|jj2| 'NUMBER-OF-ARGS 6) 
(FLAG '(|jj2|) 'OPFN) 
(PUT '|jj2| 'DEFINED-ON-LINE '583) 
(PUT '|jj2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|jj2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |jj2| (N1 N2 N3 N4 N5 N0)
    (COND
     ((OR (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N5) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N0) 0) (AEVAL (LIST '|j2| N1 N2 N3 N4 N5)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|jj23| N1 N2 N4 N5 N0)))
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|jj21| N2 N3 N4 N5 N0)))
     ((EVALEQUAL (AEVAL N2) 0) (AEVAL (LIST '|jj21| N1 N3 N5 N4 N0)))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|jj2| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5
                   N0)
             (LIST 'DIFFERENCE
                   (LIST '|jj2| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4
                         N5 N0)
                   (LIST '|jj2| N1 N2 (LIST 'PLUS N3 1) N4 N5 N0)))))
     ((AND (EVALLESSP (AEVAL N1) 0)
           (NOT
            (AND (EVALLESSP (AEVAL N2) 0)
                 (EVALGREATERP (AEVAL N2) (AEVAL N1)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|jj2| (LIST 'PLUS N1 1) N2 N3 N4 N5 N0)
                   (LIST '|jj2| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5 N0))
             (LIST '|jj2| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5
                   N0))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE (LIST '|jj2| N1 (LIST 'PLUS N2 1) N3 N4 N5 N0)
                   (LIST '|jj2| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4
                         N5 N0))
             (LIST '|jj2| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4 N5
                   N0))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|jj2| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N0)
             (LIST 'DIFFERENCE
                   (LIST '|jj2| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N0)
                   (LIST '|jj2| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N0))))))) 
(REMEMBER (LIST '|jj2|)) 
(PUT '|jj23| 'NUMBER-OF-ARGS 5) 
(FLAG '(|jj23|) 'OPFN) 
(PUT '|jj23| 'DEFINED-ON-LINE '597) 
(PUT '|jj23| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|jj23| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |jj23| (N1 N2 N4 N5 N0)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!5 '!3) '!4)
                           N0)
                     (LIST 'EXPT (LIST 'TIMES '!3 '!4 '!5) 2))))
      (AEVAL
       (FORALL
        (LIST '(M3 M4 M5) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!5 'M5))
                       (LIST '|i2| N1 N2
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'DIFFERENCE 2 'M5))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M3 M4 M5) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!5 'M5)))))))
      (RETURN (AEVAL P)))) 
(PUT '|jj21| 'NUMBER-OF-ARGS 5) 
(FLAG '(|jj21|) 'OPFN) 
(PUT '|jj21| 'DEFINED-ON-LINE '606) 
(PUT '|jj21| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|jj21| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |jj21| (N2 N3 N4 N5 N0)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!3) '!5)
                           N0)
                     (LIST 'EXPT (LIST 'TIMES '!3 '!4 '!5) 2))))
      (AEVAL
       (FORALL
        (LIST '(M3 M4 M5) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!5 'M5))
                       (LIST '|i2| N3 N2 (LIST 'DIFFERENCE 2 'M3)
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M5)))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M3 M4 M5) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!5 'M5)))))))
      (RETURN (AEVAL P)))) 
(PUT '|i3A| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3A|) 'OPFN) 
(PUT '|i3A| 'DEFINED-ON-LINE '646) 
(PUT '|i3A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3A| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3A| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N2 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1E| N2 (LIST 'PLUS N4 N3 N5 N6 N8 N7))
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 5))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|g2| N3 N6 N5 N8 N7)))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1E| N1 (LIST 'PLUS N3 N4 N5 N6 N7 N8))
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 5))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|g2| N4 N6 N5 N7 N8)))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N7 N8)
             (LIST '|i2C| N1 N2 N3 N4 (LIST 'PLUS N5 N7 N8)))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N8)
             (LIST '|i2A| N2 N1 (LIST 'PLUS N4 N6 N8) N3 N5))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N7)
             (LIST '|i2A| N1 N2 (LIST 'PLUS N3 N6 N7) N4 N5))))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i3AR1| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i3AR1| N2 N1 N4 N3 N5 N6 N8 N7)))
     ((EVALLESSP (AEVAL N6) 0)
      (COND
       ((EVALGREATERP (AEVAL N7) 1)
        (AEVAL (LIST '|i3AR67| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALGREATERP (AEVAL N8) 1)
        (AEVAL (LIST '|i3AR67| N2 N1 N4 N3 N5 N6 N8 N7)))
       (T (AEVAL (LIST '|i3AR6| N1 N2 N3 N4 N5 N6)))))
     ((EVALLESSP (AEVAL N7) 0) (AEVAL (LIST '|i3AR7| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N8) 0) (AEVAL (LIST '|i3AR7| N2 N1 N4 N3 N5 N6 N8 N7)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3A| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALEQUAL (AEVAL N4) 0) (AEVAL (LIST '|j3A| N2 N1 N3 N5 N6 N8 N7)))
     ((EVALLESSP (AEVAL N3) 0) (AEVAL (LIST '|i3AR3| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i3AR3| N2 N1 N4 N3 N5 N6 N8 N7)))
     ((EVALEQUAL (AEVAL N5) 0) (AEVAL (LIST '|j3B| N1 N2 N3 N4 N6 N7 N8)))
     ((EVALLESSP (AEVAL N5) 0)
      (COND
       ((EVALGREATERP (AEVAL N7) 1)
        (AEVAL (LIST '|i3AR57| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALGREATERP (AEVAL N8) 1)
        (AEVAL (LIST '|i3AR57| N2 N1 N4 N3 N5 N6 N8 N7)))
       (T (AEVAL (LIST '|i3AR5| N1 N2 N3 N4 N5 N6)))))
     ((EVALLEQ (AEVAL N6) (AEVAL (LIST 'MAX N5 N7 N8)))
      (AEVAL (LIST '|i3AL6| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLEQ (AEVAL (LIST 'MAX N3 N7)) (AEVAL (LIST 'MAX N4 N8)))
      (AEVAL (LIST '|i3AL| N1 N2 N3 N4 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3AL| N2 N1 N4 N3 N5 N6 N8 N7))))) 
(REMEMBER (LIST '|i3A|)) 
(PUT '|i3AL6| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AL6|) 'OPFN) 
(PUT '|i3AL6| 'DEFINED-ON-LINE '681) 
(PUT '|i3AL6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AL6| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AL6| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  N7 (LIST 'PLUS N8 1))
                            (LIST '|i3A| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6
                                  N7 (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N7) N8)
                (LIST 'TIMES 2 N6)))) 
(PUT '|i3AL| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AL|) 'OPFN) 
(PUT '|i3AL| 'DEFINED-ON-LINE '687) 
(PUT '|i3AL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AL| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AL| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1)
                                  (LIST 'DIFFERENCE N7 1) N8)
                            (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) N7 N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 N6
                                  (LIST 'DIFFERENCE N7 1) (LIST 'PLUS N8 1))
                            (LIST '|i3A| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6
                                  N7 (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6) N8)
                (LIST 'TIMES 2 N7)))) 
(PUT '|i3AR1| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR1|) 'OPFN) 
(PUT '|i3AR1| 'DEFINED-ON-LINE '693) 
(PUT '|i3AR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR1| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N2)
                                        N4)
                                  N8)
                            (LIST 'TIMES 2 N5))
                      (LIST '|i3A| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8))
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5
                                  N6 N7 N8)
                            (LIST '|i3A| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1) N6
                                  N7 N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6
                                  (LIST 'DIFFERENCE N7 1) (LIST 'PLUS N8 1))
                            (LIST '|i3A| (LIST 'PLUS N1 1) N2 N3 N4
                                  (LIST 'DIFFERENCE N5 1) N6 N7
                                  (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1) N2)
                (LIST 'TIMES 2 (LIST 'PLUS N3 N4 N5 N6 N7 N8))))) 
(PUT '|i3AR67| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR67|) 'OPFN) 
(PUT '|i3AR67| 'DEFINED-ON-LINE '700) 
(PUT '|i3AR67| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR67| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR67| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 (LIST 'PLUS N6 1) N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N5
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3A| N1 N2
                                              (LIST 'DIFFERENCE N3 1) N4
                                              (LIST 'PLUS N5 1)
                                              (LIST 'PLUS N6 1)
                                              (LIST 'DIFFERENCE N7 1) N8)
                                        (LIST '|i3A| N1 N2 N3
                                              (LIST 'DIFFERENCE N4 1)
                                              (LIST 'PLUS N5 1)
                                              (LIST 'PLUS N6 1)
                                              (LIST 'DIFFERENCE N7 1) N8)))
                            (LIST 'TIMES N1
                                  (LIST '|i3A| (LIST 'PLUS N1 1) N2 N3 N4 N5
                                        (LIST 'PLUS N6 1)
                                        (LIST 'DIFFERENCE N7 1) N8)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N1)
                                                    N5)
                                              N7)
                                        (LIST 'TIMES 2 N3))
                                  1)
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1)
                                  (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|i3AR6| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3AR6|) 'OPFN) 
(PUT '|i3AR6| 'DEFINED-ON-LINE '708) 
(PUT '|i3AR6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR6| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3AR6| (N1 N2 N3 N4 N5 N6)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1) 0 1)
                            (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) 1 1)))
                (LIST 'DIFFERENCE (LIST '|i3A| N1 N2 N3 N4 N5 N6 0 2)
                      (LIST '|i3A| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 1
                            2)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6) 3))) 
(PUT '|i3AR7| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR7|) 'OPFN) 
(PUT '|i3AR7| 'DEFINED-ON-LINE '714) 
(PUT '|i3AR7| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR7| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR7| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALGREATERP (AEVAL N8) 1)
      (AEVAL (LIST '|i3AR78| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N6) 1)
      (AEVAL (LIST '|i3AR76| N1 N2 N3 N4 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3AR768| N1 N2 N3 N4 N5 N7))))) 
(PUT '|i3AR78| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR78|) 'OPFN) 
(PUT '|i3AR78| 'DEFINED-ON-LINE '720) 
(PUT '|i3AR78| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR78| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR78| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 (LIST 'PLUS N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N4
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3A| N1 N2 N3 (LIST 'PLUS N4 1)
                                              (LIST 'DIFFERENCE N5 1) N6
                                              (LIST 'PLUS N7 1)
                                              (LIST 'DIFFERENCE N8 1))
                                        (LIST '|i3A| N1 N2
                                              (LIST 'DIFFERENCE N3 1)
                                              (LIST 'PLUS N4 1) N5 N6
                                              (LIST 'PLUS N7 1)
                                              (LIST 'DIFFERENCE N8 1))))
                            (LIST 'TIMES N2
                                  (LIST '|i3A| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'PLUS N2 1) N3 N4 N5 N6
                                        (LIST 'PLUS N7 1)
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N2)
                                                    N4)
                                              N8)
                                        (LIST 'TIMES 2 N5))
                                  1)
                            (LIST '|i3A| N1 N2 N3 N4 N5 N6 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|i3AR76| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR76|) 'OPFN) 
(PUT '|i3AR76| 'DEFINED-ON-LINE '728) 
(PUT '|i3AR76| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR76| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR76| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 (LIST 'PLUS N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1)
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) N8)
                                  (LIST '|i3A| N1 N2 N3 (LIST 'PLUS N4 1)
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) N8)))
                      (LIST 'TIMES N2
                            (LIST '|i3A| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'PLUS N2 1) N3 N4 N5
                                  (LIST 'DIFFERENCE N6 1) (LIST 'PLUS N7 1)
                                  N8))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2
                                                          (LIST 'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE
                                                                 (LIST
                                                                  'DIFFERENCE
                                                                  'D N5)
                                                                 N7)
                                                                N8))
                                                    N2)
                                              N4)
                                        N6)
                                  1)
                            (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3AR768| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3AR768|) 'OPFN) 
(PUT '|i3AR768| 'DEFINED-ON-LINE '736) 
(PUT '|i3AR768| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR768| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3AR768| (N1 N2 N3 N4 N5 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE (LIST '|i3A| N1 N2 N3 N4 N5 2 N7 0)
                      (LIST '|i3A| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 2 N7 1))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3A| N1 N2 N3 N4 N5 1 (LIST 'PLUS N7 1) 0)
                            (LIST '|i3A| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) 1
                                  (LIST 'PLUS N7 1) 1))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N7) 3))) 
(PUT '|i3AR3| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR3|) 'OPFN) 
(PUT '|i3AR3| 'DEFINED-ON-LINE '742) 
(PUT '|i3AR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR3| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR3| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALGREATERP (AEVAL N6) 1)
      (AEVAL (LIST '|i3AR36| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N7) 1)
      (AEVAL (LIST '|i3AR37| N1 N2 N3 N4 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3AR367| N1 N2 N3 N4 N5 N8))))) 
(PUT '|i3AR36| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR36|) 'OPFN) 
(PUT '|i3AR36| 'DEFINED-ON-LINE '748) 
(PUT '|i3AR36| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR36| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR36| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 (LIST 'DIFFERENCE N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N8
                            (LIST 'DIFFERENCE
                                  (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1))
                                  (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1))))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N6) N8)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3AR37| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR37|) 'OPFN) 
(PUT '|i3AR37| 'DEFINED-ON-LINE '755) 
(PUT '|i3AR37| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR37| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR37| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5 (LIST 'DIFFERENCE N6 1) N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N8
                            (LIST 'DIFFERENCE
                                  (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1))
                                  (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1)
                                        (LIST 'DIFFERENCE N4 1) N5 N6
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1))))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N7) N8)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|i3A| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6
                                  (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|i3AR367| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3AR367|) 'OPFN) 
(PUT '|i3AR367| 'DEFINED-ON-LINE '762) 
(PUT '|i3AR367| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR367| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3AR367| (N1 N2 N3 N4 N5 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3A| N1 N2 N3 N4 N5 2 1 (LIST 'DIFFERENCE N8 1))
                      (LIST '|i3A| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 2 1 N8))
                (LIST 'DIFFERENCE
                      (LIST '|i3A| N1 N2 N3 N4 N5 1 2 (LIST 'DIFFERENCE N8 1))
                      (LIST '|i3A| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) 1 2
                            N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D (LIST 'TIMES 2 N8)) 2))) 
(PUT '|i3AR57| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3AR57|) 'OPFN) 
(PUT '|i3AR57| 'DEFINED-ON-LINE '768) 
(PUT '|i3AR57| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR57| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3AR57| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3A| N1 N2 N3 N4 (LIST 'PLUS N5 1) N6 N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N6
                            (LIST 'DIFFERENCE
                                  (LIST '|i3A| N1 N2 N3 N4 (LIST 'PLUS N5 1)
                                        (LIST 'PLUS N6 1)
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3A| N1 N2 N3 (LIST 'DIFFERENCE N4 1)
                                        (LIST 'PLUS N5 1) (LIST 'PLUS N6 1)
                                        (LIST 'DIFFERENCE N7 1) N8)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N6) N7)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|i3A| N1 N2 N3 N4 (LIST 'PLUS N5 1) N6
                                  (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|i3AR5| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3AR5|) 'OPFN) 
(PUT '|i3AR5| 'DEFINED-ON-LINE '775) 
(PUT '|i3AR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3AR5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3AR5| (N1 N2 N3 N4 N5 N6)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1) 2 1)
                      (LIST '|i3A| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 2 1))
                (LIST 'DIFFERENCE
                      (LIST '|i3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1) 1 2)
                      (LIST '|i3A| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 1
                            2)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D (LIST 'TIMES 2 N6)) 2))) 
(PUT '|j3A| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3A|) 'OPFN) 
(PUT '|j3A| 'DEFINED-ON-LINE '799) 
(PUT '|j3A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3A| (N1 N2 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N2 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N5 N7) (LIST '|g1A| (LIST 'PLUS N8 N5 N7) N6)
             (LIST '|i1E| N2 (LIST 'PLUS N4 N5 N6 N7 N8)) (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N7 N8) (LIST '|i1B| N1 (LIST 'PLUS N5 N7 N8))
             (LIST '|i1C|
                   (LIST 'PLUS N2 N1 (LIST 'TIMES 2 (LIST 'PLUS N5 N7 N8))) N4)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N8) (LIST '|i1| N1 N5)
             (LIST '|i1D| (LIST 'PLUS N2 N1 (LIST 'TIMES 2 N5))
                   (LIST 'PLUS N4 N6 N8))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1E| N1 (LIST 'PLUS N4 N5 N6 N7 N8))
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 5))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|g2| N4 N6 N5 N7 N8)))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 (LIST 'TIMES 2 (LIST 'PLUS N7 N8 N5))) N6)
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|i2| N1 N2 N7 N8 N5)))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N7)
             (LIST '|i2B| (LIST 'PLUS N1 (LIST 'TIMES 2 N7)) N2 N6 N4 N8))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N7)
             (LIST '|i2A| N1 N2 (LIST 'PLUS N6 N7) N4 N5))))
     ((OR (EVALLESSP (AEVAL N1) 0) (EVALLESSP (AEVAL N2) 0)
          (EVALLESSP (AEVAL N6) 0) (EVALLESSP (AEVAL N7) 0)
          (EVALLESSP (AEVAL N8) 0))
      (AEVAL (LIST '|i3A| N1 N2 0 N4 N5 N6 N7 N8)))
     ((AND (EVALGREATERP (AEVAL N4) 0)
           (NOT
            (AND (EVALGREATERP (AEVAL N5) 0)
                 (EVALLESSP (AEVAL N5) (AEVAL N4)))))
      (AEVAL (LIST '|j3AL4| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N5) 0) (AEVAL (LIST '|j3AL5| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N6) 1) (AEVAL (LIST '|j3AR4| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N7) 1) (AEVAL (LIST '|j3AR5| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|j3AL2| N1 N2 N4 N5 N8)))
     (T
      (AEVAL
       (LIST 'TIMES (LIST '|j3AN| (LIST 'MINUS N5) (LIST 'MINUS N4) N1 N8)
             (LIST '|b| 4)))))) 
(REMEMBER (LIST '|j3A|)) 
(PUT '|j3AL4| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3AL4|) 'OPFN) 
(PUT '|j3AL4| 'DEFINED-ON-LINE '822) 
(PUT '|j3AL4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AL4| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3AL4| (N1 N2 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|j3A| N1 N2 N4 N5 (LIST 'PLUS N6 1) N7
                                  (LIST 'DIFFERENCE N8 1))
                            (LIST '|j3A| N1 N2 (LIST 'DIFFERENCE N4 1) N5
                                  (LIST 'PLUS N6 1) N7 N8)))
                (LIST 'TIMES N1
                      (LIST '|j3A| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N4
                            N5 N6 N7 N8)))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5)
                                        N7)
                                  N8))
                      N1)
                N6))) 
(PUT '|j3AL5| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3AL5|) 'OPFN) 
(PUT '|j3AL5| 'DEFINED-ON-LINE '828) 
(PUT '|j3AL5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AL5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3AL5| (N1 N2 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3A| N1 N2 N4 (LIST 'DIFFERENCE N5 1) N6
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|j3A| N1 N2 N4 N5 N6 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'TIMES N1
                      (LIST '|j3A| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N4
                            N5 N6 N7 N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N7)
                (LIST 'TIMES 2 N5)))) 
(PUT '|j3AR4| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3AR4|) 'OPFN) 
(PUT '|j3AR4| 'DEFINED-ON-LINE '834) 
(PUT '|j3AR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AR4| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3AR4| (N1 N2 N4 N5 N6 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|j3A| N1 N2 (LIST 'PLUS N4 1) N5 N6 N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'TIMES 2
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE 'D
                                                                 N5)
                                                                N7)
                                                          N8))
                                              N1)
                                        N6)
                                  1)
                            (LIST '|j3A| N1 N2 (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8))
                      (LIST 'TIMES N1
                            (LIST '|j3A| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|j3AR5| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3AR5|) 'OPFN) 
(PUT '|j3AR5| 'DEFINED-ON-LINE '841) 
(PUT '|j3AR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AR5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3AR5| (N1 N2 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|j3A| N1 N2 N4 (LIST 'PLUS N5 1) N6 N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N7)
                                        (LIST 'TIMES 2 N5))
                                  1)
                            (LIST '|j3A| N1 N2 N4 (LIST 'PLUS N5 1) N6
                                  (LIST 'DIFFERENCE N7 1) N8))
                      (LIST 'TIMES N1
                            (LIST '|j3A| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N4 (LIST 'PLUS N5 1)
                                  N6 (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3AL2| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3AL2|) 'OPFN) 
(PUT '|j3AL2| 'DEFINED-ON-LINE '848) 
(PUT '|j3AL2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AL2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3AL2| (N1 N2 N4 N5 N8)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D)
                                                    (LIST 'TIMES 2
                                                          (LIST 'PLUS N4 N5
                                                                N8)))
                                              N1)
                                        N2)
                                  3)
                            (LIST '|j3A| N1 (LIST 'DIFFERENCE N2 1) N4 N5 1 1
                                  N8))
                      (LIST 'TIMES N1
                            (LIST '|j3A| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N4 N5 1 1 N8)))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|j3AN| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3AN|) 'OPFN) 
(PUT '|j3AN| 'DEFINED-ON-LINE '854) 
(PUT '|j3AN| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3AN| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3AN| (N1 N2 N3 N4)
    (PROG (S C1 C2 C3 C4 L1 M L2 L3)
      (SETQ S (AEVAL 0))
      (SETQ L1 (AEVAL 0))
      (SETQ C1
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT (MINUS 1) N1) (LIST '|ii1| N3 1 N1 0)
                     (LIST '|ii1| 1 N4 N2 0)
                     (LIST '|i1C|
                           (LIST 'PLUS (LIST 'TIMES 2 (LIST 'DIFFERENCE N4 N1))
                                 (LIST 'DIFFERENCE N3 N2) 3)
                           1))))
      (REPEAT
       (PROGN
        (SETQ C2 (AEVAL* C1))
        (SETQ M (AEVAL* 0))
        (REPEAT
         (PROGN
          (SETQ C3 (AEVAL* C2))
          (SETQ L2 (AEVAL* 0))
          (REPEAT
           (PROGN
            (SETQ C4 (AEVAL* C3))
            (SETQ L3 (AEVAL* 0))
            (REPEAT
             (PROGN
              (SETQ S (AEVAL* (LIST 'PLUS S C4)))
              (SETQ C4
                      (AEVAL*
                       (LIST 'MINUS
                             (LIST 'TIMES C4
                                   (LIST 'QUOTIENT
                                         (LIST 'DIFFERENCE
                                               (LIST 'DIFFERENCE N2 L2) L3)
                                         (LIST 'PLUS L3 1))
                                   (LIST 'QUOTIENT
                                         (LIST 'PLUS
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2
                                                           (LIST 'PLUS
                                                                 (LIST
                                                                  'DIFFERENCE
                                                                  'D N4)
                                                                 (LIST
                                                                  'DIFFERENCE
                                                                  N1 1)))
                                                     N3)
                                               N2 L2 L3)
                                         (LIST 'PLUS
                                               (LIST 'DIFFERENCE
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 'D)
                                                           (LIST 'TIMES 2
                                                                 (LIST
                                                                  'DIFFERENCE
                                                                  N4 N1)))
                                                     N3)
                                               N2 L2
                                               (LIST 'DIFFERENCE L3 4)))))))
              (SETQ L3 (AEVAL* (LIST 'PLUS L3 1)))
              (AEVAL* 'NIL))
             (EVALEQUAL (AEVAL* C4) 0))
            (SETQ C3
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES C3 (LIST 'DIFFERENCE N2 L2)
                                 (LIST 'QUOTIENT
                                       (LIST 'DIFFERENCE
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE N4
                                                               L1)
                                                         M)
                                                   L2)
                                             1)
                                       (LIST 'PLUS L2 1))
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES 2
                                                         (LIST 'PLUS
                                                               (LIST
                                                                'DIFFERENCE 'D
                                                                N4)
                                                               (LIST
                                                                'DIFFERENCE N1
                                                                1)))
                                                   N3)
                                             N2 L2)
                                       (LIST 'TIMES
                                             (LIST 'PLUS
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE
                                                               (LIST 'TIMES 3
                                                                     'D)
                                                               (LIST 'TIMES 2
                                                                     (LIST
                                                                      'DIFFERENCE
                                                                      N4 N1)))
                                                         N3)
                                                   N2 (LIST 'DIFFERENCE L2 4))
                                             (LIST 'PLUS
                                                   (LIST 'DIFFERENCE 'D
                                                         (LIST 'TIMES 2
                                                               (LIST
                                                                'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE N4
                                                                 L1)
                                                                M)))
                                                   N2 L2)))))))
            (SETQ L2 (AEVAL* (LIST 'PLUS L2 1)))
            (AEVAL* 'NIL))
           (EVALEQUAL (AEVAL* C3) 0))
          (SETQ C2
                  (AEVAL*
                   (LIST 'TIMES C2
                         (LIST 'DIFFERENCE (LIST 'DIFFERENCE N1 L1)
                               (LIST 'TIMES 2 M))
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE (LIST 'DIFFERENCE N1 L1)
                                     (LIST 'TIMES 2 M))
                               1)
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'DIFFERENCE (LIST 'DIFFERENCE N4 L1)
                                           M)
                                     1)
                               (LIST 'PLUS M 1))
                         (LIST 'QUOTIENT
                               (LIST 'PLUS 'D
                                     (LIST 'TIMES 2
                                           (LIST 'PLUS (LIST 'MINUS N4) L1 M
                                                 N2)))
                               (LIST 'TIMES
                                     (LIST 'PLUS 'D
                                           (LIST 'TIMES 2
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'DIFFERENCE
                                                                   N1 L1)
                                                             M)
                                                       2)))
                                     (LIST 'PLUS 'D
                                           (LIST 'TIMES 2
                                                 (LIST 'PLUS (LIST 'MINUS N4)
                                                       L1 M))
                                           N2 1)
                                     (LIST 'PLUS 'D
                                           (LIST 'TIMES 2
                                                 (LIST 'PLUS (LIST 'MINUS N4)
                                                       L1 M))
                                           N2))))))
          (SETQ M (AEVAL* (LIST 'PLUS M 1)))
          (AEVAL* 'NIL))
         (EVALEQUAL (AEVAL* C2) 0))
        (SETQ C1
                (AEVAL*
                 (LIST 'TIMES C1 (LIST 'DIFFERENCE N1 L1)
                       (LIST 'QUOTIENT
                             (LIST 'DIFFERENCE (LIST 'DIFFERENCE N4 L1) 1)
                             (LIST 'PLUS L1 1))
                       (LIST 'PLUS (LIST 'DIFFERENCE 'D N3)
                             (LIST 'DIFFERENCE (LIST 'DIFFERENCE N1 L1) 2))
                       (LIST 'QUOTIENT
                             (LIST 'PLUS 'D
                                   (LIST 'TIMES 2
                                         (LIST 'PLUS (LIST 'MINUS N4) L1 N2)))
                             (LIST 'TIMES
                                   (LIST 'PLUS 'D
                                         (LIST 'TIMES 2
                                               (LIST 'DIFFERENCE
                                                     (LIST 'DIFFERENCE N1 L1)
                                                     2)))
                                   (LIST 'PLUS 'D
                                         (LIST 'TIMES 2
                                               (LIST 'PLUS (LIST 'MINUS N4)
                                                     L1))
                                         N2 1)
                                   (LIST 'PLUS 'D
                                         (LIST 'TIMES 2
                                               (LIST 'PLUS (LIST 'MINUS N4)
                                                     L1))
                                         N2))))))
        (SETQ L1 (AEVAL* (LIST 'PLUS L1 1)))
        (AEVAL* 'NIL))
       (EVALEQUAL (AEVAL* C1) 0))
      (RETURN (AEVAL S)))) 
(PUT '|j3B| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3B|) 'OPFN) 
(PUT '|j3B| 'DEFINED-ON-LINE '913) 
(PUT '|j3B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3B| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3B| (N1 N2 N3 N4 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N4)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N3 N7) (LIST '|g1A| (LIST 'PLUS N6 N3 N7) N8)
             (LIST '|i1E| N2 (LIST 'PLUS N4 N8 N6 N3 N7)) (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N8) (LIST '|g1A| (LIST 'PLUS N6 N4 N8) N7)
             (LIST '|i1E| N1 (LIST 'PLUS N3 N7 N6 N4 N8)) (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N8) (LIST '|i1| N1 N3)
             (LIST '|i1B| N2 (LIST 'PLUS N4 N6 N8)) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N7) (LIST '|i1| N2 N4)
             (LIST '|i1B| N1 (LIST 'PLUS N3 N6 N7)) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N7)
             (LIST '|i2B| (LIST 'PLUS N1 (LIST 'TIMES 2 N7)) N2 N6 N4 N8))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N8)
             (LIST '|i2B| (LIST 'PLUS N2 (LIST 'TIMES 2 N8)) N1 N6 N3 N7))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N7 N8)
             (LIST '|i2C| N1 N2 N3 N4 (LIST 'PLUS N7 N8)))))
     ((OR (EVALLESSP (AEVAL N1) 0) (EVALLESSP (AEVAL N2) 0)
          (EVALLESSP (AEVAL N6) 0) (EVALLESSP (AEVAL N7) 0)
          (EVALLESSP (AEVAL N8) 0) (EVALLESSP (AEVAL N3) 0)
          (EVALLESSP (AEVAL N4) 0))
      (AEVAL (LIST '|i3A| N1 N2 N3 N4 0 N6 N7 N8)))
     (T (AEVAL (LIST '|j3BL| N1 N2 N3 N4 N6 N7 N8))))) 
(REMEMBER (LIST '|j3B|)) 
(PUT '|j3BL| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3BL|) 'OPFN) 
(PUT '|j3BL| 'DEFINED-ON-LINE '931) 
(PUT '|j3BL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3BL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3BL| (N1 N2 N3 N4 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3B| N1 N2 N3 N4 (LIST 'DIFFERENCE N6 1)
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|j3B| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N6
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|j3B| N1 N2 N3 N4 (LIST 'DIFFERENCE N6 1) N7
                                  (LIST 'PLUS N8 1))
                            (LIST '|j3B| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N6 N7
                                  (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N7) N8)
                (LIST 'TIMES 2 N6)))) 
(PUT '|ii3A| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3A|) 'OPFN) 
(PUT '|ii3A| 'DEFINED-ON-LINE '947) 
(PUT '|ii3A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3A| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3A| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N1 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N2 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N0) 0) (AEVAL (LIST '|i3A| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N6) 1)
      (AEVAL (LIST '|ii3AL6| N1 N2 N3 N4 N5 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|ii3AL7| N1 N2 N3 N4 N5 N7 N8 N0)))
     ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|ii3AL7| N2 N1 N4 N3 N5 N8 N7 N0)))
     ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|ii3AL1| N1 N2 N3 N4 N5 N0)))
     ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|ii3AL1| N2 N1 N4 N3 N5 N0)))
     (T (AEVAL (LIST '|ii3AR68| N3 N4 N5 N0))))) 
(REMEMBER (LIST '|ii3A|)) 
(PUT '|ii3AL6| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3AL6|) 'OPFN) 
(PUT '|ii3AL6| 'DEFINED-ON-LINE '964) 
(PUT '|ii3AL6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3AL6| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3AL6| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES 2
                      (LIST 'PLUS (LIST 'TIMES 3 'D)
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE N0 N1)
                                        N2)
                                  (LIST 'TIMES 2
                                        (LIST 'PLUS N3 N4 N5 N6 N7 N8)))
                            1)
                      (LIST '|ii3A| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1) N7
                            N8 (LIST 'DIFFERENCE N0 1)))
                (LIST 'TIMES N3
                      (LIST 'DIFFERENCE
                            (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8
                                  (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3A| N1 N2 (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3A| N1 (LIST 'DIFFERENCE N2 1) N3
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3A| N1 N2 N3 (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES 2 (LIST 'DIFFERENCE N0 1)
                            (LIST '|ii3A| N1 N2 N3 N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8
                                  (LIST 'DIFFERENCE N0 2)))))
          (LIST 'DIFFERENCE N6 1))) 
(PUT '|ii3AL7| 'NUMBER-OF-ARGS 8) 
(FLAG '(|ii3AL7|) 'OPFN) 
(PUT '|ii3AL7| 'DEFINED-ON-LINE '972) 
(PUT '|ii3AL7| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3AL7| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3AL7| (N1 N2 N3 N4 N5 N7 N8 N0)
    (LIST 'PLUS
          (LIST 'DIFFERENCE
                (LIST '|ii3A| N1 N2 N3 N4 N5 1 N7 N8 (LIST 'DIFFERENCE N0 1))
                (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 1 N7 N8
                      (LIST 'DIFFERENCE N0 1)))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3A| N1 (LIST 'DIFFERENCE N2 1) N3
                                        N4 (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N7 1) N8
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2 N3
                                        N4 (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N7 1) N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES N3
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3A| N1 N2 (LIST 'PLUS N3 1) N4 N5 1
                                        (LIST 'DIFFERENCE N7 1) N8
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5 1
                                        (LIST 'DIFFERENCE N7 1) N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES 2 N1
                            (LIST '|ii3A| (LIST 'PLUS N1 1) N2 N3 N4 N5 1
                                  (LIST 'DIFFERENCE N7 1) N8
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|ii3AL1| 'NUMBER-OF-ARGS 6) 
(FLAG '(|ii3AL1|) 'OPFN) 
(PUT '|ii3AL1| 'DEFINED-ON-LINE '980) 
(PUT '|ii3AL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3AL1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3AL1| (N1 N2 N3 N4 N5 N0)
    (LIST 'PLUS (LIST '|ii3A| N1 N2 N3 N4 N5 1 1 1 (LIST 'DIFFERENCE N0 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N3
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5 0 1 1
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5 1 0 1
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2 N3
                                        N4 (LIST 'PLUS N5 1) 1 1 0
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2 N3
                                        N4 (LIST 'PLUS N5 1) 1 0 1
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N3)
                                        N5)
                                  1)
                            (LIST '|ii3A| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 1
                                  1 1 (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE N1 1)))) 
(PUT '|ii3AR68| 'NUMBER-OF-ARGS 4) 
(FLAG '(|ii3AR68|) 'OPFN) 
(PUT '|ii3AR68| 'DEFINED-ON-LINE '988) 
(PUT '|ii3AR68| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3AR68| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3AR68| (N3 N4 N5 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST '|ii3A| 1 1 N3 N4 N5 1 1 1
                                  (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3A| 0 1 N3 N4 N5 1 1 1
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE (LIST '|ii3A| 1 1 N3 N4 N5 2 0 1 N0)
                      (LIST '|ii3A| 1 1 (LIST 'DIFFERENCE N3 1) N4 N5 2 1 1
                            N0))
                (LIST 'DIFFERENCE (LIST '|ii3A| 1 1 N3 N4 N5 1 0 2 N0)
                      (LIST '|ii3A| 1 1 N3 N4 (LIST 'DIFFERENCE N5 1) 1 1 2
                            N0)))
          (LIST 'PLUS 'D (LIST 'DIFFERENCE N0 4)))) 
(PUT '|i3B| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3B|) 'OPFN) 
(PUT '|i3B| 'DEFINED-ON-LINE '1027) 
(PUT '|i3B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3B| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3B| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N1 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N2 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N7)) 0))
      0)
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N7)
             (LIST '|i2C| N1 N2 N4 N5 (LIST 'PLUS N8 N6 N7)))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|i2| N1 N3 N8 N7 N6))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 (LIST 'TIMES 2 (LIST 'PLUS N6 N7 N8)))
                   N5))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|i2| N2 N3 N8 N6 N7))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 (LIST 'TIMES 2 (LIST 'PLUS N6 N7 N8)))
                   N4))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N7)
             (LIST '|i2B| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N7)) N1 N5 N4 N8))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N6)
             (LIST '|i2B| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N6)) N2 N4 N5 N8))))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALNEQ (AEVAL N6) 1)
        (AEVAL (LIST '|i3BR36| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1)
        (AEVAL (LIST '|i3BR36| N2 N1 N3 N5 N4 N7 N6 N8)))
       (T (AEVAL (LIST '|i3BR3| N1 N2 N3 N4 N5 N8)))))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i3BR4| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N5) 0) (AEVAL (LIST '|i3BR4| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N6) 0) (AEVAL (LIST '|i3BR6| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N7) 0) (AEVAL (LIST '|i3BR6| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|j3A| N3 N2 N5 N7 N4 N6 N8)))
     ((EVALEQUAL (AEVAL N2) 0) (AEVAL (LIST '|j3A| N3 N1 N4 N6 N5 N7 N8)))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i3BR1| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i3BR1| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALEQUAL (AEVAL N8) 0) (AEVAL (LIST '|j3C| N1 N2 N3 N4 N5 N6 N7)))
     ((EVALLESSP (AEVAL N8) 0)
      (COND
       ((EVALNEQ (AEVAL N6) 1)
        (AEVAL (LIST '|i3BR86| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1)
        (AEVAL (LIST '|i3BR86| N2 N1 N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3BR84| N1 N2 N3 N4 N5 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|i3BR84| N2 N1 N3 N5 N4 N8)))
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|i3BR81| N1 N2 N3 N8)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|i3BR81| N2 N1 N3 N8)))
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|i3BR83| N3 N8)))
       (T (AEVAL (LIST '|i3BR8| N8)))))
     ((EVALLEQ (AEVAL (LIST 'MAX N1 N6)) (AEVAL (LIST 'MAX N2 N7)))
      (AEVAL (LIST '|i3BL| N1 N2 N3 N4 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3BL| N2 N1 N3 N5 N4 N7 N6 N8))))) 
(REMEMBER (LIST '|i3B|)) 
(PUT '|i3BL| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BL|) 'OPFN) 
(PUT '|i3BL| 'DEFINED-ON-LINE '1065) 
(PUT '|i3BL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BL| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BL| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|i3B| N1 N2 N3 N4 N5 N6 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'TIMES N3
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1)
                            N4 N5 N6 N7 N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3) N7)
                (LIST 'TIMES 2 N6)))) 
(PUT '|i3BR36| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR36|) 'OPFN) 
(PUT '|i3BR36| 'DEFINED-ON-LINE '1071) 
(PUT '|i3BR36| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR36| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR36| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5 N6 N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N8
                            (LIST 'DIFFERENCE
                                  (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1))
                                  (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1))))
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N4
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2
                                              (LIST 'PLUS N3 1)
                                              (LIST 'PLUS N4 1) N5
                                              (LIST 'DIFFERENCE N6 1) N7 N8)
                                        (LIST '|i3B| N1 N2 (LIST 'PLUS N3 1)
                                              (LIST 'PLUS N4 1) N5
                                              (LIST 'DIFFERENCE N6 1) N7 N8)))
                            (LIST 'TIMES 2 N1
                                  (LIST '|i3B| (LIST 'PLUS N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8))))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3BR3| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3BR3|) 'OPFN) 
(PUT '|i3BR3| 'DEFINED-ON-LINE '1079) 
(PUT '|i3BR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR3| (N1 N2 N3 N4 N5 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST '|i3B| N1 N2 N3 N4 N5 0 2 N8)
                      (LIST '|i3B| N1 N2 N3 N4 N5 1 2 (LIST 'DIFFERENCE N8 1)))
                (LIST 'TIMES N3
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1)
                            N4 N5 1 1 N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3) 3))) 
(PUT '|i3BR4| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR4|) 'OPFN) 
(PUT '|i3BR4| 'DEFINED-ON-LINE '1085) 
(PUT '|i3BR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR4| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR4| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|i3BR45| N1 N2 N3 N4 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3BR4A| N1 N2 N3 N4 N6 N7 N8))))) 
(PUT '|i3BR45| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR45|) 'OPFN) 
(PUT '|i3BR45| 'DEFINED-ON-LINE '1090) 
(PUT '|i3BR45| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR45| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR45| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3B| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N7
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3B| N1 N2 N3 (LIST 'PLUS N4 1)
                                              (LIST 'DIFFERENCE N5 1) N6
                                              (LIST 'PLUS N7 1)
                                              (LIST 'DIFFERENCE N8 1))
                                        (LIST '|i3B| N1 N2 N3 (LIST 'PLUS N4 1)
                                              (LIST 'DIFFERENCE N5 1)
                                              (LIST 'DIFFERENCE N6 1)
                                              (LIST 'PLUS N7 1) N8)))
                            (LIST 'TIMES N2
                                  (LIST '|i3B| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'PLUS N2 1) N3 (LIST 'PLUS N4 1)
                                        (LIST 'DIFFERENCE N5 1) N6 N7 N8)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N2)
                                                    N5)
                                              N7)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|i3B| N1 N2 N3 (LIST 'PLUS N4 1)
                                  (LIST 'DIFFERENCE N5 1) N6 N7 N8)))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|i3BR4A| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3BR4A|) 'OPFN) 
(PUT '|i3BR4A| 'DEFINED-ON-LINE '1098) 
(PUT '|i3BR4A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR4A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR4A| (N1 N2 N3 N4 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| N1 N2 N3 (LIST 'PLUS N4 1) 1 N6 N7 N8)
                            (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2 N3
                                  (LIST 'PLUS N4 1) 1 N6 N7 N8)))
                (LIST 'DIFFERENCE (LIST '|i3B| N1 N2 N3 N4 2 N6 N7 N8)
                      (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1) N3 N4 2 N6 N7
                            N8)))
          (LIST 'TIMES 2
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1) N2)
                            N3)
                      (LIST 'TIMES 2 (LIST 'PLUS N4 N6 N7 N8 1)))))) 
(PUT '|i3BR6| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR6|) 'OPFN) 
(PUT '|i3BR6| 'DEFINED-ON-LINE '1104) 
(PUT '|i3BR6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR6| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR6| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|i3BR67| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|i3BR63| N1 N2 N3 N4 N5 N6 N8)))
     (T (AEVAL (LIST '|i3BR6A| N1 N2 N4 N5 N6 N8))))) 
(PUT '|i3BR67| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR67|) 'OPFN) 
(PUT '|i3BR67| 'DEFINED-ON-LINE '1110) 
(PUT '|i3BR67| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR67| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR67| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3B| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1) N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N7)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|i3B| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1)
                                  (LIST 'DIFFERENCE N7 1) N8))
                      (LIST 'TIMES N3
                            (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5 (LIST 'PLUS N6 1)
                                  (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|i3BR63| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3BR63|) 'OPFN) 
(PUT '|i3BR63| 'DEFINED-ON-LINE '1117) 
(PUT '|i3BR63| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR63| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR63| (N1 N2 N3 N4 N5 N6 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| N1 N2 (LIST 'DIFFERENCE N3 2) N4 N5
                                  (LIST 'PLUS N6 1) 1 N8)
                            (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) 1 N8)))
                (LIST 'DIFFERENCE
                      (LIST '|i3B| N1 N2 (LIST 'DIFFERENCE N3 2) N4 N5 N6 2 N8)
                      (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1)
                            (LIST 'DIFFERENCE N3 1) N4 N5 N6 2 N8)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i3BR6A| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3BR6A|) 'OPFN) 
(PUT '|i3BR6A| 'DEFINED-ON-LINE '1123) 
(PUT '|i3BR6A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR6A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR6A| (N1 N2 N4 N5 N6 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| N1 N2 1 N4 N5 (LIST 'PLUS N6 1) 0 N8)
                            (LIST '|i3B| N1 N2 1 N4 N5 (LIST 'PLUS N6 1) 1
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1) 2 N4 N5 N6 1 N8))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6) 3))) 
(PUT '|i3BR1| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR1|) 'OPFN) 
(PUT '|i3BR1| 'DEFINED-ON-LINE '1129) 
(PUT '|i3BR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR1| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|i3BR18| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3BR16| N1 N2 N3 N4 N5 N6 N7)))
     ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3BR14| N1 N2 N3 N4 N5 N7)))
     ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|i3BR12| N1 N2 N3 N5 N7)))
     ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|i3BR13| N1 N3 N5 N7)))
     (T (AEVAL (LIST '|i3BR1A| N1 N5 N7))))) 
(PUT '|i3BR18| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR18|) 'OPFN) 
(PUT '|i3BR18| 'DEFINED-ON-LINE '1138) 
(PUT '|i3BR18| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR18| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR18| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3B| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 N4
                                        (LIST 'PLUS N5 1) N6 N7
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3B| (LIST 'PLUS N1 1) N2 N3 N4
                                        (LIST 'PLUS N5 1) N6 N7
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N7
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3B| (LIST 'PLUS N1 1)
                                              (LIST 'DIFFERENCE N2 1) N3 N4 N5
                                              N6 (LIST 'PLUS N7 1)
                                              (LIST 'DIFFERENCE N8 1))
                                        (LIST '|i3B| (LIST 'PLUS N1 1) N2
                                              (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                              (LIST 'PLUS N7 1)
                                              (LIST 'DIFFERENCE N8 1))))
                            (LIST 'TIMES 2 N2
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'PLUS N2 1) N3 N4 N5 N6 N7
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|i3BR16| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3BR16|) 'OPFN) 
(PUT '|i3BR16| 'DEFINED-ON-LINE '1146) 
(PUT '|i3BR16| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR16| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR16| (N1 N2 N3 N4 N5 N6 N7)
    (LIST 'PLUS
          (LIST '|i3B| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7
                1)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|i3B| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) 1)
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) 1)))
                      (LIST 'TIMES 2 N3
                            (LIST '|i3B| (LIST 'PLUS N1 1) N2 (LIST 'PLUS N3 1)
                                  N4 N5 (LIST 'DIFFERENCE N6 1) N7 1)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3BR14| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3BR14|) 'OPFN) 
(PUT '|i3BR14| 'DEFINED-ON-LINE '1153) 
(PUT '|i3BR14| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR14| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR14| (N1 N2 N3 N4 N5 N7)
    (LIST 'PLUS (LIST '|i3B| (LIST 'PLUS N1 1) N2 N3 N4 N5 1 N7 1)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|i3B| (LIST 'PLUS N1 1) N2 N3
                                        (LIST 'DIFFERENCE N4 1)
                                        (LIST 'PLUS N5 1) 1 N7 1)
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3
                                        (LIST 'DIFFERENCE N4 1)
                                        (LIST 'PLUS N5 1) 1 N7 1)))
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        (LIST 'TIMES 2 (LIST 'PLUS N4 N5 N7)))
                                  3)
                            (LIST '|i3B| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'DIFFERENCE N4 1) N5 1 N7 1)))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3BR12| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3BR12|) 'OPFN) 
(PUT '|i3BR12| 'DEFINED-ON-LINE '1160) 
(PUT '|i3BR12| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR12| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR12| (N1 N2 N3 N5 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 1
                                  (LIST 'PLUS N5 1) 1 N7 0)
                            (LIST '|i3B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 0
                                  (LIST 'PLUS N5 1) 1 N7 1)))
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 1 N5 1
                                        (LIST 'PLUS N7 1) 0)
                                  (LIST '|i3B| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 1 N5 0
                                        (LIST 'PLUS N7 1) 1)))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N2) N5)
                                        N7)
                                  1)
                            (LIST '|i3B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 1 N5 1 N7 1))))
          (LIST 'DIFFERENCE N2 1))) 
(PUT '|i3BR13| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3BR13|) 'OPFN) 
(PUT '|i3BR13| 'DEFINED-ON-LINE '1167) 
(PUT '|i3BR13| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR13| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR13| (N1 N3 N5 N7)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| (LIST 'PLUS N1 1) 1
                                  (LIST 'DIFFERENCE N3 1) 1 N5 0
                                  (LIST 'PLUS N7 1) 1)
                            (LIST '|i3B| (LIST 'PLUS N1 1) 1
                                  (LIST 'DIFFERENCE N3 1) 1 N5 1
                                  (LIST 'PLUS N7 1) 0)))
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3) N7) 1)
                      (LIST '|i3B| (LIST 'PLUS N1 1) 1 (LIST 'DIFFERENCE N3 1)
                            1 N5 1 N7 1)))
          (LIST 'DIFFERENCE N3 1))) 
(PUT '|i3BR1A| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i3BR1A|) 'OPFN) 
(PUT '|i3BR1A| 'DEFINED-ON-LINE '1173) 
(PUT '|i3BR1A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR1A| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR1A| (N1 N5 N7)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N1
                            (LIST '|i3B| (LIST 'PLUS N1 1) 1 1 1 N5 1 N7 1))
                      (LIST '|i3B| N1 2 1 1 N5 1 N7 1)
                      (LIST '|i3B| N1 1 2 1 N5 1 N7 1))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1)
                            (LIST 'TIMES 2 (LIST 'PLUS N5 N7)))
                      8)))) 
(PUT '|i3BR86| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3BR86|) 'OPFN) 
(PUT '|i3BR86| 'DEFINED-ON-LINE '1178) 
(PUT '|i3BR86| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR86| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3BR86| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|i3B| N1 N2 N3 N4 N5 N6 (LIST 'DIFFERENCE N7 1)
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N3
                            (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7
                                  (LIST 'PLUS N8 1)))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N6)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|i3B| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  N7 (LIST 'PLUS N8 1))))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3BR84| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3BR84|) 'OPFN) 
(PUT '|i3BR84| 'DEFINED-ON-LINE '1185) 
(PUT '|i3BR84| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR84| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR84| (N1 N2 N3 N4 N5 N8)
    (LIST 'PLUS
          (LIST '|i3B| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) 1 1
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N1
                            (LIST '|i3B| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3
                                  (LIST 'DIFFERENCE N4 1) N5 1 1
                                  (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N3
                            (LIST '|i3B| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1) N5
                                  1 1 (LIST 'PLUS N8 1)))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2
                                                          (LIST 'DIFFERENCE 'D
                                                                N8))
                                                    N1)
                                              N3)
                                        N4)
                                  5)
                            (LIST '|i3B| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 1
                                  1 (LIST 'PLUS N8 1))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3BR81| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3BR81|) 'OPFN) 
(PUT '|i3BR81| 'DEFINED-ON-LINE '1192) 
(PUT '|i3BR81| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR81| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3BR81| (N1 N2 N3 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 2) N2 N3 2 1 1 1 N8)
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2 N3 2 1 1 1 N8))
                (LIST 'DIFFERENCE
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 2) N2 N3 1 1 2 1 N8)
                      (LIST '|i3B| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'DIFFERENCE N3 1) 1 1 2 1 N8))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|i3B| (LIST 'DIFFERENCE N1 2) N2 N3 1 1 1 1
                                  (LIST 'PLUS N8 1))
                            (LIST '|i3B| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 1 1 1 1
                                  (LIST 'PLUS N8 1)))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3BR83| 'NUMBER-OF-ARGS 2) 
(FLAG '(|i3BR83|) 'OPFN) 
(PUT '|i3BR83| 'DEFINED-ON-LINE '1199) 
(PUT '|i3BR83| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR83| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |i3BR83| (N3 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3B| 1 1 (LIST 'DIFFERENCE N3 2) 1 1 2 1 N8)
                      (LIST '|i3B| 0 1 (LIST 'DIFFERENCE N3 1) 1 1 2 1 N8))
                (LIST 'DIFFERENCE
                      (LIST '|i3B| 1 1 (LIST 'DIFFERENCE N3 2) 1 1 1 2 N8)
                      (LIST '|i3B| 1 0 (LIST 'DIFFERENCE N3 1) 1 1 1 2 N8)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i3BR8| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i3BR8|) 'OPFN) 
(PUT '|i3BR8| 'DEFINED-ON-LINE '1205) 
(PUT '|i3BR8| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3BR8| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i3BR8| (N8)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS (LIST '|i3B| 2 1 1 1 1 1 1 N8)
                      (LIST '|i3B| 1 2 1 1 1 1 1 N8)
                      (LIST '|i3B| 1 1 2 1 1 1 1 N8))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) (LIST 'TIMES 2 N8))
                      11)))) 
(PUT '|j3C| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3C|) 'OPFN) 
(PUT '|j3C| 'DEFINED-ON-LINE '1245) 
(PUT '|j3C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3C| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3C| (N1 N2 N3 N4 N5 N6 N7)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N6)
             (LIST '|i2A| N3 N2 (LIST 'PLUS N4 N6) N5 N7))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N5 N7)
             (LIST '|i2A| N3 N1 (LIST 'PLUS N5 N7) N4 N6))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N7)
             (LIST '|i2C| N1 N2 N4 N5 (LIST 'PLUS N6 N7)))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N6)
             (LIST '|i1A| (LIST 'PLUS N3 N1 (LIST 'TIMES 2 N6)) N7)
             (LIST '|i1C|
                   (LIST 'PLUS N2 N3 N1 (LIST 'TIMES 2 (LIST 'PLUS N7 N6))) N5)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N7)
             (LIST '|i1A| (LIST 'PLUS N3 N2 (LIST 'TIMES 2 N7)) N6)
             (LIST '|i1C|
                   (LIST 'PLUS N1 N3 N2 (LIST 'TIMES 2 (LIST 'PLUS N6 N7))) N4)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N4) (LIST '|i1| N3 N7)
             (LIST '|i1A| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N7)) N5)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5) (LIST '|i1| N3 N6)
             (LIST '|i1A| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N6)) N4)
             (LIST '|b| 2))))
     ((OR (EVALLESSP (AEVAL N1) 0) (EVALLESSP (AEVAL N2) 0)
          (EVALLESSP (AEVAL N3) 0) (EVALLESSP (AEVAL N4) 0)
          (EVALLESSP (AEVAL N5) 0) (EVALLESSP (AEVAL N6) 0)
          (EVALLESSP (AEVAL N7) 0))
      (AEVAL (LIST '|i3B| N1 N2 N3 N4 N5 N6 N7 0)))
     ((EVALGREATERP (AEVAL N1) 1) (AEVAL (LIST '|j3CL1| N1 N2 N3 N4 N5 N6 N7)))
     ((EVALGREATERP (AEVAL N2) 1) (AEVAL (LIST '|j3CL1| N2 N1 N3 N5 N4 N7 N6)))
     ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|j3CL3| N3 N4 N5 N6 N7)))
     ((EVALGREATERP (AEVAL N6) 1) (AEVAL (LIST '|j3CL6| N4 N5 N6 N7)))
     ((EVALGREATERP (AEVAL N7) 1) (AEVAL (LIST '|j3CL6| N5 N4 N7 N6)))
     ((EVALGREATERP (AEVAL N4) 1) (AEVAL (LIST '|j3CL4| N4 N5)))
     ((EVALGREATERP (AEVAL N5) 1) (AEVAL (LIST '|j3CL4| N5 N4)))
     (T (AEVAL (LIST '|b| 8))))) 
(REMEMBER (LIST '|j3C|)) 
(PUT '|j3CL1| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3CL1|) 'OPFN) 
(PUT '|j3CL1| 'DEFINED-ON-LINE '1271) 
(PUT '|j3CL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3CL1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3CL1| (N1 N2 N3 N4 N5 N6 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|j3C| (LIST 'DIFFERENCE N1 2) N2 N3
                                  (LIST 'PLUS N4 1) N5 N6 N7)
                            (LIST '|j3C| (LIST 'DIFFERENCE N1 1) N2 N3
                                  (LIST 'PLUS N4 1) N5 N6 N7)))
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|j3C| (LIST 'DIFFERENCE N1 2) N2 N3 N4 N5
                                  (LIST 'PLUS N6 1) N7)
                            (LIST '|j3C| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) N7))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|j3CL3| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3CL3|) 'OPFN) 
(PUT '|j3CL3| 'DEFINED-ON-LINE '1277) 
(PUT '|j3CL3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3CL3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3CL3| (N3 N4 N5 N6 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|j3C| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5
                                  (LIST 'PLUS N6 1) N7)
                            (LIST '|j3C| 0 1 (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) N7)))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3C| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5 N6
                                  (LIST 'PLUS N7 1))
                            (LIST '|j3C| 1 0 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1)))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|j3CL6| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3CL6|) 'OPFN) 
(PUT '|j3CL6| 'DEFINED-ON-LINE '1283) 
(PUT '|j3CL6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3CL6| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3CL6| (N4 N5 N6 N7)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'QUOTIENT (LIST 'QUOTIENT N4 2)
                                  (LIST 'DIFFERENCE N6 1))
                            (LIST 'PLUS
                                  (LIST 'TIMES (LIST 'PLUS N4 1)
                                        (LIST 'DIFFERENCE
                                              (LIST '|j3C| 0 1 1
                                                    (LIST 'PLUS N4 2) N5
                                                    (LIST 'DIFFERENCE N6 1) N7)
                                              (LIST '|j3C| 1 1 1
                                                    (LIST 'PLUS N4 2) N5
                                                    (LIST 'DIFFERENCE N6 1)
                                                    N7)))
                                  (LIST 'TIMES 2
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D
                                                    (LIST 'TIMES 2 N4))
                                              3)
                                        (LIST '|j3C| 1 1 1 (LIST 'PLUS N4 1) N5
                                              (LIST 'DIFFERENCE N6 1) N7))))
                      (LIST 'TIMES (LIST 'QUOTIENT N4 2)
                            (LIST 'DIFFERENCE
                                  (LIST '|j3C| 1 1 0 (LIST 'PLUS N4 1) N5 N6
                                        N7)
                                  (LIST '|j3C| 0 1 1 (LIST 'PLUS N4 1) N5 N6
                                        N7))))
                (LIST '|j3C| 2 1 0 N4 N5 N6 N7))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D (LIST 'TIMES 2 N6)) 1))) 
(PUT '|j3CL4| 'NUMBER-OF-ARGS 2) 
(FLAG '(|j3CL4|) 'OPFN) 
(PUT '|j3CL4| 'DEFINED-ON-LINE '1290) 
(PUT '|j3CL4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3CL4| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |j3CL4| (N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'QUOTIENT
                                  (LIST 'PLUS
                                        (LIST 'TIMES 4
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N4) 2)
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          (LIST 'TIMES 2
                                                                (LIST 'PLUS N4
                                                                      N5)))
                                                    5)
                                              (LIST '|j3C| 1 1 1
                                                    (LIST 'DIFFERENCE N4 1) N5
                                                    1 1))
                                        (LIST 'TIMES N5
                                              (LIST 'PLUS
                                                    (LIST '|j3C| 2 0 1
                                                          (LIST 'DIFFERENCE N4
                                                                1)
                                                          (LIST 'PLUS N5 1) 1
                                                          1)
                                                    (LIST '|j3C| 1 0 2
                                                          (LIST 'DIFFERENCE N4
                                                                1)
                                                          (LIST 'PLUS N5 1) 1
                                                          1)
                                                    (LIST 'TIMES 2
                                                          (LIST 'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE 'D
                                                                 N4)
                                                                2)
                                                          (LIST '|j3C| 1 0 1
                                                                (LIST
                                                                 'DIFFERENCE N4
                                                                 1)
                                                                (LIST 'PLUS N5
                                                                      1)
                                                                1 1)))))
                                  (LIST 'DIFFERENCE N4 1))
                            (LIST '|j3C| 0 2 1 N4 N5 1 1))
                      (LIST '|j3C| 0 1 2 N4 N5 1 1))
                (LIST 'TIMES 2 (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4) 2)
                      (LIST '|j3C| 0 1 1 N4 N5 1 1)))
          (LIST 'TIMES 2
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 2 'D) N4) N5)
                      5)))) 
(PUT '|ii3B| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3B|) 'OPFN) 
(PUT '|ii3B| 'DEFINED-ON-LINE '1305) 
(PUT '|ii3B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3B| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3B| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N1 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N2 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N7)) 0))
      0)
     ((EVALEQUAL (AEVAL N0) 0) (AEVAL (LIST '|i3B| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N4) 1)
      (AEVAL (LIST '|ii3BL04| N1 N2 N3 N4 N5 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N5) 1)
      (AEVAL (LIST '|ii3BL04| N2 N1 N3 N5 N4 N7 N6 N8 N0)))
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|ii3BL06| N1 N2 N3 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|ii3BL06| N2 N1 N3 N7 N6 N8 N0)))
     ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|ii3BL03| N1 N2 N3 N8 N0)))
     (T (AEVAL (LIST '|ii3BL0| N1 N2 N8 N0))))) 
(PUT '|ii3BL04| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3BL04|) 'OPFN) 
(PUT '|ii3BL04| 'DEFINED-ON-LINE '1319) 
(PUT '|ii3BL04| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3BL04| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3BL04| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (LIST 'PLUS
          (LIST '|ii3B| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1) N7 N8
                (LIST 'DIFFERENCE N0 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N8
                                  (LIST 'DIFFERENCE
                                        (LIST '|ii3B| N1 N2 N3
                                              (LIST 'DIFFERENCE N4 1) N5
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))
                                        (LIST '|ii3B| N1 N2 N3
                                              (LIST 'DIFFERENCE N4 1) N5 N6
                                              (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))))
                            (LIST 'TIMES N1
                                  (LIST '|ii3B| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1)
                                        (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N1)
                                                    N4)
                                              N8)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|ii3B| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5
                                  N6 N7 N8 (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|ii3BL06| 'NUMBER-OF-ARGS 7) 
(FLAG '(|ii3BL06|) 'OPFN) 
(PUT '|ii3BL06| 'DEFINED-ON-LINE '1327) 
(PUT '|ii3BL06| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3BL06| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3BL06| (N1 N2 N3 N6 N7 N8 N0)
    (LIST 'PLUS (LIST '|ii3B| N1 N2 N3 0 1 N6 N7 N8 (LIST 'DIFFERENCE N0 1))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N8
                                  (LIST 'DIFFERENCE
                                        (LIST '|ii3B| N1 N2 N3 0 1
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))
                                        (LIST '|ii3B| N1 N2 N3 1 0
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))))
                            (LIST 'TIMES N1
                                  (LIST '|ii3B| (LIST 'PLUS N1 1) N2 N3 1 1
                                        (LIST 'DIFFERENCE N6 1) N7 N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N6)
                                        N8)
                                  1)
                            (LIST '|ii3B| N1 N2 N3 1 1 (LIST 'DIFFERENCE N6 1)
                                  N7 N8 (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|ii3BL03| 'NUMBER-OF-ARGS 5) 
(FLAG '(|ii3BL03|) 'OPFN) 
(PUT '|ii3BL03| 'DEFINED-ON-LINE '1335) 
(PUT '|ii3BL03| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3BL03| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3BL03| (N1 N2 N3 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST '|ii3B| N1 N2 (LIST 'DIFFERENCE N3 1) 1 1 1 1
                                  N8 (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3B| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 1 1
                                  N8 (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE
                      (LIST '|ii3B| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 2 1 N8
                            N0)
                      (LIST '|ii3B| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'DIFFERENCE N3 1) 1 1 2 1 N8 N0))
                (LIST 'DIFFERENCE
                      (LIST '|ii3B| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 1 2 N8
                            N0)
                      (LIST '|ii3B| N1 (LIST 'DIFFERENCE N2 1)
                            (LIST 'DIFFERENCE N3 1) 1 1 1 2 N8 N0)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|ii3BL0| 'NUMBER-OF-ARGS 4) 
(FLAG '(|ii3BL0|) 'OPFN) 
(PUT '|ii3BL0| 'DEFINED-ON-LINE '1342) 
(PUT '|ii3BL0| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3BL0| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3BL0| (N1 N2 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST '|ii3B| N1 N2 1 0 1 1 1 N8
                                  (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3B| N1 N2 1 1 1 0 1 N8
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|ii3B| N1 N2 1 1 1 0 2 N8 N0)
                            (LIST '|ii3B| N1 N2 1 1 1 1 2
                                  (LIST 'DIFFERENCE N8 1) N0))
                      (LIST '|ii3B| (LIST 'DIFFERENCE N1 1) N2 2 1 1 1 1 N8
                            N0)))
          (LIST 'PLUS 'D (LIST 'DIFFERENCE N0 4)))) 
(PUT '|i3C| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3C|) 'OPFN) 
(PUT '|i3C| 'DEFINED-ON-LINE '1379) 
(PUT '|i3C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3C| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3C| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N6)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N4 N6)
             (LIST '|i2A| N3 N2 (LIST 'PLUS N8 N4 N6) N5 N7))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N5 N7)
             (LIST '|i2A| N3 N1 (LIST 'PLUS N8 N5 N7) N4 N6))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N6)
             (LIST '|i2B| (LIST 'PLUS N3 N1 (LIST 'TIMES 2 N6)) N2 N8 N5 N7))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N7)
             (LIST '|i2B| (LIST 'PLUS N3 N2 (LIST 'TIMES 2 N7)) N1 N8 N4 N6))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N1 N4) (LIST '|i2| N3 N2 N8 N5 N7))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N2 N5) (LIST '|i2| N3 N1 N8 N4 N6))))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i3CR1| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i3CR1| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i3CR4| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N5) 0) (AEVAL (LIST '|i3CR4| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N6) 0) (AEVAL (LIST '|i3CR6| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N7) 0) (AEVAL (LIST '|i3CR6| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3B| N1 N2 N4 N5 N8 N6 N7)))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALNEQ (AEVAL N1) 1)
        (AEVAL (LIST '|i3CR31| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N2) 1)
        (AEVAL (LIST '|i3CR31| N2 N1 N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3CR36| N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|i3CR36| N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|i3CR38| N3 N4 N5 N8)))
       (T (AEVAL (LIST '|i3CR3| N3 N4 N5)))))
     ((EVALEQUAL (AEVAL N8) 0) (AEVAL (LIST '|j3C| N1 N2 N3 N4 N5 N6 N7)))
     ((EVALLESSP (AEVAL N8) 0)
      (COND
       ((EVALNEQ (AEVAL N6) 1)
        (AEVAL (LIST '|i3CR86| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1)
        (AEVAL (LIST '|i3CR86| N2 N1 N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3CR84| N1 N2 N3 N4 N5 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|i3CR84| N2 N1 N3 N5 N4 N8)))
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|i3CR83| N1 N2 N3 N8)))
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|i3CR81| N1 N2 N8)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|i3CR81| N2 N1 N8)))
       (T (AEVAL (LIST '|i3CR8| N8)))))
     ((EVALLESSP (AEVAL N7) (AEVAL N6))
      (AEVAL (LIST '|i3CL| N2 N1 N3 N5 N4 N7 N6 N8)))
     (T (AEVAL (LIST '|i3CL| N1 N2 N3 N4 N5 N6 N7 N8))))) 
(REMEMBER (LIST '|i3C|)) 
(PUT '|i3CL| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CL|) 'OPFN) 
(PUT '|i3CL| 'DEFINED-ON-LINE '1421) 
(PUT '|i3CL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CL| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CL| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| N1 N2 N3 (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)
                            (LIST '|i3C| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'TIMES N1
                      (LIST '|i3C| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1)
                            N4 N5 N6 N7 N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N4)
                (LIST 'TIMES 2 N6)))) 
(PUT '|i3CR1| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR1|) 'OPFN) 
(PUT '|i3CR1| 'DEFINED-ON-LINE '1427) 
(PUT '|i3CR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR1| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3CR16| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3CR14| N1 N2 N3 N4 N5 N7 N8)))
     (T (AEVAL (LIST '|i3CR1A| N1 N2 N3 N5 N7 N8))))) 
(PUT '|i3CR16| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR16|) 'OPFN) 
(PUT '|i3CR16| 'DEFINED-ON-LINE '1433) 
(PUT '|i3CR16| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR16| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR16| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3C| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) N8)
                                  (LIST '|i3C| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3 N4 N5
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'PLUS N7 1) N8)))
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N8
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                              (LIST 'DIFFERENCE N3 1) N4 N5
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1))
                                        (LIST '|i3C| (LIST 'PLUS N1 1) N2 N3 N4
                                              N5 (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1))))
                            (LIST 'TIMES 2 N3
                                  (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8))))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3CR14| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3CR14|) 'OPFN) 
(PUT '|i3CR14| 'DEFINED-ON-LINE '1441) 
(PUT '|i3CR14| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR14| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR14| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'DIFFERENCE (LIST '|i3C| (LIST 'PLUS N1 1) N2 N3 N4 N5 1 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1)
                                        (LIST 'DIFFERENCE N4 1) N5 1
                                        (LIST 'PLUS N7 1) N8)
                                  (LIST '|i3C| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) N3
                                        (LIST 'DIFFERENCE N4 1) N5 1
                                        (LIST 'PLUS N7 1) N8)))
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES N8
                                        (LIST 'DIFFERENCE
                                              (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                                    (LIST 'DIFFERENCE N3 1)
                                                    (LIST 'DIFFERENCE N4 1) N5
                                                    1 N7 (LIST 'PLUS N8 1))
                                              (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                                    N3 (LIST 'DIFFERENCE N4 1)
                                                    N5 1 N7
                                                    (LIST 'PLUS N8 1))))
                                  (LIST 'TIMES 2 N3
                                        (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                              (LIST 'PLUS N3 1)
                                              (LIST 'DIFFERENCE N4 1) N5 1 N7
                                              N8)))
                            (LIST 'TIMES 2 (LIST 'PLUS N1 1)
                                  (LIST '|i3C| (LIST 'PLUS N1 2) N2 N3
                                        (LIST 'DIFFERENCE N4 1) N5 1 N7 N8))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3CR1A| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3CR1A|) 'OPFN) 
(PUT '|i3CR1A| 'DEFINED-ON-LINE '1450) 
(PUT '|i3CR1A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR1A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR1A| (N1 N2 N3 N5 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST '|i3C| N1 N2 N3 0 N5 2 N7 N8)
                      (LIST '|i3C| N1 N2 N3 1 N5 2 N7 (LIST 'DIFFERENCE N8 1)))
                (LIST 'TIMES N1
                      (LIST '|i3C| (LIST 'PLUS N1 1) N2 N3 1 N5 1 N7 N8)))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) 3))) 
(PUT '|i3CR4| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR4|) 'OPFN) 
(PUT '|i3CR4| 'DEFINED-ON-LINE '1456) 
(PUT '|i3CR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR4| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR4| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3CR46| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|i3CR41| N1 N2 N3 N4 N5 N7 N8)))
     (T (AEVAL (LIST '|i3CR4A| N2 N3 N4 N5 N7 N8))))) 
(PUT '|i3CR46| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR46|) 'OPFN) 
(PUT '|i3CR46| 'DEFINED-ON-LINE '1462) 
(PUT '|i3CR46| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR46| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR46| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3C| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N6)
                                        (LIST 'TIMES 2 N4))
                                  1)
                            (LIST '|i3C| N1 N2 N3 (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8))
                      (LIST 'TIMES N1
                            (LIST '|i3C| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N4 1) N5 (LIST 'DIFFERENCE N6 1)
                                  N7 N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3CR41| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3CR41|) 'OPFN) 
(PUT '|i3CR41| 'DEFINED-ON-LINE '1469) 
(PUT '|i3CR41| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR41| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR41| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 N3
                                  (LIST 'PLUS N4 1) N5 1 N7 N8)
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2 N3
                                  (LIST 'PLUS N4 1) N5 1 N7 N8)))
                (LIST 'DIFFERENCE
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 N3 N4 N5 2 N7 N8)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'DIFFERENCE N3 1) N4 N5 2 N7 N8)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3CR4A| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3CR4A|) 'OPFN) 
(PUT '|i3CR4A| 'DEFINED-ON-LINE '1475) 
(PUT '|i3CR4A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR4A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR4A| (N2 N3 N4 N5 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| 1 N2 N3 (LIST 'PLUS N4 1) N5 0 N7 N8)
                            (LIST '|i3C| 1 N2 N3 (LIST 'PLUS N4 1) N5 1 N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST '|i3C| 2 N2 (LIST 'DIFFERENCE N3 1) N4 N5 1 N7 N8))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4) 3))) 
(PUT '|i3CR6| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR6|) 'OPFN) 
(PUT '|i3CR6| 'DEFINED-ON-LINE '1481) 
(PUT '|i3CR6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR6| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR6| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3CR64| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|i3CR61| N1 N2 N3 N5 N6 N7 N8)))
     (T (AEVAL (LIST '|i3CR6A| N2 N3 N5 N6 N7 N8))))) 
(PUT '|i3CR64| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR64|) 'OPFN) 
(PUT '|i3CR64| 'DEFINED-ON-LINE '1487) 
(PUT '|i3CR64| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR64| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR64| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3C| N1 N2 N3 N4 N5 (LIST 'PLUS N6 1) N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N4)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|i3C| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5
                                  (LIST 'PLUS N6 1) N7 N8))
                      (LIST 'TIMES N1
                            (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5 (LIST 'PLUS N6 1)
                                  N7 N8)))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3CR61| 'NUMBER-OF-ARGS 7) 
(FLAG '(|i3CR61|) 'OPFN) 
(PUT '|i3CR61| 'DEFINED-ON-LINE '1494) 
(PUT '|i3CR61| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR61| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR61| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 N3 2 N5 N6 N7 N8)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2 N3 2 N5 N6 N7
                            N8))
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 N3 1 N5
                                  (LIST 'PLUS N6 1) N7 N8)
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) 1 N5
                                  (LIST 'PLUS N6 1) N7 N8))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3CR6A| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3CR6A|) 'OPFN) 
(PUT '|i3CR6A| 'DEFINED-ON-LINE '1500) 
(PUT '|i3CR6A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR6A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR6A| (N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| 1 N2 N3 0 N5 (LIST 'PLUS N6 1) N7 N8)
                            (LIST '|i3C| 1 N2 N3 1 N5 (LIST 'PLUS N6 1) N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST '|i3C| 2 N2 N3 1 N5 N6 N7 N8))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6) 3))) 
(PUT '|i3CR31| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR31|) 'OPFN) 
(PUT '|i3CR31| 'DEFINED-ON-LINE '1506) 
(PUT '|i3CR31| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR31| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR31| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)
                            (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) (LIST 'PLUS N4 1) N5 N6 N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1)
                                        N4)
                                  (LIST 'TIMES 2 N6))
                            1)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1)
                            N4 N5 N6 N7 N8)))
          (LIST 'DIFFERENCE N1 1))) 
(PUT '|i3CR36| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3CR36|) 'OPFN) 
(PUT '|i3CR36| 'DEFINED-ON-LINE '1512) 
(PUT '|i3CR36| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR36| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR36| (N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS (LIST '|i3C| 0 1 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3C| 0 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)
                                  (LIST '|i3C| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)))
                      (LIST 'TIMES 2
                            (LIST '|i3C| 2 1 (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3CR38| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3CR38|) 'OPFN) 
(PUT '|i3CR38| 'DEFINED-ON-LINE '1519) 
(PUT '|i3CR38| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR38| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR38| (N3 N4 N5 N8)
    (LIST 'PLUS (LIST '|i3C| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3C| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5 1 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3C| 0 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5 1 1
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N5
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3C| 1 1 (LIST 'PLUS N3 1) N4
                                              (LIST 'PLUS N5 1) 1 1
                                              (LIST 'DIFFERENCE N8 1))
                                        (LIST '|i3C| 1 0 (LIST 'PLUS N3 1) N4
                                              (LIST 'PLUS N5 1) 1 1
                                              (LIST 'DIFFERENCE N8 1))))
                            (LIST 'TIMES 2
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 3 'D) N3)
                                              (LIST 'TIMES 2
                                                    (LIST 'PLUS N4 N5 N8)))
                                        5)
                                  (LIST '|i3C| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|i3CR3| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i3CR3|) 'OPFN) 
(PUT '|i3CR3| 'DEFINED-ON-LINE '1527) 
(PUT '|i3CR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR3| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR3| (N3 N4 N5)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS (LIST '|i3C| 2 1 N3 N4 N5 1 1 1)
                      (LIST '|i3C| 1 2 N3 N4 N5 1 1 1)
                      (LIST 'TIMES N3
                            (LIST '|i3C| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1 1)))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N3)
                            (LIST 'TIMES 2 (LIST 'PLUS N4 N5)))
                      8)))) 
(PUT '|i3CR86| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3CR86|) 'OPFN) 
(PUT '|i3CR86| 'DEFINED-ON-LINE '1532) 
(PUT '|i3CR86| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR86| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3CR86| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|i3C| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N6)
                                        (LIST 'TIMES 2 N4))
                                  1)
                            (LIST '|i3C| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N6 1)
                                  N7 (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N1
                            (LIST '|i3C| (LIST 'PLUS N1 1) N2 N3 N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7
                                  (LIST 'PLUS N8 1))))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3CR84| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3CR84|) 'OPFN) 
(PUT '|i3CR84| 'DEFINED-ON-LINE '1539) 
(PUT '|i3CR84| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR84| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR84| (N1 N2 N3 N4 N5 N8)
    (LIST 'DIFFERENCE (LIST '|i3C| N1 N2 N3 N4 N5 0 1 (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1)
                                        N4)
                                  1)
                            (LIST '|i3C| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 1
                                  1 (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N1
                            (LIST '|i3C| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1)
                                  (LIST 'DIFFERENCE N4 1) N5 1 1
                                  (LIST 'PLUS N8 1))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3CR83| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3CR83|) 'OPFN) 
(PUT '|i3CR83| 'DEFINED-ON-LINE '1546) 
(PUT '|i3CR83| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR83| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR83| (N1 N2 N3 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3C| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 2 1 N8)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'DIFFERENCE N3 1) 1 1 2 1 N8))
                (LIST 'DIFFERENCE
                      (LIST '|i3C| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 1 2 N8)
                      (LIST '|i3C| N1 (LIST 'DIFFERENCE N2 1)
                            (LIST 'DIFFERENCE N3 1) 1 1 1 2 N8))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|i3C| N1 N2 (LIST 'DIFFERENCE N3 2) 1 1 1 1
                                  (LIST 'PLUS N8 1))
                            (LIST '|i3C| N1 N2 (LIST 'DIFFERENCE N3 1) 1 1 1 1
                                  (LIST 'PLUS N8 1)))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i3CR81| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i3CR81|) 'OPFN) 
(PUT '|i3CR81| 'DEFINED-ON-LINE '1553) 
(PUT '|i3CR81| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR81| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3CR81| (N1 N2 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 1 2 1 1 1 N8)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2 1 2 1 1 1 N8))
                (LIST 'DIFFERENCE
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 2) N2 1 1 1 2 1 N8)
                      (LIST '|i3C| (LIST 'DIFFERENCE N1 1) N2 0 1 1 2 1 N8)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3CR8| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i3CR8|) 'OPFN) 
(PUT '|i3CR8| 'DEFINED-ON-LINE '1559) 
(PUT '|i3CR8| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3CR8| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i3CR8| (N8)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS (LIST '|i3C| 2 1 1 1 1 1 1 N8)
                      (LIST '|i3C| 1 2 1 1 1 1 1 N8)
                      (LIST '|i3C| 1 1 2 1 1 1 1 N8))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) (LIST 'TIMES 2 N8))
                      11)))) 
(PUT '|ii3C| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3C|) 'OPFN) 
(PUT '|ii3C| 'DEFINED-ON-LINE '1571) 
(PUT '|ii3C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3C| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3C| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N1 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N6)) 0))
      0)
     ((EVALEQUAL (AEVAL N0) 0) (AEVAL (LIST '|i3C| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALNEQ (AEVAL N4) 1)
      (AEVAL (LIST '|ii3CL05| N2 N1 N3 N5 N4 N7 N6 N8 N0)))
     ((EVALNEQ (AEVAL N5) 1)
      (AEVAL (LIST '|ii3CL05| N1 N2 N3 N4 N5 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|ii3CL07| N2 N1 N3 N7 N6 N8 N0)))
     ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|ii3CL07| N1 N2 N3 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|ii3CL01| N1 N2 N3 N8 N0)))
     ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|ii3CL01| N2 N1 N3 N8 N0)))
     (T (AEVAL (LIST '|ii3CL0| N3 N8 N0))))) 
(PUT '|ii3CL05| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3CL05|) 'OPFN) 
(PUT '|ii3CL05| 'DEFINED-ON-LINE '1587) 
(PUT '|ii3CL05| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3CL05| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3CL05| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES
                      (LIST 'PLUS
                            (LIST 'TIMES 2
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N5) N6)
                                        N7))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE N0 N2)
                                        N3)
                                  N8)
                            1)
                      (LIST '|ii3C| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7
                            N8 (LIST 'DIFFERENCE N0 1)))
                (LIST 'TIMES 2 (LIST 'DIFFERENCE N0 1)
                      (LIST '|ii3C| N1 N2 N3 (LIST 'DIFFERENCE N4 1)
                            (LIST 'DIFFERENCE N5 1) N6 N7 N8
                            (LIST 'DIFFERENCE N0 2)))
                (LIST 'TIMES N2
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1)
                            N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8
                            (LIST 'DIFFERENCE N0 1)))
                (LIST 'TIMES N3
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'PLUS N3 1) N4 (LIST 'DIFFERENCE N5 1) N6 N7
                            N8 (LIST 'DIFFERENCE N0 1)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|ii3C| N1 N2 N3 (LIST 'DIFFERENCE N4 1)
                                  (LIST 'DIFFERENCE N5 1) N6 N7
                                  (LIST 'PLUS N8 1) (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3C| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1)
                                  (LIST 'DIFFERENCE N6 1) N7 (LIST 'PLUS N8 1)
                                  (LIST 'DIFFERENCE N0 1)))))
          (LIST 'DIFFERENCE N5 1))) 
(PUT '|ii3CL07| 'NUMBER-OF-ARGS 7) 
(FLAG '(|ii3CL07|) 'OPFN) 
(PUT '|ii3CL07| 'DEFINED-ON-LINE '1596) 
(PUT '|ii3CL07| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3CL07| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3CL07| (N1 N2 N3 N6 N7 N8 N0)
    (LIST 'PLUS
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST '|ii3C| N1 N2 N3 1 1 (LIST 'DIFFERENCE N6 1) N7 N8
                            (LIST 'DIFFERENCE N0 1))
                      (LIST '|ii3C| N1 N2 N3 0 1 N6 N7 N8
                            (LIST 'DIFFERENCE N0 1)))
                (LIST '|ii3C| N1 N2 N3 1 0 N6 N7 N8 (LIST 'DIFFERENCE N0 1)))
          (LIST 'QUOTIENT
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N8
                                  (LIST 'DIFFERENCE
                                        (LIST '|ii3C| N1 N2 N3 1 1
                                              (LIST 'DIFFERENCE N6 1)
                                              (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))
                                        (LIST '|ii3C| N1 N2 N3 0 1 N6
                                              (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1)
                                              (LIST 'DIFFERENCE N0 1))))
                            (LIST 'TIMES N3
                                  (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) 1 1 N6
                                        (LIST 'DIFFERENCE N7 1) N8
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N3)
                                                    N7)
                                              N8)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|ii3C| N1 N2 N3 1 1 N6
                                  (LIST 'DIFFERENCE N7 1) N8
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|ii3CL01| 'NUMBER-OF-ARGS 5) 
(FLAG '(|ii3CL01|) 'OPFN) 
(PUT '|ii3CL01| 'DEFINED-ON-LINE '1605) 
(PUT '|ii3CL01| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3CL01| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3CL01| (N1 N2 N3 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST '|ii3C| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 1 1 1 1 N8
                                  (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) N2 N3 1 1 1 1
                                  N8 (LIST 'DIFFERENCE N0 1))))
                (LIST 'DIFFERENCE
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 2) N2 N3 2 1 1 1 N8
                            N0)
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) N2 N3 2 1 1 1 N8
                            N0))
                (LIST 'DIFFERENCE
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 2) N2 N3 1 1 2 1 N8
                            N0)
                      (LIST '|ii3C| (LIST 'DIFFERENCE N1 1) N2
                            (LIST 'DIFFERENCE N3 1) 1 1 2 1 N8 N0)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|ii3CL0| 'NUMBER-OF-ARGS 3) 
(FLAG '(|ii3CL0|) 'OPFN) 
(PUT '|ii3CL0| 'DEFINED-ON-LINE '1612) 
(PUT '|ii3CL0| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3CL0| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3CL0| (N3 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST '|ii3C| 1 1 N3 0 1 2 1 N8 N0)
                      (LIST '|ii3C| 2 1 N3 1 1 1 1 N8 N0))
                (LIST '|ii3C| 1 1 N3 1 1 2 1 (LIST 'DIFFERENCE N8 1) N0))
          (LIST 'PLUS 'D (LIST 'DIFFERENCE N0 4)))) 
(PUT '|i3D| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3D|) 'OPFN) 
(PUT '|i3D| 'DEFINED-ON-LINE '1655) 
(PUT '|i3D| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3D| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3D| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N1 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N1 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|j3A| N3 N2 N5 N6 N7 N4 N8)))
     ((EVALEQUAL (AEVAL N2) 0) (AEVAL (LIST '|j3A| N3 N1 N4 N7 N6 N5 N8)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3B| N1 N2 N4 N5 N8 N6 N7)))
     ((EVALEQUAL (AEVAL N4) 0) (AEVAL (LIST '|j3E| N1 N2 N3 N5 N6 N7 N8)))
     ((EVALEQUAL (AEVAL N5) 0) (AEVAL (LIST '|j3E| N2 N1 N3 N4 N7 N6 N8)))
     ((EVALEQUAL (AEVAL N6) 0) (AEVAL (LIST '|j3D| N1 N2 N3 N4 N5 N7 N8)))
     ((EVALEQUAL (AEVAL N7) 0) (AEVAL (LIST '|j3D| N2 N1 N3 N5 N4 N6 N8)))
     ((EVALEQUAL (AEVAL N8) 0) (AEVAL (LIST '|j3C| N1 N2 N3 N4 N5 N7 N6)))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|i3DR1| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|i3DR1| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALNEQ (AEVAL N1) 1)
        (AEVAL (LIST '|i3DR31| N1 N2 N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N2) 1)
        (AEVAL (LIST '|i3DR31| N2 N1 N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3DR36| N3 N4 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|i3DR36| N3 N5 N4 N7 N6 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|i3DR38| N3 N4 N5 N8)))
       (T (AEVAL (LIST '|i3DR3| N3 N4 N5)))))
     ((EVALGREATERP (AEVAL N1) 1)
      (AEVAL (LIST '|i3DL1| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALGREATERP (AEVAL N2) 1)
      (AEVAL (LIST '|i3DL1| N2 N1 N3 N5 N4 N7 N6 N8)))
     ((EVALGREATERP (AEVAL N3) 1) (AEVAL (LIST '|i3DL3| N3 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N4) 0) (AEVAL (LIST '|i3DR4| N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N5) 0) (AEVAL (LIST '|i3DR4| N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N6) 0) (AEVAL (LIST '|i3DR6| N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N7) 0) (AEVAL (LIST '|i3DR6| N5 N4 N7 N6 N8)))
     ((EVALLESSP (AEVAL N8) 0)
      (COND ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3DR84| N4 N5 N6 N7 N8)))
            ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|i3DR84| N5 N4 N7 N6 N8)))
            ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|i3DR86| N6 N7 N8)))
            ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|i3DR86| N7 N6 N8)))
            (T (AEVAL (LIST '|i3DR8| N8)))))
     ((EVALLESSP (AEVAL N7) (AEVAL N6)) (AEVAL (LIST '|i3DL| N5 N4 N7 N6 N8)))
     (T (AEVAL (LIST '|i3DL| N4 N5 N6 N7 N8))))) 
(REMEMBER (LIST '|i3D|)) 
(PUT '|i3DL| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3DL|) 'OPFN) 
(PUT '|i3DL| 'DEFINED-ON-LINE '1699) 
(PUT '|i3DL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DL| (N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 1 1 1 (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)
                            (LIST '|i3D| 1 1 1 (LIST 'PLUS N4 1) N5 N6 N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|i3D| 2 1 0 N4 N5 N6 N7 N8)
                            (LIST '|i3D| 2 0 1 N4 N5 N6 N7 N8))
                      (LIST '|i3D| 1 0 2 N4 N5 N6 N7 N8)))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4) (LIST 'TIMES 2 N6))
                1))) 
(PUT '|i3DR1| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3DR1|) 'OPFN) 
(PUT '|i3DR1| 'DEFINED-ON-LINE '1705) 
(PUT '|i3DR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3DR1| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N3)
                                        N5)
                                  (LIST 'TIMES 2 N7))
                            (LIST '|i3D| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7
                                  N8))
                      (LIST 'TIMES N2
                            (LIST '|i3D| (LIST 'PLUS N1 1) (LIST 'PLUS N2 1)
                                  (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8)))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| (LIST 'PLUS N1 1) N2 N3 N4
                                  (LIST 'PLUS N5 1) N6 N7
                                  (LIST 'DIFFERENCE N8 1))
                            (LIST '|i3D| (LIST 'PLUS N1 1) N2 N3 N4
                                  (LIST 'PLUS N5 1) N6 (LIST 'DIFFERENCE N7 1)
                                  N8))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1) N2)
                      N3)
                (LIST 'TIMES 2 (LIST 'PLUS N4 N5 N6 N7 N8))))) 
(PUT '|i3DR31| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3DR31|) 'OPFN) 
(PUT '|i3DR31| 'DEFINED-ON-LINE '1712) 
(PUT '|i3DR31| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR31| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3DR31| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3D| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4 N5 N6 N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N4)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5 N6 N7 N8))
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) (LIST 'PLUS N4 1) N5
                                        N6 N7 (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)))
                      (LIST 'TIMES (LIST 'PLUS N3 1)
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 2) N4
                                  N5 N6 N7 N8)))
                (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3DR36| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3DR36|) 'OPFN) 
(PUT '|i3DR36| 'DEFINED-ON-LINE '1720) 
(PUT '|i3DR36| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR36| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR36| (N3 N4 N5 N6 N7 N8)
    (LIST 'PLUS (LIST '|i3D| 1 0 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)
                                  (LIST '|i3D| 0 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)))
                      (LIST 'TIMES 2
                            (LIST '|i3D| 2 1 (LIST 'PLUS N3 1) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8))
                      (LIST 'TIMES 2 (LIST 'PLUS N3 1)
                            (LIST '|i3D| 1 1 (LIST 'PLUS N3 2) N4 N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|i3DR38| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3DR38|) 'OPFN) 
(PUT '|i3DR38| 'DEFINED-ON-LINE '1728) 
(PUT '|i3DR38| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR38| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR38| (N3 N4 N5 N8)
    (LIST 'PLUS (LIST '|i3D| 0 1 (LIST 'PLUS N3 1) N4 N5 1 1 N8)
          (LIST 'DIFFERENCE (LIST '|i3D| 1 0 (LIST 'PLUS N3 1) N4 N5 1 1 N8)
                (LIST '|i3D| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                              N3)
                                        (LIST 'TIMES 2 (LIST 'PLUS N4 N5 N8)))
                                  5)
                            (LIST '|i3D| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N4
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| 0 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5 1 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3D| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N4 1) N5 1 1
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| 1 0 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1) 1 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|i3D| 1 1 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1) 1 1
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|i3DR3| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i3DR3|) 'OPFN) 
(PUT '|i3DR3| 'DEFINED-ON-LINE '1736) 
(PUT '|i3DR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR3| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR3| (N3 N4 N5)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS (LIST '|i3D| 2 1 N3 N4 N5 1 1 1)
                      (LIST '|i3D| 1 2 N3 N4 N5 1 1 1)
                      (LIST 'TIMES N3
                            (LIST '|i3D| 1 1 (LIST 'PLUS N3 1) N4 N5 1 1 1)))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N3)
                            (LIST 'TIMES 2 (LIST 'PLUS N4 N5)))
                      8)))) 
(PUT '|i3DL1| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3DL1|) 'OPFN) 
(PUT '|i3DL1| 'DEFINED-ON-LINE '1741) 
(PUT '|i3DL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DL1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3DL1| (N1 N2 N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'MINUS
                      (LIST 'TIMES 2
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        (LIST 'TIMES 2
                                              (LIST 'PLUS N4 N5 N6 N7 N8)))
                                  1)
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6
                                  N7 N8)))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2 N3 N4
                                  (LIST 'PLUS N5 1) N6 N7 N8)
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 N4
                                  (LIST 'PLUS N5 1) N6 N7 N8)))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 2) N2 N3 N4 N5 N6
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|i3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|i3DL3| 'NUMBER-OF-ARGS 6) 
(FLAG '(|i3DL3|) 'OPFN) 
(PUT '|i3DL3| 'DEFINED-ON-LINE '1748) 
(PUT '|i3DL3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DL3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DL3| (N3 N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5
                                  (LIST 'PLUS N6 1) N7 N8)
                            (LIST '|i3D| 1 0 (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) N7 N8)))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|i3D| 0 1 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'TIMES N8
                      (LIST 'PLUS
                            (LIST '|i3D| 1 1 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  N7 (LIST 'PLUS N8 1))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3D| 1 1
                                              (LIST 'DIFFERENCE N3 2) N4 N5 N6
                                              N7 (LIST 'PLUS N8 1))
                                        (LIST '|i3D| 0 1
                                              (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                              N7 (LIST 'PLUS N8 1)))
                                  (LIST '|i3D| 1 0 (LIST 'DIFFERENCE N3 1) N4
                                        N5 N6 N7 (LIST 'PLUS N8 1))))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|i3DR4| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3DR4|) 'OPFN) 
(PUT '|i3DR4| 'DEFINED-ON-LINE '1756) 
(PUT '|i3DR4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR4| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR4| (N4 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N4
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 1 1 1 (LIST 'PLUS N4 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)
                            (LIST '|i3D| 1 1 1 (LIST 'PLUS N4 1) N5 N6 N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|i3D| 2 1 0 N4 N5 N6 N7 N8)
                            (LIST '|i3D| 2 0 1 N4 N5 N6 N7 N8))
                      (LIST '|i3D| 1 0 2 N4 N5 N6 N7 N8)))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4) (LIST 'TIMES 2 N6))
                1))) 
(PUT '|i3DR6| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3DR6|) 'OPFN) 
(PUT '|i3DR6| 'DEFINED-ON-LINE '1762) 
(PUT '|i3DR6| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR6| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR6| (N4 N5 N6 N7 N8)
    (COND ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|i3DR64| N4 N5 N6 N7 N8)))
          (T (AEVAL (LIST '|i3DR6A| N5 N6 N7 N8))))) 
(PUT '|i3DR64| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3DR64|) 'OPFN) 
(PUT '|i3DR64| 'DEFINED-ON-LINE '1767) 
(PUT '|i3DR64| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR64| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR64| (N4 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|i3D| 1 1 1 N4 N5 (LIST 'PLUS N6 1) N7
                (LIST 'DIFFERENCE N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4)
                                        (LIST 'TIMES 2 N6))
                                  2)
                            (LIST '|i3D| 1 1 1 (LIST 'DIFFERENCE N4 1) N5
                                  (LIST 'PLUS N6 1) N7 N8))
                      (LIST '|i3D| 1 0 2 (LIST 'DIFFERENCE N4 1) N5
                            (LIST 'PLUS N6 1) N7 N8)
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 2 0 1 (LIST 'DIFFERENCE N4 1) N5
                                  (LIST 'PLUS N6 1) N7 N8)
                            (LIST '|i3D| 2 1 0 (LIST 'DIFFERENCE N4 1) N5
                                  (LIST 'PLUS N6 1) N7 N8)))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3DR6A| 'NUMBER-OF-ARGS 4) 
(FLAG '(|i3DR6A|) 'OPFN) 
(PUT '|i3DR6A| 'DEFINED-ON-LINE '1775) 
(PUT '|i3DR6A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR6A| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR6A| (N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'PLUS
                            (LIST 'TIMES 2
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3D| 1 1 1 0 N5
                                              (LIST 'PLUS N6 1) N7 N8)
                                        (LIST '|i3D| 1 1 1 1 N5
                                              (LIST 'PLUS N6 1) N7
                                              (LIST 'DIFFERENCE N8 1))))
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| 1 0 1 1 N5 (LIST 'PLUS N6 1) N7
                                        N8)
                                  (LIST '|i3D| 1 1 0 1 N5 (LIST 'PLUS N6 1) N7
                                        N8))))
                (LIST 'TIMES 2 (LIST '|i3D| 0 1 2 1 N5 N6 N7 N8))
                (LIST 'DIFFERENCE (LIST '|i3D| 1 1 1 2 N5 N6 N7 N8)
                      (LIST '|i3D| 0 1 1 2 N5 N6 N7 N8)))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6) 3)))) 
(PUT '|i3DR84| 'NUMBER-OF-ARGS 5) 
(FLAG '(|i3DR84|) 'OPFN) 
(PUT '|i3DR84| 'DEFINED-ON-LINE '1783) 
(PUT '|i3DR84| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR84| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR84| (N4 N5 N6 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|i3D| 1 1 1 N4 N5 (LIST 'DIFFERENCE N6 1) N7
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N4)
                                  (LIST 'TIMES 2 N6))
                            (LIST '|i3D| 1 1 1 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                                  (LIST 'PLUS N8 1)))
                      (LIST '|i3D| 1 0 2 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                            (LIST 'PLUS N8 1))
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 2 0 1 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                                  (LIST 'PLUS N8 1))
                            (LIST '|i3D| 2 1 0 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                                  (LIST 'PLUS N8 1))))
                (LIST 'DIFFERENCE N4 1)))) 
(PUT '|i3DR86| 'NUMBER-OF-ARGS 3) 
(FLAG '(|i3DR86|) 'OPFN) 
(PUT '|i3DR86| 'DEFINED-ON-LINE '1791) 
(PUT '|i3DR86| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR86| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |i3DR86| (N6 N7 N8)
    (LIST 'PLUS (LIST '|i3D| 1 1 1 0 1 N6 N7 (LIST 'PLUS N8 1))
          (LIST 'DIFFERENCE
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST '|i3D| 1 0 1 1 1 N6 N7 (LIST 'PLUS N8 1))
                            (LIST '|i3D| 1 1 0 1 1 N6 N7 (LIST 'PLUS N8 1)))
                      2)
                (LIST 'QUOTIENT
                      (LIST 'PLUS
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES 2
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N6) 2)
                                        (LIST '|i3D| 1 1 1 1 1
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1)))
                                  (LIST 'TIMES 2
                                        (LIST '|i3D| 0 1 2 1 1
                                              (LIST 'DIFFERENCE N6 1) N7
                                              (LIST 'PLUS N8 1))))
                            (LIST 'DIFFERENCE
                                  (LIST '|i3D| 0 1 1 2 1
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1))
                                  (LIST '|i3D| 1 1 1 2 1
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1))))
                      (LIST 'TIMES 2 (LIST 'DIFFERENCE N6 1)))))) 
(PUT '|i3DR8| 'NUMBER-OF-ARGS 1) 
(FLAG '(|i3DR8|) 'OPFN) 
(PUT '|i3DR8| 'DEFINED-ON-LINE '1800) 
(PUT '|i3DR8| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3DR8| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |i3DR8| (N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS (LIST '|i3D| 1 1 1 2 1 1 1 N8)
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|i3D| 1 1 1 1 2 1 1 N8)
                            (LIST '|i3D| 1 0 1 1 2 1 1 N8))
                      (LIST '|i3D| 0 1 1 2 1 1 1 N8))
                (LIST 'TIMES N8
                      (LIST 'PLUS (LIST '|i3D| 1 1 1 1 1 1 1 (LIST 'PLUS N8 1))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|i3D| 1 1 0 1 1 1 1
                                              (LIST 'PLUS N8 1))
                                        (LIST '|i3D| 1 0 1 1 1 1 1
                                              (LIST 'PLUS N8 1)))
                                  (LIST '|i3D| 0 1 1 1 1 1 1
                                        (LIST 'PLUS N8 1))))))
          (LIST 'TIMES 2
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) (LIST 'TIMES 2 N8))
                      11)))) 
(PUT '|j3D| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3D|) 'OPFN) 
(PUT '|j3D| 'DEFINED-ON-LINE '1822) 
(PUT '|j3D| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3D| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3D| (N1 N2 N3 N4 N5 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N4) (LIST '|g1| N7 N8)
             (LIST '|i1B| N2 (LIST 'PLUS N5 N7 N8)) (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5) (LIST '|i1| N3 N7)
             (LIST '|i1A| (LIST 'PLUS N1 N3 (LIST 'TIMES 2 N7)) N4)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N5 N8) (LIST '|i1B| N3 (LIST 'PLUS N7 N5 N8))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N3 (LIST 'TIMES 2 (LIST 'PLUS N7 N5 N8))) N4)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N4)
             (LIST '|i2B| (LIST 'PLUS N3 (LIST 'TIMES 2 N4)) N2 N7 N5 N8))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N8)
             (LIST '|j2B| (LIST 'PLUS N2 (LIST 'TIMES 2 N8)) N1 N3 N7 N4))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N8)
             (LIST '|j2C| N1 N2 (LIST 'PLUS N3 (LIST 'TIMES 2 N8)) N4 N5))))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|j3DR32| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3DR37| N1 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3DR38| N1 N3 N4 N5 N8)))
       (T (AEVAL (LIST '|j3DR3| N1 N2 N3 N4 N5 N7)))))
     ((EVALLESSP (AEVAL N2) 0)
      (COND
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|j3DR23| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|j3DR25| N1 N2 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3DR28| N1 N2 N4 N7 N8)))
       (T (AEVAL (LIST '|j3DR3| N1 N2 N3 N4 N5 N7)))))
     ((EVALLESSP (AEVAL N8) 0)
      (COND
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3DR87| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|j3DR85| N1 N2 N3 N4 N5 N8)))
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|j3DR83| N1 N2 N3 N4 N7 N8)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|j3DR82| N1 N2 N3 N4 N5 N8)))
       (T (AEVAL (LIST '|j3DR8| N1 N4 N8)))))
     ((EVALLESSP (AEVAL N1) 0)
      (COND
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|j3DR13| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|j3DR14| N1 N2 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|j3DR12| N1 N2 N5 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3DR17| N1 N5 N7 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3DR18| N1 N5 N8)))
       (T (AEVAL (LIST '|j3DR1| N1 N5)))))
     ((EVALLESSP (AEVAL N5) 0)
      (COND
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3DR58| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3DR57| N1 N2 N3 N4 N5 N7)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|j3DR82| N1 N2 N3 N4 N5 N8)))
       (T (AEVAL (LIST '|j3DR5| N1 N3 N4 N5)))))
     ((EVALLESSP (AEVAL N7) 0)
      (COND
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3DR78| N1 N2 N3 N4 N5 N7 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|j3DR75| N1 N2 N3 N4 N5 N7)))
       ((EVALNEQ (AEVAL N3) 1) (AEVAL (LIST '|j3DR83| N1 N2 N3 N4 N7 N8)))
       (T (AEVAL (LIST '|j3DR7| N1 N2 N4 N7)))))
     (T (AEVAL (LIST '|j3DL| N1 N2 N3 N4 N5 N7 N8))))) 
(REMEMBER (LIST '|j3D|)) 
(PUT '|j3DL| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DL|) 'OPFN) 
(PUT '|j3DL| 'DEFINED-ON-LINE '1870) 
(PUT '|j3DL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DL| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'MINUS
                      (LIST 'TIMES N3
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5 N7 N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N7 1)
                                  (LIST 'PLUS N8 1))
                            (LIST '|j3D| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N7
                                  (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3) N8)
                (LIST 'TIMES 2 N7)))) 
(PUT '|j3DR32| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR32|) 'OPFN) 
(PUT '|j3DR32| 'DEFINED-ON-LINE '1876) 
(PUT '|j3DR32| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR32| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR32| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'PLUS
          (LIST 'DIFFERENCE
                (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5
                      N7 N8)
                (LIST '|j3D| N1 N2 (LIST 'PLUS N3 1) N4 N5 N7 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'MINUS
                            (LIST 'TIMES
                                  (LIST 'PLUS
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N2)
                                                    N5)
                                              (LIST 'TIMES 2 N8))
                                        1)
                                  (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) N4 N5 N7 N8)))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) N4 (LIST 'PLUS N5 1)
                                        N7 (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) N4 (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8))))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|j3DR37| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR37|) 'OPFN) 
(PUT '|j3DR37| 'DEFINED-ON-LINE '1883) 
(PUT '|j3DR37| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR37| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR37| (N1 N3 N4 N5 N7 N8)
    (LIST 'PLUS
          (LIST '|j3D| (LIST 'DIFFERENCE N1 1) 1 (LIST 'PLUS N3 1) N4 N5 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'DIFFERENCE
                            (LIST 'MINUS
                                  (LIST 'TIMES 2
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          N1)
                                                    N3)
                                              (LIST 'TIMES 2
                                                    (LIST 'PLUS N4 N5 N7 N8)))
                                        (LIST '|j3D| N1 1 (LIST 'PLUS N3 1) N4
                                              N5 (LIST 'DIFFERENCE N7 1) N8)))
                            (LIST 'TIMES 2 N1
                                  (LIST '|j3D| (LIST 'PLUS N1 1) 1
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N7 1) N8)))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| N1 1 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8)
                                  (LIST '|j3D| N1 0 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3DR38| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3DR38|) 'OPFN) 
(PUT '|j3DR38| 'DEFINED-ON-LINE '1891) 
(PUT '|j3DR38| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR38| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR38| (N1 N3 N4 N5 N8)
    (LIST 'PLUS
          (LIST '|j3D| (LIST 'DIFFERENCE N1 1) 1 (LIST 'PLUS N3 1) N4 N5 1 N8)
          (LIST 'DIFFERENCE (LIST '|j3D| N1 0 (LIST 'PLUS N3 1) N4 N5 1 N8)
                (LIST '|j3D| N1 1 (LIST 'PLUS N3 1) N4 N5 1 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'MINUS
                            (LIST 'TIMES 2
                                  (LIST '|j3D| N1 2 (LIST 'PLUS N3 1) N4 N5 1
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| N1 0 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3D| N1 1 (LIST 'PLUS N3 1) N4
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3DR3| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR3|) 'OPFN) 
(PUT '|j3DR3| 'DEFINED-ON-LINE '1898) 
(PUT '|j3DR3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR3| (N1 N2 N3 N4 N5 N7)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N2
                            (LIST '|j3D| N1 (LIST 'PLUS N2 1) N3 N4 N5 N7 1))
                      (LIST 'TIMES N3
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5 N7 1)))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES 2
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N5) N7)
                                        1))
                            N2)
                      N3)))) 
(PUT '|j3DR23| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR23|) 'OPFN) 
(PUT '|j3DR23| 'DEFINED-ON-LINE '1903) 
(PUT '|j3DR23| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR23| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR23| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'PLUS
          (LIST 'DIFFERENCE (LIST '|j3D| N1 (LIST 'PLUS N2 1) N3 N4 N5 N7 N8)
                (LIST '|j3D| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4 N5
                      N7 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'MINUS
                            (LIST 'TIMES
                                  (LIST 'PLUS
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N3)
                                                    N7)
                                              (LIST 'TIMES 2 N8))
                                        1)
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1)
                                        (LIST 'DIFFERENCE N3 1) N4 N5 N7 N8)))
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1)
                                        (LIST 'DIFFERENCE N3 1) N4 N5
                                        (LIST 'PLUS N7 1)
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1)
                                        (LIST 'DIFFERENCE N3 1) N4
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'PLUS N7 1) N8))))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|j3DR25| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR25|) 'OPFN) 
(PUT '|j3DR25| 'DEFINED-ON-LINE '1910) 
(PUT '|j3DR25| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR25| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR25| (N1 N2 N4 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3D| N1 (LIST 'PLUS N2 1) 1 N4 N5 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'DIFFERENCE
                            (LIST 'MINUS
                                  (LIST 'TIMES 2
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          N1)
                                                    N2)
                                              (LIST 'TIMES 2
                                                    (LIST 'PLUS N4 N5 N7 N8)))
                                        (LIST '|j3D| N1 (LIST 'PLUS N2 1) 1 N4
                                              (LIST 'DIFFERENCE N5 1) N7 N8)))
                            (LIST 'TIMES 2 N1
                                  (LIST '|j3D| (LIST 'PLUS N1 1)
                                        (LIST 'PLUS N2 1) 1 N4
                                        (LIST 'DIFFERENCE N5 1) N7 N8)))
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'PLUS N2 1) 1 N4
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'PLUS N7 1) N8)
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1) 0 N4
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'PLUS N7 1) N8))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|j3DR28| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3DR28|) 'OPFN) 
(PUT '|j3DR28| 'DEFINED-ON-LINE '1918) 
(PUT '|j3DR28| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR28| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR28| (N1 N2 N4 N7 N8)
    (LIST 'PLUS (LIST '|j3D| N1 (LIST 'PLUS N2 1) 1 N4 1 N7 N8)
          (LIST 'DIFFERENCE (LIST '|j3D| N1 (LIST 'PLUS N2 1) 0 N4 1 N7 N8)
                (LIST '|j3D| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) 1 N4 1
                      N7 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'MINUS
                            (LIST 'TIMES 2
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1) 2 N4 1 N7
                                        (LIST 'DIFFERENCE N8 1))))
                      (LIST 'TIMES N7
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1) 0 N4 1
                                        (LIST 'PLUS N7 1)
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3D| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'PLUS N2 1) 1 N4 1
                                        (LIST 'PLUS N7 1)
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3DR87| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR87|) 'OPFN) 
(PUT '|j3DR87| 'DEFINED-ON-LINE '1925) 
(PUT '|j3DR87| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR87| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR87| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'PLUS
          (LIST '|j3D| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N7
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N7)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 N5 (LIST 'DIFFERENCE N7 1)
                                  (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N3
                            (LIST 'PLUS
                                  (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 N5
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1))
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                              (LIST 'PLUS N3 1) N4 N5
                                              (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1))
                                        (LIST '|j3D| N1 N2 (LIST 'PLUS N3 1) N4
                                              N5 (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1))))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3DR85| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR85|) 'OPFN) 
(PUT '|j3DR85| 'DEFINED-ON-LINE '1933) 
(PUT '|j3DR85| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR85| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR85| (N1 N2 N3 N4 N5 N8)
    (LIST 'PLUS (LIST '|j3D| N1 N2 N3 N4 N5 0 (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N2) N5)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) 1
                                  (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N2
                            (LIST 'PLUS
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1)
                                        (LIST 'DIFFERENCE N3 1) N4
                                        (LIST 'DIFFERENCE N5 1) 1
                                        (LIST 'PLUS N8 1))
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 (LIST 'PLUS N2 1) N3 N4
                                              (LIST 'DIFFERENCE N5 1) 1
                                              (LIST 'PLUS N8 1))
                                        (LIST '|j3D| (LIST 'DIFFERENCE N1 1)
                                              (LIST 'PLUS N2 1) N3 N4
                                              (LIST 'DIFFERENCE N5 1) 1
                                              (LIST 'PLUS N8 1))))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|j3DR83| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR83|) 'OPFN) 
(PUT '|j3DR83| 'DEFINED-ON-LINE '1941) 
(PUT '|j3DR83| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR83| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR83| (N1 N2 N3 N4 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 N2 (LIST 'DIFFERENCE N3 2) N4 1
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 1
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'TIMES N8
                      (LIST 'PLUS
                            (LIST '|j3D| N1 N2 (LIST 'DIFFERENCE N3 2) N4 1 N7
                                  (LIST 'PLUS N8 1))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 N2
                                              (LIST 'DIFFERENCE N3 1) N4 1 N7
                                              (LIST 'PLUS N8 1))
                                        (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                              (LIST 'DIFFERENCE N3 1) N4 1 N7
                                              (LIST 'PLUS N8 1)))
                                  (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'DIFFERENCE N3 1) N4 1 N7
                                        (LIST 'PLUS N8 1))))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|j3DR82| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR82|) 'OPFN) 
(PUT '|j3DR82| 'DEFINED-ON-LINE '1948) 
(PUT '|j3DR82| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR82| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR82| (N1 N2 N3 N4 N5 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 2) N3 N4
                                  (LIST 'PLUS N5 1) 1 N8)
                            (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                                  (LIST 'PLUS N5 1) 1 N8)))
                (LIST 'TIMES N8
                      (LIST 'PLUS
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 N4 N5 1
                                  (LIST 'PLUS N8 1))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 2)
                                              N3 N4 N5 1 (LIST 'PLUS N8 1))
                                        (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                              (LIST 'DIFFERENCE N3 1) N4 N5 1
                                              (LIST 'PLUS N8 1)))
                                  (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                                        N5 1 (LIST 'PLUS N8 1))))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N2 1)))) 
(PUT '|j3DR8| 'NUMBER-OF-ARGS 3) 
(FLAG '(|j3DR8|) 'OPFN) 
(PUT '|j3DR8| 'DEFINED-ON-LINE '1955) 
(PUT '|j3DR8| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR8| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR8| (N1 N4 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS (LIST 'MINUS (LIST '|j3D| N1 2 1 N4 1 1 N8))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 1 1 N4 0 1 (LIST 'PLUS N8 1))
                            (LIST '|j3D| N1 1 1 N4 1 0 (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N8) 3))) 
(PUT '|j3DR13| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR13|) 'OPFN) 
(PUT '|j3DR13| 'DEFINED-ON-LINE '1960) 
(PUT '|j3DR13| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR13| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR13| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'TIMES 2
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE 'D
                                                                 N5)
                                                                N7)
                                                          N8))
                                              N2)
                                        N3)
                                  1)
                            (LIST '|j3D| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 N5 N7 N8))
                      (LIST 'TIMES N2
                            (LIST '|j3D| (LIST 'PLUS N1 1) (LIST 'PLUS N2 1)
                                  (LIST 'DIFFERENCE N3 1) N4 N5 N7 N8)))
                (LIST 'DIFFERENCE N3 1)))) 
(PUT '|j3DR14| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR14|) 'OPFN) 
(PUT '|j3DR14| 'DEFINED-ON-LINE '1966) 
(PUT '|j3DR14| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR14| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR14| (N1 N2 N4 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3D| (LIST 'PLUS N1 1) N2 1 N4 N5 N7 N8)
          (LIST 'TIMES 2
                (LIST 'QUOTIENT
                      (LIST 'PLUS
                            (LIST 'TIMES (LIST 'PLUS N1 1)
                                  (LIST '|j3D| (LIST 'PLUS N1 2) N2 1
                                        (LIST 'DIFFERENCE N4 1) N5 N7 N8))
                            (LIST '|j3D| (LIST 'PLUS N1 1) N2 2
                                  (LIST 'DIFFERENCE N4 1) N5 N7 N8))
                      (LIST 'DIFFERENCE N4 1))))) 
(PUT '|j3DR12| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3DR12|) 'OPFN) 
(PUT '|j3DR12| 'DEFINED-ON-LINE '1972) 
(PUT '|j3DR12| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR12| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR12| (N1 N2 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3D| (LIST 'PLUS N1 1) N2 1 1 N5 N7 N8)
          (LIST '|j3D| (LIST 'PLUS N1 1) N2 0 1 N5 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N2) N5)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|j3D| (LIST 'PLUS N1 1)
                                  (LIST 'DIFFERENCE N2 1) 1 1 N5 N7 N8))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) 1 1
                                        (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8)
                                  (LIST '|j3D| (LIST 'PLUS N1 1)
                                        (LIST 'DIFFERENCE N2 1) 1 1
                                        (LIST 'PLUS N5 1) N7
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|j3DR17| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3DR17|) 'OPFN) 
(PUT '|j3DR17| 'DEFINED-ON-LINE '1979) 
(PUT '|j3DR17| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR17| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR17| (N1 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3D| (LIST 'PLUS N1 1) 1 0 1 N5 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                              N1)
                                        (LIST 'TIMES 2 (LIST 'PLUS N5 N7 N8)))
                                  3)
                            (LIST '|j3D| (LIST 'PLUS N1 1) 1 1 1 N5
                                  (LIST 'DIFFERENCE N7 1) N8))
                      (LIST 'TIMES 2 (LIST 'PLUS N1 1)
                            (LIST '|j3D| (LIST 'PLUS N1 2) 1 1 1 N5
                                  (LIST 'DIFFERENCE N7 1) N8))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| (LIST 'PLUS N1 1) 0 1 1
                                        (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8)
                                  (LIST '|j3D| (LIST 'PLUS N1 1) 1 1 1
                                        (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) N8))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3DR18| 'NUMBER-OF-ARGS 3) 
(FLAG '(|j3DR18|) 'OPFN) 
(PUT '|j3DR18| 'DEFINED-ON-LINE '1987) 
(PUT '|j3DR18| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR18| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR18| (N1 N5 N8)
    (LIST 'PLUS (LIST '|j3D| (LIST 'PLUS N1 1) 1 1 1 N5 1 N8)
          (LIST 'DIFFERENCE (LIST '|j3D| (LIST 'PLUS N1 1) 1 0 1 N5 1 N8)
                (LIST '|j3D| (LIST 'PLUS N1 1) 0 1 1 N5 1 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES 2
                            (LIST '|j3D| (LIST 'PLUS N1 1) 2 1 1 N5 1
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3D| (LIST 'PLUS N1 1) 1 1 1
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3D| (LIST 'PLUS N1 1) 0 1 1
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3DR1| 'NUMBER-OF-ARGS 2) 
(FLAG '(|j3DR1|) 'OPFN) 
(PUT '|j3DR1| 'DEFINED-ON-LINE '1994) 
(PUT '|j3DR1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR1| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |j3DR1| (N1 N5)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES N1
                            (LIST '|j3D| (LIST 'PLUS N1 1) 1 1 1 N5 1 1))
                      (LIST '|j3D| N1 2 1 1 N5 1 1)
                      (LIST '|j3D| N1 1 2 1 N5 1 1))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1)
                            (LIST 'TIMES 2 N5))
                      8)))) 
(PUT '|j3DR58| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR58|) 'OPFN) 
(PUT '|j3DR58| 'DEFINED-ON-LINE '1999) 
(PUT '|j3DR58| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR58| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR58| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|j3D| N1 N2 N3 N4 (LIST 'PLUS N5 1) (LIST 'DIFFERENCE N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N8)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 (LIST 'PLUS N5 1) N7
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N3
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 (LIST 'PLUS N5 1) N7
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3DR57| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR57|) 'OPFN) 
(PUT '|j3DR57| 'DEFINED-ON-LINE '2006) 
(PUT '|j3DR57| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR57| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR57| (N1 N2 N3 N4 N5 N7)
    (LIST 'DIFFERENCE (LIST '|j3D| N1 N2 N3 N4 (LIST 'PLUS N5 1) N7 0)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3)
                                        N7)
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 (LIST 'PLUS N5 1)
                                  (LIST 'DIFFERENCE N7 1) 1))
                      (LIST 'TIMES N3
                            (LIST 'PLUS
                                  (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1) N4 (LIST 'PLUS N5 1)
                                        (LIST 'DIFFERENCE N7 1) 1)
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1)
                                              (LIST 'PLUS N3 1) N4
                                              (LIST 'PLUS N5 1)
                                              (LIST 'DIFFERENCE N7 1) 1)
                                        (LIST '|j3D| N1 N2 (LIST 'PLUS N3 1) N4
                                              (LIST 'PLUS N5 1)
                                              (LIST 'DIFFERENCE N7 1) 1)))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3DR5| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3DR5|) 'OPFN) 
(PUT '|j3DR5| 'DEFINED-ON-LINE '2014) 
(PUT '|j3DR5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR5| (N1 N3 N4 N5)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) 2 N3 N4 N5 1
                                  1)
                            (LIST '|j3D| N1 2 (LIST 'DIFFERENCE N3 1) N4 N5 1
                                  1))
                      (LIST '|j3D| N1 2 N3 N4 N5 1 1))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 1 N3 N4 (LIST 'PLUS N5 1) 1 0)
                            (LIST '|j3D| N1 1 N3 N4 (LIST 'PLUS N5 1) 0 1))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N5) 3))) 
(PUT '|j3DR78| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3DR78|) 'OPFN) 
(PUT '|j3DR78| 'DEFINED-ON-LINE '2020) 
(PUT '|j3DR78| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR78| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3DR78| (N1 N2 N3 N4 N5 N7 N8)
    (LIST 'PLUS
          (LIST '|j3D| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) (LIST 'PLUS N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N8)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 N5 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N3
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N4 N5 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3DR75| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3DR75|) 'OPFN) 
(PUT '|j3DR75| 'DEFINED-ON-LINE '2027) 
(PUT '|j3DR75| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR75| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR75| (N1 N2 N3 N4 N5 N7)
    (LIST 'DIFFERENCE (LIST '|j3D| N1 N2 N3 N4 N5 (LIST 'PLUS N7 1) 0)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N2)
                                        N5)
                                  1)
                            (LIST '|j3D| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1)
                                  (LIST 'PLUS N7 1) 1))
                      (LIST 'TIMES N2
                            (LIST 'PLUS
                                  (LIST '|j3D| N1 (LIST 'PLUS N2 1) N3 N4
                                        (LIST 'DIFFERENCE N5 1)
                                        (LIST 'PLUS N7 1) 1)
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3D| N1 (LIST 'PLUS N2 1)
                                              (LIST 'DIFFERENCE N3 1) N4
                                              (LIST 'DIFFERENCE N5 1)
                                              (LIST 'PLUS N7 1) 1)
                                        (LIST '|j3D| (LIST 'DIFFERENCE N1 1)
                                              (LIST 'PLUS N2 1) N3 N4
                                              (LIST 'DIFFERENCE N5 1)
                                              (LIST 'PLUS N7 1) 1)))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|j3DR7| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3DR7|) 'OPFN) 
(PUT '|j3DR7| 'DEFINED-ON-LINE '2035) 
(PUT '|j3DR7| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3DR7| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3DR7| (N1 N2 N4 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|j3D| N1 N2 2 N4 1 N7 1)
                            (LIST '|j3D| (LIST 'DIFFERENCE N1 1) N2 2 N4 1 N7
                                  1))
                      (LIST '|j3D| N1 (LIST 'DIFFERENCE N2 1) 2 N4 1 N7 1))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3D| N1 N2 1 N4 1 (LIST 'PLUS N7 1) 0)
                            (LIST '|j3D| N1 N2 1 N4 0 (LIST 'PLUS N7 1) 1))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N7) 3))) 
(PUT '|j3E| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3E|) 'OPFN) 
(PUT '|j3E| 'DEFINED-ON-LINE '2054) 
(PUT '|j3E| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3E| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3E| (N1 N2 N3 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL (LIST 'MAX N1 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N7)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N7 N8) (LIST '|i1| N3 N6)
             (LIST '|i1D| (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N6))
                   (LIST 'PLUS N5 N7 N8))
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N7)
             (LIST '|i1A| (LIST 'PLUS N3 N1 (LIST 'TIMES 2 N7)) N6)
             (LIST '|i1C|
                   (LIST 'PLUS N2 N3 N1 (LIST 'TIMES 2 (LIST 'PLUS N6 N7))) N5)
             (LIST '|b| 3))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 (LIST 'TIMES 2 (LIST 'PLUS N6 N7))) N8)
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|j2| N1 N2 N3 N6 N7)))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST '|i1C|
                   (LIST 'PLUS N1 N3 (LIST 'TIMES 2 (LIST 'PLUS N5 N7 N8))) N6)
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|i2| N1 N3 N8 N5 N7)))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N6)
             (LIST '|i2B| (LIST 'PLUS N1 (LIST 'TIMES 2 N6)) N2 N8 N5 N7))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N8)
             (LIST '|j2B| (LIST 'PLUS N1 (LIST 'TIMES 2 N8)) N2 N3 N6 N5))))
     ((EVALLESSP (AEVAL N1) 0) (AEVAL (LIST '|j3ER1| N1 N2 N3 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0) (AEVAL (LIST '|j3ER2| N1 N2 N3 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|j3ER31| N1 N2 N3 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|j3ER36| N2 N3 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N2) 1) (AEVAL (LIST '|j3ER32| N2 N3 N5 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3ER37| N3 N5 N7 N8)))
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3ER38| N3 N5 N8)))
       (T (AEVAL (LIST '|j3ER3| N3 N5)))))
     ((EVALLESSP (AEVAL N8) 0)
      (COND
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3ER87| N1 N2 N3 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|j3ER85| N1 N2 N3 N5 N6 N8)))
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|j3ER81| N1 N2 N3 N5 N6 N7 N8)))
       (T (AEVAL (LIST '|j3ER8| N2 N3 N6 N8)))))
     ((EVALLESSP (AEVAL N5) 0)
      (COND
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3ER58| N1 N2 N3 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|j3ER57| N1 N2 N3 N5 N6 N7)))
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|j3ER81| N1 N2 N3 N5 N6 N7 N8)))
       (T (AEVAL (LIST '|j3ER5| N2 N3 N5 N6)))))
     ((EVALLESSP (AEVAL N7) 0)
      (COND
       ((EVALNEQ (AEVAL N8) 1) (AEVAL (LIST '|j3ER78| N1 N2 N3 N5 N6 N7 N8)))
       ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|j3ER75| N1 N2 N3 N5 N6 N7)))
       ((EVALNEQ (AEVAL N1) 1) (AEVAL (LIST '|j3ER81| N1 N2 N3 N5 N6 N7 N8)))
       (T (AEVAL (LIST '|j3ER7| N2 N3 N6 N7)))))
     (T (AEVAL (LIST '|j3EL| N1 N2 N3 N5 N6 N7 N8))))) 
(REMEMBER (LIST '|j3E|)) 
(PUT '|j3EL| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3EL|) 'OPFN) 
(PUT '|j3EL| 'DEFINED-ON-LINE '2093) 
(PUT '|j3EL| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3EL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3EL| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'MINUS
                      (LIST 'TIMES N1
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N5 N6 N7 N8)))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| N1 N2 N3 N5 N6 (LIST 'DIFFERENCE N7 1)
                                  (LIST 'PLUS N8 1))
                            (LIST '|j3E| N1 N2 N3 (LIST 'DIFFERENCE N5 1) N6 N7
                                  (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N1) N8)
                (LIST 'TIMES 2 N7)))) 
(PUT '|j3ER1| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER1|) 'OPFN) 
(PUT '|j3ER1| 'DEFINED-ON-LINE '2099) 
(PUT '|j3ER1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER1| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N3)
                                        N5)
                                  (LIST 'TIMES 2 N7))
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2 N3 N5 N6 N7 N8))
                      (LIST 'TIMES N2
                            (LIST '|j3E| (LIST 'PLUS N1 1) (LIST 'PLUS N2 1)
                                  (LIST 'DIFFERENCE N3 1) N5 N6 N7 N8)))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N5 1) N6 N7
                                  (LIST 'DIFFERENCE N8 1))
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N5 1) N6 (LIST 'DIFFERENCE N7 1)
                                  N8))))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1) N2)
                      N3)
                (LIST 'TIMES 2 (LIST 'PLUS N5 N6 N7 N8))))) 
(PUT '|j3ER2| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER2|) 'OPFN) 
(PUT '|j3ER2| 'DEFINED-ON-LINE '2106) 
(PUT '|j3ER2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER2| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'DIFFERENCE
                (LIST 'TIMES
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N2) N3)
                            (LIST 'TIMES 2 N6))
                      (LIST '|j3E| N1 (LIST 'PLUS N2 1) N3 N5 N6 N7 N8))
                (LIST 'TIMES N1
                      (LIST '|j3E| (LIST 'PLUS N1 1) (LIST 'PLUS N2 1)
                            (LIST 'DIFFERENCE N3 1) N5 N6 N7 N8)))
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N1) N2)
                      N3)
                (LIST 'TIMES 2 (LIST 'PLUS N5 N6 N7 N8))))) 
(PUT '|j3ER31| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER31|) 'OPFN) 
(PUT '|j3ER31| 'DEFINED-ON-LINE '2112) 
(PUT '|j3ER31| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER31| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER31| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|j3E| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N5 N6 N7
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N3)
                                        (LIST 'TIMES 2 N6))
                                  1)
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) N5 N6 N7 N8))
                      (LIST 'TIMES (LIST 'PLUS N3 1)
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 2) N5
                                  N6 N7 N8)))
                (LIST 'DIFFERENCE N1 1)))) 
(PUT '|j3ER36| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3ER36|) 'OPFN) 
(PUT '|j3ER36| 'DEFINED-ON-LINE '2119) 
(PUT '|j3ER36| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER36| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER36| (N2 N3 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|j3E| 1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N5 N6 N7 N8)
          (LIST 'TIMES 2
                (LIST 'QUOTIENT
                      (LIST 'PLUS
                            (LIST '|j3E| 2 N2 (LIST 'PLUS N3 1) N5
                                  (LIST 'DIFFERENCE N6 1) N7 N8)
                            (LIST 'TIMES (LIST 'PLUS N3 1)
                                  (LIST '|j3E| 1 N2 (LIST 'PLUS N3 2) N5
                                        (LIST 'DIFFERENCE N6 1) N7 N8)))
                      (LIST 'DIFFERENCE N6 1))))) 
(PUT '|j3ER32| 'NUMBER-OF-ARGS 5) 
(FLAG '(|j3ER32|) 'OPFN) 
(PUT '|j3ER32| 'DEFINED-ON-LINE '2126) 
(PUT '|j3ER32| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER32| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER32| (N2 N3 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3E| 0 N2 (LIST 'PLUS N3 1) N5 1 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N5)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3E| 1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'PLUS N3 1) N5 1 N7 N8))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3E| 1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) (LIST 'PLUS N5 1) 1
                                        N7 (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3E| 1 (LIST 'DIFFERENCE N2 1)
                                        (LIST 'PLUS N3 1) (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N7 1) N8)))
                      (LIST 'TIMES (LIST 'PLUS N3 1)
                            (LIST '|j3E| 0 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'PLUS N3 2) N5 1 N7 N8)))
                (LIST 'DIFFERENCE N2 1)))) 
(PUT '|j3ER37| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3ER37|) 'OPFN) 
(PUT '|j3ER37| 'DEFINED-ON-LINE '2134) 
(PUT '|j3ER37| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER37| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER37| (N3 N5 N7 N8)
    (LIST 'PLUS (LIST '|j3E| 0 1 (LIST 'PLUS N3 1) N5 1 N7 N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES 2
                            (LIST '|j3E| 1 2 (LIST 'PLUS N3 1) N5 1
                                  (LIST 'DIFFERENCE N7 1) N8))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3E| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N7 1) N8)
                                  (LIST '|j3E| 1 0 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N5 1) 1
                                        (LIST 'DIFFERENCE N7 1) N8)))
                      (LIST 'TIMES 2 (LIST 'PLUS N3 1)
                            (LIST '|j3E| 1 1 (LIST 'PLUS N3 2) N5 1
                                  (LIST 'DIFFERENCE N7 1) N8)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3ER38| 'NUMBER-OF-ARGS 3) 
(FLAG '(|j3ER38|) 'OPFN) 
(PUT '|j3ER38| 'DEFINED-ON-LINE '2142) 
(PUT '|j3ER38| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER38| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER38| (N3 N5 N8)
    (LIST 'PLUS (LIST '|j3E| 1 0 (LIST 'PLUS N3 1) N5 1 1 N8)
          (LIST 'DIFFERENCE (LIST '|j3E| 0 1 (LIST 'PLUS N3 1) N5 1 1 N8)
                (LIST '|j3E| 1 1 (LIST 'PLUS N3 1) N5 1 1 N8))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES 2
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                              N3)
                                        (LIST 'TIMES 2 (LIST 'PLUS N5 N8)))
                                  5)
                            (LIST '|j3E| 1 1 (LIST 'PLUS N3 1) N5 1 1
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N5
                            (LIST 'DIFFERENCE
                                  (LIST '|j3E| 1 0 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N5 1) 1 1
                                        (LIST 'DIFFERENCE N8 1))
                                  (LIST '|j3E| 1 1 (LIST 'PLUS N3 1)
                                        (LIST 'PLUS N5 1) 1 1
                                        (LIST 'DIFFERENCE N8 1)))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3ER3| 'NUMBER-OF-ARGS 2) 
(FLAG '(|j3ER3|) 'OPFN) 
(PUT '|j3ER3| 'DEFINED-ON-LINE '2150) 
(PUT '|j3ER3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER3| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |j3ER3| (N3 N5)
    (LIST 'MINUS
          (LIST 'QUOTIENT
                (LIST 'PLUS (LIST '|j3E| 2 1 N3 N5 1 1 1)
                      (LIST '|j3E| 1 2 N3 N5 1 1 1)
                      (LIST 'TIMES N3
                            (LIST '|j3E| 1 1 (LIST 'PLUS N3 1) N5 1 1 1)))
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N3)
                            (LIST 'TIMES 2 N5))
                      8)))) 
(PUT '|j3ER87| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER87|) 'OPFN) 
(PUT '|j3ER87| 'DEFINED-ON-LINE '2155) 
(PUT '|j3ER87| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER87| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER87| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|j3E| N1 N2 N3 (LIST 'DIFFERENCE N5 1) N6 N7
                (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N7)
                                        (LIST 'TIMES 2 N8))
                                  1)
                            (LIST '|j3E| N1 N2 N3 N5 N6 (LIST 'DIFFERENCE N7 1)
                                  (LIST 'PLUS N8 1)))
                      (LIST 'TIMES N1
                            (LIST 'PLUS
                                  (LIST '|j3E| (LIST 'PLUS N1 1) N2
                                        (LIST 'DIFFERENCE N3 1) N5 N6
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1))
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3E| (LIST 'PLUS N1 1) N2 N3 N5
                                              N6 (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1))
                                        (LIST '|j3E| (LIST 'PLUS N1 1)
                                              (LIST 'DIFFERENCE N2 1) N3 N5 N6
                                              (LIST 'DIFFERENCE N7 1)
                                              (LIST 'PLUS N8 1))))))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3ER85| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3ER85|) 'OPFN) 
(PUT '|j3ER85| 'DEFINED-ON-LINE '2163) 
(PUT '|j3ER85| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER85| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER85| (N1 N2 N3 N5 N6 N8)
    (LIST 'PLUS (LIST '|j3E| N1 N2 N3 N5 N6 0 (LIST 'PLUS N8 1))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'MINUS
                            (LIST 'TIMES
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N5)
                                        1)
                                  (LIST '|j3E| N1 N2 N3 (LIST 'DIFFERENCE N5 1)
                                        N6 1 (LIST 'PLUS N8 1))))
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES N2
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3E| N1 (LIST 'PLUS N2 1)
                                              (LIST 'DIFFERENCE N3 1)
                                              (LIST 'DIFFERENCE N5 1) N6 1
                                              (LIST 'PLUS N8 1))
                                        (LIST '|j3E| (LIST 'DIFFERENCE N1 1)
                                              (LIST 'PLUS N2 1) N3
                                              (LIST 'DIFFERENCE N5 1) N6 1
                                              (LIST 'PLUS N8 1))))
                            (LIST 'TIMES N3
                                  (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2
                                        (LIST 'PLUS N3 1)
                                        (LIST 'DIFFERENCE N5 1) N6 1
                                        (LIST 'PLUS N8 1)))))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|j3ER81| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER81|) 'OPFN) 
(PUT '|j3ER81| 'DEFINED-ON-LINE '2171) 
(PUT '|j3ER81| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER81| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER81| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 2) N2 N3 N5 N6
                                  (LIST 'PLUS N7 1) N8)
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N5 N6
                                  (LIST 'PLUS N7 1) N8)))
                (LIST 'TIMES N8
                      (LIST 'PLUS
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 2) N2 N3 N5 N6 N7
                                  (LIST 'PLUS N8 1))
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|j3E| (LIST 'DIFFERENCE N1 1)
                                              (LIST 'DIFFERENCE N2 1) N3 N5 N6
                                              N7 (LIST 'PLUS N8 1))
                                        (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2
                                              (LIST 'DIFFERENCE N3 1) N5 N6 N7
                                              (LIST 'PLUS N8 1)))
                                  (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2 N3 N5
                                        N6 N7 (LIST 'PLUS N8 1))))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|j3ER8| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3ER8|) 'OPFN) 
(PUT '|j3ER8| 'DEFINED-ON-LINE '2178) 
(PUT '|j3ER8| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER8| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER8| (N2 N3 N6 N8)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'MINUS
                      (LIST '|j3E| 2 N2 (LIST 'DIFFERENCE N3 1) 1 N6 1 N8))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| 1 N2 N3 1 N6 0 (LIST 'PLUS N8 1))
                            (LIST '|j3E| 1 N2 N3 0 N6 1 (LIST 'PLUS N8 1)))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N8) 3))) 
(PUT '|j3ER58| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER58|) 'OPFN) 
(PUT '|j3ER58| 'DEFINED-ON-LINE '2184) 
(PUT '|j3ER58| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER58| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER58| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'DIFFERENCE
          (LIST '|j3E| N1 N2 N3 (LIST 'PLUS N5 1) N6 (LIST 'DIFFERENCE N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N8)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3E| N1 N2 N3 (LIST 'PLUS N5 1) N6 N7
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N1
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N5 1) N6
                                  N7 (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3ER57| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3ER57|) 'OPFN) 
(PUT '|j3ER57| 'DEFINED-ON-LINE '2191) 
(PUT '|j3ER57| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER57| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER57| (N1 N2 N3 N5 N6 N7)
    (LIST 'DIFFERENCE (LIST '|j3E| N1 N2 N3 (LIST 'PLUS N5 1) N6 N7 0)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES 2
                                                          (LIST 'DIFFERENCE 'D
                                                                N6))
                                                    N1)
                                              N3)
                                        N7)
                                  1)
                            (LIST '|j3E| N1 N2 N3 (LIST 'PLUS N5 1) N6
                                  (LIST 'DIFFERENCE N7 1) 1))
                      (LIST 'TIMES N1
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2 N3
                                  (LIST 'PLUS N5 1) N6 (LIST 'DIFFERENCE N7 1)
                                  1))
                      (LIST 'TIMES N3
                            (LIST '|j3E| N1 (LIST 'DIFFERENCE N2 1)
                                  (LIST 'PLUS N3 1) (LIST 'PLUS N5 1) N6
                                  (LIST 'DIFFERENCE N7 1) 1)))
                (LIST 'DIFFERENCE N7 1)))) 
(PUT '|j3ER5| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3ER5|) 'OPFN) 
(PUT '|j3ER5| 'DEFINED-ON-LINE '2199) 
(PUT '|j3ER5| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER5| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER5| (N2 N3 N5 N6)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|j3E| 0 N2 N3 N5 N6 2 1)
                            (LIST '|j3E| 1 N2 (LIST 'DIFFERENCE N3 1) N5 N6 2
                                  1))
                      (LIST 'TIMES 2 (LIST '|j3E| 2 N2 N3 N5 N6 1 1)))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| 1 N2 N3 (LIST 'PLUS N5 1) N6 1 1)
                            (LIST '|j3E| 1 (LIST 'DIFFERENCE N2 1) N3
                                  (LIST 'PLUS N5 1) N6 1 1))))
          (LIST 'TIMES 2
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'TIMES 3 'D) N2) N3)
                            (LIST 'TIMES 2 (LIST 'PLUS N5 N6)))
                      5)))) 
(PUT '|j3ER78| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3ER78|) 'OPFN) 
(PUT '|j3ER78| 'DEFINED-ON-LINE '2205) 
(PUT '|j3ER78| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER78| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3ER78| (N1 N2 N3 N5 N6 N7 N8)
    (LIST 'PLUS
          (LIST '|j3E| N1 N2 N3 (LIST 'DIFFERENCE N5 1) N6 (LIST 'PLUS N7 1)
                N8)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N1) N8)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3E| N1 N2 N3 N5 N6 (LIST 'PLUS N7 1)
                                  (LIST 'DIFFERENCE N8 1)))
                      (LIST 'TIMES N1
                            (LIST '|j3E| (LIST 'PLUS N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N5 N6
                                  (LIST 'PLUS N7 1) (LIST 'DIFFERENCE N8 1))))
                (LIST 'DIFFERENCE N8 1)))) 
(PUT '|j3ER75| 'NUMBER-OF-ARGS 6) 
(FLAG '(|j3ER75|) 'OPFN) 
(PUT '|j3ER75| 'DEFINED-ON-LINE '2212) 
(PUT '|j3ER75| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER75| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER75| (N1 N2 N3 N5 N6 N7)
    (LIST 'PLUS (LIST '|j3E| N1 N2 N3 N5 N6 (LIST 'PLUS N7 1) 0)
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE 'D N3) N5)
                                        (LIST 'TIMES 2 N7))
                                  1)
                            (LIST '|j3E| N1 N2 N3 (LIST 'DIFFERENCE N5 1) N6
                                  (LIST 'PLUS N7 1) 1))
                      (LIST 'TIMES N2
                            (LIST 'DIFFERENCE
                                  (LIST '|j3E| (LIST 'DIFFERENCE N1 1)
                                        (LIST 'PLUS N2 1) N3
                                        (LIST 'DIFFERENCE N5 1) N6
                                        (LIST 'PLUS N7 1) 1)
                                  (LIST '|j3E| N1 (LIST 'PLUS N2 1)
                                        (LIST 'DIFFERENCE N3 1)
                                        (LIST 'DIFFERENCE N5 1) N6
                                        (LIST 'PLUS N7 1) 1)))
                      (LIST 'TIMES N3
                            (LIST '|j3E| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N5 1) N6
                                  (LIST 'PLUS N7 1) 1)))
                (LIST 'DIFFERENCE N5 1)))) 
(PUT '|j3ER7| 'NUMBER-OF-ARGS 4) 
(FLAG '(|j3ER7|) 'OPFN) 
(PUT '|j3ER7| 'DEFINED-ON-LINE '2220) 
(PUT '|j3ER7| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3ER7| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |j3ER7| (N2 N3 N6 N7)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| 2 (LIST 'DIFFERENCE N2 1) N3 1 N6 N7
                                  1)
                            (LIST '|j3E| 2 N2 (LIST 'DIFFERENCE N3 1) 1 N6 N7
                                  1))
                      (LIST '|j3E| 2 N2 N3 1 N6 N7 1))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|j3E| 1 N2 N3 1 N6 (LIST 'PLUS N7 1) 0)
                            (LIST '|j3E| 1 N2 N3 0 N6 (LIST 'PLUS N7 1) 1))))
          (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N7) 3))) 
(PUT '|ii3D| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3D|) 'OPFN) 
(PUT '|ii3D| 'DEFINED-ON-LINE '2239) 
(PUT '|ii3D| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3D| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3D| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (COND
     ((OR (EVALLEQ (AEVAL (LIST 'MAX N4 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N8 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N1 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N2 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N5 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N1 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N5 N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N3 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N7 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N0) 0) (AEVAL (LIST '|i3D| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLEQ (AEVAL N1) 0)
      (AEVAL (LIST '|ii3D1| (LIST 'MINUS N1) N2 N3 N4 N5 N6 N7 N8 N0)))
     ((EVALLEQ (AEVAL N2) 0)
      (AEVAL (LIST '|ii3D1| (LIST 'MINUS N2) N1 N3 N5 N4 N7 N6 N8 N0)))
     ((EVALLEQ (AEVAL N3) 0)
      (AEVAL (LIST '|ii3D3| N1 N2 (LIST 'MINUS N3) N4 N5 N6 N7 N8 N0)))
     ((EVALGREATERP (AEVAL N1) 1)
      (AEVAL (LIST '|ii3DL1| N1 N2 N3 N4 N5 N6 N7 N8 N0)))
     ((EVALGREATERP (AEVAL N2) 1)
      (AEVAL (LIST '|ii3DL1| N2 N1 N3 N5 N4 N7 N6 N8 N0)))
     ((EVALGREATERP (AEVAL N3) 1)
      (AEVAL (LIST '|ii3DL3| N3 N4 N5 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N4) 1) (AEVAL (LIST '|ii3DL04| N4 N5 N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N5) 1) (AEVAL (LIST '|ii3DL04| N5 N4 N7 N6 N8 N0)))
     ((EVALNEQ (AEVAL N6) 1) (AEVAL (LIST '|ii3DL06| N6 N7 N8 N0)))
     ((EVALNEQ (AEVAL N7) 1) (AEVAL (LIST '|ii3DL06| N7 N6 N8 N0)))
     (T (AEVAL (LIST '|ii3DL0| N8 N0))))) 
(REMEMBER (LIST '|ii3D|)) 
(PUT '|ii3D1| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3D1|) 'OPFN) 
(PUT '|ii3D1| 'DEFINED-ON-LINE '2263) 
(PUT '|ii3D1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3D1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3D1| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT (LIST 'PLUS '!0 '!1) N1)
                     (LIST 'EXPT
                           (LIST 'PLUS '!5
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!6 '!3)
                                       '!8))
                           N0)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!3 '!5 '!6 '!8) 2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M5 M6 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3A| (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M1))
                             N2 (LIST 'DIFFERENCE 2 'M3) N5
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M6)) N4
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M5 M6 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3D3| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3D3|) 'OPFN) 
(PUT '|ii3D3| 'DEFINED-ON-LINE '2274) 
(PUT '|ii3D3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3D3| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3D3| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!0 '!1 (LIST 'DIFFERENCE '!2 1))
                           N3)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!5 '!3) '!4)
                           N0)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5) 2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5))
                       (LIST '|ii3A| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'DIFFERENCE 2 'M5) N8 N6 N7
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3DL1| 'NUMBER-OF-ARGS 9) 
(FLAG '(|ii3DL1|) 'OPFN) 
(PUT '|ii3DL1| 'DEFINED-ON-LINE '2285) 
(PUT '|ii3DL1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3DL1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE |ii3DL1| (N1 N2 N3 N4 N5 N6 N7 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'MINUS
                      (LIST 'TIMES 2
                            (LIST 'PLUS
                                  (LIST 'DIFFERENCE
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 3 'D)
                                                          N1)
                                                    N2)
                                              N3)
                                        (LIST 'TIMES 2
                                              (LIST 'PLUS N4 N5 N6 N7
                                                    (LIST 'DIFFERENCE N8 N0))))
                                  1)
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5
                                  N6 N7 N8 N0)))
                (LIST 'TIMES N5
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 1) N2 N3 N4
                                  (LIST 'PLUS N5 1) N6 N7 N8 N0)
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 1)
                                  (LIST 'DIFFERENCE N2 1) N3 N4
                                  (LIST 'PLUS N5 1) N6 N7 N8 N0)))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 2) N2 N3 N4 N5
                                  N6 (LIST 'PLUS N7 1) N8 N0)
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 1) N2
                                  (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8 N0)))
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5
                                  N6 N7 N8 (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3D| (LIST 'DIFFERENCE N1 2) N2 N3 N4 N5
                                  N6 N7 N8 (LIST 'DIFFERENCE N0 1)))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N1 1)))) 
(PUT '|ii3DL3| 'NUMBER-OF-ARGS 7) 
(FLAG '(|ii3DL3|) 'OPFN) 
(PUT '|ii3DL3| 'DEFINED-ON-LINE '2293) 
(PUT '|ii3DL3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3DL3| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3DL3| (N3 N4 N5 N6 N7 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'TIMES N6
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5
                                  (LIST 'PLUS N6 1) N7 N8 N0)
                            (LIST '|ii3D| 1 0 (LIST 'DIFFERENCE N3 1) N4 N5
                                  (LIST 'PLUS N6 1) N7 N8 N0)))
                (LIST 'TIMES N7
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8 N0)
                            (LIST '|ii3D| 0 1 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                  (LIST 'PLUS N7 1) N8 N0)))
                (LIST 'TIMES N8
                      (LIST 'PLUS
                            (LIST '|ii3D| 1 1 (LIST 'DIFFERENCE N3 2) N4 N5 N6
                                  N7 (LIST 'PLUS N8 1) N0)
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE
                                        (LIST '|ii3D| 1 1
                                              (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                              N7 (LIST 'PLUS N8 1) N0)
                                        (LIST '|ii3D| 0 1
                                              (LIST 'DIFFERENCE N3 1) N4 N5 N6
                                              N7 (LIST 'PLUS N8 1) N0))
                                  (LIST '|ii3D| 1 0 (LIST 'DIFFERENCE N3 1) N4
                                        N5 N6 N7 (LIST 'PLUS N8 1) N0)))))
          (LIST 'TIMES 2 (LIST 'DIFFERENCE N3 1)))) 
(PUT '|ii3DL04| 'NUMBER-OF-ARGS 6) 
(FLAG '(|ii3DL04|) 'OPFN) 
(PUT '|ii3DL04| 'DEFINED-ON-LINE '2301) 
(PUT '|ii3DL04| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3DL04| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3DL04| (N4 N5 N6 N7 N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'TIMES
                            (LIST 'PLUS
                                  (LIST 'TIMES 2
                                        (LIST 'DIFFERENCE
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE 'D N4)
                                                    N6)
                                              N7))
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE N0 N8)
                                        1))
                            (LIST '|ii3D| 1 1 1 (LIST 'DIFFERENCE N4 1) N5 N6
                                  N7 N8 (LIST 'DIFFERENCE N0 1)))
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE (LIST 'TIMES 3 'D)
                                  (LIST 'TIMES 2
                                        (LIST 'PLUS N4 N5 N6 N7
                                              (LIST 'DIFFERENCE N8 N0) 1)))
                            (LIST '|ii3D| 1 0 1 (LIST 'DIFFERENCE N4 1) N5 N6
                                  N7 N8 (LIST 'DIFFERENCE N0 1))))
                (LIST 'TIMES N8
                      (LIST 'DIFFERENCE
                            (LIST '|ii3D| 1 1 1 (LIST 'DIFFERENCE N4 1)
                                  (LIST 'DIFFERENCE N5 1) N6 N7
                                  (LIST 'PLUS N8 1) (LIST 'DIFFERENCE N0 1))
                            (LIST '|ii3D| 1 1 1 (LIST 'DIFFERENCE N4 1) N5 N6
                                  (LIST 'DIFFERENCE N7 1) (LIST 'PLUS N8 1)
                                  (LIST 'DIFFERENCE N0 1))))
                (LIST 'TIMES 2 (LIST 'DIFFERENCE N0 1)
                      (LIST '|ii3D| 1 1 1 (LIST 'DIFFERENCE N4 1)
                            (LIST 'DIFFERENCE N5 1) N6 N7 N8
                            (LIST 'DIFFERENCE N0 2))))
          (LIST 'DIFFERENCE N4 1))) 
(PUT '|ii3DL06| 'NUMBER-OF-ARGS 4) 
(FLAG '(|ii3DL06|) 'OPFN) 
(PUT '|ii3DL06| 'DEFINED-ON-LINE '2309) 
(PUT '|ii3DL06| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3DL06| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |ii3DL06| (N6 N7 N8 N0)
    (LIST 'DIFFERENCE
          (LIST 'DIFFERENCE
                (LIST 'DIFFERENCE
                      (LIST '|ii3D| 1 1 1 1 1 N6 (LIST 'DIFFERENCE N7 1) N8
                            (LIST 'DIFFERENCE N0 1))
                      (LIST '|ii3D| 1 1 1 1 0 N6 N7 N8
                            (LIST 'DIFFERENCE N0 1)))
                (LIST '|ii3D| 1 1 1 0 1 N6 N7 N8 (LIST 'DIFFERENCE N0 1)))
          (LIST 'QUOTIENT
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'DIFFERENCE
                                  (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'D N6)
                                        N8)
                                  (LIST 'TIMES 2 N7))
                            (LIST '|ii3D| 1 1 1 1 1 (LIST 'DIFFERENCE N6 1) N7
                                  N8 (LIST 'DIFFERENCE N0 1)))
                      (LIST 'TIMES N8
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3D| 1 1 1 1 0
                                        (LIST 'DIFFERENCE N6 1) N7
                                        (LIST 'PLUS N8 1)
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3D| 1 1 1 1 1
                                        (LIST 'DIFFERENCE N6 1)
                                        (LIST 'DIFFERENCE N7 1)
                                        (LIST 'PLUS N8 1)
                                        (LIST 'DIFFERENCE N0 1))))
                      (LIST '|ii3D| 0 1 2 1 1 (LIST 'DIFFERENCE N6 1) N7 N8
                            (LIST 'DIFFERENCE N0 1)))
                (LIST 'DIFFERENCE N6 1)))) 
(PUT '|ii3DL0| 'NUMBER-OF-ARGS 2) 
(FLAG '(|ii3DL0|) 'OPFN) 
(PUT '|ii3DL0| 'DEFINED-ON-LINE '2318) 
(PUT '|ii3DL0| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3DL0| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |ii3DL0| (N8 N0)
    (LIST 'QUOTIENT
          (LIST 'PLUS
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST '|ii3D| 2 1 0 1 1 1 1 N8 N0)
                            (LIST '|ii3D| 2 0 1 1 1 1 1 N8 N0))
                      (LIST '|ii3D| 1 1 1 2 1 1 1 (LIST 'DIFFERENCE N8 1) N0))
                (LIST 'DIFFERENCE (LIST '|ii3D| 1 1 1 2 1 0 1 N8 N0)
                      (LIST '|ii3D| 1 0 2 1 1 1 1 N8 N0))
                (LIST 'TIMES N0
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE
                                  (LIST '|ii3D| 1 1 1 1 1 1 0 N8
                                        (LIST 'DIFFERENCE N0 1))
                                  (LIST '|ii3D| 1 1 1 1 1 1 1
                                        (LIST 'DIFFERENCE N8 1)
                                        (LIST 'DIFFERENCE N0 1)))
                            (LIST '|ii3D| 1 1 1 1 0 1 1 N8
                                  (LIST 'DIFFERENCE N0 1)))))
          (LIST 'PLUS 'D (LIST 'DIFFERENCE N0 4)))) 
(PUT '|i3E| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3E|) 'OPFN) 
(PUT '|i3E| 'DEFINED-ON-LINE '2348) 
(PUT '|i3E| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3E| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3E| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5)
             (LIST '|i2B| (LIST 'PLUS N3 N2 (LIST 'TIMES 2 N5)) N4 N6 N7 N8))))
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N1 N5) (LIST '|i2| N3 N4 N6 N7 N8))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N7 N8)
             (LIST '|j2A| N3 N1 N2 (LIST 'PLUS N6 N7 N8) N5))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N4 N8)
             (LIST '|j2B| (LIST 'PLUS N3 N4 (LIST 'TIMES 2 N8)) N1 N2 N6 N5))))
     ((EVALEQUAL (AEVAL N8) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N4 N7) (LIST '|j2| N1 N3 N2 N5 N6))))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3D| N1 N4 N2 N5 N7 N6 N8)))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3E| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3E| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5 N6 N7 N8)
                   (LIST '|i3E| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3E| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4 N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3E| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3E| N1 (LIST 'PLUS N2 1) N3 N4 N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3E| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)
                   (LIST '|i3E| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4
                         N5 N6 N7 N8))
             (LIST '|i3E| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4 N5 N6
                   N7 N8))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3E| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8)
                   (LIST '|i3E| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8))
             (LIST '|i3E| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8)))))) 
(REMEMBER (LIST '|i3E|)) 
(PUT '|ii3E| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3E|) 'OPFN) 
(PUT '|ii3E| 'DEFINED-ON-LINE '2371) 
(PUT '|ii3E| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3E| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3E| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N8)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N13) 0) (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3E| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLEQ (AEVAL N1) 0)
      (AEVAL (LIST '|ii3E1| (LIST 'MINUS N1) N2 N3 N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N2) 0)
      (AEVAL (LIST '|ii3E2| N1 (LIST 'MINUS N2) N3 N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N3) 0)
      (AEVAL (LIST '|ii3E3| N1 N2 (LIST 'MINUS N3) N4 N5 N6 N7 N8 N13 N23)))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3E| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N13 N23)
                   (LIST '|ii3E| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8
                         N13 N23))
             (LIST '|ii3E| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8 N13
                   N23)))))) 
(REMEMBER (LIST '|ii3E|)) 
(PUT '|ii3E1| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3E1|) 'OPFN) 
(PUT '|ii3E1| 'DEFINED-ON-LINE '2387) 
(PUT '|ii3E1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3E1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3E1| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS 1 (LIST 'DIFFERENCE '!1 '!3)) N1)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N13)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8 '!0) N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                             (LIST 'EXPT '!7 'M7) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M1))
                             N4 (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4)
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                       (LIST 'EXPT '!7 'M7) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3E2| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3E2|) 'OPFN) 
(PUT '|ii3E2| 'DEFINED-ON-LINE '2401) 
(PUT '|ii3E2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3E2| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3E2| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!3 1)) N2)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N13)
                     (LIST 'EXPT '!0 N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!3 '!4 '!6 '!8) 2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M6 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             N4 (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4)) N7
                             (LIST 'DIFFERENCE 2 'M6) N8
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M6 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3E3| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3E3|) 'OPFN) 
(PUT '|ii3E3| 'DEFINED-ON-LINE '2412) 
(PUT '|ii3E3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3E3| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3E3| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE 1 '!1) '!3) N3)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!6) '!8 '!0) N13)
                     (LIST 'EXPT '!0 N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!3 '!4 '!6 '!8) 2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M6 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             N4 (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4)) N7
                             (LIST 'DIFFERENCE 2 'M6) N6
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M3 M4 M6 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|i3F| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3F|) 'OPFN) 
(PUT '|i3F| 'DEFINED-ON-LINE '2443) 
(PUT '|i3F| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3F| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3F| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N5)
             (LIST '|i2B| (LIST 'PLUS N4 N3 (LIST 'TIMES 2 N5)) N1 N7 N6 N8))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N5)
             (LIST '|i2B| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N5)) N4 N6 N7 N8))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|j2| N1 N3 N2 N8 N5))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 N4 (LIST 'TIMES 2 (LIST 'PLUS N5 N8)))
                   N7))))
     ((EVALEQUAL (AEVAL N7) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|j2| N4 N2 N3 N8 N5))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 N4 (LIST 'TIMES 2 (LIST 'PLUS N5 N8)))
                   N6))))
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|j3E| N2 N4 N3 N7 N5 N8 N6)))
     ((EVALEQUAL (AEVAL N4) 0) (AEVAL (LIST '|j3E| N3 N1 N2 N6 N5 N8 N7)))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3F| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4 N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3F| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3F| N1 (LIST 'PLUS N2 1) N3 (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3F| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4 N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3F| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3F| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5 N6 N7 N8)
                   (LIST '|i3F| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8))
             (LIST '|i3F| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6
                   N7 N8))))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3F| (LIST 'DIFFERENCE N1 1) N2 N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3F| N1 (LIST 'DIFFERENCE N2 1) N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8))
             (LIST '|i3F| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5 N6
                   N7 N8))))
     ((EVALLESSP (AEVAL N1) (AEVAL N4))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3F| (LIST 'DIFFERENCE N1 1) N2 N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3F| N1 (LIST 'DIFFERENCE N2 1) N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8))
             (LIST '|i3F| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5 N6
                   N7 N8))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5 N6 N7 N8)
                   (LIST '|i3F| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8))
             (LIST '|i3F| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6
                   N7 N8)))))) 
(REMEMBER (LIST '|i3F|)) 
(PUT '|ii3F| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3F|) 'OPFN) 
(PUT '|ii3F| 'DEFINED-ON-LINE '2469) 
(PUT '|ii3F| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3F| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3F| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N8)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N13) 0) (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3F| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLEQ (AEVAL N2) 0)
      (AEVAL (LIST '|ii3F2| N1 (LIST 'MINUS N2) N3 N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N3) 0)
      (AEVAL (LIST '|ii3F2| N4 (LIST 'MINUS N3) N2 N1 N5 N7 N6 N8 N23 N13)))
     ((EVALLEQ (AEVAL N1) 0)
      (AEVAL (LIST '|ii3F1| (LIST 'MINUS N1) N2 N3 N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N4) 0)
      (AEVAL (LIST '|ii3F1| (LIST 'MINUS N4) N3 N2 N1 N5 N7 N6 N8 N23 N13)))
     ((EVALLESSP (AEVAL N1) (AEVAL N4))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3F| (LIST 'DIFFERENCE N1 1) N2 N3
                         (LIST 'PLUS N4 1) N5 N6 N7 N8 N13 N23)
                   (LIST '|ii3F| N1 (LIST 'DIFFERENCE N2 1) N3
                         (LIST 'PLUS N4 1) N5 N6 N7 N8 N13 N23))
             (LIST '|ii3F| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5
                   N6 N7 N8 N13 N23))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3
                         N4 N5 N6 N7 N8 N13 N23)
                   (LIST '|ii3F| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1)
                         N4 N5 N6 N7 N8 N13 N23))
             (LIST '|ii3F| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1) N5
                   N6 N7 N8 N13 N23)))))) 
(REMEMBER (LIST '|ii3F|)) 
(PUT '|ii3F2| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3F2|) 'OPFN) 
(PUT '|ii3F2| 'DEFINED-ON-LINE '2487) 
(PUT '|ii3F2| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3F2| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3F2| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N2)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!6) '!8 '!0) N13)
                     (LIST 'EXPT (LIST 'PLUS '!7 (LIST 'DIFFERENCE '!8 '!5))
                           N23)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'DIFFERENCE 2 'M5)
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3F1| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3F1|) 'OPFN) 
(PUT '|ii3F1| 'DEFINED-ON-LINE '2502) 
(PUT '|ii3F1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3F1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3F1| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N1)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N13)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8 '!0) N23)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4)
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|i3G| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3G|) 'OPFN) 
(PUT '|i3G| 'DEFINED-ON-LINE '2535) 
(PUT '|i3G| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3G| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3G| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N8)) 0))
      0)
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|g1| N6 N8)
             (LIST '|j2A| N4 N1 N3 (LIST 'PLUS N7 N6 N8) N5))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|i2| N2 N3 N6 N7 N8))
             (LIST '|i1C|
                   (LIST 'PLUS N1 N2 N3 (LIST 'TIMES 2 (LIST 'PLUS N6 N7 N8)))
                   N5))))
     ((EVALEQUAL (AEVAL N6) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N8)
             (LIST '|j2C| N1 N4 (LIST 'PLUS N3 N2 (LIST 'TIMES 2 N8)) N5 N7))))
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|j3E| N2 N4 N3 N7 N5 N8 N6)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3D| N1 N4 N2 N5 N7 N6 N8)))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3G| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7 N8)
                   (LIST '|i3G| (LIST 'DIFFERENCE N1 1) N2 N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8))
             (LIST '|i3G| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5 N6
                   N7 N8))))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3G| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8)
                   (LIST '|i3G| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8))
             (LIST '|i3G| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6
                   N7 N8))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3G| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3G| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8))
             (LIST '|i3G| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5 N6
                   N7 N8))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|i3G| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8)
                   (LIST '|i3G| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8))
             (LIST '|i3G| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8)))))) 
(REMEMBER (LIST '|i3G|)) 
(PUT '|ii3G| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3G|) 'OPFN) 
(PUT '|ii3G| 'DEFINED-ON-LINE '2557) 
(PUT '|ii3G| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3G| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3G| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL (LIST 'MAX N6 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N6 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N7 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N6)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N8)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N7)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N8)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N13) 0) (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3G| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((EVALLEQ (AEVAL N1) 0)
      (AEVAL (LIST '|ii3G1| (LIST 'MINUS N1) N2 N3 N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N3) 0)
      (AEVAL (LIST '|ii3G3| N1 N2 (LIST 'MINUS N3) N4 N5 N6 N7 N8 N13 N23)))
     ((EVALLEQ (AEVAL N4) 0)
      (AEVAL (LIST '|ii3G4| N1 N2 N3 (LIST 'MINUS N4) N5 N6 N7 N8 N13 N23)))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3G| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N13 N23)
                   (LIST '|ii3G| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8
                         N13 N23))
             (LIST '|ii3G| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N13
                   N23)))))) 
(REMEMBER (LIST '|ii3G|)) 
(PUT '|ii3G1| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3G1|) 'OPFN) 
(PUT '|ii3G1| 'DEFINED-ON-LINE '2573) 
(PUT '|ii3G1| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3G1| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3G1| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE 1 '!2) '!3) N1)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N13)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8 '!0) N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!2 'M2)
                             (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                             (LIST 'EXPT '!7 'M7) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| N2
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4)
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!2 'M2)
                       (LIST 'EXPT '!3 'M3) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                       (LIST 'EXPT '!7 'M7) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3G3| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3G3|) 'OPFN) 
(PUT '|ii3G3| 'DEFINED-ON-LINE '2587) 
(PUT '|ii3G3| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3G3| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3G3| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 1)) N3)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!6) '!8 '!0) N13)
                     (LIST 'EXPT '!0 N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!2 '!4 '!6 '!8) 2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M4 M6 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M2)) N2
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4)) N7
                             (LIST 'DIFFERENCE 2 'M6) N6
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M4 M6 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|ii3G4| 'NUMBER-OF-ARGS 10) 
(FLAG '(|ii3G4|) 'OPFN) 
(PUT '|ii3G4| 'DEFINED-ON-LINE '2599) 
(PUT '|ii3G4| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3G4| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE |ii3G4| (N1 N2 N3 N4 N5 N6 N7 N8 N13 N23)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE 1 '!1) '!2) N4)
                     (LIST 'EXPT (LIST 'PLUS '!4 (LIST 'DIFFERENCE '!6 '!0))
                           N13)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!5) '!8)
                           N23)
                     (LIST 'EXPT (LIST 'TIMES '!0 '!1 '!2 '!4 '!5 '!6 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M4 M5 M6 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!4 'M4)
                             (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3B| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M2)) N2
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'DIFFERENCE 2 'M5)
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M6)) N8
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M4 M5 M6 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!4 'M4)
                       (LIST 'EXPT '!5 'M5) (LIST 'EXPT '!6 'M6)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|i3H| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3H|) 'OPFN) 
(PUT '|i3H| 'DEFINED-ON-LINE '2630) 
(PUT '|i3H| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3H| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3H| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0))
      0)
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N1 N6) (LIST '|j2| N3 N5 N4 N8 N7))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL (LIST 'TIMES (LIST '|i1| N5 N7) (LIST '|j2| N1 N3 N2 N6 N8))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N6)
             (LIST '|j2B| (LIST 'PLUS N3 N2 (LIST 'TIMES 2 N6)) N5 N4 N8 N7))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N4 N7)
             (LIST '|j2B| (LIST 'PLUS N3 N4 (LIST 'TIMES 2 N7)) N1 N2 N8 N6))))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|j3F| N1 N2 N4 N5 N6 N7 N8)))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3H| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3 N4 N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3H| N1 (LIST 'PLUS N2 1) N3 N4 N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3H| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1) N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| N1 N2 N3 (LIST 'PLUS N4 1)
                         (LIST 'DIFFERENCE N5 1) N6 N7 N8)
                   (LIST '|i3H| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3H| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3 N4
                         N5 N6 N7 N8)
                   (LIST '|i3H| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N5) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3H| N1 N2 N3 N4 (LIST 'PLUS N5 1) N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| N1 N2 N3 (LIST 'DIFFERENCE N4 1)
                         (LIST 'PLUS N5 1) N6 N7 N8)
                   (LIST '|i3H| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                         (LIST 'PLUS N5 1) N6 N7 N8)))))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALLESSP (AEVAL (LIST 'MAX N4 N5)) (AEVAL (LIST 'MAX N1 N2)))
        (AEVAL
         (LIST 'PLUS (LIST '|i3H| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)
               (LIST 'DIFFERENCE
                     (LIST '|i3H| N1 N2 (LIST 'PLUS N3 1)
                           (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8)
                     (LIST '|i3H| N1 N2 (LIST 'PLUS N3 1) N4
                           (LIST 'DIFFERENCE N5 1) N6 N7 N8)))))
       (T
        (AEVAL
         (LIST 'PLUS (LIST '|i3H| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)
               (LIST 'DIFFERENCE
                     (LIST '|i3H| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1)
                           N4 N5 N6 N7 N8)
                     (LIST '|i3H| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1)
                           N4 N5 N6 N7 N8)))))))
     ((EVALLESSP (AEVAL (LIST 'MAX N4 N5)) (AEVAL (LIST 'MAX N1 N2)))
      (AEVAL
       (LIST 'PLUS (LIST '|i3H| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8)
                   (LIST '|i3H| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7
                         N8)))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|i3H| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3H| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8)
                   (LIST '|i3H| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7
                         N8))))))) 
(REMEMBER (LIST '|i3H|)) 
(PUT '|j3F| 'NUMBER-OF-ARGS 7) 
(FLAG '(|j3F|) 'OPFN) 
(PUT '|j3F| 'DEFINED-ON-LINE '2660) 
(PUT '|j3F| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|j3F| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |j3F| (N1 N2 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N2) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N1 N6) (LIST '|i1| N4 N8)
             (LIST '|i1A| (LIST 'PLUS N5 N4 (LIST 'TIMES 2 N8)) N7)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N5 N7) (LIST '|i1| N2 N8)
             (LIST '|i1A| (LIST 'PLUS N1 N2 (LIST 'TIMES 2 N8)) N6)
             (LIST '|b| 2))))
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N2 N6)
             (LIST '|j2B| (LIST 'PLUS N2 (LIST 'TIMES 2 N6)) N5 N4 N8 N7))))
     ((EVALEQUAL (AEVAL N5) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N4 N7)
             (LIST '|j2B| (LIST 'PLUS N4 (LIST 'TIMES 2 N7)) N1 N2 N8 N6))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N4 N5 N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N4 1) N5
                         N6 N7 N8)
                   (LIST '|j3F| N1 (LIST 'PLUS N2 1) N4 (LIST 'DIFFERENCE N5 1)
                         N6 N7 N8)))))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N4 1) N5 N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| N1 N2 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)
                         N6 N7 N8)
                   (LIST '|j3F| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N4 1) N5
                         N6 N7 N8)))))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N4 N5 N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| (LIST 'PLUS N1 1) N2 N4 (LIST 'DIFFERENCE N5 1)
                         N6 N7 N8)
                   (LIST '|j3F| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N4 1) N5
                         N6 N7 N8)))))
     ((EVALLESSP (AEVAL N5) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| (LIST 'DIFFERENCE N1 1) N2 N4 (LIST 'PLUS N5 1) N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| N1 N2 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1)
                         N6 N7 N8)
                   (LIST '|j3F| N1 (LIST 'DIFFERENCE N2 1) N4 (LIST 'PLUS N5 1)
                         N6 N7 N8)))))
     ((EVALLESSP (AEVAL N5) (AEVAL N1))
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N4 N5 N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| (LIST 'PLUS N1 1) N2 N4 (LIST 'DIFFERENCE N5 1)
                         N6 N7 N8)
                   (LIST '|j3F| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N4 1) N5
                         N6 N7 N8)))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST '|j3F| (LIST 'DIFFERENCE N1 1) N2 N4 (LIST 'PLUS N5 1) N6 N7
                   N8)
             (LIST 'DIFFERENCE
                   (LIST '|j3F| N1 N2 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1)
                         N6 N7 N8)
                   (LIST '|j3F| N1 (LIST 'DIFFERENCE N2 1) N4 (LIST 'PLUS N5 1)
                         N6 N7 N8))))))) 
(PUT '|ii3H| 'NUMBER-OF-ARGS 11) 
(FLAG '(|ii3H|) 'OPFN) 
(PUT '|ii3H| 'DEFINED-ON-LINE '2681) 
(PUT '|ii3H| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3H| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3H| (N1 N2 N3 N4 N5 N6 N7 N8 N12 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N12) 0) (EVALEQUAL (AEVAL N13) 0)
           (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3H| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N4) 0))
      (AEVAL
       (LIST '|k3A| N1 N5 N3 N6 N7 N8 (LIST 'MINUS N2) (LIST 'MINUS N4) N23 N12
             N13)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3B| N1 N4 N3 N6 N7 N8 (LIST 'MINUS N2) (LIST 'MINUS N5) 0 0 N12
             N13 N23)))
     ((AND (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N1) 0))
      (AEVAL
       (LIST '|k3B| N5 N2 N3 N7 N6 N8 (LIST 'MINUS N4) (LIST 'MINUS N1) 0 0 N13
             N12 N23)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3B| N1 N4 N5 N6 N8 N7 0 (LIST 'MINUS N3) 0 (LIST 'MINUS N2) N23
             N13 N12)))
     ((AND (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3B| N5 N2 N1 N7 N8 N6 0 (LIST 'MINUS N3) 0 (LIST 'MINUS N4) N23
             N12 N13)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3C| N2 N4 N3 N6 N7 N8 (LIST 'MINUS N1) (LIST 'MINUS N5) 0 N12
             N13 N23)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3E| N2 N5 N4 N7 N8 N6 (LIST 'MINUS N1) 0 (LIST 'MINUS N3) N12
             N23 N13)))
     ((AND (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3E| N4 N1 N2 N6 N8 N7 (LIST 'MINUS N5) 0 (LIST 'MINUS N3) N13
             N23 N12)))
     ((EVALLEQ (AEVAL N3) 0)
      (AEVAL (LIST '|jj3F| N1 N2 N3 N4 N5 N6 N7 N8 N12 N13 N23)))
     ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0)
          (AND (EVALGREATERP (AEVAL N4) 0) (EVALGREATERP (AEVAL N5) 0)
               (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) (AEVAL (LIST 'MAX N1 N2)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3H| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3H| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3H| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13
                   N23))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3H| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3H| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3H| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8 N12 N13
                   N23)))))) 
(REMEMBER (LIST '|ii3H|)) 
(PUT '|jj3F| 'NUMBER-OF-ARGS 11) 
(FLAG '(|jj3F|) 'OPFN) 
(PUT '|jj3F| 'DEFINED-ON-LINE '2705) 
(PUT '|jj3F| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|jj3F| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |jj3F| (N1 N2 N3 N4 N5 N6 N7 N8 N12 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N4 N5)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N4)) 0))
      0)
     ((EVALLEQ (AEVAL N2) 0)
      (AEVAL
       (LIST '|k3B| N1 N4 N5 N6 N8 N7 0 (LIST 'MINUS N3) 0 (LIST 'MINUS N2) N23
             N13 N12)))
     ((EVALLEQ (AEVAL N4) 0)
      (AEVAL
       (LIST '|k3B| N5 N2 N1 N7 N8 N6 0 (LIST 'MINUS N3) 0 (LIST 'MINUS N4) N23
             N12 N13)))
     ((EVALLEQ (AEVAL N1) 0)
      (AEVAL
       (LIST '|k3E| N2 N5 N4 N7 N8 N6 (LIST 'MINUS N1) 0 (LIST 'MINUS N3) N12
             N23 N13)))
     ((EVALLEQ (AEVAL N5) 0)
      (AEVAL
       (LIST '|k3E| N4 N1 N2 N6 N8 N7 (LIST 'MINUS N5) 0 (LIST 'MINUS N3) N13
             N23 N12)))
     ((EVALLEQ (AEVAL (LIST 'MAX N4 N5)) (AEVAL (LIST 'MAX N1 N2)))
      (COND
       ((EVALLEQ (AEVAL N2) (AEVAL N1))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|jj3F| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|jj3F| (LIST 'PLUS N1 1) N2 N3
                           (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N12 N13 N23))
               (LIST '|jj3F| (LIST 'PLUS N1 1) N2 N3 N4 (LIST 'DIFFERENCE N5 1)
                     N6 N7 N8 N12 N13 N23))))
       (T
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|jj3F| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|jj3F| N1 (LIST 'PLUS N2 1) N3 N4
                           (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13 N23))
               (LIST '|jj3F| N1 (LIST 'PLUS N2 1) N3 (LIST 'DIFFERENCE N4 1) N5
                     N6 N7 N8 N12 N13 N23))))))
     ((EVALLEQ (AEVAL N4) (AEVAL N5))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|jj3F| (LIST 'DIFFERENCE N1 1) N2 N3 N4
                         (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23)
                   (LIST '|jj3F| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                         (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23))
             (LIST '|jj3F| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1)
                   N6 N7 N8 N12 N13 N23))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|jj3F| N1 (LIST 'DIFFERENCE N2 1) N3
                         (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23)
                   (LIST '|jj3F| (LIST 'DIFFERENCE N1 1) N2 N3
                         (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23))
             (LIST '|jj3F| N1 N2 N3 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)
                   N6 N7 N8 N12 N13 N23)))))) 
(PUT '|i3I| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3I|) 'OPFN) 
(PUT '|i3I| 'DEFINED-ON-LINE '2751) 
(PUT '|i3I| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3I| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3I| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0)
      (AEVAL
       (LIST 'TIMES
             (LIST '|i1C|
                   (LIST 'PLUS N5 N2 N3 N4 (LIST 'TIMES 2 (LIST 'PLUS N6 N8)))
                   N7)
             (LIST 'SUB (LIST 'EQUAL (LIST '|b| 1) (LIST '|b| 4))
                   (LIST 'EQUAL (LIST '|b| 2) (LIST '|b| 3))
                   (LIST '|j2| N2 N4 N3 N6 N8)))))
     ((EVALEQUAL (AEVAL N3) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N4 N8)
             (LIST '|j2B| (LIST 'PLUS N5 N4 (LIST 'TIMES 2 N8)) N1 N2 N7 N6))))
     ((EVALEQUAL (AEVAL N4) 0)
      (AEVAL
       (LIST 'TIMES (LIST '|i1| N3 N8)
             (LIST '|j2C| N1 N5 (LIST 'PLUS N2 N3 (LIST 'TIMES 2 N8)) N6 N7))))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3I| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3I| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3I| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N3) 0)
      (AEVAL
       (LIST 'PLUS
             (LIST '|i3I| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5 N6
                   N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3I| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3I| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3I| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3I| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3I| (LIST 'DIFFERENCE N1 1) N2 N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|i3I| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3I| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8)
                   (LIST '|i3I| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7
                         N8))))))) 
(REMEMBER (LIST '|i3I|)) 
(PUT '|ii3I| 'NUMBER-OF-ARGS 11) 
(FLAG '(|ii3I|) 'OPFN) 
(PUT '|ii3I| 'DEFINED-ON-LINE '2769) 
(PUT '|ii3I| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3I| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3I| (N1 N2 N3 N4 N5 N6 N7 N8 N12 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N12) 0) (EVALEQUAL (AEVAL N13) 0)
           (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3I| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3B| N1 N4 N5 N6 N8 N7 (LIST 'MINUS N2) 0 (LIST 'MINUS N3) 0 N12
             N23 N13)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3C| N2 N4 N5 N6 N8 N7 (LIST 'MINUS N1) 0 (LIST 'MINUS N3) N12
             N23 N13)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0))
      (AEVAL
       (LIST '|k3D| N3 N5 N4 N7 N6 N8 (LIST 'MINUS N1) 0 0 (LIST 'MINUS N2) N23
             N12 N13)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N4) 0))
      (AEVAL
       (LIST '|k3D| N3 N5 N2 N7 N8 N6 0 (LIST 'MINUS N1) 0 (LIST 'MINUS N4) N12
             N23 N13)))
     ((AND (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3D| N3 N1 N2 N6 N8 N7 0 (LIST 'MINUS N5) (LIST 'MINUS N4) 0 N12
             N13 N23)))
     ((AND (EVALLEQ (AEVAL N3) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3E| N4 N1 N2 N6 N7 N8 0 (LIST 'MINUS N3) (LIST 'MINUS N5) N23
             N13 N12)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N4) 0))
      (AEVAL
       (LIST '|k3F| N1 N5 N3 N6 N7 N8 (LIST 'MINUS N2) 0 (LIST 'MINUS N4) N13
             N23 N12)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3G| N2 N4 N3 N6 N8 N7 (LIST 'MINUS N5) (LIST 'MINUS N1) 0 N12
             N23 N13)))
     ((EVALLEQ (AEVAL N1) 0)
      (COND
       ((EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) (AEVAL N5))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3I| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                           (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3I| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                           (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23))
               (LIST '|ii3I| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1)
                     N6 N7 N8 N12 N13 N23))))
       ((EVALLEQ (AEVAL (LIST 'MAX N2 N3 N5)) (AEVAL N4))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3I| N1 N2 (LIST 'DIFFERENCE N3 1)
                           (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3I| N1 (LIST 'DIFFERENCE N2 1) N3
                           (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23))
               (LIST '|ii3I| N1 N2 N3 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)
                     N6 N7 N8 N12 N13 N23))))
       ((EVALLEQ (AEVAL (LIST 'MAX N2 N4 N5)) (AEVAL N3))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3I| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1)
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3I| N1 N2 (LIST 'PLUS N3 1) N4
                           (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13 N23))
               (LIST '|ii3I| N1 N2 (LIST 'PLUS N3 1) (LIST 'DIFFERENCE N4 1) N5
                     N6 N7 N8 N12 N13 N23))))
       (T
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3I| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1)
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3I| N1 (LIST 'PLUS N2 1) N3
                           (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N12 N13 N23))
               (LIST '|ii3I| N1 (LIST 'PLUS N2 1) N3 N4 (LIST 'DIFFERENCE N5 1)
                     N6 N7 N8 N12 N13 N23))))))
     ((OR (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N5) 0)
          (AND (EVALGREATERP (AEVAL N3) 0) (EVALGREATERP (AEVAL N4) 0)
               (EVALLEQ (AEVAL (LIST 'MAX N3 N4)) (AEVAL (LIST 'MAX N2 N5)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3I| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3I| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3I| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N12 N13
                   N23))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3I| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3I| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3I| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13
                   N23)))))) 
(REMEMBER (LIST '|ii3I|)) 
(PUT '|i3J| 'NUMBER-OF-ARGS 8) 
(FLAG '(|i3J|) 'OPFN) 
(PUT '|i3J| 'DEFINED-ON-LINE '2830) 
(PUT '|i3J| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|i3J| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |i3J| (N1 N2 N3 N4 N5 N6 N7 N8)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N5)) 0))
      0)
     ((EVALEQUAL (AEVAL N1) 0) (AEVAL (LIST '|i3I| N5 N4 N3 N2 0 N8 N7 N6)))
     ((EVALEQUAL (AEVAL N5) 0) (AEVAL (LIST '|i3I| N1 N2 N3 N4 0 N6 N7 N8)))
     ((EVALEQUAL (AEVAL N2) 0) (AEVAL (LIST '|i3I| N1 0 N3 N4 N5 N6 N8 N7)))
     ((EVALEQUAL (AEVAL N4) 0) (AEVAL (LIST '|i3I| N5 0 N3 N2 N1 N8 N6 N7)))
     ((EVALEQUAL (AEVAL N3) 0) (AEVAL (LIST '|i3H| N1 N2 0 N4 N5 N6 N8 N7)))
     ((EVALLESSP (AEVAL N1) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| (LIST 'PLUS N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| (LIST 'PLUS N1 1) N2 (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3J| (LIST 'PLUS N1 1) N2 N3 (LIST 'DIFFERENCE N4 1)
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N5) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| N1 N2 N3 N4 (LIST 'PLUS N5 1) N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| N1 N2 (LIST 'DIFFERENCE N3 1) N4
                         (LIST 'PLUS N5 1) N6 N7 N8)
                   (LIST '|i3J| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                         (LIST 'PLUS N5 1) N6 N7 N8)))))
     ((EVALLESSP (AEVAL N2) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| N1 (LIST 'PLUS N2 1) N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| N1 (LIST 'PLUS N2 1) (LIST 'DIFFERENCE N3 1) N4
                         N5 N6 N7 N8)
                   (LIST '|i3J| N1 (LIST 'PLUS N2 1) N3 N4
                         (LIST 'DIFFERENCE N5 1) N6 N7 N8)))))
     ((EVALLESSP (AEVAL N4) 0)
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| N1 N2 N3 (LIST 'PLUS N4 1) N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| N1 N2 (LIST 'DIFFERENCE N3 1) (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)
                   (LIST '|i3J| (LIST 'DIFFERENCE N1 1) N2 N3 (LIST 'PLUS N4 1)
                         N5 N6 N7 N8)))))
     ((EVALLESSP (AEVAL N3) 0)
      (COND
       ((EVALLESSP (AEVAL (LIST 'MAX N2 N5)) (AEVAL (LIST 'MAX N1 N4)))
        (AEVAL
         (LIST 'PLUS
               (LIST '|i3J| N1 (LIST 'DIFFERENCE N2 1) (LIST 'PLUS N3 1) N4 N5
                     N6 N7 N8)
               (LIST 'DIFFERENCE
                     (LIST '|i3J| N1 N2 (LIST 'PLUS N3 1) N4
                           (LIST 'DIFFERENCE N5 1) N6 N7 N8)
                     (LIST '|i3J| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)))))
       (T
        (AEVAL
         (LIST 'PLUS
               (LIST '|i3J| (LIST 'DIFFERENCE N1 1) N2 (LIST 'PLUS N3 1) N4 N5
                     N6 N7 N8)
               (LIST 'DIFFERENCE
                     (LIST '|i3J| N1 N2 (LIST 'PLUS N3 1)
                           (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8)
                     (LIST '|i3J| N1 N2 (LIST 'PLUS N3 1) N4 N5 N6 N7 N8)))))))
     ((EVALLESSP (AEVAL (LIST 'MAX N2 N5)) (AEVAL (LIST 'MAX N1 N4)))
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8)
                   (LIST '|i3J| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7
                         N8)))))
     (T
      (AEVAL
       (LIST 'PLUS (LIST '|i3J| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8)
             (LIST 'DIFFERENCE
                   (LIST '|i3J| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8)
                   (LIST '|i3J| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7
                         N8))))))) 
(REMEMBER (LIST '|i3J|)) 
(PUT '|ii3J| 'NUMBER-OF-ARGS 11) 
(FLAG '(|ii3J|) 'OPFN) 
(PUT '|ii3J| 'DEFINED-ON-LINE '2860) 
(PUT '|ii3J| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|ii3J| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |ii3J| (N1 N2 N3 N4 N5 N6 N7 N8 N12 N13 N23)
    (COND
     ((OR (EVALLEQ (AEVAL N6) 0) (EVALLEQ (AEVAL N7) 0) (EVALLEQ (AEVAL N8) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N1 N2 N3)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N2 N3 N4)) 0)
          (EVALLEQ (AEVAL (LIST 'MAX N3 N4 N5)) 0))
      0)
     ((AND (EVALEQUAL (AEVAL N12) 0) (EVALEQUAL (AEVAL N13) 0)
           (EVALEQUAL (AEVAL N23) 0))
      (AEVAL (LIST '|i3J| N1 N2 N3 N4 N5 N6 N7 N8)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3B| N1 N4 N5 N6 N7 N8 0 0 (LIST 'MINUS N3) (LIST 'MINUS N2) N13
             N23 N12)))
     ((AND (EVALLEQ (AEVAL N4) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3B| N5 N2 N1 N8 N7 N6 0 0 (LIST 'MINUS N3) (LIST 'MINUS N4) N13
             N12 N23)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N2) 0))
      (AEVAL
       (LIST '|k3D| N3 N5 N4 N8 N6 N7 (LIST 'MINUS N1) 0 (LIST 'MINUS N2) 0 N23
             N13 N12)))
     ((AND (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL N4) 0))
      (AEVAL
       (LIST '|k3D| N3 N1 N2 N6 N8 N7 (LIST 'MINUS N5) 0 (LIST 'MINUS N4) 0 N12
             N13 N23)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3E| N2 N5 N4 N8 N7 N6 (LIST 'MINUS N1) (LIST 'MINUS N3) 0 N12
             N13 N23)))
     ((AND (EVALLEQ (AEVAL N5) 0) (EVALLEQ (AEVAL N3) 0))
      (AEVAL
       (LIST '|k3E| N4 N1 N2 N6 N7 N8 (LIST 'MINUS N5) (LIST 'MINUS N3) 0 N23
             N13 N12)))
     ((AND (EVALLEQ (AEVAL N2) 0) (EVALLEQ (AEVAL N4) 0))
      (AEVAL
       (LIST '|k3F| N1 N5 N3 N6 N8 N7 0 (LIST 'MINUS N2) (LIST 'MINUS N4) N12
             N23 N13)))
     ((AND (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N5) 0))
      (AEVAL
       (LIST '|k3G| N2 N4 N3 N6 N8 N7 0 (LIST 'MINUS N1) (LIST 'MINUS N5) N12
             N23 N13)))
     ((EVALLEQ (AEVAL N3) 0)
      (COND
       ((EVALLEQ (AEVAL (LIST 'MAX N1 N2 N4)) (AEVAL N5))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3J| (LIST 'DIFFERENCE N1 1) N2 N3 N4
                           (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3J| N1 (LIST 'DIFFERENCE N2 1) N3 N4
                           (LIST 'PLUS N5 1) N6 N7 N8 N12 N13 N23))
               (LIST '|ii3J| N1 N2 N3 (LIST 'DIFFERENCE N4 1) (LIST 'PLUS N5 1)
                     N6 N7 N8 N12 N13 N23))))
       ((EVALLEQ (AEVAL (LIST 'MAX N5 N4 N2)) (AEVAL N1))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3J| (LIST 'PLUS N1 1) (LIST 'DIFFERENCE N2 1) N3
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3J| (LIST 'PLUS N1 1) N2 N3
                           (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N12 N13 N23))
               (LIST '|ii3J| (LIST 'PLUS N1 1) N2 N3 N4 (LIST 'DIFFERENCE N5 1)
                     N6 N7 N8 N12 N13 N23))))
       ((EVALLEQ (AEVAL (LIST 'MAX N1 N2 N5)) (AEVAL N4))
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3J| N1 (LIST 'DIFFERENCE N2 1) N3
                           (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3J| (LIST 'DIFFERENCE N1 1) N2 N3
                           (LIST 'PLUS N4 1) N5 N6 N7 N8 N12 N13 N23))
               (LIST '|ii3J| N1 N2 N3 (LIST 'PLUS N4 1) (LIST 'DIFFERENCE N5 1)
                     N6 N7 N8 N12 N13 N23))))
       (T
        (AEVAL
         (LIST 'PLUS
               (LIST 'DIFFERENCE
                     (LIST '|ii3J| (LIST 'DIFFERENCE N1 1) (LIST 'PLUS N2 1) N3
                           N4 N5 N6 N7 N8 N12 N13 N23)
                     (LIST '|ii3J| N1 (LIST 'PLUS N2 1) N3 N4
                           (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13 N23))
               (LIST '|ii3J| N1 (LIST 'PLUS N2 1) N3 (LIST 'DIFFERENCE N4 1) N5
                     N6 N7 N8 N12 N13 N23))))))
     ((OR (EVALLEQ (AEVAL N1) 0) (EVALLEQ (AEVAL N4) 0)
          (AND (EVALGREATERP (AEVAL N2) 0) (EVALGREATERP (AEVAL N5) 0)
               (EVALLEQ (AEVAL (LIST 'MAX N2 N5)) (AEVAL (LIST 'MAX N1 N4)))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3J| N1 (LIST 'DIFFERENCE N2 1) N3 N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3J| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3J| N1 N2 N3 N4 (LIST 'DIFFERENCE N5 1) N6 N7 N8 N12 N13
                   N23))))
     (T
      (AEVAL
       (LIST 'PLUS
             (LIST 'DIFFERENCE
                   (LIST '|ii3J| (LIST 'DIFFERENCE N1 1) N2 N3 N4 N5 N6 N7 N8
                         N12 N13 N23)
                   (LIST '|ii3J| N1 N2 (LIST 'DIFFERENCE N3 1) N4 N5 N6 N7 N8
                         N12 N13 N23))
             (LIST '|ii3J| N1 N2 N3 (LIST 'DIFFERENCE N4 1) N5 N6 N7 N8 N12 N13
                   N23)))))) 
(REMEMBER (LIST '|ii3J|)) 
(PUT '|k3A| 'NUMBER-OF-ARGS 11) 
(FLAG '(|k3A|) 'OPFN) 
(PUT '|k3A| 'DEFINED-ON-LINE '2908) 
(PUT '|k3A| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3A| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3A| (N1 N2 N3 N4 N5 N8 N9 N10 N45 N48 N58)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!3 1)) N9)
                     (LIST 'EXPT (LIST 'PLUS '!2 (LIST 'DIFFERENCE '!3 1)) N10)
                     (LIST 'EXPT '!0 N45)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N48)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!7 '!5) '!8)
                           N58)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'DIFFERENCE 2 'M6) (LIST 'DIFFERENCE 2 'M7)
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3B| 'NUMBER-OF-ARGS 13) 
(FLAG '(|k3B|) 'OPFN) 
(PUT '|k3B| 'DEFINED-ON-LINE '2933) 
(PUT '|k3B| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3B| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3B| (N1 N2 N3 N4 N7 N8 N9 N10 N11 N12 N48 N78 N47)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!3 1)) N9)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!2 '!3) 1) N10)
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 1)) N11)
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N12)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N48)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!7 '!5) '!8)
                           N78)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!6) '!8 '!0) N47)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'DIFFERENCE 2 'M5) (LIST 'DIFFERENCE 2 'M6)
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3C| 'NUMBER-OF-ARGS 12) 
(FLAG '(|k3C|) 'OPFN) 
(PUT '|k3C| 'DEFINED-ON-LINE '2959) 
(PUT '|k3C| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3C| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3C| (N1 N2 N3 N6 N7 N8 N9 N10 N11 N68 N78 N67)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!1 '!3) 1) N9)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!2 '!3) 1) N10)
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N11)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N68)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!7 '!5) '!8)
                           N78)
                     (LIST 'EXPT
                           (LIST 'PLUS '!4
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!5 '!6)
                                       '!7)
                                 '!0)
                           N67)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4) (LIST 'DIFFERENCE 2 'M5)
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3D| 'NUMBER-OF-ARGS 13) 
(FLAG '(|k3D|) 'OPFN) 
(PUT '|k3D| 'DEFINED-ON-LINE '2987) 
(PUT '|k3D| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3D| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3D| (N1 N2 N3 N5 N6 N7 N9 N10 N11 N12 N57 N56 N67)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!1 '!3) 1) N9)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!2) 1) N10)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!1 '!2) 1) N11)
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N12)
                     (LIST 'EXPT (LIST 'PLUS '!5 (LIST 'DIFFERENCE '!7 '!8))
                           N57)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8 '!0) N56)
                     (LIST 'EXPT
                           (LIST 'PLUS '!6
                                 (LIST 'DIFFERENCE
                                       (LIST 'DIFFERENCE
                                             (LIST 'DIFFERENCE '!7 '!4) '!5)
                                       '!0))
                           N67)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3C| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4)
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'DIFFERENCE 2 'M8)
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3E| 'NUMBER-OF-ARGS 12) 
(FLAG '(|k3E|) 'OPFN) 
(PUT '|k3E| 'DEFINED-ON-LINE '3013) 
(PUT '|k3E| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3E| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3E| (N1 N2 N3 N5 N6 N8 N9 N10 N11 N68 N58 N56)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N9)
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 1)) N10)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!2) 1) N11)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N68)
                     (LIST 'EXPT
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!7 '!5) '!8)
                           N58)
                     (LIST 'EXPT
                           (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8 '!0) N56)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4)
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'DIFFERENCE 2 'M7)
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3F| 'NUMBER-OF-ARGS 12) 
(FLAG '(|k3F|) 'OPFN) 
(PUT '|k3F| 'DEFINED-ON-LINE '3041) 
(PUT '|k3F| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3F| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3F| (N1 N2 N3 N4 N5 N8 N9 N10 N11 N48 N58 N45)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 1)) N9)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!2) 1) N10)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!1) 1) N11)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!4 '!6) '!8)
                           N48)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!5 '!7) '!8)
                           N58)
                     (LIST 'EXPT '!0 N45)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'PLUS N4 (LIST 'DIFFERENCE 2 'M4))
                             (LIST 'PLUS N5 (LIST 'DIFFERENCE 2 'M5))
                             (LIST 'DIFFERENCE 2 'M6) (LIST 'DIFFERENCE 2 'M7)
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(PUT '|k3G| 'NUMBER-OF-ARGS 12) 
(FLAG '(|k3G|) 'OPFN) 
(PUT '|k3G| 'DEFINED-ON-LINE '3069) 
(PUT '|k3G| 'DEFINED-IN-FILE 'HEPHYS/GRINDER.RED) 
(PUT '|k3G| 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |k3G| (N1 N2 N3 N6 N7 N8 N9 N10 N11 N68 N78 N67)
    (PROG (P)
      (SETQ P
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'PLUS '!1 (LIST 'DIFFERENCE '!2 '!3))
                           N9)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!2) 1) N10)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!3 '!1) 1) N11)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!6 '!4) '!8)
                           N68)
                     (LIST 'EXPT (LIST 'PLUS (LIST 'DIFFERENCE '!7 '!5) '!8)
                           N78)
                     (LIST 'EXPT
                           (LIST 'PLUS '!4
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE '!5 '!6)
                                       '!7)
                                 '!0)
                           N67)
                     (LIST 'EXPT
                           (LIST 'TIMES '!0 '!1 '!2 '!3 '!4 '!5 '!6 '!7 '!8)
                           2))))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(MATCH00
                (LIST
                 (LIST 'EQUAL
                       (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                             (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                             (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                             (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                             (LIST 'EXPT '!8 'M8))
                       (LIST '|ii3D| (LIST 'PLUS N1 (LIST 'DIFFERENCE 2 'M1))
                             (LIST 'PLUS N2 (LIST 'DIFFERENCE 2 'M2))
                             (LIST 'PLUS N3 (LIST 'DIFFERENCE 2 'M3))
                             (LIST 'DIFFERENCE 2 'M4) (LIST 'DIFFERENCE 2 'M5)
                             (LIST 'PLUS N6 (LIST 'DIFFERENCE 2 'M6))
                             (LIST 'PLUS N7 (LIST 'DIFFERENCE 2 'M7))
                             (LIST 'PLUS N8 (LIST 'DIFFERENCE 2 'M8))
                             (LIST 'DIFFERENCE 'M0 2))))))))
      (SETQ P (AEVAL P))
      (AEVAL
       (FORALL
        (LIST '(M0 M1 M2 M3 M4 M5 M6 M7 M8) 'T
              '(CLEAR
                (LIST
                 (LIST 'TIMES (LIST 'EXPT '!0 'M0) (LIST 'EXPT '!1 'M1)
                       (LIST 'EXPT '!2 'M2) (LIST 'EXPT '!3 'M3)
                       (LIST 'EXPT '!4 'M4) (LIST 'EXPT '!5 'M5)
                       (LIST 'EXPT '!6 'M6) (LIST 'EXPT '!7 'M7)
                       (LIST 'EXPT '!8 'M8)))))))
      (RETURN (AEVAL P)))) 
(SETQ *LOWER (SETQ *RAISE NIL)) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LIENDMC1)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'HEISENBERG 'COMMUTATIVE 'LIE_ALGEBRA)) 
(PUT 'LIENDIMCOM1 'NUMBER-OF-ARGS 1) 
(FLAG '(LIENDIMCOM1) 'OPFN) 
(PUT 'LIENDIMCOM1 'DEFINED-ON-LINE '36) 
(PUT 'LIENDIMCOM1 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENDIMCOM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIENDIMCOM1 (N)
    (PROG ()
      (COND
       ((OR (NOT (BOOLVALUE* (REVALX (FIXP N)))) (EVALLESSP (AEVAL N) 2))
        (AEVAL (REDERR "dimension out of range"))))
      (COND
       ((NEQ (GETTYPE 'LIENSTRUCIN) 'ARRAY) (REDERR "lienstrucin not ARRAY")))
      (COND
       ((EVALNEQ (AEVAL (LIST 'LENGTH 'LIENSTRUCIN))
                 (AEVAL
                  (LIST 'LIST (LIST 'PLUS N 1) (LIST 'PLUS N 1)
                        (LIST 'PLUS N 1))))
        (AEVAL (REDERR "dimension of lienstrucin out of range"))))
      (AEVAL (MATRIX (LIST (LIST 'LIENTRANS N N))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'LIE_CC (IEVAL N) (IEVAL N) (IEVAL N))))
      (AEVAL (LIST 'LIENINSTRUC N))
      (AEVAL (LIST 'LIENJACTEST N))
      (COND
       ((EVALNEQ (AEVAL 'LIE_JTEST) 0)
        (PROGN
         (AEVAL (CLEAR (LIST 'LIE_CC 'LIE_JTEST)))
         (AEVAL (REDERR "not a Lie algebra")))))
      (PROGN
       (AEVAL (LIST 'LIENDIMCOM N))
       (COND
        ((EVALEQUAL (AEVAL 'LIE_DIM) 0)
         (PROGN
          (COND
           ((BOOLVALUE* (REVALX *TR_LIE))
            (ASSGNPRI (AEVAL "The given Lie algebra is commutative") NIL
                      'ONLY)))
          (SETK 'LIENTRANS (AEVAL (LIST 'EXPT 'LIENTRANS 0)))
          (SETK 'LIE_LIST (AEVAL (LIST 'LIST (LIST 'COMMUTATIVE N))))))
        ((EVALEQUAL (AEVAL 'LIE_DIM) 1)
         (PROGN
          (COND
           ((EVALEQUAL (AEVAL 'LIE_HELP) 0)
            (AEVAL (LIST 'LIENCENTINCOM N 'LIE_TT 'LIE_P 'LIE_Q)))
           (T (AEVAL (LIST 'LIENCENTOUTCOM N 'LIE_TT 'LIE_S))))
          (COND
           ((BOOLVALUE* (REVALX *TR_LIE))
            (AEVAL
             (LIST 'LIENOUTFORM 'LIENTRANS N 'LIE_HELP
                   (LIST 'PLUS (LIST 'TIMES 2 'LIE_KK*) 1)))))
          (COND
           ((EVALEQUAL (AEVAL 'LIE_HELP) 1)
            (SETK 'LIE_LIST
                  (AEVAL
                   (LIST 'LIST (LIST 'LIE_ALGEBRA 2)
                         (LIST 'COMMUTATIVE (LIST 'DIFFERENCE N 2))))))
           (T
            (SETK 'LIE_LIST
                  (AEVAL
                   (LIST 'LIST
                         (LIST 'HEISENBERG
                               (LIST 'PLUS (LIST 'TIMES 2 'LIE_KK*) 1))
                         (LIST 'COMMUTATIVE
                               (LIST 'DIFFERENCE
                                     (LIST 'DIFFERENCE N
                                           (LIST 'TIMES 2 'LIE_KK*))
                                     1)))))))))
        (T
         (PROGN
          (AEVAL
           (CLEAR
            (LIST 'LIE_DIM 'LIE_HELP 'LIE_P 'LIE_Q 'LIE_TT 'LIE_S 'LIE_KK*
                  'LIE_JTEST 'LIE_CC)))
          (AEVAL (REDERR "dimension of derived algebra out of range")))))
       (AEVAL
        (CLEAR
         (LIST 'LIE_DIM 'LIE_HELP 'LIE_P 'LIE_Q 'LIE_TT 'LIE_S 'LIE_KK*
               'LIE_CONTROL))))
      (AEVAL (CLEAR (LIST 'LIE_JTEST 'LIE_CC)))
      (RETURN (AEVAL 'LIE_LIST)))) 
(PUT 'LIENINSTRUC 'NUMBER-OF-ARGS 1) 
(FLAG '(LIENINSTRUC) 'OPFN) 
(PUT 'LIENINSTRUC 'DEFINED-ON-LINE '69) 
(PUT 'LIENINSTRUC 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENINSTRUC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIENINSTRUC (N)
    (PROG ()
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J (PLUS I 1))
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
            (PROGN
             (SETK (LIST 'LIE_CC I J K) (AEVAL* (LIST 'LIENSTRUCIN I J K)))
             (SETK (LIST 'LIE_CC J I K)
                   (AEVAL* (LIST 'MINUS (LIST 'LIENSTRUCIN I J K)))))
            (SETQ K
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     K))
            (GO LAB))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB)))) 
(PUT 'LIENJACTEST 'NUMBER-OF-ARGS 1) 
(FLAG '(LIENJACTEST) 'OPFN) 
(PUT 'LIENJACTEST 'DEFINED-ON-LINE '76) 
(PUT 'LIENJACTEST 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENJACTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIENJACTEST (N)
    (PROG ()
      (SETK 'LIE_JTEST (AEVAL 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 2)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J (PLUS I 1))
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) J))
            (RETURN NIL)))
          (PROG (K)
            (SETQ K (PLUS J 1))
           LAB
            (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
            (PROG (L)
              (SETQ L 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) L)) (RETURN NIL)))
              (COND
               ((EVALNEQ
                 (PROG (R FORALL-RESULT)
                   (SETQ R 1)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) R))
                     (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'LIE_CC J K R)
                                               (LIST 'LIE_CC I R L))
                                         (LIST 'TIMES (LIST 'LIE_CC I J R)
                                               (LIST 'LIE_CC K R L))
                                         (LIST 'TIMES (LIST 'LIE_CC K I R)
                                               (LIST 'LIE_CC J R L))))
                                  FORALL-RESULT)))
                   (SETQ R
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            R))
                   (GO LAB1))
                 0)
                (PROGN
                 (SETK 'LIE_JTEST (AEVAL* 1))
                 (SETQ I (AEVAL* (LIST 'DIFFERENCE N 1)))
                 (SETQ J (AEVAL* N))
                 (SETQ K (AEVAL* (LIST 'PLUS N 1)))
                 (SETQ L (AEVAL* (LIST 'PLUS N 1))))))
              (SETQ L
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       L))
              (GO LAB))
            (SETQ K
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     K))
            (GO LAB))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB)))) 
(PUT 'LIENDIMCOM 'NUMBER-OF-ARGS 1) 
(FLAG '(LIENDIMCOM) 'OPFN) 
(PUT 'LIENDIMCOM 'DEFINED-ON-LINE '89) 
(PUT 'LIENDIMCOM 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENDIMCOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIENDIMCOM (N)
    (PROG (R HE)
      (SETQ R 0)
      (SETK 'LIE_DIM (AEVAL 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J I)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
            (COND
             ((EVALNEQ (AEVAL* (LIST 'LIE_CC I J K)) 0)
              (PROGN
               (SETK 'LIE_DIM (AEVAL* 1))
               (SETK 'LIE_P (AEVAL* I))
               (SETK 'LIE_Q (AEVAL* J))
               (SETQ R (AEVAL* K))
               (SETQ I (AEVAL* N))
               (SETQ J (SETQ K (AEVAL* (LIST 'PLUS N 1)))))))
            (SETQ K
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     K))
            (GO LAB))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((EVALNEQ (AEVAL 'LIE_DIM) 0)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
             (RETURN NIL)))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
             (PROGN
              (SETQ HE
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'LIE_CC I J R)
                             (LIST 'LIE_CC 'LIE_P 'LIE_Q R))))
              (PROG (K)
                (SETQ K 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
                (COND
                 ((EVALNEQ (AEVAL* (LIST 'LIE_CC I J K))
                           (AEVAL*
                            (LIST 'TIMES HE (LIST 'LIE_CC 'LIE_P 'LIE_Q K))))
                  (PROGN
                   (SETK 'LIE_DIM (AEVAL* 2))
                   (SETQ I (AEVAL* N))
                   (SETQ J (AEVAL* (LIST 'PLUS N 1)))
                   (SETQ K (AEVAL* (LIST 'PLUS N 1))))))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (COND
          ((EVALEQUAL (AEVAL 'LIE_DIM) 1)
           (PROGN
            (SETK 'LIE_HELP (AEVAL 0))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
              (PROG (J)
                (SETQ J 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
                (COND
                 ((EVALNEQ
                   (PROG (K FORALL-RESULT)
                     (SETQ K 1)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND
                      ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                       (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (AEVAL*
                              (LIST 'PLUS
                                    (AEVAL*
                                     (LIST 'TIMES
                                           (LIST 'LIE_CC 'LIE_P 'LIE_Q K)
                                           (LIST 'LIE_CC K I J)))
                                    FORALL-RESULT)))
                     (SETQ K
                             ((LAMBDA (FORALL-RESULT)
                                (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                              K))
                     (GO LAB1))
                   0)
                  (PROGN
                   (SETK 'LIE_HELP (AEVAL* 1))
                   (SETK 'LIE_S (AEVAL* I))
                   (SETQ R (AEVAL* J))
                   (SETQ I (SETQ J (AEVAL* (LIST 'PLUS N 1)))))))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
              (SETK (LIST 'LIENTRANS 1 I)
                    (AEVAL* (LIST 'LIE_CC 'LIE_P 'LIE_Q I)))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB))
            (COND
             ((EVALEQUAL (AEVAL 'LIE_HELP) 0)
              (PROGN
               (SETK (LIST 'LIENTRANS 2 'LIE_P)
                     (SETK (LIST 'LIENTRANS 3 'LIE_Q) (AEVAL 1)))
               (SETK 'LIE_KK* (AEVAL 1))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((AND (EVALNEQ (AEVAL* (LIST 'LIE_CC 'LIE_P 'LIE_Q I)) 0)
                         (EVALNEQ I (AEVAL* 'LIE_P))
                         (EVALNEQ I (AEVAL* 'LIE_Q)))
                    (PROGN
                     (SETK 'LIE_TT (AEVAL* I))
                     (SETQ I (AEVAL* (LIST 'PLUS N 1)))))))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB))))
             (T
              (PROGN
               (SETK (LIST 'LIENTRANS 2 'LIE_S)
                     (AEVAL
                      (LIST 'QUOTIENT (LIST 'LIE_CC 'LIE_P 'LIE_Q R)
                            (PROG (K FORALL-RESULT)
                              (SETQ K 1)
                              (SETQ FORALL-RESULT 0)
                             LAB1
                              (COND
                               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                                (RETURN FORALL-RESULT)))
                              (SETQ FORALL-RESULT
                                      (AEVAL*
                                       (LIST 'PLUS
                                             (AEVAL*
                                              (LIST 'TIMES
                                                    (LIST 'LIE_CC 'LIE_P 'LIE_Q
                                                          K)
                                                    (LIST 'LIE_CC K 'LIE_S R)))
                                             FORALL-RESULT)))
                              (SETQ K
                                      ((LAMBDA (FORALL-RESULT)
                                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                       K))
                              (GO LAB1)))))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((AND (EVALNEQ (AEVAL* (LIST 'LIE_CC 'LIE_P 'LIE_Q I)) 0)
                         (EVALNEQ I (AEVAL* 'LIE_S)))
                    (PROGN
                     (SETK 'LIE_TT (AEVAL* I))
                     (SETQ I (AEVAL* (LIST 'PLUS N 1)))))))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB))))))))))))) 
(PUT 'LIENCENTINCOM 'NUMBER-OF-ARGS 4) 
(FLAG '(LIENCENTINCOM) 'OPFN) 
(PUT 'LIENCENTINCOM 'DEFINED-ON-LINE '127) 
(PUT 'LIENCENTINCOM 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENCENTINCOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIENCENTINCOM (N TT P Q)
    (PROG (CON1 CON2)
      (SETQ CON1 0)
      (SETQ CON2 0)
      (AEVAL (MATRIX (LIST (LIST 'LIE_LAMB N N))))
      (SETK 'LIE_CONTROL (AEVAL 0))
      (SETQ CON1 (SETQ CON2 (AEVAL 0)))
      (PROG (I)
        (SETQ I 4)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (COND
         ((AND (EVALNEQ I (AEVAL* TT)) (EVALNEQ I (AEVAL* P))
               (EVALNEQ I (AEVAL* Q)))
          (SETK (LIST 'LIENTRANS I I) (AEVAL* 1)))
         ((AND (EVALNEQ (AEVAL* TT) 1) (EVALNEQ (AEVAL* P) 1)
               (EVALNEQ (AEVAL* Q) 1) (NEQ CON1 1))
          (PROGN
           (SETK (LIST 'LIENTRANS I 1) (AEVAL* 1))
           (SETQ CON1 (AEVAL* 1))))
         ((AND (EVALNEQ (AEVAL* TT) 2) (EVALNEQ (AEVAL* P) 2)
               (EVALNEQ (AEVAL* Q) 2) (NEQ CON2 1))
          (PROGN
           (SETK (LIST 'LIENTRANS I 2) (AEVAL* 1))
           (SETQ CON2 (AEVAL* 1))))
         (T (SETK (LIST 'LIENTRANS I 3) (AEVAL* 1))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((EVALGREATERP (AEVAL N) 3)
        (PROGN
         (AEVAL (LIST 'LIENNEWSTRUC N 2 TT))
         (COND
          ((EVALGREATERP (AEVAL N) 4)
           (PROG (I)
             (SETQ I 4)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'TIMES 2 (LIST 'DIFFERENCE (AEVAL* N) I)))
               (RETURN NIL)))
             (COND
              ((EVALEQUAL (PLUS I 1) (AEVAL* N))
               (PROGN
                (AEVAL* (LIST 'LIENFINDPAIR N I))
                (COND
                 ((EVALEQUAL (AEVAL* 'LIE_CONTROL) 1)
                  (SETK 'LIE_KK* (AEVAL* (LIST 'PLUS 'LIE_KK* 1)))))))
              ((EVALLESSP (PLUS I 1) (AEVAL* N))
               (PROGN
                (AEVAL* (LIST 'LIENFINDPAIR N I))
                (COND
                 ((EVALEQUAL (AEVAL* 'LIE_CONTROL) 1)
                  (PROGN
                   (AEVAL* (LIST 'LIENNEWSTRUC N I TT))
                   (SETK 'LIE_KK* (AEVAL* (LIST 'PLUS 'LIE_KK* 1)))))
                 (T (SETQ I (AEVAL* (LIST 'PLUS N 1))))))))
             (SETQ I
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 2)))
                      I))
             (GO LAB))))))))) 
(PUT 'LIENFINDPAIR 'NUMBER-OF-ARGS 2) 
(FLAG '(LIENFINDPAIR) 'OPFN) 
(PUT 'LIENFINDPAIR 'DEFINED-ON-LINE '148) 
(PUT 'LIENFINDPAIR 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENFINDPAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIENFINDPAIR (N M)
    (PROG (HE)
      (AEVAL (MATRIX (LIST (LIST 'LIE_A N N))))
      (SETK 'LIE_CONTROL (AEVAL 0))
      (PROG (I)
        (SETQ I (AEVAL* M))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J (AEVAL* (LIST 'PLUS I 1)))
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (PROGN
           (COND
            ((EVALNEQ (AEVAL* (LIST 'LIE_LAMB I J)) 0)
             (PROGN
              (SETK 'LIE_CONTROL (AEVAL* 1))
              (SETK (LIST 'LIE_A I M)
                    (SETK (LIST 'LIE_A (LIST 'PLUS M 1) J)
                          (SETK (LIST 'LIE_A J (LIST 'PLUS M 1)) (AEVAL* 1))))
              (SETK (LIST 'LIE_A M I)
                    (AEVAL* (LIST 'QUOTIENT 1 (LIST 'LIE_LAMB I J))))
              (PROG (K)
                (SETQ K 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
                (COND
                 ((AND (EVALNEQ K (AEVAL* I)) (EVALNEQ K (AEVAL* J))
                       (EVALNEQ K (AEVAL* M))
                       (EVALNEQ K (AEVAL* (LIST 'PLUS M 1))))
                  (SETK (LIST 'LIE_A K K) (AEVAL* 1))))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB))
              (SETK 'LIENTRANS (AEVAL* (LIST 'TIMES 'LIE_A 'LIENTRANS)))
              (SETQ I (AEVAL* N))
              (SETQ J (AEVAL* (LIST 'PLUS N 1)))))))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'LIE_A))))) 
(PUT 'LIENNEWSTRUC 'NUMBER-OF-ARGS 3) 
(FLAG '(LIENNEWSTRUC) 'OPFN) 
(PUT 'LIENNEWSTRUC 'DEFINED-ON-LINE '164) 
(PUT 'LIENNEWSTRUC 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENNEWSTRUC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIENNEWSTRUC (N M TT)
    (PROG ()
      (AEVAL (MATRIX (LIST (LIST 'LIE_A N N))))
      (SETK 'LIE_A (AEVAL (LIST 'EXPT 'LIE_A 0)))
      (PROG (I)
        (SETQ I (AEVAL* M))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J (AEVAL* (LIST 'PLUS I 1)))
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (SETK (LIST 'LIE_LAMB I J)
                (AEVAL*
                 (LIST 'QUOTIENT
                       (PROG (K FORALL-RESULT)
                         (SETQ K 1)
                         (SETQ FORALL-RESULT 0)
                        LAB1
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                           (RETURN FORALL-RESULT)))
                         (SETQ FORALL-RESULT
                                 (AEVAL*
                                  (LIST 'PLUS
                                        (PROG (L FORALL-RESULT)
                                          (SETQ L 1)
                                          (SETQ FORALL-RESULT 0)
                                         LAB1
                                          (COND
                                           ((|AMINUSP:|
                                             (LIST 'DIFFERENCE (AEVAL* N) L))
                                            (RETURN FORALL-RESULT)))
                                          (SETQ FORALL-RESULT
                                                  (AEVAL*
                                                   (LIST 'PLUS
                                                         (AEVAL*
                                                          (LIST 'TIMES
                                                                (LIST
                                                                 'LIENTRANS I
                                                                 K)
                                                                (LIST
                                                                 'LIENTRANS J
                                                                 L)
                                                                (LIST 'LIE_CC K
                                                                      L TT)))
                                                         FORALL-RESULT)))
                                          (SETQ L
                                                  ((LAMBDA (FORALL-RESULT)
                                                     (AEVAL*
                                                      (LIST 'PLUS FORALL-RESULT
                                                            1)))
                                                   L))
                                          (GO LAB1))
                                        FORALL-RESULT)))
                         (SETQ K
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  K))
                         (GO LAB1))
                       (LIST 'LIENTRANS 1 TT))))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (PROG (I)
        (SETQ I (AEVAL* (LIST 'PLUS M 2)))
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'LIE_A I (LIST 'PLUS M 1))
               (AEVAL* (LIST 'MINUS (LIST 'LIE_LAMB M I))))
         (SETK (LIST 'LIE_A I M) (AEVAL* (LIST 'LIE_LAMB (LIST 'PLUS M 1) I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'LIENTRANS (AEVAL (LIST 'TIMES 'LIE_A 'LIENTRANS)))
      (PROG (I)
        (SETQ I (AEVAL* (LIST 'PLUS M 2)))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J (AEVAL* (LIST 'PLUS I 1)))
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (SETK (LIST 'LIE_LAMB I J)
                (AEVAL*
                 (LIST 'QUOTIENT
                       (PROG (K FORALL-RESULT)
                         (SETQ K 1)
                         (SETQ FORALL-RESULT 0)
                        LAB1
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                           (RETURN FORALL-RESULT)))
                         (SETQ FORALL-RESULT
                                 (AEVAL*
                                  (LIST 'PLUS
                                        (PROG (L FORALL-RESULT)
                                          (SETQ L 1)
                                          (SETQ FORALL-RESULT 0)
                                         LAB1
                                          (COND
                                           ((|AMINUSP:|
                                             (LIST 'DIFFERENCE (AEVAL* N) L))
                                            (RETURN FORALL-RESULT)))
                                          (SETQ FORALL-RESULT
                                                  (AEVAL*
                                                   (LIST 'PLUS
                                                         (AEVAL*
                                                          (LIST 'TIMES
                                                                (LIST
                                                                 'LIENTRANS I
                                                                 K)
                                                                (LIST
                                                                 'LIENTRANS J
                                                                 L)
                                                                (LIST 'LIE_CC K
                                                                      L TT)))
                                                         FORALL-RESULT)))
                                          (SETQ L
                                                  ((LAMBDA (FORALL-RESULT)
                                                     (AEVAL*
                                                      (LIST 'PLUS FORALL-RESULT
                                                            1)))
                                                   L))
                                          (GO LAB1))
                                        FORALL-RESULT)))
                         (SETQ K
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  K))
                         (GO LAB1))
                       (LIST 'LIENTRANS 1 TT))))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'LIE_A))))) 
(PUT 'LIENCENTOUTCOM 'NUMBER-OF-ARGS 3) 
(FLAG '(LIENCENTOUTCOM) 'OPFN) 
(PUT 'LIENCENTOUTCOM 'DEFINED-ON-LINE '181) 
(PUT 'LIENCENTOUTCOM 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENCENTOUTCOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIENCENTOUTCOM (N TT S)
    (PROG (PP QQ)
      (SETQ PP 0)
      (SETQ QQ 0)
      (AEVAL (MATRIX (LIST (LIST 'LIE_LAMB 2 N) (LIST 'LIE_A N N))))
      (PROG (I)
        (SETQ I 3)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'LIENTRANS I I) (AEVAL* 1))
         (SETK (LIST 'LIE_LAMB 1 I)
               (AEVAL*
                (LIST 'QUOTIENT
                      (PROG (J FORALL-RESULT)
                        (SETQ J 1)
                        (SETQ FORALL-RESULT 0)
                       LAB1
                        (COND
                         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                          (RETURN FORALL-RESULT)))
                        (SETQ FORALL-RESULT
                                (AEVAL*
                                 (LIST 'PLUS
                                       (AEVAL*
                                        (LIST 'TIMES (LIST 'LIENTRANS 1 J)
                                              (LIST 'LIE_CC J I TT)))
                                       FORALL-RESULT)))
                        (SETQ J
                                ((LAMBDA (FORALL-RESULT)
                                   (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                 J))
                        (GO LAB1))
                      (LIST 'LIENTRANS 1 TT))))
         (SETK (LIST 'LIE_LAMB 2 I)
               (AEVAL*
                (LIST 'TIMES (LIST 'LIE_CC S I TT)
                      (LIST 'QUOTIENT (LIST 'LIENTRANS 2 S)
                            (LIST 'LIENTRANS 1 TT))))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((AND (EVALGREATERP (AEVAL TT) 2) (EVALGREATERP (AEVAL S) 2))
        (PROGN
         (SETK (LIST 'LIENTRANS TT TT) (SETK (LIST 'LIENTRANS S S) (AEVAL 0)))
         (SETK (LIST 'LIENTRANS TT 1) (SETK (LIST 'LIENTRANS S 2) (AEVAL 1)))
         (SETK (LIST 'LIE_LAMB 1 TT)
               (PROG (J FORALL-RESULT)
                 (SETQ J 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (AEVAL*
                                 (LIST 'TIMES (LIST 'LIENTRANS 1 J)
                                       (LIST 'QUOTIENT (LIST 'LIE_CC J 1 TT)
                                             (LIST 'LIENTRANS 1 TT))))
                                FORALL-RESULT)))
                 (SETQ J
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          J))
                 (GO LAB1)))
         (SETK (LIST 'LIE_LAMB 1 S)
               (PROG (J FORALL-RESULT)
                 (SETQ J 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (AEVAL*
                                 (LIST 'TIMES (LIST 'LIENTRANS 1 J)
                                       (LIST 'QUOTIENT (LIST 'LIE_CC J 2 TT)
                                             (LIST 'LIENTRANS 1 TT))))
                                FORALL-RESULT)))
                 (SETQ J
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          J))
                 (GO LAB1)))
         (SETK (LIST 'LIE_LAMB 2 TT)
               (AEVAL
                (LIST 'TIMES (LIST 'LIE_CC S 1 TT)
                      (LIST 'QUOTIENT (LIST 'LIENTRANS 2 S)
                            (LIST 'LIENTRANS 1 TT)))))
         (SETK (LIST 'LIE_LAMB 2 S)
               (AEVAL
                (LIST 'TIMES (LIST 'LIE_CC S 2 TT)
                      (LIST 'QUOTIENT (LIST 'LIENTRANS 2 S)
                            (LIST 'LIENTRANS 1 TT)))))))
       ((OR (EVALGREATERP (AEVAL TT) 2) (EVALGREATERP (AEVAL S) 2))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL TT) 2)
           (PROGN
            (SETQ PP (AEVAL (LIST 'DIFFERENCE 3 S)))
            (SETQ QQ (AEVAL TT))))
          (T
           (PROGN
            (SETQ PP (AEVAL (LIST 'DIFFERENCE 3 TT)))
            (SETQ QQ (AEVAL S)))))
         (SETK (LIST 'LIENTRANS QQ QQ) (AEVAL 0))
         (SETK (LIST 'LIENTRANS QQ PP) (AEVAL 1))
         (SETK (LIST 'LIE_LAMB 1 QQ)
               (AEVAL
                (LIST 'QUOTIENT
                      (PROG (J FORALL-RESULT)
                        (SETQ J 1)
                        (SETQ FORALL-RESULT 0)
                       LAB1
                        (COND
                         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                          (RETURN FORALL-RESULT)))
                        (SETQ FORALL-RESULT
                                (AEVAL*
                                 (LIST 'PLUS
                                       (AEVAL*
                                        (LIST 'TIMES (LIST 'LIENTRANS 1 J)
                                              (LIST 'LIE_CC J PP TT)))
                                       FORALL-RESULT)))
                        (SETQ J
                                ((LAMBDA (FORALL-RESULT)
                                   (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                 J))
                        (GO LAB1))
                      (LIST 'LIENTRANS 1 TT))))
         (SETK (LIST 'LIE_LAMB 2 QQ)
               (AEVAL
                (LIST 'TIMES (LIST 'LIE_CC S PP TT)
                      (LIST 'QUOTIENT (LIST 'LIENTRANS 2 S)
                            (LIST 'LIENTRANS 1 TT))))))))
      (SETK 'LIE_A (AEVAL (LIST 'EXPT 'LIE_A 0)))
      (PROG (I)
        (SETQ I 3)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'LIE_A I 2) (AEVAL* (LIST 'MINUS (LIST 'LIE_LAMB 1 I))))
         (SETK (LIST 'LIE_A I 1) (AEVAL* (LIST 'LIE_LAMB 2 I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'LIENTRANS (AEVAL (LIST 'TIMES 'LIE_A 'LIENTRANS)))
      (AEVAL (CLEAR (LIST 'LIE_LAMB 'LIE_A))))) 
(PUT 'LIENOUTFORM 'NUMBER-OF-ARGS 4) 
(FLAG '(LIENOUTFORM) 'OPFN) 
(PUT 'LIENOUTFORM 'DEFINED-ON-LINE '209) 
(PUT 'LIENOUTFORM 'DEFINED-IN-FILE 'MISC/LIENDMC1.RED) 
(PUT 'LIENOUTFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIENOUTFORM (AT N LHELP KK)
    (PROG ()
      (AEVAL (OPERATOR (LIST 'Y)))
      (SETK 'LIE_A (AEVAL AT))
      (COND
       ((EVALEQUAL (AEVAL LHELP) 1)
        (PROGN
         (ASSGNPRI
          (AEVAL
           "Your Lie algebra is the direct sum of the Lie algebra L(2) and")
          NIL 'ONLY)
         (PROGN
          (ASSGNPRI (AEVAL "the ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'DIFFERENCE N 2)) NIL NIL)
          (ASSGNPRI
           (AEVAL "-dimensional commutative Lie algebra, where L(2) is") NIL
           'LAST))
         (ASSGNPRI
          (AEVAL
           "2-dimensional and there exists a basis {X(1),X(2)} in L(2) with")
          NIL 'ONLY)
         (ASSGNPRI (AEVAL "[X(1),X(2)]=X(1).") NIL 'ONLY)))
       (T
        (PROGN
         (PROGN
          (ASSGNPRI
           (AEVAL "Your Lie algebra is the direct sum of the Lie algebra H(")
           NIL 'FIRST)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL ")") NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "and the ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'DIFFERENCE N KK)) NIL NIL)
          (ASSGNPRI (AEVAL "-dimensional commutative Lie algebra, where") NIL
                    'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "H(") NIL 'FIRST)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL ") is ") NIL NIL)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL "-dimensional and there exists a basis") NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "{X(1),...,X(") NIL 'FIRST)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL ")} in H(") NIL NIL)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL ") with:") NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "[X(2),X(3)]=[X(2*i),X(2*i+1)]=...=[X(") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'DIFFERENCE KK 1)) NIL NIL)
          (ASSGNPRI (AEVAL "),X(") NIL NIL)
          (ASSGNPRI (AEVAL KK) NIL NIL)
          (ASSGNPRI (AEVAL ")]=X(1)") NIL 'LAST)))))
      (ASSGNPRI (AEVAL "The transformation into this form is:") NIL 'ONLY)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (PROGN
         (ASSGNPRI (AEVAL* "X(") NIL 'FIRST)
         (ASSGNPRI I NIL NIL)
         (ASSGNPRI (AEVAL* "):=") NIL NIL)
         (ASSGNPRI
          (PROG (J FORALL-RESULT)
            (SETQ J 1)
            (SETQ FORALL-RESULT 0)
           LAB1
            (COND
             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
              (RETURN FORALL-RESULT)))
            (SETQ FORALL-RESULT
                    (AEVAL*
                     (LIST 'PLUS
                           (AEVAL* (LIST 'TIMES (LIST 'LIE_A I J) (LIST 'Y J)))
                           FORALL-RESULT)))
            (SETQ J
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     J))
            (GO LAB1))
          NIL 'LAST))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'Y 'LIE_A))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ARINV)) 
(FLUID '(DMODE*)) 
(GLOBAL '(ARBASE* CURDEFPOL*)) 
(PUT 'ARQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'ARQUOT 'DEFINED-ON-LINE '34) 
(PUT 'ARQUOT 'DEFINED-IN-FILE 'ARNUM/ARINV.RED) 
(PUT 'ARQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ARQUOT (U V)
    (PROG (MV R SGN X Y Z W DMODE* N)
      (SETQ N 0)
      (SETQ MV (CAAAR CURDEFPOL*))
      (SETQ X U)
      (SETQ W
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (CDR ARBASE*))
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (SETQ X
                                            (REDUCEPOWERS
                                             ((LAMBDA (G539)
                                                (COND
                                                 (*PHYSOP-LOADED
                                                  (PHYSOP-MULTF G539 X))
                                                 (T (POLY-MULTF G539 X))))
                                              (LIST (CONS (CONS MV 1) 1))))))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (SETQ X
                                    (REDUCEPOWERS
                                     ((LAMBDA (G539)
                                        (COND
                                         (*PHYSOP-LOADED (PHYSOP-MULTF G539 X))
                                         (T (POLY-MULTF G539 X))))
                                      (LIST (CONS (CONS MV 1) 1))))))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X NIL)
      (SETQ W (CONS (NEGF V) (CONS U W)))
      (PROG (J)
        (SETQ J (DIFFERENCE (CDAAR CURDEFPOL*) 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 J))) (RETURN NIL)))
        (PROGN
         (SETQ Y NIL)
         (SETQ Z 1)
         (SETQ N (MINUS 2))
         (SETQ W
                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                   (SETQ K W)
                   (COND ((NULL K) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (COND
                                        ((AND (EQUAL (DEGR K MV) J) K)
                                         (PROGN
                                          (SETQ Y
                                                  (CONS
                                                   (CONS
                                                    (LIST (SETQ N (PLUS N 1)))
                                                    (TIMES
                                                     (SETQ R
                                                             (COND
                                                              ((OR (ATOM K)
                                                                   (ATOM
                                                                    (CAR K)))
                                                               K)
                                                              (T (CDAR K))))))
                                                   Y))
                                          (COND
                                           ((EQCAR R '|:RN:|)
                                            (SETQ Z (ILCM Z (CDDR R)))))
                                          (COND
                                           ((NULL (OR (ATOM K) (ATOM (CAR K))))
                                            (CDR K)))))
                                        (T (PROGN (SETQ N (PLUS N 1)) K))))
                                     (CAR K))
                                    NIL)))
                  LOOPLABEL
                   (SETQ K (CDR K))
                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (K)
                               (COND
                                ((AND (EQUAL (DEGR K MV) J) K)
                                 (PROGN
                                  (SETQ Y
                                          (CONS
                                           (CONS (LIST (SETQ N (PLUS N 1)))
                                                 (TIMES
                                                  (SETQ R
                                                          (COND
                                                           ((OR (ATOM K)
                                                                (ATOM (CAR K)))
                                                            K)
                                                           (T (CDAR K))))))
                                           Y))
                                  (COND
                                   ((EQCAR R '|:RN:|)
                                    (SETQ Z (ILCM Z (CDDR R)))))
                                  (COND
                                   ((NULL (OR (ATOM K) (ATOM (CAR K))))
                                    (CDR K)))))
                                (T (PROGN (SETQ N (PLUS N 1)) K))))
                             (CAR K))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((NEQ Z 1)
           (SETQ Y
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J Y)
                     (COND ((NULL J) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (CONS (CAAR J)
                                            (COND
                                             ((EQCAR (CDAR J) '|:RN:|)
                                              (TIMES (CADR (CDAR J))
                                                     (QUOTIENT Z
                                                               (CDDR
                                                                (CDAR J)))))
                                             (T (TIMES (CDAR J) Z))))
                                      NIL)))
                    LOOPLABEL
                     (SETQ J (CDR J))
                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              (CONS (CAAR J)
                                    (COND
                                     ((EQCAR (CDAR J) '|:RN:|)
                                      (TIMES (CADR (CDAR J))
                                             (QUOTIENT Z (CDDR (CDAR J)))))
                                     (T (TIMES (CDAR J) Z))))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (COND ((NULL X) (SETQ X Y)) (T (SETQ X (|B:EXTMULT| Y X)))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (SETQ SGN (EVENP (LENGTH (CAAR X))))
      (SETQ Z NIL)
      (PROG (J)
        (SETQ J (CAAR X))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (SETQ Z
                   (ADDF
                    (COND
                     ((EQUAL J 0)
                      (ARNUM-MKGLSOL 0 X (SETQ SGN (NOT SGN)) (MINUS 1)))
                     (T
                      ((LAMBDA (G542)
                         ((LAMBDA (G544)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G542))
                                  (T (POLY-MULTF G544 G542))))
                          (LIST (CONS (CONS MV J) 1))))
                       (ARNUM-MKGLSOL J X (SETQ SGN (NOT SGN)) (MINUS 1)))))
                    Z)))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN Z))) 
(PUT 'ARQUOT1 'NUMBER-OF-ARGS 1) 
(PUT 'ARQUOT1 'DEFINED-ON-LINE '73) 
(PUT 'ARQUOT1 'DEFINED-IN-FILE 'ARNUM/ARINV.RED) 
(PUT 'ARQUOT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ARQUOT1 (U)
    (PROG (MV R SGN X Y Z W DMODE* N)
      (SETQ N 0)
      (SETQ MV (CAAAR CURDEFPOL*))
      (SETQ X U)
      (SETQ W
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (CDR ARBASE*))
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (SETQ X
                                            (REDUCEPOWERS
                                             ((LAMBDA (G543)
                                                (COND
                                                 (*PHYSOP-LOADED
                                                  (PHYSOP-MULTF G543 X))
                                                 (T (POLY-MULTF G543 X))))
                                              (LIST (CONS (CONS MV 1) 1))))))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (SETQ X
                                    (REDUCEPOWERS
                                     ((LAMBDA (G543)
                                        (COND
                                         (*PHYSOP-LOADED (PHYSOP-MULTF G543 X))
                                         (T (POLY-MULTF G543 X))))
                                      (LIST (CONS (CONS MV 1) 1))))))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X NIL)
      (SETQ W (MINUS (CONS 1 (CONS U W))))
      (PROG (J)
        (SETQ J (DIFFERENCE (CDAAR CURDEFPOL*) 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 J))) (RETURN NIL)))
        (PROGN
         (SETQ Y NIL)
         (SETQ Z 1)
         (SETQ N (MINUS 2))
         (SETQ W
                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                   (SETQ K W)
                   (COND ((NULL K) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (COND
                                        ((AND (EQUAL (DEGR K MV) J) K)
                                         (PROGN
                                          (SETQ Y
                                                  (CONS
                                                   (CONS
                                                    (LIST (SETQ N (PLUS N 1)))
                                                    (TIMES
                                                     (SETQ R
                                                             (COND
                                                              ((OR (ATOM K)
                                                                   (ATOM
                                                                    (CAR K)))
                                                               K)
                                                              (T (CDAR K))))))
                                                   Y))
                                          (COND
                                           ((EQCAR R '|:RN:|)
                                            (SETQ Z (ILCM Z (CDDR R)))))
                                          (COND
                                           ((NULL (OR (ATOM K) (ATOM (CAR K))))
                                            (CDR K)))))
                                        (T (PROGN (SETQ N (PLUS N 1)) K))))
                                     (CAR K))
                                    NIL)))
                  LOOPLABEL
                   (SETQ K (CDR K))
                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (K)
                               (COND
                                ((AND (EQUAL (DEGR K MV) J) K)
                                 (PROGN
                                  (SETQ Y
                                          (CONS
                                           (CONS (LIST (SETQ N (PLUS N 1)))
                                                 (TIMES
                                                  (SETQ R
                                                          (COND
                                                           ((OR (ATOM K)
                                                                (ATOM (CAR K)))
                                                            K)
                                                           (T (CDAR K))))))
                                           Y))
                                  (COND
                                   ((EQCAR R '|:RN:|)
                                    (SETQ Z (ILCM Z (CDDR R)))))
                                  (COND
                                   ((NULL (OR (ATOM K) (ATOM (CAR K))))
                                    (CDR K)))))
                                (T (PROGN (SETQ N (PLUS N 1)) K))))
                             (CAR K))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((NEQ Z 1)
           (SETQ Y
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J Y)
                     (COND ((NULL J) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (CONS (CAAR J)
                                            (COND
                                             ((EQCAR (CDAR J) '|:RN:|)
                                              (TIMES (CADR (CDAR J))
                                                     (QUOTIENT Z
                                                               (CDDR
                                                                (CDAR J)))))
                                             (T (TIMES (CDAR J) Z))))
                                      NIL)))
                    LOOPLABEL
                     (SETQ J (CDR J))
                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              (CONS (CAAR J)
                                    (COND
                                     ((EQCAR (CDAR J) '|:RN:|)
                                      (TIMES (CADR (CDAR J))
                                             (QUOTIENT Z (CDDR (CDAR J)))))
                                     (T (TIMES (CDAR J) Z))))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (COND ((NULL X) (SETQ X Y)) (T (SETQ X (|B:EXTMULT| Y X)))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (SETQ SGN (EVENP (LENGTH (CAAR X))))
      (SETQ Z NIL)
      (PROG (J)
        (SETQ J (CAAR X))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (SETQ Z
                   (ADDF
                    (COND
                     ((EQUAL J 0)
                      (ARNUM-MKGLSOL 0 X (SETQ SGN (NOT SGN)) (MINUS 1)))
                     (T
                      ((LAMBDA (G546)
                         ((LAMBDA (G544)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G546))
                                  (T (POLY-MULTF G544 G546))))
                          (LIST (CONS (CONS MV J) 1))))
                       (ARNUM-MKGLSOL J X (SETQ SGN (NOT SGN)) (MINUS 1)))))
                    Z)))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN Z))) 
(PUT 'ARINV 'NUMBER-OF-ARGS 1) 
(PUT 'ARINV 'DEFINED-ON-LINE '112) 
(PUT 'ARINV 'DEFINED-IN-FILE 'ARNUM/ARINV.RED) 
(PUT 'ARINV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ARINV (U)
    (PROG (MV SGN X Z V DMODE* K M N)
      (SETQ K 0)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ M (CDAAR CURDEFPOL*))
      (SETQ N (CDAAR U))
      (SETQ V CURDEFPOL*)
      (SETQ MV (CAAAR CURDEFPOL*))
      (SETQ X
              (CONS (CONS (LIST (DIFFERENCE M 1)) (CDAR U))
                    (CONS (CONS (LIST (MINUS 1)) (CDAR V)) NIL)))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE (PLUS N M) J)) (RETURN NIL)))
        (PROG (Y)
          (COND
           ((EQUAL J (PLUS N M))
            (SETQ Y
                    (CONS (CONS (LIST (DIFFERENCE (MINUS N) 1)) (MINUS 1))
                          NIL)))
           (T NIL))
          (COND
           ((EQUAL (PLUS N (DIFFERENCE (DIFFERENCE M J) (DEGR V MV)) 1) 0)
            (SETQ V (CDR V))))
          (COND
           ((EQUAL (PLUS N (DIFFERENCE (DIFFERENCE M J) (DEGR U MV)) 1) 0)
            (SETQ U (CDR U))))
          (SETQ Z U)
         A
          (COND
           ((AND Z
                 (LESSP
                  (SETQ K (PLUS (DIFFERENCE M J) (DIFFERENCE N (DEGR Z MV))))
                  M))
            (SETQ Y
                    (CONS
                     (CONS (LIST K)
                           (COND ((OR (ATOM Z) (ATOM (CAR Z))) Z)
                                 (T (CDAR Z))))
                     Y)))
           (T (GO B)))
          (COND ((NULL (OR (ATOM Z) (ATOM (CAR Z)))) (SETQ Z (CDR Z)))
                (T (SETQ Z NIL)))
          (GO A)
         B
          (SETQ Z V)
         C
          (COND
           ((AND Z
                 (LESSP (SETQ K (PLUS (MINUS J) (DIFFERENCE M (DEGR Z MV))))
                        0))
            (SETQ Y
                    (CONS
                     (CONS (LIST K)
                           (COND ((OR (ATOM Z) (ATOM (CAR Z))) Z)
                                 (T (CDAR Z))))
                     Y)))
           (T (GO D)))
          (COND ((NULL (OR (ATOM Z) (ATOM (CAR Z)))) (SETQ Z (CDR Z)))
                (T (SETQ Z NIL)))
          (GO C)
         D
          (SETQ X (|B:EXTMULT| Y X)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ SGN (EVENP (LENGTH (CAAR X))))
      (SETQ Z NIL)
      (PROG (J)
        (SETQ J (CAAR X))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((GREATERP J (MINUS 1))
             (SETQ Z
                     (ADDF
                      (COND
                       ((EQUAL J 0)
                        (ARNUM-MKGLSOL 0 X (SETQ SGN (NOT SGN))
                         (DIFFERENCE (MINUS N) 1)))
                       (T
                        ((LAMBDA (G548)
                           ((LAMBDA (G544)
                              (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G548))
                                    (T (POLY-MULTF G544 G548))))
                            (LIST (CONS (CONS MV J) 1))))
                         (ARNUM-MKGLSOL J X (SETQ SGN (NOT SGN))
                          (DIFFERENCE (MINUS N) 1)))))
                      Z)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN Z))) 
(PUT 'ARNUM-MKGLSOL 'NUMBER-OF-ARGS 4) 
(PUT 'ARNUM-MKGLSOL 'DEFINED-ON-LINE '154) 
(PUT 'ARNUM-MKGLSOL 'DEFINED-IN-FILE 'ARNUM/ARINV.RED) 
(PUT 'ARNUM-MKGLSOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ARNUM-MKGLSOL (U V SGN N)
    (PROG (S X Y DMODE*)
      (SETQ DMODE* '|:RN:|)
      (SETQ Y (CAAR V))
      (PROG (J)
        (SETQ J (CDR V))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND ((SETQ S (ARNUM-GLSOLTERM U Y J N)) (SETQ X S)))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (INT-EQUIV-CHK (MKRN (COND (SGN (MINUS X)) (T X)) (CDAR V)))))) 
(PUT 'ARNUM-GLSOLTERM 'NUMBER-OF-ARGS 4) 
(PUT 'ARNUM-GLSOLTERM 'DEFINED-ON-LINE '164) 
(PUT 'ARNUM-GLSOLTERM 'DEFINED-IN-FILE 'ARNUM/ARINV.RED) 
(PUT 'ARNUM-GLSOLTERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ARNUM-GLSOLTERM (U V W N)
    (PROG (X Y SGN)
      (SETQ X (CAAR W))
     A
      (COND ((NULL X) (RETURN (COND ((EQUAL (CAR Y) N) (CDAR W))))))
      (COND ((EQUAL (CAR X) U) (RETURN NIL))
            ((MEMBER (CAR X) V)
             (PROGN (SETQ X (CDR X)) (COND (Y (SETQ SGN (NOT SGN))))))
            (Y (RETURN NIL))
            (T (PROGN (SETQ Y (LIST (CAR X))) (SETQ X (CDR X)))))
      (GO A))) 
(ENDMODULE) 
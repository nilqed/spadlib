(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PLOTIMP3)) 
(PUT 'PLOTEVAL3IMPL 'NUMBER-OF-ARGS 3) 
(PUT 'PLOTEVAL3IMPL 'DEFINED-ON-LINE '32) 
(PUT 'PLOTEVAL3IMPL 'DEFINED-IN-FILE 'PLOT/PLOTIMP3.RED) 
(PUT 'PLOTEVAL3IMPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL3IMPL (X Y Z)
    (PROG (RX RY RZ F FCNS)
      (SETQ RX (PLOTRANGE X (REVAL1 (OR PLOT_XRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ RY (PLOTRANGE Y (REVAL1 (OR PLOT_YRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ RZ (PLOTRANGE Z (REVAL1 (OR PLOT_ZRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ FCNS (REVERSE PLOTFUNCTIONS*))
      (SETQ F
              (PROG (FCN FORALL-RESULT FORALL-ENDPTR)
                (SETQ FCN FCNS)
                (COND ((NULL FCN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FCN)
                                    (PLOTEVAL3IMPL1 (CDAR PLOTFUNCTIONS*) X
                                     (CAR RX) (CADR RX) Y (CAR RY) (CADR RY) Z
                                     (CAR RZ) (CADR RZ)))
                                  (CAR FCN))
                                 NIL)))
               LOOPLABEL
                (SETQ FCN (CDR FCN))
                (COND ((NULL FCN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FCN)
                            (PLOTEVAL3IMPL1 (CDAR PLOTFUNCTIONS*) X (CAR RX)
                             (CADR RX) Y (CAR RY) (CADR RY) Z (CAR RZ)
                             (CADR RZ)))
                          (CAR FCN))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (APPLY (GET PLOTDRIVER* 'PLOT-3EXP-REG) (LIST X Y Z F)))) 
(PUT 'PLOTEVAL3IMPL1 'NUMBER-OF-ARGS 10) 
(PUT 'PLOTEVAL3IMPL1 'DEFINED-ON-LINE '49) 
(PUT 'PLOTEVAL3IMPL1 'DEFINED-IN-FILE 'PLOT/PLOTIMP3.RED) 
(PUT 'PLOTEVAL3IMPL1 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE PLOTEVAL3IMPL1 (F X XLO XHI Y YLO YHI Z ZLO ZHI)
    (PROG (U DX DY DZ XX YY ZZ L FF PTS VAL W Q QQ PT FOUND DONE NX NY NZ)
      (SETQ NX 0)
      (SETQ NY 0)
      (SETQ NZ 0)
      (SETQ FF (RDWRAP F))
      (SETQ XLO (RDWRAP XLO))
      (SETQ XHI (RDWRAP XHI))
      (SETQ YLO (RDWRAP YLO))
      (SETQ YHI (RDWRAP YHI))
      (SETQ DX
              (QUOTIENT (FLOAT (DIFFERENCE XHI XLO))
                        (FLOAT (SETQ NX (PLOT-POINTS X)))))
      (SETQ DY
              (QUOTIENT (FLOAT (DIFFERENCE YHI YLO))
                        (FLOAT (SETQ NY (PLOT-POINTS Y)))))
      (SETQ DZ
              (QUOTIENT (FLOAT (DIFFERENCE ZHI ZLO))
                        (FLOAT (SETQ NZ (PLOT-POINTS Z)))))
      (SETQ PTS (MK-P-ARRAY3 NX NY NZ))
      (SETQ VAL (MK-P-ARRAY3 NX NY NZ))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
        (PROGN
         (SETQ XX (PLUS XLO (TIMES I DX)))
         (PROG (J)
           (SETQ J 0)
          LAB
           (COND ((MINUSP (DIFFERENCE NY J)) (RETURN NIL)))
           (PROGN
            (SETQ YY (PLUS YLO (TIMES J DY)))
            (PROG (K)
              (SETQ K 0)
             LAB
              (COND ((MINUSP (DIFFERENCE NZ K)) (RETURN NIL)))
              (PROGN
               (SETQ ZZ (PLUS ZLO (TIMES K DZ)))
               (P-PUT3 PTS I J K (LIST XX YY ZZ))
               (SETQ U
                       (PLOTEVALFORM FF F
                        (LIST (CONS X XX) (CONS Y YY) (CONS Z ZZ))))
               (COND ((EQCAR U 'OVERFLOW) (SETQ U 1.0)))
               (P-PUT3 VAL I J K U)
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ DONE T)
      (PROG ()
       WHILELABEL
        (COND ((NOT DONE) (RETURN NIL)))
        (PROGN
         (SETQ DONE NIL)
         (SETQ W
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 0)
                   (COND
                    ((MINUSP (DIFFERENCE (IDIFFERENCE NX 1) I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ J 0)
                                      (COND
                                       ((MINUSP
                                         (DIFFERENCE (IDIFFERENCE NY 1) J))
                                        (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       (PROGN
                                                        (SETQ Q NIL)
                                                        (SETQ FOUND NIL)
                                                        (SETQ PT
                                                                (P-GET3 PTS I J
                                                                 1))
                                                        (PROG (K)
                                                          (SETQ K NZ)
                                                         LAB
                                                          (COND
                                                           ((MINUSP
                                                             (TIMES (MINUS 1)
                                                                    (DIFFERENCE
                                                                     0 K)))
                                                            (RETURN NIL)))
                                                          (COND
                                                           ((NULL FOUND)
                                                            (PROGN
                                                             (COND
                                                              ((NULL Q)
                                                               (SETQ Q
                                                                       (P-GET3
                                                                        VAL I J
                                                                        K))))
                                                             (SETQ QQ
                                                                     (P-GET3
                                                                      VAL I J
                                                                      K))
                                                             (COND
                                                              ((AND Q QQ
                                                                    (LEQ
                                                                     (TIMES Q
                                                                            QQ)
                                                                     0.0))
                                                               (SETQ FOUND
                                                                       (COND
                                                                        ((EQUAL
                                                                          Q
                                                                          0.0)
                                                                         (CADDR
                                                                          (P-GET3
                                                                           PTS
                                                                           I J
                                                                           K)))
                                                                        (T
                                                                         (PLOTEVAL3IMPL3
                                                                          (CADDR
                                                                           (P-GET3
                                                                            PTS
                                                                            I J
                                                                            K))
                                                                          QQ
                                                                          (CADDR
                                                                           (P-GET3
                                                                            PTS
                                                                            I J
                                                                            (IPLUS2
                                                                             K
                                                                             1)))
                                                                          Q))))))
                                                             (COND
                                                              ((OR
                                                                (EQUAL Q 0.0)
                                                                (EQUAL QQ 0.0)
                                                                (NOT FOUND))
                                                               (P-PUT3 VAL I J
                                                                K NIL)))
                                                             (SETQ DONE
                                                                     (OR DONE
                                                                         FOUND))
                                                             (SETQ Q QQ)
                                                             NIL)))
                                                          (SETQ K
                                                                  (PLUS2 K
                                                                         (MINUS
                                                                          1)))
                                                          (GO LAB))
                                                        (LIST T T (CAR PT)
                                                              (CADR PT) FOUND))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ J (PLUS2 J 1))
                                      (COND
                                       ((MINUSP
                                         (DIFFERENCE (IDIFFERENCE NY 1) J))
                                        (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               (PROGN
                                                (SETQ Q NIL)
                                                (SETQ FOUND NIL)
                                                (SETQ PT (P-GET3 PTS I J 1))
                                                (PROG (K)
                                                  (SETQ K NZ)
                                                 LAB
                                                  (COND
                                                   ((MINUSP
                                                     (TIMES (MINUS 1)
                                                            (DIFFERENCE 0 K)))
                                                    (RETURN NIL)))
                                                  (COND
                                                   ((NULL FOUND)
                                                    (PROGN
                                                     (COND
                                                      ((NULL Q)
                                                       (SETQ Q
                                                               (P-GET3 VAL I J
                                                                K))))
                                                     (SETQ QQ
                                                             (P-GET3 VAL I J
                                                              K))
                                                     (COND
                                                      ((AND Q QQ
                                                            (LEQ (TIMES Q QQ)
                                                                 0.0))
                                                       (SETQ FOUND
                                                               (COND
                                                                ((EQUAL Q 0.0)
                                                                 (CADDR
                                                                  (P-GET3 PTS I
                                                                   J K)))
                                                                (T
                                                                 (PLOTEVAL3IMPL3
                                                                  (CADDR
                                                                   (P-GET3 PTS
                                                                    I J K))
                                                                  QQ
                                                                  (CADDR
                                                                   (P-GET3 PTS
                                                                    I J
                                                                    (IPLUS2 K
                                                                            1)))
                                                                  Q))))))
                                                     (COND
                                                      ((OR (EQUAL Q 0.0)
                                                           (EQUAL QQ 0.0)
                                                           (NOT FOUND))
                                                       (P-PUT3 VAL I J K NIL)))
                                                     (SETQ DONE
                                                             (OR DONE FOUND))
                                                     (SETQ Q QQ)
                                                     NIL)))
                                                  (SETQ K (PLUS2 K (MINUS 1)))
                                                  (GO LAB))
                                                (LIST T T (CAR PT) (CADR PT)
                                                      FOUND))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND
                    ((MINUSP (DIFFERENCE (IDIFFERENCE NX 1) I))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (PROG (J FORALL-RESULT FORALL-ENDPTR)
                              (SETQ J 0)
                              (COND
                               ((MINUSP (DIFFERENCE (IDIFFERENCE NY 1) J))
                                (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               (PROGN
                                                (SETQ Q NIL)
                                                (SETQ FOUND NIL)
                                                (SETQ PT (P-GET3 PTS I J 1))
                                                (PROG (K)
                                                  (SETQ K NZ)
                                                 LAB
                                                  (COND
                                                   ((MINUSP
                                                     (TIMES (MINUS 1)
                                                            (DIFFERENCE 0 K)))
                                                    (RETURN NIL)))
                                                  (COND
                                                   ((NULL FOUND)
                                                    (PROGN
                                                     (COND
                                                      ((NULL Q)
                                                       (SETQ Q
                                                               (P-GET3 VAL I J
                                                                K))))
                                                     (SETQ QQ
                                                             (P-GET3 VAL I J
                                                              K))
                                                     (COND
                                                      ((AND Q QQ
                                                            (LEQ (TIMES Q QQ)
                                                                 0.0))
                                                       (SETQ FOUND
                                                               (COND
                                                                ((EQUAL Q 0.0)
                                                                 (CADDR
                                                                  (P-GET3 PTS I
                                                                   J K)))
                                                                (T
                                                                 (PLOTEVAL3IMPL3
                                                                  (CADDR
                                                                   (P-GET3 PTS
                                                                    I J K))
                                                                  QQ
                                                                  (CADDR
                                                                   (P-GET3 PTS
                                                                    I J
                                                                    (IPLUS2 K
                                                                            1)))
                                                                  Q))))))
                                                     (COND
                                                      ((OR (EQUAL Q 0.0)
                                                           (EQUAL QQ 0.0)
                                                           (NOT FOUND))
                                                       (P-PUT3 VAL I J K NIL)))
                                                     (SETQ DONE
                                                             (OR DONE FOUND))
                                                     (SETQ Q QQ)
                                                     NIL)))
                                                  (SETQ K (PLUS2 K (MINUS 1)))
                                                  (GO LAB))
                                                (LIST T T (CAR PT) (CADR PT)
                                                      FOUND))
                                               NIL)))
                             LOOPLABEL
                              (SETQ J (PLUS2 J 1))
                              (COND
                               ((MINUSP (DIFFERENCE (IDIFFERENCE NY 1) J))
                                (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       (PROGN
                                        (SETQ Q NIL)
                                        (SETQ FOUND NIL)
                                        (SETQ PT (P-GET3 PTS I J 1))
                                        (PROG (K)
                                          (SETQ K NZ)
                                         LAB
                                          (COND
                                           ((MINUSP
                                             (TIMES (MINUS 1)
                                                    (DIFFERENCE 0 K)))
                                            (RETURN NIL)))
                                          (COND
                                           ((NULL FOUND)
                                            (PROGN
                                             (COND
                                              ((NULL Q)
                                               (SETQ Q (P-GET3 VAL I J K))))
                                             (SETQ QQ (P-GET3 VAL I J K))
                                             (COND
                                              ((AND Q QQ
                                                    (LEQ (TIMES Q QQ) 0.0))
                                               (SETQ FOUND
                                                       (COND
                                                        ((EQUAL Q 0.0)
                                                         (CADDR
                                                          (P-GET3 PTS I J K)))
                                                        (T
                                                         (PLOTEVAL3IMPL3
                                                          (CADDR
                                                           (P-GET3 PTS I J K))
                                                          QQ
                                                          (CADDR
                                                           (P-GET3 PTS I J
                                                            (IPLUS2 K 1)))
                                                          Q))))))
                                             (COND
                                              ((OR (EQUAL Q 0.0) (EQUAL QQ 0.0)
                                                   (NOT FOUND))
                                               (P-PUT3 VAL I J K NIL)))
                                             (SETQ DONE (OR DONE FOUND))
                                             (SETQ Q QQ)
                                             NIL)))
                                          (SETQ K (PLUS2 K (MINUS 1)))
                                          (GO LAB))
                                        (LIST T T (CAR PT) (CADR PT) FOUND))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND (DONE (SETQ L (CONS W L))))
         NIL)
        (GO WHILELABEL))
      (RETURN (PLOTEVAL3XY3 L)))) 
(PUT 'PLOTEVAL3IMPL3 'NUMBER-OF-ARGS 4) 
(PUT 'PLOTEVAL3IMPL3 'DEFINED-ON-LINE '106) 
(PUT 'PLOTEVAL3IMPL3 'DEFINED-IN-FILE 'PLOT/PLOTIMP3.RED) 
(PUT 'PLOTEVAL3IMPL3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL3IMPL3 (P1 F1 P2 F2)
    (PROGN
     NIL
     (QUOTIENT (DIFFERENCE (TIMES F1 P2) (TIMES F2 P1)) (DIFFERENCE F1 F2)))) 
(ENDMODULE) 
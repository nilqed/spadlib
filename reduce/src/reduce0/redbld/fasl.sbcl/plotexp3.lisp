(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PLOTEXP3)) 
(PUT 'PLOTEVAL3XY 'NUMBER-OF-ARGS 3) 
(PUT 'PLOTEVAL3XY 'DEFINED-ON-LINE '39) 
(PUT 'PLOTEVAL3XY 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL3XY (X Y Z)
    (PROG (RX RY RZ F FCNS)
      (SETQ RX (PLOTRANGE X (REVAL1 (OR PLOT_XRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ RY (PLOTRANGE Y (REVAL1 (OR PLOT_YRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ RZ (PLOTRANGE Z (REVAL1 (OR PLOT_ZRANGE NIL) T)))
      (SETQ FCNS (REVERSE PLOTFUNCTIONS*))
      (COND
       ((EQUAL Z 'IMPLICIT) (RETURN (PLOTEVAL2XYIMPL RX RY (CAR FCNS) X Y))))
      (SETQ F
              (PROG (FCN FORALL-RESULT FORALL-ENDPTR)
                (SETQ FCN FCNS)
                (COND ((NULL FCN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FCN)
                                    (COND ((EQCAR FCN 'POINTS) (CADDR FCN))
                                          (T
                                           (PLOTEVAL3XY1 (CDR FCN) Z
                                            (COND (RZ (CAR RZ)))
                                            (COND (RZ (CADR RZ))) X (CAR RX)
                                            (CADR RX) Y (CAR RY) (CADR RY)))))
                                  (CAR FCN))
                                 NIL)))
               LOOPLABEL
                (SETQ FCN (CDR FCN))
                (COND ((NULL FCN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FCN)
                            (COND ((EQCAR FCN 'POINTS) (CADDR FCN))
                                  (T
                                   (PLOTEVAL3XY1 (CDR FCN) Z
                                    (COND (RZ (CAR RZ))) (COND (RZ (CADR RZ)))
                                    X (CAR RX) (CADR RX) Y (CAR RY)
                                    (CADR RY)))))
                          (CAR FCN))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (APPLY (GET PLOTDRIVER* 'PLOT-3EXP-REG) (LIST X Y Z F)))) 
(PUT 'PLOTEVAL3XY1 'NUMBER-OF-ARGS 10) 
(PUT 'PLOTEVAL3XY1 'DEFINED-ON-LINE '58) 
(PUT 'PLOTEVAL3XY1 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XY1 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE PLOTEVAL3XY1 (F Z ZLO ZHI X XLO XHI Y YLO YHI)
    (PROG (L FF R W NX NY)
      (SETQ NX 0)
      (SETQ NY 0)
      (SETQ Z NIL)
      (SETQ FF (RDWRAP F))
      (SETQ XLO (RDWRAP XLO))
      (SETQ XHI (RDWRAP XHI))
      (SETQ YLO (RDWRAP YLO))
      (SETQ YHI (RDWRAP YHI))
      (SETQ NX (PLOT-POINTS X))
      (SETQ NY (PLOT-POINTS Y))
      (SETQ R (PLOTEVAL3XY1PTS F FF Z ZLO ZHI X XLO XHI NX Y YLO YHI NY))
      (SETQ L (CDR R))
      (SETQ W (CAR R))
      (SETQ R (LIST L))
      (PROG (Q)
        (SETQ Q W)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (SETQ R
                   (CONS
                    (CDR
                     (PLOTEVAL3XY1PTS F FF Z ZLO ZHI X (CAR Q) (CADR Q) 4 Y
                      (CADDR Q) (CADDDR Q) 4))
                    R)))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (RETURN (PLOTEVAL3XY3 R)))) 
(PUT 'PLOTEVAL3XY1PTS 'NUMBER-OF-ARGS 13) 
(PUT 'PLOTEVAL3XY1PTS 'DEFINED-ON-LINE '83) 
(PUT 'PLOTEVAL3XY1PTS 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XY1PTS 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PLOTEVAL3XY1PTS (F FF Z ZLO ZHI X XLO XHI NX Y YLO YHI NY)
    (PROG (U DX DY XX YY L W)
      (SETQ Z NIL)
      (SETQ DX (QUOTIENT (FLOAT (DIFFERENCE XHI XLO)) (FLOAT NX)))
      (SETQ DY (QUOTIENT (FLOAT (DIFFERENCE YHI YLO)) (FLOAT NY)))
      (SETQ L
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 0)
                (COND ((MINUSP (DIFFERENCE NX J)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ I 0)
                                    (COND
                                     ((MINUSP (DIFFERENCE NY I)) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     (PROGN
                                                      (SETQ XX
                                                              (PLUS XLO
                                                                    (TIMES I
                                                                           DX)))
                                                      (SETQ YY
                                                              (PLUS YLO
                                                                    (TIMES J
                                                                           DY)))
                                                      (SETQ U
                                                              (PLOTEVALFORM FF
                                                               F
                                                               (LIST
                                                                (CONS X XX)
                                                                (CONS Y YY))))
                                                      (COND
                                                       ((NULL U)
                                                        (SETQ U
                                                                (PLOTEVAL3XYSINGULAR
                                                                 FF F X XX DX Y
                                                                 YY DY ZHI
                                                                 ZLO))))
                                                      (COND
                                                       ((OR (NULL U)
                                                            (EQCAR U 'OVERFLOW)
                                                            (AND (NUMBERP U)
                                                                 (OR
                                                                  (AND ZHI
                                                                       (GREATERP
                                                                        U ZHI))
                                                                  (AND ZLO
                                                                       (LESSP U
                                                                              ZLO)))))
                                                        (PROGN
                                                         (SETQ U NIL)
                                                         (COND
                                                          ((AND (LESSP 0 J)
                                                                (LESSP J NX)
                                                                (LESSP 0 I)
                                                                (LESSP I NY))
                                                           (SETQ W
                                                                   (CONS
                                                                    (LIST
                                                                     (DIFFERENCE
                                                                      XX DX)
                                                                     (PLUS XX
                                                                           DX)
                                                                     (DIFFERENCE
                                                                      YY DY)
                                                                     (PLUS YY
                                                                           DY))
                                                                    W))))
                                                         NIL)))
                                                      (LIST T T XX YY U))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ I (PLUS2 I 1))
                                    (COND
                                     ((MINUSP (DIFFERENCE NY I))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             (PROGN
                                              (SETQ XX (PLUS XLO (TIMES I DX)))
                                              (SETQ YY (PLUS YLO (TIMES J DY)))
                                              (SETQ U
                                                      (PLOTEVALFORM FF F
                                                       (LIST (CONS X XX)
                                                             (CONS Y YY))))
                                              (COND
                                               ((NULL U)
                                                (SETQ U
                                                        (PLOTEVAL3XYSINGULAR FF
                                                                             F
                                                                             X
                                                                             XX
                                                                             DX
                                                                             Y
                                                                             YY
                                                                             DY
                                                                             ZHI
                                                                             ZLO))))
                                              (COND
                                               ((OR (NULL U)
                                                    (EQCAR U 'OVERFLOW)
                                                    (AND (NUMBERP U)
                                                         (OR
                                                          (AND ZHI
                                                               (GREATERP U
                                                                         ZHI))
                                                          (AND ZLO
                                                               (LESSP U
                                                                      ZLO)))))
                                                (PROGN
                                                 (SETQ U NIL)
                                                 (COND
                                                  ((AND (LESSP 0 J)
                                                        (LESSP J NX)
                                                        (LESSP 0 I)
                                                        (LESSP I NY))
                                                   (SETQ W
                                                           (CONS
                                                            (LIST
                                                             (DIFFERENCE XX DX)
                                                             (PLUS XX DX)
                                                             (DIFFERENCE YY DY)
                                                             (PLUS YY DY))
                                                            W))))
                                                 NIL)))
                                              (LIST T T XX YY U))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                 NIL)))
               LOOPLABEL
                (SETQ J (PLUS2 J 1))
                (COND ((MINUSP (DIFFERENCE NX J)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (PROG (I FORALL-RESULT FORALL-ENDPTR)
                            (SETQ I 0)
                            (COND ((MINUSP (DIFFERENCE NY I)) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             (PROGN
                                              (SETQ XX (PLUS XLO (TIMES I DX)))
                                              (SETQ YY (PLUS YLO (TIMES J DY)))
                                              (SETQ U
                                                      (PLOTEVALFORM FF F
                                                       (LIST (CONS X XX)
                                                             (CONS Y YY))))
                                              (COND
                                               ((NULL U)
                                                (SETQ U
                                                        (PLOTEVAL3XYSINGULAR FF
                                                                             F
                                                                             X
                                                                             XX
                                                                             DX
                                                                             Y
                                                                             YY
                                                                             DY
                                                                             ZHI
                                                                             ZLO))))
                                              (COND
                                               ((OR (NULL U)
                                                    (EQCAR U 'OVERFLOW)
                                                    (AND (NUMBERP U)
                                                         (OR
                                                          (AND ZHI
                                                               (GREATERP U
                                                                         ZHI))
                                                          (AND ZLO
                                                               (LESSP U
                                                                      ZLO)))))
                                                (PROGN
                                                 (SETQ U NIL)
                                                 (COND
                                                  ((AND (LESSP 0 J)
                                                        (LESSP J NX)
                                                        (LESSP 0 I)
                                                        (LESSP I NY))
                                                   (SETQ W
                                                           (CONS
                                                            (LIST
                                                             (DIFFERENCE XX DX)
                                                             (PLUS XX DX)
                                                             (DIFFERENCE YY DY)
                                                             (PLUS YY DY))
                                                            W))))
                                                 NIL)))
                                              (LIST T T XX YY U))
                                             NIL)))
                           LOOPLABEL
                            (SETQ I (PLUS2 I 1))
                            (COND
                             ((MINUSP (DIFFERENCE NY I))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     (PROGN
                                      (SETQ XX (PLUS XLO (TIMES I DX)))
                                      (SETQ YY (PLUS YLO (TIMES J DY)))
                                      (SETQ U
                                              (PLOTEVALFORM FF F
                                               (LIST (CONS X XX) (CONS Y YY))))
                                      (COND
                                       ((NULL U)
                                        (SETQ U
                                                (PLOTEVAL3XYSINGULAR FF F X XX
                                                                     DX Y YY DY
                                                                     ZHI
                                                                     ZLO))))
                                      (COND
                                       ((OR (NULL U) (EQCAR U 'OVERFLOW)
                                            (AND (NUMBERP U)
                                                 (OR (AND ZHI (GREATERP U ZHI))
                                                     (AND ZLO (LESSP U ZLO)))))
                                        (PROGN
                                         (SETQ U NIL)
                                         (COND
                                          ((AND (LESSP 0 J) (LESSP J NX)
                                                (LESSP 0 I) (LESSP I NY))
                                           (SETQ W
                                                   (CONS
                                                    (LIST (DIFFERENCE XX DX)
                                                          (PLUS XX DX)
                                                          (DIFFERENCE YY DY)
                                                          (PLUS YY DY))
                                                    W))))
                                         NIL)))
                                      (LIST T T XX YY U))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS W L)))) 
(PUT 'PLOTEVAL3XY2 'NUMBER-OF-ARGS 1) 
(PUT 'PLOTEVAL3XY2 'DEFINED-ON-LINE '113) 
(PUT 'PLOTEVAL3XY2 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XY2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOTEVAL3XY2 (L) (PLOTEVAL3XY3 (LIST L))) 
(PUT 'PLOTEVAL3XY3 'NUMBER-OF-ARGS 1) 
(PUT 'PLOTEVAL3XY3 'DEFINED-ON-LINE '116) 
(PUT 'PLOTEVAL3XY3 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XY3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOTEVAL3XY3 (LL)
    (PROG (L LR C W Y R NW N M)
      (SETQ N 0)
      (SETQ M 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT LL) (RETURN NIL)))
        (PROGN
         (SETQ L (CAR LL))
         (SETQ LL (CDR LL))
         (SETQ W (LIST (CAR L) (CADR L)))
         (SETQ L (CDR L))
         (SETQ N
                 (MIN (PLOTEVAL3XYBKPT (CAR W) 0 NIL)
                      (PLOTEVAL3XYBKPT (CADR W) 0 T)))
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C L (CDR L))) (RETURN NIL)))
           (PROGN
            (SETQ M (PLOTEVAL3XYBKPT (CADR L) 0 T))
            (COND
             ((OR (AND (IZEROP N) (IZEROP M))
                  (AND (IGREATERP N 0) (NOT (IGREATERP N M))))
              (PROGN (SETQ L (CDR L)) (SETQ W (NCONC W (LIST (CAR L))))))
             (T (SETQ C NIL)))
            NIL)
           (GO WHILELABEL))
         (COND ((CDR L) (SETQ LL (CONS L LL))))
         (SETQ L NIL)
         (SETQ R NIL)
         (SETQ NW NIL)
         (PROG (ROW)
           (SETQ ROW W)
          LAB
           (COND ((NULL ROW) (RETURN NIL)))
           ((LAMBDA (ROW)
              (PROGN
               (COND ((IZEROP N) (SETQ ROW (CDR ROW)))
                     (T
                      (SETQ R
                              (CONS
                               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ I 1)
                                 (COND
                                  ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (PROGN
                                                   (SETQ Y (CDDAR ROW))
                                                   (SETQ ROW (CDR ROW))
                                                   Y)
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ I (PLUS2 I 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE N I))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (PROGN
                                           (SETQ Y (CDDAR ROW))
                                           (SETQ ROW (CDR ROW))
                                           Y)
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))
                               R))))
               (COND (ROW (SETQ NW (CONS ROW NW))))
               NIL))
            (CAR ROW))
           (SETQ ROW (CDR ROW))
           (GO LAB))
         (SETQ NW (REVERSIP NW))
         (COND (NW (SETQ LL (CONS NW LL))))
         (COND ((AND R (CDR R)) (SETQ LR (CONS R LR))))
         NIL)
        (GO WHILELABEL))
      (RETURN LR))) 
(PUT 'PLOTEVAL3XYBKPT 'NUMBER-OF-ARGS 3) 
(PUT 'PLOTEVAL3XYBKPT 'DEFINED-ON-LINE '153) 
(PUT 'PLOTEVAL3XYBKPT 'DEFINED-IN-FILE 'PLOT/PLOTEXP3.RED) 
(PUT 'PLOTEVAL3XYBKPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL3XYBKPT (W N M)
    (COND ((NULL W) N) ((MEMQ NIL (CDDAR W)) N)
          ((OR (NULL (CADAR W)) (AND M (NULL (CAAR W)))) (PLUS N 1))
          (T (PLOTEVAL3XYBKPT (CDR W) (IPLUS2 N 1) M)))) 
(ENDMODULE) 
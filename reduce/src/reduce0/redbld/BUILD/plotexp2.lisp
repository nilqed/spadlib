(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PLOTEXP2)) 
(PUT 'PLOTEVAL2X 'NUMBER-OF-ARGS 2) 
(PUT 'PLOTEVAL2X 'DEFINED-ON-LINE '28) 
(PUT 'PLOTEVAL2X 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'PLOTEVAL2X 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL2X (X Y)
    (PROG (XLO XHI YLO YHI RX RY FP FCN FCNS PTS)
      (COND ((EQUAL Y 'IMPLICIT) (REDERR "no implicit plot in one dimension")))
      (SETQ RX (PLOTRANGE X (REVAL1 (OR PLOT_XRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ XLO (CAR RX))
      (SETQ XHI (CADR RX))
      (SETQ FCNS (REVERSE PLOTFUNCTIONS*))
      (SETQ RY (PLOTRANGE Y (REVAL1 (OR PLOT_YRANGE NIL) T)))
      (COND (RY (PROGN (SETQ YLO (CAR RY)) (SETQ YHI (CADR RY)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCNS) (RETURN NIL)))
        (PROGN
         (SETQ FCN (CAR FCNS))
         (SETQ FCNS (CDR FCNS))
         (COND ((EQCAR FCN 'POINTS) (SETQ FP (CONS (CADDR FCN) FP)))
               (T
                (SETQ PTS
                        (CONS (PLOTEVAL2X1 (CDR FCN) X XLO XHI YLO YHI) PTS))))
         NIL)
        (GO WHILELABEL))
      (APPLY (GET PLOTDRIVER* 'PLOT-2EXP) (LIST X Y PTS FP)))) 
(PUT 'PLOTEVAL2X1 'NUMBER-OF-ARGS 6) 
(PUT 'PLOTEVAL2X1 'DEFINED-ON-LINE '47) 
(PUT 'PLOTEVAL2X1 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'PLOTEVAL2X1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVAL2X1 (F X XLO XHI YLO YHI)
    (PROG (PLOTSYNERR* L D D0 U V VV P MX MN FF PLOTDERIV* NX)
      (SETQ NX 0)
      (SETQ FF (ERRORSET (LIST 'REVAL (MKQUOTE (LIST 'DF F X))) NIL NIL))
      (COND
       ((AND (NOT (ERRORP FF)) (NOT (SMEMQ 'DF (CAR FF))))
        (SETQ PLOTDERIV* (CONS (RDWRAP (CAR FF)) (CAR FF)))))
      (SETQ FF (RDWRAP F))
      (SETQ P (FLOAT (SETQ NX (PLOT-POINTS X))))
      (SETQ D (QUOTIENT (SETQ D0 (DIFFERENCE XHI XLO)) P))
      (SETQ V XLO)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
        (PROGN
         (SETQ VV
                 (COND ((OR (EQUAL I 0) (EQUAL I NX)) V)
                       (T
                        (PLUS V
                              (TIMES D (DIFFERENCE (RANDOM 100) 50) 0.001)))))
         (SETQ U (PLOTEVALFORM FF F (LIST (CONS X VV))))
         (COND (PLOTSYNERR* (TYPERR F "function to plot")))
         (COND ((EQCAR U 'OVERFLOW) (SETQ U NIL)))
         (COND
          (U
           (PROGN
            (COND ((AND YHI (GREATERP U YHI)) (SETQ U YHI))
                  ((AND YLO (LESSP U YLO)) (SETQ U YLO)))
            NIL
            (COND ((OR (NULL MX) (GREATERP U MX)) (SETQ MX U)))
            (COND ((OR (NULL MN) (LESSP U MN)) (SETQ MN U))))))
         (SETQ L (CONS (CONS VV U) L))
         (SETQ V (PLUS V D))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((OR (NULL MX) (NULL MN)) (REDERR "plot, sampling failed")))
      (SETQ VARIATION*
              (COND ((AND YHI (NOT (EQUAL YHI PLOTMAX*))) (DIFFERENCE YHI YLO))
                    (T (DIFFERENCE MX MN))))
      (COND
       ((GREATERP PLOT-REFINE* 0)
        (SETQ L (SMOOTH (REVERSIP L) FF F X MX MN YLO YHI D))))
      (RETURN
       (LIST
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X L)
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (LIST (CAR X) (CDR X))) (CAR X))
                                NIL)))
         LOOPLABEL
          (SETQ X (CDR X))
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (X) (LIST (CAR X) (CDR X))) (CAR X)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'PLOTEVAL2XVARIATION 'NUMBER-OF-ARGS 1) 
(PUT 'PLOTEVAL2XVARIATION 'DEFINED-ON-LINE '88) 
(PUT 'PLOTEVAL2XVARIATION 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'PLOTEVAL2XVARIATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOTEVAL2XVARIATION (L)
    (PROG (U M)
      (SETQ M 0)
      (SETQ U 1.0)
      (PROG (P)
        (SETQ P L)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ M (PLUS M 1))
            (SETQ P (CDR P))
            (COND ((AND P (NEQ P 0.0)) (SETQ U (TIMES U (ABS P)))))
            NIL))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (EXPT U (QUOTIENT 1 (FLOAT M)))))) 
(PUT 'SMOOTH 'NUMBER-OF-ARGS 9) 
(PUT 'SMOOTH 'DEFINED-ON-LINE '100) 
(PUT 'SMOOTH 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'SMOOTH 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE SMOOTH (L FF F X MAXV MINV YLO YHI D)
    (PROG (RAT GRAIN CUTMAX CUTMIN Z Z0)
      (SETQ Z L)
      (SETQ CUTMAX (OR YHI (PLUS (MINUS (TIMES 2 MINV)) (TIMES 3 MAXV))))
      (SETQ CUTMIN (OR YLO (DIFFERENCE (TIMES 3 MINV) (TIMES 2 MAXV))))
      (SETQ GRAIN (TIMES VARIATION* 0.05))
      (SETQ RAT
              (QUOTIENT D
                        (COND
                         ((AND (NUMBERP MAXV) (GREATERP MAXV MINV))
                          (DIFFERENCE MAXV MINV))
                         (T 1))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND Z (NULL (CDAR Z)) (CDR Z) (NULL (CDADR Z)))) (RETURN NIL)))
        (SETQ Z (CDR Z))
        (GO WHILELABEL))
      (COND
       ((AND Z (NULL (CDAR Z)) (CDR Z))
        (PROGN
         (SETQ Z0 (FINDSING FF F X YLO YHI (CADR Z) (CAR Z)))
         (COND (Z0 (SETQ L (SETQ Z (CONS Z0 (CDR Z))))))
         NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND Z (CDR Z))) (RETURN NIL)))
        (PROGN
         (SETQ Z0 Z)
         (SETQ Z (CDR Z))
         (SMOOTH1 Z0 FF F X CUTMIN CUTMAX GRAIN RAT 0 PLOT-REFINE*))
        (GO WHILELABEL))
      (RETURN L))) 
(PUT 'SMOOTH1 'NUMBER-OF-ARGS 10) 
(PUT 'SMOOTH1 'DEFINED-ON-LINE '122) 
(PUT 'SMOOTH1 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'SMOOTH1 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE SMOOTH1 (L FF F X MINV MAXV G RAT LEV ML)
    (SMOOTH2 L FF F X MINV MAXV G RAT LEV ML)) 
(PUT 'SMOOTH2 'NUMBER-OF-ARGS 10) 
(PUT 'SMOOTH2 'DEFINED-ON-LINE '126) 
(PUT 'SMOOTH2 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'SMOOTH2 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE SMOOTH2 (L FF F X MINV MAXV G RAT LEV ML)
    (COND ((GEQ LEV ML) (SMOOTH3 L FF F X MINV MAXV G RAT LEV ML))
          ((NULL (CDAR L)) T)
          (T
           (PROG (P0 P1 P2 P3 X1 X2 X3 Y1 Y2 Y3 D DY12 DY32 DX12 DX32 Z A W)
             (SETQ LEV (ADD1 LEV))
             (SETQ P1 (CAR L))
             (SETQ P3 (CADR L))
             (SETQ X1 (CAR P1))
             (SETQ Y1 (CDR P1))
             (SETQ X3 (CAR P3))
             (SETQ Y3 (CDR P3))
            NOPOINT
             (COND
              ((NULL Y3)
               (PROGN
                (COND ((NULL (CDDR L)) (RETURN (SETCDR L NIL))))
                (SETQ X2 X3)
                (SETQ Y2 Y3)
                (SETCDR L (CDDR L))
                (SETQ P3 (CADR L))
                (SETQ X3 (CAR P3))
                (SETQ Y3 (CDR P3))
                (COND (Y3 (GO OUTSIDE)) (T (GO NOPOINT)))
                NIL)))
             (SETQ X2 (TIMES (PLUS X1 X3) 0.5))
             (COND
              ((OR (NULL (SETQ Y2 (PLOTEVALFORM FF F (LIST (CONS X X2)))))
                   (EQCAR Y2 'OVERFLOW))
               (GO OUTSIDE)))
             (COND ((OR (LESSP Y2 MINV) (GREATERP Y2 MAXV)) (GO OUTSIDE)))
             (SETQ DY32 (TIMES (DIFFERENCE Y3 Y2) RAT))
             (SETQ DX32 (DIFFERENCE X3 X2))
             (SETQ DY12 (TIMES (DIFFERENCE Y1 Y2) RAT))
             (SETQ DX12 (DIFFERENCE X1 X2))
             (SETQ W (ERRORSET (LIST 'TIMES2 DY32 DY32) NIL NIL))
             (COND ((PLOTERRORP W) (GO DISC)) (T (SETQ W (CAR W))))
             (SETQ D (ERRORSET (LIST 'TIMES2 DY12 DY12) NIL NIL))
             (COND ((PLOTERRORP D) (GO DISC)) (T (SETQ D (CAR D))))
             (SETQ W (PLUS W (EXPT DX32 2)))
             (SETQ D (PLUS D (EXPT DX12 2)))
             (SETQ D (ERRORSET (LIST 'TIMES2 W D) NIL NIL))
             (COND ((PLOTERRORP D) (GO DISC)) (T (SETQ D (CAR D))))
             (SETQ D (SQRT D))
             (COND ((ZEROP D) (RETURN T)))
             (SETQ W (PLUS (TIMES DY32 DY12) (TIMES DX32 DX12)))
             (SETQ D (ERRORSET (LIST 'QUOTIENT W D) NIL NIL))
             (COND ((PLOTERRORP D) (GO DISC)) (T (SETQ D (CAR D))))
             (SETQ A (LESSP (ABS (DIFFERENCE Y3 Y1)) G))
             (COND
              ((AND (GREATERP D PLOTPRECISION*) A (EQUAL LEV ML)) (GO DISC)))
             (SETCDR L (CONS (CONS X2 Y2) (CDR L)))
             (COND
              ((AND (GREATERP (MINUS D) PLOTPRECISION*)
                    (OR (NULL PLOTDERIV*)
                        (AND
                         (SETQ W
                                 (PLOTEVALFORM (CAR PLOTDERIV*)
                                  (CDR PLOTDERIV*) (LIST (CONS X X2))))
                         (LESSP
                          (TIMES
                           (ABS
                            (DIFFERENCE W
                                        (QUOTIENT (DIFFERENCE Y3 Y1)
                                                  (DIFFERENCE X3 X1))))
                           RAT)
                          0.1))))
               (RETURN T)))
             (SMOOTH2 (CDR L) FF F X MINV MAXV G RAT LEV ML)
             (SMOOTH2 L FF F X MINV MAXV G RAT LEV ML)
             (RETURN T)
            OUTSIDE
             (SETCDR L (CONS (CONS X2 NIL) (CDR L)))
             (SETQ Z (CDR L))
             (SETQ P2 (CONS X2 Y2))
             (SETQ P0 (FINDSING FF F X MINV MAXV P1 P2))
             (COND
              (P0
               (PROGN
                (SETCDR L (CONS P0 Z))
                (SMOOTH2 L FF F X MINV MAXV G RAT LEV ML))))
             (SETQ P0 (FINDSING FF F X MINV MAXV P3 P2))
             (COND
              (P0
               (PROGN
                (SETCDR Z (CONS P0 (CDR Z)))
                (SMOOTH2 (CDR Z) FF F X MINV MAXV G RAT LEV ML))))
             (RETURN NIL)
            DISC
             (RETURN (SMOOTH3 L FF F X MINV MAXV G RAT LEV ML)))))) 
(PUT 'SMOOTH3 'NUMBER-OF-ARGS 10) 
(PUT 'SMOOTH3 'DEFINED-ON-LINE '212) 
(PUT 'SMOOTH3 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'SMOOTH3 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE SMOOTH3 (L FF F X MINV MAXV G RAT LEV ML)
    (PROG (P1 P2 P3 X1 X2 X3 Y1 Y2 Y3 D X2HI Y2HI X2LO Y2LO DIR HIDIR CHI CLO
           LOBOUND HIBOUND N)
      (SETQ N 0)
      (SETQ G (SETQ RAT (SETQ LEV (SETQ ML NIL))))
      (SETQ P1 (CAR L))
      (SETQ P3 (CADR L))
      (SETQ X1 (CAR P1))
      (SETQ Y1 (CDR P1))
      (SETQ X3 (CAR P3))
      (SETQ Y3 (CDR P3))
      (COND ((LESSP (ABS (DIFFERENCE Y3 Y1)) VARIATION*) (RETURN T)))
      (SETQ HIBOUND (TIMES VARIATION* 10.0))
      (SETQ LOBOUND (MINUS HIBOUND))
      (COND
       ((GREATERP Y1 Y3)
        (PROGN
         (SETQ X2HI X1)
         (SETQ Y2HI Y1)
         (SETQ X2LO X3)
         (SETQ Y2LO Y3)
         (SETQ HIDIR (MINUS 1.0))))
       (T
        (PROGN
         (SETQ X2HI X3)
         (SETQ Y2HI Y3)
         (SETQ X2LO X1)
         (SETQ Y2LO Y1)
         (SETQ HIDIR 1.0))))
      (SETQ N 0)
      (SETQ D (TIMES (DIFFERENCE X3 X1) 0.5))
      (SETQ X2 (PLUS X1 D))
     NEXT_POINT
      (COND
       ((OR (NULL (SETQ Y2 (PLOTEVALFORM FF F (LIST (CONS X X2)))))
            (EQCAR Y2 'OVERFLOW))
        (GO OUTSIDE)))
      (COND
       ((LESSP Y2 MINV)
        (PROGN (SETQ X2LO X2) (SETQ Y2LO MINV) (SETQ DIR HIDIR)))
       ((LESSP Y2 Y2LO)
        (PROGN
         (COND
          ((AND (LESSP Y2LO 0.0) (LESSP Y2 (PLUS Y2LO Y2LO))
                (LESSP Y2 LOBOUND))
           (SETQ CLO T)))
         (SETQ X2LO X2)
         (SETQ Y2LO Y2)
         (SETQ DIR HIDIR)
         NIL))
       ((GREATERP Y2 MAXV)
        (PROGN (SETQ X2HI X2) (SETQ Y2HI MAXV) (SETQ DIR (MINUS HIDIR))))
       ((GREATERP Y2 Y2HI)
        (PROGN
         (COND
          ((AND (GREATERP Y2HI 0.0) (GREATERP Y2 (PLUS Y2HI Y2HI))
                (GREATERP Y2 HIBOUND))
           (SETQ CHI T)))
         (SETQ X2HI X2)
         (SETQ Y2HI Y2)
         (SETQ DIR (MINUS HIDIR))
         NIL))
       (T (GO NO_DISC)))
      (COND
       ((AND DIR (LESSP (SETQ N (PLUS N 1)) 20) (OR (NOT CLO) (NOT CHI)))
        (PROGN
         (SETQ D (QUOTIENT D 2))
         (SETQ X2 (PLUS X2 (TIMES D DIR)))
         (GO NEXT_POINT))))
     NO_DISC
      (COND ((NULL DIR) (RETURN T)))
      (COND (CLO (SETQ Y2LO MINV)))
      (COND (CHI (SETQ Y2HI MAXV)))
     OUTSIDE
      (SETQ P1 (CONS X2HI Y2HI))
      (SETQ P3 (CONS X2LO Y2LO))
      (COND ((EQUAL HIDIR 1.0) (PROGN (SETQ P2 P3) (SETQ P3 P1) (SETQ P1 P2))))
      (SETCDR L (CONS P1 (CONS (CONS (CAR P1) NIL) (CONS P3 (CDR L)))))
      (RETURN NIL)
     BRK
      (SETCDR L (CONS (CONS (CAR P1) NIL) (CDR L)))
      (RETURN NIL))) 
(PUT 'FINDSING 'NUMBER-OF-ARGS 7) 
(PUT 'FINDSING 'DEFINED-ON-LINE '265) 
(PUT 'FINDSING 'DEFINED-IN-FILE 'PLOT/PLOTEXP2.RED) 
(PUT 'FINDSING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE FINDSING (FF F X MINV MAXV P1 P3)
    (PROG (P2 X1 X2 X3 Y1 Y2 Y3 D X2N)
      (SETQ X1 (CAR P1))
      (SETQ Y1 (CDR P1))
      (SETQ X3 (CAR P3))
      (SETQ Y3 (CDR P3))
      (SETQ D (TIMES (DIFFERENCE X3 X1) 0.5))
      (SETQ X2N (PLUS X1 D))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 5 I)) (RETURN NIL)))
        (PROGN
         (SETQ D (TIMES D 0.5))
         (SETQ X2 X2N)
         (COND
          ((OR (NULL (SETQ Y2 (PLOTEVALFORM FF F (LIST (CONS X X2)))))
               (EQCAR Y2 'OVERFLOW) (LESSP Y2 MINV) (GREATERP Y2 MAXV))
           (SETQ X2N (DIFFERENCE X2N D)))
          (T (PROGN (SETQ P2 (CONS X2 Y2)) (SETQ X2N (PLUS X2N D))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NULL P2) (RETURN NIL)))
      (SETQ X2 (CAR P2))
      (SETQ Y2 (CDR P2))
      (SETQ Y2
              (COND
               ((OR (AND (EQCAR Y2 'OVERFLOW) (LESSP (CDR Y2) 0))
                    (LESSP Y2 MINV))
                MINV)
               ((OR (EQCAR Y2 'OVERFLOW) (GREATERP Y2 MAXV)) MAXV) (T Y2)))
      (RETURN (CONS X2 Y2)))) 
(ENDMODULE) 
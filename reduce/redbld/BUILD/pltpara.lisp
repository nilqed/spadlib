(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PLTPARA)) 
(PUT 'PLOTEVALPARA1 'NUMBER-OF-ARGS 1) 
(PUT 'PLOTEVALPARA1 'DEFINED-ON-LINE '35) 
(PUT 'PLOTEVALPARA1 'DEFINED-IN-FILE 'PLOT/PLTPARA.RED) 
(PUT 'PLOTEVALPARA1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOTEVALPARA1 (X)
    (PROG (XLO XHI YLO YHI RX RY FCN FCNS PTS)
      (SETQ RX (PLOTRANGE X (REVAL1 (OR PLOT_XRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ XLO (CAR RX))
      (SETQ XHI (CADR RX))
      (SETQ FCNS (REVERSE PLOTFUNCTIONS*))
      (COND (RY (PROGN (SETQ YLO (CAR RY)) (SETQ YHI (CADR RY)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCNS) (RETURN NIL)))
        (PROGN
         (SETQ FCN (CDDAR FCNS))
         (SETQ FCNS (CDR FCNS))
         (SETQ PTS (CONS (PLOTEVALPARA11 FCN X XLO XHI) PTS))
         NIL)
        (GO WHILELABEL))
      (COND
       ((EQUAL (LENGTH FCN) 2)
        (APPLY (GET PLOTDRIVER* 'PLOT-2EXP) (LIST 'X 'Y (LIST PTS) NIL)))
       (T
        (APPLY (GET PLOTDRIVER* 'PLOT-3EXP-REG)
               (LIST 'X 'Y 'Z (LIST (LIST PTS)))))))) 
(PUT 'PLOTEVALPARA11 'NUMBER-OF-ARGS 4) 
(PUT 'PLOTEVALPARA11 'DEFINED-ON-LINE '52) 
(PUT 'PLOTEVALPARA11 'DEFINED-IN-FILE 'PLOT/PLTPARA.RED) 
(PUT 'PLOTEVALPARA11 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PLOTEVALPARA11 (FM X XLO XHI)
    (PROG (PLOTSYNERR* L D D0 U V P FL PLOTDERIV* NX)
      (SETQ NX 0)
      (SETQ FL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FM)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (CONS (RDWRAP F) F)) (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (CONS (RDWRAP F) F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ P (FLOAT (SETQ NX (PLOT-POINTS X))))
      (SETQ D (QUOTIENT (SETQ D0 (DIFFERENCE XHI XLO)) P))
      (SETQ V XLO)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
        (PROGN
         (SETQ U
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F FL)
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (F)
                                       (PLOTEVALFORM (CAR F) (CDR F)
                                        (LIST (CONS X V))))
                                     (CAR F))
                                    NIL)))
                  LOOPLABEL
                   (SETQ F (CDR F))
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (F)
                               (PLOTEVALFORM (CAR F) (CDR F)
                                (LIST (CONS X V))))
                             (CAR F))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND (PLOTSYNERR* (TYPERR FM "function to plot")))
         (COND ((SMEMQ 'OVERFLOW U) (SETQ U NIL)))
         (SETQ L (CONS U L))
         (SETQ V (PLUS V D))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (REVERSIP L)))) 
(PUT 'PLOTEVALPARA2 'NUMBER-OF-ARGS 2) 
(PUT 'PLOTEVALPARA2 'DEFINED-ON-LINE '70) 
(PUT 'PLOTEVALPARA2 'DEFINED-IN-FILE 'PLOT/PLTPARA.RED) 
(PUT 'PLOTEVALPARA2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLOTEVALPARA2 (X Y)
    (PROG (XLO XHI YLO YHI RX RY FCN FCNS PTS)
      (SETQ RX (PLOTRANGE X (REVAL1 (OR PLOT_XRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ XLO (CAR RX))
      (SETQ XHI (CADR RX))
      (SETQ FCNS (REVERSE PLOTFUNCTIONS*))
      (SETQ RY (PLOTRANGE Y (REVAL1 (OR PLOT_YRANGE '(*INTERVAL* -10 10)) T)))
      (SETQ YLO (CAR RY))
      (SETQ YHI (CADR RY))
      (SETQ FCN (CDDAR FCNS))
      (SETQ FCNS (CDR FCNS))
      (COND ((NEQ (LENGTH FCN) 3) (TYPERR (CDAR FCNS) "function to plot")))
      (SETQ PTS (PLOTEVALPARA21 FCN X XLO XHI Y YLO YHI))
      (APPLY (GET PLOTDRIVER* 'PLOT-3EXP-REG)
             (LIST 'X 'Y 'Z (LIST (LIST PTS)))))) 
(PUT 'PLOTEVALPARA21 'NUMBER-OF-ARGS 7) 
(PUT 'PLOTEVALPARA21 'DEFINED-ON-LINE '85) 
(PUT 'PLOTEVALPARA21 'DEFINED-IN-FILE 'PLOT/PLTPARA.RED) 
(PUT 'PLOTEVALPARA21 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PLOTEVALPARA21 (FM X XLO XHI Y YLO YHI)
    (PROG (PLOTSYNERR* L LL DX DY U V P FL W PLOTDERIV* NX NY)
      (SETQ NX 0)
      (SETQ NY 0)
      (SETQ FL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FM)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (CONS (RDWRAP F) F)) (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (CONS (RDWRAP F) F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ P (FLOAT (SETQ NX (PLOT-POINTS X))))
      (SETQ DX (QUOTIENT (DIFFERENCE XHI XLO) P))
      (SETQ P (FLOAT (SETQ NY (PLOT-POINTS Y))))
      (SETQ DY (QUOTIENT (DIFFERENCE YHI YLO) P))
      (SETQ V XLO)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
        (PROGN
         (SETQ W YLO)
         (SETQ L NIL)
         (PROG (J)
           (SETQ J 0)
          LAB
           (COND ((MINUSP (DIFFERENCE NY J)) (RETURN NIL)))
           (PROGN
            (SETQ U
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F FL)
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F)
                                          (PLOTEVALFORM (CAR F) (CDR F)
                                           (LIST (CONS X V) (CONS Y W))))
                                        (CAR F))
                                       NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (F)
                                  (PLOTEVALFORM (CAR F) (CDR F)
                                   (LIST (CONS X V) (CONS Y W))))
                                (CAR F))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND (PLOTSYNERR* (TYPERR FM "function to plot")))
            (COND ((SMEMQ 'OVERFLOW U) (SETQ U NIL)))
            (SETQ L (CONS U L))
            (SETQ W (PLUS W DY)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ V (PLUS V DX))
         (SETQ LL (CONS L LL))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN LL))) 
(ENDMODULE) 
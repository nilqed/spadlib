(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GNUINTFC)) 
(GLOBAL
 '(*PLOTINTERRUPTS *PLOTPAUSE *PLOTUSEPIPE PLOTHEADER* PLOTCLEANUP* PLOTTMP*)) 
(SETQ *PLOTUSEPIPE T) 
(GLOBAL '(PLOTCOMMAND* GNUPLOT_SELECT_TERMINAL*)) 
(SETQ GNUPLOT_SELECT_TERMINAL*
        "if(strstrt(GPVAL_TERMINALS,\"aqua\")!=0)set terminal aqua;else set term x11;") 
(PUT 'INITIALIZE_GNUPLOT 'NUMBER-OF-ARGS 0) 
(PUT 'INITIALIZE_GNUPLOT 'DEFINED-ON-LINE '85) 
(PUT 'INITIALIZE_GNUPLOT 'DEFINED-IN-FILE 'GNUINTFC.RED) 
(PUT 'INITIALIZE_GNUPLOT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INITIALIZE_GNUPLOT NIL
    (PROGN
     (COND
      ((EQUAL (SYSTEM "type gnuplot > /dev/null") 0)
       (PROGN
        (SETQ *PLOTPAUSE "mouse close")
        (SETQ PLOTTMP* "/tmp/")
        (SETQ PLOTDTA*
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I 1)
                  (COND ((MINUSP (DIFFERENCE 10 I)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   (BLDMSG_INTERNAL "%wplotdt%w"
                                                    (LIST PLOTTMP* I))
                                   NIL)))
                 LOOPLABEL
                  (SETQ I (PLUS2 I 1))
                  (COND ((MINUSP (DIFFERENCE 10 I)) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           (BLDMSG_INTERNAL "%wplotdt%w" (LIST PLOTTMP* I))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ PLOTCLEANUP* (CONCAT "rm " PLOTTMP* "plotdt*"))
        (COND
         (*PLOTUSEPIPE
          (PROGN
           (SETQ PLOTCOMMAND* "gnuplot")
           (SETQ PLOTCLEANUP* (LIST PLOTCLEANUP*))
           NIL))
         (T
          (PROGN
           (SETQ PLOTCMDS* (CONCAT PLOTTMP* "plotcmds"))
           (SETQ PLOTCOMMAND* (CONCAT "gnuplot " PLOTCMDS*))
           (SETQ PLOTCLEANUP* (LIST (CONCAT PLOTCLEANUP* " " PLOTCMDS*)))
           NIL)))
        (COND
         ((NULL PLOTHEADER*)
          (COND ((NULL *FORCE_GNUPLOT_TERM) (SETQ PLOTHEADER* ""))
                (T
                 ((LAMBDA (X)
                    (PROGN
                     (COND
                      ((NULL X)
                       (COND
                        ((GETENV "DISPLAY")
                         (SETQ X (CONS NIL GNUPLOT_SELECT_TERMINAL*)))
                        (T (SETQ X '(NIL . "dumb"))))))
                     (COND
                      ((LESSP (STRING-LENGTH (CDR X)) 20)
                       (SETQ PLOTHEADER*
                               (BLDMSG_INTERNAL "set term %w" (LIST (CDR X)))))
                      (T (SETQ PLOTHEADER* (CDR X))))
                     NIL))
                  (ASSOC (GETENV "TERM")
                         (CONS (CONS "xterm-color" GNUPLOT_SELECT_TERMINAL*)
                               '(("sun-cmd" . "x11") ("sun" . "x11")
                                 ("hpterm" . "x11") ("vt52" . "tek40xx")
                                 ("vt100" . "tek40xx")
                                 ("vt102" . "tek40xx")))))))))
        NIL)))
     (SETQ *PLOTINTERRUPTS '(10002))
     NIL)) 
(ENDMODULE) 
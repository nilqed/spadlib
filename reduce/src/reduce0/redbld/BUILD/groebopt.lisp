(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBOPT)) 
(PUT 'VDPVORDOPT 'NUMBER-OF-ARGS 2) 
(PUT 'VDPVORDOPT 'DEFINED-ON-LINE '43) 
(PUT 'VDPVORDOPT 'DEFINED-IN-FILE 'GROEBNER/GROEBOPT.RED) 
(PUT 'VDPVORDOPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VDPVORDOPT (W VARS)
    (PROG (C)
      (SETQ VARS (SORT VARS 'ORDOP))
      (SETQ C
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X VARS)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (CONS X (CONS 0 0))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CONS X (CONS 0 0))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (POLY)
        (SETQ POLY W)
       LAB
        (COND ((NULL POLY) (RETURN NIL)))
        ((LAMBDA (POLY) (VDPVORDOPT1 POLY VARS C)) (CAR POLY))
        (SETQ POLY (CDR POLY))
        (GO LAB))
      (SETQ C (SORT C (FUNCTION VDPVORDOPT2)))
      (SETQ INTVDPVARS*
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V C)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (VDPVORDOPT31 INTVDPVARS*))
      (COND
       (*TRGROEB
        (PROGN (PRIN2 " optimized sequence of kernels : ") (PRIN2T VARS))))
      (RETURN
       (CONS
        (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
          (SETQ POLY W)
          (COND ((NULL POLY) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS ((LAMBDA (POLY) (REORDER POLY)) (CAR POLY))
                                NIL)))
         LOOPLABEL
          (SETQ POLY (CDR POLY))
          (COND ((NULL POLY) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (POLY) (REORDER POLY)) (CAR POLY)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        VARS)))) 
(PUT 'VDPVORDOPT1 'NUMBER-OF-ARGS 3) 
(PUT 'VDPVORDOPT1 'DEFINED-ON-LINE '56) 
(PUT 'VDPVORDOPT1 'DEFINED-IN-FILE 'GROEBNER/GROEBOPT.RED) 
(PUT 'VDPVORDOPT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VDPVORDOPT1 (P VL C)
    (COND ((NULL P) 0) ((OR (OR (ATOM P) (ATOM (CAR P))) (NULL VL)) 1)
          ((NEQ (CAAAR P) (CAR VL)) (VDPVORDOPT1 P (CDR VL) C))
          (T
           (PROG (VAR POW SLOT N)
             (SETQ N 0)
             (SETQ N (VDPVORDOPT1 (CDAR P) (CDR VL) C))
             (SETQ VAR (CAAAR P))
             (SETQ POW (CDAAR P))
             (SETQ SLOT (ASSOC VAR C))
             (COND
              ((IGREATERP POW (CADR SLOT))
               (PROGN (RPLACA (CDR SLOT) POW) (RPLACD (CDR SLOT) N)))
              (T (RPLACD (CDR SLOT) (IPLUS2 N (CDDR SLOT)))))
             (RETURN (IPLUS2 N (VDPVORDOPT1 (CDR P) VL C))))))) 
(PUT 'VDPVORDOPT2 'NUMBER-OF-ARGS 2) 
(PUT 'VDPVORDOPT2 'DEFINED-ON-LINE '68) 
(PUT 'VDPVORDOPT2 'DEFINED-IN-FILE 'GROEBNER/GROEBOPT.RED) 
(PUT 'VDPVORDOPT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VDPVORDOPT2 (SL1 SL2)
    (PROGN
     (SETQ SL1 (CDR SL1))
     (SETQ SL2 (CDR SL2))
     (OR (ILESSP (CAR SL1) (CAR SL2))
         (AND (EQUAL (CAR SL1) (CAR SL2)) (ILESSP (CDR SL1) (CDR SL2)))))) 
(PUT 'VDPVORDOPT31 'NUMBER-OF-ARGS 1) 
(PUT 'VDPVORDOPT31 'DEFINED-ON-LINE '73) 
(PUT 'VDPVORDOPT31 'DEFINED-IN-FILE 'GROEBNER/GROEBOPT.RED) 
(PUT 'VDPVORDOPT31 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPVORDOPT31 (U)
    (PROG (V Y)
      (COND ((NULL U) (RETURN NIL)))
      (SETQ V
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ Y (ASSOC X DEPL*))
                            (COND
                             ((OR (NULL Y) (NULL (XNP (CDR Y) U))) (LIST X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ Y (ASSOC X DEPL*))
                            (COND
                             ((OR (NULL Y) (NULL (XNP (CDR Y) U))) (LIST X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (NCONC (VDPVORDOPT31 (SETDIFF U V)) V)))) 
(ENDMODULE) 
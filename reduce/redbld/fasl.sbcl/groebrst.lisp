(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBRST)) 
(PUT 'GROEBTESTRESTRICTION 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBTESTRESTRICTION 'DEFINED-ON-LINE '34) 
(PUT 'GROEBTESTRESTRICTION 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBTESTRESTRICTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBTESTRESTRICTION (H ARG)
    (COND ((EQUAL GROEBRESTRICTION* 'NONNEGATIVE) (GROEBNONNEG H ARG))
          ((EQUAL GROEBRESTRICTION* 'POSITIVE) (GROEBPOS H ARG))
          ((EQUAL GROEBRESTRICTION* 'IZEROPOINT) (GROEBZERO H ARG))
          (T
           (RERROR 'GROEBNR2 9
                   "Groebner: general restrictions not yet implemented")))) 
(PUT 'GROEBNONNEG 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBNONNEG 'DEFINED-ON-LINE '44) 
(PUT 'GROEBNONNEG 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBNONNEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBNONNEG (H ARG)
    (PROG (X BREAK VEV1 VEVL PROBLEMS PROBLEMS1 R)
      (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (RETURN NIL))
            ((OR (NULL (CADR H))
                 (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
             (GO FINISH)))
      (COND ((NULL (CDDR (CADR (CDDR H)))) (RETURN NIL)))
      (SETQ BREAK NIL)
      (SETQ X H)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (NULL X) (NULL (CADR (CDDR X))))) (NOT BREAK)))
          (RETURN NIL)))
        (PROGN
         (SETQ VEV1 (CADR X))
         (COND ((NOT (VBCPLUS? (CADDR X))) (SETQ BREAK T)))
         (COND ((NOT BREAK) (SETQ X (VDPRED X)))))
        (GO WHILELABEL))
      (COND (BREAK (RETURN NIL)))
      (COND
       ((OR (NULL VEV1) (AND (EQUAL (CAR VEV1) 0) (VEVZERO?1 (CDR VEV1))))
        (GO FINISH)))
      (SETQ X H)
      (SETQ VEV1 (CADR X))
      (SETQ VEVL (VEVSPLIT VEV1))
      (SETQ PROBLEMS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X VEVL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (VDPRED X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL X) (NULL (CADR (CDDR X)))))) (RETURN NIL)))
        (PROGN
         (SETQ VEV1 (CADR X))
         (SETQ VEVL (VEVSPLIT VEV1))
         (SETQ PROBLEMS1 NIL)
         (PROG (E)
           (SETQ E VEVL)
          LAB
           (COND ((NULL E) (RETURN NIL)))
           ((LAMBDA (E)
              (PROG (P)
                (SETQ P PROBLEMS)
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P)
                   (PROGN
                    (SETQ R (COND ((NOT (MEMBER E P)) (CONS E P)) (T P)))
                    (SETQ PROBLEMS1 (UNION (LIST R) PROBLEMS1))))
                 (CAR P))
                (SETQ P (CDR P))
                (GO LAB)))
            (CAR E))
           (SETQ E (CDR E))
           (GO LAB))
         (SETQ PROBLEMS PROBLEMS1)
         (SETQ X (VDPRED X)))
        (GO WHILELABEL))
      (SETQ PROBLEMS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PROBLEMS)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROG (E FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ E P)
                                      (COND ((NULL E) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E)
                                                          (VDPFMON (A2BC 1) E))
                                                        (CAR E))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ E (CDR E))
                                      (COND ((NULL E) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (E)
                                                  (VDPFMON (A2BC 1) E))
                                                (CAR E))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROG (E FORALL-RESULT FORALL-ENDPTR)
                              (SETQ E P)
                              (COND ((NULL E) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (E)
                                                  (VDPFMON (A2BC 1) E))
                                                (CAR E))
                                               NIL)))
                             LOOPLABEL
                              (SETQ E (CDR E))
                              (COND ((NULL E) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (E) (VDPFMON (A2BC 1) E))
                                        (CAR E))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (X)
        (SETQ X PROBLEMS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (Y)
             (SETQ Y PROBLEMS)
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y)
                (COND
                 ((AND (NOT (EQ X Y)) (SUBSET? X Y))
                  (SETQ PROBLEMS (DELETE Y PROBLEMS)))))
              (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ PROBLEMS1 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT PROBLEMS) (RETURN NIL)))
        (PROGN
         (COND
          ((VDPDISJOINT? (CAR PROBLEMS) ARG)
           (SETQ PROBLEMS1 (CONS (CAR PROBLEMS) PROBLEMS1))))
         (SETQ PROBLEMS (CDR PROBLEMS)))
        (GO WHILELABEL))
     FINISH
      (GROEBMESS24 H PROBLEMS1 ARG)
      (RETURN
       (COND ((NULL PROBLEMS1) 'ICANCEL) (T (CONS 'RESTRICTION PROBLEMS1)))))) 
(PUT 'GROEBPOS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBPOS 'DEFINED-ON-LINE '100) 
(PUT 'GROEBPOS 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBPOS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBPOS (H DUMMY)
    (PROG (X BREAK VEV1)
      (SETQ DUMMY NIL)
      (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (RETURN NIL))
            ((OR (NULL (CADR H))
                 (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
             (RETURN NIL)))
      (COND ((NULL (CDDR (CADR (CDDR H)))) (RETURN (GROEBPOSCANCEL H))))
      (SETQ BREAK NIL)
      (SETQ X H)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (NULL X) (NULL (CADR (CDDR X))))) (NOT BREAK)))
          (RETURN NIL)))
        (PROGN
         (SETQ VEV1 (CADR X))
         (COND ((NOT (VBCPLUS? (CADDR X))) (SETQ BREAK T)))
         (COND ((NOT BREAK) (SETQ X (VDPRED X)))))
        (GO WHILELABEL))
      (COND ((NOT BREAK) (RETURN (GROEBPOSCANCEL H))))
      (COND ((NOT (GROEBPOSVEVALUATE H)) (GROEBPOSCANCEL H)))
      (RETURN NIL))) 
(PUT 'GROEBPOSVEVALUATE 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBPOSVEVALUATE 'DEFINED-ON-LINE '121) 
(PUT 'GROEBPOSVEVALUATE 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBPOSVEVALUATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBPOSVEVALUATE (H) (PROGN (SETQ H NIL) T)) 
(PUT 'GROEBZERO 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBZERO 'DEFINED-ON-LINE '125) 
(PUT 'GROEBZERO 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBZERO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBZERO (H DUMMY)
    (PROG (L)
      (SETQ DUMMY NIL)
      (SETQ L (VDPLASTMON H))
      (COND
       ((AND L
             (OR (NULL (CDR L))
                 (AND (EQUAL (CAR (CDR L)) 0) (VEVZERO?1 (CDR (CDR L))))))
        (RETURN (GROEBPOSCANCEL H))))
      (RETURN NIL))) 
(PUT 'GROEBPOSCANCEL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBPOSCANCEL 'DEFINED-ON-LINE '132) 
(PUT 'GROEBPOSCANCEL 'DEFINED-IN-FILE 'GROEBNER/GROEBRST.RED) 
(PUT 'GROEBPOSCANCEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBPOSCANCEL (H) (PROGN (GROEBMESS24 H NIL NIL) 'CANCEL)) 
(ENDMODULE) 
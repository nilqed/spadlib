(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HGGROEB)) 
(FLUID '(DD-1* DD-2*)) 
(PUT 'DD_GROEBNER* 'NUMBER-OF-ARGS 1) 
(PUT 'DD_GROEBNER* 'DEFINED-ON-LINE '41) 
(PUT 'DD_GROEBNER* 'DEFINED-IN-FILE 'GROEBNER/HGGROEB.RED) 
(PUT 'DD_GROEBNER* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DD_GROEBNER* (Q)
    ((LAMBDA (DD-1* DD-2* U)
       (PROG (VARS W NP OLDORDER *REDEFMSG PCOUNT*)
         (SETQ PCOUNT* 0)
         (SETQ W
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (GROEREVLIST U))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ VARS (GROEBNERVARS W NIL))
         (COND ((NULL VARS) (RERROR 'GROEBNER 4 "empty system groebner")))
         (GROEDOMAINMODE)
         (SETQ OLDORDER (VDPINIT VARS))
         (SETQ W
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J W)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J) (F2VDP (CAR (SIMP J))))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (F2VDP (CAR (SIMP J)))) (CAR J))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (DD_HOMOG-CHECK W)
         (COND
          ((NOT *VDPINTEGER)
           (PROGN
            (SETQ NP T)
            (PROG (P)
              (SETQ P W)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (SETQ NP
                         (COND (NP (DIPCOEFFCIENTSFROMDOMAIN? (CADR (CDDR P))))
                               (T NIL))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (COND
             ((NOT NP) (PROGN (SETQ *VDPMODULAR NIL) (SETQ *VDPINTEGER T))))
            NIL)))
         (COND
          (*GROEBPROT
           (PROGN
            (SETQ GROEBPROTFILE
                    (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(LIST))))))
         (SETQ W (DD-BBG W))
         (COND
          (*GLTBASIS
           (SETQ GLTB
                   (PROGN
                    (SETQ ALGLIST* (CONS NIL NIL))
                    (CONS 'LIST
                          (PROG (BASE FORALL-RESULT FORALL-ENDPTR)
                            (SETQ BASE W)
                            (COND ((NULL BASE) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (BASE)
                                                (CONS 'LIST
                                                      (PROG (J FORALL-RESULT
                                                             FORALL-ENDPTR)
                                                        (SETQ J BASE)
                                                        (COND
                                                         ((NULL J)
                                                          (RETURN NIL)))
                                                        (SETQ FORALL-RESULT
                                                                (SETQ FORALL-ENDPTR
                                                                        (CONS
                                                                         ((LAMBDA
                                                                              (
                                                                               J)
                                                                            (DIP2A
                                                                             (CADR
                                                                              (CDDR
                                                                               (VDPFMON
                                                                                (A2BC
                                                                                 1)
                                                                                (CADR
                                                                                 J))))))
                                                                          (CAR
                                                                           J))
                                                                         NIL)))
                                                       LOOPLABEL
                                                        (SETQ J (CDR J))
                                                        (COND
                                                         ((NULL J)
                                                          (RETURN
                                                           FORALL-RESULT)))
                                                        (RPLACD FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (J)
                                                                    (DIP2A
                                                                     (CADR
                                                                      (CDDR
                                                                       (VDPFMON
                                                                        (A2BC
                                                                         1)
                                                                        (CADR
                                                                         J))))))
                                                                  (CAR J))
                                                                 NIL))
                                                        (SETQ FORALL-ENDPTR
                                                                (CDR
                                                                 FORALL-ENDPTR))
                                                        (GO LOOPLABEL))))
                                              (CAR BASE))
                                             NIL)))
                           LOOPLABEL
                            (SETQ BASE (CDR BASE))
                            (COND ((NULL BASE) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (BASE)
                                        (CONS 'LIST
                                              (PROG (J FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ J BASE)
                                                (COND ((NULL J) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (J)
                                                                    (DIP2A
                                                                     (CADR
                                                                      (CDDR
                                                                       (VDPFMON
                                                                        (A2BC
                                                                         1)
                                                                        (CADR
                                                                         J))))))
                                                                  (CAR J))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ J (CDR J))
                                                (COND
                                                 ((NULL J)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (J)
                                                            (DIP2A
                                                             (CADR
                                                              (CDDR
                                                               (VDPFMON
                                                                (A2BC 1)
                                                                (CADR J))))))
                                                          (CAR J))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                      (CAR BASE))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))))
         (SETQ W
                 (CONS 'LIST
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J W)
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (DIP2A (CADR (CDDR J))))
                                           (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J) (DIP2A (CADR (CDDR J))))
                                   (CAR J))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ DIPEVLIST* (LIST NIL))
         (SETQ GVARSLAST
                 (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST VARS)))
         (RETURN W)))
     (IEVAL (CAR Q)) (REVAL1 (CADR Q) T) (REVAL1 (CADDR Q) T))) 
(PUT 'DD_GROEBNER 'PSOPFN 'DD_GROEBNER*) 
(PUT 'DD_GROEBNER 'NUMBER-OF-ARGS 3) 
(PUT 'DD-BBG 'NUMBER-OF-ARGS 1) 
(PUT 'DD-BBG 'DEFINED-ON-LINE '78) 
(PUT 'DD-BBG 'DEFINED-IN-FILE 'GROEBNER/HGGROEB.RED) 
(PUT 'DD-BBG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DD-BBG (W)
    (PROG (R)
      (COPYD 'GROEBSPOLYNOM 'DD-GROEBSPOLYNOM)
      (SETQ R (ERRORSET (LIST 'GROEBNER2 (MKQUOTE W) NIL) T NIL))
      (COPYD 'GROEBSPOLYNOM 'TRUE-GROEBSPOLYNOM)
      (COND ((ERRORP R) (REDERR "dd_groebner failed")))
      (RETURN (CAAR R)))) 
(PUT 'DD_HOMOG-CHECK 'NUMBER-OF-ARGS 1) 
(PUT 'DD_HOMOG-CHECK 'DEFINED-ON-LINE '86) 
(PUT 'DD_HOMOG-CHECK 'DEFINED-IN-FILE 'GROEBNER/HGGROEB.RED) 
(PUT 'DD_HOMOG-CHECK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DD_HOMOG-CHECK (W)
    (PROG (D Q TST)
      (COND
       ((NOT (MEMQ DIPSORTMODE* '(GRADED WEIGHTED GRADLEX REVGRADLEX)))
        (TYPERR DIPSORTMODE* "term order for dd_groebner")))
      (PROG (P)
        (SETQ P W)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ Q P)
            (SETQ P (CADR (CDDR P)))
            (SETQ D (EV-GAMMA (CAR P)))
            (SETQ P (CDDR P))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (NULL P))) (RETURN NIL)))
              (PROGN
               (SETQ TST (OR TST (NEQ D (EV-GAMMA (CAR P)))))
               (SETQ P (CDDR P)))
              (GO WHILELABEL))
            (COND
             (TST (TYPERR (DIP2A (CADR (CDDR Q))) "homogeneous polynomial")))
            NIL))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB)))) 
(COPYD 'TRUE-GROEBSPOLYNOM 'GROEBSPOLYNOM) 
(PUT 'DD-GROEBSPOLYNOM 'NUMBER-OF-ARGS 2) 
(PUT 'DD-GROEBSPOLYNOM 'DEFINED-ON-LINE '100) 
(PUT 'DD-GROEBSPOLYNOM 'DEFINED-IN-FILE 'GROEBNER/HGGROEB.RED) 
(PUT 'DD-GROEBSPOLYNOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DD-GROEBSPOLYNOM (P1 P2)
    ((LAMBDA (D)
       (COND
        ((AND (LEQ DD-1* D) (OR (EQUAL DD-2* 'INFINITY) (LEQ D DD-2*)))
         (TRUE-GROEBSPOLYNOM P1 P2))
        (T (A2VDP 0))))
     (EV-GAMMA (VEVLCM (CADR P1) (CADR P2))))) 
(ENDMODULE) 
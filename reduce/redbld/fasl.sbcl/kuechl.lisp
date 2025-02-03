(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'KUECHL)) 
(SWITCH (LIST 'TRGROEB)) 
(PUT 'GROEBNER_WALK 'PSOPFN 'GROEB-WALK) 
(PUT 'GROEB-WALK 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB-WALK 'DEFINED-ON-LINE '38) 
(PUT 'GROEB-WALK 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-WALK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB-WALK (U)
    (PROG ()
      (COND
       (*GROEBOPT
        (RERROR 'GROEBNER 31 "don't call 'groebner_walk' with 'on groebopt'")))
      (COND
       ((NULL DIPVARS*)
        (RERROR 'GROEBNER 30 "'torder' must be called before")))
      (SETQ GROETIME* (TIME))
      (SETQ *GSUGAR T)
      (SETQ *GROEBRM NIL)
      (SETQ U (CAR (GROEPARAMS U 1 1)))
      (GROEBNERVARS U NIL)
      (SETQ U (GROEB-LIST U 'SIMP))
      (GROEDOMAINMODE)
      (SETQ U (GROEB-W2 U))
      (RETURN (CONS 'LIST (GROEB-COLLECT U 'MK*SQ))))) 
(PUT 'GROEB-LIST 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-LIST 'DEFINED-ON-LINE '48) 
(PUT 'GROEB-LIST 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-LIST (U FCN)
    (PROGN
     (COND
      ((OR (ATOM U) (NOT (EQCAR U 'LIST)))
       (RERROR 'GROEBNER 29 "groebner: list as argument required")))
     (GROEB-COLLECT (CDR U) FCN))) 
(PUT 'GROEB-COLLECT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-COLLECT 'DEFINED-ON-LINE '55) 
(PUT 'GROEB-COLLECT 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-COLLECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-COLLECT (L F)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (COND ((NUMBERP F) F) (T (APPLY1 F X))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X) (COND ((NUMBERP F) F) (T (APPLY1 F X)))) (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'GROEB-W2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB-W2 'DEFINED-ON-LINE '60) 
(PUT 'GROEB-W2 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB-W2 (G)
    (PROG (IWV OWV OMEGA GOMEGA GOMEGAPLUS TT TTO PC FIRST MX IMX MMX IMMX NN
           LL PRIM *VDPINTEGER *GROEBDIVIDE)
     *VDPINTEGER
      (SETQ *GROEBDIVIDE T)
      (SETQ FIRST T)
      (SETQ PCOUNT* 0)
      (SETQ MMX (*I2RN 1))
      (SETQ IMMX MMX)
      (SETQ IWV (GROEB-COLLECT DIPVARS* 1))
      (SETQ OMEGA IWV)
      (SETQ OWV (CONS 1 (GROEB-COLLECT (CDR DIPVARS*) 0)))
      (SETQ TTO OWV)
      (GROEB-W9 'WEIGHTED OMEGA)
      (SETQ G (GROEB-COLLECT G 'SQ2VDP))
      (SETQ PC PCOUNT*)
      (GBTEST G)
      (SETQ NN (LENGTH DIPVARS*))
      (SETQ LL (|RNINV:| (*I2RN NN)))
      (SETQ PRIM T)
     LOOP
      (GROEB-W9 'WEIGHTED OMEGA)
      (SETQ MX (GROEB-W6-4 (GROEB-COLLECT OMEGA 1)))
      (COND (*TRGROEB (GROEBMESS34 (CADR MX))))
      (SETQ IMX (|RNINV:| MX))
      (SETQ G (COND (FIRST (GROEB-COLLECT G 'VDPSIMPCONT)) (T (GROEB-W10 G))))
      (COND (*TRGROEB (GROEBMESS29 OMEGA)))
      (SETQ GOMEGA (COND ((OR FIRST (NOT PRIM)) G) (T (GROEB-W3 G OMEGA))))
      (SETQ PCOUNT* PC)
      (COND ((AND *TRGROEB (NOT FIRST)) (GROEBMESS32 GOMEGA)))
      (SETQ GOMEGAPLUS
              (COND (FIRST (LIST GOMEGA)) (T (GTRAVERSO GOMEGA NIL NIL))))
      (COND
       ((CDR GOMEGAPLUS)
        (RERROR 'GROEBNER 31 "groebner_walk,cdr of 'groebner' must be nil"))
       (T (SETQ GOMEGAPLUS (CAR GOMEGAPLUS))))
      (COND ((AND *TRGROEB (NOT FIRST)) (GROEBMESS30 GOMEGAPLUS)))
      (COND ((AND (NOT FIRST) PRIM) (SETQ G (GROEB-W4 GOMEGAPLUS GOMEGA G)))
            ((NOT PRIM) (SETQ G GOMEGA)))
      (COND
       ((NOT FIRST)
        (SETQ G
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X G)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (GSETSUGAR X NIL)) (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (GSETSUGAR X NIL)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND ((AND *TRGROEB (NOT FIRST)) (GROEBMESS31 G)))
      (COND ((GROEB-W5 OMEGA IMX TTO IMMX) (GO RET)))
      (COND ((AND (NOT FIRST) (|RNONEP:| TT)) (GO RET)))
      (SETQ TT (GROEB-W6-6 G TTO IMMX OMEGA IMX LL))
      (COND (*TRGROEB (GROEBMESS36 TT)))
      (COND ((NULL TT) (GO RET)))
      (SETQ PRIM (NOT (|RNONEP:| TT)))
      (COND (*TRGROEB (GROEBMESS37 PRIM)))
      (SETQ OMEGA (GROEB-W7 TT OMEGA IMX TTO IMMX))
      (COND (*TRGROEB (GROEBMESS35 OMEGA)))
      (SETQ FIRST NIL)
      (GO LOOP)
     RET
      (COND (*TRGROEB (GROEBMESS33 G)))
      (SETQ G (GROEB-COLLECT G 'VDPSIMPCONT))
      (SETQ G (GROEB-COLLECT G 'VDP2SQ))
      (RETURN G))) 
(PUT 'GROEB-W3 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W3 'DEFINED-ON-LINE '124) 
(PUT 'GROEB-W3 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W3 (G OMEGA)
    (PROG (X Y GG FF)
      (SETQ GG
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F G)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (PROGN
                                     (SETQ FF (VDPFMON (CADDR F) (CADR F)))
                                     (GSETSUGAR FF NIL)
                                     (SETQ X
                                             (EVWEIGHTEDCOMP2 0 (CADR FF)
                                                              OMEGA))
                                     (SETQ Y X)
                                     (SETQ F (VDPRED F))
                                     (PROG ()
                                      WHILELABEL
                                       (COND
                                        ((NOT
                                          (AND
                                           (NOT
                                            (OR (NULL F)
                                                (NULL (CADR (CDDR F)))))
                                           (EQUAL Y X)))
                                         (RETURN NIL)))
                                       (PROGN
                                        (SETQ Y
                                                (EVWEIGHTEDCOMP2 0 (CADR F)
                                                                 OMEGA))
                                        (COND
                                         ((EQUAL Y X)
                                          (SETQ FF
                                                  (VDPSUM FF
                                                          (VDPFMON (CADDR F)
                                                                   (CADR
                                                                    F))))))
                                        (SETQ F (VDPRED F)))
                                       (GO WHILELABEL))
                                     FF))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (PROGN
                             (SETQ FF (VDPFMON (CADDR F) (CADR F)))
                             (GSETSUGAR FF NIL)
                             (SETQ X (EVWEIGHTEDCOMP2 0 (CADR FF) OMEGA))
                             (SETQ Y X)
                             (SETQ F (VDPRED F))
                             (PROG ()
                              WHILELABEL
                               (COND
                                ((NOT
                                  (AND
                                   (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                                   (EQUAL Y X)))
                                 (RETURN NIL)))
                               (PROGN
                                (SETQ Y (EVWEIGHTEDCOMP2 0 (CADR F) OMEGA))
                                (COND
                                 ((EQUAL Y X)
                                  (SETQ FF
                                          (VDPSUM FF
                                                  (VDPFMON (CADDR F)
                                                           (CADR F))))))
                                (SETQ F (VDPRED F)))
                               (GO WHILELABEL))
                             FF))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN GG))) 
(PUT 'GROEB-W4 'NUMBER-OF-ARGS 3) 
(PUT 'GROEB-W4 'DEFINED-ON-LINE '138) 
(PUT 'GROEB-W4 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W4 (GB GOMEGA G)
    (PROG (X)
      (PROG (Y)
        (SETQ Y GB)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (GSETSUGAR Y NIL)) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ X
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y GOMEGA)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (GROEB-W8 Y GB)) (CAR Y))
                                      NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (GROEB-W8 Y GB)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z X)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Z) (GROEB-W4-1 Z G)) (CAR Z))
                                      NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Z) (GROEB-W4-1 Z G)) (CAR Z)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN X))) 
(PUT 'GROEB-W4-1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W4-1 'DEFINED-ON-LINE '147) 
(PUT 'GROEB-W4-1 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W4-1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W4-1 (PL FS)
    (PROG (Z)
      (SETQ Z (A2VDP 0))
      (GSETSUGAR Z 0)
      (PROG (P)
        (SETQ P (PAIR PL FS))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND ((CAR P) (SETQ Z (VDPSUM Z (VDPPROD (CAR P) (CDR P)))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ Z (VDPSIMPCONT Z))
      (RETURN Z))) 
(PUT 'GROEB-W5 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB-W5 'DEFINED-ON-LINE '156) 
(PUT 'GROEB-W5 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W5 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W5 (EV1 X1 EV2 X2) (GROEB-W5-1 X1 EV1 X2 EV2)) 
(PUT 'GROEB-W5-1 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB-W5-1 'DEFINED-ON-LINE '160) 
(PUT 'GROEB-W5-1 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W5-1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W5-1 (X1 EV1 X2 EV2)
    (OR (AND (NULL EV1) (NULL EV2))
        (AND
         (EQUAL (|RNTIMES:| (*I2RN (CAR EV1)) X1)
                (|RNTIMES:| (*I2RN (CAR EV2)) X2))
         (GROEB-W5-1 X1 (CDR EV1) X2 (CDR EV2))))) 
(PUT 'GROEB-W6-4 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB-W6-4 'DEFINED-ON-LINE '165) 
(PUT 'GROEB-W6-4 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB-W6-4 (OMEGA) (GROEB-W6-5 OMEGA VDPSORTEXTENSION* 0)) 
(PUT 'GROEB-W6-5 'NUMBER-OF-ARGS 3) 
(PUT 'GROEB-W6-5 'DEFINED-ON-LINE '169) 
(PUT 'GROEB-W6-5 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-5 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W6-5 (OMEGA V M)
    (COND ((NULL OMEGA) (*I2RN M))
          ((EQUAL 0 (CAR OMEGA)) (GROEB-W6-5 (CDR OMEGA) (CDR V) M))
          ((EQUAL 1 (CAR OMEGA))
           (GROEB-W6-5 (CDR OMEGA) (CDR V) (IPLUS2 M (CAR V))))
          (T
           (GROEB-W6-5 (CDR OMEGA) (CDR V)
            (IPLUS2 M (ITIMES2 (CAR OMEGA) (CAR V))))))) 
(PUT 'GROEB-W6-6 'NUMBER-OF-ARGS 6) 
(PUT 'GROEB-W6-6 'DEFINED-ON-LINE '176) 
(PUT 'GROEB-W6-6 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-6 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W6-6 (GB TT IFACTT TP IFACTP LL)
    (PROG (MN X ZERO ONE)
      (SETQ ZERO (*I2RN 0))
      (SETQ ONE (*I2RN 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL GB))) (RETURN NIL)))
        (PROGN
         (SETQ X (GROEB-W6-7 (CAR GB) TT IFACTT TP IFACTP ZERO ONE LL))
         (COND
          ((OR (NULL MN) (AND X (|RNMINUSP:| (|RNDIFFERENCE:| X MN))))
           (SETQ MN X)))
         (SETQ GB (CDR GB)))
        (GO WHILELABEL))
      (RETURN MN))) 
(PUT 'GROEB-W6-7 'NUMBER-OF-ARGS 8) 
(PUT 'GROEB-W6-7 'DEFINED-ON-LINE '185) 
(PUT 'GROEB-W6-7 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-7 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE GROEB-W6-7 (POL TT IFACTT TP IFACTP ZERO ONE LL)
    (PROG (A B EV1 EV2 X Y Z MN)
      (SETQ EV1 (CADR POL))
      (SETQ A (EVWEIGHTEDCOMP2 0 EV1 VDPSORTEXTENSION*))
      (SETQ Y (GROEB-W6-8 EV1 TT IFACTT TP IFACTP ZERO ZERO ONE LL))
      (SETQ Y (CONS (|RNMINUS:| (CAR Y)) (|RNMINUS:| (CDR Y))))
      (SETQ POL (VDPRED POL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NOT (OR (NULL POL) (NULL (CADR (CDDR POL)))))) (RETURN NIL)))
        (PROGN
         (SETQ EV2 (CADR POL))
         (SETQ POL (VDPRED POL))
         (SETQ B (EVWEIGHTEDCOMP2 0 EV2 VDPSORTEXTENSION*))
         (COND
          ((NOT (EQUAL A B))
           (PROGN
            (SETQ X
                    (GROEB-W6-9 EV2 TT IFACTT TP IFACTP (CAR Y) (CDR Y) ONE LL
                     NIL))
            (COND
             (X
              (PROGN
               (SETQ Z (|RNDIFFERENCE:| X ONE))
               (COND
                ((AND (|RNMINUSP:| (|RNDIFFERENCE:| ZERO X))
                      (OR (|RNMINUSP:| Z) (|RNZEROP:| Z))
                      (OR (NULL MN) (|RNMINUSP:| (|RNDIFFERENCE:| X MN))))
                 (SETQ MN X))))))))))
        (GO WHILELABEL))
      (RETURN MN))) 
(PUT 'GROEB-W6-8 'NUMBER-OF-ARGS 9) 
(PUT 'GROEB-W6-8 'DEFINED-ON-LINE '207) 
(PUT 'GROEB-W6-8 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-8 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE GROEB-W6-8 (EV TT IFACTT TP IFACTP SUM1 SUM2 M DM)
    (PROG (X Y Z)
      (COND
       (EV
        (PROGN
         (SETQ X (|RNTIMES:| (*I2RN (CAR EV)) M))
         (SETQ Y (|RNTIMES:| (*I2RN (CAR TP)) IFACTP))
         (SETQ Z (|RNTIMES:| (*I2RN (CAR TT)) IFACTT)))))
      (RETURN
       (COND ((NULL EV) (CONS SUM1 SUM2))
             (T
              (GROEB-W6-8 (CDR EV) (CDR TT) IFACTT (CDR TP) IFACTP
               (|RNPLUS:| SUM1 (|RNTIMES:| Y X))
               (|RNPLUS:| SUM2 (|RNTIMES:| (|RNDIFFERENCE:| Z Y) X))
               (|RNDIFFERENCE:| M DM) DM)))))) 
(PUT 'GROEB-W6-9 'NUMBER-OF-ARGS 10) 
(PUT 'GROEB-W6-9 'DEFINED-ON-LINE '220) 
(PUT 'GROEB-W6-9 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W6-9 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE GROEB-W6-9 (EV TT IFACTT TP IFACTP Y1 Y2 M DM DONE)
    (PROG (X Y Z)
      (COND
       (EV
        (PROGN
         (SETQ X (|RNTIMES:| (*I2RN (CAR EV)) M))
         (SETQ Y (|RNTIMES:| (*I2RN (CAR TP)) IFACTP))
         (SETQ Z (|RNTIMES:| (*I2RN (CAR TT)) IFACTT)))))
      (RETURN
       (COND
        ((NULL EV)
         (COND ((NULL DONE) NIL) (T (|RNQUOTIENT:| (|RNMINUS:| Y1) Y2))))
        (T
         (GROEB-W6-9 (CDR EV) (CDR TT) IFACTT (CDR TP) IFACTP
          (|RNPLUS:| Y1 (|RNTIMES:| Y X))
          (|RNPLUS:| Y2 (|RNTIMES:| (|RNDIFFERENCE:| Z Y) X))
          (|RNDIFFERENCE:| M DM) DM (OR DONE (NOT (EQUAL (CAR EV) 0))))))))) 
(PUT 'GROEB-W7 'NUMBER-OF-ARGS 5) 
(PUT 'GROEB-W7 'DEFINED-ON-LINE '240) 
(PUT 'GROEB-W7 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W7 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB-W7 (TT OMEGA X TTO Y)
    (PROG (N Z)
      (SETQ N (*I2RN 1))
      (SETQ OMEGA
              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                (SETQ G OMEGA)
                (COND ((NULL G) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (G)
                                    (PROGN
                                     (SETQ Z
                                             (|RNPLUS:|
                                              (|RNTIMES:|
                                               (|RNTIMES:| (*I2RN G) X)
                                               (|RNDIFFERENCE:| (*I2RN 1) TT))
                                              (|RNTIMES:|
                                               (|RNTIMES:| (*I2RN (CAR TTO)) Y)
                                               TT)))
                                     (SETQ TTO (CDR TTO))
                                     (SETQ N (GROEB-W7-1 N (|RNINV:| Z)))
                                     Z))
                                  (CAR G))
                                 NIL)))
               LOOPLABEL
                (SETQ G (CDR G))
                (COND ((NULL G) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (G)
                            (PROGN
                             (SETQ Z
                                     (|RNPLUS:|
                                      (|RNTIMES:| (|RNTIMES:| (*I2RN G) X)
                                                  (|RNDIFFERENCE:| (*I2RN 1)
                                                                   TT))
                                      (|RNTIMES:|
                                       (|RNTIMES:| (*I2RN (CAR TTO)) Y) TT)))
                             (SETQ TTO (CDR TTO))
                             (SETQ N (GROEB-W7-1 N (|RNINV:| Z)))
                             Z))
                          (CAR G))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ OMEGA
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A OMEGA)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (RNEQUIV (|RNTIMES:| A (*I2RN N))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A) (RNEQUIV (|RNTIMES:| A (*I2RN N))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN OMEGA))) 
(PUT 'GROEB-W7-1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W7-1 'DEFINED-ON-LINE '254) 
(PUT 'GROEB-W7-1 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W7-1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W7-1 (N M)
    (PROG (X Y Z)
      (COND ((ATOM N) (SETQ X N))
            (T
             (PROGN
              (SETQ X (|RNPREP:| N))
              (COND ((NOT (ATOM X)) (SETQ X (CADR X)))))))
      (COND ((ATOM M) (SETQ Y M))
            (T
             (PROGN
              (SETQ Y (|RNPREP:| M))
              (COND ((NOT (ATOM Y)) (SETQ Y (CADR Y)))))))
      (SETQ Z (LCM X Y))
      (RETURN Z))) 
(PUT 'GROEB-W8 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W8 'DEFINED-ON-LINE '266) 
(PUT 'GROEB-W8 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W8 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W8 (P GB)
    (PROG (X Y)
      (SETQ X (GROEB-W8-1 P GB))
      (SETQ P SECONDVALUE*)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL P) (NULL (CADR (CDDR P)))))) (RETURN NIL)))
        (PROGN
         (SETQ Y (GROEB-W8-1 P GB))
         (SETQ P SECONDVALUE*)
         (SETQ X
                 (PROG (PP)
                   (SETQ PP (PAIR X Y))
                  LAB
                   (COND ((NULL PP) (RETURN NIL)))
                   ((LAMBDA (PP)
                      (COND ((NULL (CAR PP)) (CDR PP))
                            ((NULL (CDR PP)) (CAR PP))
                            (T (VDPSUM (CAR PP) (CDR PP)))))
                    (CAR PP))
                   (SETQ PP (CDR PP))
                   (GO LAB))))
        (GO WHILELABEL))
      (RETURN X))) 
(PUT 'GROEB-W8-1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W8-1 'DEFINED-ON-LINE '279) 
(PUT 'GROEB-W8-1 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W8-1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W8-1 (P GB)
    (PROG (E CC R DONE PP)
      (SETQ PP (CADR P))
      (SETQ CC (CADDR P))
      (SETQ R
              (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                (SETQ POLY GB)
                (COND ((NULL POLY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (POLY)
                                    (COND (DONE NIL)
                                          ((VEVMTEST? PP (CADR POLY))
                                           (PROGN
                                            (SETQ DONE T)
                                            (SETQ E POLY)
                                            (SETQ CC (BCQUOT CC (CADDR POLY)))
                                            (SETQ PP (VEVDIF PP (CADR POLY)))
                                            (SETQ SECONDVALUE*
                                                    (VDPSUM
                                                     (VDPPROD
                                                      (GSETSUGAR
                                                       (VDPFMON (BCNEG CC) PP)
                                                       NIL)
                                                      POLY)
                                                     P))
                                            (VDPFMON CC PP)))
                                          (T NIL)))
                                  (CAR POLY))
                                 NIL)))
               LOOPLABEL
                (SETQ POLY (CDR POLY))
                (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (POLY)
                            (COND (DONE NIL)
                                  ((VEVMTEST? PP (CADR POLY))
                                   (PROGN
                                    (SETQ DONE T)
                                    (SETQ E POLY)
                                    (SETQ CC (BCQUOT CC (CADDR POLY)))
                                    (SETQ PP (VEVDIF PP (CADR POLY)))
                                    (SETQ SECONDVALUE*
                                            (VDPSUM
                                             (VDPPROD
                                              (GSETSUGAR
                                               (VDPFMON (BCNEG CC) PP) NIL)
                                              POLY)
                                             P))
                                    (VDPFMON CC PP)))
                                  (T NIL)))
                          (CAR POLY))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NULL E)
        (PROGN
         (PRINT P)
         (PRINT "-----------------")
         (PRINT GB)
         (RERROR 'GROEBNER 28 "groeb-w8-1 illegal structure"))))
      (RETURN R))) 
(PUT 'GROEB-W9 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB-W9 'DEFINED-ON-LINE '302) 
(PUT 'GROEB-W9 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W9 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB-W9 (MODE EXT)
    (PROG (X)
      (SETQ X VDPSORTEXTENSION*)
      (SETQ VDPSORTEXTENSION* EXT)
      (DIPSORTINGMODE MODE)
      (RETURN X))) 
(PUT 'GROEB-W10 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB-W10 'DEFINED-ON-LINE '309) 
(PUT 'GROEB-W10 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W10 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB-W10 (S) (GROEB-COLLECT S 'GROEB-W10-1)) 
(PUT 'GROEB-W10-1 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB-W10-1 'DEFINED-ON-LINE '313) 
(PUT 'GROEB-W10-1 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'GROEB-W10-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB-W10-1 (P)
    (PROG (X)
      (SETQ X (VDPFMON (CADDR P) (CADR P)))
      (SETQ X (GSETSUGAR (VDPENUMERATE X) NIL))
      (SETQ P (VDPRED P))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL P) (NULL (CADR (CDDR P)))))) (RETURN NIL)))
        (PROGN
         (SETQ X (VDPSUM (VDPFMON (CADDR P) (CADR P)) X))
         (SETQ P (VDPRED P)))
        (GO WHILELABEL))
      (RETURN X))) 
(PUT '|RNINV:| 'NUMBER-OF-ARGS 1) 
(PUT '|RNINV:| 'DEFINED-ON-LINE '323) 
(PUT '|RNINV:| 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT '|RNINV:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RNINV:| (X)
    (PROGN
     (COND ((ATOM X) (SETQ X (*I2RN X))))
     (SETQ X (CDR X))
     (COND ((LESSP (CAR X) 0) (MKRN (MINUS (CDR X)) (MINUS (CAR X))))
           (T (MKRN (CDR X) (CAR X)))))) 
(PUT 'SQ2VDP 'NUMBER-OF-ARGS 1) 
(PUT 'SQ2VDP 'DEFINED-ON-LINE '329) 
(PUT 'SQ2VDP 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'SQ2VDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ2VDP (S)
    (PROG (X Y)
      (SETQ X (F2VDP (CAR S)))
      (GSETSUGAR X NIL)
      (SETQ Y (F2VDP (CDR S)))
      (GSETSUGAR Y 0)
      (SETQ S (VDPDIVMON X (CADDR Y) (CADR Y)))
      (RETURN S))) 
(PUT 'VDP2SQ 'NUMBER-OF-ARGS 1) 
(PUT 'VDP2SQ 'DEFINED-ON-LINE '336) 
(PUT 'VDP2SQ 'DEFINED-IN-FILE 'GROEBNER/KUECHL.RED) 
(PUT 'VDP2SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDP2SQ (V)
    (PROG (X Y Z ONE)
      (SETQ ONE (CONS 1 1))
      (SETQ X (CONS NIL 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL V) (NULL (CADR (CDDR V)))))) (RETURN NIL)))
        (PROGN
         (SETQ Y (CONS (DIP2F (CADR (CDDR (VDPFMON ONE (CADR V))))) 1))
         (SETQ Z (CADDR V))
         (SETQ X (ADDSQ X (MULTSQ Z Y)))
         (SETQ V (VDPRED V)))
        (GO WHILELABEL))
      (RETURN X))) 
(ENDMODULE) 
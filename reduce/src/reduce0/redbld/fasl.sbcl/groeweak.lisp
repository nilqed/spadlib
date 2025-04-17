(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEWEAK)) 
(SWITCH (LIST 'GROEBWEAK)) 
(PUT 'GROEBWEAKZEROTEST 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBWEAKZEROTEST 'DEFINED-ON-LINE '30) 
(PUT 'GROEBWEAKZEROTEST 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBWEAKZEROTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBWEAKZEROTEST (F G TYPE)
    (PROG (F1 C VEV DIVISOR OLDMODE A)
      (COND ((OR (NULL F) (NULL (CADR (CDDR F)))) (RETURN F)))
      (COND ((EQUAL CURRENT-MODULUS 1) (SETMOD (LIST 2097143))))
      (SETQ OLDMODE (SETDMODE 'MODULAR T))
      (SETQ F (GROEBVDP2MOD F))
      (SETQ F1 (A2VDP 0))
      (SETQ A (BCFD 1))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                (OR (NULL F1) (NULL (CADR (CDDR F1))))))
          (RETURN NIL)))
        (PROG ()
          (SETQ VEV (CADR F))
          (SETQ C (CADDR F))
          (COND
           ((EQUAL TYPE 'SORT)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND G (EQUAL 1 (EVCOMP (CADR (CAR G)) VEV))))
                (RETURN NIL)))
              (SETQ G (CDR G))
              (GO WHILELABEL))))
          (SETQ DIVISOR (GROEBSEARCHINLIST VEV G))
          (COND
           ((AND DIVISOR *TRGROEBS)
            (PROGN (PRIN2 "//m-") (PRIN2 (VDPGETPROP DIVISOR 'NUMBER)))))
          (COND
           (DIVISOR
            (COND
             ((EQUAL (DIPLENGTH (CADR (CDDR DIVISOR))) 1)
              (SETQ F (VDPCANCELMVEV F (CADR DIVISOR))))
             (T
              (PROGN
               (SETQ DIVISOR (GROEBVDP2MOD DIVISOR))
               (COND
                (DIVISOR (SETQ F (GROEBREDUCEONESTEPRAT F NIL C VEV DIVISOR)))
                (T (SETQ F1 F)))))))
           (T (SETQ F1 F))))
        (GO WHILELABEL))
      (COND
       ((AND (NOT (OR (NULL F1) (NULL (CADR (CDDR F1))))) *TRGROEBS)
        (PROGN
         (PRIN2T " - nonzero result in modular reduction:")
         (VDPPRINT F1))))
      (SETDMODE 'MODULAR NIL)
      (COND (OLDMODE (SETDMODE (GET OLDMODE 'DNAME) T)))
      (RETURN (OR (NULL F1) (NULL (CADR (CDDR F1))))))) 
(PUT 'GROEBWEAKTESTBRANCH=1 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBWEAKTESTBRANCH=1 'DEFINED-ON-LINE '58) 
(PUT 'GROEBWEAKTESTBRANCH=1 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBWEAKTESTBRANCH=1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBWEAKTESTBRANCH=1 (POLY G D) (GROEBWEAKBASISTEST (LIST POLY) G D)) 
(PUT 'GROEBWEAKBASISTEST 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBWEAKBASISTEST 'DEFINED-ON-LINE '62) 
(PUT 'GROEBWEAKBASISTEST 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBWEAKBASISTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBWEAKBASISTEST (G0 G D)
    (PROG (OLDMODE D1 D2 P P1 S H *VDPINTEGER)
      (RETURN NIL)
      (COND ((NOT *GROEBFAC) (RETURN NIL)))
      (COND ((EQUAL CURRENT-MODULUS 1) (SETMOD (LIST 2097143))))
      (COND
       (*TRGROEB (PRIN2T "---------------- modular test of branch ------")))
      (SETQ OLDMODE (SETDMODE 'MODULAR T))
      (SETQ G0
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P G0)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (GROEBVDP2MOD P)) (CAR P))
                                      NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (GROEBVDP2MOD P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ G
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P G)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (GROEBVDP2MOD P)) (CAR P))
                                      NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (GROEBVDP2MOD P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ D
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P D)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (LIST (CAR P) (GROEBVDP2MOD (CADR P))
                                          (GROEBVDP2MOD (CADDR P))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (LIST (CAR P) (GROEBVDP2MOD (CADR P))
                                  (GROEBVDP2MOD (CADDR P))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (OR D G0)) (RETURN NIL)))
        (PROG ()
          (COND
           (G0
            (PROGN
             (SETQ H (CAR G0))
             (SETQ G0 (CDR G0))
             (SETQ P (LIST NIL H H))))
           (T
            (PROGN
             (SETQ P (CAR D))
             (SETQ D (DELETE P D))
             (SETQ S (GROEBSPOLYNOM (CADR P) (CADDR P)))
             (SETQ H (GROEBSIMPCONTNORMALFORM (GROEBNORMALFORM S G 'SORT)))
             (COND
              ((OR (NULL H) (NULL (CADR (CDDR H))))
               (AND *TRGROEB (GROEBMESS4 P D)))))))
          (COND
           ((OR (NULL H) (NULL (CADR (CDDR H))))
            (PROGN
             (SETQ PAIRSDONE*
                     (CONS
                      (CONS (VDPGETPROP (CADR P) 'NUMBER)
                            (VDPGETPROP (CADDR P) 'NUMBER))
                      PAIRSDONE*))
             (GO BOTT))))
          (COND
           ((OR (NULL (CADR H))
                (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
            (PROGN (AND *TRGROEB (GROEBMESS5 P H)) (GO STOP))))
          (SETQ S NIL)
          (SETQ H (VDPENUMERATE H))
          (AND *TRGROEB (GROEBMESS5 P H))
          (SETQ D1 NIL)
          (PROG (F)
            (SETQ F G)
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F)
               (PROGN
                (SETQ D1
                        (GROEBCPLISTSORTIN
                         (LIST (VEVLCM (CADR F) (CADR H)) F H) D1))
                (COND
                 ((EQUAL (VEVLCM (CADR F) (CADR H)) (CADR F))
                  (PROGN
                   (SETQ G (DELETE F G))
                   (AND *TRGROEB (GROEBMESS2 F)))))))
             (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (AND *TRGROEB (GROEBMESS51 D1))
          (SETQ D2 NIL)
          (PROG ()
           WHILELABEL
            (COND ((NOT D1) (RETURN NIL)))
            (PROGN
             (SETQ D1 (GROEBINVOKECRITF D1))
             (SETQ P1 (CAR D1))
             (SETQ D1 (CDR D1))
             (SETQ D2 (GROEBINVOKECRITBUCH4 P1 D2))
             (SETQ D1 (GROEBINVOKECRITM P1 D1)))
            (GO WHILELABEL))
          (SETQ D (GROEBINVOKECRITB H D))
          (SETQ D (GROEBCPLISTMERGE D D2))
          (SETQ G (CONS H G))
          (GO BOTT)
         STOP
          (SETQ D (SETQ G (SETQ G0 NIL)))
         BOTT)
        (GO WHILELABEL))
      (COND
       ((AND *TRGROEB (NULL G))
        (PRIN2T "**** modular test detects empty branch!")))
      (COND (*TRGROEB (PRIN2T "------ end of  modular test of branch ------")))
      (SETDMODE 'MODULAR NIL)
      (COND (OLDMODE (SETDMODE (GET OLDMODE 'DNAME) T)))
      (RETURN (NULL G)))) 
(FLUID '(*LOCALTEST)) 
(PUT 'GROEBFASTTEST 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBFASTTEST 'DEFINED-ON-LINE '119) 
(PUT 'GROEBFASTTEST 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBFASTTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBFASTTEST (G0 G D G99)
    (COND
     (*LOCALTEST
      (PROGN (SETQ *LOCALTEST NIL) (SETQ G99 NIL) (GROEBWEAKBASISTEST G0 G D)))
     ((AND *GROEBWEAK G (VDPUNIVARIATE? (CAR G))) (GROEBWEAKBASISTEST G0 G D)))) 
(PUT 'GROEBVDP2MOD 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBVDP2MOD 'DEFINED-ON-LINE '125) 
(PUT 'GROEBVDP2MOD 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBVDP2MOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBVDP2MOD (F)
    (PROG (U C MF)
      (SETQ U (VDPGETPROP F 'MODIMAGE))
      (COND (U (RETURN (COND ((EQUAL U 'NASTY) NIL) (T U)))))
      (SETQ MF (VDPRESIMP F))
      (COND (*GSUGAR (VDPPUTPROP MF 'SUGAR (VDPGETPROP F 'SUGAR))))
      (SETQ C (ERRORSET* (LIST 'VBCINV (MKQUOTE (CADDR MF))) NIL))
      (COND
       ((NOT (PAIRP C))
        (PROGN
         (PRIN2T "************** nasty module(loss of headterm) ****")
         (PRINT F)
         (PRINT U)
         (VDPPRINT F)
         (VDPPRINT U)
         (VDPPUTPROP F 'MODIMAGE 'NASTY)
         (RETURN NIL))))
      (SETQ U (VDPVBCPROD MF (CAR C)))
      (VDPPUTPROP U 'NUMBER (VDPGETPROP F 'NUMBER))
      (VDPPUTPROP F 'MODIMAGE U)
      (COND (*GSUGAR (VDPPUTPROP U 'SUGAR (VDPGETPROP F 'SUGAR))))
      (RETURN U))) 
(PUT 'GROEBMODEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMODEVAL 'DEFINED-ON-LINE '144) 
(PUT 'GROEBMODEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEWEAK.RED) 
(PUT 'GROEBMODEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMODEVAL (F BREAK)
    (PROG (OLDMODE A *VDPINTEGER GROEBMODULAR*)
      (SETQ GROEBMODULAR* T)
      (SETQ BREAK NIL)
      (COND ((EQUAL CURRENT-MODULUS 1) (SETMOD (LIST 2097143))))
      (SETQ OLDMODE (SETDMODE 'MODULAR T))
      (SETQ A (ERRORSET* F T))
      (SETDMODE 'MODULAR NIL)
      (COND (OLDMODE (SETDMODE (GET OLDMODE 'DNAME) T)))
      (RETURN (COND ((ATOM A) NIL) (T (CAR A)))))) 
(ENDMODULE) 
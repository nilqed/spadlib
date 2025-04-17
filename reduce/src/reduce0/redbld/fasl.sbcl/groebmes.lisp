(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBMES)) 
(PUT 'GROEBPAIRPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBPAIRPRINT 'DEFINED-ON-LINE '34) 
(PUT 'GROEBPAIRPRINT 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBPAIRPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBPAIRPRINT (P)
    (PROGN
     (GROEBMESSFF " pair(" (CADR P) NIL)
     (GROEBMESSFF "," (CADDR P) NIL)
     (PRIN2 "), ")
     (PRIN2 " lcm = ")
     (PRINT (CAR P)))) 
(PUT 'GROETIMEPRINT 'NUMBER-OF-ARGS 0) 
(PUT 'GROETIMEPRINT 'DEFINED-ON-LINE '39) 
(PUT 'GROETIMEPRINT 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROETIMEPRINT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GROETIMEPRINT NIL
    (PROGN
     (PRIN2 " >> accum. cpu time : ")
     (PRIN2 (DIFFERENCE (TIME) GROETIME*))
     (PRIN2T " ms "))) 
(PUT 'GROEBMESSFF 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESSFF 'DEFINED-ON-LINE '43) 
(PUT 'GROEBMESSFF 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESSFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESSFF (M1 F M2)
    (PROGN
     (PRIN2 M1)
     (PRIN2 (VDPGETPROP F 'NUMBER))
     (COND (*GSUGAR (PROGN (PRIN2 " / ") (PRIN2 (GSUGAR F)))))
     (COND (M2 (PRIN2T M2))))) 
(PUT 'GROEBMESS1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS1 'DEFINED-ON-LINE '48) 
(PUT 'GROEBMESS1 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS1 (G D)
    (COND
     (*TRGROEB
      (PROGN
       (SETQ G G)
       (SETQ D D)
       (PRIN2 " variables : ")
       (PRINT VDPVARS*)
       (PRINTBL)
       (PRIN2T " Start of ITERATION ")
       (TERPRI))))) 
(PUT 'GROEBMESS2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS2 'DEFINED-ON-LINE '53) 
(PUT 'GROEBMESS2 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS2 (F)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (GROEBMESSFF " polynomial " F " eliminated ")
       (GROETIMEPRINT))))) 
(PUT 'GROEBMESS2A 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESS2A 'DEFINED-ON-LINE '58) 
(PUT 'GROEBMESS2A 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS2A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS2A (F CF FN)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (GROEBMESSFF "polynomial " F NIL)
       (GROEBMESSFF " elim . with cofactor " CF " to ")
       (VDPPRINT FN)
       (TERPRI)
       (GROETIMEPRINT))))) 
(PUT 'GROEBMESS3 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS3 'DEFINED-ON-LINE '64) 
(PUT 'GROEBMESS3 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS3 (P S)
    (COND
     (*TRGROEBS
      (PROGN
       (PRIN2 " S - polynomial from ")
       (GROEBPAIRPRINT P)
       (VDPPRINT S)
       (TERPRI)
       (GROETIMEPRINT)
       (TERPRIT 3))))) 
(PUT 'GROEBMESS4 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS4 'DEFINED-ON-LINE '69) 
(PUT 'GROEBMESS4 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS4 (P D)
    (PROGN
     (SETQ HCOUNT* (PLUS HCOUNT* 1))
     (SETQ HZEROCOUNT* (PLUS HZEROCOUNT* 1))
     (COND
      (*TRGROEB
       ((LAMBDA (N)
          (PROGN
           (TERPRI)
           (PRINTBL)
           (GROEBMESSFF " reduction(" (CADR P) NIL)
           (GROEBMESSFF "," (CADDR P) NIL)
           (PRIN2 ")leads to 0;")
           (PRIN2 N)
           (PRIN2 (COND ((EQUAL N 1) " pair") (T " pairs")))))
        (LENGTH D))))
     (PRIN2T " left ")
     (PRINTBL)
     (GROETIMEPRINT))) 
(PUT 'GROEBMESS41 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS41 'DEFINED-ON-LINE '81) 
(PUT 'GROEBMESS41 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS41 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS41 (P)
    (PROGN
     (SETQ HCOUNT* (PLUS HCOUNT* 1))
     (SETQ HZEROCOUNT* (PLUS HZEROCOUNT* 1))
     (COND
      (*TRGROEB
       (PROGN
        (TERPRI)
        (PRINTBL)
        (GROEBMESSFF " polynomial(" P NIL)
        (PRIN2 ")reduced to 0;")
        (TERPRI)
        (PRINTBL)
        (GROETIMEPRINT)))))) 
(PUT 'GROEBMESS5 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS5 'DEFINED-ON-LINE '89) 
(PUT 'GROEBMESS5 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS5 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS5 (P H)
    (COND
     ((CAR P)
      (PROGN
       (SETQ HCOUNT* (PLUS HCOUNT* 1))
       (COND
        (*TRGROEB
         (PROGN
          (TERPRI)
          (PRIN2 " H - polynomial ")
          (PRIN2 PCOUNT*)
          (PRIN2 " ev : ")
          (PRIN2 (CADR H))
          (GROEBMESSFF " from pair(" (CADR P) NIL)
          (GROEBMESSFF "," (CADDR P) ")")
          (VDPPRINT H)
          (TERPRI)
          (GROETIMEPRINT))))))
     (*TRGROEB
      (PROGN
       (PRIN2T " from actual problem input : ")
       (VDPPRINT H)
       (GROETIMEPRINT))))) 
(PUT 'GROEBMESS50 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS50 'DEFINED-ON-LINE '101) 
(PUT 'GROEBMESS50 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS50 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS50 (G)
    (COND
     (*TRGROEB1
      (PROGN
       (PRIN2 " list of active polynomials : ")
       (PROG (D1)
         (SETQ D1 G)
        LAB
         (COND ((NULL D1) (RETURN NIL)))
         ((LAMBDA (D1) (PROGN (PRIN2 (VDPGETPROP D1 'NUMBER)) (PRIN2 " ")))
          (CAR D1))
         (SETQ D1 (CDR D1))
         (GO LAB))
       (TERPRIT 2))))) 
(PUT 'GROEBMESS51 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS51 'DEFINED-ON-LINE '106) 
(PUT 'GROEBMESS51 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS51 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS51 (D)
    (COND
     (*TRGROEB1
      (PROGN
       (PRIN2T " Candidates for pairs in this step : ")
       (PROG (D1)
         (SETQ D1 D)
        LAB
         (COND ((NULL D1) (RETURN NIL)))
         ((LAMBDA (D1) (GROEBPAIRPRINT D1)) (CAR D1))
         (SETQ D1 (CDR D1))
         (GO LAB))
       (TERPRIT 2))))) 
(PUT 'GROEBMESS52 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS52 'DEFINED-ON-LINE '111) 
(PUT 'GROEBMESS52 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS52 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS52 (D)
    (COND
     (*TRGROEB1
      (PROGN
       (PRIN2T " Actual new pairs from this step : ")
       (PROG (D1)
         (SETQ D1 D)
        LAB
         (COND ((NULL D1) (RETURN NIL)))
         ((LAMBDA (D1) (GROEBPAIRPRINT D1)) (CAR D1))
         (SETQ D1 (CDR D1))
         (GO LAB))
       (TERPRIT 2))))) 
(PUT 'GROEBMESS7 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS7 'DEFINED-ON-LINE '116) 
(PUT 'GROEBMESS7 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS7 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS7 (H)
    (COND
     (*TRGROEBS (PROGN (PRIN2T " Testing factorization for ") (VDPPRINT H))))) 
(PUT 'GROEBMESS8 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS8 'DEFINED-ON-LINE '120) 
(PUT 'GROEBMESS8 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS8 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS8 (G D)
    (COND
     (*TRGROEB1
      (PROGN
       (SETQ G G)
       (PRIN2T " actual pairs : ")
       (COND ((NULL D) (PRIN2T " null "))
             (T
              (PROG (D1)
                (SETQ D1 D)
               LAB
                (COND ((NULL D1) (RETURN NIL)))
                ((LAMBDA (D1) (GROEBPAIRPRINT D1)) (CAR D1))
                (SETQ D1 (CDR D1))
                (GO LAB))))
       (GROETIMEPRINT)))
     (*TRGROEB
      ((LAMBDA (N)
         (PROGN (PRIN2 N) (PRIN2T (COND ((EQUAL N 1) " pair") (T " pairs ")))))
       (LENGTH D))))) 
(PUT 'GROEBMESS13 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS13 'DEFINED-ON-LINE '130) 
(PUT 'GROEBMESS13 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS13 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS13 (G PROBLEMS)
    (COND
     ((OR *TRGROEB *TRGROEBR)
      (PROGN
       (COND
        (G
         (PROGN
          (SETQ BASECOUNT* (PLUS BASECOUNT* 1))
          (PRINTBL)
          (PRINTBL)
          (PRIN2 " end of iteration ")
          (PROG (F)
            (SETQ F (REVERSE FACTORLEVEL*))
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 " . "))) (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (PRIN2 ";basis ")
          (PRIN2 BASECOUNT*)
          (PRIN2T " : ")
          (PRIN2 " { ")
          (PROG (G1)
            (SETQ G1 G)
           LAB
            (COND ((NULL G1) (RETURN NIL)))
            ((LAMBDA (G1) (VDPPRIN3T G1)) (CAR G1))
            (SETQ G1 (CDR G1))
            (GO LAB))
          (PRIN2T " } ")
          (PRINTBL)
          (PRINTBL)
          (GROETIMEPRINT)))
        (T
         (PROGN
          (PRINTBL)
          (PRIN2 " end of iteration branch ")
          (PROG (F)
            (SETQ F (REVERSE FACTORLEVEL*))
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 " . "))) (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (PRIN2T "  ")
          (PRINTBL)
          (GROETIMEPRINT))))
       (COND
        ((AND PROBLEMS *TRGROEB)
         (PROGN
          (GROETIMEPRINT)
          (TERPRI)
          (PRINTBL)
          (PRIN2 " number of partial problems still to be solved : ")
          (PRIN2T (LENGTH PROBLEMS))
          (TERPRI)
          (PRIN2 " preparing  next problem ")
          (COND
           ((EQUAL (CAR (CAR PROBLEMS)) 'FILE) (PRIN2 (CDR (CAR PROBLEMS))))
           ((CADDDR (CAR PROBLEMS)) (VDPPRINT (CAR (CADDDR (CAR PROBLEMS))))))
          (TERPRI)))))))) 
(PUT 'GROEBMESS14 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS14 'DEFINED-ON-LINE '154) 
(PUT 'GROEBMESS14 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS14 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS14 (H HF)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2 " ******************* factorization of polynomial ")
       ((LAMBDA (X) (COND (X (PRIN2T X)) (T (TERPRI)))) (VDPGETPROP H 'NUMBER))
       (PRIN2T " factors : ")
       (PROG (G)
         (SETQ G HF)
        LAB
         (COND ((NULL G) (RETURN NIL)))
         ((LAMBDA (G) (VDPPRINT (CAR G))) (CAR G))
         (SETQ G (CDR G))
         (GO LAB))
       (GROETIMEPRINT))))) 
(PUT 'GROEBMESS15 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS15 'DEFINED-ON-LINE '161) 
(PUT 'GROEBMESS15 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS15 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS15 (F)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2T " ***** monomial factor reduced : ")
       (VDPPRINT (VDPFMON (A2BC 1) F)))))) 
(PUT 'GROEBMESS19 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESS19 'DEFINED-ON-LINE '166) 
(PUT 'GROEBMESS19 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS19 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS19 (P RESTR U)
    (COND
     (*TRGROEB
      (PROGN
       (SETQ U U)
       (SETQ RESTR RESTR)
       (PRINTBL)
       (PRIN2 " calculation branch ")
       (PROG (F)
         (SETQ F (REVERSE FACTORLEVEL*))
        LAB
         (COND ((NULL F) (RETURN NIL)))
         ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 " . "))) (CAR F))
         (SETQ F (CDR F))
         (GO LAB))
       (PRIN2T " cancelled because ")
       (VDPPRINT P)
       (PRIN2T " is member of an actual abort condition ")
       (PRINTBL)
       (PRINTBL))))) 
(PUT 'GROEBMESS19A 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS19A 'DEFINED-ON-LINE '176) 
(PUT 'GROEBMESS19A 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS19A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS19A (P U)
    (COND
     (*TRGROEB
      (PROGN
       (SETQ U U)
       (PRINTBL)
       (PRIN2 " during branch preparation ")
       (PROG (F)
         (SETQ F (REVERSE U))
        LAB
         (COND ((NULL F) (RETURN NIL)))
         ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 "."))) (CAR F))
         (SETQ F (CDR F))
         (GO LAB))
       (PRIN2T " cancelled because ")
       (VDPPRINT P)
       (PRIN2T " was found in the ideal branch ")
       (PRINTBL))))) 
(PUT 'GROEBMESS20 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS20 'DEFINED-ON-LINE '183) 
(PUT 'GROEBMESS20 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS20 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS20 (P)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (PRIN2 " secondary reduction starting with ")
       (VDPPRINT P))))) 
(PUT 'GROEBMESS21 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS21 'DEFINED-ON-LINE '187) 
(PUT 'GROEBMESS21 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS21 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS21 (P1 P2)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2 " polynomial ")
       (PRIN2 (VDPGETPROP P1 'NUMBER))
       (PRIN2 " replaced during secondary reduction by ")
       (VDPPRINT P2))))) 
(PUT 'GROEBMESS22 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESS22 'DEFINED-ON-LINE '193) 
(PUT 'GROEBMESS22 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS22 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS22 (FACTL ABORT1 ABORT2)
    (COND ((NULL FACTL) NIL)
          (*TRGROEB
           (PROG (N)
             (SETQ N 0)
             (PRIN2T " BRANCHING after factorization point ")
             (SETQ N 0)
             (PROG (X)
               (SETQ X (REVERSE FACTL))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (PROGN
                   (SETQ N (PLUS N 1))
                   (PRIN2 " branch ")
                   (PROG (F)
                     (SETQ F (REVERSE FACTORLEVEL*))
                    LAB
                     (COND ((NULL F) (RETURN NIL)))
                     ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 " . "))) (CAR F))
                     (SETQ F (CDR F))
                     (GO LAB))
                   (PRIN2T N)
                   (PROG (Y)
                     (SETQ Y (CAR X))
                    LAB
                     (COND ((NULL Y) (RETURN NIL)))
                     ((LAMBDA (Y) (VDPPRINT Y)) (CAR Y))
                     (SETQ Y (CDR Y))
                     (GO LAB))
                   (PRIN2T " simple IGNORE restrictions for this branch : ")
                   (PROG (Y)
                     (SETQ Y ABORT1)
                    LAB
                     (COND ((NULL Y) (RETURN NIL)))
                     ((LAMBDA (Y) (VDPPRINT Y)) (CAR Y))
                     (SETQ Y (CDR Y))
                     (GO LAB))
                   (PROG (Y)
                     (SETQ Y (CADR X))
                    LAB
                     (COND ((NULL Y) (RETURN NIL)))
                     ((LAMBDA (Y) (VDPPRINT Y)) (CAR Y))
                     (SETQ Y (CDR Y))
                     (GO LAB))
                   (COND
                    ((OR ABORT2 (CADDR X))
                     (PROGN
                      (PRIN2T
                       " set type IGNORE  restrictions for this branch : ")
                      (PROG (Y)
                        (SETQ Y ABORT2)
                       LAB
                        (COND ((NULL Y) (RETURN NIL)))
                        ((LAMBDA (Y) (VDPPRINTSET Y)) (CAR Y))
                        (SETQ Y (CDR Y))
                        (GO LAB))
                      (PROG (Y)
                        (SETQ Y (CADDR X))
                       LAB
                        (COND ((NULL Y) (RETURN NIL)))
                        ((LAMBDA (Y) (VDPPRINTSET Y)) (CAR Y))
                        (SETQ Y (CDR Y))
                        (GO LAB)))))
                   (PRINTBL)))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB)))))) 
(PUT 'GROEBMESS23 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESS23 'DEFINED-ON-LINE '211) 
(PUT 'GROEBMESS23 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS23 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS23 (G0 REST1 REST2)
    (COND
     (*TRGROEB
      (COND
       ((NULL FACTORLEVEL*)
        (PRIN2T " ** starting calculation ****************************** "))
       (T
        (PROGN
         (PRIN2 "** resuming calculation for branch ")
         (PROG (F)
           (SETQ F (REVERSE FACTORLEVEL*))
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F) (PROGN (PRIN2 F) (PRIN2 "."))) (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (TERPRI)
         (COND
          ((OR REST1 REST2)
           (PROGN
            (PRIN2T " -------IGNORE restrictions for this branch : ")
            (SETQ G0 G0)
            (PROG (X)
              (SETQ X REST1)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (VDPPRINT X)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (PROG (X)
              (SETQ X REST2)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (VDPPRINTSET X)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))))))))))) 
(PUT 'GROEBMESS24 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBMESS24 'DEFINED-ON-LINE '222) 
(PUT 'GROEBMESS24 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS24 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS24 (H PROBLEMS1 RESTR)
    (PROGN
     (PRIN2T " ********** polynomial affected by branch restriction : ")
     (VDPPRINT H)
     (COND (RESTR (PRIN2T " under current restrictions ")))
     (PROG (X)
       (SETQ X RESTR)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (VDPPRINT X)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (COND ((NULL PROBLEMS1) (PRIN2T "        CANCELLED "))
           (T
            (PROGN
             (PRIN2T " partitioned into ")
             (VDPPRINTSET (CAR PROBLEMS1))))))) 
(PUT 'GROEBMESS25 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS25 'DEFINED-ON-LINE '230) 
(PUT 'GROEBMESS25 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS25 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS25 (H ABORT2)
    (PROGN
     (PRIN2T " reduction of set type cancel conditions by ")
     (VDPPRINT H)
     (PRIN2T " remaining : ")
     (PROG (X)
       (SETQ X ABORT2)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (VDPPRINTSET X)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB)))) 
(PUT 'GROEBMESS26 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS26 'DEFINED-ON-LINE '235) 
(PUT 'GROEBMESS26 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS26 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS26 (F1 F2)
    (COND
     ((AND *TRGROEBS (NOT (VDPEQUAL F1 F2)))
      (PROGN
       (TERPRI)
       (PRIN2T " during final reduction ")
       (VDPPRINT F1)
       (PRIN2T " reduced to ")
       (VDPPRINT F2)
       (TERPRI))))) 
(PUT 'GROEBMESS27 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS27 'DEFINED-ON-LINE '240) 
(PUT 'GROEBMESS27 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS27 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS27 (R)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (PRIN2T " factor ignored(considered already): ")
       (VDPPRINT R))))) 
(PUT 'GROEBMESS27A 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMESS27A 'DEFINED-ON-LINE '244) 
(PUT 'GROEBMESS27A 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS27A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMESS27A (H R)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (VDPPRINT H)
       (PRIN2T "     reduced to zero by factor ")
       (VDPPRINT R))))) 
(PUT 'GROEBMESS28 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS28 'DEFINED-ON-LINE '249) 
(PUT 'GROEBMESS28 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS28 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS28 (R)
    (COND
     (*TRGROEB
      (PROGN
       (WRITEPRI " interim content reduction : " 'FIRST)
       (WRITEPRI (MKQUOTE (PREPSQ R)) 'LAST))))) 
(PUT 'GROEBMESS29 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS29 'DEFINED-ON-LINE '254) 
(PUT 'GROEBMESS29 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS29 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS29 (OMEGA)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (PRIN2 " actual weight vector : [ ")
       (PROG (X)
         (SETQ X OMEGA)
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (PROGN (PRIN2 " ") (PRIN2 X))) (CAR X))
         (SETQ X (CDR X))
         (GO LAB))
       (PRIN2 " ] ")
       (TERPRI)
       (TERPRI))))) 
(PUT 'GROEBMESS30 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS30 'DEFINED-ON-LINE '260) 
(PUT 'GROEBMESS30 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS30 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS30 (GOMEGAPLUS)
    (COND
     ((AND *TRGROEB GOMEGAPLUS)
      (PROGN
       (TERPRI)
       (PRIN2 " new head term(or full)basis ")
       (TERPRI)
       (PROG (X)
         (SETQ X GOMEGAPLUS)
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (PROGN (VDPPRINT X) (TERPRI))) (CAR X))
         (SETQ X (CDR X))
         (GO LAB)))))) 
(PUT 'GROEBMESS31 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS31 'DEFINED-ON-LINE '265) 
(PUT 'GROEBMESS31 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS31 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS31 (GG)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2 " full basis ")
       (TERPRI)
       (PROG (X)
         (SETQ X GG)
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (PROGN (VDPPRINT X) (TERPRI) (TERPRI))) (CAR X))
         (SETQ X (CDR X))
         (GO LAB)))))) 
(PUT 'GROEBMESS32 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS32 'DEFINED-ON-LINE '269) 
(PUT 'GROEBMESS32 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS32 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS32 (G)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (PRIN2 " ***** start of iteation with ")
       (TERPRI)
       (PROG (X)
         (SETQ X G)
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (VDPPRINT X)) (CAR X))
         (SETQ X (CDR X))
         (GO LAB))
       (PRIN2 " **************************** ")
       (TERPRI))))) 
(PUT 'GROEBMESS33 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS33 'DEFINED-ON-LINE '275) 
(PUT 'GROEBMESS33 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS33 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS33 (G)
    (COND
     (*TRGROEB
      (PROGN
       (TERPRI)
       (PRIN2 " ***** resulting system ***** ")
       (TERPRI)
       (PROG (X)
         (SETQ X G)
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (VDPPRINT X)) (CAR X))
         (SETQ X (CDR X))
         (GO LAB))
       (PRIN2 " **************************** ")
       (TERPRI))))) 
(PUT 'GROEBMESS34 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS34 'DEFINED-ON-LINE '281) 
(PUT 'GROEBMESS34 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS34 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS34 (MX)
    (COND
     (*TRGROEB
      (PROGN (TERPRI) (PRIN2 " sum of weight vector ") (PRINT MX) (TERPRI))))) 
(PUT 'GROEBMESS35 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS35 'DEFINED-ON-LINE '285) 
(PUT 'GROEBMESS35 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS35 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS35 (OMEGA)
    (COND
     (*TRGROEB
      (PROGN (TERPRI) (PRIN2 " next weight vector ") (PRINT OMEGA) (TERPRI))))) 
(PUT 'GROEBMESS36 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS36 'DEFINED-ON-LINE '289) 
(PUT 'GROEBMESS36 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS36 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS36 (TT)
    (COND (*TRGROEB (PROGN (TERPRI) (PRIN2 " new weight : ") (PRINT TT))))) 
(PUT 'GROEBMESS37 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBMESS37 'DEFINED-ON-LINE '293) 
(PUT 'GROEBMESS37 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'GROEBMESS37 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBMESS37 (S)
    (COND
     (*TRGROEB
      (PROGN
       (COND ((NOT S) (PRIN2 " NOT ")))
       (PRIN2 " taking initials ")
       (TERPRI)
       (TERPRI))))) 
(PUT 'PRINTBL 'NUMBER-OF-ARGS 0) 
(PUT 'PRINTBL 'DEFINED-ON-LINE '298) 
(PUT 'PRINTBL 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'PRINTBL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINTBL NIL (PRINTB (IDIFFERENCE (LINELENGTH NIL) 2))) 
(PUT 'PRINTB 'NUMBER-OF-ARGS 1) 
(PUT 'PRINTB 'DEFINED-ON-LINE '300) 
(PUT 'PRINTB 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'PRINTB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINTB (N)
    (PROGN
     (PROG (I)
       (SETQ I 1)
      LAB
       (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
       (PRIN2 "-")
       (SETQ I (PLUS2 I 1))
       (GO LAB))
     (TERPRI))) 
(PUT 'VDPPRINTSET 'NUMBER-OF-ARGS 1) 
(PUT 'VDPPRINTSET 'DEFINED-ON-LINE '302) 
(PUT 'VDPPRINTSET 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'VDPPRINTSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPPRINTSET (L)
    (COND
     (L
      (PROGN
       (PRIN2 " { ")
       (VDPPRIN2 (CAR L))
       (PROG (X)
         (SETQ X (CDR L))
        LAB
         (COND ((NULL X) (RETURN NIL)))
         ((LAMBDA (X) (PROGN (PRIN2 ";") (VDPPRIN2 X))) (CAR X))
         (SETQ X (CDR X))
         (GO LAB))
       (PRIN2T " } "))))) 
(PUT 'VDPPRIN2L 'NUMBER-OF-ARGS 1) 
(PUT 'VDPPRIN2L 'DEFINED-ON-LINE '307) 
(PUT 'VDPPRIN2L 'DEFINED-IN-FILE 'GROEBNER/GROEBMES.RED) 
(PUT 'VDPPRIN2L 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPPRIN2L (U)
    (PROGN
     (PRIN2 "(")
     (VDPPRIN2 (CAR U))
     (PROG (X)
       (SETQ X (CDR U))
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (PROGN (PRIN2 ",") (VDPPRIN2 X))) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (RIN2 ")"))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BUCHBG)) 
(FLAG
 '(GROEBRESTRICTION GROEBRESMAX GVARSLAST GROEBMONFAC GROEBPROTFILE GLTERMS)
 'SHARE) 
(SETQ GROEBRESTRICTION (PROGN (SETQ ALGLIST* (CONS NIL NIL)) NIL)) 
(SETQ GROEBRESMAX (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 300)) 
(SETQ GROEBMONFAC (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 1)) 
(SETQ GROECONTCOUNT* 10) 
(SETQ *GSUGAR T) 
(SETQ *GROELTERMS T) 
(SETQ *GROEBFULLREDUCTION T) 
(SETQ *GROEBDIVIDE T) 
(SWITCH
 (LIST 'GROEBOPT 'TRGROEB 'TRGROEBS 'TRGROEB1 'TRGROEBR 'GROEBFULLREDUCTION
       'GROEBSTAT 'GROEBPROT)) 
(PUT 'BUCHVEVDIVIDES? 'NUMBER-OF-ARGS 2) 
(PUT 'BUCHVEVDIVIDES? 'DEFINED-ON-LINE '64) 
(PUT 'BUCHVEVDIVIDES? 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'BUCHVEVDIVIDES? 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BUCHVEVDIVIDES? (VEV1 VEV2)
    (AND (VEVMTEST? VEV2 VEV1)
         (OR (NULL GMODULE*) (GEVCOMPATIBLE1 VEV1 VEV2 GMODULE*)))) 
(PUT 'GEVCOMPATIBLE1 'NUMBER-OF-ARGS 3) 
(PUT 'GEVCOMPATIBLE1 'DEFINED-ON-LINE '69) 
(PUT 'GEVCOMPATIBLE1 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GEVCOMPATIBLE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEVCOMPATIBLE1 (V1 V2 G)
    (COND ((NULL G) T) ((NULL V1) (OR (NULL V2) (GEVCOMPATIBLE1 '(0) V2 G)))
          ((NULL V2) (GEVCOMPATIBLE1 V1 '(0) G))
          (T
           (AND (OR (EQUAL (CAR G) 0) (EQUAL (CAR V1) (CAR V2)))
                (GEVCOMPATIBLE1 (CDR V1) (CDR V2) (CDR G)))))) 
(PUT 'GCOMPATIBLE 'NUMBER-OF-ARGS 2) 
(PUT 'GCOMPATIBLE 'DEFINED-ON-LINE '77) 
(PUT 'GCOMPATIBLE 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GCOMPATIBLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCOMPATIBLE (F H)
    (OR (NULL GMODULE*) (GEVCOMPATIBLE1 (CADR F) (CADR H) GMODULE*))) 
(PUT 'GROEBMAKEPAIR 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMAKEPAIR 'DEFINED-ON-LINE '83) 
(PUT 'GROEBMAKEPAIR 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBMAKEPAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMAKEPAIR (F H)
    (PROG (TTT SF SH)
      (SETQ TTT (VEVLCM (CADR F) (CADR H)))
      (RETURN
       (COND
        (*GSUGAR
         (PROGN
          (SETQ SF (IPLUS2 (GSUGAR F) (VEVTDEG (VEVDIF TTT (CADR F)))))
          (SETQ SH (IPLUS2 (GSUGAR H) (VEVTDEG (VEVDIF TTT (CADR H)))))
          (LIST TTT F H (MAX SF SH))))
        (T (LIST TTT F H)))))) 
(FLUID '(VDPONE*)) 
(PUT 'VDPONEPOL 'NUMBER-OF-ARGS 0) 
(PUT 'VDPONEPOL 'DEFINED-ON-LINE '97) 
(PUT 'VDPONEPOL 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'VDPONEPOL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE VDPONEPOL NIL
    (SETQ VDPONE* (VDPFMON (A2BC 1) (VEVMAPTOZERO1 VDPVARS* NIL)))) 
(PUT 'GROEBNER2 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBNER2 'DEFINED-ON-LINE '101) 
(PUT 'GROEBNER2 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBNER2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBNER2 (P R)
    (PROG (GROETIME* TIM1 SPAC SPAC1 P1 FACTORTIME* PAIRSDONE* FACTORLEVEL*
           GROESFACTORS* *GCD)
      (SETQ FACTORTIME* 0)
      (SETQ GROETIME* (TIME))
      (VDPONEPOL)
      (SETQ HCOUNT* 0)
      (SETQ MCOUNT* 0)
      (SETQ FCOUNT* 0)
      (SETQ BCOUNT* 0)
      (SETQ B4COUNT* 0)
      (SETQ HZEROCOUNT* 0)
      (SETQ BASECOUNT* 0)
      (SETQ *GCD T)
      (SETQ GLTERMS (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (LIST 'LIST)))
      (SETQ GROECONTCOUNT* 10)
      (COND
       (*TRGROEB
        (PROGN
         (PRIN2 "Groebner Calculation starting ")
         (TERPRIT 2)
         (PRIN2 " groebopt: ")
         (PRINT *GROEBOPT))))
      (SETQ SPAC (GCTIME))
      (SETQ P1
              (COND
               ((OR *GROEBFAC (NULL *GSUGAR))
                ((LAMBDA (*GSUGAR) (GROEBBASEIN P *GROEBFAC R)) NIL))
               (T (GTRAVERSO P NIL NIL))))
      (COND
       ((OR *TRGROEB *TRGROEBR *GROEBSTAT)
        (PROGN
         (SETQ SPAC1 (DIFFERENCE (GCTIME) SPAC))
         (TERPRI)
         (PRIN2T "statistics for GROEBNER calculation")
         (PRIN2T "===================================")
         (PRIN2 " total computing time(including gc): ")
         (PRIN2 (DIFFERENCE (SETQ TIM1 (TIME)) GROETIME*))
         (PRIN2T "          milliseconds  ")
         (COND
          ((NEQ FACTORTIME* 0)
           (PROGN
            (PRIN2 "(time spent in FACTOR(excl. gc):    ")
            (PRIN2 FACTORTIME*)
            (PRIN2T "          milliseconds)"))))
         (PRIN2 "(time spent for garbage collection:  ")
         (PRIN2 SPAC1)
         (PRIN2T "          milliseconds)")
         (TERPRIT 1)
         (PRIN2 "H-polynomials total: ")
         (PRIN2T HCOUNT*)
         (PRIN2 "H-polynomials zero : ")
         (PRIN2T HZEROCOUNT*)
         (PRIN2 "Crit M hits: ")
         (PRIN2T MCOUNT*)
         (PRIN2 "Crit F hits: ")
         (PRIN2T FCOUNT*)
         (PRIN2 "Crit B hits: ")
         (PRIN2T BCOUNT*)
         (PRIN2 "Crit B4 hits: ")
         (PRIN2T B4COUNT*))))
      (RETURN P1))) 
(PUT 'TESTABORT 'NUMBER-OF-ARGS 1) 
(PUT 'TESTABORT 'DEFINED-ON-LINE '138) 
(PUT 'TESTABORT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'TESTABORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'TESTABORT 'SMACRO
      '(LAMBDA (H)
         (OR (VDPMEMBER H ABORT1)
             (EQUAL 'CANCEL (SETQ ABORT2 (GROEBTESTABORT H ABORT2)))))) 
(PUT 'GROEBENUMERATE 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBENUMERATE 'DEFINED-ON-LINE '142) 
(PUT 'GROEBENUMERATE 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBENUMERATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBENUMERATE (F)
    (COND ((OR (NULL F) (NULL (CADR (CDDR F)))) F)
          (T
           (PROGN
            (DIPCONDENSE (CAR (CDDDR F)))
            (COND
             ((NOT (VDPGETPROP F 'NUMBER))
              (PROGN
               (SETQ F
                       (VDPPUTPROP F 'NUMBER
                                   (SETQ PCOUNT* (IPLUS2 PCOUNT* 1))))
               (COND
                (*GROEBPROT
                 (PROGN
                  (GROEBPROTSETQ (MKID 'POLY PCOUNT*) 'CANDIDATE)
                  (VDPPUTPROP F 'GROEBPROT (MKID 'POLY PCOUNT*))))))))
            F)))) 
(PUT 'GROEBBASEIN 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBBASEIN 'DEFINED-ON-LINE '184) 
(PUT 'GROEBBASEIN 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBBASEIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBBASEIN (G0 FACT ABORT1)
    (PROG (ABORT2 D D1 D2 G GG G1 G99 H HLIST LASTH LV P PROBLEMS VARS_G P1
           RESULTS S X GVBC PROBCOUNT*)
      (SETQ GVBC 0)
      (SETQ PROBCOUNT* 0)
      (SETQ GROEBABORT* ABORT1)
      (SETQ LV (LENGTH VDPVARS*))
      (PROG (P)
        (SETQ P G0)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((OR (NULL P) (NULL (CADR (CDDR P)))) (SETQ G0 (DELETE P G0)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (*GROEBPREREDUCE (SETQ G0 (GROEBPREREDUCE G0))))
      (SETQ X
              (PROG (FJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FJ G0)
                (COND ((NULL FJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FJ)
                                    (PROGN
                                     (GROEBSAVELTERM FJ)
                                     (GSETSUGAR (VDPENUMERATE (VDPSIMPCONT FJ))
                                                NIL)))
                                  (CAR FJ))
                                 NIL)))
               LOOPLABEL
                (SETQ FJ (CDR FJ))
                (COND ((NULL FJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FJ)
                            (PROGN
                             (GROEBSAVELTERM FJ)
                             (GSETSUGAR (VDPENUMERATE (VDPSIMPCONT FJ)) NIL)))
                          (CAR FJ))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*GROEBPROT
        (PROG (F)
          (SETQ F X)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F)
             (PROGN
              (GROEBPROTSETQ (MKID 'POLY (SETQ H (VDPGETPROP F 'NUMBER)))
               (DIP2A (CADR (CDDR F))))
              (VDPPUTPROP F 'GROEBPROT (MKID 'POLY H))))
           (CAR F))
          (SETQ F (CDR F))
          (GO LAB))))
      (SETQ G0 X)
      (SETQ PROBLEMS
              (LIST
               (LIST NIL NIL NIL G0 ABORT1 NIL NIL VBCCURRENTMODE* NIL NIL)))
      (AND *TRGROEB (GROEBMESS1 G D))
      (GO MACROLOOP)
     MACROLOOP
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PROBLEMS (LESSP GVBC GROEBRESMAX))) (RETURN NIL)))
        (PROG ()
          (SETQ X (CAR PROBLEMS))
          (SETQ D (CAR X))
          (SETQ G (CADR X))
          (SETQ G99 (VDPLSORT (CADDR X)))
          (SETQ G0 (CADDDR X))
          (SETQ ABORT1 (NTH X 5))
          (SETQ ABORT2 (NTH X 6))
          (SETQ PAIRSDONE* (NTH X 7))
          (SETQ H (NTH X 8))
          (SETQ FACTORLEVEL* (NTH X 9))
          (SETQ GROESFACTORS* (NTH X 10))
          (SETQ PROBLEMS (CDR PROBLEMS))
          (SETQ G0
                  (COND
                   ((AND FACTORLEVEL* G0 (CDR G0))
                    (CONS (CAR G0) (VDPLSORT (CDR G0))))
                   (T (VDPLSORT G0))))
          (SETQ X NIL)
          (SETQ LASTH NIL)
          (AND *TRGROEB (GROEBMESS23 G0 ABORT1 ABORT2))
          (PROG ()
           WHILELABEL
            (COND ((NOT (OR D G0)) (RETURN NIL)))
            (PROG ()
              (COND ((GROEBFASTTEST G0 G D G99) (GO STOP)))
              (AND *TRGROEB (GROEBMESS50 G))
              (COND
               (G0
                (PROGN
                 (SETQ H (CAR G0))
                 (SETQ G0 (CDR G0))
                 (GSETSUGAR H NIL)
                 (GROEBSAVELTERM H)
                 (SETQ P (LIST NIL H H))))
               (T
                (PROGN
                 (SETQ P (CAR D))
                 (SETQ D (DELETE P D))
                 (SETQ S (GROEBSPOLYNOM (CADR P) (CADDR P)))
                 (COND
                  (FACT
                   (SETQ PAIRSDONE*
                           (CONS
                            (CONS (VDPGETPROP (CADR P) 'NUMBER)
                                  (VDPGETPROP (CADDR P) 'NUMBER))
                            PAIRSDONE*))))
                 (AND *TRGROEB (GROEBMESS3 P S))
                 (SETQ H (GROEBNORMALFORM0 S G99 'TREE FACT))
                 (GROEBSAVELTERM H)
                 (SETQ H (GROEBSIMPCONTNORMALFORM H))
                 (COND
                  ((OR (NULL H) (NULL (CADR (CDDR H))))
                   (AND *TRGROEB (GROEBMESS4 P D))))
                 (COND
                  ((NOT (OR (NULL H) (NULL (CADR (CDDR H)))))
                   (PROGN
                    (SETQ S (GROEBCHAIN H (CADR P) G99))
                    (COND ((EQUAL S H) (SETQ H (GROEBCHAIN H (CADDR P) G99))))
                    (COND
                     (SECONDVALUE* (SETQ G (DELETE SECONDVALUE* G))))))))))
              (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (GO BOTT)))
              (COND
               ((OR (NULL (CADR H))
                    (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
                (PROGN (AND *TRGROEB (GROEBMESS5 P H)) (GO STOP))))
              (COND
               ((OR (VDPMEMBER H ABORT1)
                    (EQUAL 'CANCEL (SETQ ABORT2 (GROEBTESTABORT H ABORT2))))
                (PROGN
                 (AND *TRGROEB (GROEBMESS19 H ABORT1 ABORT2))
                 (GO STOP))))
              (SETQ S NIL)
              (SETQ HLIST NIL)
              (COND
               (GROEBRESTRICTION*
                (SETQ HLIST (GROEBTESTRESTRICTION H ABORT1))))
              (COND
               ((AND (NOT HLIST) FACT)
                (SETQ HLIST (GROEBFACTORIZE H ABORT1 G G99))))
              (COND ((EQUAL HLIST 'ZERO) (GO BOTT)))
              (COND (GROEFEEDBACK* (SETQ G0 (APPEND GROEFEEDBACK* G0))))
              (SETQ GROEFEEDBACK* NIL)
              (COND
               ((AND HLIST (EQUAL (LENGTH HLIST) 2))
                (PROGN (SETQ H (CAR (CADR HLIST))) (SETQ HLIST NIL))))
              (COND
               (HLIST
                (PROGN
                 (COND
                  ((NEQ HLIST 'CANCEL)
                   (SETQ PROBLEMS
                           (GROEBENCAPSULATE HLIST D G0 G G99 ABORT1 ABORT2
                            PROBLEMS FACT))))
                 (GO STOP))))
              (SETQ H (GROEBENUMERATE H))
              (AND *TRGROEB (GROEBMESS5 P H))
              (SETQ D1 NIL)
              (AND *TRGROEB (GROEBMESS50 G))
              (SETQ GG G)
              (SETQ VARS_G (VARIABLES G))
              (PROG (F)
                (SETQ F G)
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F)
                   (COND
                    ((AND
                      (OR (CAR P)
                          (NOT
                           (MEMBER
                            (CONS (VDPGETPROP H 'NUMBER)
                                  (VDPGETPROP F 'NUMBER))
                            PAIRSDONE*)))
                      (GCOMPATIBLE F H))
                     (PROGN
                      (SETQ D1 (GROEBCPLISTSORTIN (GROEBMAKEPAIR F H) D1))
                      (COND
                       ((EQUAL (VEVLCM (CADR F) (CADR H)) (CADR F))
                        (PROGN
                         (SETQ G (DELETE F G))
                         (AND *TRGROEB (GROEBMESS2 F)))))))))
                 (CAR F))
                (SETQ F (CDR F))
                (GO LAB))
              (COND ((NEQ VARS_G (VARIABLES G)) (SETQ G GG)))
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
              (COND
               ((AND (LESSP (DIPLENGTH (CADR (CDDR H))) 3) (CAR P))
                (PROGN
                 (SETQ G (GROEBSECONDARYREDUCTION H G G99 D NIL T))
                 (COND ((EQUAL G 'ABORT) (GO STOP)))
                 (SETQ G99 SECONDVALUE*)
                 (SETQ D THIRDVALUE*))))
              (SETQ G (CONS H G))
              (SETQ LASTH H)
              (SETQ G99 (GROEBLISTADD H G99))
              (AND *TRGROEB (GROEBMESS8 G D))
              (GO BOTT)
             STOP
              (SETQ D (SETQ G (SETQ G0 NIL)))
             BOTT)
            (GO WHILELABEL))
          (SETQ G (VDPLSORT G))
          (SETQ X (GROEBBASEIN2 G G99 PROBLEMS ABORT1 ABORT2 FACT))
          (SETQ G1 (CAR X))
          (SETQ PROBLEMS (CDR X))
          (COND
           (G1
            (PROGN
             (SETQ RESULTS (CONS G1 RESULTS))
             (SETQ GVBC (PLUS GVBC 1)))))
          (AND *TRGROEB (GROEBMESS13 G1 PROBLEMS)))
        (GO WHILELABEL))
      (COND
       ((GEQ GVBC GROEBRESMAX)
        (LPRIW "########" "warning: GROEBRESMAX limit reached")))
      (RETURN (GROEBBASEIN3 RESULTS)))) 
(PUT 'VARIABLES 'NUMBER-OF-ARGS 1) 
(PUT 'VARIABLES 'DEFINED-ON-LINE '305) 
(PUT 'VARIABLES 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'VARIABLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARIABLES (G)
    (PROG (L VAR_COUNT EV F CVARS VARS)
      (SETQ L 0)
      (SETQ VAR_COUNT 0)
      (COND ((NULL G) (RETURN NIL)))
      (SETQ L (LENGTH DIPVARS*))
      (SETQ VARS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE L I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NIL NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE L I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
     B
      (SETQ F (CADR (CDDR (CAR G))))
     C
      (SETQ EV (CAR F))
      (SETQ CVARS VARS)
     D
      (COND
       ((AND (IGREATERP (CAR EV) 0) (NULL (CAR CVARS)))
        (PROGN (SETCAR CVARS T) (SETQ VAR_COUNT (IPLUS2 VAR_COUNT 1)))))
      (COND ((EQ VAR_COUNT L) (RETURN VARS)))
      (SETQ EV (CDR EV))
      (COND (EV (PROGN (SETQ CVARS (CDR CVARS)) (GO D))))
      (SETQ F (CDDR F))
      (COND (F (GO C)))
      (SETQ G (CDR G))
      (COND (G (GO B)))
      (RETURN VARS))) 
(PUT 'GROEBFASTTEST 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBFASTTEST 'DEFINED-ON-LINE '325) 
(PUT 'GROEBFASTTEST 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBFASTTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBFASTTEST (G0 G D G99)
    (PROGN (SETQ G (SETQ G0 (SETQ D (SETQ G99 NIL)))) NIL)) 
(PUT 'GROEBBASEIN2 'NUMBER-OF-ARGS 6) 
(PUT 'GROEBBASEIN2 'DEFINED-ON-LINE '328) 
(PUT 'GROEBBASEIN2 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBBASEIN2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBBASEIN2 (G G99 PROBLEMS ABORT1 ABORT2 FACT)
    (PROG (F G1 *GROEBFULLREDUCTION *GROEBHEUFACT *GSUGAR H HLIST X CNT)
      (SETQ CNT 0)
      (SETQ *GROEBFULLREDUCTION T)
      (SETQ G1 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ H (CAR G))
         (SETQ G (CDR G))
         (COND
          (*GROEBPROT
           (GROEBPROTSETQ 'CANDIDATE (MKID 'POLY (VDPGETPROP H 'NUMBER)))))
         (SETQ H (GROEBNORMALFORM0 H G 'SORT NIL))
         (SETQ F (GROEBSIMPCONTNORMALFORM H))
         (AND *TRGROEB (GROEBMESS26 H F))
         (COND
          (*GROEBPROT
           (GROEBPROTSETQ (LIST 'GB (SETQ CNT (PLUS CNT 1))) 'CANDIDATE)))
         (COND
          ((AND (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                (OR (NULL (CADR F))
                    (AND (EQUAL (CAR (CADR F)) 0) (VEVZERO?1 (CDR (CADR F))))))
           (PROGN (SETQ G1 (SETQ G NIL)))))
         (COND
          ((AND FACT (NOT (OR (NULL F) (NULL (CADR (CDDR F))))))
           (PROGN
            (SETQ HLIST (GROEBFACTORIZE F ABORT1 NIL NIL))
            (COND
             ((NOT (NULL HLIST))
              (PROGN
               (SETQ HLIST
                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                         (SETQ X (CDR HLIST))
                         (COND ((NULL X) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (X) (CAR X)) (CAR X))
                                               NIL)))
                        LOOPLABEL
                         (SETQ X (CDR X))
                         (COND ((NULL X) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (PROG (H)
                 (SETQ H HLIST)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (COND
                     ((VDPMEMBER H ABORT1)
                      (PROGN
                       (SETQ HLIST (DELETE H HLIST))
                       (AND *TRGROEB (GROEBMESS19 H ABORT1 ABORT2))))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (SETQ X 0)
               (PROG (H)
                 (SETQ H HLIST)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (SETQ HLIST (DELETE H HLIST))
                     (SETQ H (GROEBENUMERATE H))
                     (SETQ PROBLEMS
                             (CONS
                              (LIST NIL (APPEND G1 G) G99 (LIST H)
                                    (APPEND HLIST ABORT1) ABORT2 PAIRSDONE*
                                    VBCCURRENTMODE*
                                    (CONS (SETQ X (PLUS X 1)) FACTORLEVEL*)
                                    GROESFACTORS*)
                              PROBLEMS))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (SETQ G (SETQ G1 NIL))
               (SETQ F NIL)))))))
         (COND
          ((AND F (NEQ (CADR H) (CADR F)))
           (PROGN (SETQ G (VDPLSORT (APPEND G (CONS F G1)))) (SETQ G1 NIL)))
          ((AND F (NOT (OR (NULL F) (NULL (CADR (CDDR F))))))
           (SETQ G1 (APPEND G1 (LIST F))))))
        (GO WHILELABEL))
      (RETURN (CONS G1 PROBLEMS)))) 
(PUT 'GROEBBASEIN3 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBBASEIN3 'DEFINED-ON-LINE '376) 
(PUT 'GROEBBASEIN3 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBBASEIN3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBBASEIN3 (RESULTS)
    (PROG (X G F P1 P2)
      (SETQ X NIL)
      (SETQ G RESULTS)
      (SETQ P1 (SETQ P2 0))
      (PROG ()
       WHILELABEL
        (COND ((NOT RESULTS) (RETURN NIL)))
        (PROGN
         (COND
          (((LAMBDA (P)
              (AND (NOT (OR (NULL P) (NULL (CADR (CDDR P)))))
                   (OR (NULL (CADR P))
                       (AND (EQUAL (CAR (CADR P)) 0)
                            (VEVZERO?1 (CDR (CADR P)))))))
            (CAR (CAR RESULTS)))
           (SETQ P1 (PLUS P1 1)))
          (T
           (PROGN
            (SETQ F
                    (PROG (P FORALL-RESULT FORALL-ENDPTR)
                      (SETQ P (CAR RESULTS))
                      (COND ((NULL P) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (P) (VDPREMALLPROPS P))
                                        (CAR P))
                                       NIL)))
                     LOOPLABEL
                      (SETQ P (CDR P))
                      (COND ((NULL P) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (P) (VDPREMALLPROPS P)) (CAR P))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND ((MEMBER F X) (SETQ P2 (PLUS P2 1)))
                  ((NOT (GROEBABORTID F GROEBABORT*)) (SETQ X (CONS F X))))
            (SETQ RESULTS (CDR RESULTS))))))
        (GO WHILELABEL))
      (SETQ RESULTS (COND ((NULL X) (LIST (LIST VDPONE*))) (T X)))
      (RETURN RESULTS))) 
(FLUID '(*VBCCOMPRESS)) 
(PUT 'GROEBCHAIN 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBCHAIN 'DEFINED-ON-LINE '396) 
(PUT 'GROEBCHAIN 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBCHAIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBCHAIN (H F G99)
    (PROG (COUNT FOUND H1 H2 H3)
      (SETQ SECONDVALUE* NIL)
      (RETURN H)
      (COND ((NOT (BUCHVEVDIVIDES? (CADR H) (CADR F))) (RETURN H)))
      (SETQ H2 H)
      (SETQ H1 F)
      (SETQ FOUND T)
      (SETQ COUNT 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT FOUND) (RETURN NIL)))
        (PROGN
         (SETQ H3 (GROEBSPOLYNOM H1 H2))
         (SETQ H3 (GROEBNORMALFORM0 H3 G99 'TREE T))
         (SETQ H3 (VDPSIMPCONT H3))
         (COND
          ((OR (NULL H3) (NULL (CADR (CDDR H3))))
           (PROGN
            (SETQ FOUND NIL)
            (PRIN2T "chain---------------------------")
            (VDPPRINT H1)
            (VDPPRINT H2)
            (VDPPRINT H3)
            (SETQ SECONDVALUE* F)
            (SETQ COUNT 9999)))
          ((AND (NOT (OR (NULL H3) (NULL (CADR (CDDR H3)))))
                (OR (NULL (CADR H3))
                    (AND (EQUAL (CAR (CADR H3)) 0)
                         (VEVZERO?1 (CDR (CADR H3))))))
           (PROGN
            (SETQ FOUND NIL)
            (PRIN2T "chain---------------------------")
            (VDPPRINT H1)
            (VDPPRINT H2)
            (VDPPRINT H3)
            (SETQ H2 H3)
            (SETQ COUNT 9999)))
          ((BUCHVEVDIVIDES? (CADR H3) (CADR H2))
           (PROGN
            (SETQ FOUND T)
            (PRIN2T "chain---------------------------")
            (VDPPRINT H1)
            (VDPPRINT H2)
            (VDPPRINT H3)
            (SETQ H1 H2)
            (SETQ H2 H3)
            (SETQ COUNT (PLUS COUNT 1))))
          (T (SETQ FOUND NIL))))
        (GO WHILELABEL))
      (RETURN
       (COND ((GREATERP COUNT 0) (PROGN (PRIN2 "CHAIN :") (PRIN2T COUNT) H2))
             (T H))))) 
(PUT 'GROEBENCAPSULATE 'NUMBER-OF-ARGS 9) 
(PUT 'GROEBENCAPSULATE 'DEFINED-ON-LINE '428) 
(PUT 'GROEBENCAPSULATE 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBENCAPSULATE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE GROEBENCAPSULATE (HLIST D G0 G G99 ABORT1 ABORT2 PROBLEMS FACT)
    (PROG (FACTL U Y Z FC)
      (SETQ FC 0)
      (COND
       ((OR (GREATERP (LENGTH VDPVARS*) 10) (NEQ (CAR HLIST) 'FACTOR))
        (RETURN
         (GROEBENCAPSULATEHARDCASE HLIST D G0 G G99 ABORT1 ABORT2 PROBLEMS
          FACT))))
      (SETQ FACTL (GROEBRECFACTL (LIST HLIST)))
      (AND *TRGROEB (GROEBMESS22 FACTL ABORT1 ABORT2))
      (PROG (X)
        (SETQ X (REVERSE FACTL))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ Y (APPEND (CAR X) G0))
            (SETQ Z (VDPUNION (CADR X) ABORT1))
            (SETQ U (APPEND (CADDR X) ABORT2))
            (SETQ PROBLEMS
                    (CONS
                     (LIST D G G Y Z U PAIRSDONE* VBCCURRENTMODE*
                           (CONS (SETQ FC (PLUS FC 1)) FACTORLEVEL*)
                           GROESFACTORS*)
                     PROBLEMS))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN PROBLEMS))) 
(PUT 'GROEBENCAPSULATEHARDCASE 'NUMBER-OF-ARGS 9) 
(PUT 'GROEBENCAPSULATEHARDCASE 'DEFINED-ON-LINE '458) 
(PUT 'GROEBENCAPSULATEHARDCASE 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBENCAPSULATEHARDCASE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE GROEBENCAPSULATEHARDCASE (HLIST D G0 G G99 ABORT1 ABORT2 PROBLEMS FACT)
    (PROG (FACTL FACTR BREAK D1 D2 F FL1 GC H P PD P1 S U Y Z FC)
      (SETQ FC 0)
      (SETQ FACTL (LIST HLIST))
      (SETQ FACTR (VDPSPACE (CAR (CADR HLIST))))
      (PROG (X)
        (SETQ X (CDR HLIST))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (P)
             (SETQ P X)
            LAB
             (COND ((NULL P) (RETURN NIL)))
             ((LAMBDA (P) (SETQ FACTR (VEVUNION FACTR (VDPSPACE P)))) (CAR P))
             (SETQ P (CDR P))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (OR D G0)) (RETURN NIL)))
        (PROG ()
          (SETQ BREAK NIL)
          (COND
           (G0
            (PROGN
             (SETQ S (CAR G0))
             (SETQ G0 (CDR G0))
             (SETQ P (LIST NIL S S))))
           (T
            (PROGN
             (SETQ P (CAR D))
             (SETQ D (DELETE P D))
             (COND ((NOT (VDPORTHSPACEP (CAR P) FACTR)) (SETQ S NIL))
                   (T
                    (PROGN
                     (SETQ S (GROEBSPOLYNOM (CADR P) (CADDR P)))
                     (AND *TRGROEB (GROEBMESS3 P S))))))))
          (COND
           ((OR (NULL S) (NOT (VDPORTHSPACEP (CADR S) FACTR)))
            (PROGN
             (SETQ F (CADR P))
             (COND ((NOT (VDPMEMBER3 F G0 G GC)) (SETQ GC (CONS F GC))))
             (SETQ F (CADDR P))
             (COND
              ((AND (CAR P) (NOT (VDPMEMBER3 F G0 G GC)))
               (SETQ GC (CONS F GC))))
             (GO BOTT))))
          (SETQ H (GROEBNORMALFORM S G99 'TREE))
          (COND
           ((AND (OR (NULL H) (NULL (CADR (CDDR H)))) (CAR P))
            (AND *TRGROEB (GROEBMESS4 P D))))
          (COND
           ((NOT (VDPORTHSPACEP (CADR H) FACTR))
            (PROGN
             (SETQ F (CADR P))
             (COND ((NOT (VDPMEMBER3 F G0 G GC)) (SETQ GC (CONS F GC))))
             (SETQ F (CADDR P))
             (COND
              ((AND (CAR P) (NOT (VDPMEMBER3 F G0 G GC)))
               (SETQ GC (CONS F GC))))
             (GO BOTT))))
          (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (GO BOTT)))
          (COND
           ((OR (NULL (CADR H))
                (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
            (GO STOP)))
          (SETQ H (GROEBSIMPCONTNORMALFORM H))
          (COND
           ((OR (VDPMEMBER H ABORT1)
                (EQUAL 'CANCEL (SETQ ABORT2 (GROEBTESTABORT H ABORT2))))
            (PROGN (AND *TRGROEB (GROEBMESS19 H ABORT1 ABORT2)) (GO STOP))))
          (SETQ S NIL)
          (SETQ HLIST NIL)
          (COND
           (GROEBRESTRICTION* (SETQ HLIST (GROEBTESTRESTRICTION H ABORT1))))
          (COND ((EQUAL HLIST 'CANCEL) (GO STOP)))
          (COND
           ((AND (NOT HLIST) FACT)
            (SETQ HLIST (GROEBFACTORIZE H ABORT1 G G99))))
          (COND (GROEFEEDBACK* (SETQ G0 (APPEND GROEFEEDBACK* G0))))
          (SETQ GROEFEEDBACK* NIL)
          (COND
           ((AND HLIST (EQUAL (LENGTH HLIST) 2))
            (PROGN (SETQ H (CAR (CADR HLIST))) (SETQ HLIST NIL))))
          (COND
           (HLIST
            (PROGN
             (PROG (X)
               (SETQ X (CDR HLIST))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (PROG (H)
                    (SETQ H X)
                   LAB
                    (COND ((NULL H) (RETURN NIL)))
                    ((LAMBDA (H) (SETQ FACTR (VEVUNION FACTR (VDPSPACE H))))
                     (CAR H))
                    (SETQ H (CDR H))
                    (GO LAB)))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (SETQ FACTL (CONS HLIST FACTL))
             (GO BOTT))))
          (SETQ H (GROEBENUMERATE H))
          (AND *TRGROEB (GROEBMESS5 P H))
          (SETQ D1 NIL)
          (PROG (F)
            (SETQ F G)
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F)
               (COND
                ((AND (EQUAL (VEVLCM (CADR F) (CADR H)) (CADR F))
                      (GCOMPATIBLE F H))
                 (PROGN
                  (SETQ G (DELETE F G))
                  (SETQ D1 (GROEBCPLISTSORTIN (GROEBMAKEPAIR F H) D1))
                  (AND *TRGROEB (GROEBMESS2 F))))))
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
          (COND
           ((LESSP (DIPLENGTH (CADR (CDDR H))) 3)
            (PROGN
             (SETQ G (GROEBSECONDARYREDUCTION H G G99 D GC T))
             (COND ((EQUAL G 'ABORT) (GO STOP)))
             (SETQ G99 SECONDVALUE*)
             (SETQ D THIRDVALUE*)
             (SETQ GC FOURTHVALUE*))))
          (SETQ G (CONS H G))
          (SETQ G99 (GROEBLISTADD H G99))
          (AND *TRGROEB (GROEBMESS8 G D))
          (GO BOTT)
         STOP
          (SETQ D (SETQ G (SETQ G0 (SETQ GC (SETQ FACTL NIL)))))
         BOTT)
        (GO WHILELABEL))
      (SETQ G0 (VDPUNION G0 (VDPUNION G GC)))
      (COND
       (FACTL
        (PROGN
         (SETQ FACTL (GROEBRECFACTL FACTL))
         (AND *TRGROEB (GROEBMESS22 FACTL ABORT1 ABORT2)))))
      (PROG (X)
        (SETQ X (REVERSE FACTL))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ FL1 (CONS (SETQ FC (PLUS FC 1)) FACTORLEVEL*))
            (SETQ BREAK NIL)
            (SETQ Y (APPEND (CAR X) G0))
            (SETQ Z (VDPUNION (CADR X) ABORT1))
            (SETQ U (APPEND (CADDR X) ABORT2))
            (COND ((VDPMEMBER VDPONE* Y) (SETQ BREAK VDPONE*)))
            (COND
             ((NOT BREAK)
              (PROG (P)
                (SETQ P Z)
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P) (COND ((VDPMEMBER P Y) (SETQ BREAK P)))) (CAR P))
                (SETQ P (CDR P))
                (GO LAB))))
            (COND
             ((NOT BREAK)
              (PROGN
               (SETQ Y (APPEND (CAR X) (GROEBREDUCEFROMFACTORS G0 (CAR X))))
               (SETQ PD SECONDVALUE*)
               (COND ((VDPMEMBER VDPONE* Y) (SETQ BREAK VDPONE*))
                     (T
                      (PROG (P)
                        (SETQ P Z)
                       LAB
                        (COND ((NULL P) (RETURN NIL)))
                        ((LAMBDA (P) (COND ((VDPMEMBER P Y) (SETQ BREAK P))))
                         (CAR P))
                        (SETQ P (CDR P))
                        (GO LAB))))
               (SETQ PD (SUBLA PD PAIRSDONE*)))))
            (COND
             ((NOT BREAK)
              (SETQ PROBLEMS
                      (CONS
                       (LIST NIL NIL NIL Y Z U NIL VBCCURRENTMODE* FL1
                             GROESFACTORS*)
                       PROBLEMS)))
             (T (AND *TRGROEB (GROEBMESS19A BREAK FL1))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN PROBLEMS))) 
(PUT 'GROEBRECFACTL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBRECFACTL 'DEFINED-ON-LINE '592) 
(PUT 'GROEBRECFACTL 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBRECFACTL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBRECFACTL (FACTL)
    (PROG (RF RES TYPE)
      (COND ((NULL FACTL) (RETURN (LIST (LIST NIL NIL NIL)))))
      (SETQ RF (GROEBRECFACTL (CDR FACTL)))
      (SETQ FACTL (CAR FACTL))
      (SETQ TYPE (CAR FACTL))
      (SETQ FACTL (CDR FACTL))
      (PROG ()
       WHILELABEL
        (COND ((NOT FACTL) (RETURN NIL)))
        (PROGN
         (PROG (Y)
           (SETQ Y RF)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (COND
               ((VDPDISJOINT? (CAR FACTL) (CADR Y))
                (SETQ RES
                        (CONS
                         (LIST (VDPUNION (CAR FACTL) (CAR Y))
                               (COND
                                ((EQUAL TYPE 'FACTOR)
                                 (APPEND
                                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ X (CDR FACTL))
                                    (COND ((NULL X) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X) (CAR X))
                                                      (CAR X))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ X (CDR X))
                                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (X) (CAR X)) (CAR X))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))
                                  (CADR Y)))
                                (T (CADR Y)))
                               (COND
                                ((NEQ TYPE 'FACTOR)
                                 (APPEND (CDR FACTL) (CADDR Y)))
                                (T (CADDR Y))))
                         RES)))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (SETQ FACTL (CDR FACTL)))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'GROEBTESTABORT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBTESTABORT 'DEFINED-ON-LINE '613) 
(PUT 'GROEBTESTABORT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBTESTABORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBTESTABORT (H ABORT2)
    (PROG (X BREAK RES)
      (SETQ X ABORT2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND X (NOT BREAK))) (RETURN NIL)))
        (PROGN (COND ((VDPMEMBER H (CAR X)) (SETQ BREAK T))) (SETQ X (CDR X)))
        (GO WHILELABEL))
      (COND ((NOT BREAK) (RETURN ABORT2)))
      (SETQ BREAK NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ABORT2 (NOT BREAK))) (RETURN NIL)))
        (PROGN
         (SETQ X (VDPDELETEMEMBER H (CAR ABORT2)))
         (COND ((NULL X) (SETQ BREAK T)))
         (SETQ RES (CONS X RES))
         (SETQ ABORT2 (CDR ABORT2)))
        (GO WHILELABEL))
      (AND *TRGROEB (GROEBMESS25 H RES))
      (COND (BREAK (RETURN 'CANCEL)))
      (RETURN RES))) 
(PUT 'GROEBNORMALFORM 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBNORMALFORM 'DEFINED-ON-LINE '637) 
(PUT 'GROEBNORMALFORM 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBNORMALFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBNORMALFORM (F G TYPE) (GROEBNORMALFORM0 F G TYPE NIL)) 
(PUT 'GROEBNORMALFORM0 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBNORMALFORM0 'DEFINED-ON-LINE '640) 
(PUT 'GROEBNORMALFORM0 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBNORMALFORM0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBNORMALFORM0 (F G TYPE M)
    (PROG (A BREAK C DIVISOR DONE F0 F1 F2 FOLD GL VEV N S1 S2 ZZZ)
      (SETQ N 0)
      (SETQ S1 0)
      (SETQ S2 0)
      (COND
       ((AND *GROEBWEAK *VDPINTEGER (GROEBWEAKZEROTEST F G TYPE))
        (RETURN (F2VDP NIL))))
      (SETQ FOLD F)
      (SETQ F1 (A2VDP 0))
      (SETQ A (BCFD 1))
      (GSETSUGAR F1 (GSUGAR F))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))) (RETURN NIL)))
        (PROG ()
          (SETQ VEV (CADR F))
          (SETQ C (CADDR F))
          (COND
           ((AND (NOT *GROEBFULLREDUCTION)
                 (NOT (OR (NULL F1) (NULL (CADR (CDDR F1))))))
            (SETQ G NIL)))
          (COND
           ((NULL G)
            (PROGN
             (SETQ F1 (VDPSUM F1 F))
             (SETQ F (A2VDP 0))
             (SETQ BREAK T)
             (SETQ DIVISOR NIL)
             (GO READY))))
          (SETQ DIVISOR (GROEBSEARCHINLIST VEV G))
          (COND
           (DIVISOR
            (PROGN
             (SETQ DONE T)
             (COND
              ((AND M (EQUAL VDPSORTMODE* 'REVGRADLEX)
                    (OR (NULL F1) (NULL (CADR (CDDR F1)))))
               (SETQ GL (CONS F GL))))
             (COND
              (*TRGROEBS
               (PROGN (PRIN2 "//-") (PRIN2 (VDPGETPROP DIVISOR 'NUMBER))))))))
          (COND
           (DIVISOR
            (COND
             ((EQUAL (DIPLENGTH (CADR (CDDR DIVISOR))) 1)
              (SETQ F (VDPCANCELMVEV F (CADR DIVISOR))))
             ((OR *VDPINTEGER (NOT *GROEBDIVIDE))
              (PROGN
               (SETQ F (GROEBREDUCEONESTEPINT F F1 C VEV DIVISOR))
               (SETQ F1 SECONDVALUE*)
               (SETQ N (PLUS N 1))
               (COND
                ((AND (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                      (IGREATERP N GROECONTCOUNT*))
                 (PROGN
                  (SETQ F0 F)
                  (SETQ F (GROEBSIMPCONT2 F F1))
                  (GROECONTENTCONTROL (NEQ F F0))
                  (SETQ F1 SECONDVALUE*)
                  (SETQ N 0))))))
             (T (SETQ F (GROEBREDUCEONESTEPRAT F NIL C VEV DIVISOR)))))
           (T
            (PROGN
             (AND *GSUGAR (PROGN (SETQ S1 (GSUGAR F)) (SETQ S2 (GSUGAR F1))))
             (SETQ F1 (VDPAPPENDMON F1 (CADDR F) (CADR F)))
             (SETQ F (VDPRED F))
             (AND *GSUGAR
                  (PROGN (GSETSUGAR F S1) (GSETSUGAR F1 (MAX S2 S1)))))))
         READY)
        (GO WHILELABEL))
      (COND (*GROEBPROT (GROEBREDUCTIONPROTOCOLBORDER)))
      (COND ((OR (NULL F1) (NULL (CADR (CDDR F1)))) (GO RET)))
      (SETQ ZZZ F1)
      (COND ((NOT DONE) (SETQ F1 FOLD))
            ((AND M (EQUAL VDPSORTMODE* 'REVGRADLEX))
             (PROGN
              (COND
               ((NOT (OR (NULL F1) (NULL (CADR (CDDR F1)))))
                (SETQ GL (CONS F1 GL))))
              (PROG ()
               WHILELABEL
                (COND ((NOT GL) (RETURN NIL)))
                (PROGN
                 (SETQ F2 (GROEBNORMALFORMSELECT (CAR GL)))
                 (COND (F2 (PROGN (SETQ F1 F2) (SETQ GL NIL)))
                       (T (SETQ GL (CDR GL)))))
                (GO WHILELABEL)))))
     RET
      (RETURN F1))) 
(PUT 'GROECONTENTCONTROL 'NUMBER-OF-ARGS 1) 
(PUT 'GROECONTENTCONTROL 'DEFINED-ON-LINE '697) 
(PUT 'GROECONTENTCONTROL 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROECONTENTCONTROL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROECONTENTCONTROL (U)
    (SETQ GROECONTCOUNT*
            (COND ((NOT (NUMBERP GROECONTCOUNT*)) 10)
                  (U (MAX 0 (DIFFERENCE GROECONTCOUNT* 1)))
                  (T (MIN 10 (PLUS GROECONTCOUNT* 1)))))) 
(PUT 'GROEBVBCBIG? 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBVBCBIG? 'DEFINED-ON-LINE '704) 
(PUT 'GROEBVBCBIG? 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBVBCBIG? 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBVBCBIG? (A)
    ((LAMBDA (X)
       (COND
        ((NUMBERP X)
         (OR (GREATERP X 1000000000000) (LESSP X (MINUS 1000000000000))))
        (T T)))
     (VBCNUMBER A))) 
(PUT 'GROEBNORMALFORMSELECT 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNORMALFORMSELECT 'DEFINED-ON-LINE '709) 
(PUT 'GROEBNORMALFORMSELECT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBNORMALFORMSELECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNORMALFORMSELECT (V) (COND ((IGREATERP (COUNTLASTVAR V T) 0) V))) 
(PUT 'GROEBSIMPCONTNORMALFORM 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBSIMPCONTNORMALFORM 'DEFINED-ON-LINE '714) 
(PUT 'GROEBSIMPCONTNORMALFORM 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSIMPCONTNORMALFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBSIMPCONTNORMALFORM (H)
    (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) H)
          (T
           (PROG (SUGAR C)
             (SETQ SUGAR (GSUGAR H))
             (SETQ C (CADDR H))
             (SETQ H (VDPSIMPCONT H))
             (GSETSUGAR H SUGAR)
             (COND
              ((AND *GROEBPROT (NOT (EQUAL C (CADDR H))))
               (GROEBREDUCTIONPROTOCOL2
                (REVAL1 (LIST 'QUOTIENT (BC2A (CADDR H)) (BC2A C)) T))))
             (RETURN H))))) 
(PUT 'GROEBSIMPCONT2 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBSIMPCONT2 'DEFINED-ON-LINE '724) 
(PUT 'GROEBSIMPCONT2 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSIMPCONT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBSIMPCONT2 (F F1)
    (PROG (C S1 S2)
      (SETQ S1 (GSUGAR F))
      (SETQ S2 (GSUGAR F1))
      (SETQ C (VDPCONTENT F))
      (COND ((BCONE? (VBCABS C)) (GO READY)))
      (COND
       ((NOT (OR (NULL F1) (NULL (CADR (CDDR F1)))))
        (PROGN
         (SETQ C (DIPNUMCONTENT (CADR (CDDR F1)) C))
         (COND ((BCONE? (VBCABS C)) (GO READY)))
         (SETQ F1 (VDPDIVMON F1 C NIL)))))
      (SETQ F (VDPDIVMON F C NIL))
      (AND *TRGROEB (GROEBMESS28 C))
      (GROEBSAVELTERMBC C)
      (GSETSUGAR F S1)
      (GSETSUGAR F1 S2)
     READY
      (SETQ SECONDVALUE* F1)
      (RETURN F))) 
(PUT 'GROEBPREREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBPREREDUCE 'DEFINED-ON-LINE '745) 
(PUT 'GROEBPREREDUCE 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBPREREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBPREREDUCE (G)
    (PROG (RES WORK OLDVEV F OLDF *GROEBWEAK *GROEBFULLREDUCTION COUNT)
      (SETQ COUNT 0)
      (COND
       (*TRGROEBS
        (PROGN
         (SETQ G
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P G)
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P) (VDPENUMERATE P)) (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (P) (VDPENUMERATE P)) (CAR P)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (P)
           (SETQ P G)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P) (VDPPRINT P)) (CAR P))
           (SETQ P (CDR P))
           (GO LAB)))))
      (SETQ RES NIL)
      (PROG (F)
        (SETQ F G)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
             (SETQ RES (CONS F RES)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ WORK (SETQ G (SETQ RES (REVERSIP RES))))
      (PROG ()
       WHILELABEL
        (COND ((NOT WORK) (RETURN NIL)))
        (PROGN
         (SETQ G (VDPLSORT RES))
         (COND (*TRGROEBS (PRIN2T "Starting cycle in prereduction.")))
         (SETQ RES NIL)
         (SETQ COUNT (PLUS COUNT 1))
         (SETQ WORK NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT G) (RETURN NIL)))
           (PROGN
            (SETQ OLDF (SETQ F (CAR G)))
            (SETQ G (CDR G))
            (SETQ OLDVEV (CADR F))
            (SETQ F (VDPSIMPCONT (GROEBNORMALFORM F G 'SORT)))
            (COND
             ((AND (OR *TRGROEBS *GROEBPROT) (NOT (VDPEQUAL F OLDF)))
              (PROGN
               (SETQ F (VDPENUMERATE F))
               (COND
                (*GROEBPROT
                 (COND
                  ((NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                   (GROEBPROTSETQ (MKID 'POLY (VDPGETPROP F 'NUMBER))
                    (DIP2A (CADR (CDDR F)))))
                  (T (GROEBPROTVAL 0)))))
               (COND
                (*TRGROEBS
                 (PROGN
                  (PRIN2T "reducing")
                  (VDPPRINT OLDF)
                  (PRIN2T "to")
                  (VDPPRINT F)))))))
            (COND
             ((NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
              (PROGN
               (COND ((NEQ OLDVEV (CADR F)) (SETQ WORK T)))
               (SETQ RES (CONS F RES))))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F RES)
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (VDPSIMPCONT F)) (CAR F)) NIL)))
        LOOPLABEL
         (SETQ F (CDR F))
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (F) (VDPSIMPCONT F)) (CAR F)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'GROEBREDUCEFROMFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBREDUCEFROMFACTORS 'DEFINED-ON-LINE '777) 
(PUT 'GROEBREDUCEFROMFACTORS 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCEFROMFACTORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBREDUCEFROMFACTORS (G FACTS)
    (PROG (NEW GNEW F NOLD NNEW NUMBERS)
      (COND
       (*TRGROEBS
        (PROGN
         (PRIN2T "replacing from factors:")
         (PROG (X)
           (SETQ X FACTS)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X) (VDPPRIN2T X)) (CAR X))
           (SETQ X (CDR X))
           (GO LAB)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR G))
         (SETQ G (CDR G))
         (SETQ NOLD (VDPGETPROP F 'NUMBER))
         (SETQ NEW (GROEBNORMALFORM F FACTS 'LIST))
         (COND
          ((OR (NULL NEW) (NULL (CADR (CDDR NEW))))
           (PROGN
            (COND
             (*TRGROEBS
              (PROGN (PRIN2 "vanishes ") (PRIN2 (VDPGETPROP F 'NUMBER)))))))
          ((OR (NULL (CADR NEW))
               (AND (EQUAL (CAR (CADR NEW)) 0) (VEVZERO?1 (CDR (CADR NEW)))))
           (PROGN
            (COND
             (*TRGROEBS
              (PROGN (PRIN2 "ONEPOL ") (PRIN2 (VDPGETPROP F 'NUMBER)))))
            (SETQ G NIL)
            (SETQ GNEW (LIST VDPONE*))))
          (T
           (PROGN
            (COND
             ((NEQ NEW F)
              (PROGN
               (SETQ NEW (VDPENUMERATE (VDPSIMPCONT NEW)))
               (SETQ NNEW (VDPGETPROP NEW 'NUMBER))
               (SETQ NUMBERS (CONS (CONS NOLD NNEW) NUMBERS))
               (COND
                (*TRGROEBS
                 (PROGN
                  (PRIN2 "replacing ")
                  (PRIN2 (VDPGETPROP F 'NUMBER))
                  (PRIN2 " by ")
                  (PRIN2T (VDPGETPROP NEW 'NUMBER))))))))
            (SETQ GNEW (CONS NEW GNEW))))))
        (GO WHILELABEL))
      (SETQ SECONDVALUE* NUMBERS)
      (RETURN GNEW))) 
(PUT 'GROEBNORMALFORM1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBNORMALFORM1 'DEFINED-ON-LINE '809) 
(PUT 'GROEBNORMALFORM1 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBNORMALFORM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBNORMALFORM1 (F P)
    (COND ((EQUAL (DIPLENGTH (CADR (CDDR P))) 1) (VDPCANCELMVEV F (CADR P)))
          (T (GROEBNORMALFORM F (LIST P) NIL)))) 
(PUT 'GROEBPROFITSFROMVEV 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBPROFITSFROMVEV 'DEFINED-ON-LINE '815) 
(PUT 'GROEBPROFITSFROMVEV 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBPROFITSFROMVEV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBPROFITSFROMVEV (P VEV)
    (COND ((OR (NULL P) (NULL (CADR (CDDR P)))) NIL)
          ((BUCHVEVDIVIDES? VEV (CADR P)) T)
          (T (GROEBPROFITSFROMVEV (VDPRED P) VEV)))) 
(PUT 'GROEBREDUCEONESTEPINT 'NUMBER-OF-ARGS 5) 
(PUT 'GROEBREDUCEONESTEPINT 'DEFINED-ON-LINE '825) 
(PUT 'GROEBREDUCEONESTEPINT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCEONESTEPINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBREDUCEONESTEPINT (F F1 C VEV G1)
    (PROG (VEVLCM A B CG X RG1)
      (COND
       (((LAMBDA (U) (OR (NULL U) (NULL (CADR (CDDR U)))))
         (SETQ RG1 (VDPRED G1)))
        (RETURN (PROGN (SETQ F (VDPRED F)) (SETQ SECONDVALUE* F1) F))))
      (SETQ VEVLCM (VEVDIF VEV (CADR G1)))
      (SETQ CG (CADDR G1))
      (SETQ X (COND ((NOT *GROEBDIVIDE) (BCFD 1)) (T (VBCGCD C CG))))
      (SETQ A (BCQUOT CG X))
      (SETQ B (BCQUOT C X))
      (COND
       ((AND F1 (NOT (OR (NULL F1) (NULL (CADR (CDDR F1))))))
        (SETQ F1 (VDPVBCPROD F1 A))))
      (COND (*GROEBPROT (GROEBREDUCTIONPROTOCOL A (BCNEG B) VEVLCM G1)))
      (SETQ F
              (VDPILCOMB1 (VDPRED F) A (VEVMAPTOZERO1 VDPVARS* NIL) RG1
                          (BCNEG B) VEVLCM))
      (SETQ SECONDVALUE* F1)
      (SETQ THIRDVALUE* A)
      (RETURN F))) 
(PUT 'GROEBREDUCEONESTEPRAT 'NUMBER-OF-ARGS 5) 
(PUT 'GROEBREDUCEONESTEPRAT 'DEFINED-ON-LINE '849) 
(PUT 'GROEBREDUCEONESTEPRAT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCEONESTEPRAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBREDUCEONESTEPRAT (F DUMMY C VEV G1)
    (PROG (X RG1 VEVLCM)
      (SETQ DUMMY NIL)
      (COND
       (((LAMBDA (U) (OR (NULL U) (NULL (CADR (CDDR U)))))
         (SETQ RG1 (VDPRED G1)))
        (RETURN (VDPRED F))))
      (SETQ X (BCNEG (BCQUOT C (CADDR G1))))
      (SETQ VEVLCM (VEVDIF VEV (CADR G1)))
      (COND (*GROEBPROT (GROEBREDUCTIONPROTOCOL (A2BC 1) X VEVLCM G1)))
      (RETURN
       (VDPILCOMB1 (VDPRED F) (A2BC 1) (VEVMAPTOZERO1 VDPVARS* NIL) RG1 X
                   VEVLCM)))) 
(PUT 'GROEBREDUCTIONPROTOCOL 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBREDUCTIONPROTOCOL 'DEFINED-ON-LINE '864) 
(PUT 'GROEBREDUCTIONPROTOCOL 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCTIONPROTOCOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBREDUCTIONPROTOCOL (A B VEVLCM G1)
    (COND
     (*GROEBPROT
      (SETQ GROEBPROTFILE
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (COND
                ((NOT (BCONE? A))
                 (APPEND GROEBPROTFILE
                         (LIST
                          (LIST 'EQUAL 'CANDIDATE
                                (LIST 'TIMES 'CANDIDATE (BC2A A)))
                          (LIST 'EQUAL 'CANDIDATE
                                (LIST 'PLUS 'CANDIDATE
                                      (LIST 'TIMES
                                            (DIP2A
                                             (CADR (CDDR (VDPFMON B VEVLCM))))
                                            (MKID 'POLY
                                                  (VDPGETPROP G1
                                                              'NUMBER))))))))
                (T
                 (APPEND GROEBPROTFILE
                         (LIST
                          (LIST 'EQUAL 'CANDIDATE
                                (LIST 'PLUS 'CANDIDATE
                                      (LIST 'TIMES
                                            (DIP2A
                                             (CADR (CDDR (VDPFMON B VEVLCM))))
                                            (MKID 'POLY
                                                  (VDPGETPROP G1
                                                              'NUMBER)))))))))))))) 
(PUT 'GROEBREDUCTIONPROTOCOL2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBREDUCTIONPROTOCOL2 'DEFINED-ON-LINE '884) 
(PUT 'GROEBREDUCTIONPROTOCOL2 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCTIONPROTOCOL2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBREDUCTIONPROTOCOL2 (A)
    (COND
     (*GROEBPROT
      (SETQ GROEBPROTFILE
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (COND
                ((NOT (EQUAL A 1))
                 (APPEND GROEBPROTFILE
                         (LIST
                          (LIST 'EQUAL 'CANDIDATE
                                (LIST 'TIMES 'CANDIDATE A))))))))))) 
(PUT 'GROEBREDUCTIONPROTOCOLBORDER 'NUMBER-OF-ARGS 0) 
(PUT 'GROEBREDUCTIONPROTOCOLBORDER 'DEFINED-ON-LINE '891) 
(PUT 'GROEBREDUCTIONPROTOCOLBORDER 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBREDUCTIONPROTOCOLBORDER 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GROEBREDUCTIONPROTOCOLBORDER NIL
    (APPEND GROEBPROTFILE (CONS '++++++++++++++++ NIL))) 
(PUT 'GROEBPROTSETQ 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBPROTSETQ 'DEFINED-ON-LINE '894) 
(PUT 'GROEBPROTSETQ 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBPROTSETQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBPROTSETQ (A B)
    (SETQ GROEBPROTFILE
            (PROGN
             (SETQ ALGLIST* (CONS NIL NIL))
             (APPEND GROEBPROTFILE (LIST (LIST 'EQUAL A B)))))) 
(PUT 'GROEBPROTVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBPROTVAL 'DEFINED-ON-LINE '897) 
(PUT 'GROEBPROTVAL 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBPROTVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBPROTVAL (A)
    (SETQ GROEBPROTFILE
            (PROGN
             (SETQ ALGLIST* (CONS NIL NIL))
             (APPEND GROEBPROTFILE
                     (LIST (LIST 'EQUAL 'INTERMEDIATERESULT A)))))) 
(PUT 'SUBSET? 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSET? 'DEFINED-ON-LINE '901) 
(PUT 'SUBSET? 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'SUBSET? 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSET? (S1 S2)
    (COND ((NULL S1) T) ((MEMBER (CAR S1) S2) (SUBSET? (CDR S1) S2)) (T NIL))) 
(PUT 'VEVSPLIT 'NUMBER-OF-ARGS 1) 
(PUT 'VEVSPLIT 'DEFINED-ON-LINE '907) 
(PUT 'VEVSPLIT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'VEVSPLIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VEVSPLIT (VEV)
    (PROG (E VP N)
      (SETQ N 0)
      (PROG (X)
        (SETQ X VEV)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ N (PLUS N 1))
            (COND
             ((NEQ X 0)
              (PROGN
               (SETQ E (APPEND (CADR VDPONE*) NIL))
               (RPLACA (PNTH E N) 1)
               (SETQ VP (CONS E VP)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN VP))) 
(PUT 'GROEBSPOLYNOM 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBSPOLYNOM 'DEFINED-ON-LINE '930) 
(PUT 'GROEBSPOLYNOM 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSPOLYNOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBSPOLYNOM (P1 P2) (GROEBSPOLYNOM2 P1 P2)) 
(PUT 'GROEBSPOLYNOM2 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBSPOLYNOM2 'DEFINED-ON-LINE '933) 
(PUT 'GROEBSPOLYNOM2 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSPOLYNOM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBSPOLYNOM2 (P1 P2)
    (COND ((OR (NULL P1) (NULL (CADR (CDDR P1)))) P2)
          ((OR (NULL P2) (NULL (CADR (CDDR P2)))) P1)
          (T
           (PROG (CAND S TP1 TP2 TS)
             (SETQ S (GROEBSPOLYNOM3 P1 P2))
             (COND
              ((OR (OR (NULL S) (NULL (CADR (CDDR S))))
                   (AND (NOT (OR (NULL S) (NULL (CADR (CDDR S)))))
                        (OR (NULL (CADR S))
                            (AND (EQUAL (CAR (CADR S)) 0)
                                 (VEVZERO?1 (CDR (CADR S))))))
                   *GROEBPROT)
               (RETURN S)))
             (SETQ TP1 (CADR P1))
             (SETQ TP2 (CADR P2))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (NOT (OR (NULL S) (NULL (CADR (CDDR S)))))
                       (OR
                        (AND (BUCHVEVDIVIDES? TP2 (SETQ TS (CADR S)))
                             (SETQ CAND P2))
                        (AND (BUCHVEVDIVIDES? TP1 (SETQ TS (CADR S)))
                             (SETQ CAND P1)))))
                 (RETURN NIL)))
               (PROGN
                (COND
                 (*VDPINTEGER
                  (SETQ S (GROEBREDUCEONESTEPINT S NIL (CADDR S) TS CAND)))
                 (T (SETQ S (GROEBREDUCEONESTEPRAT S NIL (CADDR S) TS CAND)))))
               (GO WHILELABEL))
             (RETURN S))))) 
(PUT 'GROEBSPOLYNOM3 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBSPOLYNOM3 'DEFINED-ON-LINE '951) 
(PUT 'GROEBSPOLYNOM3 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSPOLYNOM3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBSPOLYNOM3 (P Q)
    (PROG (R) (SETQ R (GROEBSPOLYNOM4 P Q)) (GROEBSAVELTERM R) (RETURN R))) 
(PUT 'GROEBSPOLYNOM4 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBSPOLYNOM4 'DEFINED-ON-LINE '955) 
(PUT 'GROEBSPOLYNOM4 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSPOLYNOM4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBSPOLYNOM4 (P1 P2)
    (PROG (DB1 DB2 EP1 EP2 EP R RP1 RP2 X)
      (SETQ EP1 (CADR P1))
      (SETQ EP2 (CADR P2))
      (SETQ EP (VEVLCM EP1 EP2))
      (SETQ RP1 (VDPRED P1))
      (SETQ RP2 (VDPRED P2))
      (GSETSUGAR RP1 (GSUGAR P1))
      (GSETSUGAR RP2 (GSUGAR P2))
      (SETQ R
              (COND
               ((AND (OR (NULL RP1) (NULL (CADR (CDDR RP1))))
                     (OR (NULL RP2) (NULL (CADR (CDDR RP2)))))
                RP1)
               (T
                (COND
                 ((OR (NULL RP1) (NULL (CADR (CDDR RP1))))
                  (PROGN
                   (SETQ DB2 (A2BC 0))
                   (VDPPROD RP2
                            (VDPFMON (SETQ DB1 (A2BC 1)) (VEVDIF EP EP2)))))
                 ((OR (NULL RP2) (NULL (CADR (CDDR RP2))))
                  (PROGN
                   (SETQ DB1 (A2BC 0))
                   (VDPPROD RP1
                            (VDPFMON (SETQ DB2 (A2BC 1)) (VEVDIF EP EP1)))))
                 (T
                  (PROGN
                   (SETQ DB1 (CADDR P1))
                   (SETQ DB2 (CADDR P2))
                   (COND
                    (*VDPINTEGER
                     (PROGN
                      (SETQ X (VBCGCD DB1 DB2))
                      (COND
                       ((NOT (BCONE? X))
                        (PROGN
                         (SETQ DB1 (BCQUOT DB1 X))
                         (SETQ DB2 (BCQUOT DB2 X))))))))
                   (VDPILCOMB1 RP2 DB1 (VEVDIF EP EP2) RP1 (BCNEG DB2)
                               (VEVDIF EP EP1))))))))
      (COND
       (*GROEBPROT
        (GROEBPROTSETQ 'CANDIDATE
         (LIST 'DIFFERENCE
               (LIST 'TIMES (DIP2A (CADR (CDDR (VDPFMON DB2 (VEVDIF EP EP2)))))
                     (MKID 'POLY (VDPGETPROP P1 'NUMBER)))
               (LIST 'TIMES (DIP2A (CADR (CDDR (VDPFMON DB1 (VEVDIF EP EP1)))))
                     (MKID 'POLY (VDPGETPROP P2 'NUMBER)))))))
      (RETURN R))) 
(PUT 'GROEBSAVELTERM 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBSAVELTERM 'DEFINED-ON-LINE '988) 
(PUT 'GROEBSAVELTERM 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSAVELTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBSAVELTERM (R)
    (COND
     ((AND *GROELTERMS (NOT (OR (NULL R) (NULL (CADR (CDDR R))))))
      (GROEBSAVELTERMBC (CADDR R))))) 
(PUT 'GROEBSAVELTERMBC 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBSAVELTERMBC 'DEFINED-ON-LINE '991) 
(PUT 'GROEBSAVELTERMBC 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBSAVELTERMBC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBSAVELTERMBC (R)
    (PROGN
     (SETQ R (BC2A R))
     (COND
      ((AND (NOT (NUMBERP R)) (NOT (CONSTANT_EXPRP R)))
       (PROG (P)
         (SETQ P (CDR (FCTRF (CAR (SIMP R)))))
        LAB
         (COND ((NULL P) (RETURN NIL)))
         ((LAMBDA (P)
            (PROGN
             (SETQ P (PREPF (CAR P)))
             (COND ((NOT (MEMBER P GLTERMS)) (NCONC GLTERMS (LIST P))))))
          (CAR P))
         (SETQ P (CDR P))
         (GO LAB)))))) 
(PUT 'SFCONT 'NUMBER-OF-ARGS 1) 
(PUT 'SFCONT 'DEFINED-ON-LINE '998) 
(PUT 'SFCONT 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'SFCONT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFCONT (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) F)
          (T (GCDF (SFCONT (CDAR F)) (SFCONT (CDR F)))))) 
(PUT 'VDPLMON 'NUMBER-OF-ARGS 1) 
(PUT 'VDPLMON 'DEFINED-ON-LINE '1002) 
(PUT 'VDPLMON 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'VDPLMON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPLMON (U) (VDPFMON (CADDR U) (CADDR U))) 
(PUT 'VDPMEMBER3 'NUMBER-OF-ARGS 4) 
(PUT 'VDPMEMBER3 'DEFINED-ON-LINE '1004) 
(PUT 'VDPMEMBER3 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'VDPMEMBER3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VDPMEMBER3 (P G1 G2 G3)
    (OR (VDPMEMBER P G1) (VDPMEMBER P G2) (VDPMEMBER P G3))) 
(PUT 'GROEBABORTID 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBABORTID 'DEFINED-ON-LINE '1008) 
(PUT 'GROEBABORTID 'DEFINED-IN-FILE 'GROEBNER/BUCHBG.RED) 
(PUT 'GROEBABORTID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBABORTID (BASE ABORT1)
    (COND ((NULL ABORT1) NIL)
          (T
           (OR
            ((LAMBDA (U) (OR (NULL U) (NULL (CADR (CDDR U)))))
             (GROEBNORMALFORM (CAR ABORT1) BASE 'LIST))
            (GROEBABORTID BASE (CDR ABORT1)))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HILLE)) 
(PUT 'GROEPOSTHILLEBRAND 'NUMBER-OF-ARGS 2) 
(PUT 'GROEPOSTHILLEBRAND 'DEFINED-ON-LINE '36) 
(PUT 'GROEPOSTHILLEBRAND 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'GROEPOSTHILLEBRAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEPOSTHILLEBRAND (U V)
    (PROG (A D E)
      (SETQ U (CDR U))
      (VDPINIT (GROEBNERVARS U NIL))
      (GROEDOMAINMODE)
      (SETQ A
              (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                (SETQ UU U)
                (COND ((NULL UU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (UU) (CAR (SIMP UU))) (CAR UU))
                                      NIL)))
               LOOPLABEL
                (SETQ UU (CDR UU))
                (COND ((NULL UU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (UU) (CAR (SIMP UU))) (CAR UU)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS* (SETQ DIPVARS* (SETQ VDPVARS* (CDR V))))
      (COND ((NULL (HILLEBRANDTRIANGULAR (CDR V) A NIL)) (RETURN NIL)))
      (SETQ *GROEBOPT NIL)
      (SETQ A (HILLEBRAND A NIL))
      (PROG (B)
        (SETQ B A)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ D
                    (CONS 'LIST
                          (PROG (C FORALL-RESULT FORALL-ENDPTR)
                            (SETQ C B)
                            (COND ((NULL C) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (C) (PREPF C)) (CAR C))
                                             NIL)))
                           LOOPLABEL
                            (SETQ C (CDR C))
                            (COND ((NULL C) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (C) (PREPF C)) (CAR C))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
            (SETQ E (APPEND E (GROEPOSTFASTSOLVE D V)))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (RETURN (GROESOLVEARB E V)))) 
(PUT 'HILLEBRANDSTDSAT 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDSTDSAT 'DEFINED-ON-LINE '57) 
(PUT 'HILLEBRANDSTDSAT 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDSTDSAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDSTDSAT (G P)
    (PROG (A B C E)
      (COND ((CDR G) (GO A)))
      (SETQ P (CONS P 1))
      (SETQ G (CONS (CAR G) 1))
      (SETQ A T)
      (PROG ()
       WHILELABEL
        (COND ((NOT A) (RETURN NIL)))
        (PROGN (SETQ A (HILLEBRANDQUOT G P)) (COND (A (SETQ G A))))
        (GO WHILELABEL))
      (SETQ E
              (COND
               ((NOT (OR (ATOM (CAR G)) (ATOM (CAR (CAR G))))) (LIST (CAR G)))
               (T NIL)))
      (RETURN E)
     A
      (SETQ P (PREPF P))
      (SETQ A
              (CONS 'LIST
                    (PROG (GG FORALL-RESULT FORALL-ENDPTR)
                      (SETQ GG G)
                      (COND ((NULL GG) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (GG) (PREPF GG)) (CAR GG))
                                            NIL)))
                     LOOPLABEL
                      (SETQ GG (CDR GG))
                      (COND ((NULL GG) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (GG) (PREPF GG)) (CAR GG)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ B (SATURATIONEVAL (LIST A P)))
      (COND ((EQUAL B '(LIST 1)) (RETURN '(1))))
      (SETQ C
              (PROG (BB FORALL-RESULT FORALL-ENDPTR)
                (SETQ BB (CDR B))
                (COND ((NULL BB) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (BB) (CAR (SIMP BB))) (CAR BB))
                                      NIL)))
               LOOPLABEL
                (SETQ BB (CDR BB))
                (COND ((NULL BB) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (BB) (CAR (SIMP BB))) (CAR BB)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (SORT C (FUNCTION HILLEBRANDCOMPARE))))) 
(PUT 'HILLEBRANDQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDQUOT 'DEFINED-ON-LINE '72) 
(PUT 'HILLEBRANDQUOT 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDQUOT (G P)
    ((LAMBDA (A)
       (COND ((HILLEBRANDVAR (CDR A) VARS*) (CONS (CAR A) 1)) (T NIL)))
     (MULTSQ G (INVSQ P)))) 
(PUT 'HILLEBRANDVAR 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDVAR 'DEFINED-ON-LINE '79) 
(PUT 'HILLEBRANDVAR 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDVAR (P M)
    (COND ((OR (ATOM P) (ATOM (CAR P))) T) ((MEMBER (CAAAR P) M) NIL)
          (T (AND (HILLEBRANDVAR (CDAR P) M) (HILLEBRANDVAR (CDR P) M))))) 
(PUT 'HILLEBRAND 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRAND 'DEFINED-ON-LINE '87) 
(PUT 'HILLEBRAND 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRAND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRAND (G FACT)
    (PROG (A)
      (SETQ VARS* DIPVARS*)
      (AND *TRGROESOLV (HILLEBRANDMSG1 G))
      (SETQ A (HILLEBRAND1 (SORT G (FUNCTION HILLEBRANDCOMPARE)) FACT))
      (AND *TRGROESOLV (HILLEBRANDMSG2 A))
      (RETURN A))) 
(PUT 'HILLEBRANDCOMPARE 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDCOMPARE 'DEFINED-ON-LINE '97) 
(PUT 'HILLEBRANDCOMPARE 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDCOMPARE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDCOMPARE (A B) (HILLEBRANDCOMPARE1 A B VARS*)) 
(PUT 'HILLEBRANDCOMPARE1 'NUMBER-OF-ARGS 3) 
(PUT 'HILLEBRANDCOMPARE1 'DEFINED-ON-LINE '101) 
(PUT 'HILLEBRANDCOMPARE1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDCOMPARE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDCOMPARE1 (A B V)
    (PROG (AA BB C)
      (SETQ AA A)
      (SETQ BB B)
      (COND
       ((OR (OR (ATOM AA) (ATOM (CAR AA))) (NOT (MEMBER (CAAAR AA) V)))
        (RETURN T))
       ((OR (OR (ATOM BB) (ATOM (CAR BB))) (NOT (MEMBER (CAAAR BB) V)))
        (COND ((MEMBER (CAAAR AA) V) (RETURN NIL)) (T (RETURN T)))))
     AA
      (COND
       ((OR (OR (ATOM BB) (ATOM (CAR BB))) (NOT (MEMBER (CAAAR BB) V)))
        (COND
         ((OR (OR (ATOM AA) (ATOM (CAR AA))) (NOT (MEMBER (CAAAR AA) V)))
          (RETURN (HILLEBRANDCOMPARE1 (CDR A) (CDR B) V)))
         (T (RETURN T))))
       ((AND (MEMBER (CAAAR AA) V) (EQUAL (CAAAR AA) (CAAAR BB)))
        (COND
         ((EQUAL (CDAAR AA) (CDAAR BB))
          (PROGN (SETQ AA (CDAR AA)) (SETQ BB (CDAR BB)) (GO AA)))
         ((ILESSP (CDAAR AA) (CDAAR BB)) (RETURN T)) (T (RETURN NIL))))
       ((SETQ C (MEMBER (CAAAR BB) V))
        (COND
         ((OR (OR (ATOM AA) (ATOM (CAR AA))) (NOT (MEMBER (CAAAR AA) C))
              (MEMBER (CAAAR AA) (CDR C)))
          (RETURN T))
         ((MEMBER (CAAAR AA) V) (RETURN NIL)))))
      (RETURN (HILLEBRANDCOMPARE1 (CDR A) (CDR B) V)))) 
(PUT 'HILLEBRAND1 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRAND1 'DEFINED-ON-LINE '122) 
(PUT 'HILLEBRAND1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRAND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRAND1 (G FACT)
    (COND ((EQUAL G '(1)) NIL)
          ((HILLEBRANDTRIANGULAR VARS* G T) (HILLEBRANDFACTORIZELAST G FACT))
          (T
           (PROG (A AA B C R F FF FH G2 G3 H L O)
             (SETQ G3 G)
             (PROG ()
              WHILELABEL
               (COND ((NOT (CDR G3)) (RETURN NIL)))
               (SETQ G3 (CDR G3))
               (GO WHILELABEL))
             (SETQ A (HILLEBRANDDECOMPOSE G (CAAAR (CAR G3))))
             (SETQ C
                     (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                       (SETQ AA (CDR A))
                       (COND ((NULL AA) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (AA) (CDAR AA)) (CAR AA))
                                             NIL)))
                      LOOPLABEL
                       (SETQ AA (CDR AA))
                       (COND ((NULL AA) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (AA) (CDAR AA)) (CAR AA)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ R (HILLEBRANDGROEBNER (HILLEBRANDJOIN (CAR A) C)))
             (SETQ F (HILLEBRAND1 R FACT))
             (SETQ AA (HILLEBRANDLAST G))
             (PROG (TT)
               (SETQ TT F)
              LAB
               (COND ((NULL TT) (RETURN NIL)))
               ((LAMBDA (TT)
                  (PROGN
                   (SETQ B (HILLEBRANDNORMALFORM AA TT))
                   (SETQ FF (HILLEBRANDAPPEND1 FF TT B))))
                (CAR TT))
               (SETQ TT (CDR TT))
               (GO LAB))
             (SETQ F (REVERSIP FF))
             (SETQ H (CAR A))
             (SETQ O (LENGTH C))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE O K)) (RETURN NIL)))
               (PROGN
                (SETQ L (NTH C K))
                (SETQ G2 (HILLEBRANDSTDSAT H L))
                (COND
                 ((NOT (EQUAL (CAR G2) 1))
                  (PROGN
                   (SETQ FH (HILLEBRAND1 G2 FACT))
                   (SETQ FH
                           (PROG (TT FORALL-RESULT FORALL-ENDPTR)
                             (SETQ TT FH)
                             (COND ((NULL TT) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (TT)
                                                 (HILLEBRANDGROEBNER
                                                  (HILLEBRANDAPPEND TT
                                                   (LIST (CAR (CDR A))))))
                                               (CAR TT))
                                              NIL)))
                            LOOPLABEL
                             (SETQ TT (CDR TT))
                             (COND ((NULL TT) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (TT)
                                         (HILLEBRANDGROEBNER
                                          (HILLEBRANDAPPEND TT
                                           (LIST (CAR (CDR A))))))
                                       (CAR TT))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                   (SETQ F (HILLEBRANDAPPEND F FH)))))
                (SETQ H (APPEND H (LIST (NTH C O)))))
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (SETQ F
                     (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FF F)
                       (COND ((NULL FF) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FF)
                                           (SORT FF
                                                 (FUNCTION HILLEBRANDCOMPARE)))
                                         (CAR FF))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FF (CDR FF))
                       (COND ((NULL FF) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FF)
                                   (SORT FF (FUNCTION HILLEBRANDCOMPARE)))
                                 (CAR FF))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN F))))) 
(PUT 'HILLEBRANDAPPEND 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDAPPEND 'DEFINED-ON-LINE '159) 
(PUT 'HILLEBRANDAPPEND 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDAPPEND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDAPPEND (A B) (COND ((NULL (CAR B)) A) (T (APPEND A B)))) 
(PUT 'HILLEBRANDAPPEND1 'NUMBER-OF-ARGS 3) 
(PUT 'HILLEBRANDAPPEND1 'DEFINED-ON-LINE '162) 
(PUT 'HILLEBRANDAPPEND1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDAPPEND1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDAPPEND1 (FF TT B)
    (PROGN (COND (B (SETQ TT (APPEND TT (LIST B))))) (CONS TT FF))) 
(PUT 'HILLEBRANDTRIANGULAR 'NUMBER-OF-ARGS 3) 
(PUT 'HILLEBRANDTRIANGULAR 'DEFINED-ON-LINE '168) 
(PUT 'HILLEBRANDTRIANGULAR 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDTRIANGULAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDTRIANGULAR (A G M)
    (PROG (B C)
      (PROG (GG)
        (SETQ GG G)
       LAB
        (COND ((NULL GG) (RETURN NIL)))
        ((LAMBDA (GG)
           (COND
            ((OR (OR (ATOM (CDAR GG)) (ATOM (CAR (CDAR GG))))
                 (NOT (MEMBER (CAAAR (CDAR GG)) A)))
             (SETQ B (CONS (CAAAR GG) B)))
            (T (SETQ C T))))
         (CAR GG))
        (SETQ GG (CDR GG))
        (GO LAB))
      (COND ((AND M C) (RETURN NIL)))
      (SETQ C T)
      (PROG (GG)
        (SETQ GG G)
       LAB
        (COND ((NULL GG) (RETURN NIL)))
        ((LAMBDA (GG) (AND C (SETQ C (HILLEBRANDTRIANGULAR1 A GG B))))
         (CAR GG))
        (SETQ GG (CDR GG))
        (GO LAB))
      (RETURN C))) 
(PUT 'HILLEBRANDTRIANGULAR1 'NUMBER-OF-ARGS 3) 
(PUT 'HILLEBRANDTRIANGULAR1 'DEFINED-ON-LINE '179) 
(PUT 'HILLEBRANDTRIANGULAR1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDTRIANGULAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDTRIANGULAR1 (A G B)
    (COND ((OR (OR (ATOM G) (ATOM (CAR G))) (NOT (MEMBER (CAAAR G) A))) T)
          ((NOT (MEMBER (CAAAR G) B)) NIL)
          (T
           (AND (HILLEBRANDTRIANGULAR1 A (CDAR G) B)
                (HILLEBRANDTRIANGULAR1 A (CDR G) B))))) 
(PUT 'HILLEBRANDFACTORIZELAST 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDFACTORIZELAST 'DEFINED-ON-LINE '187) 
(PUT 'HILLEBRANDFACTORIZELAST 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDFACTORIZELAST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDFACTORIZELAST (G F)
    (COND ((NULL F) (LIST G))
          (T
           (PROG (A B C D)
            AA
             (COND
              ((CDR G) (PROGN (SETQ A (CONS (CAR G) A)) (SETQ G (CDR G)))))
             (COND ((CDR G) (GO AA)))
             (SETQ B (FCTRF (CAR G)))
             (COND ((OR (ATOM (CAR B)) (ATOM (CAR (CAR B)))) (SETQ B (CDR B))))
             (SETQ C
                     (PROG (BB FORALL-RESULT FORALL-ENDPTR)
                       (SETQ BB B)
                       (COND ((NULL BB) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (BB)
                                           (PROGN
                                            (SETQ D (LIST (CAR BB)))
                                            (PROG (AA)
                                              (SETQ AA A)
                                             LAB
                                              (COND ((NULL AA) (RETURN NIL)))
                                              ((LAMBDA (AA)
                                                 (SETQ D (CONS AA D)))
                                               (CAR AA))
                                              (SETQ AA (CDR AA))
                                              (GO LAB))
                                            D))
                                         (CAR BB))
                                        NIL)))
                      LOOPLABEL
                       (SETQ BB (CDR BB))
                       (COND ((NULL BB) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (BB)
                                   (PROGN
                                    (SETQ D (LIST (CAR BB)))
                                    (PROG (AA)
                                      (SETQ AA A)
                                     LAB
                                      (COND ((NULL AA) (RETURN NIL)))
                                      ((LAMBDA (AA) (SETQ D (CONS AA D)))
                                       (CAR AA))
                                      (SETQ AA (CDR AA))
                                      (GO LAB))
                                    D))
                                 (CAR BB))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN
              (COND ((NULL (CDR C)) C)
                    (T
                     (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                       (SETQ CC C)
                       (COND ((NULL CC) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (CC)
                                           (SORT CC
                                                 (FUNCTION HILLEBRANDCOMPARE)))
                                         (CAR CC))
                                        NIL)))
                      LOOPLABEL
                       (SETQ CC (CDR CC))
                       (COND ((NULL CC) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (CC)
                                   (SORT CC (FUNCTION HILLEBRANDCOMPARE)))
                                 (CAR CC))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))))) 
(PUT 'HILLEBRANDDECOMPOSE 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDDECOMPOSE 'DEFINED-ON-LINE '200) 
(PUT 'HILLEBRANDDECOMPOSE 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDDECOMPOSE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDDECOMPOSE (G V)
    (PROG (A B C D)
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ C (CAR G))
         (SETQ D (HILLEBRANDDECOMPOSE1 C V VARS* 0))
         (COND ((EQUAL D 1) (SETQ A (CONS C A)))
               ((EQUAL D 2) (SETQ B (CONS C B))))
         (SETQ G (CDR G)))
        (GO WHILELABEL))
      (RETURN (CONS (REVERSIP A) (REVERSIP (CDR B)))))) 
(PUT 'HILLEBRANDDECOMPOSE1 'NUMBER-OF-ARGS 4) 
(PUT 'HILLEBRANDDECOMPOSE1 'DEFINED-ON-LINE '207) 
(PUT 'HILLEBRANDDECOMPOSE1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDDECOMPOSE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDDECOMPOSE1 (P V VV M)
    (COND ((OR (OR (ATOM P) (ATOM (CAR P))) (NOT (MEMBER (CAAAR P) VV))) M)
          (T
           ((LAMBDA (N) (HILLEBRANDDECOMPOSE1 (CDAR P) V VV N))
            (COND ((EQUAL (CAAAR P) V) 2)
                  ((AND (ILESSP M 1) (MEMBER (CAAAR P) VV)) 1) (T M)))))) 
(PUT 'HILLEBRANDJOIN 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDJOIN 'DEFINED-ON-LINE '217) 
(PUT 'HILLEBRANDJOIN 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDJOIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDJOIN (A B) (COND ((NULL B) A) (T (APPEND A B)))) 
(PUT 'HILLEBRANDLAST 'NUMBER-OF-ARGS 1) 
(PUT 'HILLEBRANDLAST 'DEFINED-ON-LINE '223) 
(PUT 'HILLEBRANDLAST 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDLAST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HILLEBRANDLAST (G)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND ((NOT (CDR G)) (RETURN NIL)))
       (SETQ G (CDR G))
       (GO WHILELABEL))
     (CAR G))) 
(PUT 'HILLEBRANDGROEBNER 'NUMBER-OF-ARGS 1) 
(PUT 'HILLEBRANDGROEBNER 'DEFINED-ON-LINE '228) 
(PUT 'HILLEBRANDGROEBNER 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDGROEBNER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HILLEBRANDGROEBNER (G)
    (PROG (A B C D)
      (PROG (GG)
        (SETQ GG G)
       LAB
        (COND ((NULL GG) (RETURN NIL)))
        ((LAMBDA (GG)
           (PROGN
            (SETQ D (PREPF GG))
            (COND ((NOT (EQUAL D 0)) (SETQ A (CONS D A))))))
         (CAR GG))
        (SETQ GG (CDR GG))
        (GO LAB))
      ((LAMBDA (DIPVARS* VDPVARS*)
         (SETQ B (GROEBNEREVAL (LIST (CONS 'LIST A) (CONS 'LIST VARS*)))))
       DIPVARS* VDPVARS*)
      (SETQ C
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR B))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CAR (SIMP X))) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CAR (SIMP X))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (SORT C (FUNCTION HILLEBRANDCOMPARE))))) 
(PUT 'HILLEBRANDNORMALFORM 'NUMBER-OF-ARGS 2) 
(PUT 'HILLEBRANDNORMALFORM 'DEFINED-ON-LINE '241) 
(PUT 'HILLEBRANDNORMALFORM 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDNORMALFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILLEBRANDNORMALFORM (P G)
    (PROGN
     (SETQ P (HILLEBRANDF2VDP P))
     (SETQ G
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X G)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (HILLEBRANDF2VDP X)) (CAR X))
                                     NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (HILLEBRANDF2VDP X)) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (DIP2F (CADR (CDDR (GROEBNORMALFORM P G 'SORT)))))) 
(PUT 'HILLEBRANDF2VDP 'NUMBER-OF-ARGS 1) 
(PUT 'HILLEBRANDF2VDP 'DEFINED-ON-LINE '247) 
(PUT 'HILLEBRANDF2VDP 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDF2VDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HILLEBRANDF2VDP (P) ((LAMBDA (A) (GSETSUGAR A NIL)) (F2VDP P))) 
(PUT 'HILLEBRANDMSG1 'NUMBER-OF-ARGS 1) 
(PUT 'HILLEBRANDMSG1 'DEFINED-ON-LINE '252) 
(PUT 'HILLEBRANDMSG1 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDMSG1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HILLEBRANDMSG1 (G)
    (COND
     (*TRGROESOLV
      (PROGN
       (WRITEPRI " " 'ONLY)
       (WRITEPRI " Hillebrand routine;solve{" 'ONLY)
       (PROG ()
        WHILELABEL
         (COND ((NOT G) (RETURN NIL)))
         (PROGN
          (WRITEPRI (MKQUOTE (PREPF (CAR G))) 'FIRST)
          (SETQ G (CDR G))
          (COND (G (WRITEPRI " , " 'LAST))))
         (GO WHILELABEL))
       (WRITEPRI " } with respect to " NIL)
       (WRITEPRI (MKQUOTE (CONS 'LIST VARS*)) 'LAST)
       (WRITEPRI " " 'ONLY)
       NIL)))) 
(PUT 'HILLEBRANDMSG2 'NUMBER-OF-ARGS 1) 
(PUT 'HILLEBRANDMSG2 'DEFINED-ON-LINE '261) 
(PUT 'HILLEBRANDMSG2 'DEFINED-IN-FILE 'GROEBNER/HILLE.RED) 
(PUT 'HILLEBRANDMSG2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HILLEBRANDMSG2 (A)
    (COND
     (*TRGROESOLV
      (PROGN
       (WRITEPRI " Decomposition by Hillebrand : " 'ONLY)
       (PROG (AA)
         (SETQ AA A)
        LAB
         (COND ((NULL AA) (RETURN NIL)))
         ((LAMBDA (AA)
            (PROGN
             (WRITEPRI " { " 'ONLY)
             (PROG ()
              WHILELABEL
               (COND ((NOT AA) (RETURN NIL)))
               (PROGN
                (WRITEPRI (MKQUOTE (PREPF (CAR AA))) 'FIRST)
                (SETQ AA (CDR AA))
                (COND (AA (WRITEPRI " , " 'LAST))))
               (GO WHILELABEL))
             (WRITEPRI " } " 'LAST)))
          (CAR AA))
         (SETQ AA (CDR AA))
         (GO LAB))
       (WRITEPRI " " 'ONLY)
       NIL)))) 
(ENDMODULE) 
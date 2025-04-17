(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DESIR)) 
(CREATE-PACKAGE '(DESIR) '(SOLVE)) 
(PUT 'DEG 'NUMBER-OF-ARGS 2) 
(PUT 'DEG 'DEFINED-ON-LINE '166) 
(PUT 'DEG 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'DEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEG (U KERN)
    ((LAMBDA (DMODE*)
       (PROGN
        (SETQ U (SIMP* U))
        (TSTPOLYARG (CDR U) KERN)
        (NUMRDEG (CAR U) KERN)))
     GDMODE*)) 
(FLUID '(*PRECISE *TRDESIR)) 
(SWITCH (LIST 'TRDESIR)) 
(GLOBAL '(MULTIPLICITIES*)) 
(FLAG '(MULTIPLICITIES*) 'SHARE) 
(SETQ *PRECISE NIL) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'DESIR 'NUMBER-OF-ARGS 0) 
(FLAG '(DESIR) 'OPFN) 
(PUT 'DESIR 'DEFINED-ON-LINE '200) 
(PUT 'DESIR 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'DESIR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DESIR NIL
    (PROG (K GRILLE REPETITION LCOEFF PARAM N NS SOLUTIONS LSOL J)
      (SETQ J 0)
      (COND
       ((AND (EVALNEQ (AEVAL REPETITION) (AEVAL 'NON))
             (EVALNEQ (AEVAL REPETITION) (AEVAL 'NONON)))
        (PROGN
         (ASSGNPRI
          (AEVAL "   ATTENTION : chaque donnee doit etre suivie de ; ou de $")
          NIL 'ONLY)
         (SETQ REPETITION (AEVAL 'NONON))
         (AEVAL 'NIL))))
      (SETQ SOLUTIONS (AEVAL (LIST 'LIST)))
      (SETQ LSOL (AEVAL (LIST 'LIST)))
      (SETQ LCOEFF (AEVAL (LIST 'LECTABCOEF)))
      (SETQ PARAM (AEVAL (LIST 'SECOND LCOEFF)))
      (SETQ LCOEFF (AEVAL (LIST 'FIRST LCOEFF)))
      (SETK 'CONTINUE (AEVAL 'OUI))
      (ASSGNPRI (AEVAL "transformation ? (oui;/non;)  ") NIL 'ONLY)
      (SETK 'OK (AEVAL (LIST 'XREAD 'NIL)))
      (WHILE (EQ (REVALX 'CONTINUE) (REVALX 'OUI))
             (PROGN
              (COND
               ((EQ (REVALX 'OK) (REVALX 'OUI))
                (PROGN
                 (SETQ LCOEFF (AEVAL* (LIST 'TRANSFORMATION LCOEFF PARAM)))
                 (SETQ PARAM (AEVAL* (LIST 'SECOND LCOEFF)))
                 (SETQ LCOEFF (AEVAL* (LIST 'FIRST LCOEFF)))
                 (AEVAL* 'NIL))))
              (ASSGNPRI (AEVAL* "nombre de termes desires pour la solution ?")
                        NIL 'ONLY)
              (SETQ K (AEVAL* (LIST 'XREAD 'NIL)))
              (COND
               ((EVALNEQ (AEVAL* K) 0)
                (PROGN
                 (SETQ GRILLE (AEVAL* 1))
                 (COND
                  ((AND (EVALNEQ (AEVAL* REPETITION) (AEVAL* 'NON))
                        (BOOLVALUE* (REVALX *TRDESIR)))
                   (PROGN
                    (ASSGNPRI (AEVAL* " ") NIL 'ONLY)
                    (PROGN
                     (ASSGNPRI
                      (AEVAL*
                       "a chaque etape le polygone NRM sera visualise par la ")
                      NIL 'FIRST)
                     (ASSGNPRI
                      (AEVAL* "donnee des aretes modifiees , sous la forme :")
                      NIL 'LAST))
                    (ASSGNPRI (AEVAL* " ") NIL 'ONLY)
                    (PROGN
                     (ASSGNPRI
                      (AEVAL*
                       "    ARETE No i : coordonnees de l'origine gauche, pente,")
                      NIL 'FIRST)
                     (ASSGNPRI (AEVAL* " longueur ") NIL 'LAST))
                    (AEVAL* 'NIL))))
                 (ASSGNPRI (AEVAL* " ") NIL 'ONLY)
                 (AEVAL* (ON (LIST 'DIV)))
                 (AEVAL* (ON (LIST 'GCD)))
                 (SETQ SOLUTIONS
                         (AEVAL* (LIST 'DELIRE 'X K GRILLE LCOEFF PARAM)))
                 (SETQ NS (AEVAL* (LIST 'LENGTH SOLUTIONS)))
                 (SETQ N (AEVAL* (LIST 'DIFFERENCE (LIST 'LENGTH LCOEFF) 1)))
                 (COND
                  ((EVALNEQ (AEVAL* NS) 0)
                   (PROGN
                    (PROGN
                     (ASSGNPRI (AEVAL* "LES ") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* NS) NIL NIL)
                     (ASSGNPRI
                      (AEVAL* " SOLUTIONS CALCULEES SONT LES SUIVANTES") NIL
                      'LAST))
                    (SETQ J (AEVAL* 1))
                    (PROG (ELT)
                      (SETQ ELT (GETRLIST (AEVAL* SOLUTIONS)))
                     LAB
                      (COND ((NULL ELT) (RETURN NIL)))
                      ((LAMBDA (ELT)
                         (PROGN
                          (ASSGNPRI (AEVAL* " ") NIL 'ONLY)
                          (ASSGNPRI (AEVAL* " ==============") NIL 'ONLY)
                          (PROGN
                           (ASSGNPRI (AEVAL* "  SOLUTION No ") NIL 'FIRST)
                           (ASSGNPRI J NIL 'LAST))
                          (ASSGNPRI (AEVAL* " ==============") NIL 'ONLY)
                          (AEVAL* (LIST 'SORSOL ELT))
                          (SETQ J (AEVAL* (PLUS J 1)))
                          (AEVAL* 'NIL)))
                       (CAR ELT))
                      (SETQ ELT (CDR ELT))
                      (GO LAB))
                    (AEVAL* 'NIL))))
                 (AEVAL* (OFF (LIST 'DIV)))
                 (COND
                  ((EVALNEQ (AEVAL* NS) (AEVAL* N))
                   (PROGN
                    (ASSGNPRI (AEVAL* (LIST 'DIFFERENCE N NS)) NIL 'FIRST)
                    (ASSGNPRI (AEVAL* " solutions n'ont pu etre calculees") NIL
                              'LAST))))
                 (SETQ REPETITION (AEVAL* 'NON))
                 (SETQ LSOL
                         (AEVAL*
                          (LIST 'APPEND LSOL
                                (LIST 'LIST (LIST 'LIST LCOEFF SOLUTIONS)))))
                 (ASSGNPRI (AEVAL* "voulez-vous continuer ? ") NIL 'ONLY)
                 (ASSGNPRI
                  (AEVAL*
                   "'non;' : la liste des solutions calculees est affichee (sous")
                  NIL 'ONLY)
                 (ASSGNPRI (AEVAL* " forme generalisee).") NIL 'ONLY)
                 (ASSGNPRI (AEVAL* "'non$' : cette liste n'est pas affichee.")
                           NIL 'ONLY)
                 (SETK 'CONTINUE (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETK 'OK (AEVAL* 'OUI))
                 (AEVAL* 'NIL)))
               (T (SETK 'CONTINUE (AEVAL* 'NON))))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL LSOL)))) 
(PUT 'SOLVALIDE 'NUMBER-OF-ARGS 3) 
(FLAG '(SOLVALIDE) 'OPFN) 
(PUT 'SOLVALIDE 'DEFINED-ON-LINE '294) 
(PUT 'SOLVALIDE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SOLVALIDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVALIDE (SOLUTIONS SOLK K)
    (PROG (Z LCOEFF SOL ESSAI QX GRI R COEFF1 D ZZ J)
      (SETQ J 0)
      (SETQ LCOEFF (AEVAL (LIST 'FIRST SOLUTIONS)))
      (SETQ SOL (AEVAL (LIST 'PART (LIST 'SECOND SOLUTIONS) SOLK)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'LENGTH SOL)) 1)
        (PROGN
         (ASSGNPRI (AEVAL "presence de solutions conditionnelles :") NIL
                   'FIRST)
         (ASSGNPRI (AEVAL " cette procedure ne peut pas etre appelee.") NIL
                   'LAST)))
       (T
        (PROGN
         (SETQ Z (AEVAL (LIST 'FIRST SOL)))
         (SETQ ESSAI (AEVAL (LIST 'FIRST Z)))
         (SETQ QX (AEVAL (LIST 'FIRST ESSAI)))
         (SETQ ESSAI (AEVAL (LIST 'REST ESSAI)))
         (SETQ GRI (AEVAL (LIST 'FIRST ESSAI)))
         (SETQ SOL (AEVAL (LIST 'SECOND ESSAI)))
         (SETQ R (AEVAL (LIST 'THIRD ESSAI)))
         (SETQ ESSAI (AEVAL (LIST 'SECOND Z)))
         (COND
          ((EVALGREATERP (AEVAL (LIST 'LENGTH ESSAI)) 0)
           (ASSGNPRI
            (AEVAL "presence d'une condition : cette procedure ne peut pas etre
           appelee.")
            NIL 'ONLY))
          (T
           (PROGN
            (SETQ COEFF1
                    (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELT (GETRLIST (AEVAL LCOEFF)))
                      (COND ((NULL ELT) (RETURN (MAKELIST NIL))))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELT)
                                          (AEVAL
                                           (LIST 'SUB
                                                 (LIST 'EQUAL 'X
                                                       (LIST 'EXPT 'XT
                                                             (LIST 'QUOTIENT 1
                                                                   GRI)))
                                                 ELT)))
                                        (CAR ELT))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELT (CDR ELT))
                      (COND ((NULL ELT) (RETURN (CONS 'LIST FORALL-RESULT))))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELT)
                                  (AEVAL
                                   (LIST 'SUB
                                         (LIST 'EQUAL 'X
                                               (LIST 'EXPT 'XT
                                                     (LIST 'QUOTIENT 1 GRI)))
                                         ELT)))
                                (CAR ELT))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             ((EVALNEQ (AEVAL QX) 0)
              (PROGN
               (SETQ D (AEVAL (LIST 'SOLVIREG COEFF1 QX 'XT)))
               (SETQ COEFF1
                       (AEVAL
                        (LIST 'CHANGEFONC COEFF1 'XT '&PHI
                              (LIST 'TIMES (LIST 'EXPT 'E QX) '&PHI))))
               (AEVAL 'NIL))))
            (SETQ D (AEVAL (LIST 'DIFFERENCE (LIST 'ALTMIN COEFF1 'XT) D)))
            (SETQ QX
                    (AEVAL
                     (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI)) QX)))
            (SETQ SOL (AEVAL (LIST 'SUB (LIST 'EQUAL 'LAMBD R) SOL)))
            (SETQ SOL
                    (AEVAL
                     (LIST 'TIMES (LIST 'EXPT 'E QX)
                           (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                           (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                 SOL))))
            (PROGN
             (ASSGNPRI (AEVAL "La solution numero ") NIL 'FIRST)
             (ASSGNPRI (AEVAL SOLK) NIL NIL)
             (ASSGNPRI (AEVAL " est ") NIL NIL)
             (ASSGNPRI (AEVAL SOL) NIL 'LAST))
            (PROGN
             (ASSGNPRI
              (AEVAL "La partie reguliere du reste est de l'ordre de x**(") NIL
              'FIRST)
             (ASSGNPRI (AEVAL (LIST 'TIMES GRI (LIST 'PLUS K 1 D R))) NIL NIL)
             (ASSGNPRI (AEVAL ")") NIL 'LAST))
            (SETQ Z (AEVAL 0))
            (PROG (ELT)
              (SETQ ELT (GETRLIST (AEVAL LCOEFF)))
             LAB
              (COND ((NULL ELT) (RETURN NIL)))
              ((LAMBDA (ELT)
                 (PROGN
                  (SETQ Z
                          (AEVAL
                           (LIST 'PLUS Z
                                 (LIST 'TIMES ELT (LIST 'DF SOL 'X J)))))
                  (SETQ J (AEVAL (PLUS J 1)))
                  (AEVAL 'NIL)))
               (CAR ELT))
              (SETQ ELT (CDR ELT))
              (GO LAB))
            (SETQ ZZ
                    (AEVAL
                     (LIST 'TIMES (LIST 'EXPT 'E (LIST 'MINUS QX))
                           (LIST 'EXPT 'X (LIST 'MINUS (LIST 'TIMES R GRI)))
                           Z)))
            (SETQ ZZ
                    (AEVAL
                     (LIST 'SUB
                           (LIST 'EQUAL 'X
                                 (LIST 'EXPT 'XT (LIST 'QUOTIENT 1 GRI)))
                           ZZ)))
            (AEVAL (ON (LIST 'RATIONAL)))
            (PROGN
             (ASSGNPRI
              (AEVAL "Si on reporte cette solution dans l'equation, le terme ")
              NIL 'FIRST)
             (ASSGNPRI (AEVAL "significatif du reste") NIL NIL)
             (ASSGNPRI (AEVAL " est : ") NIL NIL)
             (ASSGNPRI
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT 'E QX)
                     (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                     (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                           (LIST 'VALTERM ZZ 'XT))))
              NIL 'LAST))
            (AEVAL (OFF (LIST 'RATIONAL)))
            (RETURN (AEVAL Z))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))))) 
(PUT 'SOLVIREG 'NUMBER-OF-ARGS 3) 
(FLAG '(SOLVIREG) 'OPFN) 
(PUT 'SOLVIREG 'DEFINED-ON-LINE '353) 
(PUT 'SOLVIREG 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SOLVIREG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVIREG (LCOEFF Q X)
    (PROG (F J N)
      (SETQ J 0)
      (SETQ N 0)
      (AEVAL (DEPEND (LIST '&Y X)))
      (AEVAL (DEPEND (LIST '&PHI X)))
      (SETK 'L (AEVAL LCOEFF))
      (WHILE (EVALNEQ (AEVAL* 'L) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ F
                      (AEVAL*
                       (LIST 'PLUS F
                             (LIST 'TIMES (LIST 'FIRST 'L)
                                   (LIST 'DF '&Y X J)))))
              (SETQ J (AEVAL* (PLUS J 1)))
              (SETK 'L (AEVAL* (LIST 'REST 'L)))))
      (SETQ N (AEVAL (LIST 'LENGTH LCOEFF)))
      (AEVAL
       (LET (LIST (LIST 'EQUAL '&Y (LIST 'TIMES (LIST 'EXPT 'E Q) '&PHI)))))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (SETQ F
                (AEVAL*
                 (LIST 'SUB
                       (LIST 'EQUAL (LIST 'DF '&PHI X J) (LIST 'EXPT 'ZZ J))
                       F)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ F (AEVAL (LIST 'SUB (LIST 'EQUAL '&PHI 1) F)))
      (AEVAL (CLEAR (LIST '&Y)))
      (AEVAL (NODEPEND (LIST '&Y X)))
      (AEVAL (NODEPEND (LIST '&PHI X)))
      (RETURN (AEVAL (LIST 'DEG (LIST 'DEN F) X))))) 
(PUT 'ALTMIN 'NUMBER-OF-ARGS 2) 
(FLAG '(ALTMIN) 'OPFN) 
(PUT 'ALTMIN 'DEFINED-ON-LINE '372) 
(PUT 'ALTMIN 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'ALTMIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALTMIN (LCOEFF X)
    (PROG (J MIN D)
      (SETQ J 0)
      (SETQ MIN 0)
      (SETQ D 0)
      (SETQ MIN (AEVAL (LIST 'DEG (LIST 'VALTERM (LIST 'FIRST LCOEFF) X) X)))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL (LIST 'REST LCOEFF))))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ J (AEVAL (PLUS J 1)))
            (SETQ D (AEVAL (LIST 'DEG (LIST 'VALTERM ELT X) X)))
            (COND
             ((LESSP (DIFFERENCE D J) MIN)
              (SETQ MIN (AEVAL (DIFFERENCE D J)))))
            (AEVAL 'NIL)))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN MIN))) 
(PUT 'VALTERM 'NUMBER-OF-ARGS 2) 
(FLAG '(VALTERM) 'OPFN) 
(PUT 'VALTERM 'DEFINED-ON-LINE '384) 
(PUT 'VALTERM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'VALTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VALTERM (POLY X)
    (PROG (L ELT J)
      (SETQ J 0)
      (SETQ L (AEVAL (LIST 'COEFF POLY X)))
      (SETQ ELT (AEVAL (LIST 'FIRST L)))
      (WHILE
       (AND (EVALEQUAL (AEVAL* ELT) 0)
            (EVALNEQ (AEVAL* (LIST 'REST L)) (AEVAL* (LIST 'LIST))))
       (PROGN
        (SETQ J (AEVAL* (PLUS J 1)))
        (SETQ L (AEVAL* (LIST 'REST L)))
        (SETQ ELT (AEVAL* (LIST 'FIRST L)))))
      (RETURN (AEVAL (LIST 'TIMES ELT (LIST 'EXPT X J)))))) 
(PUT 'STANDSOL 'NUMBER-OF-ARGS 1) 
(FLAG '(STANDSOL) 'OPFN) 
(PUT 'STANDSOL 'DEFINED-ON-LINE '397) 
(PUT 'STANDSOL 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'STANDSOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STANDSOL (SOLUTIONS)
    (PROG (Z LCOEFF SOL SOLNEW SOLCOND ESSAI QX GRI R J)
      (SETQ J 0)
      (SETQ SOLNEW (AEVAL (LIST 'LIST)))
      (SETQ SOLCOND (AEVAL (LIST 'LIST)))
      (SETQ LCOEFF (AEVAL (LIST 'FIRST SOLUTIONS)))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL (LIST 'SECOND SOLUTIONS))))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((EVALGREATERP (AEVAL (LIST 'LENGTH ELT)) 1)
             (SETQ SOLCOND (AEVAL (LIST 'APPEND SOLCOND (LIST 'LIST ELT)))))
            (T
             (PROGN
              (SETQ Z (AEVAL (LIST 'FIRST ELT)))
              (SETQ ESSAI (AEVAL (LIST 'FIRST Z)))
              (SETQ QX (AEVAL (LIST 'FIRST ESSAI)))
              (SETQ ESSAI (AEVAL (LIST 'REST ESSAI)))
              (SETQ GRI (AEVAL (LIST 'FIRST ESSAI)))
              (SETQ QX
                      (AEVAL
                       (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI)) QX)))
              (SETQ SOL (AEVAL (LIST 'SECOND ESSAI)))
              (SETQ R (AEVAL (LIST 'THIRD ESSAI)))
              (SETQ ESSAI (AEVAL (LIST 'SECOND Z)))
              (COND
               ((EVALGREATERP (AEVAL (LIST 'LENGTH ESSAI)) 0)
                (SETQ SOLCOND (AEVAL (LIST 'APPEND SOLCOND (LIST 'LIST ELT)))))
               (T
                (PROGN
                 (SETQ SOL (AEVAL (LIST 'SUB (LIST 'EQUAL 'LAMBD R) SOL)))
                 (SETQ SOL
                         (AEVAL
                          (LIST 'TIMES (LIST 'EXPT 'E QX)
                                (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                                (LIST 'SUB
                                      (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                      SOL))))
                 (SETQ SOLNEW (AEVAL (LIST 'APPEND SOLNEW (LIST 'LIST SOL))))
                 (AEVAL 'NIL))))
              (AEVAL 'NIL)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (AEVAL (LIST 'LIST LCOEFF SOLNEW SOLCOND))))) 
(PUT 'SORSOL 'NUMBER-OF-ARGS 1) 
(FLAG '(SORSOL) 'OPFN) 
(PUT 'SORSOL 'DEFINED-ON-LINE '445) 
(PUT 'SORSOL 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SORSOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORSOL (SOL)
    (PROG (ESSAI QX GRI R)
      (SETK 'NONNUL (AEVAL "  non nul"))
      (SETK 'ENTNUL (AEVAL "  nul"))
      (SETK 'NONENT (AEVAL "  non entier"))
      (SETK 'ENTPOS (AEVAL "  entier positif"))
      (SETK 'ENTNEG (AEVAL "  entier negatif"))
      (PROG (Z)
        (SETQ Z (GETRLIST (AEVAL SOL)))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (PROGN
            (SETQ ESSAI (AEVAL (LIST 'FIRST Z)))
            (SETQ QX (AEVAL (LIST 'FIRST ESSAI)))
            (SETQ ESSAI (AEVAL (LIST 'REST ESSAI)))
            (SETQ GRI (AEVAL (LIST 'FIRST ESSAI)))
            (SETQ QX
                    (AEVAL
                     (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI)) QX)))
            (SETQ SOL (AEVAL (LIST 'SECOND ESSAI)))
            (SETQ R (AEVAL (LIST 'THIRD ESSAI)))
            (SETQ ESSAI (AEVAL (LIST 'SECOND Z)))
            (SETQ SOL
                    (AEVAL
                     (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI)) SOL)))
            (COND
             ((EVALGREATERP (AEVAL (LIST 'LENGTH ESSAI)) 0)
              (PROGN
               (COND
                ((EVALEQUAL (AEVAL (LIST 'DEG (LIST 'NUM SOL) 'LAMBD)) 0)
                 (PROGN
                  (ASSGNPRI
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI)) SOL))
                   NIL 'ONLY)
                  (ASSGNPRI (AEVAL "Si : ") NIL 'ONLY)
                  (PROG (W)
                    (SETQ W (GETRLIST (AEVAL ESSAI)))
                   LAB
                    (COND ((NULL W) (RETURN NIL)))
                    ((LAMBDA (W)
                       (COND
                        ((OR (EVALEQUAL (AEVAL (LIST 'LENGTH W)) 2)
                             (NOT (BOOLVALUE* (REVALX *TRDESIR))))
                         (PROGN
                          (ASSGNPRI (AEVAL (LIST 'FIRST W)) NIL 'FIRST)
                          (ASSGNPRI (AEVAL (LIST 'SECOND W)) NIL 'LAST)))
                        (T
                         (PROGN
                          (PROGN
                           (ASSGNPRI (AEVAL (LIST 'FIRST W)) NIL 'FIRST)
                           (ASSGNPRI (AEVAL (LIST 'SECOND W)) NIL NIL)
                           (ASSGNPRI (AEVAL (LIST 'THIRD W)) NIL 'LAST))
                          (SETQ W
                                  (AEVAL
                                   (LIST 'REST (LIST 'REST (LIST 'REST W)))))
                          (PROG (W1)
                            (SETQ W1 (GETRLIST (AEVAL W)))
                           LAB
                            (COND ((NULL W1) (RETURN NIL)))
                            ((LAMBDA (W1)
                               (PROGN
                                (ASSGNPRI (AEVAL "                    +-") NIL
                                          'FIRST)
                                (ASSGNPRI (AEVAL W1) NIL 'LAST)))
                             (CAR W1))
                            (SETQ W1 (CDR W1))
                            (GO LAB))
                          (AEVAL 'NIL)))))
                     (CAR W))
                    (SETQ W (CDR W))
                    (GO LAB)))))))
             (T
              (PROGN
               (SETQ SOL (AEVAL (LIST 'SUB (LIST 'EQUAL 'LAMBD R) SOL)))
               (ASSGNPRI
                (AEVAL
                 (LIST 'TIMES (LIST 'EXPT 'E QX)
                       (LIST 'EXPT 'X (LIST 'TIMES R GRI)) SOL))
                NIL 'ONLY)
               (AEVAL 'NIL))))
            (AEVAL 'NIL)))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'NONNUL 'ENTNUL 'NONENT 'ENTPOS 'ENTNEG))))) 
(PUT 'CHANGEHOM 'NUMBER-OF-ARGS 4) 
(FLAG '(CHANGEHOM) 'OPFN) 
(PUT 'CHANGEHOM 'DEFINED-ON-LINE '492) 
(PUT 'CHANGEHOM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CHANGEHOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGEHOM (LCOEFF X SECMEMBRE ID)
    (PROG (L FCT CF N J)
      (SETQ J 0)
      (AEVAL (DEPEND (LIST '&Y X)))
      (SETQ FCT (AEVAL SECMEMBRE))
      (SETQ L (AEVAL LCOEFF))
      (WHILE (EVALNEQ (AEVAL* L) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ FCT
                      (AEVAL*
                       (LIST 'PLUS FCT
                             (LIST 'TIMES (LIST 'FIRST L)
                                   (LIST 'DF '&Y X J)))))
              (SETQ J (AEVAL* (PLUS J 1)))
              (SETQ L (AEVAL* (LIST 'REST L)))))
      (SETQ FCT (AEVAL (LIST 'DF FCT X ID)))
      (SETQ N (AEVAL (LIST 'PLUS (LIST 'LENGTH LCOEFF) ID)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (SETQ FCT
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DF '&Y X J) (LIST 'EXPT 'ZZ J))
                       FCT)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ FCT (AEVAL (LIST 'SUB (LIST 'EQUAL '&Y 1) FCT)))
      (SETQ CF (AEVAL (LIST 'COEFF FCT 'ZZ)))
      (SETQ J (AEVAL 0))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROG (ELT)
          (SETQ ELT (GETRLIST (AEVAL CF)))
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (PROGN
              (PROGN
               (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
               (ASSGNPRI J NIL NIL)
               (ASSGNPRI (AEVAL ") = ") NIL NIL)
               (ASSGNPRI (AEVAL ELT) NIL 'LAST))
              (SETQ J (AEVAL (PLUS J 1)))
              (AEVAL 'NIL)))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))))
      (AEVAL (NODEPEND (LIST '&Y X)))
      (RETURN (AEVAL CF)))) 
(PUT 'DESIR-CHANGEVAR 'NUMBER-OF-ARGS 4) 
(FLAG '(DESIR-CHANGEVAR) 'OPFN) 
(PUT 'DESIR-CHANGEVAR 'DEFINED-ON-LINE '525) 
(PUT 'DESIR-CHANGEVAR 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'DESIR-CHANGEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DESIR-CHANGEVAR (LCOEFF X V FCT)
    (PROG (F CF J N)
      (SETQ J 0)
      (SETQ N 0)
      (AEVAL (DEPEND (LIST '&Y X)))
      (SETK 'L (AEVAL LCOEFF))
      (WHILE (EVALNEQ (AEVAL* 'L) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ F
                      (AEVAL*
                       (LIST 'PLUS F
                             (LIST 'TIMES (LIST 'FIRST 'L)
                                   (LIST 'DF '&Y X J)))))
              (SETQ J (AEVAL* (PLUS J 1)))
              (SETK 'L (AEVAL* (LIST 'REST 'L)))))
      (SETQ N (AEVAL (LIST 'LENGTH LCOEFF)))
      (SETQ F (AEVAL (LIST 'CHANGE '&Y X V FCT F N)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (SETQ F
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DF '&Y V J) (LIST 'EXPT 'ZZ J))
                       F)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ F (AEVAL (LIST 'SUB (LIST 'EQUAL '&Y 1) F)))
      (SETQ CF (AEVAL (LIST 'COEFF (LIST 'NUM F) 'ZZ)))
      (SETQ J (AEVAL 0))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROG (ELT)
          (SETQ ELT (GETRLIST (AEVAL CF)))
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (PROGN
              (PROGN
               (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
               (ASSGNPRI J NIL NIL)
               (ASSGNPRI (AEVAL ") = ") NIL NIL)
               (ASSGNPRI (AEVAL ELT) NIL 'LAST))
              (SETQ J (AEVAL (PLUS J 1)))
              (AEVAL 'NIL)))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))))
      (AEVAL (NODEPEND (LIST '&Y X)))
      (RETURN (AEVAL CF)))) 
(PUT 'CHANGEFONC 'NUMBER-OF-ARGS 4) 
(FLAG '(CHANGEFONC) 'OPFN) 
(PUT 'CHANGEFONC 'DEFINED-ON-LINE '558) 
(PUT 'CHANGEFONC 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CHANGEFONC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGEFONC (LCOEFF X Q FCT)
    (PROG (F CF J N)
      (SETQ J 0)
      (SETQ N 0)
      (AEVAL (DEPEND (LIST '&Y X)))
      (AEVAL (DEPEND (LIST Q X)))
      (SETK 'L (AEVAL LCOEFF))
      (WHILE (EVALNEQ (AEVAL* 'L) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ F
                      (AEVAL*
                       (LIST 'PLUS F
                             (LIST 'TIMES (LIST 'FIRST 'L)
                                   (LIST 'DF '&Y X J)))))
              (SETQ J (AEVAL* (PLUS J 1)))
              (SETK 'L (AEVAL* (LIST 'REST 'L)))))
      (SETQ N (AEVAL (LIST 'LENGTH LCOEFF)))
      (AEVAL (LET (LIST (LIST 'EQUAL '&Y FCT))))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (SETQ F
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DF Q X J) (LIST 'EXPT 'ZZ J))
                       F)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ F (AEVAL (LIST 'SUB (LIST 'EQUAL Q 1) F)))
      (SETQ CF (AEVAL (LIST 'COEFF (LIST 'NUM F) 'ZZ)))
      (SETQ J (AEVAL 1))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROG (ELT)
          (SETQ ELT (GETRLIST (AEVAL CF)))
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (PROGN
              (PROGN
               (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
               (ASSGNPRI J NIL NIL)
               (ASSGNPRI (AEVAL ") = ") NIL NIL)
               (ASSGNPRI (AEVAL ELT) NIL 'LAST))
              (SETQ J (AEVAL (PLUS J 1)))
              (AEVAL 'NIL)))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))))
      (AEVAL (CLEAR (LIST '&Y)))
      (AEVAL (NODEPEND (LIST '&Y X)))
      (AEVAL (NODEPEND (LIST Q X)))
      (RETURN (AEVAL CF)))) 
(PUT 'SORPARAM 'NUMBER-OF-ARGS 2) 
(FLAG '(SORPARAM) 'OPFN) 
(PUT 'SORPARAM 'DEFINED-ON-LINE '596) 
(PUT 'SORPARAM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SORPARAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORPARAM (SOLUTIONS PARAM)
    (PROG (ESSAI SEC QX GRI SOL SOL1 SOL2 R SOLNEW COEFNEW OMEGA OMEGAC J
           IPARAM)
      (SETQ J 0)
      (SETQ IPARAM 0)
      (SETQ SOLNEW (AEVAL (LIST 'LIST)))
      (SETQ IPARAM (AEVAL (LIST 'LENGTH PARAM)))
      (COND
       ((EQUAL IPARAM 0)
        (AEVAL
         (REDERR
          (REVALX "La liste des parametres est vide : utiliser STANDSOL")))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'PARM IPARAM) (LIST 'PARMVAL IPARAM)))
      (SETQ J (AEVAL 1))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL PARAM)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "donner la valeur du parametre ") NIL 'FIRST)
             (ASSGNPRI (AEVAL ELT) NIL 'LAST))
            (SETK (LIST 'PARM J) (AEVAL ELT))
            (SETK (LIST 'PARMVAL J) (AEVAL (LIST 'XREAD 'NIL)))
            (SETQ J (AEVAL (PLUS J 1)))
            (AEVAL 'NIL)))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ J (AEVAL 1))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL (LIST 'SECOND SOLUTIONS))))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (PROG (Z)
              (SETQ Z (GETRLIST (AEVAL ELT)))
             LAB
              (COND ((NULL Z) (RETURN NIL)))
              ((LAMBDA (Z)
                 (PROGN
                  (SETQ ESSAI (AEVAL (LIST 'FIRST Z)))
                  (SETQ QX (AEVAL (LIST 'FIRST ESSAI)))
                  (SETQ ESSAI (AEVAL (LIST 'REST ESSAI)))
                  (SETQ GRI (AEVAL (LIST 'FIRST ESSAI)))
                  (SETQ QX
                          (AEVAL
                           (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                 QX)))
                  (SETQ SOL1 (AEVAL (LIST 'SECOND ESSAI)))
                  (SETQ R (AEVAL (LIST 'THIRD ESSAI)))
                  (SETQ ESSAI (AEVAL (LIST 'SECOND Z)))
                  (COND
                   ((EVALEQUAL (AEVAL ESSAI) (AEVAL (LIST 'LIST)))
                    (PROGN
                     (SETQ SOL
                             (AEVAL
                              (LIST 'TIMES (LIST 'EXPT 'E QX)
                                    (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                                    (LIST 'SUB
                                          (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                          SOL1))))
                     (PROG (J)
                       (SETQ J 1)
                      LAB
                       (COND ((MINUSP (DIFFERENCE IPARAM J)) (RETURN NIL)))
                       (SETQ SOL
                               (AEVAL*
                                (LIST 'SUB
                                      (LIST 'EQUAL (LIST 'PARM J)
                                            (LIST 'PARMVAL J))
                                      SOL)))
                       (SETQ J (PLUS2 J 1))
                       (GO LAB))
                     (AEVAL 'NIL)))
                   (T
                    (PROGN
                     (SETQ SOL2
                             (AEVAL
                              (LIST 'SORPARAMCOND ESSAI IPARAM QX GRI R SOL1)))
                     (COND ((EVALNEQ (AEVAL SOL2) 0) (SETQ SOL (AEVAL SOL2))))
                     (AEVAL 'NIL))))
                  (AEVAL 'NIL)))
               (CAR Z))
              (SETQ Z (CDR Z))
              (GO LAB))
            (ASSGNPRI (AEVAL " ") NIL 'ONLY)
            (ASSGNPRI (AEVAL " ==============") NIL 'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "  SOLUTION No ") NIL 'FIRST)
             (ASSGNPRI J NIL 'LAST))
            (ASSGNPRI (AEVAL " ==============") NIL 'ONLY)
            (COND
             ((EVALNEQ (AEVAL SOL) 0)
              (PROGN
               (ASSGNPRI (AEVAL SOL) NIL 'ONLY)
               (SETQ SOLNEW (AEVAL (LIST 'APPEND SOLNEW (LIST 'LIST SOL))))))
             (T (ASSGNPRI (AEVAL "solution non calculee") NIL 'ONLY)))
            (SETQ J (AEVAL (PLUS J 1)))
            (AEVAL 'NIL)))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ COEFNEW
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (GETRLIST (AEVAL (LIST 'FIRST SOLUTIONS))))
                (COND ((NULL ELT) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (PROG (COF)
                                      (SETQ COF (AEVAL ELT))
                                      (PROG (J)
                                        (SETQ J 1)
                                       LAB
                                        (COND
                                         ((MINUSP (DIFFERENCE IPARAM J))
                                          (RETURN NIL)))
                                        (SETQ COF
                                                (AEVAL*
                                                 (LIST 'SUB
                                                       (LIST 'EQUAL
                                                             (LIST 'PARM J)
                                                             (LIST 'PARMVAL J))
                                                       COF)))
                                        (SETQ J (PLUS2 J 1))
                                        (GO LAB))
                                      (RETURN (AEVAL COF))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (PROG (COF)
                              (SETQ COF (AEVAL ELT))
                              (PROG (J)
                                (SETQ J 1)
                               LAB
                                (COND
                                 ((MINUSP (DIFFERENCE IPARAM J)) (RETURN NIL)))
                                (SETQ COF
                                        (AEVAL*
                                         (LIST 'SUB
                                               (LIST 'EQUAL (LIST 'PARM J)
                                                     (LIST 'PARMVAL J))
                                               COF)))
                                (SETQ J (PLUS2 J 1))
                                (GO LAB))
                              (RETURN (AEVAL COF))))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL (CLEAR (LIST 'PARM 'PARMVAL)))
      (RETURN (AEVAL (LIST 'LIST COEFNEW SOLNEW))))) 
(PUT 'SORPARAMCOND 'NUMBER-OF-ARGS 6) 
(FLAG '(SORPARAMCOND) 'OPFN) 
(PUT 'SORPARAMCOND 'DEFINED-ON-LINE '658) 
(PUT 'SORPARAMCOND 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SORPARAMCOND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SORPARAMCOND (ESSAI IPARAM QX GRI R SOL1)
    (PROG (SOL SEC OMEGA OMEGAC)
      (SETQ ESSAI (AEVAL (LIST 'FIRST ESSAI)))
      (SETQ OMEGA (AEVAL (LIST 'FIRST ESSAI)))
      (SETQ SEC (AEVAL (LIST 'SECOND ESSAI)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
        (SETQ OMEGA
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                       OMEGA)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ OMEGAC
              (AEVAL (LIST 'APPEND (LIST 'COEFF OMEGA 'I) (LIST 'LIST 0))))
      (AEVAL (ON (LIST 'ROUNDED)))
      (COND
       ((OR (NOT (EVALNUMBERP (AEVAL (LIST 'FIRST OMEGAC))))
            (NOT (EVALNUMBERP (AEVAL (LIST 'SECOND OMEGAC)))))
        (AEVAL
         (REDERR
          (REVALX
           (LIST 'LIST "Les valeurs donnees aux parametres ne"
                 "permettent pas de choisir parmi les solutions conditionnelles."))))))
      (AEVAL (OFF (LIST 'ROUNDED)))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'NONNUL))
        (COND
         ((EVALNEQ (AEVAL OMEGA) 0)
          (PROGN
           (SETQ SOL
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                          (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                SOL1))))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
             (SETQ SOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                            SOL)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (AEVAL 'NIL))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTNUL))
        (COND
         ((EVALEQUAL (AEVAL OMEGA) 0)
          (PROGN
           (SETQ SOL
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                          (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                SOL1))))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
             (SETQ SOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                            SOL)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (AEVAL 'NIL))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'NONENT))
        (COND
         ((NOT (FIXP (REVALX OMEGA)))
          (PROGN
           (SETQ SOL
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                          (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                SOL1))))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
             (SETQ SOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                            SOL)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (AEVAL 'NIL))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTPOS))
        (COND
         ((AND (FIXP (REVALX OMEGA)) (EVALGREATERP (AEVAL OMEGA) 0))
          (PROGN
           (SETQ SOL
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                          (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                SOL1))))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
             (SETQ SOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                            SOL)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (AEVAL 'NIL))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTNEG))
        (COND
         ((AND (FIXP (REVALX OMEGA)) (EVALLESSP (AEVAL OMEGA) 0))
          (PROGN
           (SETQ SOL
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT 'E QX)
                          (LIST 'EXPT 'X (LIST 'TIMES R GRI))
                          (LIST 'SUB (LIST 'EQUAL 'XT (LIST 'EXPT 'X GRI))
                                SOL1))))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
             (SETQ SOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                            SOL)))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB))
           (AEVAL 'NIL))))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'DEG (LIST 'NUM SOL) 'LAMBD)) 0)
        (PROGN
         (SETQ SOL (AEVAL (LIST 'SUB (LIST 'EQUAL 'LAMBD R) SOL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
           (SETQ SOL
                   (AEVAL*
                    (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                          SOL)))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB))
         (AEVAL 'NIL))))
      (RETURN (AEVAL SOL)))) 
(PUT 'SOLPARAM 'NUMBER-OF-ARGS 3) 
(FLAG '(SOLPARAM) 'OPFN) 
(PUT 'SOLPARAM 'DEFINED-ON-LINE '707) 
(PUT 'SOLPARAM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SOLPARAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLPARAM (SOLUTIONS PARAM VALPARAM)
    (PROG (ESSAI SOL SOL1 SOLG SOLNEW COEFNEW J IPARAM)
      (SETQ J 0)
      (SETQ IPARAM 0)
      (SETQ SOLNEW (AEVAL (LIST 'LIST)))
      (SETQ IPARAM (AEVAL (LIST 'LENGTH PARAM)))
      (COND
       ((EQUAL IPARAM 0)
        (AEVAL
         (REDERR
          (REVALX "La liste des parametres est vide : utiliser STANDSOL")))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'PARM IPARAM) (LIST 'PARMVAL IPARAM)))
      (SETQ J (AEVAL 1))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL PARAM)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETK (LIST 'PARM J) (AEVAL ELT))
            (SETQ J (AEVAL (PLUS J 1)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ J (AEVAL 1))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL VALPARAM)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETK (LIST 'PARMVAL J) (AEVAL ELT))
            (SETQ J (AEVAL (PLUS J 1)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL (LIST 'SECOND SOLUTIONS))))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (PROG (Z)
              (SETQ Z (GETRLIST (AEVAL ELT)))
             LAB
              (COND ((NULL Z) (RETURN NIL)))
              ((LAMBDA (Z)
                 (PROGN
                  (SETQ SOLG (AEVAL (LIST 'FIRST Z)))
                  (SETQ ESSAI (AEVAL (LIST 'SECOND Z)))
                  (COND
                   ((EVALEQUAL (AEVAL ESSAI) (AEVAL (LIST 'LIST)))
                    (SETQ SOL1 (AEVAL SOLG)))
                   (T
                    (SETQ SOL1
                            (AEVAL (LIST 'SOLPARAMCOND ESSAI IPARAM SOLG)))))
                  (COND
                   ((EVALNEQ (AEVAL SOL1) (AEVAL (LIST 'LIST)))
                    (PROGN
                     (SETQ ESSAI (AEVAL (LIST 'REST (LIST 'REST SOL1))))
                     (SETK 'R (AEVAL (LIST 'SECOND ESSAI)))
                     (COND
                      ((EVALNEQ
                        (AEVAL
                         (LIST 'DEG
                               (LIST 'NUM
                                     (SETQ SOL (AEVAL (LIST 'FIRST ESSAI))))
                               'LAMBD))
                        0)
                       (PROGN
                        (SETQ SOL
                                (AEVAL
                                 (LIST 'SUB (LIST 'EQUAL 'LAMBD 'R) SOL)))
                        (PROG (J)
                          (SETQ J 1)
                         LAB
                          (COND ((MINUSP (DIFFERENCE IPARAM J)) (RETURN NIL)))
                          (SETQ SOL
                                  (AEVAL*
                                   (LIST 'SUB
                                         (LIST 'EQUAL (LIST 'PARM J)
                                               (LIST 'PARMVAL J))
                                         SOL)))
                          (SETQ J (PLUS2 J 1))
                          (GO LAB))
                        (AEVAL 'NIL))))
                     (SETQ SOL1
                             (AEVAL
                              (LIST 'LIST (LIST 'FIRST SOL1)
                                    (LIST 'SECOND SOL1) SOL 'R)))
                     (SETQ SOLNEW
                             (AEVAL
                              (LIST 'APPEND SOLNEW
                                    (LIST 'LIST
                                          (LIST 'LIST
                                                (LIST 'LIST SOL1
                                                      (LIST 'LIST)))))))
                     (AEVAL 'NIL))))
                  (AEVAL 'NIL)))
               (CAR Z))
              (SETQ Z (CDR Z))
              (GO LAB))
            (AEVAL 'NIL)))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ COEFNEW
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (GETRLIST (AEVAL (LIST 'FIRST SOLUTIONS))))
                (COND ((NULL ELT) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (PROG (COF)
                                      (SETQ COF (AEVAL ELT))
                                      (PROG (J)
                                        (SETQ J 1)
                                       LAB
                                        (COND
                                         ((MINUSP (DIFFERENCE IPARAM J))
                                          (RETURN NIL)))
                                        (SETQ COF
                                                (AEVAL*
                                                 (LIST 'SUB
                                                       (LIST 'EQUAL
                                                             (LIST 'PARM J)
                                                             (LIST 'PARMVAL J))
                                                       COF)))
                                        (SETQ J (PLUS2 J 1))
                                        (GO LAB))
                                      (RETURN (AEVAL COF))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (PROG (COF)
                              (SETQ COF (AEVAL ELT))
                              (PROG (J)
                                (SETQ J 1)
                               LAB
                                (COND
                                 ((MINUSP (DIFFERENCE IPARAM J)) (RETURN NIL)))
                                (SETQ COF
                                        (AEVAL*
                                         (LIST 'SUB
                                               (LIST 'EQUAL (LIST 'PARM J)
                                                     (LIST 'PARMVAL J))
                                               COF)))
                                (SETQ J (PLUS2 J 1))
                                (GO LAB))
                              (RETURN (AEVAL COF))))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL (CLEAR (LIST 'PARM 'PARMVAL)))
      (RETURN (AEVAL (LIST 'LIST COEFNEW SOLNEW))))) 
(PUT 'SOLPARAMCOND 'NUMBER-OF-ARGS 3) 
(FLAG '(SOLPARAMCOND) 'OPFN) 
(PUT 'SOLPARAMCOND 'DEFINED-ON-LINE '767) 
(PUT 'SOLPARAMCOND 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SOLPARAMCOND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLPARAMCOND (ESSAI IPARAM SOLG)
    (PROG (SEC SOL1 SOL OMEGA OMEGAC)
      (SETQ ESSAI (AEVAL (LIST 'FIRST ESSAI)))
      (SETQ OMEGA (AEVAL (LIST 'FIRST ESSAI)))
      (SETQ SEC (AEVAL (LIST 'SECOND ESSAI)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) J)) (RETURN NIL)))
        (SETQ OMEGA
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'PARM J) (LIST 'PARMVAL J))
                       OMEGA)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ OMEGAC
              (AEVAL (LIST 'APPEND (LIST 'COEFF OMEGA 'I) (LIST 'LIST 0))))
      (AEVAL (ON (LIST 'ROUNDED)))
      (COND
       ((OR (NOT (EVALNUMBERP (AEVAL (LIST 'FIRST OMEGAC))))
            (NOT (EVALNUMBERP (AEVAL (LIST 'SECOND OMEGAC)))))
        (AEVAL
         (REDERR
          (REVALX
           (LIST 'LIST "Les valeurs donnees aux parametres"
                 "ne permettent pas de choisir parmi les solutions conditionnelles."))))))
      (AEVAL (OFF (LIST 'ROUNDED)))
      (SETQ SOL1 (AEVAL (LIST 'LIST)))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'NONNUL))
        (COND
         ((EVALNEQ (AEVAL OMEGA) 0)
          (SETQ SOL1
                  (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELEM (GETRLIST (AEVAL SOLG)))
                    (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELEM)
                                        (PROG ()
                                          (SETQ SOL (AEVAL ELEM))
                                          (PROG (J)
                                            (SETQ J 1)
                                           LAB
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE
                                                     (AEVAL* IPARAM) J))
                                              (RETURN NIL)))
                                            (SETQ SOL
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL
                                                                 (LIST 'PARM J)
                                                                 (LIST 'PARMVAL
                                                                       J))
                                                           SOL)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB))
                                          (RETURN (AEVAL SOL))))
                                      (CAR ELEM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ELEM (CDR ELEM))
                    (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELEM)
                                (PROG ()
                                  (SETQ SOL (AEVAL ELEM))
                                  (PROG (J)
                                    (SETQ J 1)
                                   LAB
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* IPARAM) J))
                                      (RETURN NIL)))
                                    (SETQ SOL
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL (LIST 'PARM J)
                                                         (LIST 'PARMVAL J))
                                                   SOL)))
                                    (SETQ J
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             J))
                                    (GO LAB))
                                  (RETURN (AEVAL SOL))))
                              (CAR ELEM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTNUL))
        (COND
         ((EVALEQUAL (AEVAL OMEGA) 0)
          (SETQ SOL1
                  (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELEM (GETRLIST (AEVAL SOLG)))
                    (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELEM)
                                        (PROG ()
                                          (SETQ SOL (AEVAL ELEM))
                                          (PROG (J)
                                            (SETQ J 1)
                                           LAB
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE
                                                     (AEVAL* IPARAM) J))
                                              (RETURN NIL)))
                                            (SETQ SOL
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL
                                                                 (LIST 'PARM J)
                                                                 (LIST 'PARMVAL
                                                                       J))
                                                           SOL)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB))
                                          (RETURN (AEVAL SOL))))
                                      (CAR ELEM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ELEM (CDR ELEM))
                    (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELEM)
                                (PROG ()
                                  (SETQ SOL (AEVAL ELEM))
                                  (PROG (J)
                                    (SETQ J 1)
                                   LAB
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* IPARAM) J))
                                      (RETURN NIL)))
                                    (SETQ SOL
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL (LIST 'PARM J)
                                                         (LIST 'PARMVAL J))
                                                   SOL)))
                                    (SETQ J
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             J))
                                    (GO LAB))
                                  (RETURN (AEVAL SOL))))
                              (CAR ELEM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'NONENT))
        (COND
         ((NOT (FIXP (REVALX OMEGA)))
          (SETQ SOL1
                  (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELEM (GETRLIST (AEVAL SOLG)))
                    (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELEM)
                                        (PROG ()
                                          (SETQ SOL (AEVAL ELEM))
                                          (PROG (J)
                                            (SETQ J 1)
                                           LAB
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE
                                                     (AEVAL* IPARAM) J))
                                              (RETURN NIL)))
                                            (SETQ SOL
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL
                                                                 (LIST 'PARM J)
                                                                 (LIST 'PARMVAL
                                                                       J))
                                                           SOL)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB))
                                          (RETURN (AEVAL SOL))))
                                      (CAR ELEM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ELEM (CDR ELEM))
                    (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELEM)
                                (PROG ()
                                  (SETQ SOL (AEVAL ELEM))
                                  (PROG (J)
                                    (SETQ J 1)
                                   LAB
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* IPARAM) J))
                                      (RETURN NIL)))
                                    (SETQ SOL
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL (LIST 'PARM J)
                                                         (LIST 'PARMVAL J))
                                                   SOL)))
                                    (SETQ J
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             J))
                                    (GO LAB))
                                  (RETURN (AEVAL SOL))))
                              (CAR ELEM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTPOS))
        (COND
         ((AND (FIXP (REVALX OMEGA)) (EVALGREATERP (AEVAL OMEGA) 0))
          (SETQ SOL1
                  (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELEM (GETRLIST (AEVAL SOLG)))
                    (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELEM)
                                        (PROG ()
                                          (SETQ SOL (AEVAL ELEM))
                                          (PROG (J)
                                            (SETQ J 1)
                                           LAB
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE
                                                     (AEVAL* IPARAM) J))
                                              (RETURN NIL)))
                                            (SETQ SOL
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL
                                                                 (LIST 'PARM J)
                                                                 (LIST 'PARMVAL
                                                                       J))
                                                           SOL)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB))
                                          (RETURN (AEVAL SOL))))
                                      (CAR ELEM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ELEM (CDR ELEM))
                    (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELEM)
                                (PROG ()
                                  (SETQ SOL (AEVAL ELEM))
                                  (PROG (J)
                                    (SETQ J 1)
                                   LAB
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* IPARAM) J))
                                      (RETURN NIL)))
                                    (SETQ SOL
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL (LIST 'PARM J)
                                                         (LIST 'PARMVAL J))
                                                   SOL)))
                                    (SETQ J
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             J))
                                    (GO LAB))
                                  (RETURN (AEVAL SOL))))
                              (CAR ELEM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (COND
       ((EVALEQUAL (AEVAL SEC) (AEVAL 'ENTNEG))
        (COND
         ((AND (FIXP (REVALX OMEGA)) (EVALLESSP (AEVAL OMEGA) 0))
          (SETQ SOL1
                  (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELEM (GETRLIST (AEVAL SOLG)))
                    (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELEM)
                                        (PROG ()
                                          (SETQ SOL (AEVAL ELEM))
                                          (PROG (J)
                                            (SETQ J 1)
                                           LAB
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE
                                                     (AEVAL* IPARAM) J))
                                              (RETURN NIL)))
                                            (SETQ SOL
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL
                                                                 (LIST 'PARM J)
                                                                 (LIST 'PARMVAL
                                                                       J))
                                                           SOL)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB))
                                          (RETURN (AEVAL SOL))))
                                      (CAR ELEM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ELEM (CDR ELEM))
                    (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELEM)
                                (PROG ()
                                  (SETQ SOL (AEVAL ELEM))
                                  (PROG (J)
                                    (SETQ J 1)
                                   LAB
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* IPARAM) J))
                                      (RETURN NIL)))
                                    (SETQ SOL
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL (LIST 'PARM J)
                                                         (LIST 'PARMVAL J))
                                                   SOL)))
                                    (SETQ J
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             J))
                                    (GO LAB))
                                  (RETURN (AEVAL SOL))))
                              (CAR ELEM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (RETURN (AEVAL SOL1)))) 
(PUT 'LECTABCOEF 'NUMBER-OF-ARGS 0) 
(FLAG '(LECTABCOEF) 'OPFN) 
(PUT 'LECTABCOEF 'DEFINED-ON-LINE '834) 
(PUT 'LECTABCOEF 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'LECTABCOEF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LECTABCOEF NIL
    (PROG (N OK IPARAM LCOEFF PARAM)
      (ASSGNPRI (AEVAL " ") NIL 'ONLY)
      (ASSGNPRI
       (AEVAL "                     *****  INTRODUCTION DES DONNEES  ***** ")
       NIL 'ONLY)
      (ASSGNPRI (AEVAL " ") NIL 'ONLY)
      (ASSGNPRI (AEVAL "  L' equation est  de la forme") NIL 'ONLY)
      (ASSGNPRI
       (AEVAL "      a(0)(x)d^0 + a(1)(x)d^1 + .... + a(n)(x)d^n = 0 ") NIL
       'ONLY)
      (ASSGNPRI (AEVAL " ordre de l'equation ? ") NIL 'ONLY)
      (SETQ N (AEVAL (LIST 'XREAD 'NIL)))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'TABCOEF (IEVAL N))))
      (ASSGNPRI (AEVAL "  Donner les coefficients   a(j)(x), j = 0..n") NIL
                'ONLY)
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (SETK (LIST 'TABCOEF J) (AEVAL* (LIST 'XREAD 'NIL)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (PROGN
         (ASSGNPRI (AEVAL* "a(") NIL 'FIRST)
         (ASSGNPRI J NIL NIL)
         (ASSGNPRI (AEVAL* ") = ") NIL NIL)
         (ASSGNPRI (AEVAL* (LIST 'TABCOEF J)) NIL 'LAST))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (ASSGNPRI (AEVAL "  ") NIL 'ONLY)
      (ASSGNPRI (AEVAL "correction ? ( oui; / non; )   ") NIL 'ONLY)
      (SETQ OK (AEVAL (LIST 'XREAD 'NIL)))
      (WHILE (EQ (REVALX OK) (REVALX 'OUI))
             (PROGN
              (ASSGNPRI (AEVAL* "valeur de j :") NIL 'ONLY)
              (SETK 'J (AEVAL* (LIST 'XREAD 'NIL)))
              (ASSGNPRI (AEVAL* "expression du coefficient :") NIL 'ONLY)
              (SETK (LIST 'TABCOEF 'J) (AEVAL* (LIST 'XREAD 'NIL)))
              (ASSGNPRI (AEVAL* "correction ?") NIL 'ONLY)
              (SETQ OK (AEVAL* (LIST 'XREAD 'NIL)))
              (AEVAL* 'NIL)))
      (SETQ LCOEFF (AEVAL (LIST 'LIST (LIST 'TABCOEF N))))
      (PROG (J)
        (SETQ J (AEVAL* (LIST 'DIFFERENCE N 1)))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 0 J)))
          (RETURN NIL)))
        (SETQ LCOEFF (AEVAL* (LIST 'CONS (LIST 'TABCOEF J) LCOEFF)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT)
                   (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                 J))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'TESTPARAM LCOEFF 'X)))
        (PROGN
         (ASSGNPRI (AEVAL "nombre de parametres ?  ") NIL 'ONLY)
         (SETQ IPARAM (AEVAL (LIST 'XREAD 'NIL)))
         (COND
          ((EVALNEQ (AEVAL IPARAM) 0)
           (PROGN
            (SETQ PARAM (AEVAL (LIST 'LIST)))
            (COND
             ((EVALEQUAL (AEVAL IPARAM) 1)
              (ASSGNPRI (AEVAL "donner ce parametre :") NIL 'ONLY))
             (T (ASSGNPRI (AEVAL "donner ces parametres :") NIL 'ONLY)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* IPARAM) I))
                (RETURN NIL)))
              (SETQ PARAM (AEVAL* (LIST 'CONS (LIST 'XREAD 'NIL) PARAM)))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))
       (T (SETQ PARAM (AEVAL (LIST 'LIST)))))
      (AEVAL (CLEAR (LIST 'TABCOEF)))
      (RETURN (AEVAL (LIST 'LIST LCOEFF PARAM))))) 
(PUT 'DELIRE 'NUMBER-OF-ARGS 5) 
(FLAG '(DELIRE) 'OPFN) 
(PUT 'DELIRE 'DEFINED-ON-LINE '897) 
(PUT 'DELIRE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'DELIRE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DELIRE (X K GRILLE LCOEFF PARAM)
    (PROG (PROF ORDREMAX NS N L)
      (SETQ PROF 0)
      (SETQ ORDREMAX 0)
      (SETQ NS 0)
      (SETQ N (AEVAL (LIST 'DIFFERENCE (LIST 'LENGTH LCOEFF) 1)))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST 'DER (IEVAL N)) (LIST '&SOLUTION (IEVAL N))
                     (LIST '&AA (IEVAL N))))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST 'GRI 20) (LIST 'LU 20) (LIST 'QX 20) (LIST 'EQU 20)
                     (LIST 'CL 20 (IEVAL N)) (LIST 'CLU 20 (IEVAL N))))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST 'NBARETE 20) (LIST 'XPOLY 20 (IEVAL N))
                     (LIST 'YPOLY 20 (IEVAL N)) (LIST 'PPOLY 20 (IEVAL N))
                     (LIST 'LPOLY 20 (IEVAL N))))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST 'XSQ (IEVAL (LIST 'PLUS N 1)))
                     (LIST 'YSQ (IEVAL (LIST 'PLUS N 1)))
                     (LIST 'RXM (IEVAL (LIST 'PLUS N 1)))))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST 'RU 20 (IEVAL N)) (LIST 'MULTI 20 (IEVAL N))
                     (LIST 'NBRACINE 20)))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'RAC 10) (LIST 'ORDREMULT 10)))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'CONDPROF 20) (LIST 'SOLPARM (IEVAL N))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'SOLEQU (IEVAL N))))
      (AEVAL (ON (LIST 'GCD)))
      (SETQ L (AEVAL LCOEFF))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'CL 0 I) (AEVAL* (LIST 'FIRST L)))
         (SETQ L (AEVAL* (LIST 'REST L)))
         (AEVAL* 'NIL))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'IPARAM (AEVAL (LIST 'LENGTH PARAM)))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'PARM (IEVAL 'IPARAM))))
      (SETK (LIST 'PARM 0) (AEVAL 'IPARAM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'IPARAM) I)) (RETURN NIL)))
        (SETK (LIST 'PARM I) (AEVAL* (LIST 'PART PARAM I)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK (LIST 'GRI 0) (AEVAL GRILLE))
      (SETK (LIST 'DER 0) (AEVAL (LIST '&FF X)))
      (PROG (IK)
        (SETQ IK 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) IK)) (RETURN NIL)))
        (SETK (LIST 'DER IK)
              (AEVAL*
               (LIST 'DIFFERENCE (LIST 'DF (LIST 'DER (DIFFERENCE IK 1)) X)
                     (LIST 'TIMES
                           (LIST 'QUOTIENT (LIST 'TIMES '&LAMB '&U)
                                 (LIST 'EXPT X (LIST 'PLUS '&LAMB 1)))
                           (LIST 'DER (DIFFERENCE IK 1))))))
        (SETQ IK
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 IK))
        (GO LAB))
      (SETK (LIST 'QX 0) (AEVAL 0))
      (SETQ ORDREMAX (AEVAL N))
      (SETQ PROF (AEVAL 1))
      (SETK (LIST 'CONDPROF 0) (AEVAL (LIST 'LIST)))
      (SETQ NS (AEVAL (LIST 'NEWTON_POLY PROF ORDREMAX N X K 0)))
      (SETQ L
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NS I)) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (LIST 'SOLEQU I)) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE NS I))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'SOLEQU I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL
       (CLEAR
        (LIST 'DER '&SOLUTION '&AA 'GRI 'LU 'QX 'EQU 'CL 'CLU 'NBARETE 'XPOLY
              'YPOLY 'PPOLY 'LPOLY 'XSQ 'YSQ 'RXM 'TJ 'RU 'MULTI 'NBRACINE
              'PARM)))
      (AEVAL (CLEAR (LIST 'RAC 'ORDREMULT)))
      (AEVAL (CLEAR (LIST 'CONDPROF 'SOLPARM 'SOLEQU)))
      (RETURN (AEVAL L)))) 
(PUT 'TESTPARAM 'NUMBER-OF-ARGS 2) 
(FLAG '(TESTPARAM) 'OPFN) 
(PUT 'TESTPARAM 'DEFINED-ON-LINE '976) 
(PUT 'TESTPARAM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'TESTPARAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTPARAM (L X)
    (PROG (B L1 L2)
      (SETQ B (AEVAL 'NIL))
      (SETQ L1 (AEVAL L))
      (WHILE
       (AND (EVALEQUAL (AEVAL* B) (AEVAL* 'NIL))
            (EVALNEQ (AEVAL* L1) (AEVAL* (LIST 'LIST))))
       (PROGN
        (SETQ L2
                (AEVAL*
                 (LIST 'COEFFP (LIST 'LIST (LIST 'FIRST L1)) (LIST 'LIST X))))
        (PROG (ELT)
          (SETQ ELT (GETRLIST (AEVAL* L2)))
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (PROGN
              (COND ((NOT (EVALNUMBERP (AEVAL* ELT))) (SETQ B (AEVAL* 'TRUE))))
              (AEVAL* 'NIL)))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))
        (SETQ L1 (AEVAL* (LIST 'REST L1)))
        (AEVAL* 'NIL)))
      (RETURN (AEVAL B)))) 
(PUT 'COEFFP 'NUMBER-OF-ARGS 2) 
(FLAG '(COEFFP) 'OPFN) 
(PUT 'COEFFP 'DEFINED-ON-LINE '991) 
(PUT 'COEFFP 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'COEFFP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFFP (POLY VAR)
    (PROG (L L1)
      (COND
       ((EVALEQUAL (AEVAL VAR) (AEVAL (LIST 'LIST))) (RETURN (AEVAL POLY))))
      (SETQ L (AEVAL (LIST 'LIST)))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL POLY)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ L1 (AEVAL (LIST 'COEFF ELT (LIST 'FIRST VAR))))
            (PROG (EL1)
              (SETQ EL1 (GETRLIST (AEVAL L1)))
             LAB
              (COND ((NULL EL1) (RETURN NIL)))
              ((LAMBDA (EL1)
                 (COND
                  ((EVALNEQ (AEVAL EL1) 0)
                   (SETQ L (AEVAL (LIST 'APPEND L (LIST 'LIST EL1)))))))
               (CAR EL1))
              (SETQ EL1 (CDR EL1))
              (GO LAB))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (AEVAL (LIST 'COEFFP L (LIST 'REST VAR)))))) 
(PUT 'TRANSFORMATION 'NUMBER-OF-ARGS 2) 
(FLAG '(TRANSFORMATION) 'OPFN) 
(PUT 'TRANSFORMATION 'DEFINED-ON-LINE '1010) 
(PUT 'TRANSFORMATION 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'TRANSFORMATION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFORMATION (LCOEFF PARAM)
    (PROG (F ID FCT FCT1 COEFF1 LSOR)
      (SETK 'OK (AEVAL 'OUI))
      (SETQ COEFF1 (AEVAL LCOEFF))
      (WHILE (EQ (REVALX 'OK) (REVALX 'OUI))
             (PROGN
              (ASSGNPRI (AEVAL* "derivation : 1; ") NIL 'ONLY)
              (ASSGNPRI (AEVAL* "changement de variable : 2; ") NIL 'ONLY)
              (ASSGNPRI (AEVAL* "changement de fonction inconnue : 3;") NIL
                        'ONLY)
              (ASSGNPRI (AEVAL* "substitution : 4;") NIL 'ONLY)
              (SETK 'ICHOIX (AEVAL* (LIST 'XREAD 'NIL)))
              (COND
               ((EVALEQUAL (AEVAL* 'ICHOIX) 1)
                (PROGN
                 (ASSGNPRI (AEVAL* "donner le second membre : ") NIL 'ONLY)
                 (SETQ F (AEVAL* (LIST 'XREAD 'NIL)))
                 (ASSGNPRI (AEVAL* "donner le nombre de derivations : ") NIL
                           'ONLY)
                 (SETQ ID (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETQ COEFF1 (AEVAL* (LIST 'CHANGEHOM COEFF1 'X F ID)))
                 (SETQ LSOR (AEVAL* (LIST 'LIST COEFF1 PARAM))))))
              (COND
               ((EVALEQUAL (AEVAL* 'ICHOIX) 2)
                (PROGN
                 (ASSGNPRI
                  (AEVAL*
                   "valeur de x en fonction de la nouvelle variable v ? ")
                  NIL 'ONLY)
                 (SETQ FCT (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETQ COEFF1
                         (AEVAL* (LIST 'DESIR-CHANGEVAR COEFF1 'X 'V FCT)))
                 (SETQ COEFF1
                         (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                           (SETQ ELT (GETRLIST (AEVAL* COEFF1)))
                           (COND ((NULL ELT) (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (ELT)
                                               (AEVAL*
                                                (LIST 'SUB (LIST 'EQUAL 'V 'X)
                                                      ELT)))
                                             (CAR ELT))
                                            NIL)))
                          LOOPLABEL
                           (SETQ ELT (CDR ELT))
                           (COND
                            ((NULL ELT) (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (AEVAL*
                                        (LIST 'SUB (LIST 'EQUAL 'V 'X) ELT)))
                                     (CAR ELT))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ LSOR (AEVAL* (LIST 'LIST COEFF1 PARAM))))))
              (COND
               ((EVALEQUAL (AEVAL* 'ICHOIX) 3)
                (PROGN
                 (ASSGNPRI
                  (AEVAL*
                   "valeur de y en fonction de la nouvelle fonction inconnue q ?")
                  NIL 'ONLY)
                 (SETQ FCT (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETQ COEFF1 (AEVAL* (LIST 'CHANGEFONC COEFF1 'X 'Q FCT)))
                 (SETQ LSOR (AEVAL* (LIST 'LIST COEFF1 PARAM))))))
              (COND
               ((EVALEQUAL (AEVAL* 'ICHOIX) 4)
                (PROGN
                 (ASSGNPRI (AEVAL* "donner la regle de substitution , ") NIL
                           'ONLY)
                 (ASSGNPRI
                  (AEVAL* "le premier membre de l'{galit{ ,puis le second : ")
                  NIL 'ONLY)
                 (SETQ FCT (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETQ FCT1 (AEVAL* (LIST 'XREAD 'NIL)))
                 (SETQ LSOR (AEVAL* (LIST 'SUBSFONC COEFF1 PARAM FCT FCT1)))
                 (SETQ COEFF1 (AEVAL* (LIST 'FIRST LSOR)))
                 (AEVAL* 'NIL))))
              (ASSGNPRI (AEVAL* "transformation ? (oui;/non;)  ") NIL 'ONLY)
              (SETK 'OK (AEVAL* (LIST 'XREAD 'NIL)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL LSOR)))) 
(PUT 'SUBSFONC 'NUMBER-OF-ARGS 4) 
(FLAG '(SUBSFONC) 'OPFN) 
(PUT 'SUBSFONC 'DEFINED-ON-LINE '1062) 
(PUT 'SUBSFONC 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SUBSFONC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBSFONC (LCOEFF PARAM FCT FCT1)
    (PROG (LSOR LSOR1 J)
      (SETQ J 0)
      (SETQ LSOR
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (GETRLIST (AEVAL LCOEFF)))
                (COND ((NULL ELT) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (AEVAL
                                     (LIST 'SUB (LIST 'EQUAL FCT FCT1) ELT)))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (AEVAL (LIST 'SUB (LIST 'EQUAL FCT FCT1) ELT)))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL LSOR)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ J (AEVAL (PLUS J 1)))
            (PROGN
             (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
             (ASSGNPRI J NIL NIL)
             (ASSGNPRI (AEVAL ") = ") NIL NIL)
             (ASSGNPRI (AEVAL ELT) NIL 'LAST))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ LSOR1
              (PROG (ELT)
                (SETQ ELT (GETRLIST (AEVAL PARAM)))
               LAB
                (COND ((NULL ELT) (RETURN NIL)))
                ((LAMBDA (ELT)
                   (COND
                    ((EVALNEQ (AEVAL FCT) (AEVAL ELT))
                     (AEVAL (LIST 'COLLECT ELT)))))
                 (CAR ELT))
                (SETQ ELT (CDR ELT))
                (GO LAB)))
      (COND
       ((EVALEQUAL (AEVAL LSOR1) 0)
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "nouvelle liste de parametres : ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'LIST)) NIL 'LAST))
         (RETURN (AEVAL (LIST 'LIST LSOR (LIST 'LIST))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "nouvelle liste de parametres : ") NIL 'FIRST)
          (ASSGNPRI (AEVAL LSOR1) NIL 'LAST))
         (RETURN (AEVAL (LIST 'LIST LSOR LSOR1)))
         (AEVAL 'NIL)))))) 
(PUT 'CHANGE 'NUMBER-OF-ARGS 6) 
(FLAG '(CHANGE) 'OPFN) 
(PUT 'CHANGE 'DEFINED-ON-LINE '1078) 
(PUT 'CHANGE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGE (Y X V FCT EXP N)
    (PROG (&EXP)
      (SETK (LIST '&HP 'XT)
            (AEVAL
             (LIST 'QUOTIENT 1
                   (LIST 'DF (LIST 'SUB (LIST 'EQUAL V 'XT) FCT) 'XT))))
      (SETQ &EXP (AEVAL EXP))
      (PROG (I)
        (SETQ I (AEVAL* N))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 0 I)))
          (RETURN NIL)))
        (SETQ &EXP
                (AEVAL*
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DF Y X I) (LIST '&D 'XT I))
                       &EXP)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT)
                   (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                 I))
        (GO LAB))
      (SETQ &EXP (AEVAL (LIST 'SUB (LIST 'EQUAL X FCT) &EXP)))
      (AEVAL (DEPEND (LIST Y V)))
      (PROG (I)
        (SETQ I (AEVAL* N))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 0 I)))
          (RETURN NIL)))
        (SETQ &EXP
                (AEVAL*
                 (LIST 'SUB
                       (LIST 'EQUAL (LIST 'DF (LIST '&FG 'XT) 'XT I)
                             (LIST 'DF Y V I))
                       &EXP)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT)
                   (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                 I))
        (GO LAB))
      (RETURN (AEVAL (LIST 'SUB (LIST 'EQUAL 'XT V) &EXP))))) 
(OPERATOR (LIST '&FF '&HP '&FG)) 
(PUT '&D 'NUMBER-OF-ARGS 2) 
(FLAG '(&D) 'OPFN) 
(PUT '&D 'DEFINED-ON-LINE '1109) 
(PUT '&D 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT '&D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE &D (XT N)
    (PROG ()
      (COND ((EVALEQUAL (AEVAL N) 0) (RETURN (AEVAL (LIST '&FG XT))))
            ((AND (FIXP (REVALX N)) (EVALGREATERP (AEVAL N) 0))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST '&HP XT)
                     (LIST 'DF (LIST '&D XT (LIST 'DIFFERENCE N 1)) XT)))))))) 
(PUT 'NEWTON_POLY 'NUMBER-OF-ARGS 6) 
(FLAG '(NEWTON_POLY) 'OPFN) 
(PUT 'NEWTON_POLY 'DEFINED-ON-LINE '1116) 
(PUT 'NEWTON_POLY 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'NEWTON_POLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWTON_POLY (PROF ORDREMAX N X K NS)
    (PROG (NBA NADEP NBSOL Q NBS CONDIT SOL SUBSTITUTION)
      (SETQ NBA 0)
      (SETQ NADEP 0)
      (SETQ NBSOL 0)
      (SETQ Q 0)
      (SETQ NBS (AEVAL NS))
      (SETQ NBA (AEVAL (LIST 'POLYGONENRM PROF ORDREMAX X)))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE NBA J)) (RETURN NIL)))
          (PROGN
           (ASSGNPRI (AEVAL* (LIST 'XPOLY PROF J)) NIL 'FIRST)
           (ASSGNPRI (AEVAL* " ") NIL NIL)
           (ASSGNPRI (AEVAL* (LIST 'YPOLY PROF J)) NIL NIL)
           (ASSGNPRI (AEVAL* " ") NIL NIL)
           (ASSGNPRI (AEVAL* (LIST 'PPOLY PROF J)) NIL NIL)
           (ASSGNPRI (AEVAL* " ") NIL NIL)
           (ASSGNPRI (AEVAL* (LIST 'LPOLY PROF J)) NIL 'LAST))
          (SETQ J (PLUS2 J 1))
          (GO LAB))))
      (SETQ NADEP (AEVAL 1))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'NUM (LIST 'PPOLY PROF 1))) 0)
        (PROGN
         (SETQ NBSOL (AEVAL (LIST 'LPOLY PROF 1)))
         (AEVAL (LIST 'NOUVEAUXAJ PROF N X))
         (SETK 'CONDL (AEVAL (LIST 'CONDPROF PROF)))
         (COND
          ((BOOLVALUE* (REVALX *TRDESIR))
           (PROGN
            (ASSGNPRI (AEVAL "Equation reduite : ") NIL 'ONLY)
            (PROG (I)
              (SETQ I (AEVAL* N))
             LAB
              (COND
               ((|AMINUSP:| (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 1 I)))
                (RETURN NIL)))
              (PROGN
               (ASSGNPRI (AEVAL* "     ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* (LIST '&AA I)) NIL NIL)
               (ASSGNPRI (AEVAL* " * DF(Y,XT,") NIL NIL)
               (ASSGNPRI (AEVAL* I) NIL NIL)
               (ASSGNPRI (AEVAL* ") + ") NIL 'LAST))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                       I))
              (GO LAB))
            (PROGN
             (ASSGNPRI (AEVAL "     ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST '&AA 0)) NIL NIL)
             (ASSGNPRI (AEVAL " * Y") NIL 'LAST)))))
         (SETQ NBSOL (AEVAL (LIST 'FROBENIUS N 'XT K)))
         (COND
          ((NEQ NBSOL 0)
           (PROG (I)
             (SETQ I 1)
            LAB
             (COND ((MINUSP (DIFFERENCE NBSOL I)) (RETURN NIL)))
             (PROGN
              (SETK (LIST 'SOLEQU (LIST 'PLUS NBS I)) (AEVAL* (LIST 'LIST)))
              (PROG (EL)
                (SETQ EL (GETRLIST (AEVAL* (LIST 'SOLPARM I))))
               LAB
                (COND ((NULL EL) (RETURN NIL)))
                ((LAMBDA (EL)
                   (PROGN
                    (COND
                     ((EVALGREATERP (AEVAL* (LIST 'LENGTH EL)) 1)
                      (SETQ CONDIT (AEVAL* (LIST 'SECOND EL))))
                     (T (SETQ CONDIT (AEVAL* (LIST 'LIST)))))
                    (SETQ SOL (AEVAL* (LIST 'FIRST EL)))
                    (SETQ SOL
                            (AEVAL*
                             (LIST 'APPEND
                                   (LIST 'LIST
                                         (LIST 'SUB
                                               (LIST 'EQUAL X
                                                     (LIST 'EXPT 'XT
                                                           (LIST 'QUOTIENT 1
                                                                 (LIST 'GRI
                                                                       (LIST
                                                                        'DIFFERENCE
                                                                        PROF
                                                                        1)))))
                                               (LIST 'QX
                                                     (LIST 'DIFFERENCE PROF
                                                           1)))
                                         (LIST 'GRI (LIST 'DIFFERENCE PROF 1)))
                                   SOL)))
                    (SETK (LIST 'SOLEQU (LIST 'PLUS NBS I))
                          (AEVAL*
                           (LIST 'APPEND (LIST 'SOLEQU (LIST 'PLUS NBS I))
                                 (LIST 'LIST (LIST 'LIST SOL CONDIT)))))
                    (AEVAL* 'NIL)))
                 (CAR EL))
                (SETQ EL (CDR EL))
                (GO LAB))
              (AEVAL* 'NIL))
             (SETQ I (PLUS2 I 1))
             (GO LAB))))
         (SETQ NBS (AEVAL (LIST 'PLUS NBS NBSOL)))
         (SETQ NADEP (AEVAL 2))
         (AEVAL (CLEAR (LIST '&F '&DEGREC)))
         (AEVAL 'NIL))))
      (PROG (NA)
        (SETQ NA NADEP)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'NBARETE PROF)) NA))
          (RETURN NIL)))
        (SETQ NBS (AEVAL* (LIST 'NEWTON_POLYARETE PROF NA N X K NBS)))
        (SETQ NA
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 NA))
        (GO LAB))
      (RETURN (AEVAL NBS)))) 
(PUT 'NEWTON_POLYARETE 'NUMBER-OF-ARGS 6) 
(FLAG '(NEWTON_POLYARETE) 'OPFN) 
(PUT 'NEWTON_POLYARETE 'DEFINED-ON-LINE '1192) 
(PUT 'NEWTON_POLYARETE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'NEWTON_POLYARETE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWTON_POLYARETE (PROF NA N X K NBS)
    (PROG (Q ORDREMAX)
      (SETQ Q (AEVAL (LIST 'DEN (LIST 'PPOLY PROF NA))))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROGN
         (ASSGNPRI (AEVAL "        ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'XPOLY PROF NA)) NIL NIL)
         (ASSGNPRI (AEVAL "  ") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'YPOLY PROF NA)) NIL NIL)
         (ASSGNPRI (AEVAL "  ") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'PPOLY PROF NA)) NIL NIL)
         (ASSGNPRI (AEVAL "  ") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'LPOLY PROF NA)) NIL 'LAST))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'LPOLY PROF NA)) 1)
        (SETK (LIST 'GRI PROF) (AEVAL (LIST 'GRI (LIST 'DIFFERENCE PROF 1)))))
       (T
        (SETK (LIST 'GRI PROF)
              (AEVAL
               (LIST 'TIMES
                     (LIST 'GCD Q
                           (LIST 'QUOTIENT 1
                                 (LIST 'GRI (LIST 'DIFFERENCE PROF 1))))
                     (LIST 'QUOTIENT (LIST 'GRI (LIST 'DIFFERENCE PROF 1))
                           Q))))))
      (SETK (LIST 'LU PROF)
            (AEVAL
             (LIST 'SUB (LIST 'EQUAL '&LAMB (LIST 'PPOLY PROF NA))
                   (LIST 'TIMES (LIST 'CL (LIST 'DIFFERENCE PROF 1) 0)
                         (LIST 'DER 0)))))
      (PROG (IK)
        (SETQ IK 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) IK)) (RETURN NIL)))
        (SETK (LIST 'LU PROF)
              (AEVAL*
               (LIST 'PLUS (LIST 'LU PROF)
                     (LIST 'SUB (LIST 'EQUAL '&LAMB (LIST 'PPOLY PROF NA))
                           (LIST 'TIMES (LIST 'CL (LIST 'DIFFERENCE PROF 1) IK)
                                 (LIST 'DER IK))))))
        (SETQ IK
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 IK))
        (GO LAB))
      (AEVAL (LIST 'DECOMPLU PROF N X NA))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROGN
         (ASSGNPRI (AEVAL "Equation caracteristique : ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'EQU PROF)) NIL 'LAST))))
      (AEVAL (LIST 'RACINESEQU PROF NA))
      (PROG (NK)
        (SETQ NK 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'NBRACINE PROF)) NK))
          (RETURN NIL)))
        (PROGN
         (SETK (LIST 'QX PROF)
               (AEVAL*
                (LIST 'PLUS (LIST 'QX (LIST 'DIFFERENCE PROF 1))
                      (LIST 'QUOTIENT (LIST 'RU PROF NK)
                            (LIST 'EXPT X (LIST 'PPOLY PROF NA))))))
         (PROG (IK)
           (SETQ IK 0)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) IK)) (RETURN NIL)))
           (SETK (LIST 'CL PROF IK)
                 (AEVAL*
                  (LIST 'SUB (LIST 'EQUAL '&U (LIST 'RU PROF NK))
                        (LIST 'CLU PROF IK))))
           (SETQ IK
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    IK))
           (GO LAB))
         (SETQ ORDREMAX (AEVAL* (LIST 'MULTI PROF NK)))
         (COND
          ((BOOLVALUE* (REVALX *TRDESIR))
           (PROGN
            (ASSGNPRI (AEVAL* "Racine eq. carac. : ") NIL 'FIRST)
            (ASSGNPRI (AEVAL* (LIST 'RU PROF NK)) NIL 'LAST))))
         (COND
          ((EVALLESSP (AEVAL* PROF) 20)
           (SETQ NBS
                   (AEVAL*
                    (LIST 'NEWTON_POLY (LIST 'PLUS PROF 1) ORDREMAX N X K
                          NBS))))
          (T
           (PROGN
            (ASSGNPRI (AEVAL* "la profondeur 20 est atteinte :") NIL 'FIRST)
            (ASSGNPRI (AEVAL* " le calcul est arrete pour cette racine") NIL
                      'LAST))))
         (AEVAL* 'NIL))
        (SETQ NK
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 NK))
        (GO LAB))
      (RETURN (AEVAL NBS)))) 
(PUT 'SQUELETTE 'NUMBER-OF-ARGS 3) 
(FLAG '(SQUELETTE) 'OPFN) 
(PUT 'SQUELETTE 'DEFINED-ON-LINE '1243) 
(PUT 'SQUELETTE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'SQUELETTE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SQUELETTE (PROF ORDREMAX X)
    (PROG (T00 TQ YI CC IK NK NBELSQ DEGDEN DEGRE RXI)
      (SETQ IK 0)
      (SETQ NK 0)
      (SETQ NBELSQ 0)
      (SETQ DEGDEN 0)
      (SETQ DEGRE 0)
      (SETQ RXI 0)
      (SETK (LIST 'CONDPROF PROF)
            (AEVAL (LIST 'CONDPROF (LIST 'DIFFERENCE PROF 1))))
      (SETQ T00 (AEVAL 0))
      (PROG (IK)
        (SETQ IK 0)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDREMAX) IK)) (RETURN NIL)))
        (COND
         ((EVALNEQ (AEVAL* (LIST 'CL (LIST 'DIFFERENCE PROF 1) IK)) 0)
          (PROGN
           (SETQ NK (AEVAL* (PLUS NK 1)))
           (SETK (LIST 'XSQ NK) (AEVAL* IK)))))
        (SETQ IK
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 IK))
        (GO LAB))
      (SETQ NBELSQ (AEVAL NK))
      (PROG (NK)
        (SETQ NK 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NBELSQ NK)) (RETURN NIL)))
        (PROGN
         (SETQ TQ
                 (AEVAL*
                  (LIST 'SUB
                        (LIST 'EQUAL X
                              (LIST 'EXPT '&T
                                    (LIST 'QUOTIENT 1
                                          (LIST 'GRI
                                                (LIST 'DIFFERENCE PROF 1)))))
                        (LIST 'CL (LIST 'DIFFERENCE PROF 1) (LIST 'XSQ NK)))))
         (SETQ DEGDEN (AEVAL* (LIST 'DEG (LIST 'DEN TQ) '&T)))
         (SETQ CC (AEVAL* (LIST 'COEFF (LIST 'NUM TQ) '&T)))
         (SETQ IK (AEVAL* 0))
         (WHILE (EVALEQUAL (AEVAL* (LIST 'FIRST CC)) 0)
                (PROGN
                 (SETQ IK (AEVAL* (PLUS IK 1)))
                 (SETQ CC (AEVAL* (LIST 'REST CC)))))
         (SETK (LIST 'YSQ NK)
               (AEVAL*
                (LIST 'DIFFERENCE
                      (LIST 'TIMES (DIFFERENCE IK DEGDEN)
                            (LIST 'GRI (LIST 'DIFFERENCE PROF 1)))
                      (LIST 'XSQ NK))))
         (SETK (LIST 'TRAV1 NK) (AEVAL* (LIST 'FIRST CC)))
         (AEVAL* 'NIL))
        (SETQ NK (PLUS2 NK 1))
        (GO LAB))
      (SETK (LIST 'RXM 0) (AEVAL 0))
      (SETQ IK (AEVAL 0))
      (WHILE (EVALLESSP (AEVAL* (LIST 'RXM IK)) NBELSQ)
             (PROGN
              (SETQ RXI (AEVAL* (LIST 'PLUS (LIST 'RXM IK) 1)))
              (SETQ YI (AEVAL* (LIST 'YSQ RXI)))
              (PROG (J)
                (SETQ J (PLUS RXI 1))
               LAB
                (COND ((MINUSP (DIFFERENCE NBELSQ J)) (RETURN NIL)))
                (COND
                 ((EVALLEQ
                   (AEVAL* (LIST 'NUM (LIST 'DIFFERENCE (LIST 'YSQ J) YI))) 0)
                  (PROGN
                   (SETQ YI (AEVAL* (LIST 'YSQ J)))
                   (SETQ RXI (AEVAL* J)))))
                (SETQ J (PLUS2 J 1))
                (GO LAB))
              (SETQ IK (AEVAL* (PLUS IK 1)))
              (SETK (LIST 'RXM IK) (AEVAL* RXI))
              (AEVAL* 'NIL)))
      (RETURN IK))) 
(PUT 'POLYGONENRM 'NUMBER-OF-ARGS 3) 
(FLAG '(POLYGONENRM) 'OPFN) 
(PUT 'POLYGONENRM 'DEFINED-ON-LINE '1289) 
(PUT 'POLYGONENRM 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'POLYGONENRM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYGONENRM (PROF ORDREMAX X)
    (PROG (YDEP YFINAL PENTE IK IMIN JMIN NBMIN RXMIN LONG XFINAL XDEP DEG1
           RXI)
      (SETQ IK 0)
      (SETQ IMIN 0)
      (SETQ JMIN 0)
      (SETQ NBMIN 0)
      (SETQ RXMIN 0)
      (SETQ LONG 0)
      (SETQ XFINAL 0)
      (SETQ XDEP 0)
      (SETQ DEG1 0)
      (SETQ RXI 0)
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'TRAV1 20)))
      (SETQ NBMIN (AEVAL (LIST 'SQUELETTE PROF ORDREMAX X)))
      (SETQ IK (AEVAL 0))
      (SETQ XFINAL (AEVAL (LIST 'XSQ (LIST 'RXM 1))))
      (SETQ YFINAL (AEVAL (LIST 'YSQ (LIST 'RXM 1))))
      (SETK (LIST 'XPOLY PROF 1) (AEVAL 0))
      (SETK (LIST 'YPOLY PROF 1) (AEVAL YFINAL))
      (SETK (LIST 'PPOLY PROF 1) (AEVAL 0))
      (SETQ RXI (AEVAL (LIST 'RXM 1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'PARM 0)) I))
          (RETURN NIL)))
        (SETQ DEG1
                (AEVAL*
                 (LIST 'PLUS DEG1
                       (LIST 'DEG (LIST 'TRAV1 RXI) (LIST 'PARM I)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((NEQ DEG1 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRDESIR))
           (PROGN
            (ASSGNPRI (AEVAL "Si : ") NIL 'FIRST)
            (ASSGNPRI (AEVAL (LIST 'TRAV1 RXI)) NIL NIL)
            (ASSGNPRI (AEVAL " non nul") NIL 'LAST))))
         (COND
          ((NOT
            (BOOLVALUE*
             (REVALX
              (LIST 'MEMBRE (LIST 'LIST (LIST 'TRAV1 RXI) 'NONNUL)
                    (LIST 'CONDPROF PROF)))))
           (SETK (LIST 'CONDPROF PROF)
                 (AEVAL
                  (LIST 'CONS (LIST 'LIST (LIST 'TRAV1 RXI) 'NONNUL)
                        (LIST 'CONDPROF PROF))))))
         (AEVAL 'NIL))))
      (COND
       ((NEQ XFINAL 0)
        (PROGN
         (SETQ IK (AEVAL 1))
         (SETK (LIST 'LPOLY PROF 1) (AEVAL XFINAL)))))
      (SETQ JMIN (AEVAL 1))
      (WHILE (EVALLESSP XFINAL (AEVAL* ORDREMAX))
             (PROGN
              (SETQ IK (AEVAL* (PLUS IK 1)))
              (SETK (LIST 'XPOLY PROF IK) (AEVAL* XFINAL))
              (SETQ XDEP (AEVAL* XFINAL))
              (SETK (LIST 'YPOLY PROF IK) (AEVAL* YFINAL))
              (SETQ YDEP (AEVAL* YFINAL))
              (SETQ IMIN (AEVAL* (PLUS JMIN 1)))
              (SETQ JMIN (AEVAL* IMIN))
              (SETQ XFINAL (AEVAL* (LIST 'XSQ (LIST 'RXM IMIN))))
              (SETQ YFINAL (AEVAL* (LIST 'YSQ (LIST 'RXM IMIN))))
              (SETK (LIST 'LPOLY PROF IK) (AEVAL* (DIFFERENCE XFINAL XDEP)))
              (SETK (LIST 'PPOLY PROF IK)
                    (AEVAL*
                     (LIST 'QUOTIENT (LIST 'DIFFERENCE YFINAL YDEP)
                           (LIST 'LPOLY PROF IK))))
              (SETQ DEG1 (AEVAL* 0))
              (PROG (II)
                (SETQ II 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'PARM 0)) II))
                  (RETURN NIL)))
                (SETQ DEG1
                        (AEVAL*
                         (LIST 'PLUS DEG1
                               (LIST 'DEG (LIST 'TRAV1 (LIST 'RXM IMIN))
                                     (LIST 'PARM II)))))
                (SETQ II
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         II))
                (GO LAB))
              (COND
               ((NEQ DEG1 0)
                (PROGN
                 (COND
                  ((BOOLVALUE* (REVALX *TRDESIR))
                   (PROGN
                    (ASSGNPRI (AEVAL* "Si : ") NIL 'FIRST)
                    (ASSGNPRI (AEVAL* (LIST 'TRAV1 (LIST 'RXM IMIN))) NIL NIL)
                    (ASSGNPRI (AEVAL* " non nul") NIL 'LAST))))
                 (COND
                  ((NOT
                    (BOOLVALUE*
                     (REVALX
                      (LIST 'MEMBRE
                            (LIST 'LIST (LIST 'TRAV1 (LIST 'RXM IMIN)) 'NONNUL)
                            (LIST 'CONDPROF PROF)))))
                   (SETK (LIST 'CONDPROF PROF)
                         (AEVAL*
                          (LIST 'CONS
                                (LIST 'LIST (LIST 'TRAV1 (LIST 'RXM IMIN))
                                      'NONNUL)
                                (LIST 'CONDPROF PROF))))))
                 (AEVAL* 'NIL))))
              (WHILE (LESSP IMIN NBMIN)
                     (PROGN
                      (SETQ IMIN (AEVAL* (PLUS IMIN 1)))
                      (SETQ RXMIN (AEVAL* (LIST 'RXM IMIN)))
                      (SETQ LONG
                              (AEVAL*
                               (LIST 'DIFFERENCE (LIST 'XSQ RXMIN) XDEP)))
                      (SETQ PENTE
                              (AEVAL*
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE (LIST 'YSQ RXMIN) YDEP)
                                     LONG)))
                      (COND
                       ((EVALLEQ
                         (AEVAL*
                          (LIST 'NUM
                                (LIST 'DIFFERENCE PENTE
                                      (LIST 'PPOLY PROF IK))))
                         0)
                        (PROGN
                         (SETK (LIST 'LPOLY PROF IK) (AEVAL* LONG))
                         (SETK (LIST 'PPOLY PROF IK) (AEVAL* PENTE))
                         (SETQ XFINAL (AEVAL* (LIST 'XSQ RXMIN)))
                         (SETQ YFINAL (AEVAL* (LIST 'YSQ RXMIN)))
                         (SETQ JMIN (AEVAL* IMIN))
                         (AEVAL* 'NIL))))
                      (AEVAL* 'NIL)))
              (AEVAL* 'NIL)))
      (SETK (LIST 'NBARETE PROF) (AEVAL IK))
      (AEVAL (CLEAR (LIST 'TRAV1)))
      (RETURN IK))) 
(PUT 'NOUVEAUXAJ 'NUMBER-OF-ARGS 3) 
(FLAG '(NOUVEAUXAJ) 'OPFN) 
(PUT 'NOUVEAUXAJ 'DEFINED-ON-LINE '1370) 
(PUT 'NOUVEAUXAJ 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'NOUVEAUXAJ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NOUVEAUXAJ (PROF N X)
    (PROG (GR T00 COEFFS)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (SETK (LIST '&AA I) (AEVAL* (LIST 'CL (LIST 'DIFFERENCE PROF 1) I)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETQ GR
              (AEVAL (LIST 'QUOTIENT 1 (LIST 'GRI (LIST 'DIFFERENCE PROF 1)))))
      (SETK (LIST '&HP 'XT)
            (AEVAL (LIST 'QUOTIENT 1 (LIST 'DF (LIST 'EXPT 'XT GR) 'XT))))
      (SETQ T00
              (AEVAL
               (LIST 'NUM
                     (PROG (J FORALL-RESULT)
                       (SETQ J 0)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES
                                             (LIST 'SUB
                                                   (LIST 'EQUAL X
                                                         (LIST 'EXPT 'XT GR))
                                                   (LIST '&AA J))
                                             (LIST '&D 'XT J)))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1)))))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (PROGN
         (SETQ COEFFS
                 (COND ((EQUAL J 0) (AEVAL* (LIST 'COEFF T00 (LIST '&FG 'XT))))
                       ((EQUAL J 1)
                        (AEVAL*
                         (LIST 'COEFF T00 (LIST 'DF (LIST '&FG 'XT) 'XT))))
                       (T
                        (AEVAL*
                         (LIST 'COEFF T00 (LIST 'DF (LIST '&FG 'XT) 'XT J))))))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'LENGTH COEFFS)) 1)
           (SETK (LIST '&AA J) (AEVAL* 0)))
          (T (SETK (LIST '&AA J) (AEVAL* (LIST 'SECOND COEFFS)))))
         (SETQ T00 (AEVAL* (LIST 'FIRST COEFFS))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB)))) 
(PUT 'DECOMPLU 'NUMBER-OF-ARGS 4) 
(FLAG '(DECOMPLU) 'OPFN) 
(PUT 'DECOMPLU 'DEFINED-ON-LINE '1397) 
(PUT 'DECOMPLU 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'DECOMPLU 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DECOMPLU (PROF N X NA)
    (PROG (GR T00 TQ TJ1 TJ1C COEFFS)
      (SETQ GR (AEVAL (LIST 'QUOTIENT 1 (LIST 'GRI PROF))))
      (SETQ T00 (AEVAL (LIST 'NUM (LIST 'LU PROF))))
      (SETQ TQ (AEVAL (LIST 'DEN (LIST 'LU PROF))))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (PROGN
         (SETQ COEFFS
                 (COND ((EQUAL J 0) (AEVAL* (LIST 'COEFF T00 (LIST '&FF X))))
                       ((EQUAL J 1)
                        (AEVAL* (LIST 'COEFF T00 (LIST 'DF (LIST '&FF X) X))))
                       (T
                        (AEVAL*
                         (LIST 'COEFF T00 (LIST 'DF (LIST '&FF X) X J))))))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'LENGTH COEFFS)) 1)
           (PROGN
            (SETK (LIST 'CLU PROF J) (AEVAL* 0))
            (SETQ T00 (AEVAL* (LIST 'FIRST COEFFS)))))
          (T
           (PROGN
            (SETQ TJ1
                    (AEVAL*
                     (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT '&T GR))
                           (LIST 'SECOND COEFFS))))
            (SETQ TJ1C (AEVAL* (LIST 'COEFF TJ1 '&T)))
            (WHILE (EVALEQUAL (AEVAL* (LIST 'FIRST TJ1C)) 0)
                   (SETQ TJ1C (AEVAL* (LIST 'REST TJ1C))))
            (SETQ T00 (AEVAL* (LIST 'FIRST COEFFS)))
            (COND
             ((EQUAL J 0)
              (PROGN
               (SETK (LIST 'CLU PROF J)
                     (AEVAL* (LIST 'QUOTIENT (LIST 'SECOND COEFFS) TQ)))
               (SETK (LIST 'EQU PROF)
                     (AEVAL*
                      (LIST 'QUOTIENT (LIST 'NUM (LIST 'FIRST TJ1C))
                            (LIST 'EXPT '&U
                                  (LIST 'DIFFERENCE
                                        (LIST 'DEG
                                              (LIST 'NUM (LIST 'FIRST TJ1C))
                                              '&U)
                                        (LIST 'LPOLY PROF NA))))))))
             (T
              (SETK (LIST 'CLU PROF J)
                    (AEVAL* (LIST 'QUOTIENT (LIST 'SECOND COEFFS) TQ)))))
            (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB)))) 
(PUT 'RACINESEQU 'NUMBER-OF-ARGS 2) 
(FLAG '(RACINESEQU) 'OPFN) 
(PUT 'RACINESEQU 'DEFINED-ON-LINE '1426) 
(PUT 'RACINESEQU 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'RACINESEQU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RACINESEQU (PROF NA)
    (PROG (NRAC NK Q1 GQ G1 DEQU)
      (SETQ NK 0)
      (SETQ Q1 0)
      (SETQ GQ 0)
      (SETQ G1 0)
      (SETQ DEQU 0)
      (SETQ DEQU (AEVAL (LIST 'DEG (LIST 'EQU PROF) '&U)))
      (SETQ G1 (AEVAL (LIST 'DEN (LIST 'GRI (LIST 'DIFFERENCE PROF 1)))))
      (SETQ Q1 (AEVAL (LIST 'DEN (LIST 'PPOLY PROF NA))))
      (SETQ GQ (AEVAL (LIST 'GCD G1 Q1)))
      (WHILE (GREATERP GQ 1)
             (PROGN
              (SETQ G1 (AEVAL* (LIST 'QUOTIENT G1 GQ)))
              (SETQ Q1 (AEVAL* (LIST 'QUOTIENT Q1 GQ)))
              (SETQ GQ (AEVAL* (LIST 'GCD G1 Q1)))))
      (AEVAL (LET (LIST (LIST 'EQUAL (LIST 'EXPT '&U Q1) '&T))))
      (SETQ NRAC (AEVAL (LIST 'RACINE (LIST 'EQU PROF) '&T)))
      (PROG (IK)
        (SETQ IK 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NRAC) IK)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE Q1 J)) (RETURN NIL)))
          (PROGN
           (SETK (LIST 'MULTI PROF (PLUS (TIMES (DIFFERENCE IK 1) Q1) J))
                 (AEVAL* (LIST 'ORDREMULT IK)))
           (SETK (LIST 'RU PROF (PLUS (TIMES (DIFFERENCE IK 1) Q1) J))
                 (AEVAL*
                  (LIST 'TIMES
                        (LIST 'EXPT (LIST 'RAC IK) (LIST 'QUOTIENT 1 Q1))
                        (LIST 'EXPT 'E
                              (LIST 'TIMES 2 (DIFFERENCE J 1) 'I
                                    (LIST 'QUOTIENT 'PI Q1))))))
           (SETQ NK (AEVAL* (LIST 'PLUS NK (LIST 'ORDREMULT IK))))
           (AEVAL* 'NIL))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ IK
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 IK))
        (GO LAB))
      (SETK (LIST 'NBRACINE PROF) (AEVAL (LIST 'TIMES NRAC Q1)))
      (AEVAL (CLEAR (LIST (LIST 'EXPT '&U Q1))))
      (COND
       ((NEQ (DIFFERENCE DEQU NK) 0)
        (PROGN
         (ASSGNPRI (AEVAL "IL Y A ") NIL 'FIRST)
         (ASSGNPRI (AEVAL 'IK) NIL NIL)
         (ASSGNPRI (AEVAL " SOLUTIONS RELATIVES A L'ARETE ") NIL NIL)
         (ASSGNPRI (AEVAL NA) NIL NIL)
         (ASSGNPRI (AEVAL " QUI NE PEUVENT PAS ETRE ATTEINTES : ") NIL NIL)
         (ASSGNPRI (AEVAL "equation a resoudre de degre >=3 ") NIL 'LAST)))))) 
(OPERATOR (LIST '&G)) 
(PUT '&W 'NUMBER-OF-ARGS 4) 
(FLAG '(&W) 'OPFN) 
(PUT '&W 'DEFINED-ON-LINE '1466) 
(PUT '&W 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT '&W 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE &W (II X LAMBD K)
    (COND
     ((FIXP (REVALX K))
      (PROG (J FORALL-RESULT)
        (SETQ J 0)
        (SETQ FORALL-RESULT 0)
       LAB1
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) J)) (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT
                (AEVAL*
                 (LIST 'PLUS
                       (AEVAL*
                        (LIST 'TIMES (LIST 'DF (LIST '&G J) LAMBD II)
                              (LIST 'EXPT X J)))
                       FORALL-RESULT)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB1))))) 
(PUT 'FROBENIUS 'NUMBER-OF-ARGS 3) 
(FLAG '(FROBENIUS) 'OPFN) 
(PUT 'FROBENIUS 'DEFINED-ON-LINE '1469) 
(PUT 'FROBENIUS 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'FROBENIUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FROBENIUS (N X K)
    (PROG (NB NBRAC NBSOLUTION SS SY ESSAI)
      (SETQ NB 0)
      (SETQ NBRAC 0)
      (SETQ NBSOLUTION 0)
      (AEVAL (LIST 'EQUAIND N X K))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROGN
         (ASSGNPRI (AEVAL "Equation indicielle : ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST '&F 0)) NIL 'LAST))))
      (SETQ NB (AEVAL (LIST 'RACINE (LIST '&F 0) 'LAMBD)))
      (COND
       ((EQUAL NB 0)
        (PROGN
         (PROGN
          (ASSGNPRI
           (AEVAL "le calcul des racines est impossible dans ce cas.  ") NIL
           'FIRST)
          (ASSGNPRI (AEVAL "Utilisez la version ALGEBRIQUE. ") NIL 'LAST))
         (SETQ NBSOLUTION (AEVAL 0))
         (RETURN NBSOLUTION)
         (AEVAL 'NIL))))
      (SETQ NBRAC
              (PROG (I FORALL-RESULT)
                (SETQ I 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE NB I)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS (AEVAL* (LIST 'ORDREMULT I))
                               FORALL-RESULT)))
                (SETQ I (PLUS2 I 1))
                (GO LAB1)))
      (COND
       ((EQUAL NBRAC 1)
        (PROGN
         (SETQ NBSOLUTION (AEVAL 1))
         (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 1) 1))
         (SETK (LIST 'SOLPARM 1)
               (AEVAL
                (LIST 'LIST
                      (LIST 'LIST
                            (LIST 'LIST (LIST '&SOLUTION 1) (LIST 'RAC 1))
                            'CONDL))))
         (AEVAL 'NIL))))
      (COND
       ((EQUAL NBRAC 2)
        (PROGN
         (AEVAL (LIST 'CLASSEMENT2R X K))
         (SETQ NBSOLUTION (AEVAL 2))
         (AEVAL 'NIL))))
      (COND
       ((EQUAL NBRAC 3)
        (PROGN
         (AEVAL (LIST 'CLASSEMENT3R X K))
         (SETQ NBSOLUTION (AEVAL 3))
         (AEVAL 'NIL))))
      (COND
       ((GREATERP NBRAC 3)
        (ASSGNPRI
         (AEVAL "ce cas n'est pas traite. Utilisez la version ALGEBRIQUE") NIL
         'ONLY))
       (T
        (PROG (I)
          (SETQ I 0)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
          (AEVAL* (CLEAR (LIST (LIST '&G I))))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB))))
      (RETURN NBSOLUTION))) 
(PUT 'CLASSEMENT2R 'NUMBER-OF-ARGS 2) 
(FLAG '(CLASSEMENT2R) 'OPFN) 
(PUT 'CLASSEMENT2R 'DEFINED-ON-LINE '1540) 
(PUT 'CLASSEMENT2R 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CLASSEMENT2R 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLASSEMENT2R (X K)
    (PROG (SS ESSAI)
      (COND
       ((EVALEQUAL (AEVAL (LIST 'ORDREMULT 1)) 2)
        (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 1)))))
      (SETK 'OMEGA (AEVAL (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 2))))
      (COND
       ((FIXP (REVALX 'OMEGA))
        (PROGN
         (SETK 'NBSOLUTION (AEVAL 2))
         (COND
          ((EVALGREATERP (AEVAL (LIST 'COEFFN (LIST 'RAC 2) 'I 0))
                         (AEVAL (LIST 'COEFFN (LIST 'RAC 1) 'I 0)))
           (PROGN
            (SETQ SS (AEVAL (LIST 'RAC 1)))
            (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 2)))
            (SETK (LIST 'RAC 2) (AEVAL SS))
            (AEVAL 'NIL))))
         (AEVAL (LIST 'FROBENIUSGENERAL X K 'NBSOLUTION))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I))
             (RETURN NIL)))
           (SETK (LIST 'SOLPARM I)
                 (AEVAL*
                  (LIST 'LIST
                        (LIST 'LIST
                              (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                              'CONDL))))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL (LIST 'PARM 0)) 0)
        (PROGN
         (SETK 'NBSOLUTION (AEVAL 2))
         (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 1) 1))
         (SETK (LIST '&SOLUTION 2)
               (PROG (I FORALL-RESULT)
                 (SETQ I 0)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (AEVAL*
                                 (LIST 'TIMES
                                       (LIST 'SUB
                                             (LIST 'EQUAL 'LAMBD (LIST 'RAC 2))
                                             (LIST '&G I))
                                       (LIST 'EXPT X I)))
                                FORALL-RESULT)))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB1)))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I))
             (RETURN NIL)))
           (SETK (LIST 'SOLPARM I)
                 (AEVAL*
                  (LIST 'LIST
                        (LIST 'LIST
                              (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                              'CONDL))))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETK 'NBSOLUTION (AEVAL 2))
         (AEVAL (LIST 'CLASSEMENT2RNE X K))
         (AEVAL 'NIL)))))) 
(PUT 'CLASSEMENT2RNE 'NUMBER-OF-ARGS 2) 
(FLAG '(CLASSEMENT2RNE) 'OPFN) 
(PUT 'CLASSEMENT2RNE 'DEFINED-ON-LINE '1577) 
(PUT 'CLASSEMENT2RNE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CLASSEMENT2RNE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLASSEMENT2RNE (X K)
    (PROG (SS ESSAI)
      (SETK 'NBSOLUTION (AEVAL 2))
      (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 1) 1))
      (SETQ ESSAI
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (COND
                         ((EVALEQUAL (AEVAL* (LIST '&G I)) 0)
                          (AEVAL* (LIST 'LIST I)))
                         (T (AEVAL* (LIST 'LIST)))))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         (COND
                          ((EVALEQUAL (AEVAL* (LIST '&G I)) 0)
                           (AEVAL* (LIST 'LIST I)))
                          (T (AEVAL* (LIST 'LIST))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (GO LOOPLABEL)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'LENGTH ESSAI)) 0)
        (SETQ ESSAI (AEVAL (LIST 'CONS ", sauf :" ESSAI)))))
      (SETQ ESSAI (AEVAL (LIST 'APPEND (LIST 'LIST 'OMEGA 'NONENT) ESSAI)))
      (SETQ ESSAI (AEVAL (LIST 'CONS ESSAI 'CONDL)))
      (SETK (LIST '&SOLUTION 2)
            (PROG (I FORALL-RESULT)
              (SETQ I 0)
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             (AEVAL*
                              (LIST 'TIMES
                                    (LIST 'SUB
                                          (LIST 'EQUAL 'LAMBD (LIST 'RAC 2))
                                          (LIST '&G I))
                                    (LIST 'EXPT X I)))
                             FORALL-RESULT)))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I)) (RETURN NIL)))
        (SETK (LIST 'SOLPARM I)
              (AEVAL*
               (LIST 'LIST
                     (LIST 'LIST (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                           ESSAI))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (AEVAL* (CLEAR (LIST (LIST '&G I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'NBSOLUTION (AEVAL 2))
      (AEVAL (LIST 'FROBENIUSGENERAL X K 'NBSOLUTION))
      (SETQ ESSAI (AEVAL (LIST 'CONS (LIST 'LIST 'OMEGA 'ENTPOS) 'CONDL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I)) (RETURN NIL)))
        (SETK (LIST 'SOLPARM I)
              (AEVAL*
               (LIST 'APPEND (LIST 'SOLPARM I)
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                                 ESSAI)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (AEVAL* (CLEAR (LIST (LIST '&G I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'NBSOLUTION (AEVAL 2))
      (SETQ SS (AEVAL (LIST 'RAC 1)))
      (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 2)))
      (SETK (LIST 'RAC 2) (AEVAL SS))
      (AEVAL (LIST 'FROBENIUSGENERAL X K 'NBSOLUTION))
      (SETQ ESSAI (AEVAL (LIST 'CONS (LIST 'LIST 'OMEGA 'ENTNEG) 'CONDL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I)) (RETURN NIL)))
        (SETK (LIST 'SOLPARM I)
              (AEVAL*
               (LIST 'APPEND (LIST 'SOLPARM I)
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                                 ESSAI)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (AEVAL* (CLEAR (LIST (LIST '&G I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'NBSOLUTION (AEVAL 2))
      (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 1)))
      (AEVAL (LIST 'FROBENIUSGENERAL X K 'NBSOLUTION))
      (COND
       ((NOT
         (BOOLVALUE*
          (REVALX (LIST 'MEMBRE (LIST 'LIST 'OMEGA 'ENTNUL) 'CONDL))))
        (SETQ ESSAI (AEVAL (LIST 'CONS (LIST 'LIST 'OMEGA 'ENTNUL) 'CONDL))))
       (T (SETQ ESSAI (AEVAL 'CONDL))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'NBSOLUTION) I)) (RETURN NIL)))
        (SETK (LIST 'SOLPARM I)
              (AEVAL*
               (LIST 'APPEND (LIST 'SOLPARM I)
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                                 ESSAI)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB)))) 
(PUT 'CLASSEMENT3R 'NUMBER-OF-ARGS 2) 
(FLAG '(CLASSEMENT3R) 'OPFN) 
(PUT 'CLASSEMENT3R 'DEFINED-ON-LINE '1619) 
(PUT 'CLASSEMENT3R 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CLASSEMENT3R 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLASSEMENT3R (X K)
    (PROG (SS SY NBSOLUTION)
      (COND
       ((EVALEQUAL (AEVAL (LIST 'ORDREMULT 1)) 3)
        (PROGN
         (SETK (LIST 'RAC 2) (SETK (LIST 'RAC 3) (AEVAL (LIST 'RAC 1)))))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'ORDREMULT 1)) 1)
             (EVALEQUAL (AEVAL (LIST 'ORDREMULT 2)) 2))
        (PROGN
         (SETQ SS (AEVAL (LIST 'RAC 1)))
         (SETQ SY (AEVAL (LIST 'ORDREMULT 1)))
         (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 2)))
         (SETK (LIST 'ORDREMULT 1) (AEVAL (LIST 'ORDREMULT 2)))
         (SETK (LIST 'RAC 3) (AEVAL SS))
         (SETK (LIST 'ORDREMULT 3) (AEVAL SY))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL (LIST 'ORDREMULT 1)) 2)
        (PROGN
         (SETK (LIST 'RAC 3) (AEVAL (LIST 'RAC 2)))
         (SETK (LIST 'ORDREMULT 3) (AEVAL (LIST 'ORDREMULT 2)))
         (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 1)))
         (SETK (LIST 'ORDREMULT 2) (AEVAL (LIST 'ORDREMULT 1)))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'ORDREMULT 1)) 3)
        (PROGN
         (SETQ NBSOLUTION (AEVAL 3))
         (AEVAL (LIST 'FROBENIUSGENERAL X K NBSOLUTION))))
       (T
        (PROGN
         (COND
          ((AND (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 2))))
                (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 2) (LIST 'RAC 3)))))
           (PROGN
            (SETK '*X1 (AEVAL (LIST 'COEFFN (LIST 'RAC 1) 'I 0)))
            (SETK '*X2 (AEVAL (LIST 'COEFFN (LIST 'RAC 2) 'I 0)))
            (SETK '*X3 (AEVAL (LIST 'COEFFN (LIST 'RAC 3) 'I 0)))
            (COND
             ((EVALLESSP (AEVAL '*X1) (AEVAL '*X2))
              (PROGN
               (SETQ SS (AEVAL (LIST 'RAC 1)))
               (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 2)))
               (SETK (LIST 'RAC 2) (AEVAL SS))
               (SETQ SS (AEVAL '*X1))
               (SETK '*X1 (AEVAL '*X2))
               (SETK '*X2 (AEVAL SS))
               (AEVAL 'NIL))))
            (COND
             ((EVALLESSP (AEVAL '*X1) (AEVAL '*X3))
              (PROGN
               (SETQ SS (AEVAL (LIST 'RAC 1)))
               (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 3)))
               (SETK (LIST 'RAC 3) (AEVAL SS))
               (SETK '*X3 (AEVAL '*X1))
               (AEVAL 'NIL))))
            (COND
             ((EVALLESSP (AEVAL '*X2) (AEVAL '*X3))
              (PROGN
               (SETQ SS (AEVAL (LIST 'RAC 2)))
               (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 3)))
               (SETK (LIST 'RAC 3) (AEVAL SS))
               (AEVAL 'NIL))))
            (SETQ NBSOLUTION (AEVAL 3))
            (AEVAL (LIST 'FROBENIUSGENERAL X K NBSOLUTION))
            (AEVAL 'NIL))))
         (COND
          ((AND (EVALEQUAL (AEVAL (LIST 'RAC 1)) (AEVAL (LIST 'RAC 2)))
                (NOT
                 (FIXP
                  (REVALX (LIST 'DIFFERENCE (LIST 'RAC 2) (LIST 'RAC 3))))))
           (PROGN
            (SETQ NBSOLUTION (AEVAL 2))
            (AEVAL (LIST 'FROBENIUSGENERAL X K NBSOLUTION))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
              (AEVAL* (CLEAR (LIST (LIST '&G I))))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB))
            (SETQ NBSOLUTION (AEVAL 3))
            (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 3) 3))
            (AEVAL 'NIL))))
         (COND
          ((AND
            (NOT
             (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 2)))))
            (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 2) (LIST 'RAC 3)))))
           (PROGN
            (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 1) 3))
            (SETK (LIST 'RAC 1) (AEVAL (LIST 'RAC 2)))
            (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 3)))
            (SETQ NBSOLUTION (AEVAL 2))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
              (AEVAL* (CLEAR (LIST (LIST '&G I))))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LAB))
            (AEVAL (LIST 'FROBENIUSGENERAL X K NBSOLUTION))
            (SETQ NBSOLUTION (AEVAL 3))
            (AEVAL 'NIL))))
         (COND
          ((AND
            (NOT
             (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 2)))))
            (NOT
             (FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 2) (LIST 'RAC 3))))))
           (PROGN
            (AEVAL (LIST 'CLASS3RNE X K))
            (SETQ NBSOLUTION (AEVAL 3))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NBSOLUTION) I)) (RETURN NIL)))
        (SETK (LIST 'SOLPARM I)
              (AEVAL*
               (LIST 'LIST
                     (LIST 'LIST (LIST 'LIST (LIST '&SOLUTION I) (LIST 'RAC I))
                           'CONDL))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB)))) 
(PUT 'CLASS3RNE 'NUMBER-OF-ARGS 2) 
(FLAG '(CLASS3RNE) 'OPFN) 
(PUT 'CLASS3RNE 'DEFINED-ON-LINE '1705) 
(PUT 'CLASS3RNE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'CLASS3RNE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLASS3RNE (X K)
    (PROG (NBSOLUTION)
      (COND
       ((FIXP (REVALX (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 3))))
        (PROGN
         (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 2) 3))
         (SETK (LIST 'RAC 2) (AEVAL (LIST 'RAC 3)))
         (SETQ NBSOLUTION (AEVAL 2))
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
           (AEVAL* (CLEAR (LIST (LIST '&G I))))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (AEVAL (LIST 'FROBENIUSGENERAL X K NBSOLUTION))
         (SETQ NBSOLUTION (AEVAL 3))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ NBSOLUTION (AEVAL 3))
         (AEVAL (LIST 'FROBENIUSSIMPLE X K (LIST 'RAC 1) 1))
         (SETK (LIST '&SOLUTION 2)
               (PROG (I FORALL-RESULT)
                 (SETQ I 0)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (AEVAL*
                                 (LIST 'TIMES
                                       (LIST 'SUB
                                             (LIST 'EQUAL 'LAMBD (LIST 'RAC 2))
                                             (LIST '&G I))
                                       (LIST 'EXPT X I)))
                                FORALL-RESULT)))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB1)))
         (SETK (LIST '&SOLUTION 3)
               (PROG (I FORALL-RESULT)
                 (SETQ I 0)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (AEVAL*
                                 (LIST 'TIMES
                                       (LIST 'SUB
                                             (LIST 'EQUAL 'LAMBD (LIST 'RAC 3))
                                             (LIST '&G I))
                                       (LIST 'EXPT X I)))
                                FORALL-RESULT)))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB1)))
         (AEVAL 'NIL)))))) 
(PUT 'EQUAIND 'NUMBER-OF-ARGS 3) 
(FLAG '(EQUAIND) 'OPFN) 
(PUT 'EQUAIND 'DEFINED-ON-LINE '1731) 
(PUT 'EQUAIND 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'EQUAIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQUAIND (N X K)
    (PROG (L DENOML FF M DI MINAI LFF)
      (SETQ M 0)
      (SETQ DI 0)
      (SETQ MINAI 0)
      (SETQ LFF 0)
      (SETQ M (AEVAL (LIST 'DEG (LIST '&AA 0) X)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
        (COND
         ((EVALGREATERP (AEVAL* (LIST 'DEG (LIST '&AA I) X)) M)
          (SETQ M (AEVAL* (LIST 'DEG (LIST '&AA I) X)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST '&Y (IEVAL N)) (LIST 'DEGRAI (IEVAL N))
                     (LIST '&F (IEVAL (LIST 'PLUS K M N 1)))))
      (SETK (LIST '&Y 0)
            (AEVAL
             (LIST 'TIMES (LIST 'EXPT X 'LAMBD)
                   (PROG (I FORALL-RESULT)
                     (SETQ I 0)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND
                      ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I))
                       (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (AEVAL*
                              (LIST 'PLUS
                                    (AEVAL*
                                     (LIST 'TIMES (LIST '&G I)
                                           (LIST 'EXPT X I)))
                                    FORALL-RESULT)))
                     (SETQ I
                             ((LAMBDA (FORALL-RESULT)
                                (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                              I))
                     (GO LAB1)))))
      (PROG (II)
        (SETQ II 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) II)) (RETURN NIL)))
        (SETK (LIST '&Y II) (AEVAL* (LIST 'DF (LIST '&Y (DIFFERENCE II 1)) X)))
        (SETQ II
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 II))
        (GO LAB))
      (SETQ L (AEVAL (LIST 'TIMES (LIST '&AA 0) (LIST '&Y 0))))
      (PROG (II)
        (SETQ II 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) II)) (RETURN NIL)))
        (SETQ L
                (AEVAL*
                 (LIST 'PLUS L (LIST 'TIMES (LIST '&AA II) (LIST '&Y II)))))
        (SETQ II
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 II))
        (GO LAB))
      (COND
       ((EVALNEQ (AEVAL (LIST 'DEN L)) 1)
        (PROGN
         (SETQ DENOML (AEVAL (LIST 'DEN L)))
         (SETQ L (AEVAL (LIST 'NUM L)))
         (AEVAL 'NIL)))
       (T (SETQ DENOML (AEVAL 1))))
      (PROG (II)
        (SETQ II 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) II)) (RETURN NIL)))
        (PROGN
         (COND
          ((EVALNEQ (AEVAL* DENOML) 1)
           (SETK (LIST '&AA II) (AEVAL* (LIST 'TIMES (LIST '&AA II) DENOML)))))
         (SETK (LIST 'DEGRAI II)
               (COND
                ((OR (EVALEQUAL (AEVAL* (LIST 'DEN (LIST '&AA II))) 1)
                     (FIXP (REVALX (LIST 'DEN (LIST '&AA II)))))
                 (AEVAL*
                  (LIST 'DIFFERENCE
                        (LIST 'LENGTH (LIST 'COEFF (LIST '&AA II) X)) 1))))))
        (SETQ II
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 II))
        (GO LAB))
      (SETQ MINAI (AEVAL 0))
      (SETK 'MAXAI (AEVAL 0))
      (PROG (II)
        (SETQ II 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) II)) (RETURN NIL)))
        (PROGN
         (SETQ DI (AEVAL* (LIST 'DIFFERENCE (LIST 'DEGRAI II) II)))
         (COND ((AND (LESSP DI 0) (LESSP DI MINAI)) (SETQ MINAI (AEVAL* DI))))
         (COND ((EVALGREATERP DI (AEVAL* 'MAXAI)) (SETK 'MAXAI (AEVAL* DI))))
         (AEVAL* 'NIL))
        (SETQ II
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 II))
        (GO LAB))
      (SETQ L
              (AEVAL
               (LIST 'QUOTIENT L (LIST 'EXPT X (LIST 'PLUS 'LAMBD MINAI)))))
      (SETK 'MAXAI (AEVAL (LIST 'DIFFERENCE 'MAXAI MINAI)))
      (SETQ FF (AEVAL (LIST 'COEFF L X)))
      (WHILE (EVALEQUAL (AEVAL* (LIST 'FIRST FF)) 0)
             (SETQ FF (AEVAL* (LIST 'REST FF))))
      (SETQ LFF (AEVAL (LIST 'DIFFERENCE (LIST 'LENGTH FF) 1)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE LFF I)) (RETURN NIL)))
        (SETK (LIST '&F I) (AEVAL* (LIST 'PART FF (PLUS I 1))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETK '&DEGREC (AEVAL 'MAXAI))
      (SETK (LIST '&F 0) (AEVAL (LIST 'QUOTIENT (LIST '&F 0) (LIST '&G 0))))
      (AEVAL (CLEAR (LIST '&Y 'DEGRAI))))) 
(PUT 'FROBENIUSSIMPLE 'NUMBER-OF-ARGS 4) 
(FLAG '(FROBENIUSSIMPLE) 'OPFN) 
(PUT 'FROBENIUSSIMPLE 'DEFINED-ON-LINE '1797) 
(PUT 'FROBENIUSSIMPLE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'FROBENIUSSIMPLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FROBENIUSSIMPLE (X K RAC NBSOL)
    (PROG (FCOEFF)
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'FF (IEVAL K))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (SETK (LIST 'FF I) (AEVAL* (LIST '&F I)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK (LIST '&G 0) (AEVAL 1))
      (PROG (II)
        (SETQ II 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) II)) (RETURN NIL)))
        (PROGN
         (COND
          ((EVALNEQ (AEVAL* (LIST 'DEN (LIST 'FF II))) 1)
           (SETK (LIST 'FF II) (AEVAL* (LIST 'NUM (LIST 'FF II))))))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'FF II)) 0)
           (SETK (LIST '&G II) (AEVAL* 0)))
          (T
           (PROGN
            (SETQ FCOEFF (AEVAL* (LIST 'COEFF (LIST 'FF II) (LIST '&G II))))
            (SETK (LIST '&G II)
                  (AEVAL*
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'FIRST FCOEFF)
                               (LIST 'SECOND FCOEFF)))))
            (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ II
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 II))
        (GO LAB))
      (SETK (LIST '&SOLUTION NBSOL)
            (PROG (II FORALL-RESULT)
              (SETQ II 0)
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) II))
                (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             (AEVAL*
                              (LIST 'TIMES
                                    (LIST 'SUB (LIST 'EQUAL 'LAMBD RAC)
                                          (LIST '&G II))
                                    (LIST 'EXPT X II)))
                             FORALL-RESULT)))
              (SETQ II
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       II))
              (GO LAB1)))
      (AEVAL (CLEAR (LIST 'FF))))) 
(PUT 'FROBENIUSGENERAL 'NUMBER-OF-ARGS 3) 
(FLAG '(FROBENIUSGENERAL) 'OPFN) 
(PUT 'FROBENIUSGENERAL 'DEFINED-ON-LINE '1826) 
(PUT 'FROBENIUSGENERAL 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'FROBENIUSGENERAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FROBENIUSGENERAL (X K NBSOLUTION)
    (PROG (OMEGA FCOEFF)
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'FF (IEVAL K))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (SETK (LIST 'FF I) (AEVAL* (LIST '&F I)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL NBSOLUTION) 2)
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'RAC 1)) (AEVAL (LIST 'RAC 2)))
           (SETK (LIST '&G 0) (AEVAL 1)))
          (T
           (PROGN
            (SETQ OMEGA (AEVAL (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 2))))
            (SETK (LIST '&G 0)
                  (AEVAL
                   (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'PLUS 'LAMBD OMEGA))
                         (LIST '&F 0))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL NBSOLUTION) 3)
        (PROGN
         (SETQ OMEGA (AEVAL (LIST 'DIFFERENCE (LIST 'RAC 1) (LIST 'RAC 3))))
         (SETK (LIST '&G 0)
               (PROG (I FORALL-RESULT)
                 (SETQ I 1)
                 (SETQ FORALL-RESULT 1)
                LAB1
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* OMEGA) I))
                   (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'TIMES
                                (AEVAL*
                                 (LIST 'SUB
                                       (LIST 'EQUAL 'LAMBD
                                             (LIST 'PLUS 'LAMBD I))
                                       (LIST '&F 0)))
                                FORALL-RESULT)))
                 (SETQ I
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          I))
                 (GO LAB1)))
         (AEVAL 'NIL))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* K) I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'FF I) (AEVAL* (LIST 'NUM (LIST 'FF I))))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'FF I)) 0) (SETK (LIST '&G I) (AEVAL* 0)))
          (T
           (PROGN
            (SETQ FCOEFF (AEVAL* (LIST 'COEFF (LIST 'FF I) (LIST '&G I))))
            (SETK (LIST '&G I)
                  (AEVAL*
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'FIRST FCOEFF)
                               (LIST 'SECOND FCOEFF)))))
            (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX *TRDESIR))
        (PROGN
         (ASSGNPRI (AEVAL "Solution en l'indeterminee lambda : ") NIL 'ONLY)
         (AEVAL (FACTOR (LIST X)))
         (ASSGNPRI (AEVAL (LIST '&W 0 X 'LAMBD K)) NIL 'ONLY)
         (AEVAL (REMFAC (LIST X))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'RAC 1)) (AEVAL (LIST 'RAC 2)))
        (PROGN
         (SETK (LIST '&SOLUTION 1)
               (AEVAL
                (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 1))
                      (LIST '&W 0 X 'LAMBD K))))
         (SETK (LIST '&SOLUTION 2)
               (AEVAL
                (LIST 'PLUS
                      (LIST 'TIMES
                            (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 1))
                                  (LIST '&W 0 X 'LAMBD K))
                            (LIST 'LOG X))
                      (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 1))
                            (LIST '&W 1 X 'LAMBD K)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETK (LIST '&SOLUTION 1)
               (AEVAL
                (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 1))
                      (LIST '&W 0 X 'LAMBD K))))
         (COND
          ((EVALEQUAL (AEVAL (LIST 'PARM 0)) 0)
           (SETK (LIST '&SOLUTION 2)
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'TIMES
                              (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 2))
                                    (LIST '&W 0 X 'LAMBD K))
                              (LIST 'LOG X))
                        (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 2))
                              (LIST '&W 1 X 'LAMBD K))))))
          (T
           (SETK (LIST '&SOLUTION 2)
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'TIMES (LIST '&W 0 X 'LAMBD K) (LIST 'LOG X))
                        (LIST '&W 1 X 'LAMBD K))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL NBSOLUTION) 3)
        (SETK (LIST '&SOLUTION 3)
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES
                           (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 3))
                                 (LIST '&W 0 X 'LAMBD K))
                           (LIST 'EXPT (LIST 'LOG X) 2))
                     (LIST 'TIMES 2
                           (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 3))
                                 (LIST '&W 1 X 'LAMBD K))
                           (LIST 'LOG X))
                     (LIST 'SUB (LIST 'EQUAL 'LAMBD (LIST 'RAC 3))
                           (LIST '&W 2 X 'LAMBD K)))))))
      (AEVAL (CLEAR (LIST 'FF))))) 
(PUT 'RACINE 'NUMBER-OF-ARGS 2) 
(FLAG '(RACINE) 'OPFN) 
(PUT 'RACINE 'DEFINED-ON-LINE '1915) 
(PUT 'RACINE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'RACINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RACINE (F X)
    (PROG (NBRAC SOL MULTSOL)
      (SETQ NBRAC 0)
      (SETQ NBRAC (AEVAL 0))
      (SETQ SOL (AEVAL (LIST 'SOLVE F X)))
      (SETQ MULTSOL (AEVAL MULTIPLICITIES*))
      (PROG (ELT)
        (SETQ ELT (GETRLIST (AEVAL SOL)))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((EVALEQUAL (AEVAL (LIST 'LHS ELT)) (AEVAL X))
             (PROGN
              (SETQ NBRAC (AEVAL (PLUS NBRAC 1)))
              (SETK (LIST 'ORDREMULT NBRAC) (AEVAL (LIST 'FIRST MULTSOL)))
              (SETQ MULTSOL (AEVAL (LIST 'REST MULTSOL)))
              (SETK (LIST 'RAC NBRAC) (AEVAL (LIST 'RHS ELT)))
              (AEVAL 'NIL)))
            (T (SETQ MULTSOL (AEVAL (LIST 'REST MULTSOL))))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN NBRAC))) 
(PUT 'MEMBRE 'NUMBER-OF-ARGS 2) 
(FLAG '(MEMBRE) 'OPFN) 
(PUT 'MEMBRE 'DEFINED-ON-LINE '1943) 
(PUT 'MEMBRE 'DEFINED-IN-FILE 'SOLVE/DESIR.RED) 
(PUT 'MEMBRE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MEMBRE (ELT LIST)
    (PROG (BOOL)
      (PROG (W)
        (SETQ W (GETRLIST (AEVAL LIST)))
       LAB
        (COND ((NULL W) (RETURN NIL)))
        ((LAMBDA (W)
           (COND ((EVALEQUAL (AEVAL W) (AEVAL ELT)) (SETQ BOOL (AEVAL 'T)))))
         (CAR W))
        (SETQ W (CDR W))
        (GO LAB))
      (RETURN (AEVAL BOOL)))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(COND
 (*TRDESIR
  (PROGN
   (TERPRI)
   (TERPRI)
   (PRINC " DESIR : solutions formelles d'equations differentielles")
   (TERPRI)
   (PRINC "          lineaires homogenes au voisinage de zero, point ")
   (TERPRI)
   (PRINC "          singulier regulier ou irregulier, ou point regulier")
   (TERPRI)
   (TERPRI)
   (PRINC "                Version  3.3  -  Novembre 1993 ")
   (TERPRI)
   (PRINC "                   Appel par desir();        ")
   (TERPRI)
   (TERPRI)
   NIL))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(ENDMODULE) 
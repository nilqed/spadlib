(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DCFSFKACEM)) 
(REVISION 'DCFSFKACEM
          "$Id: dcfsfkacem.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'DCFSFKACEM "(c) 2004-2009 A. Dolzmann, T. Sturm, 2017 T. Sturm") 
(FLUID '(DQE_COUNTER* *DQEVERBOSE *DQEGRADORD *DQEOPTQELIM *DQEOPTSIMP)) 
(SWITCH (LIST 'DQEVERBOSE)) 
(SWITCH (LIST 'DQEGRADORD)) 
(SWITCH (LIST 'DQEOPTQELIM)) 
(SWITCH (LIST 'DQEOPTSIMP)) 
(ON1 'DQEVERBOSE) 
(ON1 'DQEGRADORD) 
(ON1 'DQEOPTQELIM) 
(ON1 'DQEOPTSIMP) 
(AEVAL (FORALL (LIST '(X N) 'T '(LET00 '((EQUAL (DF (D X N) X) 0)))))) 
(PUT 'DQE_ISCONSTANT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ISCONSTANT 'DEFINED-ON-LINE '50) 
(PUT 'DQE_ISCONSTANT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ISCONSTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ISCONSTANT (PHI)
    (OR (NUMBERP PHI)
        (AND (PAIRP PHI) (EQ (CAR PHI) 'QUOTIENT) (NUMBERP (CADDR PHI))
             (NUMBERP (REVAL1 (CADR PHI) T))))) 
(PUT 'DQE_ISATOMARP 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ISATOMARP 'DEFINED-ON-LINE '71) 
(PUT 'DQE_ISATOMARP 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ISATOMARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ISATOMARP (PHI)
    (AND (PAIRP PHI) (OR (EQ (CAR PHI) 'NEQ) (EQ (CAR PHI) 'EQUAL)))) 
(PUT 'DQE_ISQUANTFREE 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ISQUANTFREE 'DEFINED-ON-LINE '87) 
(PUT 'DQE_ISQUANTFREE 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ISQUANTFREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ISQUANTFREE (PHI)
    (PROG (ERG)
      (COND ((OR (ATOM PHI) (NOT PHI) (DQE_ISATOMARP PHI)) (RETURN T)))
      (COND ((EQUAL (CAR PHI) 'NOTT) (RETURN (DQE_ISQUANTFREE (CADR PHI)))))
      (COND
       ((OR (EQ (CAR PHI) 'OR) (EQ (CAR PHI) 'AND))
        (PROGN
         (SETQ PHI (CDR PHI))
         (SETQ ERG T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND ERG PHI)) (RETURN NIL)))
           (PROGN (SETQ ERG (DQE_ISQUANTFREE (CAR PHI))) (SETQ PHI (CDR PHI)))
           (GO WHILELABEL))
         (RETURN ERG)
         NIL)))
      (RETURN NIL))) 
(PUT 'DQE_ISPRENEXP 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ISPRENEXP 'DEFINED-ON-LINE '121) 
(PUT 'DQE_ISPRENEXP 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ISPRENEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ISPRENEXP (PHI)
    (PROG (ERG)
      (COND ((OR (ATOM PHI) (NOT PHI)) (SETQ ERG T))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (OR (EQUAL (CAR PHI) 'EX) (EQUAL (CAR PHI) 'ALL)))
                  (RETURN NIL)))
                (SETQ PHI (CADDR PHI))
                (GO WHILELABEL))
              (SETQ ERG (DQE_ISQUANTFREE PHI)))))
      (RETURN ERG))) 
(PUT 'DQE_MODATOMAR 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_MODATOMAR 'DEFINED-ON-LINE '145) 
(PUT 'DQE_MODATOMAR 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MODATOMAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_MODATOMAR (PHI)
    (COND ((EQUAL (CADDR PHI) 0) PHI)
          (T
           (LIST (CAR PHI) (REVAL1 (LIST 'DIFFERENCE (CADR PHI) (CADDR PHI)) T)
                 0)))) 
(PUT 'DQE_HELPELIM 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_HELPELIM 'DEFINED-ON-LINE '172) 
(PUT 'DQE_HELPELIM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPELIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_HELPELIM (PHI)
    (PROG (OP)
      (COND ((OR (EQ PHI T) (NOT PHI)) (RETURN PHI)))
      (SETQ OP (CAR PHI))
      (COND
       ((EQ OP 'NEQ) (RETURN (LIST (REVAL1 (CADR (DQE_MODATOMAR PHI)) T)))))
      (COND
       ((EQ OP 'EQUAL)
        (RETURN (LIST 1 (REVAL1 (CADR (DQE_MODATOMAR PHI)) T)))))
      (COND ((EQ OP 'AND) (RETURN (DQE_HELPELIM-AND (CDR PHI)))))
      (REDERR "dqe_helpelim: internal error"))) 
(PUT 'DQE_HELPELIM-AND 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_HELPELIM-AND 'DEFINED-ON-LINE '186) 
(PUT 'DQE_HELPELIM-AND 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPELIM-AND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_HELPELIM-AND (PHI)
    (PROG (A EQS G)
      (SETQ G 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT PHI) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR PHI))
         (COND
          ((EQ (CAR A) 'EQUAL)
           (SETQ EQS (LTO_INSERT (REVAL1 (CADR (DQE_MODATOMAR A)) T) EQS)))
          (T
           (SETQ G
                   (REVAL1 (LIST 'TIMES G (REVAL1 (CADR (DQE_MODATOMAR A)) T))
                           T))))
         (SETQ PHI (CDR PHI)))
        (GO WHILELABEL))
      (RETURN (CONS G (REVERSIP EQS))))) 
(PUT 'DQE_ANDORVALEUR 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ANDORVALEUR 'DEFINED-ON-LINE '228) 
(PUT 'DQE_ANDORVALEUR 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ANDORVALEUR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ANDORVALEUR (PHI)
    (PROG (ERG HILF HILFF ANDOR)
      (SETQ ERG NIL)
      (SETQ ANDOR (CAR PHI))
      (SETQ HILF (CADR PHI))
      (SETQ HILFF (CADDR PHI))
      (COND
       (HILF
        (PROGN
         (COND
          (HILFF
           (PROGN
            (COND
             ((AND (EQUAL (CAR HILF) ANDOR) (EQUAL (CAR HILFF) ANDOR))
              (PROGN
               (SETQ HILF (REVERSE (CDR HILF)))
               (SETQ HILFF (CDR HILFF))
               (PROG ()
                WHILELABEL
                 (COND ((NOT HILF) (RETURN NIL)))
                 (PROGN
                  (SETQ HILFF (DQE_CONSM (CAR HILF) HILFF))
                  (SETQ HILF (CDR HILF)))
                 (GO WHILELABEL))
               (COND ((NOT (CDR HILFF)) (SETQ ERG (CAR HILFF)))
                     (T (SETQ ERG (CONS ANDOR HILFF))))))
             ((EQUAL (CAR HILF) ANDOR) (SETQ ERG (DQE_MODCONS HILFF HILF)))
             ((EQUAL (CAR HILFF) ANDOR)
              (SETQ ERG (CONS ANDOR (DQE_CONSM HILF (CDR HILFF)))))
             (T (SETQ ERG PHI)))))
          (T (SETQ ERG HILF)))))
       (T (SETQ ERG HILFF)))
      (RETURN ERG))) 
(PUT 'DQE_CONSM 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_CONSM 'DEFINED-ON-LINE '273) 
(PUT 'DQE_CONSM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_CONSM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_CONSM (ELEM LISTE)
    (COND ((MEMBER ELEM LISTE) LISTE) (T (CONS ELEM LISTE)))) 
(PUT 'DQE_MODCONS 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_MODCONS 'DEFINED-ON-LINE '292) 
(PUT 'DQE_MODCONS 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MODCONS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_MODCONS (ELEM LISTE)
    (COND ((MEMBER ELEM LISTE) LISTE)
          (T (REVERSE (CONS ELEM (REVERSE LISTE)))))) 
(PUT 'DQE_MAKEPOSITIVEAT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_MAKEPOSITIVEAT 'DEFINED-ON-LINE '309) 
(PUT 'DQE_MAKEPOSITIVEAT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MAKEPOSITIVEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_MAKEPOSITIVEAT (PHI)
    (PROG (PSI)
      (SETQ PSI (CADR PHI))
      (RETURN
       (COND ((EQ (CAR PSI) 'EQUAL) (LIST 'NEQ (CADR PSI) (CADDR PSI)))
             (T (LIST 'EQUAL (CADR PSI) (CADDR PSI))))))) 
(PUT 'DQE_MAKEPOSITIVE 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_MAKEPOSITIVE 'DEFINED-ON-LINE '335) 
(PUT 'DQE_MAKEPOSITIVE 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MAKEPOSITIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_MAKEPOSITIVE (FORMEL)
    (PROG (ERG HILFSERG HILF)
      (COND ((OR (EQUAL FORMEL T) (NOT FORMEL)) (SETQ ERG FORMEL))
            ((EQUAL (CAR FORMEL) 'NOTT)
             (PROGN
              (SETQ FORMEL (CADR FORMEL))
              (COND ((EQUAL FORMEL T) (SETQ ERG NIL))
                    ((EQUAL FORMEL NIL) (SETQ ERG T))
                    ((EQUAL (CAR FORMEL) 'NOTT)
                     (SETQ ERG (DQE_MAKEPOSITIVE (CADR FORMEL))))
                    ((EQUAL (CAR FORMEL) 'EX)
                     (PROGN
                      (SETQ ERG (DQE_MAKEPOSITIVE (LIST 'NOTT (CADDR FORMEL))))
                      (SETQ ERG (LIST 'ALL (CADR FORMEL) ERG))))
                    ((EQUAL (CAR FORMEL) 'ALL)
                     (PROGN
                      (SETQ ERG (DQE_MAKEPOSITIVE (LIST 'NOTT (CADDR FORMEL))))
                      (SETQ ERG (LIST 'EX (CADR FORMEL) ERG))))
                    ((EQUAL (CAR FORMEL) 'AND)
                     (PROGN
                      (SETQ HILF (CDR FORMEL))
                      (SETQ HILFSERG NIL)
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT HILF) (RETURN NIL)))
                        (PROGN
                         (SETQ HILFSERG
                                 (DQE_MAKEPOSITIVE (LIST 'NOTT (CAR HILF))))
                         (SETQ ERG (CONS HILFSERG ERG))
                         (SETQ HILF (CDR HILF)))
                        (GO WHILELABEL))
                      (COND ((CDR ERG) (SETQ ERG (CONS 'OR (REVERSE ERG)))))))
                    ((EQUAL (CAR FORMEL) 'OR)
                     (PROGN
                      (SETQ HILF (CDR FORMEL))
                      (SETQ HILFSERG NIL)
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT HILF) (RETURN NIL)))
                        (PROGN
                         (SETQ HILFSERG
                                 (DQE_MAKEPOSITIVE (LIST 'NOTT (CAR HILF))))
                         (SETQ ERG (CONS HILFSERG ERG))
                         (SETQ HILF (CDR HILF)))
                        (GO WHILELABEL))
                      (COND ((CDR ERG) (SETQ ERG (CONS 'AND (REVERSE ERG)))))))
                    (T (SETQ ERG (DQE_MAKEPOSITIVEAT (LIST 'NOTT FORMEL)))))))
            (T
             (PROGN
              (COND
               ((EQUAL (CAR FORMEL) 'EX)
                (PROGN
                 (SETQ ERG (DQE_MAKEPOSITIVE (CADDR FORMEL)))
                 (SETQ ERG (LIST 'EX (CADR FORMEL) ERG))))
               ((EQUAL (CAR FORMEL) 'ALL)
                (PROGN
                 (SETQ ERG (DQE_MAKEPOSITIVE (CADDR FORMEL)))
                 (SETQ ERG (LIST 'ALL (CADR FORMEL) ERG))))
               ((EQUAL (CAR FORMEL) 'AND)
                (PROGN
                 (SETQ HILF (CDR FORMEL))
                 (SETQ HILFSERG NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT HILF) (RETURN NIL)))
                   (PROGN
                    (SETQ HILFSERG (DQE_MAKEPOSITIVE (CAR HILF)))
                    (SETQ ERG (CONS HILFSERG ERG))
                    (SETQ HILF (CDR HILF)))
                   (GO WHILELABEL))
                 (COND ((CDR ERG) (SETQ ERG (CONS 'AND (REVERSE ERG)))))))
               ((EQUAL (CAR FORMEL) 'OR)
                (PROGN
                 (SETQ HILF (CDR FORMEL))
                 (SETQ HILFSERG NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT HILF) (RETURN NIL)))
                   (PROGN
                    (SETQ HILFSERG (DQE_MAKEPOSITIVE (CAR HILF)))
                    (SETQ ERG (CONS HILFSERG ERG))
                    (SETQ HILF (CDR HILF)))
                   (GO WHILELABEL))
                 (COND ((CDR ERG) (SETQ ERG (CONS 'OR (REVERSE ERG)))))))
               (T (SETQ ERG FORMEL))))))
      (RETURN ERG))) 
(PUT 'DQE_INTERCHANGE7 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_INTERCHANGE7 'DEFINED-ON-LINE '434) 
(PUT 'DQE_INTERCHANGE7 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_INTERCHANGE7 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_INTERCHANGE7 (L LS A)
    (PROG (QLIST HILF PHI QB QB1 WEITER)
      (SETQ QLIST NIL)
      (SETQ WEITER T)
      (SETQ HILF NIL)
      (SETQ QB 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN (SETQ HILF (CONS (CAAR L) HILF)) (SETQ L (CDR L)))
        (GO WHILELABEL))
      (SETQ L HILF)
      (PROG ()
       WHILELABEL
        (COND ((NOT WEITER) (RETURN NIL)))
        (PROGN
         (SETQ WEITER NIL)
         (SETQ HILF NIL)
         (SETQ QB1 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ PHI (CAR L))
            (SETQ L (CDR L))
            (PROG ()
             WHILELABEL
              (COND ((NOT (EQUAL (CAR PHI) A)) (RETURN NIL)))
              (PROGN
               (SETQ QLIST (CONS (LIST (CAR PHI) (CADR PHI)) QLIST))
               (SETQ PHI (CADDR PHI))
               (SETQ QB1 (PLUS QB1 1)))
              (GO WHILELABEL))
            (SETQ HILF (CONS PHI HILF)))
           (GO WHILELABEL))
         (SETQ L HILF)
         (COND ((GREATERP QB1 0) (SETQ QB (PLUS QB 1))))
         (COND ((EQUAL A 'EX) (SETQ A 'ALL)) (T (SETQ A 'EX)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND HILF (NOT WEITER))) (RETURN NIL)))
           (PROGN
            (COND
             ((OR (EQUAL (CAAR HILF) 'EX) (EQUAL (CAAR HILF) 'ALL))
              (SETQ WEITER T)))
            (SETQ HILF (CDR HILF)))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (SETQ PHI (CONS LS L))
      (PROG ()
       WHILELABEL
        (COND ((NOT QLIST) (RETURN NIL)))
        (PROGN
         (SETQ PHI (APPEND (CAR QLIST) (LIST PHI)))
         (SETQ QLIST (CDR QLIST)))
        (GO WHILELABEL))
      (RETURN (LIST PHI QB)))) 
(PUT 'DQE_PNFQUANTOR 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_PNFQUANTOR 'DEFINED-ON-LINE '476) 
(PUT 'DQE_PNFQUANTOR 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNFQUANTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_PNFQUANTOR (PHI)
    (PROG (ERG N M HILF HILF1 Z DEC)
      (SETQ DEC (CAR PHI))
      (SETQ DQE_COUNTER* (PLUS DQE_COUNTER* 1))
      (SETQ Z (MKID 'NEWID DQE_COUNTER*))
      (SETQ ERG (DQE_PNF (SUBST Z (CADR PHI) (CADDR PHI))))
      (COND
       ((CDR ERG)
        (PROGN
         (SETQ N (CADR (CAR ERG)))
         (SETQ M (CADR (CADR ERG)))
         (COND
          ((LESSP N M)
           (PROGN
            (SETQ HILF (CAAR ERG))
            (SETQ HILF1 (LIST DEC Z HILF))
            (COND ((EQUAL (CAR HILF) DEC) (SETQ HILF1 (LIST HILF1 N)))
                  (T (SETQ HILF1 (LIST HILF1 (PLUS N 1)))))
            (SETQ ERG (LIST HILF1))))
          ((GREATERP N M)
           (PROGN
            (SETQ HILF (CAADR ERG))
            (SETQ HILF1 (LIST DEC Z HILF))
            (COND ((EQUAL (CAR HILF) DEC) (SETQ HILF1 (LIST HILF M)))
                  (T (SETQ HILF1 (LIST HILF (PLUS M 1)))))
            (SETQ ERG (LIST HILF1))))
          (T
           (PROGN
            (SETQ HILF ERG)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND HILF (NEQ (CAAAR HILF) DEC))) (RETURN NIL)))
              (SETQ HILF (CDR HILF))
              (GO WHILELABEL))
            (COND
             (HILF
              (PROGN
               (SETQ HILF (LIST (LIST DEC Z (CAAR HILF)) N))
               (SETQ ERG (LIST HILF))))
             (T
              (PROGN
               (SETQ ERG (LIST (LIST DEC Z (CAAR ERG)) (PLUS N 1)))
               (SETQ ERG (LIST ERG))))))))))
       (T
        (PROGN
         (COND ((NEQ (CAAAR ERG) DEC) (SETQ M (PLUS (CADAR ERG) 1)))
               (T (SETQ M (CADAR ERG))))
         (SETQ ERG (LIST (LIST (LIST DEC Z (CAAR ERG)) M))))))
      (RETURN ERG))) 
(PUT 'DQE_PNFJUNKTOR 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_PNFJUNKTOR 'DEFINED-ON-LINE '524) 
(PUT 'DQE_PNFJUNKTOR 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNFJUNKTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_PNFJUNKTOR (PHI)
    (PROG (ERG DEC HILF HILF1 HILF2 PSI PAIR1 PAIR2 POSS1 POSS2 L1 L2 M M1)
      (SETQ DEC (CAR PHI))
      (SETQ M (MINUS 1))
      (SETQ POSS1 T)
      (SETQ POSS2 T)
      (SETQ HILF1 NIL)
      (SETQ HILF2 NIL)
      (SETQ HILF (CDR PHI))
      (SETQ L1 NIL)
      (SETQ L2 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT HILF) (RETURN NIL)))
        (PROGN
         (SETQ PSI (DQE_PNF (CAR HILF)))
         (SETQ HILF (CDR HILF))
         (SETQ HILF1 (CONS (CAR PSI) HILF1))
         (COND ((CDR PSI) (SETQ HILF2 (CONS (CADR PSI) HILF2)))
               (T (SETQ HILF2 (CONS (CAR PSI) HILF2))))
         (SETQ M1 (CADAR PSI))
         (COND ((GREATERP M1 M) (SETQ M M1))))
        (GO WHILELABEL))
      (COND
       ((GREATERP M 0)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT HILF1) (RETURN NIL)))
           (PROGN
            (SETQ PAIR1 (CAR HILF1))
            (SETQ PAIR2 (CAR HILF2))
            (SETQ HILF1 (CDR HILF1))
            (SETQ HILF2 (CDR HILF2))
            (SETQ L1 (CONS PAIR1 L1))
            (SETQ L2 (CONS PAIR2 L2))
            (COND
             ((AND (EQUAL (CADR PAIR1) M) (NEQ (CAAR PAIR1) 'EX))
              (SETQ POSS1 NIL)))
            (COND
             ((AND (EQUAL (CADR PAIR2) M) (NEQ (CAAR PAIR2) 'ALL))
              (SETQ POSS2 NIL))))
           (GO WHILELABEL))
         (COND
          ((AND POSS1 (NOT POSS2))
           (SETQ ERG (LIST (DQE_INTERCHANGE7 L1 DEC 'EX))))
          ((AND POSS2 (NOT POSS1))
           (SETQ ERG (LIST (DQE_INTERCHANGE7 L2 DEC 'ALL))))
          (T
           (SETQ ERG
                   (LIST (DQE_INTERCHANGE7 L1 DEC 'EX)
                         (DQE_INTERCHANGE7 L2 DEC 'ALL)))))))
       (T (SETQ ERG (LIST (LIST PHI 0)))))
      (RETURN ERG))) 
(PUT 'DQE_PNF 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_PNF 'DEFINED-ON-LINE '589) 
(PUT 'DQE_PNF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_PNF (PHI)
    (PROG (DEC ERG)
      (SETQ DEC (CAR PHI))
      (COND
       ((OR (EQUAL DEC 'EX) (EQUAL DEC 'ALL)) (SETQ ERG (DQE_PNFQUANTOR PHI)))
       ((OR (EQUAL DEC 'OR) (EQUAL DEC 'AND)) (SETQ ERG (DQE_PNFJUNKTOR PHI)))
       (T (SETQ ERG (LIST (LIST PHI 0)))))
      (RETURN ERG))) 
(PUT 'DQE_MAKEPRENEX 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_MAKEPRENEX 'DEFINED-ON-LINE '611) 
(PUT 'DQE_MAKEPRENEX 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MAKEPRENEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_MAKEPRENEX (PHI)
    (PROG (ERG)
      (SETQ DQE_COUNTER* 0)
      (SETQ ERG (DQE_PNF PHI))
      (COND
       ((CDR ERG)
        (PROGN
         (COND ((LEQ (CADR (CAR ERG)) (CADR (CADR ERG))) (SETQ ERG (CAAR ERG)))
               (T (SETQ ERG (CAADR ERG))))))
       (T (SETQ ERG (CAAR ERG))))
      (RETURN ERG))) 
(PUT 'DQE_PNFQUANTORMOD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_PNFQUANTORMOD 'DEFINED-ON-LINE '632) 
(PUT 'DQE_PNFQUANTORMOD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNFQUANTORMOD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_PNFQUANTORMOD (PHI LISTE)
    (PROG (ERG N M HILF HILF1 Z DEC)
      (SETQ DEC (CAR PHI))
      (SETQ DQE_COUNTER* (PLUS DQE_COUNTER* 1))
      (SETQ Z (MKID 'NEWID DQE_COUNTER*))
      (SETQ LISTE (CONS (CADR PHI) (CONS Z LISTE)))
      (SETQ ERG (DQE_PNFMOD (SUBST Z (CADR PHI) (CADDR PHI)) LISTE))
      (SETQ LISTE (CADR ERG))
      (SETQ ERG (CAR ERG))
      (COND
       ((CDR ERG)
        (PROGN
         (SETQ N (CADR (CAR ERG)))
         (SETQ M (CADR (CADR ERG)))
         (COND
          ((LESSP N M)
           (PROGN
            (SETQ HILF (CAAR ERG))
            (SETQ HILF1 (LIST DEC Z HILF))
            (COND ((EQUAL (CAR HILF) DEC) (SETQ HILF1 (LIST HILF1 N)))
                  (T (SETQ HILF1 (LIST HILF1 (PLUS N 1)))))
            (SETQ ERG (LIST HILF1))))
          ((GREATERP N M)
           (PROGN
            (SETQ HILF (CAADR ERG))
            (SETQ HILF1 (LIST DEC Z HILF))
            (COND ((EQUAL (CAR HILF) DEC) (SETQ HILF1 (LIST HILF M)))
                  (T (SETQ HILF1 (LIST HILF (PLUS M 1)))))
            (SETQ ERG (LIST HILF1))))
          (T
           (PROGN
            (SETQ HILF ERG)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND HILF (NEQ (CAAAR HILF) DEC))) (RETURN NIL)))
              (SETQ HILF (CDR HILF))
              (GO WHILELABEL))
            (COND
             (HILF
              (PROGN
               (SETQ HILF (LIST (LIST DEC Z (CAAR HILF)) N))
               (SETQ ERG (LIST HILF))))
             (T
              (PROGN
               (SETQ ERG (LIST (LIST DEC Z (CAAR ERG)) (PLUS N 1)))
               (SETQ ERG (LIST ERG))))))))))
       (T
        (PROGN
         (COND ((NEQ (CAAAR ERG) DEC) (SETQ M (PLUS (CADAR ERG) 1)))
               (T (SETQ M (CADAR ERG))))
         (SETQ ERG (LIST (LIST (LIST DEC Z (CAAR ERG)) M))))))
      (RETURN (LIST ERG LISTE)))) 
(PUT 'DQE_PNFJUNKTORMOD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_PNFJUNKTORMOD 'DEFINED-ON-LINE '676) 
(PUT 'DQE_PNFJUNKTORMOD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNFJUNKTORMOD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_PNFJUNKTORMOD (PHI LISTE)
    (PROG (ERG DEC HILF HILF1 HILF2 PSI PAIR1 PAIR2 POSS1 POSS2 L1 L2 M M1)
      (SETQ DEC (CAR PHI))
      (SETQ M (MINUS 1))
      (SETQ POSS1 T)
      (SETQ POSS2 T)
      (SETQ HILF1 NIL)
      (SETQ HILF2 NIL)
      (SETQ HILF (CDR PHI))
      (SETQ L1 NIL)
      (SETQ L2 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT HILF) (RETURN NIL)))
        (PROGN
         (SETQ PSI (DQE_PNFMOD (CAR HILF) LISTE))
         (SETQ LISTE (CADR PSI))
         (SETQ PSI (CAR PSI))
         (SETQ HILF (CDR HILF))
         (SETQ HILF1 (CONS (CAR PSI) HILF1))
         (COND ((CDR PSI) (SETQ HILF2 (CONS (CADR PSI) HILF2)))
               (T (SETQ HILF2 (CONS (CAR PSI) HILF2))))
         (SETQ M1 (CADAR PSI))
         (COND ((GREATERP M1 M) (SETQ M M1))))
        (GO WHILELABEL))
      (COND
       ((GREATERP M 0)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT HILF1) (RETURN NIL)))
           (PROGN
            (SETQ PAIR1 (CAR HILF1))
            (SETQ PAIR2 (CAR HILF2))
            (SETQ HILF1 (CDR HILF1))
            (SETQ HILF2 (CDR HILF2))
            (SETQ L1 (CONS PAIR1 L1))
            (SETQ L2 (CONS PAIR2 L2))
            (COND
             ((AND (EQUAL (CADR PAIR1) M) (NEQ (CAAR PAIR1) 'EX))
              (SETQ POSS1 NIL)))
            (COND
             ((AND (EQUAL (CADR PAIR2) M) (NEQ (CAAR PAIR2) 'ALL))
              (SETQ POSS2 NIL))))
           (GO WHILELABEL))
         (COND
          ((AND POSS1 (NOT POSS2))
           (SETQ ERG (LIST (LIST (DQE_INTERCHANGE7 L1 DEC 'EX)) LISTE)))
          ((AND POSS2 (NOT POSS1))
           (SETQ ERG (LIST (LIST (DQE_INTERCHANGE7 L2 DEC 'ALL)) LISTE)))
          (T
           (SETQ ERG
                   (LIST
                    (LIST (DQE_INTERCHANGE7 L1 DEC 'EX)
                          (DQE_INTERCHANGE7 L2 DEC 'ALL))
                    LISTE))))))
       (T (SETQ ERG (LIST (LIST (LIST PHI 0)) LISTE))))
      (RETURN ERG))) 
(PUT 'DQE_PNFMOD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_PNFMOD 'DEFINED-ON-LINE '714) 
(PUT 'DQE_PNFMOD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PNFMOD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_PNFMOD (PHI LISTE)
    (PROG (DEC ERG)
      (SETQ DEC (CAR PHI))
      (COND
       ((OR (EQUAL DEC 'EX) (EQUAL DEC 'ALL))
        (SETQ ERG (DQE_PNFQUANTORMOD PHI LISTE)))
       ((OR (EQUAL DEC 'OR) (EQUAL DEC 'AND))
        (SETQ ERG (DQE_PNFJUNKTORMOD PHI LISTE)))
       (T (SETQ ERG (LIST (LIST (LIST PHI 0)) LISTE))))
      (RETURN ERG))) 
(PUT 'DQE_MAKEPRENEXMOD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_MAKEPRENEXMOD 'DEFINED-ON-LINE '736) 
(PUT 'DQE_MAKEPRENEXMOD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_MAKEPRENEXMOD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_MAKEPRENEXMOD (PHI DIFFEQUALISTE)
    (PROG (ERG HILFLISTE LISTE AUSG VAR NEWVAR HILF)
      (SETQ AUSG NIL)
      (SETQ DQE_COUNTER* 0)
      (SETQ LISTE NIL)
      (SETQ HILFLISTE DIFFEQUALISTE)
      (SETQ ERG (DQE_PNFMOD PHI LISTE))
      (SETQ LISTE (CADR ERG))
      (SETQ ERG (CAR ERG))
      (COND
       ((CDR ERG)
        (PROGN
         (COND ((LEQ (CADR (CAR ERG)) (CADR (CADR ERG))) (SETQ ERG (CAAR ERG)))
               (T (SETQ ERG (CAADR ERG))))))
       (T (SETQ ERG (CAAR ERG))))
      (PROG ()
       WHILELABEL
        (COND ((NOT LISTE) (RETURN NIL)))
        (PROGN
         (SETQ VAR (CAR LISTE))
         (SETQ NEWVAR (CADR LISTE))
         (SETQ LISTE (CDDR LISTE))
         (SETQ HILFLISTE (SUBST NEWVAR VAR HILFLISTE)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT HILFLISTE) (RETURN NIL)))
        (PROGN
         (SETQ VAR (CAR HILFLISTE))
         (SETQ HILF (CADR HILFLISTE))
         (SETQ HILFLISTE (CDDR HILFLISTE))
         (COND
          ((NOT (MEMBER VAR DIFFEQUALISTE))
           (SETQ DIFFEQUALISTE (CONS VAR (CONS HILF DIFFEQUALISTE))))))
        (GO WHILELABEL))
      (SETQ AUSG (LIST ERG DIFFEQUALISTE))
      (RETURN AUSG))) 
(PUT 'DQE_DISJNF 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_DISJNF 'DEFINED-ON-LINE '782) 
(PUT 'DQE_DISJNF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_DISJNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_DISJNF (FORMEL)
    (PROG (ERG HILF)
      (SETQ ERG NIL)
      (COND
       ((OR (EQUAL FORMEL T) (NOT FORMEL) (DQE_ISATOMARP FORMEL))
        (SETQ ERG FORMEL))
       ((EQUAL (CAR FORMEL) 'AND) (SETQ ERG (DQE_DISTRIBUTIV FORMEL)))
       ((EQUAL (CAR FORMEL) 'OR)
        (PROGN
         (SETQ FORMEL (CDR FORMEL))
         (PROG ()
          WHILELABEL
           (COND ((NOT FORMEL) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR FORMEL))
            (SETQ FORMEL (CDR FORMEL))
            (SETQ HILF (DQE_DISJNF HILF))
            (COND
             ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF)
                  (EQUAL (CAR HILF) 'AND))
              (PROGN
               (COND ((NOT ERG) (SETQ ERG (LIST HILF)))
                     ((NOT (CDR ERG))
                      (PROGN
                       (COND
                        ((NOT (EQUAL HILF (CAR ERG)))
                         (SETQ ERG (LIST 'OR (CAR ERG) HILF))))))
                     (T (SETQ ERG (DQE_MODCONS HILF ERG))))))
             (T
              (PROGN
               (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG))))
               (SETQ ERG (DQE_ANDORVALEUR (LIST 'OR ERG HILF)))))))
           (GO WHILELABEL))
         (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG))))))
       (T (SETQ ERG FORMEL)))
      (COND (*DQEOPTSIMP (SETQ ERG (DQE_DKNFSIMPLIFY ERG))))
      (RETURN ERG))) 
(PUT 'DQE_KONJNF 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_KONJNF 'DEFINED-ON-LINE '840) 
(PUT 'DQE_KONJNF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_KONJNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_KONJNF (FORMEL)
    (PROG (ERG HILF)
      (SETQ ERG NIL)
      (COND
       ((OR (EQUAL FORMEL T) (NOT FORMEL) (DQE_ISATOMARP FORMEL))
        (SETQ ERG FORMEL))
       ((EQUAL (CAR FORMEL) 'OR) (SETQ ERG (DQE_DISTRIBUTIV FORMEL)))
       ((EQUAL (CAR FORMEL) 'AND)
        (PROGN
         (SETQ FORMEL (CDR FORMEL))
         (PROG ()
          WHILELABEL
           (COND ((NOT FORMEL) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR FORMEL))
            (SETQ FORMEL (CDR FORMEL))
            (SETQ HILF (DQE_KONJNF HILF))
            (COND
             ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF)
                  (EQUAL (CAR HILF) 'OR))
              (PROGN
               (COND ((NOT ERG) (SETQ ERG (LIST HILF)))
                     ((NOT (CDR ERG))
                      (PROGN
                       (COND
                        ((NOT (EQUAL HILF (CAR ERG)))
                         (SETQ ERG (LIST 'AND (CAR ERG) HILF))))))
                     (T (SETQ ERG (DQE_MODCONS HILF ERG))))))
             (T
              (PROGN
               (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG))))
               (SETQ ERG (DQE_ANDORVALEUR (LIST 'AND ERG HILF)))))))
           (GO WHILELABEL))
         (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG))))))
       (T (SETQ ERG FORMEL)))
      (COND (*DQEOPTSIMP (SETQ ERG (DQE_DKNFSIMPLIFY ERG))))
      (RETURN ERG))) 
(PUT 'DQE_DISTRIBUTIV 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_DISTRIBUTIV 'DEFINED-ON-LINE '902) 
(PUT 'DQE_DISTRIBUTIV 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_DISTRIBUTIV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_DISTRIBUTIV (FORMEL)
    (PROG (SYMB1 SYMB2 AUSG HILF1 HILF2 HILF HILF3 HILFF)
      (SETQ SYMB1 (CAR FORMEL))
      (SETQ AUSG NIL)
      (COND ((EQUAL SYMB1 'OR) (SETQ SYMB2 'AND)) (T (SETQ SYMB2 'OR)))
      (SETQ FORMEL (CDR FORMEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT FORMEL) (RETURN NIL)))
        (PROGN
         (SETQ HILF (CAR FORMEL))
         (SETQ FORMEL (CDR FORMEL))
         (COND
          ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF))
           (PROGN
            (COND ((NOT AUSG) (SETQ AUSG (CONS HILF AUSG)))
                  ((NOT (CDR AUSG))
                   (PROGN
                    (SETQ HILF1 (CAR AUSG))
                    (COND
                     ((NOT (EQUAL HILF HILF1))
                      (SETQ AUSG (LIST SYMB1 HILF1 HILF))))))
                  ((EQUAL (CAR AUSG) SYMB1)
                   (SETQ AUSG (DQE_MODCONS HILF AUSG)))
                  (T
                   (PROGN
                    (SETQ HILF1 (CDR AUSG))
                    (SETQ AUSG NIL)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT HILF1) (RETURN NIL)))
                      (PROGN
                       (SETQ HILF2 (CAR HILF1))
                       (SETQ HILF1 (CDR HILF1))
                       (COND
                        ((OR (EQUAL HILF2 T) (NOT HILF2) (DQE_ISATOMARP HILF2))
                         (PROGN
                          (COND
                           ((NOT (EQUAL HILF2 HILF1))
                            (SETQ HILF2 (LIST SYMB1 HILF2 HILF))))))
                        (T (SETQ HILF2 (DQE_MODCONS HILF HILF2))))
                       (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                      (GO WHILELABEL))
                    (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG)))))))))
          ((EQUAL (CAR HILF) SYMB1)
           (PROGN
            (SETQ HILF (DQE_DISTRIBUTIV HILF))
            (COND
             ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF))
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG (CONS HILF AUSG)))
                     ((NOT (CDR AUSG))
                      (PROGN
                       (SETQ HILF1 (CAR AUSG))
                       (COND
                        ((NOT (EQUAL HILF HILF1))
                         (SETQ AUSG (LIST SYMB1 HILF1 HILF))))))
                     ((EQUAL (CAR AUSG) SYMB1)
                      (SETQ AUSG (DQE_MODCONS HILF AUSG)))
                     (T
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (PROGN
                             (COND
                              ((NOT (EQUAL HILF2 HILF1))
                               (SETQ HILF2 (LIST SYMB1 HILF2 HILF))))))
                           (T (SETQ HILF2 (DQE_MODCONS HILF HILF2))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG)))))))))
             ((EQUAL (CAR HILF) SYMB1)
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG HILF))
                     ((NOT (CDR AUSG))
                      (SETQ AUSG
                              (CONS SYMB1 (DQE_CONSM (CAR AUSG) (CDR HILF)))))
                     ((EQUAL (CAR AUSG) SYMB1)
                      (SETQ AUSG (DQE_ANDORVALEUR (LIST SYMB1 AUSG HILF))))
                     (T
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (SETQ HILF2
                                    (LIST SYMB1 (DQE_CONSM HILF2 (CDR HILF)))))
                           (T
                            (SETQ HILF2
                                    (DQE_ANDORVALEUR
                                     (LIST SYMB1 HILF2 HILF)))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG)))))))))
             (T
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG HILF))
                     ((NOT (CDR AUSG))
                      (PROGN
                       (SETQ HILF1 (CAR AUSG))
                       (SETQ AUSG NIL)
                       (SETQ HILF (CDR HILF))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF))
                          (SETQ HILF (CDR HILF))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (PROGN
                             (COND
                              ((NOT (EQUAL HILF1 HILF2))
                               (SETQ HILF2 (LIST SYMB1 HILF1 HILF2))))))
                           (T
                            (SETQ HILF2
                                    (CONS SYMB1
                                          (DQE_CONSM HILF1 (CDR HILF2))))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))
                     ((EQUAL (CAR AUSG) SYMB2)
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (SETQ HILFF (CDR HILF))
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT HILFF) (RETURN NIL)))
                            (PROGN
                             (SETQ HILF3 (CAR HILFF))
                             (SETQ HILFF (CDR HILFF))
                             (COND
                              ((OR (EQUAL HILF2 T) (NOT HILF2)
                                   (DQE_ISATOMARP HILF2))
                               (PROGN
                                (COND
                                 ((OR (EQUAL HILF3 T) (NOT HILF3)
                                      (DQE_ISATOMARP HILF3))
                                  (PROGN
                                   (COND
                                    ((NOT (EQUAL HILF3 HILF2))
                                     (SETQ HILF3 (LIST SYMB1 HILF2 HILF3))))))
                                 (T
                                  (PROGN
                                   (SETQ HILF3 (DQE_CONSM HILF2 (CDR HILF3)))
                                   (SETQ HILF3 (CONS SYMB1 HILF3)))))))
                              (T
                               (PROGN
                                (COND
                                 ((OR (EQUAL HILF3 T) (NOT HILF3)
                                      (DQE_ISATOMARP HILF3))
                                  (SETQ HILF3 (DQE_MODCONS HILF3 HILF2)))
                                 (T
                                  (SETQ HILF3
                                          (DQE_ANDORVALEUR
                                           (LIST SYMB1 HILF2 HILF3))))))))
                             (SETQ AUSG (DQE_MODCONS HILF3 AUSG)))
                            (GO WHILELABEL)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))
                     (T
                      (PROGN
                       (SETQ HILF (CDR HILF))
                       (SETQ HILF1 AUSG)
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF))
                          (SETQ HILF (CDR HILF))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (SETQ HILF2 (DQE_MODCONS HILF2 HILF1)))
                           (T
                            (SETQ HILF2
                                    (DQE_ANDORVALEUR
                                     (LIST SYMB1 HILF1 HILF2)))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND
                        ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))))))))
          (T
           (PROGN
            (COND ((EQUAL SYMB2 'OR) (SETQ HILF (DQE_DISJNF HILF)))
                  (T (SETQ HILF (DQE_KONJNF HILF))))
            (COND
             ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF))
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG (CONS HILF AUSG)))
                     ((NOT (CDR AUSG))
                      (PROGN
                       (SETQ HILF1 (CAR AUSG))
                       (COND
                        ((NOT (EQUAL HILF HILF1))
                         (SETQ AUSG (LIST SYMB1 HILF1 HILF))))))
                     ((EQUAL (CAR AUSG) SYMB1)
                      (SETQ AUSG (DQE_MODCONS HILF AUSG)))
                     (T
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (PROGN
                             (COND
                              ((NOT (EQUAL HILF2 HILF1))
                               (SETQ HILF2 (LIST SYMB1 HILF2 HILF))))))
                           (T (SETQ HILF2 (DQE_MODCONS HILF HILF2))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG)))))))))
             ((EQUAL (CAR HILF) SYMB2)
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG HILF))
                     ((NOT (CDR AUSG))
                      (PROGN
                       (SETQ HILF1 (CAR AUSG))
                       (SETQ AUSG NIL)
                       (SETQ HILF (CDR HILF))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF))
                          (SETQ HILF (CDR HILF))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (PROGN
                             (COND
                              ((NOT (EQUAL HILF2 HILF1))
                               (SETQ HILF2 (LIST SYMB1 HILF1 HILF2))))))
                           (T
                            (SETQ HILF2
                                    (CONS SYMB1
                                          (DQE_CONSM HILF1 (CDR HILF2))))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))
                     ((EQUAL (CAR AUSG) SYMB2)
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (SETQ HILFF (CDR HILF))
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT HILFF) (RETURN NIL)))
                            (PROGN
                             (SETQ HILF3 (CAR HILFF))
                             (SETQ HILFF (CDR HILFF))
                             (COND
                              ((OR (EQUAL HILF2 T) (NOT HILF2)
                                   (DQE_ISATOMARP HILF2))
                               (PROGN
                                (COND
                                 ((OR (EQUAL HILF3 T) (NOT HILF3)
                                      (DQE_ISATOMARP HILF3))
                                  (PROGN
                                   (COND
                                    ((NOT (EQUAL HILF2 HILF3))
                                     (SETQ HILF3 (LIST SYMB1 HILF2 HILF3))))))
                                 (T
                                  (PROGN
                                   (SETQ HILF3 (DQE_CONSM HILF2 (CDR HILF3)))
                                   (SETQ HILF3 (CONS SYMB1 HILF3)))))))
                              (T
                               (PROGN
                                (COND
                                 ((OR (EQUAL HILF3 T) (NOT HILF3)
                                      (DQE_ISATOMARP HILF3))
                                  (SETQ HILF3 (DQE_MODCONS HILF3 HILF2)))
                                 (T
                                  (SETQ HILF3
                                          (DQE_ANDORVALEUR
                                           (LIST SYMB1 HILF2 HILF3))))))))
                             (SETQ AUSG (DQE_MODCONS HILF3 AUSG)))
                            (GO WHILELABEL)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))
                     (T
                      (PROGN
                       (SETQ HILF1 AUSG)
                       (SETQ AUSG NIL)
                       (SETQ HILF (CDR HILF))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF))
                          (SETQ HILF (CDR HILF))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (SETQ HILF2 (DQE_MODCONS HILF2 HILF1)))
                           (T
                            (SETQ HILF2
                                    (DQE_ANDORVALEUR
                                     (LIST SYMB1 HILF1 HILF2)))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG)))))))))
             (T
              (PROGN
               (COND ((NOT AUSG) (SETQ AUSG HILF))
                     ((NOT (CDR AUSG))
                      (SETQ AUSG
                              (CONS SYMB1 (DQE_CONSM (CAR AUSG) (CDR HILF)))))
                     ((EQUAL (CAR AUSG) SYMB1)
                      (SETQ AUSG (DQE_ANDORVALEUR (LIST SYMB1 AUSG HILF))))
                     (T
                      (PROGN
                       (SETQ HILF1 (CDR AUSG))
                       (SETQ AUSG NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT HILF1) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF2 (CAR HILF1))
                          (SETQ HILF1 (CDR HILF1))
                          (COND
                           ((OR (EQUAL HILF2 T) (NOT HILF2)
                                (DQE_ISATOMARP HILF2))
                            (SETQ HILF2
                                    (CONS SYMB1 (DQE_CONSM HILF2 (CDR HILF)))))
                           (T
                            (SETQ HILF2
                                    (DQE_ANDORVALEUR
                                     (LIST SYMB1 HILF2 HILF)))))
                          (SETQ AUSG (DQE_MODCONS HILF2 AUSG)))
                         (GO WHILELABEL))
                       (COND
                        ((CDR AUSG) (SETQ AUSG (CONS SYMB2 AUSG))))))))))))))
        (GO WHILELABEL))
      (COND ((EQUAL (LENGTH AUSG) 1) (SETQ AUSG (CAR AUSG))))
      (RETURN AUSG))) 
(PUT 'DQE_SIMPLIFYAT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_SIMPLIFYAT 'DEFINED-ON-LINE '1217) 
(PUT 'DQE_SIMPLIFYAT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SIMPLIFYAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_SIMPLIFYAT (PHI)
    (PROG (DIFF ERG HILF LISTE)
      (COND ((OR (ATOM PHI) (NOT PHI)) (SETQ ERG PHI))
            (T
             (PROGN
              (SETQ DIFF (CADR PHI))
              (COND
               ((DQE_ISCONSTANT DIFF)
                (SETQ ERG (EVAL (LIST (CAR PHI) DIFF 0))))
               ((LISTP DIFF)
                (PROGN
                 (COND
                  ((OR (EQUAL (CAR DIFF) 'MINUS) (EQUAL (CAR DIFF) 'EXPT))
                   (PROGN
                    (SETQ DIFF (CADR DIFF))
                    (SETQ ERG (DQE_SIMPLIFYAT (LIST (CAR PHI) DIFF 0)))))
                  ((EQUAL (CAR DIFF) 'TIMES)
                   (PROGN
                    (SETQ DIFF (CDR DIFF))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT DIFF) (RETURN NIL)))
                      (PROGN
                       (SETQ HILF (CAR DIFF))
                       (COND
                        ((NOT (DQE_ISCONSTANT HILF))
                         (SETQ LISTE (DQE_CONSM HILF LISTE))))
                       (SETQ DIFF (CDR DIFF)))
                      (GO WHILELABEL))
                    (COND ((NOT LISTE) (SETQ ERG (EVAL (LIST (CAR PHI) 1 0))))
                          ((NOT (CDR LISTE))
                           (SETQ ERG (LIST (CAR PHI) (CAR LISTE) 0)))
                          (T
                           (PROGN
                            (PROG ()
                             WHILELABEL
                              (COND ((NOT LISTE) (RETURN NIL)))
                              (PROGN
                               (SETQ HILF (CAR LISTE))
                               (SETQ LISTE (CDR LISTE))
                               (SETQ HILF
                                       (DQE_SIMPLIFYAT
                                        (LIST (CAR PHI) HILF 0)))
                               NIL
                               (SETQ ERG (DQE_MODCONS HILF ERG)))
                              (GO WHILELABEL))
                            (COND ((NOT (CDR ERG)) (SETQ ERG (CAR ERG)))
                                  ((EQUAL (CAR PHI) 'NEQ)
                                   (SETQ ERG (CONS 'AND ERG)))
                                  (T (SETQ ERG (CONS 'OR ERG)))))))))
                  ((EQUAL (CAR DIFF) 'PLUS)
                   (PROGN
                    (SETQ HILF (QE92_LIN_NORMCONTENT DIFF))
                    (COND
                     ((NOT (EQUAL HILF 1))
                      (SETQ DIFF (REVAL1 (LIST 'QUOTIENT DIFF HILF) T))))
                    (COND
                     ((MINUSF (CAR (SIMP DIFF)))
                      (SETQ DIFF (REVAL1 (LIST 'MINUS DIFF) T))))
                    (SETQ ERG (LIST (CAR PHI) DIFF 0))))
                  (T (SETQ ERG (LIST (CAR PHI) DIFF 0))))))
               (T (SETQ ERG PHI))))))
      (RETURN ERG))) 
(PUT 'DQE_SIMPLIFY 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_SIMPLIFY 'DEFINED-ON-LINE '1286) 
(PUT 'DQE_SIMPLIFY 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_SIMPLIFY (PHI)
    (PROG (ERG HILF ERGHILF WEITER)
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ ERG PHI))
            ((EQUAL (CAR PHI) 'AND)
             (PROGN
              (SETQ WEITER T)
              (SETQ HILF (CDR PHI))
              (SETQ ERG NIL)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND WEITER HILF)) (RETURN NIL)))
                (PROGN
                 (SETQ ERGHILF (DQE_SIMPLIFY (CAR HILF)))
                 (SETQ HILF (CDR HILF))
                 (COND ((EQUAL ERGHILF NIL) (SETQ WEITER NIL))
                       ((NEQ ERGHILF T) (SETQ ERG (DQE_MODCONS ERGHILF ERG)))))
                (GO WHILELABEL))
              (COND ((EQUAL WEITER NIL) (SETQ ERG NIL))
                    ((NOT ERG) (SETQ ERG T))
                    ((CDR ERG) (SETQ ERG (CONS 'AND ERG)))
                    (T (SETQ ERG (CAR ERG))))))
            ((EQUAL (CAR PHI) 'OR)
             (PROGN
              (SETQ WEITER T)
              (SETQ HILF (CDR PHI))
              (SETQ ERG NIL)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND WEITER HILF)) (RETURN NIL)))
                (PROGN
                 (SETQ ERGHILF (DQE_SIMPLIFY (CAR HILF)))
                 (SETQ HILF (CDR HILF))
                 (COND ((EQUAL ERGHILF T) (SETQ WEITER NIL))
                       ((NEQ ERGHILF NIL)
                        (SETQ ERG (DQE_MODCONS ERGHILF ERG)))))
                (GO WHILELABEL))
              (COND ((EQUAL WEITER NIL) (SETQ ERG T))
                    ((NOT ERG) (SETQ ERG NIL))
                    ((CDR ERG) (SETQ ERG (CONS 'OR ERG)))
                    (T (SETQ ERG (CAR ERG))))))
            (T (SETQ ERG (DQE_SIMPLIFYAT PHI))))
      (COND (*DQEOPTSIMP (SETQ ERG (DQE_HELPSIMPLIFY ERG))))
      (RETURN ERG))) 
(PUT 'QE92_LIN_NORMCONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'QE92_LIN_NORMCONTENT 'DEFINED-ON-LINE '1344) 
(PUT 'QE92_LIN_NORMCONTENT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'QE92_LIN_NORMCONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QE92_LIN_NORMCONTENT (U) (PREPF (QE92_LIN_NORMCONTENT1 (CAR (SIMP U)) NIL))) 
(PUT 'QE92_LIN_NORMCONTENT1 'NUMBER-OF-ARGS 2) 
(PUT 'QE92_LIN_NORMCONTENT1 'DEFINED-ON-LINE '1347) 
(PUT 'QE92_LIN_NORMCONTENT1 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'QE92_LIN_NORMCONTENT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QE92_LIN_NORMCONTENT1 (U G)
    (COND ((EQUAL G 1) G) ((OR (ATOM U) (ATOM (CAR U))) (GCDF (ABSF U) G))
          (T
           (QE92_LIN_NORMCONTENT1 (CDR U) (QE92_LIN_NORMCONTENT1 (CDAR U) G))))) 
(PUT 'DQE_HELPREMAINDER 'NUMBER-OF-ARGS 3) 
(FLAG '(DQE_HELPREMAINDER) 'OPFN) 
(PUT 'DQE_HELPREMAINDER 'DEFINED-ON-LINE '1363) 
(PUT 'DQE_HELPREMAINDER 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPREMAINDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_HELPREMAINDER (PHI PSI VAR)
    (PROG (ERG)
      (AEVAL (KORDER (LIST VAR)))
      (SETQ ERG (AEVAL (LIST 'REMAINDER PHI PSI)))
      (RETURN (AEVAL ERG)))) 
(PUT 'DQE_HELPCOEFF 'NUMBER-OF-ARGS 2) 
(FLAG '(DQE_HELPCOEFF) 'OPFN) 
(PUT 'DQE_HELPCOEFF 'DEFINED-ON-LINE '1380) 
(PUT 'DQE_HELPCOEFF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_HELPCOEFF (PHI VAR)
    (PROG (ERG) (SETQ ERG (AEVAL (LIST 'COEFF PHI VAR))) (RETURN (AEVAL ERG)))) 
(PUT 'DQE_KOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_KOEFF 'DEFINED-ON-LINE '1399) 
(PUT 'DQE_KOEFF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_KOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_KOEFF (PHI VAR)
    (PROG (ERG)
      (SETQ ERG (CDR (REVAL1 (DQE_HELPCOEFF PHI VAR) T)))
      (RETURN ERG))) 
(PUT 'DQE_RESTFKT 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_RESTFKT 'DEFINED-ON-LINE '1419) 
(PUT 'DQE_RESTFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_RESTFKT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_RESTFKT (PHI PSI VAR)
    (PROG (ERG)
      (SETQ ERG (DQE_PFORM (DQE_HELPREMAINDER PHI PSI VAR)))
      (COND ((NOT ERG) (SETQ ERG 0)))
      (RETURN ERG))) 
(PUT 'DQE_PSEUDF 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_PSEUDF 'DEFINED-ON-LINE '1439) 
(PUT 'DQE_PSEUDF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PSEUDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_PSEUDF (PHI VAR) (REVAL1 (LIST 'DF PHI VAR) T)) 
(PUT 'DQE_VARMENGEFKT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_VARMENGEFKT 'DEFINED-ON-LINE '1458) 
(PUT 'DQE_VARMENGEFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_VARMENGEFKT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_VARMENGEFKT (PHI)
    (PROG (VARMENGE HILF ELEM HILFMENGE)
      (SETQ HILF PHI)
      (SETQ VARMENGE NIL)
      (COND
       ((ATOM HILF)
        (PROGN
         (COND ((NOT (DQE_ISCONSTANT HILF)) (SETQ VARMENGE (LIST HILF))))))
       ((EQUAL (CAR HILF) 'D) (SETQ VARMENGE (LIST HILF)))
       (T
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT HILF) (RETURN NIL)))
           (PROGN
            (SETQ ELEM (CAR HILF))
            (SETQ HILF (CDR HILF))
            (COND
             ((ATOM ELEM)
              (PROGN
               (COND
                ((NOT
                  (OR (EQUAL ELEM 'PLUS) (EQUAL ELEM 'TIMES) (EQUAL ELEM 'EXPT)
                      (EQUAL ELEM 'MINUS) (DQE_ISCONSTANT ELEM)))
                 (SETQ VARMENGE (DQE_MODCONS ELEM VARMENGE))))))
             (T
              (PROGN
               (SETQ HILFMENGE (DQE_VARMENGEFKT ELEM))
               (PROG ()
                WHILELABEL
                 (COND ((NOT HILFMENGE) (RETURN NIL)))
                 (PROGN
                  (SETQ VARMENGE (DQE_MODCONS (CAR HILFMENGE) VARMENGE))
                  (SETQ HILFMENGE (CDR HILFMENGE)))
                 (GO WHILELABEL))))))
           (GO WHILELABEL)))))
      (RETURN VARMENGE))) 
(PUT 'DQE_PARTIELDF 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_PARTIELDF 'DEFINED-ON-LINE '1501) 
(PUT 'DQE_PARTIELDF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PARTIELDF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_PARTIELDF (PHI VAR DIFFEQUALISTE)
    (PROG (HILF LISTE AUSG)
      (SETQ AUSG 0)
      (SETQ HILF (DQE_PSEUDF PHI VAR))
      (COND
       ((NOT (MEMBER VAR DIFFEQUALISTE))
        (PROGN
         (COND
          ((ATOM VAR)
           (SETQ AUSG (REVAL1 (LIST 'TIMES HILF (LIST 'D VAR 1)) T)))
          (T
           (SETQ AUSG
                   (REVAL1
                    (LIST 'TIMES HILF
                          (LIST 'D (CADR VAR)
                                (EVAL (LIST 'PLUS (CADDR VAR) 1))))
                    T))))))
       (T
        (PROGN
         (SETQ LISTE DIFFEQUALISTE)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (EQUAL VAR (CAR LISTE)))) (RETURN NIL)))
           (PROGN (SETQ LISTE (CDDR LISTE)))
           (GO WHILELABEL))
         (COND ((EQUAL (CADR LISTE) 0) (SETQ AUSG 0))
               (T (SETQ AUSG (REVAL1 (LIST 'TIMES HILF (CADR LISTE)) T)))))))
      (RETURN AUSG))) 
(PUT 'DQE_DIFFKT 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_DIFFKT 'DEFINED-ON-LINE '1536) 
(PUT 'DQE_DIFFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_DIFFKT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_DIFFKT (PHI DIFFEQUALISTE)
    (PROG (VAR VARMENGE HILF ERG)
      (SETQ ERG NIL)
      (SETQ VARMENGE (DQE_VARMENGEFKT PHI))
      (PROG ()
       WHILELABEL
        (COND ((NOT VARMENGE) (RETURN NIL)))
        (PROGN
         (SETQ VAR (CAR VARMENGE))
         (SETQ VARMENGE (CDR VARMENGE))
         (SETQ HILF (DQE_PARTIELDF PHI VAR DIFFEQUALISTE))
         (COND ((NOT (EQUAL HILF 0)) (SETQ ERG (CONS HILF ERG)))))
        (GO WHILELABEL))
      (COND ((NOT ERG) (SETQ ERG 0)) ((NOT (CDR ERG)) (SETQ ERG (CAR ERG)))
            (T (SETQ ERG (REVAL1 (CONS 'PLUS ERG) T))))
      (RETURN ERG))) 
(PUT 'DQE_DIFF 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_DIFF 'DEFINED-ON-LINE '1567) 
(PUT 'DQE_DIFF 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_DIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_DIFF (PHI CONST DIFFEQUALISTE)
    (PROG (HILF ERG)
      (SETQ ERG PHI)
      (SETQ HILF 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ CONST HILF)) (RETURN NIL)))
        (PROGN
         (SETQ ERG (DQE_DIFFKT ERG DIFFEQUALISTE))
         (SETQ HILF (PLUS HILF 1)))
        (GO WHILELABEL))
      (RETURN ERG))) 
(PUT 'DQE_TERMCOEFKT 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_TERMCOEFKT 'DEFINED-ON-LINE '1595) 
(PUT 'DQE_TERMCOEFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_TERMCOEFKT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_TERMCOEFKT (PHI VAR)
    (PROG (HILF ORDC REST CONST ERG AUSG)
      (SETQ AUSG NIL)
      (SETQ ORDC (DQE_ORD PHI VAR))
      (SETQ REST (DQE_RESTFKT PHI VAR VAR))
      (SETQ CONST 1)
      (COND
       ((AND (NOT (EQUAL ORDC 0)) (NOT (EQUAL REST 0)))
        (PROG ()
         WHILELABEL
          (COND ((NOT (LEQ CONST ORDC)) (RETURN NIL)))
          (PROGN
           (SETQ REST
                   (DQE_RESTFKT REST (LIST 'D VAR CONST) (LIST 'D VAR CONST)))
           (COND ((EQUAL REST 0) (SETQ CONST (PLUS ORDC 1)))
                 (T (SETQ CONST (PLUS CONST 1)))))
          (GO WHILELABEL))))
      (SETQ HILF (REVAL1 (LIST 'DIFFERENCE PHI REST) T))
      (SETQ HILF (DQE_KOEFF HILF VAR))
      (SETQ CONST 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ CONST ORDC)) (RETURN NIL)))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT HILF) (RETURN NIL)))
           (PROGN
            (COND
             ((NOT (EQUAL (CAR HILF) 0))
              (PROGN
               (SETQ ERG (DQE_KOEFF (CAR HILF) (LIST 'D VAR CONST)))
               (SETQ AUSG (APPEND AUSG ERG)))))
            (SETQ HILF (CDR HILF)))
           (GO WHILELABEL))
         (SETQ HILF AUSG)
         (SETQ AUSG NIL)
         (SETQ CONST (PLUS CONST 1)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT HILF) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (EQUAL (CAR HILF) 0))
           (SETQ AUSG (DQE_MODCONS (CAR HILF) AUSG))))
         (SETQ HILF (CDR HILF)))
        (GO WHILELABEL))
      (SETQ AUSG (CONS REST AUSG))
      (RETURN AUSG))) 
(PUT 'DQE_HELPORD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_HELPORD 'DEFINED-ON-LINE '1645) 
(PUT 'DQE_HELPORD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_HELPORD (PHI VAR)
    (PROG (ERG HILF)
      (SETQ ERG 0)
      (COND ((ATOM PHI) (SETQ ERG 0))
            ((EQUAL (CAR PHI) 'D)
             (PROGN
              (COND ((EQUAL (CADR PHI) VAR) (SETQ ERG (CADDR PHI)))
                    (T (SETQ ERG 0)))))
            ((EQUAL (CAR PHI) 'EXPT) (SETQ ERG (DQE_HELPORD (CADR PHI) VAR)))
            ((EQUAL (CAR PHI) 'MINUS) (SETQ ERG (DQE_HELPORD (CADR PHI) VAR)))
            ((EQUAL (CAR PHI) 'TIMES)
             (PROGN
              (SETQ PHI (CDR PHI))
              (SETQ ERG 0)
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ PHI (CDR PHI))
                 (SETQ HILF (DQE_HELPORD HILF VAR))
                 (SETQ ERG (PLUS ERG HILF)))
                (GO WHILELABEL))))
            (T (SETQ ERG 0)))
      (RETURN ERG))) 
(PUT 'DQE_ORD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_ORD 'DEFINED-ON-LINE '1687) 
(PUT 'DQE_ORD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_ORD (PHI VAR)
    (PROG (AUSG HILF)
      (SETQ AUSG 0)
      (COND ((ATOM PHI) (SETQ AUSG 0))
            ((NOT (EQUAL (CAR PHI) 'PLUS)) (SETQ AUSG (DQE_HELPORD PHI VAR)))
            (T
             (PROGN
              (SETQ PHI (CDR PHI))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ PHI (CDR PHI))
                 (SETQ HILF (DQE_HELPORD HILF VAR))
                 (COND ((LESSP AUSG HILF) (SETQ AUSG HILF))))
                (GO WHILELABEL)))))
      (RETURN AUSG))) 
(PUT 'DQE_GRAD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_GRAD 'DEFINED-ON-LINE '1719) 
(PUT 'DQE_GRAD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_GRAD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_GRAD (PHI VAR)
    (PROG (ERG HILF ORDC)
      (SETQ ORDC (DQE_ORD PHI VAR))
      (COND ((EQUAL ORDC 0) (SETQ HILF VAR))
            (T (SETQ HILF (LIST 'D VAR ORDC))))
      (SETQ ERG (DEG PHI HILF))
      (COND ((NULL ERG) (SETQ ERG 0)))
      (RETURN ERG))) 
(PUT 'DQE_INITIAL 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_INITIAL 'DEFINED-ON-LINE '1741) 
(PUT 'DQE_INITIAL 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_INITIAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_INITIAL (PHI VAR)
    (PROG (ORDC HILFVAR AUSG)
      (SETQ ORDC (DQE_ORD PHI VAR))
      (COND ((EQUAL ORDC 0) (SETQ HILFVAR VAR))
            (T (SETQ HILFVAR (LIST 'D VAR ORDC))))
      (SETQ AUSG (REVAL1 (LCOF PHI HILFVAR) T))
      (RETURN AUSG))) 
(PUT 'DQE_REDUKTUM 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_REDUKTUM 'DEFINED-ON-LINE '1763) 
(PUT 'DQE_REDUKTUM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_REDUKTUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_REDUKTUM (PHI VAR)
    (PROG (ORDC GRADC HILF HILFVAR AUSG)
      (SETQ ORDC (DQE_ORD PHI VAR))
      (SETQ GRADC (DQE_GRAD PHI VAR))
      (COND ((EQUAL ORDC 0) (SETQ HILFVAR VAR))
            (T (SETQ HILFVAR (LIST 'D VAR ORDC))))
      (SETQ HILF (LIST 'EXPT HILFVAR GRADC))
      (SETQ HILF (REVAL1 (LIST 'TIMES (DQE_INITIAL PHI VAR) HILF) T))
      (SETQ AUSG (REVAL1 (LIST 'DIFFERENCE PHI HILF) T))
      (RETURN AUSG))) 
(PUT 'DQE_SEPARANTE 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_SEPARANTE 'DEFINED-ON-LINE '1787) 
(PUT 'DQE_SEPARANTE 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SEPARANTE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_SEPARANTE (PHI VAR)
    (PROG (ORDC HILFVAR AUSG)
      (SETQ ORDC (DQE_ORD PHI VAR))
      (COND ((EQUAL ORDC 0) (SETQ HILFVAR VAR))
            (T (SETQ HILFVAR (LIST 'D VAR ORDC))))
      (SETQ AUSG (DQE_PSEUDF PHI HILFVAR))
      (RETURN AUSG))) 
(PUT 'DQE_PSEUDREST 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_PSEUDREST 'DEFINED-ON-LINE '1810) 
(PUT 'DQE_PSEUDREST 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PSEUDREST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_PSEUDREST (PHI PSI VAR)
    (PROG (REST Q K L HILF)
      (SETQ REST PHI)
      (SETQ HILF (DEG REST VAR))
      (COND ((NOT HILF) (SETQ HILF 0)))
      (SETQ Q 0)
      (SETQ K 0)
      (SETQ L (DEG PSI VAR))
      (COND ((NOT L) (SETQ L 0)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (EQUAL HILF 0)) (NOT (EQUAL L 0)) (GEQ HILF L)))
          (RETURN NIL)))
        (PROGN
         (SETQ K
                 (LIST 'TIMES (REVAL1 (LCOF REST VAR) T)
                       (LIST 'EXPT VAR (REVAL1 (LIST 'DIFFERENCE HILF L) T))))
         (SETQ Q (LIST 'PLUS (LIST 'TIMES (REVAL1 (LCOF PSI VAR) T) Q) K))
         (SETQ REST
                 (REVAL1
                  (LIST 'DIFFERENCE
                        (REVAL1 (LIST 'TIMES (LCOF PSI VAR) REST) T)
                        (LIST 'TIMES K PSI))
                  T))
         (SETQ HILF (DEG REST VAR))
         (COND ((NOT HILF) (SETQ HILF 0))))
        (GO WHILELABEL))
      (COND ((NOT REST) (SETQ REST 0)) (T (SETQ REST (REVAL1 REST T))))
      (RETURN REST))) 
(PUT 'DQE_LISTENORD 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_LISTENORD 'DEFINED-ON-LINE '1854) 
(PUT 'DQE_LISTENORD 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_LISTENORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_LISTENORD (PHI VAR)
    (PROG (GEORDLISTE HILFLIST HILF HILF1 ERG TESTVAR)
      (SETQ GEORDLISTE NIL)
      (SETQ ERG NIL)
      (SETQ TESTVAR T)
      (COND
       ((CDR PHI)
        (PROGN
         (SETQ HILFLIST (LIST (CAR PHI)))
         (SETQ PHI (CDR PHI))
         (PROG ()
          WHILELABEL
           (COND ((NOT PHI) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR PHI))
            (SETQ PHI (CDR PHI))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND HILFLIST TESTVAR)) (RETURN NIL)))
              (PROGN
               (SETQ HILF1 (CAR HILFLIST))
               (COND
                ((GREATERP (DQE_ORD HILF VAR) (DQE_ORD HILF1 VAR))
                 (PROGN
                  (SETQ ERG (DQE_CONSM HILF HILFLIST))
                  (SETQ GEORDLISTE (APPEND GEORDLISTE ERG))
                  (SETQ TESTVAR NIL)))
                ((AND (EQUAL (DQE_ORD HILF VAR) (DQE_ORD HILF1 VAR))
                      (GEQ (DQE_GRAD HILF VAR) (DQE_GRAD HILF1 VAR)))
                 (PROGN
                  (SETQ ERG (DQE_CONSM HILF HILFLIST))
                  (SETQ GEORDLISTE (APPEND GEORDLISTE ERG))
                  (SETQ TESTVAR NIL)))
                (T
                 (PROGN
                  (SETQ GEORDLISTE (REVERSE GEORDLISTE))
                  (SETQ GEORDLISTE (REVERSE (DQE_CONSM HILF1 GEORDLISTE)))
                  (SETQ HILFLIST (CDR HILFLIST)))))
               (COND
                ((AND (NOT HILFLIST) TESTVAR)
                 (SETQ GEORDLISTE (DQE_MODCONS HILF GEORDLISTE)))))
              (GO WHILELABEL))
            (COND
             (PHI
              (PROGN
               (SETQ HILFLIST GEORDLISTE)
               (SETQ GEORDLISTE NIL)
               (SETQ TESTVAR T)))
             (T (SETQ GEORDLISTE (REVERSE GEORDLISTE)))))
           (GO WHILELABEL))))
       (T (SETQ GEORDLISTE PHI)))
      (RETURN GEORDLISTE))) 
(PUT 'DQE_NEQNULLFKT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_NEQNULLFKT 'DEFINED-ON-LINE '1910) 
(PUT 'DQE_NEQNULLFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_NEQNULLFKT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_NEQNULLFKT (PHI)
    (PROG (R)
      (COND ((NOT PHI) (RETURN NIL)))
      (SETQ R
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM PHI)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (LIST 'NEQ (REVAL1 ELEM T) 0))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (LIST 'NEQ (REVAL1 ELEM T) 0))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NOT (CDR R)) (RETURN (CAR R))))
      (RETURN (CONS 'OR R)))) 
(PUT 'DQE_EQUALNULLFKT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_EQUALNULLFKT 'DEFINED-ON-LINE '1938) 
(PUT 'DQE_EQUALNULLFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_EQUALNULLFKT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_EQUALNULLFKT (PHI)
    (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
      (SETQ ELEM PHI)
      (COND ((NULL ELEM) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (ELEM) (LIST 'EQUAL (REVAL1 ELEM T) 0))
                        (CAR ELEM))
                       NIL)))
     LOOPLABEL
      (SETQ ELEM (CDR ELEM))
      (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (ELEM) (LIST 'EQUAL (REVAL1 ELEM T) 0)) (CAR ELEM))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'DQE_ELIMSIMPLIFY 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIMSIMPLIFY 'DEFINED-ON-LINE '1959) 
(PUT 'DQE_ELIMSIMPLIFY 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMSIMPLIFY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMSIMPLIFY (PHI ZWERG VAR)
    (PROG (HILFG HILF ERG1 ERG2 AUSG)
      (SETQ AUSG NIL)
      (SETQ ERG1 NIL)
      (SETQ ERG2 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT PHI) (RETURN NIL)))
        (PROGN
         (SETQ HILF (CAR PHI))
         (SETQ HILFG (DQE_GRAD HILF VAR))
         (COND ((EQUAL HILFG 0) (SETQ ERG1 (DQE_MODCONS (REVAL1 HILF T) ERG1)))
               (T (SETQ ERG2 (DQE_CONSM HILF ERG2))))
         (SETQ PHI (CDR PHI)))
        (GO WHILELABEL))
      (SETQ ERG1 (DQE_EQUALNULLFKT ERG1))
      (COND
       (ERG1
        (PROGN
         (COND ((NOT (CDR ERG1)) (SETQ ERG1 (CAR ERG1)))
               (T (SETQ ERG1 (CONS 'AND ERG1)))))))
      (COND ((AND ZWERG (NOT (CDR ZWERG))) (SETQ ZWERG (CAR ZWERG))))
      (SETQ ERG1 (DQE_ANDORVALEUR (LIST 'AND ZWERG ERG1)))
      (SETQ AUSG (LIST ERG1 ERG2))
      (RETURN AUSG))) 
(PUT 'DQE_START1 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_START1 'DEFINED-ON-LINE '2002) 
(PUT 'DQE_START1 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_START1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_START1 (PHI)
    (PROG (AUSG DIFFEQUALISTE)
      (SETQ DIFFEQUALISTE NIL)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "+++++++++++++++++++++++++++++++++++++++++++++++++++++++")
         (COND
          (*DQEOPTSIMP
           (PROGN
            (PRIN2T "+++ dqeoptsimp ist on d.h. die ergebnisse von simplify+")
            (PRIN2T
             "+++ bzw. disjnf bzw. konjnf werden vereinfacht      +++")))
          (T
           (PRIN2T "+++ deqoptsimp ist off                              +++")))
         (COND
          ((NOT *DQEGRADORD)
           (PRIN2T
            "+++ dqegradord ist off                              +++"))))))
      (COND
       (*DQEOPTQELIM
        (PROGN
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2T "+++ das qe_verfahren wird mit aussagenlogischen     +++")
            (PRIN2T "+++        vereinfachungen ausgefuehrt.             +++")
            (PRIN2T
             "+++++++++++++++++++++++++++++++++++++++++++++++++++++++"))))
         (SETQ AUSG (DQE_QUANTELIMOPT PHI DIFFEQUALISTE))))
       (T
        (PROGN
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2T "+++ das qe_verfahren wird ohne aussagenlogischen    +++")
            (PRIN2T "+++         vereinfachungen ausgefuehrt.            +++")
            (PRIN2T
             "+++++++++++++++++++++++++++++++++++++++++++++++++++++++"))))
         (SETQ AUSG (DQE_QUANTELIM PHI DIFFEQUALISTE)))))
      (RETURN AUSG))) 
(PUT 'DQE_START2 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_START2 'DEFINED-ON-LINE '2060) 
(PUT 'DQE_START2 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_START2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_START2 (PHI DIFFEQUALISTE)
    (PROG (AUSG)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "+++++++++++++++++++++++++++++++++++++++++++++++++++++++")
         (COND
          (*DQEOPTSIMP
           (PROGN
            (PRIN2T "+++ dqeoptsimp ist on d.h. die ergebnisse von simplify+")
            (PRIN2T
             "+++ bzw. disjnf bzw. konjnf werden vereinfacht      +++")))
          (T
           (PRIN2T "+++ deqoptsimp ist off                              +++")))
         (COND
          ((NOT *DQEGRADORD)
           (PRIN2T
            "+++ dqegradord ist off                              +++"))))))
      (COND
       (*DQEOPTQELIM
        (PROGN
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2T "+++ das qe_verfahren wird mit aussagenlogischen     +++")
            (PRIN2T "+++        vereinfachungen ausgefuehrt.             +++")
            (PRIN2T
             "+++++++++++++++++++++++++++++++++++++++++++++++++++++++"))))
         (SETQ AUSG (DQE_QUANTELIMOPT PHI DIFFEQUALISTE))))
       (T
        (PROGN
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2T "+++ das qe_verfahren wird ohne aussagenlogischen    +++")
            (PRIN2T "+++         vereinfachungen ausgefuehrt.            +++")
            (PRIN2T
             "+++++++++++++++++++++++++++++++++++++++++++++++++++++++"))))
         (SETQ AUSG (DQE_QUANTELIM PHI DIFFEQUALISTE))
         NIL)))
      (RETURN AUSG))) 
(PUT 'DQE_ELIM 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIM 'DEFINED-ON-LINE '2116) 
(PUT 'DQE_ELIM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIM (PHI DIFFEQUALISTE VAR)
    (PROG (HILF ORDHILF ERG1 ERG2 AUSG ZWERG PHI1 HILFVAR PHI2 REST HILFF HILFG
           GHILF GRADF GRADG ORDF ORDG REDF INITF CONST ERG21 ERG22 ERG PHI21
           PHI22 REDHILF SEPF GGHILF LISTE HELPLIST)
      (COND ((AND *DQEGRADORD *DQEVERBOSE) (PRIN2T "++++")))
      (SETQ ZWERG NIL)
      (SETQ PHI (DQE_HELPELIM PHI))
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ AUSG PHI))
            ((NOT (CDR PHI))
             (PROGN
              (SETQ HILF (CAR PHI))
              (COND
               ((AND *DQEGRADORD *DQEVERBOSE)
                (PROGN
                 (SETQ ORDG (DQE_ORD HILF VAR))
                 (SETQ GRADG (DQE_GRAD HILF VAR))
                 (PRIN2T "()")
                 (PRIN2T (LIST ORDG GRADG)))))
              (SETQ AUSG (DQE_NEQNULLFKT (DQE_TERMCOEFKT HILF VAR)))))
            ((AND (EQUAL (CAR PHI) 1) (NOT (CDDR PHI)))
             (PROGN
              (SETQ HILF (CADR PHI))
              (COND
               ((AND *DQEGRADORD *DQEVERBOSE)
                (PROGN
                 (SETQ ORDF (DQE_ORD HILF VAR))
                 (SETQ GRADF (DQE_GRAD HILF VAR))
                 (PRIN2T (LIST ORDF GRADF))
                 (PRIN2T "()"))))
              (SETQ ERG (DQE_TERMCOEFKT HILF VAR))
              (SETQ HILF (LIST 'EQUAL (REVAL1 (CAR ERG) T) 0))
              (SETQ ERG (DQE_NEQNULLFKT (CDR ERG)))
              (SETQ AUSG (DQE_ANDORVALEUR (LIST 'OR HILF ERG)))))
            (T
             (PROGN
              (SETQ HILFG (CAR PHI))
              (COND
               ((AND (DQE_ISCONSTANT HILFG) (NOT (EQUAL HILFG 0)))
                (SETQ HILFG 1)))
              (SETQ PHI (CDR PHI))
              (SETQ ORDG (DQE_ORD HILFG VAR))
              (SETQ GRADG (DQE_GRAD HILFG VAR))
              (COND
               ((NOT (CDR PHI))
                (PROGN
                 (SETQ HILFF (CAR PHI))
                 (SETQ ORDF (DQE_ORD HILFF VAR))
                 (SETQ GRADF (DQE_GRAD HILFF VAR))
                 (COND
                  ((AND *DQEGRADORD *DQEVERBOSE)
                   (PROGN
                    (PRIN2T (LIST ORDF GRADF))
                    (PRIN2T (LIST ORDG GRADG)))))
                 (COND
                  ((EQUAL GRADF 0)
                   (PROGN
                    (SETQ ERG1 (LIST 'EQUAL (REVAL1 HILFF T) 0))
                    (SETQ ERG2 (DQE_NEQNULLFKT (DQE_TERMCOEFKT HILFG VAR)))
                    (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG1 ERG2)))))
                  (T
                   (PROGN
                    (SETQ REDF (DQE_REDUKTUM HILFF VAR))
                    (SETQ INITF (DQE_INITIAL HILFF VAR))
                    (COND
                     ((EQUAL REDF 0)
                      (SETQ PHI1
                              (LIST 'AND (LIST 'NEQ HILFG 0)
                                    (LIST 'EQUAL INITF 0))))
                     (T
                      (PROGN
                       (SETQ PHI1
                               (DQE_EQUALNULLFKT
                                (DQE_CONSM INITF (LIST REDF))))
                       (SETQ PHI1
                               (CONS 'AND (CONS (LIST 'NEQ HILFG 0) PHI1))))))
                    (COND
                     ((GREATERP ORDF ORDG)
                      (PROGN
                       (SETQ ERG21 (DQE_NEQNULLFKT (DQE_TERMCOEFKT HILFG VAR)))
                       (SETQ ERG22 (DQE_NEQNULLFKT (DQE_TERMCOEFKT INITF VAR)))
                       (SETQ ERG2 (DQE_ANDORVALEUR (LIST 'AND ERG21 ERG22)))))
                     ((EQUAL ORDF ORDG)
                      (PROGN
                       (COND ((EQUAL ORDF 0) (SETQ HILFVAR VAR))
                             (T (SETQ HILFVAR (LIST 'D VAR ORDF))))
                       (SETQ GHILF
                               (DQE_PSEUDREST (LIST 'EXPT HILFG GRADF) HILFF
                                HILFVAR))
                       (SETQ ERG21 (DQE_NEQNULLFKT (DQE_TERMCOEFKT GHILF VAR)))
                       (SETQ ERG22 (DQE_NEQNULLFKT (DQE_TERMCOEFKT INITF VAR)))
                       (SETQ ERG2 (DQE_ANDORVALEUR (LIST 'AND ERG21 ERG22)))))
                     (T
                      (PROGN
                       (SETQ CONST (REVAL1 (LIST 'DIFFERENCE ORDG ORDF) T))
                       (SETQ HILF (DQE_DIFF HILFF CONST DIFFEQUALISTE))
                       (SETQ HILFVAR (LIST 'D VAR ORDG))
                       (SETQ GHILF (DQE_PSEUDREST HILFG HILF HILFVAR))
                       (COND
                        ((NOT (DQE_ISCONSTANT INITF))
                         (SETQ GHILF (REVAL1 (LIST 'TIMES INITF GHILF) T))))
                       (SETQ PHI21
                               (LIST 'AND (LIST 'NEQ GHILF 0)
                                     (LIST 'EQUAL HILFF 0)))
                       (SETQ ERG21 (DQE_ELIM PHI21 DIFFEQUALISTE VAR))
                       (COND ((DQE_ISCONSTANT INITF) (SETQ GGHILF HILFG))
                             (T
                              (SETQ GGHILF
                                      (REVAL1 (LIST 'TIMES INITF HILFG) T))))
                       (SETQ SEPF (DQE_SEPARANTE HILFF VAR))
                       (SETQ REDHILF (DQE_REDUKTUM HILF VAR))
                       (SETQ PHI22
                               (DQE_CONSM (LIST 'EQUAL SEPF 0)
                                (DQE_CONSM (LIST 'EQUAL REDHILF 0)
                                 (LIST (LIST 'EQUAL HILFF 0)))))
                       (SETQ PHI22
                               (CONS 'AND (CONS (LIST 'NEQ GGHILF 0) PHI22)))
                       (SETQ ERG22 (DQE_ELIM PHI22 DIFFEQUALISTE VAR))
                       (SETQ ERG2 (DQE_ANDORVALEUR (LIST 'OR ERG21 ERG22))))))
                    (SETQ ERG1 (DQE_ELIM PHI1 DIFFEQUALISTE VAR))
                    (SETQ AUSG (DQE_ANDORVALEUR (LIST 'OR ERG1 ERG2))))))))
               (T
                (PROGN
                 (SETQ PHI (DQE_ELIMSIMPLIFY PHI ZWERG VAR))
                 (SETQ ZWERG (CAR PHI))
                 (SETQ PHI (CADR PHI))
                 (COND
                  ((NOT PHI)
                   (PROGN
                    (COND
                     ((AND *DQEGRADORD *DQEVERBOSE)
                      (PROGN (PRIN2T "()") (PRIN2T (LIST ORDG GRADG)))))
                    (SETQ ERG (DQE_NEQNULLFKT (DQE_TERMCOEFKT HILFG VAR)))
                    (COND
                     ((AND ZWERG (NOT (CDR ZWERG)))
                      (SETQ AUSG
                              (DQE_ANDORVALEUR (LIST 'AND ERG (CAR ZWERG)))))
                     (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG ZWERG)))))))
                  ((NOT (CDR PHI))
                   (PROGN
                    (SETQ PHI
                            (LIST 'AND (LIST 'NEQ HILFG 0)
                                  (LIST 'EQUAL (CAR PHI) 0)))
                    (SETQ ERG (DQE_ELIM PHI DIFFEQUALISTE VAR))
                    (COND
                     ((AND ZWERG (NOT (CDR ZWERG)))
                      (SETQ AUSG
                              (DQE_ANDORVALEUR (LIST 'AND ERG (CAR ZWERG)))))
                     (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG ZWERG)))))))
                  (T
                   (PROGN
                    (SETQ PHI (DQE_LISTENORD PHI VAR))
                    (COND
                     ((AND *DQEGRADORD *DQEVERBOSE)
                      (PROGN
                       (SETQ LISTE PHI)
                       (SETQ HELPLIST NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT LISTE) (RETURN NIL)))
                         (PROGN
                          (SETQ HILF (CAR LISTE))
                          (SETQ LISTE (CDR LISTE))
                          (SETQ HELPLIST
                                  (CONS
                                   (LIST (DQE_ORD HILF VAR)
                                         (DQE_GRAD HILF VAR))
                                   HELPLIST)))
                         (GO WHILELABEL))
                       (PRIN2T HELPLIST)
                       (PRIN2T (LIST ORDG GRADG))
                       NIL)))
                    (SETQ HILFF (CAR PHI))
                    (SETQ INITF (DQE_INITIAL HILFF VAR))
                    (SETQ REDF (DQE_REDUKTUM HILFF VAR))
                    (SETQ ORDF (DQE_ORD HILFF VAR))
                    (COND
                     ((EQUAL REDF 0)
                      (PROGN
                       (SETQ PHI1
                               (DQE_EQUALNULLFKT (DQE_CONSM INITF (CDR PHI))))
                       (SETQ PHI1
                               (CONS 'AND (CONS (LIST 'NEQ HILFG 0) PHI1)))))
                     (T
                      (PROGN
                       (SETQ PHI1
                               (DQE_EQUALNULLFKT
                                (DQE_CONSM INITF (DQE_CONSM REDF (CDR PHI)))))
                       (SETQ PHI1
                               (CONS 'AND (CONS (LIST 'NEQ HILFG 0) PHI1))))))
                    (COND ((DQE_ISCONSTANT INITF) (SETQ GHILF HILFG))
                          (T
                           (SETQ GHILF (REVAL1 (LIST 'TIMES INITF HILFG) T))))
                    (SETQ HILF (CADR PHI))
                    (SETQ ORDHILF (DQE_ORD HILF VAR))
                    (COND ((EQUAL ORDHILF 0) (SETQ HILFVAR VAR))
                          (T (SETQ HILFVAR (LIST 'D VAR ORDHILF))))
                    (COND
                     ((EQUAL ORDHILF ORDF)
                      (SETQ REST (DQE_PSEUDREST HILF HILFF HILFVAR)))
                     (T
                      (PROGN
                       (SETQ CONST (REVAL1 (LIST 'DIFFERENCE ORDHILF ORDF) T))
                       (SETQ REST
                               (DQE_PSEUDREST HILF
                                (DQE_DIFF HILFF CONST DIFFEQUALISTE)
                                HILFVAR)))))
                    (COND
                     ((EQUAL REST 0)
                      (SETQ PHI2
                              (DQE_EQUALNULLFKT (DQE_CONSM HILFF (CDDR PHI)))))
                     (T
                      (SETQ PHI2
                              (DQE_EQUALNULLFKT
                               (DQE_CONSM REST
                                (DQE_CONSM HILFF (CDDR PHI)))))))
                    (SETQ PHI2 (CONS 'AND (CONS (LIST 'NEQ GHILF 0) PHI2)))
                    (SETQ ERG1 (DQE_ELIM PHI1 DIFFEQUALISTE VAR))
                    (SETQ ERG2 (DQE_ELIM PHI2 DIFFEQUALISTE VAR))
                    (SETQ ERG (DQE_ANDORVALEUR (LIST 'OR ERG1 ERG2)))
                    (COND
                     ((AND ZWERG (NOT (CDR ZWERG)))
                      (SETQ AUSG
                              (DQE_ANDORVALEUR (LIST 'AND ERG (CAR ZWERG)))))
                     (T
                      (SETQ AUSG
                              (DQE_ANDORVALEUR
                               (LIST 'AND ERG ZWERG))))))))))))))
      (RETURN AUSG))) 
(PUT 'DQE_EXQELIM 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_EXQELIM 'DEFINED-ON-LINE '2344) 
(PUT 'DQE_EXQELIM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_EXQELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_EXQELIM (PHI DIFFEQUALISTE VAR)
    (PROG (HILF AUSG K N TIMEVAR GCTIMEVAR ERG)
      (SETQ AUSG NIL)
      (SETQ N 0)
      (SETQ K 0)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "++nun wird ein existenzquantor eliminiert, also muss zuerst")
         (PRIN2T
          "++die formel in disjunktive normalform transformiert werden.")
         (PRIN2T "++die disjunktive normalform von ")
         (MATHPRINT PHI)
         (PRIN2T "++ist :")
         NIL)))
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_DISJNF PHI))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms."))))
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ AUSG PHI))
            ((EQUAL (CAR PHI) 'OR)
             (PROGN
              (SETQ PHI (CDR PHI))
              (COND
               (*DQEVERBOSE
                (PROGN
                 (SETQ N (LENGTH PHI))
                 (PRIN2 "++ die anzahl der konjunktionen ist ")
                 (PRIN2T N)
                 (SETQ ERG (DQE_ELIMBERECHNUNG PHI))
                 (PRIN2 "++die gesamte anzahl der atomaren formeln ist ")
                 (PRIN2T (CAR ERG))
                 (PRIN2 "++der ")
                 (PRIN2 (CADR ERG))
                 (PRIN2T "_te disjunktionsglied hat die hoechste")
                 (PRIN2 "++ anzahl von atomaren formeln und zwar ")
                 (PRIN2T (CADDR ERG))
                 NIL)))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ K (PLUS K 1))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (PRIN2 "++elimination des quantors ex ")
                    (PRIN2 VAR)
                    (PRIN2 " vor dem ")
                    (PRIN2 K)
                    (PRIN2T "-ten konjunktionsglied ")
                    (MATHPRINT HILF)
                    NIL)))
                 (SETQ TIMEVAR (TIME))
                 (SETQ GCTIMEVAR (GCTIME))
                 (SETQ HILF (DQE_ELIM HILF DIFFEQUALISTE VAR))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
                    (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
                    (PRIN2 "++die aequivalaente zum ")
                    (PRIN2 K)
                    (PRIN2T "-ten konjunktionsglied ist : ")
                    (MATHPRINT HILF)
                    (PRIN2 TIMEVAR)
                    (PRIN2 " ms plus ")
                    (PRIN2 GCTIMEVAR)
                    (PRIN2T " ms."))))
                 (SETQ AUSG (DQE_MODCONS HILF AUSG))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL))
              (COND ((EQUAL (LENGTH AUSG) 1) (SETQ AUSG (CAR AUSG)))
                    ((CDR AUSG) (SETQ AUSG (CONS 'OR AUSG))))))
            (T (SETQ AUSG (DQE_ELIM PHI DIFFEQUALISTE VAR))))
      (RETURN AUSG))) 
(PUT 'DQE_ALLQELIM 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ALLQELIM 'DEFINED-ON-LINE '2443) 
(PUT 'DQE_ALLQELIM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ALLQELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ALLQELIM (PHI DIFFEQUALISTE VAR)
    (PROG (HILF AUSGB K N TIMEVAR GCTIMEVAR ERG)
      (SETQ AUSGB NIL)
      (SETQ N 0)
      (SETQ K 0)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "++nun wird ein allquantor eliminiert, also muss zuerst ")
         (PRIN2T
          "++die formel in konjunktive normalform transformiert werden. ")
         (PRIN2T "++die konjunktive normalform von ")
         (MATHPRINT PHI)
         (PRIN2T "ist :")
         NIL)))
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_KONJNF PHI))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms."))))
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ AUSGB PHI))
            ((EQUAL (CAR PHI) 'AND)
             (PROGN
              (SETQ PHI (CDR PHI))
              (SETQ N (LENGTH PHI))
              (COND
               (*DQEVERBOSE
                (PROGN
                 (PRIN2 "++die anzahl der disjunktionen ist ")
                 (PRIN2T N)
                 (SETQ ERG (DQE_ELIMBERECHNUNG PHI))
                 (PRIN2 "++die gesamte anzahl der atomaren formeln ist ")
                 (PRIN2T (CAR ERG))
                 (PRIN2 "++der ")
                 (PRIN2 (CADR ERG))
                 (PRIN2T "_te disjunktionsglied hat die hoechste")
                 (PRIN2 " anzahl von atomaren formeln und zwar ")
                 (PRIN2T (CADDR ERG))
                 NIL)))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ K (PLUS K 1))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (PRIN2 "++elimination des quantors all ")
                    (PRIN2 VAR)
                    (PRIN2 " vor dem ")
                    (PRIN2 K)
                    (PRIN2T "-ten disjunktionsglied ")
                    (MATHPRINT HILF)
                    NIL)))
                 (SETQ TIMEVAR (TIME))
                 (SETQ GCTIMEVAR (GCTIME))
                 (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
                 (SETQ HILF (DQE_ELIM HILF DIFFEQUALISTE VAR))
                 (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
                    (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
                    (PRIN2 "++die aequivalaente zum ")
                    (PRIN2 K)
                    (PRIN2T "-ten disjunktionsglied ist : ")
                    (MATHPRINT HILF)
                    (PRIN2 TIMEVAR)
                    (PRIN2 " ms plus ")
                    (PRIN2 GCTIMEVAR)
                    (PRIN2T " ms."))))
                 (SETQ AUSGB (DQE_MODCONS HILF AUSGB))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL))
              (COND ((EQUAL (LENGTH AUSGB) 1) (SETQ AUSGB (CAR AUSGB)))
                    ((CDR AUSGB) (SETQ AUSGB (CONS 'AND AUSGB))))))
            (T
             (PROGN
              (SETQ PHI (DQE_MAKEPOSITIVE (LIST 'NOTT PHI)))
              (SETQ AUSGB (DQE_ELIM PHI DIFFEQUALISTE VAR))
              (SETQ AUSGB (DQE_MAKEPOSITIVE (LIST 'NOTT AUSGB))))))
      (RETURN AUSGB))) 
(PUT 'DQE_QUANTELIM 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_QUANTELIM 'DEFINED-ON-LINE '2546) 
(PUT 'DQE_QUANTELIM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_QUANTELIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_QUANTELIM (PHI DIFFEQUALISTE)
    (PROG (HILF LISTE VAR ERG QUANT N K TIMEVAR GCTIMEVAR)
      (SETQ LISTE NIL)
      (SETQ ERG NIL)
      (SETQ N 0)
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_MAKEPOSITIVE PHI))
      (COND
       ((NOT (DQE_ISPRENEXP PHI))
        (PROGN
         (COND ((NOT DIFFEQUALISTE) (SETQ PHI (DQE_MAKEPRENEX PHI)))
               (T
                (PROGN
                 (SETQ HILF (DQE_MAKEPRENEXMOD PHI DIFFEQUALISTE))
                 (SETQ PHI (CAR HILF))
                 (SETQ DIFFEQUALISTE (CADR HILF))))))))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (PRIN2T "+++die praenexe form der eingabeformel ist")
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms.")
         NIL)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (OR (EQUAL (CAR PHI) 'EX) (EQUAL (CAR PHI) 'ALL)))
          (RETURN NIL)))
        (PROGN
         (SETQ HILF (LIST (CAR PHI) (CADR PHI)))
         (SETQ LISTE (CONS HILF LISTE))
         (SETQ N (PLUS N 1))
         (SETQ PHI (CADDR PHI)))
        (GO WHILELABEL))
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "+++die matrix der eingabeformel ist")
         (MATHPRINT PHI)
         NIL)))
      (SETQ ERG PHI)
      (COND
       (*DQEVERBOSE
        (PROGN (PRIN2 "+++die anzahl der quantoren ist ") (MATHPRINT N) NIL)))
      (COND
       ((EQUAL N 0)
        (PROGN
         (COND
          (*DQEVERBOSE (PRIN2T "+++die eingabeformel ist quantorenfrei")))))
       ((EQUAL N 1)
        (PROGN
         (SETQ HILF (CAR LISTE))
         (SETQ LISTE (CDR LISTE))
         (SETQ QUANT (CAR HILF))
         (SETQ VAR (CADR HILF))
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2 "+++es gibt nur den quantor ")
            (PRIN2 QUANT)
            (PRIN2 ",")
            (PRIN2 VAR)
            (PRIN2T " zu eliminieren.")
            NIL)))
         (COND
          ((EQUAL QUANT 'EX) (SETQ ERG (DQE_EXQELIM ERG DIFFEQUALISTE VAR)))
          (T (SETQ ERG (DQE_ALLQELIM ERG DIFFEQUALISTE VAR))))))
       (T
        (PROGN
         (SETQ K 0)
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2 "es gibt ")
            (PRIN2 N)
            (PRIN2T " quantoren zu eliminieren.")
            NIL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT LISTE) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR LISTE))
            (SETQ LISTE (CDR LISTE))
            (SETQ QUANT (CAR HILF))
            (SETQ VAR (CADR HILF))
            (SETQ K (PLUS K 1))
            (COND
             (*DQEVERBOSE
              (PROGN
               (PRIN2 " elimination des ")
               (PRIN2 K)
               (PRIN2 "-ten quantors ")
               (PRIN2 QUANT)
               (PRIN2T VAR))))
            (SETQ TIMEVAR (TIME))
            (SETQ GCTIMEVAR (GCTIME))
            (COND
             ((EQUAL QUANT 'EX) (SETQ ERG (DQE_EXQELIM ERG DIFFEQUALISTE VAR)))
             (T (SETQ ERG (DQE_ALLQELIM ERG DIFFEQUALISTE VAR))))
            (COND
             (*DQEVERBOSE
              (PROGN
               (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
               (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
               (PRIN2 "nach der elimination des ")
               (PRIN2 K)
               (PRIN2T "-ten quantors")
               (PRIN2T "sieht die quantorenfreie formel, wie folgt, aus: ")
               (MATHPRINT ERG)
               (PRIN2 TIMEVAR)
               (PRIN2 " ms plus ")
               (PRIN2 GCTIMEVAR)
               (PRIN2T " ms.")
               NIL)))
            NIL)
           (GO WHILELABEL)))))
      (COND
       (*DQEVERBOSE
        (PRIN2T "+++die aequivalaente quantorenfreie formel ist+++: ")))
      (RETURN ERG))) 
(PUT 'DQE_ELIMOPT 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIMOPT 'DEFINED-ON-LINE '2676) 
(PUT 'DQE_ELIMOPT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT (PHI DIFFEQUALISTE VAR)
    (PROG (NF)
      (COND ((AND *DQEGRADORD *DQEVERBOSE) (PRIN2T "++++")))
      (SETQ NF (DQE_HELPELIM PHI))
      (COND ((OR (EQUAL NF T) (NOT NF)) (RETURN NF)))
      (COND ((NOT (CDR NF)) (RETURN (DQE_ELIMOPT-NEQ NF DIFFEQUALISTE VAR))))
      (COND
       ((AND (EQUAL (CAR NF) 1) (NOT (CDDR NF)))
        (RETURN (DQE_ELIMOPT-ONEEQ NF DIFFEQUALISTE VAR))))
      (RETURN (DQE_ELIMOPT-REGULAR NF DIFFEQUALISTE VAR)))) 
(PUT 'DQE_ELIMOPT-NEQ 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIMOPT-NEQ 'DEFINED-ON-LINE '2690) 
(PUT 'DQE_ELIMOPT-NEQ 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT-NEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT-NEQ (PHI DIFFEQUALISTE VAR)
    (PROG (RES PROD ORDG GRADG)
      (SETQ PROD (CAR PHI))
      (COND
       ((AND *DQEGRADORD *DQEVERBOSE)
        (PROGN
         (SETQ ORDG (DQE_ORD PROD VAR))
         (SETQ GRADG (DQE_GRAD PROD VAR))
         (PRIN2T "()")
         (PRIN2T (LIST ORDG GRADG))
         NIL)))
      (SETQ RES (DQE_NEQNULLFKT (DQE_TERMCOEFKT PROD VAR)))
      (SETQ RES (DQE_SIMPLIFY RES))
      (RETURN RES))) 
(PUT 'DQE_ELIMOPT-ONEEQ 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIMOPT-ONEEQ 'DEFINED-ON-LINE '2704) 
(PUT 'DQE_ELIMOPT-ONEEQ 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT-ONEEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT-ONEEQ (PHI DIFFEQUALISTE VAR)
    (PROG (EQU ORDF GRADF ERG RES)
      (SETQ EQU (CADR PHI))
      (COND
       ((AND *DQEGRADORD *DQEVERBOSE)
        (PROGN
         (SETQ ORDF (DQE_ORD EQU VAR))
         (SETQ GRADF (DQE_GRAD EQU VAR))
         (PRIN2T (LIST ORDF GRADF))
         (PRIN2T "()")
         NIL)))
      (SETQ ERG (DQE_TERMCOEFKT EQU VAR))
      (SETQ EQU (DQE_SIMPLIFY (LIST 'EQUAL (REVAL1 (CAR ERG) T) 0)))
      (COND ((EQUAL EQU T) (RETURN T)))
      (SETQ ERG (CDR ERG))
      (SETQ ERG (DQE_NEQNULLFKT ERG))
      (SETQ RES (DQE_ANDORVALEUR (LIST 'OR EQU ERG)))
      (SETQ RES (DQE_SIMPLIFY RES))
      (RETURN RES))) 
(PUT 'DQE_ELIMOPT-REGULAR 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ELIMOPT-REGULAR 'DEFINED-ON-LINE '2724) 
(PUT 'DQE_ELIMOPT-REGULAR 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT-REGULAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT-REGULAR (PHI DIFFEQUALISTE VAR)
    (PROG (G EQS)
      (SETQ G (CAR PHI))
      (SETQ EQS (CDR PHI))
      (COND ((AND (DQE_ISCONSTANT G) (NOT (EQUAL G 0))) (SETQ G 1)))
      (COND
       ((NOT (CDR EQS))
        (RETURN (DQE_ELIMOPT-REGULAR-ONEEQ G EQS DIFFEQUALISTE VAR))))
      (RETURN (DQE_ELIMOPT-REGULAR1 G EQS DIFFEQUALISTE VAR)))) 
(PUT 'DQE_ELIMOPT-REGULAR1 'NUMBER-OF-ARGS 4) 
(PUT 'DQE_ELIMOPT-REGULAR1 'DEFINED-ON-LINE '2735) 
(PUT 'DQE_ELIMOPT-REGULAR1 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT-REGULAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT-REGULAR1 (G EQS DIFFEQUALISTE VAR)
    (PROG (HILF ORDHILF ERG1 ERG2 AUSG ZWERG PHI1 HILFVAR PHI2 REST HILFF GHILF
           GRADG ORDF ORDG REDF INITF CONST ERG WEITER LISTE HELPLIST PHI)
      (SETQ ORDG (DQE_ORD G VAR))
      (SETQ GRADG (DQE_GRAD G VAR))
      (SETQ PHI (DQE_ELIMSIMPLIFY EQS ZWERG VAR))
      (SETQ ZWERG (CAR PHI))
      (SETQ PHI (CADR PHI))
      (COND ((NOT ZWERG) (SETQ WEITER T))
            (T
             (PROGN
              (COND ((NOT (CDR ZWERG)) (SETQ ZWERG (DQE_SIMPLIFY (CAR ZWERG))))
                    (T (SETQ ZWERG (DQE_SIMPLIFY ZWERG))))
              (COND ((EQUAL ZWERG NIL) (SETQ WEITER NIL))
                    (T (SETQ WEITER T))))))
      (COND ((EQUAL WEITER NIL) (SETQ AUSG NIL))
            (T
             (PROGN
              (COND
               ((NOT PHI)
                (PROGN
                 (COND
                  ((AND *DQEGRADORD *DQEVERBOSE)
                   (PROGN (PRIN2T "()") (PRIN2T (LIST ORDG GRADG)))))
                 (SETQ ERG (DQE_NEQNULLFKT (DQE_TERMCOEFKT G VAR)))
                 (COND ((EQUAL ZWERG T) (SETQ AUSG ERG))
                       (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG ZWERG)))))
                 (SETQ AUSG (DQE_SIMPLIFY AUSG))))
               ((NOT (CDR PHI))
                (PROGN
                 (SETQ PHI
                         (LIST 'AND (LIST 'NEQ G 0) (LIST 'EQUAL (CAR PHI) 0)))
                 (SETQ ERG (DQE_ELIMOPT PHI DIFFEQUALISTE VAR))
                 (COND ((EQUAL ZWERG T) (SETQ AUSG ERG))
                       ((EQUAL ERG T)
                        (PROGN
                         (COND ((NOT ZWERG) (SETQ AUSG T))
                               (T (SETQ AUSG ZWERG)))))
                       (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG ZWERG)))))
                 (SETQ AUSG (DQE_SIMPLIFY AUSG))))
               (T
                (PROGN
                 (SETQ PHI (DQE_LISTENORD PHI VAR))
                 (COND
                  ((AND *DQEGRADORD *DQEVERBOSE)
                   (PROGN
                    (SETQ LISTE PHI)
                    (SETQ HELPLIST NIL)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT LISTE) (RETURN NIL)))
                      (PROGN
                       (SETQ HILF (CAR LISTE))
                       (SETQ LISTE (CDR LISTE))
                       (SETQ HELPLIST
                               (CONS
                                (LIST (DQE_ORD HILF VAR) (DQE_GRAD HILF VAR))
                                HELPLIST)))
                      (GO WHILELABEL))
                    (PRIN2T HELPLIST)
                    (PRIN2T (LIST ORDG GRADG))
                    NIL)))
                 (SETQ HILFF (CAR PHI))
                 (SETQ INITF (DQE_INITIAL HILFF VAR))
                 (SETQ REDF (DQE_REDUKTUM HILFF VAR))
                 (SETQ ORDF (DQE_ORD HILFF VAR))
                 (COND
                  ((EQUAL REDF 0)
                   (SETQ PHI1 (DQE_EQUALNULLFKT (DQE_CONSM INITF (CDR PHI)))))
                  (T
                   (SETQ PHI1
                           (DQE_EQUALNULLFKT
                            (DQE_CONSM INITF (DQE_CONSM REDF (CDR PHI)))))))
                 (SETQ PHI1 (CONS 'AND (CONS (LIST 'NEQ G 0) PHI1)))
                 (COND ((DQE_ISCONSTANT INITF) (SETQ GHILF G))
                       (T (SETQ GHILF (REVAL1 (LIST 'TIMES INITF G) T))))
                 (SETQ HILF (CADR PHI))
                 (SETQ ORDHILF (DQE_ORD HILF VAR))
                 (COND ((EQUAL ORDHILF 0) (SETQ HILFVAR VAR))
                       (T (SETQ HILFVAR (LIST 'D VAR ORDHILF))))
                 (COND
                  ((EQUAL ORDHILF ORDF)
                   (SETQ REST (DQE_PSEUDREST HILF HILFF HILFVAR)))
                  (T
                   (PROGN
                    (SETQ CONST (REVAL1 (LIST 'DIFFERENCE ORDHILF ORDF) T))
                    (SETQ REST
                            (DQE_PSEUDREST HILF
                             (DQE_DIFF HILFF CONST DIFFEQUALISTE) HILFVAR)))))
                 (COND
                  ((EQUAL REST 0)
                   (SETQ PHI2 (DQE_EQUALNULLFKT (DQE_CONSM HILFF (CDDR PHI)))))
                  (T
                   (SETQ PHI2
                           (DQE_EQUALNULLFKT
                            (DQE_CONSM REST (DQE_CONSM HILFF (CDDR PHI)))))))
                 (SETQ PHI2 (CONS 'AND (CONS (LIST 'NEQ GHILF 0) PHI2)))
                 (SETQ ERG2 (DQE_ELIMOPT PHI2 DIFFEQUALISTE VAR))
                 (COND ((EQUAL ERG2 T) (SETQ ERG T))
                       (T
                        (PROGN
                         (SETQ ERG1 (DQE_ELIMOPT PHI1 DIFFEQUALISTE VAR))
                         (COND ((EQUAL ERG1 T) (SETQ ERG T))
                               (T
                                (SETQ ERG
                                        (DQE_ANDORVALEUR
                                         (LIST 'OR ERG1 ERG2))))))))
                 (COND ((EQUAL ZWERG T) (SETQ AUSG ERG))
                       ((EQUAL ERG T)
                        (PROGN
                         (COND ((NOT ZWERG) (SETQ AUSG T))
                               (T (SETQ AUSG ZWERG)))))
                       (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG ZWERG)))))
                 (SETQ AUSG (DQE_SIMPLIFY AUSG))))))))
      (RETURN AUSG))) 
(PUT 'DQE_ELIMOPT-REGULAR-ONEEQ 'NUMBER-OF-ARGS 4) 
(PUT 'DQE_ELIMOPT-REGULAR-ONEEQ 'DEFINED-ON-LINE '2856) 
(PUT 'DQE_ELIMOPT-REGULAR-ONEEQ 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMOPT-REGULAR-ONEEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ELIMOPT-REGULAR-ONEEQ (G EQS DIFFEQUALISTE VAR)
    (PROG (HILF ERG1 ERG2 AUSG PHI1 HILFVAR HILFF GHILF GRADF GRADG ORDF ORDG
           REDF INITF CONST ERG21 ERG22 PHI21 PHI22 REDHILF SEPF GGHILF)
      (SETQ ORDG (DQE_ORD G VAR))
      (SETQ GRADG (DQE_GRAD G VAR))
      (SETQ HILFF (CAR EQS))
      (SETQ GRADF (DQE_GRAD HILFF VAR))
      (SETQ ORDF (DQE_ORD HILFF VAR))
      (COND
       ((AND *DQEGRADORD *DQEVERBOSE)
        (PROGN (PRIN2T (LIST ORDF GRADF)) (PRIN2T (LIST ORDG GRADG)) NIL)))
      (COND
       ((EQUAL GRADF 0)
        (PROGN
         (SETQ ERG1 (DQE_SIMPLIFY (LIST 'EQUAL (REVAL1 HILFF T) 0)))
         (COND ((EQUAL ERG1 NIL) (SETQ AUSG NIL))
               (T
                (PROGN
                 (SETQ ERG2 (DQE_NEQNULLFKT (DQE_TERMCOEFKT G VAR)))
                 (COND ((EQUAL ERG1 T) (SETQ AUSG ERG2))
                       (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'AND ERG1 ERG2)))))
                 (SETQ AUSG (DQE_SIMPLIFY AUSG)))))))
       (T
        (PROGN
         (SETQ REDF (DQE_REDUKTUM HILFF VAR))
         (SETQ INITF (DQE_INITIAL HILFF VAR))
         (COND
          ((EQUAL REDF 0)
           (SETQ PHI1 (LIST 'AND (LIST 'NEQ G 0) (LIST 'EQUAL INITF 0))))
          (T
           (PROGN
            (SETQ PHI1 (DQE_EQUALNULLFKT (DQE_CONSM INITF (LIST REDF))))
            (SETQ PHI1 (CONS 'AND (CONS (LIST 'NEQ G 0) PHI1))))))
         (COND
          ((GREATERP ORDF ORDG)
           (PROGN
            (SETQ ERG21 (DQE_NEQNULLFKT (DQE_TERMCOEFKT G VAR)))
            (SETQ ERG22 (DQE_NEQNULLFKT (DQE_TERMCOEFKT INITF VAR)))
            (SETQ ERG2
                    (DQE_SIMPLIFY (DQE_ANDORVALEUR (LIST 'AND ERG21 ERG22))))))
          ((EQUAL ORDF ORDG)
           (PROGN
            (COND ((EQUAL ORDF 0) (SETQ HILFVAR VAR))
                  (T (SETQ HILFVAR (LIST 'D VAR ORDF))))
            (SETQ GHILF (DQE_PSEUDREST (LIST 'EXPT G GRADF) HILFF HILFVAR))
            (SETQ ERG21 (DQE_NEQNULLFKT (DQE_TERMCOEFKT GHILF VAR)))
            (SETQ ERG22 (DQE_NEQNULLFKT (DQE_TERMCOEFKT INITF VAR)))
            (SETQ ERG2
                    (DQE_SIMPLIFY (DQE_ANDORVALEUR (LIST 'AND ERG21 ERG22))))))
          (T
           (PROGN
            (SETQ CONST (REVAL1 (LIST 'DIFFERENCE ORDG ORDF) T))
            (SETQ HILF (DQE_DIFF HILFF CONST DIFFEQUALISTE))
            (SETQ HILFVAR (LIST 'D VAR ORDG))
            (SETQ GHILF (DQE_PSEUDREST G HILF HILFVAR))
            (COND
             ((NOT (DQE_ISCONSTANT INITF))
              (SETQ GHILF (REVAL1 (LIST 'TIMES INITF GHILF) T))))
            (SETQ PHI21 (LIST 'AND (LIST 'NEQ GHILF 0) (LIST 'EQUAL HILFF 0)))
            (SETQ ERG21 (DQE_ELIMOPT PHI21 DIFFEQUALISTE VAR))
            (COND ((EQUAL ERG21 T) (SETQ ERG2 ERG21))
                  (T
                   (PROGN
                    (COND ((DQE_ISCONSTANT INITF) (SETQ GGHILF G))
                          (T (SETQ GGHILF (REVAL1 (LIST 'TIMES INITF G) T))))
                    (SETQ SEPF (DQE_SEPARANTE HILFF VAR))
                    (SETQ REDHILF (DQE_REDUKTUM HILF VAR))
                    (SETQ PHI22
                            (DQE_CONSM (LIST 'EQUAL SEPF 0)
                             (DQE_CONSM (LIST 'EQUAL REDHILF 0)
                              (LIST (LIST 'EQUAL HILFF 0)))))
                    (SETQ PHI22 (CONS 'AND (CONS (LIST 'NEQ GGHILF 0) PHI22)))
                    (SETQ ERG22 (DQE_ELIMOPT PHI22 DIFFEQUALISTE VAR))
                    (SETQ ERG2 (DQE_ANDORVALEUR (LIST 'OR ERG21 ERG22)))))))))
         (COND ((EQUAL ERG2 T) (SETQ AUSG T))
               (T
                (PROGN
                 (SETQ ERG1 (DQE_ELIMOPT PHI1 DIFFEQUALISTE VAR))
                 (COND ((EQUAL ERG1 T) (SETQ AUSG T))
                       (T (SETQ AUSG (DQE_ANDORVALEUR (LIST 'OR ERG1 ERG2)))))
                 (SETQ AUSG (DQE_SIMPLIFY AUSG))))))))
      (RETURN AUSG))) 
(PUT 'DQE_EXQELIMOPT 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_EXQELIMOPT 'DEFINED-ON-LINE '2964) 
(PUT 'DQE_EXQELIMOPT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_EXQELIMOPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_EXQELIMOPT (PHI DIFFEQUALISTE VAR)
    (PROG (HILF ERG TESTVAR K N TIMEVAR GCTIMEVAR AUSG)
      (SETQ ERG NIL)
      (SETQ TESTVAR T)
      (SETQ N 0)
      (SETQ K 0)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T
          "++nun wird ein existenzquantor eliminiert, also muss zuerst ")
         (PRIN2T
          "++die formel in disjunktive normalform transformiert werden. ")
         (PRIN2T "++die disjunktive normalform von ")
         (MATHPRINT PHI)
         (PRIN2T " ist")
         NIL)))
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_DISJNF PHI))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms."))))
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ ERG PHI))
            ((EQUAL (CAR PHI) 'OR)
             (PROGN
              (SETQ PHI (CDR PHI))
              (SETQ TESTVAR T)
              (COND
               (*DQEVERBOSE
                (PROGN
                 (SETQ N (LENGTH PHI))
                 (PRIN2 "++die anzahl der konjunktionen ist ")
                 (PRIN2T N)
                 (SETQ AUSG (DQE_ELIMBERECHNUNG PHI))
                 (PRIN2 "++die gesamte anzahl der atomaren formeln ist ")
                 (PRIN2T (CAR AUSG))
                 (PRIN2 "++der ")
                 (PRIN2 (CADR AUSG))
                 (PRIN2T "_te disjunktionsglied hat die
            hoechste")
                 (PRIN2 " ++anzahl von atomaren formeln und zwar ")
                 (PRIN2T (CADDR AUSG))
                 NIL)))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND PHI TESTVAR)) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ K (PLUS K 1))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (PRIN2 "++elimination des quantors ex")
                    (PRIN2 ",")
                    (PRIN2 VAR)
                    (PRIN2 " vor dem ")
                    (PRIN2 K)
                    (PRIN2T "-ten konjunktionsglied ")
                    (MATHPRINT HILF)
                    NIL)))
                 (SETQ TIMEVAR (TIME))
                 (SETQ GCTIMEVAR (GCTIME))
                 (SETQ HILF (DQE_ELIMOPT HILF DIFFEQUALISTE VAR))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
                    (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
                    (PRIN2 "++ die aequivalaente zum ")
                    (PRIN2 K)
                    (PRIN2T "-ten konjunktionsglied ist :")
                    (MATHPRINT HILF)
                    (PRIN2 TIMEVAR)
                    (PRIN2 " ms plus ")
                    (PRIN2 GCTIMEVAR)
                    (PRIN2T " ms."))))
                 (COND ((EQUAL HILF T) (SETQ TESTVAR NIL))
                       (T (SETQ ERG (DQE_CONSM HILF ERG))))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL))
              (COND ((NOT TESTVAR) (SETQ ERG T))
                    ((EQUAL (LENGTH ERG) 1)
                     (SETQ ERG (DQE_SIMPLIFY (CAR ERG))))
                    ((CDR ERG)
                     (SETQ ERG (DQE_SIMPLIFY (CONS 'OR (REVERSE ERG))))))))
            (T (SETQ ERG (DQE_ELIMOPT PHI DIFFEQUALISTE VAR))))
      (RETURN ERG))) 
(PUT 'DQE_ALLQELIMOPT 'NUMBER-OF-ARGS 3) 
(PUT 'DQE_ALLQELIMOPT 'DEFINED-ON-LINE '3072) 
(PUT 'DQE_ALLQELIMOPT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ALLQELIMOPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DQE_ALLQELIMOPT (PHI DIFFEQUALISTE VAR)
    (PROG (HILF ERG TESTVAR K N TIMEVAR GCTIMEVAR AUSG)
      (SETQ ERG NIL)
      (SETQ TESTVAR T)
      (SETQ K 0)
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "++nun wird ein allquantor eliminiert, also muss zuerst ")
         (PRIN2T
          "++die formel in konjunktive normalform transformiert werden. ")
         (PRIN2T "++die konjunktive normalform von ")
         (MATHPRINT PHI)
         (PRIN2T "ist:"))))
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_KONJNF PHI))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms."))))
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ ERG PHI))
            ((EQUAL (CAR PHI) 'AND)
             (PROGN
              (SETQ PHI (CDR PHI))
              (SETQ K 0)
              (SETQ N (LENGTH PHI))
              (COND
               (*DQEVERBOSE
                (PROGN
                 (PRIN2 "++ die anzahl der disjunktionen ist ")
                 (PRIN2T N)
                 (SETQ AUSG (DQE_ELIMBERECHNUNG PHI))
                 (PRIN2 "++die gesamte anzahl der atomaren formeln ist ")
                 (PRIN2T (CAR AUSG))
                 (PRIN2 "++der ")
                 (PRIN2 (CADR AUSG))
                 (PRIN2T "_te disjunktionsglied hat die hoechste")
                 (PRIN2 " anzahl von atomaren formeln und zwar ")
                 (PRIN2T (CADDR AUSG))
                 NIL)))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND PHI TESTVAR)) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ K (PLUS K 1))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (PRIN2 "elimination des quantors all ")
                    (PRIN2 ",")
                    (PRIN2 VAR)
                    (PRIN2 " vor dem ")
                    (PRIN2 K)
                    (PRIN2T "-ten disjunktionsglied ")
                    (MATHPRINT HILF)
                    NIL)))
                 (SETQ TIMEVAR (TIME))
                 (SETQ GCTIMEVAR (GCTIME))
                 (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT (CAR PHI))))
                 (SETQ HILF (DQE_ELIMOPT HILF DIFFEQUALISTE VAR))
                 (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
                 (COND
                  (*DQEVERBOSE
                   (PROGN
                    (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
                    (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
                    (PRIN2 "++ die aequivalaente zum ")
                    (PRIN2 K)
                    (PRIN2T "-ten disjunktionsglied ist :")
                    (MATHPRINT HILF)
                    (PRIN2 TIMEVAR)
                    (PRIN2 " ms plus ")
                    (PRIN2 GCTIMEVAR)
                    (PRIN2T " ms."))))
                 (COND ((EQUAL HILF NIL) (SETQ TESTVAR NIL))
                       (T (SETQ ERG (DQE_CONSM HILF ERG))))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL))
              (COND ((NOT TESTVAR) (SETQ ERG NIL))
                    ((EQUAL (LENGTH ERG) 1)
                     (SETQ ERG (DQE_SIMPLIFY (CAR ERG))))
                    ((CDR ERG)
                     (SETQ ERG (DQE_SIMPLIFY (CONS 'AND (REVERSE ERG))))))))
            (T
             (PROGN
              (SETQ PHI (DQE_MAKEPOSITIVE (LIST 'NOTT PHI)))
              (SETQ ERG (DQE_ELIMOPT PHI DIFFEQUALISTE VAR))
              (SETQ ERG (DQE_MAKEPOSITIVE (LIST 'NOTT ERG))))))
      (RETURN ERG))) 
(PUT 'DQE_QUANTELIMOPT 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_QUANTELIMOPT 'DEFINED-ON-LINE '3181) 
(PUT 'DQE_QUANTELIMOPT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_QUANTELIMOPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_QUANTELIMOPT (PHI DIFFEQUALISTE)
    (PROG (HILF LISTE VAR AUSG QUANT WEITER K N TIMEVAR GCTIMEVAR)
      (SETQ WEITER T)
      (SETQ N 0)
      (SETQ LISTE NIL)
      (SETQ AUSG NIL)
      (SETQ TIMEVAR (TIME))
      (SETQ GCTIMEVAR (GCTIME))
      (SETQ PHI (DQE_MAKEPOSITIVE PHI))
      (COND
       ((NOT (DQE_ISPRENEXP PHI))
        (PROGN
         (COND ((NOT DIFFEQUALISTE) (SETQ PHI (DQE_MAKEPRENEX PHI)))
               (T
                (PROGN
                 (SETQ HILF (DQE_MAKEPRENEXMOD PHI DIFFEQUALISTE))
                 (SETQ PHI (CAR HILF))
                 (SETQ DIFFEQUALISTE (CADR HILF))))))))
      (COND
       (*DQEVERBOSE
        (PROGN
         (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
         (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
         (PRIN2T "+++die praenexe form der eingabeformel ist")
         (MATHPRINT PHI)
         (PRIN2 TIMEVAR)
         (PRIN2 " ms plus ")
         (PRIN2 GCTIMEVAR)
         (PRIN2T " ms.")
         NIL)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (OR (EQUAL (CAR PHI) 'EX) (EQUAL (CAR PHI) 'ALL)))
          (RETURN NIL)))
        (PROGN
         (SETQ HILF (LIST (CAR PHI) (CADR PHI)))
         (SETQ LISTE (CONS HILF LISTE))
         (SETQ N (PLUS N 1))
         (SETQ PHI (CADDR PHI)))
        (GO WHILELABEL))
      (COND
       (*DQEVERBOSE
        (PROGN
         (PRIN2T "+++die matrix der eingabeformel ist")
         (MATHPRINT PHI)
         NIL)))
      (SETQ AUSG PHI)
      (COND
       (*DQEVERBOSE
        (PROGN (PRIN2 "+++die anzahl der quantoren ist ") (MATHPRINT N))))
      (COND
       ((EQUAL N 0)
        (PROGN
         (COND
          (*DQEVERBOSE (PRIN2T "+++die eingabeformel ist quantorenfrei")))))
       ((EQUAL N 1)
        (PROGN
         (SETQ HILF (CAR LISTE))
         (SETQ LISTE (CDR LISTE))
         (SETQ QUANT (CAR HILF))
         (SETQ VAR (CADR HILF))
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2 "+++es gibt nur den quantor ")
            (PRIN2 QUANT)
            (PRIN2 ",")
            (PRIN2 VAR)
            (PRIN2T " zu eliminieren.")
            NIL)))
         (COND
          ((EQUAL QUANT 'EX)
           (SETQ AUSG (DQE_EXQELIMOPT AUSG DIFFEQUALISTE VAR)))
          (T (SETQ AUSG (DQE_ALLQELIMOPT AUSG DIFFEQUALISTE VAR))))
         NIL))
       (T
        (PROGN
         (SETQ K 0)
         (COND
          (*DQEVERBOSE
           (PROGN
            (PRIN2 "+++es gibt ")
            (PRIN2 N)
            (PRIN2T " quantoren zu eliminieren.")
            NIL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND LISTE WEITER)) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR LISTE))
            (SETQ LISTE (CDR LISTE))
            (SETQ QUANT (CAR HILF))
            (SETQ VAR (CADR HILF))
            (SETQ K (PLUS K 1))
            (COND
             (*DQEVERBOSE
              (PROGN
               (PRIN2 " elimination des ")
               (PRIN2 K)
               (PRIN2 "-ten quantors ")
               (PRIN2 QUANT)
               (PRIN2T VAR)
               NIL)))
            (SETQ TIMEVAR (TIME))
            (SETQ GCTIMEVAR (GCTIME))
            (COND
             ((EQUAL QUANT 'EX)
              (SETQ AUSG (DQE_EXQELIMOPT AUSG DIFFEQUALISTE VAR)))
             (T (SETQ AUSG (DQE_ALLQELIMOPT AUSG DIFFEQUALISTE VAR))))
            (COND
             (*DQEVERBOSE
              (PROGN
               (SETQ TIMEVAR (DIFFERENCE (TIME) TIMEVAR))
               (SETQ GCTIMEVAR (DIFFERENCE (GCTIME) GCTIMEVAR))
               (PRIN2 "+++nach der elimination des ")
               (PRIN2 K)
               (PRIN2T "-ten quantors")
               (PRIN2T "sieht die quantorenfreie formel, wie folgt, aus: ")
               (MATHPRINT AUSG)
               (PRIN2 TIMEVAR)
               (PRIN2 " ms plus ")
               (PRIN2 GCTIMEVAR)
               (PRIN2T " ms.")
               NIL)))
            (COND ((OR (EQUAL AUSG T) (NOT AUSG)) (SETQ WEITER NIL))))
           (GO WHILELABEL)))))
      (COND
       (*DQEVERBOSE
        (PRIN2T
         "+++die aequivalaente vereinfachte quantorenfreie formel ist: ")))
      (RETURN AUSG))) 
(PUT 'DQE_ELIMBERECHNUNG 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_ELIMBERECHNUNG 'DEFINED-ON-LINE '3323) 
(PUT 'DQE_ELIMBERECHNUNG 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_ELIMBERECHNUNG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_ELIMBERECHNUNG (PHI)
    (PROG (ERG ERG1 ERG2 ERG3 HILF K)
      (COND
       ((OR (EQUAL PHI T) (NOT PHI) (DQE_ISATOMARP PHI))
        (PROGN (SETQ ERG1 1) (SETQ ERG2 1) (SETQ ERG3 1)))
       (T
        (PROGN
         (SETQ ERG1 0)
         (SETQ ERG2 0)
         (SETQ ERG3 0)
         (SETQ K 0)
         (SETQ PHI (CDR PHI))
         (PROG ()
          WHILELABEL
           (COND ((NOT PHI) (RETURN NIL)))
           (PROGN
            (SETQ K (PLUS K 1))
            (SETQ HILF (CAR PHI))
            (SETQ PHI (CDR PHI))
            (SETQ HILF (DQE_ELIMBERECHNUNG HILF))
            (SETQ ERG1 (PLUS ERG1 (CAR HILF)))
            (COND
             ((GREATERP (CAR HILF) ERG3)
              (PROGN (SETQ ERG2 K) (SETQ ERG3 (CAR HILF))))))
           (GO WHILELABEL)))))
      (SETQ ERG (LIST ERG1 ERG2 ERG3))
      (RETURN ERG))) 
(PUT 'DQE_HELPSIMPLIFY 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_HELPSIMPLIFY 'DEFINED-ON-LINE '3368) 
(PUT 'DQE_HELPSIMPLIFY 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_HELPSIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_HELPSIMPLIFY (PHI)
    (PROG (AUSG HILF HILFPHI LISTE1 LISTE2 WEITER ALISTE KLISTE)
      (SETQ AUSG NIL)
      (COND ((OR (EQUAL PHI T) (NOT PHI) (DQE_ISATOMARP PHI)) (SETQ AUSG PHI))
            (T
             (PROGN
              (SETQ PHI (DQE_DISJNF PHI))
              (COND
               ((EQUAL (CAR PHI) 'AND)
                (PROGN
                 (SETQ HILFPHI (CDR PHI))
                 (SETQ LISTE1 NIL)
                 (SETQ LISTE2 NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT HILFPHI) (RETURN NIL)))
                   (PROGN
                    (SETQ HILF (CAR HILFPHI))
                    (SETQ HILFPHI (CDR HILFPHI))
                    (COND
                     ((EQUAL (CAR HILF) 'NEQ)
                      (SETQ LISTE1 (DQE_CONSM HILF LISTE1)))
                     (T (SETQ LISTE2 (DQE_CONSM HILF LISTE2)))))
                   (GO WHILELABEL))
                 (SETQ WEITER T)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND LISTE1 WEITER)) (RETURN NIL)))
                   (PROGN
                    (SETQ HILF (CAR LISTE1))
                    (SETQ LISTE1 (CDR LISTE1))
                    (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
                    (COND ((MEMBER HILF LISTE2) (SETQ WEITER NIL))))
                   (GO WHILELABEL))
                 (COND ((NOT WEITER) (SETQ AUSG NIL)) (T (SETQ AUSG PHI)))))
               (T
                (PROGN
                 (SETQ HILFPHI (CDR PHI))
                 (SETQ WEITER T)
                 (SETQ ALISTE NIL)
                 (SETQ KLISTE NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND HILFPHI WEITER)) (RETURN NIL)))
                   (PROGN
                    (SETQ HILF (CAR HILFPHI))
                    (SETQ HILFPHI (CDR HILFPHI))
                    (SETQ HILF (DQE_HELPSIMPLIFY HILF))
                    (COND ((EQUAL HILF T) (SETQ WEITER NIL))
                          ((DQE_ISATOMARP HILF)
                           (SETQ ALISTE (DQE_MODCONS HILF ALISTE)))
                          (HILF (SETQ KLISTE (DQE_MODCONS HILF KLISTE)))))
                   (GO WHILELABEL))
                 (COND
                  (KLISTE
                   (PROGN
                    (SETQ LISTE1 ALISTE)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT LISTE1) (RETURN NIL)))
                      (PROGN
                       (SETQ LISTE2 KLISTE)
                       (SETQ HILF (CAR LISTE1))
                       (SETQ LISTE1 (CDR LISTE1))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT LISTE2) (RETURN NIL)))
                         (PROGN
                          (COND
                           ((MEMBER HILF (CAR LISTE2))
                            (SETQ KLISTE (DQE_SANSELEM (CAR LISTE2) KLISTE))))
                          (SETQ LISTE2 (CDR LISTE2)))
                         (GO WHILELABEL)))
                      (GO WHILELABEL)))))
                 (COND ((NOT WEITER) (SETQ AUSG T))
                       (T
                        (PROGN
                         (SETQ HILFPHI ALISTE)
                         (COND
                          ((EQUAL (LENGTH ALISTE) 1)
                           (SETQ ALISTE (CAR ALISTE)))
                          (ALISTE (SETQ ALISTE (CONS 'OR ALISTE))))
                         (SETQ LISTE1 NIL)
                         (SETQ LISTE2 NIL)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT HILFPHI) (RETURN NIL)))
                           (PROGN
                            (SETQ HILF (CAR HILFPHI))
                            (SETQ HILFPHI (CDR HILFPHI))
                            (COND
                             ((EQUAL (CAR HILF) 'NEQ)
                              (SETQ LISTE1 (DQE_CONSM HILF LISTE1)))
                             (T (SETQ LISTE2 (DQE_CONSM HILF LISTE2)))))
                           (GO WHILELABEL))
                         (SETQ WEITER T)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT (AND LISTE1 WEITER)) (RETURN NIL)))
                           (PROGN
                            (SETQ HILF (CAR LISTE1))
                            (SETQ LISTE1 (CDR LISTE1))
                            (SETQ HILF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
                            (COND ((MEMBER HILF LISTE2) (SETQ WEITER NIL))))
                           (GO WHILELABEL))
                         (COND ((NOT WEITER) (SETQ AUSG T))
                               ((NOT KLISTE) (SETQ AUSG ALISTE))
                               ((NOT (CDR KLISTE))
                                (SETQ AUSG
                                        (DQE_ANDORVALEUR
                                         (LIST 'OR ALISTE (CAR KLISTE)))))
                               (T
                                (PROGN
                                 (SETQ HILFPHI (DQE_LOGSIMP KLISTE))
                                 (COND ((NOT HILFPHI) (SETQ AUSG ALISTE))
                                       ((DQE_ISATOMARP HILFPHI)
                                        (PROGN
                                         (COND
                                          ((NOT ALISTE) (SETQ AUSG HILFPHI))
                                          (T
                                           (PROGN
                                            (COND
                                             ((DQE_ISATOMARP ALISTE)
                                              (SETQ AUSG
                                                      (LIST 'OR ALISTE
                                                            HILFPHI)))
                                             (T
                                              (SETQ AUSG
                                                      (DQE_MODCONS HILFPHI
                                                       ALISTE))))
                                            (SETQ AUSG
                                                    (DQE_HELPSIMPLIFY
                                                     PHI)))))))
                                       ((EQUAL (CAR HILFPHI) 'AND)
                                        (SETQ AUSG
                                                (DQE_ANDORVALEUR
                                                 (LIST 'OR ALISTE HILFPHI))))
                                       (T
                                        (PROGN
                                         (SETQ AUSG
                                                 (DQE_ANDORVALEUR
                                                  (LIST 'OR ALISTE HILFPHI)))
                                         (COND
                                          ((NOT (EQUAL (CDR HILFPHI) KLISTE))
                                           (SETQ AUSG
                                                   (DQE_HELPSIMPLIFY
                                                    AUSG)))))))))))))))))))
      (RETURN AUSG))) 
(PUT 'DQE_LOGSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_LOGSIMP 'DEFINED-ON-LINE '3533) 
(PUT 'DQE_LOGSIMP 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_LOGSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_LOGSIMP (PHI)
    (PROG (KONJLIST ERG HILF ALISTE LISTE WEITER HILFPHI CONSTANT HILFF)
      (SETQ ERG NIL)
      (SETQ LISTE NIL)
      (SETQ HILFPHI PHI)
      (PROG ()
       WHILELABEL
        (COND ((NOT HILFPHI) (RETURN NIL)))
        (PROGN
         (SETQ KONJLIST (CDAR HILFPHI))
         (SETQ CONSTANT (CAR HILFPHI))
         (SETQ HILFPHI (CDR HILFPHI))
         (SETQ LISTE HILFPHI)
         (SETQ ALISTE NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT KONJLIST) (RETURN NIL)))
           (PROGN
            (SETQ HILF (CAR KONJLIST))
            (SETQ KONJLIST (CDR KONJLIST))
            (SETQ HILFF (DQE_MAKEPOSITIVE (LIST 'NOTT HILF)))
            (SETQ WEITER T)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND LISTE WEITER)) (RETURN NIL)))
              (PROGN
               (COND
                ((AND (MEMBER HILFF (CAR LISTE))
                      (DQE_LISTEQUAL (DQE_SANSELEM (CAR LISTE) HILFF)
                       (DQE_SANSELEM CONSTANT HILF)))
                 (SETQ WEITER NIL))
                (T (SETQ LISTE (CDR LISTE)))))
              (GO WHILELABEL))
            (COND (WEITER (SETQ ALISTE (DQE_CONSM HILF ALISTE)))
                  (T (SETQ HILFPHI (DQE_SANSELEM HILFPHI (CAR LISTE)))))
            (SETQ LISTE HILFPHI))
           (GO WHILELABEL))
         (COND
          ((EQUAL (LENGTH ALISTE) 1) (SETQ ERG (DQE_CONSM (CAR ALISTE) ERG)))
          (ALISTE (SETQ ERG (DQE_CONSM (CONS 'AND (REVERSE ALISTE)) ERG)))))
        (GO WHILELABEL))
      (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG)))
            (ERG (SETQ ERG (CONS 'OR (REVERSE ERG)))))
      (RETURN ERG))) 
(PUT 'DQE_LISTEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_LISTEQUAL 'DEFINED-ON-LINE '3585) 
(PUT 'DQE_LISTEQUAL 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_LISTEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_LISTEQUAL (PHI PSI)
    (PROG (AUSG WEITER)
      (SETQ AUSG NIL)
      (SETQ WEITER T)
      (COND ((NOT (EQUAL (LENGTH PHI) (LENGTH PSI))) (SETQ AUSG NIL))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND PHI WEITER)) (RETURN NIL)))
                (PROGN
                 (COND ((MEMBER (CAR PHI) PSI) (SETQ PHI (CDR PHI)))
                       (T (SETQ WEITER NIL))))
                (GO WHILELABEL))
              (COND (WEITER (SETQ AUSG T)) (T (SETQ AUSG NIL))))))
      (RETURN AUSG))) 
(PUT 'DQE_VORKOMMEN 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_VORKOMMEN 'DEFINED-ON-LINE '3611) 
(PUT 'DQE_VORKOMMEN 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_VORKOMMEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_VORKOMMEN (ELEM PHI)
    (PROG (ERG HILF)
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ ERG 0))
            ((OR (EQUAL (CAR PHI) 'NEQ) (EQUAL (CAR PHI) 'EQUAL))
             (PROGN
              (COND ((EQUAL (CADR PHI) ELEM) (SETQ ERG 1)) (T (SETQ ERG 0)))))
            (T
             (PROGN
              (SETQ PHI (CDR PHI))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (DQE_VORKOMMEN ELEM (CAR PHI)))
                 (SETQ ERG (PLUS ERG HILF))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL)))))
      (RETURN ERG))) 
(PUT 'DQE_LAENGEFKT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_LAENGEFKT 'DEFINED-ON-LINE '3638) 
(PUT 'DQE_LAENGEFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_LAENGEFKT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_LAENGEFKT (PHI)
    (PROG (ERG HILF)
      (SETQ ERG 0)
      (COND ((OR (EQUAL PHI T) (NOT PHI)) (SETQ ERG 0))
            ((OR (EQUAL (CAR PHI) 'EQUAL) (EQUAL (CAR PHI) 'NEQ)) (SETQ ERG 1))
            (T
             (PROGN
              (SETQ PHI (CDR PHI))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (DQE_LAENGEFKT (CAR PHI)))
                 (SETQ ERG (PLUS ERG HILF))
                 (SETQ PHI (CDR PHI)))
                (GO WHILELABEL)))))
      (RETURN ERG))) 
(PUT 'DQE_SPECNEQ 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_SPECNEQ 'DEFINED-ON-LINE '3671) 
(PUT 'DQE_SPECNEQ 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SPECNEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_SPECNEQ (PHI ELEM)
    (PROG (ERG)
      (SETQ ERG
              (DQE_SIMPLIFY
               (SUBST T (LIST 'NEQ ELEM 0)
                      (SUBST NIL (LIST 'EQUAL ELEM 0) PHI))))
      (RETURN ERG))) 
(PUT 'DQE_SPECEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_SPECEQUAL 'DEFINED-ON-LINE '3694) 
(PUT 'DQE_SPECEQUAL 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SPECEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_SPECEQUAL (PHI ELEM)
    (PROG (ERG)
      (SETQ ERG
              (DQE_SIMPLIFY
               (SUBST T (LIST 'EQUAL ELEM 0)
                      (SUBST NIL (LIST 'NEQ ELEM 0) PHI))))
      (RETURN ERG))) 
(PUT 'DQE_TABLEAU 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_TABLEAU 'DEFINED-ON-LINE '3713) 
(PUT 'DQE_TABLEAU 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_TABLEAU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_TABLEAU (PHI ELEM)
    (PROG (ERG)
      (SETQ ERG
              (DQE_SIMPLIFY
               (LIST 'OR
                     (LIST 'AND (LIST 'EQUAL ELEM 0) (DQE_SPECEQUAL PHI ELEM))
                     (LIST 'AND (LIST 'NEQ ELEM 0) (DQE_SPECNEQ PHI ELEM)))))
      (COND
       ((EQUAL ERG (LIST 'OR (LIST 'EQUAL ELEM 0) (LIST 'NEQ ELEM 0)))
        (SETQ ERG T)))
      (RETURN ERG))) 
(PUT 'DQE_LTABLEAU 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_LTABLEAU 'DEFINED-ON-LINE '3736) 
(PUT 'DQE_LTABLEAU 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_LTABLEAU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_LTABLEAU (PHI VARLISTE)
    (PROG (ERG ELEM)
      (SETQ ERG PHI)
      (PROG ()
       WHILELABEL
        (COND ((NOT VARLISTE) (RETURN NIL)))
        (PROGN
         (SETQ ELEM (CAR VARLISTE))
         (SETQ VARLISTE (CDR VARLISTE))
         (SETQ ERG (DQE_TABLEAU ERG ELEM)))
        (GO WHILELABEL))
      (RETURN ERG))) 
(PUT 'DQE_DKNFSIMPLIFY 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_DKNFSIMPLIFY 'DEFINED-ON-LINE '3762) 
(PUT 'DQE_DKNFSIMPLIFY 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_DKNFSIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_DKNFSIMPLIFY (PHI)
    (PROG (ERG HILF HILFF LISTE WEITER SYMB)
      (SETQ ERG NIL)
      (COND ((OR (EQUAL PHI T) (NOT PHI) (DQE_ISATOMARP PHI)) (SETQ ERG PHI))
            (T
             (PROGN
              (SETQ SYMB (CAR PHI))
              (SETQ PHI (CDR PHI))
              (PROG ()
               WHILELABEL
                (COND ((NOT PHI) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR PHI))
                 (SETQ PHI (CDR PHI))
                 (COND
                  ((OR (EQUAL HILF T) (NOT HILF) (DQE_ISATOMARP HILF))
                   (SETQ ERG (DQE_MODCONS HILF ERG)))
                  (T
                   (PROGN
                    (SETQ LISTE (LIST (CADR HILF)))
                    (SETQ HILFF (CDDR HILF))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT HILFF) (RETURN NIL)))
                      (PROGN
                       (SETQ LISTE (DQE_CONSM (CAR HILFF) LISTE))
                       (SETQ HILFF (CDR HILFF)))
                      (GO WHILELABEL))
                    (COND
                     ((EQUAL (LENGTH LISTE) 1)
                      (SETQ ERG (DQE_MODCONS (CAR LISTE) ERG)))
                     (T
                      (PROGN
                       (SETQ HILF (CONS (CAR HILF) (REVERSE LISTE)))
                       (COND ((NOT ERG) (SETQ ERG (LIST HILF)))
                             ((NOT (MEMBER HILF ERG))
                              (PROGN
                               (SETQ LISTE ERG)
                               (SETQ WEITER T)
                               (PROG ()
                                WHILELABEL
                                 (COND ((NOT (AND LISTE WEITER)) (RETURN NIL)))
                                 (PROGN
                                  (COND
                                   ((DQE_LISTEQUAL HILF (CAR LISTE))
                                    (SETQ WEITER NIL))
                                   (T (SETQ LISTE (CDR LISTE)))))
                                 (GO WHILELABEL))
                               (COND
                                (WEITER
                                 (SETQ ERG (DQE_MODCONS HILF ERG))))))))))))))
                (GO WHILELABEL))
              (COND ((EQUAL (LENGTH ERG) 1) (SETQ ERG (CAR ERG)))
                    ((CDR ERG) (SETQ ERG (CONS SYMB ERG)))))))
      (RETURN ERG))) 
(PUT 'DQE_PERMUTATIONFKT 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_PERMUTATIONFKT 'DEFINED-ON-LINE '3828) 
(PUT 'DQE_PERMUTATIONFKT 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PERMUTATIONFKT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_PERMUTATIONFKT (PHI)
    (PROG (ERGLISTE LISTE HILF HILFLISTE ERGHILF)
      (SETQ ERGLISTE NIL)
      (COND ((OR (NOT PHI) (EQUAL (LENGTH PHI) 1)) (SETQ ERGLISTE NIL))
            ((EQUAL (LENGTH PHI) 2) (SETQ ERGLISTE (LIST PHI (REVERSE PHI))))
            (T
             (PROGN
              (SETQ LISTE PHI)
              (PROG ()
               WHILELABEL
                (COND ((NOT LISTE) (RETURN NIL)))
                (PROGN
                 (SETQ HILF (CAR LISTE))
                 (SETQ LISTE (CDR LISTE))
                 (SETQ HILFLISTE (DQE_SANSELEM PHI HILF))
                 (SETQ HILFLISTE (DQE_PERMUTATIONFKT HILFLISTE))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT HILFLISTE) (RETURN NIL)))
                   (PROGN
                    (SETQ ERGHILF (CONS HILF (CAR HILFLISTE)))
                    (SETQ ERGLISTE (CONS ERGHILF ERGLISTE))
                    (SETQ HILFLISTE (CDR HILFLISTE)))
                   (GO WHILELABEL)))
                (GO WHILELABEL))
              (SETQ ERGLISTE (REVERSE ERGLISTE)))))
      (RETURN ERGLISTE))) 
(PUT 'DQE_SANSELEM 'NUMBER-OF-ARGS 2) 
(PUT 'DQE_SANSELEM 'DEFINED-ON-LINE '3870) 
(PUT 'DQE_SANSELEM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_SANSELEM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQE_SANSELEM (PHI ELEM)
    (PROG (HILF ERG)
      (SETQ ERG NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT PHI) (RETURN NIL)))
        (PROGN
         (SETQ HILF (CAR PHI))
         (SETQ PHI (CDR PHI))
         (COND ((NOT (EQUAL ELEM HILF)) (SETQ ERG (CONS HILF ERG)))))
        (GO WHILELABEL))
      (RETURN (REVERSE ERG)))) 
(PUT 'DQE_PFORM 'NUMBER-OF-ARGS 1) 
(PUT 'DQE_PFORM 'DEFINED-ON-LINE '3885) 
(PUT 'DQE_PFORM 'DEFINED-IN-FILE 'REDLOG/DCFSF/DCFSFKACEM.RED) 
(PUT 'DQE_PFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DQE_PFORM (F)
    (COND ((AND (LISTP F) (EQ (CAR F) '*SQ)) (PREPSQ (CADR F))) (T F))) 
(ENDMODULE) 
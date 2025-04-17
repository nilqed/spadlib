(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODAD1)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL '(ROWMIN ROWMAX KVARLST CODBEXL*)) 
(PUT 'GETCIND 'NUMBER-OF-ARGS 5) 
(PUT 'GETCIND 'DEFINED-ON-LINE '59) 
(PUT 'GETCIND 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'GETCIND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETCIND (VAR VARLST OP FA IV)
    (PROG (Y Z)
      (COND
       ((NULL (SETQ Y (GET VAR VARLST)))
        (PROGN
         (SETQ Y (SETQ ROWMIN (DIFFERENCE ROWMIN 1)))
         (PUT VAR VARLST Y)
         (SETROW Y OP VAR NIL NIL))))
      (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4
            (INSZZZN
             (SETQ Z
                     (COND ((OR (IDP IV) (CONSTP IV)) (CONS FA (CONS IV NIL)))
                           (T (CONS FA IV))))
             (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4)))
      (RETURN
       (COND ((OR (IDP (CDR Z)) (CONSTP (CDR Z))) (CONS Y (CONS (CDR Z) NIL)))
             (T (CONS Y (CDR Z))))))) 
(DE FIND+VAR (VAR FA IV) (GETCIND VAR 'VARLST+ 'PLUS FA IV)) 
(PUT 'FIND+VAR 'NUMBER-OF-ARGS 3) 
(PUT 'FIND+VAR 'DEFINED-ON-LINE '80) 
(PUT 'FIND+VAR 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'FIND+VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FIND+VAR 'INLINE
      '(LAMBDA (VAR FA IV) (GETCIND VAR 'VARLST+ 'PLUS FA IV))) 
(DE FIND*VAR (VAR FA IV) (GETCIND VAR 'VARLST* 'TIMES FA IV)) 
(PUT 'FIND*VAR 'NUMBER-OF-ARGS 3) 
(PUT 'FIND*VAR 'DEFINED-ON-LINE '83) 
(PUT 'FIND*VAR 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'FIND*VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FIND*VAR 'INLINE
      '(LAMBDA (VAR FA IV) (GETCIND VAR 'VARLST* 'TIMES FA IV))) 
(PUT 'IMPROVELAYOUT 'NUMBER-OF-ARGS 0) 
(PUT 'IMPROVELAYOUT 'DEFINED-ON-LINE '90) 
(PUT 'IMPROVELAYOUT 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'IMPROVELAYOUT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE IMPROVELAYOUT NIL
    (PROG (VAR B)
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (COND
         ((AND
           (NOT
            (OR (NUMBERP (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))
                (PAIRP VAR)
                (AND (MEMBER X CODBEXL*)
                     (OR (GET VAR 'NEX) (NOT (FLAGP VAR 'NEWSYM))
                         (GET VAR 'ALIAS)))))
           (TESTONONEEL VAR X))
          (SETQ B T)))
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (COND (B (IMPROVEKVARLST)))
      (RETURN B))) 
(PUT 'TESTONONEEL 'NUMBER-OF-ARGS 2) 
(PUT 'TESTONONEEL 'DEFINED-ON-LINE '126) 
(PUT 'TESTONONEEL 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TESTONONEEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTONONEEL (VAR X)
    (PROG (SCOL SROW EL SIGNIV SIGNEC ZZ ORDRX NEGCOF TROW OLDVAR B EL1 SCOF
           BOP+ LHS)
      (COND
       ((AND (SETQ ZZ (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)) (NULL (CDR ZZ))
             (NULL (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
             (|:ONEP| (DM-ABS (SETQ SIGNIV (CAR (CDR (SETQ EL (CAR ZZ)))))))
             (|:ONEP| (SETQ SIGNEC (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))))
        (PROGN
         (SETQ SCOL (CAR EL))
         (SETQ OLDVAR (GETV (GETV CODMAT (PLUS MAXVAR SCOL)) 3))
         (COND ((SETQ SROW (GET OLDVAR 'ROWINDEX)) (SETQ B T))
               ((AND (ASSOC OLDVAR KVARLST) (|:ONEP| SIGNIV) (|:ONEP| SIGNEC)
                     (NOT (MEMBER OLDVAR CODBEXL*)))
                (SETQ B T)))
         (COND
          (B
           (PROGN
            (PUTV (GETV CODMAT (PLUS MAXVAR SCOL)) 4
                  (DELYZZ X (GETV (GETV CODMAT (PLUS MAXVAR SCOL)) 4)))
            (TSHRINKCOL OLDVAR VAR 'VARLST+)
            (TSHRINKCOL OLDVAR VAR 'VARLST*)
            (COND
             ((OR
               (AND (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
                    (|:ONEP| (DM-MINUS SIGNIV)))
               (AND (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'TIMES)
                    (|:ONEP| (DM-MINUS SIGNEC))))
              (PROGN
               (SETQ VAR (LIST 'MINUS VAR))
               (SETQ KVARLST (SUBST VAR OLDVAR KVARLST))
               (SETQ PREPREFIXLIST (SUBST VAR OLDVAR PREPREFIXLIST))
               (SETQ VAR (CADR VAR))
               (SETQ NEGCOF (MINUS 1))))
             (T
              (PROGN
               (SETQ KVARLST (SUBST VAR OLDVAR KVARLST))
               (SETQ PREPREFIXLIST (SUBST VAR OLDVAR PREPREFIXLIST))
               (SETQ NEGCOF 1))))
            (COND
             ((SETQ LHS (GET OLDVAR 'INLHS))
              (PROGN
               (PUT LHS 'NEX (SUBST VAR OLDVAR (GET LHS 'NEX)))
               (REMPROP OLDVAR 'INLHS))))
            (COND
             ((SETQ LHS (GET OLDVAR 'INALIAS))
              (PROGN (UPDATEALIASES OLDVAR VAR) (REMPROP OLDVAR 'INALIAS))))
            (COND
             (SROW
              (PROGN
               (SETQ ORDRX (GETV (GETV CODMAT (PLUS MAXVAR X)) 8))
               (SETQ BOP+ (EQ (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 2) 'PLUS))
               (COND
                (BOP+ (SETQ SCOF (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 6)))
                (T
                 (SETQ SCOF
                         (DM-TIMES NEGCOF
                          (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 6)))))
               (SETROW X (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 2) VAR
                (LIST (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 5) SCOF)
                (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 4))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 8
                     (APPEND (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 8)
                             (REMORDR SROW ORDRX)))
               (COND
                ((|:ONEP| (DM-MINUS SIGNIV))
                 (PROGN
                  (PROG (Z)
                    (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR SCOL)) 4))
                   LAB
                    (COND ((NULL Z) (RETURN NIL)))
                    ((LAMBDA (Z) (RPLACA (CDR Z) (DM-MINUS (CAR (CDR Z)))))
                     (CAR Z))
                    (SETQ Z (CDR Z))
                    (GO LAB))
                  (PROG (CH)
                    (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
                   LAB
                    (COND ((NULL CH) (RETURN NIL)))
                    ((LAMBDA (CH)
                       (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 6
                             (DM-MINUS
                              (GETV (GETV CODMAT (PLUS MAXVAR CH)) 6))))
                     (CAR CH))
                    (SETQ CH (CDR CH))
                    (GO LAB))
                  (COND
                   ((SETQ TROW (GET VAR 'VARLST*))
                    (PROG (EL)
                      (SETQ EL (GETV (GETV CODMAT (PLUS MAXVAR TROW)) 4))
                     LAB
                      (COND ((NULL EL) (RETURN NIL)))
                      ((LAMBDA (EL)
                         (PUTV (GETV CODMAT (PLUS MAXVAR (CAR EL))) 6
                               (DM-MINUS
                                (GETV (GETV CODMAT (PLUS MAXVAR (CAR EL)))
                                      6))))
                       (CAR EL))
                      (SETQ EL (CDR EL))
                      (GO LAB))))
                  NIL)))
               (PROG (CH)
                 (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR SROW)) 5))
                LAB
                 (COND ((NULL CH) (RETURN NIL)))
                 ((LAMBDA (CH) (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 3 X))
                  (CAR CH))
                 (SETQ CH (CDR CH))
                 (GO LAB))
               (CLEARROW SROW)
               (PUTV (GETV CODMAT (PLUS MAXVAR SROW)) 8 NIL)
               (SETQ CODBEXL* (SUBST X SROW CODBEXL*))
               (PROG (Z)
                 (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
                LAB
                 (COND ((NULL Z) (RETURN NIL)))
                 ((LAMBDA (Z)
                    (PROGN
                     (COND
                      (BOP+ (RPLACA (CDR Z) (DM-TIMES SIGNIV (CAR (CDR Z))))))
                     (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                           (INSZZZ
                            (COND
                             ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                              (CONS X (CONS (CDR Z) NIL)))
                             (T (CONS X (CDR Z))))
                            (DELYZZ SROW
                             (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4))))))
                  (CAR Z))
                 (SETQ Z (CDR Z))
                 (GO LAB))
               (PROG (SINDEX)
                 (SETQ SINDEX 0)
                LAB
                 (COND ((MINUSP (DIFFERENCE ROWMAX SINDEX)) (RETURN NIL)))
                 (PUTV (GETV CODMAT (PLUS MAXVAR SINDEX)) 8
                       (SUBST X SROW
                              (GETV (GETV CODMAT (PLUS MAXVAR SINDEX)) 8)))
                 (SETQ SINDEX (PLUS2 SINDEX 1))
                 (GO LAB))
               (TESTONONEEL VAR X)))
             (T
              (PROGN
               (SETQ CODBEXL* (SUBST VAR X CODBEXL*))
               (SETQ ORDRX
                       (REMORDR OLDVAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 8)))
               (CLEARROW X)
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 8 NIL)
               (PROG (SINDEX)
                 (SETQ SINDEX 0)
                LAB
                 (COND ((MINUSP (DIFFERENCE ROWMAX SINDEX)) (RETURN NIL)))
                 (PUTV (GETV CODMAT (PLUS MAXVAR SINDEX)) 8
                       (UPDORDR (GETV (GETV CODMAT (PLUS MAXVAR SINDEX)) 8) VAR
                        OLDVAR ORDRX X))
                 (SETQ SINDEX (PLUS2 SINDEX 1))
                 (GO LAB))
               (IMPROVEKVARLST))))
            NIL))))))
      (RETURN B))) 
(PUT 'REMORDR 'NUMBER-OF-ARGS 2) 
(PUT 'REMORDR 'DEFINED-ON-LINE '267) 
(PUT 'REMORDR 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'REMORDR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMORDR (X OLST)
    (COND ((NULL OLST) OLST) ((EQUAL (CAR OLST) X) (REMORDR X (CDR OLST)))
          (T (CONS (CAR OLST) (REMORDR X (CDR OLST)))))) 
(PUT 'UPDORDR 'NUMBER-OF-ARGS 5) 
(PUT 'UPDORDR 'DEFINED-ON-LINE '283) 
(PUT 'UPDORDR 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'UPDORDR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPDORDR (OLST VAR OLDVAR ORDRX X)
    (COND ((NULL OLST) OLST)
          ((EQ (CAR OLST) OLDVAR)
           (CONS VAR (UPDORDR (CDR OLST) VAR OLDVAR ORDRX X)))
          ((EQUAL (CAR OLST) X)
           (APPEND (CONS VAR ORDRX) (UPDORDR (CDR OLST) VAR OLDVAR ORDRX X)))
          (T (CONS (CAR OLST) (UPDORDR (CDR OLST) VAR OLDVAR ORDRX X))))) 
(PUT 'IMPROVEKVARLST 'NUMBER-OF-ARGS 0) 
(PUT 'IMPROVEKVARLST 'DEFINED-ON-LINE '300) 
(PUT 'IMPROVEKVARLST 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'IMPROVEKVARLST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE IMPROVEKVARLST NIL
    (PROG (INVKVL NEWKVL X Y KV LKVL CD CD1)
      (SETQ NEWKVL KVARLST)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ LKVL (SETQ KVARLST NEWKVL))
         (SETQ INVKVL (SETQ NEWKVL NIL))
         (PROG ()
          WHILELABEL
           (COND ((NOT LKVL) (RETURN NIL)))
           (PROGN
            (SETQ KV (CAR LKVL))
            (SETQ LKVL (CDR LKVL))
            (SETQ CD1 (MEMBER (CAR KV) CODBEXL*))
            (SETQ X (ASSOC (CDR KV) INVKVL))
            (COND (X (SETQ CD (AND CD1 (MEMBER (CDR X) CODBEXL*)))))
            (COND
             ((AND X (NOT CD))
              (PROGN
               (SETQ KV (CAR KV))
               (SETQ X (CDR X))
               (COND (CD1 (PROGN (SETQ Y X) (SETQ X KV) (SETQ KV Y))))
               (TSHRINKCOL KV X 'VARLST+)
               (TSHRINKCOL KV X 'VARLST*)
               (PROG (RINDX)
                 (SETQ RINDX 0)
                LAB
                 (COND ((MINUSP (DIFFERENCE ROWMAX RINDX)) (RETURN NIL)))
                 (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 8
                       (SUBST X KV (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 8)))
                 (SETQ RINDX (PLUS2 RINDX 1))
                 (GO LAB))
               (SETQ NEWKVL (SUBST X KV NEWKVL))
               (SETQ INVKVL (SUBST X KV INVKVL))
               (SETQ LKVL (SUBST X KV LKVL))))
             (T
              (PROGN
               (SETQ INVKVL (CONS (CONS (CDR KV) (CAR KV)) INVKVL))
               (SETQ NEWKVL (CONS KV NEWKVL))))))
           (GO WHILELABEL)))
        (COND
         ((NOT (EQUAL (LENGTH KVARLST) (LENGTH NEWKVL))) (GO REPEATLABEL)))))) 
(PUT 'TSHRINKCOL 'NUMBER-OF-ARGS 3) 
(PUT 'TSHRINKCOL 'DEFINED-ON-LINE '349) 
(PUT 'TSHRINKCOL 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TSHRINKCOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TSHRINKCOL (OLDVAR VAR VARLST)
    (PROG (Y1 Y2)
      (COND ((GET OLDVAR 'INALIAS) (UPDATEALIASES OLDVAR VAR)))
      (COND
       ((SETQ Y1 (GET OLDVAR VARLST))
        (PROGN
         (COND
          ((SETQ Y2 (GET VAR VARLST))
           (PROGN
            (PROG (Z)
              (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR Y1)) 4))
             LAB
              (COND ((NULL Z) (RETURN NIL)))
              ((LAMBDA (Z)
                 (PROGN
                  (PUTV (GETV CODMAT (PLUS MAXVAR Y2)) 4
                        (INSZZZN Z (GETV (GETV CODMAT (PLUS MAXVAR Y2)) 4)))
                  (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                        (INSZZZR
                         (COND
                          ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                           (CONS Y2 (CONS (CDR Z) NIL)))
                          (T (CONS Y2 (CDR Z))))
                         (DELYZZ Y1
                          (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4))))))
               (CAR Z))
              (SETQ Z (CDR Z))
              (GO LAB))
            (CLEARROW Y1)))
          (T
           (PROGN
            (PUTV (GETV CODMAT (PLUS MAXVAR Y1)) 3 VAR)
            (PUT VAR VARLST Y1))))
         (REMPROP OLDVAR VARLST))))
      (REMPROP OLDVAR 'NPCDVAR)
      (REMPROP OLDVAR 'NVARLST))) 
(PUT 'UPDATEALIASES 'NUMBER-OF-ARGS 2) 
(PUT 'UPDATEALIASES 'DEFINED-ON-LINE '381) 
(PUT 'UPDATEALIASES 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'UPDATEALIASES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPDATEALIASES (OLD NEW)
    (PROG (ORIGINAL)
      (PUT NEW 'INALIAS (GET OLD 'INALIAS))
      (FLAG (LIST NEW) 'ALIASNEWSYM)
      (PROG (EL)
        (SETQ EL (GET OLD 'INALIAS))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (PUT EL 'ALIAS (SUBST NEW OLD (SETQ ORIGINAL (GET EL 'ALIAS))))
            (COND
             ((ATOM ORIGINAL)
              (PUT ORIGINAL 'FINALALIAS
                   (SUBST NEW OLD (GET ORIGINAL 'FINALALIAS))))
             (T
              (PUT (CAR ORIGINAL) 'FINALALIAS
                   (SUBST NEW OLD (GET (CAR ORIGINAL) 'FINALALIAS)))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB)))) 
(PUT 'TCHSCHEME 'NUMBER-OF-ARGS 0) 
(PUT 'TCHSCHEME 'DEFINED-ON-LINE '411) 
(PUT 'TCHSCHEME 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TCHSCHEME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TCHSCHEME NIL
    (PROG (ZZ B)
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (COND
         ((AND (NOT (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1)))
               (SETQ ZZ (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)) (NULL (CDR ZZ))
               (TRANSFERROW X (CAR (CDR (CAR ZZ)))))
          (PROGN (CHSCHEME X (CAR ZZ)) (SETQ B T))))
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (RETURN B))) 
(PUT 'CHSCHEME 'NUMBER-OF-ARGS 2) 
(PUT 'CHSCHEME 'DEFINED-ON-LINE '429) 
(PUT 'CHSCHEME 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'CHSCHEME 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHSCHEME (X Z)
    (PROG (FA OPV COF EXP)
      (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
            (DELYZZ X (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4)))
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 NIL)
      (COND
       ((EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
        (PROGN (SETQ EXP 1) (SETQ COF (CAR (CDR Z)))))
       (T (PROGN (SETQ EXP (CAR (CDR Z))) (SETQ COF 1))))
     L1
      (SETQ FA (GETV (GETV CODMAT (PLUS MAXVAR X)) 3))
      (SETQ OPV (GETV (GETV CODMAT (PLUS MAXVAR X)) 2))
      (COND
       ((EQ OPV 'PLUS)
        (PROGN
         (SETQ COF (DM-EXPT COF (GETV (GETV CODMAT (PLUS MAXVAR X)) 6)))
         (SETQ EXP (DM-TIMES (GETV (GETV CODMAT (PLUS MAXVAR X)) 6) EXP))
         (CHDEL FA X)
         (CLEARROW X)
         (COND
          ((AND (NULL (GETV (GETV CODMAT (PLUS MAXVAR FA)) 4))
                (TRANSFERROW FA EXP))
           (PROGN (SETQ X FA) (GO L1))))))
       (T
        (PROGN
         (COND
          ((EQ OPV 'TIMES)
           (PROGN
            (SETQ COF (DM-TIMES COF (GETV (GETV CODMAT (PLUS MAXVAR X)) 6)))
            (CHDEL FA X)
            (CLEARROW X)
            (COND
             ((AND (NULL (GETV (GETV CODMAT (PLUS MAXVAR FA)) 4))
                   (TRANSFERROW FA COF))
              (PROGN (SETQ X FA) (GO L1))))))))))
      (UPDFA FA EXP COF Z))) 
(PUT 'UPDFA 'NUMBER-OF-ARGS 4) 
(PUT 'UPDFA 'DEFINED-ON-LINE '465) 
(PUT 'UPDFA 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'UPDFA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPDFA (FA EXP COF Z)
    (COND
     ((EQ (GETV (GETV CODMAT (PLUS MAXVAR FA)) 2) 'PLUS)
      (PUTV (GETV CODMAT (PLUS MAXVAR FA)) 4
            (INSZZZR
             (GETCIND (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3) 'VARLST+
              'PLUS FA COF)
             (GETV (GETV CODMAT (PLUS MAXVAR FA)) 4))))
     (T
      (PROGN
       (PUTV (GETV CODMAT (PLUS MAXVAR FA)) 4
             (INSZZZR
              (GETCIND (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3) 'VARLST*
               'TIMES FA EXP)
              (GETV (GETV CODMAT (PLUS MAXVAR FA)) 4)))
       (PUTV (GETV CODMAT (PLUS MAXVAR FA)) 6
             (DM-TIMES COF (GETV (GETV CODMAT (PLUS MAXVAR FA)) 6))))))) 
(PUT 'TRANSFERROW 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFERROW 'DEFINED-ON-LINE '479) 
(PUT 'TRANSFERROW 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TRANSFERROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFERROW (X IV)
    (COND
     ((EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
      (AND (TRANSFERROW1 X)
           (EQ
            (GETV
             (GETV CODMAT (PLUS MAXVAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))
             2)
            'TIMES)))
     (T (AND (TRANSFERROW1 X) (TRANSFERROW2 X IV))))) 
(PUT 'TRANSFERROW1 'NUMBER-OF-ARGS 1) 
(PUT 'TRANSFERROW1 'DEFINED-ON-LINE '490) 
(PUT 'TRANSFERROW1 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TRANSFERROW1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRANSFERROW1 (X)
    (AND (NULL (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
         (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))) 
(PUT 'TRANSFERROW2 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFERROW2 'DEFINED-ON-LINE '498) 
(PUT 'TRANSFERROW2 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'TRANSFERROW2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFERROW2 (X IV)
    (PROG (FA B)
      (SETQ FA (GETV (GETV CODMAT (PLUS MAXVAR X)) 3))
      (CHDEL FA X)
      (SETQ B
              (AND (EQ (GETV (GETV CODMAT (PLUS MAXVAR FA)) 2) 'PLUS)
                   (OR (EQUAL IV 1)
                       (AND (NULL (GETV (GETV CODMAT (PLUS MAXVAR FA)) 4))
                            (TRANSFERROW FA
                             (TIMES IV
                                    (GETV (GETV CODMAT (PLUS MAXVAR FA))
                                          6)))))))
      (PUTV (GETV CODMAT (PLUS MAXVAR FA)) 5
            (CONS X (GETV (GETV CODMAT (PLUS MAXVAR FA)) 5)))
      (RETURN B))) 
(PUT 'CODFAC 'NUMBER-OF-ARGS 0) 
(PUT 'CODFAC 'DEFINED-ON-LINE '548) 
(PUT 'CODFAC 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'CODFAC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CODFAC NIL
    (PROG (B LXX)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND
           (NOT
            (OR (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3) (MINUS 1))
                (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3) (MINUS 2))))
           (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'TIMES)
           (SETQ LXX (SAMEFAR Y)))
          (PROGN
           (SETQ B T)
           (PROG (EL)
             (SETQ EL LXX)
            LAB
             (COND ((NULL EL) (RETURN NIL)))
             ((LAMBDA (EL) (COMMONFAC Y EL)) (CAR EL))
             (SETQ EL (CDR EL))
             (GO LAB)))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (RETURN B))) 
(PUT 'SAMEFAR 'NUMBER-OF-ARGS 1) 
(PUT 'SAMEFAR 'DEFINED-ON-LINE '579) 
(PUT 'SAMEFAR 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'SAMEFAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SAMEFAR (Y)
    (PROG (FLST S FAR)
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((AND
              (NUMBERP (SETQ FAR (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3)))
              (EQ (GETV (GETV CODMAT (PLUS MAXVAR FAR)) 2) 'PLUS))
             (COND ((SETQ S (ASSOC FAR FLST)) (RPLACD S (INSZZZ Z (CDR S))))
                   (T (SETQ FLST (CONS (CONS FAR (INSZZZ Z S)) FLST)))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN
       (PROG (EL FORALL-RESULT FORALL-ENDPTR)
         (SETQ EL FLST)
        STARTOVER
         (COND ((NULL EL) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (EL) (COND ((CDDR EL) (LIST EL)) (T NIL))) (CAR EL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ EL (CDR EL))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL EL) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (EL) (COND ((CDDR EL) (LIST EL)) (T NIL))) (CAR EL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ EL (CDR EL))
         (GO LOOPLABEL))))) 
(PUT 'COMMONFAC 'NUMBER-OF-ARGS 2) 
(PUT 'COMMONFAC 'DEFINED-ON-LINE '602) 
(PUT 'COMMONFAC 'DEFINED-IN-FILE 'SCOPE/CODAD1.RED) 
(PUT 'COMMONFAC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMMONFAC (Y XX)
    (PROG (FAR CH1 CH2 CH4 CHINDEX ZEL ZELI ZZ2 ZZ3 ZZ4 NSUM NPROD OPV Y1 COF X
           IVALX)
      (SETQ FAR (CAR XX))
      (SETQ CH1 (GETV (GETV CODMAT (PLUS MAXVAR FAR)) 5))
      (SETQ ZZ3 (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
      (SETQ NPROD (PLUS ROWMAX 1))
      (SETQ NSUM (SETQ ROWMAX (PLUS ROWMAX 2)))
      (PROG (ITEM)
        (SETQ ITEM (CDR XX))
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (SETQ X (CAR ITEM))
            (COND
             ((EQUAL (SETQ IVALX (CAR (CDR ITEM))) 1)
              (SETQ ZZ4 (DELYZZ Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))))
             (T
              (SETQ ZZ4
                      (INSZZZR
                       (SETQ ZELI
                               ((LAMBDA (G157)
                                  (COND
                                   ((OR (IDP G157) (CONSTP G157))
                                    (CONS Y (CONS G157 NIL)))
                                   (T (CONS Y G157))))
                                (DIFFERENCE IVALX 1)))
                       (DELYZZ Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))))))
            (SETQ CH4 (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
            (SETQ COF (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))
            (COND
             ((AND (NULL ZZ4) (NULL (CDR CH4)) (CAR CH4))
              (PROGN
               (COND
                ((AND
                  (EQ
                   (SETQ OPV
                           (GETV
                            (GETV CODMAT (PLUS MAXVAR (SETQ CH4 (CAR CH4))))
                            2))
                   'PLUS)
                  (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR CH4)) 6) 1))
                 (PROGN
                  (PROG (Z)
                    (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR CH4)) 4))
                   LAB
                    (COND ((NULL Z) (RETURN NIL)))
                    ((LAMBDA (Z)
                       (PROGN
                        (SETQ ZEL
                                ((LAMBDA (G159)
                                   (COND
                                    ((OR (IDP G159) (CONSTP G159))
                                     (CONS (CAR Z) (CONS G159 NIL)))
                                    (T (CONS (CAR Z) G159))))
                                 (DM-TIMES (CAR (CDR Z)) COF)))
                        (SETQ ZZ2 (INSZZZR ZEL ZZ2))
                        (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                              (INSZZZ
                               ((LAMBDA (G161)
                                  (COND
                                   ((OR (IDP G161) (CONSTP G161))
                                    (CONS NSUM (CONS G161 NIL)))
                                   (T (CONS NSUM G161))))
                                (CAR (CDR ZEL)))
                               (DELYZZ CH4
                                (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z)))
                                      4))))))
                     (CAR Z))
                    (SETQ Z (CDR Z))
                    (GO LAB))
                  (PROG (CH)
                    (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR CH4)) 5))
                   LAB
                    (COND ((NULL CH) (RETURN NIL)))
                    ((LAMBDA (CH)
                       (PROGN
                        (SETQ CHINDEX CH)
                        (COND
                         ((NOT (|:ONEP| COF))
                          (COND
                           ((EQ (GETV (GETV CODMAT (PLUS MAXVAR CH)) 2) 'TIMES)
                            (PROGN
                             (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 6
                                   (DM-TIMES COF
                                    (GETV (GETV CODMAT (PLUS MAXVAR CH)) 6)))
                             (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 3 NSUM)))
                           (T
                            (PROGN
                             (SETQ CHINDEX (SETQ ROWMAX (PLUS ROWMAX 1)))
                             (SETROW CHINDEX 'TIMES NSUM (CONS CH COF) NIL)))))
                         (T (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 3 NSUM)))
                        (SETQ CH2 (CONS CHINDEX CH2))))
                     (CAR CH))
                    (SETQ CH (CDR CH))
                    (GO LAB))
                  (CLEARROW CH4)
                  NIL))
                (T
                 (PROGN
                  (PUTV (GETV CODMAT (PLUS MAXVAR CH4)) 3 NSUM)
                  (SETQ CH2 (CONS CH4 CH2)))))
               (CLEARROW X)))
             ((AND (NULL CH4) (NULL (CDR ZZ4)) (CAR ZZ4))
              (PROGN
               (COND
                ((NOT (|:ONEP| (CAR (CDR (CAR ZZ4)))))
                 (PROGN
                  (PUTV (GETV CODMAT (PLUS MAXVAR X)) 3 NSUM)
                  (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 ZZ4)
                  (SETQ CH2 (CONS X CH2))))
                (T
                 (PROGN
                  (SETQ ZZ2
                          (INSZZZR
                           (GETCIND
                            (GETV
                             (GETV CODMAT
                                   (PLUS MAXVAR (SETQ Y1 (CAR (CAR ZZ4)))))
                             3)
                            'VARLST+ 'PLUS NSUM COF)
                           ZZ2))
                  (PUTV (GETV CODMAT (PLUS MAXVAR Y1)) 4
                        (DELYZZ X (GETV (GETV CODMAT (PLUS MAXVAR Y1)) 4)))
                  (CLEARROW X))))))
             (T
              (PROGN
               (SETQ CH2 (CONS X CH2))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 3 NSUM)
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 ZZ4))))
            (SETQ CH1 (DELETE X CH1))
            (SETQ ZZ3 (DELYZZ X ZZ3))
            (COND
             ((GREATERP IVALX 2)
              (SETQ ZZ3
                      (INSZZZ
                       (COND
                        ((OR (IDP (CDR ZELI)) (CONSTP (CDR ZELI)))
                         (CONS X (CONS (CDR ZELI) NIL)))
                        (T (CONS X (CDR ZELI))))
                       ZZ3))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4
            (CONS
             (SETQ ZEL
                     (COND ((OR (IDP 1) (CONSTP 1)) (CONS NPROD (CONS 1 NIL)))
                           (T (CONS NPROD 1))))
             ZZ3))
      (PUTV (GETV CODMAT (PLUS MAXVAR FAR)) 5 (CONS NPROD CH1))
      (PROG (Z)
        (SETQ Z ZZ2)
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((ZEROPP (CAR (CDR Z)))
             (PROGN
              (SETQ ZZ2 (DELYZZ (SETQ Y1 (CAR Z)) ZZ2))
              (PUTV (GETV CODMAT (PLUS MAXVAR Y1)) 4
                    (DELYZZ NSUM (GETV (GETV CODMAT (PLUS MAXVAR Y1)) 4)))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (SETROW NPROD 'TIMES FAR (LIST (LIST NSUM))
       (LIST
        (COND
         ((OR (IDP (CDR ZEL)) (CONSTP (CDR ZEL)))
          (CONS Y (CONS (CDR ZEL) NIL)))
         (T (CONS Y (CDR ZEL))))))
      (SETROW NSUM 'PLUS NPROD (LIST CH2) ZZ2))) 
(ENDMODULE) 
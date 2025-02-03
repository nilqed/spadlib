(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GHORNER)) 
(GLOBAL '(*ALGPRI AUTOHORN)) 
(SWITCH (LIST 'ALGPRI)) 
(SETQ *ALGPRI T) 
(PUT 'GHORNER 'STAT 'GHORNERSTAT) 
(PUT 'GHORNERSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GHORNERSTAT 'DEFINED-ON-LINE '85) 
(PUT 'GHORNERSTAT 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'GHORNERSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GHORNERSTAT NIL
    (PROG (X Y)
      (FLAG '(VORDER) 'DELIM)
      (FLAG '(*RSQB) 'DELIM)
      (COND ((EQUAL (CAR (SETQ X (XREAD T))) 'PROGN) (SETQ X (CDR X)))
            (T (SETQ X (LIST X))))
      (COND
       ((NOT (EQ CURSYM* 'VORDER))
        (COND ((EQ CURSYM* '*SEMICOL*) (SETQ AUTOHORN T))
              (T (SYMERR 'GHORNER T))))
       (T (PROGN (SETQ AUTOHORN NIL) (SETQ Y (REMCOMMA (XREAD NIL))))))
      (REMFLAG '(VORDER) 'DELIM)
      (REMFLAG '(*RSQB) 'DELIM)
      (RETURN (LIST 'GHORNER X Y)))) 
(PUT 'GHORNER 'FORMFN 'FORMGHORNER) 
(PUT 'FORMGHORNER 'NUMBER-OF-ARGS 3) 
(PUT 'FORMGHORNER 'DEFINED-ON-LINE '110) 
(PUT 'FORMGHORNER 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'FORMGHORNER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMGHORNER (U VARS MODE)
    (LIST 'GHORNER (MKQUOTE (CADR U)) (MKQUOTE (CADDR U)))) 
(PUT 'GHORNER 'NUMBER-OF-ARGS 2) 
(PUT 'GHORNER 'DEFINED-ON-LINE '113) 
(PUT 'GHORNER 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'GHORNER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GHORNER (ASSSET VARLIST)
    (PROG (H HEXP RES)
      (SETQ HEXP *EXP)
      (SETQ *EXP NIL)
      (SETQ RES
              (PROG (ASS FORALL-RESULT FORALL-ENDPTR)
                (SETQ ASS ASSSET)
                (COND ((NULL ASS) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ASS)
                                    (COND
                                     ((NOT (EQCAR ASS 'SETQ))
                                      (REDERR "Assignment statement expected"))
                                     (T
                                      (PROGN
                                       (SETQ H
                                               (INITHORNER (CADDR ASS)
                                                VARLIST))
                                       (COND
                                        (*ALGPRI
                                         (PROGN
                                          (COND
                                           ((EQCAR H 'QUOTIENT)
                                            (PUT (CADR ASS) 'AVALUE
                                                 (LIST 'SCALAR
                                                       (MK*SQ
                                                        (CONS
                                                         (CAR
                                                          (CONS
                                                           (*Q2F
                                                            (SIMP* (CADR H)))
                                                           1))
                                                         (CAR
                                                          (CONS
                                                           (*Q2F
                                                            (SIMP* (CADDR H)))
                                                           1)))))))
                                           (T
                                            (PUT (CADR ASS) 'AVALUE
                                                 (LIST 'SCALAR
                                                       (MK*SQ
                                                        (CONS (*Q2F (SIMP* H))
                                                              1))))))
                                          (ASSGNPRI H (LIST (CADR ASS)) T)
                                          (TERPRI)))
                                        (T (LIST (CAR ASS) (CADR ASS) H)))))))
                                  (CAR ASS))
                                 NIL)))
               LOOPLABEL
                (SETQ ASS (CDR ASS))
                (COND ((NULL ASS) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ASS)
                            (COND
                             ((NOT (EQCAR ASS 'SETQ))
                              (REDERR "Assignment statement expected"))
                             (T
                              (PROGN
                               (SETQ H (INITHORNER (CADDR ASS) VARLIST))
                               (COND
                                (*ALGPRI
                                 (PROGN
                                  (COND
                                   ((EQCAR H 'QUOTIENT)
                                    (PUT (CADR ASS) 'AVALUE
                                         (LIST 'SCALAR
                                               (MK*SQ
                                                (CONS
                                                 (CAR
                                                  (CONS (*Q2F (SIMP* (CADR H)))
                                                        1))
                                                 (CAR
                                                  (CONS
                                                   (*Q2F (SIMP* (CADDR H)))
                                                   1)))))))
                                   (T
                                    (PUT (CADR ASS) 'AVALUE
                                         (LIST 'SCALAR
                                               (MK*SQ
                                                (CONS (*Q2F (SIMP* H)) 1))))))
                                  (ASSGNPRI H (LIST (CADR ASS)) T)
                                  (TERPRI)))
                                (T (LIST (CAR ASS) (CADR ASS) H)))))))
                          (CAR ASS))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ AUTOHORN NIL)
      (SETQ *EXP HEXP)
      (COND ((NOT *ALGPRI) (RETURN RES))))) 
(PUT 'INITHORNER 'NUMBER-OF-ARGS 2) 
(PUT 'INITHORNER 'DEFINED-ON-LINE '161) 
(PUT 'INITHORNER 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'INITHORNER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INITHORNER (P VARLIST)
    (PROG (N D HMCD RES)
      (SETQ HMCD *MCD)
      (SETQ *MCD T)
      (SETQ P (REVAL1 P T))
      (SETQ RES (HORNERSUMS P VARLIST))
      (SETQ *MCD HMCD)
      (RETURN RES))) 
(PUT 'HORNERSUMS 'NUMBER-OF-ARGS 2) 
(PUT 'HORNERSUMS 'DEFINED-ON-LINE '177) 
(PUT 'HORNERSUMS 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNERSUMS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNERSUMS (P VARLIST)
    (COND ((OR (ATOM P) (DOMPROP P)) P) ((EQCAR P 'PLUS) (HORNER P VARLIST))
          (T
           (APPEND (LIST (CAR P))
                   (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                     (SETQ ELT (CDR P))
                     (COND ((NULL ELT) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (ELT) (HORNERSUMS ELT VARLIST))
                                       (CAR ELT))
                                      NIL)))
                    LOOPLABEL
                     (SETQ ELT (CDR ELT))
                     (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (ELT) (HORNERSUMS ELT VARLIST))
                               (CAR ELT))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))) 
(PUT 'HORNER 'NUMBER-OF-ARGS 2) 
(PUT 'HORNER 'DEFINED-ON-LINE '186) 
(PUT 'HORNER 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNER (P VARLIST)
    (PROG (HEXP TREE VAR)
      (SETQ HEXP *EXP)
      (SETQ *EXP T)
      (SETQ P (REVAL1 P T))
      (SETQ TREE '(NIL NIL NIL))
      (SETQ VAR (COND (VARLIST (CAR VARLIST)) (AUTOHORN (MAINVAR2 P)) (T NIL)))
      (COND
       (VAR
        (PROGN
         (PROG (KTERM)
           (SETQ KTERM (CDR P))
          LAB
           (COND ((NULL KTERM) (RETURN NIL)))
           ((LAMBDA (KTERM)
              (SETQ TREE (PUTTREE TREE (ORDERTERM KTERM VAR) VAR)))
            (CAR KTERM))
           (SETQ KTERM (CDR KTERM))
           (GO LAB))
         (SETQ P (GATHERTREE TREE (CONS VAR (CDR VARLIST))))
         (SETQ P (SCHEMA P VAR (KPOW (CAR P) VAR))))))
      (SETQ *EXP HEXP)
      (RETURN P))) 
(PUT 'HORNERCOEF 'NUMBER-OF-ARGS 2) 
(PUT 'HORNERCOEF 'DEFINED-ON-LINE '216) 
(PUT 'HORNERCOEF 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNERCOEF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNERCOEF (TERM VARLIST)
    (PROG (N COF)
      (RETURN
       (COND
        ((NULL (SETQ COF (KCOF TERM (SETQ N (KPOW TERM (CAR VARLIST)))))) NIL)
        ((ATOM COF) TERM) ((EQUAL N 0) (HORNERSUMS COF (CDR VARLIST)))
        (T (LIST (CAR TERM) (HORNERSUMS COF (CDR VARLIST)) (CADDR TERM))))))) 
(PUT 'PUTTREE 'NUMBER-OF-ARGS 3) 
(PUT 'PUTTREE 'DEFINED-ON-LINE '238) 
(PUT 'PUTTREE 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'PUTTREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PUTTREE (TREE TERM VAR)
    (PROG (C N M)
      (RETURN
       (COND ((OR (NULL TREE) (NULL (CAR TREE))) (LIST TERM NIL NIL))
             ((LESSP (SETQ N (KPOW TERM VAR)) (SETQ M (KPOW (CAR TREE) VAR)))
              (LIST (CAR TREE) (PUTTREE (CADR TREE) TERM VAR) (CADDR TREE)))
             ((GREATERP N M)
              (LIST (CAR TREE) (CADR TREE) (PUTTREE (CADDR TREE) TERM VAR)))
             (T
              (PROGN
               (SETQ C (KCOF (CAR TREE) N))
               (COND ((AND (PAIRP C) (EQUAL (CAR C) 'PLUS)) (SETQ C (CDR C)))
                     (T (SETQ C (LIST C))))
               (COND
                ((EQUAL N 0)
                 (LIST (APPEND '(PLUS) (APPEND (LIST (KCOF TERM N)) C))
                       (CADR TREE) (CADDR TREE)))
                (T
                 (LIST
                  (LIST 'TIMES (APPEND '(PLUS) (APPEND (LIST (KCOF TERM N)) C))
                        (COND ((EQUAL (CAR C) 1) (CAR TREE))
                              (T (CADDAR TREE))))
                  (CADR TREE) (CADDR TREE)))))))))) 
(PUT 'GATHERTREE 'NUMBER-OF-ARGS 2) 
(PUT 'GATHERTREE 'DEFINED-ON-LINE '287) 
(PUT 'GATHERTREE 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'GATHERTREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GATHERTREE (TREE VARLIST)
    (PROG ()
      (RETURN
       (COND ((NULL TREE) NIL)
             (T
              (APPEND (GATHERTREE (CADDR TREE) VARLIST)
                      (APPEND (LIST (HORNERCOEF (CAR TREE) VARLIST))
                              (GATHERTREE (CADR TREE) VARLIST)))))))) 
(PUT 'ORDERTERM 'NUMBER-OF-ARGS 2) 
(PUT 'ORDERTERM 'DEFINED-ON-LINE '306) 
(PUT 'ORDERTERM 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'ORDERTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDERTERM (TT VAR)
    (PROG (H RES FACTR MIN)
      (SETQ MIN NIL)
      (COND ((EQUAL TT VAR) (SETQ RES TT))
            (T
             (PROGN
              (COND
               ((EQCAR TT 'MINUS) (PROGN (SETQ MIN T) (SETQ TT (CADR TT)))))
              (COND
               ((NOT (EQCAR TT 'TIMES))
                (COND
                 (MIN
                  (COND
                   ((OR (EQUAL TT VAR)
                        (AND (EQCAR TT 'EXPT) (EQUAL (CADR TT) VAR)))
                    (SETQ RES (LIST 'TIMES '(MINUS 1) TT)))
                   (T (SETQ RES (LIST 'MINUS TT)))))
                 (T (SETQ RES TT))))
               (T
                (PROGN
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (NOT (NULL (SETQ TT (CDR TT))))) (RETURN NIL)))
                   (PROGN
                    (COND
                     ((AND (PAIRP (SETQ H (CAR TT))) (EQCAR H 'MINUS))
                      (PROGN (SETQ MIN (NOT MIN)) (SETQ H (CADR H)))))
                    (COND ((EQUAL H VAR) (SETQ FACTR H))
                          (T
                           (PROGN
                            (COND
                             ((AND (EQCAR H 'EXPT) (EQUAL (CADR H) VAR))
                              (SETQ FACTR H))
                             (T (SETQ RES (APPEND RES (LIST H)))))))))
                   (GO WHILELABEL))
                 (COND
                  (MIN
                   (PROGN
                    (SETQ H (LIST 'MINUS (CAR RES)))
                    (COND ((NULL (CDR RES)) (SETQ RES (LIST H)))
                          (T (SETQ RES (APPEND (LIST H) (CDR RES))))))))
                 (SETQ RES
                         (COND ((NULL FACTR) (CONS 'TIMES RES))
                               ((NULL (CDR RES)) (LIST 'TIMES (CAR RES) FACTR))
                               (T
                                (LIST 'TIMES (APPEND '(TIMES) RES)
                                      FACTR))))))))))
      (RETURN RES))) 
(PUT 'SCHEMA 'NUMBER-OF-ARGS 3) 
(PUT 'SCHEMA 'DEFINED-ON-LINE '358) 
(PUT 'SCHEMA 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'SCHEMA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SCHEMA (PN VAR N)
    (PROG (HN K K+1MIS)
      (SETQ HN (KCOF (CAR PN) N))
      (COND ((NULL (SETQ PN (CDR PN))) (SETQ PN (LIST NIL))))
      (PROG (K)
        (SETQ K (DIFFERENCE N 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 K))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (KPOW (CAR PN) VAR) K)
           (PROGN
            (SETQ HN
                    (LIST 'PLUS (KCOF (CAR PN) K)
                          (COND ((EQUAL HN 1) VAR)
                                ((AND (NOT (EQUAL K (DIFFERENCE N 1))) K+1MIS)
                                 (COND
                                  ((AND (PAIRP HN) (EQUAL (CAR HN) 'TIMES))
                                   (LIST 'TIMES
                                         (LIST 'EXPT VAR
                                               (PLUS (KPOW (CADR HN) VAR) 1))
                                         (CADDR HN)))
                                  (T (LIST 'EXPT VAR (PLUS (KPOW HN VAR) 1)))))
                                (T (LIST 'TIMES VAR HN)))))
            (SETQ K+1MIS NIL)
            (COND ((NULL (SETQ PN (CDR PN))) (SETQ PN (LIST NIL))))))
          (T
           (PROGN
            (SETQ HN
                    (COND ((EQUAL HN 1) VAR)
                          ((AND (NOT (EQUAL K (DIFFERENCE N 1))) K+1MIS)
                           (COND
                            ((AND (PAIRP HN) (EQUAL (CAR HN) 'TIMES))
                             (LIST 'TIMES
                                   (LIST 'EXPT VAR
                                         (PLUS (KPOW (CADR HN) VAR) 1))
                                   (CADDR HN)))
                            (T (LIST 'EXPT VAR (PLUS (KPOW HN VAR) 1)))))
                          (T (LIST 'TIMES VAR HN))))
            (SETQ K+1MIS T)))))
        (SETQ K (PLUS2 K (MINUS 1)))
        (GO LAB))
      (RETURN HN))) 
(PUT 'KPOW 'NUMBER-OF-ARGS 2) 
(PUT 'KPOW 'DEFINED-ON-LINE '417) 
(PUT 'KPOW 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'KPOW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOW (TERM VAR)
    (PROG (H)
      (RETURN
       (COND ((NULL TERM) NIL) ((EQUAL (SETQ H TERM) VAR) 1)
             ((AND (EQCAR H 'EXPT) (EQCAR (CDR H) VAR)) (CADDR H))
             ((EQCAR H 'TIMES)
              (COND ((EQUAL (SETQ H (CADDR H)) VAR) 1)
                    ((AND (NOT (ATOM H)) (EQCAR (CDR H) VAR)) (CADDR H))
                    (T 0)))
             (T 0))))) 
(PUT 'KCOF 'NUMBER-OF-ARGS 2) 
(PUT 'KCOF 'DEFINED-ON-LINE '441) 
(PUT 'KCOF 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'KCOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KCOF (TERM N)
    (COND ((NULL N) NIL) ((EQUAL N 0) TERM)
          ((EQUAL N 1) (COND ((NOT (EQCAR TERM 'TIMES)) 1) (T (CADR TERM))))
          ((EQCAR TERM 'EXPT) 1) (T (CADR TERM)))) 
(PUT 'MAINVAR2 'NUMBER-OF-ARGS 1) 
(PUT 'MAINVAR2 'DEFINED-ON-LINE '459) 
(PUT 'MAINVAR2 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'MAINVAR2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAINVAR2 (U)
    (PROG (RES)
      (SETQ RES
              (COND
               (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                 (SETQ U (CAR (SIMP* U))))
                NIL)
               ((SFP (SETQ U (CAAAR U))) (PREPF U)) (T U)))
      (COND ((EQCAR RES 'EXPT) (SETQ RES NIL)))
      (RETURN RES))) 
(PUT 'ALGHORNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ALGHORNEREVAL 'DEFINED-ON-LINE '481) 
(PUT 'ALGHORNEREVAL 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'ALGHORNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGHORNEREVAL (U)
    (PROG (ALGPRI ASSSET RES VARLIST NARGS)
      (SETQ NARGS 0)
      (SETQ NARGS (LENGTH U))
      (COND
       ((LESSP NARGS 3)
        (PROGN
         (SETQ ASSSET
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL
                           (COND ((ATOM (CAR U)) (CDR (REVAL1 (CAR U) T)))
                                 (T (CDAR U))))
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL)
                                       (LIST 'SETQ (CADR EL) (CADDR EL)))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL) (LIST 'SETQ (CADR EL) (CADDR EL)))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND ((EQUAL NARGS 2) (SETQ VARLIST (CDADR U))))))
       (T (SETQ ASSSET '**ERROR**)))
      (COND
       ((EQ ASSSET '**ERROR**) (REDERR "WRONG NUMBER OF ARGUMENTS ALGHORNER"))
       (T
        (PROGN
         (SETQ ALGPRI *ALGPRI)
         (SETQ *ALGPRI NIL)
         (SETQ RES (APPLY 'GHORNER (LIST ASSSET VARLIST)))
         (COND
          ((SETQ *ALGPRI ALGPRI)
           (RETURN
            (ALGRESULTS1
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL RES)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL)))
                                 (CAR EL))
                                NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL))) (CAR EL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
          (T (RETURN RES)))))))) 
(PUT 'ALGHORNER 'PSOPFN 'ALGHORNEREVAL) 
(PUT 'HORNER0 'NUMBER-OF-ARGS 2) 
(FLAG '(HORNER0) 'OPFN) 
(PUT 'HORNER0 'DEFINED-ON-LINE '518) 
(PUT 'HORNER0 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNER0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNER0 (P X)
    (PROG (C H)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ P (AEVAL P))
      (SETQ C (AEVAL (LIST 'REVERSE (LIST 'COEFF P X))))
      (AEVAL (OFF (LIST 'EXP)))
      (SETQ H (AEVAL 0))
      (WHILE (EVALNEQ (AEVAL* C) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ H (AEVAL* (LIST 'PLUS (LIST 'TIMES H X) (LIST 'FIRST C))))
              (SETQ C (AEVAL* (LIST 'REST C)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL H)))) 
(PUT 'HORNER1 'NUMBER-OF-ARGS 1) 
(FLAG '(HORNER1) 'OPFN) 
(PUT 'HORNER1 'DEFINED-ON-LINE '537) 
(PUT 'HORNER1 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNER1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNER1 (P)
    (COND ((EVALNUMBERP (AEVAL P)) (AEVAL P))
          (T
           (PROG (C H X)
             (AEVAL (ON (LIST 'EXP)))
             (SETQ P (AEVAL P))
             (SETQ X (AEVAL (LIST 'MAINVAR P)))
             (SETQ C (AEVAL (LIST 'REVERSE (LIST 'COEFF P X))))
             (AEVAL (OFF (LIST 'EXP)))
             (SETQ H (AEVAL 0))
             (WHILE (EVALNEQ (AEVAL* C) (AEVAL* (LIST 'LIST)))
                    (PROGN
                     (SETQ H
                             (AEVAL*
                              (LIST 'PLUS (LIST 'TIMES H X)
                                    (LIST 'HORNER1 (LIST 'FIRST C)))))
                     (SETQ C (AEVAL* (LIST 'REST C)))
                     (AEVAL* 'NIL)))
             (RETURN (AEVAL H)))))) 
(GLOBAL '(HVLST)) 
(PUT 'HORNER2 'NUMBER-OF-ARGS 1) 
(FLAG '(HORNER2) 'OPFN) 
(PUT 'HORNER2 'DEFINED-ON-LINE '560) 
(PUT 'HORNER2 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNER2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNER2 (P) (PROGN (AEVAL (LIST 'CLHVLIST)) (AEVAL (LIST 'HORNER20 P)))) 
(PUT 'HORNER20 'NUMBER-OF-ARGS 1) 
(FLAG '(HORNER20) 'OPFN) 
(PUT 'HORNER20 'DEFINED-ON-LINE '571) 
(PUT 'HORNER20 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HORNER20 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNER20 (P)
    (COND ((EVALNUMBERP (AEVAL P)) (AEVAL P))
          (T
           (PROG (Q X C)
             (AEVAL (ON (LIST 'EXP)))
             (SETQ Q (AEVAL P))
             (SETQ X (AEVAL (LIST 'HVAR1 Q)))
             (SETQ C (AEVAL (LIST 'SUB (LIST 'EQUAL X 0) Q)))
             (SETQ Q (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE Q C) X)))
             (AEVAL (OFF (LIST 'EXP)))
             (SETQ Q
                     (AEVAL
                      (LIST 'PLUS (LIST 'TIMES (LIST 'HORNER20 Q) X)
                            (LIST 'HORNER20 C))))
             (RETURN (AEVAL Q)))))) 
(PUT 'HVAR1 'NUMBER-OF-ARGS 1) 
(PUT 'HVAR1 'DEFINED-ON-LINE '589) 
(PUT 'HVAR1 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HVAR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HVAR1 (Q)
    (COND ((NUMBERP Q) (REDERR "HVAR1: impossible!"))
          (T
           (PROG (X Y V)
             (SETQ Q (REVAL1 Q T))
             (COND ((AND (NULL (ATOM Q)) (EQ (CAR Q) 'PLUS)) (SETQ Q (CDR Q)))
                   (T (SETQ Q (LIST Q))))
             (PROG (Z)
               (SETQ Z Q)
              LAB
               (COND ((NULL Z) (RETURN NIL)))
               ((LAMBDA (Z)
                  (PROGN
                   (COND
                    ((AND (NULL (ATOM Z)) (EQ (CAR Z) 'MINUS))
                     (SETQ Z (CADR Z))))
                   (COND
                    ((AND (NULL (ATOM Z)) (EQ (CAR Z) 'TIMES))
                     (SETQ Z (CDR Z)))
                    (T (SETQ Z (LIST Z))))
                   (PROG (W)
                     (SETQ W Z)
                    LAB
                     (COND ((NULL W) (RETURN NIL)))
                     ((LAMBDA (W)
                        (PROGN
                         (COND
                          ((AND (NULL (ATOM W)) (EQ (CAR W) 'EXPT))
                           (SETQ W (CADR W)))
                          ((NUMBERP W) (SETQ W NIL)))
                         (COND
                          ((AND W (SETQ Y (ASSOC W V)))
                           (RPLACD Y (PLUS (CDR Y) 1)))
                          (W (SETQ V (CONS (CONS W 1) V))))
                         NIL))
                      (CAR W))
                     (SETQ W (CDR W))
                     (GO LAB))
                   NIL))
                (CAR Z))
               (SETQ Z (CDR Z))
               (GO LAB))
             (SETQ X (CAR V))
             (PROG (Z)
               (SETQ Z (CDR V))
              LAB
               (COND ((NULL Z) (RETURN NIL)))
               ((LAMBDA (Z) (COND ((GREATERP (CDR Z) (CDR X)) (SETQ X Z))))
                (CAR Z))
               (SETQ Z (CDR Z))
               (GO LAB))
             (SETQ HVLST (CONS (CONS Q V) HVLST))
             (RETURN (CAR X)))))) 
(PUT 'KHORNER20 'NUMBER-OF-ARGS 2) 
(FLAG '(KHORNER20) 'OPFN) 
(PUT 'KHORNER20 'DEFINED-ON-LINE '623) 
(PUT 'KHORNER20 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'KHORNER20 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KHORNER20 (P VLST)
    (COND ((EVALNUMBERP (AEVAL P)) (AEVAL P))
          (T
           (PROG (Q X C)
             (AEVAL (ON (LIST 'EXP)))
             (SETQ Q (AEVAL P))
             (COND
              ((BOOLVALUE* (SETQ X (REVALX (LIST 'KHVAR1 Q VLST))))
               (PROGN
                (SETQ C (AEVAL (LIST 'SUB (LIST 'EQUAL X 0) Q)))
                (SETQ Q (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE Q C) X)))
                (AEVAL (OFF (LIST 'EXP)))
                (RETURN
                 (AEVAL
                  (LIST 'PLUS (LIST 'TIMES (LIST 'KHORNER20 Q VLST) X)
                        (LIST 'KHORNER20 C VLST))))))
              (T
               (PROGN
                (AEVAL (OFF (LIST 'EXP)))
                (RETURN (AEVAL (LIST 'NESTEDFAC Q)))))))))) 
(PUT 'KHVAR1 'NUMBER-OF-ARGS 2) 
(PUT 'KHVAR1 'DEFINED-ON-LINE '647) 
(PUT 'KHVAR1 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'KHVAR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KHVAR1 (Q VLST)
    (COND ((NUMBERP Q) (REDERR "HVAR1: impossible!"))
          (T
           (PROG (X Y V)
             (SETQ VLST (CDR VLST))
             (SETQ Q (REVAL1 Q T))
             (COND ((AND (NULL (ATOM Q)) (EQ (CAR Q) 'PLUS)) (SETQ Q (CDR Q)))
                   (T (SETQ Q (LIST Q))))
             (PROG (Z)
               (SETQ Z Q)
              LAB
               (COND ((NULL Z) (RETURN NIL)))
               ((LAMBDA (Z)
                  (PROGN
                   (COND
                    ((AND (NULL (ATOM Z)) (EQ (CAR Z) 'MINUS))
                     (SETQ Z (CADR Z))))
                   (COND
                    ((AND (NULL (ATOM Z)) (EQ (CAR Z) 'TIMES))
                     (SETQ Z (CDR Z)))
                    (T (SETQ Z (LIST Z))))
                   (PROG (W)
                     (SETQ W Z)
                    LAB
                     (COND ((NULL W) (RETURN NIL)))
                     ((LAMBDA (W)
                        (PROGN
                         (COND
                          ((AND (NULL (ATOM W)) (EQ (CAR W) 'EXPT))
                           (SETQ W (CADR W)))
                          ((NUMBERP W) (SETQ W NIL)))
                         (COND
                          ((AND W (MEMQ W VLST))
                           (COND
                            ((SETQ Y (ASSOC W V)) (RPLACD Y (PLUS (CDR Y) 1)))
                            (T (SETQ V (CONS (CONS W 1) V))))))))
                      (CAR W))
                     (SETQ W (CDR W))
                     (GO LAB))))
                (CAR Z))
               (SETQ Z (CDR Z))
               (GO LAB))
             (COND
              (V
               (PROGN
                (SETQ X (CAR V))
                (PROG (Z)
                  (SETQ Z (CDR V))
                 LAB
                  (COND ((NULL Z) (RETURN NIL)))
                  ((LAMBDA (Z) (COND ((GREATERP (CDR Z) (CDR X)) (SETQ X Z))))
                   (CAR Z))
                  (SETQ Z (CDR Z))
                  (GO LAB))
                (RETURN (CAR X))))
              (T (RETURN NIL))))))) 
(PUT 'HVLIST 'NUMBER-OF-ARGS 0) 
(PUT 'HVLIST 'DEFINED-ON-LINE '683) 
(PUT 'HVLIST 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'HVLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE HVLIST NIL
    (PROG (X)
      (SETQ X HVLST)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X) (PRINT X)) (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'CLHVLIST 'NUMBER-OF-ARGS 0) 
(PUT 'CLHVLIST 'DEFINED-ON-LINE '690) 
(PUT 'CLHVLIST 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'CLHVLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLHVLIST NIL (SETQ HVLST NIL)) 
(FLAG '(KHVAR1 HVAR1 HVLIST CLHVLIST) 'OPFN) 
(PUT 'KHORNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'KHORNEREVAL 'DEFINED-ON-LINE '710) 
(PUT 'KHORNEREVAL 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'KHORNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KHORNEREVAL (U)
    (PROG (POLY VARLST NARGS)
      (SETQ NARGS 0)
      (SETQ NARGS (LENGTH U))
      (COND
       ((LESSP NARGS 3)
        (PROGN
         (SETQ POLY (REVAL1 (CAR U) NIL))
         (COND ((EQUAL NARGS 2) (SETQ VARLST (REVAL1 (CADR U) NIL))))))
       (T (SETQ POLY '**ERROR**)))
      (COND ((EQ POLY '**ERROR**) (REDERR "WRONG NUMBER OF ARGUMENTS KHORNER"))
            (T
             (RETURN
              (COND ((EQUAL NARGS 1) (REVAL1 (HORNER2 POLY) T))
                    (T (REVAL1 (KHORNER20 POLY VARLST) T)))))))) 
(PUT 'KHORNER 'PSOPFN 'KHORNEREVAL) 
(PUT 'GKHORNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GKHORNEREVAL 'DEFINED-ON-LINE '726) 
(PUT 'GKHORNEREVAL 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'GKHORNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GKHORNEREVAL (U)
    (PROG (POLY_S VARLST NARGS)
      (SETQ NARGS 0)
      (SETQ NARGS (LENGTH U))
      (COND
       ((LESSP NARGS 3)
        (PROGN
         (SETQ POLY_S (CDAR U))
         (COND ((EQUAL NARGS 2) (SETQ VARLST (CADR U))))))
       (T (SETQ POLY_S '**ERROR**)))
      (COND
       ((EQ POLY_S '**ERROR**) (REDERR "WRONG NUMBER OF ARGUMENTS GKHORNER"))
       (T
        (RETURN
         (COND
          ((AND (PAIRP (CAR POLY_S)) (EQ (CAAR POLY_S) 'EQUAL))
           (APPEND (LIST 'LIST)
                   (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                     (SETQ POLY POLY_S)
                     (COND ((NULL POLY) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (POLY)
                                         (LIST 'EQUAL (CADR POLY)
                                               (KHORNEREVAL
                                                (COND
                                                 ((EQUAL NARGS 1) (CDDR POLY))
                                                 (T
                                                  (LIST (CADDR POLY)
                                                        VARLST))))))
                                       (CAR POLY))
                                      NIL)))
                    LOOPLABEL
                     (SETQ POLY (CDR POLY))
                     (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (POLY)
                                 (LIST 'EQUAL (CADR POLY)
                                       (KHORNEREVAL
                                        (COND ((EQUAL NARGS 1) (CDDR POLY))
                                              (T
                                               (LIST (CADDR POLY) VARLST))))))
                               (CAR POLY))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
          (T
           (APPEND (LIST 'LIST)
                   (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                     (SETQ POLY POLY_S)
                     (COND ((NULL POLY) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (POLY)
                                         (KHORNEREVAL
                                          (COND ((EQUAL NARGS 1) (LIST POLY))
                                                (T (LIST POLY VARLST)))))
                                       (CAR POLY))
                                      NIL)))
                    LOOPLABEL
                     (SETQ POLY (CDR POLY))
                     (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (POLY)
                                 (KHORNEREVAL
                                  (COND ((EQUAL NARGS 1) (LIST POLY))
                                        (T (LIST POLY VARLST)))))
                               (CAR POLY))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))))))) 
(PUT 'GKHORNER 'PSOPFN 'GKHORNEREVAL) 
(PUT 'ALGGKHORNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ALGGKHORNEREVAL 'DEFINED-ON-LINE '751) 
(PUT 'ALGGKHORNEREVAL 'DEFINED-IN-FILE 'SCOPE/CODHRN.RED) 
(PUT 'ALGGKHORNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGGKHORNEREVAL (U)
    (PROG (POLY_S VARLST NARGS)
      (SETQ NARGS 0)
      (SETQ NARGS (LENGTH U))
      (COND
       ((LESSP NARGS 3)
        (PROGN
         (SETQ POLY_S (CDAR U))
         (COND ((EQUAL NARGS 2) (SETQ VARLST (CADR U))))))
       (T (SETQ POLY_S '**ERROR**)))
      (COND
       ((EQ POLY_S '**ERROR**) (REDERR "WRONG NUMBER OF ARGUMENTS GKHORNER"))
       (T
        (RETURN
         (ALGRESULTS1
          (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
            (SETQ POLY POLY_S)
            (COND ((NULL POLY) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (POLY)
                                (CONS (CADR POLY)
                                      (KHORNEREVAL
                                       (COND ((EQUAL NARGS 1) (CDDR POLY))
                                             (T (LIST (CADDR POLY) VARLST))))))
                              (CAR POLY))
                             NIL)))
           LOOPLABEL
            (SETQ POLY (CDR POLY))
            (COND ((NULL POLY) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (POLY)
                        (CONS (CADR POLY)
                              (KHORNEREVAL
                               (COND ((EQUAL NARGS 1) (CDDR POLY))
                                     (T (LIST (CADDR POLY) VARLST))))))
                      (CAR POLY))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))))))) 
(PUT 'ALGGKHORNER 'PSOPFN 'ALGGKHORNEREVAL) 
(ENDMODULE) 
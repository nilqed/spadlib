(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LINALGSYS)) 
(FLUID '(COUNT_TRIES TR_SUBSYS MAX_LOSOF MATRIX_849)) 
(SETQ TR_SUBSYS NIL) 
(PUT 'ADD_EQU_TO_FL 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_EQU_TO_FL 'DEFINED-ON-LINE '38) 
(PUT 'ADD_EQU_TO_FL 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ADD_EQU_TO_FL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_EQU_TO_FL (P FL)
    (PROG (H CP)
      (SETQ H (GET P 'ALLVARFCTS))
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROGN
         (SETQ CP FL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND CP (NEQ (CAR H) (CADAR CP)))) (RETURN NIL)))
           (SETQ CP (CDR CP))
           (GO WHILELABEL))
         (COND ((NULL CP) (SETQ FL (CONS (LIST 1 (CAR H) P) FL)))
               (T
                (RPLACA CP
                        (CONS (PLUS 1 (CAAR CP))
                              (CONS (CADAR CP) (CONS P (CDDAR CP)))))))
         (SETQ H (CDR H)))
        (GO WHILELABEL))
      (RETURN FL))) 
(PUT 'DEL_EQU_FROM_FL 'NUMBER-OF-ARGS 2) 
(PUT 'DEL_EQU_FROM_FL 'DEFINED-ON-LINE '52) 
(PUT 'DEL_EQU_FROM_FL 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'DEL_EQU_FROM_FL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEL_EQU_FROM_FL (P FL)
    (PROG (CP)
      (SETQ CP FL)
      (PROG ()
       WHILELABEL
        (COND ((NOT CP) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (FREEOF (CDDAR CP) P))
           (RPLACA CP
                   (CONS (DIFFERENCE (CAAR CP) 1)
                         (CONS (CADAR CP) (DELETE P (CDDAR CP)))))))
         (SETQ CP (CDR CP)))
        (GO WHILELABEL))
      (RETURN FL))) 
(PUT 'ALL_EQU_WITH_ANY_FL 'NUMBER-OF-ARGS 2) 
(PUT 'ALL_EQU_WITH_ANY_FL 'DEFINED-ON-LINE '65) 
(PUT 'ALL_EQU_WITH_ANY_FL 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ALL_EQU_WITH_ANY_FL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALL_EQU_WITH_ANY_FL (RSOE SF)
    (PROG (N NEWRSOE NEWNSOE)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT RSOE) (RETURN NIL)))
        (PROGN
         (COND
          ((FREEOFLIST (GET (CAR RSOE) 'ALLVARFCTS) SF)
           (SETQ NEWRSOE (CONS (CAR RSOE) NEWRSOE)))
          (T
           (PROGN (SETQ N (ADD1 N)) (SETQ NEWNSOE (CONS (CAR RSOE) NEWNSOE)))))
         (SETQ RSOE (CDR RSOE)))
        (GO WHILELABEL))
      (RETURN (LIST N NEWNSOE NEWRSOE)))) 
(PUT 'ERR_CATCH_SUBSYS 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_SUBSYS 'DEFINED-ON-LINE '78) 
(PUT 'ERR_CATCH_SUBSYS 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ERR_CATCH_SUBSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_SUBSYS (PDES)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_SS))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_SS)
      ((LAMBDA (*PROTFG)
         (SETQ H (ERRORSET (LIST 'SHOW_SUB_SYSTEMS (MKQUOTE PDES)) NIL NIL)))
       T)
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS12 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS12 'DEFINED-ON-LINE '95) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS12 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS12 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS12 (ARGLIST)
    (FAUSS1 (CAR ARGLIST) (CADR ARGLIST) 2)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS13 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS13 'DEFINED-ON-LINE '98) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS13 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS13 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS13 (ARGLIST)
    (FAUSS1 (CAR ARGLIST) (CADR ARGLIST) 3)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS14 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS14 'DEFINED-ON-LINE '101) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS14 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS14 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS14 (ARGLIST)
    (FAUSS1 (CAR ARGLIST) (CADR ARGLIST) 4)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS15 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS15 'DEFINED-ON-LINE '104) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS15 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS15 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS15 (ARGLIST)
    (FAUSS1 (CAR ARGLIST) (CADR ARGLIST) 5)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS22 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS22 'DEFINED-ON-LINE '107) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS22 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS22 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS22 (ARGLIST)
    (FAUSS2 (CAR ARGLIST) (CADR ARGLIST) 2)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS23 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS23 'DEFINED-ON-LINE '110) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS23 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS23 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS23 (ARGLIST)
    (FAUSS2 (CAR ARGLIST) (CADR ARGLIST) 3)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS24 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS24 'DEFINED-ON-LINE '113) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS24 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS24 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS24 (ARGLIST)
    (FAUSS2 (CAR ARGLIST) (CADR ARGLIST) 4)) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS25 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS25 'DEFINED-ON-LINE '116) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS25 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FIND_AND_USE_SUB_SYSTEMS25 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_AND_USE_SUB_SYSTEMS25 (ARGLIST)
    (FAUSS2 (CAR ARGLIST) (CADR ARGLIST) 5)) 
(PUT 'DETERMINANTE 'NUMBER-OF-ARGS 1) 
(PUT 'DETERMINANTE 'DEFINED-ON-LINE '119) 
(PUT 'DETERMINANTE 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'DETERMINANTE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DETERMINANTE (MATR)
    (PROG (D)
      (SETQ D (REVAL1 (LIST 'DET MATR) NIL))
      (RETURN
       (COND ((AND (PAIRP D) (EQUAL (CAR D) '*SQ)) (CADR D)) (T (SIMP D)))))) 
(PUT 'SETZEWERT 'NUMBER-OF-ARGS 4) 
(PUT 'SETZEWERT 'DEFINED-ON-LINE '126) 
(PUT 'SETZEWERT 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'SETZEWERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETZEWERT (MATR II JJ VAL) (SETK (LIST MATR II JJ) VAL)) 
(PUT 'MACHEMATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'MACHEMATRIX 'DEFINED-ON-LINE '129) 
(PUT 'MACHEMATRIX 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'MACHEMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MACHEMATRIX (MATR IDIM JDIM)
    (SETK MATR
          (REVAL1
           (CONS 'MAT
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                   (COND ((MINUSP (DIFFERENCE IDIM I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ J 1)
                                      (COND
                                       ((MINUSP (DIFFERENCE JDIM J))
                                        (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS NIL NIL)))
                                     LOOPLABEL
                                      (SETQ J (PLUS2 J 1))
                                      (COND
                                       ((MINUSP (DIFFERENCE JDIM J))
                                        (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND ((MINUSP (DIFFERENCE IDIM I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (PROG (J FORALL-RESULT FORALL-ENDPTR)
                              (SETQ J 1)
                              (COND
                               ((MINUSP (DIFFERENCE JDIM J)) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR (CONS NIL NIL)))
                             LOOPLABEL
                              (SETQ J (PLUS2 J 1))
                              (COND
                               ((MINUSP (DIFFERENCE JDIM J))
                                (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
           NIL))) 
(PUT 'FAUSS1 'NUMBER-OF-ARGS 3) 
(PUT 'FAUSS1 'DEFINED-ON-LINE '133) 
(PUT 'FAUSS1 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FAUSS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FAUSS1 (PDES FORG ML)
    (PROG (DE ODET N H)
      (SETQ MAX_LOSOF ML)
      (SETQ ODET (SPOT_OVER_DET PDES NIL NIL FLIN_))
      (COND ((NULL ODET) (RETURN NIL)))
      (SETQ N 0)
      (PROGN
       (PRIN2 "The following is a list of not underdetermined sub-systems")
       NIL)
      (TERPRI)
      (PROG (DE)
        (SETQ DE ODET)
       LAB
        (COND ((NULL DE) (RETURN NIL)))
        ((LAMBDA (DE)
           (PROGN
            (SETQ N (ADD1 N))
            (PROGN (PRIN2 "Sub-system ") (PRIN2 N) (PRIN2 ":") NIL)
            (TERPRI)
            (PLOT_DEP_MATRIX (CAR DE) (CDR DE))
            NIL))
         (CAR DE))
        (SETQ DE (CDR DE))
        (GO LAB))
      (COND ((NULL ODET) (SETQ H NIL))
            (T
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ H (ERR_CATCH_GROEB (LIST PDES FORG VL_ (CAAR ODET))))
                (SETQ ODET (CDR ODET)))
               (COND ((NOT (OR H (NULL ODET))) (GO REPEATLABEL))))))
      (RETURN H))) 
(PUT 'FAUSS2 'NUMBER-OF-ARGS 3) 
(PUT 'FAUSS2 'DEFINED-ON-LINE '158) 
(PUT 'FAUSS2 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'FAUSS2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FAUSS2 (PDES FORG ML)
    (COND
     ((AND FLIN_ (FREEOF LAST_STEPS 'SUB_SYS))
      (PROG (OSOF NSOF ODET H N0FLIN_ N0FLIN_CP NCONDI DE U V SYSLI SY R S
             SOME_NEW FL1 FL2 F M NO_OF_PDES SB OK)
        (SETQ MAX_LOSOF ML)
        (SETQ N0FLIN_ NIL)
        (SETQ FL1
                (SETDIFF_ACCORDING_TO FTEM_
                 (SETDIFF_ACCORDING_TO FTEM_ FLIN_ FTEM_) FTEM_))
        (PROG (M)
          (SETQ M 1)
         LAB
          (COND ((MINUSP (DIFFERENCE MAX_LOSOF M)) (RETURN NIL)))
          (PROGN
           (PROG (H)
             (SETQ H INEQ_)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (PROGN
                 (SETQ FL2 (SMEMBERL FL1 H))
                 (COND
                  ((EQUAL (LENGTH FL2) M)
                   (PROGN
                    (SETQ N0FLIN_CP N0FLIN_)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND N0FLIN_CP (NOT_INCLUDED (CAR N0FLIN_CP) FL2)))
                        (RETURN NIL)))
                      (SETQ N0FLIN_CP (CDR N0FLIN_CP))
                      (GO WHILELABEL))
                    (COND
                     ((NULL N0FLIN_CP)
                      (PROGN
                       (SETQ H
                               (SUBSQ H
                                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ F FL2)
                                        (COND ((NULL F) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (F)
                                                            (CONS F 0))
                                                          (CAR F))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ F (CDR F))
                                        (COND
                                         ((NULL F) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (F) (CONS F 0))
                                                  (CAR F))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))))
                       (COND
                        ((SQZEROP H)
                         (SETQ N0FLIN_ (CONS FL2 N0FLIN_))))))))))))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           (PROG (H)
             (SETQ H INEQ_OR)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (PROGN
                 (SETQ FL2 (SMEMBERL FL1 H))
                 (COND
                  ((EQUAL (LENGTH FL2) M)
                   (PROGN
                    (SETQ N0FLIN_CP N0FLIN_)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND N0FLIN_CP (NOT_INCLUDED (CAR N0FLIN_CP) FL2)))
                        (RETURN NIL)))
                      (SETQ N0FLIN_CP (CDR N0FLIN_CP))
                      (GO WHILELABEL))
                    (COND
                     ((NULL N0FLIN_CP)
                      (PROGN
                       (SETQ SB
                               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ F FL2)
                                 (COND ((NULL F) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (F) (CONS F 0))
                                                   (CAR F))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ F (CDR F))
                                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (F) (CONS F 0)) (CAR F))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (SETQ OK NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT H) (RETURN NIL)))
                         (COND
                          ((PROGN
                            (SETQ U (CAR H))
                            (PROG ()
                             WHILELABEL
                              (COND
                               ((NOT
                                 (AND U (NULL (SQZEROP (SUBSQ (CAR U) SB)))))
                                (RETURN NIL)))
                              (SETQ U (CDR U))
                              (GO WHILELABEL))
                            (COND (U NIL) (T T)))
                           (PROGN (SETQ OK T) (SETQ H NIL)))
                          (T (SETQ H (CDR H))))
                         (GO WHILELABEL))
                       (COND
                        ((NULL OK) (SETQ N0FLIN_ (CONS FL2 N0FLIN_))))))))))))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           NIL)
          (SETQ M (PLUS2 M 1))
          (GO LAB))
        (SETQ N0FLIN_CP (REVERSE N0FLIN_))
        (PROG ()
         WHILELABEL
          (COND ((NOT N0FLIN_CP) (RETURN NIL)))
          (PROGN
           (SETQ OSOF (CAR N0FLIN_CP))
           (SETQ ODET (SPOT_OVER_DET PDES FLIN_ OSOF NSOF))
           (PROG ()
            WHILELABEL
             (COND ((NOT ODET) (RETURN NIL)))
             (PROGN
              (SETQ DE (CAR ODET))
              (SETQ ODET (CDR ODET))
              (SETQ U (LENGTH (CAR DE)))
              (SETQ V (LENGTH (CDR DE)))
              (SETQ SYSLI (OUT_OFF V U (CAR DE)))
              (PROG (SY)
                (SETQ SY SYSLI)
               LAB
                (COND ((NULL SY) (RETURN NIL)))
                ((LAMBDA (SY)
                   (PROGN
                    (COND
                     (TR_SUBSYS
                      (PROGN (PROGN (PRIN2 "sy=") (PRIN2 SY) NIL) (TERPRI))))
                    (MACHEMATRIX 'MATRIX_849 V V)
                    (PROG (R)
                      (SETQ R 1)
                     LAB
                      (COND ((MINUSP (DIFFERENCE V R)) (RETURN NIL)))
                      (PROG (S)
                        (SETQ S 1)
                       LAB
                        (COND ((MINUSP (DIFFERENCE V S)) (RETURN NIL)))
                        (SETZEWERT 'MATRIX_849 R S
                         (COEFFN (LIST '*SQ (GET (NTH SY R) 'SQVAL) T)
                                 (NTH (CDR DE) S) 1))
                        (SETQ S (PLUS2 S 1))
                        (GO LAB))
                      (SETQ R (PLUS2 R 1))
                      (GO LAB))
                    (SETQ NCONDI (CONS (DETERMINANTE 'MATRIX_849) NCONDI))
                    (SETK 'MATRIX_849 NIL)))
                 (CAR SY))
                (SETQ SY (CDR SY))
                (GO LAB)))
             (GO WHILELABEL))
           (SETQ NSOF (APPEND (CAR N0FLIN_CP) NSOF))
           (SETQ N0FLIN_CP (CDR N0FLIN_CP)))
          (GO WHILELABEL))
        (COND
         (TR_SUBSYS
          (PROGN (PROGN (PRIN2 "ncondi=") (PRIN2 NCONDI) NIL) (TERPRI))))
        (SETQ NO_OF_PDES (LENGTH PDES))
        (SETQ S NIL)
        (PROG (H)
          (SETQ H NCONDI)
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (PROGN
              (SETQ R
                      (MKEQSQ H NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL
                       PDES))
              (SETQ PDES (EQINSERT R PDES))
              (SETQ S (CONS R S))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (PROG (H)
          (SETQ H S)
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (COND ((MEMBER H PDES) (SETQ SOME_NEW (CONS H SOME_NEW)))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (COND
         ((AND PRINT_ SOME_NEW)
          (PROGN
           (PROGN
            (PRIN2 "New equations due to vanishing coeff. determinants: ")
            (PRIN2 (CAR SOME_NEW))
            NIL)
           (PROG (H)
             (SETQ H (CDR SOME_NEW))
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H) (PROGN (PRIN2 ", ") (PRIN2 H) NIL)) (CAR H))
             (SETQ H (CDR H))
             (GO LAB)))))
        (RETURN
         (COND
          ((OR SOME_NEW (NEQ NO_OF_PDES (LENGTH PDES)))
           (COND
            ((IN_CYCLE
              (PROGN
               (SETQ M 0)
               (PROG (R)
                 (SETQ R SOME_NEW)
                LAB
                 (COND ((NULL R) (RETURN NIL)))
                 ((LAMBDA (R) (SETQ M (PLUS M (GET R 'PRINTLENGTH)))) (CAR R))
                 (SETQ R (CDR R))
                 (GO LAB))
               (SETQ R (LENGTH SOME_NEW))
               (SETQ S 0)
               (SETQ H NIL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND (LESSP S 3) SOME_NEW)) (RETURN NIL)))
                 (PROGN
                  (SETQ S (ADD1 S))
                  (SETQ H (CONS (GET (CAR SOME_NEW) 'TERMS) H))
                  (SETQ SOME_NEW (CDR SOME_NEW)))
                 (GO WHILELABEL))
               (CONS 'SUB_SYS (CONS STEPCOUNTER_ (CONS R (CONS M H))))))
             NIL)
            (T (LIST PDES FORG))))
          (T NIL))))))) 
(PUT 'SHOW_SUB_SYSTEMS 'NUMBER-OF-ARGS 1) 
(PUT 'SHOW_SUB_SYSTEMS 'DEFINED-ON-LINE '305) 
(PUT 'SHOW_SUB_SYSTEMS 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'SHOW_SUB_SYSTEMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHOW_SUB_SYSTEMS (PDES)
    (PROG (FL OSOF NSOF ODET)
      (CHANGE_PROMPT_TO "")
      (COND
       ((AND FLIN_
             (YESP "Shall only functions from the list flin_ be considered?"))
        (SETQ FL FLIN_)))
      (COND
       ((YESP "Are there functions which should appear in the sub-system? ")
        (SETQ OSOF (SELECT_FROM_LIST FTEM_ NIL))))
      (COND
       ((YESP
         "Are there functions which should NOT appear in the sub-system? ")
        (SETQ NSOF (SELECT_FROM_LIST FTEM_ NIL))))
      (COND
       ((YESP
         "Is there a maximum of the number of functions in the sub-system? ")
        (PROGN
         (PROGN (PRIN2 "What is the maximum? ") NIL)
         (SETQ MAX_LOSOF (TERMREAD))))
       (T (SETQ MAX_LOSOF 100000)))
      (SETQ ODET (SPOT_OVER_DET PDES FL OSOF NSOF))
      (PROG ()
       WHILELABEL
        (COND ((NOT ODET) (RETURN NIL)))
        (PROGN (PLOT_DEP_MATRIX (CAAR ODET) FTEM_) (SETQ ODET (CDR ODET)))
        (GO WHILELABEL))
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'OUT_OFF 'NUMBER-OF-ARGS 3) 
(PUT 'OUT_OFF 'DEFINED-ON-LINE '333) 
(PUT 'OUT_OFF 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'OUT_OFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OUT_OFF (M N L)
    (COND ((EQUAL M 0) (LIST NIL)) ((EQUAL M N) (LIST L))
          (T
           (NCONC (OUT_OFF M (DIFFERENCE N 1) (CDR L))
                  (PROG (H FORALL-RESULT FORALL-ENDPTR)
                    (SETQ H
                            (OUT_OFF (DIFFERENCE M 1) (DIFFERENCE N 1)
                             (CDR L)))
                    (COND ((NULL H) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (H) (CONS (CAR L) H)) (CAR H))
                                     NIL)))
                   LOOPLABEL
                    (SETQ H (CDR H))
                    (COND ((NULL H) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (H) (CONS (CAR L) H)) (CAR H)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))) 
(PUT 'SPOT_OVER_DET 'NUMBER-OF-ARGS 4) 
(PUT 'SPOT_OVER_DET 'DEFINED-ON-LINE '342) 
(PUT 'SPOT_OVER_DET 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'SPOT_OVER_DET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPOT_OVER_DET (PDES ALLF OSOF NSOF)
    (PROG (P H OSOE LOSOE NSOE LNSOE RSOE LRSOE LOSOF LNSOF RSOF LRSOF)
      (COND
       ((NULL ALLF)
        (PROG (P)
          (SETQ P PDES)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P) (SETQ ALLF (UNION (GET P 'ALLVARFCTS) ALLF))) (CAR P))
          (SETQ P (CDR P))
          (GO LAB))))
      (SETQ LRSOE (LENGTH PDES))
      (SETQ LRSOF (LENGTH ALLF))
      (COND
       (OSOF
        (PROGN
         (SETQ H (ADDSOE PDES OSOF ALLF))
         (SETQ LOSOE (CAR H))
         (SETQ OSOE (CADR H))
         (SETQ RSOE (CADDR H))
         (SETQ LRSOE (DIFFERENCE LRSOE LOSOE))
         (SETQ LOSOF (LENGTH OSOF))
         (SETQ RSOF (SETDIFF ALLF OSOF))
         (SETQ LRSOF (DIFFERENCE LRSOF LOSOF))
         NIL))
       (T
        (PROGN
         (SETQ LOSOE 0)
         (SETQ OSOE NIL)
         (SETQ RSOE PDES)
         (SETQ LOSOF 0)
         (SETQ RSOF ALLF))))
      (COND
       (NSOF
        (PROGN
         (SETQ H (ALL_EQU_WITH_ANY_FL RSOE NSOF))
         (SETQ LNSOE (CAR H))
         (SETQ NSOE (CADR H))
         (SETQ RSOE (CADDR H))
         (SETQ LRSOE (DIFFERENCE LRSOE LNSOE))
         (SETQ LNSOF (LENGTH NSOF))
         (SETQ RSOF (SETDIFF RSOF NSOF))
         (SETQ LRSOF (DIFFERENCE LRSOF LNSOF))))
       (T (PROGN (SETQ LNSOE 0) (SETQ NSOE NIL) (SETQ LNSOF 0))))
      (SETQ COUNT_TRIES 0)
      (RETURN
       (TRY LOSOE OSOE LNSOE NSOE LRSOE RSOE LOSOF OSOF LNSOF NSOF LRSOF RSOF
        ALLF)))) 
(PUT 'ADDSOE 'NUMBER-OF-ARGS 3) 
(PUT 'ADDSOE 'DEFINED-ON-LINE '403) 
(PUT 'ADDSOE 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ADDSOE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDSOE (RSOE SF ALLF)
    (PROG (N NEWSOE NEWRSOE)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT RSOE) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT_INCLUDED (INTERSECTION ALLF (GET (CAR RSOE) 'ALLVARFCTS)) SF)
           (SETQ NEWRSOE (CONS (CAR RSOE) NEWRSOE)))
          (T (PROGN (SETQ NEWSOE (CONS (CAR RSOE) NEWSOE)) (SETQ N (ADD1 N)))))
         (SETQ RSOE (CDR RSOE)))
        (GO WHILELABEL))
      (RETURN (LIST N NEWSOE NEWRSOE)))) 
(PUT 'ADDNSOE 'NUMBER-OF-ARGS 2) 
(PUT 'ADDNSOE 'DEFINED-ON-LINE '417) 
(PUT 'ADDNSOE 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ADDNSOE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDNSOE (RSOE SF)
    (PROG (N NEWRSOE NEWNSOE)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT RSOE) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT_INCLUDED SF (GET (CAR RSOE) 'ALLVARFCTS))
           (SETQ NEWRSOE (CONS (CAR RSOE) NEWRSOE)))
          (T
           (PROGN (SETQ NEWNSOE (CONS (CAR RSOE) NEWNSOE)) (SETQ N (ADD1 N)))))
         (SETQ RSOE (CDR RSOE)))
        (GO WHILELABEL))
      (RETURN (LIST N NEWNSOE NEWRSOE)))) 
(PUT 'ADDSOF 'NUMBER-OF-ARGS 2) 
(PUT 'ADDSOF 'DEFINED-ON-LINE '430) 
(PUT 'ADDSOF 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'ADDSOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDSOF (RSOF SF)
    (PROG (N NEWSOF NEWRSOF)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT RSOF) (RETURN NIL)))
        (PROGN
         (COND
          ((FREEOF SF (CAR RSOF)) (SETQ NEWRSOF (CONS (CAR RSOF) NEWRSOF)))
          (T (PROGN (SETQ N (ADD1 N)) (SETQ NEWSOF (CONS (CAR RSOF) NEWSOF)))))
         (SETQ RSOF (CDR RSOF)))
        (GO WHILELABEL))
      (RETURN (LIST N NEWSOF NEWRSOF)))) 
(PUT 'TRY 'NUMBER-OF-ARGS 13) 
(PUT 'TRY 'DEFINED-ON-LINE '443) 
(PUT 'TRY 'DEFINED-IN-FILE 'CRACK/CRSUBSYS.RED) 
(PUT 'TRY 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TRY
    (LOSOE OSOE LNSOE NSOE LRSOE RSOE LOSOF OSOF LNSOF NSOF LRSOF RSOF ALLF)
    (COND ((GREATERP LOSOF MAX_LOSOF) NIL)
          ((AND OSOE (GEQ LOSOE LOSOF)) (LIST (CONS OSOE OSOF)))
          ((OR (ZEROP LRSOE) (ZEROP LRSOF) (EQUAL LOSOF MAX_LOSOF)
               (LESSP (PLUS LOSOE LRSOE) LOSOF))
           NIL)
          ((OR (LESSP LRSOF LRSOE) (LESSP (DIFFERENCE MAX_LOSOF LOSOF) LRSOE))
           (PROG (LI SF NE)
             (SETQ COUNT_TRIES (PLUS COUNT_TRIES 2))
             (COND
              ((GREATERP (PLUS LOSOE LRSOE) LOSOF)
               (PROGN
                (COND
                 (TR_SUBSYS
                  (PROGN
                   (PROGN (PRIN2 (CAR RSOF)) (PRIN2 " goes into osof") NIL)
                   (TERPRI))))
                (SETQ SF (CONS (CAR RSOF) OSOF))
                (SETQ NE (ADDSOE RSOE SF ALLF))
                (SETQ LI
                        (TRY (PLUS LOSOE (CAR NE)) (APPEND OSOE (CADR NE))
                         LNSOE NSOE (DIFFERENCE LRSOE (CAR NE)) (CADDR NE)
                         (PLUS LOSOF 1) SF LNSOF NSOF (DIFFERENCE LRSOF 1)
                         (CDR RSOF) ALLF)))))
             (COND
              (TR_SUBSYS
               (PROGN
                (PROGN (PRIN2 (CAR RSOF)) (PRIN2 " goes into nsof") NIL)
                (TERPRI))))
             (SETQ NE (ADDNSOE RSOE (LIST (CAR RSOF))))
             (RETURN
              (APPEND LI
                      (TRY LOSOE OSOE (PLUS LNSOE (CAR NE))
                       (APPEND NSOE (CADR NE)) (DIFFERENCE LRSOE (CAR NE))
                       (CADDR NE) LOSOF OSOF (PLUS LNSOF 1)
                       (CONS (CAR RSOF) NSOF) (DIFFERENCE LRSOF 1) (CDR RSOF)
                       ALLF)))))
          (T
           (PROG (LI SF NE NF)
             (SETQ COUNT_TRIES (PLUS COUNT_TRIES 2))
             (SETQ SF
                     (SETDIFF (INTERSECTION ALLF (GET (CAR RSOE) 'ALLVARFCTS))
                              OSOF))
             (COND
              ((GREATERP (PLUS LOSOE LRSOE) LOSOF)
               (PROGN
                (COND
                 (TR_SUBSYS
                  (PROGN
                   (PROGN (PRIN2 (CAR RSOE)) (PRIN2 " goes into nsoe:") NIL)
                   (TERPRI))))
                (SETQ NE (ADDNSOE RSOE SF))
                (COND
                 ((AND (EQUAL (LENGTH SF) 1) (FREEOF NSOF (CAR SF)))
                  (SETQ LI
                          (TRY LOSOE OSOE (PLUS LNSOE (CAR NE))
                           (APPEND NSOE (CADR NE)) (DIFFERENCE LRSOE (CAR NE))
                           (CADDR NE) LOSOF OSOF (ADD1 LNSOF)
                           (CONS (CAR SF) NSOF) (SUB1 LRSOF) (SETDIFF RSOF SF)
                           ALLF)))
                 (T
                  (SETQ LI
                          (TRY LOSOE OSOE (PLUS LNSOE (CAR NE))
                           (APPEND NSOE (CADR NE)) (DIFFERENCE LRSOE (CAR NE))
                           (CADDR NE) LOSOF OSOF LNSOF NSOF LRSOF RSOF
                           ALLF)))))))
             (COND
              (TR_SUBSYS
               (PROGN
                (PROGN (PRIN2 (CAR RSOE)) (PRIN2 " goes into osoe:") NIL)
                (TERPRI))))
             (SETQ NE (ADDSOE RSOE (APPEND SF OSOF) ALLF))
             (SETQ NF (ADDSOF RSOF SF))
             (RETURN
              (APPEND LI
                      (TRY (PLUS LOSOE (CAR NE)) (APPEND OSOE (CADR NE)) LNSOE
                       NSOE (DIFFERENCE LRSOE (CAR NE)) (CADDR NE)
                       (PLUS LOSOF (CAR NF)) (APPEND OSOF (CADR NF)) LNSOF NSOF
                       (DIFFERENCE LRSOF (CAR NF)) (CADDR NF) ALLF))))))) 
(ENDMODULE) 
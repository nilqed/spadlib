(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DUMMYCNT)) 
(FLUID '(G_DVNAMES G_DVBASE G_SC_VE G_INIT_STREE G_SKIP_TO_LEVEL *DISTRIBUTE)) 
(PUT 'AD_SPLITNAME 'NUMBER-OF-ARGS 1) 
(PUT 'AD_SPLITNAME 'DEFINED-ON-LINE '32) 
(PUT 'AD_SPLITNAME 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'AD_SPLITNAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AD_SPLITNAME (U)
    (COND
     ((IDP U)
      (PROG (UU NN)
        (SETQ UU (REVERSE (EXPLODE U)))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND UU (|CHARNUMP:| (CAR UU)))) (RETURN NIL)))
          (PROGN (SETQ NN (CONS (CAR UU) NN)) (SETQ UU (CDR UU)) NIL)
          (GO WHILELABEL))
        (COND (UU (SETQ UU (INTERN (COMPRESS (REVERSE UU))))))
        (COND (NN (SETQ NN (COMPRESS NN))))
        (RETURN (CONS UU NN)))))) 
(PUT 'ANTICOM_ASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'ANTICOM_ASSOC 'DEFINED-ON-LINE '46) 
(PUT 'ANTICOM_ASSOC 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ANTICOM_ASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ANTICOM_ASSOC (U V)
    (PROG (NEXT_CELL)
      (COND ((NULL V) (RETURN NIL))
            ((EQUAL U (CAAR V)) (RETURN (CONS 1 (CAR V))))
            (T
             (PROGN
              (SETQ NEXT_CELL (ANTICOM_ASSOC U (CDR V)))
              (COND ((NULL NEXT_CELL) (RETURN NIL)))
              (COND
               ((ODDP (LENGTH (CDAR V)))
                (RPLACA NEXT_CELL (MINUS (CAR NEXT_CELL)))))
              (RETURN NEXT_CELL)
              NIL))))) 
(PUT 'AD_SIGNSORT 'NUMBER-OF-ARGS 2) 
(PUT 'AD_SIGNSORT 'DEFINED-ON-LINE '63) 
(PUT 'AD_SIGNSORT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'AD_SIGNSORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AD_SIGNSORT (L FN)
    (PROG (TOSORT SORTED INSERTL DIG THESIGN)
      (SETQ THESIGN 0)
      (SETQ TOSORT (COPY L))
      (SETQ THESIGN 1)
      (SETQ SORTED NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT TOSORT) (RETURN NIL)))
        (COND
         ((NULL SORTED)
          (PROGN
           (SETQ SORTED (CONS (CAR TOSORT) SORTED))
           (SETQ TOSORT (CDR TOSORT))
           NIL))
         ((EQUAL (CAR TOSORT) (CAR SORTED))
          (PROGN (SETQ THESIGN 0) (SETQ SORTED (SETQ TOSORT NIL)) NIL))
         ((APPLY FN (LIST (CAR SORTED) (CAR TOSORT)))
          (PROGN
           (SETQ SORTED (CONS (CAR TOSORT) SORTED))
           (SETQ TOSORT (CDR TOSORT))
           NIL))
         (T
          (PROGN
           (SETQ THESIGN (MINUS THESIGN))
           (SETQ INSERTL SORTED)
           (SETQ DIG T)
           (PROG ()
            WHILELABEL
             (COND ((NOT DIG) (RETURN NIL)))
             (COND ((NULL (CDR INSERTL)) (SETQ DIG NIL))
                   ((EQUAL (CADR INSERTL) (CAR TOSORT))
                    (PROGN
                     (SETQ INSERTL (LIST NIL))
                     (SETQ DIG NIL)
                     (SETQ THESIGN 0)
                     (SETQ SORTED (SETQ TOSORT NIL))
                     NIL))
                   ((NOT (APPLY FN (LIST (CADR INSERTL) (CAR TOSORT))))
                    (PROGN
                     (SETQ INSERTL (CDR INSERTL))
                     (SETQ THESIGN (MINUS THESIGN))
                     NIL))
                   (T (SETQ DIG NIL)))
             (GO WHILELABEL))
           (COND
            (TOSORT
             (PROGN
              (RPLACD INSERTL (CONS (CAR TOSORT) (CDR INSERTL)))
              (SETQ TOSORT (CDR TOSORT))
              NIL)))
           NIL)))
        (GO WHILELABEL))
      (RETURN (CONS THESIGN (REVERSE SORTED))))) 
(PUT 'CDR_SORT 'NUMBER-OF-ARGS 2) 
(PUT 'CDR_SORT 'DEFINED-ON-LINE '116) 
(PUT 'CDR_SORT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CDR_SORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CDR_SORT (LST FN)
    (PROG (TOSORT SORTED INSERTL)
      (SETQ TOSORT LST)
      (PROG ()
       WHILELABEL
        (COND ((NOT TOSORT) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (NULL SORTED) (APPLY FN (LIST (CDAR SORTED) (CDAR TOSORT))))
           (PROGN
            (SETQ SORTED (CONS (CAR TOSORT) SORTED))
            (SETQ TOSORT (CDR TOSORT))
            NIL))
          (T
           (PROGN
            (SETQ INSERTL SORTED)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (CDR INSERTL)
                      (NOT (APPLY FN (LIST (CDADR INSERTL) (CDAR TOSORT))))))
                (RETURN NIL)))
              (SETQ INSERTL (CDR INSERTL))
              (GO WHILELABEL))
            (RPLACD INSERTL (CONS (CAR TOSORT) (CDR INSERTL)))
            (SETQ TOSORT (CDR TOSORT))))))
        (GO WHILELABEL))
      (RETURN (REVERSE SORTED)))) 
(PUT 'CDR_SIGNSORT 'NUMBER-OF-ARGS 2) 
(PUT 'CDR_SIGNSORT 'DEFINED-ON-LINE '139) 
(PUT 'CDR_SIGNSORT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CDR_SIGNSORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CDR_SIGNSORT (L FN)
    (PROG (TOSORT SORTED INSERTL DIG THESIGN)
      (SETQ THESIGN 0)
      (SETQ TOSORT (COPY L))
      (SETQ THESIGN 1)
      (SETQ SORTED NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT TOSORT) (RETURN NIL)))
        (COND
         ((NULL SORTED)
          (PROGN
           (SETQ SORTED (CONS (CAR TOSORT) SORTED))
           (SETQ TOSORT (CDR TOSORT))
           NIL))
         ((EQUAL (CDAR TOSORT) (CDAR SORTED))
          (PROGN (SETQ THESIGN 0) (SETQ SORTED (SETQ TOSORT NIL)) NIL))
         ((APPLY FN (LIST (CDAR SORTED) (CDAR TOSORT)))
          (PROGN
           (SETQ SORTED (CONS (CAR TOSORT) SORTED))
           (SETQ TOSORT (CDR TOSORT))
           NIL))
         (T
          (PROGN
           (SETQ THESIGN (MINUS THESIGN))
           (SETQ INSERTL SORTED)
           (SETQ DIG T)
           (PROG ()
            WHILELABEL
             (COND ((NOT DIG) (RETURN NIL)))
             (COND ((NULL (CDR INSERTL)) (SETQ DIG NIL))
                   ((EQUAL (CDADR INSERTL) (CDAR TOSORT))
                    (PROGN
                     (SETQ DIG NIL)
                     (SETQ THESIGN 0)
                     (SETQ SORTED (SETQ TOSORT NIL))
                     NIL))
                   ((NOT (APPLY FN (LIST (CDADR INSERTL) (CDAR TOSORT))))
                    (PROGN
                     (SETQ INSERTL (CDR INSERTL))
                     (SETQ THESIGN (MINUS THESIGN))
                     NIL))
                   (T (SETQ DIG NIL)))
             (GO WHILELABEL))
           (COND
            (TOSORT
             (PROGN
              (RPLACD INSERTL (CONS (CAR TOSORT) (CDR INSERTL)))
              (SETQ TOSORT (CDR TOSORT)))))
           NIL)))
        (GO WHILELABEL))
      (RETURN (CONS THESIGN (REVERSE SORTED))))) 
(PUT 'NUM_SIGNSORT 'NUMBER-OF-ARGS 1) 
(PUT 'NUM_SIGNSORT 'DEFINED-ON-LINE '191) 
(PUT 'NUM_SIGNSORT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'NUM_SIGNSORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_SIGNSORT (L) (AD_SIGNSORT L (FUNCTION (LAMBDA (X Y) (LEQ X Y))))) 
(PUT 'CONS_ORDP 'NUMBER-OF-ARGS 3) 
(PUT 'CONS_ORDP 'DEFINED-ON-LINE '194) 
(PUT 'CONS_ORDP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CONS_ORDP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONS_ORDP (U V FN)
    (COND ((NULL U) T) ((NULL V) NIL)
          ((PAIRP U)
           (COND
            ((PAIRP V)
             (COND ((EQUAL (CAR U) (CAR V)) (CONS_ORDP (CDR U) (CDR V) FN))
                   (T (CONS_ORDP (CAR U) (CAR V) FN))))
            (T NIL)))
          ((PAIRP V) T) (T (APPLY2 FN U V)))) 
(PUT 'ATOM_COMPARE 'NUMBER-OF-ARGS 2) 
(PUT 'ATOM_COMPARE 'DEFINED-ON-LINE '208) 
(PUT 'ATOM_COMPARE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ATOM_COMPARE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ATOM_COMPARE (U V)
    (COND ((NUMBERP U) (AND (NUMBERP V) (NOT (LESSP U V))))
          ((IDP V) (ORDERP U V)) (T (NUMBERP V)))) 
(PUT 'IDCONS_ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'IDCONS_ORDP 'DEFINED-ON-LINE '213) 
(PUT 'IDCONS_ORDP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'IDCONS_ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDCONS_ORDP (U V) (CONS_ORDP U V (FUNCTION ATOM_COMPARE))) 
(PUT 'SKP_ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'SKP_ORDP 'DEFINED-ON-LINE '216) 
(PUT 'SKP_ORDP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SKP_ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SKP_ORDP (U V) (CONS_ORDP (CAR U) (CAR V) (FUNCTION ATOM_COMPARE))) 
(PUT 'NUMLIST_ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'NUMLIST_ORDP 'DEFINED-ON-LINE '219) 
(PUT 'NUMLIST_ORDP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'NUMLIST_ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUMLIST_ORDP (U V) (CONS_ORDP U V (FUNCTION (LAMBDA (X Y) (LEQ X Y))))) 
(PUT 'AD_NUMSORT 'NUMBER-OF-ARGS 1) 
(PUT 'AD_NUMSORT 'DEFINED-ON-LINE '222) 
(PUT 'AD_NUMSORT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'AD_NUMSORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AD_NUMSORT (L) (SORT L (FUNCTION (LAMBDA (X Y) (LEQ X Y))))) 
(PUT 'SC_KERN 'NUMBER-OF-ARGS 1) 
(PUT 'SC_KERN 'DEFINED-ON-LINE '227) 
(PUT 'SC_KERN 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SC_KERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SC_KERN (IND) (CADDR (GETV G_SC_VE (DIFFERENCE IND 1)))) 
(PUT 'SC_REP 'NUMBER-OF-ARGS 1) 
(PUT 'SC_REP 'DEFINED-ON-LINE '230) 
(PUT 'SC_REP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SC_REP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SC_REP (IND) (CADR (GETV G_SC_VE (DIFFERENCE IND 1)))) 
(PUT 'SC_DESC 'NUMBER-OF-ARGS 1) 
(PUT 'SC_DESC 'DEFINED-ON-LINE '233) 
(PUT 'SC_DESC 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SC_DESC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SC_DESC (IND) (CAR (GETV G_SC_VE (DIFFERENCE IND 1)))) 
(PUT 'DUMMYP 'NUMBER-OF-ARGS 1) 
(PUT 'DUMMYP 'DEFINED-ON-LINE '236) 
(PUT 'DUMMYP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DUMMYP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUMMYP (VAR)
    (PROG (VARSPLIT COUNT RES)
      (SETQ COUNT 0)
      (SETQ RES 0)
      (COND ((NOT (IDP VAR)) (RETURN NIL)))
      (SETQ COUNT 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ COUNT (UPBVE G_DVNAMES))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL VAR (GETV G_DVNAMES (DIFFERENCE COUNT 1)))
           (PROGN (SETQ RES COUNT) (SETQ COUNT (PLUS (UPBVE G_DVNAMES) 1))))
          (T (SETQ COUNT (PLUS COUNT 1))))
         NIL)
        (GO WHILELABEL))
      (COND
       ((EQ RES 0)
        (PROGN
         (SETQ VARSPLIT (AD_SPLITNAME VAR))
         (COND ((EQ (CAR VARSPLIT) G_DVBASE) (RETURN (CDR VARSPLIT))))))
       (T (RETURN RES))))) 
(PUT 'DV_IND2VAR 'NUMBER-OF-ARGS 1) 
(PUT 'DV_IND2VAR 'DEFINED-ON-LINE '260) 
(PUT 'DV_IND2VAR 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_IND2VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_IND2VAR (IND)
    (COND ((LEQ IND (UPBVE G_DVNAMES)) (GETV G_DVNAMES (DIFFERENCE IND 1)))
          (T (MKID G_DVBASE IND)))) 
(PUT 'SC_REPKERN 'NUMBER-OF-ARGS 2) 
(PUT 'SC_REPKERN 'DEFINED-ON-LINE '268) 
(PUT 'SC_REPKERN 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SC_REPKERN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SC_REPKERN (S_CELL N)
    (COND
     ((EQ (CAR S_CELL) '*)
      (PROG (KERN REST NEXT_REST HEAD REP)
        (SETQ HEAD 0)
        (SETQ REP 0)
        (SETQ REST (CDR S_CELL))
        (SETQ REP 0)
        (PROG ()
         WHILELABEL
          (COND ((NOT REST) (RETURN NIL)))
          (PROGN
           (SETQ HEAD (CAR REST))
           (SETQ KERN (CONS (LIST HEAD) KERN))
           (SETQ REST (CDR REST))
           (SETQ NEXT_REST NIL)
           (SETQ REP (PLUS (TIMES REP 2) 1))
           (PROG (ELT)
             (SETQ ELT REST)
            LAB
             (COND ((NULL ELT) (RETURN NIL)))
             ((LAMBDA (ELT)
                (PROGN
                 (COND ((EQ ELT HEAD) (SETQ REP (PLUS (TIMES REP 2) 1)))
                       (T
                        (PROGN
                         (SETQ REP (TIMES REP 2))
                         (SETQ NEXT_REST (CONS ELT NEXT_REST)))))))
              (CAR ELT))
             (SETQ ELT (CDR ELT))
             (GO LAB))
           (SETQ REST (REVERSE NEXT_REST))
           NIL)
          (GO WHILELABEL))
        (RETURN (LIST REP (PA_LIST2VECT (REVERSE KERN) N)))))
     (T
      (PROG (COUNT REPLIST REP KERN LAST_COUNT)
        (SETQ LAST_COUNT 0)
        (SETQ S_CELL (CDR S_CELL))
        (PROG (ELT)
          (SETQ ELT S_CELL)
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (COND
              ((SETQ COUNT (ASSOC ELT REPLIST))
               (RPLACD COUNT (PLUS (CDR COUNT) 1)))
              (T (SETQ REPLIST (CONS (CONS ELT 1) REPLIST)))))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))
        (SETQ REPLIST
                (SORT REPLIST (FUNCTION (LAMBDA (X Y) (LEQ (CDR X) (CDR Y))))))
        (SETQ LAST_COUNT 0)
        (PROG (ELT)
          (SETQ ELT REPLIST)
         LAB
          (COND ((NULL ELT) (RETURN NIL)))
          ((LAMBDA (ELT)
             (COND
              ((NEQ (CDR ELT) LAST_COUNT)
               (PROGN
                (SETQ REP (CONS (CONS (CDR ELT) 1) REP))
                (SETQ KERN (CONS (LIST (CAR ELT)) KERN))
                (SETQ LAST_COUNT (CDR ELT))
                NIL))
              (T
               (PROGN
                (RPLACD (CAR REP) (PLUS (CDAR REP) 1))
                (RPLACA KERN (CONS (CAR ELT) (CAR KERN)))))))
           (CAR ELT))
          (SETQ ELT (CDR ELT))
          (GO LAB))
        (RETURN (LIST REP (PA_LIST2VECT KERN N))))))) 
(PUT 'PA_LIST2VECT 'NUMBER-OF-ARGS 2) 
(PUT 'PA_LIST2VECT 'DEFINED-ON-LINE '323) 
(PUT 'PA_LIST2VECT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PA_LIST2VECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PA_LIST2VECT (PA N)
    (PROG (VE REPS ABS)
      (SETQ ABS 0)
      (SETQ VE (MKVECT (DIFFERENCE N 1)))
      (PROG (CELL)
        (SETQ CELL PA)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (PROGN
            (SETQ REPS (CONS (EVAL (CONS 'MIN CELL)) REPS))
            (PROG (ELT)
              (SETQ ELT CELL)
             LAB
              (COND ((NULL ELT) (RETURN NIL)))
              ((LAMBDA (ELT) (PUTV VE (DIFFERENCE ELT 1) (CAR REPS)))
               (CAR ELT))
              (SETQ ELT (CDR ELT))
              (GO LAB))
            NIL))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N COUNT)) (RETURN NIL)))
        (PROGN
         (COND
          ((NULL (GETV VE (DIFFERENCE COUNT 1)))
           (PROGN
            (COND
             ((EQUAL ABS 0)
              (PROGN (SETQ ABS COUNT) (SETQ REPS (CONS ABS REPS)))))
            (PUTV VE (DIFFERENCE COUNT 1) ABS)))))
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (RETURN (CONS (REVERSE REPS) VE)))) 
(PUT 'PA_PART2LIST 'NUMBER-OF-ARGS 1) 
(PUT 'PA_PART2LIST 'DEFINED-ON-LINE '347) 
(PUT 'PA_PART2LIST 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PA_PART2LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PA_PART2LIST (P)
    (PROG (VE LEN REP)
      (SETQ LEN 0)
      (SETQ REP 0)
      (SETQ LEN (UPBVE (CDR P)))
      (SETQ VE (MKVECT (DIFFERENCE LEN 1)))
      (PROG (COUNT)
        (SETQ COUNT LEN)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 COUNT))) (RETURN NIL)))
        (PROGN
         (SETQ REP (GETV (CDR P) (DIFFERENCE COUNT 1)))
         (PUTV VE (DIFFERENCE REP 1) (CONS COUNT (GETV VE (DIFFERENCE REP 1))))
         NIL)
        (SETQ COUNT (PLUS2 COUNT (MINUS 1)))
        (GO LAB))
      (RETURN
       (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
         (SETQ COUNT (CAR P))
        STARTOVER
         (COND ((NULL COUNT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (COUNT) (COPY (GETV VE (DIFFERENCE COUNT 1))))
                  (CAR COUNT)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ COUNT (CDR COUNT))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL COUNT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (COUNT) (COPY (GETV VE (DIFFERENCE COUNT 1))))
                  (CAR COUNT)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ COUNT (CDR COUNT))
         (GO LOOPLABEL))))) 
(PUT 'PA_VECT2LIST 'NUMBER-OF-ARGS 1) 
(PUT 'PA_VECT2LIST 'DEFINED-ON-LINE '361) 
(PUT 'PA_VECT2LIST 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PA_VECT2LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PA_VECT2LIST (PA)
    (PROG (VE COUNT REP)
      (SETQ COUNT 0)
      (SETQ REP 0)
      (SETQ VE (MKVECT (DIFFERENCE (UPBVE (CDR PA)) 1)))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBVE (CDR PA)) COUNT)) (RETURN NIL)))
        (PROGN
         (SETQ REP (GETV (CDR PA) (DIFFERENCE COUNT 1)))
         (PUTV VE (DIFFERENCE REP 1) (CONS COUNT (GETV VE (DIFFERENCE REP 1))))
         NIL)
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (RETURN
       (PROG (REP FORALL-RESULT FORALL-ENDPTR)
         (SETQ REP (CAR PA))
         (COND ((NULL REP) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (REP) (ORDN (GETV VE (DIFFERENCE REP 1))))
                           (CAR REP))
                          NIL)))
        LOOPLABEL
         (SETQ REP (CDR REP))
         (COND ((NULL REP) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (REP) (ORDN (GETV VE (DIFFERENCE REP 1))))
                   (CAR REP))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'PA_COINC_SPLIT 'NUMBER-OF-ARGS 2) 
(PUT 'PA_COINC_SPLIT 'DEFINED-ON-LINE '373) 
(PUT 'PA_COINC_SPLIT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PA_COINC_SPLIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PA_COINC_SPLIT (P1 P2)
    (PROG (VE1 VE2 CURSPLIT SPLIT_ALIST SPLIT_INFO COINC SPLIT COUNT PLENGTH)
      (SETQ COUNT 0)
      (SETQ PLENGTH 0)
      (SETQ PLENGTH (UPBVE (CDR P1)))
      (SETQ VE1 (MKVECT (DIFFERENCE PLENGTH 1)))
      (SETQ VE2 (MKVECT (DIFFERENCE PLENGTH 1)))
      (SETQ SPLIT (MKVECT (DIFFERENCE PLENGTH 1)))
      (SETQ COUNT 0)
      (PROG (REP)
        (SETQ REP (CAR P1))
       LAB
        (COND ((NULL REP) (RETURN NIL)))
        ((LAMBDA (REP)
           (PROGN
            (SETQ COUNT (PLUS COUNT 1))
            (PUTV VE1 (DIFFERENCE REP 1) COUNT)))
         (CAR REP))
        (SETQ REP (CDR REP))
        (GO LAB))
      (SETQ COUNT 0)
      (PROG (REP)
        (SETQ REP (CAR P2))
       LAB
        (COND ((NULL REP) (RETURN NIL)))
        ((LAMBDA (REP)
           (PROGN
            (SETQ COUNT (PLUS COUNT 1))
            (PUTV VE2 (DIFFERENCE REP 1) COUNT)))
         (CAR REP))
        (SETQ REP (CDR REP))
        (GO LAB))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE PLENGTH COUNT)) (RETURN NIL)))
        (PROGN
         (SETQ CURSPLIT
                 (CONS
                  (GETV VE1
                        (DIFFERENCE (GETV (CDR P1) (DIFFERENCE COUNT 1)) 1))
                  (GETV VE2
                        (DIFFERENCE (GETV (CDR P2) (DIFFERENCE COUNT 1)) 1))))
         (COND
          ((SETQ SPLIT_INFO (ASSOC CURSPLIT SPLIT_ALIST))
           (PROGN
            (RPLACD (CDR SPLIT_INFO) (PLUS (CDDR SPLIT_INFO) 1))
            (PUTV SPLIT (DIFFERENCE COUNT 1) (CADR SPLIT_INFO))))
          (T
           (PROGN
            (SETQ SPLIT_INFO (CONS CURSPLIT (CONS COUNT 1)))
            (SETQ SPLIT_ALIST (CONS SPLIT_INFO SPLIT_ALIST))
            (PUTV SPLIT (DIFFERENCE COUNT 1) COUNT)))))
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (SETQ SPLIT_ALIST
              (SORT SPLIT_ALIST
                    (FUNCTION
                     (LAMBDA (X Y)
                       (COND ((LESSP (CAAR X) (CAAR Y)) T)
                             ((LESSP (CAAR Y) (CAAR X)) NIL)
                             (T (LEQ (CDAR X) (CDAR Y))))))))
      (SETQ SPLIT
              (CONS
               (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ CELL SPLIT_ALIST)
                 (COND ((NULL CELL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (CELL) (CADR CELL)) (CAR CELL))
                                       NIL)))
                LOOPLABEL
                 (SETQ CELL (CDR CELL))
                 (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (CELL) (CADR CELL)) (CAR CELL)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               SPLIT))
      (SETQ COINC
              (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                (SETQ CELL SPLIT_ALIST)
                (COND ((NULL CELL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CELL) (CONS (CAR CELL) (CDDR CELL)))
                                  (CAR CELL))
                                 NIL)))
               LOOPLABEL
                (SETQ CELL (CDR CELL))
                (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CELL) (CONS (CAR CELL) (CDDR CELL)))
                          (CAR CELL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS COINC SPLIT)))) 
(PUT 'ST_FLATTEN 'NUMBER-OF-ARGS 1) 
(PUT 'ST_FLATTEN 'DEFINED-ON-LINE '423) 
(PUT 'ST_FLATTEN 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_FLATTEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ST_FLATTEN (STREE)
    (COND ((NUMBERP (CADR STREE)) (CDR STREE))
          (T
           (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
             (SETQ ELT (CDR STREE))
            STARTOVER
             (COND ((NULL ELT) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (ELT) (COPY (ST_FLATTEN ELT))) (CAR ELT)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ ELT (CDR ELT))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL ELT) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (ELT) (COPY (ST_FLATTEN ELT))) (CAR ELT)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ ELT (CDR ELT))
             (GO LOOPLABEL))))) 
(PUT 'ST_EXTRACT_SYMCELLS 'NUMBER-OF-ARGS 2) 
(PUT 'ST_EXTRACT_SYMCELLS 'DEFINED-ON-LINE '429) 
(PUT 'ST_EXTRACT_SYMCELLS 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_EXTRACT_SYMCELLS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ST_EXTRACT_SYMCELLS (STREE MAXIND)
    (PROG (VE SYMCELLS COUNT)
      (SETQ COUNT 0)
      (COND ((NULL STREE) (RETURN (CONS NIL (MKVECT (DIFFERENCE 0 1))))))
      (SETQ SYMCELLS (ST_EXTRACT_SYMCELLS1 (ST_CONSOLIDATE STREE) NIL 1))
      (SETQ STREE (CAR SYMCELLS))
      (COND ((NOT (LISTP STREE)) (SETQ STREE (LIST '* STREE))))
      (SETQ SYMCELLS (CADR SYMCELLS))
      (SETQ VE (MKVECT (DIFFERENCE (LENGTH SYMCELLS) 1)))
      (SETQ COUNT (UPBVE VE))
      (PROG ()
       WHILELABEL
        (COND ((NOT SYMCELLS) (RETURN NIL)))
        (PROGN
         (PUTV VE (DIFFERENCE COUNT 1)
               (CONS (CAR SYMCELLS) (SC_REPKERN (CAR SYMCELLS) MAXIND)))
         (SETQ SYMCELLS (CDR SYMCELLS))
         (SETQ COUNT (DIFFERENCE COUNT 1)))
        (GO WHILELABEL))
      (RETURN (CONS (ST_CONSOLIDATE STREE) VE)))) 
(PUT 'ST_EXTRACT_SYMCELLS1 'NUMBER-OF-ARGS 3) 
(PUT 'ST_EXTRACT_SYMCELLS1 'DEFINED-ON-LINE '449) 
(PUT 'ST_EXTRACT_SYMCELLS1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_EXTRACT_SYMCELLS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ST_EXTRACT_SYMCELLS1 (STREE SYMCELLS COUNT)
    (PROG (RES NEW_STREE)
      (COND
       ((NOT (LISTP (CADR STREE)))
        (RETURN (LIST COUNT (CONS STREE SYMCELLS) (PLUS COUNT 1))))
       (T
        (PROGN
         (SETQ NEW_STREE
                 (CONS (CAR STREE)
                       (PROG (INNER_STREE FORALL-RESULT FORALL-ENDPTR)
                         (SETQ INNER_STREE (CDR STREE))
                         (COND ((NULL INNER_STREE) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (INNER_STREE)
                                             (PROGN
                                              (SETQ RES
                                                      (ST_EXTRACT_SYMCELLS1
                                                       INNER_STREE SYMCELLS
                                                       COUNT))
                                              (SETQ SYMCELLS (CADR RES))
                                              (SETQ COUNT (CADDR RES))
                                              (COND
                                               ((NUMBERP (CAR RES))
                                                (LIST '* (CAR RES)))
                                               (T (CAR RES)))))
                                           (CAR INNER_STREE))
                                          NIL)))
                        LOOPLABEL
                         (SETQ INNER_STREE (CDR INNER_STREE))
                         (COND ((NULL INNER_STREE) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (INNER_STREE)
                                     (PROGN
                                      (SETQ RES
                                              (ST_EXTRACT_SYMCELLS1 INNER_STREE
                                               SYMCELLS COUNT))
                                      (SETQ SYMCELLS (CADR RES))
                                      (SETQ COUNT (CADDR RES))
                                      (COND
                                       ((NUMBERP (CAR RES))
                                        (LIST '* (CAR RES)))
                                       (T (CAR RES)))))
                                   (CAR INNER_STREE))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (RETURN (LIST NEW_STREE SYMCELLS COUNT))))))) 
(PUT 'ST_SIGNCHANGE 'NUMBER-OF-ARGS 2) 
(PUT 'ST_SIGNCHANGE 'DEFINED-ON-LINE '470) 
(PUT 'ST_SIGNCHANGE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_SIGNCHANGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ST_SIGNCHANGE (VE1 VE2)
    (TIMES (CAR (ST_SIGNCHANGE1 G_INIT_STREE (VECT2LIST VE1)))
           (CAR (ST_SIGNCHANGE1 G_INIT_STREE (VECT2LIST VE2))))) 
(PUT 'ST_SIGNCHANGE1 'NUMBER-OF-ARGS 2) 
(PUT 'ST_SIGNCHANGE1 'DEFINED-ON-LINE '474) 
(PUT 'ST_SIGNCHANGE1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_SIGNCHANGE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ST_SIGNCHANGE1 (STREE ELTLIST)
    (PROG (LEVLIST ELT_LEVLIST SUBSIGN THE_SIGN)
      (SETQ THE_SIGN 0)
      (SETQ THE_SIGN 1)
      (SETQ LEVLIST
              (PROG (CHILD FORALL-RESULT FORALL-ENDPTR)
                (SETQ CHILD (CDR STREE))
                (COND ((NULL CHILD) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CHILD)
                                    (COND ((NUMBERP CHILD) CHILD)
                                          (T
                                           (PROGN
                                            (SETQ SUBSIGN
                                                    (ST_SIGNCHANGE1 CHILD
                                                     ELTLIST))
                                            (SETQ THE_SIGN
                                                    (TIMES THE_SIGN
                                                           (CAR SUBSIGN)))
                                            (CDR SUBSIGN)))))
                                  (CAR CHILD))
                                 NIL)))
               LOOPLABEL
                (SETQ CHILD (CDR CHILD))
                (COND ((NULL CHILD) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CHILD)
                            (COND ((NUMBERP CHILD) CHILD)
                                  (T
                                   (PROGN
                                    (SETQ SUBSIGN
                                            (ST_SIGNCHANGE1 CHILD ELTLIST))
                                    (SETQ THE_SIGN
                                            (TIMES THE_SIGN (CAR SUBSIGN)))
                                    (CDR SUBSIGN)))))
                          (CAR CHILD))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NOT (CDR LEVLIST)) (RETURN (CONS THE_SIGN (CAR LEVLIST)))))
      (SETQ ELT_LEVLIST ELTLIST)
      (COND
       ((MEMBER (CAR ELTLIST) LEVLIST) (SETQ ELT_LEVLIST (CONS 0 ELT_LEVLIST)))
       (T
        (PROG ()
         WHILELABEL
          (COND ((NOT (NOT (MEMBER (CADR ELT_LEVLIST) LEVLIST))) (RETURN NIL)))
          (SETQ ELT_LEVLIST (CDR ELT_LEVLIST))
          (GO WHILELABEL))))
      (COND
       ((AND (EQ (CAR STREE) '-) (NOT (PERMP LEVLIST (CDR ELT_LEVLIST))))
        (SETQ THE_SIGN (MINUS THE_SIGN))))
      (RPLACD ELT_LEVLIST (PNTH (CDR ELT_LEVLIST) (LENGTH LEVLIST)))
      (RETURN (CONS THE_SIGN (CADR ELT_LEVLIST))))) 
(PUT 'ST_SORTTREE 'NUMBER-OF-ARGS 3) 
(PUT 'ST_SORTTREE 'DEFINED-ON-LINE '504) 
(PUT 'ST_SORTTREE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_SORTTREE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ST_SORTTREE (STREE VE FN) (CDR (ST_SORTTREE1 STREE VE FN))) 
(PUT 'ST_SORTTREE1 'NUMBER-OF-ARGS 3) 
(PUT 'ST_SORTTREE1 'DEFINED-ON-LINE '507) 
(PUT 'ST_SORTTREE1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_SORTTREE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ST_SORTTREE1 (STREE VE FN)
    (PROG (SCHILD VALLIST SORTED THESIGN TOSORT)
      (SETQ THESIGN 1)
      (COND
       ((NUMBERP (CADR STREE))
        (PROGN
         (COND
          ((EQ (CAR STREE) '*)
           (PROGN
            (SETQ VALLIST
                    (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELT (CDR STREE))
                      (COND ((NULL ELT) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELT)
                                          (GETV VE (DIFFERENCE ELT 1)))
                                        (CAR ELT))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELT (CDR ELT))
                      (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELT) (GETV VE (DIFFERENCE ELT 1)))
                                (CAR ELT))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (RETURN (CONS VALLIST (CONS 1 STREE))))))
         (SETQ TOSORT
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT (CDR STREE))
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (CONS ELT (GETV VE (DIFFERENCE ELT 1))))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT)
                               (CONS ELT (GETV VE (DIFFERENCE ELT 1))))
                             (CAR ELT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         NIL))
       (T
        (PROGN
         (COND
          ((EQ (CAR STREE) '*)
           (PROGN
            (PROG (CHILD)
              (SETQ CHILD (CDR STREE))
             LAB
              (COND ((NULL CHILD) (RETURN NIL)))
              ((LAMBDA (CHILD)
                 (COND
                  ((NEQ THESIGN 0)
                   (PROGN
                    (SETQ SCHILD (ST_SORTTREE1 CHILD VE FN))
                    (SETQ THESIGN (TIMES THESIGN (CADR SCHILD)))
                    (SETQ VALLIST (CONS (CAR SCHILD) VALLIST))
                    (SETQ SORTED (CONS (CDDR SCHILD) SORTED))
                    NIL))))
               (CAR CHILD))
              (SETQ CHILD (CDR CHILD))
              (GO LAB))
            (COND ((EQUAL THESIGN 0) (RETURN (CONS NIL (CONS 0 NIL))))
                  (T
                   (PROGN
                    (SETQ SORTED (REVERSE SORTED))
                    (SETQ VALLIST (REVERSE VALLIST))
                    (RETURN (CONS VALLIST (CONS THESIGN (CONS '* SORTED))))
                    NIL))))))
         (PROG (CHILD)
           (SETQ CHILD (CDR STREE))
          LAB
           (COND ((NULL CHILD) (RETURN NIL)))
           ((LAMBDA (CHILD)
              (COND
               ((NEQ THESIGN 0)
                (PROGN
                 (SETQ SCHILD (ST_SORTTREE1 CHILD VE FN))
                 (SETQ THESIGN (TIMES THESIGN (CADR SCHILD)))
                 (SETQ TOSORT (CONS (CONS (CDDR SCHILD) (CAR SCHILD)) TOSORT))
                 NIL))))
            (CAR CHILD))
           (SETQ CHILD (CDR CHILD))
           (GO LAB))
         NIL)))
      (COND ((EQUAL THESIGN 0) (RETURN (CONS NIL (CONS 0 NIL)))))
      (COND ((EQUAL (CAR STREE) '+) (SETQ TOSORT (CDR_SORT TOSORT FN)))
            (T
             (PROGN
              (SETQ TOSORT (CDR_SIGNSORT TOSORT FN))
              (COND ((EQUAL (CAR TOSORT) 0) (RETURN (CONS NIL (CONS 0 NIL))))
                    (T (SETQ THESIGN (TIMES THESIGN (CAR TOSORT)))))
              (SETQ TOSORT (CDR TOSORT))
              NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT TOSORT) (RETURN NIL)))
        (PROGN
         (SETQ SORTED (CONS (CAAR TOSORT) SORTED))
         (SETQ VALLIST (CONS (CDAR TOSORT) VALLIST))
         (SETQ TOSORT (CDR TOSORT))
         NIL)
        (GO WHILELABEL))
      (SETQ SORTED (CONS (CAR STREE) (REVERSE SORTED)))
      (SETQ VALLIST (REVERSE VALLIST))
      (RETURN (CONS VALLIST (CONS THESIGN SORTED))))) 
(PUT 'ST_AD_NUMSORTTREE 'NUMBER-OF-ARGS 1) 
(PUT 'ST_AD_NUMSORTTREE 'DEFINED-ON-LINE '573) 
(PUT 'ST_AD_NUMSORTTREE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_AD_NUMSORTTREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ST_AD_NUMSORTTREE (STREE)
    (PROG (SORTED)
      (SETQ SORTED (ST_AD_NUMSORTTREE1 STREE))
      (RETURN (CONS (CAR SORTED) (CADR SORTED))))) 
(PUT 'ST_AD_NUMSORTTREE1 'NUMBER-OF-ARGS 1) 
(PUT 'ST_AD_NUMSORTTREE1 'DEFINED-ON-LINE '579) 
(PUT 'ST_AD_NUMSORTTREE1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_AD_NUMSORTTREE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ST_AD_NUMSORTTREE1 (STREE)
    (PROG (SUBTREE CONTENTS TOSORT THESIGN)
      (SETQ THESIGN 0)
      (COND ((NUMBERP STREE) (RETURN (LIST 1 STREE STREE))))
      (SETQ THESIGN 1)
      (COND
       ((EQ (CAR STREE) '*)
        (PROGN
         (SETQ STREE
                 (CONS '*
                       (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                         (SETQ ELT (CDR STREE))
                         (COND ((NULL ELT) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (ELT)
                                             (PROGN
                                              (SETQ SUBTREE
                                                      (ST_AD_NUMSORTTREE1 ELT))
                                              (SETQ THESIGN
                                                      (TIMES THESIGN
                                                             (CAR SUBTREE)))
                                              (SETQ CONTENTS
                                                      (CONS (CDDR SUBTREE)
                                                            CONTENTS))
                                              (CADR SUBTREE)))
                                           (CAR ELT))
                                          NIL)))
                        LOOPLABEL
                         (SETQ ELT (CDR ELT))
                         (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELT)
                                     (PROGN
                                      (SETQ SUBTREE (ST_AD_NUMSORTTREE1 ELT))
                                      (SETQ THESIGN
                                              (TIMES THESIGN (CAR SUBTREE)))
                                      (SETQ CONTENTS
                                              (CONS (CDDR SUBTREE) CONTENTS))
                                      (CADR SUBTREE)))
                                   (CAR ELT))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ CONTENTS
                 (AD_NUMSORT
                  (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELT CONTENTS)
                   STARTOVER
                    (COND ((NULL ELT) (RETURN NIL)))
                    (SETQ FORALL-RESULT ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ ELT (CDR ELT))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ ELT (CDR ELT))
                    (GO LOOPLABEL))))
         (RETURN (CONS THESIGN (CONS STREE CONTENTS)))
         NIL)))
      (SETQ TOSORT
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (CDR STREE))
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (PROGN
                                     (SETQ SUBTREE (ST_AD_NUMSORTTREE1 ELT))
                                     (SETQ THESIGN
                                             (TIMES THESIGN (CAR SUBTREE)))
                                     (CDR SUBTREE)))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (PROGN
                             (SETQ SUBTREE (ST_AD_NUMSORTTREE1 ELT))
                             (SETQ THESIGN (TIMES THESIGN (CAR SUBTREE)))
                             (CDR SUBTREE)))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EQ (CAR STREE) '+)
        (PROGN
         (SETQ TOSORT (CDR_SORT TOSORT (FUNCTION NUMLIST_ORDP)))
         (SETQ TOSORT
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT TOSORT)
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (PROGN
                                        (SETQ CONTENTS
                                                (CONS (CDR ELT) CONTENTS))
                                        (CAR ELT)))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT)
                               (PROGN
                                (SETQ CONTENTS (CONS (CDR ELT) CONTENTS))
                                (CAR ELT)))
                             (CAR ELT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ CONTENTS
                 (AD_NUMSORT
                  (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELT (REVERSE CONTENTS))
                   STARTOVER
                    (COND ((NULL ELT) (RETURN NIL)))
                    (SETQ FORALL-RESULT ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ ELT (CDR ELT))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ ELT (CDR ELT))
                    (GO LOOPLABEL))))
         (RETURN (CONS THESIGN (CONS (CONS '+ TOSORT) CONTENTS)))
         NIL)))
      (COND
       ((EQ (CAR STREE) '-)
        (PROGN
         (SETQ TOSORT (CDR_SIGNSORT TOSORT (FUNCTION NUMLIST_ORDP)))
         (SETQ THESIGN (CAR TOSORT))
         (SETQ TOSORT
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT (CDR TOSORT))
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (PROGN
                                        (SETQ CONTENTS
                                                (CONS (CDR ELT) CONTENTS))
                                        (CAR ELT)))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT)
                               (PROGN
                                (SETQ CONTENTS (CONS (CDR ELT) CONTENTS))
                                (CAR ELT)))
                             (CAR ELT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ CONTENTS
                 (AD_NUMSORT
                  (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELT (REVERSE CONTENTS))
                   STARTOVER
                    (COND ((NULL ELT) (RETURN NIL)))
                    (SETQ FORALL-RESULT ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ ELT (CDR ELT))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR ((LAMBDA (ELT) ELT) (CAR ELT)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ ELT (CDR ELT))
                    (GO LOOPLABEL))))
         (RETURN (CONS THESIGN (CONS (CONS '- TOSORT) CONTENTS)))
         NIL))))) 
(PUT 'ST_CONSOLIDATE 'NUMBER-OF-ARGS 1) 
(PUT 'ST_CONSOLIDATE 'DEFINED-ON-LINE '627) 
(PUT 'ST_CONSOLIDATE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_CONSOLIDATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ST_CONSOLIDATE (STREE)
    (PROG (JOIN_CELLS CHILDREN TMP)
      (COND ((NULL STREE) (RETURN NIL)))
      (COND ((NUMBERP (CADR STREE)) (RETURN STREE)))
      (SETQ JOIN_CELLS T)
      (PROG (CHILD)
        (SETQ CHILD (REVERSE (CDR STREE)))
       LAB
        (COND ((NULL CHILD) (RETURN NIL)))
        ((LAMBDA (CHILD)
           (PROGN
            (SETQ TMP (ST_CONSOLIDATE CHILD))
            (COND
             (TMP
              (PROGN
               (COND ((CDDR TMP) (SETQ JOIN_CELLS NIL))
                     (T (SETQ TMP (LIST '* (CADR TMP)))))
               (SETQ CHILDREN (CONS TMP CHILDREN))
               NIL)))
            NIL))
         (CAR CHILD))
        (SETQ CHILD (CDR CHILD))
        (GO LAB))
      (COND
       (CHILDREN
        (PROGN
         (COND ((NULL (CDR CHILDREN)) (RETURN (CAR CHILDREN))))
         (COND
          (JOIN_CELLS
           (SETQ CHILDREN
                   (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                     (SETQ ELT CHILDREN)
                     (COND ((NULL ELT) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (ELT) (CADR ELT)) (CAR ELT))
                                      NIL)))
                    LOOPLABEL
                     (SETQ ELT (CDR ELT))
                     (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (ELT) (CADR ELT)) (CAR ELT)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (RETURN (CONS (CAR STREE) CHILDREN))))
       (T (RETURN NIL))))) 
(PUT 'DV_CAMBHEAD 'NUMBER-OF-ARGS 1) 
(PUT 'DV_CAMBHEAD 'DEFINED-ON-LINE '658) 
(PUT 'DV_CAMBHEAD 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_CAMBHEAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_CAMBHEAD (CAMB)
    (PROG ()
      (COND
       ((LISTP CAMB)
        (PROGN
         (COND
          ((MEMBER (CAR CAMB) (LIST 'EXPT 'MINUS))
           (RETURN (DV_CAMBHEAD (CADR CAMB)))))
         (COND ((LISTP CAMB) (RETURN (CAR CAMB))))
         NIL))))) 
(PUT 'DV_SKELHEAD 'NUMBER-OF-ARGS 1) 
(PUT 'DV_SKELHEAD 'DEFINED-ON-LINE '668) 
(PUT 'DV_SKELHEAD 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKELHEAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_SKELHEAD (SKELPAIR) (DV_CAMBHEAD (CAR SKELPAIR))) 
(PUT 'DV_SKELSPLIT 'NUMBER-OF-ARGS 1) 
(PUT 'DV_SKELSPLIT 'DEFINED-ON-LINE '671) 
(PUT 'DV_SKELSPLIT 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKELSPLIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_SKELSPLIT (CAMB)
    (PROG (SKEL STREE SUBSKELS COUNT IND MAXIND THESIGN)
      (SETQ COUNT 0)
      (SETQ IND 0)
      (SETQ MAXIND 0)
      (SETQ THESIGN 0)
      (SETQ THESIGN 1)
      (COND
       ((NOT (LISTP CAMB))
        (COND
         ((SETQ IND (DUMMYP CAMB))
          (RETURN (LIST 1 IND (CONS '~DV (LIST '* IND)))))
         (T (RETURN (LIST 1 0 (CONS CAMB NIL)))))))
      (SETQ STREE (GET (CAR CAMB) 'SYMTREE))
      (COND
       ((NOT STREE)
        (PROGN
         (SETQ STREE
                 (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ COUNT 1)
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH (CDR CAMB)) COUNT))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS COUNT NIL)))
                  LOOPLABEL
                   (SETQ COUNT (PLUS2 COUNT 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH (CDR CAMB)) COUNT))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS COUNT NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND ((FLAGP (CAR CAMB) 'SYMMETRIC) (SETQ STREE (CONS '+ STREE)))
               ((FLAGP (CAR CAMB) 'ANTISYMMETRIC) (SETQ STREE (CONS '- STREE)))
               (T (SETQ STREE (CONS '* STREE)))))))
      (SETQ SUBSKELS (MKVECT (DIFFERENCE (LENGTH (CDR CAMB)) 1)))
      (SETQ COUNT 0)
      (PROG (ARG)
        (SETQ ARG (CDR CAMB))
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (PROGN
            (SETQ COUNT (PLUS COUNT 1))
            (COND
             ((LISTP ARG) (PUTV SUBSKELS (DIFFERENCE COUNT 1) (CONS ARG NIL)))
             ((SETQ IND (DUMMYP ARG))
              (PROGN
               (SETQ MAXIND (MAX MAXIND IND))
               (PUTV SUBSKELS (DIFFERENCE COUNT 1) (CONS '~DV (LIST '* IND)))))
             (T (PUTV SUBSKELS (DIFFERENCE COUNT 1) (CONS ARG NIL))))
            NIL))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (SETQ STREE (ST_SORTTREE STREE SUBSKELS (FUNCTION SKP_ORDP)))
      (COND ((AND STREE (EQUAL (CAR STREE) 0)) (RETURN NIL)))
      (SETQ THESIGN (CAR STREE))
      (SETQ SKEL (DV_SKELSPLIT1 (CDR STREE) SUBSKELS))
      (SETQ STREE (ST_CONSOLIDATE (CDR SKEL)))
      (SETQ SKEL (CONS (CAR CAMB) (CAR SKEL)))
      (RETURN (LIST THESIGN MAXIND (CONS SKEL STREE))))) 
(PUT 'DV_SKELSPLIT1 'NUMBER-OF-ARGS 2) 
(PUT 'DV_SKELSPLIT1 'DEFINED-ON-LINE '715) 
(PUT 'DV_SKELSPLIT1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKELSPLIT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_SKELSPLIT1 (STREE SKELVE)
    (PROG (CELL_STREE CHILD_LIST CUR_CELL DV_STREE PART SKEL VE COUNT LEN)
      (SETQ COUNT 0)
      (SETQ LEN 0)
      (COND
       ((NUMBERP (CADR STREE))
        (PROGN
         (SETQ VE SKELVE)
         (SETQ CHILD_LIST (CDR STREE))
         (SETQ SKEL
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT (CDR STREE))
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (CAR (GETV VE (DIFFERENCE ELT 1))))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT) (CAR (GETV VE (DIFFERENCE ELT 1))))
                             (CAR ELT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         NIL))
       (T
        (PROGN
         (SETQ LEN (LENGTH (CDR STREE)))
         (SETQ VE (MKVECT (DIFFERENCE LEN 1)))
         (SETQ COUNT LEN)
         (PROG (CHILD)
           (SETQ CHILD (REVERSE (CDR STREE)))
          LAB
           (COND ((NULL CHILD) (RETURN NIL)))
           ((LAMBDA (CHILD)
              (PROGN
               (PUTV VE (DIFFERENCE COUNT 1) (DV_SKELSPLIT1 CHILD SKELVE))
               (SETQ SKEL (CONS (CAR (GETV VE (DIFFERENCE COUNT 1))) SKEL))
               (SETQ CHILD_LIST (CONS COUNT CHILD_LIST))
               (SETQ COUNT (DIFFERENCE COUNT 1))
               NIL))
            (CAR CHILD))
           (SETQ CHILD (CDR CHILD))
           (GO LAB))
         (SETQ SKEL
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT SKEL)
                  STARTOVER
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT ((LAMBDA (ELT) (COPY ELT)) (CAR ELT)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ ELT (CDR ELT))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR ((LAMBDA (ELT) (COPY ELT)) (CAR ELT)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ ELT (CDR ELT))
                   (GO LOOPLABEL)))
         NIL)))
      (COND
       ((EQ (CAR STREE) '*)
        (PROGN
         (PROG (ELT)
           (SETQ ELT (REVERSE CHILD_LIST))
          LAB
           (COND ((NULL ELT) (RETURN NIL)))
           ((LAMBDA (ELT)
              (COND
               ((CDR (GETV VE (DIFFERENCE ELT 1)))
                (SETQ DV_STREE
                        (CONS (CDR (GETV VE (DIFFERENCE ELT 1))) DV_STREE)))))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         (COND ((EQUAL (LENGTH DV_STREE) 1) (SETQ DV_STREE (CAR DV_STREE)))
               (DV_STREE (SETQ DV_STREE (CONS '* DV_STREE))))
         (RETURN (CONS SKEL DV_STREE))
         NIL)))
      (PROG (ELT)
        (SETQ ELT CHILD_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((NULL CUR_CELL)
             (SETQ CUR_CELL
                     (CONS (CAR (GETV VE (DIFFERENCE ELT 1)))
                           (LIST (CDR (GETV VE (DIFFERENCE ELT 1)))))))
            ((EQUAL (CAR (GETV VE (DIFFERENCE ELT 1))) (CAR CUR_CELL))
             (RPLACD CUR_CELL
                     (CONS (CDR (GETV VE (DIFFERENCE ELT 1))) (CDR CUR_CELL))))
            (T
             (PROGN
              (SETQ PART (CONS CUR_CELL PART))
              (SETQ CUR_CELL
                      (CONS (CAR (GETV VE (DIFFERENCE ELT 1)))
                            (LIST (CDR (GETV VE (DIFFERENCE ELT 1))))))
              NIL))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ PART (CONS CUR_CELL PART))
      (PROG (CELL)
        (SETQ CELL PART)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (COND
            ((CDR CELL)
             (PROGN
              (SETQ CELL_STREE (CONS (CAR STREE) (REVERSE (CDR CELL))))
              (SETQ DV_STREE (CONS CELL_STREE DV_STREE))))))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (COND ((NEQ (LENGTH DV_STREE) 1) (SETQ DV_STREE (CONS '* DV_STREE)))
            (T (SETQ DV_STREE (CAR DV_STREE))))
      (RETURN (CONS SKEL DV_STREE)))) 
(PUT 'NODUM_VARP 'NUMBER-OF-ARGS 1) 
(PUT 'NODUM_VARP 'DEFINED-ON-LINE '782) 
(PUT 'NODUM_VARP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'NODUM_VARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NODUM_VARP (U)
    (COND ((LISTP U) T)
          ((OR (FLAGP U 'DUMMY) (EQUAL (CAR (AD_SPLITNAME U)) G_DVBASE)
               (MEMBER U (LIST '~DV '~DVA)))
           NIL)
          (T T))) 
(PUT 'LIST_IS_ALL_FREE 'NUMBER-OF-ARGS 1) 
(PUT 'LIST_IS_ALL_FREE 'DEFINED-ON-LINE '794) 
(PUT 'LIST_IS_ALL_FREE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'LIST_IS_ALL_FREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIST_IS_ALL_FREE (U)
    (COND ((NULL U) T) ((NODUM_VARP (CAR U)) (LIST_IS_ALL_FREE (CDR U)))
          (T NIL))) 
(PUT 'DV_SKELPROD 'NUMBER-OF-ARGS 2) 
(PUT 'DV_SKELPROD 'DEFINED-ON-LINE '804) 
(PUT 'DV_SKELPROD 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKELPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_SKELPROD (SKLIST MAXIND)
    (PROG (SKFORM STREE SYMCELLS SKEL APAIR ANTICOM_ALIST COM_ALIST
           NONCOM_ALIST ACOM_ODD ACOM_EVEN IDVECT VARSKEL THE_SIGN COUNT)
      (SETQ THE_SIGN 0)
      (SETQ COUNT 0)
      (SETQ THE_SIGN 1)
      (PROG (SKELPAIR)
        (SETQ SKELPAIR SKLIST)
       LAB
        (COND ((NULL SKELPAIR) (RETURN NIL)))
        ((LAMBDA (SKELPAIR)
           (PROGN
            (SETQ SKEL (CAR SKELPAIR))
            (SETQ VARSKEL
                    (COND
                     ((LISTP SKEL)
                      (COND ((NEQ (CAR SKEL) 'EXPT) (CDR SKEL))))))
            (COND
             ((FLAGP (DV_SKELHEAD SKELPAIR) 'ANTICOM)
              (PROGN
               (COND
                ((SETQ APAIR (ANTICOM_ASSOC SKEL ANTICOM_ALIST))
                 (PROGN
                  (COND
                   ((MEMBER (CDR SKELPAIR) (CDDR APAIR)) (SETQ THE_SIGN 0))
                   (T (SETQ THE_SIGN (TIMES THE_SIGN (CAR APAIR)))))
                  (RPLACD (CDR APAIR) (CONS (CDR SKELPAIR) (CDDR APAIR)))))
                (T
                 (SETQ ANTICOM_ALIST
                         (CONS (CONS SKEL (LIST (CDR SKELPAIR)))
                               ANTICOM_ALIST))))
               NIL))
             ((FLAGP (DV_SKELHEAD SKELPAIR) 'NONCOM)
              (SETQ NONCOM_ALIST
                      (CONS (CONS SKEL (LIST (CDR SKELPAIR))) NONCOM_ALIST)))
             ((AND (OR (NULL (LIST_IS_ALL_FREE VARSKEL)) (ATOM SKEL))
                   (SETQ APAIR (ASSOC SKEL COM_ALIST)))
              (RPLACD APAIR (CONS (CDR SKELPAIR) (CDR APAIR))))
             (T
              (SETQ COM_ALIST
                      (CONS (CONS SKEL (LIST (CDR SKELPAIR))) COM_ALIST))))
            NIL))
         (CAR SKELPAIR))
        (SETQ SKELPAIR (CDR SKELPAIR))
        (GO LAB))
      (COND ((EQUAL THE_SIGN 0) (RETURN NIL)))
      (SETQ ANTICOM_ALIST
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT ANTICOM_ALIST)
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (CONS (CAR ELT) (REVERSE (CDR ELT))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT) (CONS (CAR ELT) (REVERSE (CDR ELT))))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ COM_ALIST
              (SORT COM_ALIST
                    (FUNCTION (LAMBDA (X Y) (IDCONS_ORDP (CAR X) (CAR Y))))))
      (PROG (ELT)
        (SETQ ELT ANTICOM_ALIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((EVENP (LENGTH (CDR ELT))) (SETQ ACOM_EVEN (CONS ELT ACOM_EVEN)))
            (T (SETQ ACOM_ODD (CONS ELT ACOM_ODD)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ ACOM_EVEN
              (SORT ACOM_EVEN
                    (FUNCTION (LAMBDA (X Y) (IDCONS_ORDP (CAR X) (CAR Y))))))
      (SETQ ANTICOM_ALIST
              (AD_SIGNSORT ACOM_ODD
               (FUNCTION (LAMBDA (X Y) (IDCONS_ORDP (CAR X) (CAR Y))))))
      (SETQ THE_SIGN (TIMES THE_SIGN (CAR ANTICOM_ALIST)))
      (SETQ ANTICOM_ALIST
              (MERGE_LIST1 ACOM_EVEN (CDR ANTICOM_ALIST)
                           (FUNCTION IDCONS_ORDP)))
      (SETQ SKFORM (APPEND COM_ALIST ANTICOM_ALIST))
      (SETQ SKFORM (APPEND SKFORM (REVERSE NONCOM_ALIST)))
      (COND
       ((EQUAL MAXIND 0)
        (PROGN
         (COND
          ((EQUAL THE_SIGN (MINUS 1))
           (SETQ SKFORM (CONS (CONS (MINUS 1) (LIST NIL)) SKFORM))))
         (RETURN (CONS SKFORM NIL))
         NIL)))
      (PROG (ELT)
        (SETQ ELT (REVERSE NONCOM_ALIST))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT) (SETQ STREE (CONS (CADR ELT) STREE))) (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (PROG (ELT)
        (SETQ ELT (REVERSE ANTICOM_ALIST))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((GREATERP (LENGTH (CDR ELT)) 1)
             (SETQ STREE (CONS (CONS '- (CDR ELT)) STREE)))
            ((CDR ELT) (SETQ STREE (CONS (CADR ELT) STREE)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (PROG (ELT)
        (SETQ ELT (REVERSE COM_ALIST))
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND
            ((GREATERP (LENGTH (CDR ELT)) 1)
             (SETQ STREE (CONS (CONS '+ (CDR ELT)) STREE)))
            ((CDR ELT) (SETQ STREE (CONS (CADR ELT) STREE)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (COND ((GREATERP (LENGTH STREE) 1) (SETQ STREE (CONS '* STREE)))
            (T (SETQ STREE (CAR STREE))))
      (SETQ STREE (ST_CONSOLIDATE STREE))
      (SETQ IDVECT (MKVECT (DIFFERENCE MAXIND 1)))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MAXIND COUNT)) (RETURN NIL)))
        (PUTV IDVECT (DIFFERENCE COUNT 1) COUNT)
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (SETQ STREE (ST_SORTTREE STREE IDVECT (FUNCTION NUMLIST_ORDP)))
      (COND ((EQUAL (CAR STREE) 0) (RETURN NIL)))
      (COND
       ((EQUAL THE_SIGN (MINUS 1))
        (SETQ SKFORM (CONS (CONS (MINUS 1) (LIST NIL)) SKFORM))))
      (SETQ SYMCELLS (ST_EXTRACT_SYMCELLS (CDR STREE) MAXIND))
      (RETURN (CONS SKFORM SYMCELLS)))) 
(PUT 'DV_SKEL2FACTOR1 'NUMBER-OF-ARGS 2) 
(PUT 'DV_SKEL2FACTOR1 'DEFINED-ON-LINE '912) 
(PUT 'DV_SKEL2FACTOR1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKEL2FACTOR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_SKEL2FACTOR1 (SKEL_KERN DVARS)
    (PROG (DVAR SCR)
      (COND ((NULL SKEL_KERN) (RETURN NIL)))
      (RETURN
       (COND
        ((LISTP SKEL_KERN)
         (PROGN
          (SETQ SCR (DV_SKEL2FACTOR1 (CAR SKEL_KERN) DVARS))
          (SETQ SCR (CONS SCR (DV_SKEL2FACTOR1 (CDR SKEL_KERN) DVARS)))))
        ((EQ SKEL_KERN '~DV)
         (PROGN
          (SETQ DVAR (CAR DVARS))
          (COND
           ((CDR DVARS)
            (PROGN
             (RPLACA DVARS (CADR DVARS))
             (RPLACD DVARS (CDDR DVARS))
             NIL)))
          DVAR))
        (T SKEL_KERN))))) 
(PUT 'PST_TERMNODEP 'NUMBER-OF-ARGS 1) 
(PUT 'PST_TERMNODEP 'DEFINED-ON-LINE '937) 
(PUT 'PST_TERMNODEP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_TERMNODEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_TERMNODEP (PST) (NULL (CDR (GETV (CDR PST) (DIFFERENCE 1 1))))) 
(PUT 'PST_MKPST 'NUMBER-OF-ARGS 1) 
(PUT 'PST_MKPST 'DEFINED-ON-LINE '940) 
(PUT 'PST_MKPST 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_MKPST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_MKPST (STREE) (PST_EQUITABLE (CONS NIL (PST_MKPST1 STREE)))) 
(PUT 'ST_MULTTERMNODEP 'NUMBER-OF-ARGS 1) 
(PUT 'ST_MULTTERMNODEP 'DEFINED-ON-LINE '943) 
(PUT 'ST_MULTTERMNODEP 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ST_MULTTERMNODEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ST_MULTTERMNODEP (STREE)
    (PROG (RES SUBTREES)
      (COND ((NEQ (CAR STREE) '*) (RETURN NIL)))
      (SETQ SUBTREES (CDR STREE))
      (SETQ RES T)
      (PROG ()
       WHILELABEL
        (COND ((NOT SUBTREES) (RETURN NIL)))
        (PROGN
         (COND ((NUMBERP (CADAR SUBTREES)) (SETQ SUBTREES (CDR SUBTREES)))
               (T (PROGN (SETQ SUBTREES NIL) (SETQ RES NIL) NIL))))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'PST_MKPST1 'NUMBER-OF-ARGS 1) 
(PUT 'PST_MKPST1 'DEFINED-ON-LINE '962) 
(PUT 'PST_MKPST1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_MKPST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_MKPST1 (STREE)
    (PROG (SUBTREES S_CELLS VE PST CELL COUNT LASTCOUNT)
      (SETQ COUNT 0)
      (SETQ LASTCOUNT 0)
      (COND ((NULL STREE) (RETURN NIL)))
      (SETQ VE (MKVECT (DIFFERENCE (LENGTH (CDR STREE)) 1)))
      (SETQ SUBTREES (CDR STREE))
      (SETQ COUNT 1)
      (COND
       ((NUMBERP (CAR SUBTREES))
        (PROG ()
         WHILELABEL
          (COND ((NOT SUBTREES) (RETURN NIL)))
          (PROGN
           (PUTV VE (DIFFERENCE COUNT 1) (CONS (LIST (CAR SUBTREES)) NIL))
           (SETQ COUNT (PLUS COUNT 1))
           (SETQ SUBTREES (CDR SUBTREES))
           NIL)
          (GO WHILELABEL)))
       ((ST_MULTTERMNODEP STREE)
        (PROGN
         (SETQ VE
                 (MKVECT
                  (DIFFERENCE
                   (PROG (CELL FORALL-RESULT)
                     (SETQ CELL SUBTREES)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS
                              ((LAMBDA (CELL) (LENGTH (CDR CELL))) (CAR CELL))
                              FORALL-RESULT))
                     (SETQ CELL (CDR CELL))
                     (GO LAB1))
                   1)))
         (SETQ LASTCOUNT 0)
         (PROG (S_CELL)
           (SETQ S_CELL SUBTREES)
          LAB
           (COND ((NULL S_CELL) (RETURN NIL)))
           ((LAMBDA (S_CELL)
              (PROGN
               (SETQ CELL (CDR S_CELL))
               (COND
                ((EQ (CAR S_CELL) '*)
                 (PROG (COUNT)
                   (SETQ COUNT 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH CELL) COUNT)) (RETURN NIL)))
                   (SETQ PST (CONS (LIST (PLUS COUNT LASTCOUNT)) PST))
                   (SETQ COUNT (PLUS2 COUNT 1))
                   (GO LAB)))
                (T
                 (SETQ PST
                         (CONS
                          (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
                            (SETQ COUNT 1)
                            (COND
                             ((MINUSP (DIFFERENCE (LENGTH CELL) COUNT))
                              (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS (PLUS COUNT LASTCOUNT) NIL)))
                           LOOPLABEL
                            (SETQ COUNT (PLUS2 COUNT 1))
                            (COND
                             ((MINUSP (DIFFERENCE (LENGTH CELL) COUNT))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS (PLUS COUNT LASTCOUNT) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          PST))))
               (SETQ COUNT (PLUS LASTCOUNT 1))
               (SETQ LASTCOUNT (PLUS LASTCOUNT (LENGTH CELL)))
               (PROG (ELT)
                 (SETQ ELT CELL)
                LAB
                 (COND ((NULL ELT) (RETURN NIL)))
                 ((LAMBDA (ELT)
                    (PROGN
                     (PUTV VE (DIFFERENCE COUNT 1) (LIST (LIST ELT)))
                     (SETQ COUNT (PLUS COUNT 1))
                     NIL))
                  (CAR ELT))
                 (SETQ ELT (CDR ELT))
                 (GO LAB))
               NIL))
            (CAR S_CELL))
           (SETQ S_CELL (CDR S_CELL))
           (GO LAB))
         (RETURN (CONS (REVERSE PST) VE))
         NIL))
       (T
        (PROG ()
         WHILELABEL
          (COND ((NOT SUBTREES) (RETURN NIL)))
          (PROGN
           (SETQ PST (PST_MKPST1 (CAR SUBTREES)))
           (SETQ S_CELLS NIL)
           (PROG (COUNT2)
             (SETQ COUNT2 1)
            LAB
             (COND
              ((MINUSP (DIFFERENCE (UPBVE (CDR PST)) COUNT2)) (RETURN NIL)))
             (SETQ S_CELLS
                     (APPEND (CAR (GETV (CDR PST) (DIFFERENCE COUNT2 1)))
                             S_CELLS))
             (SETQ COUNT2 (PLUS2 COUNT2 1))
             (GO LAB))
           (PUTV VE (DIFFERENCE COUNT 1) (CONS S_CELLS PST))
           (SETQ COUNT (PLUS COUNT 1))
           (SETQ SUBTREES (CDR SUBTREES))
           NIL)
          (GO WHILELABEL))))
      (COND
       ((EQ (CAR STREE) '*)
        (SETQ PST
                (CONS
                 (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ COUNT 1)
                   (COND ((MINUSP (DIFFERENCE (UPBVE VE) COUNT)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR (CONS (LIST COUNT) NIL)))
                  LOOPLABEL
                   (SETQ COUNT (PLUS2 COUNT 1))
                   (COND
                    ((MINUSP (DIFFERENCE (UPBVE VE) COUNT))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS (LIST COUNT) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 VE)))
       (T
        (SETQ PST
                (CONS
                 (LIST
                  (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
                    (SETQ COUNT 1)
                   STARTOVER
                    (COND
                     ((MINUSP (DIFFERENCE (UPBVE VE) COUNT)) (RETURN NIL)))
                    (SETQ FORALL-RESULT (LIST COUNT))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ COUNT (PLUS2 COUNT 1))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND
                     ((MINUSP (DIFFERENCE (UPBVE VE) COUNT))
                      (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (LIST COUNT))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ COUNT (PLUS2 COUNT 1))
                    (GO LOOPLABEL)))
                 VE))))
      (RETURN PST))) 
(PUT 'PST_SUBPST 'NUMBER-OF-ARGS 2) 
(PUT 'PST_SUBPST 'DEFINED-ON-LINE '1022) 
(PUT 'PST_SUBPST 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_SUBPST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PST_SUBPST (PST IND) (GETV (CDR PST) (DIFFERENCE IND 1))) 
(PUT 'PST_REDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'PST_REDUCE 'DEFINED-ON-LINE '1025) 
(PUT 'PST_REDUCE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_REDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_REDUCE (PST)
    (PROG (ISOLATED F_CELL RPST TMP NPART NSUBS IND COUNT)
      (SETQ IND 0)
      (SETQ COUNT 0)
      (COND ((NULL PST) (RETURN (CONS NIL NIL))))
      (COND ((NULL (CDR PST)) (RETURN PST)))
      (SETQ F_CELL (CAAR PST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQ (LENGTH F_CELL) 1)) (RETURN NIL)))
        (PROGN
         (SETQ IND (CAR F_CELL))
         (COND
          ((PST_TERMNODEP PST)
           (PROGN
            (SETQ ISOLATED
                    (APPEND ISOLATED
                            (LIST (CAAR (GETV (CDR PST) (DIFFERENCE IND 1))))))
            (COND
             ((CDAR PST)
              (PROGN
               (RPLACA PST (CDAR PST))
               (SETQ NPART
                       (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                         (SETQ CELL (CAR PST))
                         (COND ((NULL CELL) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (CELL)
                                             (PROG (ELT FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ ELT CELL)
                                               (COND ((NULL ELT) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (ELT)
                                                                   (COND
                                                                    ((GREATERP
                                                                      ELT IND)
                                                                     (DIFFERENCE
                                                                      ELT 1))
                                                                    (T ELT)))
                                                                 (CAR ELT))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ ELT (CDR ELT))
                                               (COND
                                                ((NULL ELT)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (ELT)
                                                           (COND
                                                            ((GREATERP ELT IND)
                                                             (DIFFERENCE ELT
                                                                         1))
                                                            (T ELT)))
                                                         (CAR ELT))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                           (CAR CELL))
                                          NIL)))
                        LOOPLABEL
                         (SETQ CELL (CDR CELL))
                         (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (CELL)
                                     (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ ELT CELL)
                                       (COND ((NULL ELT) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (ELT)
                                                           (COND
                                                            ((GREATERP ELT IND)
                                                             (DIFFERENCE ELT
                                                                         1))
                                                            (T ELT)))
                                                         (CAR ELT))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ ELT (CDR ELT))
                                       (COND
                                        ((NULL ELT) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (ELT)
                                                   (COND
                                                    ((GREATERP ELT IND)
                                                     (DIFFERENCE ELT 1))
                                                    (T ELT)))
                                                 (CAR ELT))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR CELL))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ NSUBS
                       (MKVECT
                        (DIFFERENCE (DIFFERENCE (UPBVE (CDR PST)) 1) 1)))
               (PROG (COUNT)
                 (SETQ COUNT 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (UPBVE NSUBS) COUNT)) (RETURN NIL)))
                 (COND
                  ((GEQ COUNT IND)
                   (PUTV NSUBS (DIFFERENCE COUNT 1)
                         (GETV (CDR PST) (DIFFERENCE (PLUS COUNT 1) 1))))
                  (T
                   (PUTV NSUBS (DIFFERENCE COUNT 1)
                         (GETV (CDR PST) (DIFFERENCE COUNT 1)))))
                 (SETQ COUNT (PLUS2 COUNT 1))
                 (GO LAB))
               (RPLACA PST NPART)
               (RPLACD PST NSUBS)
               (SETQ F_CELL (CAAR PST))
               NIL))
             (T (SETQ F_CELL (SETQ PST NIL))))
            NIL))
          (T
           (PROGN
            (SETQ RPST (PST_REDUCE (CDR (PST_SUBPST PST IND))))
            (COND
             ((CAR RPST)
              (PROGN
               (SETQ ISOLATED (APPEND ISOLATED (CAR RPST)))
               (COND
                ((CDR RPST)
                 (PROGN
                  (SETQ TMP (PST_SUBPST PST IND))
                  (RPLACA TMP (SETDIFF (CAR TMP) (CAR RPST)))
                  (RPLACD TMP (CDR RPST))
                  (SETQ F_CELL NIL)
                  NIL))
                (T
                 (PROGN
                  (COND
                   ((CDAR PST)
                    (PROGN
                     (RPLACA PST (CDAR PST))
                     (SETQ NPART
                             (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                               (SETQ CELL (CAR PST))
                               (COND ((NULL CELL) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (CELL)
                                                   (PROG (ELT FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ ELT CELL)
                                                     (COND
                                                      ((NULL ELT)
                                                       (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (
                                                                            ELT)
                                                                         (COND
                                                                          ((GREATERP
                                                                            ELT
                                                                            IND)
                                                                           (DIFFERENCE
                                                                            ELT
                                                                            1))
                                                                          (T
                                                                           ELT)))
                                                                       (CAR
                                                                        ELT))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ ELT (CDR ELT))
                                                     (COND
                                                      ((NULL ELT)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (ELT)
                                                                 (COND
                                                                  ((GREATERP
                                                                    ELT IND)
                                                                   (DIFFERENCE
                                                                    ELT 1))
                                                                  (T ELT)))
                                                               (CAR ELT))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL)))
                                                 (CAR CELL))
                                                NIL)))
                              LOOPLABEL
                               (SETQ CELL (CDR CELL))
                               (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (CELL)
                                           (PROG (ELT FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ ELT CELL)
                                             (COND ((NULL ELT) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (ELT)
                                                                 (COND
                                                                  ((GREATERP
                                                                    ELT IND)
                                                                   (DIFFERENCE
                                                                    ELT 1))
                                                                  (T ELT)))
                                                               (CAR ELT))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ ELT (CDR ELT))
                                             (COND
                                              ((NULL ELT)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (ELT)
                                                         (COND
                                                          ((GREATERP ELT IND)
                                                           (DIFFERENCE ELT 1))
                                                          (T ELT)))
                                                       (CAR ELT))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))
                                         (CAR CELL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                     (SETQ NSUBS
                             (MKVECT
                              (DIFFERENCE (DIFFERENCE (UPBVE (CDR PST)) 1) 1)))
                     (PROG (COUNT)
                       (SETQ COUNT 1)
                      LAB
                       (COND
                        ((MINUSP (DIFFERENCE (UPBVE NSUBS) COUNT))
                         (RETURN NIL)))
                       (COND
                        ((GEQ COUNT IND)
                         (PUTV NSUBS (DIFFERENCE COUNT 1)
                               (GETV (CDR PST) (DIFFERENCE (PLUS COUNT 1) 1))))
                        (T
                         (PUTV NSUBS (DIFFERENCE COUNT 1)
                               (GETV (CDR PST) (DIFFERENCE COUNT 1)))))
                       (SETQ COUNT (PLUS2 COUNT 1))
                       (GO LAB))
                     (RPLACA PST NPART)
                     (RPLACD PST NSUBS)
                     (SETQ F_CELL (CAAR PST))
                     NIL))
                   (T (SETQ F_CELL (SETQ PST NIL))))
                  NIL)))
               NIL))
             (T (PROGN (SETQ F_CELL NIL) NIL)))
            NIL))))
        (GO WHILELABEL))
      (RETURN (CONS ISOLATED PST)))) 
(PUT 'PST_ISOLABLE 'NUMBER-OF-ARGS 1) 
(PUT 'PST_ISOLABLE 'DEFINED-ON-LINE '1110) 
(PUT 'PST_ISOLABLE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_ISOLABLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_ISOLABLE (RPST)
    (PROG (VE F_CELL)
      (COND ((NULL (CDR RPST)) (RETURN NIL)))
      (SETQ F_CELL (CAADR RPST))
      (SETQ VE (CDDR RPST))
      (COND
       ((NULL (CDR (GETV VE (DIFFERENCE (CAR F_CELL) 1))))
        (RETURN
         (PROG (IND FORALL-RESULT FORALL-ENDPTR)
           (SETQ IND F_CELL)
           (COND ((NULL IND) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (IND) (CAAR (GETV VE (DIFFERENCE IND 1))))
                             (CAR IND))
                            NIL)))
          LOOPLABEL
           (SETQ IND (CDR IND))
           (COND ((NULL IND) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (IND) (CAAR (GETV VE (DIFFERENCE IND 1))))
                     (CAR IND))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (RETURN
       (PROG (IND FORALL-RESULT FORALL-ENDPTR)
         (SETQ IND F_CELL)
        STARTOVER
         (COND ((NULL IND) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (IND)
                    (COPY
                     (PST_ISOLABLE
                      (CONS NIL (CDR (GETV VE (DIFFERENCE IND 1)))))))
                  (CAR IND)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ IND (CDR IND))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL IND) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (IND)
                    (COPY
                     (PST_ISOLABLE
                      (CONS NIL (CDR (GETV VE (DIFFERENCE IND 1)))))))
                  (CAR IND)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ IND (CDR IND))
         (GO LOOPLABEL))))) 
(PUT 'PST_ISOLATE 'NUMBER-OF-ARGS 2) 
(PUT 'PST_ISOLATE 'DEFINED-ON-LINE '1127) 
(PUT 'PST_ISOLATE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_ISOLATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PST_ISOLATE (S_CELL RPST)
    (PROG (REDISOL)
      (SETQ REDISOL (PST_REDUCE (PST_ISOLATE1 S_CELL (CDR RPST))))
      (RPLACA REDISOL (APPEND (CAR RPST) (CAR REDISOL)))
      (RETURN REDISOL))) 
(PUT 'PST_ISOLATE1 'NUMBER-OF-ARGS 2) 
(PUT 'PST_ISOLATE1 'DEFINED-ON-LINE '1136) 
(PUT 'PST_ISOLATE1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_ISOLATE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PST_ISOLATE1 (S_CELL PST)
    (PROG (FCELL TMP SPST IND)
      (SETQ IND 0)
      (SETQ FCELL (CAAR PST))
      (SETQ TMP FCELL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL IND 0)) (RETURN NIL)))
        (PROGN
         (COND ((NULL TMP) (SETQ IND (MINUS 1))))
         (SETQ IND (CAR TMP))
         (SETQ TMP (CDR TMP))
         (COND
          ((NOT (MEMBER S_CELL (CAR (SETQ SPST (PST_SUBPST PST IND)))))
           (SETQ IND 0))))
        (GO WHILELABEL))
      (COND ((EQUAL IND (MINUS 1)) (RETURN NIL)))
      (COND
       ((GREATERP (LENGTH FCELL) 1)
        (PROGN
         (SETQ TMP (CONS (DELETE IND FCELL) (CDAR PST)))
         (SETQ TMP (CONS (LIST IND) TMP))
         (RPLACA PST TMP))))
      (COND
       ((NOT (PST_TERMNODEP PST))
        (PROGN
         (SETQ SPST (CONS (CAR SPST) (PST_ISOLATE1 S_CELL (CDR SPST))))
         (PUTV (CDR PST) (DIFFERENCE IND 1) SPST))))
      (RETURN PST))) 
(PUT 'PST_EQUITABLE 'NUMBER-OF-ARGS 1) 
(PUT 'PST_EQUITABLE 'DEFINED-ON-LINE '1174) 
(PUT 'PST_EQUITABLE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_EQUITABLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PST_EQUITABLE (RPST)
    (PROG (NRPST REDUCED ISOL)
      (COND ((NULL (CDR RPST)) (RETURN RPST)))
      (SETQ ISOL (CAR RPST))
      (SETQ NRPST (PST_REDUCE (CDR RPST)))
      (RPLACA NRPST (APPEND ISOL (CAR NRPST)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ ISOL (CAR NRPST))
         (SETQ NRPST (CONS ISOL (PST_EQUITABLE1 ISOL (CDR NRPST))))
         (SETQ REDUCED (PST_REDUCE (CDR NRPST)))
         (COND
          ((CAR REDUCED)
           (SETQ NRPST (CONS (APPEND ISOL (CAR REDUCED)) (CDR REDUCED)))))
         (SETQ REDUCED (CAR REDUCED)))
        (COND ((NOT (NOT REDUCED)) (GO REPEATLABEL))))
      (RETURN NRPST))) 
(PUT 'PST_EQUITABLE1 'NUMBER-OF-ARGS 2) 
(PUT 'PST_EQUITABLE1 'DEFINED-ON-LINE '1195) 
(PUT 'PST_EQUITABLE1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_EQUITABLE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PST_EQUITABLE1 (ISOLATED PST)
    (PROG (ISOL VE ALPHA BETA P1 EQUIT CELL PSI LEN K N_DELEMS)
      (SETQ LEN 0)
      (SETQ K 0)
      (SETQ N_DELEMS 0)
      (COND ((NULL PST) (RETURN NIL)))
      (SETQ ISOL ISOLATED)
      (SETQ LEN (LENGTH ISOLATED))
      (SETQ VE (MKVECT (DIFFERENCE (PLUS (UPBVE (CDR PST)) LEN) 1)))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBVE (CDR PST)) COUNT)) (RETURN NIL)))
        (PUTV VE (DIFFERENCE COUNT 1)
              (CAR (GETV (CDR PST) (DIFFERENCE COUNT 1))))
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (SETQ ALPHA (CAR PST))
      (PROG (COUNT)
        (SETQ COUNT (PLUS (UPBVE (CDR PST)) 1))
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBVE VE) COUNT)) (RETURN NIL)))
        (PROGN
         (PUTV VE (DIFFERENCE COUNT 1) (LIST (CAR ISOL)))
         (SETQ ISOL (CDR ISOL))
         (SETQ ALPHA (CONS (LIST COUNT) ALPHA))
         NIL)
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (SETQ P1 (FULLCOPY ALPHA))
      (SETQ LEN (LENGTH P1))
      (SETQ N_DELEMS (UPBVE VE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ALPHA (LESSP LEN N_DELEMS))) (RETURN NIL)))
        (PROGN
         (SETQ BETA (CAR ALPHA))
         (SETQ ALPHA (CDR ALPHA))
         (SETQ EQUIT NIL)
         (SETQ LEN 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT P1) (RETURN NIL)))
           (PROGN
            (SETQ CELL (CAR P1))
            (SETQ P1 (CDR P1))
            (SETQ PSI
                    (COND ((CDR CELL) (PST_PARTITION CELL BETA VE))
                          (T (LIST CELL))))
            (SETQ K (LENGTH PSI))
            (SETQ EQUIT (APPEND EQUIT PSI))
            (SETQ LEN (PLUS LEN K))
            (COND ((GEQ K 2) (SETQ ALPHA (APPEND (CDR PSI) ALPHA))))
            NIL)
           (GO WHILELABEL))
         (SETQ P1 EQUIT)
         NIL)
        (GO WHILELABEL))
      (SETQ EQUIT (PNTH P1 (PLUS (LENGTH ISOLATED) 1)))
      (COND
       ((NOT (PST_TERMNODEP PST))
        (PROG (COUNT)
          (SETQ COUNT 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (UPBVE (CDR PST)) COUNT)) (RETURN NIL)))
          (PROGN
           (SETQ P1 (GETV (CDR PST) (DIFFERENCE COUNT 1)))
           (PUTV (CDR PST) (DIFFERENCE COUNT 1)
                 (CONS (CAR P1) (PST_EQUITABLE1 ISOLATED (CDR P1))))
           NIL)
          (SETQ COUNT (PLUS2 COUNT 1))
          (GO LAB))))
      (RETURN (CONS EQUIT (CDR PST))))) 
(PUT 'PST_D1 'NUMBER-OF-ARGS 3) 
(PUT 'PST_D1 'DEFINED-ON-LINE '1249) 
(PUT 'PST_D1 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_D1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PST_D1 (D1 D2 VE)
    (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
      (SETQ E1 (GETV VE (DIFFERENCE D1 1)))
      (COND ((NULL E1) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (E1)
                          (ORDN
                           (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ E2 (GETV VE (DIFFERENCE D2 1)))
                             (COND ((NULL E2) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (E2)
                                                 (ORDN
                                                  (CAR
                                                   (PA_COINC_SPLIT (SC_KERN E1)
                                                    (SC_KERN E2)))))
                                               (CAR E2))
                                              NIL)))
                            LOOPLABEL
                             (SETQ E2 (CDR E2))
                             (COND ((NULL E2) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (E2)
                                         (ORDN
                                          (CAR
                                           (PA_COINC_SPLIT (SC_KERN E1)
                                            (SC_KERN E2)))))
                                       (CAR E2))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                        (CAR E1))
                       NIL)))
     LOOPLABEL
      (SETQ E1 (CDR E1))
      (COND ((NULL E1) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (E1)
                  (ORDN
                   (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                     (SETQ E2 (GETV VE (DIFFERENCE D2 1)))
                     (COND ((NULL E2) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (E2)
                                         (ORDN
                                          (CAR
                                           (PA_COINC_SPLIT (SC_KERN E1)
                                            (SC_KERN E2)))))
                                       (CAR E2))
                                      NIL)))
                    LOOPLABEL
                     (SETQ E2 (CDR E2))
                     (COND ((NULL E2) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (E2)
                                 (ORDN
                                  (CAR
                                   (PA_COINC_SPLIT (SC_KERN E1)
                                    (SC_KERN E2)))))
                               (CAR E2))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
                (CAR E1))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PST_D 'NUMBER-OF-ARGS 3) 
(PUT 'PST_D 'DEFINED-ON-LINE '1254) 
(PUT 'PST_D 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PST_D (D1 D2 VE)
    (COND
     ((LISTP D1)
      (COND
       ((LISTP D2)
        (ORDN
         (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
           (SETQ E1 D1)
           (COND ((NULL E1) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (E1)
                               (ORDN
                                (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ E2 D2)
                                  (COND ((NULL E2) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (E2)
                                                      (PST_D E1 E2 VE))
                                                    (CAR E2))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ E2 (CDR E2))
                                  (COND ((NULL E2) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (E2) (PST_D E1 E2 VE))
                                            (CAR E2))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))
                             (CAR E1))
                            NIL)))
          LOOPLABEL
           (SETQ E1 (CDR E1))
           (COND ((NULL E1) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (E1)
                       (ORDN
                        (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ E2 D2)
                          (COND ((NULL E2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (E2) (PST_D E1 E2 VE))
                                            (CAR E2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ E2 (CDR E2))
                          (COND ((NULL E2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (E2) (PST_D E1 E2 VE)) (CAR E2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
                     (CAR E1))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))
       (T
        (ORDN
         (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
           (SETQ E1 D1)
           (COND ((NULL E1) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (E1) (PST_D E1 D2 VE)) (CAR E1))
                                 NIL)))
          LOOPLABEL
           (SETQ E1 (CDR E1))
           (COND ((NULL E1) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (E1) (PST_D E1 D2 VE)) (CAR E1)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))
     ((LISTP D2)
      (ORDN
       (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
         (SETQ E2 D2)
         (COND ((NULL E2) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (E2) (PST_D D1 E2 VE)) (CAR E2)) NIL)))
        LOOPLABEL
         (SETQ E2 (CDR E2))
         (COND ((NULL E2) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (E2) (PST_D D1 E2 VE)) (CAR E2)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     (T (PST_D1 D1 D2 VE)))) 
(PUT 'PST_PARTITION 'NUMBER-OF-ARGS 3) 
(PUT 'PST_PARTITION 'DEFINED-ON-LINE '1267) 
(PUT 'PST_PARTITION 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'PST_PARTITION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PST_PARTITION (S1 S2 VE)
    (PROG (ELT_D ELT_APAIR PST_ALIST)
      (PROG (ELT)
        (SETQ ELT S1)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ ELT_D (PST_D ELT S2 VE))
            (COND
             ((SETQ ELT_APAIR (ASSOC ELT_D PST_ALIST))
              (RPLACD ELT_APAIR (CONS ELT (CDR ELT_APAIR))))
             (T (SETQ PST_ALIST (CONS (CONS ELT_D (LIST ELT)) PST_ALIST))))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ PST_ALIST
              (SORT PST_ALIST
                    (FUNCTION (LAMBDA (X Y) (NUMLIST_ORDP (CAR X) (CAR Y))))))
      (RETURN
       (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
         (SETQ ELT PST_ALIST)
         (COND ((NULL ELT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (ELT) (REVERSE (CDR ELT))) (CAR ELT))
                               NIL)))
        LOOPLABEL
         (SETQ ELT (CDR ELT))
         (COND ((NULL ELT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (ELT) (REVERSE (CDR ELT))) (CAR ELT)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'DV_NEXT_CHOICE 'NUMBER-OF-ARGS 4) 
(PUT 'DV_NEXT_CHOICE 'DEFINED-ON-LINE '1287) 
(PUT 'DV_NEXT_CHOICE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_NEXT_CHOICE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DV_NEXT_CHOICE (SC PARTIAL_PERM RPST COMP_INFO)
    (PROG (NEXT_PERM EXTENSIONS NRPST NEW_AUT NPOINTS LEN NEXT_IMG)
      (SETQ NPOINTS 0)
      (SETQ LEN 0)
      (SETQ NEXT_IMG 0)
      (SETQ NPOINTS (UPBVE (CAR SC)))
      (SETQ G_SKIP_TO_LEVEL (SETQ LEN (PLUS (UPBVE PARTIAL_PERM) 1)))
      (SC_SETBASE SC PARTIAL_PERM)
      (SETQ EXTENSIONS (PST_ISOLABLE RPST))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ EXTENSIONS
                 (IDSORT
                  (INTERSECTION EXTENSIONS
                                (CANDIDATE_EXTENSIONS SC PARTIAL_PERM))))
         (COND
          (EXTENSIONS
           (PROGN
            (SETQ NEXT_IMG (CAR EXTENSIONS))
            (SETQ EXTENSIONS (CDR EXTENSIONS))
            (SETQ NRPST (PST_EQUITABLE (PST_ISOLATE NEXT_IMG (FULLCOPY RPST))))
            (SETQ NEXT_PERM (LIST2VECT* (CAR NRPST) 'SYMBOLIC))
            (SETQ COMP_INFO (DV_COMPARE NEXT_PERM COMP_INFO LEN NPOINTS))
            (COND
             ((EQUAL (CAR COMP_INFO) 0)
              (COND
               ((EQUAL (UPBVE NEXT_PERM) NPOINTS)
                (PROGN
                 (SETQ NEW_AUT
                         (PE_MULT
                          (PE_INV (GETV (CADR COMP_INFO) (DIFFERENCE 1 1)))
                          NEXT_PERM))
                 (PROCESS_NEW_AUTOMORPHISM SC NEW_AUT)
                 NIL))
               (T
                (SETQ COMP_INFO
                        (DV_NEXT_CHOICE SC NEXT_PERM NRPST COMP_INFO)))))
             ((EQUAL (CAR COMP_INFO) 1)
              (COND
               ((LESSP (UPBVE NEXT_PERM) NPOINTS)
                (SETQ COMP_INFO (DV_NEXT_CHOICE SC NEXT_PERM NRPST COMP_INFO)))
               (T (RPLACA COMP_INFO 0)))))
            (RPLACD (CDR COMP_INFO) (CDR (CDDR COMP_INFO)))
            NIL))))
        (COND
         ((NOT (OR (NULL EXTENSIONS) (GREATERP LEN G_SKIP_TO_LEVEL)))
          (GO REPEATLABEL))))
      (RETURN COMP_INFO))) 
(PUT 'CAN_REP_CELL 'NUMBER-OF-ARGS 2) 
(PUT 'CAN_REP_CELL 'DEFINED-ON-LINE '1327) 
(PUT 'CAN_REP_CELL 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CAN_REP_CELL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CAN_REP_CELL (COMP_INFO LEVEL)
    (GETV (GETV (CADR COMP_INFO) (DIFFERENCE 2 1)) (DIFFERENCE LEVEL 1))) 
(PUT 'LAST_PART_KERN 'NUMBER-OF-ARGS 1) 
(PUT 'LAST_PART_KERN 'DEFINED-ON-LINE '1330) 
(PUT 'LAST_PART_KERN 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'LAST_PART_KERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAST_PART_KERN (COMP_INFO) (CAR (CDDR COMP_INFO))) 
(PUT 'DV_COMPARE 'NUMBER-OF-ARGS 4) 
(PUT 'DV_COMPARE 'DEFINED-ON-LINE '1333) 
(PUT 'DV_COMPARE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_COMPARE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DV_COMPARE (NEXT_PERM COMP_INFO LEN NPOINTS)
    (PROG (PART_KERN PART_REP CAN_REP CURLEV RES)
      (COND
       ((EQUAL (CAR COMP_INFO) 1)
        (RETURN (DV_FILL_COMP_INFO NEXT_PERM COMP_INFO LEN NPOINTS NIL NIL))))
      (COND
       ((EQUAL LEN 1)
        (PROGN
         (SETQ PART_KERN (SC_KERN (GETV NEXT_PERM (DIFFERENCE 1 1))))
         (SETQ PART_REP (LIST (SC_REP (GETV NEXT_PERM (DIFFERENCE 1 1)))))
         NIL))
       (T
        (PROGN
         (SETQ PART_KERN (LAST_PART_KERN COMP_INFO))
         (SETQ PART_KERN
                 (PA_COINC_SPLIT PART_KERN
                  (SC_KERN (GETV NEXT_PERM (DIFFERENCE LEN 1)))))
         (SETQ PART_REP
                 (CONS (SC_REP (GETV NEXT_PERM (DIFFERENCE LEN 1)))
                       (CAR PART_KERN)))
         (SETQ PART_KERN (CDR PART_KERN))
         NIL)))
      (SETQ CAN_REP (CAN_REP_CELL COMP_INFO LEN))
      (SETQ CURLEV LEN)
      (SETQ RES 0)
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((EQUAL CAN_REP PART_REP)
           (PROGN
            (SETQ RES 0)
            (COND
             ((LESSP CURLEV (UPBVE NEXT_PERM))
              (PROGN
               (SETQ CURLEV (PLUS CURLEV 1))
               (SETQ PART_KERN
                       (PA_COINC_SPLIT PART_KERN
                        (SC_KERN (GETV NEXT_PERM (DIFFERENCE CURLEV 1)))))
               (SETQ PART_REP
                       (CONS (SC_REP (GETV NEXT_PERM (DIFFERENCE CURLEV 1)))
                             (CAR PART_KERN)))
               (SETQ PART_KERN (CDR PART_KERN))
               (SETQ CAN_REP (CAN_REP_CELL COMP_INFO CURLEV))
               NIL)))))
          ((NUMLIST_ORDP CAN_REP PART_REP)
           (PROGN
            (SETQ RES 1)
            (RPLACA COMP_INFO 1)
            (SETQ COMP_INFO
                    (DV_FILL_COMP_INFO NEXT_PERM COMP_INFO CURLEV NPOINTS
                     PART_REP PART_KERN))
            NIL))
          (T
           (PROGN
            (SETQ RES 2)
            (RPLACD (CDR COMP_INFO) (CONS NIL (CDDR COMP_INFO)))
            (RPLACA COMP_INFO 2)
            NIL))))
        (COND
         ((NOT (OR (NEQ RES 0) (EQUAL CURLEV (UPBVE NEXT_PERM))))
          (GO REPEATLABEL))))
      (COND
       ((EQUAL RES 0)
        (PROGN
         (RPLACD (CDR COMP_INFO) (CONS PART_KERN (CDDR COMP_INFO)))
         (COND
          ((AND (EQUAL CURLEV NPOINTS) (DV_NEW_AUT_HOOK NEXT_PERM COMP_INFO))
           (PROGN (SETQ G_SKIP_TO_LEVEL 0) (RPLACA COMP_INFO 2) NIL)))
         NIL)))
      (RETURN COMP_INFO))) 
(PUT 'DV_FILL_COMP_INFO 'NUMBER-OF-ARGS 6) 
(PUT 'DV_FILL_COMP_INFO 'DEFINED-ON-LINE '1403) 
(PUT 'DV_FILL_COMP_INFO 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_FILL_COMP_INFO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DV_FILL_COMP_INFO (PE COMP_INFO LEN NPOINTS PART_REP PART_KERN)
    (PROG (LEVEL)
      (SETQ LEVEL 0)
      (COND
       ((EQUAL LEN 1)
        (PROGN
         (SETQ PART_KERN (SC_KERN (GETV PE (DIFFERENCE 1 1))))
         (SETQ PART_REP (LIST (SC_REP (GETV PE (DIFFERENCE 1 1)))))
         NIL))
       ((NULL PART_KERN)
        (PROGN
         (SETQ PART_KERN (LAST_PART_KERN COMP_INFO))
         (SETQ PART_KERN
                 (PA_COINC_SPLIT PART_KERN
                  (SC_KERN (GETV PE (DIFFERENCE LEN 1)))))
         (SETQ PART_REP
                 (CONS (SC_REP (GETV PE (DIFFERENCE LEN 1))) (CAR PART_KERN)))
         (SETQ PART_KERN (CDR PART_KERN))
         NIL)))
      (PUTV (GETV (CADR COMP_INFO) (DIFFERENCE 2 1)) (DIFFERENCE LEN 1)
            PART_REP)
      (SETQ LEVEL (PLUS LEN 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ LEVEL (UPBVE PE))) (RETURN NIL)))
        (PROGN
         (SETQ PART_KERN
                 (PA_COINC_SPLIT PART_KERN
                  (SC_KERN (GETV PE (DIFFERENCE LEVEL 1)))))
         (SETQ PART_REP
                 (CONS (SC_REP (GETV PE (DIFFERENCE LEVEL 1)))
                       (CAR PART_KERN)))
         (SETQ PART_KERN (CDR PART_KERN))
         (PUTV (GETV (CADR COMP_INFO) (DIFFERENCE 2 1)) (DIFFERENCE LEVEL 1)
               PART_REP)
         (SETQ LEVEL (PLUS LEVEL 1)))
        (GO WHILELABEL))
      (RPLACD (CDR COMP_INFO) (CONS PART_KERN (CDDR COMP_INFO)))
      (COND
       ((EQUAL LEVEL (PLUS NPOINTS 1))
        (COND
         ((AND (NULL (GETV (CADR COMP_INFO) (DIFFERENCE 1 1)))
               (DV_NULL_FIRST_KERN PART_KERN))
          (PROGN (SETQ G_SKIP_TO_LEVEL 0) (RPLACA COMP_INFO 2) NIL))
         (T
          (PROGN
           (PUTV (CADR COMP_INFO) (DIFFERENCE 1 1) (FULLCOPY PE))
           (PUTV (CADR COMP_INFO) (DIFFERENCE 3 1) PART_KERN)
           NIL)))))
      (RETURN COMP_INFO))) 
(PUT 'DV_NULL_FIRST_KERN 'NUMBER-OF-ARGS 1) 
(PUT 'DV_NULL_FIRST_KERN 'DEFINED-ON-LINE '1446) 
(PUT 'DV_NULL_FIRST_KERN 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_NULL_FIRST_KERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_NULL_FIRST_KERN (KERN)
    (PROG (L_KERN CELL NULLEXP ACELL COUNT COUNT2)
      (SETQ COUNT 0)
      (SETQ COUNT2 0)
      (SETQ NULLEXP NIL)
      (SETQ L_KERN (PA_VECT2LIST KERN))
      (PROG (CELL)
        (SETQ CELL L_KERN)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (COND
            ((AND (CDR CELL) (NOT NULLEXP))
             (PROGN
              (SETQ COUNT 0)
              (PROG (COUNT2)
                (SETQ COUNT2 1)
               LAB
                (COND
                 ((MINUSP (DIFFERENCE (UPBVE G_SC_VE) COUNT2)) (RETURN NIL)))
                (COND
                 ((AND
                   (EQ
                    (CAR
                     (SETQ ACELL (CAR (GETV G_SC_VE (DIFFERENCE COUNT2 1)))))
                    '-)
                   (MEMBER (CAR CELL) ACELL))
                  (SETQ COUNT (PLUS COUNT 1))))
                (SETQ COUNT2 (PLUS2 COUNT2 1))
                (GO LAB))
              (COND ((ODDP COUNT) (SETQ NULLEXP T)))
              NIL))))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (RETURN NULLEXP))) 
(PUT 'DV_NEW_AUT_HOOK 'NUMBER-OF-ARGS 2) 
(PUT 'DV_NEW_AUT_HOOK 'DEFINED-ON-LINE '1467) 
(PUT 'DV_NEW_AUT_HOOK 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_NEW_AUT_HOOK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_NEW_AUT_HOOK (PE COMP_INFO)
    (PROG (TMP1 TMP2 VE COUNT THESIGN)
      (SETQ COUNT 0)
      (SETQ THESIGN 0)
      (SETQ THESIGN
              (ST_SIGNCHANGE (GETV (CADR COMP_INFO) (DIFFERENCE 1 1)) PE))
      (SETQ TMP1 (PA_PART2LIST (GETV (CADR COMP_INFO) (DIFFERENCE 3 1))))
      (SETQ TMP2 (PA_PART2LIST (CADDR COMP_INFO)))
      (SETQ VE (MKVECT (DIFFERENCE (LENGTH TMP1) 1)))
      (SETQ COUNT 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT TMP1) (RETURN NIL)))
        (PROGN
         (PUTV VE (DIFFERENCE (CAR TMP1) 1) (CAR TMP2))
         (SETQ TMP1 (CDR TMP1))
         (SETQ TMP2 (CDR TMP2))
         (SETQ COUNT (PLUS COUNT 1))
         NIL)
        (GO WHILELABEL))
      (PROG (COUNT)
        (SETQ COUNT 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBVE G_SC_VE) COUNT)) (RETURN NIL)))
        (PROGN
         (SETQ TMP1 (CAR (GETV G_SC_VE (DIFFERENCE COUNT 1))))
         (COND
          ((EQ (CAR TMP1) '-)
           (PROGN
            (SETQ TMP1 (CDR TMP1))
            (SETQ TMP2
                    (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELT TMP1)
                      (COND ((NULL ELT) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELT)
                                          (GETV VE (DIFFERENCE ELT 1)))
                                        (CAR ELT))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELT (CDR ELT))
                      (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELT) (GETV VE (DIFFERENCE ELT 1)))
                                (CAR ELT))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ THESIGN (TIMES THESIGN (CAR (NUM_SIGNSORT TMP2))))
            NIL))))
        (SETQ COUNT (PLUS2 COUNT 1))
        (GO LAB))
      (COND ((EQUAL THESIGN (MINUS 1)) (RETURN T)))
      (RETURN NIL))) 
(PUT 'DV_CANON_MONOMIAL 'NUMBER-OF-ARGS 1) 
(PUT 'DV_CANON_MONOMIAL 'DEFINED-ON-LINE '1502) 
(PUT 'DV_CANON_MONOMIAL 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_CANON_MONOMIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_CANON_MONOMIAL (SF)
    (PROG (TMP SKLIST CAMB SKEL SKPROD AUT_SC CAN_KERN NEW_DVNAMES PST
           COMP_INFO FACTLST RES FACT SORTED_FACTLST COUNT EXPNT THESIGN
           MAXIND)
      (SETQ COUNT 0)
      (SETQ EXPNT 0)
      (SETQ THESIGN 0)
      (SETQ MAXIND 0)
      (SETQ THESIGN 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM SF) (ATOM (CAR SF))))) (RETURN NIL)))
        (PROGN
         (SETQ TMP (CAAR SF))
         (SETQ SF (CDAR SF))
         (SETQ EXPNT (CDR TMP))
         (SETQ CAMB (CAR TMP))
         (COND
          ((AND (NEQ EXPNT 1) (FLAGP (DV_CAMBHEAD CAMB) 'ANTICOM))
           (PROGN (SETQ SKEL NIL) (SETQ SF NIL) NIL))
          (T (SETQ SKEL (DV_SKELSPLIT CAMB))))
         (COND ((NULL SKEL) (SETQ SF NIL))
               (T
                (PROGN
                 (COND
                  ((LESSP (CAR SKEL) 0)
                   (PROGN
                    (SETQ SKEL (CDR SKEL))
                    (COND ((ODDP EXPNT) (SETQ THESIGN (MINUS THESIGN)))
                          (T (RPLACD (CDR SKEL) (SUBST '- '+ (CDR SKEL)))))
                    NIL))
                  (T (SETQ SKEL (CDR SKEL))))
                 (COND ((GREATERP (CAR SKEL) MAXIND) (SETQ MAXIND (CAR SKEL))))
                 (SETQ SKEL (CADR SKEL))
                 (COND
                  ((NEQ EXPNT 1) (RPLACA SKEL (LIST 'EXPT (CAR SKEL) EXPNT))))
                 (SETQ SKLIST (CONS SKEL SKLIST))
                 NIL)))
         NIL)
        (GO WHILELABEL))
      (COND ((NULL SF) (RETURN NIL)))
      (SETQ SKLIST (REVERSE (CONS (CONS SF NIL) SKLIST)))
      (SETQ SKPROD (DV_SKELPROD SKLIST MAXIND))
      (COND ((NULL SKPROD) (RETURN NIL)))
      (SETQ SKLIST (CAR SKPROD))
      (COND
       ((GREATERP MAXIND 0)
        (PROGN
         (SETQ G_SC_VE (CDDR SKPROD))
         (SETQ G_INIT_STREE (CADR SKPROD))
         (SETQ AUT_SC (SC_CREATE (UPBVE G_SC_VE)))
         (SETQ COMP_INFO (MKVECT (DIFFERENCE 3 1)))
         (PUTV COMP_INFO (DIFFERENCE 2 1)
               (MKVECT (DIFFERENCE (UPBVE G_SC_VE) 1)))
         (SETQ COMP_INFO (LIST 1 COMP_INFO NIL))
         (SETQ PST (PST_MKPST G_INIT_STREE))
         (SETQ TMP (LIST2VECT* (CAR PST) 'SYMBOLIC))
         (SETQ G_SKIP_TO_LEVEL 1)
         (COND
          ((CAR PST)
           (SETQ COMP_INFO (DV_COMPARE TMP COMP_INFO 1 (UPBVE G_SC_VE)))))
         (COND
          ((CDR PST)
           (SETQ COMP_INFO (DV_NEXT_CHOICE AUT_SC TMP PST COMP_INFO))))
         (COND ((EQUAL G_SKIP_TO_LEVEL 0) (RETURN NIL)))
         (SETQ CAN_KERN
                 (PA_PART2LIST (GETV (CADR COMP_INFO) (DIFFERENCE 3 1))))
         (SETQ COUNT 0)
         (SETQ NEW_DVNAMES NIL)
         (PROG (ELT)
           (SETQ ELT CAN_KERN)
          LAB
           (COND ((NULL ELT) (RETURN NIL)))
           ((LAMBDA (ELT)
              (PROGN
               (SETQ COUNT (PLUS COUNT 1))
               (COND
                ((NEQ ELT COUNT)
                 (SETQ NEW_DVNAMES (CONS (CONS ELT COUNT) NEW_DVNAMES))))
               NIL))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         NIL)))
      (PROG (CELL)
        (SETQ CELL SKLIST)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (PROGN
            (SETQ FACTLST NIL)
            (SETQ SKEL (CAR CELL))
            (COND
             ((CADR CELL)
              (PROGN
               (PROG (STREE)
                 (SETQ STREE (CDR CELL))
                LAB
                 (COND ((NULL STREE) (RETURN NIL)))
                 ((LAMBDA (STREE)
                    (PROGN
                     (SETQ FACT (DV_SKEL2FACTOR (CONS SKEL STREE) NEW_DVNAMES))
                     (COND
                      ((EQUAL (CAR FACT) (MINUS 1))
                       (SETQ THESIGN (MINUS THESIGN))))
                     (SETQ FACTLST (CONS (CDR FACT) FACTLST))
                     NIL))
                  (CAR STREE))
                 (SETQ STREE (CDR STREE))
                 (GO LAB))
               (SETQ FACTLST (REVERSE FACTLST))
               (COND
                ((FLAGP (DV_CAMBHEAD SKEL) 'ANTICOM)
                 (PROGN
                  (SETQ SORTED_FACTLST (AD_SIGNSORT FACTLST 'IDCONS_ORDP))
                  (SETQ THESIGN (TIMES THESIGN (CAR SORTED_FACTLST)))
                  (SETQ SORTED_FACTLST (CDR SORTED_FACTLST))
                  NIL))
                (T (SETQ SORTED_FACTLST (SORT FACTLST 'IDCONS_ORDP))))
               (SETQ RES (APPEND RES SORTED_FACTLST))
               NIL))
             (T (SETQ RES (APPEND RES (LIST SKEL)))))
            NIL))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (COND
       ((EQUAL THESIGN (MINUS 1))
        (SETQ SKPROD (LIST 'MINUS (CONS 'TIMES RES))))
       ((EQUAL THESIGN 1) (SETQ SKPROD (CONS 'TIMES RES))) (T (SETQ SKPROD 0)))
      (RETURN (*Q2F (SIMP* SKPROD))))) 
(PUT 'DV_SKEL2FACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'DV_SKEL2FACTOR 'DEFINED-ON-LINE '1612) 
(PUT 'DV_SKEL2FACTOR 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DV_SKEL2FACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_SKEL2FACTOR (SKELPAIR NEWNAMES)
    (PROG (STREE DVARS)
      (COND ((NULL (CDR SKELPAIR)) (RETURN (CAR SKELPAIR))))
      (SETQ STREE (SUBLIS NEWNAMES (CDR SKELPAIR)))
      (SETQ STREE (ST_AD_NUMSORTTREE STREE))
      (SETQ DVARS
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (ST_FLATTEN (CDR STREE)))
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT) (DV_IND2VAR ELT)) (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELT) (DV_IND2VAR ELT)) (CAR ELT)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS (CAR STREE) (DV_SKEL2FACTOR1 (CAR SKELPAIR) DVARS))))) 
(PUT 'CANONICAL 'NUMBER-OF-ARGS 1) 
(PUT 'CANONICAL 'DEFINED-ON-LINE '1625) 
(PUT 'CANONICAL 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CANONICAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CANONICAL (SQ)
    (PROG (SF DENOM RES *DISTRIBUTE)
      (SETQ RES NIL)
      (SETQ SQ (SIMP* (CAR SQ)))
      (SETQ DENOM (CDR SQ))
      (ON (LIST 'DISTRIBUTE))
      (SETQ SF (DISTRI_POL (CAR SQ)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM SF) (ATOM (CAR SF))))) (RETURN NIL)))
        (PROGN
         (SETQ RES (ADDF RES (DV_CANON_MONOMIAL (CONS (CAR SF) NIL))))
         (SETQ SF (CDR SF))
         NIL)
        (GO WHILELABEL))
      (SETQ RES (ADDF RES SF))
      (RETURN (SIMP* (LIST '*SQ (CONS RES DENOM) NIL))))) 
(PUT 'CANONICAL 'SIMPFN 'CANONICAL) 
(FLAG '(SYMTREE) 'OPFN) 
(PUT 'SYMTREE 'NUMBER-OF-ARGS 2) 
(PUT 'SYMTREE 'DEFINED-ON-LINE '1649) 
(PUT 'SYMTREE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SYMTREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMTREE (NAME S) (PROGN (PUT NAME 'SYMTREE (ALG_TO_SYMB S)) NIL)) 
(PUT 'REMSYM 'NUMBER-OF-ARGS 1) 
(PUT 'REMSYM 'DEFINED-ON-LINE '1654) 
(PUT 'REMSYM 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'REMSYM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMSYM (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J)
         (COND ((FLAGP J 'SYMMETRIC) (REMFLAG (LIST J) 'SYMMETRIC))
               ((FLAGP J 'ANTISYMMETRIC) (REMFLAG (LIST J) 'ANTISYMMETRIC))
               (T (REMPROP J 'SYMTREE))))
       (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'DUMMY_NAMES 'NUMBER-OF-ARGS 1) 
(PUT 'DUMMY_NAMES 'DEFINED-ON-LINE '1662) 
(PUT 'DUMMY_NAMES 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DUMMY_NAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUMMY_NAMES (U)
    (PROGN
     (COND
      (G_DVBASE
       (MSGPRI "The created dummy base" G_DVBASE "must be cleared" NIL T)))
     (SETQ G_DVNAMES (LIST2VECT* U 'SYMBOLIC))
     (FLAG U 'DUMMY)
     T)) 
(RLISTAT '(DUMMY_NAMES)) 
(PUT 'SHOW_DUMMY_NAMES 'NUMBER-OF-ARGS 0) 
(PUT 'SHOW_DUMMY_NAMES 'DEFINED-ON-LINE '1671) 
(PUT 'SHOW_DUMMY_NAMES 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'SHOW_DUMMY_NAMES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SHOW_DUMMY_NAMES NIL
    (COND (G_DVNAMES (SYMB_TO_ALG (VECT2LIST G_DVNAMES)))
          (T (SYMB_TO_ALG (LIST 'LIST))))) 
(PUT 'DUMMY_BASE 'NUMBER-OF-ARGS 1) 
(PUT 'DUMMY_BASE 'DEFINED-ON-LINE '1675) 
(PUT 'DUMMY_BASE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'DUMMY_BASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUMMY_BASE (U)
    (COND
     (G_DVNAMES
      (MSGPRI "Named variables" (SYMB_TO_ALG (VECT2LIST G_DVNAMES))
              "must be eliminated" NIL T))
     (T (SETQ G_DVBASE U)))) 
(PUT 'CLEAR_DUMMY_BASE 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_DUMMY_BASE 'DEFINED-ON-LINE '1681) 
(PUT 'CLEAR_DUMMY_BASE 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CLEAR_DUMMY_BASE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_DUMMY_BASE NIL (PROGN (SETQ G_DVBASE NIL) T)) 
(PUT 'CLEAR_DUMMY_NAMES 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_DUMMY_NAMES 'DEFINED-ON-LINE '1684) 
(PUT 'CLEAR_DUMMY_NAMES 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'CLEAR_DUMMY_NAMES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_DUMMY_NAMES NIL (PROGN (SETQ G_DVNAMES NIL) T)) 
(FLAG '(SHOW_DUMMY_NAMES CLEAR_DUMMY_NAMES DUMMY_BASE CLEAR_DUMMY_BASE) 'OPFN) 
(DEFLIST '((CLEAR_DUMMY_BASE ENDSTAT) (CLEAR_DUMMY_NAMES ENDSTAT)) 'STAT) 
(PUT 'ANTICOM 'NUMBER-OF-ARGS 1) 
(PUT 'ANTICOM 'DEFINED-ON-LINE '1693) 
(PUT 'ANTICOM 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'ANTICOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANTICOM (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (PROGN (FLAG (LIST X) 'ANTICOM) (FLAG (LIST X) 'NONCOM)))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'REMANTICOM 'NUMBER-OF-ARGS 1) 
(PUT 'REMANTICOM 'DEFINED-ON-LINE '1698) 
(PUT 'REMANTICOM 'DEFINED-IN-FILE 'ASSIST/DUMMYCNT.RED) 
(PUT 'REMANTICOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMANTICOM (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (PROGN (REMFLAG X 'NONCOM) (REMFLAG X 'ANTICOM))) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(DEFLIST '((ANTICOM RLIS) (REMANTICOM RLIS)) 'STAT) 
(ENDMODULE) 
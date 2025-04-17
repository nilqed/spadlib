(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PRECOATS)) 
(FLUID
 '(*TRA BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS SQRT-INTVAR TAYLORVARIABLE
   THISPLACE)) 
(EXPORTS (LIST 'PRECOATES)) 
(IMPORTS
 (LIST 'MKSP 'ALGINT-SUBF 'SUBZERO2 'SUBSTITUTESQ 'REMOVEDUPLICATES 'PRINTSQ
       'BASICPLACE 'EXTENPLACE 'INTERR 'GET-CORRECT-SQRTS 'PRINTPLACE
       'SIMPTIMES 'SUBZERO 'NEGSQ 'ADDSQ 'INVOLVESQ 'TAYLORFORM 'TAYLOREVALUATE
       'MK*SQ '*EXPTSQ '*MULTSQ '*INVSQ 'SQRT2TOP 'JFACTOR 'SQRTSAVE 'ANTISUBS)) 
(PUT 'INFSUBS 'NUMBER-OF-ARGS 1) 
(PUT 'INFSUBS 'DEFINED-ON-LINE '45) 
(PUT 'INFSUBS 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'INFSUBS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INFSUBS (W)
    (COND ((EQUAL (CAAR W) THISPLACE) (CONS (CDAR W) (CDR W)))
          (T (CONS (CONS THISPLACE (CAR W)) (CDR W))))) 
(PUT 'PRECOATES 'NUMBER-OF-ARGS 3) 
(PUT 'PRECOATES 'DEFINED-ON-LINE '52) 
(PUT 'PRECOATES 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'PRECOATES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRECOATES (RESIDUES X MOVEDTOINFINITY)
    (PROG (ANSWER PLACEVAL RESLIST PLACELIST PLACELIST2 THISPLACE)
      (SETQ RESLIST RESIDUES)
      (SETQ PLACELIST NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT RESLIST) (RETURN NIL)))
        (PROGN
         (SETQ PLACEVAL
                 (ALGINT-SUBF (CONS (CONS (GETPOWER (FKERN X) 1) 1) NIL)
                  (CAAR RESLIST)))
         (COND
          ((NEQ 0 (CDAR RESLIST))
           (COND
            ((NULL (CAR (SUBZERO2 (CDR PLACEVAL) X)))
             (PROGN
              (COND ((NULL ANSWER) (SETQ ANSWER 'INFINITY))
                    ((EQ ANSWER 'FINITE) (SETQ ANSWER 'MIXED)))
              (COND
               (*TRA
                (PROGN
                 (PRIN2 "We have an residue at infinity")
                 (TERPRI)
                 "We have an residue at infinity")))))
            (T
             (PROGN
              (COND ((NULL ANSWER) (SETQ ANSWER 'FINITE))
                    ((EQ ANSWER 'INFINITY) (SETQ ANSWER 'MIXED)))
              (SETQ PLACELIST (CONS PLACEVAL PLACELIST))
              (COND
               (*TRA
                (PROGN
                 (PRIN2 "This is a finite residue")
                 (TERPRI)
                 "This is a finite residue"))))))))
         (SETQ RESLIST (CDR RESLIST)))
        (GO WHILELABEL))
      (COND ((EQ ANSWER 'MIXED) (RETURN ANSWER)))
      (COND
       ((EQ ANSWER 'INFINITY)
        (PROGN
         (SETQ THISPLACE (LIST X 'QUOTIENT 1 X))
         (SETQ ANSWER
                 (PRECOATES
                  (PROG (U FORALL-RESULT FORALL-ENDPTR)
                    (SETQ U RESIDUES)
                    (COND ((NULL U) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (U) (INFSUBS U)) (CAR U))
                                          NIL)))
                   LOOPLABEL
                    (SETQ U (CDR U))
                    (COND ((NULL U) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (U) (INFSUBS U)) (CAR U)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  X T))
         (COND ((ATOM ANSWER) (RETURN ANSWER))
               (T (RETURN (SUBSTITUTESQ ANSWER (LIST THISPLACE))))))))
      (SETQ PLACELIST2 (REMOVEDUPLICATES PLACELIST))
      (SETQ ANSWER (CONS 1 1))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "The divisor has elements at:")
          (TERPRI)
          "The divisor has elements at:")
         (PROG (J FORALL-RESULT FORALL-ENDPTR)
           (SETQ J PLACELIST2)
           (COND ((NULL J) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (PRINTSQ J)) (CAR J)) NIL)))
          LOOPLABEL
           (SETQ J (CDR J))
           (COND ((NULL J) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (PRINTSQ J)) (CAR J)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACELIST2) (RETURN NIL)))
        (PROG (PLACELIST3 EXTRASUBS U BPLACE)
          (SETQ RESLIST RESIDUES)
          (SETQ PLACELIST3 PLACELIST)
          (SETQ PLACEVAL NIL)
          (PROG ()
           WHILELABEL
            (COND ((NOT RESLIST) (RETURN NIL)))
            (PROGN
             (COND
              ((EQUAL (CAR PLACELIST2) (CAR PLACELIST3))
               (PROGN
                (SETQ PLACEVAL (CONS (CDAR RESLIST) PLACEVAL))
                (SETQ THISPLACE (CAAR RESLIST))
                (SETQ U (CAAR RESLIST))
                (SETQ BPLACE (BASICPLACE U))
                (SETQ U (EXTENPLACE U))
                (SETQ EXTRASUBS (CONS U EXTRASUBS)))))
             (SETQ RESLIST (CDR RESLIST))
             (SETQ PLACELIST3 (CDR PLACELIST3)))
            (GO WHILELABEL))
          (COND
           (*TRA
            (PROGN
             (PRINC "List of multiplicities at this place:")
             (PROGN (PRIN2 PLACEVAL) (TERPRI) PLACEVAL)
             (PRINC "with substitutions:")
             (SUPERPRINT EXTRASUBS))))
          (COND
           ((NEQ 0 (MAPPLY (FUNCTION PLUS2) PLACEVAL))
            (INTERR "Divisor not effective")))
          (GET-CORRECT-SQRTS BPLACE)
          (SETQ U (PBUILD X EXTRASUBS PLACEVAL))
          (SQRTSAVE BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS BPLACE)
          (COND ((ATOM U) (PROGN (SETQ PLACELIST2 NIL) (SETQ ANSWER U)))
                (T
                 (PROGN
                  (SETQ ANSWER
                          (SUBSTITUTESQ (*MULTSQ ANSWER U)
                           (ANTISUBS THISPLACE X)))
                  (SETQ PLACELIST2 (CDR PLACELIST2))))))
        (GO WHILELABEL))
      (RETURN ANSWER))) 
(PUT 'DLIST 'NUMBER-OF-ARGS 1) 
(PUT 'DLIST 'DEFINED-ON-LINE '139) 
(PUT 'DLIST 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'DLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DLIST (U)
    (COND ((NULL U) NIL) ((NULL (CAR U)) (DLIST (CDR U)))
          (T (APPEND (CAR U) (DLIST (CDR U)))))) 
(PUT 'DEBRANCH 'NUMBER-OF-ARGS 2) 
(PUT 'DEBRANCH 'DEFINED-ON-LINE '148) 
(PUT 'DEBRANCH 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'DEBRANCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEBRANCH (EXTRASUBS RESLIST)
    (PROG (SUBSTLIST)
      (PROG (U)
        (SETQ U (DLIST EXTRASUBS))
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (COND
            ((NOT (MEMBER (CAR U) SUBSTLIST))
             (SETQ SUBSTLIST (CONS (CAR U) SUBSTLIST)))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT SUBSTLIST) (RETURN NIL)))
        (PROG (TSQRT USQRT WITH1 WITH2 WITHOUT1 WITHOUT2 WRES A1 A2 B1 B2)
          (SETQ TSQRT (CAR SUBSTLIST))
          (SETQ SUBSTLIST (CDR SUBSTLIST))
          (SETQ WRES RESLIST)
          (PROG (PLACE)
            (SETQ PLACE EXTRASUBS)
           LAB
            (COND ((NULL PLACE) (RETURN NIL)))
            ((LAMBDA (PLACE)
               (PROGN
                (SETQ USQRT (ASSOC TSQRT PLACE))
                (COND ((NULL USQRT) (INTERR "Places not all there")))
                (COND
                 ((EQ (CADR USQRT) 'SQRT)
                  (PROGN
                   (SETQ WITH2 (CONS (CAR WRES) WITH2))
                   (SETQ WITH1 (CONS (DELETE USQRT PLACE) WITH1))))
                 (T
                  (PROGN
                   (COND
                    ((NOT (EQ (CADR USQRT) 'MINUS))
                     (INTERR "Ramification format error")))
                   (SETQ WITHOUT2 (CONS (CAR WRES) WITHOUT2))
                   (SETQ WITHOUT1 (CONS (DELETE USQRT PLACE) WITHOUT1)))))
                (SETQ WRES (CDR WRES))))
             (CAR PLACE))
            (SETQ PLACE (CDR PLACE))
            (GO LAB))
          (COND ((NULL WITH1) (GO ITSWITHOUT)))
          (COND ((NULL WITHOUT1) (GO ITSWITH)))
          (SETQ A1 WITH1)
          (SETQ A2 WITH2)
         OUTERLOOP
          (SETQ B1 WITHOUT1)
          (SETQ B2 WITHOUT2)
         INNERLOOP
          (COND
           ((EQUAL (CAR A1) (CAR B1))
            (PROGN
             (COND ((NEQ (CAR A2) (CAR B2)) (RETURN NIL))
                   (T (GO OUTERITERATE))))))
          (SETQ B1 (CDR B1))
          (SETQ B2 (CDR B2))
          (COND ((NULL B1) (RETURN NIL)) (T (GO INNERLOOP)))
         OUTERITERATE
          (SETQ A1 (CDR A1))
          (SETQ A2 (CDR A2))
          (COND (A1 (GO OUTERLOOP)))
          (COND
           (*TRA
            (PROGN
             (PRINC "Residues reduce to:")
             (PROGN (PRIN2 WITHOUT2) (TERPRI) WITHOUT2)
             (PROGN (PRIN2 "at ") (TERPRI) "at ")
             (MAPC WITHOUT1 (FUNCTION PRINTPLACE)))))
          (SETQ EXTRASUBS WITHOUT1)
          (SETQ RESLIST WITHOUT2)
          (RETURN NIL)
         ITSWITHOUT
          (SETQ WITH1 WITHOUT1)
          (SETQ WITH2 WITHOUT2)
         ITSWITH
          (SETQ EXTRASUBS
                  (PROG (U FORALL-RESULT FORALL-ENDPTR)
                    (SETQ U WITH1)
                    (COND ((NULL U) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (U) (DELETE (ASSOC TSQRT U) U))
                                      (CAR U))
                                     NIL)))
                   LOOPLABEL
                    (SETQ U (CDR U))
                    (COND ((NULL U) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (U) (DELETE (ASSOC TSQRT U) U)) (CAR U))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (COND
           (*TRA
            (PROGN
             (PROGN
              (PRIN2 "The following appears throughout the list ")
              (TERPRI)
              "The following appears throughout the list ")
             (PROGN (PRIN2 TSQRT) (TERPRI) TSQRT))))
          (SETQ RESLIST WITH2))
        (GO WHILELABEL))
      (RETURN (CONS EXTRASUBS RESLIST)))) 
(PUT 'PBUILD 'NUMBER-OF-ARGS 3) 
(PUT 'PBUILD 'DEFINED-ON-LINE '230) 
(PUT 'PBUILD 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'PBUILD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PBUILD (X EXTRASUBS PLACEVAL)
    (PROG (MULTIVALS U V ANSWER)
      (SETQ U (DEBRANCH EXTRASUBS PLACEVAL))
      (SETQ EXTRASUBS (CAR U))
      (SETQ PLACEVAL (CDR U))
      (COND ((GREATERP (LENGTH (CAR EXTRASUBS)) 1) (RETURN 'DIFFICULT)))
      (SETQ MULTIVALS (MAPOVERCAR (DLIST EXTRASUBS)))
      (SETQ U (SIMPTIMES (REMOVEDUPLICATES MULTIVALS)))
      (SETQ ANSWER (CONS 1 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT EXTRASUBS) (RETURN NIL)))
        (PROGN
         (SETQ V (SUBSTITUTESQ U (CAR EXTRASUBS)))
         (SETQ V (*ADDSQ U (NEGSQ (SUBZERO V X))))
         (SETQ V (MKORD1 V X))
         (COND (*TRA (PROGN (PRINC "Required component is ") (PRINTSQ V))))
         (SETQ ANSWER (*MULTSQ ANSWER (*EXPTSQ V (CAR PLACEVAL))))
         (SETQ EXTRASUBS (CDR EXTRASUBS))
         (SETQ PLACEVAL (CDR PLACEVAL)))
        (GO WHILELABEL))
      (COND
       ((GREATERP (LENGTH (JFACTOR (CDR (SQRT2TOP (*INVSQ ANSWER))) X)) 1)
        (RETURN 'MANY-POLES))
       (T (RETURN ANSWER))))) 
(PUT 'FINDORD 'NUMBER-OF-ARGS 2) 
(PUT 'FINDORD 'DEFINED-ON-LINE '260) 
(PUT 'FINDORD 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'FINDORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDORD (V X)
    (PROG (NORD VD)
      (SETQ NORD 0)
      (SETQ TAYLORVARIABLE X)
      (PROG ()
       WHILELABEL
        (COND ((NOT (INVOLVESQ V SQRT-INTVAR)) (RETURN NIL)))
        (SETQ V (SUBSTITUTESQ V (LIST (CONS X (LIST 'EXPT X 2)))))
        (GO WHILELABEL))
      (SETQ VD (TAYLORFORM V))
     LOOP
      (SETQ NORD (PLUS NORD 1))
      (COND ((NULL (CAR (TAYLOREVALUATE VD NORD))) (GO LOOP)))
      (RETURN NORD))) 
(PUT 'MKORD1 'NUMBER-OF-ARGS 2) 
(PUT 'MKORD1 'DEFINED-ON-LINE '277) 
(PUT 'MKORD1 'DEFINED-IN-FILE 'ALGINT/PRECOATS.RED) 
(PUT 'MKORD1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKORD1 (V X)
    (PROG (NORD)
      (SETQ NORD (FINDORD V X))
      (COND ((IEQUAL NORD 1) (RETURN V)))
      (COND
       (*TRA
        (PROGN
         (PRINC "Order reduction: ")
         (PRINTSQ V)
         (PRINC "from order ")
         (PRINC NORD)
         (PROGN (PRIN2 " to order 1") (TERPRI) " to order 1"))))
      (RETURN
       (CONS
        (LIST (CONS (GETPOWER (FKERN (LIST 'NTHROOT (MK*SQ V) NORD)) 1) 1))
        1)))) 
(ENDMODULE) 
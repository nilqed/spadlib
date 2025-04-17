(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'F5CORE)) 
(REVISION 'F5CORE "$Id: f5core.red 6359 2022-07-29 06:13:09Z thomas-sturm $") 
(COPYRIGHT 'F5CORE "(c) 2022 A. Demin, T. Sturm, MPI Informatics, Germany") 
(DE CORE_CRITICALPAIR (TT K U L V SUGAR) (LIST 'CP TT K U L V SUGAR)) 
(PUT 'CORE_CRITICALPAIR 'NUMBER-OF-ARGS 6) 
(PUTC 'CORE_CRITICALPAIR 'INLINE
      '(LAMBDA (TT K U L V SUGAR) (LIST 'CP TT K U L V SUGAR))) 
(DE CORE_GETPAIRLCM (P) (CADR P)) 
(PUT 'CORE_GETPAIRLCM 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETPAIRLCM 'INLINE '(LAMBDA (P) (CADR P))) 
(DE CORE_GETPAIRFIRST (P) (CONS (CADDR P) (CADDDR P))) 
(PUT 'CORE_GETPAIRFIRST 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETPAIRFIRST 'INLINE '(LAMBDA (P) (CONS (CADDR P) (CADDDR P)))) 
(DE CORE_GETPAIRSECOND (P) (CONS (CAR (CDDDDR P)) (CADR (CDDDDR P)))) 
(PUT 'CORE_GETPAIRSECOND 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETPAIRSECOND 'INLINE
      '(LAMBDA (P) (CONS (CAR (CDDDDR P)) (CADR (CDDDDR P))))) 
(DE CORE_GETPAIRSUGAR (P) (CADDR (CDDDDR P))) 
(PUT 'CORE_GETPAIRSUGAR 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETPAIRSUGAR 'INLINE '(LAMBDA (P) (CADDR (CDDDDR P)))) 
(DE CORE_REWRITERULE (IDX TT) (LIST 'RR IDX TT)) 
(PUT 'CORE_REWRITERULE 'NUMBER-OF-ARGS 2) 
(PUTC 'CORE_REWRITERULE 'INLINE '(LAMBDA (IDX TT) (LIST 'RR IDX TT))) 
(DE CORE_GETRULEINDEX (R) (CADR R)) 
(PUT 'CORE_GETRULEINDEX 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETRULEINDEX 'INLINE '(LAMBDA (R) (CADR R))) 
(DE CORE_GETRULETERM (R) (CADDR R)) 
(PUT 'CORE_GETRULETERM 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETRULETERM 'INLINE '(LAMBDA (R) (CADDR R))) 
(FLUID '(CORE_INITIALBASISSIZE*)) 
(SETQ CORE_INITIALBASISSIZE* 10000) 
(PUT 'CORE_BASISTRACKER 'NUMBER-OF-ARGS 1) 
(DE CORE_BASISTRACKER (CAPACITY) (LIST 'BT (MKVECT CAPACITY) 0 CAPACITY)) 
(PUT 'CORE_ADDPOLY 'NUMBER-OF-ARGS 2) 
(DE CORE_ADDPOLY (R F)
    (PROG (POLYS NEWPOLYS CAPACITY IDX)
      (SETQ CAPACITY 0)
      (SETQ IDX 0)
      (SETQ POLYS (CADR R))
      (SETQ IDX (CADDR R))
      (SETQ CAPACITY (CADDDR R))
      (COND
       ((GEQ IDX CAPACITY)
        (PROGN
         (SETQ CAPACITY (TIMES CAPACITY 2))
         (SETCAR (CDDDR R) CAPACITY)
         (SETQ NEWPOLYS (MKVECT CAPACITY))
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((MINUSP (DIFFERENCE IDX I)) (RETURN NIL)))
           (PUTV NEWPOLYS I (GETV POLYS I))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETCAR (CDR R) NEWPOLYS))))
      (PUTV (CADR R) (CADDR R) F)
      (SETCAR (CDDR R) (IPLUS2 (CADDR R) 1)))) 
(DE CORE_SETPOLY (R I F) (PUTV (CADR R) I F)) 
(PUT 'CORE_SETPOLY 'NUMBER-OF-ARGS 3) 
(PUTC 'CORE_SETPOLY 'INLINE '(LAMBDA (R I F) (PUTV (CADR R) I F))) 
(DE CORE_GETPOLY (R I) (GETV (CADR R) I)) 
(PUT 'CORE_GETPOLY 'NUMBER-OF-ARGS 2) 
(PUTC 'CORE_GETPOLY 'INLINE '(LAMBDA (R I) (GETV (CADR R) I))) 
(DE CORE_GETBASISIDX (R) (IDIFFERENCE (CADDR R) 1)) 
(PUT 'CORE_GETBASISIDX 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_GETBASISIDX 'INLINE '(LAMBDA (R) (IDIFFERENCE (CADDR R) 1))) 
(PUT 'CORE_PAIRTOTALDEGREECMP 'NUMBER-OF-ARGS 2) 
(DE CORE_PAIRTOTALDEGREECMP (P1 P2)
    (ILESSP (POLY_TOTALDEGTERM (CADR P1)) (POLY_TOTALDEGTERM (CADR P2)))) 
(PUT 'CORE_PAIRSUGARCMP 'NUMBER-OF-ARGS 2) 
(DE CORE_PAIRSUGARCMP (P1 P2) (ILESSP (CADDR (CDDDDR P1)) (CADDR (CDDDDR P2)))) 
(PUT 'CORE_PAIRLCMCMP 'NUMBER-OF-ARGS 2) 
(DE CORE_PAIRLCMCMP (P1 P2) (POLY_CMPTERM (CADR P1) (CADR P2))) 
(PUT 'CORE_ASSOCSGNCMP 'NUMBER-OF-ARGS 2) 
(DE CORE_ASSOCSGNCMP (PR1 PR2)
    (LP_CMPSGN (LP_SGN (CDR PR1)) (LP_SGN (CDR PR2)))) 
(PUT 'CORE_ASSOCLENGTHCMP 'NUMBER-OF-ARGS 2) 
(DE CORE_ASSOCLENGTHCMP (PR1 PR2)
    (ILESSP (POLY_LENGTH (LP_EVAL (CDR PR1)))
            (POLY_LENGTH (LP_EVAL (CDR PR2))))) 
(PUT 'CORE_ASSOCLEADCMP 'NUMBER-OF-ARGS 2) 
(DE CORE_ASSOCLEADCMP (PR1 PR2)
    (POLY_LEADTOTALDEGREECMP (LP_EVAL (CDR PR1)) (LP_EVAL (CDR PR2)))) 
(PUT 'CORE_CONSTRUCTMODULE 'NUMBER-OF-ARGS 1) 
(DE CORE_CONSTRUCTMODULE (INPUTBASIS)
    (PROG (OUTPUTMODULE I)
      (SETQ I 0)
      (COND
       (*F5FRACTIONFREE
        (SETQ INPUTBASIS
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F INPUTBASIS)
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F) (POLY_SCALEDENOMINATORS F))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F) (POLY_SCALEDENOMINATORS F)) (CAR F))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ INPUTBASIS (CORE_INTERREDUCEINPUT INPUTBASIS))
      (SETQ INPUTBASIS (SORT INPUTBASIS 'POLY_LEADTOTALDEGREECMP))
      (SETQ I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT INPUTBASIS) (RETURN NIL)))
        (PROGN
         (PROG (W1)
           (SETQ W1
                   (LP_LABELEDPOLYNOMIAL1
                    (PROG1 (CAR INPUTBASIS) (SETQ INPUTBASIS (CDR INPUTBASIS)))
                    I))
           (SETQ OUTPUTMODULE (CONS W1 OUTPUTMODULE))
           (RETURN W1))
         (SETQ I (PLUS I 1)))
        (GO WHILELABEL))
      (COND (*F5STATISTICS (STAT_INIT (DIFFERENCE I 1))))
      (RETURN (REVERSIP OUTPUTMODULE)))) 
(PUT 'CORE_NORMALFORM 'NUMBER-OF-ARGS 4) 
(DE CORE_NORMALFORM (F GPREV R TOPREDUCE)
    (PROG (UPDATED REDUCER REDUCED UPDATEDTORETURN)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ UPDATED NIL)
         (PROG (G)
           (SETQ G GPREV)
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (SETQ REDUCER (LP_EVAL (GETV (CADR R) G)))
               (COND
                ((AND (NOT (POLY_ISZERO? REDUCER)) (NOT (POLY_ISZERO? F)))
                 (PROGN
                  (PROG (G126)
                    (SETQ G126
                            (COND
                             (TOPREDUCE (POLY_TRYTOPREDUCTIONSTEP F REDUCER))
                             (T (POLY_TRYREDUCTIONSTEP F REDUCER))))
                    (SETQ REDUCED (CAR G126))
                    (SETQ F (CDR G126))
                    (RETURN G126))
                  (SETQ UPDATED (OR REDUCED UPDATED)))))))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (SETQ UPDATEDTORETURN (OR UPDATED UPDATEDTORETURN)))
        (COND ((NOT (NOT UPDATED)) (GO REPEATLABEL))))
      (RETURN (CONS UPDATEDTORETURN F)))) 
(PUT 'CORE_NORMALFORMREDUCERS 'NUMBER-OF-ARGS 3) 
(DE CORE_NORMALFORMREDUCERS (F REDUCERS TOPREDUCE)
    (PROG (UPDATED REDUCER REDUCED UPDATEDTORETURN)
      (SETQ UPDATED T)
      (PROG ()
       WHILELABEL
        (COND ((NOT UPDATED) (RETURN NIL)))
        (PROGN
         (SETQ UPDATED NIL)
         (PROG (REDUCER)
           (SETQ REDUCER REDUCERS)
          LAB
           (COND ((NULL REDUCER) (RETURN NIL)))
           ((LAMBDA (REDUCER)
              (PROGN
               (COND
                ((AND (NOT (POLY_ISZERO? REDUCER)) (NOT (POLY_ISZERO? F)))
                 (PROGN
                  (PROG (G127)
                    (SETQ G127
                            (COND
                             (TOPREDUCE (POLY_TRYTOPREDUCTIONSTEP F REDUCER))
                             (T (POLY_TRYREDUCTIONSTEP F REDUCER))))
                    (SETQ REDUCED (CAR G127))
                    (SETQ F (CDR G127))
                    (RETURN G127))
                  (SETQ UPDATED (OR UPDATED REDUCED))
                  (SETQ UPDATEDTORETURN (OR UPDATED REDUCED)))))))
            (CAR REDUCER))
           (SETQ REDUCER (CDR REDUCER))
           (GO LAB)))
        (GO WHILELABEL))
      (RETURN (CONS UPDATEDTORETURN F)))) 
(PUT 'CORE_GETREDUCERS 'NUMBER-OF-ARGS 2) 
(DE CORE_GETREDUCERS (I G)
    (PROG (REDUCERS POLY J)
      (SETQ J 0)
      (SETQ J 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ POLY (PROG1 (CAR G) (SETQ G (CDR G))))
         (COND
          ((NOT (IEQUAL J I))
           (PROGN (SETQ REDUCERS (CONS POLY REDUCERS)) POLY))))
        (GO WHILELABEL))
      (SETQ J (IPLUS2 J 1))
      (RETURN REDUCERS))) 
(PUT 'CORE_INTERREDUCEINPUT 'NUMBER-OF-ARGS 1) 
(DE CORE_INTERREDUCEINPUT (INPUT)
    (PROG (REDUCERS REDUCER REDUCEME REDUCED F READY UPDATED)
      (SETQ UPDATED T)
      (SETQ INPUT (SORT INPUT 'POLY_LEADTOTALDEGREECMP))
      (SETQ INPUT
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F INPUT)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (POLY_NORMALIZE F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (POLY_NORMALIZE F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT UPDATED) (RETURN NIL)))
        (PROGN
         (SETQ UPDATED NIL)
         (SETQ READY NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT INPUT) (RETURN NIL)))
           (PROGN
            (SETQ REDUCEME (PROG1 (CAR INPUT) (SETQ INPUT (CDR INPUT))))
            (SETQ REDUCERS INPUT)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND REDUCERS (NOT (POLY_ISZERO? REDUCEME))))
                (RETURN NIL)))
              (PROGN
               (SETQ REDUCER
                       (PROG1 (CAR REDUCERS) (SETQ REDUCERS (CDR REDUCERS))))
               (PROG (G128)
                 (SETQ G128 (POLY_TRYTOPREDUCTIONSTEP REDUCEME REDUCER))
                 (SETQ REDUCED (CAR G128))
                 (SETQ REDUCEME (CDR G128))
                 (RETURN G128))
               (COND
                (REDUCED
                 (PROGN
                  (PROGN (SETQ REDUCERS (CONS REDUCER REDUCERS)) REDUCER)
                  (SETQ UPDATED T)))))
              (GO WHILELABEL))
            (SETQ REDUCERS READY)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND REDUCERS (NOT (POLY_ISZERO? REDUCEME))))
                (RETURN NIL)))
              (PROGN
               (SETQ REDUCER
                       (PROG1 (CAR REDUCERS) (SETQ REDUCERS (CDR REDUCERS))))
               (PROG (G129)
                 (SETQ G129 (POLY_TRYTOPREDUCTIONSTEP REDUCEME REDUCER))
                 (SETQ REDUCED (CAR G129))
                 (SETQ REDUCEME (CDR G129))
                 (RETURN G129))
               (COND
                (REDUCED
                 (PROGN
                  (PROGN (SETQ REDUCERS (CONS REDUCER REDUCERS)) REDUCER)
                  (SETQ UPDATED T)))))
              (GO WHILELABEL))
            (COND
             ((NOT (POLY_ISZERO? REDUCEME))
              (PROG (W1)
                (SETQ W1 (POLY_NORMALIZE REDUCEME))
                (SETQ READY (CONS W1 READY))
                (RETURN W1)))))
           (GO WHILELABEL))
         (SETQ INPUT READY))
        (GO WHILELABEL))
      (RETURN READY))) 
(PUT 'CORE_INTERREDUCEBASIS 'NUMBER-OF-ARGS 2) 
(DE CORE_INTERREDUCEBASIS (GPREV R)
    (PROG (REDUCERS UPDATED REDUCED F NEWGPREV I)
      (SETQ I 0)
      (SETQ UPDATED T)
      (PROG ()
       WHILELABEL
        (COND ((NOT UPDATED) (RETURN NIL)))
        (PROGN
         (SETQ UPDATED NIL)
         (SETQ NEWGPREV NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT GPREV) (RETURN NIL)))
           (PROGN
            (SETQ I (PROG1 (CAR GPREV) (SETQ GPREV (CDR GPREV))))
            (SETQ F (LP_EVAL (GETV (CADR R) I)))
            (SETQ REDUCERS (APPEND NEWGPREV GPREV))
            (PROG (G130)
              (SETQ G130 (CORE_NORMALFORM F REDUCERS R NIL))
              (SETQ REDUCED (CAR G130))
              (SETQ F (CDR G130))
              (RETURN G130))
            (SETQ UPDATED (OR UPDATED REDUCED))
            (LP_SETEVAL (GETV (CADR R) I) F)
            (COND
             ((NOT (POLY_ISZERO? F))
              (PROGN (SETQ NEWGPREV (CONS I NEWGPREV)) I))))
           (GO WHILELABEL))
         (SETQ GPREV NEWGPREV))
        (GO WHILELABEL))
      (RETURN NEWGPREV))) 
(PUT 'CORE_ISTOPREDUCIBLETERM 'NUMBER-OF-ARGS 3) 
(DE CORE_ISTOPREDUCIBLETERM (M GPREV R)
    (PROG (GLEAD ISREDUCIBLE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND GPREV (NOT ISREDUCIBLE))) (RETURN NIL)))
        (PROGN
         (SETQ GLEAD
                 (POLY_LEADTERM
                  (LP_EVAL
                   (GETV (CADR R)
                         (PROG1 (CAR GPREV) (SETQ GPREV (CDR GPREV)))))))
         (COND ((POLY_DIVIDESTERM? GLEAD M) (SETQ ISREDUCIBLE T))))
        (GO WHILELABEL))
      (RETURN ISREDUCIBLE))) 
(PUT 'CORE_ADDRULE 'NUMBER-OF-ARGS 3) 
(DE CORE_ADDRULE (RULE SGN K)
    (PROG (RULES)
      (SETQ RULES (GETV RULE (LP_INDEXSGN SGN)))
      (PROG (W1)
        (SETQ W1 (LIST 'RR K (LP_TERMSGN SGN)))
        (SETQ RULES (CONS W1 RULES))
        (RETURN W1))
      (PUTV RULE (LP_INDEXSGN SGN) RULES))) 
(PUT 'CORE_FINDREWRITING 'NUMBER-OF-ARGS 4) 
(DE CORE_FINDREWRITING (U K R RULE)
    (PROG (SGN USGNT FOUNDREWRITER RULESATSGN RULECURRENT REWRITER)
      (SETQ REWRITER 0)
      (SETQ REWRITER K)
      (SETQ SGN (LP_SGN (GETV (CADR R) K)))
      (SETQ USGNT (LP_TERMSGN (LP_MULSGN SGN U)))
      (SETQ RULESATSGN (GETV RULE (LP_INDEXSGN SGN)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND RULESATSGN (NOT FOUNDREWRITER))) (RETURN NIL)))
        (PROGN
         (SETQ RULECURRENT
                 (PROG1 (CAR RULESATSGN) (SETQ RULESATSGN (CDR RULESATSGN))))
         (SETQ FOUNDREWRITER (POLY_DIVIDESTERM? (CADDR RULECURRENT) USGNT))
         (COND (FOUNDREWRITER (SETQ REWRITER (CADR RULECURRENT)))))
        (GO WHILELABEL))
      (RETURN REWRITER))) 
(DE CORE_ISREWRITABLE (U K R RULE)
    (NOT (IEQUAL (CORE_FINDREWRITING U K R RULE) K))) 
(PUT 'CORE_ISREWRITABLE 'NUMBER-OF-ARGS 4) 
(PUTC 'CORE_ISREWRITABLE 'INLINE
      '(LAMBDA (U K R RULE) (NOT (IEQUAL (CORE_FINDREWRITING U K R RULE) K)))) 
(PUT 'CORE_FILTERREDUNDANT 'NUMBER-OF-ARGS 2) 
(DE CORE_FILTERREDUNDANT (GPREV R)
    (PROG (GNEW ALG GSORTED GLEAD GI)
      (SETQ GI 0)
      (SETQ ALG
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I GPREV)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I) (CONS I (GETV (CADR R) I)))
                                  (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (CONS I (GETV (CADR R) I))) (CAR I))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALG (SORT ALG 'CORE_ASSOCLEADCMP))
      (SETQ GSORTED
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ALG)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT GSORTED) (RETURN NIL)))
        (PROGN
         (SETQ GI (PROG1 (CAR GSORTED) (SETQ GSORTED (CDR GSORTED))))
         (SETQ GLEAD (POLY_LEADTERM (LP_EVAL (GETV (CADR R) GI))))
         (COND
          ((NOT (CORE_ISTOPREDUCIBLETERM GLEAD GNEW R))
           (PROGN (SETQ GNEW (CONS GI GNEW)) GI))))
        (GO WHILELABEL))
      (RETURN GNEW))) 
(PUT 'CORE_NORMALIZEBASIS 'NUMBER-OF-ARGS 1) 
(DE CORE_NORMALIZEBASIS (BASIS)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X BASIS)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (LP_NORMALIZE X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (LP_NORMALIZE X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CORE_STANDARDIZEOUTPUT 'NUMBER-OF-ARGS 1) 
(DE CORE_STANDARDIZEOUTPUT (BASIS)
    (PROG (NORMALIZEDBASIS)
      (SETQ NORMALIZEDBASIS (CORE_NORMALIZEBASIS BASIS))
      (RETURN (SORT NORMALIZEDBASIS 'LP_CMPLPLEADREVERSE)))) 
(PUT 'CORE_CHECKIDEALINCLUSION1 'NUMBER-OF-ARGS 2) 
(DE CORE_CHECKIDEALINCLUSION1 (BASIS POLYS)
    (PROG (INCLUDED POLY NF FLAG_)
      (SETQ INCLUDED T)
      (SETQ BASIS
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F BASIS)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (LP_EVAL F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (LP_EVAL F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND INCLUDED POLYS)) (RETURN NIL)))
        (PROGN
         (SETQ POLY (LP_EVAL (PROG1 (CAR POLYS) (SETQ POLYS (CDR POLYS)))))
         (PROG (G131)
           (SETQ G131 (CORE_NORMALFORMREDUCERS POLY BASIS T))
           (SETQ FLAG_ (CAR G131))
           (SETQ NF (CDR G131))
           (RETURN G131))
         (COND ((NOT (POLY_ISZERO? NF)) (SETQ INCLUDED NIL))))
        (GO WHILELABEL))
      (RETURN INCLUDED))) 
(PUT 'CORE_ISGROEBNER1 'NUMBER-OF-ARGS 1) 
(DE CORE_ISGROEBNER1 (BASIS)
    (PROG (ISBASIS TMP SPOLY NF FLAG_)
      (SETQ ISBASIS T)
      (SETQ BASIS
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F BASIS)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (LP_EVAL F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (LP_EVAL F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT BASIS) (RETURN NIL)))
        (PROGN
         (SETQ TMP (CDR BASIS))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND ISBASIS TMP)) (RETURN NIL)))
           (PROGN
            (SETQ SPOLY (POLY_SPOLY (CAR TMP) (CAR BASIS)))
            (PROG (G132)
              (SETQ G132 (CORE_NORMALFORMREDUCERS SPOLY BASIS T))
              (SETQ FLAG_ (CAR G132))
              (SETQ NF (CDR G132))
              (RETURN G132))
            (COND ((NOT (POLY_ISZERO? NF)) (SETQ ISBASIS NIL)))
            (PROG1 (CAR TMP) (SETQ TMP (CDR TMP))))
           (GO WHILELABEL))
         (PROG1 (CAR BASIS) (SETQ BASIS (CDR BASIS))))
        (GO WHILELABEL))
      (RETURN ISBASIS))) 
(PUT 'CORE_FINDTOPREDUCERF5 'NUMBER-OF-ARGS 5) 
(DE CORE_FINDTOPREDUCERF5 (K GPREV NEWGCURR R RULE)
    (PROG (TT RK RKEVAL RKSGN RJ U RJSGNMULT TJ J REDUCER)
      (SETQ J 0)
      (SETQ REDUCER 0)
      (SETQ RK (GETV (CADR R) K))
      (SETQ RKEVAL (LP_EVAL RK))
      (SETQ RKSGN (LP_SGN RK))
      (SETQ TT (POLY_LEADTERM RKEVAL))
      (PROG (J)
        (SETQ J NEWGCURR)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ RJ (GETV (CADR R) J))
            (SETQ TJ (POLY_LEADTERM (LP_EVAL RJ)))
            (COND
             ((AND (IEQUAL REDUCER 0) (POLY_DIVIDESTERM? TJ TT))
              (PROGN
               (SETQ U (POLY_DIVTERM TT TJ))
               (SETQ RJSGNMULT (LP_MULSGN (LP_SGN RJ) U))
               (COND
                ((NOT (LP_EQSGN RJSGNMULT RKSGN))
                 (COND
                  ((NOT (NOT (IEQUAL (CORE_FINDREWRITING U J R RULE) J)))
                   (COND
                    ((NOT
                      (CORE_ISTOPREDUCIBLETERM (LP_TERMSGN RJSGNMULT) GPREV R))
                     (SETQ REDUCER J))))))))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN REDUCER))) 
(PUT 'CORE_TOPREDUCTIONF5 'NUMBER-OF-ARGS 5) 
(DE CORE_TOPREDUCTIONF5 (K GPREV NEWGCURR R RULE)
    (PROG (RK RJ U NEWPOLY REDUCED REDUCERSGN FLAG_ J)
      (SETQ J 0)
      (SETQ RK (GETV (CADR R) K))
      (COND
       ((LP_ISZERO? RK)
        (PROGN
         (COND (*F5STATISTICS (STAT_INCREMENTZEROREDUCTIONS)))
         (RETURN (CONS NIL NIL)))))
      (SETQ J (CORE_FINDTOPREDUCERF5 K GPREV NEWGCURR R RULE))
      (COND
       ((IEQUAL J 0)
        (PROGN (PUTV (CADR R) K (LP_NORMALIZE RK)) (RETURN (CONS K NIL)))))
      (SETQ RJ (GETV (CADR R) J))
      (PROG (G133)
        (SETQ G133 (POLY_TRYTOPREDUCTIONSTEP (LP_EVAL RK) (LP_EVAL RJ)))
        (SETQ FLAG_ (CAR G133))
        (SETQ REDUCED (CDR G133))
        (RETURN G133))
      (COND
       ((NOT (POLY_ISZERO? REDUCED)) (SETQ REDUCED (POLY_NORMALIZE REDUCED))))
      (SETQ U
              (POLY_DIVTERM (POLY_LEADTERM (LP_EVAL RK))
               (POLY_LEADTERM (LP_EVAL RJ))))
      (SETQ REDUCERSGN (LP_MULSGN (LP_SGN RJ) U))
      (RETURN
       (COND
        ((LP_CMPSGN REDUCERSGN (LP_SGN RK))
         (PROGN
          (LP_SETEVAL RK REDUCED)
          (PUTV (CADR R) K RK)
          (CONS NIL (CONS K NIL))))
        (T
         (PROGN
          (SETQ NEWPOLY (LP_LABELEDPOLYNOMIAL2 REDUCED REDUCERSGN))
          (CORE_ADDPOLY R NEWPOLY)
          (CORE_ADDRULE RULE (LP_SGN NEWPOLY) (IDIFFERENCE (CADDR R) 1))
          (CONS NIL (CONS K (CONS (IDIFFERENCE (CADDR R) 1) NIL))))))))) 
(PUT 'CORE_INSERTSORTED 'NUMBER-OF-ARGS 3) 
(DE CORE_INSERTSORTED (TODO J R)
    (PROG (TMP SJ S)
      (COND ((NULL TODO) (RETURN (LIST J))))
      (SETQ SJ (LP_SGN (GETV (CADR R) J)))
      (SETQ S (LP_SGN (GETV (CADR R) (CAR TODO))))
      (COND ((LP_CMPSGN SJ S) (RETURN (CONS J TODO))))
      (SETQ TMP TODO)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (CDR TODO)
                (LP_CMPSGN (LP_SGN (GETV (CADR R) (CADR TODO))) SJ)))
          (RETURN NIL)))
        (PROG1 (CAR TODO) (SETQ TODO (CDR TODO)))
        (GO WHILELABEL))
      (SETCDR TODO (CONS J (CDR TODO)))
      (RETURN TMP))) 
(PUT 'CORE_REDUCTION 'NUMBER-OF-ARGS 5) 
(DE CORE_REDUCTION (S GPREV GCURR R RULE)
    (PROG (TODO COMPLETED NEWGCURR RK REDUCED NEWCOMPLETED REDO FLAG_ K J
           NNORMALFORMS)
      (SETQ K 0)
      (SETQ J 0)
      (SETQ NNORMALFORMS 0)
      (SETQ TODO S)
      (SETQ NEWGCURR (COPY GCURR))
      (PROG ()
       WHILELABEL
        (COND ((NOT TODO) (RETURN NIL)))
        (PROGN
         (SETQ K (PROG1 (CAR TODO) (SETQ TODO (CDR TODO))))
         (SETQ RK (GETV (CADR R) K))
         (PROG (G134)
           (SETQ G134 (CORE_NORMALFORM (LP_EVAL RK) GPREV R T))
           (SETQ FLAG_ (CAR G134))
           (SETQ REDUCED (CDR G134))
           (RETURN G134))
         (SETQ NNORMALFORMS (PLUS NNORMALFORMS 1))
         (LP_SETEVAL RK REDUCED)
         (PUTV (CADR R) K RK)
         (PROG (G135)
           (SETQ G135 (CORE_TOPREDUCTIONF5 K GPREV NEWGCURR R RULE))
           (SETQ NEWCOMPLETED (CAR G135))
           (SETQ REDO (CDR G135))
           (RETURN G135))
         (COND
          (NEWCOMPLETED
           (PROGN
            (PROGN (SETQ COMPLETED (CONS NEWCOMPLETED COMPLETED)) NEWCOMPLETED)
            (PROGN
             (SETQ NEWGCURR (CONS NEWCOMPLETED NEWGCURR))
             NEWCOMPLETED))))
         (PROG (J)
           (SETQ J REDO)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J) (SETQ TODO (CORE_INSERTSORTED TODO J R))) (CAR J))
           (SETQ J (CDR J))
           (GO LAB)))
        (GO WHILELABEL))
      (COND (*F5STATISTICS (STAT_UPDATEREDUCTIONS (LENGTH S) NNORMALFORMS)))
      (RETURN COMPLETED))) 
(PUT 'CORE_COMPUTESPOLYS 'NUMBER-OF-ARGS 3) 
(DE CORE_COMPUTESPOLYS (PAIRS R RULE)
    (PROG (S P U V LPK LPL ALS SEVAL SSGN L K)
      (SETQ L 0)
      (SETQ K 0)
      (SETQ PAIRS (SORT PAIRS 'CORE_PAIRLCMCMP))
      (PROG ()
       WHILELABEL
        (COND ((NOT PAIRS) (RETURN NIL)))
        (PROGN
         (SETQ P (PROG1 (CAR PAIRS) (SETQ PAIRS (CDR PAIRS))))
         (PROG (G136)
           (SETQ G136 (CONS (CADDR P) (CADDDR P)))
           (SETQ K (CAR G136))
           (SETQ U (CDR G136))
           (RETURN G136))
         (PROG (G137)
           (SETQ G137 (CONS (CAR (CDDDDR P)) (CADR (CDDDDR P))))
           (SETQ L (CAR G137))
           (SETQ V (CDR G137))
           (RETURN G137))
         (COND
          ((AND (NOT (NOT (IEQUAL (CORE_FINDREWRITING U K R RULE) K)))
                (NOT (NOT (IEQUAL (CORE_FINDREWRITING V L R RULE) L))))
           (PROGN
            (SETQ LPK (GETV (CADR R) K))
            (SETQ LPL (GETV (CADR R) L))
            (COND
             (*F5PARAMETRIC
              (PROGN
               (PARAM_ADDASSUMPTIONSPOL (POLY_LEADCOEFF (LP_EVAL LPK)))
               (PARAM_ADDASSUMPTIONSPOL (POLY_LEADCOEFF (LP_EVAL LPL))))))
            (SETQ SEVAL (POLY_SPOLY (LP_EVAL LPK) (LP_EVAL LPL)))
            (SETQ SSGN (LP_MULSGN (LP_SGN LPK) U))
            (COND
             ((NOT (POLY_ISZERO? SEVAL)) (SETQ SEVAL (POLY_NORMALIZE SEVAL))))
            (CORE_ADDPOLY R (LP_LABELEDPOLYNOMIAL2 SEVAL SSGN))
            (CORE_ADDRULE RULE SSGN (IDIFFERENCE (CADDR R) 1))
            (COND
             ((NOT (POLY_ISZERO? SEVAL))
              (PROG (W1)
                (SETQ W1 (IDIFFERENCE (CADDR R) 1))
                (SETQ S (CONS W1 S))
                (RETURN W1))))))))
        (GO WHILELABEL))
      (SETQ ALS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I S)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I) (CONS I (GETV (CADR R) I)))
                                  (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (CONS I (GETV (CADR R) I))) (CAR I))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALS (SORT ALS 'CORE_ASSOCSGNCMP))
      (SETQ S
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ALS)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN S))) 
(PUT 'CORE_MAKECRITICALPAIR 'NUMBER-OF-ARGS 6) 
(DE CORE_MAKECRITICALPAIR (I K L GPREV R RULE)
    (PROG (RK RL TK TL TT U1 U2 USGN1 USGN2 SUGAR)
      (SETQ SUGAR 0)
      (SETQ RK (GETV (CADR R) K))
      (SETQ RL (GETV (CADR R) L))
      (SETQ TK (POLY_LEADTERM (LP_EVAL RK)))
      (SETQ TL (POLY_LEADTERM (LP_EVAL RL)))
      (SETQ TT (POLY_LCMTERM TK TL))
      (SETQ U1 (POLY_SUBEXP TT TK))
      (SETQ U2 (POLY_SUBEXP TT TL))
      (SETQ USGN1 (LP_MULSGN (LP_SGN RK) U1))
      (SETQ USGN2 (LP_MULSGN (LP_SGN RL) U2))
      (COND
       ((AND (IEQUAL (LP_INDEXSGN USGN1) I)
             (CORE_ISTOPREDUCIBLETERM (LP_TERMSGN USGN1) GPREV R))
        (RETURN NIL)))
      (COND
       ((AND (IEQUAL (LP_INDEXSGN USGN2) I)
             (CORE_ISTOPREDUCIBLETERM (LP_TERMSGN USGN2) GPREV R))
        (RETURN NIL)))
      (COND
       ((OR (NOT (IEQUAL (CORE_FINDREWRITING U1 K R RULE) K))
            (NOT (IEQUAL (CORE_FINDREWRITING U2 L R RULE) L)))
        (RETURN NIL)))
      (COND
       ((LP_CMPSGN USGN1 USGN2)
        (PROGN
         (PROG (G138)
           (SETQ G138 (CONS U2 U1))
           (SETQ U1 (CAR G138))
           (SETQ U2 (CDR G138))
           (RETURN G138))
         (PROG (G139)
           (SETQ G139 (CONS L K))
           (SETQ K (CAR G139))
           (SETQ L (CDR G139))
           (RETURN G139)))))
      (SETQ SUGAR
              (MAX (PLUS (POLY_TOTALDEGTERM U1) (POLY_GETSUGAR (LP_EVAL RK)))
                   (PLUS (POLY_TOTALDEGTERM U2) (POLY_GETSUGAR (LP_EVAL RL)))))
      (RETURN (LIST 'CP TT K U1 L U2 SUGAR)))) 
(PUT 'CORE_SELECTPAIRSSUGAR 'NUMBER-OF-ARGS 1) 
(DE CORE_SELECTPAIRSSUGAR (PAIRS)
    (PROG (SELECTEDPAIRS P DEG)
      (SETQ DEG 0)
      (SETQ PAIRS (SORT PAIRS 'CORE_PAIRSUGARCMP))
      (SETQ P (PROG1 (CAR PAIRS) (SETQ PAIRS (CDR PAIRS))))
      (SETQ SELECTEDPAIRS (LIST P))
      (SETQ DEG (CADDR (CDDDDR P)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND PAIRS (IEQUAL (CADDR (CDDDDR (CAR PAIRS))) DEG)))
          (RETURN NIL)))
        (PROG (W1)
          (SETQ W1 (PROG1 (CAR PAIRS) (SETQ PAIRS (CDR PAIRS))))
          (SETQ SELECTEDPAIRS (CONS W1 SELECTEDPAIRS))
          (RETURN W1))
        (GO WHILELABEL))
      (RETURN (CONS SELECTEDPAIRS PAIRS)))) 
(PUT 'CORE_SELECTPAIRSNORMAL 'NUMBER-OF-ARGS 1) 
(DE CORE_SELECTPAIRSNORMAL (PAIRS)
    (PROG (SELECTEDPAIRS P DEG)
      (SETQ DEG 0)
      (SETQ PAIRS (SORT PAIRS 'CORE_PAIRTOTALDEGREECMP))
      (SETQ P (PROG1 (CAR PAIRS) (SETQ PAIRS (CDR PAIRS))))
      (SETQ SELECTEDPAIRS (LIST P))
      (SETQ DEG (POLY_TOTALDEGTERM (CADR P)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND PAIRS (IEQUAL (POLY_TOTALDEGTERM (CADR (CAR PAIRS))) DEG)))
          (RETURN NIL)))
        (PROG (W1)
          (SETQ W1 (PROG1 (CAR PAIRS) (SETQ PAIRS (CDR PAIRS))))
          (SETQ SELECTEDPAIRS (CONS W1 SELECTEDPAIRS))
          (RETURN W1))
        (GO WHILELABEL))
      (RETURN (CONS SELECTEDPAIRS PAIRS)))) 
(DE CORE_SELECTPAIRS (PAIRS)
    (COND (*F5SUGAR (CORE_SELECTPAIRSSUGAR PAIRS))
          (T (CORE_SELECTPAIRSNORMAL PAIRS)))) 
(PUT 'CORE_SELECTPAIRS 'NUMBER-OF-ARGS 1) 
(PUTC 'CORE_SELECTPAIRS 'INLINE
      '(LAMBDA (PAIRS)
         (COND (*F5SUGAR (CORE_SELECTPAIRSSUGAR PAIRS))
               (T (CORE_SELECTPAIRSNORMAL PAIRS))))) 
(PUT 'CORE_SETUPREDUCEDBASIS 'NUMBER-OF-ARGS 3) 
(DE CORE_SETUPREDUCEDBASIS (GPREV R RULE)
    (PROG (F B BTMP I)
      (SETQ I 0)
      (SETQ GPREV (CORE_INTERREDUCEBASIS GPREV R))
      (SETQ B
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F GPREV)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (GETV (CADR R) F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (GETV (CADR R) F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GPREV
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE (LENGTH B) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH B) I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS I NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BTMP B)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH B) I)) (RETURN NIL)))
        (PROGN
         (SETQ F (PROG1 (CAR BTMP) (SETQ BTMP (CDR BTMP))))
         (PUTV (CADR R) I (LP_LABELEDPOLYNOMIAL1 F I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN GPREV))) 
(PUT 'CORE_INCREMENTALBASIS 'NUMBER-OF-ARGS 4) 
(DE CORE_INCREMENTALBASIS (I GPREV R RULE)
    (PROG (GCURR PAIRS P S SELECTEDPAIRS REDUCED K TMP ALGPREV J CURRIDX
           NCURRENTPAIRS)
      (SETQ J 0)
      (SETQ CURRIDX 0)
      (SETQ NCURRENTPAIRS 0)
      (SETQ CURRIDX (IDIFFERENCE (CADDR R) 1))
      (SETQ GCURR (COPY GPREV))
      (PROGN (SETQ GCURR (CONS CURRIDX GCURR)) CURRIDX)
      (PROG (J)
        (SETQ J GPREV)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ P (CORE_MAKECRITICALPAIR I CURRIDX J GPREV R RULE))
            (COND (P (PROGN (SETQ PAIRS (CONS P PAIRS)) P)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ ALGPREV
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I GPREV)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I) (CONS I (GETV (CADR R) I)))
                                  (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (CONS I (GETV (CADR R) I))) (CAR I))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALGPREV
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ALGPREV)
               STARTOVER
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PR)
                           (COND ((NOT (POLY_ISZERO? (CDR PR))) (LIST PR))
                                 (T NIL)))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PR (CDR PR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PR)
                           (COND ((NOT (POLY_ISZERO? (CDR PR))) (LIST PR))
                                 (T NIL)))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PR (CDR PR))
                (GO LOOPLABEL)))
      (SETQ ALGPREV (SORT ALGPREV 'CORE_ASSOCLENGTHCMP))
      (SETQ GPREV
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ALGPREV)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*F5STATISTICS (STAT_UPDATEPAIRS (LENGTH PAIRS))))
      (PROG ()
       WHILELABEL
        (COND ((NOT PAIRS) (RETURN NIL)))
        (PROGN
         (PROG (G140)
           (SETQ G140
                   (COND (*F5SUGAR (CORE_SELECTPAIRSSUGAR PAIRS))
                         (T (CORE_SELECTPAIRSNORMAL PAIRS))))
           (SETQ SELECTEDPAIRS (CAR G140))
           (SETQ PAIRS (CDR G140))
           (RETURN G140))
         (SETQ S (CORE_COMPUTESPOLYS SELECTEDPAIRS R RULE))
         (SETQ REDUCED (CORE_REDUCTION S GPREV GCURR R RULE))
         (SETQ REDUCED (REVERSIP REDUCED))
         (SETQ NCURRENTPAIRS 0)
         (COND (*F5STATISTICS (PROGN (SETQ NCURRENTPAIRS (LENGTH PAIRS)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT REDUCED) (RETURN NIL)))
           (PROGN
            (SETQ K (PROG1 (CAR REDUCED) (SETQ REDUCED (CDR REDUCED))))
            (SETQ TMP GCURR)
            (PROG ()
             WHILELABEL
              (COND ((NOT TMP) (RETURN NIL)))
              (PROGN
               (SETQ J (PROG1 (CAR TMP) (SETQ TMP (CDR TMP))))
               (SETQ P (CORE_MAKECRITICALPAIR I J K GPREV R RULE))
               (COND (P (PROGN (SETQ PAIRS (CONS P PAIRS)) P))))
              (GO WHILELABEL))
            (PROGN (SETQ GCURR (CONS K GCURR)) K))
           (GO WHILELABEL))
         (COND
          (*F5STATISTICS
           (PROGN
            (STAT_UPDATEINCREMENTAL
             (POLY_TOTALDEGTERM (CADR (CAR SELECTEDPAIRS)))
             (DIFFERENCE (LENGTH PAIRS) NCURRENTPAIRS))))))
        (GO WHILELABEL))
      (RETURN GCURR))) 
(PUT 'CORE_GROEBNER1 'NUMBER-OF-ARGS 1) 
(DE CORE_GROEBNER1 (BASIS)
    (PROG (F1 R GPREV RULE FI M I)
      (SETQ M 0)
      (SETQ I 0)
      (SETQ M (LENGTH BASIS))
      (SETQ F1 (PROG1 (CAR BASIS) (SETQ BASIS (CDR BASIS))))
      (SETQ F1 (LP_NORMALIZE F1))
      (SETQ R (CORE_BASISTRACKER CORE_INITIALBASISSIZE*))
      (CORE_ADDPOLY R F1)
      (SETQ GPREV (LIST 0))
      (SETQ RULE (MKVECT M))
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (ILESSP I M)) (RETURN NIL)))
        (PROGN
         (SETQ FI (PROG1 (CAR BASIS) (SETQ BASIS (CDR BASIS))))
         (SETQ FI (LP_NORMALIZE FI))
         (CORE_ADDPOLY R FI)
         (PUTV RULE I NIL)
         (COND (*F5STATISTICS (STAT_UPDATEMODULEINDEX)))
         (SETQ GPREV (CORE_INCREMENTALBASIS I GPREV R RULE))
         (COND (*F5USEF5C (SETQ GPREV (CORE_SETUPREDUCEDBASIS GPREV R RULE))))
         (SETQ I (IPLUS2 I 1)))
        (GO WHILELABEL))
      (SETQ GPREV (CORE_FILTERREDUNDANT GPREV R))
      (COND (*F5INTERREDUCE (SETQ GPREV (CORE_INTERREDUCEBASIS GPREV R))))
      (SETQ BASIS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I GPREV)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (I) (GETV (CADR R) I)) (CAR I))
                                      NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (GETV (CADR R) I)) (CAR I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*F5STATISTICS (STAT_PRINT)))
      (RETURN (CORE_STANDARDIZEOUTPUT BASIS)))) 
(ENDMODULE) 
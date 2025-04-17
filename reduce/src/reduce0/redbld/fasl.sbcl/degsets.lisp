(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEGSETS)) 
(FLUID
 '(*TRALLFAC *TRFAC BAD-CASE BEST-SET-POINTER DPOLY FACTOR-LEVEL
   FACTOR-TRACE-LIST FACTORED-LC IRREDUCIBLE MODULAR-INFO
   ONE-COMPLETE-DEG-ANALYSIS-DONE PREVIOUS-DEGREE-MAP SPLIT-LIST
   VALID-IMAGE-SETS)) 
(PUT 'CHECK-DEGREE-SETS 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK-DEGREE-SETS 'DEFINED-ON-LINE '45) 
(PUT 'CHECK-DEGREE-SETS 'DEFINED-IN-FILE 'FACTOR/DEGSETS.RED) 
(PUT 'CHECK-DEGREE-SETS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK-DEGREE-SETS (N MULTIVARIATE-CASE)
    (PROG (DEGREE-SETS W X-IS-FACTOR DEGS)
      (SETQ W SPLIT-LIST)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (COND
          (MULTIVARIATE-CASE
           (SETQ X-IS-FACTOR
                   (NOT
                    (NUMBERP
                     (CADDR (CDDR (GETV VALID-IMAGE-SETS (CDAR W)))))))))
         (SETQ DEGS
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (GETV MODULAR-INFO (CDAR W)))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (CDAAR V)) (CAR V))
                                         NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (CDAAR V)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ DEGREE-SETS
                 (CONS (COND (X-IS-FACTOR (CONS 1 DEGS)) (T DEGS))
                       DEGREE-SETS))
         (SETQ W (CDR W)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (CHECK-DEGREE-SETS-1 DEGREE-SETS)
      (SETQ BEST-SET-POINTER (CDAR SPLIT-LIST))
      (COND
       ((AND MULTIVARIATE-CASE FACTORED-LC)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND
               (NULL
                (SETQ W
                        (CADR
                         (CDDR
                          (CDDDR (GETV VALID-IMAGE-SETS BEST-SET-POINTER))))))
               (SETQ SPLIT-LIST (CDR SPLIT-LIST))))
             (RETURN NIL)))
           (SETQ BEST-SET-POINTER (CDAR SPLIT-LIST))
           (GO WHILELABEL))
         (COND ((NULL W) (SETQ BAD-CASE T)))))))) 
(PUT 'CHECK-DEGREE-SETS-1 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-DEGREE-SETS-1 'DEFINED-ON-LINE '70) 
(PUT 'CHECK-DEGREE-SETS-1 'DEFINED-IN-FILE 'FACTOR/DEGSETS.RED) 
(PUT 'CHECK-DEGREE-SETS-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-DEGREE-SETS-1 (L)
    (PROG (I DEGREE-MAP DEGREE-MAP1 DPOLY PLAUSIBLE-SPLIT-FOUND TARGET-COUNT)
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROGN
            (PRIN2T "Degree sets are:")
            (PROG (S)
              (SETQ S L)
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S)
                 (PROGN
                  (PRIN2 "     ")
                  (PROG (N)
                    (SETQ N S)
                   LAB
                    (COND ((NULL N) (RETURN NIL)))
                    ((LAMBDA (N) (PROGN (PRIN2 " ") (PRIN2 N))) (CAR N))
                    (SETQ N (CDR N))
                    (GO LAB))
                  (TERPRI)))
               (CAR S))
              (SETQ S (CDR S))
              (GO LAB)))
           (WRS STREAM)))))
      (SETQ DPOLY (SUM-LIST (CAR L)))
      (SETQ TARGET-COUNT (LENGTH (CAR L)))
      (PROG (S)
        (SETQ S (CDR L))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S) (SETQ TARGET-COUNT (MIN TARGET-COUNT (LENGTH S))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (COND
       ((NULL PREVIOUS-DEGREE-MAP)
        (PROGN
         (SETQ DEGREE-MAP (MKVECT DPOLY))
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((MINUSP (DIFFERENCE DPOLY I)) (RETURN NIL)))
           (PUTV DEGREE-MAP I T)
           (SETQ I (PLUS2 I 1))
           (GO LAB))))
       (T
        (PROGN
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              "Refine an existing degree map"
              (WRS STREAM)))))
         (SETQ DEGREE-MAP PREVIOUS-DEGREE-MAP))))
      (SETQ DEGREE-MAP1 (MKVECT DPOLY))
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND ((MINUSP (DIFFERENCE DPOLY I)) (RETURN NIL)))
              (PUTV DEGREE-MAP1 I NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (PUTV DEGREE-MAP1 0 T)
            (PUTV DEGREE-MAP1 DPOLY T)
            (PROG (D)
              (SETQ D S)
             LAB
              (COND ((NULL D) (RETURN NIL)))
              ((LAMBDA (D)
                 (PROG (I)
                   (SETQ I (IDIFFERENCE (IDIFFERENCE DPOLY D) 1))
                  LAB
                   (COND
                    ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
                   (COND
                    ((GETV DEGREE-MAP1 I) (PUTV DEGREE-MAP1 (IPLUS2 I D) T)))
                   (SETQ I (PLUS2 I (MINUS 1)))
                   (GO LAB)))
               (CAR D))
              (SETQ D (CDR D))
              (GO LAB))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND ((MINUSP (DIFFERENCE DPOLY I)) (RETURN NIL)))
              (PUTV DEGREE-MAP I
                    (AND (GETV DEGREE-MAP I) (GETV DEGREE-MAP1 I)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROGN
            (PRIN2T "Possible degrees for factors are: ")
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND
               ((MINUSP (DIFFERENCE (IDIFFERENCE DPOLY 1) I)) (RETURN NIL)))
              (COND ((GETV DEGREE-MAP I) (PROGN (PRIN2 I) (PRIN2 " "))))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (TERPRI))
           (WRS STREAM)))))
      (SETQ I (IDIFFERENCE DPOLY 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (IGREATERP I 0)) (RETURN NIL)))
        (COND ((GETV DEGREE-MAP I) (SETQ I (MINUS 1)))
              (T (SETQ I (IDIFFERENCE I 1))))
        (GO WHILELABEL))
      (COND
       ((EQUAL I 0)
        (PROGN
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              (PRIN2T "Degree analysis proves polynomial irreducible")
              (WRS STREAM)))))
         (RETURN (SETQ IRREDUCIBLE T)))))
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((EQUAL (LENGTH S) TARGET-COUNT)
             (PROG ()
               (SETQ I S)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND I (GETV DEGREE-MAP (CAR I)))) (RETURN NIL)))
                 (SETQ I (CDR I))
                 (GO WHILELABEL))
               (COND ((NULL I) (SETQ PLAUSIBLE-SPLIT-FOUND T)))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ PREVIOUS-DEGREE-MAP DEGREE-MAP)
      (COND
       ((OR PLAUSIBLE-SPLIT-FOUND ONE-COMPLETE-DEG-ANALYSIS-DONE)
        (RETURN NIL)))
      (RETURN (SETQ BAD-CASE T)))) 
(PUT 'SUM-LIST 'NUMBER-OF-ARGS 1) 
(PUT 'SUM-LIST 'DEFINED-ON-LINE '138) 
(PUT 'SUM-LIST 'DEFINED-IN-FILE 'FACTOR/DEGSETS.RED) 
(PUT 'SUM-LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUM-LIST (L)
    (COND ((NULL (CDR L)) (CAR L)) (T (IPLUS2 (CAR L) (SUM-LIST (CDR L)))))) 
(ENDMODULE) 
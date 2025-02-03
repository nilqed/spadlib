(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RPRINTF)) 
(FLUID '(BLDMSG_CHARS* *LL*)) 
(SETQ BLDMSG_CHARS* NIL) 
(PUT 'P_PRINC 'NUMBER-OF-ARGS 2) 
(PUT 'P_PRINC 'DEFINED-ON-LINE '40) 
(PUT 'P_PRINC 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_PRINC (U BLANKFIRST)
    (PROG (W)
      (SETQ W (EXPLODE2 U))
      (COND
       (BLDMSG_CHARS*
        (PROGN
         (COND (BLANKFIRST (SETQ BLDMSG_CHARS* (CONS BLANK BLDMSG_CHARS*))))
         (PROG (C)
           (SETQ C W)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (SETQ BLDMSG_CHARS* (CONS C BLDMSG_CHARS*))) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))))
       (T
        (PROGN
         (COND
          (BLANKFIRST
           (PROGN
            (COND ((GREATERP (PLUS (POSN) (LENGTH W) 1) *LL*) (TERPRI)))
            (PRIN2 " ")))
          ((GREATERP (PLUS (POSN) (LENGTH W)) *LL*) (TERPRI)))
         (PRIN2 U)))))) 
(PUT 'P_PRIN 'NUMBER-OF-ARGS 2) 
(PUT 'P_PRIN 'DEFINED-ON-LINE '59) 
(PUT 'P_PRIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_PRIN (U BLANKFIRST)
    (PROG (W)
      (SETQ W (EXPLODE U))
      (COND ((EQCAR W '_) (SETQ W (CONS '! W))))
      (COND
       (BLDMSG_CHARS*
        (PROGN
         (COND (BLANKFIRST (SETQ BLDMSG_CHARS* (CONS BLANK BLDMSG_CHARS*))))
         (PROG (C)
           (SETQ C W)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (SETQ BLDMSG_CHARS* (CONS C BLDMSG_CHARS*))) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))))
       (T
        (PROGN
         (COND
          (BLANKFIRST
           (PROGN
            (COND ((GREATERP (PLUS (POSN) (LENGTH W) 1) *LL*) (TERPRI)))
            (PRIN2 " ")))
          ((GREATERP (PLUS (POSN) (LENGTH W)) *LL*) (TERPRI)))
         (PROG (C)
           (SETQ C W)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (PRIN2 C)) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))))))) 
(FLUID '(*PRINT-ARRAY* *PRINT-LENGTH* *PRINT-LEVEL*)) 
(SETQ *PRINT-ARRAY* T) 
(SETQ *PRINT-LENGTH* NIL) 
(SETQ *PRINT-LEVEL* NIL) 
(FLUID '(*PRINL_VISITED_NODES* *PRINL_INDEX*)) 
(SETQ *PRINL_VISITED_NODES* (MKHASH 200 0 1.5)) 
(PUT 'P_PRINL0 'NUMBER-OF-ARGS 2) 
(PUT 'P_PRINL0 'DEFINED-ON-LINE '95) 
(PUT 'P_PRINL0 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINL0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_PRINL0 (X ESCAPED)
    (PROG (*PRINL_INDEX*)
      (SETQ *PRINL_INDEX* 0)
      (CLRHASH *PRINL_VISITED_NODES*)
      (P_PRINL1 X 0)
      (P_PRINL2 X 0 ESCAPED NIL)
      (CLRHASH *PRINL_VISITED_NODES*)
      (RETURN X))) 
(PUT 'P_PRINL1 'NUMBER-OF-ARGS 2) 
(PUT 'P_PRINL1 'DEFINED-ON-LINE '118) 
(PUT 'P_PRINL1 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_PRINL1 (X DEPTH)
    (PROG (W LENGTH)
      (COND
       ((AND (FIXP *PRINT-LEVEL*) (GREATERP DEPTH *PRINT-LEVEL*))
        (RETURN NIL)))
      (SETQ LENGTH 0)
     TOP
      (COND ((AND (ATOM X) (NOT (VECTORP X)) (NOT (GENSYMP X))) (RETURN NIL))
            ((SETQ W (GETHASH X *PRINL_VISITED_NODES*))
             (PROGN
              (COND
               ((EQUAL W 0)
                (PROGN
                 (SETQ *PRINL_INDEX* (PLUS *PRINL_INDEX* 1))
                 (PUTHASH X *PRINL_VISITED_NODES* *PRINL_INDEX*))))
              (RETURN NIL)))
            (T
             (PROGN
              (PUTHASH X *PRINL_VISITED_NODES* 0)
              (COND
               ((VECTORP X)
                (PROGN
                 (COND
                  (*PRINT-ARRAY*
                   (PROGN
                    (SETQ LENGTH (UPBV X))
                    (COND
                     ((AND (FIXP *PRINT-LENGTH*) (LESSP *PRINT-LENGTH* LENGTH))
                      (SETQ LENGTH *PRINT-LENGTH*)))
                    (PROG (I)
                      (SETQ I 0)
                     LAB
                      (COND ((MINUSP (DIFFERENCE LENGTH I)) (RETURN NIL)))
                      (P_PRINL1 (GETV X I) (PLUS DEPTH 1))
                      (SETQ I (PLUS2 I 1))
                      (GO LAB)))))))
               ((NOT (ATOM X))
                (PROGN
                 (P_PRINL1 (CAR X) (PLUS DEPTH 1))
                 (COND
                  ((AND (FIXP *PRINT-LENGTH*)
                        (GREATERP (SETQ LENGTH (PLUS LENGTH 1))
                                  *PRINT-LENGTH*))
                   (RETURN NIL)))
                 (SETQ X (CDR X))
                 (GO TOP))))))))) 
(PUT 'P_PRINTREF 'NUMBER-OF-ARGS 3) 
(PUT 'P_PRINTREF 'DEFINED-ON-LINE '150) 
(PUT 'P_PRINTREF 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINTREF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE P_PRINTREF (W BLANKFIRST CH)
    (PROG (LEN)
      (SETQ LEN (LENGTH (EXPLODE W)))
      (COND (BLANKFIRST (SETQ LEN (PLUS LEN 1))))
      (COND
       ((AND (NOT BLDMSG_CHARS*) (GREATERP (PLUS (POSN) 2 LEN) *LL*))
        (PROGN (SETQ BLANKFIRST NIL) (TERPRI))))
      (P_PRINC "#" BLANKFIRST)
      (P_PRINC W NIL)
      (P_PRINC CH NIL))) 
(PUT 'P_PRINL2 'NUMBER-OF-ARGS 4) 
(PUT 'P_PRINL2 'DEFINED-ON-LINE '168) 
(PUT 'P_PRINL2 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINL2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE P_PRINL2 (X DEPTH ESCAPED BLANKFIRST)
    (COND
     ((AND (FIXP *PRINT-LEVEL*) (GREATERP DEPTH *PRINT-LEVEL*))
      (P_PRINC "#" BLANKFIRST))
     ((AND (ATOM X) (NOT (VECTORP X)) (NOT (GENSYMP X)))
      (PROGN
       (COND (ESCAPED (P_PRIN X BLANKFIRST)) (T (P_PRINC X BLANKFIRST)))))
     (T
      (PROG (W LENGTH)
        (SETQ W (GETHASH X *PRINL_VISITED_NODES*))
        (COND
         ((AND W (NOT (ZEROP W)))
          (PROGN
           (COND
            ((LESSP W 0)
             (PROGN (P_PRINTREF (MINUS W) BLANKFIRST "#") (RETURN NIL)))
            (T
             (PROGN
              (PUTHASH X *PRINL_VISITED_NODES* (MINUS W))
              (P_PRINTREF W BLANKFIRST "=")
              (SETQ BLANKFIRST NIL)))))))
        (COND
         ((VECTORP X)
          (PROGN
           (P_PRINC "%(" BLANKFIRST)
           (COND
            (*PRINT-ARRAY*
             (PROGN
              (SETQ LENGTH (UPBV X))
              (COND
               ((AND (FIXP *PRINT-LENGTH*) (LESSP *PRINT-LENGTH* LENGTH))
                (SETQ LENGTH *PRINT-LENGTH*)))
              (PROG (I)
                (SETQ I 0)
               LAB
                (COND ((MINUSP (DIFFERENCE LENGTH I)) (RETURN NIL)))
                (P_PRINL2 (GETV X I) (PLUS DEPTH 1) ESCAPED (NEQ I 0))
                (SETQ I (PLUS2 I 1))
                (GO LAB))))
            (T (P_PRINC "..." NIL)))
           (P_PRINC ")" NIL)
           (RETURN NIL)))
         ((ATOM X)
          (PROGN
           (COND (ESCAPED (P_PRIN X BLANKFIRST)) (T (P_PRINC X BLANKFIRST)))
           (RETURN NIL))))
        (P_PRINC "(" BLANKFIRST)
        (P_PRINL2 (CAR X) (PLUS DEPTH 1) ESCAPED NIL)
        (SETQ X (CDR X))
        (SETQ LENGTH 0)
       LOOP
        (COND
         ((ATOM X)
          (PROGN
           (COND
            ((NEQ X NIL) (PROGN (P_PRINC "." T) (P_PRINL2 X DEPTH ESCAPED T))))
           (RETURN (P_PRINC ")" NIL)))))
        (COND
         ((AND (FIXP *PRINT-LENGTH*)
               (GREATERP (SETQ LENGTH (PLUS LENGTH 1)) *PRINT-LENGTH*))
          (PROGN (P_PRINC "..." T) (RETURN (P_PRINC ")" NIL)))))
        (SETQ W (GETHASH X *PRINL_VISITED_NODES*))
        (COND
         ((AND W (NOT (ZEROP W)))
          (PROGN
           (COND
            ((LESSP W 0)
             (PROGN
              (P_PRINC "." T)
              (P_PRINTREF (MINUS W) T "#")
              (RETURN (P_PRINC ")" NIL))))
            (T
             (PROGN
              (P_PRINC "." T)
              (P_PRINL2 X (PLUS DEPTH 1) ESCAPED T)
              (RETURN (P_PRINC ")" NIL))))))))
        (P_PRINL2 (CAR X) (PLUS DEPTH 1) ESCAPED T)
        (SETQ X (CDR X))
        (GO LOOP))))) 
(PUT 'RLISP_PRINTL 'NUMBER-OF-ARGS 1) 
(PUT 'RLISP_PRINTL 'DEFINED-ON-LINE '238) 
(PUT 'RLISP_PRINTL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'RLISP_PRINTL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLISP_PRINTL (X) (PROGN (RPLISP_PRINL X) (TERPRI) X)) 
(PUT 'RLISP_PRINTCL 'NUMBER-OF-ARGS 1) 
(PUT 'RLISP_PRINTCL 'DEFINED-ON-LINE '243) 
(PUT 'RLISP_PRINTCL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'RLISP_PRINTCL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLISP_PRINTCL (X) (PROGN (RLISP_PRINCL X) (TERPRI) X)) 
(PUT 'RLISP_PRINCL 'NUMBER-OF-ARGS 1) 
(PUT 'RLISP_PRINCL 'DEFINED-ON-LINE '248) 
(PUT 'RLISP_PRINCL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'RLISP_PRINCL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLISP_PRINCL (X)
    (PROG (*LL*)
      (SETQ *LL* (DIFFERENCE (LINELENGTH NIL) 2))
      (P_PRINL0 X NIL)
      (RETURN X))) 
(PUT 'RLISP_PRINL 'NUMBER-OF-ARGS 1) 
(PUT 'RLISP_PRINL 'DEFINED-ON-LINE '256) 
(PUT 'RLISP_PRINL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'RLISP_PRINL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLISP_PRINL (X)
    (PROG (*LL*)
      (SETQ *LL* (DIFFERENCE (LINELENGTH NIL) 2))
      (P_PRINL0 X T)
      (RETURN X))) 
(PUT 'PORTABLE_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'PORTABLE_PRINT 'DEFINED-ON-LINE '264) 
(PUT 'PORTABLE_PRINT 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PORTABLE_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PORTABLE_PRINT (X) (PROGN (PORTABLE_PRIN X) (TERPRI) X)) 
(PUT 'PORTABLE_PRINTC 'NUMBER-OF-ARGS 1) 
(PUT 'PORTABLE_PRINTC 'DEFINED-ON-LINE '269) 
(PUT 'PORTABLE_PRINTC 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PORTABLE_PRINTC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PORTABLE_PRINTC (X) (PROGN (PORTABLE_PRINC X) (TERPRI) X)) 
(PUT 'PORTABLE_PRINC 'NUMBER-OF-ARGS 1) 
(PUT 'PORTABLE_PRINC 'DEFINED-ON-LINE '274) 
(PUT 'PORTABLE_PRINC 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PORTABLE_PRINC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PORTABLE_PRINC (X)
    (PROG (*LL*)
      (SETQ *LL* (DIFFERENCE (LINELENGTH NIL) 2))
      (P_PRINL2 X 0 NIL NIL)
      (RETURN X))) 
(PUT 'PORTABLE_PRIN 'NUMBER-OF-ARGS 1) 
(PUT 'PORTABLE_PRIN 'DEFINED-ON-LINE '282) 
(PUT 'PORTABLE_PRIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PORTABLE_PRIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PORTABLE_PRIN (X)
    (PROG (*LL*)
      (SETQ *LL* (DIFFERENCE (LINELENGTH NIL) 2))
      (P_PRINL2 X 0 T NIL)
      (RETURN X))) 
(PUT 'P_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'P_MINUS 'DEFINED-ON-LINE '300) 
(PUT 'P_MINUS 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P_MINUS (U)
    (COND ((EQCAR U 'MINUS) (CADR U))
          ((EQCAR U 'PLUS)
           (CONS 'PLUS
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (CDR U))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (P_MINUS V)) (CAR V))
                                         NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (P_MINUS V)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQCAR U 'DIFFERENCE)
           (CONS 'PLUS (CONS (P_MINUS (CADR U)) (CDDR U))))
          (T (LIST 'MINUS U)))) 
(PUT 'P_DIFF2MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'P_DIFF2MINUS 'DEFINED-ON-LINE '308) 
(PUT 'P_DIFF2MINUS 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_DIFF2MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P_DIFF2MINUS (U)
    (PROG (R)
      (SETQ R (CAR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ U (CDR U))) (RETURN NIL)))
        (SETQ R (CONS (P_MINUS (CAR U)) R))
        (GO WHILELABEL))
      (RETURN (CONS 'PLUS (REVERSE R))))) 
(PUT 'P_PREFIX 'NUMBER-OF-ARGS 2) 
(PUT 'P_PREFIX 'DEFINED-ON-LINE '316) 
(PUT 'P_PREFIX 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PREFIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_PREFIX (U PREC)
    (COND ((ATOM U) (P_PRINC U NIL))
          ((EQCAR U '*SQ) (P_PREFIX (PREPSQ (CADR U)) PREC))
          (T
           (PROG (OP P1)
             (SETQ OP (CAR U))
             (COND
              ((EQUAL OP 'EXPT)
               (PROGN
                (COND ((GREATERP PREC 3) (P_PRINC "(" NIL)))
                (P_PREFIX (CADR U) 4)
                (P_PRINC "^" NIL)
                (P_PREFIX (CADDR U) 3)
                (COND ((GREATERP PREC 3) (P_PRINC ")" NIL)))
                (RETURN NIL)))
              ((OR (EQUAL OP 'TIMES) (EQUAL OP 'QUOTIENT))
               (PROGN
                (COND ((GREATERP PREC 2) (P_PRINC "(" NIL)))
                (P_PREFIX (CAR (SETQ U (CDR U))) 2)
                (COND ((EQUAL OP 'TIMES) (PROGN (SETQ P1 2) (SETQ OP "*")))
                      (T (PROGN (SETQ P1 3) (SETQ OP "/"))))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (NOT (ATOM (SETQ U (CDR U))))) (RETURN NIL)))
                  (PROGN (P_PRINC OP NIL) (P_PREFIX (CAR U) P1))
                  (GO WHILELABEL))
                (COND ((GREATERP PREC 2) (P_PRINC ")" NIL)))
                (RETURN NIL))))
             (COND
              ((EQUAL OP 'DIFFERENCE)
               (RETURN (P_PREFIX (P_DIFF2MINUS (CDR U)) PREC)))
              ((EQUAL OP 'PLUS)
               (PROGN
                (COND ((GREATERP PREC 1) (P_PRINC "(" NIL)))
                (P_PREFIX (CAR (SETQ U (CDR U))) 1)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (NOT (ATOM (SETQ U (CDR U))))) (RETURN NIL)))
                  (PROGN
                   (SETQ P1 (CAR U))
                   (COND
                    ((EQCAR P1 'MINUS)
                     (PROGN (SETQ P1 (CADR P1)) (P_PRINC " - " NIL)))
                    (T (P_PRINC " + " NIL)))
                   (P_PREFIX P1 1))
                  (GO WHILELABEL))
                (COND ((GREATERP PREC 1) (P_PRINC ")" NIL)))
                (RETURN NIL)))
              ((EQUAL OP 'MINUS)
               (PROGN
                (P_PRINC "-" NIL)
                (COND ((LESSP PREC 2) (SETQ PREC 2)))
                (RETURN (P_PREFIX (CADR U) PREC)))))
             (COND
              ((OR (NOT (ATOM OP)) (AND (NUMBERP OP) (MINUSP OP)))
               (PROGN (P_PRINC "(" NIL) (P_PREFIX OP 0) (P_PRINC ")" NIL)))
              (T (P_PRINC OP NIL)))
             (SETQ OP "(")
             (COND ((ATOM (SETQ U (CDR U))) (P_PRINC "(" NIL))
                   (T
                    (PROGN
                     (PROG ()
                      WHILELABEL
                       (COND ((NOT (NOT (ATOM U))) (RETURN NIL)))
                       (PROGN
                        (P_PRINC OP NIL)
                        (SETQ OP ",")
                        (P_PREFIX (CAR U) 0)
                        (SETQ U (CDR U)))
                       (GO WHILELABEL))
                     (COND
                      ((NOT (NULL U))
                       (PROGN (P_PRINC " . " NIL) (P_PRINC U NIL)))))))
             (RETURN (P_PRINC ")" NIL)))))) 
(PUT 'PRIN_WITH_MARGIN 'NUMBER-OF-ARGS 1) 
(PUT 'PRIN_WITH_MARGIN 'DEFINED-ON-LINE '397) 
(PUT 'PRIN_WITH_MARGIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRIN_WITH_MARGIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIN_WITH_MARGIN (U)
    (PRINT_WITH_MARGIN_SUB U (POSN) (DIFFERENCE (LINELENGTH NIL) 2)
                           (FUNCTION EXPLODE))) 
(PUT 'PRINC_WITH_MARGIN 'NUMBER-OF-ARGS 1) 
(PUT 'PRINC_WITH_MARGIN 'DEFINED-ON-LINE '400) 
(PUT 'PRINC_WITH_MARGIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRINC_WITH_MARGIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINC_WITH_MARGIN (U)
    (PRINT_WITH_MARGIN_SUB U (POSN) (DIFFERENCE (LINELENGTH NIL) 2)
                           (FUNCTION EXPLODE2))) 
(PUT 'PRINT_WITH_MARGIN 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_WITH_MARGIN 'DEFINED-ON-LINE '403) 
(PUT 'PRINT_WITH_MARGIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRINT_WITH_MARGIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_WITH_MARGIN (U) (PROGN (PRIN_WITH_MARGIN U) (TERPRI) U)) 
(PUT 'PRINTC_WITH_MARGIN 'NUMBER-OF-ARGS 1) 
(PUT 'PRINTC_WITH_MARGIN 'DEFINED-ON-LINE '408) 
(PUT 'PRINTC_WITH_MARGIN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRINTC_WITH_MARGIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINTC_WITH_MARGIN (U) (PROGN (PRINC_WITH_MARGIN U) (TERPRI) U)) 
(PUT 'PRINT_WITH_MARGIN_SUB 'NUMBER-OF-ARGS 4) 
(PUT 'PRINT_WITH_MARGIN_SUB 'DEFINED-ON-LINE '416) 
(PUT 'PRINT_WITH_MARGIN_SUB 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRINT_WITH_MARGIN_SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINT_WITH_MARGIN_SUB (U LEFT RIGHT EXPLFN)
    (PROG (V)
      (COND ((LESSP RIGHT 10) (SETQ RIGHT 10)))
      (COND
       ((GREATERP LEFT (DIFFERENCE RIGHT 10))
        (SETQ LEFT (DIFFERENCE RIGHT 10))))
      (SETQ V U)
      (COND
       ((NOT (ATOM V))
        (PROGN
         (COND ((GEQ (POSN) RIGHT) (PROGN (TERPRI) (TTAB LEFT))))
         (PRIN2 "(")
         (PRINT_WITH_MARGIN_SUB (CAR V) LEFT RIGHT EXPLFN)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (ATOM (SETQ V (CDR V))))) (RETURN NIL)))
           (PROGN
            (COND ((GEQ (POSN) RIGHT) (PROGN (TERPRI) (TTAB LEFT)))
                  (T (PRIN2 " ")))
            (PRINT_WITH_MARGIN_SUB (CAR V) LEFT RIGHT EXPLFN))
           (GO WHILELABEL))
         (COND
          ((NOT (NULL V))
           (PROGN
            (COND
             ((GEQ (POSN) (DIFFERENCE RIGHT 1))
              (PROGN (TERPRI) (TTAB LEFT) (PRIN2 ". ")))
             (T (PRIN2 " .")))
            (PRINT_WITH_MARGIN_SUB V LEFT RIGHT EXPLFN))))
         (COND ((GEQ (POSN) RIGHT) (PROGN (TERPRI) (TTAB LEFT))))
         (PRIN2 ")")
         (RETURN U))))
      (SETQ V (APPLY EXPLFN (LIST U)))
     VERYLONG
      (COND
       ((LESSP (PLUS (POSN) (LENGTH V)) RIGHT)
        (PROGN
         (PROG (C)
           (SETQ C V)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (PRIN2 C)) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))
         (RETURN U)))
       ((LEQ (LENGTH V) (DIFFERENCE RIGHT LEFT))
        (PROGN
         (TERPRI)
         (TTAB LEFT)
         (PROG (C)
           (SETQ C V)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (PRIN2 C)) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))
         (RETURN U)))
       ((LESSP (LENGTH V) RIGHT)
        (PROGN
         (TERPRI)
         (TTAB (DIFFERENCE RIGHT (LENGTH V)))
         (PROG (C)
           (SETQ C V)
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (PRIN2 C)) (CAR C))
           (SETQ C (CDR C))
           (GO LAB))
         (RETURN U)))
       (T
        (PROGN
         (COND
          ((GEQ (POSN) (DIFFERENCE RIGHT 5)) (PROGN (TERPRI) (TTAB LEFT))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (LESSP (POSN) (DIFFERENCE RIGHT 1))) (RETURN NIL)))
           (PROGN (PRIN2 (CAR V)) (SETQ V (CDR V)))
           (GO WHILELABEL))
         (PRIN2 "\\")
         (TERPRI)
         (GO VERYLONG)))))) 
(PUT 'BLDMSG_INTERNAL 'NUMBER-OF-ARGS 2) 
(PUT 'BLDMSG_INTERNAL 'DEFINED-ON-LINE '497) 
(PUT 'BLDMSG_INTERNAL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'BLDMSG_INTERNAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BLDMSG_INTERNAL (FMT ARGS)
    (PROG (BLDMSG_CHARS* U V)
      (SETQ BLDMSG_CHARS* T)
      (PRINTF_INTERNAL FMT ARGS)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ATOM BLDMSG_CHARS*))) (RETURN NIL)))
        (PROGN
         (SETQ U (CDR BLDMSG_CHARS*))
         (RPLACD BLDMSG_CHARS* V)
         (SETQ V BLDMSG_CHARS*)
         (SETQ BLDMSG_CHARS* U))
        (GO WHILELABEL))
      (RETURN (LIST2WIDESTRING V)))) 
(PUT 'P_POSN 'NUMBER-OF-ARGS 0) 
(PUT 'P_POSN 'DEFINED-ON-LINE '510) 
(PUT 'P_POSN 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_POSN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE P_POSN NIL
    (COND
     (BLDMSG_CHARS*
      (PROG (W N)
        (SETQ N 0)
        (SETQ W BLDMSG_CHARS*)
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (AND (NOT (ATOM W)) (NOT (EQUAL (CAR W) $EOL$))))
            (RETURN NIL)))
          (PROGN (SETQ N (PLUS N 1)) (SETQ W (CDR W)))
          (GO WHILELABEL))
        (RETURN N)))
     (T (POSN)))) 
(GLOBAL '(P_HEXDIGITS*)) 
(SETQ P_HEXDIGITS* (MKVECT 15)) 
(PUTV P_HEXDIGITS* 0 '|0|) 
(PUTV P_HEXDIGITS* 1 '|1|) 
(PUTV P_HEXDIGITS* 2 '|2|) 
(PUTV P_HEXDIGITS* 3 '|3|) 
(PUTV P_HEXDIGITS* 4 '|4|) 
(PUTV P_HEXDIGITS* 5 '|5|) 
(PUTV P_HEXDIGITS* 6 '|6|) 
(PUTV P_HEXDIGITS* 7 '|7|) 
(PUTV P_HEXDIGITS* 8 '|8|) 
(PUTV P_HEXDIGITS* 9 '|9|) 
(PUTV P_HEXDIGITS* 10 'A) 
(PUTV P_HEXDIGITS* 11 'B) 
(PUTV P_HEXDIGITS* 12 'C) 
(PUTV P_HEXDIGITS* 13 'D) 
(PUTV P_HEXDIGITS* 14 'E) 
(PUTV P_HEXDIGITS* 15 'F) 
(PUT 'P_PRINHEX 'NUMBER-OF-ARGS 1) 
(PUT 'P_PRINHEX 'DEFINED-ON-LINE '545) 
(PUT 'P_PRINHEX 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINHEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P_PRINHEX (N)
    (COND ((NOT (FIXP N)) (P_PRINC "<not-a-number>" NIL))
          (T
           (PROG (B W)
             (COND
              ((GEQ N 0)
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (GEQ N 16)) (RETURN NIL)))
                  (PROGN
                   (SETQ B (CONS (GETV P_HEXDIGITS* (SETQ W (MOD N 16))) B))
                   (SETQ N (QUOTIENT (DIFFERENCE N W) 16)))
                  (GO WHILELABEL))
                (SETQ B (CONS (GETV P_HEXDIGITS* (MOD N 16)) B))))
              (T
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (LESSP N (MINUS 1))) (RETURN NIL)))
                  (PROGN
                   (SETQ B (CONS (GETV P_HEXDIGITS* (SETQ W (MOD N 16))) B))
                   (SETQ N (QUOTIENT (DIFFERENCE N W) 16)))
                  (GO WHILELABEL))
                (SETQ B (CONS '~ (CONS (GETV P_HEXDIGITS* (MOD N 16)) B))))))
             (COND
              ((AND (NULL BLDMSG_CHARS*)
                    (GREATERP (PLUS (POSN) (LENGTH B)) *LL*))
               (TERPRI)))
             (PROG (C)
               (SETQ C B)
              LAB
               (COND ((NULL C) (RETURN NIL)))
               ((LAMBDA (C) (P_PRINC C NIL)) (CAR C))
               (SETQ C (CDR C))
               (GO LAB)))))) 
(PUT 'P_PRINOCTAL 'NUMBER-OF-ARGS 1) 
(PUT 'P_PRINOCTAL 'DEFINED-ON-LINE '563) 
(PUT 'P_PRINOCTAL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'P_PRINOCTAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P_PRINOCTAL (N)
    (COND ((NOT (FIXP N)) (P_PRINC "<not-a-number>" NIL))
          (T
           (PROG (B W)
             (COND
              ((GEQ N 0)
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (GEQ N 8)) (RETURN NIL)))
                  (PROGN
                   (SETQ B (CONS (SETQ W (MOD N 8)) B))
                   (SETQ N (QUOTIENT (DIFFERENCE N W) 8)))
                  (GO WHILELABEL))
                (SETQ B (CONS (MOD N 8) B))))
              (T
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (LESSP N (MINUS 1))) (RETURN NIL)))
                  (PROGN
                   (SETQ B (CONS (SETQ W (MOD N 8)) B))
                   (SETQ N (QUOTIENT (DIFFERENCE N W) 8)))
                  (GO WHILELABEL))
                (SETQ B (CONS '~ (CONS (MOD N 8) B))))))
             (COND
              ((AND (NULL BLDMSG_CHARS*)
                    (GREATERP (PLUS (POSN) (LENGTH B)) *LL*))
               (TERPRI)))
             (PROG (C)
               (SETQ C B)
              LAB
               (COND ((NULL C) (RETURN NIL)))
               ((LAMBDA (C) (P_PRINC C NIL)) (CAR C))
               (SETQ C (CDR C))
               (GO LAB)))))) 
(PUT 'PRINTF_INTERNAL 'NUMBER-OF-ARGS 2) 
(PUT 'PRINTF_INTERNAL 'DEFINED-ON-LINE '581) 
(PUT 'PRINTF_INTERNAL 'DEFINED-IN-FILE 'RTOOLS/RPRINTF.RED) 
(PUT 'PRINTF_INTERNAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINTF_INTERNAL (FMT ARGS)
    (PROG (A C *LL*)
      (SETQ *LL* (DIFFERENCE (LINELENGTH NIL) 2))
      (SETQ FMT (EXPLODE2 FMT))
      (PROG ()
       WHILELABEL
        (COND ((NOT FMT) (RETURN NIL)))
        (PROGN
         (SETQ C (CAR FMT))
         (SETQ FMT (CDR FMT))
         (COND ((NEQ C '%) (P_PRINC C NIL))
               (T
                (PROGN
                 (SETQ C (CAR FMT))
                 (SETQ FMT (CDR FMT))
                 (COND
                  ((EQUAL C 'F)
                   (PROGN
                    (COND
                     ((AND (NOT BLDMSG_CHARS*) (NOT (ZEROP (POSN))))
                      (TERPRI)))))
                  ((OR (EQUAL C 'N) (EQUAL C '|n|)) (P_PRINC $EOL$ NIL))
                  ((EQUAL C '%) (P_PRINC C NIL))
                  (T
                   (PROGN
                    (COND ((NULL ARGS) (SETQ A NIL))
                          (T
                           (PROGN (SETQ A (CAR ARGS)) (SETQ ARGS (CDR ARGS)))))
                    (COND
                     ((AND (OR (EQUAL C 'B) (EQUAL C '|b|)) (FIXP A))
                      (PROG (I)
                        (SETQ I 1)
                       LAB
                        (COND ((MINUSP (DIFFERENCE A I)) (RETURN NIL)))
                        (P_PRINC " " NIL)
                        (SETQ I (PLUS2 I 1))
                        (GO LAB)))
                     ((OR (EQUAL C 'C) (EQUAL C '|c|))
                      (PROGN
                       (COND
                        ((FIXP A) (P_PRINC (LIST2WIDESTRING (LIST A)) NIL))
                        (T (P_PRINC A NIL)))))
                     ((OR (EQUAL C 'L) (EQUAL C '|l|))
                      (PROGN
                       (COND
                        ((NOT (ATOM A))
                         (PROGN
                          (PORTABLE_PRINC (CAR A))
                          (PROG (X)
                            (SETQ X (CDR A))
                           LAB
                            (COND ((NULL X) (RETURN NIL)))
                            ((LAMBDA (X)
                               (PROGN (P_PRINC " " NIL) (PORTABLE_PRINC X)))
                             (CAR X))
                            (SETQ X (CDR X))
                            (GO LAB)))))))
                     ((OR (EQUAL C 'O) (EQUAL C '|o|)) (P_PRINOCTAL A))
                     ((OR (EQUAL C 'P) (EQUAL C '|p|)) (PORTABLE_PRIN A))
                     ((OR (EQUAL C 'Q) (EQUAL C '|q|)) (RLISP_PRINL A))
                     ((OR (EQUAL C 'R) (EQUAL C '|r|))
                      (PROGN
                       (P_PRINC "'" NIL)
                       (RLISP_PRINL A)
                       (P_PRINC "'" NIL)))
                     ((AND (OR (EQUAL C 'T) (EQUAL C '|t|)) (FIXP A))
                      (PROGN
                       (COND ((GREATERP (P_POSN) A) (P_PRINC $EOL$ NIL)))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT (LESSP (P_POSN) A)) (RETURN NIL)))
                         (P_PRINC " " NIL)
                         (GO WHILELABEL))))
                     ((OR (EQUAL C 'W) (EQUAL C 'D) (EQUAL C 'S) (EQUAL C '|w|)
                          (EQUAL C '|d|) (EQUAL C '|s|))
                      (PORTABLE_PRINC A))
                     ((OR (EQUAL C 'X) (EQUAL C '|x|)) (P_PRINHEX A))
                     ((EQUAL C '@)
                      (PROGN
                       (SETQ C (CAR FMT))
                       (SETQ FMT (CDR FMT))
                       (COND
                        ((OR (EQUAL C 'F) (EQUAL C '|f|))
                         (P_PREFIX (PREPF A) 0))
                        ((OR (EQUAL C 'Q) (EQUAL C '|q|))
                         (P_PREFIX (PREPSQ A) 0))
                        ((OR (EQUAL C 'P) (EQUAL C '|p|)) (P_PREFIX A 0))
                        (T (PROGN (P_PRINC "%@" NIL) (P_PRINC C NIL))))))
                     ((OR (EQUAL C 'E) (EQUAL C '|e|)) (EVAL A))
                     (T (PROGN (P_PRINC "%" NIL) (P_PRINC C NIL)))))))))))
        (GO WHILELABEL)))) 
(ENDMODULE) 
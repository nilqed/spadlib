(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DFPART)) 
(CREATE-PACKAGE '(DFPART) '(CONTRIB MISC)) 
(FLUID '(YCOORD* YMIN*)) 
(PUT 'DFP 'SIMPFN 'SIMPDFP) 
(PUT 'SIMPDFP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDFP 'DEFINED-ON-LINE '38) 
(PUT 'SIMPDFP 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'SIMPDFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDFP (U)
    (PROG (F FN DD P W L)
      (COND ((LESSP (LENGTH U) 2) (GO ERROR)))
      (SETQ F (REVAL1 (CAR U) T))
      (COND
       ((NOT (PAIRP F))
        (RETURN
         (COND ((MEMBER (CADR U) FRLIS*) (MKSQ (CONS 'DFP U) 1))
               (T (SIMPDF (CONS F (CDR (CADR U)))))))))
      (SETQ FN (CAR F))
      (SETQ P (CDR F))
      (SETQ DD (REVAL1 (CADR U) T))
      (COND
       ((AND (NOT (MEMBER DD FRLIS*)) (NOT (EQCAR DD 'LIST)))
        (PROGN
         (SETQ DD
                 (CONS DD
                       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Y (CDDR U))
                         (COND ((NULL Y) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Y) (REVAL1 Y T)) (CAR Y))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Y (CDR Y))
                         (COND ((NULL Y) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (Y) (REVAL1 Y T)) (CAR Y))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ DD (CONS 'LIST (DFP-NORMALIZE DD NIL)))
         (RETURN (SIMP (LIST 'DFP F DD)))
         NIL)))
      (SETQ L (GET FN 'GENERIC_FUNCTION))
      (SETQ W T)
      (COND
       ((AND L (EQCAR DD 'LIST))
        (PROG (Y)
          (SETQ Y (CDR DD))
         LAB
          (COND ((NULL Y) (RETURN NIL)))
          ((LAMBDA (Y) (SETQ W (AND W (MEMBER Y L)))) (CAR Y))
          (SETQ Y (CDR Y))
          (GO LAB))))
      (COND ((NOT W) (RETURN (CONS NIL 1))))
      (COND ((EQUAL DD '(LIST)) (RETURN (MKSQ F 1))))
      (COND
       ((AND L (FLAGP FN 'DFP_COMMUTE))
        ((LAMBDA (KORD*) (SETQ DD (CONS 'LIST (SORT (CDR DD) 'ORDP)))) L)))
      (SETQ U (LIST 'DFP F DD))
      (RETURN (MKSQ U 1))
     ERROR
      (TYPERR (CONS 'DFP U) "generic differential"))) 
(PUT 'DFP-NORMALIZE 'NUMBER-OF-ARGS 2) 
(PUT 'DFP-NORMALIZE 'DEFINED-ON-LINE '69) 
(PUT 'DFP-NORMALIZE 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFP-NORMALIZE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DFP-NORMALIZE (L X)
    (COND ((NULL L) NIL)
          ((IDP (CAR L)) (CONS (CAR L) (DFP-NORMALIZE (CDR L) (CAR L))))
          ((NUMBERP (CAR L))
           (APPEND
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 2)
              (COND ((MINUSP (DIFFERENCE (CAR L) I)) (RETURN NIL)))
              (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS X NIL)))
             LOOPLABEL
              (SETQ I (PLUS2 I 1))
              (COND ((MINUSP (DIFFERENCE (CAR L) I)) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS X NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (DFP-NORMALIZE (CDR L) NIL)))
          (T (TYPERR (CAR L) "dfp variable")))) 
(PUT 'GENERIC_FUNCTION 'NUMBER-OF-ARGS 1) 
(PUT 'GENERIC_FUNCTION 'DEFINED-ON-LINE '76) 
(PUT 'GENERIC_FUNCTION 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'GENERIC_FUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERIC_FUNCTION (U)
    (PROG (FC)
      (SETQ FC U)
     LAB
      (COND ((NULL FC) (RETURN NIL)))
      ((LAMBDA (FC)
         (PROG (RS PARS FCN L)
           (SETQ L 0)
           (COND
            ((OR (ATOM FC) (NOT (IDP (SETQ FCN (CAR FC)))))
             (TYPERR FC "generic function")))
           (SETQ L (LENGTH (CDR FC)))
           (AEVAL (CLEAR (LIST FCN)))
           (REMFLAG (LIST FCN) 'EVEN)
           (REMFLAG (LIST FCN) 'ODD)
           (REMFLAG (LIST FCN) 'NONZERO)
           (APPLY 'DEPEND (LIST FC))
           ((LAMBDA (*MODE) (APPLY 'OPERATOR (LIST (LIST FCN)))) 'ALGEBRAIC)
           (SETQ PARS
                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                     (SETQ I 1)
                     (COND ((MINUSP (DIFFERENCE L I)) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS (LIST '~ (GENSYM)) NIL)))
                    LOOPLABEL
                     (SETQ I (PLUS2 I 1))
                     (COND ((MINUSP (DIFFERENCE L I)) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR (CONS (LIST '~ (GENSYM)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ RS
                   (LIST 'LIST
                         (LIST 'REPLACEBY (LIST 'DF (LIST FCN) (LIST '~ '|:X|))
                               (LIST 'DF FC '|:X|))
                         (LIST 'REPLACEBY
                               (LIST 'DF (CONS FCN PARS) (LIST '~ '|:X|))
                               (CONS 'PLUS
                                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ I 1)
                                       (COND
                                        ((MINUSP (DIFFERENCE L I))
                                         (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        (LIST 'TIMES
                                                              (LIST 'DFP
                                                                    (CONS FCN
                                                                          PARS)
                                                                    (LIST 'LIST
                                                                          (NTH
                                                                           (CDR
                                                                            FC)
                                                                           I)))
                                                              (LIST 'DF
                                                                    (NTH PARS
                                                                         I)
                                                                    '|:X|))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ I (PLUS2 I 1))
                                       (COND
                                        ((MINUSP (DIFFERENCE L I))
                                         (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                (LIST 'TIMES
                                                      (LIST 'DFP
                                                            (CONS FCN PARS)
                                                            (LIST 'LIST
                                                                  (NTH (CDR FC)
                                                                       I)))
                                                      (LIST 'DF (NTH PARS I)
                                                            '|:X|))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))))
           (PUT FCN 'GENERIC_FUNCTION (CDR FC))
           (PUT FCN 'SUBFUNC 'GENERIC-SUB)
           (AEVAL (LET (LIST RS)))))
       (CAR FC))
      (SETQ FC (CDR FC))
      (GO LAB))) 
(PUT 'GENERIC_FUNCTION 'STAT 'RLIS) 
(PUT 'DFP_COMMUTE 'NUMBER-OF-ARGS 1) 
(PUT 'DFP_COMMUTE 'DEFINED-ON-LINE '117) 
(PUT 'DFP_COMMUTE 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFP_COMMUTE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DFP_COMMUTE (U)
    (PROG (F)
      (SETQ F U)
     LAB
      (COND ((NULL F) (RETURN NIL)))
      ((LAMBDA (F)
         (PROGN
          (SETQ F (REVAL1 F T))
          (COND ((NOT (IDP F)) (SETQ F (CAR F))))
          (COND ((NOT (GET F 'GENERIC_FUNCTION)) (TYPERR F "generic function"))
                (T (FLAG (LIST F) 'DFP_COMMUTE)))
          NIL))
       (CAR F))
      (SETQ F (CDR F))
      (GO LAB))) 
(PUT 'DFP_COMMUTE 'STAT 'RLIS) 
(PUT 'GENERIC_ARGUMENTS 'NUMBER-OF-ARGS 1) 
(PUT 'GENERIC_ARGUMENTS 'DEFINED-ON-LINE '127) 
(PUT 'GENERIC_ARGUMENTS 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'GENERIC_ARGUMENTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERIC_ARGUMENTS (F) (CONS 'LIST (GET (CAR F) 'GENERIC_FUNCTION))) 
(PUT 'ACTUAL_ARGUMENTS 'NUMBER-OF-ARGS 1) 
(PUT 'ACTUAL_ARGUMENTS 'DEFINED-ON-LINE '131) 
(PUT 'ACTUAL_ARGUMENTS 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'ACTUAL_ARGUMENTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ACTUAL_ARGUMENTS (F)
    (CONS 'LIST (OR (CDR F) (GET (CAR F) 'GENERIC_FUNCTION)))) 
(FLAG '(GENERIC_ARGUMENTS) 'OPFN) 
(FLAG '(ACTUAL_ARGUMENTS) 'OPFN) 
(PUT 'DFP-RULE-FOUND 'NUMBER-OF-ARGS 2) 
(PUT 'DFP-RULE-FOUND 'DEFINED-ON-LINE '141) 
(PUT 'DFP-RULE-FOUND 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFP-RULE-FOUND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DFP-RULE-FOUND (NEWFORM OLDF)
    (OR (NOT (EQCAR NEWFORM 'DFP)) (NEQ (CADR NEWFORM) OLDF))) 
(FLAG '(DFP-RULE-FOUND) 'OPFN) 
(PUT 'SOFT-APPEND 'NUMBER-OF-ARGS 2) 
(PUT 'SOFT-APPEND 'DEFINED-ON-LINE '146) 
(PUT 'SOFT-APPEND 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'SOFT-APPEND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOFT-APPEND (A B)
    (PROGN
     (SETQ A (COND ((EQCAR A 'LIST) (CDR A)) (T (LIST A))))
     (SETQ B (COND ((EQCAR B 'LIST) (CDR B)) (T (LIST B))))
     (CONS 'LIST (APPEND A B)))) 
(FLAG '(SOFT-APPEND) 'OPFN) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SETK 'DFP_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'DFP (LIST '~ 'F) (LIST '~ 'Q))
                         (LIST '~ 'X))
                   (LIST 'PROG (LIST 'I 'FORALL-RESULT) (LIST 'SETQ 'I 1)
                         (LIST 'SETQ 'FORALL-RESULT 0) 'LAB1
                         (LIST 'COND
                               (LIST
                                (LIST '|AMINUSP:|
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'AEVAL*
                                                  (LIST 'LIST ''LENGTH
                                                        (LIST 'LIST
                                                              ''GENERIC_ARGUMENTS
                                                              ''F)))
                                            'I))
                                (LIST 'RETURN 'FORALL-RESULT)))
                         (LIST 'SETQ 'FORALL-RESULT
                               (LIST 'AEVAL*
                                     (LIST 'LIST ''PLUS
                                           (LIST 'AEVAL*
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DFP ''F
                                                             (LIST 'LIST
                                                                   ''APPEND ''Q
                                                                   (LIST 'LIST
                                                                         ''LIST
                                                                         (LIST
                                                                          'LIST
                                                                          ''PART
                                                                          (LIST
                                                                           'LIST
                                                                           ''GENERIC_ARGUMENTS
                                                                           ''F)
                                                                          'I))))
                                                       (LIST 'LIST ''DF
                                                             (LIST 'LIST ''PART
                                                                   (LIST 'LIST
                                                                         ''ACTUAL_ARGUMENTS
                                                                         ''F)
                                                                   'I)
                                                             ''X)))
                                           'FORALL-RESULT)))
                         (LIST 'SETQ 'I
                               (LIST
                                (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS 'FORALL-RESULT
                                                  1)))
                                'I))
                         (LIST 'GO 'LAB1)))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'G))
                         (LIST '~ 'Q))
                   (LIST 'PLUS (LIST 'DFP 'F 'Q) (LIST 'DFP 'G 'Q)))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'MINUS (LIST '~ 'F)) (LIST '~ 'Q))
                   (LIST 'MINUS (LIST 'DFP 'F 'Q)))
             (LIST 'REPLACEBY (LIST 'DFP (LIST '~ 'F) (LIST '~ 'Q))
                   (LIST 'WHEN
                         (LIST 'DFP
                               (LIST 'DFP 'F (LIST 'LIST (LIST 'FIRST 'Q)))
                               (LIST 'REST 'Q))
                         (LIST 'AND
                               (LIST 'NEQ (LIST 'ARGLENGTH 'Q) (LIST 'MINUS 1))
                               (LIST 'EQUAL (LIST 'PART 'Q 0) 'LIST)
                               (LIST 'GREATERP (LIST 'LENGTH 'Q) 1)
                               (LIST 'DFP-RULE-FOUND
                                     (LIST 'DFP 'F
                                           (LIST 'LIST (LIST 'FIRST 'Q)))
                                     'F))))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'TIMES (LIST '~ 'F) (LIST '~ 'G))
                         (LIST 'LIST (LIST '~ 'Q)))
                   (LIST 'PLUS (LIST 'TIMES (LIST 'DFP 'F (LIST 'LIST 'Q)) 'G)
                         (LIST 'TIMES (LIST 'DFP 'G (LIST 'LIST 'Q)) 'F)))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G))
                         (LIST 'LIST (LIST '~ 'Q)))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'DFP 'F (LIST 'LIST 'Q)) 'G)
                               (LIST 'TIMES (LIST 'DFP 'G (LIST 'LIST 'Q)) 'F))
                         (LIST 'EXPT 'G 2)))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'EXPT (LIST '~ 'F) (LIST '~ 'N))
                         (LIST 'LIST (LIST '~ 'Q)))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'EXPT 'F (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DFP 'F (LIST 'LIST 'Q)))
                         (LIST 'NUMBERP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'DFP (LIST 'DFP (LIST '~ 'F) (LIST '~ 'Q))
                         (LIST '~ 'R))
                   (LIST 'DFP 'F (LIST 'SOFT-APPEND 'Q 'R)))))) 
(LET '(DFP_RULES)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'GENERIC-SUB 'NUMBER-OF-ARGS 2) 
(PUT 'GENERIC-SUB 'DEFINED-ON-LINE '192) 
(PUT 'GENERIC-SUB 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'GENERIC-SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENERIC-SUB (U V) (DFP-SUB U (LIST 'DFP V (LIST 'LIST)))) 
(PUT 'DFP-SUB 'NUMBER-OF-ARGS 2) 
(PUT 'DFP-SUB 'DEFINED-ON-LINE '195) 
(PUT 'DFP-SUB 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFP-SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DFP-SUB (U V)
    (PROG (P F FN NF L W)
      (SETQ F (CADR V))
      (SETQ P (CDR F))
      (SETQ L (GET (SETQ FN (CAR F)) 'GENERIC_FUNCTION))
      (COND
       ((NULL P)
        (PROGN
         (SETQ W NIL)
         (PROG (Y)
           (SETQ Y L)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y) (SETQ W (OR W (ASSOC Y U)))) (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (COND (W (SETQ P L)))
         NIL)))
      (SETQ P (CDR (LISTSUB U (CONS 'LIST P))))
      (COND
       ((AND (NULL (SETQ NF (ASSOC FN U)))
             (NULL (SETQ NF (ASSOC (CONS FN L) U))))
        (RETURN (LIST 'DFP (CONS FN P) (CADDR V)))))
      (SETQ NF (REVAL1 (CDR NF) T))
      (SETQ NF (DFP-SUB1 NF (COND (P (PAIR L P))) U))
      (RETURN (LIST 'DFP NF (CADDR V))))) 
(PUT 'DFP-SUB1 'NUMBER-OF-ARGS 3) 
(PUT 'DFP-SUB1 'DEFINED-ON-LINE '219) 
(PUT 'DFP-SUB1 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFP-SUB1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFP-SUB1 (U L S)
    (COND
     ((IDP U) (COND ((GET U 'GENERIC_FUNCTION) (DFP-SUB1 (LIST U) L S)) (T U)))
     ((ATOM U) U)
     (T
      (PROG (OP P PP)
        (SETQ OP (CAR U))
        (COND
         ((SETQ P (GET OP 'GENERIC_FUNCTION))
          (PROGN
           (SETQ P (OR (CDR U) P))
           (SETQ PP (SUBLA L P))
           (SETQ PP (SUBLA S PP))
           (RETURN (COND ((EQUAL P PP) U) (T (REVAL1 (CONS OP PP) T))))
           NIL)))
        (RETURN
         (CONS OP
               (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                 (SETQ Q (CDR U))
                 (COND ((NULL Q) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (Q) (DFP-SUB1 Q L S)) (CAR Q))
                                       NIL)))
                LOOPLABEL
                 (SETQ Q (CDR Q))
                 (COND ((NULL Q) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (Q) (DFP-SUB1 Q L S)) (CAR Q)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'DFP 'SUBFUNC 'DFP-SUB) 
(PUT 'DFPPRI 'NUMBER-OF-ARGS 1) 
(PUT 'DFPPRI 'DEFINED-ON-LINE '241) 
(PUT 'DFPPRI 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'DFPPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DFPPRI (L)
    (PROG (DD F)
      (COND ((OR (NOT *NAT) *FORT) (RETURN 'FAILED)))
      (SETQ F (CADR L))
      (SETQ DD (CADDR L))
      (COND
       ((OR (ATOM F) (NOT (GET (CAR F) 'GENERIC_FUNCTION))) (RETURN 'FAILED)))
      (PRIN2* (CAR F))
      (SETQ YCOORD* (DIFFERENCE YCOORD* 1))
      (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
      (PROG (Y)
        (SETQ Y (CDR DD))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (PRIN2* Y)) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ YCOORD* (PLUS YCOORD* 1))
      (COND
       ((CDR F)
        (PROGN (PRIN2* "(") (INPRINT '*COMMA* 0 (CDR F)) (PRIN2* ")") NIL)))
      (RETURN L))) 
(PUT 'DFP 'PRIFN 'DFPPRI) 
(PUT 'FANCY-DFPPRI 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-DFPPRI 'DEFINED-ON-LINE '262) 
(PUT 'FANCY-DFPPRI 'DEFINED-IN-FILE 'MISC/DFPART.RED) 
(PUT 'FANCY-DFPPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-DFPPRI (L) (FANCY-DFPRIINDEXED L NIL)) 
(PUT 'DFP 'FANCY-PRIFN 'FANCY-DFPPRI) 
(ENDMODULE) 
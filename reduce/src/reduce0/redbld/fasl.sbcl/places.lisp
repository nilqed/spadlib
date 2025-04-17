(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PLACES)) 
(FLUID
 '(BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS INTVAR LISTOFALLSQRTS
   LISTOFNEWSQRTS SQRT-INTVAR SQRT-PLACES-ALIST SQRTS-IN-INTEGRAND)) 
(EXPORTS
 (LIST 'GETSQRTSFROMPLACES 'SQRTSINPLACES 'GET-CORRECT-SQRTS 'BASICPLACE
       'EXTENPLACE 'EQUALPLACE 'PRINTPLACE)) 
(PUT 'GETSQRTSFROMPLACES 'NUMBER-OF-ARGS 1) 
(PUT 'GETSQRTSFROMPLACES 'DEFINED-ON-LINE '80) 
(PUT 'GETSQRTSFROMPLACES 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'GETSQRTSFROMPLACES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETSQRTSFROMPLACES (PLACES)
    (PROG (BASIS V B C VV)
      (PROG (U)
        (SETQ U PLACES)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (SETQ V (ANTISUBS (BASICPLACE U) INTVAR))
            (SETQ VV
                    (SQRTSINSQ
                     (SUBSTITUTESQ
                      (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)) 1) V)
                     INTVAR))
            (COND (VV (SETQ VV (SIMP (CADR (CAR VV))))))
            (PROG (W)
              (SETQ W (EXTENPLACE U))
             LAB
              (COND ((NULL W) (RETURN NIL)))
              ((LAMBDA (W)
                 (PROGN
                  (SETQ B (SUBSTITUTESQ (SIMP (CAR W)) V))
                  (SETQ B (DELETE SQRT-INTVAR (SQRTSINSQ B INTVAR)))
                  (PROG (U)
                    (SETQ U B)
                   LAB
                    (COND ((NULL U) (RETURN NIL)))
                    ((LAMBDA (U)
                       (PROG (V)
                         (SETQ V (DELETE U B))
                        LAB
                         (COND ((NULL V) (RETURN NIL)))
                         ((LAMBDA (V)
                            (COND ((DEPENDSP V U) (SETQ B (DELETE U B)))))
                          (CAR V))
                         (SETQ V (CDR V))
                         (GO LAB)))
                     (CAR U))
                    (SETQ U (CDR U))
                    (GO LAB))
                  (COND ((IEQUAL (LENGTH B) 1) (SETQ B (CAR B)))
                        (T
                         (SETQ B
                                 (CAAAR
                                  (CAR
                                   (SIMPSQRTSQ
                                    (MAPPLY (FUNCTION *MULTSQ)
                                            (PROG (U FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ U B)
                                              (COND ((NULL U) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (U)
                                                                  (SIMP
                                                                   (CADR U)))
                                                                (CAR U))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ U (CDR U))
                                              (COND
                                               ((NULL U)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (U)
                                                          (SIMP (CADR U)))
                                                        (CAR U))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))))))))
                  (COND
                   ((AND VV (NOT (MEMBER B SQRTS-IN-INTEGRAND)))
                    (PROGN
                     (SETQ C (CAR (MULTSQ (SIMP (CADR B)) VV)))
                     (SETQ C (CAR (SQRTSINSF (SIMPSQRT2 C) NIL INTVAR)))
                     (COND ((MEMBER C SQRTS-IN-INTEGRAND) (SETQ B C))))))
                  (COND ((NOT (MEMBER B BASIS)) (SETQ BASIS (CONS B BASIS))))))
               (CAR W))
              (SETQ W (CDR W))
              (GO LAB))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (PROG (U)
        (SETQ U PLACES)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROG ()
             (SETQ V (CDR U))
             (COND ((OR (NULL V) (NEQ (CAR (CDAR V)) 'EXPT)) (RETURN NIL)))
             (SETQ U (SIMP* (SUBST (LIST 'MINUS INTVAR) INTVAR (CDAR U))))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND V (EQ (CAR (CDAR V)) 'EXPT))) (RETURN NIL)))
               (PROGN
                (SETQ U (SIMPSQRTSQ U))
                (SETQ V (CDR V))
                (SETQ BASIS
                        (UNION BASIS
                               (DELETE SQRT-INTVAR (SQRTSINSQ U INTVAR)))))
               (GO WHILELABEL))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (RETURN (REMOVE-EXTRA-SQRTS BASIS)))) 
(PUT 'SQRTSINPLACES 'NUMBER-OF-ARGS 1) 
(PUT 'SQRTSINPLACES 'DEFINED-ON-LINE '131) 
(PUT 'SQRTSINPLACES 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'SQRTSINPLACES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRTSINPLACES (U)
    (COND ((NULL U) NIL)
          (T
           (SQRTSINTREE
            (PROG (V FORALL-RESULT FORALL-ENDPTR)
              (SETQ V (CAR U))
              (COND ((NULL V) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL)))
             LOOPLABEL
              (SETQ V (CDR V))
              (COND ((NULL V) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            INTVAR (SQRTSINPLACES (CDR U)))))) 
(PUT 'GET-CORRECT-SQRTS 'NUMBER-OF-ARGS 1) 
(PUT 'GET-CORRECT-SQRTS 'DEFINED-ON-LINE '157) 
(PUT 'GET-CORRECT-SQRTS 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'GET-CORRECT-SQRTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-CORRECT-SQRTS (U)
    (PROG (V)
      (SETQ V (ASSOC U SQRT-PLACES-ALIST))
      (COND
       (V
        (PROGN
         (SETQ V (CDR V))
         (SETQ LISTOFALLSQRTS (CDR V))
         (SETQ LISTOFNEWSQRTS (CAR V))))
       (T
        (PROGN
         (SETQ LISTOFNEWSQRTS BASIC-LISTOFNEWSQRTS)
         (SETQ LISTOFALLSQRTS BASIC-LISTOFALLSQRTS))))
      (RETURN NIL))) 
(PUT 'BASICPLACE 'NUMBER-OF-ARGS 1) 
(PUT 'BASICPLACE 'DEFINED-ON-LINE '193) 
(PUT 'BASICPLACE 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'BASICPLACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BASICPLACE (U)
    (COND ((NULL U) NIL) ((ATOM (CAAR U)) (CONS (CAR U) (BASICPLACE (CDR U))))
          (T NIL))) 
(PUT 'EXTENPLACE 'NUMBER-OF-ARGS 1) 
(PUT 'EXTENPLACE 'DEFINED-ON-LINE '203) 
(PUT 'EXTENPLACE 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'EXTENPLACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTENPLACE (U) (COND ((AND U (ATOM (CAAR U))) (EXTENPLACE (CDR U))) (T U))) 
(PUT 'EQUALPLACE 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALPLACE 'DEFINED-ON-LINE '211) 
(PUT 'EQUALPLACE 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'EQUALPLACE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALPLACE (A B)
    (COND ((NULL A) (COND ((NULL B) T) (T NIL))) ((NULL B) NIL)
          ((MEMBER (CAR A) B) (EQUALPLACE (CDR A) (DELETE (CAR A) B))) (T NIL))) 
(PUT 'REMOVE-EXTRA-SQRTS 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE-EXTRA-SQRTS 'DEFINED-ON-LINE '225) 
(PUT 'REMOVE-EXTRA-SQRTS 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'REMOVE-EXTRA-SQRTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE-EXTRA-SQRTS (BASIS)
    (PROG (BASIS2 SAVE)
      (SETQ SAVE
              (SETQ BASIS2
                      (PROG (U FORALL-RESULT FORALL-ENDPTR)
                        (SETQ U BASIS)
                        (COND ((NULL U) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (U) (*Q2F (SIMP (CADR U))))
                                          (CAR U))
                                         NIL)))
                       LOOPLABEL
                        (SETQ U (CDR U))
                        (COND ((NULL U) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (PROG (U)
        (SETQ U BASIS2)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROG (V)
             (SETQ V (DELETE U BASIS2))
            LAB
             (COND ((NULL V) (RETURN NIL)))
             ((LAMBDA (V)
                (COND
                 (((LAMBDA (*EXP) (QUOTF1 V U)) T)
                  (SETQ BASIS2 (DELETE V BASIS2)))))
              (CAR V))
             (SETQ V (CDR V))
             (GO LAB)))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (COND ((EQ BASIS2 SAVE) (RETURN BASIS))
            (T
             (RETURN
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U BASIS2)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (LIST 'SQRT (PREPF U))) (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (LIST 'SQRT (PREPF U))) (CAR U))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'PRINTPLACE 'NUMBER-OF-ARGS 1) 
(PUT 'PRINTPLACE 'DEFINED-ON-LINE '240) 
(PUT 'PRINTPLACE 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'PRINTPLACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINTPLACE (U)
    (PROG (A N V)
      (SETQ A (CDAR U))
      (PRINC (SETQ V (CAAR U)))
      (PRINC "=")
      (COND ((ATOM A) (PRINC "0"))
            ((AND (EQ (CAR A) 'QUOTIENT) (EQUAL (CADR A) 1))
             (PRINC "infinity"))
            (T
             (PROGN
              (SETQ N
                      (NEGSQ
                       (ADDSQ (CONS (LIST (CONS (GETPOWER (FKERN V) 1) 1)) 1)
                              (NEGSQ (SIMP* A)))))
              (COND
               ((AND (NUMBERP (CAR N)) (NUMBERP (CDR N)))
                (PROGN
                 (PRINC (CAR N))
                 (COND
                  ((NOT (ONEP (CDR N)))
                   (PROGN (PRINC " / ") (PRINC (CDR N)))))))
               (T
                (PROGN
                 (COND
                  ((GREATERP (DEGREEIN (CAR N) INTVAR) 1)
                   (PROGN (PRIN2 "Any root of:") (TERPRI) "Any root of:")))
                 (PRINTSQ N)
                 (COND ((CDR U) (PRINC "at the place ")))))))))
      (SETQ U (CDR U))
      (COND ((NULL U) (GO NL-RETURN)))
      (SETQ N 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (EQ (CAR (CDAR U)) 'EXPT))) (RETURN NIL)))
        (PROGN (SETQ N (TIMES N (CADDR (CDAR U)))) (SETQ U (CDR U)))
        (GO WHILELABEL))
      (COND
       ((NEQ N 1)
        (PROGN
         (TERPRI* NIL)
         (PRIN2 " ")
         (PRINC V)
         (PRINC "=>")
         (PRINC V)
         (PRINC "**")
         (PRINC N))))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND ((EQ (CAR (CDAR U)) 'MINUS) (PRINC "-")) (T (PRINC "+")))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
     NL-RETURN
      (TERPRI)
      (RETURN NIL))) 
(PUT 'DEGREEIN 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREEIN 'DEFINED-ON-LINE '295) 
(PUT 'DEGREEIN 'DEFINED-IN-FILE 'ALGINT/PLACES.RED) 
(PUT 'DEGREEIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREEIN (SF VAR)
    (COND ((ATOM SF) 0) ((EQ (CAAAR SF) VAR) (CDAAR SF))
          (T (MAX (DEGREEIN (CDAR SF) VAR) (DEGREEIN (CDR SF) VAR))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'STR)) 
(FLUID '(*FORT *NAT *SAVESTRUCTR SCOUNTR SVAR SVARLIS)) 
(GLOBAL '(VARNAM*)) 
(SETQ VARNAM* 'ANS) 
(SWITCH (LIST 'SAVESTRUCTR)) 
(FLAG '(STRUCTR) 'INTFN) 
(PUT 'STRUCTR 'NUMBER-OF-ARGS 1) 
(PUT 'STRUCTR 'DEFINED-ON-LINE '45) 
(PUT 'STRUCTR 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'STRUCTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRUCTR (U)
    (PROG (SCOUNTR FVAR SVAR SVARLIS)
      (SETQ SCOUNTR 0)
      (SETQ FVAR (SETQ SVAR VARNAM*))
      (COND
       ((CDR U)
        (PROGN
         (SETQ FVAR (SETQ SVAR (CADR U)))
         (COND ((CDDR U) (SETQ FVAR (CADDR U)))))))
      (SETQ U (STRUCTR1 (REVAL1 (CAR U) NIL)))
      (COND (*FORT (SETQ SVARLIS (REVERSIP* SVARLIS)))
            ((NOT *SAVESTRUCTR)
             (PROGN
              (ASSGNPRI U NIL 'ONLY)
              (COND ((NOT (EQCAR U 'MAT)) (TERPRI)))
              (COND ((EQUAL SCOUNTR 0) (RETURN NIL))
                    (T
                     (PROGN
                      (COND ((NULL *NAT) (TERPRI)))
                      (PRIN2T "   where")))))))
      (COND
       ((OR *FORT (NOT *SAVESTRUCTR))
        (PROG (X)
          (SETQ X SVARLIS)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROGN
              (TERPRI* T)
              (COND ((NULL *FORT) (PRIN2* "      ")))
              (ASSGNPRI (CDDR X) (LIST (CADR X)) T)))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))))
      (COND (*FORT (ASSGNPRI U (LIST FVAR) T))
            (*SAVESTRUCTR
             (RETURN
              (CONS 'LIST
                    (CONS U
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X SVARLIS)
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (X)
                                                (LIST 'EQUAL (CADR X)
                                                      (MKQUOTE (CDDR X))))
                                              (CAR X))
                                             NIL)))
                           LOOPLABEL
                            (SETQ X (CDR X))
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X)
                                        (LIST 'EQUAL (CADR X)
                                              (MKQUOTE (CDDR X))))
                                      (CAR X))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))))))) 
(RLISTAT '(STRUCTR)) 
(PUT 'STRUCTR1 'NUMBER-OF-ARGS 1) 
(PUT 'STRUCTR1 'DEFINED-ON-LINE '76) 
(PUT 'STRUCTR1 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'STRUCTR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRUCTR1 (U)
    (COND ((ATOM U) U)
          ((EQ (CAR U) 'MAT)
           (CONS (CAR U)
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (CDR U))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ K J)
                                         (COND ((NULL K) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (K)
                                                             (STRUCTR1 K))
                                                           (CAR K))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ K (CDR K))
                                         (COND
                                          ((NULL K) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (K) (STRUCTR1 K))
                                                   (CAR K))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J)
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K J)
                                 (COND ((NULL K) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (K) (STRUCTR1 K))
                                                   (CAR K))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ K (CDR K))
                                 (COND ((NULL K) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K) (STRUCTR1 K)) (CAR K))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQ (CAR U) 'LIST)
           (CONS 'LIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (CDR U))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (STRUCTR1 J)) (CAR J))
                                         NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (STRUCTR1 J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQ (CAR U) 'EQUAL) (LIST 'EQUAL (CADR U) (STRUCTR1 (CADDR U))))
          ((EQ (CAR U) '*SQ)
           (MK*SQ (CONS (STRUCTF (CAR (CADR U))) (STRUCTF (CDR (CADR U))))))
          ((GETRTYPE U) (TYPERR U "STRUCTR argument")) (T U))) 
(PUT 'STRUCTF 'NUMBER-OF-ARGS 1) 
(PUT 'STRUCTF 'DEFINED-ON-LINE '91) 
(PUT 'STRUCTF 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'STRUCTF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRUCTF (U)
    (COND ((NULL U) NIL) ((OR (ATOM U) (ATOM (CAR U))) U)
          (T
           (PROG (X Y)
             (SETQ X (CAAAR U))
             (COND
              ((SFP X)
               (COND ((SETQ Y (ASSOC X SVARLIS)) (SETQ X (CADR Y)))
                     (T
                      (SETQ X
                              (STRUCTK (PREPSQ* (CONS (STRUCTF X) 1))
                                       (STRUCTVAR) X)))))
              ((AND (NOT (ATOM X))
                    (NOT (AND (ATOM (CAR X)) (FLAGP (CAR X) 'NOREPLACE))))
               (COND ((SETQ Y (ASSOC X SVARLIS)) (SETQ X (CADR Y)))
                     (T (SETQ X (STRUCTK X (STRUCTVAR) X))))))
             (RETURN
              (CONS (CONS (CONS X (CDAAR U)) (STRUCTF (CDAR U)))
                    (STRUCTF (CDR U)))))))) 
(PUT 'STRUCTK 'NUMBER-OF-ARGS 3) 
(PUT 'STRUCTK 'DEFINED-ON-LINE '114) 
(PUT 'STRUCTK 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'STRUCTK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STRUCTK (U ID V)
    (PROG (X)
      (COND
       ((SETQ X (SUBCHK1 U SVARLIS ID))
        (RPLACD X (CONS (CONS V (CONS ID U)) (CDR X))))
       ((SETQ X (SUBCHK2 U SVARLIS))
        (SETQ SVARLIS (CONS (CONS V (CONS ID X)) SVARLIS)))
       (T (SETQ SVARLIS (CONS (CONS V (CONS ID U)) SVARLIS))))
      (RETURN ID))) 
(PUT 'SUBCHK1 'NUMBER-OF-ARGS 3) 
(PUT 'SUBCHK1 'DEFINED-ON-LINE '124) 
(PUT 'SUBCHK1 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'SUBCHK1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBCHK1 (U V ID)
    (PROG (W)
      (PROG ()
       WHILELABEL
        (COND ((NOT V) (RETURN NIL)))
        (PROGN
         (AND (SMEMBER U (CDDAR V))
              (PROGN (SETQ W V) (RPLACD (CDAR V) (SUBST ID U (CDDAR V)))))
         (SETQ V (CDR V)))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'SUBCHK2 'NUMBER-OF-ARGS 2) 
(PUT 'SUBCHK2 'DEFINED-ON-LINE '133) 
(PUT 'SUBCHK2 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'SUBCHK2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBCHK2 (U V)
    (PROG (BOOL)
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (AND (SMEMBER (CDDR X) U)
                (PROGN (SETQ BOOL T) (SETQ U (SUBST (CADR X) (CDDR X) U)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND (BOOL (RETURN U)) (T (RETURN NIL))))) 
(PUT 'STRUCTVAR 'NUMBER-OF-ARGS 0) 
(PUT 'STRUCTVAR 'DEFINED-ON-LINE '141) 
(PUT 'STRUCTVAR 'DEFINED-IN-FILE 'ALG/STR.RED) 
(PUT 'STRUCTVAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE STRUCTVAR NIL
    (PROG ()
      (SETQ SCOUNTR (PLUS SCOUNTR 1))
      (RETURN
       (COND ((ARRAYP SVAR) (LIST SVAR SCOUNTR))
             (T
              (INTERN (COMPRESS (APPEND (EXPLODE SVAR) (EXPLODE SCOUNTR))))))))) 
(ENDMODULE) 
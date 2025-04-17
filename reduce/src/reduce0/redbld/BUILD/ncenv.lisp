(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NCENV)) 
(SHARE (LIST 'NCPI-BRACKETS* 'NCPI-COMM-RULES* 'NCPI-NAME-RULES*)) 
(AEVAL (OPERATOR (LIST 'NC*))) 
(AEVAL (NONCOM (LIST 'NC*))) 
(PUT 'NC* 'PRIFN 'PRI-NC*) 
(PUT 'NC* 'DIPPRIFN 'DIPPRI-NC*) 
(PUT 'PRI-NC* 'NUMBER-OF-ARGS 1) 
(PUT 'PRI-NC* 'DEFINED-ON-LINE '44) 
(PUT 'PRI-NC* 'DEFINED-IN-FILE 'NCPOLY/NCENV.RED) 
(PUT 'PRI-NC* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRI-NC* (U)
    ((LAMBDA (W) (PRIN2* (OR (AND W (CDR W)) U))) (ASSOC U NCPI-NAMES*))) 
(PUT 'DIPPRI-NC* 'NUMBER-OF-ARGS 1) 
(PUT 'DIPPRI-NC* 'DEFINED-ON-LINE '47) 
(PUT 'DIPPRI-NC* 'DEFINED-IN-FILE 'NCPOLY/NCENV.RED) 
(PUT 'DIPPRI-NC* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIPPRI-NC* (U)
    ((LAMBDA (W) (DIPPRIN2 (OR (AND W (CDR W)) U))) (ASSOC U NCPI-NAMES*))) 
(PUT 'NCPI-SETUP 'NUMBER-OF-ARGS 1) 
(PUT 'NCPI-SETUP 'DEFINED-ON-LINE '50) 
(PUT 'NCPI-SETUP 'DEFINED-IN-FILE 'NCPOLY/NCENV.RED) 
(PUT 'NCPI-SETUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NCPI-SETUP (U)
    (PROG (VARS AL B B0 F M RS RN NA RH LH S X Y W *EVALLHSEQP)
      (COND
       ((OR (SETQ W (MEMBER 'LEFT U)) (SETQ W (MEMBER 'RIGHT U)))
        (PROGN
         (SETQ U (DELETE (CAR W) U))
         (SETQ *NCG-RIGHT (EQUAL (CAR W) 'RIGHT)))))
      (COND ((GREATERP (LENGTH U) 2) (REDERR "illegal number of arguments")))
      (COND
       (NCPI-NAME-RULES*
        (AEVAL (CLEARRULES (LIST NCPI-COMM-RULES* NCPI-NAME-RULES*)))))
      (SETQ U (SUBLIS NCPI-NAMES* U))
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (CDR (LISTEVAL (CAR U) NIL)))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROGN
                             (SETQ X (REVAL1 X T))
                             (SETQ Y (LIST 'NC* (MKID '_ X)))
                             (SETQ NA (CONS (CONS Y X) NA))
                             (SETQ RN (CONS (LIST 'REPLACEBY X Y) RN))
                             (SETQ AL (CONS (CONS X Y) AL))
                             (SETQ VARS (APPEND VARS (LIST Y)))))
                          (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (X)
                    (PROGN
                     (SETQ X (REVAL1 X T))
                     (SETQ Y (LIST 'NC* (MKID '_ X)))
                     (SETQ NA (CONS (CONS Y X) NA))
                     (SETQ RN (CONS (LIST 'REPLACEBY X Y) RN))
                     (SETQ AL (CONS (CONS X Y) AL))
                     (SETQ VARS (APPEND VARS (LIST Y)))))
                  (CAR X))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (SETQ NCPI-NAMES* NA)
      (SETQ NCPI-NAME-RULES*
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST RN)))
      (SETQ M
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH VARS) 1) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J (PLUS I 1))
                          (COND
                           ((MINUSP (DIFFERENCE (LENGTH VARS) J))
                            (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           (CONS (NTH VARS I) (NTH VARS J))
                                           NIL)))
                         LOOPLABEL
                          (SETQ J (PLUS2 J 1))
                          (COND
                           ((MINUSP (DIFFERENCE (LENGTH VARS) J))
                            (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS (CONS (NTH VARS I) (NTH VARS J)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH VARS) 1) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J (PLUS I 1))
                          (COND
                           ((MINUSP (DIFFERENCE (LENGTH VARS) J))
                            (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           (CONS (NTH VARS I) (NTH VARS J))
                                           NIL)))
                         LOOPLABEL
                          (SETQ J (PLUS2 J 1))
                          (COND
                           ((MINUSP (DIFFERENCE (LENGTH VARS) J))
                            (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS (CONS (NTH VARS I) (NTH VARS J)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (COND
       ((CDR U)
        (SETQ NCPI-BRACKETS*
                (PROGN
                 (SETQ ALGLIST* (CONS NIL NIL))
                 (LISTEVAL (CADR U) NIL)))))
      (COND ((NULL NCPI-BRACKETS*) (REDERR "commutator relations missing")))
      (PROG (B)
        (SETQ B (CDR NCPI-BRACKETS*))
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ B0 (SUBLIS AL B))
            (SETQ W
                    (AND (EQCAR B0 'EQUAL) (SETQ LH (CADR B0))
                         (SETQ RH (CADDR B0)) (EQCAR LH 'DIFFERENCE)
                         (SETQ F (CADR LH)) (SETQ S (CADDR LH))
                         (EQCAR F 'TIMES) (EQCAR S 'TIMES) (SETQ X (CADR F))
                         (SETQ Y (CADDR F)) (MEMBER X VARS) (MEMBER Y VARS)
                         (EQUAL X (CADDR S)) (EQUAL Y (CADR S))))
            (COND ((NOT W) (TYPERR B "commutator relation")))
            (COND
             ((MEMBER (CONS X Y) M)
              (PROGN
               (SETQ W X)
               (SETQ X Y)
               (SETQ Y W)
               (SETQ RH (LIST 'MINUS RH)))))
            (SETQ M (DELETE (CONS Y X) M))
            (SETQ RS
                    (CONS
                     (LIST 'REPLACEBY (LIST 'TIMES X Y)
                           (LIST 'PLUS (LIST 'TIMES Y X) RH))
                     RS))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (NCDSETUP* (LIST (CONS 'LIST VARS) (CONS 'LIST RS)))
      (APPLY 'KORDER (LIST VARS))
      (APPLY 'ORDER (LIST VARS))
      (PROG (C)
        (SETQ C M)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (SETQ RS
                   (CONS
                    (LIST 'REPLACEBY (LIST 'TIMES (CDR C) (CAR C))
                          (LIST 'TIMES (CAR C) (CDR C)))
                    RS)))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (SETQ NCPI-COMM-RULES*
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST RS)))
      (AEVAL (LET (LIST NCPI-COMM-RULES* NCPI-NAME-RULES*))))) 
(PUT 'NC_SETUP 'PSOPFN 'NCPI-SETUP) 
(PUT 'NC_CLEANUP 'NUMBER-OF-ARGS 0) 
(PUT 'NC_CLEANUP 'DEFINED-ON-LINE '91) 
(PUT 'NC_CLEANUP 'DEFINED-IN-FILE 'NCPOLY/NCENV.RED) 
(PUT 'NC_CLEANUP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NC_CLEANUP NIL
    (PROGN
     (AEVAL (CLEARRULES (LIST NCPI-COMM-RULES* NCPI-NAME-RULES*)))
     (AEVAL (KORDER (LIST 'NIL)))
     (AEVAL (ORDER (LIST 'NIL))))) 
(PUT 'NC_CLEANUP 'STAT 'ENDSTAT) 
(ENDMODULE) 
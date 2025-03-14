(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RESTORE)) 
(FLUID '(PREFIXLIST)) 
(GLOBAL '(*VECTORC MALST OPTLANG*)) 
(PUT 'VECTORCODE 'NUMBER-OF-ARGS 1) 
(PUT 'VECTORCODE 'DEFINED-ON-LINE '32) 
(PUT 'VECTORCODE 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'VECTORCODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORCODE (LIST_OF_NAMES)
    (PROGN
     (FLAG LIST_OF_NAMES 'SUBSCRIPTED)
     (FLAG LIST_OF_NAMES 'VECTORVAR)
     NIL)) 
(PUT 'VECTORCODE 'STAT 'RLIS) 
(FLAG '(VECTORCODE) 'OPFN) 
(PUT 'VCLEAR 'NUMBER-OF-ARGS 1) 
(PUT 'VCLEAR 'DEFINED-ON-LINE '43) 
(PUT 'VCLEAR 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'VCLEAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VCLEAR (LIST_OF_NAMES)
    (PROGN
     (REMFLAG LIST_OF_NAMES 'SUBSCRIPTED)
     (REMFLAG LIST_OF_NAMES 'VECTORVAR)
     NIL)) 
(PUT 'VCLEAR 'STAT 'RLIS) 
(FLAG '(VCLEAR) 'OPFN) 
(PUT 'VECTORVARP 'NUMBER-OF-ARGS 1) 
(PUT 'VECTORVARP 'DEFINED-ON-LINE '53) 
(PUT 'VECTORVARP 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'VECTORVARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORVARP (U) (OR (AND *VECTORC (SUBSCRIPTEDVARP U)) (FLAGP U 'VECTORVAR))) 
(PUT 'OPTLANG 'NUMBER-OF-ARGS 1) 
(PUT 'OPTLANG 'DEFINED-ON-LINE '60) 
(PUT 'OPTLANG 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'OPTLANG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPTLANG (U)
    (COND
     ((NOT (MEMBER (CAR U) '(NIL C FORTRAN F90 PASCAL RATFOR)))
      (COND ((EQ (CAR U) 'FORTRAN90) (SETQ OPTLANG* 'F90))
            (T (REDERR "No such targetlanguage available !!!"))))
     (T (SETQ OPTLANG* (CAR U))))) 
(PUT 'OPTLANG 'STAT 'RLIS) 
(GLOBAL '(AVARLST)) 
(SETQ MALST (SETQ AVARLST 'NIL)) 
(PUT 'ALGRESULTS 'NUMBER-OF-ARGS 0) 
(PUT 'ALGRESULTS 'DEFINED-ON-LINE '71) 
(PUT 'ALGRESULTS 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'ALGRESULTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ALGRESULTS NIL (ALGRESULTS1 PREFIXLIST)) 
(PUT 'ALGRESULTS1 'NUMBER-OF-ARGS 1) 
(PUT 'ALGRESULTS1 'DEFINED-ON-LINE '74) 
(PUT 'ALGRESULTS1 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'ALGRESULTS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGRESULTS1 (PREFIXLIST)
    (PROG (RESULTS)
      (PROG (ITEM)
        (SETQ ITEM PREFIXLIST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (CHECK_INFO (CAR ITEM))
            (CHECK_INFO (CDR ITEM))
            (SETQ RESULTS
                    (CONS (LIST 'EQUAL (CAR ITEM) (REVAL1 (CDR ITEM) T))
                          RESULTS))
            NIL))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (COND
       (MALST
        (PROG (EL)
          (SETQ EL MALST)
         LAB
          (COND ((NULL EL) (RETURN NIL)))
          ((LAMBDA (EL) (PUT (CAR EL) 'SIMPFN 'SIMPIDEN)) (CAR EL))
          (SETQ EL (CDR EL))
          (GO LAB))))
      (RETURN (APPEND (LIST 'LIST) (REVERSE RESULTS))))) 
(FLAG '(ALGRESULTS) 'OPFN) 
(AEVAL (OPERATOR (LIST 'ARESULTS))) 
(AEVAL (LET '((EQUAL ARESULTS (ALGRESULTS))))) 
(PUT 'CHECK_INFO 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_INFO 'DEFINED-ON-LINE '99) 
(PUT 'CHECK_INFO 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'CHECK_INFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_INFO (INFO)
    (PROG (AVAL)
      (COND
       ((PAIRP INFO)
        (COND ((CONSTP INFO) INFO)
              (T
               (PROG (ITEM)
                 (SETQ ITEM INFO)
                LAB
                 (COND ((NULL ITEM) (RETURN NIL)))
                 ((LAMBDA (ITEM) (CHECK_INFO ITEM)) (CAR ITEM))
                 (SETQ ITEM (CDR ITEM))
                 (GO LAB)))))
       ((AND (IDP INFO) (NOT (MEMQ INFO AVARLST))
             (SETQ AVAL (GET INFO 'AVALUE)))
        (PROGN
         (PUT INFO 'A2VALUE AVAL)
         (REMPROP INFO 'AVALUE)
         (SETQ AVARLST (CONS INFO AVARLST))
         (COND
          ((MEMBER (GET INFO 'RTYPE) '(ARRAY MATRIX))
           (PROGN
            (SETQ MALST (CONS (CONS INFO (GET INFO 'RTYPE)) MALST))
            (REMPROP INFO 'RTYPE))))))))) 
(PUT 'ARESTORE 'NUMBER-OF-ARGS 1) 
(PUT 'ARESTORE 'DEFINED-ON-LINE '124) 
(PUT 'ARESTORE 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'ARESTORE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ARESTORE (LIST_OF_NAMES)
    (PROG (NAME)
      (SETQ NAME LIST_OF_NAMES)
     LAB
      (COND ((NULL NAME) (RETURN NIL)))
      ((LAMBDA (NAME)
         (PROGN
          (PUT NAME 'AVALUE (GET NAME 'A2VALUE))
          (REMPROP NAME 'A2VALUE)
          (SETQ AVARLST (DELETE NAME AVARLST))
          (COND
           ((ASSOC NAME MALST)
            (PROGN
             (PUT NAME 'RTYPE (CDR (ASSOC NAME MALST)))
             (REMKLIST NAME)
             (REMPROP NAME 'SIMPFN)
             (SETQ MALST (DELETE (ASSOC NAME MALST) MALST)))))))
       (CAR NAME))
      (SETQ NAME (CDR NAME))
      (GO LAB))) 
(PUT 'ARESTORE 'STAT 'RLIS) 
(FLAG '(ARESTORE) 'OPFN) 
(PUT 'RESTOREALL 'NUMBER-OF-ARGS 0) 
(PUT 'RESTOREALL 'DEFINED-ON-LINE '143) 
(PUT 'RESTOREALL 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'RESTOREALL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESTOREALL NIL (ARESTORE AVARLST)) 
(REMPROP 'RESTOREALL 'STAT) 
(FLAG '(RESTOREALL) 'OPFN) 
(PUT 'RESTOREALL 'STAT 'ENDSTAT) 
(PUT 'IREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'IREVAL 'DEFINED-ON-LINE '155) 
(PUT 'IREVAL 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'IREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IREVAL (EX)
    (PROG ()
      (CHECK_INFO EX)
      (COND ((ATOM EX) (RETURN EX))
            (T
             (RETURN
              (CONS (CAR EX)
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL (CDR EX))
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (REVAL1 EL T)) (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (EL) (REVAL1 EL T)) (CAR EL))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))))) 
(PUT 'IDS_TO_RESTORE 'NUMBER-OF-ARGS 0) 
(PUT 'IDS_TO_RESTORE 'DEFINED-ON-LINE '172) 
(PUT 'IDS_TO_RESTORE 'DEFINED-IN-FILE 'SCOPE/RESTORE.RED) 
(PUT 'IDS_TO_RESTORE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE IDS_TO_RESTORE NIL (APPEND (LIST 'LIST) AVARLST)) 
(FLAG '(IDS_TO_RESTORE) 'OPFN) 
(AEVAL (OPERATOR (LIST 'RESTORABLES))) 
(AEVAL (LET '((EQUAL RESTORABLES (IDS_TO_RESTORE))))) 
(ENDMODULE) 
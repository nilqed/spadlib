(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PM)) 
(CREATE-PACKAGE '(PM PMPATCH PATTDEFN PMINTRFC PATTPERM UNIFY PMRULES)
                '(CONTRIB PM)) 
(REMFLAG '(I) 'RESERVED) 
(REMPROP 'GAMMA 'SIMPFN) 
(FLAG '(AP) 'OPFN) 
(PUT 'AP 'NUMBER-OF-ARGS 2) 
(PUT 'AP 'DEFINED-ON-LINE '45) 
(PUT 'AP 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'AP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AP (F V)
    (COND ((NEQ (CAR V) 'LIST) (TYPERR V 'AP))
          ((NOT (GENEXP F))
           (COND ((ATOM F) (CONS F (CDR V))) (T (APPEND F (CDR V)))))
          (T
           (PROG (NV)
             (SETQ NV (IDSORT (UNION (FINDNEWVARS F) NIL)))
             (SETQ V (CDR V))
             (SETQ F (SUBLIS (NPAIR NV V) F))
             (COND
              ((LESSP (LENGTH NV) (LENGTH V))
               (SETQ F (APPEND F (PNTH V (PLUS (LENGTH NV) 1))))))
             (RETURN F))))) 
(PUT 'NPAIR 'NUMBER-OF-ARGS 2) 
(PUT 'NPAIR 'DEFINED-ON-LINE '59) 
(PUT 'NPAIR 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'NPAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NPAIR (U V)
    (COND ((AND U V) (CONS (CONS (CAR U) (CAR V)) (NPAIR (CDR U) (CDR V))))
          (T NIL))) 
(PUT 'MAP 'PSOPFN 'MAP0) 
(PUT 'MAP0 'NUMBER-OF-ARGS 1) 
(PUT 'MAP0 'DEFINED-ON-LINE '68) 
(PUT 'MAP0 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'MAP0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAP0 (ARG)
    (COND ((LESSP (LENGTH ARG) 2) NIL)
          (T
           (MAP1 (CAR ARG) (CADR ARG)
            (COND ((GEQ (LENGTH ARG) 3) (CADDR ARG)) (T 1)))))) 
(PUT 'MAP1 'NUMBER-OF-ARGS 3) 
(PUT 'MAP1 'DEFINED-ON-LINE '72) 
(PUT 'MAP1 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'MAP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAP1 (FN V DEP)
    (COND
     ((GREATERP DEP 0)
      (CONS (CAR V)
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR V))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (MAP1 FN J (DIFFERENCE DEP 1)))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (MAP1 FN J (DIFFERENCE DEP 1))) (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T
      (AP FN (COND ((OR (ATOM V) (NEQ (CAR V) 'LIST)) (LIST 'LIST V)) (T V)))))) 
(PUT 'AR 'PSOPFN 'AR0) 
(PUT 'AR0 'NUMBER-OF-ARGS 1) 
(PUT 'AR0 'DEFINED-ON-LINE '80) 
(PUT 'AR0 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'AR0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AR0 (ARG)
    (COND ((LEQ (LENGTH ARG) 1) NIL)
          (T
           (AR1 (CAR ARG) (COND ((GEQ (LENGTH ARG) 2) (CADR ARG)) (T 'LIST)))))) 
(PUT 'AR1 'NUMBER-OF-ARGS 2) 
(PUT 'AR1 'DEFINED-ON-LINE '84) 
(PUT 'AR1 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'AR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AR1 (ARG FN)
    (COND ((FIXP ARG) (AR4 (LIST (LIST 1 ARG 1)) FN))
          ((OR (ATOM ARG) (NEQ (CAR ARG) 'LIST)) (TYPERR ARG 'AR))
          (T
           (AR4
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR ARG))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (AARG J)) (CAR J)) NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (AARG J)) (CAR J)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            FN)))) 
(PUT 'AARG 'NUMBER-OF-ARGS 1) 
(PUT 'AARG 'DEFINED-ON-LINE '89) 
(PUT 'AARG 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'AARG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AARG (ARG)
    (REVLIS
     (COND ((OR (FIXP ARG) (GENP ARG)) (LIST 1 ARG 1))
           ((OR (ATOM ARG) (NEQ (CAR ARG) 'LIST)) (TYPERR ARG 'AR))
           (T
            (PROG (L)
              (SETQ ARG (CDR ARG))
              (SETQ L (LENGTH ARG))
              (RETURN
               (COND ((EQUAL L 1) (LIST 1 (CAR ARG) 1))
                     ((EQUAL L 2) (LIST (CAR ARG) (CADR ARG) 1))
                     ((EQUAL L 3) (LIST (CAR ARG) (CADR ARG) (CADDR ARG)))
                     (T (TYPERR ARG "Ar"))))))))) 
(PUT 'AR4 'NUMBER-OF-ARGS 2) 
(PUT 'AR4 'DEFINED-ON-LINE '102) 
(PUT 'AR4 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'AR4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AR4 (LST FN)
    (PROG (S U V W)
      (SETQ U (CAAR LST))
      (SETQ V (CADAR LST))
      (SETQ W (CADDAR LST))
      (SETQ LST (CDR LST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ U V)) (RETURN NIL)))
        (PROGN (SETQ S (APPEND S (LIST U))) (SETQ U (PLUS U W)))
        (GO WHILELABEL))
      (RETURN
       (COND
        ((EQUAL (LENGTH LST) 0)
         (COND ((EQ FN 'LIST) (CONS 'LIST S)) (T (MAP1 FN (CONS 'LIST S) 1))))
        (T
         (CONS 'LIST
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDR (MAP1 (LIST LST FN) (CONS 'LIST S) 1)))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J) (AR4 (CAR J) (CDR J))) (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (AR4 (CAR J) (CDR J))) (CAR J))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'CAT 'PSOPFN 'CATX) 
(PUT 'CATX 'NUMBER-OF-ARGS 1) 
(PUT 'CATX 'DEFINED-ON-LINE '117) 
(PUT 'CATX 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'CATX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CATX (U)
    ((LAMBDA (X Y)
       (COND ((NOT (EQCAR X 'LIST)) (TYPERR (CAR U) "list"))
             ((NOT (EQCAR Y 'LIST)) (TYPERR (CADR U) "list"))
             (T (CONS 'LIST (APPEND (CDR X) (CDR Y))))))
     (REVAL1 (CAR U) T) (REVAL1 (CADR U) T))) 
(PUT 'SIMPEQ 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPEQ 'DEFINED-ON-LINE '127) 
(PUT 'SIMPEQ 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'SIMPEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPEQ (ARG)
    (PROG (X)
      (COND ((LESSP (LENGTH ARG) 2) (TYPERR (CONS 'EQUAL ARG) "relation")))
      (SETQ ARG (REVAL1 (CONS 'DIFFERENCE ARG) T))
      (SETQ ARG
              (COND ((NUMBERP ARG) (REVAL1 (EQUAL ARG 0) T))
                    (T
                     (PROGN
                      (SETQ ARG (LIST 'EQUAL ARG 0))
                      (COND ((SETQ X (OPMTCH ARG)) X) (T ARG))))))
      (RETURN (MKSQ ARG 1)))) 
(PUT 'SIMPGT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGT 'DEFINED-ON-LINE '137) 
(PUT 'SIMPGT 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'SIMPGT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGT (ARG)
    (PROG (X)
      (COND ((LESSP (LENGTH ARG) 2) (TYPERR (CONS 'GREATERP ARG) "relation")))
      (SETQ ARG (REVAL1 (CONS 'DIFFERENCE ARG) T))
      (SETQ ARG
              (COND ((NUMBERP ARG) (REVAL1 (GREATERP ARG 0) T))
                    (T
                     (PROGN
                      (SETQ ARG (LIST 'GREATERP ARG 0))
                      (COND ((SETQ X (OPMTCH ARG)) X) (T ARG))))))
      (RETURN (MKSQ ARG 1)))) 
(PUT 'SIMPGE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGE 'DEFINED-ON-LINE '147) 
(PUT 'SIMPGE 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'SIMPGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGE (ARG)
    (PROG (X)
      (COND ((LESSP (LENGTH ARG) 2) (TYPERR (CONS 'GEQ ARG) "relation")))
      (SETQ ARG (REVAL1 (CONS 'DIFFERENCE ARG) T))
      (SETQ ARG
              (COND ((NUMBERP ARG) (REVAL1 (GEQ ARG 0) T))
                    (T
                     (PROGN
                      (SETQ ARG (LIST 'GEQ ARG 0))
                      (COND ((SETQ X (OPMTCH ARG)) X) (T ARG))))))
      (RETURN (MKSQ ARG 1)))) 
(PUT 'SIMPLT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLT 'DEFINED-ON-LINE '157) 
(PUT 'SIMPLT 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'SIMPLT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLT (ARG) (SIMPGT (LIST (CADR ARG) (CAR ARG)))) 
(PUT 'SIMPLE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLE 'DEFINED-ON-LINE '160) 
(PUT 'SIMPLE 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'SIMPLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLE (ARG) (SIMPGE (LIST (CADR ARG) (CAR ARG)))) 
(PUT 'EQUAL 'SIMPFN 'SIMPEQ) 
(PUT 'GREATERP 'SIMPFN 'SIMPGT) 
(PUT 'GEQ 'SIMPFN 'SIMPGE) 
(PUT 'LESSP 'SIMPFN 'SIMPLT) 
(PUT 'LEQ 'SIMPFN 'SIMPLE) 
(PUT 'FORMGEN 'NUMBER-OF-ARGS 3) 
(PUT 'FORMGEN 'DEFINED-ON-LINE '176) 
(PUT 'FORMGEN 'DEFINED-IN-FILE 'PM/PM.RED) 
(PUT 'FORMGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMGEN (U VARS MODE)
    (PROG (X)
      (SETQ U (CADR U))
      (COND
       ((ATOM U)
        (COND
         ((EQ U '?)
          (PROGN
           (SETQ U (INTERN '??))
           (SETQ X (LIST (MKQUOTE U) (MKQUOTE 'MGEN) T))))
         (T
          (PROGN
           (SETQ U (INTERN (COMPRESS (CONS '! (CONS '? (EXPLODE U))))))
           (SETQ X (LIST (MKQUOTE U) (MKQUOTE 'GEN) T))))))
       ((NEQ (CAR U) '?)
        (PROGN
         (SETQ U
                 (CONS
                  (INTERN (COMPRESS (CONS '! (CONS '? (EXPLODE (CAR U))))))
                  (CDR U)))
         (SETQ X (LIST (MKQUOTE (CAR U)) (MKQUOTE 'GEN) T))))
       ((AND (EQ (CAR U) '?) (ATOM (CADR U)))
        (PROGN
         (SETQ U
                 (INTERN
                  (COMPRESS
                   (CONS '!
                         (CONS '? (CONS '! (CONS '? (EXPLODE (CADR U)))))))))
         (SETQ X (LIST (MKQUOTE U) (MKQUOTE 'MGEN) T))))
       (T
        (PROGN
         (SETQ U (CADR U))
         (SETQ U
                 (CONS
                  (INTERN
                   (COMPRESS
                    (CONS '! (CONS '? (CONS '! (CONS '? (EXPLODE (CAR U))))))))
                  (CDR U)))
         (SETQ X (LIST (MKQUOTE (CAR U)) (MKQUOTE 'GEN) T)))))
      (RETURN (LIST 'PROGN (CONS 'PUT X) (FORM1 U VARS MODE))))) 
(PUT '? 'FORMFN 'FORMGEN) 
(ENDMODULE) 
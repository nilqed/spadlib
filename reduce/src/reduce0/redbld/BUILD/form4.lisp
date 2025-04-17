(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FORM4)) 
(FLUID '(*SPECIFICATION *SPECIFICATION_REDUCE *GENERATE_RETRACTS *INSTANTIATE)) 
(SWITCH (LIST 'INSTANTIATE 'SPECIFICATION)) 
(PUT 'N_FORM 'NUMBER-OF-ARGS 1) 
(PUT 'N_FORM 'DEFINED-ON-LINE '45) 
(PUT 'N_FORM 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE N_FORM (U) (CADR (N_FORM1 U *VARS*))) 
(PUT 'N_FORM1 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORM1 'DEFINED-ON-LINE '49) 
(PUT 'N_FORM1 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORM1 (U VARS)
    (PROG (X Z CTYPE ARITY_PAIRS ARGS FNC)
      (RETURN
       (COND
        ((ATOM U)
         (COND
          ((NUMBERP U)
           (COND
            ((FIXP U)
             (COND
              ((EQUAL U 0)
               (PROGN
                (SETQ Z (PCKG_TYPE 'ZERO))
                (LIST Z (MKQUOTE (LIST Z 0)))))
              (T
               (PROGN
                (SETQ Z (PCKG_TYPE 'INT))
                (LIST Z (MKQUOTE (TYPE_REDUCE U Z)))))))
            (T (LIST 'FLOAT (MKQUOTE (LIST 'FLOAT U))))))
          ((STRINGP U) (LIST 'STRING (MKQUOTE (LIST 'STRING U))))
          ((ARRAYP U) (LIST 'ARRAY (MKQUOTE (LIST 'ARRAY U))))
          ((SETQ X (ATSOC U VARS)) (LIST (CDR X) U))
          ((SETQ X (GET U 'AVALUE)) (LIST (TYPE X) (LIST 'IDEVAL (MKQUOTE U))))
          (T
           (LIST (SETQ X (PCKG_TYPE 'VARIABLE)) (LIST 'IDEVAL (MKQUOTE U))))))
        ((NOT (IDP (CAR U))) (TYPERR (CAR U) "operator"))
        ((OR (AND (NULL (CDR U)) (NEQ (CAR U) 'LIST))
             (FLAGP (CAR U) 'NON_FORM))
         (LIST 'NON_FORM U))
        ((FLAGP (CAR U) 'NON_FORM) (LIST 'NON_FORM U))
        ((SETQ X (GET (CAR U) 'N_FORMFN)) (APPLY2 X U VARS))
        ((SETQ X (GET (CAR U) 'XFORM))
         (CONS X
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDR U))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J))
                                       NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        (T
         (PROGN
          (SETQ X
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J (CDR U))
                    (COND ((NULL J) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J))
                                     NIL)))
                   LOOPLABEL
                    (SETQ J (CDR J))
                    (COND ((NULL J) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ ARITY_PAIRS
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J X)
                    (COND ((NULL J) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (J) (LIST (TYPE J))) (CAR J))
                                     NIL)))
                   LOOPLABEL
                    (SETQ J (CDR J))
                    (COND ((NULL J) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (J) (LIST (TYPE J))) (CAR J)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ ARGS
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J X)
                    (COND ((NULL J) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (VALUE J)) (CAR J))
                                          NIL)))
                   LOOPLABEL
                    (SETQ J (CDR J))
                    (COND ((NULL J) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (J) (VALUE J)) (CAR J)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ FNC (TYPE_FUNCTION2 (CAR U) ARITY_PAIRS ARGS))
          (SETQ CTYPE
                  (COND
                   ((NULL FNC)
                    (COND
                     (*SPECIFICATION
                      (REDERR (LIST "no meaning for" (CAR U) X)))
                     (T 'GENERIC)))
                   (T (CADR FNC))))
          (COND
           (*GENERATE_RETRACTS
            (SETQ ARGS
                    (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ARG ARGS)
                      (COND ((NULL ARG) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ARG)
                                          (PROGN
                                           (SETQ X ARITY_PAIRS)
                                           (SETQ ARITY_PAIRS (CDR ARITY_PAIRS))
                                           (COND
                                            ((CDAR X)
                                             (MKRETRACT (CAAR X) (CADAR X)
                                              ARG))
                                            (T ARG))))
                                        (CAR ARG))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ARG (CDR ARG))
                      (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ARG)
                                  (PROGN
                                   (SETQ X ARITY_PAIRS)
                                   (SETQ ARITY_PAIRS (CDR ARITY_PAIRS))
                                   (COND
                                    ((CDAR X)
                                     (MKRETRACT (CAAR X) (CADAR X) ARG))
                                    (T ARG))))
                                (CAR ARG))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
          (COND
           ((AND *INSTANTIATE FNC)
            (LIST CTYPE (LIST 'TYPE_REDUCE1 (CONS (CAR FNC) ARGS))))
           (T
            (LIST CTYPE
                  (LIST 'RAPPLY (MKQUOTE (CAR U)) (CONS 'LIST ARGS))))))))))) 
(PUT 'MK_TYPE_REDUCE 'NUMBER-OF-ARGS 2) 
(PUT 'MK_TYPE_REDUCE 'DEFINED-ON-LINE '106) 
(PUT 'MK_TYPE_REDUCE 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'MK_TYPE_REDUCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_TYPE_REDUCE (U V)
    (COND ((FLAGP V 'DEFINING) (MKQUOTE (LIST V U)))
          (T (LIST 'TYPE_REDUCE U (MKQUOTE V))))) 
(PUT 'TYPE_FUNCTION2 'NUMBER-OF-ARGS 3) 
(PUT 'TYPE_FUNCTION2 'DEFINED-ON-LINE '112) 
(PUT 'TYPE_FUNCTION2 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'TYPE_FUNCTION2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TYPE_FUNCTION2 (FN TYPELIST ARGS)
    (PROG (X)
      (RETURN
       (COND
        ((AND (SETQ X (GET FN 'RANKS)) (SETQ X (ASSOC (LENGTH TYPELIST) X))
              (SETQ X (TYPE_ASSOCF TYPELIST (CDR X) ARGS)))
         X)
        (T NIL))))) 
(PUT 'TYPE_ASSOCF 'NUMBER-OF-ARGS 3) 
(PUT 'TYPE_ASSOCF 'DEFINED-ON-LINE '126) 
(PUT 'TYPE_ASSOCF 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'TYPE_ASSOCF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TYPE_ASSOCF (TYPELIST TYPE_ASSOC_LIST ARGS)
    (PROG (X)
      (COND
       ((SETQ X
                (TYPE_ASSOC1F (CAR TYPELIST) (CDR TYPELIST) TYPE_ASSOC_LIST
                 ARGS))
        (RETURN X))
       ((SETQ X (ATSOC 'GENERIC TYPE_ASSOC_LIST)) (RETURN (CDR X)))
       (T (RETURN NIL))))) 
(PUT 'TYPE_ASSOC1F 'NUMBER-OF-ARGS 4) 
(PUT 'TYPE_ASSOC1F 'DEFINED-ON-LINE '138) 
(PUT 'TYPE_ASSOC1F 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'TYPE_ASSOC1F 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TYPE_ASSOC1F (TYPE TYPELIST TYPE_ASSOC_LIST ARGS)
    (PROG (X Y Z)
      (COND
       ((AND (TYPE_IN_PCKGP TYPE) (SETQ X (TYPE_ASSOC0F TYPE TYPE_ASSOC_LIST)))
        (COND
         ((NULL TYPELIST) (RETURN (CEILING_OF_CONSTRAINTS (CDADDR (CDR X)))))
         ((SETQ Y (TYPE_ASSOC1F (CAR TYPELIST) (CDR TYPELIST) (CDR X) ARGS))
          (RETURN Y)))))
      (COND
       ((SETQ Z (GET (CAR TYPE) 'UPTREE))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND Z
                   (NOT
                    (SETQ X
                            (TYPE_ASSOC1F (RPLACA TYPE (CAR Z)) TYPELIST
                             TYPE_ASSOC_LIST ARGS)))))
             (RETURN NIL)))
           (SETQ Z (CDR Z))
           (GO WHILELABEL))
         (RETURN X)))
       (T (RETURN NIL))))) 
(PUT 'CEILING_OF_CONSTRAINTS 'NUMBER-OF-ARGS 1) 
(PUT 'CEILING_OF_CONSTRAINTS 'DEFINED-ON-LINE '156) 
(PUT 'CEILING_OF_CONSTRAINTS 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'CEILING_OF_CONSTRAINTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEILING_OF_CONSTRAINTS (U)
    (COND ((NULL U) NIL) ((EQ (CAAR U) T) (CADADR (CAR U)))
          (T (CEILING_OF_CONSTRAINTS (CDR U))))) 
(PUT 'TYPE_ASSOC0F 'NUMBER-OF-ARGS 2) 
(PUT 'TYPE_ASSOC0F 'DEFINED-ON-LINE '161) 
(PUT 'TYPE_ASSOC0F 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'TYPE_ASSOC0F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPE_ASSOC0F (TYPE TYPE_ASSOC_LIST)
    (COND ((NULL TYPE_ASSOC_LIST) NIL)
          ((EQ (CAR TYPE) (CAAR TYPE_ASSOC_LIST)) (CAR TYPE_ASSOC_LIST))
          ((AND (XTYPE1 (CAAR TYPE_ASSOC_LIST) (CAR TYPE)) *SPECIFICATION)
           (PROGN
            (LPRIM (LIST TYPE " -> " (CAAR TYPE_ASSOC_LIST)))
            (CAR TYPE_ASSOC_LIST)
            (RPLACD TYPE (LIST (CAAR TYPE_ASSOC_LIST)))
            (CAR TYPE_ASSOC_LIST)))
          (T (TYPE_ASSOC0F TYPE (CDR TYPE_ASSOC_LIST))))) 
(FLAG '(LOAD) 'NON_FORM) 
(PUT 'TYPE 'XFORM 'TYPE_1) 
(PUT 'TYPE_1 'NUMBER-OF-ARGS 1) 
(PUT 'TYPE_1 'DEFINED-ON-LINE '177) 
(PUT 'TYPE_1 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'TYPE_1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPE_1 (U) (LIST 'VARIABLE (TYPE U))) 
(PUT 'N_FORMBOOL 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMBOOL 'DEFINED-ON-LINE '179) 
(PUT 'N_FORMBOOL 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMBOOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMBOOL (U VARS)
    (PROG (X)
      (COND
       ((ATOM U)
        (COND ((EQ U 'T) (RETURN (LIST 'BOOL U)))
              ((SETQ X (ATSOC U VARS))
               (COND
                ((OR (EQ (CDR X) 'BOOL) (EQ (CDR X) 'GENERIC))
                 (RETURN (LIST 'BOOL (LIST 'N_BOOLVALUE* U))))
                (T
                 (REDERR
                  (LIST "a boolean was expected, but got" (CDR X)))))))))
      (SETQ X (N_FORM1 U VARS))
      (COND
       ((NULL (OR (EQ (TYPE X) 'BOOL) (EQ (TYPE X) 'GENERIC)))
        (REDERR (LIST "a boolean was expected, but got" (TYPE X)))))
      (RETURN (LIST 'BOOL (LIST 'N_BOOLVALUE* (VALUE X)))))) 
(PUT 'N_BOOLVALUE* 'NUMBER-OF-ARGS 1) 
(PUT 'N_BOOLVALUE* 'DEFINED-ON-LINE '194) 
(PUT 'N_BOOLVALUE* 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_BOOLVALUE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE N_BOOLVALUE* (U) ((LAMBDA (V) (AND V (NULL (EQUAL V 0)))) (VALUE U))) 
(PUT 'N_FORMCOND 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMCOND 'DEFINED-ON-LINE '198) 
(PUT 'N_FORMCOND 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMCOND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMCOND (U VARS)
    ((LAMBDA (X) (LIST (TYPE X) (CONS 'COND (VALUE X))))
     (N_FORMCOND1 (CDR U) VARS))) 
(PUT 'N_FORMCOND1 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMCOND1 'DEFINED-ON-LINE '201) 
(PUT 'N_FORMCOND1 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMCOND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMCOND1 (U VARS)
    (PROG (V EPTR X RESTYPE)
      (SETQ V (SETQ EPTR (LIST NIL)))
     A
      (COND ((NULL U) (RETURN (LIST RESTYPE (CDR V)))))
      (SETQ X (N_FORM1 (CADAR U) VARS))
      (COND ((NULL RESTYPE) (SETQ RESTYPE (TYPE X)))
            ((XTYPE1 (TYPE X) RESTYPE) NIL)
            ((XTYPE1 RESTYPE (TYPE X)) (SETQ RESTYPE (TYPE X)))
            (T
             (REDERR
              (LIST "types in conditional" (TYPE X) "and" RESTYPE
                    "are unrelated"))))
      (SETQ EPTR
              (CDR
               (RPLACD EPTR
                       (LIST
                        (LIST (VALUE (N_FORMBOOL (CAAR U) VARS)) (VALUE X))))))
      (SETQ U (CDR U))
      (GO A))) 
(PUT 'COND 'N_FORMFN 'N_FORMCOND) 
(PUT 'N_FORMLIST 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMLIST 'DEFINED-ON-LINE '223) 
(PUT 'N_FORMLIST 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMLIST (U VARS)
    (PROG (X Y ELTYPE)
      (COND ((NULL (CDR U)) (RETURN (LIST 'EMPTY_LIST ''(EMPTY_LIST NIL)))))
      (SETQ X (N_FORM1 (CADR U) VARS))
      (SETQ ELTYPE (TYPE X))
      (SETQ Y (VALUE X))
      (SETQ Y
              (CONS Y
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDDR U))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (PROGN
                                           (SETQ X (N_FORM1 J VARS))
                                           (VALUE X)))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (PROGN (SETQ X (N_FORM1 J VARS)) (VALUE X)))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN (LIST 'NON_EMPTY_LIST (LIST 'MKLISTT (CONS 'LIST Y)))))) 
(PUT 'MKLISTT 'NUMBER-OF-ARGS 1) 
(PUT 'MKLISTT 'DEFINED-ON-LINE '240) 
(PUT 'MKLISTT 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'MKLISTT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKLISTT (U) (TYPE_REDUCE U 'LIST)) 
(PUT 'LIST 'N_FORMFN 'N_FORMLIST) 
(PUT 'N_FORMPROGN 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMPROGN 'DEFINED-ON-LINE '248) 
(PUT 'N_FORMPROGN 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMPROGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMPROGN (U VARS)
    (PROG (RESTYPE X)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CDR U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ RESTYPE (N_FORM1 J VARS))
                                     (VALUE RESTYPE)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ RESTYPE (N_FORM1 J VARS))
                             (VALUE RESTYPE)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST (TYPE RESTYPE) (CONS 'PROGN X))))) 
(PUT 'PROGN 'N_FORMFN 'N_FORMPROGN) 
(PUT 'N_FORMSETQ 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMSETQ 'DEFINED-ON-LINE '260) 
(PUT 'N_FORMSETQ 'DEFINED-IN-FILE 'REDUCE4/FORM4.RED) 
(PUT 'N_FORMSETQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMSETQ (U VARS)
    (PROG (X Y Z)
      (SETQ Z (N_FORM1 (CADDR U) VARS))
      (COND
       ((AND (IDP (CADR U)) (SETQ X (ATSOC (CADR U) VARS)))
        (PROGN
         (COND
          ((NOT (EQ (CDR X) 'GENERIC))
           (COND ((XTYPE1 (TYPE Z) (CDR X)) NIL)
                 ((XTYPE1 (CDR X) (TYPE Z))
                  (LPRIM
                   (LIST "assignment is only valid if type of rhs" (TYPE Z)
                         "is retractable to" (CDR X))))
                 (T
                  (REDERR
                   (LIST "type of lhs" (CDR X)
                         "in assignment is unrelated to ceiling type" (TYPE Z)
                         "of rhs"))))))
         (RETURN (LIST (CAR Z) (LIST 'SETQ (CADR U) (CADR Z))))))
       ((AND (NOT (ATOM (CADR U))) (SETQ X (GETOBJECT (CAADR U)))
             (SETQ Y (GET (TYPE X) 'PUTFN)))
        (RETURN
         (LIST (CAR Z)
               (LIST Y (MKQUOTE X)
                     (CONS 'LIST
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J (CDADR U))
                             (COND ((NULL J) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (J)
                                                 (CADR (N_FORM1 J VARS)))
                                               (CAR J))
                                              NIL)))
                            LOOPLABEL
                             (SETQ J (CDR J))
                             (COND ((NULL J) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (J) (CADR (N_FORM1 J VARS)))
                                       (CAR J))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                     (CADR Z)))))
       (T
        (RETURN
         (LIST (CAR Z)
               (LIST 'RAPPLY (MKQUOTE 'SETQ)
                     (LIST 'LIST
                           (LIST 'MKOBJECT (MKQUOTE (CADR U))
                                 (MKQUOTE 'VARIABLE))
                           (CADR Z))))))))) 
(PUT 'SETQ 'N_FORMFN 'N_FORMSETQ) 
(ENDMODULE) 
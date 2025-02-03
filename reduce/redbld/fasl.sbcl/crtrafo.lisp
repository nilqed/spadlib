(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRANSFORM)) 
(PUT 'INPUT_TRAFO 'NUMBER-OF-ARGS 1) 
(PUT 'INPUT_TRAFO 'DEFINED-ON-LINE '34) 
(PUT 'INPUT_TRAFO 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'INPUT_TRAFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INPUT_TRAFO (PDES)
    (PROG (ULIST VLIST U V YLIST XLIST YSLIST XSLIST XL2 NOTALLOWED
           FULL_SIMPLIFY SINGUL H VL_BAK YPLIST XPLIST)
      (CHANGE_PROMPT_TO "")
      (PROG (H)
        (SETQ H PDES)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ YPLIST (UNION (GET H 'FCTS) YPLIST))
            (SETQ XPLIST (UNION (GET H 'VARS) XPLIST))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (PROGN
       (PRIN2 "Under the following conditions this program performs arbitrary")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "transformations.") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN
       (PRIN2
        "If not only variables but also functions are transformed then it")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "is assumed that all new functions depend on the same new variables")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "and that all old functions depend on the same old variables.")
       NIL)
      (TERPRI)
      (TERPRI)
      (PROGN
       (PRIN2
        "For these procedures to be applicable the old functions and variables")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "must be given in terms of the new ones explicitly, i.e. not involving")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "unspecified functions of the new ones. Also, the differential ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "equations to be transformed must contain the old independent and")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "dependent variables and their partial derivatives explicitly.")
       NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 "The old (i.e. current) list of functions is ") NIL)
      (TERPRI)
      (FCTPRINT_SQ YPLIST)
      (TERPRI)
      (PROGN (PRIN2 "and the list of old (i.e. current) variables is ") NIL)
      (TERPRI)
      (FCTPRINT XPLIST)
      (TERPRI)
      (TERPRI)
      (PROGN
       (PRIN2 "Give a list of new functions (i.e. dependent variables), ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "e.g. `u1,u2,u3;' in the order to be used to sort dervatives. ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Use new function names which have not been used before in this")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "computation. If there are no new functions enter ;") NIL)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ ULIST (TERMLISTREAD))
         (SETQ H ULIST)
         (COND
          (H
           (COND
            ((NEQ (LENGTH ULIST) (LENGTH YPLIST))
             (PROGN
              (PROGN
               (PRIN2 "If you specify new functions then you need as many as ")
               (PRIN2 (LENGTH YPLIST))
               (PRIN2 ".")
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "Please try again.") NIL)
              (TERPRI)))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND H (ATOM (CAR H)) (FREEOF DONE_TRAFO (CAR H))
                        (FREEOF FTEM_ (CAR H)) (FREEOF VL_ (CAR H))))
                  (RETURN NIL)))
                (SETQ H (CDR H))
                (GO WHILELABEL))
              (COND
               (H
                (PROGN
                 (COND
                  ((NOT (ATOM (CAR H)))
                   (PROGN
                    (PRIN2 "The function ")
                    (PRIN2 (CAR H))
                    (PRIN2 " is not an identifier.")
                    NIL))
                  (T
                   (PROGN
                    (PRIN2 "The function name ")
                    (PRIN2 (CAR H))
                    (PRIN2 " is already in use.")
                    NIL)))
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "Please try again and choose a different function name.")
                  NIL)
                 (TERPRI)))
               (T
                (PROGN
                 (SETQ H ULIST)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND H (FREEOF (CDR H) (CAR H)))) (RETURN NIL)))
                   (SETQ H (CDR H))
                   (GO WHILELABEL))
                 (COND
                  (H
                   (PROGN
                    (PROGN
                     (PRIN2 "The function ")
                     (PRIN2 (CAR H))
                     (PRIN2 " is listed more than once. Please try again.")
                     NIL)
                    (TERPRI)))))))))))))
        (COND ((NOT (NULL H)) (GO REPEATLABEL))))
      (TERPRI)
      (PROGN
       (PRIN2
        "Give a list of all new variables (i.e. independent variables), ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "e.g. 'v1,v2,v3;' in the order to be used to sort derivatives.")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "If there are no new variables enter ;") NIL)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ VLIST (TERMLISTREAD))
         (SETQ H VLIST)
         (COND
          (H
           (COND
            ((NEQ (LENGTH VLIST) (LENGTH XPLIST))
             (PROGN
              (PROGN
               (PRIN2 "If you specify new variables then you need as many as ")
               (PRIN2 (LENGTH XPLIST))
               (PRIN2 ".")
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "Please try again.") NIL)
              (TERPRI)))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND H (ATOM (CAR H)) (FREEOF DONE_TRAFO (CAR H))
                        (FREEOF FTEM_ (CAR H)) (FREEOF VL_ (CAR H))))
                  (RETURN NIL)))
                (SETQ H (CDR H))
                (GO WHILELABEL))
              (COND
               (H
                (PROGN
                 (COND
                  ((NOT (ATOM (CAR H)))
                   (PROGN
                    (PRIN2 "The variable ")
                    (PRIN2 (CAR H))
                    (PRIN2 " is not an identifier.")
                    NIL))
                  (T
                   (PROGN
                    (PRIN2 "The variable name ")
                    (PRIN2 (CAR H))
                    (PRIN2 " is already in use.")
                    NIL)))
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "Please try again and choose a different variable name.")
                  NIL)
                 (TERPRI)))
               (T
                (PROGN
                 (SETQ H VLIST)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND H (FREEOF (CDR H) (CAR H)))) (RETURN NIL)))
                   (SETQ H (CDR H))
                   (GO WHILELABEL))
                 (COND
                  (H
                   (PROGN
                    (PROGN
                     (PRIN2 "The function ")
                     (PRIN2 (CAR H))
                     (PRIN2 " is listed more than once. Please try again.")
                     NIL)
                    (TERPRI)))))))))))))
        (COND ((NOT (NULL H)) (GO REPEATLABEL))))
      (SETQ VL_BAK VL_)
      (SETQ VL_ (UNION VL_ VLIST))
      (COND
       (ULIST
        (PROGN
         (PROG (U)
           (SETQ U ULIST)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (COND
               (VLIST
                (PROG (V)
                  (SETQ V VLIST)
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V) (DEPEND (LIST U V))) (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB)))
               (T
                (PROG (V)
                  (SETQ V VL_)
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V) (DEPEND (LIST U V))) (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB)))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (TERPRI)
         (PROGN
          (PRIN2
           "Give a list of all substitutions of old functions in terms of")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "new functions and new variables, e.g. y1=2*u1+u2*v2, y2=u2-u1*v1;")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "If there are no substitutions of old functions enter ;")
          NIL)
         (TERPRI)
         (SETQ YSLIST (TERMLISTREAD))
         (SETQ YLIST
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U YSLIST)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (U) (CADR U)) (CAR U)) NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (U) (CADR U)) (CAR U)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ XLIST (FCTARGS (CAR YLIST)))
         (PROG (U)
           (SETQ U (CDR YLIST))
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (PROGN
               (SETQ XL2 (FCTARGS U))
               (COND
                ((OR (NOT_INCLUDED XLIST XL2) (NOT_INCLUDED XL2 XLIST))
                 (PROGN
                  (SETQ NOTALLOWED T)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "Functions ")
                   (PRIN2 (CAR YLIST))
                   (PRIN2 ",")
                   (PRIN2 U)
                   (PRIN2 " do not depend on the same variables!")
                   NIL)
                  NIL)))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (PROG (U)
           (SETQ U YSLIST)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (COND
               ((SETQ V (ZERO_DEN (REVAL1 (CADDR U) T) (UNION FTEM_ ULIST)))
                (SETQ SINGUL (UNION V SINGUL)))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         NIL)))
      (COND (NOTALLOWED (RETURN NIL)))
      (COND
       (VLIST
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2
           "Give a list of all substitutions of old variables in terms of")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "new functions and new variables, e.g. x1=v1-v2*u2, x2=3*v2+v1*u1;")
          NIL)
         (TERPRI)
         (SETQ XSLIST (TERMLISTREAD))
         (PROG (U)
           (SETQ U XSLIST)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (COND
               ((SETQ V (ZERO_DEN (REVAL1 (CADDR U) T) (UNION FTEM_ ULIST)))
                (SETQ SINGUL (UNION V SINGUL)))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (COND
          ((NEQ (LENGTH VLIST) (LENGTH XSLIST))
           (PROGN
            (PROGN
             (PRIN2 "The number of substitutions is not ")
             (PRIN2 (LENGTH VLIST))
             NIL)
            (SETQ NOTALLOWED T)
            NIL)))
         (SETQ XLIST
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V XSLIST)
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (CADR V)) (CAR V)) NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (CADR V)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((OR (NOT_INCLUDED XPLIST XLIST) (NOT_INCLUDED XLIST XPLIST))
           (PROGN
            (PROGN
             (PRIN2
              "The set of left hand side variables is not the set of old variables.")
             NIL)
            (SETQ NOTALLOWED T)
            NIL)))
         (PROG (V)
           (SETQ V XSLIST)
          LAB
           (COND ((NULL V) (RETURN NIL)))
           ((LAMBDA (V)
              (COND
               ((NOT (FREEOFLIST (CADDR V) XLIST))
                (PROGN
                 (PROGN
                  (PRIN2 "A right hand side involves old variables.")
                  NIL)
                 (SETQ NOTALLOWED T)
                 NIL))))
            (CAR V))
           (SETQ V (CDR V))
           (GO LAB)))))
      (COND (NOTALLOWED (RETURN NIL)))
      (SETQ VL_ VL_BAK)
      (COND
       (SINGUL
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2
           "The right hand sides of transformations may become singular.")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "Therefore the following expressions are assumed to be non-zero")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "from now on:") NIL)
         (PROG (U)
           (SETQ U SINGUL)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (PROGN
               (SETQ INEQ_ (CONS U INEQ_))
               (MATHPRINT (LIST '*SQ U NIL))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB)))))
      (TERPRI)
      (PROGN (PRIN2 "Shall the transformed equation be fully simplified,") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "i.e. redundand non-vanishing factors be dropped y/n : ")
       NIL)
      (SETQ FULL_SIMPLIFY (TERMREAD))
      (COND ((EQUAL FULL_SIMPLIFY 'N) (SETQ FULL_SIMPLIFY NIL)))
      (TERPRI)
      (PROG (V)
        (SETQ V XSLIST)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ XLIST (SETDIFF XLIST (LIST (CADR V))))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (U)
        (SETQ U ULIST)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROG (V)
             (SETQ V XLIST)
            LAB
             (COND ((NULL V) (RETURN NIL)))
             ((LAMBDA (V) (DEPEND (LIST U V))) (CAR V))
             (SETQ V (CDR V))
             (GO LAB)))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (SETQ VLIST (APPEND VLIST XLIST))
      (PROG (V)
        (SETQ V XLIST)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ XSLIST (CONS (LIST 'EQUAL V V) XSLIST))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RESTORE_INTERACTIVE_PROMPT)
      (COND
       (PRINT_
        (PROGN
         (PROGN (PRIN2 "The transformation:") NIL)
         (TERPRI)
         (COND
          (VLIST
           (PROGN
            (PROGN (PRIN2 "The new variables: ") NIL)
            (LISTPRINT VLIST)
            (TERPRI))))
         (COND
          (ULIST
           (PROGN
            (PROGN (PRIN2 "The new functions: ") NIL)
            (LISTPRINT ULIST)
            (TERPRI))))
         (COND
          (XSLIST
           (PROGN
            (PROGN (PRIN2 "The old variables expressed:") NIL)
            (TERPRI)
            (MATHPRINT (CONS 'LIST XSLIST)))))
         (COND
          (YSLIST
           (PROGN
            (PROGN (PRIN2 "The old functions expressed:") NIL)
            (TERPRI)
            (MATHPRINT (CONS 'LIST YSLIST)))))
         NIL)))
      (SETQ H
              (ERR_CATCH_SOLVE
               (CONS 'LIST
                     (APPEND
                      (PROG (H FORALL-RESULT FORALL-ENDPTR)
                        (SETQ H XSLIST)
                        (COND ((NULL H) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (H)
                                            (LIST 'DIFFERENCE (CADR H)
                                                  (CADDR H)))
                                          (CAR H))
                                         NIL)))
                       LOOPLABEL
                        (SETQ H (CDR H))
                        (COND ((NULL H) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (LIST 'DIFFERENCE (CADR H) (CADDR H)))
                                  (CAR H))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      (PROG (H FORALL-RESULT FORALL-ENDPTR)
                        (SETQ H YSLIST)
                        (COND ((NULL H) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (H)
                                            (LIST 'DIFFERENCE (CADR H)
                                                  (CADDR H)))
                                          (CAR H))
                                         NIL)))
                       LOOPLABEL
                        (SETQ H (CDR H))
                        (COND ((NULL H) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (LIST 'DIFFERENCE (CADR H) (CADDR H)))
                                  (CAR H))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
               (CONS 'LIST (APPEND VLIST ULIST))))
      (COND (H (SETQ DONE_TRAFO (CONS 'LIST (CONS (CAR H) (CDR DONE_TRAFO)))))
            (T
             (PROGN
              (SETQ INVERSE_TRAFO_LIST_INCOMPLETE T)
              (SETQ DONE_TRAFO
                      (CONS 'LIST
                            (CONS (CONS 'LIST (APPEND XSLIST YSLIST))
                                  (CDR DONE_TRAFO)))))))
      (RETURN
       (LIST 'LIST (CONS 'LIST ULIST) (CONS 'LIST VLIST) (CONS 'LIST YSLIST)
             (CONS 'LIST XSLIST) FULL_SIMPLIFY)))) 
(PUT 'ADDDEP 'NUMBER-OF-ARGS 1) 
(PUT 'ADDDEP 'DEFINED-ON-LINE '321) 
(PUT 'ADDDEP 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'ADDDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADDDEP (XLIST)
    (PROG (NEWDEP XS DP)
      (PROG (XS)
        (SETQ XS XLIST)
       LAB
        (COND ((NULL XS) (RETURN NIL)))
        ((LAMBDA (XS)
           (PROGN
            (SETQ NEWDEP NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT DEPL*) (RETURN NIL)))
              (PROGN
               (SETQ DP (CAR DEPL*))
               (SETQ DEPL* (CDR DEPL*))
               (COND
                ((NOT (FREEOF DP (CAR XS)))
                 (SETQ DP (CONS (CAR DP) (UNION (CDR XS) (CDR DP))))))
               (SETQ NEWDEP (CONS DP NEWDEP))
               NIL)
              (GO WHILELABEL))
            (SETQ DEPL* (REVERSE NEWDEP))
            NIL))
         (CAR XS))
        (SETQ XS (CDR XS))
        (GO LAB)))) 
(PUT 'ADD_DEP_AND_EQN 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_DEP_AND_EQN 'DEFINED-ON-LINE '341) 
(PUT 'ADD_DEP_AND_EQN 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'ADD_DEP_AND_EQN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_DEP_AND_EQN (XLIST)
    (PROG (NEWDEP NEWEQ DP X)
      (PROG ()
       WHILELABEL
        (COND ((NOT DEPL*) (RETURN NIL)))
        (PROGN
         (SETQ DP (CAR DEPL*))
         (SETQ DEPL* (CDR DEPL*))
         (COND
          ((AND (MEMBER (CAR DP) FTEM_) (NOT (FREEOFLIST (CDR DP) XLIST)))
           (PROG (X)
             (SETQ X XLIST)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (COND
                 ((FREEOF (CDR DP) X)
                  (PROGN
                   (SETQ NEWEQ (CONS (LIST 'DF (CAR DP) X) NEWEQ))
                   (SETQ DP (CONS (CAR DP) (CONS X (CDR DP))))))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB))))
         (SETQ NEWDEP (CONS DP NEWDEP))
         NIL)
        (GO WHILELABEL))
      (SETQ DEPL* (REVERSE NEWDEP))
      (RETURN NEWEQ))) 
(PUT 'DROPDEP 'NUMBER-OF-ARGS 1) 
(PUT 'DROPDEP 'DEFINED-ON-LINE '366) 
(PUT 'DROPDEP 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'DROPDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROPDEP (XLIST)
    (PROG (X DP NEWDEP)
      (PROG (X)
        (SETQ X XLIST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ NEWDEP NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT DEPL*) (RETURN NIL)))
              (PROGN
               (SETQ DP (CAR DEPL*))
               (SETQ DEPL* (CDR DEPL*))
               (COND ((NOT (FREEOF DP X)) (SETQ DP (DELETE X DP))))
               (SETQ NEWDEP (CONS DP NEWDEP))
               NIL)
              (GO WHILELABEL))
            (SETQ DEPL* (REVERSE NEWDEP))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'TRANSFODF 'NUMBER-OF-ARGS 4) 
(PUT 'TRANSFODF 'DEFINED-ON-LINE '385) 
(PUT 'TRANSFODF 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'TRANSFODF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANSFODF (DY YSLIST XLIST VLIST)
    (PROG (CPY X DYM1 M N NEWDY V)
      (SETQ CPY YSLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CPY (NEQ DY (CADAR CPY)))) (RETURN NIL)))
        (SETQ CPY (CDR CPY))
        (GO WHILELABEL))
      (RETURN
       (COND ((NOT (NULL CPY)) (CONS (CAR CPY) YSLIST))
             ((NOT (PAIRP DY)) (CONS (LIST 'EQUAL DY (SIMP DY)) YSLIST))
             (T
              (PROGN
               (COND
                ((OR (EQUAL (LENGTH DY) 3)
                     (AND (EQUAL (LENGTH DY) 4) (EQUAL (CADDDR DY) 1)))
                 (PROGN (SETQ X (CADDR DY)) (SETQ DYM1 (CADR DY))))
                (T
                 (PROGN
                  (SETQ CPY (REVERSE DY))
                  (SETQ DYM1
                          (REVERSE
                           (COND
                            ((NOT (NUMBERP (CAR CPY)))
                             (PROGN (SETQ X (CAR CPY)) (CDR CPY)))
                            ((EQUAL (CAR CPY) 1)
                             (PROGN (SETQ X (CADR CPY)) (CDDR CPY)))
                            ((EQUAL (CAR CPY) 2)
                             (PROGN (SETQ X (CADR CPY)) (CDR CPY)))
                            (T
                             (PROGN
                              (SETQ X (CADR CPY))
                              (CONS (SUB1 (CAR CPY)) (CDR CPY))))))))))
               (SETQ YSLIST (TRANSFODF DYM1 YSLIST XLIST VLIST))
               (SETQ DYM1 (CAR YSLIST))
               (SETQ DYM1 (CADDR DYM1))
               (SETQ YSLIST (CDR YSLIST))
               (SETQ M 1)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND XLIST (NEQ X (CAR XLIST)))) (RETURN NIL)))
                 (PROGN (SETQ M (ADD1 M)) (SETQ XLIST (CDR XLIST)))
                 (GO WHILELABEL))
               (COND ((NULL XLIST) (SETQ NEWDY (DIFFSQ DYM1 X)))
                     (T
                      (PROGN
                       (SETQ NEWDY (CONS NIL 1))
                       (SETQ N 0)
                       (PROG (V)
                         (SETQ V VLIST)
                        LAB
                         (COND ((NULL V) (RETURN NIL)))
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ N (ADD1 N))
                             (COND
                              ((NOT (ZEROP (AEVAL (LIST 'DV/DX N M))))
                               (SETQ NEWDY
                                       (ADDSQ
                                        (MULTSQ (DIFFSQ DYM1 V)
                                                (SIMP
                                                 (AEVAL (LIST 'DV/DX N M))))
                                        NEWDY))))
                             NIL))
                          (CAR V))
                         (SETQ V (CDR V))
                         (GO LAB)))))
               (CONS (LIST 'EQUAL DY NEWDY)
                     (CONS (LIST 'EQUAL DY NEWDY) YSLIST)))))))) 
(PUT 'DO_TRAFO 'NUMBER-OF-ARGS 2) 
(PUT 'DO_TRAFO 'DEFINED-ON-LINE '445) 
(PUT 'DO_TRAFO 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'DO_TRAFO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO_TRAFO (ARGLIST X)
    (PROG (YSLIST XSLIST ULIST VLIST XLIST YLIST M N OVAR NVAR E1 E2 E3 E4
           DETPD PDES HVAL TRFO NEWFORG NEWINEQ_ NEWEQ DRVS FULL_SIMPLIFY
           DEPL*BAK)
      (SETQ X (CDR X))
      (SETQ ULIST (CAR X))
      (SETQ VLIST (CADR X))
      (SETQ YSLIST (CADDR X))
      (SETQ XSLIST (CADDDR X))
      (SETQ FULL_SIMPLIFY (CADDDR (CDR X)))
      (SETQ X NIL)
      (SETQ DEPL*BAK DEPL*)
      (SETQ XLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 (CDR XSLIST))
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (E1) (CADR E1)) (CAR E1)) NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E1) (CADR E1)) (CAR E1)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NEWEQ (ADD_DEP_AND_EQN XLIST))
      (SETQ XLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 (CDR XSLIST))
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (PROGN
                                     (SETQ X (CADDR E1))
                                     (SETQ E3 NIL)
                                     (PROG (E2)
                                       (SETQ E2 (CDR VLIST))
                                      LAB
                                       (COND ((NULL E2) (RETURN NIL)))
                                       ((LAMBDA (E2)
                                          (COND
                                           ((NOT (FREEOF X E2))
                                            (SETQ E3 (CONS E2 E3)))))
                                        (CAR E2))
                                       (SETQ E2 (CDR E2))
                                       (GO LAB))
                                     (CONS (CADR E1) E3)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (PROGN
                             (SETQ X (CADDR E1))
                             (SETQ E3 NIL)
                             (PROG (E2)
                               (SETQ E2 (CDR VLIST))
                              LAB
                               (COND ((NULL E2) (RETURN NIL)))
                               ((LAMBDA (E2)
                                  (COND
                                   ((NOT (FREEOF X E2))
                                    (SETQ E3 (CONS E2 E3)))))
                                (CAR E2))
                               (SETQ E2 (CDR E2))
                               (GO LAB))
                             (CONS (CADR E1) E3)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (ADDDEP XLIST)
      (PROGN
       (SETQ M (AEVAL (LIST 'LENGTH XSLIST)))
       (SETQ N (AEVAL (LIST 'PLUS (LIST 'LENGTH YSLIST) M)))
       (AEVAL (CLEAR (LIST 'DYX/DUV 'DV/DX)))
       (AEVAL (MATRIX (LIST (LIST 'DYX/DUV N N))))
       (AEVAL (MATRIX (LIST (LIST 'DV/DX M M))))
       (SETQ OVAR (AEVAL (LIST 'APPEND YSLIST XSLIST)))
       (SETQ NVAR (AEVAL (LIST 'APPEND ULIST VLIST)))
       (SETQ N (AEVAL 0))
       (PROG (E1)
         (SETQ E1 (GETRLIST (AEVAL OVAR)))
        LAB
         (COND ((NULL E1) (RETURN NIL)))
         ((LAMBDA (E1)
            (PROGN
             (SETQ N (AEVAL (LIST 'PLUS N 1)))
             (SETQ M (AEVAL 0))
             (PROG (E2)
               (SETQ E2 (GETRLIST (AEVAL NVAR)))
              LAB
               (COND ((NULL E2) (RETURN NIL)))
               ((LAMBDA (E2)
                  (PROGN
                   (SETQ M (AEVAL (LIST 'PLUS M 1)))
                   (SETK (LIST 'DYX/DUV M N)
                         (AEVAL (LIST 'DF (LIST 'RHS E1) E2)))))
                (CAR E2))
               (SETQ E2 (CDR E2))
               (GO LAB))))
          (CAR E1))
         (SETQ E1 (CDR E1))
         (GO LAB))
       (SETQ DETPD (AEVAL (LIST 'DET 'DYX/DUV)))
       (COND
        ((EVALEQUAL (AEVAL DETPD) 0)
         (RETURN
          (PROGN
           (ASSGNPRI
            (AEVAL "##### The proposed transformation is not regular!") NIL
            'ONLY)
           (ASSGNPRI (AEVAL "The Jacobi matrix  J(i,j) := df(old_i,new_j) is")
                     NIL 'ONLY)
           (PROGN
            (ASSGNPRI (AEVAL "dyx!/duv = ") NIL 'FIRST)
            (ASSGNPRI (AEVAL 'DYX/DUV) NIL 'LAST))
           (ASSGNPRI (AEVAL "where :") NIL 'ONLY)
           (PROG (E1)
             (SETQ E1 (GETRLIST (AEVAL OVAR)))
            LAB
             (COND ((NULL E1) (RETURN NIL)))
             ((LAMBDA (E1)
                (PROGN
                 (PROG (E2)
                   (SETQ E2 (GETRLIST (AEVAL NVAR)))
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (PROGN
                       (ASSGNPRI (AEVAL "df(") NIL 'FIRST)
                       (ASSGNPRI (AEVAL (LIST 'LHS E1)) NIL NIL)
                       (ASSGNPRI (AEVAL ",") NIL NIL)
                       (ASSGNPRI (AEVAL E2) NIL NIL)
                       (ASSGNPRI (AEVAL ") = ") NIL NIL)
                       (ASSGNPRI (AEVAL (LIST 'DF (LIST 'RHS E1) E2)) NIL
                                 'LAST)))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB))
                 (ASSGNPRI (AEVAL " ") NIL 'ONLY)))
              (CAR E1))
             (SETQ E1 (CDR E1))
             (GO LAB))
           (SETQ DEPL* DEPL*BAK)
           (AEVAL 'NIL)))))
       (AEVAL (CLEAR (LIST 'DYX/DUV)))
       (SETQ N (AEVAL 0))
       (PROG (E1)
         (SETQ E1 (GETRLIST (AEVAL XSLIST)))
        LAB
         (COND ((NULL E1) (RETURN NIL)))
         ((LAMBDA (E1)
            (PROGN
             (SETQ N (AEVAL (LIST 'PLUS N 1)))
             (SETQ M (AEVAL 0))
             (PROG (E2)
               (SETQ E2 (GETRLIST (AEVAL VLIST)))
              LAB
               (COND ((NULL E2) (RETURN NIL)))
               ((LAMBDA (E2)
                  (PROGN
                   (SETQ M (AEVAL (LIST 'PLUS M 1)))
                   (SETK (LIST 'DV/DX N M)
                         (AEVAL
                          (LIST 'TOTAL_ALG_MODE_DERIV (LIST 'RHS E1) E2)))))
                (CAR E2))
               (SETQ E2 (CDR E2))
               (GO LAB))))
          (CAR E1))
         (SETQ E1 (CDR E1))
         (GO LAB))
       (SETK 'DV/DX (AEVAL (LIST 'EXPT 'DV/DX (MINUS 1)))))
      (SETQ XSLIST (CDR XSLIST))
      (SETQ YSLIST (CDR YSLIST))
      (SETQ VLIST (CDR VLIST))
      (SETQ ULIST (CDR ULIST))
      (COND
       (ULIST
        (PROGN
         (PROG (E1)
           (SETQ E1 YSLIST)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1) (SETQ FTEM_ (DELETE (CADR E1) FTEM_))) (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (PROG (E1)
           (SETQ E1 ULIST)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1) (SETQ FTEM_ (FCTINSERT E1 FTEM_))) (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         NIL)))
      (SETQ XLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 XSLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1) (REVAL1 (CADR E1) T)) (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E1) (REVAL1 (CADR E1) T)) (CAR E1))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (E1)
        (SETQ E1 XLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (SETQ VL_ (DELETE E1 VL_))) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ VL_ (APPEND VL_ VLIST))
      (SETQ YLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 YSLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1) (REVAL1 (CADR E1) T)) (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E1) (REVAL1 (CADR E1) T)) (CAR E1))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (NEWEQ
        (SETQ NEWEQ
                (MKEQSQLIST NIL NIL NEWEQ FTEM_ VL_ ALLFLAGS_ T (LIST 0)
                 NIL))))
      (SETQ PDES (APPEND NEWEQ (CAR ARGLIST)))
      (PROG (E1)
        (SETQ E1 PDES)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ HVAL (GET E1 'SQVAL))
            (SETQ DRVS (APPEND (SEARCH_LI2 HVAL 'DF) YLIST))
            (PROG (E3)
              (SETQ E3 DRVS)
             LAB
              (COND ((NULL E3) (RETURN NIL)))
              ((LAMBDA (E3)
                 (PROGN
                  (SETQ TRFO (TRANSFODF E3 YSLIST XLIST VLIST))
                  (SETQ HVAL
                          (SUBSQ HVAL
                                 (LIST
                                  (CONS (CADAR TRFO)
                                        (LIST '*SQ (CADDAR TRFO) T)))))
                  (SETQ YSLIST (CDR TRFO))))
               (CAR E3))
              (SETQ E3 (CDR E3))
              (GO LAB))
            (SETQ E3 NIL)
            (PROG (E2)
              (SETQ E2 XSLIST)
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (COND
                  ((NOT (FREEOF HVAL (CADR E2)))
                   (SETQ E3 (CONS (CONS (CADR E2) (CADDR E2)) E3)))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (SETQ HVAL (SUBSQ HVAL E3))
            (PUT E1 'SQVAL HVAL)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROG (E1)
        (SETQ E1 (CADR ARGLIST))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((AND (PAIRP E1) (EQUAL (CAR E1) 'EQUAL))
             (PROGN
              (SETQ HVAL (CADDR E1))
              (SETQ DRVS (APPEND (SEARCH_LI2 HVAL 'DF) YLIST))
              (PROG (E3)
                (SETQ E3 DRVS)
               LAB
                (COND ((NULL E3) (RETURN NIL)))
                ((LAMBDA (E3)
                   (PROGN
                    (SETQ TRFO (TRANSFODF E3 YSLIST XLIST VLIST))
                    (SETQ HVAL
                            (SUBSQ HVAL
                                   (LIST
                                    (CONS (CADAR TRFO)
                                          (LIST '*SQ (CADDAR TRFO) T)))))
                    (SETQ YSLIST (CDR TRFO))))
                 (CAR E3))
                (SETQ E3 (CDR E3))
                (GO LAB))
              (PROG (E2)
                (SETQ E2 XSLIST)
               LAB
                (COND ((NULL E2) (RETURN NIL)))
                ((LAMBDA (E2)
                   (COND
                    ((NOT (FREEOF HVAL (CADR E2)))
                     (SETQ HVAL
                             (SUBSQ HVAL
                                    (LIST
                                     (CONS (CADR E2)
                                           (REVAL1 (CADDR E2) T))))))))
                 (CAR E2))
                (SETQ E2 (CDR E2))
                (GO LAB))
              (SETQ NEWFORG (CONS (LIST 'EQUAL (CADR E1) HVAL) NEWFORG))
              (SETQ E2 NIL)
              (PROG (E3)
                (SETQ E3 FTEM_)
               LAB
                (COND ((NULL E3) (RETURN NIL)))
                ((LAMBDA (E3)
                   (COND ((NOT (FREEOF HVAL E3)) (SETQ E2 (CONS E3 E2)))))
                 (CAR E3))
                (SETQ E3 (CDR E3))
                (GO LAB))
              (PUT (CADR E1) 'FCTS E2)))
            ((NOT (FREEOF YLIST E1))
             (PROGN
              (SETQ E3 YSLIST)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND E3 (NEQ (CADAR E3) E1))) (RETURN NIL)))
                (SETQ E3 (CDR E3))
                (GO WHILELABEL))
              (COND (E3 (SETQ NEWFORG (CONS (CAR E3) NEWFORG)))
                    (T (SETQ NEWFORG (CONS E1 NEWFORG))))))
            (T (SETQ NEWFORG (CONS E1 NEWFORG)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ INEQ_OR
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 INEQ_OR)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ E2 E1)
                                      (COND ((NULL E2) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E2)
                                                          (PROG (E3
                                                                 FORALL-RESULT
                                                                 FORALL-ENDPTR)
                                                            (SETQ E3 E2)
                                                            (COND
                                                             ((NULL E3)
                                                              (RETURN NIL)))
                                                            (SETQ FORALL-RESULT
                                                                    (SETQ FORALL-ENDPTR
                                                                            (CONS
                                                                             ((LAMBDA
                                                                                  (
                                                                                   E3)
                                                                                (PROGN
                                                                                 (SETQ DRVS
                                                                                         (APPEND
                                                                                          (SEARCH_LI2
                                                                                           E3
                                                                                           'DF)
                                                                                          YLIST))
                                                                                 (PROG (E4)
                                                                                   (SETQ E4
                                                                                           DRVS)
                                                                                  LAB
                                                                                   (COND
                                                                                    ((NULL
                                                                                      E4)
                                                                                     (RETURN
                                                                                      NIL)))
                                                                                   ((LAMBDA
                                                                                        (
                                                                                         E4)
                                                                                      (PROGN
                                                                                       (SETQ TRFO
                                                                                               (TRANSFODF
                                                                                                E4
                                                                                                YSLIST
                                                                                                XLIST
                                                                                                VLIST))
                                                                                       (SETQ E3
                                                                                               (SUBSQ
                                                                                                E3
                                                                                                (LIST
                                                                                                 (CONS
                                                                                                  (CADAR
                                                                                                   TRFO)
                                                                                                  (LIST
                                                                                                   '*SQ
                                                                                                   (CADDAR
                                                                                                    TRFO)
                                                                                                   T)))))
                                                                                       (SETQ YSLIST
                                                                                               (CDR
                                                                                                TRFO))))
                                                                                    (CAR
                                                                                     E4))
                                                                                   (SETQ E4
                                                                                           (CDR
                                                                                            E4))
                                                                                   (GO
                                                                                    LAB))
                                                                                 (PROG (E4)
                                                                                   (SETQ E4
                                                                                           XSLIST)
                                                                                  LAB
                                                                                   (COND
                                                                                    ((NULL
                                                                                      E4)
                                                                                     (RETURN
                                                                                      NIL)))
                                                                                   ((LAMBDA
                                                                                        (
                                                                                         E4)
                                                                                      (COND
                                                                                       ((NOT
                                                                                         (FREEOF
                                                                                          E3
                                                                                          (CADR
                                                                                           E4)))
                                                                                        (SETQ E3
                                                                                                (SUBSQ
                                                                                                 E3
                                                                                                 (LIST
                                                                                                  (CONS
                                                                                                   (CADR
                                                                                                    E4)
                                                                                                   (REVAL1
                                                                                                    (CADDR
                                                                                                     E4)
                                                                                                    T))))))))
                                                                                    (CAR
                                                                                     E4))
                                                                                   (SETQ E4
                                                                                           (CDR
                                                                                            E4))
                                                                                   (GO
                                                                                    LAB))
                                                                                 E3))
                                                                              (CAR
                                                                               E3))
                                                                             NIL)))
                                                           LOOPLABEL
                                                            (SETQ E3 (CDR E3))
                                                            (COND
                                                             ((NULL E3)
                                                              (RETURN
                                                               FORALL-RESULT)))
                                                            (RPLACD
                                                             FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (E3)
                                                                 (PROGN
                                                                  (SETQ DRVS
                                                                          (APPEND
                                                                           (SEARCH_LI2
                                                                            E3
                                                                            'DF)
                                                                           YLIST))
                                                                  (PROG (E4)
                                                                    (SETQ E4
                                                                            DRVS)
                                                                   LAB
                                                                    (COND
                                                                     ((NULL E4)
                                                                      (RETURN
                                                                       NIL)))
                                                                    ((LAMBDA
                                                                         (E4)
                                                                       (PROGN
                                                                        (SETQ TRFO
                                                                                (TRANSFODF
                                                                                 E4
                                                                                 YSLIST
                                                                                 XLIST
                                                                                 VLIST))
                                                                        (SETQ E3
                                                                                (SUBSQ
                                                                                 E3
                                                                                 (LIST
                                                                                  (CONS
                                                                                   (CADAR
                                                                                    TRFO)
                                                                                   (LIST
                                                                                    '*SQ
                                                                                    (CADDAR
                                                                                     TRFO)
                                                                                    T)))))
                                                                        (SETQ YSLIST
                                                                                (CDR
                                                                                 TRFO))))
                                                                     (CAR E4))
                                                                    (SETQ E4
                                                                            (CDR
                                                                             E4))
                                                                    (GO LAB))
                                                                  (PROG (E4)
                                                                    (SETQ E4
                                                                            XSLIST)
                                                                   LAB
                                                                    (COND
                                                                     ((NULL E4)
                                                                      (RETURN
                                                                       NIL)))
                                                                    ((LAMBDA
                                                                         (E4)
                                                                       (COND
                                                                        ((NOT
                                                                          (FREEOF
                                                                           E3
                                                                           (CADR
                                                                            E4)))
                                                                         (SETQ E3
                                                                                 (SUBSQ
                                                                                  E3
                                                                                  (LIST
                                                                                   (CONS
                                                                                    (CADR
                                                                                     E4)
                                                                                    (REVAL1
                                                                                     (CADDR
                                                                                      E4)
                                                                                     T))))))))
                                                                     (CAR E4))
                                                                    (SETQ E4
                                                                            (CDR
                                                                             E4))
                                                                    (GO LAB))
                                                                  E3))
                                                               (CAR E3))
                                                              NIL))
                                                            (SETQ FORALL-ENDPTR
                                                                    (CDR
                                                                     FORALL-ENDPTR))
                                                            (GO LOOPLABEL)))
                                                        (CAR E2))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ E2 (CDR E2))
                                      (COND ((NULL E2) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (E2)
                                                  (PROG (E3 FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ E3 E2)
                                                    (COND
                                                     ((NULL E3) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (E3)
                                                                        (PROGN
                                                                         (SETQ DRVS
                                                                                 (APPEND
                                                                                  (SEARCH_LI2
                                                                                   E3
                                                                                   'DF)
                                                                                  YLIST))
                                                                         (PROG (E4)
                                                                           (SETQ E4
                                                                                   DRVS)
                                                                          LAB
                                                                           (COND
                                                                            ((NULL
                                                                              E4)
                                                                             (RETURN
                                                                              NIL)))
                                                                           ((LAMBDA
                                                                                (
                                                                                 E4)
                                                                              (PROGN
                                                                               (SETQ TRFO
                                                                                       (TRANSFODF
                                                                                        E4
                                                                                        YSLIST
                                                                                        XLIST
                                                                                        VLIST))
                                                                               (SETQ E3
                                                                                       (SUBSQ
                                                                                        E3
                                                                                        (LIST
                                                                                         (CONS
                                                                                          (CADAR
                                                                                           TRFO)
                                                                                          (LIST
                                                                                           '*SQ
                                                                                           (CADDAR
                                                                                            TRFO)
                                                                                           T)))))
                                                                               (SETQ YSLIST
                                                                                       (CDR
                                                                                        TRFO))))
                                                                            (CAR
                                                                             E4))
                                                                           (SETQ E4
                                                                                   (CDR
                                                                                    E4))
                                                                           (GO
                                                                            LAB))
                                                                         (PROG (E4)
                                                                           (SETQ E4
                                                                                   XSLIST)
                                                                          LAB
                                                                           (COND
                                                                            ((NULL
                                                                              E4)
                                                                             (RETURN
                                                                              NIL)))
                                                                           ((LAMBDA
                                                                                (
                                                                                 E4)
                                                                              (COND
                                                                               ((NOT
                                                                                 (FREEOF
                                                                                  E3
                                                                                  (CADR
                                                                                   E4)))
                                                                                (SETQ E3
                                                                                        (SUBSQ
                                                                                         E3
                                                                                         (LIST
                                                                                          (CONS
                                                                                           (CADR
                                                                                            E4)
                                                                                           (REVAL1
                                                                                            (CADDR
                                                                                             E4)
                                                                                            T))))))))
                                                                            (CAR
                                                                             E4))
                                                                           (SETQ E4
                                                                                   (CDR
                                                                                    E4))
                                                                           (GO
                                                                            LAB))
                                                                         E3))
                                                                      (CAR E3))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ E3 (CDR E3))
                                                    (COND
                                                     ((NULL E3)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (E3)
                                                                (PROGN
                                                                 (SETQ DRVS
                                                                         (APPEND
                                                                          (SEARCH_LI2
                                                                           E3
                                                                           'DF)
                                                                          YLIST))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           DRVS)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (PROGN
                                                                       (SETQ TRFO
                                                                               (TRANSFODF
                                                                                E4
                                                                                YSLIST
                                                                                XLIST
                                                                                VLIST))
                                                                       (SETQ E3
                                                                               (SUBSQ
                                                                                E3
                                                                                (LIST
                                                                                 (CONS
                                                                                  (CADAR
                                                                                   TRFO)
                                                                                  (LIST
                                                                                   '*SQ
                                                                                   (CADDAR
                                                                                    TRFO)
                                                                                   T)))))
                                                                       (SETQ YSLIST
                                                                               (CDR
                                                                                TRFO))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           XSLIST)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (COND
                                                                       ((NOT
                                                                         (FREEOF
                                                                          E3
                                                                          (CADR
                                                                           E4)))
                                                                        (SETQ E3
                                                                                (SUBSQ
                                                                                 E3
                                                                                 (LIST
                                                                                  (CONS
                                                                                   (CADR
                                                                                    E4)
                                                                                   (REVAL1
                                                                                    (CADDR
                                                                                     E4)
                                                                                    T))))))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 E3))
                                                              (CAR E3))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))
                                                (CAR E2))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ E2 E1)
                              (COND ((NULL E2) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (E2)
                                                  (PROG (E3 FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ E3 E2)
                                                    (COND
                                                     ((NULL E3) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (E3)
                                                                        (PROGN
                                                                         (SETQ DRVS
                                                                                 (APPEND
                                                                                  (SEARCH_LI2
                                                                                   E3
                                                                                   'DF)
                                                                                  YLIST))
                                                                         (PROG (E4)
                                                                           (SETQ E4
                                                                                   DRVS)
                                                                          LAB
                                                                           (COND
                                                                            ((NULL
                                                                              E4)
                                                                             (RETURN
                                                                              NIL)))
                                                                           ((LAMBDA
                                                                                (
                                                                                 E4)
                                                                              (PROGN
                                                                               (SETQ TRFO
                                                                                       (TRANSFODF
                                                                                        E4
                                                                                        YSLIST
                                                                                        XLIST
                                                                                        VLIST))
                                                                               (SETQ E3
                                                                                       (SUBSQ
                                                                                        E3
                                                                                        (LIST
                                                                                         (CONS
                                                                                          (CADAR
                                                                                           TRFO)
                                                                                          (LIST
                                                                                           '*SQ
                                                                                           (CADDAR
                                                                                            TRFO)
                                                                                           T)))))
                                                                               (SETQ YSLIST
                                                                                       (CDR
                                                                                        TRFO))))
                                                                            (CAR
                                                                             E4))
                                                                           (SETQ E4
                                                                                   (CDR
                                                                                    E4))
                                                                           (GO
                                                                            LAB))
                                                                         (PROG (E4)
                                                                           (SETQ E4
                                                                                   XSLIST)
                                                                          LAB
                                                                           (COND
                                                                            ((NULL
                                                                              E4)
                                                                             (RETURN
                                                                              NIL)))
                                                                           ((LAMBDA
                                                                                (
                                                                                 E4)
                                                                              (COND
                                                                               ((NOT
                                                                                 (FREEOF
                                                                                  E3
                                                                                  (CADR
                                                                                   E4)))
                                                                                (SETQ E3
                                                                                        (SUBSQ
                                                                                         E3
                                                                                         (LIST
                                                                                          (CONS
                                                                                           (CADR
                                                                                            E4)
                                                                                           (REVAL1
                                                                                            (CADDR
                                                                                             E4)
                                                                                            T))))))))
                                                                            (CAR
                                                                             E4))
                                                                           (SETQ E4
                                                                                   (CDR
                                                                                    E4))
                                                                           (GO
                                                                            LAB))
                                                                         E3))
                                                                      (CAR E3))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ E3 (CDR E3))
                                                    (COND
                                                     ((NULL E3)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (E3)
                                                                (PROGN
                                                                 (SETQ DRVS
                                                                         (APPEND
                                                                          (SEARCH_LI2
                                                                           E3
                                                                           'DF)
                                                                          YLIST))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           DRVS)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (PROGN
                                                                       (SETQ TRFO
                                                                               (TRANSFODF
                                                                                E4
                                                                                YSLIST
                                                                                XLIST
                                                                                VLIST))
                                                                       (SETQ E3
                                                                               (SUBSQ
                                                                                E3
                                                                                (LIST
                                                                                 (CONS
                                                                                  (CADAR
                                                                                   TRFO)
                                                                                  (LIST
                                                                                   '*SQ
                                                                                   (CADDAR
                                                                                    TRFO)
                                                                                   T)))))
                                                                       (SETQ YSLIST
                                                                               (CDR
                                                                                TRFO))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           XSLIST)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (COND
                                                                       ((NOT
                                                                         (FREEOF
                                                                          E3
                                                                          (CADR
                                                                           E4)))
                                                                        (SETQ E3
                                                                                (SUBSQ
                                                                                 E3
                                                                                 (LIST
                                                                                  (CONS
                                                                                   (CADR
                                                                                    E4)
                                                                                   (REVAL1
                                                                                    (CADDR
                                                                                     E4)
                                                                                    T))))))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 E3))
                                                              (CAR E3))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))
                                                (CAR E2))
                                               NIL)))
                             LOOPLABEL
                              (SETQ E2 (CDR E2))
                              (COND ((NULL E2) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (E2)
                                          (PROG (E3 FORALL-RESULT
                                                 FORALL-ENDPTR)
                                            (SETQ E3 E2)
                                            (COND ((NULL E3) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (E3)
                                                                (PROGN
                                                                 (SETQ DRVS
                                                                         (APPEND
                                                                          (SEARCH_LI2
                                                                           E3
                                                                           'DF)
                                                                          YLIST))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           DRVS)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (PROGN
                                                                       (SETQ TRFO
                                                                               (TRANSFODF
                                                                                E4
                                                                                YSLIST
                                                                                XLIST
                                                                                VLIST))
                                                                       (SETQ E3
                                                                               (SUBSQ
                                                                                E3
                                                                                (LIST
                                                                                 (CONS
                                                                                  (CADAR
                                                                                   TRFO)
                                                                                  (LIST
                                                                                   '*SQ
                                                                                   (CADDAR
                                                                                    TRFO)
                                                                                   T)))))
                                                                       (SETQ YSLIST
                                                                               (CDR
                                                                                TRFO))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 (PROG (E4)
                                                                   (SETQ E4
                                                                           XSLIST)
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E4)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E4)
                                                                      (COND
                                                                       ((NOT
                                                                         (FREEOF
                                                                          E3
                                                                          (CADR
                                                                           E4)))
                                                                        (SETQ E3
                                                                                (SUBSQ
                                                                                 E3
                                                                                 (LIST
                                                                                  (CONS
                                                                                   (CADR
                                                                                    E4)
                                                                                   (REVAL1
                                                                                    (CADDR
                                                                                     E4)
                                                                                    T))))))))
                                                                    (CAR E4))
                                                                   (SETQ E4
                                                                           (CDR
                                                                            E4))
                                                                   (GO LAB))
                                                                 E3))
                                                              (CAR E3))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ E3 (CDR E3))
                                            (COND
                                             ((NULL E3)
                                              (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (E3)
                                                        (PROGN
                                                         (SETQ DRVS
                                                                 (APPEND
                                                                  (SEARCH_LI2
                                                                   E3 'DF)
                                                                  YLIST))
                                                         (PROG (E4)
                                                           (SETQ E4 DRVS)
                                                          LAB
                                                           (COND
                                                            ((NULL E4)
                                                             (RETURN NIL)))
                                                           ((LAMBDA (E4)
                                                              (PROGN
                                                               (SETQ TRFO
                                                                       (TRANSFODF
                                                                        E4
                                                                        YSLIST
                                                                        XLIST
                                                                        VLIST))
                                                               (SETQ E3
                                                                       (SUBSQ
                                                                        E3
                                                                        (LIST
                                                                         (CONS
                                                                          (CADAR
                                                                           TRFO)
                                                                          (LIST
                                                                           '*SQ
                                                                           (CADDAR
                                                                            TRFO)
                                                                           T)))))
                                                               (SETQ YSLIST
                                                                       (CDR
                                                                        TRFO))))
                                                            (CAR E4))
                                                           (SETQ E4 (CDR E4))
                                                           (GO LAB))
                                                         (PROG (E4)
                                                           (SETQ E4 XSLIST)
                                                          LAB
                                                           (COND
                                                            ((NULL E4)
                                                             (RETURN NIL)))
                                                           ((LAMBDA (E4)
                                                              (COND
                                                               ((NOT
                                                                 (FREEOF E3
                                                                         (CADR
                                                                          E4)))
                                                                (SETQ E3
                                                                        (SUBSQ
                                                                         E3
                                                                         (LIST
                                                                          (CONS
                                                                           (CADR
                                                                            E4)
                                                                           (REVAL1
                                                                            (CADDR
                                                                             E4)
                                                                            T))))))))
                                                            (CAR E4))
                                                           (SETQ E4 (CDR E4))
                                                           (GO LAB))
                                                         E3))
                                                      (CAR E3))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))
                                        (CAR E2))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SIMPSQINEQ_OR_ADHOC PDES)
      (SETQ NEWINEQ_ NIL)
      (PROG (E1)
        (SETQ E1 INEQ_)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ DRVS (APPEND (SEARCH_LI2 E1 'DF) YLIST))
            (PROG (E3)
              (SETQ E3 DRVS)
             LAB
              (COND ((NULL E3) (RETURN NIL)))
              ((LAMBDA (E3)
                 (PROGN
                  (SETQ TRFO (TRANSFODF E3 YSLIST XLIST VLIST))
                  (SETQ E1
                          (SUBSQ E1
                                 (LIST
                                  (CONS (CADAR TRFO)
                                        (LIST '*SQ (CADDAR TRFO) T)))))
                  (SETQ YSLIST (CDR TRFO))))
               (CAR E3))
              (SETQ E3 (CDR E3))
              (GO LAB))
            (PROG (E2)
              (SETQ E2 XSLIST)
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (COND
                  ((NOT (FREEOF E1 (CADR E2)))
                   (SETQ E1
                           (SUBSQ E1
                                  (LIST
                                   (CONS (CADR E2) (REVAL1 (CADDR E2) T))))))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (SETQ NEWINEQ_ (CONS (CONS (CAR E1) 1) NEWINEQ_))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ INEQ_ NIL)
      (PROG (E1)
        (SETQ E1 NEWINEQ_)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (ADDSQINEQ PDES E1 T)) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ XLIST NIL)
      (PROG (E1)
        (SETQ E1 XSLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((NEQ (CADR E1) (CADDR E1)) (SETQ XLIST (CONS (CADR E1) XLIST)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (DROPDEP XLIST)
      (PROG (E1)
        (SETQ E1 PDES)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (PROG (E2)
              (SETQ E2 ALLFLAGS_)
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2) (FLAG (LIST E1) E2)) (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (UPDATESQ E1 (GET E1 'SQVAL) NIL NIL FTEM_ VL_ FULL_SIMPLIFY
             (LIST 0) PDES)
            (DROP_PDE_FROM_IDTIES E1 PDES NIL)
            (DROP_PDE_FROM_PROPERTIES E1 PDES)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'DV/DX)))
      (RETURN (LIST PDES NEWFORG VL_)))) 
(PUT 'FIND_TRAFO 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_TRAFO 'DEFINED-ON-LINE '646) 
(PUT 'FIND_TRAFO 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'FIND_TRAFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_TRAFO (ARGLIST)
    (PROG (DLI AVF F SOL PDE PDES FORG BATCH_BAK PRINT_BAK VLIST XSLIST VL H1
           H2 H3 H4 TRTR ELIGFNCS ELIGPDES EPDES REMAIN OLD_HISTORY_BAK
           PROC_LIST_BAK EPDESCP ELIGPDESCP FDVAR)
      (SETQ PDES (CAR ARGLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (SETQ PDE (CAR PDES))
         (SETQ PDES (CDR PDES))
         (SETQ ELIGFNCS NIL)
         (SETQ AVF (GET PDE 'ALLVARFCTS))
         (COND
          ((AND AVF (NULL (CDR AVF)))
           (PROGN
            (SETQ F (CAR AVF))
            (PROG (H1)
              (SETQ H1 (GET PDE 'DERIVS))
             LAB
              (COND ((NULL H1) (RETURN NIL)))
              ((LAMBDA (H1)
                 (COND
                  ((EQUAL (CAAR H1) F) (SETQ FDVAR (UNION FDVAR (CDAR H1))))))
               (CAR H1))
              (SETQ H1 (CDR H1))
              (GO LAB))
            (SETQ AVF (GET PDE 'FCTS))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND AVF
                      (OR (EQUAL (CAR AVF) F)
                          (FREEOFLIST (FCTARGS (CAR AVF)) FDVAR))))
                (RETURN NIL)))
              (SETQ AVF (CDR AVF))
              (GO WHILELABEL))
            (COND
             ((NULL AVF)
              (PROGN
               (SETQ DLI (GET PDE 'DERIVS))
               (SETQ H1 T)
               (SETQ H2 0)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND DLI H1)) (RETURN NIL)))
                 (COND
                  ((OR (NOT (PAIRP (CAAR DLI))) (NEQ (CAAAR DLI) F))
                   (SETQ DLI (CDR DLI)))
                  ((NULL (CDAAR DLI)) (SETQ DLI (CDR DLI)))
                  ((NULL (CDDAAR DLI))
                   (PROGN (SETQ H2 (ADD1 H2)) (SETQ DLI (CDR DLI))))
                  (T (SETQ H1 NIL)))
                 (GO WHILELABEL))
               (COND
                ((AND (NULL DLI) (GREATERP H2 1))
                 (SETQ ELIGFNCS (CONS F ELIGFNCS)))))))
            (COND
             (ELIGFNCS
              (PROGN
               (SETQ ELIGPDES (CONS (CONS PDE ELIGFNCS) ELIGPDES))
               (SETQ EPDES (CONS PDE EPDES)))))))))
        (GO WHILELABEL))
      (COND (TRTR (PROGN (PROGN (PRIN2 "222") NIL) (TERPRI))))
      (COND ((NULL EPDES) (RETURN NIL)))
      (SETQ EPDESCP EPDES)
      (SETQ ELIGPDESCP ELIGPDES)
     AGAIN
      (COND ((NULL EPDESCP) (RETURN NIL)))
      (SETQ EPDES EPDESCP)
      (SETQ ELIGPDES ELIGPDESCP)
      (COND
       (EXPERT_MODE
        (COND ((NULL (SETQ PDE (SELECTPDES EPDES 1))) (RETURN NIL))
              (T
               (PROGN
                (SETQ PDE (CAR PDE))
                (SETQ F (GET PDE 'ALLVARFCTS))
                (COND ((NULL F) (SETQ PDE NIL)) (T (SETQ F (CAR F))))))))
       (T
        (PROGN
         (COND (TRTR (PROGN (PROGN (PRIN2 "333") NIL) (TERPRI))))
         (SETQ H2 10000)
         (SETQ REMAIN NIL)
         (PROG (H1)
           (SETQ H1 EPDES)
          LAB
           (COND ((NULL H1) (RETURN NIL)))
           ((LAMBDA (H1)
              (PROGN
               (SETQ H3 (LENGTH (GET H1 'ALLVARFCTS)))
               (COND
                ((LESSP H3 H2) (PROGN (SETQ H2 H3) (SETQ REMAIN (LIST H1))))
                ((EQUAL H3 H2) (SETQ REMAIN (CONS H1 REMAIN))))
               NIL))
            (CAR H1))
           (SETQ H1 (CDR H1))
           (GO LAB))
         (SETQ EPDES REMAIN)
         (COND (TRTR (PROGN (PROGN (PRIN2 "444") NIL) (TERPRI))))
         (SETQ H2 0)
         (PROG (H1)
           (SETQ H1 EPDES)
          LAB
           (COND ((NULL H1) (RETURN NIL)))
           ((LAMBDA (H1)
              (PROGN
               (SETQ H3 (GET H1 'NVARS))
               (COND
                ((GREATERP H3 H2) (PROGN (SETQ H2 H3) (SETQ REMAIN (LIST H1))))
                ((EQUAL H3 H2) (SETQ REMAIN (CONS H1 REMAIN))))
               NIL))
            (CAR H1))
           (SETQ H1 (CDR H1))
           (GO LAB))
         (SETQ EPDES REMAIN)
         (COND (TRTR (PROGN (PROGN (PRIN2 "555") NIL) (TERPRI))))
         (SETQ H2 10000)
         (PROG (H1)
           (SETQ H1 EPDES)
          LAB
           (COND ((NULL H1) (RETURN NIL)))
           ((LAMBDA (H1)
              (PROGN
               (SETQ H3
                       (DIFFERENCE (LENGTH (GET H1 'FCTS))
                                   (LENGTH (GET H1 'ALLVARFCTS))))
               (COND
                ((LESSP H3 H2) (PROGN (SETQ H2 H3) (SETQ REMAIN (LIST H1))))
                ((EQUAL H3 H2) (SETQ REMAIN (CONS H1 REMAIN))))
               NIL))
            (CAR H1))
           (SETQ H1 (CDR H1))
           (GO LAB))
         (SETQ EPDES REMAIN)
         (COND (TRTR (PROGN (PROGN (PRIN2 "5a5a5a") NIL) (TERPRI))))
         (SETQ H2 10000)
         (PROG (H1)
           (SETQ H1 EPDES)
          LAB
           (COND ((NULL H1) (RETURN NIL)))
           ((LAMBDA (H1)
              (PROGN
               (SETQ H3 (GET H1 'TERMS))
               (COND
                ((LESSP H3 H2) (PROGN (SETQ H2 H3) (SETQ REMAIN (LIST H1))))
                ((EQUAL H3 H2) (SETQ REMAIN (CONS H1 REMAIN))))
               NIL))
            (CAR H1))
           (SETQ H1 (CDR H1))
           (GO LAB))
         (SETQ EPDES REMAIN)
         (COND (TRTR (PROGN (PROGN (PRIN2 "666") NIL) (TERPRI))))
         (SETQ PDE (CAR EPDES))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND ELIGPDES (NEQ (CAAR ELIGPDES) PDE))) (RETURN NIL)))
           (SETQ ELIGPDES (CDR ELIGPDES))
           (GO WHILELABEL))
         (SETQ F (CADAR ELIGPDES))
         NIL)))
      (COND (TRTR (PROGN (PROGN (PRIN2 "777") NIL) (TERPRI))))
      (COND ((NULL PDE) (RETURN NIL)))
      (COND (TRTR (PROGN (PROGN (PRIN2 "888") NIL) (TERPRI))))
      (SETQ EPDESCP (DELETE PDE EPDESCP))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2 "Finding a transformation to integrate the 1st order PDE ")
          (PRIN2 PDE)
          (PRIN2 ":")
          NIL)
         (TERPRI)
         NIL)))
      (SETQ OLD_HISTORY_BAK OLD_HISTORY)
      (SETQ OLD_HISTORY NIL)
      (SETQ PROC_LIST_BAK PROC_LIST_)
      (SETQ PROC_LIST_ DEFAULT_PROC_LIST_)
      (SETQ PRINT_BAK PRINT_)
      (SETQ BATCH_BAK *BATCH_MODE)
      (SETQ *BATCH_MODE BATCH_MODE_SUB)
      (COND ((NULL *BATCH_MODE) (SETQ PRINT_ 8)))
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (SETQ H1 (LEVEL_STRING SESSION_))
      (SETQ H1 (BLDMSG_INTERNAL "%s%s" (LIST H1 "qlp")))
      (BACKUP_TO_FILE PDES FORG H1)
      (COND (TRTR (PROGN (PROGN (PRIN2 "999") NIL) (TERPRI))))
      (SETQ SOL
              (REVAL1
               (AEVAL
                (LIST 'QUASILINPDE (LIST '*SQ (GET PDE 'SQVAL) T) F
                      (CONS 'LIST (GET PDE 'VARS))))
               T))
      (RESTORE_BACKUP_FROM_FILE PDES FORG H1)
      (SYSTEM (BLDMSG_INTERNAL "rm '%s'" (LIST H1)))
      (COND (TRTR (PROGN (PROGN (PRIN2 "000") NIL) (TERPRI))))
      (COND
       (TRTR (PROGN (PROGN (PRIN2 "sol0=") NIL) (MATHPRINT SOL) (TERPRI))))
      (SETQ OLD_HISTORY OLD_HISTORY_BAK)
      (SETQ PROC_LIST_ PROC_LIST_BAK)
      (SETQ *BATCH_MODE BATCH_BAK)
      (SETQ PRINT_ PRINT_BAK)
      (COND ((OR (NULL SOL) (NULL (CDR SOL)) (NULL (CDADR SOL))) (RETURN NIL)))
      (SETQ SOL (CDR SOL))
     WHAT_NEXT
      (COND
       ((AND FREEINT_ (NULL (FREEOF (CAR SOL) 'INT)))
        (PROGN
         (PROGN
          (PRIN2
           "The found point transformation includes an unperformed integration:")
          NIL)
         (MATHPRINT (CAR SOL))
         (COND
          (*NAT
           (PROGN
            (AEVAL (OFF (LIST 'NAT)))
            (PROGN (PRIN2 "and again in machine readable form:") NIL)
            (TERPRI)
            (MATHPRINT (CAR SOL))
            (TERPRI)
            (AEVAL (ON (LIST 'NAT)))
            NIL)))
         (PROGN
          (PRIN2 "This will probably cause problems later on in the")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "computation. If you want to enforce using this") NIL)
         (TERPRI)
         (PROGN (PRIN2 "substitution you would have to set the flag") NIL)
         (TERPRI)
         (PROGN (PRIN2 "freeint_ to nil using the 'as' command and") NIL)
         (TERPRI)
         (PROGN (PRIN2 "re-do this step.") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN
          (PRIN2
           "Do you want to proceed with finding a transformations for this PDE (1)")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "or try to find a transformation for another PDE                    (2)")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "or stop finding transformations                                    (3)")
          NIL)
         (TERPRI)
         (CHANGE_PROMPT_TO "")
         (SETQ H1 (TERMREAD))
         (COND ((EQUAL H1 1) NIL) ((EQUAL H1 2) (GO AGAIN))
               ((EQUAL H1 3) (RETURN NIL)) (T (GO WHAT_NEXT))))))
      (SETQ H1 (ZERO_DEN SOL FTEM_))
      (SETQ H3 NIL)
      (PROG (H2)
        (SETQ H2 H1)
       LAB
        (COND ((NULL H2) (RETURN NIL)))
        ((LAMBDA (H2) (SETQ H3 (UNION (SIMPLIFYSQ H2 FTEM_ T NIL T) H3)))
         (CAR H2))
        (SETQ H2 (CDR H2))
        (GO LAB))
      (COND
       (H3
        (RETURN
         (PROGN
          (PROG (H2)
            (SETQ H2 H3)
           LAB
            (COND ((NULL H2) (RETURN NIL)))
            ((LAMBDA (H2)
               (COND
                ((NEQ H2 (LIST (CONS 1 1)))
                 (PROGN
                  (COND
                   (PRINT_
                    (PROGN
                     (PROGN (PRIN2 "The intended transformation ") NIL)
                     (TERPRI)
                     (MATHPRINT SOL)
                     (PROGN
                      (PRIN2 "leads to a case distinction whether :")
                      NIL)
                     (TERPRI)
                     (MATHPRINT (LIST '*SQ H2 T))
                     (PROGN
                      (PRIN2
                       "vanishes or not. This case distinction is done next.")
                      NIL)
                     (TERPRI))))
                  (SETQ TO_DO_LIST
                          (CONS (LIST 'SPLIT_INTO_CASES H2) TO_DO_LIST))))))
             (CAR H2))
            (SETQ H2 (CDR H2))
            (GO LAB))
          NIL))))
      (SETQ SOL (CDAR SOL))
      (SETQ H1 SOL)
      (PROG (H2)
        (SETQ H2 H1)
       LAB
        (COND ((NULL H2) (RETURN NIL)))
        ((LAMBDA (H2) (COND ((NOT (FREEOF H2 F)) (SETQ SOL (DELETE H2 SOL)))))
         (CAR H2))
        (SETQ H2 (CDR H2))
        (GO LAB))
      (COND
       (TRTR
        (PROGN
         (PROGN (PRIN2 "f=") (PRIN2 F) NIL)
         (TERPRI)
         (PROGN (PRIN2 "h1=") (PRIN2 H1) NIL)
         (TERPRI)
         (PROGN (PRIN2 "sol0=") NIL)
         (MATHPRINT (CONS 'LIST SOL)))))
      (SETQ H2 (GET PDE 'VARS))
      (SETQ H4 NIL)
      (PROG (F)
        (SETQ F H2)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((MEMBER F SOL)
             (PROGN
              (SETQ H4 (CONS F H4))
              (SETQ H2 (DELETE F H2))
              (SETQ SOL (DELETE F SOL))
              NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       (TRTR (PROGN (PROGN (PRIN2 "sol1=") NIL) (MATHPRINT (CONS 'LIST SOL)))))
      (COND (TRTR (PROGN (PROGN (PRIN2 "h2=") (PRIN2 H2) NIL) (TERPRI))))
      (SETQ XSLIST (ERR_CATCH_SOLVE (CONS 'LIST SOL) (CONS 'LIST H2)))
      (COND
       ((NULL XSLIST)
        (RETURN
         (PROGN
          (PROGN (PRIN2 "REDUCE was not able to solve") NIL)
          (MATHPRINT (CONS 'LIST SOL))
          (PROGN (PRIN2 "for one of ") NIL)
          (LISTPRINT H2)
          (TERPRI)
          NIL)))
       (T (SETQ XSLIST (CDR (REVAL1 (CAR XSLIST) T)))))
      (COND
       (TRTR (PROGN (PROGN (PRIN2 "xslist=") (PRIN2 XSLIST) NIL) (TERPRI))))
      (SETQ H3 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT XSLIST) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR XSLIST))
         (SETQ XSLIST (CDR XSLIST))
         (COND
          ((AND (PAIRP F) (EQUAL (CAR F) 'EQUAL) (PAIRP (CADDR F))
                (EQUAL (CAADDR F) 'ARBCOMPLEX))
           (PROGN
            (SETQ H2 (DELETE (CADR F) H2))
            (SETQ H3 (CONS (CADR F) H3))
            (SETQ XSLIST (SUBST 1 (CADDR F) XSLIST))))))
        (GO WHILELABEL))
      (COND
       (TRTR
        (PROGN
         (PROGN (PRIN2 "h3new=") (PRIN2 H3) NIL)
         (TERPRI)
         (PROGN (PRIN2 "h2new=") (PRIN2 H2) NIL)
         (TERPRI))))
      (SETQ SOL (APPEND H4 SOL))
      (COND (TRTR (PROGN (PROGN (PRIN2 "sol3=") (PRIN2 SOL) NIL) (TERPRI))))
      (COND
       (TRTR (PROGN (PROGN (PRIN2 "before: h4=") (PRIN2 H4) NIL) (TERPRI))))
      (SETQ H3 (APPEND H4 (APPEND H2 H3)))
      (COND (TRTR (PROGN (PROGN (PRIN2 "new: h3=") (PRIN2 H3) NIL) (TERPRI))))
      (SETQ H4 H3)
      (PROG (H1)
        (SETQ H1 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH SOL) H1)) (RETURN NIL)))
        (SETQ H4 (CDR H4))
        (SETQ H1 (PLUS2 H1 1))
        (GO LAB))
      (SETQ SOL (APPEND SOL H4))
      (COND (TRTR (PROGN (PROGN (PRIN2 "new: h4=") (PRIN2 H4) NIL) (TERPRI))))
      (SETQ H4 (MKID '% (LENGTH DONE_TRAFO)))
      (SETQ VLIST
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F H3)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (MKID F H4)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (MKID F H4)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (TRTR (PROGN (PROGN (PRIN2 "vlist=") (PRIN2 VLIST) NIL) (TERPRI))))
      (SETQ H1 VLIST)
      (COND
       (TRTR
        (PROGN
         (PROGN (PRIN2 "sol-2=") NIL)
         (MATHPRINT (CONS 'LIST SOL))
         (TERPRI))))
      (SETQ H2 SOL)
      (PROG ()
       WHILELABEL
        (COND ((NOT SOL) (RETURN NIL)))
        (PROGN
         (SETQ VL (CONS (LIST 'DIFFERENCE (CAR SOL) (CAR H1)) VL))
         (SETQ SOL (CDR SOL))
         (SETQ H1 (CDR H1))
         NIL)
        (GO WHILELABEL))
      (COND
       (TRTR
        (PROGN
         (PROGN (PRIN2 "2nd SOLVE: vl=") NIL)
         (MATHPRINT (CONS 'LIST VL))
         (TERPRI)
         (PROGN (PRIN2 "h3=") (PRIN2 H3) NIL))))
      (SETQ H4 !ARBINT)
      (SETQ XSLIST (ERR_CATCH_SOLVE (CONS 'LIST VL) (CONS 'LIST H3)))
      (COND
       ((NULL XSLIST)
        (RETURN
         (PROGN
          (PROGN (PRIN2 "REDUCE was not able to solve") NIL)
          (MATHPRINT (CONS 'LIST VL))
          (PROGN (PRIN2 "for the variables ") NIL)
          (LISTPRINT H3)
          NIL))))
      (SETQ XSLIST (CONS 'LIST XSLIST))
      (PROG (H1)
        (SETQ H1 (PLUS H4 1))
       LAB
        (COND ((MINUSP (DIFFERENCE !ARBINT H1)) (RETURN NIL)))
        (SETQ XSLIST
                (AEVAL* (LIST 'SUB (LIST 'EQUAL (LIST 'ARBINT H1) 0) XSLIST)))
        (SETQ H1 (PLUS2 H1 1))
        (GO LAB))
      (SETQ XSLIST (CDR (REVAL1 XSLIST T)))
      (COND
       ((NOT (FREEOF XSLIST 'SUB))
        (PROGN
         (PROGN (PRIN2 "'sub check ++++++++++") (PRIN2 XSLIST) NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "freeof(xslist,'sub) = ")
          (PRIN2 (FREEOF XSLIST 'SUB))
          NIL)
         (TERPRI)
         (COND ((FREEOF XSLIST 'SUB) (PROGN (PRIN2 "free") NIL))
               (T (PROGN (PRIN2 "not free") NIL)))
         (TERPRI)
         (PROGN
          (PRIN2
           "The found point transformation includes an uncompleted substitution:")
          NIL)
         (MATHPRINT XSLIST)
         (COND
          (*NAT
           (PROGN
            (AEVAL (OFF (LIST 'NAT)))
            (PROGN (PRIN2 "and again in machine readable form:") NIL)
            (TERPRI)
            (MATHPRINT XSLIST)
            (AEVAL (ON (LIST 'NAT)))
            NIL)))
         (PROGN
          (PRIN2 "Because this will probably cause problems later on in the")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "computation, you will need to input it manually using the")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "pt-command.") NIL)
         (TERPRI)
         (GO AGAIN)))
       ((NOT (FREEOF XSLIST 'ROOT_OF))
        (RETURN
         (PROGN
          (PROGN
           (PRIN2 "The following variable transformation expresses variables")
           NIL)
          (TERPRI)
          (LISTPRINT H3)
          (TERPRI)
          (PROGN (PRIN2 "  through variables  ") NIL)
          (TERPRI)
          (LISTPRINT VLIST)
          (PROGN (PRIN2 " :") NIL)
          (TERPRI)
          (PROG (F)
            (SETQ F (CDR (CAR XSLIST)))
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F) (MATHPRINT F)) (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (COND
           (*NAT
            (PROGN
             (AEVAL (OFF (LIST 'NAT)))
             (PROGN (PRIN2 "and again in machine readable form:") NIL)
             (TERPRI)
             (PROG (F)
               (SETQ F (CDR (CAR XSLIST)))
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F) (MATHPRINT F)) (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (AEVAL (ON (LIST 'NAT)))
             NIL)))
          (PROGN
           (PRIN2 "BUT the transformation could not be done explicitly.")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "The user is recommended to try finding a transformation")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "manually and input it with the 'pt' command.") NIL)
          (TERPRI)
          NIL)))
       (T (SETQ XSLIST (CAR XSLIST))))
     WHAT_NEXT2
      (COND
       ((OR TRTR PRINT_)
        (PROGN
         (PROGN
          (PRIN2 "The following variable transformation expresses variables")
          NIL)
         (TERPRI)
         (LISTPRINT H3)
         (TERPRI)
         (PROGN (PRIN2 "  through variables  ") NIL)
         (TERPRI)
         (LISTPRINT VLIST)
         (PROGN (PRIN2 " :") NIL)
         (TERPRI)
         (PROG (F)
           (SETQ F (CDR XSLIST))
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F) (MATHPRINT F)) (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (COND
          (*NAT
           (PROGN
            (AEVAL (OFF (LIST 'NAT)))
            (PROGN (PRIN2 "and again in machine readable form:") NIL)
            (TERPRI)
            (MATHPRINT XSLIST)
            (AEVAL (ON (LIST 'NAT)))
            NIL)))
         (TERPRI)
         (PROGN
          (PRIN2 "Do you want to proceed with this transformation  (1)")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "or input a modified transformation               (2)")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "or stop the whole transformation                 (3)")
          NIL)
         (TERPRI)
         (CHANGE_PROMPT_TO "")
         (SETQ H1 (TERMREAD))
         (COND ((EQUAL H1 1) NIL)
               ((EQUAL H1 2)
                (PROGN
                 (PROGN
                  (PRIN2 "Input the list of transformations in a form like")
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "{x=x!%1+y!%1, y=x!%1-y!%1};") NIL)
                 (TERPRI)
                 (SETQ XSLIST (TERMXREAD))
                 (GO WHAT_NEXT2)))
               ((EQUAL H1 3) (RETURN NIL)) (T (GO WHAT_NEXT2))))))
      (SETQ H3
              (PROG (H1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ H1 VL)
                (COND ((NULL H1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H1)
                                    (LIST 'EQUAL (CADDR H1) (CADR H1)))
                                  (CAR H1))
                                 NIL)))
               LOOPLABEL
                (SETQ H1 (CDR H1))
                (COND ((NULL H1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H1) (LIST 'EQUAL (CADDR H1) (CADR H1)))
                          (CAR H1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DONE_TRAFO (CONS 'LIST (CONS (CONS 'LIST H3) (CDR DONE_TRAFO))))
      (RETURN
       (DO_TRAFO ARGLIST
        (LIST 'LIST (LIST 'LIST) (CONS 'LIST VLIST) (LIST 'LIST) XSLIST T))))) 
(PUT 'GENERAL_TRAFO 'NUMBER-OF-ARGS 1) 
(PUT 'GENERAL_TRAFO 'DEFINED-ON-LINE '1073) 
(PUT 'GENERAL_TRAFO 'DEFINED-IN-FILE 'CRACK/CRTRAFO.RED) 
(PUT 'GENERAL_TRAFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERAL_TRAFO (ARGLIST)
    (PROG (X)
      (SETQ X (INPUT_TRAFO (CAR ARGLIST)))
      (COND
       ((NULL X)
        (RETURN
         (PROGN
          (TERPRI)
          (PROGN (PRIN2 "No proper input --> no transformation") NIL)
          NIL))))
      (RETURN (DO_TRAFO ARGLIST X)))) 
(ENDMODULE) 
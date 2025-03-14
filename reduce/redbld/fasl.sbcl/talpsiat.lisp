(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TALPSIAT)) 
(REVISION 'TALPSIAT
          "$Id: talpsiat.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'TALPSIAT "Copyright (c) 2004-2009 A. Dolzmann and T. Sturm") 
(PUT 'TALP_SIMPLAT1 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SIMPLAT1 'DEFINED-ON-LINE '33) 
(PUT 'TALP_SIMPLAT1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLAT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLAT1 (AT SOP)
    (PROG (RESULT)
      (SETQ AT (TALP_SIMPAT AT))
      (SETQ RESULT (TALP_SIMPLAT2 (CAR AT) (CADR AT) (CADDR AT)))
      (COND (RESULT (COND ((EQ RESULT T) (RETURN 'TRUE)) (T (RETURN RESULT))))
            (T (RETURN 'FALSE))))) 
(PUT 'TALP_SIMPLAT2 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SIMPLAT2 'DEFINED-ON-LINE '51) 
(PUT 'TALP_SIMPLAT2 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLAT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLAT2 (OP LHS RHS)
    (COND ((TALP_INVP RHS) (TALP_SIMPLATRINV OP LHS RHS))
          ((ATOM LHS) (TALP_SIMPLATAT OP LHS RHS))
          ((TALP_INVP LHS) (TALP_SIMPLATLINV OP LHS RHS))
          (T (TALP_SIMPLATFN OP LHS RHS)))) 
(PUT 'TALP_SIMPLATRINV 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SIMPLATRINV 'DEFINED-ON-LINE '70) 
(PUT 'TALP_SIMPLATRINV 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLATRINV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLATRINV (OP LHS RHS)
    (PROGN
     (SETQ RHS (TALP_SIMPLT RHS))
     (COND
      ((TALP_INVP RHS)
       (COND
        ((TALP_INVP LHS)
         (PROGN
          (SETQ LHS (TALP_SIMPLT LHS))
          (COND
           ((TALP_INVP LHS)
            (COND ((TALP_EQTP LHS RHS) (COND ((EQ OP 'EQUAL) T) (T NIL)))
                  (T (LIST OP LHS RHS))))
           (T (TALP_SIMPLAT2 OP LHS RHS)))))
        (T (LIST OP (TALP_SIMPLT LHS) RHS))))
      (T (TALP_SIMPLAT2 OP LHS RHS))))) 
(PUT 'TALP_SIMPLATLINV 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SIMPLATLINV 'DEFINED-ON-LINE '104) 
(PUT 'TALP_SIMPLATLINV 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLATLINV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLATLINV (OP LHS RHS)
    (PROGN
     (SETQ LHS (TALP_SIMPLT LHS))
     (COND ((TALP_INVP LHS) (LIST OP LHS RHS))
           (T (TALP_SIMPLAT2 OP LHS (TALP_SIMPLT RHS)))))) 
(PUT 'TALP_SIMPLATFN 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SIMPLATFN 'DEFINED-ON-LINE '121) 
(PUT 'TALP_SIMPLATFN 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLATFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLATFN (OP LHS RHS)
    (PROG (RESULT LIST)
      (COND
       ((ATOM RHS)
        (COND
         ((NOT (ATSOC RHS TALP_LANG*))
          (COND ((TALP_TELP RHS LHS) (RETURN (NEQ OP 'EQUAL)))
                (T (RETURN (LIST OP (TALP_SIMPLT LHS) RHS)))))
         (T (RETURN (NEQ OP 'EQUAL)))))
       (T
        (PROGN
         (SETQ RESULT (EQ (CAR LHS) (CAR RHS)))
         (SETQ LHS (CDR LHS))
         (SETQ RHS (CDR RHS))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND LHS RHS RESULT)) (RETURN NIL)))
           (PROGN
            (SETQ RESULT (TALP_SIMPLAT2 'EQUAL (CAR LHS) (CAR RHS)))
            (COND
             ((AND RESULT (NEQ RESULT T))
              (COND ((EQ OP 'EQUAL) (SETQ LIST (CONS RESULT LIST)))
                    (T (SETQ LIST (CONS (CONS 'NEQ (CDR RESULT)) LIST))))))
            (SETQ LHS (CDR LHS))
            (SETQ RHS (CDR RHS))
            NIL)
           (GO WHILELABEL))
         (COND
          ((AND RESULT LIST)
           (COND
            ((CDR LIST)
             (COND ((EQ OP 'EQUAL) (RETURN (CONS 'AND LIST)))
                   (T (RETURN (CONS 'OR LIST)))))
            (T (RETURN (CAR LIST)))))
          ((EQ OP 'EQUAL) (RETURN RESULT)) (T (RETURN (NOT RESULT))))))))) 
(PUT 'TALP_SIMPLATAT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SIMPLATAT 'DEFINED-ON-LINE '169) 
(PUT 'TALP_SIMPLATAT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLATAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLATAT (OP LHS RHS)
    (COND ((EQ LHS RHS) (EQ OP 'EQUAL))
          ((NOT (ATSOC LHS TALP_LANG*))
           (COND ((ATOM RHS) (LIST OP LHS RHS))
                 ((TALP_TELP LHS RHS) (NEQ OP 'EQUAL)) (T (LIST OP LHS RHS))))
          ((ATOM RHS)
           (COND ((NOT (ATSOC RHS TALP_LANG*)) (LIST OP LHS (TALP_SIMPLT RHS)))
                 (T (NEQ OP 'EQUAL))))
          (T (NEQ OP 'EQUAL)))) 
(PUT 'TALP_SIMPLT 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_SIMPLT 'DEFINED-ON-LINE '199) 
(PUT 'TALP_SIMPLT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_SIMPLT (TERM) (TALP_SIMPLT1 TERM NIL)) 
(PUT 'TALP_SIMPLT1 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SIMPLT1 'DEFINED-ON-LINE '205) 
(PUT 'TALP_SIMPLT1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_SIMPLT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SIMPLT1 (TERM STACK)
    (PROG (ARG)
      (COND
       ((TALP_INVP TERM)
        (PROGN
         (SETQ ARG (CADR TERM))
         (COND
          ((AND (ATOM ARG) (NOT (ATSOC ARG TALP_LANG*)))
           (PROGN
            (COND
             (STACK
              (PROG ()
               WHILELABEL
                (COND ((NOT STACK) (RETURN NIL)))
                (PROGN
                 (SETQ TERM (CONS (STACK_TOP STACK) (LIST TERM)))
                 (SETQ STACK (STACK_POP STACK)))
                (GO WHILELABEL))))
            (RETURN TERM)))
          ((TALP_INVP ARG)
           (RETURN (TALP_SIMPLT1 ARG (STACK_PUSH (CAR TERM) STACK))))
          ((EQCAR ARG (TALP_INVF TERM))
           (RETURN (TALP_SIMPLT1 (NTH (CDR ARG) (TALP_INVN TERM)) STACK)))
          (T (RETURN (TALP_SIMPLT1 ARG STACK))))))
       (T
        (PROGN
         (COND
          ((PAIRP TERM)
           (SETQ TERM
                   (CONS (CAR TERM)
                         (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                           (SETQ ARG (CDR TERM))
                           (COND ((NULL ARG) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (ARG)
                                               (TALP_SIMPLT1 ARG NIL))
                                             (CAR ARG))
                                            NIL)))
                          LOOPLABEL
                           (SETQ ARG (CDR ARG))
                           (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ARG) (TALP_SIMPLT1 ARG NIL))
                                     (CAR ARG))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))
         (COND
          (STACK
           (RETURN
            (TALP_SIMPLT1 (CONS (STACK_TOP STACK) (LIST TERM))
             (STACK_POP STACK))))
          ((TALP_INVP TERM) (RETURN (CAR TERM))) (T (RETURN TERM)))))))) 
(PUT 'TALP_TELP 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_TELP 'DEFINED-ON-LINE '250) 
(PUT 'TALP_TELP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'TALP_TELP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_TELP (VAR TERM)
    (PROG (FLAG)
      (COND ((ATOM TERM) (RETURN (EQ VAR TERM)))
            ((NOT (TALP_INVP TERM))
             (PROGN
              (SETQ TERM (CDR TERM))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND (NOT FLAG) TERM)) (RETURN NIL)))
                (PROGN
                 (SETQ FLAG (TALP_TELP VAR (CAR TERM)))
                 (SETQ TERM (CDR TERM)))
                (GO WHILELABEL))
              (RETURN FLAG)))))) 
(PUT 'STACK_NEW 'NUMBER-OF-ARGS 0) 
(PUT 'STACK_NEW 'DEFINED-ON-LINE '268) 
(PUT 'STACK_NEW 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'STACK_NEW 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE STACK_NEW NIL NIL) 
(PUT 'STACK_PUSH 'NUMBER-OF-ARGS 2) 
(PUT 'STACK_PUSH 'DEFINED-ON-LINE '272) 
(PUT 'STACK_PUSH 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'STACK_PUSH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STACK_PUSH (X S) (CONS X S)) 
(PUT 'STACK_TOP 'NUMBER-OF-ARGS 1) 
(PUT 'STACK_TOP 'DEFINED-ON-LINE '277) 
(PUT 'STACK_TOP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'STACK_TOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STACK_TOP (S) (CAR S)) 
(PUT 'STACK_POP 'NUMBER-OF-ARGS 1) 
(PUT 'STACK_POP 'DEFINED-ON-LINE '282) 
(PUT 'STACK_POP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'STACK_POP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STACK_POP (S) (CDR S)) 
(PUT 'STACK_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'STACK_PRINT 'DEFINED-ON-LINE '287) 
(PUT 'STACK_PRINT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSIAT.RED) 
(PUT 'STACK_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STACK_PRINT (S)
    (PROG (X)
      (SETQ X S)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X) (PRIN2T X)) (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(ENDMODULE) 
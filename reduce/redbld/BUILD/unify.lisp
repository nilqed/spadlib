(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'UNIFY)) 
(FLUID '(*SEMANTIC SUBSTITUTION)) 
(FLUID '(SYMM OP R P I UPB IDENTITY EXPAND ACONTRACT MCONTRACT COMB)) 
(SWITCH (LIST (LIST 'EQUAL 'SEMANTIC 'ON))) 
(PUT 'AMATCH 'NUMBER-OF-ARGS 4) 
(PUT 'AMATCH 'DEFINED-ON-LINE '41) 
(PUT 'AMATCH 'DEFINED-IN-FILE 'PM/UNIFY.RED) 
(PUT 'AMATCH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE AMATCH (R P SUCHL PMSTACK)
    (COND ((ATOM R) (UNIFY NIL (LIST R) (LIST P) SUCHL PMSTACK))
          ((NOT (OR (ATOM P) (NEQ (CAR R) (CAR P))))
           (UNIFY (CAR R) (CDR R) (CDR P) SUCHL PMSTACK))
          (((LAMBDA (U) (AND (NOT (ATOM U)) (EQ (CAR U) 'SUCH-THAT))) R)
           (AMATCH (CADR R) P (CONS (CADDR R) SUCHL) PMSTACK))
          (*SEMANTIC (RESUME (CONS (LIST 'EQUAL R P) SUCHL) PMSTACK)))) 
(PUT 'SUSPEND 'NUMBER-OF-ARGS 5) 
(PUT 'SUSPEND 'DEFINED-ON-LINE '48) 
(PUT 'SUSPEND 'DEFINED-IN-FILE 'PM/UNIFY.RED) 
(PUT 'SUSPEND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUSPEND (OP R P SUCHL PMSTACK)
    (AMATCH (CAR R) (CAR P) SUCHL
     (CONS (LIST (CONS OP (CDR R)) (CONS OP (CDR P))) PMSTACK))) 
(PUT 'RESUME 'NUMBER-OF-ARGS 2) 
(PUT 'RESUME 'DEFINED-ON-LINE '52) 
(PUT 'RESUME 'DEFINED-IN-FILE 'PM/UNIFY.RED) 
(PUT 'RESUME 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RESUME (SUCHL PMSTACK)
    (COND (PMSTACK (AMATCH (CAAR PMSTACK) (CADAR PMSTACK) SUCHL (CDR PMSTACK)))
          ((EQ (CHK SUCHL) T) (BSUBS SUBSTITUTION)))) 
(PUT 'UNIFY 'NUMBER-OF-ARGS 5) 
(PUT 'UNIFY 'DEFINED-ON-LINE '57) 
(PUT 'UNIFY 'DEFINED-IN-FILE 'PM/UNIFY.RED) 
(PUT 'UNIFY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE UNIFY (OP R P SUCHL PMSTACK)
    (COND ((AND (NULL R) (NULL P)) (RESUME SUCHL PMSTACK))
          ((NULL R)
           (PROGN
            (PRIN2 "UNIFY:pattern over-run for function ")
            (PRINT OP)
            NIL))
          ((AND (NULL P)
                (NOT
                 (OR (GET OP 'IDENTITY)
                     ((LAMBDA (U) (AND (ATOM U) (GET U 'MGEN))) (CAR R)))))
           NIL)
          (T
           (PROG (MMATCH ST ARG SYMM COMB IDENTITY MCONTRACT ACONTRACT EXPAND I
                  UPB)
             (SETQ I 0)
             (SETQ UPB 0)
             (COND
              ((EQ ((LAMBDA (X) (COND (X (CAR X)))) (GET (CAR R) 'BINDING))
                   'UNBOUND)
               (SETQ SUCHL
                       (CONS
                        ((LAMBDA (U)
                           (AND (ATOM U) (OR (GET U 'GEN) (MGENP U))))
                         (CAR R))
                        SUCHL))))
             (INITARG P)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND (NOT MMATCH) (SETQ ARG (NEXTARG P)))) (RETURN NIL)))
               (PROG ()
                 (COND
                  ((NOT (ATOM (CAR R)))
                   (SETQ MMATCH (SUSPEND OP R ARG SUCHL PMSTACK)))
                  ((EQ ((LAMBDA (X) (COND (X (CAR X)))) (GET (CAR R) 'BINDING))
                       'UNBOUND)
                   (PROG ()
                     ((LAMBDA (G135)
                        (PUT G135 'BINDING
                             (CONS (CAR ARG) (GET G135 'BINDING))))
                      (CAR R))
                     (COND
                      ((SETQ ST (CHK SUCHL))
                       (SETQ MMATCH (UNIFY OP (CDR R) (CDR ARG) ST PMSTACK))))
                     ((LAMBDA (U) (PUT U 'BINDING (CDR (GET U 'BINDING))))
                      (CAR R))))
                  (((LAMBDA (G137)
                      ((LAMBDA (X)
                         (EQUAL (COND ((AND X (NEQ X 'UNBOUND)) X) (T G137))
                                (CAR ARG)))
                       ((LAMBDA (X) (COND (X (CAR X)))) (GET G137 'BINDING))))
                    (CAR R))
                   (SETQ MMATCH (UNIFY OP (CDR R) (CDR ARG) SUCHL PMSTACK)))))
               (GO WHILELABEL))
             (RETURN MMATCH))))) 
(ENDMODULE) 
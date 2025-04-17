(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CONSTRE)) 
(PUT 'CONSTANTRE 'NUMBER-OF-ARGS 5) 
(FLAG '(CONSTANTRE) 'OPFN) 
(PUT 'CONSTANTRE 'DEFINED-ON-LINE '28) 
(PUT 'CONSTANTRE 'DEFINED-IN-FILE 'SPECFN/CONSTRE.RED) 
(PUT 'CONSTANTRE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONSTANTRE (CAP_R LEADCOEFF DFFPOINTER K X)
    (PROG (DENR FRACT II M0 M1 C0 CK S C DF2 Q R2 LTERM NN S0 LEADCOEFF2)
      (SETQ DENR (AEVAL (LIST 'SOLVE LEADCOEFF K)))
      (SETQ M0 (AEVAL (LIST 'LIST)))
      (PROG (XX)
        (SETQ XX (GETRLIST (AEVAL DENR)))
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (COND
            ((BOOLVALUE* (REVALX (LIST 'TYPE_RATIONAL (LIST 'RHS XX))))
             (SETQ M0 (AEVAL (LIST 'CONS (LIST 'PLUS (LIST 'RHS XX) 1) M0))))))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (COND
       ((NOT (EVALEQUAL (AEVAL M0) (AEVAL (LIST 'LIST))))
        (SETQ M0 (AEVAL (LIST 'MAX M0))))
       (T (SETQ M0 (AEVAL 0))))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL ">>> m0 = ") NIL 'FIRST)
          (ASSGNPRI (AEVAL M0) NIL 'LAST))
         (ASSGNPRI (AEVAL "RE is constant") NIL 'ONLY)
         (PROGN
          (ASSGNPRI (AEVAL "RE: for all k >= ") NIL 'FIRST)
          (ASSGNPRI (AEVAL M0) NIL NIL)
          (ASSGNPRI (AEVAL ": a (k + 1) = ") NIL NIL)
          (ASSGNPRI (AEVAL (LIST 'TIMES CAP_R (LIST 'A K))) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "leadcoeff := ") NIL 'FIRST)
          (ASSGNPRI (AEVAL LEADCOEFF) NIL 'LAST))
         (AEVAL 'NIL))))
      (SETQ FRACT (AEVAL (LIST 'LIST)))
      (PROG (XX)
        (SETQ XX (GETRLIST (AEVAL DENR)))
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (COND
            ((BOOLVALUE* (REVALX (LIST 'TYPE_FRACTION (LIST 'RHS XX))))
             (SETQ FRACT
                     (AEVAL (LIST 'CONS (LIST 'DEN (LIST 'RHS XX)) FRACT))))))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (COND
       ((NOT (EVALEQUAL (AEVAL FRACT) (AEVAL (LIST 'LIST))))
        (PROGN
         (SETQ Q (AEVAL (LIST 'FIRST FRACT)))
         (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
               (AEVAL
                (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT X Q))
                      (LIST 'DFF DFFPOINTER))))
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "RE modified to nn= ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'QUOTIENT K Q)) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "=> f := ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))) NIL
                       'LAST)))))
         (SETQ S
                 (AEVAL
                  (LIST 'CONSTANTRE
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'QUOTIENT K Q)) CAP_R)
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'QUOTIENT K Q))
                              LEADCOEFF)
                        (LIST 'PLUS DFFPOINTER 10) K X)))
         (RETURN
          (AEVAL
           (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT X (LIST 'QUOTIENT 1 Q))) S)))
         (AEVAL 'NIL))))
      (COND
       ((EVALLESSP (AEVAL M0) 0)
        (PROGN
         (SETQ NN (AEVAL (LIST 'PLUS (LIST 'MINUS M0) 1)))
         (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
               (SETQ DF2
                       (AEVAL
                        (LIST 'TIMES (LIST 'EXPT X NN)
                              (LIST 'DFF DFFPOINTER)))))
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "working with ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'EXPT X NN)) NIL NIL)
             (ASSGNPRI (AEVAL "*f") NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "=> f :=") NIL 'FIRST)
             (ASSGNPRI (AEVAL DF2) NIL 'LAST)))))
         (SETQ S
                 (AEVAL
                  (LIST 'CONSTANTRE
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K NN))
                              CAP_R)
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K NN))
                              LEADCOEFF)
                        (LIST 'PLUS DFFPOINTER 10) K X)))
         (RETURN (AEVAL (LIST 'UPDATE_COEFF S X (LIST 'MINUS NN))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL M0) 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (ASSGNPRI (AEVAL "PS does not exist") NIL 'ONLY)))
         (RETURN (AEVAL 'FAILED)))))
      (COND
       ((EVALGREATERP (AEVAL M0) 0)
        (PROGN
         (SETQ M1 (AEVAL (LIST 'LIST)))
         (PROG (XX)
           (SETQ XX (GETRLIST (AEVAL DENR)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (COND
               ((BOOLVALUE* (REVALX (LIST 'TYPE_RATIONAL (LIST 'RHS XX))))
                (SETQ M1
                        (AEVAL
                         (LIST 'APPEND
                               (LIST 'LIST (LIST 'PLUS (LIST 'RHS XX) 1))
                               M1))))))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (SETQ M1 (AEVAL (LIST 'MIN M1)))
         (COND
          ((EVALLESSP (AEVAL M1) 0)
           (PROGN
            (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
                  (SETQ DF2
                          (AEVAL
                           (LIST 'TIMES (LIST 'EXPT X (LIST 'MINUS M1))
                                 (LIST 'DFF DFFPOINTER)))))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "working with ") NIL 'FIRST)
                (ASSGNPRI (AEVAL (LIST 'EXPT X (LIST 'MINUS M1))) NIL NIL)
                (ASSGNPRI (AEVAL "*f") NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL "=> f :=") NIL 'FIRST)
                (ASSGNPRI (AEVAL DF2) NIL 'LAST)))))
            (SETQ S
                    (AEVAL
                     (LIST 'CONSTANTRE
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K M1)) CAP_R)
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K M1))
                                 LEADCOEFF)
                           (LIST 'PLUS DFFPOINTER 10) K X)))
            (RETURN (AEVAL (LIST 'UPDATE_COEFF S X M1))))))
         (AEVAL 'NIL))))
      (SETQ S (AEVAL 0))
      (SETQ S0 (AEVAL 0))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* M0) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((EVALGREATERP I (AEVAL* M1))
           (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER I))
                 (AEVAL*
                  (LIST 'DF
                        (LIST 'DFF (LIST 'PLUS DFFPOINTER (DIFFERENCE I 1)))
                        X)))))
         (SETQ C0
                 (AEVAL*
                  (LIST 'LIMIT (LIST 'DFF (LIST 'PLUS DFFPOINTER I)) X 0)))
         (COND
          ((AND (NOT (EVALNUMBERP (AEVAL* C0)))
                (EVALEQUAL (AEVAL* (LIST 'PART C0 0)) (AEVAL* 'LIMIT)))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL* "Could not find the limit of: ") NIL 'FIRST)
             (ASSGNPRI (AEVAL* (LIST 'DFF (LIST 'PLUS DFFPOINTER I))) NIL NIL)
             (ASSGNPRI (AEVAL* ",") NIL NIL)
             (ASSGNPRI (AEVAL* X) NIL NIL)
             (ASSGNPRI (AEVAL* ",") NIL NIL)
             (ASSGNPRI 0 NIL 'LAST))
            (AEVAL* (REDERR (REVALX "problem using limit operator")))))
          (T
           (PROGN
            (SETQ C0 (AEVAL* (LIST 'QUOTIENT C0 (LIST 'FACTORIAL I))))
            (SETQ S (AEVAL* (LIST 'PLUS S (LIST 'TIMES C0 (LIST 'EXPT X I)))))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL* " S = ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* S) NIL 'LAST))))
            (AEVAL* 'NIL)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL S)))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(COND ((NULL (GETD 'MKUNITP)) (IN (LIST "perm.red")))) 
(COND ((NULL (GETD 'PV_ADD)) (IN (LIST "pvector.red")))) 
NIL 
(MODULE (LIST 'BASIS)) 
(GLOBAL '(*BASIS)) 
(PUT 'SIEVE_PV 'NUMBER-OF-ARGS 2) 
(PUT 'SIEVE_PV 'DEFINED-ON-LINE '51) 
(PUT 'SIEVE_PV 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'SIEVE_PV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIEVE_PV (V B) (SIEVE_PV0 V B T)) 
(PUT 'SIEVE_PV0 'NUMBER-OF-ARGS 3) 
(PUT 'SIEVE_PV0 'DEFINED-ON-LINE '54) 
(PUT 'SIEVE_PV0 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'SIEVE_PV0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIEVE_PV0 (V B NORM)
    (COND ((NULL V) NIL)
          (T
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND B (GREATERP (CDAAR B) (CDAR V)))) (RETURN NIL)))
              (SETQ B (CDR B))
              (GO WHILELABEL))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND V B)) (RETURN NIL)))
              (PROGN (SETQ V (REDUCE_PV0 V (CAR B) NORM)) (SETQ B (CDR B)) NIL)
              (GO WHILELABEL))
            V)))) 
(PUT 'REDUCE_PV 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCE_PV 'DEFINED-ON-LINE '71) 
(PUT 'REDUCE_PV 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'REDUCE_PV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCE_PV (V Q) (REDUCE_PV0 V Q T)) 
(GLOBAL '(PV_DEN)) 
(PUT 'REDUCE_PV0 'NUMBER-OF-ARGS 3) 
(PUT 'REDUCE_PV0 'DEFINED-ON-LINE '76) 
(PUT 'REDUCE_PV0 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'REDUCE_PV0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDUCE_PV0 (V Q NORM)
    (COND ((NULL Q) V) ((NULL V) NIL)
          (T
           (PROG (W K)
             (SETQ W V)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND W Q (GREATERP (CDAR W) (CDAR Q)))) (RETURN NIL)))
               (SETQ W (CDR W))
               (GO WHILELABEL))
             (COND
              ((AND W Q (EQUAL (CDAR Q) (CDAR W)))
               (PROGN
                (SETQ K (LCM (CAAR W) (CAAR Q)))
                (SETQ V
                        (PV_ADD (PV_MULTC V (QUOTIENT K (CAAR W)))
                         (PV_MULTC Q (MINUS (QUOTIENT K (CAAR Q))))))
                (COND
                 ((NULL NORM)
                  (SETQ PV_DEN (TIMES PV_DEN (QUOTIENT K (CAAR W)))))
                 (T (SETQ PV_DEN 1)))
                NIL)))
             (RETURN V))))) 
(PUT 'INSERT_PV 'NUMBER-OF-ARGS 2) 
(PUT 'INSERT_PV 'DEFINED-ON-LINE '100) 
(PUT 'INSERT_PV 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'INSERT_PV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSERT_PV (PV BL)
    ((LAMBDA (X) (COND ((NULL X) BL) (T (INSERT_PV1 (PV_RENORM X) BL NIL))))
     (SIEVE_PV PV BL))) 
(PUT 'INSERT_PV1 'NUMBER-OF-ARGS 3) 
(PUT 'INSERT_PV1 'DEFINED-ON-LINE '108) 
(PUT 'INSERT_PV1 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'INSERT_PV1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSERT_PV1 (PV BL BL1)
    (COND
     ((NULL BL) (COND ((NULL PV) (REVERSIP BL1)) (T (REVERSIP (CONS PV BL1)))))
     ((NULL PV) (INSERT_PV1 NIL (CDR BL) (CONS (CAR BL) BL1)))
     ((GREATERP (CDAAR BL) (CDAR PV))
      (INSERT_PV1 PV (CDR BL) (CONS (PV_RENORM (REDUCE_PV (CAR BL) PV)) BL1)))
     (T (INSERT_PV1 NIL BL (CONS PV BL1))))) 
(PUT 'INSERT_PV_ 'NUMBER-OF-ARGS 2) 
(PUT 'INSERT_PV_ 'DEFINED-ON-LINE '119) 
(PUT 'INSERT_PV_ 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'INSERT_PV_ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSERT_PV_ (V B)
    (COND ((NULL V) B) ((NULL B) (LIST V))
          (T
           (PROG (B1 W)
             (SETQ V (PV_RENORM (SIEVE_PV V B)))
             (COND ((NULL V) (RETURN B)))
             (SETQ B1 B)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND (CDR B1) (GREATERP (CDAAR B1) (CDAR V))))
                 (RETURN NIL)))
               (PROGN
                (RPLACA B1 (PV_RENORM (REDUCE_PV (CAR B1) V)))
                (SETQ B1 (CDR B1))
                NIL)
               (GO WHILELABEL))
             (COND
              ((GREATERP (CDAAR B1) (CDAR V))
               (PROGN
                (RPLACA B1 (PV_RENORM (REDUCE_PV (CAR B1) V)))
                (RPLACD B1 (CONS V (CDR B1)))
                NIL))
              (T
               (PROGN
                (SETQ W (CONS (CAR B1) (CDR B1)))
                (RPLACD (RPLACA B1 V) W)
                NIL)))
             (RETURN B))))) 
(REMPROP 'BASIS 'STAT) 
(PUT 'UPDATE_PV 'NUMBER-OF-ARGS 2) 
(PUT 'UPDATE_PV 'DEFINED-ON-LINE '146) 
(PUT 'UPDATE_PV 'DEFINED-IN-FILE 'ATENSOR/BASIS.RED) 
(PUT 'UPDATE_PV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPDATE_PV (V B)
    (COND ((NULL V) NIL)
          (T
           (PROG (R W)
             (COND
              ((NULL (EQ (CAR B) '*BASIS))
               (REDERR (LIST 'UPDATEV ": 2-nd arg. is not a basis."))))
             (SETQ R V)
             (PROG ()
              WHILELABEL
               (COND ((NOT V) (RETURN NIL)))
               (PROGN
                (SETQ W (MEMBER (CDAR V) (CDR B)))
                (COND (W (RPLACD (CAR V) (CAR W)))
                      (T (RPLACD B (CONS (CDAR V) (CDR B)))))
                (SETQ V (CDR V))
                NIL)
               (GO WHILELABEL))
             (RETURN R))))) 
(ENDMODULE) 
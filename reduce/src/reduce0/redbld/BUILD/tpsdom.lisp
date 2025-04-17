(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TPSDOM)) 
(FLUID '(|PS:EXP-LIM| |PS:MAX-ORDER|)) 
(GLOBAL '(DOMAINLIST*)) 
(SETQ DOMAINLIST* (UNION '(|:PS:|) DOMAINLIST*)) 
(PUT 'TPS 'TAG '|:PS:|) 
(PUT '|:PS:| 'DNAME 'TPS) 
(FLAG '(|:PS:|) 'FIELD) 
(PUT '|:PS:| 'I2D 'I2PS) 
(PUT '|:PS:| 'MINUSP '|PS:MINUSP:|) 
(PUT '|:PS:| 'PLUS '|PS:PLUS:|) 
(PUT '|:PS:| 'TIMES '|PS:TIMES:|) 
(PUT '|:PS:| 'DIFFERENCE '|PS:DIFFERENCE:|) 
(PUT '|:PS:| 'QUOTIENT '|PS:QUOTIENT:|) 
(PUT '|:PS:| 'ZEROP '|PS:ZEROP:|) 
(PUT '|:PS:| 'ONEP '|PS:ONEP:|) 
(PUT '|:PS:| 'PREPFN '|PS:PREPFN:|) 
(PUT '|:PS:| 'PRIFN '|PS:PRINT0|) 
(PUT '|:PS:| 'PPRIFN '|PS:PRINT|) 
(PUT '|:PS:| 'INTEQUIVFN '|PSINTEQUIV:|) 
(PUT '|:PS:| 'EXPT '|PS:EXPT:|) 
(PUT '|:PS:| 'ABS '|PS:ABS:|) 
(PUT '|:PS:| '|:MOD:| (MKDMODERR '|:PS:| '|:MOD:|)) 
(PUT '|:RN:| '|:PS:| '*D2PS) 
(PUT '|:FT:| '|:PS:| '*D2PS) 
(PUT '|:BF:| '|:PS:| '*D2PS) 
(PUT '|:GI:| '|:PS:| '*D2PS) 
(PUT '|:GF:| '|:PS:| '*D2PS) 
(PUT '|:RD:| '|:PS:| '*D2PS) 
(PUT '|:CR:| '|:PS:| '*D2PS) 
(PUT '|:CRN:| '|:PS:| '*D2PS) 
(PUT '|PSINTEQUIV:| 'NUMBER-OF-ARGS 1) 
(PUT '|PSINTEQUIV:| 'DEFINED-ON-LINE '70) 
(PUT '|PSINTEQUIV:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PSINTEQUIV:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PSINTEQUIV:| (U)
    (COND
     ((OR (IDP (CDR U)) (|PS:DEPVAR| U)
          (NEQ (CDR (SETQ U (|PS:GET-TERM| U 0))) 1))
      NIL)
     (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ U (CAR U)))
      (COND ((ATOM U) (COND ((NULL U) 0) (T U)))
            (T
             ((LAMBDA (X) (COND ((AND X (SETQ X (APPLY1 X U))) X) (T NIL)))
              (GET (CAR U) 'INTEQUIVFN)))))
     (T NIL))) 
(DE I2PS (U) U) 
(PUT 'I2PS 'NUMBER-OF-ARGS 1) 
(PUT 'I2PS 'DEFINED-ON-LINE '79) 
(PUT 'I2PS 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT 'I2PS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'I2PS 'INLINE '(LAMBDA (U) U)) 
(PUT '*D2PS 'NUMBER-OF-ARGS 1) 
(PUT '*D2PS 'DEFINED-ON-LINE '82) 
(PUT '*D2PS 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '*D2PS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *D2PS (U) (MAKE-CONSTANTPS (CONS U 1) (PREPSQXX (CONS U 1)) NIL)) 
(DE |PS:MINUSP:| (U) NIL) 
(PUT '|PS:MINUSP:| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:MINUSP:| 'DEFINED-ON-LINE '95) 
(PUT '|PS:MINUSP:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:MINUSP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|PS:MINUSP:| 'INLINE '(LAMBDA (U) NIL)) 
(DE |PS:PLUS:| (U V) (|PS:OPERATOR:| 'PLUS U V)) 
(PUT '|PS:PLUS:| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:PLUS:| 'DEFINED-ON-LINE '98) 
(PUT '|PS:PLUS:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:PLUS:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC '|PS:PLUS:| 'INLINE '(LAMBDA (U V) (|PS:OPERATOR:| 'PLUS U V))) 
(DE |PS:DIFFERENCE:| (U V) (|PS:OPERATOR:| 'DIFFERENCE U V)) 
(PUT '|PS:DIFFERENCE:| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:DIFFERENCE:| 'DEFINED-ON-LINE '101) 
(PUT '|PS:DIFFERENCE:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:DIFFERENCE:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC '|PS:DIFFERENCE:| 'INLINE
      '(LAMBDA (U V) (|PS:OPERATOR:| 'DIFFERENCE U V))) 
(DE |PS:TIMES:| (U V) (|PS:OPERATOR:| 'TIMES U V)) 
(PUT '|PS:TIMES:| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:TIMES:| 'DEFINED-ON-LINE '104) 
(PUT '|PS:TIMES:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:TIMES:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC '|PS:TIMES:| 'INLINE '(LAMBDA (U V) (|PS:OPERATOR:| 'TIMES U V))) 
(DE |PS:QUOTIENT:| (U V) (|PS:OPERATOR:| 'QUOTIENT U V)) 
(PUT '|PS:QUOTIENT:| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:QUOTIENT:| 'DEFINED-ON-LINE '107) 
(PUT '|PS:QUOTIENT:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:QUOTIENT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC '|PS:QUOTIENT:| 'INLINE '(LAMBDA (U V) (|PS:OPERATOR:| 'QUOTIENT U V))) 
(PUT '|PS:DIFF:| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:DIFF:| 'DEFINED-ON-LINE '111) 
(PUT '|PS:DIFF:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:DIFF:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:DIFF:| (U V)
    ((LAMBDA (DERIV)
       (CONS
        (COND
         ((IDP DERIV)
          (MAKE-PS-ID DERIV (|PS:DEPVAR| U) (|PS:EXPANSION-POINT| U)))
         ((NUMBERP DERIV) (COND ((ZEROP DERIV) NIL) (T DERIV)))
         (T
          (PROGN
           (SETQ U
                   (MAKE-PS (LIST 'DF U V) DERIV (|PS:DEPVAR| U)
                    (|PS:EXPANSION-POINT| U)))
           (|PS:FIND-ORDER| U)
           U)))
        1))
     (PREPSQXX (SIMP* (LIST 'DF (|PS:VALUE| U) V))))) 
(PUT '|:PS:| 'DOMAIN-DIFF-FN '|PS:DIFF:|) 
(PUT '|PS:DEPENDS-FN| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:DEPENDS-FN| 'DEFINED-ON-LINE '127) 
(PUT '|PS:DEPENDS-FN| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:DEPENDS-FN| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:DEPENDS-FN| (U V) (DEPENDS (|PS:VALUE| U) V)) 
(PUT '|:PS:| 'DOMAIN-DEPENDS-FN '|PS:DEPENDS-FN|) 
(PUT '|PS:OPERATOR:| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:OPERATOR:| 'DEFINED-ON-LINE '132) 
(PUT '|PS:OPERATOR:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:OPERATOR:| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:OPERATOR:| (OP U V)
    (PROG (VALUE X X0 Y Y0)
      (COND
       ((NOT (AND (PAIRP V) (EQUAL (CAR V) '|:PS:|)))
        (PROGN (SETQ X (|PS:DEPVAR| U)) (SETQ X0 (|PS:EXPANSION-POINT| U))))
       ((NOT (AND (PAIRP U) (EQUAL (CAR U) '|:PS:|)))
        (PROGN (SETQ X (|PS:DEPVAR| V)) (SETQ X0 (|PS:EXPANSION-POINT| V))))
       (T
        (PROGN
         (SETQ X (|PS:DEPVAR| U))
         (SETQ Y (|PS:DEPVAR| V))
         (SETQ X0 (|PS:EXPANSION-POINT| U))
         (SETQ Y0 (|PS:EXPANSION-POINT| V))
         (COND
          ((AND X0 Y0)
           (COND ((AND (EQUAL X0 Y0) (EQUAL X Y)) NIL)
                 ((NEQ X0 Y0)
                  (RERROR 'TPS 29
                          (LIST "power series expansion points differ in "
                                OP)))
                 (T
                  (RERROR 'TPS 30
                          (LIST "power series dependent variables differ in "
                                OP))))))
         (COND ((NULL X0) (SETQ X0 Y0)) ((NULL Y0) (SETQ Y0 X0)))
         NIL)))
      (COND
       ((NULL X0)
        (PROGN
         (COND
          ((AND X Y)
           (COND ((EQUAL X Y) NIL)
                 (T
                  (RERROR 'TPS 31
                          (LIST "power series dependent variables differ in "
                                OP)))))
          (Y (SETQ X Y)))
         (COND
          ((AND (PAIRP U) (EQUAL (CAR U) '|:PS:|)) (SETQ U (|PS:VALUE| U))))
         (COND
          ((AND (PAIRP V) (EQUAL (CAR V) '|:PS:|)) (SETQ V (|PS:VALUE| V))))
         (SETQ VALUE (SIMP* (LIST OP U V)))
         (COND
          ((AND (EQUAL (CDR VALUE) 1)
                (OR (ATOM (CAR VALUE)) (ATOM (CAR (CAR VALUE)))))
           (RETURN (CAR VALUE)))
          (T (RETURN (MAKE-CONSTANTPS VALUE (PREPSQXX VALUE) X)))))))
      (COND
       ((AND X Y)
        (COND ((EQUAL X Y) NIL)
              (T
               (RERROR 'TPS 32
                       (LIST "power series dependent variables differ in "
                             OP)))))
       (Y (SETQ X Y)))
      (SETQ VALUE (SIMP* (LIST OP (|PS:VALUE| U) (|PS:VALUE| V))))
      (COND
       ((AND (EQUAL (CDR VALUE) 1)
             (OR (ATOM (CAR VALUE)) (ATOM (CAR (CAR VALUE)))))
        (RETURN (CAR VALUE))))
      (SETQ U (MAKE-PS (LIST OP U V) (PREPSQXX VALUE) X X0))
      (|PS:FIND-ORDER| U)
      (RETURN U))) 
(DE |PS:ZEROP:| (U) ((LAMBDA (V) (AND (NUMBERP V) (ZEROP V))) (|PS:VALUE| U))) 
(PUT '|PS:ZEROP:| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:ZEROP:| 'DEFINED-ON-LINE '184) 
(PUT '|PS:ZEROP:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:ZEROP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|PS:ZEROP:| 'INLINE
      '(LAMBDA (U) ((LAMBDA (V) (AND (NUMBERP V) (ZEROP V))) (|PS:VALUE| U)))) 
(DE |PS:ONEP:| (U) (ONEP (|PS:VALUE| U))) 
(PUT '|PS:ONEP:| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:ONEP:| 'DEFINED-ON-LINE '187) 
(PUT '|PS:ONEP:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:ONEP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|PS:ONEP:| 'INLINE '(LAMBDA (U) (ONEP (|PS:VALUE| U)))) 
(DE |PS:PREPFN:| (U) U) 
(PUT '|PS:PREPFN:| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:PREPFN:| 'DEFINED-ON-LINE '190) 
(PUT '|PS:PREPFN:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:PREPFN:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|PS:PREPFN:| 'INLINE '(LAMBDA (U) U)) 
(PUT '|PS:ABS:| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:ABS:| 'DEFINED-ON-LINE '214) 
(PUT '|PS:ABS:| 'DEFINED-IN-FILE 'TPS/TPSDOM.RED) 
(PUT '|PS:ABS:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:ABS:| (U)
    (COND (*COMPLEX (LIST (CONS (GETPOWER (FKERN (LIST 'ABS U)) 1) 1)))
          (T
           (PROG (ORD FIRST ABSTERM1 PSPART DIVISOR REST ABOUT)
             (SETQ ORD (|PS:ORDER| U))
             (SETQ ABOUT (|PS:EXPANSION-POINT| U))
             (SETQ FIRST (PREPSQXX (|PS:GET-TERM| U ORD)))
             (SETQ ABSTERM1 (SIMP (LIST 'ABS FIRST)))
             (SETQ DIVISOR
                     (COND
                      ((OR (EQUAL ABOUT 0) (EQUAL ABOUT '|PS:INF|))
                       (|PS:DEPVAR| U))
                      (T (LIST 'PLUS (|PS:DEPVAR| U) (LIST 'MINUS ABOUT)))))
             (COND
              ((DEPENDS ABSTERM1 'ABS)
               (PROGN
                (SETQ PSPART
                        (MAKE-CONSTANTPS (|PS:GET-TERM| U ORD) FIRST
                         (|PS:DEPVAR| U)))
                (COND
                 ((EVENP ORD)
                  (PROGN
                   (SETQ PSPART (LIST 'QUOTIENT U PSPART))
                   (SETQ REST
                           (LIST
                            (CONS (GETPOWER (FKERN (LIST 'ABS FIRST)) 1) 1)))))
                 (T
                  (PROGN
                   (SETQ PSPART
                           (LIST 'QUOTIENT U (LIST 'TIMES DIVISOR PSPART)))
                   (SETQ REST
                           (LIST
                            (CONS
                             (GETPOWER
                              (FKERN (LIST 'ABS (LIST 'TIMES DIVISOR FIRST)))
                              1)
                             1))))))))
              ((EQUAL ABSTERM1 (|PS:GET-TERM| U ORD))
               (COND ((EVENP ORD) (PROGN (SETQ PSPART U) (SETQ REST 1)))
                     (T
                      (PROGN
                       (SETQ PSPART (LIST 'QUOTIENT U DIVISOR))
                       (SETQ REST
                               (LIST
                                (CONS (GETPOWER (FKERN (LIST 'ABS DIVISOR)) 1)
                                      1)))))))
              ((EVENP ORD) (PROGN (SETQ PSPART (LIST 'MINUS U)) (SETQ REST 1)))
              (T
               (PROGN
                (SETQ PSPART (LIST 'QUOTIENT (LIST 'MINUS U) DIVISOR))
                (SETQ REST
                        (LIST
                         (CONS (GETPOWER (FKERN (LIST 'ABS DIVISOR)) 1) 1))))))
             (SETQ PSPART
                     (CAR
                      (SIMPPS1 PSPART (|PS:DEPVAR| U)
                               (|PS:EXPANSION-POINT| U))))
             (RETURN
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF REST PSPART))
                    (T (POLY-MULTF REST PSPART)))))))) 
(INITDMODE 'TPS) 
(ENDMODULE) 
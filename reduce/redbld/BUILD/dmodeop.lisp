(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DMODEOP)) 
(FLUID '(*NOEQUIV)) 
(PUT '|:DIFFERENCE| 'NUMBER-OF-ARGS 2) 
(PUT '|:DIFFERENCE| 'DEFINED-ON-LINE '40) 
(PUT '|:DIFFERENCE| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:DIFFERENCE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:DIFFERENCE| (U V)
    (COND ((NULL U) (|:MINUS| V)) ((NULL V) U) ((EQUAL U V) NIL)
          ((AND (ATOM U) (ATOM V)) (DIFFERENCE U V))
          (T (DCOMBINE U V 'DIFFERENCE)))) 
(PUT '|:DIVIDE| 'NUMBER-OF-ARGS 2) 
(PUT '|:DIVIDE| 'DEFINED-ON-LINE '46) 
(PUT '|:DIVIDE| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:DIVIDE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:DIVIDE| (U V)
    (COND ((NULL U) (CONS NIL NIL))
          ((NULL V) (RERROR 'POLY 202 "zero divisor"))
          ((AND (ATOM U) (ATOM V)) (DIVIDEF U V)) (T (DCOMBINE U V 'DIVIDE)))) 
(PUT 'DIVIDEF 'NUMBER-OF-ARGS 2) 
(PUT 'DIVIDEF 'DEFINED-ON-LINE '55) 
(PUT 'DIVIDEF 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT 'DIVIDEF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIVIDEF (M N)
    ((LAMBDA (X)
       (CONS (COND ((EQUAL (CAR X) 0) NIL) (T (CAR X)))
             (COND ((EQUAL (CDR X) 0) NIL) (T (CDR X)))))
     (DIVIDE M N))) 
(PUT '|:EXPT| 'NUMBER-OF-ARGS 2) 
(PUT '|:EXPT| 'DEFINED-ON-LINE '59) 
(PUT '|:EXPT| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:EXPT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:EXPT| (U N)
    (COND
     ((NULL U) (COND ((EQUAL N 0) (RERROR 'POLY 11 "0**0 formed")) (T NIL)))
     ((EQUAL N 0) 1) ((EQUAL N 1) U) ((EQUAL U 1) 1)
     ((LESSP N 0)
      (|:RECIP|
       (|:EXPT| (COND ((NOT (FIELDP U)) (MKRATNUM U)) (T U)) (MINUS N))))
     ((ATOM U) (EXPT U N))
     (T
      (PROG (V W X)
        (COND ((SETQ X (GET (CAR U) 'EXPT)) (RETURN (APPLY2 X U N))))
        (SETQ V (APPLY1 (GET (CAR U) 'I2D) 1))
        (SETQ X (GET (CAR U) 'TIMES))
       A
        (SETQ W N)
        (COND
         ((NEQ (DIFFERENCE W (TIMES 2 (SETQ N (QUOTIENT N 2)))) 0)
          (SETQ V (APPLY2 X U V))))
        (COND ((EQUAL N 0) (RETURN V)))
        (SETQ U (APPLY2 X U U))
        (GO A))))) 
(PUT '|:GCD| 'NUMBER-OF-ARGS 2) 
(PUT '|:GCD| 'DEFINED-ON-LINE '82) 
(PUT '|:GCD| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:GCD| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:GCD| (U V)
    (COND ((NULL U) V) ((NULL V) U) ((AND (ATOM U) (ATOM V)) (GCDN U V))
          ((OR (FIELDP U) (FIELDP V)) 1) (T (DCOMBINE U V 'GCD)))) 
(PUT '|:MINUS| 'NUMBER-OF-ARGS 1) 
(PUT '|:MINUS| 'DEFINED-ON-LINE '90) 
(PUT '|:MINUS| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:MINUS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:MINUS| (U)
    (COND ((NULL U) NIL) ((ATOM U) (MINUS U))
          (T
           ((LAMBDA (X)
              (COND (X (APPLY1 X U)) (T (DCOMBINE U (MINUS 1) 'TIMES))))
            (GET (CAR U) 'MINUS))))) 
(PUT '|:MINUSP| 'NUMBER-OF-ARGS 1) 
(PUT '|:MINUSP| 'DEFINED-ON-LINE '97) 
(PUT '|:MINUSP| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:MINUSP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:MINUSP| (U)
    (COND ((ATOM U) (MINUSP U)) (T (APPLY1 (GET (CAR U) 'MINUSP) U)))) 
(PUT '|:ONEP| 'NUMBER-OF-ARGS 1) 
(PUT '|:ONEP| 'DEFINED-ON-LINE '100) 
(PUT '|:ONEP| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:ONEP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:ONEP| (U) (COND ((ATOM U) (ONEP U)) (T (APPLY1 (GET (CAR U) 'ONEP) U)))) 
(PUT '|:PLUS| 'NUMBER-OF-ARGS 2) 
(PUT '|:PLUS| 'DEFINED-ON-LINE '103) 
(PUT '|:PLUS| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:PLUS| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:PLUS| (U V)
    (COND ((NULL U) V) ((NULL V) U)
          ((AND (ATOM U) (ATOM V))
           ((LAMBDA (W) (COND ((EQUAL W 0) NIL) (T W))) (PLUS U V)))
          (T (DCOMBINE U V 'PLUS)))) 
(PUT '|:QUOTIENT| 'NUMBER-OF-ARGS 2) 
(PUT '|:QUOTIENT| 'DEFINED-ON-LINE '113) 
(PUT '|:QUOTIENT| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:QUOTIENT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:QUOTIENT| (U V)
    (COND ((OR (NULL U) (EQUAL U 0)) NIL)
          ((OR (NULL V) (EQUAL V 0)) (RERROR 'POLY 12 "Zero divisor"))
          ((AND (ATOM U) (ATOM V))
           (COND ((NULL DMODE*) (QUOTIENT U V))
                 (T
                  ((LAMBDA (RECIPV)
                     (COND ((ATOM RECIPV) (TIMES U RECIPV))
                           (T (DCOMBINE U RECIPV 'TIMES))))
                   (|:RECIP| V)))))
          (T (DCOMBINE U V 'QUOTIENT)))) 
(PUT '|:RECIP| 'NUMBER-OF-ARGS 1) 
(PUT '|:RECIP| 'DEFINED-ON-LINE '123) 
(PUT '|:RECIP| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:RECIP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:RECIP| (U)
    (PROG ()
      (COND
       ((NUMBERP U)
        (COND ((EQUAL (ABS U) 1) (RETURN U))
              ((OR (NULL DMODE*) (MEMQ DMODE* '(|:RD:| |:CR:|)))
               (RETURN (|:RN2RD| (MKRN 1 U))))
              (T (SETQ U (APPLY1 (GET DMODE* 'I2D) U))))))
      (RETURN
       ((LAMBDA (X)
          (COND ((AND (NOT (ATOM X)) (EQUAL (CAR X) '|:RN:|)) (|:RN2RD| X))
                (T X)))
        (DCOMBINE 1 U 'QUOTIENT))))) 
(PUT '|:RN2RD| 'NUMBER-OF-ARGS 1) 
(PUT '|:RN2RD| 'DEFINED-ON-LINE '135) 
(PUT '|:RN2RD| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:RN2RD| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:RN2RD| (X) (COND ((AND *ROUNDALL *ROUNDED) (*RN2RD X)) (T X))) 
(PUT '|:TIMES| 'NUMBER-OF-ARGS 2) 
(PUT '|:TIMES| 'DEFINED-ON-LINE '139) 
(PUT '|:TIMES| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:TIMES| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:TIMES| (U V)
    (COND ((OR (NULL U) (NULL V)) NIL) ((AND (ATOM U) (ATOM V)) (TIMES U V))
          (T (DCOMBINE U V 'TIMES)))) 
(PUT '|:ZEROP| 'NUMBER-OF-ARGS 1) 
(PUT '|:ZEROP| 'DEFINED-ON-LINE '145) 
(PUT '|:ZEROP| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:ZEROP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:ZEROP| (U)
    (COND ((OR (NULL U) (EQUAL U 0)) T) ((ATOM U) NIL)
          (T (APPLY1 (GET (CAR U) 'ZEROP) U)))) 
(PUT 'FIELDP 'NUMBER-OF-ARGS 1) 
(PUT 'FIELDP 'DEFINED-ON-LINE '150) 
(PUT 'FIELDP 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT 'FIELDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIELDP (U) (AND (NOT (ATOM U)) (FLAGP (CAR U) 'FIELD))) 
(PUT 'GETTRANSFERFN 'NUMBER-OF-ARGS 2) 
(PUT 'GETTRANSFERFN 'DEFINED-ON-LINE '155) 
(PUT 'GETTRANSFERFN 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT 'GETTRANSFERFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETTRANSFERFN (U V) ((LAMBDA (X) (COND (X X) (T (DMODERR U V)))) (GET U V))) 
(PUT 'DCOMBINE 'NUMBER-OF-ARGS 3) 
(PUT 'DCOMBINE 'DEFINED-ON-LINE '160) 
(PUT 'DCOMBINE 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT 'DCOMBINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DCOMBINE (U V FN)
    (PROGN
     (SETQ U
             (COND
              ((ATOM U)
               (APPLY2 (GET (CAR V) FN) (APPLY1 (GET (CAR V) 'I2D) U) V))
              ((ATOM V)
               (APPLY2 (GET (CAR U) FN) U (APPLY1 (GET (CAR U) 'I2D) V)))
              ((EQ (CAR U) (CAR V)) (APPLY2 (GET (CAR U) FN) U V))
              (T
               ((LAMBDA (DU DV)
                  ((LAMBDA (X DML DL)
                     (PROGN
                      (COND
                       ((AND
                         (OR (NOT (AND X (ATOM X)))
                             (AND
                              (OR (AND (GET DU 'CMPXFN) (NOT (GET DV 'CMPXFN)))
                                  (AND (MEMQ DU DL) (NOT (MEMQ DV DL))))
                              (NEQ DV '|:PS:|)))
                         (NOT (FLAGP DV 'NOCONVERT)))
                        (PROGN
                         (COND
                          ((OR (AND (MEMQ DU DML) (EQ DV '|:RD:|))
                               (AND (EQ DU '|:RD:|) (MEMQ DV DML)))
                           (PROGN
                            (SETQ U (APPLY1 (GET DU '|:CR:|) U))
                            (SETQ DU '|:CR:|)))
                          ((OR (AND (EQ DU '|:RN:|) (EQ DV '|:GI:|))
                               (AND (EQ DU '|:GI:|) (EQ DV '|:RN:|)))
                           (PROGN
                            (SETQ U (APPLY1 (GET DU '|:CRN:|) U))
                            (SETQ DU '|:CRN:|))))
                         (SETQ V (APPLY1 (GET DV DU) V))
                         (SETQ X (GET DU FN))))
                       (T (PROGN (SETQ U (APPLY1 X U)) (SETQ X (GET DV FN)))))
                      (APPLY2 X U V)))
                   (GET DU DV) '(|:CRN:| |:GI:|) '(|:RD:| |:CR:|)))
                (CAR U) (CAR V)))))
     (COND
      ((AND *ROUNDED *ROUNDALL (NOT (ATOM U)))
       (INT-EQUIV-CHK
        (COND ((AND (EQ (SETQ V (CAR U)) '|:RN:|) (NEQ (CDDR U) 1)) (*RN2RD U))
              ((AND (EQ V '|:CRN:|) (OR (NEQ (CDADR U) 1) (NEQ (CDDDR U) 1)))
               (*CRN2CR U))
              (T U))))
      ((EQ FN 'DIVIDE) (CONS (INT-EQUIV-CHK (CAR U)) (INT-EQUIV-CHK (CDR U))))
      (T (INT-EQUIV-CHK U))))) 
(PUT 'INT-EQUIV-CHK 'NUMBER-OF-ARGS 1) 
(PUT 'INT-EQUIV-CHK 'DEFINED-ON-LINE '204) 
(PUT 'INT-EQUIV-CHK 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT 'INT-EQUIV-CHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT-EQUIV-CHK (U)
    (COND (*NOEQUIV U)
          (T
           (PROG (X)
             (COND ((ATOM U) (RETURN (COND ((EQUAL U 0) NIL) (T U))))
                   ((APPLY1 (GET (CAR U) 'ZEROP) U) (RETURN NIL))
                   ((APPLY1 (GET (CAR U) 'ONEP) U) (RETURN 1))
                   ((AND (SETQ X (GET (CAR U) 'INTEQUIVFN))
                         (SETQ X (APPLY1 X U)))
                    (RETURN (COND ((EQUAL X 0) NIL) (T X))))
                   (T (RETURN U))))))) 
(PUT '|:ABS| 'NUMBER-OF-ARGS 1) 
(PUT '|:ABS| 'DEFINED-ON-LINE '233) 
(PUT '|:ABS| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:ABS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:ABS| (U)
    (COND ((NULL U) NIL) ((ATOM U) (COND ((MINUSP U) (MINUS U)) (T U)))
          (T
           ((LAMBDA (X)
              (COND (X (APPLY1 X U))
                    ((APPLY1 (GET (CAR U) 'MINUSP) U) (|:MINUS| U)) (T U)))
            (GET (CAR U) 'ABS))))) 
(PUT '|:REPART| 'NUMBER-OF-ARGS 1) 
(PUT '|:REPART| 'DEFINED-ON-LINE '243) 
(PUT '|:REPART| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:REPART| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:REPART| (U)
    (COND ((ATOM U) U)
          (T
           ((LAMBDA (X)
              (COND (X (APPLY1 X U))
                    ((GET (CAR U) 'CMPXFN)
                     (INT-EQUIV-CHK
                      (CONS (CAR U)
                            (CONS (CADR U)
                                  (CADR (APPLY1 (GET (CAR U) 'I2D) 0))))))
                    (T U)))
            (GET (CAR U) 'REPART))))) 
(PUT '|:IMPART| 'NUMBER-OF-ARGS 1) 
(PUT '|:IMPART| 'DEFINED-ON-LINE '253) 
(PUT '|:IMPART| 'DEFINED-IN-FILE 'POLY/DMODEOP.RED) 
(PUT '|:IMPART| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:IMPART| (U)
    (COND ((ATOM U) NIL)
          (T
           ((LAMBDA (X)
              (COND (X (APPLY1 X U))
                    ((GET (CAR U) 'CMPXFN)
                     (INT-EQUIV-CHK
                      (CONS (CAR U)
                            (CONS (CDDR U)
                                  (CADR (APPLY1 (GET (CAR U) 'I2D) 0))))))
                    (T NIL)))
            (GET (CAR U) 'IMPART))))) 
(ENDMODULE) 
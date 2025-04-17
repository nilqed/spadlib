(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUBS2Q)) 
(FLUID '(*EXP *MCD *STRUCTURE *SUB2 ALGLIST* DMODE* FRLIS*)) 
(FLUID '(POWLIS* POWLIS1*)) 
(GLOBAL '(*RESUBS SIMPCOUNT* SIMPLIMIT*)) 
(PUT 'SUBS2Q 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS2Q 'DEFINED-ON-LINE '42) 
(PUT 'SUBS2Q 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'SUBS2Q 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS2Q (U)
    ((LAMBDA (V W)
       ((LAMBDA (X Y)
          (COND
           ((AND (EQUAL (CDR X) 1) (EQUAL (CDR Y) 1))
            (COND ((AND (EQUAL (CAR X) V) (EQUAL (CAR Y) W)) U)
                  (T (MULTSQ X (INVSQ Y)))))
           (T (SUBS2Q (MULTSQ X (INVSQ Y))))))
        (SUBS2F V) (SUBS2F W)))
     (CAR U) (CDR U))) 
(PUT 'SUBS2F 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS2F 'DEFINED-ON-LINE '57) 
(PUT 'SUBS2F 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'SUBS2F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS2F (U)
    (PROG (X)
      (COND
       ((GREATERP SIMPCOUNT* SIMPLIMIT*)
        (PROGN
         (SETQ SIMPCOUNT* 0)
         (RERROR 'POLY 21 "Simplification recursion too deep"))))
      (SETQ SIMPCOUNT* (PLUS SIMPCOUNT* 1))
      (SETQ *SUB2 NIL)
      (SETQ X (SUBS2F1 U))
      (COND
       ((AND (OR *SUB2 POWLIS1*) *RESUBS)
        (COND ((AND (EQUAL (CAR X) U) (EQUAL (CDR X) 1)) (SETQ *SUB2 NIL))
              (T (SETQ X (SUBS2Q X))))))
      (SETQ SIMPCOUNT* (DIFFERENCE SIMPCOUNT* 1))
      (RETURN X))) 
(PUT 'SUBS2F1 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS2F1 'DEFINED-ON-LINE '72) 
(PUT 'SUBS2F1 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'SUBS2F1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS2F1 (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (*D2Q U))
          (T
           (PROG (KERN V W X Y Z STACK)
            S
             (SETQ V (SETQ W (SETQ X (SETQ Y NIL))))
             (SETQ KERN (CAAAR U))
             (SETQ Z (CONS NIL 1))
            A
             (COND ((OR (NULL U) (EQUAL (DEGR U KERN) 0)) (GO A1)))
             (SETQ Y (CONS (CAR U) Y))
             (SETQ U (CDR U))
             (GO A)
            A1
             (SETQ X POWLIS*)
            A2
             (COND ((NULL X) (GO B))
                   ((EQUAL (CAAAR Y) (CAAR X))
                    (PROGN
                     (SETQ W (SUBS2P (CAAR Y) (CADAR X) (CADDDR (CAR X))))
                     (GO E1)))
                   ((AND (EQCAR KERN 'EXPT) (EQUAL (CADR KERN) (CAAR X))
                         (EQCAR (CADDR KERN) 'QUOTIENT)
                         (EQUAL (CADR (CADDR KERN)) 1)
                         (NUMBERP (CADDR (CADDR KERN))))
                    (PROGN
                     (SETQ V (DIVIDE (CDAAR Y) (CADDR (CADDR KERN))))
                     (COND
                      ((NEQ (CAR V) 0)
                       (SETQ W (EXPTSQ (SIMP (CADR KERN)) (CAR V))))
                      (T (SETQ W (CONS 1 1))))
                     (COND
                      ((NEQ (CDR V) 0)
                       (PROGN
                        (PROG (ALGLIST* DMODE*)
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (SETQ V
                                  (CANCEL
                                   (CONS (CDR V) (CADDR (CADDR KERN))))))
                        (SETQ W
                                (MULTSQ
                                 (RADDSQ
                                  (SUBS2P (CONS (CADR KERN) (CAR V)) (CADAR X)
                                          (CADDDR (CAR X)))
                                  (CDR V))
                                 W)))))
                     (GO E1))))
             (SETQ X (CDR X))
             (GO A2)
            B
             (SETQ X POWLIS1*)
            L2
             (COND ((NULL X) (GO L3))
                   ((SETQ W
                            (MTCHP (CAAR Y) (CAAR X) (CADDAR X) (CAADAR X)
                                   (CDADAR X)))
                    (GO E1)))
             (SETQ X (CDR X))
             (GO L2)
            L3
             (COND ((AND (EQCAR KERN 'EXPT) (NOT *STRUCTURE)) (GO L1)))
             (SETQ Z
                     (ADDSQ
                      (MULTSQ (CONS (LIST (CONS (CAAR Y) 1)) 1)
                              (SUBS2F1 (CDAR Y)))
                      Z))
            C
             (SETQ Y (CDR Y))
             (COND (Y (GO A1)))
            D
             (COND
              ((OR (ATOM U) (ATOM (CAR U))) (PROGN (SETQ Y (*D2Q U)) (GO X))))
             (SETQ STACK (CONS Z STACK))
             (GO S)
            X
             (COND
              ((NULL *EXP) (SETQ Y (CONS (MKPROD (CAR Y)) (MKPROD (CDR Y))))))
             (SETQ Y (ADDSQ Z Y))
             (COND
              (STACK
               (PROGN (SETQ Z (CAR STACK)) (SETQ STACK (CDR STACK)) (GO X))))
             (RETURN Y)
            E1
             (SETQ Z (ADDSQ (MULTSQ W (SUBS2F1 (CDAR Y))) Z))
             (GO C)
            L1
             (COND
              ((AND (EQUAL (CDAAR Y) 1) (NOT (EQCAR (CADR KERN) 'EXPT)))
               (SETQ W (MKSQ KERN 1)))
              (T
               (SETQ W
                       ((LAMBDA (U)
                          (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U))
                                (T (BASIC-SIMPEXPT U))))
                        (LIST (CADR KERN)
                              (LIST 'TIMES (CADDR KERN) (CDAAR Y)))))))
             (SETQ Z (ADDSQ (MULTSQ W (SUBS2F1 (CDAR Y))) Z))
             (SETQ Y (CDR Y))
             (COND (Y (GO L1)) (T (GO D))))))) 
(PUT 'SUBS2P 'NUMBER-OF-ARGS 3) 
(PUT 'SUBS2P 'DEFINED-ON-LINE '145) 
(PUT 'SUBS2P 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'SUBS2P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBS2P (U V W)
    (PROG ()
      (COND
       ((OR (NOT (FIXP (CDR U))) (EQUAL (CAR (SETQ V (DIVIDE (CDR U) V))) 0))
        (RETURN (CONS (LIST (CONS U 1)) 1))))
      (SETQ W (EXPTSQ (SIMP W) (CAR V)))
      (RETURN
       (COND ((EQUAL (CDR V) 0) W)
             (T (MULTSQ (CONS (LIST (CONS (CONS (CAR U) (CDR V)) 1)) 1) W)))))) 
(PUT 'RADDSQ 'NUMBER-OF-ARGS 2) 
(PUT 'RADDSQ 'DEFINED-ON-LINE '156) 
(PUT 'RADDSQ 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'RADDSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RADDSQ (U N)
    ((LAMBDA (U)
       (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U)) (T (BASIC-SIMPEXPT U))))
     (LIST (MK*SQ U) (LIST 'QUOTIENT 1 N)))) 
(PUT 'MTCHP 'NUMBER-OF-ARGS 5) 
(PUT 'MTCHP 'DEFINED-ON-LINE '160) 
(PUT 'MTCHP 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'MTCHP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MTCHP (U V W FLG BOOL)
    (PROG (X)
      (SETQ X (MTCHP1 U V FLG BOOL))
     A
      (COND ((NULL X) (RETURN NIL)) ((LISPEVAL (SUBLA (CAR X) BOOL)) (GO B)))
      (SETQ X (CDR X))
      (GO A)
     B
      (SETQ V (DIVIDE (CDR U) (SUBLA (CAR X) (CDR V))))
      (SETQ W (EXPTSQ (SIMP (SUBLA (CAR X) W)) (CAR V)))
      (COND
       ((NEQ (CDR V) 0)
        (SETQ W (MULTSQ (CONS (LIST (CONS (CONS (CAR U) (CDR V)) 1)) 1) W))))
      (RETURN W))) 
(PUT 'MTCHP1 'NUMBER-OF-ARGS 4) 
(PUT 'MTCHP1 'DEFINED-ON-LINE '179) 
(PUT 'MTCHP1 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'MTCHP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MTCHP1 (U V FLG BOOL)
    (PROG (X)
      (COND ((EQUAL U V) (RETURN (LIST NIL)))
            ((NOT (SETQ X (MCHK* (CAR U) (CAR V)))) (RETURN NIL))
            ((MEMQ (CDR V) FRLIS*)
             (COND
              ((OR (EQUAL (CDR U) 1)
                   (NOT (SETQ X (POWMTCH (CDR V) X (CDR U)))))
               (RETURN NIL))
              (T (RETURN (MAPCONS X (CONS (CDR V) (CDR U)))))))
            ((OR (AND FLG (NOT (EQUAL (CDR U) (CDR V))))
                 (NOT (NUMBERP (CDR V))) (NOT (NUMBERP (CDR U)))
                 (COND (*MCD (LESSP (CDR U) (CDR V)))
                       (T
                        (OR (LESSP (TIMES (CDR U) (CDR V)) 0)
                            (LESSP (ABS (CDR U)) (ABS (CDR V)))))))
             (RETURN NIL))
            (T (RETURN X))))) 
(PUT 'POWMTCH 'NUMBER-OF-ARGS 3) 
(PUT 'POWMTCH 'DEFINED-ON-LINE '203) 
(PUT 'POWMTCH 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'POWMTCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POWMTCH (U V W)
    (COND ((NULL V) NIL)
          (T
           ((LAMBDA (X)
              (COND
               ((OR (NULL X) (EQUAL (CDR X) W))
                (CONS (CAR V) (POWMTCH U (CDR V) W)))
               (T (POWMTCH U (CDR V) W))))
            (ATSOC U (CAR V)))))) 
(PUT 'MCHK* 'NUMBER-OF-ARGS 2) 
(PUT 'MCHK* 'DEFINED-ON-LINE '213) 
(PUT 'MCHK* 'DEFINED-IN-FILE 'POLY/SUBS2Q.RED) 
(PUT 'MCHK* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MCHK* (U V)
    (PROG (X)
      (COND ((SETQ X (MCHK U V)) (RETURN X))
            ((OR *MCD (NOT (AND (SFP U) (SFP V)))) (RETURN NIL))
            (T (RETURN (MCHK (PREPF U) (PREPF V))))))) 
(ENDMODULE) 
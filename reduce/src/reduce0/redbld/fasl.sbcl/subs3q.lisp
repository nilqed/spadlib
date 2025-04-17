(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUBS3Q)) 
(FLUID '(*MCD POWLIS1* *SUB2 SUBFG*)) 
(GLOBAL '(*MATCH *RESUBS MCHFG*)) 
(PUT 'SUBS3Q 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS3Q 'DEFINED-ON-LINE '36) 
(PUT 'SUBS3Q 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'SUBS3Q 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS3Q (U)
    (PROG (X)
      (SETQ X MCHFG*)
      (SETQ MCHFG* NIL)
      (SETQ U (MULTSQ (SUBS3F (CAR U)) (INVSQ (SUBS3F (CDR U)))))
      (SETQ MCHFG* X)
      (RETURN U))) 
(PUT 'SUBS3F 'NUMBER-OF-ARGS 1) 
(PUT 'SUBS3F 'DEFINED-ON-LINE '47) 
(PUT 'SUBS3F 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'SUBS3F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBS3F (U) (SUBS3F1 U *MATCH T)) 
(PUT 'SUBS3F1 'NUMBER-OF-ARGS 3) 
(PUT 'SUBS3F1 'DEFINED-ON-LINE '52) 
(PUT 'SUBS3F1 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'SUBS3F1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBS3F1 (U L BOOL)
    (PROG (X Z)
      (SETQ Z (CONS NIL 1))
     A
      (COND ((NULL U) (RETURN Z))
            ((OR (ATOM U) (ATOM (CAR U))) (RETURN (ADDSQ Z (CONS U 1))))
            ((AND BOOL (OR (ATOM (CDAR U)) (ATOM (CAR (CDAR U))))) (GO C)))
      (SETQ X (SUBS3T (CAR U) L))
      (COND ((OR (NOT BOOL) (NOT MCHFG*)) (GO B)))
      (SETQ MCHFG* NIL)
      (COND
       ((AND (EQUAL (CAR X) U) (EQUAL (CDR X) 1))
        (PROGN (SETQ X (CONS U 1)) (GO B)))
       ((NULL *RESUBS) (GO B)) ((OR *SUB2 POWLIS1*) (SETQ X (SUBS2Q X))))
      (SETQ X (SUBS3Q X))
     B
      (SETQ Z (ADDSQ Z X))
      (SETQ U (CDR U))
      (GO A)
     C
      (SETQ X (CONS (LIST (CAR U)) 1))
      (GO B))) 
(PUT 'SUBS3T 'NUMBER-OF-ARGS 2) 
(PUT 'SUBS3T 'DEFINED-ON-LINE '80) 
(PUT 'SUBS3T 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'SUBS3T 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBS3T (U V)
    (PROG (BOOL W X Y Z)
      (SETQ X
              (MTCHK (CAR U)
                     (COND
                      ((OR (ATOM (CDR U)) (ATOM (CAR (CDR U)))) (SIZCHK V 1))
                      (T V))))
      (COND ((NULL X) (GO A)) ((NULL (CAAR X)) (GO B)))
      (SETQ Y (SUBS3F1 (CDR U) X NIL))
      (COND (MCHFG* (RETURN (MULTSQ (CONS (LIST (CONS (CAR U) 1)) 1) Y))))
     A
      (RETURN (CONS (LIST U) 1))
     B
      (SETQ X (CDDAR X))
      (SETQ Z (CAADR X))
      (SETQ MCHFG* NIL)
      (SETQ Y (SUBS3F1 (CDR U) *MATCH NIL))
      (SETQ MCHFG* T)
      (COND ((NEQ (CAR Z) (CAAR U)) (GO E))
            ((NEQ Z (CAR U))
             (SETQ Y
                     (MULTSQ
                      (CONS
                       (LIST
                        (CONS (CONS (CAAR U) (DIFFERENCE (CDAR U) (CDR Z))) 1))
                       1)
                      Y))))
     B1
      (SETQ Y (MULTSQ (SIMP (CAR X)) Y))
      (SETQ X (CDADR X))
      (COND ((NULL X) (RETURN Y)))
      (SETQ Z 1)
     C
      (COND ((NULL X) (GO D)))
      (SETQ W
              (COND ((OR (ATOM (CAAR X)) (SFP (CAAR X))) (CAAR X))
                    (T
                     ((LAMBDA (SUBFG*)
                        ((LAMBDA (WW)
                           (COND
                            ((AND (KERNP WW)
                                  (EQCAR (SETQ WW (CAAAR (CAR WW)))
                                         (CAR (CAAR X))))
                             WW)
                            (T (REVOP1 (CAAR X)))))
                         (SIMP (CAAR X))))
                      NIL))))
      (COND ((OR (AND *NCMP (NONCOMP1 W)) (NOT *MCD)) (SETQ BOOL T)))
      (SETQ Z
              ((LAMBDA (G699)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z G699))
                       (T (POLY-MULTF Z G699))))
               (LIST
                (CONS
                 (GETPOWER (FKERN W)
                           (COND ((NULL BOOL) (CDAR X)) (T (MINUS (CDAR X)))))
                 1))))
      (SETQ X (CDR X))
      (GO C)
     D
      (RETURN
       (COND
        ((NOT BOOL)
         (CONS (CAR Y)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z (CDR Y)))
                     (T (POLY-MULTF Z (CDR Y))))))
        (T
         (CONS
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z (CAR Y)))
                (T (POLY-MULTF Z (CAR Y))))
          (CDR Y)))))
     E
      (COND
       ((NEQ (SIMP (CAR Z)) (SIMP (CAAR U))) (ERRACH (LIST 'SUBS3T U X Z))))
      (COND
       ((NEQ (CDR Z) (CDAR U))
        (SETQ Y
                (MULTSQ
                 (CONS
                  (LIST (CONS (CONS (CAAR U) (DIFFERENCE (CDAR U) (CDR Z))) 1))
                  1)
                 Y))))
      (GO B1))) 
(PUT 'SIZCHK 'NUMBER-OF-ARGS 2) 
(PUT 'SIZCHK 'DEFINED-ON-LINE '134) 
(PUT 'SIZCHK 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'SIZCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIZCHK (U N)
    (COND ((NULL U) NIL) ((GREATERP (LENGTH (CAAR U)) N) (SIZCHK (CDR U) N))
          (T (CONS (CAR U) (SIZCHK (CDR U) N))))) 
(PUT 'MTCHK 'NUMBER-OF-ARGS 2) 
(PUT 'MTCHK 'DEFINED-ON-LINE '139) 
(PUT 'MTCHK 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'MTCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MTCHK (U V)
    (PROG (FLG V1 W X Y Z LASTPAIRZ)
      (SETQ FLG (AND *NCMP (NONCOMP1 (CAR U))))
     A0
      (COND ((NULL V) (RETURN Z)))
      (SETQ V1 (CAR V))
      (SETQ W (CAR V1))
     A
      (COND ((NULL W) (GO D)))
      (SETQ X (MTCHP1 U (CAR W) (CAADR V1) (CDADR V1)))
     B
      (COND ((NULL X) (GO C))
            ((CAR
              (SETQ Y
                      (CONS (SUBLA (CAR X) (DELETE (CAR W) (CAR V1)))
                            (LIST (SUBLA (CAR X) (CADR V1))
                                  (SUBLA (CAR X) (CADDR V1))
                                  (CONS (SUBLA (CAR X) (CAR W))
                                        (CADDDR V1))))))
             (PROGN
              (SETQ Z (CONS Y Z))
              (COND ((NULL LASTPAIRZ) (SETQ LASTPAIRZ Z)))))
            ((LISPEVAL (SUBLA (CAR X) (CDADR V1))) (RETURN (LIST Y))))
      (SETQ X (CDR X))
      (GO B)
     C
      (COND ((NULL FLG) (PROGN (SETQ W (CDR W)) (GO A)))
            ((AND (CADDDR V1) (NOCP W)) (GO E)))
     D
      (COND ((NULL Z) (SETQ Z (SETQ LASTPAIRZ (LIST V1))))
            (T
             (PROGN
              (RPLACD LASTPAIRZ (LIST V1))
              (SETQ LASTPAIRZ (CDR LASTPAIRZ)))))
     E
      (SETQ V (CDR V))
      (GO A0))) 
(PUT 'NOCP 'NUMBER-OF-ARGS 1) 
(PUT 'NOCP 'DEFINED-ON-LINE '175) 
(PUT 'NOCP 'DEFINED-IN-FILE 'POLY/SUBS3Q.RED) 
(PUT 'NOCP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOCP (U) (OR (NULL U) (AND (AND *NCMP (NONCOMP1 (CAAR U))) (NOCP (CDR U))))) 
(ENDMODULE) 
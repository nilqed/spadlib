(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NOUT)) 
(PUT 'NC_COMPACT 'NUMBER-OF-ARGS 1) 
(PUT 'NC_COMPACT 'DEFINED-ON-LINE '32) 
(PUT 'NC_COMPACT 'DEFINED-IN-FILE 'NCPOLY/NCOUT.RED) 
(PUT 'NC_COMPACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_COMPACT (U)
    (PROG (VL T1 T2 Y R D W)
      (SETQ VL
              (INTERSECTION KORD*
                            (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Y NCPI-NAMES*)
                              (COND ((NULL Y) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y) (CAR Y)) (CAR Y))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Y (CDR Y))
                              (COND ((NULL Y) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
      (PROG (X)
        (SETQ X VL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ Y (GENSYM))
            (SETQ T1 (CONS (CONS X Y) T1))
            (SETQ T2 (CONS (CONS Y X) T2))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      ((LAMBDA (*FACTOR *FACTORS *EXP) (SETQ W (SIMP U))) NIL NIL T)
      (SETQ D (CDR W))
      (SETQ R (NC_COMPACTR (CAR W) (REVERSE VL) T1 T2))
      (RETURN (MK*SQ (CONS R D))))) 
(PUT 'NC_COMPACTR 'NUMBER-OF-ARGS 4) 
(PUT 'NC_COMPACTR 'DEFINED-ON-LINE '43) 
(PUT 'NC_COMPACTR 'DEFINED-IN-FILE 'NCPOLY/NCOUT.RED) 
(PUT 'NC_COMPACTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_COMPACTR (U VL T1 T2)
    (PROG (X XN Y Q W R S N M)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ X (CAR VL))
      (SETQ VL (CDR VL))
      (SETQ W (NC_COMPACTD U))
      (SETQ N (MINUS 1))
     LOOP
      (COND ((NULL W) (GO DONE)))
      (SETQ N (PLUS N 1))
      (SETQ XN (COND ((EQUAL N 0) 1) (T (CONS (CONS (CONS X N) 1) NIL))))
      (SETQ Q (NC_COMPACTX W X XN))
      (SETQ W (CDR Q))
      (SETQ Q (CAR Q))
      (COND
       (Q
        (PROG (*FACTOR *EXP)
          (COND
           ((OR (NULL VL) (NULL (CDR VL))
                (GREATERP 2
                          (PROGN
                           (SETQ M 0)
                           (PROG (Y)
                             (SETQ Y VL)
                            LAB
                             (COND ((NULL Y) (RETURN NIL)))
                             ((LAMBDA (Y)
                                (COND ((SMEMBER Y Q) (SETQ M (PLUS M 1)))))
                              (CAR Y))
                             (SETQ Y (CDR Y))
                             (GO LAB))
                           M)))
            (PROGN
             (SETQ Q
                     (CONS 'PLUS
                           (PROG (S FORALL-RESULT FORALL-ENDPTR)
                             (SETQ S Q)
                             (COND ((NULL S) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (S)
                                                 (PREPF (SUBLIS T1 S)))
                                               (CAR S))
                                              NIL)))
                            LOOPLABEL
                             (SETQ S (CDR S))
                             (COND ((NULL S) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (S) (PREPF (SUBLIS T1 S)))
                                       (CAR S))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
             (SETQ *FACTOR T)
             (SETQ Q (REORDER (SUBLIS T2 (CAR (SIMP (REVAL1 Q NIL))))))))
           (T
            (PROGN
             (SETQ S NIL)
             (PROG (F)
               (SETQ F Q)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F) (SETQ S (ADDF S F))) (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ Q (NC_COMPACTR S VL T1 T2)))))
          (SETQ R
                  (ADDF
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF Q XN))
                         (T (POLY-MULTF Q XN)))
                   R)))))
      (GO LOOP)
     DONE
      (RETURN R))) 
(FLAG '(NC_COMPACT) 'OPFN) 
(PUT 'NC_COMPACTD 'NUMBER-OF-ARGS 1) 
(PUT 'NC_COMPACTD 'DEFINED-ON-LINE '71) 
(PUT 'NC_COMPACTD 'DEFINED-IN-FILE 'NCPOLY/NCOUT.RED) 
(PUT 'NC_COMPACTD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_COMPACTD (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (LIST U))
          (T
           (APPEND
            (PROG (S FORALL-RESULT FORALL-ENDPTR)
              (SETQ S (NC_COMPACTD (CDAR U)))
              (COND ((NULL S) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (S) (CONS (CONS (CAAR U) S) NIL))
                                (CAR S))
                               NIL)))
             LOOPLABEL
              (SETQ S (CDR S))
              (COND ((NULL S) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (S) (CONS (CONS (CAAR U) S) NIL)) (CAR S))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (AND (CDR U) (NC_COMPACTD (CDR U))))))) 
(PUT 'NC_COMPACTX 'NUMBER-OF-ARGS 3) 
(PUT 'NC_COMPACTX 'DEFINED-ON-LINE '77) 
(PUT 'NC_COMPACTX 'DEFINED-IN-FILE 'NCPOLY/NCOUT.RED) 
(PUT 'NC_COMPACTX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_COMPACTX (U X XN)
    (PROG (YES NO W)
      (PROG (R)
        (SETQ R U)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (COND
            ((AND (EQUAL XN 1) (NOT (SMEMBER X R))) (SETQ YES (CONS R YES)))
            ((AND (SETQ W ((LAMBDA (*EXP) (QUOTF1 R XN)) T))
                  (NOT (SMEMBER X W)))
             (SETQ YES (CONS W YES)))
            (T (SETQ NO (CONS R NO)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN (CONS YES NO)))) 
(ENDMODULE) 
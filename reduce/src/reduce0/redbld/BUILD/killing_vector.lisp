(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'KILLINGVEC)) 
(FLUID '(METRICD*)) 
(GLOBAL '(BASISFORML* BASISVECTORL* NATURALVECTOR2FRAMEVECTOR)) 
(PUT 'KILLING_VECTOR 'NUMBER-OF-ARGS 1) 
(PUT 'KILLING_VECTOR 'DEFINED-ON-LINE '33) 
(PUT 'KILLING_VECTOR 'DEFINED-IN-FILE 'EXCALC/KILLING_VECTOR.RED) 
(PUT 'KILLING_VECTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KILLING_VECTOR (KNAME)
    (PROG (OLDKORD COORDS KF B1 B2 LB1 LB2 L12 RES KV)
      (SETQ KNAME (CAR KNAME))
      (SETQ L12 (CONS NIL 1))
      (PUTFORM (LIST KNAME NIL) 0)
      (REMFLAG (LIST KNAME) 'COVARIANT)
      (SETQ COORDS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J NATURALFRAME2COFRAME)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ KF (CONS KNAME COORDS))
      (FDOMAIN1 (LIST (LIST 'EQUAL KNAME KF)))
      (COND
       (BASISVECTORL*
        (PROG (L)
          (SETQ L BASISVECTORL*)
         LAB
          (COND ((NULL L) (RETURN NIL)))
          ((LAMBDA (L)
             (SETQ KV
                     (ADDPF
                      (MULTPFS (PARTITOP L) (PARTITOP (LIST KNAME (CADADR L))))
                      KV)))
           (CAR L))
          (SETQ L (CDR L))
          (GO LAB)))
       (T
        (PROG (L)
          (SETQ L COORDS)
         LAB
          (COND ((NULL L) (RETURN NIL)))
          ((LAMBDA (L)
             (SETQ KV
                     (ADDPF
                      (MULTPFS (PARTITOP (LIST 'PARTDF L))
                       (PARTITOP (LIST KNAME L)))
                      KV)))
           (CAR L))
          (SETQ L (CDR L))
          (GO LAB))))
      (SETQ OLDKORD (SETKORDER BASISFORML*))
      (PROG (K)
        (SETQ K METRICD*)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROG (L)
             (SETQ L (CDR K))
            LAB
             (COND ((NULL L) (RETURN NIL)))
             ((LAMBDA (L)
                (PROGN
                 (SETQ B1 (PARTITOP (RASSOC (LIST (CAR K)) BASISFORML*)))
                 (SETQ B2 (PARTITOP (RASSOC (LIST (CAR L)) BASISFORML*)))
                 (SETQ LB1
                         (MULTSQ (*PF2SQ B2)
                                 (*PF2SQ
                                  (MULTPFSQ (LIEDFPF KV B1) (SIMP (CDR L))))))
                 (SETQ LB2
                         (MULTSQ (*PF2SQ B1)
                                 (*PF2SQ
                                  (LIEDFPF KV (MULTPFSQ B2 (SIMP (CDR L)))))))
                 (SETQ L12 (ADDSQ (ADDSQ LB1 LB2) L12))))
              (CAR L))
             (SETQ L (CDR L))
             (GO LAB)))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (SETKORDER OLDKORD)
      (SETQ RES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CAR L12))
               STARTOVER
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (COND
                         ((EQUAL (CDAAR J) 2)
                          (LIST (KILLING_NORMALIZE_PDE (CDAR J))))
                         (T
                          (PROG (M FORALL-RESULT FORALL-ENDPTR)
                            (SETQ M (CDAR J))
                            (COND ((NULL M) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             (KILLING_NORMALIZE_PDE (CDAR M))
                                             NIL)))
                           LOOPLABEL
                            (SETQ M (CDR M))
                            (COND ((NULL M) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS (KILLING_NORMALIZE_PDE (CDAR M))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ J (CDR J))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (COND
                         ((EQUAL (CDAAR J) 2)
                          (LIST (KILLING_NORMALIZE_PDE (CDAR J))))
                         (T
                          (PROG (M FORALL-RESULT FORALL-ENDPTR)
                            (SETQ M (CDAR J))
                            (COND ((NULL M) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             (KILLING_NORMALIZE_PDE (CDAR M))
                                             NIL)))
                           LOOPLABEL
                            (SETQ M (CDR M))
                            (COND ((NULL M) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS (KILLING_NORMALIZE_PDE (CDAR M))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ J (CDR J))
                (GO LOOPLABEL)))
      (RETURN (LIST 'LIST (MK*SQ (*PF2SQ KV)) (CONS 'LIST RES))))) 
(PUT 'KILLING_NORMALIZE_PDE 'NUMBER-OF-ARGS 1) 
(PUT 'KILLING_NORMALIZE_PDE 'DEFINED-ON-LINE '66) 
(PUT 'KILLING_NORMALIZE_PDE 'DEFINED-IN-FILE 'EXCALC/KILLING_VECTOR.RED) 
(PUT 'KILLING_NORMALIZE_PDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KILLING_NORMALIZE_PDE (U)
    (PROG (C)
      (COND ((MINUSP (LNC U)) (SETQ U (NEGF U))))
      (SETQ C (COMFAC U))
      (SETQ C (IGNORE_PARTDF (CDR C)))
      (RETURN (MK*SQ (CONS (QUOTF1 U C) 1))))) 
(PUT 'IGNORE_PARTDF 'NUMBER-OF-ARGS 1) 
(PUT 'IGNORE_PARTDF 'DEFINED-ON-LINE '74) 
(PUT 'IGNORE_PARTDF 'DEFINED-IN-FILE 'EXCALC/KILLING_VECTOR.RED) 
(PUT 'IGNORE_PARTDF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IGNORE_PARTDF (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) U)
          ((EQCAR (CAAAR U) 'PARTDF) (IGNORE_PARTDF (CDAR U)))
          (T
           (CONS (CONS (CAAR U) (IGNORE_PARTDF (CDAR U)))
                 (IGNORE_PARTDF (CDR U)))))) 
(PUT 'KILLING_VECTOR 'PSOPFN 'KILLING_VECTOR) 
(ENDMODULE) 
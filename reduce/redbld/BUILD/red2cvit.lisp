(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RED2CVIT)) 
(EXPORTS (LIST 'ISIMP1 'REPLACE_BY_VECTOR 'REPLACE_BY_VECTORP 'GAMMA5P)) 
(IMPORTS (LIST 'CALC_SPUR 'ISIMP2)) 
(SWITCH (LIST 'CVIT)) 
(SETQ *CVIT T) 
(REMFLAG '(ISIMP1) 'LOSE) 
(PUT 'ISIMP1 'NUMBER-OF-ARGS 5) 
(PUT 'ISIMP1 'DEFINED-ON-LINE '45) 
(PUT 'ISIMP1 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'ISIMP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ISIMP1 (U I V W X)
    (COND ((NULL U) NIL)
          ((OR (ATOM U) (ATOM (CAR U)))
           (COND
            (X
             (MULTD U
                    (COND (*CVIT (CALC_SPURX I V W X))
                          (T (SPUR0 (CAR X) I V W (CDR X))))))
            (V (MULTD U (INDEX_SIMP 1 I V W)))
            (W
             ((LAMBDA (G127 G128)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G127 G128))
                      (T (POLY-MULTF G127 G128))))
              (EMULT W) (ISIMP1 U I V NIL NIL)))
            (T U)))
          (T (ADDF (ISIMP2 (CAR U) I V W X) (ISIMP1 (CDR U) I V W X))))) 
(FLAG '(ISIMP1) 'LOSE) 
(PUT 'INDEX_SIMP 'NUMBER-OF-ARGS 4) 
(PUT 'INDEX_SIMP 'DEFINED-ON-LINE '61) 
(PUT 'INDEX_SIMP 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'INDEX_SIMP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INDEX_SIMP (U I V W)
    (COND
     (V
      (INDEX_SIMP
       ((LAMBDA (G129)
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G129 U))
                (T (POLY-MULTF G129 U))))
        (MKSPROD (CAAR V) (CDAR V)))
       (UPDATE_INDEX I (CAR V)) (CDR V) W))
     (T (ISIMP1 U I NIL W NIL)))) 
(PUT 'MKSPROD 'NUMBER-OF-ARGS 2) 
(PUT 'MKSPROD 'DEFINED-ON-LINE '66) 
(PUT 'MKSPROD 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'MKSPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKSPROD (X Y)
    (MKSF
     (CONS 'CONS
           (ORD2
            (COND ((MEMQ X INDICES*) (OR (GET X 'REPLACE_BY_VECTOR) X)) (T X))
            (COND ((MEMQ Y INDICES*) (OR (GET Y 'REPLACE_BY_VECTOR) Y))
                  (T Y)))))) 
(PUT 'UPDATE_INDEX 'NUMBER-OF-ARGS 2) 
(PUT 'UPDATE_INDEX 'DEFINED-ON-LINE '70) 
(PUT 'UPDATE_INDEX 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'UPDATE_INDEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPDATE_INDEX (I V) (DELETE (CDR V) (DELETE (CAR V) I))) 
(PUT 'CALC_SPURX 'NUMBER-OF-ARGS 4) 
(PUT 'CALC_SPURX 'DEFINED-ON-LINE '78) 
(PUT 'CALC_SPURX 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'CALC_SPURX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALC_SPURX (I V W X)
    (PROG (U X1 DINDICES* C)
      (COND
       ((AND (NUMBERP NDIMS*) (NULL (EVENP NDIMS*)))
        (CVITERR
         (LIST 'CALC_SPUR ":" NDIMS*
               "is not even dimension of G-matrix space"))))
      (SETQ C 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT X) (RETURN NIL)))
        (PROGN
         (COND
          ((FLAGP (CAAR X) 'NOSPUR)
           (CVITERR (LIST "Nospur not yet implemented"))))
         (SETQ U (CDAR X))
         (SETQ X (CDR X))
         (COND
          ((CAR U)
           (COND
            ((EVENP NDIMS*) (SETQ U (CONS (NEXT_GAMMA5) (REVERSE (CDR U)))))
            (T (CVITERR (LIST "G5 invalid for non even dimension")))))
          (T (SETQ U (REVERSE (CDR U)))))
         (COND ((NULL U) NIL)
               ((NULL
                 (EVENP
                  (LENGTH
                   (COND ((AND (MEMQ (CAR U) (CAR GAMMA5*)) (CDR U)) (CDR U))
                         (T U)))))
                (SETQ X (SETQ C NIL)))
               (T
                (PROGN
                 (SETQ U (REMOVE_GX*GX U))
                 (SETQ C
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) C))
                               (T (POLY-MULTF (CAR U) C))))
                 (SETQ U (REPLACE_VECTOR (CDR U) I V W))
                 (SETQ I (CADR U))
                 (SETQ V (CADDR U))
                 (SETQ W (CADDDR U))
                 (COND (U (SETQ X1 (CONS (CAR U) X1))))))))
        (GO WHILELABEL))
      (SETQ X1
              (COND ((NULL C) (CONS NIL 1))
                    (X1 (MULTSQ (CONS C 1) (CALC_SPUR X1))) (T (CONS C 1))))
      (COND
       ((NEQ (CDR X1) 1)
        (CVITERR (LIST 'CALC_SPURX ":" X1 "has non unit denominator"))))
      (CLEAR_WINDICES)
      (SETQ GAMMA5* (CONS NIL (APPEND (REVERSE (CAR GAMMA5*)) (CDR GAMMA5*))))
      (RETURN (ISIMP1 (CAR X1) I V W NIL)))) 
(PUT 'THIRD_EQ_INDEXP 'NUMBER-OF-ARGS 1) 
(PUT 'THIRD_EQ_INDEXP 'DEFINED-ON-LINE '127) 
(PUT 'THIRD_EQ_INDEXP 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'THIRD_EQ_INDEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE THIRD_EQ_INDEXP (I)
    (PROG (Z)
      (COND
       ((NULL (SETQ Z (ASSOC I DINDICES*)))
        (SETQ DINDICES* (CONS (CONS I NIL) DINDICES*)))
       ((NULL (CDR Z))
        (SETQ DINDICES* (CONS (CONS I T) (DELETE Z DINDICES*)))))
      (RETURN (COND (Z (CDR Z)) (T NIL))))) 
(PUT 'REPLACE_VECTOR 'NUMBER-OF-ARGS 4) 
(PUT 'REPLACE_VECTOR 'DEFINED-ON-LINE '136) 
(PUT 'REPLACE_VECTOR 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'REPLACE_VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPLACE_VECTOR (U I V W)
    (PROG (Z Y X U1)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ Z (CAR U))
         (SETQ U (CDR U))
         (COND
          ((MEMQ Z INDICES*)
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (SETQ Y (BASSOC Z V))) (RETURN NIL)))
              (PROGN
               (SETQ I (DELETE Z I))
               (SETQ V (DELETE Y V))
               (SETQ X (COND ((EQ Z (CAR Y)) (CDR Y)) (T (CAR Y))))
               (COND ((MEMQ X INDICES*) (SETQ Z X))
                     ((MEMQ X (CAR GAMMA5*))
                      (CVITERR (LIST "G5 bad structure")))
                     (T (REPLACE_BY_INDEX X Z))))
              (GO WHILELABEL))
            (SETQ U1 (CONS Z U1))))
          ((MEMQ Z (CAR GAMMA5*)) (SETQ U1 (CONS Z U1)))
          (T
           (PROGN
            (SETQ Z (REPLACE_BY_INDEX Z (NEXT_WINDEX)))
            (SETQ U1 (CONS Z U1))))))
        (GO WHILELABEL))
      (RETURN (LIST (REVERSE U1) I V W)))) 
(PUT 'REPLACE_BY_INDEX 'NUMBER-OF-ARGS 2) 
(PUT 'REPLACE_BY_INDEX 'DEFINED-ON-LINE '171) 
(PUT 'REPLACE_BY_INDEX 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'REPLACE_BY_INDEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPLACE_BY_INDEX (V Y)
    (PROG (Z)
      (COND
       ((EQ (SETQ Z (GET Y 'REPLACE_BY_VECTOR)) V)
        (CVITERR
         (LIST 'REPLACE_BY_INDEX ":" Y "is already defined for vector" Z))))
      (PUT Y 'REPLACE_BY_VECTOR V)
      (RETURN Y))) 
(PUT 'REMOVE_GX*GX 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE_GX*GX 'DEFINED-ON-LINE '180) 
(PUT 'REMOVE_GX*GX 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'REMOVE_GX*GX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE_GX*GX (U)
    (PROG (X C L L1)
      (SETQ L 0)
      (SETQ L1 0)
      (SETQ C 1)
      (SETQ L1 (SETQ L (LENGTH U)))
      (SETQ U
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z U)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z)
                                    (PROGN
                                     (COND
                                      ((MEMQ Z INDICES*)
                                       (COND
                                        ((THIRD_EQ_INDEXP Z)
                                         (CVITERR
                                          (LIST "Three indices have name" Z)))
                                        (T NIL)))
                                      ((NULL (EQ (GET Z 'RTYPE) 'HVECTOR))
                                       (COND
                                        ((CVITDECLP Z 'VECTOR)
                                         (VECTOR1 (LIST Z)))
                                        (T (CVITERR NIL))))
                                      (T NIL))
                                     Z))
                                  (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Z)
                            (PROGN
                             (COND
                              ((MEMQ Z INDICES*)
                               (COND
                                ((THIRD_EQ_INDEXP Z)
                                 (CVITERR (LIST "Three indices have name" Z)))
                                (T NIL)))
                              ((NULL (EQ (GET Z 'RTYPE) 'HVECTOR))
                               (COND ((CVITDECLP Z 'VECTOR) (VECTOR1 (LIST Z)))
                                     (T (CVITERR NIL))))
                              (T NIL))
                             Z))
                          (CAR Z))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((LESSP L 2) (RETURN U)))
      (SETQ X U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR X)) (RETURN NIL)))
        (SETQ X (CDR X))
        (GO WHILELABEL))
      (RPLACD X U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP L1 0)) (RETURN NIL)))
        (COND
         ((EQ (CAR U) (CADR U))
          (PROGN
           (SETQ C
                   ((LAMBDA (G131)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G131 C))
                            (T (POLY-MULTF G131 C))))
                    (COND ((MEMQ (CAR U) INDICES*) NDIMS*)
                          (T (MKSF (CONS 'CONS (ORD2 (CAR U) (CAR U))))))))
           (RPLACA U (CADDR U))
           (RPLACD U (CDDDR U))
           (SETQ L1 (SETQ L (DIFFERENCE L 2)))))
         (T (PROGN (SETQ U (CDR U)) (SETQ L1 (DIFFERENCE L1 1)))))
        (GO WHILELABEL))
      (SETQ X (CDR U))
      (RPLACD U NIL)
      (RETURN (CONS C (COND ((AND (CDR X) (EQ (CAR X) (CADR X))) NIL) (T X)))))) 
(PUT 'CVITERR 'NUMBER-OF-ARGS 1) 
(PUT 'CVITERR 'DEFINED-ON-LINE '221) 
(PUT 'CVITERR 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'CVITERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CVITERR (U)
    (PROGN
     (CLEAR_WINDICES)
     (SETQ GAMMA5* (CONS NIL (APPEND (REVERSE (CAR GAMMA5*)) (CDR GAMMA5*))))
     (COND (U (REDERR U)) (T (ERROR 0 NIL))))) 
(PUT 'CVITDECLP 'NUMBER-OF-ARGS 2) 
(PUT 'CVITDECLP 'DEFINED-ON-LINE '226) 
(PUT 'CVITDECLP 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'CVITDECLP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CVITDECLP (U V)
    (COND ((NULL *MSG) NIL) ((TERMINALP) (YESP (LIST "Declare" U V "?")))
          (T (PROGN (LPRIM (LIST U "Declare" V)) T)))) 
(PUT 'CLEAR_WINDICES 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_WINDICES 'DEFINED-ON-LINE '234) 
(PUT 'CLEAR_WINDICES 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'CLEAR_WINDICES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_WINDICES NIL
    (PROG ()
     WHILELABEL
      (COND ((NOT (CAR WINDICES*)) (RETURN NIL)))
      (PROG (Z)
        (SETQ Z (CAAR WINDICES*))
        (SETQ WINDICES* (CONS (CDAR WINDICES*) (CONS Z (CDR WINDICES*))))
        (REMPROP Z 'REPLACE_BY_VECTOR)
        (SETQ INDICES* (DELETE Z INDICES*)))
      (GO WHILELABEL))) 
(PUT 'NEXT_WINDEX 'NUMBER-OF-ARGS 0) 
(PUT 'NEXT_WINDEX 'DEFINED-ON-LINE '243) 
(PUT 'NEXT_WINDEX 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'NEXT_WINDEX 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEXT_WINDEX NIL
    (PROG (I)
      (SETQ WINDICES*
              (COND
               ((NULL (CDR WINDICES*))
                (CONS (CONS (INTERN (GENSYM)) (CAR WINDICES*))
                      (CDR WINDICES*)))
               (T
                (CONS (CONS (CADR WINDICES*) (CAR WINDICES*))
                      (CDDR WINDICES*)))))
      (SETQ I (CAAR WINDICES*))
      (VECTOR1 (LIST I))
      (SETQ INDICES* (CONS I INDICES*))
      (RETURN I))) 
(PUT 'NEXT_GAMMA5 'NUMBER-OF-ARGS 0) 
(PUT 'NEXT_GAMMA5 'DEFINED-ON-LINE '256) 
(PUT 'NEXT_GAMMA5 'DEFINED-IN-FILE 'HEPHYS/RED2CVIT.RED) 
(PUT 'NEXT_GAMMA5 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEXT_GAMMA5 NIL
    (PROG (V)
      (CVITERR (LIST "GAMMA5 is not yet implemented. use OFF CVIT"))
      (SETQ GAMMA5*
              (COND
               ((NULL (CDR GAMMA5*))
                (CONS (CONS (INTERN (GENSYM)) (CAR GAMMA5*)) (CDR GAMMA5*)))
               (T (CONS (CONS (CADR GAMMA5*) (CAR GAMMA5*)) (CDDR GAMMA5*)))))
      (SETQ V (LIST (CAAR GAMMA5*)))
      (VECTOR1 V)
      (RETURN (CAR V)))) 
(ENDMODULE) 
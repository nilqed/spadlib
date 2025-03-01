(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EXTOUT)) 
(FLUID '(*ALLFAC *DIV *MCD *NOEQUIV *PRI *RAT FACTORS* KORD* *COMBINELOGS WTL*)) 
(GLOBAL '(DNL* ORDL* UPL*)) 
(SWITCH (LIST (LIST 'EQUAL 'ALLFAC 'ON) 'DIV (LIST 'EQUAL 'PRI 'ON) 'RAT)) 
(PUT 'FACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR 'DEFINED-ON-LINE '51) 
(PUT 'FACTOR 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'FACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR (U) (FACTOR1 U T 'FACTORS*)) 
(PUT 'FACTOR1 'NUMBER-OF-ARGS 3) 
(PUT 'FACTOR1 'DEFINED-ON-LINE '54) 
(PUT 'FACTOR1 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'FACTOR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FACTOR1 (U V W)
    (PROG (X Y Z R)
      (SETQ Y (LISPEVAL W))
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((AND (SETQ X (GETRTYPE J)) (SETQ Z (GET X 'FACTOR1FN)))
             (APPLY2 Z U V))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND (EQCAR (SETQ J (REVAL1 J T)) 'LIST) (CDR J)))
                  (RETURN NIL)))
                (PROGN (SETQ R (APPEND R (CDDR J))) (SETQ J (CADR J)))
                (GO WHILELABEL))
              (SETQ X (*A2KWOWEIGHT J))
              (COND (V (SETQ Y (ACONC* (DELETE X Y) X)))
                    ((NOT (MEMBER X Y)) (MSGPRI NIL J "not found" NIL NIL))
                    (T (SETQ Y (DELETE X Y))))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SET W Y)
      (COND (R (RETURN (FACTOR1 R V W)))))) 
(PUT 'REMFAC 'NUMBER-OF-ARGS 1) 
(PUT 'REMFAC 'DEFINED-ON-LINE '71) 
(PUT 'REMFAC 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'REMFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMFAC (U) (FACTOR1 U NIL 'FACTORS*)) 
(RLISTAT '(FACTOR REMFAC)) 
(PUT 'ORDER 'NUMBER-OF-ARGS 1) 
(PUT 'ORDER 'DEFINED-ON-LINE '76) 
(PUT 'ORDER 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'ORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ORDER (U)
    (PROGN
     (RMSUBS)
     (COND ((AND U (NULL (CAR U)) (NULL (CDR U))) (SETQ ORDL* NIL))
           (T
            (PROG (X)
              (SETQ X (KERNEL-LIST U))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (PROGN
                  (COND ((MEMBER X ORDL*) (SETQ ORDL* (DELETE X ORDL*))))
                  (SETQ ORDL* (ACONC* ORDL* X))))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB)))))) 
(RLISTAT '(ORDER)) 
(PUT 'UP 'NUMBER-OF-ARGS 1) 
(PUT 'UP 'DEFINED-ON-LINE '86) 
(PUT 'UP 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'UP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UP (U) (FACTOR1 U T 'UPL*)) 
(PUT 'DOWN 'NUMBER-OF-ARGS 1) 
(PUT 'DOWN 'DEFINED-ON-LINE '89) 
(PUT 'DOWN 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'DOWN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DOWN (U) (FACTOR1 U T 'DNL*)) 
(PUT 'FORMOP 'NUMBER-OF-ARGS 1) 
(PUT 'FORMOP 'DEFINED-ON-LINE '94) 
(PUT 'FORMOP 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'FORMOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORMOP (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) U)
          (T (RADDF (MULTOP (CAAR U) (FORMOP (CDAR U))) (FORMOP (CDR U)))))) 
(PUT 'MULTOP 'NUMBER-OF-ARGS 2) 
(PUT 'MULTOP 'DEFINED-ON-LINE '98) 
(PUT 'MULTOP 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'MULTOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTOP (U V)
    (COND
     ((NULL KORD*)
      ((LAMBDA (G188)
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G188 V)) (T (POLY-MULTF G188 V))))
       (LIST (CONS U 1))))
     ((EQ (CAR U) 'K*) V) (T (RMULTPF U V)))) 
(DE LCX (U) (CDR (CARX U 'LCX))) 
(PUT 'LCX 'NUMBER-OF-ARGS 1) 
(PUT 'LCX 'DEFINED-ON-LINE '103) 
(PUT 'LCX 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'LCX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LCX 'INLINE '(LAMBDA (U) (CDR (CARX U 'LCX)))) 
(PUT 'QUOTOF 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTOF 'DEFINED-ON-LINE '108) 
(PUT 'QUOTOF 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'QUOTOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTOF (P Q)
    (COND ((NULL P) NIL) ((EQUAL P Q) 1) ((EQUAL Q 1) P)
          ((OR (ATOM Q) (ATOM (CAR Q))) (QUOTOFD P Q))
          ((OR (ATOM P) (ATOM (CAR P)))
           ((LAMBDA (X)
              (CONS
               (CONS
                (GETPOWER (FKERN (CAAAR Q))
                          (COND ((NUMBERP X) (MINUS X)) (T (LIST 'MINUS X))))
                (QUOTOF P (CDR (CARX Q 'LCX))))
               NIL))
            (CDAAR Q)))
          (T
           ((LAMBDA (X Y)
              (COND
               ((EQ (CAR X) (CAR Y))
                ((LAMBDA (N W Z)
                   (COND ((EQUAL N 0) (RADDF W Z))
                         (T (CONS (CONS (CONS (CAR Y) N) W) Z))))
                 (DIFFERENCE (CDR X) (CDR Y))
                 (QUOTOF (CDAR P) (CDR (CARX Q 'LCX))) (QUOTOF (CDR P) Q)))
               ((ORDOP (CAR X) (CAR Y))
                (CONS (CONS X (QUOTOF (CDAR P) Q)) (QUOTOF (CDR P) Q)))
               (T
                (CONS
                 (CONS (GETPOWER (FKERN (CAR Y)) (MINUS (CDR Y)))
                       (QUOTOF P (CDR (CARX Q 'LCX))))
                 NIL))))
            (CAAR P) (CAAR Q))))) 
(PUT 'QUOTOFD 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTOFD 'DEFINED-ON-LINE '132) 
(PUT 'QUOTOFD 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'QUOTOFD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTOFD (P Q)
    (COND ((NULL P) NIL) ((OR (ATOM P) (ATOM (CAR P))) (QUOTODD P Q))
          (T (CONS (CONS (CAAR P) (QUOTOFD (CDAR P) Q)) (QUOTOFD (CDR P) Q))))) 
(PUT 'QUOTODD 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTODD 'DEFINED-ON-LINE '139) 
(PUT 'QUOTODD 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'QUOTODD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTODD (P Q)
    (COND ((AND (ATOM P) (ATOM Q)) (INT-EQUIV-CHK (MKRN P Q)))
          (T (LOWEST-TERMS P Q)))) 
(PUT 'LOWEST-TERMS 'NUMBER-OF-ARGS 2) 
(PUT 'LOWEST-TERMS 'DEFINED-ON-LINE '144) 
(PUT 'LOWEST-TERMS 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'LOWEST-TERMS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOWEST-TERMS (U V)
    (COND ((EQUAL U V) 1)
          ((OR (FLAGP DMODE* 'FIELD)
               (AND (NOT (ATOM U)) (FLAGP (CAR U) 'FIELD))
               (AND (NOT (ATOM V)) (FLAGP (CAR V) 'FIELD)))
           (DCOMBINE* U V 'QUOTIENT))
          (T
           (PROG (X)
             (COND
              ((AND (ATOM (SETQ X (DCOMBINE* U V 'GCD))) (NEQ X 1))
               (PROGN
                (SETQ U (DCOMBINE* U X 'QUOTIENT))
                (SETQ V (DCOMBINE* V X 'QUOTIENT)))))
             (RETURN (COND ((EQUAL V 1) U) (T (CONS '|:RN:| (CONS U V))))))))) 
(PUT 'DCOMBINE* 'NUMBER-OF-ARGS 3) 
(PUT 'DCOMBINE* 'DEFINED-ON-LINE '160) 
(PUT 'DCOMBINE* 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'DCOMBINE* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DCOMBINE* (U V W)
    (COND ((AND (ATOM U) (ATOM V)) (APPLY2 W U V)) (T (DCOMBINE U V W)))) 
(PUT 'CKRN 'NUMBER-OF-ARGS 1) 
(PUT 'CKRN 'DEFINED-ON-LINE '163) 
(PUT 'CKRN 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'CKRN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CKRN (U)
    (COND
     ((AND (FLAGP DMODE* 'FIELD) (NOT (MEMQ DMODE* '(|:RD:| |:CR:|))))
      (PROG (X)
        (SETQ X (LNC U))
        (SETQ X
                ((LAMBDA (G619)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF X G619))
                         (T (POLY-MULTF X G619))))
                 (CKRN1 (QUOTFD U X))))
        (COND ((NULL X) (SETQ X 1)))
        (RETURN X)))
     (T (CKRN1 U)))) 
(PUT 'CKRN1 'NUMBER-OF-ARGS 1) 
(PUT 'CKRN1 'DEFINED-ON-LINE '175) 
(PUT 'CKRN1 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'CKRN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CKRN1 (U)
    (PROG (X)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN U)))
     A
      (SETQ X (GCK2 (CKRN1 (CDAR U)) X))
      (COND
       ((NULL (CDR U))
        (RETURN
         (COND ((AND *NCMP (NONCOMP1 (CAAAR U))) X)
               (T (LIST (CONS (CAAR U) X))))))
       ((OR (OR (ATOM (CDR U)) (ATOM (CAR (CDR U))))
            (NOT (EQ (CAAAR U) (CAAADR U))))
        (RETURN (GCK2 (CKRN1 (CDR U)) X))))
      (SETQ U (CDR U))
      (GO A))) 
(PUT 'GCK2 'NUMBER-OF-ARGS 2) 
(PUT 'GCK2 'DEFINED-ON-LINE '187) 
(PUT 'GCK2 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'GCK2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCK2 (U V)
    (COND ((NULL V) U) ((EQUAL U V) U)
          ((OR (ATOM U) (ATOM (CAR U)))
           (COND
            ((OR (ATOM V) (ATOM (CAR V)))
             (COND
              ((OR (FLAGP DMODE* 'FIELD) (AND (PAIRP U) (FLAGP (CAR U) 'FIELD))
                   (AND (PAIRP V) (FLAGP (CAR V) 'FIELD)))
               1)
              ((EQ DMODE* '|:GI:|) (INTGCDD U V)) (T (GCDDD U V))))
            (T (GCK2 U (CDARX V)))))
          ((OR (ATOM V) (ATOM (CAR V))) (GCK2 (CDARX U) V))
          (T
           ((LAMBDA (X Y)
              (COND
               ((EQ (CAR X) (CAR Y))
                (LIST
                 (CONS (COND ((GREATERP (CDR X) (CDR Y)) Y) (T X))
                       (GCK2 (CDARX U) (CDARX V)))))
               ((ORDOP (CAR X) (CAR Y)) (GCK2 (CDARX U) V))
               (T (GCK2 U (CDARX V)))))
            (CAAR U) (CAAR V))))) 
(PUT 'CDARX 'NUMBER-OF-ARGS 1) 
(PUT 'CDARX 'DEFINED-ON-LINE '208) 
(PUT 'CDARX 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'CDARX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CDARX (U) (CDR (CARX U 'CDAR))) 
(PUT 'NEGF* 'NUMBER-OF-ARGS 1) 
(PUT 'NEGF* 'DEFINED-ON-LINE '211) 
(PUT 'NEGF* 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'NEGF* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEGF* (U) ((LAMBDA (*NOEQUIV) (NEGF U)) T)) 
(PUT 'PREPSQ* 'NUMBER-OF-ARGS 1) 
(PUT 'PREPSQ* 'DEFINED-ON-LINE '213) 
(PUT 'PREPSQ* 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'PREPSQ* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPSQ* (U)
    (PROG (X Y *COMBINELOGS)
      (COND ((NULL (CAR U)) (RETURN 0)))
      (SETQ X (SETKORDER ORDL*))
      (SETKORDER
       (APPEND
        (SORT
         (PROG (J FORALL-RESULT FORALL-ENDPTR)
           (SETQ J FACTORS*)
          STARTOVER
           (COND ((NULL J) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   ((LAMBDA (J)
                      (COND ((NOT (IDP J)) NIL)
                            ((SETQ Y (GET J 'PREPSQ*FN)) (APPLY2 Y U J))
                            (T
                             (PROG (K FORALL-RESULT FORALL-ENDPTR)
                               (SETQ K (GET J 'KLIST))
                               (COND ((NULL K) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (K) (CAR K)) (CAR K))
                                                NIL)))
                              LOOPLABEL
                               (SETQ K (CDR K))
                               (COND ((NULL K) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS ((LAMBDA (K) (CAR K)) (CAR K))
                                             NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
                    (CAR J)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ J (CDR J))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL J) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   ((LAMBDA (J)
                      (COND ((NOT (IDP J)) NIL)
                            ((SETQ Y (GET J 'PREPSQ*FN)) (APPLY2 Y U J))
                            (T
                             (PROG (K FORALL-RESULT FORALL-ENDPTR)
                               (SETQ K (GET J 'KLIST))
                               (COND ((NULL K) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (K) (CAR K)) (CAR K))
                                                NIL)))
                              LOOPLABEL
                               (SETQ K (CDR K))
                               (COND ((NULL K) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS ((LAMBDA (K) (CAR K)) (CAR K))
                                             NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
                    (CAR J)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ J (CDR J))
           (GO LOOPLABEL))
         'ORDOP)
        (APPEND (SORT FACTORS* 'ORDOP) ORDL*)))
      (COND
       ((OR (NEQ KORD* X) WTL*)
        (SETQ U (CONS (FORMOP (CAR U)) (FORMOP (CDR U))))))
      (SETQ U
              (COND
               ((OR *RAT *DIV UPL* DNL*)
                (REPLUS (PREPSQ*1 (CAR U) (CDR U) NIL)))
               (T (SQFORM U (FUNCTION PREPSQ*2)))))
      (SETKORDER X)
      (RETURN U))) 
(PUT 'PREPSQ*0 'NUMBER-OF-ARGS 2) 
(PUT 'PREPSQ*0 'DEFINED-ON-LINE '239) 
(PUT 'PREPSQ*0 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'PREPSQ*0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREPSQ*0 (U V)
    (PROG (X)
      (RETURN
       (COND ((NULL (CAR U)) NIL)
             ((NEQ (SETQ X (GCDF (CAR U) (CDR U))) 1)
              (PREPSQ*1 (QUOTF-FAIL (CAR U) X) (QUOTF-FAIL (CDR U) X) V))
             (T (PREPSQ*1 (CAR U) (CDR U) V)))))) 
(PUT 'PREPSQ*1 'NUMBER-OF-ARGS 3) 
(PUT 'PREPSQ*1 'DEFINED-ON-LINE '250) 
(PUT 'PREPSQ*1 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'PREPSQ*1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREPSQ*1 (U V W)
    (PROG (X Y Z)
      (COND
       ((AND (NOT (OR (ATOM U) (ATOM (CAR U))))
             (OR (MEMBER (CAAAR U) FACTORS*)
                 (AND (NOT (ATOM (CAAAR U)))
                      (MEMBER (CAR (CAAAR U)) FACTORS*))))
        (RETURN
         (NCONC*
          (COND ((EQUAL V 1) (PREPSQ*0 (CONS (CDAR U) V) (CONS (CAAR U) W)))
                (T
                 (PROG (N V1 Z1)
                   (SETQ N (CDAAR U))
                   (SETQ V1 V)
                   (SETQ Z1 (LIST (CONS (TO (CAAAR U) 1) 1)))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (SETQ Z (QUOTFM V1 Z1))) (RETURN NIL)))
                     (PROGN (SETQ V1 Z) (SETQ N (DIFFERENCE N 1)))
                     (GO WHILELABEL))
                   (RETURN
                    (PREPSQ*0 (CONS (CDAR U) V1)
                              (COND
                               ((GREATERP N 0) (CONS (CONS (CAAAR U) N) W))
                               ((LESSP N 0)
                                (CONS
                                 (GETPOWER (FKERN (LIST 'EXPT (CAAAR U) N)) 1)
                                 W))
                               (T W)))))))
          (PREPSQ*0 (CONS (CDR U) V) W)))))
      (COND
       ((NOT (OR (ATOM V) (ATOM (CAR V))))
        (PROG (J)
          (SETQ J KORD*)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J)
             (PROG (N Z1)
               (SETQ N 0)
               (SETQ N 0)
               (SETQ Z1 (LIST (CONS (TO J 1) 1)))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (SETQ Z (QUOTFM V Z1))) (RETURN NIL)))
                 (PROGN (SETQ N (DIFFERENCE N 1)) (SETQ V Z))
                 (GO WHILELABEL))
               (COND
                ((LESSP N 0)
                 (SETQ W (CONS (GETPOWER (FKERN (LIST 'EXPT J N)) 1) W))))))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB))))
      (COND ((KERNLP U) (PROGN (SETQ U (MKKL W U)) (SETQ W NIL))))
      (COND
       (DNL*
        (PROGN
         (SETQ X (COND ((NULL *ALLFAC) 1) (T (CKRN U))))
         (SETQ Z (CKRN* X DNL*))
         (SETQ X (QUOTOF X Z))
         (SETQ U (QUOTOF U Z))
         (SETQ V (QUOTOF V Z)))))
      (COND
       (UPL*
        (PROGN
         (SETQ Y (CKRN V))
         (SETQ Z (CKRN* Y UPL*))
         (SETQ Y (QUOTOF Y Z))
         (SETQ U (QUOTOF U Z))
         (SETQ V (QUOTOF V Z))))
       (*DIV (SETQ Y (CKRN V))) (T (SETQ Y 1)))
      (SETQ U (CANONSQ (CONS U (QUOTOF V Y))))
      (SETQ U (CONS (QUOTOF (CAR U) Y) (CDR U)))
      (COND
       (*ALLFAC
        (PROGN
         (SETQ X (CKRN (CAR U)))
         (SETQ Y (CKRN (CDR U)))
         (COND
          ((AND (OR (NEQ X 1) (NEQ Y 1)) (OR (NEQ X (CAR U)) (NEQ Y (CDR U))))
           (PROGN
            (SETQ V (QUOTOF (CDR U) Y))
            (SETQ U (QUOTOF (CAR U) X))
            (SETQ W (PREPF (MKKL W X)))
            (SETQ X (PREPF Y))
            (SETQ U (ADDFACTORS W U))
            (SETQ V (ADDFACTORS X V))
            (RETURN
             (COND ((EQUAL V 1) (RMPLUS U))
                   (T
                    (LIST
                     (COND
                      ((EQCAR U 'MINUS)
                       (LIST 'MINUS (LIST 'QUOTIENT (CADR U) V)))
                      (T (LIST 'QUOTIENT U V)))))))))))))
      (RETURN
       (COND (W (LIST (RETIMES (ACONC* (EXCHK W) (PREPSQ U)))))
             (T (RMPLUS (PREPSQ U))))))) 
(PUT 'ADDFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'ADDFACTORS 'DEFINED-ON-LINE '322) 
(PUT 'ADDFACTORS 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'ADDFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDFACTORS (U V)
    (COND ((EQUAL U 1) (PREPF V)) ((EQUAL V 1) U)
          ((EQCAR U 'TIMES) (CONS 'TIMES (ACONC* (CDR U) (PREPF V))))
          (T (RETIMES (LIST U (PREPF V)))))) 
(PUT 'RMPLUS 'NUMBER-OF-ARGS 1) 
(PUT 'RMPLUS 'DEFINED-ON-LINE '330) 
(PUT 'RMPLUS 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'RMPLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMPLUS (U) (COND ((EQCAR U 'PLUS) (CDR U)) (T (LIST U)))) 
(PUT 'PREPSQ*2 'NUMBER-OF-ARGS 1) 
(PUT 'PREPSQ*2 'DEFINED-ON-LINE '332) 
(PUT 'PREPSQ*2 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'PREPSQ*2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPSQ*2 (U) (REPLUS (PREPSQ*1 U 1 NIL))) 
(PUT 'CKRN* 'NUMBER-OF-ARGS 2) 
(PUT 'CKRN* 'DEFINED-ON-LINE '334) 
(PUT 'CKRN* 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'CKRN* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CKRN* (U V)
    (COND ((NULL U) (ERRACH 'CKRN*)) ((OR (ATOM U) (ATOM (CAR U))) 1)
          ((MEMBER (CAAAR U) V)
           (LIST (CONS (CAAR U) (CKRN* (CDR (CARX U 'CKRN)) V))))
          (T (CKRN* (CDR (CARX U 'CKRN)) V)))) 
(PUT 'MKKL 'NUMBER-OF-ARGS 2) 
(PUT 'MKKL 'DEFINED-ON-LINE '341) 
(PUT 'MKKL 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'MKKL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKKL (U V) (COND ((NULL U) V) (T (MKKL (CDR U) (LIST (CONS (CAR U) V)))))) 
(PUT 'QUOTFM 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTFM 'DEFINED-ON-LINE '344) 
(PUT 'QUOTFM 'DEFINED-IN-FILE 'ALG/EXTOUT.RED) 
(PUT 'QUOTFM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTFM (U V)
    (PROG (*MCD) (SETQ *MCD T) (RETURN ((LAMBDA (*EXP) (QUOTF1 U V)) T)))) 
(ENDMODULE) 
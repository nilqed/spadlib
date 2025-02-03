(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MRIQE)) 
(REVISION 'MRIQE "$Id: mriqe.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'MRIQE "(c) 2008-2021 T. Sturm") 
(PUT 'MRI_QE 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_QE 'DEFINED-ON-LINE '32) 
(PUT 'MRI_QE 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_QE (F THEO)
    (PROG (W Q QL VARL VARLL)
      (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "++++++ MRI entering mri_qe"))))
      (COND (THEO (LPRIM (LIST "mri_mriqe: ignoring theory"))))
      (SETQ F (CL_SIMPL F NIL (MINUS 1)))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN F)))
      (SETQ W (MRI_SPLITQF F))
      (SETQ QL (CAR W))
      (SETQ VARLL (CADR W))
      (SETQ F (CADDR W))
      (PROG ()
       WHILELABEL
        (COND ((NOT QL) (RETURN NIL)))
        (PROGN
         (SETQ Q (CAR QL))
         (SETQ QL (CDR QL))
         (SETQ VARL (CAR VARLL))
         (SETQ VARLL (CDR VARLL))
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2
            (LIST "+++++ MRI current block is " (CONS Q (REVERSE VARL))))))
         (SETQ F (MRI_QEBLOCK F Q VARL))
         NIL)
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'MRI_SPLITQF 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_SPLITQF 'DEFINED-ON-LINE '54) 
(PUT 'MRI_SPLITQF 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_SPLITQF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_SPLITQF (F)
    (PROG (Q OP QL VARL VARLL)
      (SETQ Q (SETQ OP (COND ((ATOM F) F) (T (CAR F)))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((NEQ OP Q)
           (PROGN
            (SETQ QL (CONS Q QL))
            (SETQ VARLL (CONS VARL VARLL))
            (SETQ Q OP)
            (SETQ VARL NIL))))
         (SETQ VARL (CONS (CADR F) VARL))
         (SETQ F (CADDR F)))
        (COND
         ((NOT
           (NOT
            ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
             (SETQ OP (COND ((ATOM F) F) (T (CAR F)))))))
          (GO REPEATLABEL))))
      (SETQ QL (CONS Q QL))
      (SETQ VARLL (CONS VARL VARLL))
      (RETURN (LIST QL VARLL F)))) 
(PUT 'MRI_QEBLOCK 'NUMBER-OF-ARGS 3) 
(PUT 'MRI_QEBLOCK 'DEFINED-ON-LINE '74) 
(PUT 'MRI_QEBLOCK 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRI_QEBLOCK (F Q VARL)
    (COND ((EQ Q 'EX) (MRI_QEBLOCK1 F VARL))
          (T (CL_NNFNOT (MRI_QEBLOCK1 (CL_NNFNOT F) VARL))))) 
(PUT 'MRI_QEBLOCK1 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_QEBLOCK1 'DEFINED-ON-LINE '80) 
(PUT 'MRI_QEBLOCK1 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEBLOCK1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_QEBLOCK1 (F VARL)
    (PROG (V W)
      (PROG ()
       WHILELABEL
        (COND ((NOT VARL) (RETURN NIL)))
        (PROGN
         (SETQ V (MRI_VARSEL VARL))
         (SETQ VARL (LTO_DELQ V VARL))
         (COND
          ((MRI_REALVARP V)
           (PROGN
            (MRI_VBIN "+++ MRI expanding bounded quantifiers for real qe" F)
            (SETQ W (MRI_EXPAND F))
            (MRI_VBOUT F W)
            (SETQ W (MRI_QEREAL W V))
            (SETQ F (CAR W))
            (SETQ VARL (CONS (CDR W) VARL))))
          (T (SETQ F (MRI_QEINT F V)))))
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'MRI_VARSEL 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_VARSEL 'DEFINED-ON-LINE '98) 
(PUT 'MRI_VARSEL 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_VARSEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_VARSEL (VARL)
    (PROG (W)
      (COND
       ((AND *RLVERBOSE *RLMRIVB)
        (IOTO_TPRIN2
         (LIST "++++ MRI picking next variable from " (REVERSE VARL)
               " ... "))))
      (SETQ W (MRI_VARSELREAL VARL))
      (IOTO_PRIN2
       (COND (W (LIST W " (real)")) (T (LIST (CAR VARL) " (integer)"))))
      (RETURN (OR W (CAR VARL))))) 
(PUT 'MRI_VARSELREAL 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_VARSELREAL 'DEFINED-ON-LINE '108) 
(PUT 'MRI_VARSELREAL 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_VARSELREAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_VARSELREAL (VARL)
    (COND
     (VARL
      (COND ((MRI_REALVARP (CAR VARL)) (CAR VARL))
            (T (MRI_VARSELREAL (CDR VARL))))))) 
(PUT 'MRI_QEREAL 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_QEREAL 'DEFINED-ON-LINE '115) 
(PUT 'MRI_QEREAL 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEREAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_QEREAL (F V)
    (PROG (W VINT VTRUNC)
      (SETQ W (MRI_TRUNCATE F V))
      (SETQ F (CAR W))
      (SETQ VTRUNC (CADR W))
      (SETQ VINT (CADDR W))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "++++ MRI introduced new quantified variables " VTRUNC " and "
               VINT " for " V))))
      (SETQ W (LIST 'EX VTRUNC (MRI_CASE2 (MRI_LEMMA33 F VTRUNC) VTRUNC)))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ MRI entering real qe for " VTRUNC " ... "))))
      (SETQ W (MRI_QEREAL1 W))
      (COND
       (*RLVERBOSE (PROGN (IOTO_PRIN2 "finished") (MATHPRINT (RL_MK*FOF W)))))
      (RETURN (CONS W VINT)))) 
(PUT 'MRI_QEREAL1 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_QEREAL1 'DEFINED-ON-LINE '135) 
(PUT 'MRI_QEREAL1 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEREAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_QEREAL1 (F)
    (PROG (W *RLVERBOSE *MSG)
      (SETQ *RLVERBOSE *RLMRIVB2)
      (RL_SET '(MRI_OFSF))
      (SETQ W (RL_QE F NIL))
      (RL_SET '(MRI))
      (RETURN W))) 
(PUT 'MRI_TRUNCATE 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_TRUNCATE 'DEFINED-ON-LINE '144) 
(PUT 'MRI_TRUNCATE 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_TRUNCATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_TRUNCATE (F U)
    (PROG (UTRUNC UINT W)
      (MRI_VBIN (LIST "+++ MRI truncating " U) F)
      (SETQ UTRUNC (INTERN (LTO_IDCONCAT2 U '_TRUNC)))
      (MRI_PUTREAL UTRUNC)
      (SETQ UINT (INTERN (LTO_IDCONCAT2 U '_INT)))
      (SETQ W
              (MRI_SMARTAND
               (LIST (MRI_0MK2 'GEQ (LIST (CONS (CONS UTRUNC 1) 1)) 'REAL)
                     (MRI_0MK2 'LESSP
                               (ADDF (LIST (CONS (CONS UTRUNC 1) 1)) (NEGF 1))
                               'REAL)
                     (RL_SUBFOF (LIST (CONS U (LIST 'PLUS UINT UTRUNC))) F))))
      (MRI_VBOUT F W)
      (RETURN (LIST W UTRUNC UINT)))) 
(PUT 'MRI_SMARTAND 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_SMARTAND 'DEFINED-ON-LINE '157) 
(PUT 'MRI_SMARTAND 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_SMARTAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_SMARTAND (L)
    ((LAMBDA (G126)
       (COND ((AND G126 (CDR G126)) (CONS 'AND G126))
             ((NULL G126) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G126))))
     (PROG (F FORALL-RESULT FORALL-ENDPTR)
       (SETQ F L)
      STARTOVER
       (COND ((NULL F) (RETURN NIL)))
       (SETQ FORALL-RESULT
               ((LAMBDA (F)
                  (COND
                   ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
                    (APPEND (CDR F) NIL))
                   (T (LIST F))))
                (CAR F)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ F (CDR F))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL F) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               ((LAMBDA (F)
                  (COND
                   ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
                    (APPEND (CDR F) NIL))
                   (T (LIST F))))
                (CAR F)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ F (CDR F))
       (GO LOOPLABEL)))) 
(PUT 'MRI_VBIN 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_VBIN 'DEFINED-ON-LINE '161) 
(PUT 'MRI_VBIN 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_VBIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_VBIN (MSG F)
    (COND
     (*RLVERBOSE
      (COND
       (*RLMRIVB
        (PROGN
         (IOTO_TPRIN2 MSG)
         (COND
          (*RLMRIVBIO
           (PROGN (IOTO_TPRIN2 "+++ in:") (MATHPRINT (RL_MK*FOF F))))))))))) 
(PUT 'MRI_VBOUT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_VBOUT 'DEFINED-ON-LINE '171) 
(PUT 'MRI_VBOUT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_VBOUT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_VBOUT (F W)
    (COND
     (*RLVERBOSE
      (COND
       (*RLMRIVB
        (PROGN
         (COND
          (*RLMRIVBIO
           (PROGN (IOTO_PRIN2 "+++ out:") (MATHPRINT (RL_MK*FOF W))))
          ((NEQ W F) (MATHPRINT (RL_MK*FOF W)))
          (T (IOTO_PRIN2 " - no changes"))))))))) 
(PUT 'MRI_LEMMA33 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA33 'DEFINED-ON-LINE '183) 
(PUT 'MRI_LEMMA33 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA33 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA33 (F U)
    (PROG (W)
      (MRI_VBIN (LIST "+++ MRI applying Lemma 3.3 (remove " U " from floors)")
       F)
      (SETQ W (CL_APPLY2ATS1 F (FUNCTION MRI_LEMMA33AT) (LIST U)))
      (MRI_VBOUT F W)
      (RETURN W))) 
(PUT 'MRI_LEMMA33AT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA33AT 'DEFINED-ON-LINE '191) 
(PUT 'MRI_LEMMA33AT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA33AT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA33AT (AT U)
    (PROG (LHS CD PHI S N)
      (SETQ LHS (MRI_ARG2L AT))
      (COND ((NOT (MRI_FLOORKERNELP LHS)) (RETURN AT)))
      (SETQ CD (MRI_LEMMA33F LHS U))
      (RETURN
       ((LAMBDA (G130)
          (COND ((AND G130 (CDR G130)) (CONS 'OR G130))
                ((NULL G130) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G130))))
        (PROG (C FORALL-RESULT FORALL-ENDPTR)
          (SETQ C CD)
          (COND ((NULL C) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (C)
                              (PROGN
                               (SETQ PHI (CAR C))
                               (SETQ S (CADR C))
                               (SETQ N (CADDR C))
                               (CONS 'AND
                                     (LIST PHI
                                           (MRI_0MK2 (MRI_OP AT)
                                                     (ADDF
                                                      ((LAMBDA (G128)
                                                         (COND
                                                          (*PHYSOP-LOADED
                                                           (PHYSOP-MULTF N
                                                            G128))
                                                          (T
                                                           (POLY-MULTF N
                                                                       G128))))
                                                       (LIST
                                                        (CONS (CONS U 1) 1)))
                                                      S)
                                                     'REAL)))))
                            (CAR C))
                           NIL)))
         LOOPLABEL
          (SETQ C (CDR C))
          (COND ((NULL C) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (C)
                      (PROGN
                       (SETQ PHI (CAR C))
                       (SETQ S (CADR C))
                       (SETQ N (CADDR C))
                       (CONS 'AND
                             (LIST PHI
                                   (MRI_0MK2 (MRI_OP AT)
                                             (ADDF
                                              ((LAMBDA (G128)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF N G128))
                                                  (T (POLY-MULTF N G128))))
                                               (LIST (CONS (CONS U 1) 1)))
                                              S)
                                             'REAL)))))
                    (CAR C))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'MRI_LEMMA33F 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA33F 'DEFINED-ON-LINE '205) 
(PUT 'MRI_LEMMA33F 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA33F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA33F (F U)
    (PROG (CDLC CDKERN CDRED)
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RETURN (LIST (LIST 'TRUE F NIL)))))
      (SETQ CDLC (MRI_LEMMA33F (CDAR F) U))
      (SETQ CDKERN (MRI_LEMMA33K (CAAAR F) U))
      (SETQ CDRED (MRI_LEMMA33F (CDR F) U))
      (RETURN
       (MRI_ADD33 (MRI_MULT33 CDLC (MRI_EXPT33 CDKERN (CDAAR F))) CDRED)))) 
(PUT 'MRI_LEMMA33K 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA33K 'DEFINED-ON-LINE '215) 
(PUT 'MRI_LEMMA33K 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA33K 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA33K (KER U)
    (PROG (CD PHI S N PHIJ FS FSJ FSJ1 SS)
      (COND ((EQ KER U) (RETURN (LIST (LIST 'TRUE NIL 1)))))
      (COND
       ((IDP KER)
        (RETURN (LIST (LIST 'TRUE (LIST (CONS (CONS KER 1) 1)) NIL)))))
      (COND ((NOT (EQCAR KER 'FLOOR)) (REDERR (LIST "invalid kernel" KER))))
      (SETQ CD (MRI_LEMMA33F (CAR (SIMP (CADR KER))) U))
      (COND ((CDR CD) (REDERR "Check!")))
      (SETQ CD (CAR CD))
      (SETQ PHI (CAR CD))
      (SETQ S (CADR CD))
      (SETQ N (CADDR CD))
      (COND
       ((NOT (OR (ATOM N) (ATOM (CAR N))))
        (REDERR "mri_lemma33k: real variable with parametric coefficient")))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J 0)
         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (PROGN
                           (SETQ FS
                                   (LIST
                                    (CONS
                                     (CONS (*A2K (LIST 'FLOOR (PREPF S))) 1)
                                     1)))
                           (SETQ FSJ (ADDF FS J))
                           (SETQ FSJ1 (ADDF FSJ 1))
                           (SETQ SS
                                   (ADDF
                                    ((LAMBDA (G132)
                                       (COND
                                        (*PHYSOP-LOADED (PHYSOP-MULTF N G132))
                                        (T (POLY-MULTF N G132))))
                                     (LIST (CONS (CONS U 1) 1)))
                                    FS))
                           (SETQ PHIJ
                                   (CONS 'AND
                                         (LIST PHI
                                               (MRI_0MK2 'LEQ
                                                         (ADDF FSJ (NEGF SS))
                                                         'REAL)
                                               (MRI_0MK2 'LESSP
                                                         (ADDF SS (NEGF FSJ1))
                                                         'REAL))))
                           (LIST PHIJ FSJ NIL))
                          NIL)))
        LOOPLABEL
         (SETQ J (PLUS2 J 1))
         (COND ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  (PROGN
                   (SETQ FS
                           (LIST
                            (CONS (CONS (*A2K (LIST 'FLOOR (PREPF S))) 1) 1)))
                   (SETQ FSJ (ADDF FS J))
                   (SETQ FSJ1 (ADDF FSJ 1))
                   (SETQ SS
                           (ADDF
                            ((LAMBDA (G132)
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF N G132))
                                     (T (POLY-MULTF N G132))))
                             (LIST (CONS (CONS U 1) 1)))
                            FS))
                   (SETQ PHIJ
                           (CONS 'AND
                                 (LIST PHI
                                       (MRI_0MK2 'LEQ (ADDF FSJ (NEGF SS))
                                                 'REAL)
                                       (MRI_0MK2 'LESSP (ADDF SS (NEGF FSJ1))
                                                 'REAL))))
                   (LIST PHIJ FSJ NIL))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MRI_ADD33 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_ADD33 'DEFINED-ON-LINE '243) 
(PUT 'MRI_ADD33 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_ADD33 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_ADD33 (CD1 CD2)
    (PROG (PHI1 PHI2 S1 S2 N1 N2)
      (RETURN
       (PROG (T1 FORALL-RESULT FORALL-ENDPTR)
         (SETQ T1 CD1)
        STARTOVER
         (COND ((NULL T1) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (T1)
                    (PROGN
                     (SETQ PHI1 (CAR T1))
                     (SETQ S1 (CADR T1))
                     (SETQ N1 (CADDR T1))
                     (PROG (T2 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ T2 CD2)
                       (COND ((NULL T2) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (T2)
                                           (PROGN
                                            (SETQ PHI2 (CAR T2))
                                            (SETQ S2 (CADR T2))
                                            (SETQ N2 (CADDR T2))
                                            (LIST (CONS 'AND (LIST PHI1 PHI2))
                                                  (ADDF S1 S2) (ADDF N1 N2))))
                                         (CAR T2))
                                        NIL)))
                      LOOPLABEL
                       (SETQ T2 (CDR T2))
                       (COND ((NULL T2) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (T2)
                                   (PROGN
                                    (SETQ PHI2 (CAR T2))
                                    (SETQ S2 (CADR T2))
                                    (SETQ N2 (CADDR T2))
                                    (LIST (CONS 'AND (LIST PHI1 PHI2))
                                          (ADDF S1 S2) (ADDF N1 N2))))
                                 (CAR T2))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
                  (CAR T1)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ T1 (CDR T1))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL T1) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (T1)
                    (PROGN
                     (SETQ PHI1 (CAR T1))
                     (SETQ S1 (CADR T1))
                     (SETQ N1 (CADDR T1))
                     (PROG (T2 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ T2 CD2)
                       (COND ((NULL T2) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (T2)
                                           (PROGN
                                            (SETQ PHI2 (CAR T2))
                                            (SETQ S2 (CADR T2))
                                            (SETQ N2 (CADDR T2))
                                            (LIST (CONS 'AND (LIST PHI1 PHI2))
                                                  (ADDF S1 S2) (ADDF N1 N2))))
                                         (CAR T2))
                                        NIL)))
                      LOOPLABEL
                       (SETQ T2 (CDR T2))
                       (COND ((NULL T2) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (T2)
                                   (PROGN
                                    (SETQ PHI2 (CAR T2))
                                    (SETQ S2 (CADR T2))
                                    (SETQ N2 (CADDR T2))
                                    (LIST (CONS 'AND (LIST PHI1 PHI2))
                                          (ADDF S1 S2) (ADDF N1 N2))))
                                 (CAR T2))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
                  (CAR T1)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ T1 (CDR T1))
         (GO LOOPLABEL))))) 
(PUT 'MRI_MULT33 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_MULT33 'DEFINED-ON-LINE '258) 
(PUT 'MRI_MULT33 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_MULT33 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_MULT33 (CD1 CD2)
    (PROG (PHI1 PHI2 S1 S2 N1 N2)
      (RETURN
       (PROG (T1 FORALL-RESULT FORALL-ENDPTR)
         (SETQ T1 CD1)
        STARTOVER
         (COND ((NULL T1) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (T1)
                    (PROGN
                     (SETQ PHI1 (CAR T1))
                     (SETQ S1 (CADR T1))
                     (SETQ N1 (CADDR T1))
                     (PROG (T2 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ T2 CD2)
                       (COND ((NULL T2) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (T2)
                                           (PROGN
                                            (SETQ PHI2 (CAR T2))
                                            (SETQ S2 (CADR T2))
                                            (SETQ N2 (CADDR T2))
                                            (COND
                                             ((AND N1 N2)
                                              (REDERR
                                               "mri_mult33: real variable with degree > 1")))
                                            (COND
                                             ((AND (OR N1 N2)
                                                   (OR
                                                    (NOT
                                                     (OR (ATOM S1)
                                                         (ATOM (CAR S1))))
                                                    (NOT
                                                     (OR (ATOM S2)
                                                         (ATOM (CAR S2))))))
                                              (REDERR
                                               "mri_mult33: real variable with parametric coefficient")))
                                            (LIST (CONS 'AND (LIST PHI1 PHI2))
                                                  (COND
                                                   (*PHYSOP-LOADED
                                                    (PHYSOP-MULTF S1 S2))
                                                   (T (POLY-MULTF S1 S2)))
                                                  (ADDF
                                                   (COND
                                                    (*PHYSOP-LOADED
                                                     (PHYSOP-MULTF N1 S2))
                                                    (T (POLY-MULTF N1 S2)))
                                                   (COND
                                                    (*PHYSOP-LOADED
                                                     (PHYSOP-MULTF N2 S1))
                                                    (T (POLY-MULTF N2 S1)))))))
                                         (CAR T2))
                                        NIL)))
                      LOOPLABEL
                       (SETQ T2 (CDR T2))
                       (COND ((NULL T2) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (T2)
                                   (PROGN
                                    (SETQ PHI2 (CAR T2))
                                    (SETQ S2 (CADR T2))
                                    (SETQ N2 (CADDR T2))
                                    (COND
                                     ((AND N1 N2)
                                      (REDERR
                                       "mri_mult33: real variable with degree > 1")))
                                    (COND
                                     ((AND (OR N1 N2)
                                           (OR
                                            (NOT
                                             (OR (ATOM S1) (ATOM (CAR S1))))
                                            (NOT
                                             (OR (ATOM S2) (ATOM (CAR S2))))))
                                      (REDERR
                                       "mri_mult33: real variable with parametric coefficient")))
                                    (LIST (CONS 'AND (LIST PHI1 PHI2))
                                          (COND
                                           (*PHYSOP-LOADED
                                            (PHYSOP-MULTF S1 S2))
                                           (T (POLY-MULTF S1 S2)))
                                          (ADDF
                                           (COND
                                            (*PHYSOP-LOADED
                                             (PHYSOP-MULTF N1 S2))
                                            (T (POLY-MULTF N1 S2)))
                                           (COND
                                            (*PHYSOP-LOADED
                                             (PHYSOP-MULTF N2 S1))
                                            (T (POLY-MULTF N2 S1)))))))
                                 (CAR T2))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
                  (CAR T1)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ T1 (CDR T1))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL T1) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (T1)
                    (PROGN
                     (SETQ PHI1 (CAR T1))
                     (SETQ S1 (CADR T1))
                     (SETQ N1 (CADDR T1))
                     (PROG (T2 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ T2 CD2)
                       (COND ((NULL T2) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (T2)
                                           (PROGN
                                            (SETQ PHI2 (CAR T2))
                                            (SETQ S2 (CADR T2))
                                            (SETQ N2 (CADDR T2))
                                            (COND
                                             ((AND N1 N2)
                                              (REDERR
                                               "mri_mult33: real variable with degree > 1")))
                                            (COND
                                             ((AND (OR N1 N2)
                                                   (OR
                                                    (NOT
                                                     (OR (ATOM S1)
                                                         (ATOM (CAR S1))))
                                                    (NOT
                                                     (OR (ATOM S2)
                                                         (ATOM (CAR S2))))))
                                              (REDERR
                                               "mri_mult33: real variable with parametric coefficient")))
                                            (LIST (CONS 'AND (LIST PHI1 PHI2))
                                                  (COND
                                                   (*PHYSOP-LOADED
                                                    (PHYSOP-MULTF S1 S2))
                                                   (T (POLY-MULTF S1 S2)))
                                                  (ADDF
                                                   (COND
                                                    (*PHYSOP-LOADED
                                                     (PHYSOP-MULTF N1 S2))
                                                    (T (POLY-MULTF N1 S2)))
                                                   (COND
                                                    (*PHYSOP-LOADED
                                                     (PHYSOP-MULTF N2 S1))
                                                    (T (POLY-MULTF N2 S1)))))))
                                         (CAR T2))
                                        NIL)))
                      LOOPLABEL
                       (SETQ T2 (CDR T2))
                       (COND ((NULL T2) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (T2)
                                   (PROGN
                                    (SETQ PHI2 (CAR T2))
                                    (SETQ S2 (CADR T2))
                                    (SETQ N2 (CADDR T2))
                                    (COND
                                     ((AND N1 N2)
                                      (REDERR
                                       "mri_mult33: real variable with degree > 1")))
                                    (COND
                                     ((AND (OR N1 N2)
                                           (OR
                                            (NOT
                                             (OR (ATOM S1) (ATOM (CAR S1))))
                                            (NOT
                                             (OR (ATOM S2) (ATOM (CAR S2))))))
                                      (REDERR
                                       "mri_mult33: real variable with parametric coefficient")))
                                    (LIST (CONS 'AND (LIST PHI1 PHI2))
                                          (COND
                                           (*PHYSOP-LOADED
                                            (PHYSOP-MULTF S1 S2))
                                           (T (POLY-MULTF S1 S2)))
                                          (ADDF
                                           (COND
                                            (*PHYSOP-LOADED
                                             (PHYSOP-MULTF N1 S2))
                                            (T (POLY-MULTF N1 S2)))
                                           (COND
                                            (*PHYSOP-LOADED
                                             (PHYSOP-MULTF N2 S1))
                                            (T (POLY-MULTF N2 S1)))))))
                                 (CAR T2))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
                  (CAR T1)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ T1 (CDR T1))
         (GO LOOPLABEL))))) 
(PUT 'MRI_EXPT33 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_EXPT33 'DEFINED-ON-LINE '278) 
(PUT 'MRI_EXPT33 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_EXPT33 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_EXPT33 (CD K)
    (PROG (PHI S N)
      (RETURN
       (PROG (C FORALL-RESULT FORALL-ENDPTR)
         (SETQ C CD)
         (COND ((NULL C) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (C)
                             (PROGN
                              (SETQ PHI (CAR C))
                              (SETQ S (CADR C))
                              (SETQ N (CADDR C))
                              (COND
                               ((AND N (GREATERP K 1))
                                (REDERR
                                 "mri_expt33: real variable with degree > 1")))
                              (LIST PHI (EXPTF S K) N)))
                           (CAR C))
                          NIL)))
        LOOPLABEL
         (SETQ C (CDR C))
         (COND ((NULL C) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (C)
                     (PROGN
                      (SETQ PHI (CAR C))
                      (SETQ S (CADR C))
                      (SETQ N (CADDR C))
                      (COND
                       ((AND N (GREATERP K 1))
                        (REDERR "mri_expt33: real variable with degree > 1")))
                      (LIST PHI (EXPTF S K) N)))
                   (CAR C))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MRI_QEINT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_QEINT 'DEFINED-ON-LINE '290) 
(PUT 'MRI_QEINT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_QEINT (F V)
    (PROG (W)
      (SETQ W (LIST 'EX V (MRI_CASE1 (MRI_LEMMA32 F V) V)))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ MRI entering integer qe for " V " ... "))))
      (SETQ W (MRI_QEINT1 W))
      (COND
       (*RLVERBOSE (PROGN (IOTO_PRIN2 "finished") (MATHPRINT (RL_MK*FOF W)))))
      (RETURN W))) 
(PUT 'MRI_QEINT1 'NUMBER-OF-ARGS 1) 
(PUT 'MRI_QEINT1 'DEFINED-ON-LINE '303) 
(PUT 'MRI_QEINT1 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_QEINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRI_QEINT1 (F)
    (PROG (W *MSG *RLVERBOSE)
      (SETQ *RLVERBOSE *RLMRIVB2)
      (RL_SET '(MRI_PASF))
      (SETQ W (PASF_WQE F NIL))
      (RL_SET '(MRI))
      (RETURN W))) 
(PUT 'MRI_LEMMA32 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA32 'DEFINED-ON-LINE '312) 
(PUT 'MRI_LEMMA32 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA32 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA32 (F XI)
    (PROG (W)
      (MRI_VBIN (LIST "+++ MRI applying Lemma 3.2 (remove " XI " from floors)")
       F)
      (SETQ W (CL_APPLY2ATS1 F (FUNCTION MRI_LEMMA32AT) (LIST XI)))
      (MRI_VBOUT F W)
      (RETURN W))) 
(PUT 'MRI_LEMMA32AT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA32AT 'DEFINED-ON-LINE '320) 
(PUT 'MRI_LEMMA32AT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA32AT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA32AT (F XI)
    (MRI_0MK2 (MRI_OP F) (MRI_LEMMA32F (MRI_ARG2L F) XI) (MRI_TYPE F))) 
(PUT 'MRI_LEMMA32F 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_LEMMA32F 'DEFINED-ON-LINE '323) 
(PUT 'MRI_LEMMA32F 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_LEMMA32F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_LEMMA32F (U XI)
    (PROG (W C V R XPND)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN U)))
      (SETQ C (MRI_LEMMA32F (CDAR U) XI))
      (SETQ R (MRI_LEMMA32F (CDR U) XI))
      (SETQ V (CAAAR U))
      (COND
       ((IDP V)
        (RETURN
         (ADDF
          (EXPTF
           ((LAMBDA (G134)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF C G134))
                    (T (POLY-MULTF C G134))))
            (LIST (CONS (CONS V 1) 1)))
           (CDAAR U))
          R))))
      (SETQ W (SFTO_REORDER (CAR (SIMP (CADR V))) XI))
      (COND
       ((OR (OR (ATOM W) (ATOM (CAR W))) (NOT (EQ (CAAAR W) XI)))
        (RETURN
         (ADDF
          (EXPTF
           ((LAMBDA (G136)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF C G136))
                    (T (POLY-MULTF C G136))))
            (LIST (CONS (CONS V 1) 1)))
           (CDAAR U))
          R))))
      (SETQ XPND
              (ADDF
               ((LAMBDA (G138)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR W) G138))
                        (T (POLY-MULTF (CDAR W) G138))))
                (LIST (CONS (CONS XI 1) 1)))
               (CAR (SIMP (LIST 'FLOOR (PREPF (CDR W)))))))
      (RETURN
       (ADDF
        (EXPTF
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF C XPND)) (T (POLY-MULTF C XPND)))
         (CDAAR U))
        R)))) 
(PUT 'MRI_CASE1 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_CASE1 'DEFINED-ON-LINE '339) 
(PUT 'MRI_CASE1 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE1 (F XI)
    (PROG (W)
      (MRI_VBIN
       (LIST "+++ MRI applying Theorem 3.1 Case 1 (restrict " XI
             " to integer atfs)")
       F)
      (SETQ W
              (CL_SIMPL (CL_APPLY2ATS1 F (FUNCTION MRI_CASE1AT) (LIST XI)) NIL
                        (MINUS 1)))
      (MRI_VBOUT F W)
      (RETURN W))) 
(PUT 'MRI_CASE1AT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_CASE1AT 'DEFINED-ON-LINE '348) 
(PUT 'MRI_CASE1AT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE1AT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE1AT (F XI)
    (PROG (LHS NXI S FS OP)
      (SETQ LHS (SFTO_REORDER (MRI_ARG2L F) XI))
      (COND
       ((OR (OR (ATOM LHS) (ATOM (CAR LHS))) (NOT (EQ (CAAAR LHS) XI)))
        (RETURN F)))
      (SETQ NXI
              ((LAMBDA (G140)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR LHS) G140))
                       (T (POLY-MULTF (CDAR LHS) G140))))
               (LIST (CONS (CONS (CAAAR LHS) 1) 1))))
      (SETQ S (NEGF (CDR LHS)))
      (SETQ FS (CAR (SIMP (LIST 'FLOOR (PREPF S)))))
      (SETQ OP (MRI_OP F))
      (RETURN (MRI_CASE1AT1 OP NXI S FS)))) 
(PUT 'MRI_CASE1AT1 'NUMBER-OF-ARGS 4) 
(PUT 'MRI_CASE1AT1 'DEFINED-ON-LINE '360) 
(PUT 'MRI_CASE1AT1 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE1AT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE1AT1 (OP NXI S FS)
    (COND ((EQ OP 'NEQ) (CL_NNFNOT (MRI_CASE1AT2 'EQUAL NXI S FS)))
          ((EQ OP 'GEQ) (CL_NNFNOT (MRI_CASE1AT2 'LESSP NXI S FS)))
          ((EQCAR OP 'NCONG)
           (CL_NNFNOT (MRI_CASE1AT2 (MRI_MKOP 'CONG (CDR OP)) NXI S FS)))
          ((EQ OP 'GREATERP) (CL_NNFNOT (MRI_CASE1AT2 'LEQ NXI S FS)))
          (T (MRI_CASE1AT2 OP NXI S FS)))) 
(PUT 'MRI_CASE1AT2 'NUMBER-OF-ARGS 4) 
(PUT 'MRI_CASE1AT2 'DEFINED-ON-LINE '375) 
(PUT 'MRI_CASE1AT2 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE1AT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE1AT2 (OP NXI S FS)
    (PROG (W WW WWW)
      (SETQ W (MRI_0MK2 OP (ADDF NXI (NEGF FS)) 'INT))
      (COND
       ((OR (EQ OP 'EQUAL) (EQCAR OP 'CONG))
        (RETURN
         (CONS 'AND (LIST W (MRI_0MK2 'EQUAL (ADDF S (NEGF FS)) NIL))))))
      (COND ((EQ OP 'LEQ) (RETURN W)))
      (SETQ WW (MRI_0MK2 'EQUAL (ADDF NXI (NEGF FS)) 'INT))
      (SETQ WWW (MRI_0MK2 'LESSP (ADDF FS (NEGF S)) NIL))
      (RETURN (CONS 'OR (LIST W (CONS 'AND (LIST WW WWW))))))) 
(PUT 'MRI_CASE2 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_CASE2 'DEFINED-ON-LINE '388) 
(PUT 'MRI_CASE2 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE2 (F U)
    (PROG (W)
      (MRI_VBIN
       (LIST "+++ MRI applying Theorem 3.1 Case 2 (remove " U
             " from congruences)")
       F)
      (SETQ W
              (CL_SIMPL (CL_APPLY2ATS1 F (FUNCTION MRI_CASE2AT) (LIST U)) NIL
                        (MINUS 1)))
      (MRI_VBOUT F W)
      (RETURN W))) 
(PUT 'MRI_CASE2AT 'NUMBER-OF-ARGS 2) 
(PUT 'MRI_CASE2AT 'DEFINED-ON-LINE '397) 
(PUT 'MRI_CASE2AT 'DEFINED-IN-FILE 'REDLOG/MRI/MRIQE.RED) 
(PUT 'MRI_CASE2AT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRI_CASE2AT (AT U)
    (PROG (W N S FS FSI NUMS)
      (COND
       ((OR (NOT (MRI_CONGP AT)) (NOT (MEMQ U (KERNELS (MRI_ARG2L AT)))))
        (RETURN AT)))
      (SETQ W (SFTO_REORDER (MRI_ARG2L AT) U))
      (COND
       ((NEQ (CDAAR W) 1)
        (REDERR "mri_case2at: real variable with degree > 1")))
      (SETQ N (CDAR W))
      (COND
       ((NOT (OR (ATOM N) (ATOM (CAR N))))
        (REDERR "mri_case2at: real variable with parametric coefficient")))
      (SETQ S (NEGF (CDR W)))
      (SETQ FS (LIST (CONS (CONS (*A2K (LIST 'FLOOR (PREPF S))) 1) 1)))
      (SETQ NUMS
              (ADDF
               ((LAMBDA (G142)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF N G142))
                        (T (POLY-MULTF N G142))))
                (LIST (CONS (CONS U 1) 1)))
               (NEGF S)))
      (RETURN
       ((LAMBDA (G144)
          (COND ((AND G144 (CDR G144)) (CONS 'OR G144))
                ((NULL G144) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G144))))
        (PROG (I FORALL-RESULT FORALL-ENDPTR)
          (SETQ I 0)
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           (PROGN
                            (SETQ FSI (ADDF FS (NEGF I)))
                            (SETQ W (ADDF NUMS FSI))
                            (CONS 'AND
                                  (LIST (MRI_0MK2 'EQUAL W NIL)
                                        (MRI_0MK2 (MRI_OP AT) FSI 'INT))))
                           NIL)))
         LOOPLABEL
          (SETQ I (PLUS2 I 1))
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   (PROGN
                    (SETQ FSI (ADDF FS (NEGF I)))
                    (SETQ W (ADDF NUMS FSI))
                    (CONS 'AND
                          (LIST (MRI_0MK2 'EQUAL W NIL)
                                (MRI_0MK2 (MRI_OP AT) FSI 'INT))))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(ENDMODULE) 
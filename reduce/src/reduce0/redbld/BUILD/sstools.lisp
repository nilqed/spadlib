(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SSTOOLS)) 
(CREATE-PACKAGE '(SSTOOLS) NIL) 
(GLOBAL '(*SSTOOLS-LOADED)) 
(SETQ *SSTOOLS-LOADED T) 
(MODULE (LIST 'DOP)) 
(FLUID '(T_CHANGES_PARITY S_CHANGES_PARITY ASYMPLIS* NCMP*)) 
(SWITCH (LIST 'T_CHANGES_PARITY 'S_CHANGES_PARITY)) 
(PUT 'T_CHANGES_PARITY 'SIMPFG
     '((T (RMSUBS) (SETQ T_CHANGES_PARITY T) (CLEAR_T_CHANGES_PARITY))
       (NIL (RMSUBS) (SETQ T_CHANGES_PARITY NIL) (CLEAR_T_CHANGES_PARITY)))) 
(PUT 'S_CHANGES_PARITY 'SIMPFG
     '((T (RMSUBS) (SETQ S_CHANGES_PARITY T) (CLEAR_S_CHANGES_PARITY))
       (NIL (RMSUBS) (SETQ S_CHANGES_PARITY NIL) (CLEAR_S_CHANGES_PARITY)))) 
(PUT 'CLEAR_T_CHANGES_PARITY 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_T_CHANGES_PARITY 'DEFINED-ON-LINE '64) 
(PUT 'CLEAR_T_CHANGES_PARITY 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CLEAR_T_CHANGES_PARITY 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_T_CHANGES_PARITY NIL
    (PROG () (SETQ ASYMPLIS* (CLEAR_T_CHANGES_POWER_RULES ASYMPLIS*)))) 
(PUT 'CLEAR_S_CHANGES_PARITY 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_S_CHANGES_PARITY 'DEFINED-ON-LINE '69) 
(PUT 'CLEAR_S_CHANGES_PARITY 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CLEAR_S_CHANGES_PARITY 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_S_CHANGES_PARITY NIL
    (PROG () (SETQ ASYMPLIS* (CLEAR_S_CHANGES_POWER_RULES ASYMPLIS*)))) 
(PUT 'CLEAR_T_CHANGES_POWER_RULES 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAR_T_CHANGES_POWER_RULES 'DEFINED-ON-LINE '74) 
(PUT 'CLEAR_T_CHANGES_POWER_RULES 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CLEAR_T_CHANGES_POWER_RULES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAR_T_CHANGES_POWER_RULES (U)
    (COND ((NULL U) U)
          ((SMEMBER 'T (CAR U)) (CLEAR_T_CHANGES_POWER_RULES (CDR U)))
          (T (CONS (CAR U) (CLEAR_T_CHANGES_POWER_RULES (CDR U)))))) 
(PUT 'CLEAR_S_CHANGES_POWER_RULES 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAR_S_CHANGES_POWER_RULES 'DEFINED-ON-LINE '79) 
(PUT 'CLEAR_S_CHANGES_POWER_RULES 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CLEAR_S_CHANGES_POWER_RULES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAR_S_CHANGES_POWER_RULES (U)
    (COND ((NULL U) U)
          ((SMEMBER 'S (CAR U)) (CLEAR_S_CHANGES_POWER_RULES (CDR U)))
          (T (CONS (CAR U) (CLEAR_S_CHANGES_POWER_RULES (CDR U)))))) 
(SETQ NCMP* T) 
(RLISTAT '(FERMION)) 
(PUT 'FERMION 'NUMBER-OF-ARGS 1) 
(PUT 'FERMION 'DEFINED-ON-LINE '88) 
(PUT 'FERMION 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'FERMION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FERMION (U) (FERMION1 U)) 
(PUT 'FERMION1 'NUMBER-OF-ARGS 1) 
(PUT 'FERMION1 'DEFINED-ON-LINE '90) 
(PUT 'FERMION1 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'FERMION1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FERMION1 (U)
    (PROGN
     (FLAG U 'FERMIONIC)
     (PROG (J)
       (SETQ J U)
      LAB
       (COND ((NULL J) (RETURN NIL)))
       ((LAMBDA (J) (PROGN (FLAG (LIST J) 'FULL) (PUT J 'SIMPFN 'SIMPFERMION)))
        (CAR J))
       (SETQ J (CDR J))
       (GO LAB))
     (PROG (J)
       (SETQ J U)
      LAB
       (COND ((NULL J) (RETURN NIL)))
       ((LAMBDA (J) (NONCOM1 J)) (CAR J))
       (SETQ J (CDR J))
       (GO LAB)))) 
(PUT 'SIMPFERMION 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPFERMION 'DEFINED-ON-LINE '96) 
(PUT 'SIMPFERMION 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SIMPFERMION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPFERMION (U)
    (PROG (X Y Z)
      (COND ((SETQ X (OPMTCH U)) (RETURN (SIMP X))))
      (SETQ U
              (CONS (CAR U)
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDR U))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (REVAL1 J NIL)) (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (REVAL1 J NIL)) (CAR J)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ Y (MKSQ U 1))
      (COND ((OR (ATOM (CAR Y)) (ATOM (CAR (CAR Y)))) (RETURN Y)))
      (SETQ Z (CAAAR (CAR Y)))
      (COND ((OR (ATOM Z) (NULL (EQ (CAR Z) (CAR U)))) (RETURN Y)))
      (COND
       ((NULL (ATSOC Z ASYMPLIS*))
        (SETQ ASYMPLIS* (CONS (CONS Z 2) ASYMPLIS*))))
      (RETURN Y))) 
(RLISTAT '(BOSON)) 
(PUT 'BOSON 'NUMBER-OF-ARGS 1) 
(PUT 'BOSON 'DEFINED-ON-LINE '111) 
(PUT 'BOSON 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'BOSON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOSON (U) (BOSON1 U)) 
(PUT 'BOSON1 'NUMBER-OF-ARGS 1) 
(PUT 'BOSON1 'DEFINED-ON-LINE '113) 
(PUT 'BOSON1 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'BOSON1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOSON1 (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J) (PROGN (FLAG (LIST J) 'BOSONIC) (MKOP J))) (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'D 'SIMPFN 'SIMPDOP) 
(NONCOM1 'D) 
(PUT 'SIMPDOP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDOP 'DEFINED-ON-LINE '121) 
(PUT 'SIMPDOP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SIMPDOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDOP (U)
    (PROG (X)
      (SETQ U (CONS (REVAL1 (CAR U) NIL) (CDR U)))
      (COND ((SETQ X (OPMTCH (CONS 'D U))) (RETURN (SIMP X))))
      (SETQ X (SIMP (CADR U)))
      (COND ((NULL (CAR X)) (RETURN X)))
      (COND
       ((OR (OR (ATOM (CDR X)) (ATOM (CAR (CDR X))))
            (NULL (FERMIONICP (CAAAR (CDR X)))))
        (RETURN (MULTSQ (DOPF (CAR U) (CAR X)) (CONS 1 (CDR X))))))
      (REDERR "encountered non constant den in simpdop ..."))) 
(PUT 'DOPF 'NUMBER-OF-ARGS 2) 
(PUT 'DOPF 'DEFINED-ON-LINE '133) 
(PUT 'DOPF 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'DOPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOPF (N U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (CONS NIL 1))
          (T
           (ADDSQ
            (ADDSQ
             (MULTSQ (CONS (LIST (CONS (CAAR U) 1)) 1)
                     (DOPF N
                      (COND ((FERMIONICP (CAAAR U)) (NEGF (CDAR U)))
                            (T (CDAR U)))))
             (MULTSQ (DOPP N (CAAR U)) (CONS (CDAR U) 1)))
            (DOPF N (CDR U)))))) 
(PUT 'DOPP 'NUMBER-OF-ARGS 2) 
(PUT 'DOPP 'DEFINED-ON-LINE '140) 
(PUT 'DOPP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'DOPP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOPP (N U)
    (COND
     ((OR (ATOM (CAR U))
          (NULL
           (OR (FLAGP (CAAR U) 'FERMIONIC) (FLAGP (CAAR U) 'BOSONIC)
               (EQ (CAAR U) 'D) (EQ (CAAR U) 'DF))))
      (CONS NIL 1))
     ((GREATERP (CDR U) 1)
      (MULTSQ (CONS (CDR U) 1)
              (MULTSQ
               ((LAMBDA (U)
                  (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U))
                        (T (BASIC-SIMPEXPT U))))
                (LIST (CAR U) (DIFFERENCE (CDR U) 1)))
               (SIMPDOP (LIST N (CAR U))))))
     ((EQ (CAAR U) 'D)
      (COND ((EQUAL N (CADR (CAR U))) (SIMPDF (LIST (CADDR (CAR U)) 'X)))
            ((GREATERP N (CADR (CAR U)))
             (NEGSQ
              (SIMPDOP (LIST (CADR (CAR U)) (LIST 'D N (CADDR (CAR U)))))))
            (T (MKDSQ (LIST 'D N (CAR U)) 1))))
     (T (MKDSQ (LIST 'D N (CAR U)) 1)))) 
(PUT 'MKDSQ 'NUMBER-OF-ARGS 2) 
(PUT 'MKDSQ 'DEFINED-ON-LINE '157) 
(PUT 'MKDSQ 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'MKDSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKDSQ (U N)
    (PROG (X Z)
      (COND ((SETQ X (OPMTCH U)) (RETURN (SIMP X))))
      (SETQ X (MKSQ U N))
      (COND ((NULL (CAR X)) (RETURN X)))
      (SETQ Z (CAAAR (CAR X)))
      (COND
       ((AND (FERMIONICP Z) (NULL (ATSOC Z ASYMPLIS*)))
        (SETQ ASYMPLIS* (CONS (CONS Z 2) ASYMPLIS*))))
      (RETURN X))) 
(PUT 'DFDP 'NUMBER-OF-ARGS 3) 
(PUT 'DFDP 'DEFINED-ON-LINE '168) 
(PUT 'DFDP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'DFDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFDP (U V N)
    (PROG (X)
      (SETQ X (SIMPDF (LIST (CADDR U) V N)))
      (COND ((NULL (CAR X)) (RETURN X)))
      (COND
       ((OR (AND (EQ V 'T) T_CHANGES_PARITY) (AND (EQ V 'S) S_CHANGES_PARITY))
        (SETQ X (NEGSQ X))))
      (COND
       ((OR (OR (ATOM (CDR X)) (ATOM (CAR (CDR X))))
            (NULL (FERMIONICP (CAAAR (CDR X)))))
        (RETURN (MULTSQ (DOPF (CADR U) (CAR X)) (CONS 1 (CDR X)))))
       (T (REDERR "fermioic den encountered in dfdp"))))) 
(PUT 'D 'DFFORM 'DFDP) 
(PUT 'FERMIONICP 'NUMBER-OF-ARGS 1) 
(PUT 'FERMIONICP 'DEFINED-ON-LINE '183) 
(PUT 'FERMIONICP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'FERMIONICP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FERMIONICP (U)
    (AND (NULL (ATOM U))
         (OR (FLAGP (CAR U) 'FERMIONIC)
             (AND (EQ (CAR U) 'D) (NULL (FERMIONICP (CADDR U))))
             (AND (EQ (CAR U) 'DF) (FERMIONICP (CADDR U)))
             (AND (EQ (CAR U) 'DF)
                  (COND
                   ((FERMIONICP (CADR U))
                    (COND ((MEMQ 'S (CDDR U)) (NOT S_CHANGES_PARITY))
                          ((MEMQ 'T (CDDR U)) (NOT T_CHANGES_PARITY)) (T T)))
                   ((MEMQ 'S (CDDR U)) S_CHANGES_PARITY)
                   ((MEMQ 'T (CDDR U)) T_CHANGES_PARITY) (T NIL)))))) 
(PUT 'SSTOOLS-MULTFNC 'NUMBER-OF-ARGS 2) 
(PUT 'SSTOOLS-MULTFNC 'DEFINED-ON-LINE '197) 
(PUT 'SSTOOLS-MULTFNC 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSTOOLS-MULTFNC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SSTOOLS-MULTFNC (U V)
    (PROG (X Y)
      (SETQ X
              ((LAMBDA (G125)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) G125))
                       (T (POLY-MULTF (CDAR U) G125))))
               (LIST (CAR V))))
      (COND ((NULL X) NIL)
            ((AND (NOT (OR (ATOM X) (ATOM (CAR X)))) (EQ (CAAAR X) (CAAAR U)))
             (SETQ X
                     (ADDF
                      (COND
                       ((NULL
                         (SETQ Y (MKSPM (CAAAR U) (PLUS (CDAAR U) (CDAAR X)))))
                        NIL)
                       ((EQUAL Y 1) (CDAR X)) (T (LIST (CONS Y (CDAR X)))))
                      ((LAMBDA (G126)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G126 (CDR X)))
                               (T (POLY-MULTF G126 (CDR X)))))
                       (LIST (CONS (CAAR U) 1))))))
            ((AND *NCMP (NONCOMP1 (CAAAR U)))
             (COND
              ((ORDOP (CAAAR U) (CAAAR X)) (SETQ X (LIST (CONS (CAAR U) X))))
              ((NULL
                (SETQ Y
                        ((LAMBDA (G128)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G128 (CDAR X)))
                                 (T (POLY-MULTF G128 (CDAR X)))))
                         (LIST (CONS (CAAR U) 1)))))
               (SETQ X
                       ((LAMBDA (G130)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G130 (CDR X)))
                                (T (POLY-MULTF G130 (CDR X)))))
                        (LIST (CONS (CAAR U) 1)))))
              (T
               (SETQ X
                       (ADDF
                        (LIST
                         (CONS (CAAR X)
                               (COND
                                ((AND (FERMIONICP (CAAAR U))
                                      (FERMIONICP (CAAAR X)))
                                 (NEGF Y))
                                (T Y))))
                        ((LAMBDA (G132)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G132 (CDR X)))
                                 (T (POLY-MULTF G132 (CDR X)))))
                         (LIST (CONS (CAAR U) 1))))))))
            (T
             ((LAMBDA (**PROCESSED)
                (SETQ X
                        ((LAMBDA (G134)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G134 X))
                                 (T (POLY-MULTF G134 X))))
                         (LIST (CONS (CAAR U) 1)))))
              T)))
      (RETURN
       (ADDF X
             (ADDF
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR U) V))
                    (T (POLY-MULTF (CDR U) V)))
              ((LAMBDA (G136)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G136 (CDR V)))
                       (T (POLY-MULTF G136 (CDR V)))))
               (LIST (CAR U)))))))) 
(PUT 'DIFFF 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFF 'DEFINED-ON-LINE '223) 
(PUT 'DIFFF 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'DIFFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFF (U V)
    (COND ((ATOM U) (CONS NIL 1))
          ((ATOM (CAR U))
           ((LAMBDA (DIFF-FN)
              (COND (DIFF-FN (APPLY2 DIFF-FN U V)) (T (CONS NIL 1))))
            (GET (CAR U) 'DOMAIN-DIFF-FN)))
          (T
           (ADDSQ
            (ADDSQ
             (MULTSQ (CONS (LIST (CONS (CAAR U) 1)) 1)
                     (DIFFF
                      (COND
                       ((AND (FERMIONICP (CAAAR U))
                             (OR (AND (EQ V 'S) S_CHANGES_PARITY)
                                 (AND (EQ V 'T) T_CHANGES_PARITY)
                                 (FERMIONICP V)))
                        (NEGF (CDAR U)))
                       (T (CDAR U)))
                      V))
             (MULTSQ
              (COND
               ((AND (EQUAL (CDR (CAAR U)) 1)
                     (OR (AND (EQ V 'S) S_CHANGES_PARITY)
                         (AND (EQ V 'T) T_CHANGES_PARITY)
                         (FERMIONICP (CAR (CAAR U)))))
                (DIFFDP (CAAR U) V))
               (T (DIFFP (CAAR U) V)))
              (CONS (CDAR U) 1)))
            (DIFFF (CDR U) V))))) 
(PUT 'DIFFDP 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFDP 'DEFINED-ON-LINE '245) 
(PUT 'DIFFDP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'DIFFDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFDP (U V)
    (PROG (X Y Z W)
      (SETQ U (CAR U))
      (COND
       ((AND (NULL (ATOM U)) (SETQ X (GET (CAR U) 'DFFORM)))
        (RETURN (APPLY3 X U V 1))))
      (COND ((NULL (DEPENDS U V)) (RETURN (CONS NIL 1))))
      (COND ((EQ U V) (RETURN (CONS 1 1))))
      (COND
       ((AND (EQCAR U 'DF) (EQ V 'S) T_CHANGES_PARITY S_CHANGES_PARITY
             (UNEVEN_T_P (CDDR U)))
        (SETQ Z (CONS (MINUS 1) 1)))
       (T (SETQ Z (CONS 1 1))))
      (COND
       ((EQ (CAR U) 'DF)
        (COND
         ((SETQ X
                  (FIND_SUB_DF (SETQ W (CONS (CADR U) (MERGE-IND-VARS U V)))
                               (GET 'DF 'KVALUE)))
          (PROGN
           (SETQ W (SIMP (CAR X)))
           (PROG (EL)
             (SETQ EL (CDR X))
            LAB
             (COND ((NULL EL) (RETURN NIL)))
             ((LAMBDA (EL)
                (PROG (I)
                  (SETQ I 1)
                 LAB
                  (COND ((MINUSP (DIFFERENCE (CDR EL) I)) (RETURN NIL)))
                  (SETQ W (DIFFSQ W (CAR EL)))
                  (SETQ I (PLUS2 I 1))
                  (GO LAB)))
              (CAR EL))
             (SETQ EL (CDR EL))
             (GO LAB))
           (RETURN W)))
         (T (SETQ U (CONS 'DF W)))))
       (T (SETQ U (LIST 'DF U V))))
      (SETQ W (CONS 1 1))
      (COND ((SETQ X (OPMTCH U)) (RETURN (MULTSQ Z (SIMP X)))))
      (SETQ X (MKSQ U 1))
      (COND ((NULL (CAR X)) (RETURN X)))
      (SETQ Y (CAAAR (CAR X)))
      (COND
       ((AND (FERMIONICP Y) (NULL (ATSOC Y ASYMPLIS*)))
        (SETQ ASYMPLIS* (CONS (CONS Y 2) ASYMPLIS*))))
      (RETURN (MULTSQ Z X)))) 
(PUT 'UNEVEN_T_P 'NUMBER-OF-ARGS 1) 
(PUT 'UNEVEN_T_P 'DEFINED-ON-LINE '275) 
(PUT 'UNEVEN_T_P 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'UNEVEN_T_P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNEVEN_T_P (U)
    (COND
     ((EQ (CAR U) 'T)
      (COND ((CDR U) (NULL (AND (FIXP (CADR U)) (EVENP (CADR U))))) (T T)))
     (T (AND (CDR U) (UNEVEN_T_P (CDR U)))))) 
(BOTHTIMES (PUT 'IS_FERMIONIC 'BOOLFN 'EVALFERMIONICP)) 
(PUT 'EVALFERMIONICP 'NUMBER-OF-ARGS 1) 
(PUT 'EVALFERMIONICP 'DEFINED-ON-LINE '283) 
(PUT 'EVALFERMIONICP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'EVALFERMIONICP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALFERMIONICP (U) (FERMIONICFP (CAR (SIMP U)))) 
(PUT 'FERMIONICFP 'NUMBER-OF-ARGS 1) 
(PUT 'FERMIONICFP 'DEFINED-ON-LINE '290) 
(PUT 'FERMIONICFP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'FERMIONICFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FERMIONICFP (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          ((FERMIONICP (CAAAR U)) (NOT (FERMIONICFP (CDAR U))))
          (T (FERMIONICFP (CDAR U))))) 
(PUT 'REORDABLEP 'NUMBER-OF-ARGS 2) 
(PUT 'REORDABLEP 'DEFINED-ON-LINE '301) 
(PUT 'REORDABLEP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'REORDABLEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REORDABLEP (U V) (OR (ATOM U) (ATOM V) (REORDABLEKP U V))) 
(PUT 'REORDABLEKP 'NUMBER-OF-ARGS 2) 
(PUT 'REORDABLEKP 'DEFINED-ON-LINE '304) 
(PUT 'REORDABLEKP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'REORDABLEKP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REORDABLEKP (U V) (OR (SSTOOLS_KERNELP U) (SSTOOLS_KERNELP V))) 
(PUT 'SSTOOLS_KERNELP 'NUMBER-OF-ARGS 1) 
(PUT 'SSTOOLS_KERNELP 'DEFINED-ON-LINE '307) 
(PUT 'SSTOOLS_KERNELP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSTOOLS_KERNELP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SSTOOLS_KERNELP (U)
    (OR (EQ (CAR U) 'D) (FLAGP (CAR U) 'FERMIONIC) (FLAGP (CAR U) 'BOSONIC))) 
(PUT 'REORDOP 'NUMBER-OF-ARGS 2) 
(PUT 'REORDOP 'DEFINED-ON-LINE '310) 
(PUT 'REORDOP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'REORDOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REORDOP (U V)
    (COND ((REORDABLEP U V) (ORDOP U V))
          (T (OR (AND *NCMP (NONCOMP1 U) (NONCOMP1 V)) (ORDOP U V))))) 
(PUT 'RMULTPF 'NUMBER-OF-ARGS 2) 
(PUT 'RMULTPF 'DEFINED-ON-LINE '314) 
(PUT 'RMULTPF 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'RMULTPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RMULTPF (U V) (*Q2F (SIMP* (PREPF (CONS (CONS U V) NIL))))) 
(ENDMODULE) 
(FERMION1 '(F TH)) 
(BOSON1 '(B)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(DEPEND (LIST (LIST 'LIST 'F 'B) 'T 'X)) 
(SETQ SIMPLIMIT* 100000) 
(SETQ *NOARG NIL) 
(SETQ *ALLFAC NIL) 
(FLUID
 '(X FLIST BLIST FBLIST SYSFBL N_ FNAME_ FNAME_LIST NFCT_ USE_NEW_CRACKOUT
   PRINT_ PRINT_MORE COLLECT_SOL SOL_LIST PROC_LIST_ MAX_FACTOR FLIN_ SUBST_1
   PDELIMIT_1 OLD_HISTORY HOMOGEN_ MAX_GC_SHORT MAX_GC_RED_LEN RECORD_HIST
   *TIME LIN_TEST_CONST LINES_WRITTEN INEQ_ HTML_OUT *T_CHANGES_PARITY
   *S_CHANGES_PARITY)) 
(SETK 'LRULE1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'D (LIST '~ 'N) (LIST 'DF (LIST '~ 'H) 'X))
                   (LIST 'EXPT 'LIN_TEST_CONST 3))
             (LIST 'REPLACEBY
                   (LIST 'D (LIST '~ 'N)
                         (LIST 'DF (LIST '~ 'H) 'X (LIST '~ 'M)))
                   (LIST 'EXPT 'LIN_TEST_CONST
                         (LIST 'PLUS (LIST 'TIMES 2 'M) 1)))))) 
(SETK 'LRULE2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'D (LIST '~ 'N) (LIST '~ 'H))
                   'LIN_TEST_CONST)
             (LIST 'REPLACEBY (LIST 'DF (LIST '~ 'H) 'X)
                   (LIST 'EXPT 'LIN_TEST_CONST 2))
             (LIST 'REPLACEBY (LIST 'DF (LIST '~ 'H) 'X (LIST '~ 'N))
                   (LIST 'EXPT 'LIN_TEST_CONST (LIST 'TIMES 2 'N)))))) 
(SETK 'SUBLI (AEVAL (LIST 'LIST))) 
(SETK 'SYMANSATZ (AEVAL (LIST 'LIST))) 
(COND
 ((BOOLVALUE* (REVALX (NULL (GETD 'REDFRONT_COLOR))))
  (PROGN
   (PUT 'REDFRONT_COLOR 'NUMBER-OF-ARGS 1)
   (PUT 'REDFRONT_COLOR 'DEFINED-ON-LINE '360)
   (PUT 'REDFRONT_COLOR 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED)
   (PUT 'REDFRONT_COLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL))
   (DE REDFRONT_COLOR (A) A)))) 
(FLAG '(IS_CONST) 'OPFN) 
(PUT 'IS_CONST 'NUMBER-OF-ARGS 1) 
(PUT 'IS_CONST 'DEFINED-ON-LINE '365) 
(PUT 'IS_CONST 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'IS_CONST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS_CONST (A) (AND (FREEOF A 'F) (FREEOF A 'B))) 
(FLAG '(IS_FERMION) 'OPFN) 
(PUT 'IS_FERMION 'NUMBER-OF-ARGS 1) 
(PUT 'IS_FERMION 'DEFINED-ON-LINE '371) 
(PUT 'IS_FERMION 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'IS_FERMION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS_FERMION (A)
    (COND
     ((PAIRP A)
      (COND ((EQUAL (CAR A) 'D) (IS_BOSON (CADDR A)))
            ((EQUAL (CAR A) 'DF)
             (COND
              ((IS_FERMION (CADR A))
               (COND ((EQUAL (CADDR A) 'S) (NOT *S_CHANGES_PARITY))
                     ((EQUAL (CADDR A) 'T) (NOT *T_CHANGES_PARITY)) (T T)))
              ((EQUAL (CADDR A) 'S) *S_CHANGES_PARITY)
              ((EQUAL (CADDR A) 'T) *T_CHANGES_PARITY) (T NIL)))
            ((EQUAL (CAR A) 'EXPT)
             (COND ((IS_BOSON (CADR A)) NIL) ((EQUAL (CADDR A) 1) T)
                   ((FIXP (CADDR A)) NIL)
                   (T
                    (PROGN
                     (PRIN2 "###### ERROR: UNDEFINED PARITY!! ######")
                     NIL))))
            ((EQUAL (CAR A) 'TIMES)
             (PROG (N H)
               (SETQ N 0)
               (PROG (H)
                 (SETQ H (CDR A))
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H) (COND ((IS_FERMION H) (SETQ N (ADD1 N)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (RETURN (NOT (EVENP N)))))
            ((OR (EQUAL (CAR A) 'MINUS) (EQUAL (CAR A) 'QUOTIENT))
             (IS_FERMION (CADR A)))
            ((EQUAL (CAR A) 'PLUS)
             (PROG (H)
               (SETQ H (CDR A))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND H (ZEROP (CAR H)))) (RETURN NIL)))
                 (SETQ H (CDR H))
                 (GO WHILELABEL))
               (RETURN (COND (H (IS_FERMION (CAR H))) (T NIL)))))
            ((NOT (FREEOF A 'F)) T) (T NIL)))
     ((NOT (FREEOF A 'F)) T) (T NIL))) 
(FLAG '(IS_BOSON) 'OPFN) 
(PUT 'IS_BOSON 'NUMBER-OF-ARGS 1) 
(PUT 'IS_BOSON 'DEFINED-ON-LINE '409) 
(PUT 'IS_BOSON 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'IS_BOSON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS_BOSON (A)
    (COND
     ((PAIRP A)
      (COND ((EQUAL (CAR A) 'D) (IS_FERMION (CADDR A)))
            ((EQUAL (CAR A) 'DF)
             (COND
              ((IS_BOSON (CADR A))
               (COND ((EQUAL (CADDR A) 'S) (NOT *S_CHANGES_PARITY))
                     ((EQUAL (CADDR A) 'T) (NOT *T_CHANGES_PARITY)) (T T)))
              ((EQUAL (CADDR A) 'S) *S_CHANGES_PARITY)
              ((EQUAL (CADDR A) 'T) *T_CHANGES_PARITY) (T NIL)))
            ((EQUAL (CAR A) 'EXPT)
             (COND ((IS_BOSON (CADR A)) T) ((EQUAL (CADDR A) 1) NIL)
                   ((FIXP (CADDR A)) T)
                   (T
                    (PROGN
                     (PRIN2 "###### ERROR: UNDEFINED PARITY!! ######")
                     NIL))))
            ((OR (EQUAL (CAR A) 'MINUS) (EQUAL (CAR A) 'QUOTIENT))
             (IS_BOSON (CADR A)))
            ((EQUAL (CAR A) 'TIMES)
             (PROG (N H)
               (SETQ N 0)
               (PROG (H)
                 (SETQ H (CDR A))
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H) (COND ((IS_FERMION H) (SETQ N (ADD1 N)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (RETURN (EVENP N))))
            ((EQUAL (CAR A) 'PLUS)
             (PROG (H)
               (SETQ H (CDR A))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND H (ZEROP (CAR H)))) (RETURN NIL)))
                 (SETQ H (CDR H))
                 (GO WHILELABEL))
               (RETURN (COND (H (IS_BOSON (CAR H))) (T T)))))
            ((NOT (FREEOF A 'B)) T) (T NIL)))
     ((NOT (FREEOF A 'B)) T) (T NIL))) 
(FLAG '(SSINI) 'OPFN) 
(PUT 'SSINI 'NUMBER-OF-ARGS 3) 
(PUT 'SSINI 'DEFINED-ON-LINE '448) 
(PUT 'SSINI 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSINI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SSINI (N NF NB)
    (PROG (J)
      (SETQ N_ N)
      (COND ((EQUAL NF 1) (PUT 'F 'PRIFN 'MYFPRI)) (T (PUT 'F 'PRIFN NIL)))
      (COND ((EQUAL NB 1) (PUT 'B 'PRIFN 'MYFPRI)) (T (PUT 'B 'PRIFN NIL)))
      (SETQ FBLIST
              (APPEND
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J 1)
                 (COND ((MINUSP (DIFFERENCE NF J)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'F J) NIL)))
                LOOPLABEL
                 (SETQ J (PLUS2 J 1))
                 (COND ((MINUSP (DIFFERENCE NF J)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'F J) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J 1)
                 (COND ((MINUSP (DIFFERENCE NB J)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'B J) NIL)))
                LOOPLABEL
                 (SETQ J (PLUS2 J 1))
                 (COND ((MINUSP (DIFFERENCE NB J)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'B J) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))) 
(PUT 'PRESIMPLIFY 'NUMBER-OF-ARGS 2) 
(PUT 'PRESIMPLIFY 'DEFINED-ON-LINE '463) 
(PUT 'PRESIMPLIFY 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'PRESIMPLIFY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRESIMPLIFY (ES FL)
    (PROG (W INEQ_BAK NOPOWERSBAK EQNS M K CPU P LEN_ES)
      (SETQ LEN_ES (SUB1 (LENGTH ES)))
      (PROGN (PRIN2 LEN_ES) (PRIN2 " equations result") NIL)
      (TERPRI)
      (SETQ CPU (TIME))
      (SETQ W NIL)
      (SETQ INEQ_BAK INEQ_)
      (SETQ INEQ_ NIL)
      (SETQ NOPOWERSBAK *NOPOWERS)
      (AEVAL (OFF (LIST 'NOPOWERS)))
      (SETQ EQNS NIL)
      (SETQ FL (CDR (REVAL1 FL T)))
      (SETQ M NIL)
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          (M
           (PROGN
            (SETQ ES
                    (AEVAL*
                     (LIST 'SUB
                           (CONS 'LIST
                                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ K M)
                                   (COND ((NULL K) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (K)
                                                       (LIST 'EQUAL K 0))
                                                     (CAR K))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ K (CDR K))
                                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (K) (LIST 'EQUAL K 0))
                                             (CAR K))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                           ES)))
            (SETQ W (NCONC M W))
            (SETQ M NIL)
            NIL)))
         (SETQ ES (CDR ES))
         (PROG ()
          WHILELABEL
           (COND ((NOT ES) (RETURN NIL)))
           (COND ((ZEROP (CAR ES)) (SETQ ES (CDR ES)))
                 (T
                  (PROGN
                   (SETQ K (AEVAL* (LIST 'FACTORIZE (CAR ES))))
                   (SETQ ES (CDR ES))
                   (SETQ K (CDR K))
                   (SETQ P NIL)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT K) (RETURN NIL)))
                     (PROGN
                      (COND
                       ((NOT (FREEOFLIST (CADR (CAR K)) FL))
                        (SETQ P (CONS (CADR (CAR K)) P))))
                      (SETQ K (CDR K))
                      NIL)
                     (GO WHILELABEL))
                   (COND ((NULL P) (SETQ EQNS (CONS 1 EQNS)))
                         (T
                          (PROGN
                           (COND ((CDR P) (SETQ P (CONS 'TIMES P)))
                                 (T
                                  (SETQ P
                                          (SIMPLIFYTERM (REVAL1 (CAR P) T)
                                           FL))))
                           (COND ((ATOM P) (SETQ M (UNION (LIST P) M)))
                                 (T (SETQ EQNS (UNION (LIST P) EQNS))))))))))
           (GO WHILELABEL))
         (SETQ ES (CONS 'LIST EQNS))
         (SETQ EQNS NIL))
        (COND ((NOT (NULL M)) (GO REPEATLABEL))))
      (PROGN (PRIN2 (LENGTH W)) (PRIN2 " coefficients found to be zero: ") NIL)
      (LISTPRINT W)
      (TERPRI)
      (SETQ EQNS (NCONC ES W))
      (PROGN
       (PRIN2 (REVAL1 (PLUS LEN_ES (DIFFERENCE 1 (LENGTH EQNS))) T))
       (PRIN2 " redundant equations have been droped.")
       NIL)
      (TERPRI)
      (SETQ INEQ_ INEQ_BAK)
      (COND (NOPOWERSBAK (AEVAL (ON (LIST 'NOPOWERS)))))
      (PROGN
       (PRIN2 (QUOTIENT (DIFFERENCE (TIME) CPU) 1000))
       (PRIN2 " s for pre-simplification")
       NIL)
      (TERPRI)
      (RETURN EQNS))) 
(PUT 'INPUT_CONSISTENCY_TEST 'NUMBER-OF-ARGS 2) 
(PUT 'INPUT_CONSISTENCY_TEST 'DEFINED-ON-LINE '521) 
(PUT 'INPUT_CONSISTENCY_TEST 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'INPUT_CONSISTENCY_TEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INPUT_CONSISTENCY_TEST (AFWLIST ABWLIST)
    (PROG ()
      (COND
       ((AND (LESSP (LENGTH AFWLIST) 2) (LESSP (LENGTH ABWLIST) 2))
        (REDERR "flist AND blist are both not assigned.")))
      (SETQ AFWLIST (CDR AFWLIST))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND AFWLIST (FIXP (CAR AFWLIST)) (GEQ (CAR AFWLIST) 0)))
          (RETURN NIL)))
        (SETQ AFWLIST (CDR AFWLIST))
        (GO WHILELABEL))
      (COND
       (AFWLIST
        (COND
         ((NOT (FIXP (CAR AFWLIST)))
          (REDERR "The fermionic weight list contains not only numbers!"))
         ((NOT (FIXP (REVAL1 (AEVAL 'MAX_DEG) T)))
          (REDERR "If a fermionic weight is < 0 then max_deg must be set!")))))
      (SETQ ABWLIST (CDR ABWLIST))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND ABWLIST (FIXP (CAR ABWLIST)) (GREATERP (CAR ABWLIST) 0)))
          (RETURN NIL)))
        (SETQ ABWLIST (CDR ABWLIST))
        (GO WHILELABEL))
      (COND
       (ABWLIST
        (COND
         ((NOT (FIXP (CAR ABWLIST)))
          (REDERR "The bosonic weight list contains not only numbers!"))
         ((NOT (FIXP (REVAL1 (AEVAL 'MAX_DEG) T)))
          (REDERR "If a bosonic weight is < 1 then max_deg must be set!"))))))) 
(PUT 'CROSSPRODU 'NUMBER-OF-ARGS 4) 
(PUT 'CROSSPRODU 'DEFINED-ON-LINE '544) 
(PUT 'CROSSPRODU 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CROSSPRODU 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CROSSPRODU (SETA SETB WGT ENDPROD)
    (PROG (G K SETBCP)
      (COND
       (SETB
        (PROG (G)
          (SETQ G SETA)
         LAB
          (COND ((NULL G) (RETURN NIL)))
          ((LAMBDA (G)
             (PROGN
              (SETQ SETBCP SETB)
              (SETQ K (PLUS (CAR G) (CAAR SETBCP)))
              (PROG ()
               WHILELABEL
                (COND ((NOT (LEQ K WGT)) (RETURN NIL)))
                (PROGN
                 (COND
                  ((EQUAL K WGT)
                   (SETQ ENDPROD
                           (CONS (CONS K (APPEND (CDR G) (CDAR SETBCP)))
                                 ENDPROD))))
                 (SETQ SETBCP (CDR SETBCP))
                 (COND (SETBCP (SETQ K (PLUS (CAR G) (CAAR SETBCP))))
                       (T (SETQ K 10000000))))
                (GO WHILELABEL))))
           (CAR G))
          (SETQ G (CDR G))
          (GO LAB))))
      (RETURN ENDPROD))) 
(PUT 'RHS_TERM_LIST 'NUMBER-OF-ARGS 7) 
(PUT 'RHS_TERM_LIST 'DEFINED-ON-LINE '562) 
(PUT 'RHS_TERM_LIST 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'RHS_TERM_LIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE RHS_TERM_LIST (N ALLF ALLB MAXWGT LINSUB FORBID VERBOSE)
    (PROG (G H J K MINWGT MAXDEG MAXTERMWGT ALLV NEWV MAXPOW MAXWGTM2 FPROD
           BPROD ALLPROD NEWPROD LINONLY P Q R S)
      (SETQ MINWGT 0)
      (SETQ MAXDEG (AEVAL 'MAX_DEG))
      (SETQ ALLV (APPEND ALLF ALLB))
      (PROG (G)
        (SETQ G ALLV)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (COND
            ((LESSP (CAR G) 0)
             (SETQ MINWGT (PLUS MINWGT (TIMES MAXDEG (CAR G)))))))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ MAXTERMWGT (DIFFERENCE MAXWGT MINWGT))
      (SETQ MAXWGTM2 (DIFFERENCE MAXTERMWGT 2))
      (PROG (G)
        (SETQ G ALLV)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (LEQ (CAR G) MAXWGTM2)
                      (NOT (MEMBER (REVAL1 (LIST 'DF (CADR G) 'X) T) FORBID))))
                (RETURN NIL)))
              (PROGN
               (SETQ H (PLUS 2 (CAR G)))
               (SETQ G (LIST H (LIST 'DF (CADR G) 'X)))
               (SETQ NEWV (CONS G NEWV)))
              (GO WHILELABEL))))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ ALLV (APPEND ALLV NEWV))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ NEWV NIL)
         (PROG (G)
           (SETQ G ALLV)
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (COND
               ((LESSP (CAR G) MAXTERMWGT)
                (SETQ NEWV
                        (CONS (LIST (ADD1 (CAR G)) (LIST 'D J (CADR G)))
                              NEWV)))))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (SETQ ALLV (APPEND ALLV NEWV))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ ALLPROD (LIST (LIST 0 NIL)))
      (COND
       (VERBOSE
        (PROGN
         (PROGN (PRIN2 "maxwgt=") (PRIN2 MAXWGT) NIL)
         (TERPRI)
         (SETQ Q (LENGTH ALLV))
         (SETQ P 0)
         (SETQ S 0)
         NIL)))
      (PROG (G)
        (SETQ G ALLV)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (COND
             (VERBOSE
              (PROGN
               (SETQ P (ADD1 P))
               (PROGN
                (PRIN2 P)
                (PRIN2 "(")
                (PRIN2 Q)
                (PRIN2 "): ")
                (PRIN2 G)
                NIL)
               (TERPRI))))
            (COND ((NULL LINSUB) (SETQ LINONLY NIL))
                  ((SMEMBERL LINSUB (CADR G)) (SETQ LINONLY T))
                  (T (SETQ LINONLY NIL)))
            (SETQ MAXPOW
                    (COND ((OR LINONLY (IS_FERMION (CADR G))) 1)
                          ((LESSP (CAR G) 1) MAXDEG) (T 10000000)))
            (SETQ NEWPROD NIL)
            (COND (VERBOSE (SETQ R 0)))
            (PROG (H)
              (SETQ H ALLPROD)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (COND
                  ((OR (NULL LINONLY) (NULL (CADR H)))
                   (PROGN
                    (SETQ K (PLUS (CAR H) (CAR G)))
                    (SETQ J 1)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (AND (LEQ J MAXPOW) (LEQ K MAXTERMWGT)))
                        (RETURN NIL)))
                      (PROGN
                       (SETQ H
                               (CONS K
                                     (CONS (OR LINONLY (CADR H))
                                           (CONS (CADR G) (CDDR H)))))
                       (SETQ NEWPROD (CONS H NEWPROD))
                       (SETQ K (PLUS (CAR H) (CAR G)))
                       (SETQ J (ADD1 J)))
                      (GO WHILELABEL))
                    (COND (VERBOSE (SETQ R (PLUS R (DIFFERENCE J 1)))))))))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (COND (VERBOSE (SETQ S (PLUS S R))))
            (SETQ ALLPROD (NCONC ALLPROD NEWPROD))))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ ALLPROD (CDR ALLPROD))
      (PROG ()
       WHILELABEL
        (COND ((NOT ALLPROD) (RETURN NIL)))
        (PROGN
         (SETQ G (CAR ALLPROD))
         (SETQ ALLPROD (CDR ALLPROD))
         (COND
          ((AND (EQUAL (CAR G) MAXWGT) (OR (NULL LINSUB) (CADR G)))
           (COND
            ((IS_FERMION (CONS 'TIMES (CDR G)))
             (SETQ FPROD
                     (CONS
                      (COND ((NULL (CDDDR G)) (CADDR G))
                            (T (CONS 'TIMES (CDDR G))))
                      FPROD)))
            (T
             (SETQ BPROD
                     (CONS
                      (COND ((NULL (CDDDR G)) (CADDR G))
                            (T (CONS 'TIMES (CDDR G))))
                      BPROD)))))))
        (GO WHILELABEL))
      (COND
       (VERBOSE
        (PROGN
         (PROGN
          (PRIN2 (LENGTH FPROD))
          (PRIN2 " fermionic and ")
          (PRIN2 (LENGTH BPROD))
          (PRIN2 " bosonic monomials")
          NIL)
         (TERPRI))))
      (RETURN (LIST FPROD BPROD)))) 
(PUT 'GEN_EQN 'NUMBER-OF-ARGS 6) 
(PUT 'GEN_EQN 'DEFINED-ON-LINE '663) 
(PUT 'GEN_EQN 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'GEN_EQN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEN_EQN (AWLIST RHSLIST EQN_LIST FL FERMIONIC TNAME)
    (PROG (H W RLCP RHS NF PRINT_BAK SVAR)
      (SETQ SVAR (REVAL1 TNAME T))
      (PROG ()
       WHILELABEL
        (COND ((NOT AWLIST) (RETURN NIL)))
        (PROGN
         (SETQ H (CAR AWLIST))
         (SETQ AWLIST (CDR AWLIST))
         (SETQ W (CAR H))
         (SETQ H (CADR H))
         (SETQ RLCP RHSLIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ (CAAR RLCP) W)) (RETURN NIL)))
           (SETQ RLCP (CDR RLCP))
           (GO WHILELABEL))
         (SETQ RHS (COND (FERMIONIC (CADAR RLCP)) (T (CADDAR RLCP))))
         (SETQ PRINT_BAK PRINT_)
         (SETQ PRINT_ NIL)
         (SETQ RHS
                 (PROG (R FORALL-RESULT FORALL-ENDPTR)
                   (SETQ R RHS)
                   (COND ((NULL R) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (R)
                                       (PROGN
                                        (SETQ NF (NEWFCT FNAME_ NIL NFCT_))
                                        (SETQ FL (CONS NF FL))
                                        (SETQ NFCT_ (ADD1 NFCT_))
                                        (LIST 'TIMES NF R)))
                                     (CAR R))
                                    NIL)))
                  LOOPLABEL
                   (SETQ R (CDR R))
                   (COND ((NULL R) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (R)
                               (PROGN
                                (SETQ NF (NEWFCT FNAME_ NIL NFCT_))
                                (SETQ FL (CONS NF FL))
                                (SETQ NFCT_ (ADD1 NFCT_))
                                (LIST 'TIMES NF R)))
                             (CAR R))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ PRINT_ PRINT_BAK)
         (SETQ RHS
                 (COND ((NULL RHS) 0) ((CDR RHS) (CONS 'PLUS RHS))
                       (T (CAR RHS))))
         (SETQ EQN_LIST (CONS (LIST 'EQUAL (LIST 'DF H SVAR) RHS) EQN_LIST)))
        (GO WHILELABEL))
      (RETURN (LIST EQN_LIST FL)))) 
(PUT 'SSPOL 'NUMBER-OF-ARGS 10) 
(PUT 'SSPOL 'DEFINED-ON-LINE '696) 
(PUT 'SSPOL 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSPOL 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE SSPOL
    (N SYSFBL AFWLIST ABWLIST DIFFORDER TNAME LINSUB FORBID FLIP_PAR VERBOSE)
    (PROG (G H I FWLIST BWLIST ALLF ALLB W_LIST RHS_LIST EQN_LIST FL AWLIST)
      (SETQ FWLIST (CDR (REVAL1 AFWLIST T)))
      (SETQ BWLIST (CDR (REVAL1 ABWLIST T)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH FLIST) I)) (RETURN NIL)))
        (SETQ ALLF (CONS (LIST (NTH FWLIST I) (NTH FLIST I)) ALLF))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH BLIST) I)) (RETURN NIL)))
        (SETQ ALLB (CONS (LIST (NTH BWLIST I) (NTH BLIST I)) ALLB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ W_LIST (UNION FWLIST (UNION BWLIST NIL)))
      (SETQ RHS_LIST
              (PROG (W FORALL-RESULT FORALL-ENDPTR)
                (SETQ W W_LIST)
                (COND ((NULL W) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W)
                                    (CONS W
                                          (RHS_TERM_LIST N ALLF ALLB
                                           (PLUS W DIFFORDER) LINSUB FORBID
                                           VERBOSE)))
                                  (CAR W))
                                 NIL)))
               LOOPLABEL
                (SETQ W (CDR W))
                (COND ((NULL W) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (W)
                            (CONS W
                                  (RHS_TERM_LIST N ALLF ALLB (PLUS W DIFFORDER)
                                   LINSUB FORBID VERBOSE)))
                          (CAR W))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (H)
        (SETQ H SYSFBL)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((EQUAL (CAR H) 'B)
             (PROGN
              (SETQ G ALLB)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND G (NEQ (CADAR G) H))) (RETURN NIL)))
                (SETQ G (CDR G))
                (GO WHILELABEL))
              (SETQ AWLIST (CONS (CAR G) AWLIST))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ NFCT_ 1)
      (SETQ H (GEN_EQN AWLIST RHS_LIST EQN_LIST FL FLIP_PAR TNAME))
      (SETQ AWLIST NIL)
      (PROG (H)
        (SETQ H SYSFBL)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((EQUAL (CAR H) 'F)
             (PROGN
              (SETQ G ALLF)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND G (NEQ (CADAR G) H))) (RETURN NIL)))
                (SETQ G (CDR G))
                (GO WHILELABEL))
              (SETQ AWLIST (CONS (CAR G) AWLIST))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ H (GEN_EQN AWLIST RHS_LIST (CAR H) (CADR H) (NOT FLIP_PAR) TNAME))
      (RETURN (LIST (CONS 'LIST (CAR H)) (CADR H))))) 
(PUT 'PRINT_LIST_OF_LISTS 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_LIST_OF_LISTS 'DEFINED-ON-LINE '749) 
(PUT 'PRINT_LIST_OF_LISTS 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'PRINT_LIST_OF_LISTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_LIST_OF_LISTS (LI)
    (PROG (K)
      (PROG (K)
        (SETQ K LI)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (PROGN (PRIN2 "{") NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT K) (RETURN NIL)))
              (PROGN
               (PROGN (PRIN2 (CAR K)) NIL)
               (SETQ K (CDR K))
               (COND (K (PROGN (PRIN2 ",") NIL))))
              (GO WHILELABEL))
            (PROGN (PRIN2 "}") NIL)
            (TERPRI)
            NIL))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB)))) 
(FLAG '(LISTINE) 'OPFN) 
(PUT 'LISTINE 'NUMBER-OF-ARGS 7) 
(PUT 'LISTINE 'DEFINED-ON-LINE '761) 
(PUT 'LISTINE 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'LISTINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE LISTINE (N SYS SYM FL INEQL NON_LIN_TEST SAVE_LISTS)
    (PROG (EQN CND H K FB FLCP NFL EVOLIST RS)
      (SETQ FL (CDR FL))
      (SETQ INEQL
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H (CDR INEQL))
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EVOLIST
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H (APPEND (CDR SYS) (CDR SYM)))
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (H) (CADDR H)) (CAR H)) NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (H) (CADDR H)) (CAR H)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ K (GENSYM))
      (PROG (EQN)
        (SETQ EQN (CDR SYS))
       LAB
        (COND ((NULL EQN) (RETURN NIL)))
        ((LAMBDA (EQN)
           (PROGN
            (COND
             ((NULL (SETQ H (SMEMBERL FL (CADDR EQN)))) (SETQ H (LIST 0))))
            (SETQ INEQL (CONS H INEQL))))
         (CAR EQN))
        (SETQ EQN (CDR EQN))
        (GO LAB))
      (SETQ H
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H (CDR SYM))
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (H) (CADDR H)) (CAR H)) NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (H) (CADDR H)) (CAR H)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL (SETQ H (SMEMBERL FL H))) (SETQ H (LIST 0))))
      (SETQ INEQL (CONS H INEQL))
      (COND
       ((CDR FBLIST)
        (PROG (EQN)
          (SETQ EQN (CDR SYS))
         LAB
          (COND ((NULL EQN) (RETURN NIL)))
          ((LAMBDA (EQN)
             (PROGN
              (SETQ RS (CADDR EQN))
              (SETQ FB (DELETE (CADR (CADR EQN)) FBLIST))
              (COND
               ((AND (PAIRP RS) (EQUAL (CAR RS) '*SQ))
                (SETQ RS
                        (LIST '*SQ
                              (SUBSQ (CADR RS)
                                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ H FB)
                                       (COND ((NULL H) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (H)
                                                           (CONS H 0))
                                                         (CAR H))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ H (CDR H))
                                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (H) (CONS H 0))
                                                 (CAR H))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                              T)))
               (T
                (PROG (H)
                  (SETQ H FB)
                 LAB
                  (COND ((NULL H) (RETURN NIL)))
                  ((LAMBDA (H) (SETQ RS (SUBST 0 H RS))) (CAR H))
                  (SETQ H (CDR H))
                  (GO LAB))))
              (SETQ H
                      (SETDIFF (SMEMBERL FL (CADDR EQN))
                               (SMEMBERL FL (REVAL1 RS T))))
              (COND ((NULL H) (SETQ INEQL (CONS (LIST 0) INEQL)))
                    (T (SETQ INEQL (CONS H INEQL))))))
           (CAR EQN))
          (SETQ EQN (CDR EQN))
          (GO LAB))))
      (COND
       (NON_LIN_TEST
        (PROGN
         (SETQ NFL NIL)
         (PROG (EQN)
           (SETQ EQN EVOLIST)
          LAB
           (COND ((NULL EQN) (RETURN NIL)))
           ((LAMBDA (EQN)
              (PROGN
               (SETQ FLCP (SMEMBERL FL EQN))
               (PROG (H)
                 (SETQ H FBLIST)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H) (SETQ EQN (SUBST (LIST 'TIMES K H) H EQN)))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (SETQ H (SMEMBERL FLCP (COEFFN (REVAL1 EQN T) K 1)))
               (SETQ FLCP (SETDIFF FLCP H))
               (SETQ NFL (APPEND NFL FLCP))))
            (CAR EQN))
           (SETQ EQN (CDR EQN))
           (GO LAB))
         (COND ((NULL NFL) (SETQ NFL (LIST 0))))
         (SETQ INEQL (CONS NFL INEQL))
         NIL)))
      (COND
       ((NULL FLIST)
        (PROGN
         (SETQ FLCP FL)
         (PROGN
          (SETK 'DROP_FD_RULES
                (AEVAL
                 (LIST 'LIST
                       (LIST 'REPLACEBY (LIST 'D (LIST '~ 'I) (LIST '~ 'J)) 0)
                       (LIST 'REPLACEBY (LIST 'F (LIST '~ 'I)) 0))))
          (AEVAL (LET '(DROP_FD_RULES)))
          (AEVAL 'NIL))
         (PROG (EQN)
           (SETQ EQN EVOLIST)
          LAB
           (COND ((NULL EQN) (RETURN NIL)))
           ((LAMBDA (EQN)
              (PROGN
               (SETQ H (SMEMBERL FL (REVAL1 EQN T)))
               (SETQ FLCP (SETDIFF FLCP H))))
            (CAR EQN))
           (SETQ EQN (CDR EQN))
           (GO LAB))
         (AEVAL (CLEARRULES (LIST 'DROP_FD_RULES)))
         (COND ((NULL FLCP) (SETQ FLCP (LIST 0))))
         (SETQ INEQL (CONS FLCP INEQL))
         NIL)))
      (COND
       ((GREATERP N 1)
        (PROG (H)
          (SETQ H 1)
         LAB
          (COND ((MINUSP (DIFFERENCE N H)) (RETURN NIL)))
          (PROGN
           (SETQ FLCP FL)
           (PROGN
            (SETQ K (AEVAL* H))
            (SETK 'DROP_FD_RULES
                  (AEVAL*
                   (LIST 'LIST (LIST 'REPLACEBY (LIST 'D K (LIST '~ 'J)) 0))))
            (AEVAL* (LET '(DROP_FD_RULES))))
           (PROG (EQN)
             (SETQ EQN EVOLIST)
            LAB
             (COND ((NULL EQN) (RETURN NIL)))
             ((LAMBDA (EQN)
                (PROGN
                 (SETQ K (SMEMBERL FL (REVAL1 EQN T)))
                 (SETQ FLCP (SETDIFF FLCP K))))
              (CAR EQN))
             (SETQ EQN (CDR EQN))
             (GO LAB))
           (AEVAL* (CLEARRULES (LIST 'DROP_FD_RULES)))
           (COND ((NULL FLCP) (SETQ FLCP (LIST 0))))
           (SETQ INEQL (CONS FLCP INEQL))
           NIL)
          (SETQ H (PLUS2 H 1))
          (GO LAB))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ RS INEQL)
         (PROG ()
          WHILELABEL
           (COND ((NOT RS) (RETURN NIL)))
           (PROGN
            (SETQ K (DELETE (CAR RS) INEQL))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND K (NOT_INCLUDED (CAR RS) (CAR K)))) (RETURN NIL)))
              (SETQ K (CDR K))
              (GO WHILELABEL))
            (COND
             (K (PROGN (SETQ INEQL (DELETE (CAR K) INEQL)) (SETQ RS INEQL)))
             (T (SETQ RS (CDR RS)))))
           (GO WHILELABEL)))
        (COND ((NOT (NULL RS)) (GO REPEATLABEL))))
      (COND
       (INEQL
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2
           "From each of the following lists at least one element must be non-zero:")
          NIL)
         (TERPRI)
         (TERPRI)
         (PRINT_LIST_OF_LISTS INEQL)
         (TERPRI)
         NIL)))
      (COND
       (SAVE_LISTS
        (PROGN
         (OUT (LIST "inelist"))
         (PRINT_LIST_OF_LISTS INEQL)
         (SHUT (LIST "inelist"))
         NIL)))
      (SETQ INEQL
              (PROG (CND FORALL-RESULT FORALL-ENDPTR)
                (SETQ CND INEQL)
                (COND ((NULL CND) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CND)
                                    (COND ((NULL (CDR CND)) (CAR CND))
                                          (T
                                           (CONS 'PLUS
                                                 (PROG (H FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ H CND)
                                                   (COND
                                                    ((NULL H) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (H)
                                                                       (LIST
                                                                        'TIMES
                                                                        H
                                                                        (GENSYM)))
                                                                     (CAR H))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ H (CDR H))
                                                   (COND
                                                    ((NULL H)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (H)
                                                               (LIST 'TIMES H
                                                                     (GENSYM)))
                                                             (CAR H))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))))))
                                  (CAR CND))
                                 NIL)))
               LOOPLABEL
                (SETQ CND (CDR CND))
                (COND ((NULL CND) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CND)
                            (COND ((NULL (CDR CND)) (CAR CND))
                                  (T
                                   (CONS 'PLUS
                                         (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ H CND)
                                           (COND ((NULL H) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (H)
                                                               (LIST 'TIMES H
                                                                     (GENSYM)))
                                                             (CAR H))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ H (CDR H))
                                           (COND
                                            ((NULL H) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (H)
                                                       (LIST 'TIMES H
                                                             (GENSYM)))
                                                     (CAR H))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))))))
                          (CAR CND))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST INEQL)))) 
(PUT 'POWER_PARTI 'NUMBER-OF-ARGS 1) 
(FLAG '(POWER_PARTI) 'OPFN) 
(PUT 'POWER_PARTI 'DEFINED-ON-LINE '886) 
(PUT 'POWER_PARTI 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'POWER_PARTI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POWER_PARTI (EQNS)
    (PROG (W AFBLIST EQUA FF EX A P PARTI MAXPOW H)
      (SETQ AFBLIST (AEVAL (CONS 'LIST FBLIST)))
      (SETQ W (AEVAL LIN_TEST_CONST))
      (SETQ MAXPOW (AEVAL 0))
      (SETQ H
              (PROG (EQUA FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQUA (GETRLIST (AEVAL EQNS)))
                (COND ((NULL EQUA) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQUA)
                                    (PROGN
                                     (SETQ FF (AEVAL (LIST 'PART EQUA 1)))
                                     (SETQ EX (AEVAL (LIST 'PART EQUA 2)))
                                     (PROG (A)
                                       (SETQ A (GETRLIST (AEVAL AFBLIST)))
                                      LAB
                                       (COND ((NULL A) (RETURN NIL)))
                                       ((LAMBDA (A)
                                          (SETQ EX
                                                  (AEVAL
                                                   (LIST 'SUB
                                                         (LIST 'EQUAL A
                                                               (LIST 'TIMES A
                                                                     W))
                                                         EX))))
                                        (CAR A))
                                       (SETQ A (CDR A))
                                       (GO LAB))
                                     (SETQ PARTI
                                             (AEVAL
                                              (LIST 'REST (LIST 'COEFF EX W))))
                                     (COND
                                      ((EVALGREATERP (AEVAL HIPOW*)
                                                     (AEVAL MAXPOW))
                                       (SETQ MAXPOW (AEVAL HIPOW*))))
                                     (AEVAL (LIST 'EQUAL FF PARTI))))
                                  (CAR EQUA))
                                 NIL)))
               LOOPLABEL
                (SETQ EQUA (CDR EQUA))
                (COND ((NULL EQUA) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQUA)
                            (PROGN
                             (SETQ FF (AEVAL (LIST 'PART EQUA 1)))
                             (SETQ EX (AEVAL (LIST 'PART EQUA 2)))
                             (PROG (A)
                               (SETQ A (GETRLIST (AEVAL AFBLIST)))
                              LAB
                               (COND ((NULL A) (RETURN NIL)))
                               ((LAMBDA (A)
                                  (SETQ EX
                                          (AEVAL
                                           (LIST 'SUB
                                                 (LIST 'EQUAL A
                                                       (LIST 'TIMES A W))
                                                 EX))))
                                (CAR A))
                               (SETQ A (CDR A))
                               (GO LAB))
                             (SETQ PARTI
                                     (AEVAL (LIST 'REST (LIST 'COEFF EX W))))
                             (COND
                              ((EVALGREATERP (AEVAL HIPOW*) (AEVAL MAXPOW))
                               (SETQ MAXPOW (AEVAL HIPOW*))))
                             (AEVAL (LIST 'EQUAL FF PARTI))))
                          (CAR EQUA))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'LENGTH H)) 1)
        (SETQ H
                (PROG (EQUA FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EQUA (GETRLIST (AEVAL H)))
                  (COND ((NULL EQUA) (RETURN (MAKELIST NIL))))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (EQUA)
                                      (AEVAL
                                       (LIST 'EQUAL (LIST 'PART EQUA 1)
                                             (PROGN
                                              (SETK 'LE
                                                    (AEVAL
                                                     (LIST 'LENGTH
                                                           (LIST 'PART EQUA
                                                                 2))))
                                              (COND
                                               ((EVALEQUAL (AEVAL 'LE)
                                                           (AEVAL MAXPOW))
                                                (AEVAL (LIST 'PART EQUA 2)))
                                               (T
                                                (PROGN
                                                 (SETQ EX (AEVAL (LIST 'LIST)))
                                                 (PROG (G)
                                                   (SETQ G 1)
                                                  LAB
                                                   (COND
                                                    ((|AMINUSP:|
                                                      (LIST 'DIFFERENCE
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   MAXPOW 'LE))
                                                            G))
                                                     (RETURN NIL)))
                                                   (SETQ EX
                                                           (AEVAL*
                                                            (LIST 'CONS 0 EX)))
                                                   (SETQ G
                                                           ((LAMBDA
                                                                (FORALL-RESULT)
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     FORALL-RESULT
                                                                     1)))
                                                            G))
                                                   (GO LAB))
                                                 (AEVAL
                                                  (LIST 'APPEND
                                                        (LIST 'PART EQUA 2)
                                                        EX)))))))))
                                    (CAR EQUA))
                                   NIL)))
                 LOOPLABEL
                  (SETQ EQUA (CDR EQUA))
                  (COND ((NULL EQUA) (RETURN (CONS 'LIST FORALL-RESULT))))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (EQUA)
                              (AEVAL
                               (LIST 'EQUAL (LIST 'PART EQUA 1)
                                     (PROGN
                                      (SETK 'LE
                                            (AEVAL
                                             (LIST 'LENGTH
                                                   (LIST 'PART EQUA 2))))
                                      (COND
                                       ((EVALEQUAL (AEVAL 'LE) (AEVAL MAXPOW))
                                        (AEVAL (LIST 'PART EQUA 2)))
                                       (T
                                        (PROGN
                                         (SETQ EX (AEVAL (LIST 'LIST)))
                                         (PROG (G)
                                           (SETQ G 1)
                                          LAB
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE
                                                    (AEVAL*
                                                     (LIST 'DIFFERENCE MAXPOW
                                                           'LE))
                                                    G))
                                             (RETURN NIL)))
                                           (SETQ EX (AEVAL* (LIST 'CONS 0 EX)))
                                           (SETQ G
                                                   ((LAMBDA (FORALL-RESULT)
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             FORALL-RESULT 1)))
                                                    G))
                                           (GO LAB))
                                         (AEVAL
                                          (LIST 'APPEND (LIST 'PART EQUA 2)
                                                EX)))))))))
                            (CAR EQUA))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN (AEVAL (LIST 'LIST MAXPOW H))))) 
(PUT 'ADD_DF_RULES_TO_D_RULE 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_DF_RULES_TO_D_RULE 'DEFINED-ON-LINE '912) 
(PUT 'ADD_DF_RULES_TO_D_RULE 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'ADD_DF_RULES_TO_D_RULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_DF_RULES_TO_D_RULE (SUBL)
    (COND ((NULL (CDR SUBL)) SUBL)
          (T
           (PROG (G H)
             (SETQ G (CDR SUBL))
             (PROG (H)
               (SETQ H G)
              LAB
               (COND ((NULL H) (RETURN NIL)))
               ((LAMBDA (H)
                  (COND
                   ((AND (PAIRP H) (EQUAL (CAR H) 'REPLACEBY) (PAIRP (CADR H))
                         (EQUAL (CAADR H) 'D) (PAIRP (CADDR (CADR H)))
                         (OR (EQUAL (CAADDR (CADR H)) 'F)
                             (EQUAL (CAADDR (CADR H)) 'B)))
                    (PROGN
                     (SETQ SUBL
                             (CONS 'LIST
                                   (UNION
                                    (LIST
                                     (LIST 'REPLACEBY
                                           (LIST 'DF (CADDR (CADR H)) 'X)
                                           (REVAL1
                                            (LIST 'D (CADR (CADR H)) (CADDR H))
                                            T))
                                     (LIST 'REPLACEBY
                                           (LIST 'DF (CADDR (CADR H)) 'X
                                                 (LIST '~ 'N))
                                           (LIST 'D (CADR (CADR H))
                                                 (LIST 'DF (CADDR H) 'X
                                                       (LIST 'DIFFERENCE 'N
                                                             1))))
                                     (LIST 'REPLACEBY
                                           (LIST 'D (CADR (CADR H))
                                                 (LIST 'DF (CADDR (CADR H))
                                                       (LIST '~ 'X)))
                                           (REVAL1 (LIST 'DF (CADDR H) 'X) T))
                                     (LIST 'REPLACEBY
                                           (LIST 'D (CADR (CADR H))
                                                 (LIST 'DF (CADDR (CADR H))
                                                       (LIST '~ 'X)
                                                       (LIST '~ 'N)))
                                           (LIST 'DF (CADDR H) 'X 'N))
                                     (LIST 'REPLACEBY
                                           (LIST 'D (CADR (CADR H))
                                                 (LIST 'DF (CADDR (CADR H))
                                                       (LIST '~ 'T)
                                                       (LIST '~ 'X)
                                                       (LIST '~ 'N)))
                                           (LIST 'DF (CADDR H) 'T 'X 'N)))
                                    (CDR SUBL))))))))
                (CAR H))
               (SETQ H (CDR H))
               (GO LAB))
             (RETURN SUBL))))) 
(PUT 'SIEVE 'NUMBER-OF-ARGS 2) 
(FLAG '(SIEVE) 'OPFN) 
(PUT 'SIEVE 'DEFINED-ON-LINE '940) 
(PUT 'SIEVE 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SIEVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIEVE (INPU WGL)
    (PROG (SYM CL SW FLI BLI SU N H NEWSY TO_DROP SY)
      (SETQ CL (AEVAL (LIST 'SECOND INPU)))
      (SETQ SW (AEVAL (LIST 'FIRST WGL)))
      (SETQ FLI (AEVAL (LIST 'SECOND WGL)))
      (SETQ BLI (AEVAL (LIST 'THIRD WGL)))
      (SETQ SU (AEVAL (LIST 'LIST)))
      (SETQ N (AEVAL 0))
      (PROG (H)
        (SETQ H (GETRLIST (AEVAL FLI)))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ N (AEVAL (LIST 'PLUS N 1)))
            (SETQ SU
                    (AEVAL
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'F N)
                                 (LIST 'TIMES (LIST 'F N)
                                       (LIST 'EXPT 'LIN_TEST_CONST H)))
                           SU)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ N (AEVAL 0))
      (PROG (H)
        (SETQ H (GETRLIST (AEVAL BLI)))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ N (AEVAL (LIST 'PLUS N 1)))
            (SETQ SU
                    (AEVAL
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'B N)
                                 (LIST 'TIMES (LIST 'B N)
                                       (LIST 'EXPT 'LIN_TEST_CONST H)))
                           SU)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ SYM (AEVAL (LIST 'SUB SU (LIST 'FIRST INPU))))
      (AEVAL (LET '(LRULE1)))
      (SETQ SYM (AEVAL SYM))
      (AEVAL (CLEARRULES (LIST 'LRULE1)))
      (AEVAL (LET '(LRULE2)))
      (SETQ SYM (AEVAL SYM))
      (AEVAL (CLEARRULES (LIST 'LRULE2)))
      (SETQ NEWSY (AEVAL (LIST 'LIST)))
      (SETQ TO_DROP (AEVAL (LIST 'LIST)))
      (PROG (SY)
        (SETQ SY (GETRLIST (AEVAL SYM)))
       LAB
        (COND ((NULL SY) (RETURN NIL)))
        ((LAMBDA (SY)
           (PROGN
            (AEVAL (LIST 'COEFF (LIST 'LHS SY) 'LIN_TEST_CONST))
            (SETQ N (AEVAL (LIST 'PLUS HIPOW* SW)))
            (COND
             ((EVALLESSP (AEVAL N) 0)
              (PROGN
               (SETQ H
                       (AEVAL
                        (LIST 'TIMES
                              (LIST 'EXPT 'LIN_TEST_CONST (LIST 'MINUS N))
                              (LIST 'RHS SY))))
               (SETQ NEWSY
                       (AEVAL
                        (LIST 'CONS (LIST 'COEFFN H 'LIN_TEST_CONST N)
                              NEWSY)))))
             (T
              (SETQ NEWSY
                      (AEVAL
                       (LIST 'CONS
                             (LIST 'COEFFN (LIST 'RHS SY) 'LIN_TEST_CONST N)
                             NEWSY)))))
            (AEVAL 'NIL)))
         (CAR SY))
        (SETQ SY (CDR SY))
        (GO LAB))
      (SETQ SU (AEVAL (LIST 'LIST)))
      (PROG (H)
        (SETQ H (GETRLIST (AEVAL CL)))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((FREEOF (REVALX NEWSY) (REVALX H))
             (PROGN
              (SETQ SU (AEVAL (LIST 'CONS (LIST 'EQUAL H 0) SU)))
              (SETQ TO_DROP (AEVAL (LIST 'CONS H TO_DROP)))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'SUB SU (LIST 'FIRST INPU))
              (CONS 'LIST (SETDIFF (CDR CL) (CDR TO_DROP)))))))) 
(PUT 'NON_T_LHS_DVS 'NUMBER-OF-ARGS 1) 
(PUT 'NON_T_LHS_DVS 'DEFINED-ON-LINE '980) 
(PUT 'NON_T_LHS_DVS 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'NON_T_LHS_DVS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NON_T_LHS_DVS (PDES)
    (PROG (H FORBID)
      (PROG (H)
        (SETQ H PDES)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((AND (PAIRP H)
                  (OR (EQUAL (CAR H) 'EQUAL) (EQUAL (CAR H) 'REPLACE))
                  (PAIRP (CADR H))
                  (OR (EQUAL (CAADR H) 'DF) (EQUAL (CAADR H) 'D)
                      (EQUAL (CAADR H) 'F) (EQUAL (CAADR H) 'B))
                  (FREEOF (CADR H) T))
             (PROGN
              (SETQ FORBID (CONS (CADR H) FORBID))
              (COND
               ((EQUAL (CAADR H) 'D)
                (SETQ FORBID (CONS (LIST 'DF (CADDR (CADR H)) 'X) FORBID)))
               ((EQUAL (CAADR H) 'DF)
                (PROG (G)
                  (SETQ G 1)
                 LAB
                  (COND ((MINUSP (DIFFERENCE N_ G)) (RETURN NIL)))
                  (SETQ FORBID (CONS (LIST 'D G (CADR H)) FORBID))
                  (SETQ G (PLUS2 G 1))
                  (GO LAB))))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN FORBID))) 
(PUT 'SSYM 'NUMBER-OF-ARGS 9) 
(FLAG '(SSYM) 'OPFN) 
(PUT 'SSYM 'DEFINED-ON-LINE '998) 
(PUT 'SSYM 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSYM 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE SSYM (N TW SW AFWLIST ABWLIST EQNLIST FL INELIST MODE)
    (PROG (G H K CPU GTI FBNO PSYS PSYM MSYSP MSYMP TOTPOW SYSPOW SYMPOW NEWCD
           AFBLIST SUBLIST ZEROCOEFF FL_E NON_LIN_TEST DO_INE_TEST INIT SUBL
           SUBL2 SYS SYM LS RS LINSUB FILTER FORBID RHSSYL SYCON NW W NP P NQ Q
           PSYCON CN LP VERBOSE PLAIN_COM POWER_SPLIT_COM MSGBAK INTERACTIVE
           NFR NFE NBR NBE NF NB LHSLIST SAVE_LISTS)
      (AEVAL (LIST 'BACKUP_REDUCE_FLAGS))
      (PROGN
       (SETQ RECORD_HIST NIL)
       (COND (*TIME (PROGN (SETQ CPU (TIME)) (SETQ GTI (GCTIME)))))
       (SETQ EQNLIST (CDR EQNLIST))
       (SETQ FL (CDR FL))
       (COND (FL (SETQ HOMOGEN_ NIL)) (T (SETQ HOMOGEN_ T)))
       (SETQ MODE (CDR MODE))
       (COND ((MEMBER 'INIT MODE) (SETQ INIT T)) (T (SETQ INIT NIL)))
       (COND ((MEMBER 'ZEROCOEFF MODE) (SETQ ZEROCOEFF T))
             (T (SETQ ZEROCOEFF NIL)))
       (COND ((MEMBER 'PLAIN_COM MODE) (SETQ PLAIN_COM T))
             (T (SETQ PLAIN_COM NIL)))
       (COND ((MEMBER 'POWER_SPLIT_COM MODE) (SETQ POWER_SPLIT_COM T))
             (T (SETQ POWER_SPLIT_COM NIL)))
       (COND ((MEMBER 'VERBOSE MODE) (SETQ VERBOSE T)) (T (SETQ VERBOSE NIL)))
       (COND ((MEMBER 'INTERACTIVE MODE) (SETQ INTERACTIVE T))
             (T (SETQ INTERACTIVE NIL)))
       (COND ((MEMBER 'FILTER MODE) (SETQ FILTER T)) (T (SETQ FILTER NIL)))
       (COND ((MEMBER 'TPAR MODE) (AEVAL (ON (LIST 'T_CHANGES_PARITY))))
             (T (AEVAL (OFF (LIST 'T_CHANGES_PARITY)))))
       (COND ((MEMBER 'SPAR MODE) (AEVAL (ON (LIST 'S_CHANGES_PARITY))))
             (T (AEVAL (OFF (LIST 'S_CHANGES_PARITY)))))
       (COND ((MEMBER 'LOG MODE) (SETQ SAVE_LISTS T)))
       (COND
        ((MEMBER 'LIN MODE)
         (PROGN
          (SETQ NF (DIFFERENCE (LENGTH AFWLIST) 1))
          (SETQ NB (DIFFERENCE (LENGTH ABWLIST) 1))
          (COND
           ((MEMBER 'SPAR MODE)
            (COND
             ((NEQ NF NB)
              (PROGN
               (PROGN
                (PRIN2 "In the case of spar #(boson fields)=#(fermion fields)")
                (PRIN2 " which is not the case.")
                NIL)
               (SETQ FORBID T)))
             (T NIL)))
           (T
            (PROGN
             (COND
              ((NULL (EVENP NF))
               (PROGN
                (PROGN
                 (PRIN2 "If no spar then #(fermion fields) needs to be even")
                 NIL)
                (SETQ FORBID T))))
             (COND
              ((NULL (EVENP NB))
               (PROGN
                (PROGN
                 (PRIN2 "If no spar then #(boson fields) needs to be even")
                 NIL)
                (SETQ FORBID T))))
             (PROG (G)
               (SETQ G EQNLIST)
              LAB
               (COND ((NULL G) (RETURN NIL)))
               ((LAMBDA (G)
                  (COND ((PAIRP G) (SETQ LHSLIST (CONS (CADR G) LHSLIST)))))
                (CAR G))
               (SETQ G (CDR G))
               (GO LAB))
             (SETQ NF (QUOTIENT NF 2))
             (SETQ NB (QUOTIENT NB 2))
             (PROG (G)
               (SETQ G LHSLIST)
              LAB
               (COND ((NULL G) (RETURN NIL)))
               ((LAMBDA (G)
                  (COND
                   ((EQUAL (CAR G) 'DF)
                    (COND
                     ((EQUAL (CAADR G) 'F)
                      (COND
                       ((OR
                         (AND (LEQ (CADADR G) NF)
                              (NOT
                               (MEMBER
                                (LIST 'DF (LIST 'F (PLUS (CADADR G) NF))
                                      (CADDR G))
                                LHSLIST)))
                         (AND (GREATERP (CADADR G) NF)
                              (NOT
                               (MEMBER
                                (LIST 'DF (LIST 'F (DIFFERENCE (CADADR G) NF))
                                      (CADDR G))
                                LHSLIST))))
                        (PROGN
                         (PROGN
                          (PRIN2 "The counterpart of ")
                          (PRIN2 G)
                          (PRIN2 " is missing on a left hand side.")
                          NIL)
                         (SETQ FORBID T)))
                       (T NIL)))
                     ((EQUAL (CAADR G) 'B)
                      (COND
                       ((OR
                         (AND (LEQ (CADADR G) NB)
                              (NOT
                               (MEMBER
                                (LIST 'DF (LIST 'B (PLUS (CADADR G) NB))
                                      (CADDR G))
                                LHSLIST)))
                         (AND (GREATERP (CADADR G) NB)
                              (NOT
                               (MEMBER
                                (LIST 'DF (LIST 'B (DIFFERENCE (CADADR G) NB))
                                      (CADDR G))
                                LHSLIST))))
                        (PROGN
                         (PROGN
                          (PRIN2 "The counterpart of ")
                          (PRIN2 G)
                          (PRIN2 " is missing on a left hand side.")
                          NIL)
                         (SETQ FORBID T)))
                       (T NIL)))
                     (T NIL)))
                   ((EQUAL (CAR G) 'D)
                    (COND
                     ((EQUAL (CAADDR G) 'F)
                      (COND
                       ((OR
                         (AND (LEQ (CAR (CDADDR G)) NF)
                              (NOT
                               (MEMBER
                                (LIST 'D (CADR G)
                                      (LIST 'F (PLUS (CAR (CDADDR G)) NF)))
                                LHSLIST)))
                         (AND (GREATERP (CAR (CDADDR G)) NF)
                              (NOT
                               (MEMBER
                                (LIST 'D (CADR G)
                                      (LIST 'F
                                            (DIFFERENCE (CAR (CDADDR G)) NF)))
                                LHSLIST))))
                        (PROGN
                         (PROGN
                          (PRIN2 "The counterpart of ")
                          (PRIN2 G)
                          (PRIN2 " is missing on a left hand side.")
                          NIL)
                         (SETQ FORBID T)))
                       (T NIL)))
                     ((EQUAL (CAADDR G) 'B)
                      (COND
                       ((OR
                         (AND (LEQ (CAR (CDADDR G)) NB)
                              (NOT
                               (MEMBER
                                (LIST 'D (CADR G)
                                      (LIST 'B (PLUS (CAR (CDADDR G)) NB)))
                                LHSLIST)))
                         (AND (GREATERP (CAR (CDADDR G)) NB)
                              (NOT
                               (MEMBER
                                (LIST 'D (CADR G)
                                      (LIST 'B
                                            (DIFFERENCE (CAR (CDADDR G)) NB)))
                                LHSLIST))))
                        (PROGN
                         (PROGN
                          (PRIN2 "The counterpart of ")
                          (PRIN2 G)
                          (PRIN2 " is missing on a left hand side.")
                          NIL)
                         (SETQ FORBID T)))
                       (T NIL)))
                     (T NIL)))
                   (T NIL)))
                (CAR G))
               (SETQ G (CDR G))
               (GO LAB)))))
          (COND (FORBID (PROGN (SETQ FORBID NIL) (REDERR ""))))
          NIL)))
       (COND
        (NIL
         (COND
          ((MEMBER 'LIN MODE)
           (PROGN
            (SETQ NFR 0)
            (SETQ NBR 0)
            (SETQ NFE 0)
            (SETQ NBE 0)
            (PROG (G)
              (SETQ G EQNLIST)
             LAB
              (COND ((NULL G) (RETURN NIL)))
              ((LAMBDA (G)
                 (COND
                  ((PAIRP G)
                   (COND
                    ((EQUAL (CAR G) 'EQUAL)
                     (COND ((NOT (FREEOF (CADR G) 'F)) (SETQ NFE (PLUS NFE 1)))
                           ((NOT (FREEOF (CADR G) 'B)) (SETQ NBE (PLUS NBE 1)))
                           (T NIL)))
                    ((EQUAL (CAR G) 'REPLACEBY)
                     (COND ((NOT (FREEOF (CADR G) 'F)) (SETQ NFR (PLUS NFR 1)))
                           ((NOT (FREEOF (CADR G) 'B)) (SETQ NBR (PLUS NBR 1)))
                           (T NIL)))
                    (T NIL)))))
               (CAR G))
              (SETQ G (CDR G))
              (GO LAB))
            (PROGN
             (PRIN2 "nfr= ")
             (PRIN2 NFR)
             (PRIN2 " nfe=")
             (PRIN2 NFE)
             (PRIN2 "nbr= ")
             (PRIN2 NBR)
             (PRIN2 " nbe=")
             (PRIN2 NBE)
             NIL)
            (COND
             ((OR (AND (MEMBER 'SPAR MODE) (EQUAL NFR NBE) (EQUAL NBR NFE))
                  (AND (NOT (MEMBER 'SPAR MODE)) (EQUAL NFR NFE)
                       (EQUAL NBR NBE)))
              NIL)
             (T
              (PROGN
               (COND
                ((MEMBER 'SPAR MODE)
                 (PROGN
                  (COND
                   ((NEQ NFR NBE)
                    (PROGN
                     (PROGN
                      (PRIN2 "For spar 
      the number of '=>' relations with a fermion on the lhs should be equal 
      the number of '='  relations with a boson   on the lhs.")
                      NIL)
                     (TERPRI))))
                  (COND
                   ((NEQ NBR NFE)
                    (PROGN
                     (PROGN
                      (PRIN2 "For spar 
      the number of '=>' relations with a boson   on the lhs should be equal 
      the number of '='  relations with a fermion on the lhs.")
                      NIL)
                     (TERPRI))))))
                ((NOT (MEMBER 'SPAR MODE))
                 (PROGN
                  (COND
                   ((NEQ NFR NFE)
                    (PROGN
                     (PROGN
                      (PRIN2 "For missing spar 
      the number of '=>' relations with a fermion on the lhs should be equal 
      the number of '='  relations with a fermion on the lhs.")
                      NIL)
                     (TERPRI))))
                  (COND
                   ((NEQ NBR NBE)
                    (PROGN
                     (PROGN
                      (PRIN2 "For missing spar 
      the number of '=>' relations with a boson   on the lhs should be equal 
      the number of '='  relations with a boson   on the lhs.")
                      NIL)
                     (TERPRI)))))))
               (REDERR "")))))))))
       (INPUT_CONSISTENCY_TEST AFWLIST ABWLIST)
       (SETQ N_ N)
       (SETQ FLIST
               (PROG (G FORALL-RESULT FORALL-ENDPTR)
                 (SETQ G 1)
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH AFWLIST) 1) G))
                   (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'F G) NIL)))
                LOOPLABEL
                 (SETQ G (PLUS2 G 1))
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH AFWLIST) 1) G))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'F G) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (COND ((AND FLIST (EQUAL (LENGTH FLIST) 1)) (PUT 'F 'PRIFN 'MYFPRI))
             (T (PUT 'F 'PRIFN NIL)))
       (SETQ BLIST
               (PROG (G FORALL-RESULT FORALL-ENDPTR)
                 (SETQ G 1)
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH ABWLIST) 1) G))
                   (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'B G) NIL)))
                LOOPLABEL
                 (SETQ G (PLUS2 G 1))
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH ABWLIST) 1) G))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'B G) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (COND ((AND BLIST (EQUAL (LENGTH BLIST) 1)) (PUT 'B 'PRIFN 'MYFPRI))
             (T (PUT 'B 'PRIFN NIL)))
       (SETQ FBLIST (APPEND FLIST BLIST)))
      (SETQ AFBLIST (AEVAL (CONS 'LIST FBLIST)))
      (PROG (G)
        (SETQ G (GETRLIST (AEVAL AFBLIST)))
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G) (AEVAL (DEPEND (LIST G 'X 'T)))) (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (PROGN
       (SETQ FORBID (NON_T_LHS_DVS EQNLIST))
       (TERPRI)
       (PROGN
        (PRIN2 "#######  This is the case N")
        (PRIN2 N)
        (PRIN2 "f")
        (PRIN2 (LENGTH FLIST))
        (PRIN2 "b")
        (PRIN2 (LENGTH BLIST))
        (PRIN2 "t")
        (PRIN2 TW)
        (PRIN2 "s")
        (PRIN2 SW)
        (PRIN2 "w")
        NIL)
       (PROG (G)
         (SETQ G (APPEND (CDR AFWLIST) (CDR ABWLIST)))
        LAB
         (COND ((NULL G) (RETURN NIL)))
         ((LAMBDA (G) (PROGN (PRIN2 G) NIL)) (CAR G))
         (SETQ G (CDR G))
         (GO LAB))
       (PROGN (PRIN2 ".  #######") NIL)
       (TERPRI)
       (COND
        (FNAME_LIST
         (PROGN
          (SETQ FNAME_ (CAR FNAME_LIST))
          (SETQ FNAME_LIST (CDR FNAME_LIST))))
        (T (SETQ FNAME_ 'P)))
       (COND
        ((AND EQNLIST (PAIRP (CAR EQNLIST))
              (OR
               (AND (EQUAL (CAAR EQNLIST) 'EQUAL) (PAIRP (CADAR EQNLIST))
                    (EQUAL (CAADAR EQNLIST) 'DF))
               (AND (EQUAL (CAAR EQNLIST) 'REPLACEBY) (PAIRP (CADAR EQNLIST))
                    (OR (EQUAL (CAADAR EQNLIST) 'DF)
                        (EQUAL (CAADAR EQNLIST) 'D)))))
         (PROGN
          (SETQ G NIL)
          (PROG (H)
            (SETQ H EQNLIST)
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H)
               (COND
                ((PAIRP H)
                 (COND
                  ((AND (PAIRP H) (EQUAL (CAR H) 'EQUAL) (PAIRP (CADR H))
                        (EQUAL (CAADR H) 'DF)
                        (EQUAL (REVAL1 (CADDR (CADR H)) T) 'T))
                   (PROGN
                    (SETQ SYS (CONS H SYS))
                    (SETQ SUBL2
                            (CONS (LIST 'REPLACEBY (CADR H) (CADDR H))
                                  SUBL2))))
                  ((AND (EQUAL (CAR H) 'REPLACEBY) (PAIRP (CADR H))
                        (OR (EQUAL (CAADR H) 'DF) (EQUAL (CAADR H) 'D)))
                   (SETQ SUBL (CONS H SUBL)))
                  (T (SETQ G (CONS H G)))))))
             (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (SETQ EQNLIST G)
          (SETQ SYSFBL
                  (REVERSE
                   (PROG (H FORALL-RESULT FORALL-ENDPTR)
                     (SETQ H SYS)
                     (COND ((NULL H) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (H) (CADADR H)) (CAR H))
                                           NIL)))
                    LOOPLABEL
                     (SETQ H (CDR H))
                     (COND ((NULL H) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (H) (CADADR H)) (CAR H)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
          (COND
           (VERBOSE
            (PROGN
             (PROGN (PRIN2 "Simplifying the substitutions.") NIL)
             (TERPRI))))
          (SETQ SUBL2 (CONS 'LIST (APPEND SUBL SUBL2)))
          (AEVAL (LET (LIST SUBL2)))
          (SETQ SUBL
                  (PROG (H FORALL-RESULT FORALL-ENDPTR)
                    (SETQ H SUBL)
                    (COND ((NULL H) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (H)
                                        (LIST 'REPLACEBY (CADR H)
                                              (AEVAL (CADDR H))))
                                      (CAR H))
                                     NIL)))
                   LOOPLABEL
                    (SETQ H (CDR H))
                    (COND ((NULL H) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (H)
                                (LIST 'REPLACEBY (CADR H) (AEVAL (CADDR H))))
                              (CAR H))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          NIL
          (SETQ SUBL (CONS 'LIST (REVERSE SUBL)))
          (AEVAL (CLEARRULES (LIST SUBL2)))
          (SETQ SUBL2 NIL)
          (SETQ SYS (CONS 'LIST (REVERSE SYS)))
          (SETQ FL_E FL)
          (SETQ FL NIL)))
        (T
         (PROGN
          (SETQ DO_INE_TEST T)
          (COND ((NULL EQNLIST) (SETQ NON_LIN_TEST T)))
          (SETQ SYSFBL (APPEND FLIST BLIST))
          (SETQ SUBL2 (CONS 'LIST (APPEND SUBL SUBL2)))
          (COND
           (VERBOSE
            (PROGN
             (PROGN (PRIN2 "Formulating rhs's of the system") NIL)
             (TERPRI))))
          (SETQ H
                  (SSPOL N SYSFBL AFWLIST ABWLIST TW (AEVAL 'T) LINSUB FORBID
                   *T_CHANGES_PARITY VERBOSE))
          (SETQ SYS (CAR H))
          (SETQ FL_E (CADR H))
          (SETQ SUBL (LIST 'LIST)))))
       (PROGN (AEVAL (NODEPEND (LIST 'F 'S))) (AEVAL (NODEPEND (LIST 'B 'S))))
       (PROG (H)
         (SETQ H FBLIST)
        LAB
         (COND ((NULL H) (RETURN NIL)))
         ((LAMBDA (H) (AEVAL (NODEPEND (LIST H 'S)))) (CAR H))
         (SETQ H (CDR H))
         (GO LAB))
       (PROG (H)
         (SETQ H SYSFBL)
        LAB
         (COND ((NULL H) (RETURN NIL)))
         ((LAMBDA (H) (AEVAL (DEPEND (LIST H 'S)))) (CAR H))
         (SETQ H (CDR H))
         (GO LAB))
       (SETQ SUBL (ADD_DF_RULES_TO_D_RULE SUBL))
       (COND
        (FNAME_LIST
         (PROGN
          (SETQ FNAME_ (CAR FNAME_LIST))
          (SETQ FNAME_LIST (CDR FNAME_LIST))))
        (T (SETQ FNAME_ 'Q)))
       (COND
        (VERBOSE
         (PROGN
          (PROGN (PRIN2 "Formulating rhs's of the symmetry") NIL)
          (TERPRI))))
       (SETQ H
               (SSPOL N SYSFBL AFWLIST ABWLIST SW (AEVAL 'S) LINSUB FORBID
                *S_CHANGES_PARITY VERBOSE))
       (COND
        ((AND FILTER (PAIRP (AEVAL 'HOM_WEI))
              (EQUAL (CAR (AEVAL 'HOM_WEI)) 'LIST))
         (PROGN
          (COND
           (VERBOSE
            (PROGN
             (PROGN (PRIN2 "Applying an extra homogeneity filter") NIL)
             (TERPRI))))
          (SETQ H (LIST 'LIST (CAR H) (CONS 'LIST (CADR H))))
          (PROG (G)
            (SETQ G (GETRLIST (AEVAL 'HOM_WEI)))
           LAB
            (COND ((NULL G) (RETURN NIL)))
            ((LAMBDA (G) (SETQ H (AEVAL (LIST 'SIEVE H G)))) (CAR G))
            (SETQ G (CDR G))
            (GO LAB))
          (SETQ H (LIST (CADR H) (CDADDR H))))))
       (SETQ SYM (CAR H))
       (SETQ FLIN_ (CADR H))
       (SETQ H NIL)
       (SETQ G EQNLIST)
       (SETQ EQNLIST NIL)
       (PROG (H)
         (SETQ H G)
        LAB
         (COND ((NULL H) (RETURN NIL)))
         ((LAMBDA (H)
            (COND
             ((AND (PAIRP H) (EQUAL (CAR H) 'EQUAL))
              (PROGN
               (SETQ SUBLIST (CONS H SUBLIST))
               (SETQ EQNLIST
                       (CONS (LIST 'DIFFERENCE (CADR H) (CADDR H)) EQNLIST))))
             (T (SETQ EQNLIST (CONS H EQNLIST)))))
          (CAR H))
         (SETQ H (CDR H))
         (GO LAB))
       (COND
        (ZEROCOEFF
         (PROG (H)
           (SETQ H (APPEND FL_E FLIN_))
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND
               ((FREEOF INELIST H)
                (SETQ SUBLIST (CONS (LIST 'EQUAL H 0) SUBLIST)))
               (T (SETQ FL (CONS H FL)))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB)))
        (T (SETQ FL (APPEND FL_E FLIN_))))
       (SETQ FL_E NIL)
       (SETQ FL (CONS 'LIST FL))
       (SETQ SUBLIST (CONS 'LIST SUBLIST))
       (SETQ EQNLIST (CONS 'LIST EQNLIST))
       NIL)
      (COND
       ((BOOLVALUE* SUBLIST)
        (PROGN
         (SETQ SYS (AEVAL (LIST 'SUB SUBLIST SYS)))
         (SETQ SYM (AEVAL (LIST 'SUB SUBLIST SYM))))))
      (COND
       ((BOOLVALUE* (REVALX SAVE_LISTS))
        (PROGN
         (AEVAL (OFF (LIST 'NAT)))
         (AEVAL (OUT (LIST "drvlist")))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SYS)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (ASSGNPRI (AEVAL (LIST 'LHS G)) NIL 'FIRST)
               (ASSGNPRI (AEVAL " := ") NIL NIL)
               (ASSGNPRI (AEVAL (LIST 'RHS G)) NIL 'LAST)))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SYM)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (ASSGNPRI (AEVAL (LIST 'LHS G)) NIL 'FIRST)
               (ASSGNPRI (AEVAL " := ") NIL NIL)
               (ASSGNPRI (AEVAL (LIST 'RHS G)) NIL 'LAST)))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (ASSGNPRI (AEVAL "end$") NIL 'ONLY)
         (AEVAL (SHUT (LIST "drvlist")))
         (AEVAL 'NIL))))
      (AEVAL (ON (LIST 'NAT)))
      (AEVAL (ON (LIST 'DFPRINT)))
      (AEVAL (OFF (LIST 'NOARG)))
      (COND
       ((BOOLVALUE* (REVALX SAVE_LISTS))
        (PROGN
         (AEVAL (OUT (LIST "evolist")))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SYS)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (ASSGNPRI (AEVAL (LIST 'LHS G)) NIL 'FIRST)
               (ASSGNPRI (AEVAL " := ") NIL NIL)
               (ASSGNPRI (AEVAL (LIST 'RHS G)) NIL 'LAST)))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (PROGN
          (TERPRI)
          (PROGN (PRIN2 "</pre> with symmetries") NIL)
          (TERPRI)
          (PROGN (PRIN2 "<pre>") NIL))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SYM)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (ASSGNPRI (AEVAL (LIST 'LHS G)) NIL 'FIRST)
               (ASSGNPRI (AEVAL " := ") NIL NIL)
               (ASSGNPRI (AEVAL (LIST 'RHS G)) NIL 'LAST)))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (AEVAL (SHUT (LIST "evolist")))
         (AEVAL (OUT (LIST "unolist")))
         (PROGN (LISTPRINT (REVERSE (CDR FL))) (TERPRI))
         (AEVAL (SHUT (LIST "unolist")))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* INTERACTIVE)
        (PROGN (AEVAL (OFF (LIST 'BATCH_MODE))) (SETQ PRINT_ 6)))
       (T (PROGN (AEVAL (ON (LIST 'BATCH_MODE))) (SETQ PRINT_ NIL))))
      (PROGN
       (SETQ USE_NEW_CRACKOUT T)
       (SETQ COLLECT_SOL NIL)
       (SETQ SUBST_1 11)
       (SETQ PDELIMIT_1 400)
       (SETQ MAX_GC_SHORT 4)
       (SETQ MAX_GC_RED_LEN 4)
       NIL)
      (COND
       ((EVALEQUAL (AEVAL (LIST 'LENGTH SYS)) 1)
        (ASSGNPRI (AEVAL "The equation:") NIL 'ONLY))
       (T (ASSGNPRI (AEVAL "The system:") NIL 'ONLY)))
      (PROG (G)
        (SETQ G (GETRLIST (AEVAL SYS)))
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G) (ASSGNPRI (AEVAL G) NIL 'ONLY)) (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (COND
       ((EVALNEQ (AEVAL SUBL) (AEVAL (LIST 'LIST)))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'LENGTH SUBL)) 1)
           (ASSGNPRI (AEVAL "Extra condition:") NIL 'ONLY))
          (T (ASSGNPRI (AEVAL "Extra conditions:") NIL 'ONLY)))
         (AEVAL (OFF (LIST 'NAT)))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SUBL)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G) (ASSGNPRI (AEVAL G) NIL 'ONLY)) (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (AEVAL (ON (LIST 'NAT))))))
      (COND
       ((BOOLVALUE* VERBOSE)
        (PROGN
         (ASSGNPRI (AEVAL "The symmetry:") NIL 'ONLY)
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL SYM)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G) (ASSGNPRI (AEVAL G) NIL 'ONLY)) (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* DO_INE_TEST)
        (PROGN
         (SETQ INELIST
                 (AEVAL
                  (LIST 'LISTINE N SYS SYM FL INELIST NON_LIN_TEST
                        SAVE_LISTS)))
         (SETQ G (AEVAL INELIST))
         (WHILE
          (AND (EVALNEQ (AEVAL* G) (AEVAL* (LIST 'LIST)))
               (EVALNEQ (AEVAL* (LIST 'FIRST G)) 0))
          (SETQ G (AEVAL* (LIST 'REST G))))
         (COND
          ((EVALNEQ (AEVAL G) (AEVAL (LIST 'LIST)))
           (RETURN
            (PROGN
             (AEVAL (OUT (LIST "invalid")))
             (ASSGNPRI
              (AEVAL "SYSTEM & SYMMETRY DO NOT SATISFY MINIMAL REQUIREMENTS!")
              NIL 'ONLY)
             (AEVAL (SHUT (LIST "invalid")))
             (ASSGNPRI
              (AEVAL "SYSTEM & SYMMETRY DO NOT SATISFY MINIMAL REQUIREMENTS!")
              NIL 'ONLY)
             (AEVAL 'NIL))))))))
      (COND
       ((EVALNEQ (AEVAL SUBL) (AEVAL (LIST 'LIST))) (AEVAL (LET (LIST SUBL)))))
      (SETQ FBNO (AEVAL (LENGTH SYSFBL)))
      (COND
       ((EVALNEQ (AEVAL SUBL) (AEVAL (LIST 'LIST)))
        (SETQ POWER_SPLIT_COM (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* POWER_SPLIT_COM)
        (PROGN
         (SETQ LIN_TEST_CONST (GENSYM))
         (SETQ PSYS (AEVAL (LIST 'POWER_PARTI SYS)))
         (SETQ MSYSP (AEVAL (LIST 'FIRST PSYS)))
         (SETQ PSYS (AEVAL (LIST 'SECOND PSYS)))
         (SETQ PSYM (AEVAL (LIST 'POWER_PARTI SYM)))
         (SETQ MSYMP (AEVAL (LIST 'FIRST PSYM)))
         (SETQ PSYM (AEVAL (LIST 'SECOND PSYM)))
         (PROG (TOTPOW)
           (SETQ TOTPOW 2)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS MSYSP MSYMP)) TOTPOW))
             (RETURN NIL)))
           (PROGN
            (PROGN
             (PROGN
              (PRIN2 "Generating all terms of total degree ")
              (PRIN2 TOTPOW)
              NIL)
             (TERPRI))
            (SETQ NEWCD (AEVAL* (LIST 'LIST)))
            (PROG (G)
              (SETQ G 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* FBNO) G)) (RETURN NIL)))
              (SETQ NEWCD (AEVAL* (LIST 'CONS 0 NEWCD)))
              (SETQ G
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       G))
              (GO LAB))
            (PROG (SYSPOW)
              (SETQ SYSPOW 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MSYSP) SYSPOW))
                (RETURN NIL)))
              (PROG (SYMPOW)
                (SETQ SYMPOW 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MSYMP) SYMPOW))
                  (RETURN NIL)))
                (COND
                 ((EVALEQUAL (AEVAL* (LIST 'PLUS SYSPOW SYMPOW))
                             (AEVAL* TOTPOW))
                  (PROGN
                   (PROGN
                    (PROGN
                     (PRIN2 "  Pairing now degree ")
                     (PRIN2 SYSPOW)
                     (PRIN2 " terms from the system")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2 "         with degree ")
                     (PRIN2 SYMPOW)
                     (PRIN2 " terms from the symmetry")
                     NIL)
                    (TERPRI))
                   (SETQ SUBL2
                           (AEVAL*
                            (LIST 'APPEND
                                  (PROG (G FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ G (GETRLIST (AEVAL* PSYS)))
                                    (COND ((NULL G) (RETURN (MAKELIST NIL))))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (G)
                                                        (PROGN
                                                         (SETQ LS
                                                                 (AEVAL*
                                                                  (LIST 'LHS
                                                                        G)))
                                                         (SETQ RS
                                                                 (AEVAL*
                                                                  (LIST 'PART
                                                                        (LIST
                                                                         'RHS
                                                                         G)
                                                                        SYSPOW)))
                                                         (AEVAL*
                                                          (LIST 'REPLACEBY LS
                                                                RS))))
                                                      (CAR G))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ G (CDR G))
                                    (COND
                                     ((NULL G)
                                      (RETURN (CONS 'LIST FORALL-RESULT))))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (G)
                                                (PROGN
                                                 (SETQ LS
                                                         (AEVAL*
                                                          (LIST 'LHS G)))
                                                 (SETQ RS
                                                         (AEVAL*
                                                          (LIST 'PART
                                                                (LIST 'RHS G)
                                                                SYSPOW)))
                                                 (AEVAL*
                                                  (LIST 'REPLACEBY LS RS))))
                                              (CAR G))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))
                                  (PROG (G FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ G (GETRLIST (AEVAL* PSYM)))
                                    (COND ((NULL G) (RETURN (MAKELIST NIL))))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (G)
                                                        (PROGN
                                                         (SETQ LS
                                                                 (AEVAL*
                                                                  (LIST 'LHS
                                                                        G)))
                                                         (SETQ RS
                                                                 (AEVAL*
                                                                  (LIST 'PART
                                                                        (LIST
                                                                         'RHS
                                                                         G)
                                                                        SYMPOW)))
                                                         (AEVAL*
                                                          (LIST 'REPLACEBY LS
                                                                RS))))
                                                      (CAR G))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ G (CDR G))
                                    (COND
                                     ((NULL G)
                                      (RETURN (CONS 'LIST FORALL-RESULT))))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (G)
                                                (PROGN
                                                 (SETQ LS
                                                         (AEVAL*
                                                          (LIST 'LHS G)))
                                                 (SETQ RS
                                                         (AEVAL*
                                                          (LIST 'PART
                                                                (LIST 'RHS G)
                                                                SYMPOW)))
                                                 (AEVAL*
                                                  (LIST 'REPLACEBY LS RS))))
                                              (CAR G))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))))
                   (AEVAL* (LET (LIST SUBL2)))
                   (PROG (G)
                     (SETQ G 1)
                    LAB
                     (COND
                      ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* FBNO) G))
                       (RETURN NIL)))
                     (SETQ NEWCD
                             (AEVAL*
                              (LIST 'SETPART* NEWCD G
                                    (AEVAL*
                                     (LIST 'PLUS (LIST 'PART NEWCD G)
                                           (LIST 'DF
                                                 (LIST 'PART
                                                       (LIST 'RHS
                                                             (LIST 'PART PSYS
                                                                   G))
                                                       SYSPOW)
                                                 'S)
                                           (COND
                                            ((AND
                                              (BOOLVALUE*
                                               (REVALX T_CHANGES_PARITY))
                                              (BOOLVALUE*
                                               (REVALX S_CHANGES_PARITY)))
                                             (AEVAL*
                                              (LIST 'DF
                                                    (LIST 'PART
                                                          (LIST 'RHS
                                                                (LIST 'PART
                                                                      PSYM G))
                                                          SYMPOW)
                                                    'T)))
                                            (T
                                             (AEVAL*
                                              (LIST 'MINUS
                                                    (LIST 'DF
                                                          (LIST 'PART
                                                                (LIST 'RHS
                                                                      (LIST
                                                                       'PART
                                                                       PSYM G))
                                                                SYMPOW)
                                                          'T))))))))))
                     (SETQ G
                             ((LAMBDA (FORALL-RESULT)
                                (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                              G))
                     (GO LAB))
                   (AEVAL* (CLEARRULES (LIST SUBL2))))))
                (SETQ SYMPOW
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         SYMPOW))
                (GO LAB))
              (SETQ SYSPOW
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       SYSPOW))
              (GO LAB))
            (SETQ EQNLIST (AEVAL* (LIST 'APPEND NEWCD EQNLIST))))
           (SETQ TOTPOW
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    TOTPOW))
           (GO LAB))
         (SETQ PSYS (SETQ PSYM (AEVAL 0))))))
      (SETQ SUBL2
              (AEVAL
               (LIST 'APPEND
                     (PROG (G FORALL-RESULT FORALL-ENDPTR)
                       (SETQ G (GETRLIST (AEVAL SYS)))
                       (COND ((NULL G) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (G)
                                           (PROGN
                                            (SETQ LS (AEVAL (LIST 'LHS G)))
                                            (SETQ RS (AEVAL (LIST 'RHS G)))
                                            (AEVAL (LIST 'REPLACEBY LS RS))))
                                         (CAR G))
                                        NIL)))
                      LOOPLABEL
                       (SETQ G (CDR G))
                       (COND ((NULL G) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (G)
                                   (PROGN
                                    (SETQ LS (AEVAL (LIST 'LHS G)))
                                    (SETQ RS (AEVAL (LIST 'RHS G)))
                                    (AEVAL (LIST 'REPLACEBY LS RS))))
                                 (CAR G))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (G FORALL-RESULT FORALL-ENDPTR)
                       (SETQ G (GETRLIST (AEVAL SYM)))
                       (COND ((NULL G) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (G)
                                           (PROGN
                                            (SETQ LS (AEVAL (LIST 'LHS G)))
                                            (SETQ RS (AEVAL (LIST 'RHS G)))
                                            (AEVAL (LIST 'REPLACEBY LS RS))))
                                         (CAR G))
                                        NIL)))
                      LOOPLABEL
                       (SETQ G (CDR G))
                       (COND ((NULL G) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (G)
                                   (PROGN
                                    (SETQ LS (AEVAL (LIST 'LHS G)))
                                    (SETQ RS (AEVAL (LIST 'RHS G)))
                                    (AEVAL (LIST 'REPLACEBY LS RS))))
                                 (CAR G))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (AEVAL (LET (LIST SUBL2)))
      (COND
       ((BOOLVALUE* VERBOSE)
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "sys = ") NIL 'FIRST)
          (ASSGNPRI (AEVAL SYS) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "sym = ") NIL 'FIRST)
          (ASSGNPRI (AEVAL SYM) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "df(rhs part(sys,1),s) = ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'DF (LIST 'RHS (LIST 'PART SYS 1)) 'S)) NIL
                    'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "df(rhs part(sym,1),t) = ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'DF (LIST 'RHS (LIST 'PART SYM 1)) 'T)) NIL
                    'LAST))
         (AEVAL 'NIL))))
      (COND
       ((AND (NOT (BOOLVALUE* POWER_SPLIT_COM)) (BOOLVALUE* PLAIN_COM))
        (PROG (G)
          (SETQ G 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* FBNO) G)) (RETURN NIL)))
          (SETQ EQNLIST
                  (AEVAL*
                   (LIST 'CONS
                         (LIST 'PLUS
                               (LIST 'DF (LIST 'RHS (LIST 'PART SYS G)) 'S)
                               (COND
                                ((AND (BOOLVALUE* (REVALX T_CHANGES_PARITY))
                                      (BOOLVALUE* (REVALX S_CHANGES_PARITY)))
                                 (AEVAL*
                                  (LIST 'DF (LIST 'RHS (LIST 'PART SYM G))
                                        'T)))
                                (T
                                 (AEVAL*
                                  (LIST 'MINUS
                                        (LIST 'DF
                                              (LIST 'RHS (LIST 'PART SYM G))
                                              'T))))))
                         EQNLIST)))
          (SETQ G
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   G))
          (GO LAB)))
       ((NOT (BOOLVALUE* POWER_SPLIT_COM))
        (PROGN
         (SETQ SYM (REVAL1 SYM T))
         (SETQ EQNLIST (CDR EQNLIST))
         (SETQ RHSSYL
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H (CDR SYM))
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (H)
                                       (COND
                                        ((AND (PAIRP (CADDR H))
                                              (EQUAL (CAADDR H) 'PLUS))
                                         (CDADDR H))
                                        (T (LIST (CADDR H)))))
                                     (CAR H))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (H)
                               (COND
                                ((AND (PAIRP (CADDR H))
                                      (EQUAL (CAADDR H) 'PLUS))
                                 (CDADDR H))
                                (T (LIST (CADDR H)))))
                             (CAR H))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ NW 0)
         (PROG (W)
           (SETQ W SYSFBL)
          LAB
           (COND ((NULL W) (RETURN NIL)))
           ((LAMBDA (W)
              (PROGN
               (COND
                (VERBOSE
                 (PROGN
                  (PRIN2 "Integrability conditions are computed for ")
                  (PRIN2 W)
                  (PRIN2 " :")
                  NIL)))
               (TERPRI)
               (SETQ NW (ADD1 NW))
               (SETQ SYCON 0)
               (SETQ NP 0)
               (SETQ MSGBAK *MSG)
               (SETQ *MSG NIL)
               (PROG (H)
                 (SETQ H SYSFBL)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))
                     (SETK (LIST 'DF H 'S) (AEVAL 0))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (SETQ *MSG MSGBAK)
               (SETQ CN 0)
               (SETQ LP
                       (PROG (P FORALL-RESULT)
                         (SETQ P RHSSYL)
                         (SETQ FORALL-RESULT 0)
                        LAB1
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (SETQ FORALL-RESULT
                                 (PLUS ((LAMBDA (P) (LENGTH P)) (CAR P))
                                       FORALL-RESULT))
                         (SETQ P (CDR P))
                         (GO LAB1)))
               (SETQ PSYCON NIL)
               (PROG (P)
                 (SETQ P RHSSYL)
                LAB
                 (COND ((NULL P) (RETURN NIL)))
                 ((LAMBDA (P)
                    (PROGN
                     (SETQ MSGBAK *MSG)
                     (SETQ *MSG NIL)
                     (COND
                      ((GREATERP NP 0)
                       (PROGN
                        (SETQ H (NTH SYSFBL NP))
                        (PROGN
                         (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))
                         (SETK (LIST 'DF H 'S) (AEVAL 0))))))
                     (SETQ *MSG MSGBAK)
                     (SETQ NP (ADD1 NP))
                     (SETQ H (NTH SYSFBL NP))
                     (SETQ NQ 0)
                     (PROG (Q)
                       (SETQ Q P)
                      LAB
                       (COND ((NULL Q) (RETURN NIL)))
                       ((LAMBDA (Q)
                          (PROGN
                           (SETQ NQ (ADD1 NQ))
                           (SETQ CN (ADD1 CN))
                           (COND
                            (VERBOSE
                             (PROGN
                              (ASSGNPRI (AEVAL "   ") NIL 'FIRST)
                              (ASSGNPRI (AEVAL CN) NIL NIL)
                              (ASSGNPRI (AEVAL "(") NIL NIL)
                              (ASSGNPRI (AEVAL LP) NIL NIL)
                              (ASSGNPRI (AEVAL ")/") NIL NIL)
                              (ASSGNPRI (AEVAL NW) NIL NIL)
                              (ASSGNPRI (AEVAL "(") NIL NIL)
                              (ASSGNPRI (AEVAL FBNO) NIL NIL)
                              (ASSGNPRI (AEVAL "). Currently non-vanishing: ")
                                        NIL NIL)
                              (ASSGNPRI Q NIL 'LAST))))
                           (SETQ MSGBAK *MSG)
                           (SETQ *MSG NIL)
                           (PROGN
                            (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))
                            (SETK (LIST 'DF H 'S) (AEVAL (NTH P NQ))))
                           (SETQ *MSG MSGBAK)
                           (SETQ PSYCON
                                   (CONS
                                    (PROGN
                                     (SETQ K (AEVAL (LIST 'DF W 'T)))
                                     (SETQ G (AEVAL (LIST 'DF W 'S)))
                                     (AEVAL
                                      (LIST 'PLUS (LIST 'DF K 'S)
                                            (COND
                                             ((AND
                                               (BOOLVALUE*
                                                (REVALX T_CHANGES_PARITY))
                                               (BOOLVALUE*
                                                (REVALX S_CHANGES_PARITY)))
                                              (AEVAL (LIST 'DF G 'T)))
                                             (T
                                              (AEVAL
                                               (LIST 'MINUS
                                                     (LIST 'DF G 'T))))))))
                                    PSYCON))))
                        (CAR Q))
                       (SETQ Q (CDR Q))
                       (GO LAB))))
                  (CAR P))
                 (SETQ P (CDR P))
                 (GO LAB))
               (PROGN
                (SETQ SYCON (AEVAL (LIST 'NUM (CONS 'PLUS PSYCON))))
                (SETQ H (AEVAL (LIST 'LENGTH SYCON)))
                (COND
                 ((BOOLVALUE* VERBOSE)
                  (COND
                   ((EVALEQUAL (AEVAL H) 1)
                    (PROGN
                     (ASSGNPRI (AEVAL "The symmetry condition for ") NIL
                               'FIRST)
                     (ASSGNPRI W NIL NIL)
                     (ASSGNPRI (AEVAL " has ") NIL NIL)
                     (ASSGNPRI (AEVAL H) NIL NIL)
                     (ASSGNPRI (AEVAL " term.") NIL 'LAST)))
                   (T
                    (PROGN
                     (ASSGNPRI (AEVAL "The symmetry condition for ") NIL
                               'FIRST)
                     (ASSGNPRI W NIL NIL)
                     (ASSGNPRI (AEVAL " has ") NIL NIL)
                     (ASSGNPRI (AEVAL H) NIL NIL)
                     (ASSGNPRI (AEVAL " terms.") NIL 'LAST)))))))
               (SETQ EQNLIST (CONS SYCON EQNLIST))))
            (CAR W))
           (SETQ W (CDR W))
           (GO LAB))
         (SETQ EQNLIST (CONS 'LIST EQNLIST))
         (SETQ MSGBAK *MSG)
         (SETQ *MSG NIL)
         (PROG (H)
           (SETQ H SYSFBL)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H) (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))) (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (SETQ *MSG MSGBAK)
         NIL)))
      (PROGN (SETQ MSGBAK *MSG) (SETQ *MSG NIL))
      (AEVAL (CLEARRULES (LIST SUBL2)))
      (AEVAL (CLEARRULES (LIST SUBL)))
      (SETQ *MSG MSGBAK)
      (SETK 'SUBLI (AEVAL SUBL))
      (SETK 'SYMANSATZ (AEVAL SUBL2))
      (SETQ SYS (SETQ SYM (AEVAL 0)))
      (COND
       ((NOT (BOOLVALUE* INIT))
        (PROGN
         (COND
          (*TIME
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2 "CPU time so far: ")
             (PRIN2 (DIFFERENCE (TIME) CPU))
             (PRIN2 " ms  ")
             (PRIN2 " GC time so far: ")
             (PRIN2 (DIFFERENCE (GCTIME) GTI))
             (PRIN2 " ms")
             NIL)
            (TERPRI))))
         (PROGN
          (SETQ H '(T S X))
          (SETQ EQNLIST
                  (SPLIT_SIMPLIFY
                   (LIST EQNLIST (LIST 'LIST) FL (CONS 'LIST H) T)))
          (COND
           (PRINT_
            (PROGN (PROGN (PRIN2 "Now crack is called.") NIL) (TERPRI))))
          (SETQ OLD_HISTORY '(CP (*COMMA* 1 3 45 11 8 20 27 30 47 21 38) S DP))
          NIL)
         (AEVAL (LIST 'CRACK EQNLIST INELIST FL (LIST 'LIST)))
         (PROGN
          (TERPRI)
          (PROGN
           (PRIN2 (COND ((NULL SOL_LIST) "No") (T (LENGTH SOL_LIST))))
           (PRIN2
            (COND ((EQUAL (LENGTH SOL_LIST) 1) " solution was")
                  (T " solutions were")))
           (PRIN2 " found.")
           NIL)
          (TERPRI)
          NIL)
         (AEVAL 'NIL)))))) 
(PUT 'SAMELISTS 'NUMBER-OF-ARGS 2) 
(FLAG '(SAMELISTS) 'OPFN) 
(PUT 'SAMELISTS 'DEFINED-ON-LINE '1592) 
(PUT 'SAMELISTS 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SAMELISTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAMELISTS (A B)
    (COND
     ((EVALEQUAL (AEVAL A) (AEVAL (LIST 'LIST)))
      (COND ((EVALEQUAL (AEVAL B) (AEVAL (LIST 'LIST))) (AEVAL 'TRUE))
            (T (AEVAL 'NIL))))
     ((EVALEQUAL (AEVAL B) (AEVAL (LIST 'LIST))) (AEVAL 'NIL))
     ((EVALNEQ (AEVAL (LIST 'FIRST A)) (AEVAL (LIST 'FIRST B))) (AEVAL 'NIL))
     (T (AEVAL (LIST 'SAMELISTS (LIST 'REST A) (LIST 'REST B)))))) 
(PUT 'CRACK_OUT 'NUMBER-OF-ARGS 5) 
(FLAG '(CRACK_OUT) 'OPFN) 
(PUT 'CRACK_OUT 'DEFINED-ON-LINE '1602) 
(PUT 'CRACK_OUT 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CRACK_OUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CRACK_OUT (EQNS ASSIGNS FREEF INEQ SOL_COUNT)
    (COND
     ((BOOLVALUE* (REVALX USE_NEW_CRACKOUT))
      (PROG (G H TM RATBAK ALLBAK EQLIST EQCP1 EQCP2 AFBLIST FF SYMLIST FRSYM
             ALL MSGBAK SOL_COUNT2)
        (SETQ ALLBAK (AEVAL *ALLFAC))
        (AEVAL (ON (LIST 'ALLFAC)))
        (SETQ RATBAK (AEVAL *RATIONAL))
        (AEVAL (ON (LIST 'RATIONAL)))
        (SETQ AFBLIST (AEVAL (CONS 'LIST SYSFBL)))
        (AEVAL (LET '(SYMANSATZ)))
        (SETK 'HTML_OUT (AEVAL 'NIL))
        (SETQ EQLIST
                (PROG (H FORALL-RESULT FORALL-ENDPTR)
                  (SETQ H (GETRLIST (AEVAL AFBLIST)))
                  (COND ((NULL H) (RETURN (MAKELIST NIL))))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (H)
                                      (AEVAL
                                       (LIST 'SUB ASSIGNS (LIST 'DF H 'T))))
                                    (CAR H))
                                   NIL)))
                 LOOPLABEL
                  (SETQ H (CDR H))
                  (COND ((NULL H) (RETURN (CONS 'LIST FORALL-RESULT))))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (H)
                              (AEVAL (LIST 'SUB ASSIGNS (LIST 'DF H 'T))))
                            (CAR H))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ EQLIST
                (AEVAL
                 (LIST 'APPEND EQLIST
                       (PROG (H FORALL-RESULT FORALL-ENDPTR)
                         (SETQ H (GETRLIST (AEVAL AFBLIST)))
                         (COND ((NULL H) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (H)
                                             (AEVAL
                                              (LIST 'SUB ASSIGNS
                                                    (LIST 'DF H 'S))))
                                           (CAR H))
                                          NIL)))
                        LOOPLABEL
                         (SETQ H (CDR H))
                         (COND ((NULL H) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (H)
                                     (AEVAL
                                      (LIST 'SUB ASSIGNS (LIST 'DF H 'S))))
                                   (CAR H))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
        (SETQ SYMLIST
                (PROG (H FORALL-RESULT FORALL-ENDPTR)
                  (SETQ H (GETRLIST (AEVAL AFBLIST)))
                  (COND ((NULL H) (RETURN (MAKELIST NIL))))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (H) (AEVAL (LIST 'DF H 'S)))
                                    (CAR H))
                                   NIL)))
                 LOOPLABEL
                  (SETQ H (CDR H))
                  (COND ((NULL H) (RETURN (CONS 'LIST FORALL-RESULT))))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (H) (AEVAL (LIST 'DF H 'S))) (CAR H))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ FRSYM (AEVAL (LIST 'LIST)))
        (PROG (FF)
          (SETQ FF (GETRLIST (AEVAL FREEF)))
         LAB
          (COND ((NULL FF) (RETURN NIL)))
          ((LAMBDA (FF)
             (COND
              ((NOT (FREEOF (REVALX SYMLIST) (REVALX FF)))
               (SETQ FRSYM (AEVAL (LIST 'CONS FF FRSYM))))))
           (CAR FF))
          (SETQ FF (CDR FF))
          (GO LAB))
        (SETQ ALL (AEVAL (LIST 'LIST)))
        (SETQ SOL_COUNT2 (AEVAL 0))
        (PROGN
         (ASSGNPRI (AEVAL "All symmetries of solution ") NIL 'FIRST)
         (ASSGNPRI (AEVAL SOL_COUNT) NIL NIL)
         (ASSGNPRI (AEVAL " printed separately:") NIL 'LAST))
        (PROG (FF)
          (SETQ FF (GETRLIST (AEVAL FRSYM)))
         LAB
          (COND ((NULL FF) (RETURN NIL)))
          ((LAMBDA (FF)
             (COND
              ((NOT (FREEOF (REVALX EQLIST) (REVALX FF)))
               (PROGN
                (SETQ EQCP1 (AEVAL EQLIST))
                (PROG (H)
                  (SETQ H (GETRLIST (AEVAL FRSYM)))
                 LAB
                  (COND ((NULL H) (RETURN NIL)))
                  ((LAMBDA (H)
                     (COND
                      ((EVALNEQ (AEVAL H) (AEVAL FF))
                       (SETQ EQCP1
                               (AEVAL (LIST 'SUB (LIST 'EQUAL H 0) EQCP1))))))
                   (CAR H))
                  (SETQ H (CDR H))
                  (GO LAB))
                (SETQ G (AEVAL EQCP1))
                (WHILE
                 (AND (EVALNEQ (AEVAL* G) (AEVAL* (LIST 'LIST)))
                      (FREEOF (REVALX (LIST 'FIRST G)) (REVALX FF)))
                 (SETQ G (AEVAL* (LIST 'REST G))))
                (COND
                 ((EVALNEQ (AEVAL G) (AEVAL (LIST 'LIST)))
                  (PROGN
                   (COND
                    ((BOOLVALUE*
                      (REVALX
                       (LIST 'MY_FREEOF (LIST 'DEN (LIST 'FIRST G)) 'X)))
                     (SETQ H (AEVAL (LIST 'DEN (LIST 'FIRST G)))))
                    (T (SETQ H (AEVAL 1))))
                   (SETQ TM (AEVAL (LIST 'FIRST G)))
                   (SETQ H (AEVAL (LIST 'QUOTIENT H (LIST 'NUMCOEFF TM))))
                   (SETQ EQCP1 (AEVAL (LIST 'SUB (LIST 'EQUAL FF H) EQCP1))))))
                (SETQ G (AEVAL ALL))
                (WHILE
                 (AND (EVALNEQ (AEVAL* G) (AEVAL* (LIST 'LIST)))
                      (NOT
                       (BOOLVALUE*
                        (REVALX (LIST 'SAMELISTS EQCP1 (LIST 'FIRST G))))))
                 (SETQ G (AEVAL* (LIST 'REST G))))
                (COND
                 ((EVALEQUAL (AEVAL G) (AEVAL (LIST 'LIST)))
                  (PROGN
                   (SETQ G (AEVAL (LIST 'LIST)))
                   (SETQ ALL (AEVAL (LIST 'CONS EQCP1 ALL)))
                   (SETQ SOL_COUNT2 (AEVAL (LIST 'PLUS SOL_COUNT2 1)))
                   (SETQ EQCP2 (AEVAL EQCP1))
                   (PROGN
                    (TERPRI)
                    (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
                    (PROGN
                     (PRIN2 "Solution ")
                     (PRIN2 SOL_COUNT)
                     (PRIN2 ", symmetry ")
                     (PRIN2 SOL_COUNT2)
                     (PRIN2 ". :")
                     NIL)
                    (TERPRI)
                    (TERPRI)
                    (COND
                     ((EQUAL (LENGTH AFBLIST) 2)
                      (PROGN (PRIN2 "The equation: ") NIL))
                     (T (PROGN (PRIN2 "The system: ") NIL)))
                    (TERPRI)
                    (COND
                     (HTML_OUT (PROGN (PROGN (PRIN2 "<pre>") NIL) (TERPRI)))))
                   (PROG (H)
                     (SETQ H (GETRLIST (AEVAL AFBLIST)))
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H)
                        (PROGN
                         (SETQ G (AEVAL (LIST 'DF H 'T)))
                         (SETQ MSGBAK (AEVAL '*MSG))
                         (SETK '*MSG (AEVAL 'NIL))
                         (AEVAL (CLEAR (LIST (LIST 'DF H 'T))))
                         (SETK '*MSG (AEVAL MSGBAK))
                         (PROGN
                          (ASSGNPRI (AEVAL (LIST 'DF H 'T)) NIL 'FIRST)
                          (ASSGNPRI (AEVAL "=") NIL NIL)
                          (ASSGNPRI (AEVAL (LIST 'FIRST EQCP1)) NIL 'LAST))
                         (SETK (LIST 'DF H 'T) (AEVAL G))
                         (SETQ EQCP1 (AEVAL (LIST 'REST EQCP1)))))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (PROGN
                    (PROGN
                     (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
                     (TERPRI)
                     (PROGN (PRIN2 "The symmetry: ") NIL)
                     (TERPRI)
                     (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL)))
                     NIL)
                    (PROG (H)
                      (SETQ H (GETRLIST (AEVAL AFBLIST)))
                     LAB
                      (COND ((NULL H) (RETURN NIL)))
                      ((LAMBDA (H)
                         (PROGN
                          (SETQ G (AEVAL (LIST 'DF H 'S)))
                          (SETQ MSGBAK (AEVAL '*MSG))
                          (SETK '*MSG (AEVAL 'NIL))
                          (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))
                          (SETK '*MSG (AEVAL MSGBAK))
                          (PROGN
                           (ASSGNPRI (AEVAL (LIST 'DF H 'S)) NIL 'FIRST)
                           (ASSGNPRI (AEVAL "=") NIL NIL)
                           (ASSGNPRI (AEVAL (LIST 'FIRST EQCP1)) NIL 'LAST))
                          (SETK (LIST 'DF H 'S) (AEVAL G))
                          (SETQ EQCP1 (AEVAL (LIST 'REST EQCP1)))))
                       (CAR H))
                      (SETQ H (CDR H))
                      (GO LAB)))
                   (PROGN
                    (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
                    (TERPRI)
                    (PROGN (PRIN2 "And now in machine readable form: ") NIL)
                    (TERPRI)
                    (COND (HTML_OUT (PROGN (PRIN2 "<p>") NIL)))
                    (COND
                     ((EQUAL (LENGTH AFBLIST) 2)
                      (PROGN (PRIN2 "The equation: ") NIL))
                     (T (PROGN (PRIN2 "The system: ") NIL)))
                    (TERPRI)
                    (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL))))
                   (AEVAL (OFF (LIST 'NAT)))
                   (PROG (H)
                     (SETQ H (GETRLIST (AEVAL AFBLIST)))
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H)
                        (PROGN
                         (PROGN
                          (ASSGNPRI (AEVAL "df(") NIL 'FIRST)
                          (ASSGNPRI (AEVAL H) NIL NIL)
                          (ASSGNPRI (AEVAL ",t)=") NIL NIL)
                          (ASSGNPRI (AEVAL (LIST 'FIRST EQCP2)) NIL 'LAST))
                         (SETQ EQCP2 (AEVAL (LIST 'REST EQCP2)))))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (AEVAL (ON (LIST 'NAT)))
                   (PROGN
                    (PROGN
                     (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
                     (TERPRI)
                     (PROGN (PRIN2 "The symmetry: ") NIL)
                     (TERPRI)
                     (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL))))
                    (AEVAL (OFF (LIST 'NAT)))
                    (PROG (H)
                      (SETQ H (GETRLIST (AEVAL AFBLIST)))
                     LAB
                      (COND ((NULL H) (RETURN NIL)))
                      ((LAMBDA (H)
                         (PROGN
                          (PROGN
                           (ASSGNPRI (AEVAL "df(") NIL 'FIRST)
                           (ASSGNPRI (AEVAL H) NIL NIL)
                           (ASSGNPRI (AEVAL ",s)=") NIL NIL)
                           (ASSGNPRI (AEVAL (LIST 'FIRST EQCP2)) NIL 'LAST))
                          (SETQ EQCP2 (AEVAL (LIST 'REST EQCP2)))))
                       (CAR H))
                      (SETQ H (CDR H))
                      (GO LAB))
                    (AEVAL (ON (LIST 'NAT)))
                    (AEVAL 'NIL)))))))))
           (CAR FF))
          (SETQ FF (CDR FF))
          (GO LAB))
        (COND
         ((EVALGREATERP (AEVAL (LIST 'LENGTH 'SFRSYM)) 1)
          (PROGN
           (PROGN
            (TERPRI)
            (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
            (TERPRI)
            (PROGN
             (PRIN2 "Again solution ")
             (PRIN2 SOL_COUNT)
             (PRIN2 ", now with ALL its parameters (symmetries):")
             NIL)
            (TERPRI)
            (COND
             ((EQUAL (LENGTH AFBLIST) 2) (PROGN (PRIN2 "The equation: ") NIL))
             (T (PROGN (PRIN2 "The system: ") NIL)))
            (TERPRI)
            (COND (HTML_OUT (PROGN (PROGN (PRIN2 "<pre>") NIL) (TERPRI)))))
           (PROG (H)
             (SETQ H (GETRLIST (AEVAL AFBLIST)))
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (PROGN
                 (SETQ G (AEVAL (LIST 'DF H 'T)))
                 (SETQ MSGBAK (AEVAL '*MSG))
                 (SETK '*MSG (AEVAL 'NIL))
                 (AEVAL (CLEAR (LIST (LIST 'DF H 'T))))
                 (SETK '*MSG (AEVAL MSGBAK))
                 (PROGN
                  (ASSGNPRI (AEVAL (LIST 'DF H 'T)) NIL 'FIRST)
                  (ASSGNPRI (AEVAL "=") NIL NIL)
                  (ASSGNPRI (AEVAL (LIST 'SUB ASSIGNS G)) NIL 'LAST))
                 (SETK (LIST 'DF H 'T) (AEVAL G))))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           (PROGN
            (PROGN
             (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
             (TERPRI)
             (PROGN (PRIN2 "The symmetry: ") NIL)
             (TERPRI)
             (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL)))
             NIL)
            (PROG (H)
              (SETQ H (GETRLIST (AEVAL AFBLIST)))
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (PROGN
                  (SETQ G (AEVAL (LIST 'DF H 'S)))
                  (SETQ MSGBAK (AEVAL '*MSG))
                  (SETK '*MSG (AEVAL 'NIL))
                  (AEVAL (CLEAR (LIST (LIST 'DF H 'S))))
                  (SETK '*MSG (AEVAL MSGBAK))
                  (PROGN
                   (ASSGNPRI (AEVAL (LIST 'DF H 'S)) NIL 'FIRST)
                   (ASSGNPRI (AEVAL "=") NIL NIL)
                   (ASSGNPRI (AEVAL (LIST 'SUB ASSIGNS G)) NIL 'LAST))
                  (SETK (LIST 'DF H 'S) (AEVAL G))
                  (AEVAL 'NIL)))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB)))
           (PROGN
            (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
            (TERPRI)
            (PROGN (PRIN2 "And now in machine readable form: ") NIL)
            (TERPRI)
            (COND (HTML_OUT (PROGN (PRIN2 "<p>") NIL)))
            (COND
             ((EQUAL (LENGTH AFBLIST) 2) (PROGN (PRIN2 "The equation: ") NIL))
             (T (PROGN (PRIN2 "The system: ") NIL)))
            (TERPRI)
            (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL))))
           (AEVAL (OFF (LIST 'NAT)))
           (PROG (H)
             (SETQ H (GETRLIST (AEVAL AFBLIST)))
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (PROGN
                 (ASSGNPRI (AEVAL "df(") NIL 'FIRST)
                 (ASSGNPRI (AEVAL H) NIL NIL)
                 (ASSGNPRI (AEVAL ",t)=") NIL NIL)
                 (ASSGNPRI (AEVAL (LIST 'SUB ASSIGNS (LIST 'DF H 'T))) NIL
                           'LAST)))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           (AEVAL (ON (LIST 'NAT)))
           (PROGN
            (PROGN
             (COND (HTML_OUT (PROGN (PRIN2 "</pre>") NIL)))
             (TERPRI)
             (PROGN (PRIN2 "The symmetry: ") NIL)
             (TERPRI)
             (COND (HTML_OUT (PROGN (PRIN2 "<pre>") NIL))))
            (AEVAL (OFF (LIST 'NAT)))
            (PROG (H)
              (SETQ H (GETRLIST (AEVAL AFBLIST)))
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (PROGN
                  (ASSGNPRI (AEVAL "df(") NIL 'FIRST)
                  (ASSGNPRI (AEVAL H) NIL NIL)
                  (ASSGNPRI (AEVAL ",s)=") NIL NIL)
                  (ASSGNPRI (AEVAL (LIST 'SUB ASSIGNS (LIST 'DF H 'S))) NIL
                            'LAST)))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (AEVAL (ON (LIST 'NAT)))
            (AEVAL 'NIL))
           (AEVAL 'NIL))))
        (PROGN
         (PROGN (PRIN2 "-----------------------------------") NIL)
         (TERPRI))
        (AEVAL (CLEARRULES (LIST 'SYMANSATZ)))
        (COND
         ((EVALEQUAL (AEVAL ALLBAK) (AEVAL 'NIL))
          (AEVAL (OFF (LIST 'ALLFAC)))))
        (COND
         ((EVALEQUAL (AEVAL RATBAK) (AEVAL 'NIL))
          (AEVAL (OFF (LIST 'RATIONAL))))))))) 
(PUT 'BIGDPRI 'NUMBER-OF-ARGS 1) 
(PUT 'BIGDPRI 'DEFINED-ON-LINE '1809) 
(PUT 'BIGDPRI 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'BIGDPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BIGDPRI (L)
    (PROG (DD F)
      (COND ((OR (NOT *NAT) *FORT) (RETURN 'FAILED)))
      (SETQ F (CADR L))
      (SETQ DD (CADDR L))
      (PRIN2* '|d|)
      (SETQ YCOORD* (DIFFERENCE YCOORD* 1))
      (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
      (COND ((NEQ N_ 1) (PRIN2* F)))
      (SETQ YCOORD* (PLUS YCOORD* 1))
      (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
      (COND (DD (PROGN (MAPRIN DD) NIL)))
      (RETURN L))) 
(PUT 'D 'PRIFN 'BIGDPRI) 
(PUT 'BIGDPRI 'EXPT 'INBRACKETS) 
(PUT 'MYFPRI 'NUMBER-OF-ARGS 1) 
(PUT 'MYFPRI 'DEFINED-ON-LINE '1836) 
(PUT 'MYFPRI 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'MYFPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYFPRI (X)
    (COND ((OR (NOT *NAT) *FORT) 'FAILED) (T (PROGN (PRIN2* (CAR X)) X)))) 
(PUT 'SEARCH_LI3 'NUMBER-OF-ARGS 2) 
(PUT 'SEARCH_LI3 'DEFINED-ON-LINE '1889) 
(PUT 'SEARCH_LI3 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SEARCH_LI3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEARCH_LI3 (L CARLI)
    (PROG (B RES)
      (COND
       ((PAIRP L)
        (PROGN
         (SETQ B CARLI)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND B (NULL RES))) (RETURN NIL)))
           (COND ((EQUAL (CAR L) (CAR B)) (SETQ RES (LIST L)))
                 (T (SETQ B (CDR B))))
           (GO WHILELABEL))))
       ((AND L (MEMBER L CARLI)) (SETQ RES (LIST L))))
      (COND
       ((NULL RES)
        (PROG ()
         WHILELABEL
          (COND ((NOT (PAIRP L)) (RETURN NIL)))
          (PROGN
           (COND
            ((SETQ B (SEARCH_LI3 (CAR L) CARLI)) (SETQ RES (UNION B RES))))
           (SETQ L (CDR L)))
          (GO WHILELABEL))))
      (RETURN RES))) 
(PUT 'ADD_TERMS 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_TERMS 'DEFINED-ON-LINE '1915) 
(PUT 'ADD_TERMS 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'ADD_TERMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_TERMS (H)
    (PROG (FL NF)
      (SETQ H
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R H)
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R)
                                    (PROGN
                                     (SETQ NF (NEWFCT FNAME_ NIL NFCT_))
                                     (SETQ FL (CONS NF FL))
                                     (SETQ NFCT_ (ADD1 NFCT_))
                                     (LIST 'TIMES NF R)))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (PROGN
                             (SETQ NF (NEWFCT FNAME_ NIL NFCT_))
                             (SETQ FL (CONS NF FL))
                             (SETQ NFCT_ (ADD1 NFCT_))
                             (LIST 'TIMES NF R)))
                          (CAR R))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (LIST
        (COND ((NULL H) 0) ((NULL (CDR H)) (REVAL1 (CAR H) T))
              (T (REVAL1 (CONS 'PLUS H) T)))
        FL)))) 
(PUT 'SSCONLAW 'NUMBER-OF-ARGS 7) 
(FLAG '(SSCONLAW) 'OPFN) 
(PUT 'SSCONLAW 'DEFINED-ON-LINE '1930) 
(PUT 'SSCONLAW 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSCONLAW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SSCONLAW (N TW CW AFWLIST ABWLIST PDES FERMI)
    (PROG (CPU GTI SW FWLIST BWLIST ALLF ALLB G H K L M PT PX PDL FL NF FBNO
           EQN PTCP PXCP PDLCP SOL SPEZSOL QL TERMS TE MINU FACTORS PREFAC FA
           DLI DFLI INTE DELTA DLIREVCP TR_CL PDES2 PDES3 FNO BNO SYLI DENO
           ALLCL NONTRIV FORBID PT1 PX1 PDL1 VERBOSE)
      (AEVAL (LIST 'BACKUP_REDUCE_FLAGS))
      (PROGN
       (SETQ RECORD_HIST NIL)
       (SETQ HOMOGEN_ T)
       (COND (*TIME (PROGN (SETQ CPU (TIME)) (SETQ GTI (GCTIME)))))
       (INPUT_CONSISTENCY_TEST AFWLIST ABWLIST)
       (SETQ N_ N)
       (SETQ FNO (DIFFERENCE (LENGTH AFWLIST) 1))
       (SETQ BNO (DIFFERENCE (LENGTH ABWLIST) 1))
       (SETQ FLIST
               (PROG (G FORALL-RESULT FORALL-ENDPTR)
                 (SETQ G 1)
                 (COND ((MINUSP (DIFFERENCE FNO G)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'F G) NIL)))
                LOOPLABEL
                 (SETQ G (PLUS2 G 1))
                 (COND ((MINUSP (DIFFERENCE FNO G)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'F G) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (COND
        ((AND FLIST (EQUAL (LENGTH FLIST) 1) (EQUAL TR_CL NIL))
         (PUT 'F 'PRIFN 'MYFPRI))
        (T (PUT 'F 'PRIFN NIL)))
       (SETQ BLIST
               (PROG (G FORALL-RESULT FORALL-ENDPTR)
                 (SETQ G 1)
                 (COND ((MINUSP (DIFFERENCE BNO G)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LIST 'B G) NIL)))
                LOOPLABEL
                 (SETQ G (PLUS2 G 1))
                 (COND ((MINUSP (DIFFERENCE BNO G)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LIST 'B G) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (COND
        ((AND BLIST (EQUAL (LENGTH BLIST) 1) (EQUAL TR_CL NIL))
         (PUT 'B 'PRIFN 'MYFPRI))
        (T (PUT 'B 'PRIFN NIL)))
       (SETQ FBLIST (APPEND FLIST BLIST)))
      (SETK 'AFBLIST (AEVAL (CONS 'LIST FBLIST)))
      (PROG (G)
        (SETQ G (GETRLIST (AEVAL 'AFBLIST)))
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G) (AEVAL (DEPEND (LIST G 'X 'T)))) (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (PROGN
       (PROG (G)
         (SETQ G (CDR PDES))
        LAB
         (COND ((NULL G) (RETURN NIL)))
         ((LAMBDA (G)
            (PROGN
             (COND
              ((IS_FERMION (CADR G))
               (PROGN (SETQ FNO (ADD1 FNO)) (SETQ H (LIST 'F FNO))))
              (T (PROGN (SETQ BNO (ADD1 BNO)) (SETQ H (LIST 'B BNO)))))
             (AEVAL (DEPEND (LIST H 'X 'T)))
             (SETQ PDES2
                     (CONS (LIST 'REPLACEBY (CADR G) (LIST 'PLUS (CADDR G) H))
                           PDES2))
             (SETQ PDES3
                     (CONS (LIST H 0 (LIST 'DIFFERENCE (CADR G) (CADDR G)))
                           PDES3))
             (SETQ SYLI (CONS H SYLI))))
          (CAR G))
         (SETQ G (CDR G))
         (GO LAB))
       (SETQ FORBID (NON_T_LHS_DVS (CDR PDES)))
       (SETQ PDES2 (CONS 'LIST PDES2))
       (COND
        (TR_CL
         (PROGN
          (PROGN
           (ASSGNPRI (AEVAL "pdes2=") NIL 'FIRST)
           (ASSGNPRI (AEVAL PDES2) NIL 'LAST))
          (PROGN (PRIN2 "pdes3=") NIL)
          (PRETTYPRINT PDES3)
          (TERPRI)
          (PROGN (PRIN2 "syli=") (PRIN2 SYLI) NIL)
          (TERPRI))))
       (COND
        (FNAME_LIST
         (PROGN
          (SETQ FNAME_ (CAR FNAME_LIST))
          (SETQ FNAME_LIST (CDR FNAME_LIST))))
        (T (SETQ FNAME_ 'R)))
       (SETQ FWLIST (CDR (REVAL1 AFWLIST T)))
       (SETQ BWLIST (CDR (REVAL1 ABWLIST T)))
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND ((MINUSP (DIFFERENCE (LENGTH FLIST) I)) (RETURN NIL)))
         (SETQ ALLF (CONS (LIST (NTH FWLIST I) (NTH FLIST I)) ALLF))
         (SETQ I (PLUS2 I 1))
         (GO LAB))
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND ((MINUSP (DIFFERENCE (LENGTH BLIST) I)) (RETURN NIL)))
         (SETQ ALLB (CONS (LIST (NTH BWLIST I) (NTH BLIST I)) ALLB))
         (SETQ I (PLUS2 I 1))
         (GO LAB))
       (SETQ NFCT_ 1)
       (SETQ PRINT_ NIL)
       (SETQ H
               (RHS_TERM_LIST N ALLF ALLB (DIFFERENCE CW TW) NIL FORBID
                VERBOSE))
       (COND (FERMI (SETQ H (CAR H))) (T (SETQ H (CADR H))))
       (SETQ H (ADD_TERMS H))
       (SETQ PT (CAR H))
       (SETQ FL (APPEND (CADR H) FL))
       (SETQ EQN (LIST (LIST 'DF PT 'T)))
       (COND
        ((EQUAL N 0)
         (PROGN
          (SETQ H
                  (RHS_TERM_LIST N ALLF ALLB (DIFFERENCE CW 2) NIL FORBID
                   VERBOSE))
          (COND (FERMI (SETQ H (CAR H))) (T (SETQ H (CADR H))))
          (SETQ H (ADD_TERMS H))
          (SETQ PX (CAR H))
          (SETQ FL (APPEND (CADR H) FL))
          (SETQ EQN (CONS (LIST 'DF PX 'X) EQN))
          (SETQ PDL NIL)
          NIL))
        (T
         (PROGN
          (SETQ PX 0)
          (SETQ H
                  (RHS_TERM_LIST N ALLF ALLB (DIFFERENCE CW 1) NIL FORBID
                   VERBOSE))
          (COND
           (*T_CHANGES_PARITY
            (COND (FERMI (SETQ H (CAR H))) (T (SETQ H (CADR H)))))
           (FERMI (SETQ H (CADR H))) (T (SETQ H (CAR H))))
          (SETQ PDL NIL)
          (PROG (G)
            (SETQ G N)
           LAB
            (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 G))) (RETURN NIL)))
            (PROGN
             (SETQ K (ADD_TERMS H))
             (SETQ PDL (CONS (LIST 'LIST G (CAR K)) PDL))
             (SETQ FL (APPEND (CADR K) FL))
             (SETQ EQN (CONS (LIST 'D G (CAR K)) EQN))
             NIL)
            (SETQ G (PLUS2 G (MINUS 1)))
            (GO LAB)))))
       (SETQ EQN (CONS 'PLUS EQN))
       (SETQ PDL (CONS 'LIST PDL))
       (SETQ FL (CONS 'LIST FL)))
      (COND
       ((BOOLVALUE* TR_CL)
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "Ansatz for Pt: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL PT) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "Ansatz for Px: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL PX) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "Ansatz for Pdl: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL PDL) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "list of unknown coefficients: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL FL) NIL 'LAST))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL FL) (AEVAL (LIST 'LIST)))
        (RETURN
         (PROGN
          (ASSGNPRI (AEVAL "No valid ansatz in this case.") NIL 'ONLY)
          (AEVAL 'NIL)))))
      (AEVAL (LET (LIST PDES)))
      (SETQ EQN (AEVAL EQN))
      (AEVAL (CLEARRULES (LIST PDES)))
      (PROGN
       (SETQ H (SEARCH_LI3 (REVAL1 EQN T) (LIST 'DF 'F 'B 'D 'X 'T)))
       (COND
        (*TIME
         (PROGN
          (TERPRI)
          (PROGN
           (PRIN2 "CPU time so far: ")
           (PRIN2 (DIFFERENCE (TIME) CPU))
           (PRIN2 " ms  ")
           (PRIN2 " GC time so far: ")
           (PRIN2 (DIFFERENCE (GCTIME) GTI))
           (PRIN2 " ms")
           NIL)
          (TERPRI)
          NIL)))
       (SETQ K (SETKORDER H))
       (SETQ EQN
               (CONS 'LIST
                     (PROG (G FORALL-RESULT FORALL-ENDPTR)
                       (SETQ G
                               (ITERCOEFF
                                (COND ((NOT (PAIRP EQN)) EQN)
                                      ((EQUAL (CAR EQN) '*SQ)
                                       (REORDER (CAR (CADR EQN))))
                                      (T (CAR (SIMP EQN))))
                                H))
                       (COND ((NULL G) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (G) (LIST '*SQ (CONS G 1) T))
                                         (CAR G))
                                        NIL)))
                      LOOPLABEL
                       (SETQ G (CDR G))
                       (COND ((NULL G) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (G) (LIST '*SQ (CONS G 1) T)) (CAR G))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
       (SETKORDER K)
       (SETQ !ARBINT 0)
       (TERPRI)
       (PROGN
        (PRIN2 (LENGTH (CDR EQN)))
        (PRIN2 " conditions for ")
        (PRIN2 (LENGTH (CDR FL)))
        (PRIN2 " unknowns")
        NIL)
       (TERPRI))
      (SETQ SOL (AEVAL (LIST 'FIRST (LIST 'SOLVE EQN FL))))
      (PROGN
       (PROGN
        (PRIN2 (COND ((EQUAL 0 !ARBINT) "No") (T !ARBINT)))
        (PRIN2 (COND ((NEQ 0 !ARBINT) " (possibly trivial) ") (T " ")))
        (PRIN2 (COND (FERMI "fermionic") (T "bosonic")))
        (PRIN2 " conservation ")
        (PRIN2 (COND ((EQUAL 1 !ARBINT) "law") (T "laws")))
        (PRIN2 " of weight ")
        (PRIN2 CW)
        (PRIN2 (COND ((EQUAL 0 !ARBINT) ".") (T ":")))
        NIL)
       (TERPRI)
       NIL)
      (SETQ NONTRIV (AEVAL 0))
      (PROG (G)
        (SETQ G 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) G)) (RETURN NIL)))
        (PROGN
         (SETQ SPEZSOL
                 (AEVAL* (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX G) 1) SOL)))
         (PROG (H)
           (SETQ H 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) H)) (RETURN NIL)))
           (COND
            ((EVALNEQ (AEVAL* H) (AEVAL* G))
             (SETQ SPEZSOL
                     (AEVAL*
                      (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX H) 0)
                            SPEZSOL)))))
           (SETQ H
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    H))
           (GO LAB))
         (SETQ PTCP (AEVAL* (LIST 'SUB SPEZSOL PT)))
         (SETQ PXCP (AEVAL* (LIST 'SUB SPEZSOL PX)))
         (SETQ PDLCP (AEVAL* (LIST 'SUB SPEZSOL PDL)))
         (COND ((EVALEQUAL (AEVAL* PTCP) 0) (SETQ K (AEVAL* 0)))
               (T (SETQ K (AEVAL* 1))))
         (COND ((EVALNEQ (AEVAL* PXCP) 0) (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
         (SETQ H (AEVAL* PDLCP))
         (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                (PROGN
                 (COND
                  ((EVALNEQ (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) 0)
                   (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
                 (SETQ H (AEVAL* (LIST 'REST H)))))
         (COND
          ((EVALEQUAL (AEVAL* K) 0)
           (PROGN
            (ASSGNPRI (AEVAL* "We drop a conservation law with Pt, Px, Pd[i] ")
                      NIL 'FIRST)
            (ASSGNPRI (AEVAL* "all being zero") NIL 'LAST)))
          ((EVALEQUAL (AEVAL* K) 1)
           (PROGN
            (ASSGNPRI (AEVAL* "We drop a conservation law with only ") NIL
                      'FIRST)
            (ASSGNPRI
             (COND ((EVALNEQ (AEVAL* PTCP) 0) (AEVAL* "Pt"))
                   (T (AEVAL* "one Pd[i]")))
             NIL NIL)
            (ASSGNPRI (AEVAL* " nonzero.") NIL 'LAST)))
          (T
           (PROGN
            (SETQ PT1 (AEVAL* PTCP))
            (SETQ PX1 (AEVAL* PXCP))
            (SETQ PDL1 (AEVAL* PDLCP))
            (COND
             ((BOOLVALUE* TR_CL)
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL* ">>>>> ") NIL 'FIRST)
                (ASSGNPRI (AEVAL* G) NIL NIL)
                (ASSGNPRI (AEVAL* ". Conservation law: ") NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL* "Pt = ") NIL 'FIRST)
                (ASSGNPRI (AEVAL* PTCP) NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL* "Px = ") NIL 'FIRST)
                (ASSGNPRI (AEVAL* PXCP) NIL 'LAST))
               (SETQ H (AEVAL* PDLCP))
               (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                      (PROGN
                       (PROGN
                        (ASSGNPRI (AEVAL* "Pd[") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* (LIST 'FIRST (LIST 'FIRST H))) NIL
                                  NIL)
                        (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                        (ASSGNPRI (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) NIL
                                  'LAST))
                       (SETQ H (AEVAL* (LIST 'REST H))))))))
            (SETQ EQN
                    (AEVAL*
                     (LIST 'PLUS (LIST 'DF PTCP 'T) (LIST 'DF PXCP 'X)
                           (PROG (L FORALL-RESULT)
                             (SETQ L 1)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) L))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (PROGN
                                             (SETQ M
                                                     (AEVAL*
                                                      (LIST 'FIRST
                                                            (LIST 'PART PDLCP
                                                                  L))))
                                             (AEVAL*
                                              (LIST 'D M
                                                    (LIST 'SECOND
                                                          (LIST 'PART PDLCP
                                                                L)))))
                                            FORALL-RESULT)))
                             (SETQ L
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      L))
                             (GO LAB1)))))
            (COND
             ((BOOLVALUE* TR_CL)
              (PROGN
               (ASSGNPRI (AEVAL* "rhs = ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* EQN) NIL 'LAST))))
            (COND ((EVALEQUAL (AEVAL* EQN) 0) (AEVAL* 'NIL))
                  (T
                   (PROGN
                    (SETQ QL PDES3)
                    (SETQ PDES2 (ADD_DF_RULES_TO_D_RULE PDES2))
                    (AEVAL* (LET (LIST PDES2)))
                    (SETQ EQN (REVAL1 EQN T))
                    (AEVAL* (CLEARRULES (LIST PDES2)))
                    (COND
                     (TR_CL
                      (PROGN
                       (PROGN (PRIN2 "rhs in terms of equations = ") NIL)
                       (PRETTYPRINT EQN)
                       (TERPRI))))
                    (SETQ DENO NIL)
                    (COND
                     ((AND (PAIRP EQN) (EQUAL (CAR EQN) 'QUOTIENT))
                      (COND
                       ((IS_CONST (CADDR EQN))
                        (PROGN (SETQ DENO (CADDR EQN)) (SETQ EQN (CADR EQN))))
                       (T (REDERR " Something is wrong!! (0) *****")))))
                    (COND
                     ((OR (NOT (PAIRP EQN)) (NEQ (CAR EQN) 'PLUS))
                      (SETQ TERMS (LIST EQN)))
                     (T (SETQ TERMS (CDR EQN))))
                    (PROG (TE)
                      (SETQ TE TERMS)
                     LAB
                      (COND ((NULL TE) (RETURN NIL)))
                      ((LAMBDA (TE)
                         (PROGN
                          (SETQ MINU NIL)
                          (COND
                           ((AND (PAIRP TE) (EQUAL (CAR TE) 'MINUS))
                            (PROGN (SETQ MINU T) (SETQ TE (CADR TE)))))
                          (COND
                           ((OR (NOT (PAIRP TE)) (NEQ (CAR TE) 'TIMES))
                            (SETQ FACTORS (LIST TE)))
                           (T (SETQ FACTORS (CDR TE))))
                          (SETQ PREFAC NIL)
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT FACTORS) (RETURN NIL)))
                            (PROGN
                             (SETQ FA (CAR FACTORS))
                             (SETQ FACTORS (CDR FACTORS))
                             (COND
                              ((FREEOFLIST FA SYLI)
                               (SETQ PREFAC (CONS FA PREFAC)))
                              (T
                               (PROGN
                                (COND
                                 (TR_CL
                                  (PROGN
                                   (PROGN
                                    (PRIN2
                                     " Next term to be partially integrated:")
                                    NIL)
                                   (TERPRI)
                                   (PRETTYPRINT PREFAC)
                                   (TERPRI)
                                   (PROGN (PRIN2 " times ") NIL)
                                   (TERPRI)
                                   (PRETTYPRINT FA)
                                   (TERPRI)
                                   (PROGN (PRIN2 " times ") NIL)
                                   (TERPRI)
                                   (PRETTYPRINT FACTORS)
                                   (TERPRI)
                                   (PROGN (PRIN2 "minu=") (PRIN2 MINU) NIL)
                                   (TERPRI))))
                                (COND
                                 ((AND (PAIRP FA) (EQUAL (CAR FA) 'EXPT))
                                  (PROGN
                                   (SETQ FACTORS
                                           (CONS
                                            (COND
                                             ((EQUAL (CADDR FA) 2) (CADR FA))
                                             (T
                                              (LIST 'EXPT (CADR FA)
                                                    (SUB1 (CADDR FA)))))
                                            FACTORS))
                                   (SETQ FA (CADR FA)))))
                                (COND ((NULL FACTORS) (SETQ FACTORS 1))
                                      ((NULL (CDR FACTORS))
                                       (SETQ FACTORS (CAR FACTORS)))
                                      (T (SETQ FACTORS (CONS 'TIMES FACTORS))))
                                (COND ((NULL PREFAC) (SETQ PREFAC 1))
                                      ((NULL (CDR PREFAC))
                                       (SETQ PREFAC (CAR PREFAC)))
                                      (T
                                       (SETQ PREFAC
                                               (CONS 'TIMES
                                                     (REVERSE PREFAC)))))
                                (COND
                                 ((AND (IS_FERMION FA) (IS_FERMION FACTORS))
                                  (SETQ MINU (NOT MINU))))
                                (SETQ PREFAC (LIST 'TIMES PREFAC FACTORS))
                                (SETQ FACTORS NIL)
                                (COND
                                 (MINU (SETQ PREFAC (LIST 'MINUS PREFAC))))
                                (SETQ MINU NIL)
                                (SETQ DLI NIL)
                                (PROG ()
                                 WHILELABEL
                                  (COND
                                   ((NOT (AND (PAIRP FA) (EQUAL (CAR FA) 'D)))
                                    (RETURN NIL)))
                                  (PROGN
                                   (SETQ DLI (CONS (CADR FA) DLI))
                                   (SETQ FA (CADDR FA)))
                                  (GO WHILELABEL))
                                (SETQ DLI (REVERSE DLI))
                                (COND
                                 ((AND (PAIRP FA) (EQUAL (CAR FA) 'DF))
                                  (PROGN
                                   (SETQ DELTA (CADR FA))
                                   (SETQ DFLI (CDDR FA))
                                   (SETQ H NIL)
                                   (PROG ()
                                    WHILELABEL
                                     (COND ((NOT DFLI) (RETURN NIL)))
                                     (PROGN
                                      (COND
                                       ((FIXP (CAR DFLI))
                                        (PROG (K)
                                          (SETQ K 2)
                                         LAB
                                          (COND
                                           ((MINUSP (DIFFERENCE (CAR DFLI) K))
                                            (RETURN NIL)))
                                          (SETQ H (CONS (CAR H) H))
                                          (SETQ K (PLUS2 K 1))
                                          (GO LAB)))
                                       (T (SETQ H (CONS (CAR DFLI) H))))
                                      (SETQ DFLI (CDR DFLI)))
                                     (GO WHILELABEL))
                                   (SETQ DFLI (REVERSE H))))
                                 (T (PROGN (SETQ DELTA FA) (SETQ DFLI NIL))))
                                (SETQ K (ASSOC DELTA QL))
                                (SETQ QL (DELETE K QL))
                                (SETQ DELTA (CADDR K))
                                (COND
                                 (DENO
                                  (SETQ DELTA (LIST 'QUOTIENT DELTA DENO))))
                                (COND
                                 (TR_CL
                                  (PROGN
                                   (PROGN
                                    (PRIN2 "delta=")
                                    (PRIN2 DELTA)
                                    (PRIN2 "  dfli=")
                                    (PRIN2 DFLI)
                                    NIL)
                                   (TERPRI))))
                                (PROG ()
                                 WHILELABEL
                                  (COND ((NOT DLI) (RETURN NIL)))
                                  (PROGN
                                   (SETQ H (CAR DLI))
                                   (SETQ DLI (CDR DLI))
                                   (COND
                                    ((IS_FERMION PREFAC)
                                     (SETQ PREFAC (LIST 'MINUS PREFAC))))
                                   (SETQ INTE DELTA)
                                   (COND
                                    (DFLI
                                     (SETQ INTE (CONS 'DF (CONS INTE DFLI)))))
                                   (SETQ DLIREVCP (REVERSE DLI))
                                   (PROG ()
                                    WHILELABEL
                                     (COND ((NOT DLIREVCP) (RETURN NIL)))
                                     (PROGN
                                      (SETQ INTE (LIST 'D (CAR DLIREVCP) INTE))
                                      (SETQ DLIREVCP (CDR DLIREVCP)))
                                     (GO WHILELABEL))
                                   (COND
                                    ((BOOLVALUE* TR_CL)
                                     (PROGN
                                      (ASSGNPRI (AEVAL* "Pdlcp before = ") NIL
                                                'FIRST)
                                      (ASSGNPRI (AEVAL* PDLCP) NIL 'LAST))))
                                   (SETQ PDLCP
                                           (AEVAL*
                                            (LIST 'SETPART* PDLCP H
                                                  (AEVAL*
                                                   (LIST 'LIST
                                                         (LIST 'FIRST
                                                               (LIST 'PART
                                                                     PDLCP H))
                                                         (LIST 'DIFFERENCE
                                                               (LIST 'SECOND
                                                                     (LIST
                                                                      'PART
                                                                      PDLCP H))
                                                               (LIST 'TIMES
                                                                     PREFAC
                                                                     INTE)))))))
                                   (COND
                                    ((BOOLVALUE* TR_CL)
                                     (PROGN
                                      (ASSGNPRI (AEVAL* "Pdlcp after = ") NIL
                                                'FIRST)
                                      (ASSGNPRI (AEVAL* PDLCP) NIL 'LAST))))
                                   (SETQ PREFAC
                                           (REVAL1
                                            (LIST 'MINUS (LIST 'D H PREFAC))
                                            T)))
                                  (GO WHILELABEL))
                                (PROG ()
                                 WHILELABEL
                                  (COND
                                   ((NOT (AND DFLI (NOT (ZEROP PREFAC))))
                                    (RETURN NIL)))
                                  (PROGN
                                   (COND
                                    (TR_CL
                                     (PROGN
                                      (PROGN
                                       (PRIN2 "Partial ")
                                       (PRIN2 (CAR DFLI))
                                       (PRIN2 "-integration:")
                                       NIL)
                                      (TERPRI))))
                                   (SETQ INTE DELTA)
                                   (COND
                                    ((CDR DFLI)
                                     (SETQ INTE
                                             (CONS 'DF
                                                   (CONS INTE (CDR DFLI))))))
                                   (COND
                                    ((EQUAL (CAR DFLI) 'X)
                                     (PROGN
                                      (COND
                                       ((BOOLVALUE* TR_CL)
                                        (PROGN
                                         (ASSGNPRI (AEVAL* "Pxcp before = ")
                                                   NIL 'FIRST)
                                         (ASSGNPRI (AEVAL* PXCP) NIL 'LAST))))
                                      (SETQ PXCP
                                              (LIST 'DIFFERENCE PXCP
                                                    (LIST 'TIMES PREFAC INTE)))
                                      (COND
                                       ((BOOLVALUE* TR_CL)
                                        (PROGN
                                         (ASSGNPRI (AEVAL* "Pxcp after = ") NIL
                                                   'FIRST)
                                         (ASSGNPRI (AEVAL* PXCP) NIL 'LAST))))
                                      NIL))
                                    (T
                                     (PROGN
                                      (COND
                                       ((BOOLVALUE* TR_CL)
                                        (PROGN
                                         (ASSGNPRI (AEVAL* "Ptcp before = ")
                                                   NIL 'FIRST)
                                         (ASSGNPRI (AEVAL* PTCP) NIL 'LAST))))
                                      (SETQ PTCP
                                              (LIST 'DIFFERENCE PTCP
                                                    (LIST 'TIMES PREFAC INTE)))
                                      (COND
                                       ((BOOLVALUE* TR_CL)
                                        (PROGN
                                         (ASSGNPRI (AEVAL* "Ptcp after = ") NIL
                                                   'FIRST)
                                         (ASSGNPRI (AEVAL* PTCP) NIL 'LAST))))
                                      NIL)))
                                   (SETQ PREFAC
                                           (REVAL1
                                            (LIST 'MINUS
                                                  (LIST 'DF PREFAC (CAR DFLI)))
                                            T))
                                   (SETQ DFLI (CDR DFLI)))
                                  (GO WHILELABEL))
                                (COND
                                 (DENO
                                  (SETQ PREFAC (LIST 'QUOTIENT PREFAC DENO))))
                                (SETQ QL
                                        (CONS
                                         (LIST (CAR K)
                                               (LIST 'PLUS PREFAC (CADR K))
                                               (CADDR K))
                                         QL))
                                NIL))))
                            (GO WHILELABEL))))
                       (CAR TE))
                      (SETQ TE (CDR TE))
                      (GO LAB))
                    (SETQ NF
                            (CONS 'LIST
                                  (PROG (G FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ G PDES3)
                                    (COND ((NULL G) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (G)
                                                        (LIST 'EQUAL (CAR G)
                                                              (CADDR G)))
                                                      (CAR G))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ G (CDR G))
                                    (COND ((NULL G) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (G)
                                                (LIST 'EQUAL (CAR G)
                                                      (CADDR G)))
                                              (CAR G))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                    (SETQ QL
                            (PROG (H FORALL-RESULT FORALL-ENDPTR)
                              (SETQ H QL)
                              (COND ((NULL H) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (H)
                                                  (LIST (CAR H)
                                                        (AEVAL*
                                                         (LIST 'SUB NF
                                                               (CADR H)))
                                                        (CADDR H)))
                                                (CAR H))
                                               NIL)))
                             LOOPLABEL
                              (SETQ H (CDR H))
                              (COND ((NULL H) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (H)
                                          (LIST (CAR H)
                                                (AEVAL*
                                                 (LIST 'SUB NF (CADR H)))
                                                (CADDR H)))
                                        (CAR H))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    (SETQ H QL)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT (AND H (ZEROP (CADAR H)))) (RETURN NIL)))
                      (SETQ H (CDR H))
                      (GO WHILELABEL))
                    (COND
                     (TR_CL
                      (PROGN
                       (COND ((NULL H) (PROGN (PRIN2 "CL is TRIVIAL!") NIL))
                             (T (PROGN (PRIN2 "CL is GENUINE!") NIL)))
                       (TERPRI)
                       NIL)))
                    (COND
                     (H
                      (PROGN
                       (SETQ PTCP (AEVAL* (LIST 'SUB NF PTCP)))
                       (SETQ PXCP (AEVAL* (LIST 'SUB NF PXCP)))
                       (COND
                        ((EVALGREATERP (AEVAL* N) 0)
                         (PROGN
                          (SETQ M (AEVAL* (LIST 'FIRST (LIST 'FIRST PDLCP))))
                          (COND
                           ((BOOLVALUE* TR_CL)
                            (PROGN
                             (ASSGNPRI (AEVAL* "We add Pxcp=") NIL 'FIRST)
                             (ASSGNPRI (AEVAL* PXCP) NIL NIL)
                             (ASSGNPRI (AEVAL* " to Pd[") NIL NIL)
                             (ASSGNPRI (AEVAL* M) NIL NIL)
                             (ASSGNPRI (AEVAL* "].") NIL 'LAST))))
                          (SETQ PDLCP
                                  (AEVAL*
                                   (LIST 'CONS
                                         (LIST 'LIST M
                                               (LIST 'PLUS (LIST 'D M PXCP)
                                                     (LIST 'SECOND
                                                           (LIST 'FIRST
                                                                 PDLCP))))
                                         (LIST 'REST PDLCP))))
                          (SETQ PXCP (AEVAL* 0))
                          (SETQ PDLCP (AEVAL* (LIST 'SUB NF PDLCP)))
                          (AEVAL* 'NIL))))
                       (SETQ H
                               (COND
                                ((EQUAL (LENGTH QL) 1)
                                 (LIST 'TIMES (CADAR QL) (CADDAR QL)))
                                (T
                                 (CONS 'PLUS
                                       (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ H QL)
                                         (COND ((NULL H) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (H)
                                                             (LIST 'TIMES
                                                                   (CADR H)
                                                                   (CADDR H)))
                                                           (CAR H))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ H (CDR H))
                                         (COND
                                          ((NULL H) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (H)
                                                     (LIST 'TIMES (CADR H)
                                                           (CADDR H)))
                                                   (CAR H))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL))))))
                       (SETQ K
                               (AEVAL*
                                (LIST 'PLUS (LIST 'DF PTCP 'T)
                                      (LIST 'DIFFERENCE (LIST 'DF PXCP 'X) H)
                                      (PROG (G FORALL-RESULT)
                                        (SETQ G 1)
                                        (SETQ FORALL-RESULT 0)
                                       LAB1
                                        (COND
                                         ((|AMINUSP:|
                                           (LIST 'DIFFERENCE (AEVAL* N) G))
                                          (RETURN FORALL-RESULT)))
                                        (SETQ FORALL-RESULT
                                                (AEVAL*
                                                 (LIST 'PLUS
                                                       (PROGN
                                                        (SETQ M
                                                                (AEVAL*
                                                                 (LIST 'FIRST
                                                                       (LIST
                                                                        'PART
                                                                        PDLCP
                                                                        G))))
                                                        (AEVAL*
                                                         (LIST 'D M
                                                               (LIST 'SECOND
                                                                     (LIST
                                                                      'PART
                                                                      PDLCP
                                                                      G)))))
                                                       FORALL-RESULT)))
                                        (SETQ G
                                                ((LAMBDA (FORALL-RESULT)
                                                   (AEVAL*
                                                    (LIST 'PLUS FORALL-RESULT
                                                          1)))
                                                 G))
                                        (GO LAB1)))))
                       (SETQ QL
                               (CONS 'LIST
                                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ H QL)
                                       (COND ((NULL H) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (H)
                                                           (LIST 'LIST
                                                                 (CADDR H)
                                                                 (CADR H)))
                                                         (CAR H))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ H (CDR H))
                                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (H)
                                                   (LIST 'LIST (CADDR H)
                                                         (CADR H)))
                                                 (CAR H))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                       (COND
                        (T
                         (PROGN
                          (PROGN
                           (SETQ NONTRIV (ADD1 NONTRIV))
                           (TERPRI)
                           (PROGN
                            (PRIN2 ">>>>> ")
                            (PRIN2 NONTRIV)
                            (PRIN2 ". Non-trivial conservation law: ")
                            NIL)
                           (TERPRI)
                           (TERPRI)
                           (PROGN
                            (PRIN2
                             "At first in the form of a conserved current vanishing mod eqn.s:")
                            NIL)
                           (TERPRI)
                           (TERPRI)
                           NIL)
                          (PROGN
                           (ASSGNPRI (AEVAL* "Pt = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PT1) NIL 'LAST))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Px = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PX1) NIL 'LAST))
                          (SETQ H (AEVAL* PDL1))
                          (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "Pd[") NIL 'FIRST)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'FIRST (LIST 'FIRST H))) NIL
                                    NIL)
                                   (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) NIL
                                    'LAST))
                                  (SETQ H (AEVAL* (LIST 'REST H)))))
                          (ASSGNPRI
                           (AEVAL* "----- and in machine readable form:") NIL
                           'ONLY)
                          (AEVAL* (OFF (LIST 'NAT)))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Pt = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PT1) NIL 'LAST))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Px = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PX1) NIL 'LAST))
                          (SETQ H (AEVAL* PDL1))
                          (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "Pd[") NIL 'FIRST)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'FIRST (LIST 'FIRST H))) NIL
                                    NIL)
                                   (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) NIL
                                    'LAST))
                                  (SETQ H (AEVAL* (LIST 'REST H)))))
                          (AEVAL* (ON (LIST 'NAT)))
                          (ASSGNPRI
                           (AEVAL*
                            "Now as conserved current with characteristic functions:")
                           NIL 'ONLY)
                          (PROGN
                           (ASSGNPRI (AEVAL* "Pt = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PTCP) NIL 'LAST))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Px = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PXCP) NIL 'LAST))
                          (SETQ H (AEVAL* PDLCP))
                          (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "Pd[") NIL 'FIRST)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'FIRST (LIST 'FIRST H))) NIL
                                    NIL)
                                   (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) NIL
                                    'LAST))
                                  (SETQ H (AEVAL* (LIST 'REST H)))))
                          (PROG (H)
                            (SETQ H (GETRLIST (AEVAL* QL)))
                           LAB
                            (COND ((NULL H) (RETURN NIL)))
                            ((LAMBDA (H)
                               (PROGN
                                (ASSGNPRI (AEVAL* "Q[") NIL 'FIRST)
                                (ASSGNPRI (AEVAL* (LIST 'FIRST H)) NIL NIL)
                                (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                (ASSGNPRI (AEVAL* (LIST 'SECOND H)) NIL
                                          'LAST)))
                             (CAR H))
                            (SETQ H (CDR H))
                            (GO LAB))
                          (ASSGNPRI
                           (AEVAL* "----- and in machine readable form:") NIL
                           'ONLY)
                          (AEVAL* (OFF (LIST 'NAT)))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Pt = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PTCP) NIL 'LAST))
                          (PROGN
                           (ASSGNPRI (AEVAL* "Px = ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* PXCP) NIL 'LAST))
                          (SETQ H (AEVAL* PDLCP))
                          (WHILE (EVALNEQ (AEVAL* H) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "Pd[") NIL 'FIRST)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'FIRST (LIST 'FIRST H))) NIL
                                    NIL)
                                   (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                   (ASSGNPRI
                                    (AEVAL* (LIST 'SECOND (LIST 'FIRST H))) NIL
                                    'LAST))
                                  (SETQ H (AEVAL* (LIST 'REST H)))))
                          (PROG (H)
                            (SETQ H (GETRLIST (AEVAL* QL)))
                           LAB
                            (COND ((NULL H) (RETURN NIL)))
                            ((LAMBDA (H)
                               (PROGN
                                (ASSGNPRI (AEVAL* "Q[") NIL 'FIRST)
                                (ASSGNPRI (AEVAL* (LIST 'FIRST H)) NIL NIL)
                                (ASSGNPRI (AEVAL* "] = ") NIL NIL)
                                (ASSGNPRI (AEVAL* (LIST 'SECOND H)) NIL
                                          'LAST)))
                             (CAR H))
                            (SETQ H (CDR H))
                            (GO LAB))
                          (AEVAL* (ON (LIST 'NAT)))
                          (AEVAL* 'NIL))))
                       (COND
                        ((EVALNEQ (AEVAL* K) 0)
                         (PROGN
                          (ASSGNPRI (AEVAL* "***** ERROR: ") NIL 'ONLY)
                          (PROGN
                           (ASSGNPRI (AEVAL* " 0 <> ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* K) NIL 'LAST))
                          (AEVAL*
                           (REDERR
                            (REVALX " A test shows a contradiction! *****")))
                          (AEVAL* 'NIL))))))))))))))
        (SETQ G
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 G))
        (GO LAB)))) 
(PUT 'SSCONL 'NUMBER-OF-ARGS 7) 
(FLAG '(SSCONL) 'OPFN) 
(PUT 'SSCONL 'DEFINED-ON-LINE '2389) 
(PUT 'SSCONL 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSCONL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SSCONL (N TW MINCW MAXCW AFWLIST ABWLIST PDES)
    (PROGN
     (PROGN
      (TERPRI)
      (PROGN
       (PRIN2 "##### ssconl: This is the case N")
       (PRIN2 N)
       (PRIN2 "f")
       (PRIN2 (LENGTH (CDR AFWLIST)))
       (PRIN2 "b")
       (PRIN2 (LENGTH (CDR ABWLIST)))
       (PRIN2 "t")
       (PRIN2 TW)
       (PRIN2 "w")
       NIL)
      (PROG (G)
        (SETQ G (APPEND (CDR AFWLIST) (CDR ABWLIST)))
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G) (PROGN (PRIN2 G) NIL)) (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (PROGN (PRIN2 ". #####") NIL)
      (TERPRI)
      NIL)
     (PROGN
      (ASSGNPRI (AEVAL "The ") NIL 'FIRST)
      (ASSGNPRI
       (COND ((EVALEQUAL (AEVAL (LIST 'LENGTH PDES)) 1) (AEVAL "equation"))
             (T (AEVAL "system")))
       NIL NIL)
      (ASSGNPRI (AEVAL " to be investigated:") NIL 'LAST))
     (PROG (H)
       (SETQ H (GETRLIST (AEVAL PDES)))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H) (ASSGNPRI (AEVAL H) NIL 'ONLY)) (CAR H))
       (SETQ H (CDR H))
       (GO LAB))
     (SETQ PDES
             (PROG (H FORALL-RESULT FORALL-ENDPTR)
               (SETQ H (GETRLIST (AEVAL PDES)))
               (COND ((NULL H) (RETURN (MAKELIST NIL))))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (H)
                                   (AEVAL
                                    (LIST 'REPLACEBY (CADR (AEVAL H))
                                          (CADDR (AEVAL H)))))
                                 (CAR H))
                                NIL)))
              LOOPLABEL
               (SETQ H (CDR H))
               (COND ((NULL H) (RETURN (CONS 'LIST FORALL-RESULT))))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (H)
                           (AEVAL
                            (LIST 'REPLACEBY (CADR (AEVAL H))
                                  (CADDR (AEVAL H)))))
                         (CAR H))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (PROGN
      (TERPRI)
      (PROGN (PRIN2 "Each CL has the form:  ") NIL)
      (TERPRI)
      (COND
       ((EQUAL N 0) (PROGN (PRIN2 "    Dt(Pt) + Dx(Px) = Q1*PDE1 + ..") NIL))
       (T (PROGN (PRIN2 "    Dt(Pt) + sum_i Di(Pd(i)) = Q1*PDE1 + ..") NIL)))
      (TERPRI)
      (PROGN
       (PRIN2
        "where, e.g. PDE1 is (left hand side) - (right hand side) of PDE 1.")
       NIL)
      (TERPRI))
     (PROG (H)
       (SETQ H (AEVAL* MINCW))
      LAB
       (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MAXCW) H)) (RETURN NIL)))
       (PROGN
        (PROGN
         (ASSGNPRI (AEVAL* "NEXT: FERMIONIC CONSERVATION LAWS OF WEIGHT ") NIL
                   'FIRST)
         (ASSGNPRI (AEVAL* H) NIL 'LAST))
        (AEVAL* (LIST 'SSCONLAW N TW H AFWLIST ABWLIST PDES 'T))
        (PROGN
         (ASSGNPRI (AEVAL* "NEXT: BOSONIC CONSERVATION LAWS OF WEIGHT ") NIL
                   'FIRST)
         (ASSGNPRI (AEVAL* H) NIL 'LAST))
        (AEVAL* (LIST 'SSCONLAW N TW H AFWLIST ABWLIST PDES 'NIL)))
       (SETQ H
               ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                H))
       (GO LAB)))) 
(PUT 'LOFL 'NUMBER-OF-ARGS 2) 
(PUT 'LOFL 'DEFINED-ON-LINE '2421) 
(PUT 'LOFL 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'LOFL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOFL (N MWT)
    (COND
     ((EQUAL N 1)
      (PROG (I FORALL-RESULT FORALL-ENDPTR)
        (SETQ I 0)
        (COND ((MINUSP (DIFFERENCE MWT I)) (RETURN NIL)))
        (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (LIST I (LIST I)) NIL)))
       LOOPLABEL
        (SETQ I (PLUS2 I 1))
        (COND ((MINUSP (DIFFERENCE MWT I)) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS (LIST I (LIST I)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (PROG (L FORALL-RESULT FORALL-ENDPTR)
        (SETQ L (LOFL (DIFFERENCE N 1) MWT))
       STARTOVER
        (COND ((NULL L) (RETURN NIL)))
        (SETQ FORALL-RESULT
                ((LAMBDA (L)
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J 0)
                     (COND
                      ((MINUSP (DIFFERENCE (DIFFERENCE MWT (CAR L)) J))
                       (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (LIST (PLUS J (CAR L)) (CONS J (CADR L)))
                                      NIL)))
                    LOOPLABEL
                     (SETQ J (PLUS2 J 1))
                     (COND
                      ((MINUSP (DIFFERENCE (DIFFERENCE MWT (CAR L)) J))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS (LIST (PLUS J (CAR L)) (CONS J (CADR L)))
                                   NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                 (CAR L)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ L (CDR L))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL L) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                ((LAMBDA (L)
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J 0)
                     (COND
                      ((MINUSP (DIFFERENCE (DIFFERENCE MWT (CAR L)) J))
                       (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (LIST (PLUS J (CAR L)) (CONS J (CADR L)))
                                      NIL)))
                    LOOPLABEL
                     (SETQ J (PLUS2 J 1))
                     (COND
                      ((MINUSP (DIFFERENCE (DIFFERENCE MWT (CAR L)) J))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS (LIST (PLUS J (CAR L)) (CONS J (CADR L)))
                                   NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                 (CAR L)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ L (CDR L))
        (GO LOOPLABEL))))) 
(PUT 'WGTOF 'NUMBER-OF-ARGS 3) 
(PUT 'WGTOF 'DEFINED-ON-LINE '2429) 
(PUT 'WGTOF 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'WGTOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WGTOF (H ALI WL)
    (PROG (W WT M)
      (RETURN
       (COND ((SETQ W (ASSOC H ALI)) (LIST (CDR W) ALI WL))
             ((AND (PAIRP H) (EQUAL (CAR H) 'D))
              (PROGN
               (SETQ W (WGTOF (CADDR H) ALI WL))
               (CONS (LIST 'PLUS (CAR W) 1) (CDR W))))
             ((AND (PAIRP H) (EQUAL (CAR H) 'DF))
              (PROGN
               (SETQ W (WGTOF (CADR H) ALI WL))
               (SETQ WT (CAR W))
               (SETQ ALI (CADR W))
               (SETQ WL (CADDR W))
               (SETQ H (CDDR H))
               (PROG ()
                WHILELABEL
                 (COND ((NOT H) (RETURN NIL)))
                 (PROGN
                  (SETQ W (WGTOF (CAR H) ALI WL))
                  (SETQ H (CDR H))
                  (SETQ ALI (CADR W))
                  (SETQ WL (CADDR W))
                  (COND ((OR (NULL H) (NOT (FIXP (CAR H)))) (SETQ M 1))
                        (T (PROGN (SETQ M (CAR H)) (SETQ H (CDR H)))))
                  (SETQ WT (LIST 'DIFFERENCE WT (LIST 'TIMES (CAR W) M))))
                 (GO WHILELABEL))
               (LIST WT ALI WL)))
             (T
              (PROGN
               (SETQ WT (MKID 'W_ (COND ((PAIRP H) (GENSYM)) (T H))))
               (SETQ ALI (CONS (CONS H WT) ALI))
               (SETQ WL (CONS WT WL))
               (LIST WT ALI WL))))))) 
(FLAG '(FINDSSWEIGHTS) 'OPFN) 
(PUT 'FINDSSWEIGHTS 'NUMBER-OF-ARGS 6) 
(PUT 'FINDSSWEIGHTS 'DEFINED-ON-LINE '2466) 
(PUT 'FINDSSWEIGHTS 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'FINDSSWEIGHTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FINDSSWEIGHTS (N NF NB EXLI ZEROWEI VERBOSE)
    (PROG (J K S BSN W JMAX H EX FWLI BWLI SC FH BH EH POSI ZROP POSH NEGH BS
           ALI ALLALI WL SF TF WTLI WT ELI EWLI HLIST)
      (COND
       ((OR (NOT (PAIRP EXLI)) (NEQ (CAR EXLI) 'LIST))
        (RETURN
         (PROGN
          (PROGN (PRIN2 "The input expression is not a list  {  } .") NIL)
          (TERPRI)
          (PROGN (PRIN2 "Try again.") NIL)
          (TERPRI)))))
      (SETQ BS (LIST NIL))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (PROG (S)
           (SETQ S BS)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (PROGN
               (SETQ BSN (CONS (CONS 0 S) BSN))
               (SETQ BSN (CONS (CONS 1 S) BSN))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ BS BSN)
         (SETQ BSN NIL))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ BS
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S BS)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS
                                     (PROG (J FORALL-RESULT)
                                       (SETQ J S)
                                       (SETQ FORALL-RESULT 0)
                                      LAB1
                                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                                       (SETQ FORALL-RESULT
                                               (PLUS ((LAMBDA (J) J) (CAR J))
                                                     FORALL-RESULT))
                                       (SETQ J (CDR J))
                                       (GO LAB1))
                                     S))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (CONS
                             (PROG (J FORALL-RESULT)
                               (SETQ J S)
                               (SETQ FORALL-RESULT 0)
                              LAB1
                               (COND ((NULL J) (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (PLUS ((LAMBDA (J) J) (CAR J))
                                             FORALL-RESULT))
                               (SETQ J (CDR J))
                               (GO LAB1))
                             S))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALI (LIST (CONS 'X (MINUS 2))))
      (PROG (H)
        (SETQ H (CDR (REVAL1 ZEROWEI T)))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H) (SETQ ALI (CONS (CONS H 0) ALI))) (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (SETQ ALI (CONS (CONS (LIST 'TH J) (MINUS 1)) ALI))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NF J)) (RETURN NIL)))
        (PROGN
         (SETQ W (MKID 'W_ (MKID 'F J)))
         (SETQ WL (CONS W WL))
         (SETQ ALI (CONS (CONS (LIST 'F J) W) ALI))
         (PROG (S)
           (SETQ S BS)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (SETQ ALI
                      (CONS
                       (CONS
                        (COND ((EVENP (CAR S)) (APPEND (LIST 'F J) (CDR S)))
                              (T (APPEND (LIST 'B J) (CDR S))))
                        (COND ((ZEROP (CAR S)) W) (T (LIST 'PLUS W (CAR S)))))
                       ALI)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NB J)) (RETURN NIL)))
        (PROGN
         (SETQ W (MKID 'W_ (MKID 'B J)))
         (SETQ WL (CONS W WL))
         (SETQ ALI (CONS (CONS (LIST 'B J) W) ALI))
         (PROG (S)
           (SETQ S BS)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (SETQ ALI
                      (CONS
                       (CONS
                        (COND ((EVENP (CAR S)) (APPEND (LIST 'B J) (CDR S)))
                              (T (APPEND (LIST 'F J) (CDR S))))
                        (COND ((ZEROP (CAR S)) W) (T (LIST 'PLUS W (CAR S)))))
                       ALI)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (EX)
        (SETQ EX (CDR EXLI))
       LAB
        (COND ((NULL EX) (RETURN NIL)))
        ((LAMBDA (EX)
           (PROGN
            (SETQ SF
                    (CAR
                     (COND
                      ((AND (PAIRP EX) (EQUAL (CAR EX) 'EQUAL))
                       (ADDSQ (SIMP (CADR EX)) (NEGSQ (SIMP (CADDR EX)))))
                      (T (SIMP EX)))))
            (SETQ WTLI NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT SF) (RETURN NIL)))
              (PROGN
               (SETQ TF (FIRST_TERM_SF SF))
               (SETQ SF (ADDF SF (NEGF TF)))
               (SETQ WT NIL)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND TF (NOT (OR (ATOM TF) (ATOM (CAR TF))))))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ W (WGTOF (CAAAR TF) ALI WL))
                  (SETQ WT (CONS (LIST 'TIMES (CDAAR TF) (CAR W)) WT))
                  (SETQ ALI (CADR W))
                  (SETQ WL (CADDR W))
                  (SETQ TF (CDAR TF)))
                 (GO WHILELABEL))
               (SETQ WT
                       (COND ((NULL WT) 0) ((CDR WT) (CONS 'PLUS WT))
                             (T (CAR WT))))
               (SETQ WTLI (CONS (REVAL1 WT T) WTLI)))
              (GO WHILELABEL))
            (COND
             ((AND WTLI (CDR WTLI))
              (PROG (W)
                (SETQ W (CDR WTLI))
               LAB
                (COND ((NULL W) (RETURN NIL)))
                ((LAMBDA (W)
                   (SETQ ELI
                           (CONS (REVAL1 (LIST 'DIFFERENCE (CAR WTLI) W) T)
                                 ELI)))
                 (CAR W))
                (SETQ W (CDR W))
                (GO LAB))))
            (SETQ EWLI (CONS (CAR WTLI) EWLI))))
         (CAR EX))
        (SETQ EX (CDR EX))
        (GO LAB))
      (SETQ !ARBINT 0)
      (COND
       ((NULL ELI)
        (COND ((NULL WL) (SETQ S NIL))
              (T
               (PROGN
                (SETQ S NIL)
                (PROG (W)
                  (SETQ W WL)
                 LAB
                  (COND ((NULL W) (RETURN NIL)))
                  ((LAMBDA (W)
                     (PROGN
                      (SETQ !ARBINT (ADD1 !ARBINT))
                      (SETQ S
                              (CONS (LIST 'EQUAL W (LIST 'ARBCOMPLEX !ARBINT))
                                    S))))
                   (CAR W))
                  (SETQ W (CDR W))
                  (GO LAB))
                (SETQ S (CONS 'LIST S))))))
       (T
        (PROGN
         (SETQ S (SOLVEEVAL (LIST (CONS 'LIST ELI) (CONS 'LIST WL))))
         (COND (S (SETQ S (CDR S))))
         (COND (S (SETQ S (CAR S))))
         (COND
          ((AND (PAIRP S) (EQUAL (CAR S) 'EQUAL)) (SETQ S (LIST 'LIST S)))))))
      (COND
       (S
        (PROGN
         (SETQ FWLI
                 (CONS 'LIST
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J 1)
                         (COND ((MINUSP (DIFFERENCE NF J)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS (CDR (ASSOC (LIST 'F J) ALI))
                                               NIL)))
                        LOOPLABEL
                         (SETQ J (PLUS2 J 1))
                         (COND
                          ((MINUSP (DIFFERENCE NF J)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS (CDR (ASSOC (LIST 'F J) ALI)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ BWLI
                 (CONS 'LIST
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J 1)
                         (COND ((MINUSP (DIFFERENCE NB J)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS (CDR (ASSOC (LIST 'B J) ALI))
                                               NIL)))
                        LOOPLABEL
                         (SETQ J (PLUS2 J 1))
                         (COND
                          ((MINUSP (DIFFERENCE NB J)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS (CDR (ASSOC (LIST 'B J) ALI)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ EWLI (CONS 'LIST EWLI))
         (SETQ JMAX (COND ((EQUAL !ARBINT 0) 1) (T !ARBINT)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE JMAX J)) (RETURN NIL)))
           (PROGN
            (SETQ SC S)
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE !ARBINT K)) (RETURN NIL)))
              (COND
               ((NEQ K J)
                (SETQ SC
                        (AEVAL*
                         (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX K) 0)
                               SC)))))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETQ K ALI)
            (PROG (H)
              (SETQ H (CDR SC))
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H) (SETQ K (SUBST (CADDR H) (REVAL1 (CADR H) T) K)))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (SETQ ALLALI (CONS K ALLALI))
            (COND
             ((NEQ J 0)
              (SETQ SC
                      (AEVAL*
                       (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX J) 1) SC)))))
            (SETQ FH (AEVAL* (LIST 'SUB SC FWLI)))
            (SETQ BH (AEVAL* (LIST 'SUB SC BWLI)))
            (SETQ EH (AEVAL* (LIST 'SUB SC EWLI)))
            (SETQ POSI T)
            (SETQ ZROP T)
            (PROG (K)
              (SETQ K (CDR FH))
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (PROGN
                  (COND ((LEQ K 0) (SETQ POSI NIL)))
                  (COND ((NEQ K 0) (SETQ ZROP NIL)))))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))
            (PROG (K)
              (SETQ K (CDR BH))
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (PROGN
                  (COND ((LEQ K 0) (SETQ POSI NIL)))
                  (COND ((NEQ K 0) (SETQ ZROP NIL)))))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))
            (COND
             ((NULL ZROP)
              (PROGN
               (COND (POSI (SETQ POSH (CONS (LIST 'LIST FH BH EH) POSH)))
                     (T (SETQ NEGH (CONS (LIST 'LIST FH BH EH) NEGH))))
               NIL))))
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (SETQ HLIST (CONS 'LIST (APPEND POSH NEGH)))
      (COND
       (VERBOSE
        (PROGN
         (COND
          ((EQUAL HLIST (LIST 'LIST))
           (PROGN (PRIN2 "This system is not homogeneous.") NIL))
          (T
           (PROGN
            (COND
             ((EQUAL !ARBINT 0)
              (PROGN (PRIN2 "This system has the following homogeneity:") NIL))
             (T
              (PROGN
               (PRIN2 "This system has the following homogeneities:")
               NIL)))
            (TERPRI)
            (PROG (ALI)
              (SETQ ALI ALLALI)
             LAB
              (COND ((NULL ALI) (RETURN NIL)))
              ((LAMBDA (ALI)
                 (PROGN
                  (PROG (W)
                    (SETQ W ALI)
                   LAB
                    (COND ((NULL W) (RETURN NIL)))
                    ((LAMBDA (W)
                       (PROGN
                        (ASSGNPRI (AEVAL "W[") NIL 'FIRST)
                        (ASSGNPRI (AEVAL (CAR W)) NIL NIL)
                        (ASSGNPRI (AEVAL "] = ") NIL NIL)
                        (ASSGNPRI (REVAL1 (CDR W) T) NIL 'LAST)))
                     (CAR W))
                    (SETQ W (CDR W))
                    (GO LAB))
                  (PROGN (PRIN2 "=================================") NIL)
                  (TERPRI)
                  NIL))
               (CAR ALI))
              (SETQ ALI (CDR ALI))
              (GO LAB))
            (PROGN (PRIN2 "The program returns a list of ") NIL)
            (SETQ H (DIFFERENCE (LENGTH HLIST) 1))
            (COND
             ((EQUAL H 1)
              (PROGN
               (PROGN (PRIN2 "one homogeneity") NIL)
               (TERPRI)
               (PROGN (PRIN2 "which is a list of") NIL)))
             (T
              (PROGN
               (PROGN (PRIN2 H) (PRIN2 " homogeneities,") NIL)
               (TERPRI)
               (PROGN (PRIN2 "each homogeneity being a list of") NIL)
               NIL)))
            (TERPRI)
            (PROGN (PRIN2 "a list of all f(1),b(2),.. weights and") NIL)
            (TERPRI)
            (PROGN (PRIN2 "a list of all b(1),b(2),.. weights and") NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "a list of the weights of equations/expressions")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "in the input. ") NIL)
            (COND
             ((NEQ !ARBINT 0)
              (PROGN
               (PROGN (PRIN2 "In this output free parameters") NIL)
               (TERPRI)
               (PROGN (PRIN2 "arbcomplex(i) are replaced by 1:") NIL)
               NIL)))
            (TERPRI)
            (MATHPRINT HLIST)
            NIL))))))
      (RETURN HLIST))) 
(PUT 'LINEARIZE 'NUMBER-OF-ARGS 5) 
(FLAG '(LINEARIZE) 'OPFN) 
(PUT 'LINEARIZE 'DEFINED-ON-LINE '2694) 
(PUT 'LINEARIZE 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'LINEARIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LINEARIZE (PDES NF NB TPAR SPAR)
    (PROG (SPAR_BAK TPAR_BAK N M P NPDES REL)
      (SETQ SPAR_BAK (AEVAL S_CHANGES_PARITY))
      (SETQ TPAR_BAK (AEVAL T_CHANGES_PARITY))
      (COND ((BOOLVALUE* TPAR) (AEVAL (ON (LIST 'T_CHANGES_PARITY))))
            (T (AEVAL (OFF (LIST 'T_CHANGES_PARITY)))))
      (SETQ N (AEVAL (LIST 'LENGTH PDES)))
      (PROGN
       (TERPRI)
       (PROGN
        (PRIN2 "The following linearization generates ")
        (PRIN2 (COND ((EQUAL N 1) "a condition") (T "conditions")))
        (PRIN2 " for the right hand side")
        (PRIN2 (COND ((EQUAL N 1) " ") (T "s ")))
        NIL)
       (TERPRI)
       (PROGN (PRIN2 "of the symmetry: ") NIL)
       (TERPRI)
       (TERPRI))
      (SETQ REL (AEVAL (LIST 'LIST)))
      (COND
       ((BOOLVALUE* SPAR)
        (PROGN
         (AEVAL (ON (LIST 'S_CHANGES_PARITY)))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NF) N)) (RETURN NIL)))
           (PROGN
            (SETQ M (AEVAL* (LIST 'PLUS NB N)))
            (AEVAL* (DEPEND (LIST (LIST 'B M) 'X 'T)))
            (AEVAL* (DEPEND (LIST (LIST 'F N) 'S)))
            (SETQ REL
                    (AEVAL*
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'DF (LIST 'F N) 'S) (LIST 'B M))
                           REL)))
            (SETK (LIST 'DF (LIST 'F N) 'S) (AEVAL* (LIST 'B M)))
            (PROGN
             (PRIN2 "   D_s f(")
             (PRIN2 N)
             (PRIN2 ") = b(")
             (PRIN2 M)
             (PRIN2 ")")
             NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NB) N)) (RETURN NIL)))
           (PROGN
            (SETQ M (AEVAL* (LIST 'PLUS NF N)))
            (AEVAL* (DEPEND (LIST (LIST 'F M) 'X 'T)))
            (AEVAL* (DEPEND (LIST (LIST 'B N) 'S)))
            (SETQ REL
                    (AEVAL*
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'DF (LIST 'B N) 'S) (LIST 'F M))
                           REL)))
            (SETK (LIST 'DF (LIST 'B N) 'S) (AEVAL* (LIST 'F M)))
            (PROGN
             (PRIN2 "   D_s b(")
             (PRIN2 N)
             (PRIN2 ") = f(")
             (PRIN2 M)
             (PRIN2 ")")
             NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))))
       (T
        (PROGN
         (AEVAL (OFF (LIST 'S_CHANGES_PARITY)))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NF) N)) (RETURN NIL)))
           (PROGN
            (SETQ M (AEVAL* (LIST 'PLUS NF N)))
            (AEVAL* (DEPEND (LIST (LIST 'B M) 'X 'T)))
            (AEVAL* (DEPEND (LIST (LIST 'F N) 'S)))
            (SETQ REL
                    (AEVAL*
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'DF (LIST 'F N) 'S) (LIST 'F M))
                           REL)))
            (SETK (LIST 'DF (LIST 'F N) 'S) (AEVAL* (LIST 'F M)))
            (PROGN
             (PRIN2 "   D_s f(")
             (PRIN2 N)
             (PRIN2 ") = f(")
             (PRIN2 M)
             (PRIN2 ")")
             NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NB) N)) (RETURN NIL)))
           (PROGN
            (SETQ M (AEVAL* (LIST 'PLUS NB N)))
            (AEVAL* (DEPEND (LIST (LIST 'F M) 'X 'T)))
            (AEVAL* (DEPEND (LIST (LIST 'B N) 'S)))
            (SETQ REL
                    (AEVAL*
                     (LIST 'CONS
                           (LIST 'EQUAL (LIST 'DF (LIST 'B N) 'S) (LIST 'B M))
                           REL)))
            (SETK (LIST 'DF (LIST 'B N) 'S) (AEVAL* (LIST 'B M)))
            (PROGN
             (PRIN2 "   D_s b(")
             (PRIN2 N)
             (PRIN2 ") = b(")
             (PRIN2 M)
             (PRIN2 ")")
             NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB)))))
      (TERPRI)
      (ASSGNPRI (AEVAL "The original system:") NIL 'ONLY)
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL PDES)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (ASSGNPRI (AEVAL P) NIL 'ONLY)) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ NPDES (AEVAL (LIST 'LIST)))
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL PDES)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ M (AEVAL (LIST 'DF (LIST 'LHS P) 'S)))
            (COND
             ((EVALEQUAL (AEVAL (LIST 'PART M 0)) (AEVAL 'MINUS))
              (SETQ N (AEVAL 'T)))
             (T (SETQ N (AEVAL 'NIL))))
            (SETQ NPDES
                    (AEVAL
                     (LIST 'SQCONS
                           (COND
                            ((AND (BOOLVALUE* TPAR) (BOOLVALUE* SPAR))
                             (COND
                              ((BOOLVALUE* N)
                               (AEVAL
                                (LIST 'EQUAL (LIST 'MINUS M)
                                      (LIST 'DF (LIST 'RHS P) 'S))))
                              (T
                               (AEVAL
                                (LIST 'EQUAL M
                                      (LIST 'MINUS
                                            (LIST 'DF (LIST 'RHS P) 'S)))))))
                            ((BOOLVALUE* N)
                             (AEVAL
                              (LIST 'EQUAL (LIST 'MINUS M)
                                    (LIST 'MINUS
                                          (LIST 'DF (LIST 'RHS P) 'S)))))
                            (T
                             (AEVAL
                              (LIST 'EQUAL M (LIST 'DF (LIST 'RHS P) 'S)))))
                           NPDES)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ NPDES (AEVAL (LIST 'REVERSE NPDES)))
      (ASSGNPRI (AEVAL "The linearized system: ") NIL 'ONLY)
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL NPDES)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (ASSGNPRI (AEVAL P) NIL 'ONLY)) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (ASSGNPRI (AEVAL "and in machine readable form: ") NIL 'ONLY)
      (AEVAL (OFF (LIST 'NAT)))
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL NPDES)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (ASSGNPRI (AEVAL P) NIL 'ONLY)) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (AEVAL (ON (LIST 'NAT)))
      (COND
       ((BOOLVALUE* (REVALX (NEQ SPAR_BAK S_CHANGES_PARITY)))
        (COND ((BOOLVALUE* SPAR_BAK) (AEVAL (ON (LIST 'S_CHANGES_PARITY))))
              (T (AEVAL (OFF (LIST 'S_CHANGES_PARITY)))))))
      (COND
       ((BOOLVALUE* (REVALX (NEQ TPAR_BAK T_CHANGES_PARITY)))
        (COND ((BOOLVALUE* TPAR_BAK) (AEVAL (ON (LIST 'T_CHANGES_PARITY))))
              (T (AEVAL (OFF (LIST 'T_CHANGES_PARITY)))))))
      (RETURN (AEVAL (LIST 'LIST REL NPDES))))) 
(FLAG '(GENSSPOLY) 'OPFN) 
(PUT 'GENSSPOLY 'NUMBER-OF-ARGS 4) 
(PUT 'GENSSPOLY 'DEFINED-ON-LINE '2780) 
(PUT 'GENSSPOLY 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'GENSSPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GENSSPOLY (N WGTLIST CNAME MODE)
    (PROG (LINSUB G H FLIST BLIST AFBLIST FORBID NON0COEFF FNAME_BAK FL ALLF
           ALLB FWLIST BWLIST NF NB VERBOSE AFWLIST ABWLIST POL FONLY BONLY WGT
           NEWF NEWB PRINT_BAK)
      (SETQ WGTLIST (CDR WGTLIST))
      (SETQ AFWLIST (CADR (CAR WGTLIST)))
      (SETQ ABWLIST (CADDR (CAR WGTLIST)))
      (SETQ WGT (CADDDR (CAR WGTLIST)))
      (SETQ WGTLIST (CDR WGTLIST))
      (SETQ NF (DIFFERENCE (LENGTH AFWLIST) 1))
      (SETQ FWLIST (CDR (REVAL1 AFWLIST T)))
      (SETQ NB (DIFFERENCE (LENGTH ABWLIST) 1))
      (SETQ BWLIST (CDR (REVAL1 ABWLIST T)))
      (SETQ MODE (CDR MODE))
      (PROG ()
       WHILELABEL
        (COND ((NOT MODE) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (CAR MODE) 'LIN)
           (PROGN
            (COND
             ((NOT (EVENP NF))
              (REDERR "The flag `lin' can not be run with odd many fermions")))
            (SETQ LINSUB
                    (PROG (H FORALL-RESULT FORALL-ENDPTR)
                      (SETQ H (PLUS (QUOTIENT NF 2) 1))
                      (COND ((MINUSP (DIFFERENCE NF H)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (LIST 'F H) NIL)))
                     LOOPLABEL
                      (SETQ H (PLUS2 H 1))
                      (COND
                       ((MINUSP (DIFFERENCE NF H)) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (LIST 'F H) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             ((NOT (EVENP NB))
              (REDERR "The flag `lin' can not be run with odd many bosons")))
            (SETQ LINSUB
                    (CONS 'LIST
                          (APPEND LINSUB
                                  (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ H (PLUS (QUOTIENT NB 2) 1))
                                    (COND
                                     ((MINUSP (DIFFERENCE NB H)) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS (LIST 'B H) NIL)))
                                   LOOPLABEL
                                    (SETQ H (PLUS2 H 1))
                                    (COND
                                     ((MINUSP (DIFFERENCE NB H))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS (LIST 'B H) NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))))))
          ((EQUAL (CAR MODE) 'VERBOSE) (SETQ VERBOSE T))
          ((EQUAL (CAR MODE) 'FONLY) (SETQ FONLY T))
          ((EQUAL (CAR MODE) 'BONLY) (SETQ BONLY T))
          ((AND (PAIRP (CAR MODE)) (EQUAL (CADAR MODE) 'FORBID))
           (SETQ FORBID (CDDAR MODE)))
          ((AND (PAIRP (CAR MODE)) (EQUAL (CADAR MODE) 'NON0COEFF))
           (SETQ NON0COEFF (CDDAR MODE))))
         (SETQ MODE (CDR MODE)))
        (GO WHILELABEL))
      (INPUT_CONSISTENCY_TEST AFWLIST ABWLIST)
      (SETQ N_ N)
      (SETQ FLIST
              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                (SETQ G 1)
                (COND ((MINUSP (DIFFERENCE NF G)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (LIST 'F G) NIL)))
               LOOPLABEL
                (SETQ G (PLUS2 G 1))
                (COND ((MINUSP (DIFFERENCE NF G)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (LIST 'F G) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BLIST
              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                (SETQ G 1)
                (COND ((MINUSP (DIFFERENCE NB G)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (LIST 'B G) NIL)))
               LOOPLABEL
                (SETQ G (PLUS2 G 1))
                (COND ((MINUSP (DIFFERENCE NB G)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (LIST 'B G) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROGN
       (SETQ AFBLIST (AEVAL (CONS 'LIST (APPEND FLIST BLIST))))
       (PROG (G)
         (SETQ G (GETRLIST (AEVAL AFBLIST)))
        LAB
         (COND ((NULL G) (RETURN NIL)))
         ((LAMBDA (G) (AEVAL (DEPEND (LIST G 'X 'T)))) (CAR G))
         (SETQ G (CDR G))
         (GO LAB))
       (AEVAL 'NIL))
      (SETQ FNAME_BAK FNAME_)
      (SETQ FNAME_ (REVAL1 CNAME T))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NF I)) (RETURN NIL)))
        (SETQ ALLF (CONS (LIST (NTH FWLIST I) (NTH FLIST I)) ALLF))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NB I)) (RETURN NIL)))
        (SETQ ALLB (CONS (LIST (NTH BWLIST I) (NTH BLIST I)) ALLB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ POL (RHS_TERM_LIST N ALLF ALLB WGT LINSUB FORBID VERBOSE))
      (SETQ POL (LIST 'LIST (CONS 'LIST (CAR POL)) (CONS 'LIST (CADR POL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT WGTLIST) (RETURN NIL)))
        (PROGN
         (SETQ AFWLIST (CADR (CAR WGTLIST)))
         (SETQ ABWLIST (CADDR (CAR WGTLIST)))
         (SETQ WGT (CADDDR (CAR WGTLIST)))
         (SETQ WGTLIST (CDR WGTLIST))
         (PROGN
          (SETK 'SU (AEVAL* (LIST 'LIST)))
          (SETQ G (AEVAL* 0))
          (PROG (H)
            (SETQ H (GETRLIST (AEVAL* AFWLIST)))
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H)
               (PROGN
                (SETQ G (AEVAL* (LIST 'PLUS G 1)))
                (SETK 'SU
                      (AEVAL*
                       (LIST 'CONS
                             (LIST 'EQUAL (LIST 'F G)
                                   (LIST 'TIMES (LIST 'F G)
                                         (LIST 'EXPT 'LIN_TEST_CONST H)))
                             'SU)))))
             (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (SETQ G (AEVAL* 0))
          (PROG (H)
            (SETQ H (GETRLIST (AEVAL* ABWLIST)))
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H)
               (PROGN
                (SETQ G (AEVAL* (LIST 'PLUS G 1)))
                (SETK 'SU
                      (AEVAL*
                       (LIST 'CONS
                             (LIST 'EQUAL (LIST 'B G)
                                   (LIST 'TIMES (LIST 'B G)
                                         (LIST 'EXPT 'LIN_TEST_CONST H)))
                             'SU)))))
             (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (SETQ POL (AEVAL* (LIST 'SUB 'SU POL)))
          (AEVAL* (LET '(LRULE1)))
          (SETQ POL (AEVAL* POL))
          (AEVAL* (CLEARRULES (LIST 'LRULE1)))
          (AEVAL* (LET '(LRULE2)))
          (SETQ POL (AEVAL* POL))
          (AEVAL* (CLEARRULES (LIST 'LRULE2)))
          (SETQ NEWF (AEVAL* (LIST 'LIST)))
          (PROG (H)
            (SETQ H (GETRLIST (AEVAL* (LIST 'FIRST POL))))
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H)
               (PROGN
                (SETQ H
                        (AEVAL*
                         (LIST 'QUOTIENT H (LIST 'EXPT 'LIN_TEST_CONST WGT))))
                (COND
                 ((FREEOF (REVALX H) (REVALX 'LIN_TEST_CONST))
                  (SETQ NEWF (AEVAL* (LIST 'CONS H NEWF)))))))
             (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (SETQ NEWB (AEVAL* (LIST 'LIST)))
          (PROG (H)
            (SETQ H (GETRLIST (AEVAL* (LIST 'SECOND POL))))
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H)
               (PROGN
                (SETQ H
                        (AEVAL*
                         (LIST 'QUOTIENT H (LIST 'EXPT 'LIN_TEST_CONST WGT))))
                (COND
                 ((FREEOF (REVALX H) (REVALX 'LIN_TEST_CONST))
                  (SETQ NEWB (AEVAL* (LIST 'CONS H NEWB)))))))
             (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (SETQ POL (AEVAL* (LIST 'LIST NEWF NEWB)))))
        (GO WHILELABEL))
      (SETQ PRINT_BAK PRINT_)
      (SETQ PRINT_ NIL)
      (SETQ NEWF
              (COND (BONLY 0)
                    (T
                     (PROGN
                      (SETQ H
                              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                                (SETQ G (CDADR POL))
                                (COND ((NULL G) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (G)
                                                    (PROGN
                                                     (SETQ H
                                                             (NEWFCT FNAME_ NIL
                                                              NFCT_))
                                                     (SETQ FL (CONS H FL))
                                                     (SETQ NFCT_ (ADD1 NFCT_))
                                                     (LIST 'TIMES H G)))
                                                  (CAR G))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ G (CDR G))
                                (COND ((NULL G) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (G)
                                            (PROGN
                                             (SETQ H (NEWFCT FNAME_ NIL NFCT_))
                                             (SETQ FL (CONS H FL))
                                             (SETQ NFCT_ (ADD1 NFCT_))
                                             (LIST 'TIMES H G)))
                                          (CAR G))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (COND (H (COND ((CDR H) (CONS 'PLUS H)) (T (CAR H))))
                            (T 0))))))
      (SETQ NEWB
              (COND (FONLY 0)
                    (T
                     (PROGN
                      (SETQ H
                              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                                (SETQ G (CDADDR POL))
                                (COND ((NULL G) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (G)
                                                    (PROGN
                                                     (SETQ H
                                                             (NEWFCT FNAME_ NIL
                                                              NFCT_))
                                                     (SETQ FL (CONS H FL))
                                                     (SETQ NFCT_ (ADD1 NFCT_))
                                                     (LIST 'TIMES H G)))
                                                  (CAR G))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ G (CDR G))
                                (COND ((NULL G) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (G)
                                            (PROGN
                                             (SETQ H (NEWFCT FNAME_ NIL NFCT_))
                                             (SETQ FL (CONS H FL))
                                             (SETQ NFCT_ (ADD1 NFCT_))
                                             (LIST 'TIMES H G)))
                                          (CAR G))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (COND (H (COND ((CDR H) (CONS 'PLUS H)) (T (CAR H))))
                            (T 0))))))
      (SETQ PRINT_ PRINT_BAK)
      (SETQ FNAME_ FNAME_BAK)
      (SETQ FLIN_ FL)
      (RETURN (LIST 'LIST NEWF NEWB (CONS 'LIST FL))))) 
(PUT 'THGEN 'NUMBER-OF-ARGS 1) 
(FLAG '(THGEN) 'OPFN) 
(PUT 'THGEN 'DEFINED-ON-LINE '2909) 
(PUT 'THGEN 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'THGEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE THGEN (K)
    (COND
     ((EVALEQUAL (AEVAL K) 0)
      (AEVAL (LIST 'LIST (LIST 'LIST 1 0 (LIST 'LIST)))))
     (T
      (PROG (A B)
        (SETQ A (AEVAL (LIST 'THGEN (LIST 'DIFFERENCE K 1))))
        (RETURN
         (PROG (B FORALL-RESULT FORALL-ENDPTR)
           (SETQ B (GETRLIST (AEVAL A)))
          STARTOVER
           (COND ((NULL B) (RETURN (MAKELIST NIL))))
           (SETQ FORALL-RESULT
                   ((LAMBDA (B)
                      (AEVAL
                       (LIST 'LIST
                             (LIST 'LIST (LIST 'FIRST B) (LIST 'SECOND B)
                                   (LIST 'APPEND (LIST 'THIRD B)
                                         (LIST 'LIST 0)))
                             (LIST 'LIST
                                   (LIST 'TIMES (LIST 'TH K) (LIST 'FIRST B))
                                   (LIST 'PLUS (LIST 'SECOND B) 1)
                                   (LIST 'APPEND (LIST 'THIRD B)
                                         (LIST 'LIST 1))))))
                    (CAR B)))
           (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
           (SETQ B (CDR B))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL B) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (GETRLIST
                    ((LAMBDA (B)
                       (AEVAL
                        (LIST 'LIST
                              (LIST 'LIST (LIST 'FIRST B) (LIST 'SECOND B)
                                    (LIST 'APPEND (LIST 'THIRD B)
                                          (LIST 'LIST 0)))
                              (LIST 'LIST
                                    (LIST 'TIMES (LIST 'TH K) (LIST 'FIRST B))
                                    (LIST 'PLUS (LIST 'SECOND B) 1)
                                    (LIST 'APPEND (LIST 'THIRD B)
                                          (LIST 'LIST 1))))))
                     (CAR B))))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ B (CDR B))
           (GO LOOPLABEL))))))) 
(SETK 'DRULE
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'D (LIST '~ 'I) (LIST '~ 'A))
                   (LIST 'PLUS (LIST 'DF 'A (LIST 'TH 'I))
                         (LIST 'TIMES (LIST 'TH 'I) (LIST 'DF 'A 'X))))))) 
(PUT 'TOCOO 'NUMBER-OF-ARGS 4) 
(FLAG '(TOCOO) 'OPFN) 
(PUT 'TOCOO 'DEFINED-ON-LINE '2926) 
(PUT 'TOCOO 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'TOCOO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TOCOO (N NF NB EX)
    (PROG (THLI SUBFNB I H)
      (SETQ N_ N)
      (AEVAL (PUT 'F 'PRIFN 'NIL))
      (AEVAL (PUT 'B 'PRIFN 'NIL))
      (SETQ THLI (AEVAL (LIST 'THGEN N)))
      (SETQ SUBFNB (AEVAL (LIST 'LIST)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NF) I)) (RETURN NIL)))
        (SETQ SUBFNB
                (AEVAL*
                 (LIST 'CONS
                       (LIST 'EQUAL (LIST 'F I)
                             (PROG (H FORALL-RESULT)
                               (SETQ H (GETRLIST (AEVAL* THLI)))
                               (SETQ FORALL-RESULT 0)
                              LAB1
                               (COND ((NULL H) (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'PLUS
                                              ((LAMBDA (H)
                                                 (COND
                                                  ((BOOLVALUE*
                                                    (REVALX
                                                     (LIST 'EVENP
                                                           (LIST 'SECOND H))))
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          (CONS 'F
                                                                (CDR
                                                                 (AEVAL*
                                                                  (LIST 'CONS I
                                                                        (LIST
                                                                         'THIRD
                                                                         H)))))
                                                          (LIST 'FIRST H))))
                                                  (T
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          (CONS 'B
                                                                (CDR
                                                                 (AEVAL*
                                                                  (LIST 'CONS I
                                                                        (LIST
                                                                         'THIRD
                                                                         H)))))
                                                          (LIST 'FIRST H))))))
                                               (CAR H))
                                              FORALL-RESULT)))
                               (SETQ H (CDR H))
                               (GO LAB1)))
                       SUBFNB)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NB) I)) (RETURN NIL)))
        (SETQ SUBFNB
                (AEVAL*
                 (LIST 'CONS
                       (LIST 'EQUAL (LIST 'B I)
                             (PROG (H FORALL-RESULT)
                               (SETQ H (GETRLIST (AEVAL* THLI)))
                               (SETQ FORALL-RESULT 0)
                              LAB1
                               (COND ((NULL H) (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'PLUS
                                              ((LAMBDA (H)
                                                 (COND
                                                  ((BOOLVALUE*
                                                    (REVALX
                                                     (LIST 'EVENP
                                                           (LIST 'SECOND H))))
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          (CONS 'B
                                                                (CDR
                                                                 (AEVAL*
                                                                  (LIST 'CONS I
                                                                        (LIST
                                                                         'THIRD
                                                                         H)))))
                                                          (LIST 'FIRST H))))
                                                  (T
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          (CONS 'F
                                                                (CDR
                                                                 (AEVAL*
                                                                  (LIST 'CONS I
                                                                        (LIST
                                                                         'THIRD
                                                                         H)))))
                                                          (LIST 'FIRST H))))))
                                               (CAR H))
                                              FORALL-RESULT)))
                               (SETQ H (CDR H))
                               (GO LAB1)))
                       SUBFNB)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETQ EX (AEVAL (LIST 'SUB SUBFNB EX)))
      (AEVAL (LET '(DRULE)))
      (SETQ EX (AEVAL EX))
      (AEVAL (CLEARRULES (LIST 'DRULE)))
      (RETURN (AEVAL EX)))) 
(PUT 'TOFIELD 'NUMBER-OF-ARGS 5) 
(FLAG '(TOFIELD) 'OPFN) 
(PUT 'TOFIELD 'DEFINED-ON-LINE '2965) 
(PUT 'TOFIELD 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'TOFIELD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TOFIELD (N NF NB EX ZEROWEI)
    (COND
     ((BOOLVALUE* (REVALX (AND (PAIRP (REVALX EX)) (EQUAL (CAR EX) 'LIST))))
      (ASSGNPRI (AEVAL "The input should be a polynomial, not an equations!")
                NIL 'ONLY))
     (T
      (PROG (SSP ANS FIELDANS RLIST INDEPV H EQNLI SOLU WFLIST WBLIST TW GS K
             FT SB NOTNUM NOTPOS)
        (SETK 'W
              (AEVAL (LIST 'FINDSSWEIGHTS N NF NB (LIST 'LIST EX) ZEROWEI 'T)))
        (COND
         ((EVALEQUAL (AEVAL 'W) (AEVAL (LIST 'LIST)))
          (RETURN
           (PROGN
            (PROGN
             (PRIN2 "The input expression does not have a homogeneity")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "which could be used to compute a field form.") NIL)
            (TERPRI)))))
        (SETK 'W (AEVAL (LIST 'FIRST 'W)))
        (SETQ WFLIST (AEVAL (LIST 'FIRST 'W)))
        (SETQ WBLIST (AEVAL (LIST 'SECOND 'W)))
        (SETQ TW (AEVAL (LIST 'FIRST (LIST 'THIRD 'W))))
        (PROG (H)
          (SETQ H (GETRLIST (AEVAL (LIST 'APPEND WFLIST WBLIST))))
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (COND ((NOT (FIXP (REVALX H))) (SETQ NOTNUM (AEVAL 'T)))
                   ((EVALLEQ (AEVAL H) 0) (SETQ NOTPOS (AEVAL 'T)))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (COND
         ((BOOLVALUE* NOTNUM)
          (PROGN
           (PROGN
            (PRIN2 "#####   Strange error: returned weights returned")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "        from FindSSWeights() are not integers!   #####")
            NIL)
           (TERPRI)))
         ((BOOLVALUE* NOTPOS)
          (PROGN
           (SETQ GS (GENSYM))
           (SETQ K (SETKORDER (LIST GS)))
           (SETQ FT
                   (APPEND (SEARCH_LI2 (AEVAL EX) 'F)
                           (SEARCH_LI2 (AEVAL EX) 'B)))
           (PROG (H)
             (SETQ H FT)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H) (SETQ SB (CONS (CONS H (LIST 'TIMES H GS)) SB)))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           (SETQ H (CAR (SIMP (LIST '*SQ (SUBSQ (SIMP (AEVAL EX)) SB) NIL))))
           (SETK 'MAX_DEG
                 (COND ((NEQ (CAAAR H) GS) 0)
                       (T
                        (CDAAR
                         (CAR
                          (SIMP
                           (LIST '*SQ (SUBSQ (SIMP (AEVAL EX)) SB) NIL)))))))
           (SETKORDER K))))
        (COND
         ((BOOLVALUE*
           (REVALX
            (LIST 'IS_FERMIONIC
                  (LIST '*SQ (CONS (FIRST_TERM_SF (CAR (SIMP (REVALX EX)))) 1)
                        T))))
          (PROGN
           (SETQ SSP
                   (AEVAL
                    (LIST 'GENSSPOLY N
                          (LIST 'LIST (LIST 'LIST WFLIST WBLIST TW)) 'R
                          (LIST 'LIST 'FONLY))))
           (SETQ FIELDANS (AEVAL (LIST 'FIRST SSP)))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ SSP
                   (AEVAL
                    (LIST 'GENSSPOLY N
                          (LIST 'LIST (LIST 'LIST WFLIST WBLIST TW)) 'R
                          (LIST 'LIST 'BONLY))))
           (SETQ FIELDANS (AEVAL (LIST 'SECOND SSP)))
           (AEVAL 'NIL))))
        (SETQ ANS (AEVAL (LIST 'TOCOO N NF NB FIELDANS)))
        (SETQ RLIST (AEVAL (LIST 'THIRD SSP)))
        (SETQ INDEPV
                (AEVAL
                 (LIST 'APPEND (LIST 'LIST 'X 'T)
                       (PROG (H FORALL-RESULT FORALL-ENDPTR)
                         (SETQ H 1)
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) H))
                           (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS (AEVAL* (LIST 'TH H)) NIL)))
                        LOOPLABEL
                         (SETQ H
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  H))
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) H))
                           (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS (AEVAL* (LIST 'TH H)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
        (SETQ PRINT_ NIL)
        (SETQ EQNLI
                (AEVAL
                 (LIST 'SPLIT_SIMP (LIST 'LIST (LIST 'DIFFERENCE ANS EX))
                       (LIST 'LIST) RLIST INDEPV 'T)))
        (SETQ SOLU (AEVAL (LIST 'CRACK EQNLI (LIST 'LIST) RLIST (LIST 'LIST))))
        (RETURN
         (COND
          ((EVALEQUAL (AEVAL SOLU) (AEVAL (LIST 'LIST)))
           (PROGN
            (PROGN
             (PRIN2 "The expression to be transformed has a homogeneity")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "but a field form does not exist.") NIL)
            (TERPRI)))
          (T
           (PROGN
            (SETQ SOLU (AEVAL (LIST 'SECOND (LIST 'FIRST SOLU))))
            (AEVAL (LIST 'SUB SOLU FIELDANS)))))))))) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'NUMBER-OF-ARGS 0) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'DEFINED-ON-LINE '3048) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESTORE_INTERACTIVE_PROMPT NIL
    (PROG (*REDEFMSG *USERMODE) (COPYD 'UPDATE_PROMPT 'RESTORE_UPDATE_PROMPT))) 
(FLUID '(PROMPTSTRING*)) 
(PUT 'CHANGE_PROMPT 'NUMBER-OF-ARGS 0) 
(PUT 'CHANGE_PROMPT 'DEFINED-ON-LINE '3057) 
(PUT 'CHANGE_PROMPT 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CHANGE_PROMPT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHANGE_PROMPT NIL
    (PROG (*USERMODE)
      (COND ((NULL PROMPTSTRING*) (SETQ PROMPTSTRING* "")))
      (SETPCHAR PROMPTSTRING*)
      (SETQ PROMPTEXP* PROMPTSTRING*))) 
(PUT 'CHANGE_PROMPT_TO 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE_PROMPT_TO 'DEFINED-ON-LINE '3066) 
(PUT 'CHANGE_PROMPT_TO 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CHANGE_PROMPT_TO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE_PROMPT_TO (U)
    (PROG (OLDPROMPT *REDEFMSG *USERMODE)
      (SETQ OLDPROMPT PROMPTSTRING*)
      (SETQ PROMPTSTRING* U)
      (COPYD 'RESTORE_UPDATE_PROMPT 'UPDATE_PROMPT)
      (COPYD 'UPDATE_PROMPT 'CHANGE_PROMPT)
      (UPDATE_PROMPT)
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN OLDPROMPT))) 
(PUT 'CNT_L_ 'NUMBER-OF-ARGS 0) 
(PUT 'CNT_L_ 'DEFINED-ON-LINE '3079) 
(PUT 'CNT_L_ 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'CNT_L_ 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CNT_L_ NIL
    (COND
     ((GEQ LINES_WRITTEN 10)
      (PROGN
       (CHANGE_PROMPT_TO "")
       (PROGN
        (PRIN2 "                                   Press Enter to continue ")
        NIL)
       (RESTORE_INTERACTIVE_PROMPT)
       (RDS NIL)
       (WRS NIL)
       (READCH)
       (COND (IFL* (RDS (CADR IFL*))))
       (COND (OFL* (WRS (CDR OFL*))))
       (SETQ LINES_WRITTEN 0))))) 
(PUT 'WL1 'NUMBER-OF-ARGS 1) 
(PUT 'WL1 'DEFINED-ON-LINE '3093) 
(PUT 'WL1 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'WL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WL1 (L)
    (PROGN
     (PROGN (PRIN2 L) NIL)
     (TERPRI)
     (CNT_L_)
     (SETQ LINES_WRITTEN (PLUS LINES_WRITTEN 1)))) 
(PUT 'WL2 'NUMBER-OF-ARGS 1) 
(PUT 'WL2 'DEFINED-ON-LINE '3095) 
(PUT 'WL2 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'WL2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WL2 (L)
    (PROGN
     (PROGN (PRIN2 L) NIL)
     (TERPRI)
     (TERPRI)
     (CNT_L_)
     (SETQ LINES_WRITTEN (PLUS LINES_WRITTEN 2)))) 
(PUT 'SSHELP1 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP1 'DEFINED-ON-LINE '3100) 
(PUT 'SSHELP1 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP1 NIL
    (PROGN
     (WL2 "Purpose: ")
     (WL1 "to determine evolutionary supersymmetric PDEs with higher")
     (WL1 "symmetries, to compute symmetries and conservation laws for")
     (WL1 "given supersymmetric PDEs, or to do any other algebra or")
     (WL2 "differentiations of polynomial supersymmetric expressions.")
     NIL)) 
(PUT 'SSHELP2 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP2 'DEFINED-ON-LINE '3111) 
(PUT 'SSHELP2 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP2 NIL
    (PROGN
     (WL2 "Notation: ")
     (WL1 "Fields f(1),f(2),.. are treated as fermionic, and b(1),.. are")
     (WL1 "treated as bosonic. The independent variables are t, x and in")
     (WL1 "symmetry computations the symmetry `time' variable is s. ")
     (WL2 "Further fermionic variables can be defined, for example, theta.")
     (WL1 "The super derivatives D_i which satisfy (D_i)**2 = d/dx ")
     (WL2 "are implemented as d(i, .. ) .  ")
     (WL1 "When refering to homogeneity weights, all weights are scaled")
     (WL1 "such that the weight of d(i, .. ) is 1 and thus the weight")
     (WL2 "of df( .. , x) is two, i.e. twice the usual value. ")
     NIL)) 
(PUT 'SSHELP3 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP3 'DEFINED-ON-LINE '3129) 
(PUT 'SSHELP3 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP3 NIL
    (PROGN
     (WL2 "Initializations: ")
     (WL1
      "Before starting super-computations the number N of superfields theta_i")
     (WL1
      "needs to be known. Also the number nb of boson fields and the nf of")
     (WL1
      "fermion fields is needed to have compact printing on the screen (to")
     (WL1
      "avoid printing the index 1 if nf=1 or nb=1). The input of N,nf,nb is")
     (WL1 "done either through a call of the procedure ssym (to compute")
     (WL1
      "symmetries) or of the procedure ssconl (to compute conservation laws)")
     (WL1 "or with a call of procedure ssini. This help item describes ssini.")
     (WL2 "It's format is:")
     (WL2 "  ssini(N,nf,nb)$")
     (WL2 "where")
     (WL1 " N      .. the number of superfields theta_i")
     (WL1 " nf     .. number of fermion fields f(1) .. f(nf)")
     (WL2 " nb     .. number of boson   fields b(1) .. b(nb)")
     (WL2 "-----------------")
     (WL2 "Example: For N=1 and 2 fermion and 2 boson fields we do")
     (WL2 "ssini(1,2,2)$ ")
     (WL2 "and verify easily that fermions anticommute: ")
     (WL1 "f(1)**2; ")
     (WL2 "f(1)*f(2)+f(2)*f(1);")
     (WL2 "that bosons commute:")
     (WL1 "b(1)**3;")
     (WL2 "b(1)*b(2)-b(2)*b(1);")
     (WL2 "that x,t-differentiations by default do not change parity")
     (WL1 "df(b(1),t)**2;")
     (WL2 "df(b(1),x)**2;")
     (WL2 "unless one explicitly defines d/dt to be parity changing through")
     (WL2 "on t_changes_parity$")
     (WL2 "demonstrated by")
     (WL2 "df(b(1),t)**2;")
     (WL1 "which is also possible to define for the symmetry variable s ")
     (WL1 "(through       on s_changes_parity$             , see below)")
     (WL2 "but not for x. For the remainder of this demo we change back")
     (WL1 "off t_changes_parity$")
     (WL2 "df(b(1),t)**2;")
     (WL1 "Differentiations d/dt, d/dx, D_i follow the supersymmetric")
     (WL2 "product rule:")
     (WL1 "df(b(1)*f(2),x);")
     (WL1 "d(1,b(1)*f(2));")
     (WL1 "df(f(1)*f(2),x);")
     (WL2 "d(1,f(1)*f(2));")
     (WL2 "furthermore D_i changes parity:")
     (WL1 "d(1,f(1))**2;")
     (WL2 "d(1,b(1))**2;")
     (WL2 "and satisfies  D^2 = d/dx:")
     (WL2 "d(1,d(1,b(1)));")
     (WL1 "To compactify output, in the special case N=1 the index of D_i")
     (WL1 "is not printed as seen above. Similarly, indices of f(1), b(1)")
     (WL2
      "are not printed if we have only one fermion and/or one boson field:")
     (WL1 "ssini(2,1,2)$")
     (WL2 "f(1)*b(1)*d(1,b(2));")
     (WL1 "Any initializations made by ssini are overwritten by a call to the")
     (WL2 "procedures ssym or ssconl.")
     NIL)) 
(PUT 'SSHELP4 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP4 'DEFINED-ON-LINE '3216) 
(PUT 'SSHELP4 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP4 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP4 NIL
    (PROGN
     (WL2 "Coefficients: ")
     (WL1 "coeffn is a standard REDUCE command although its implementation")
     (WL1 "(at least in REDUCE distributions up to version 3.8) does not work")
     (WL1
      "with non-commuting variables. By loading SsTools some of the REDUCE")
     (WL1
      "routines are re-defined so that coeffn can be used for supersymmetric")
     (WL2
      "expressions. The call is unchanged from the standard REDUCE command:")
     (WL2 "coeffn(ex,h,n)$")
     (WL1 "which computes the coefficient of h**n in the expression ex which")
     (WL1 "is supposed to be polynomial in h. The returned result is the")
     (WL1 "coefficient AFTER h**n has been moved to be the first factor in")
     (WL2 "each term of ex. This matters if h is fermionic and thus n=1.")
     (WL2 "Example:")
     (WL1 "SSIni(2,0,2)$")
     (WL1 "ex:=D(2,b(1))*D(1,df(b(2),x,2));")
     (WL2 "coeffn(ex,D(1,df(b(2),x,2)),1);")
     (WL1
      "Because D_2 b(1) and D_1 D_x D_x b(2) are fermionic a minus sign is")
     (WL1 "introduced when making D_1 D_x D_x b(2) the first factor in the")
     (WL2 "product. This explains the minus in the result of the coeffn call.")
     (WL1 "NOTE: When using SsTools the REDUCE command noncom has no effect,")
     (WL1
      "i.e. it is not possible to define additional noncommuting quantities")
     (WL2
      "besides fermionic ones which are introduced by the command fermionic.")
     NIL)) 
(PUT 'SSHELP5 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP5 'DEFINED-ON-LINE '3250) 
(PUT 'SSHELP5 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP5 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP5 NIL
    (PROGN
     (WL2 "Computing symmetries:")
     (WL1 "To compute PDEs together with higher symmetries, or to")
     (WL2 "compute higher symmetries for a given PDE(-system) the call is:")
     (WL2 "  ssym(N,tw,sw,afwlist,abwlist,eqnlist,fl,inelist,mode)$")
     (WL2 "where")
     (WL1 " N       .. the number of superfields theta_i")
     (WL1
      " tw      .. 2 times the differential order of the equation = weight(t)")
     (WL1
      " sw      .. 2 times the differential order of the symmetry = weight(s)")
     (WL1 " afwlist .. (algebraic mode) list of weights of the fermion fields")
     (WL1 "            f(1), f(2), ... . The number of elements of this list")
     (WL1 "            determines the number of fermion fields. ")
     (WL1 " abwlist .. (algebraic mode) list of weights of the boson fields")
     (WL1 "            b(1), b(2), ... . The number of elements of this list")
     (WL1 "            determines the number of boson fields. ")
     (WL1
      " eqnlist .. - in the nonlinear case a list of extra conditions on the")
     (WL1
      "              undetermined coefficients where conditions a3=.. are executed")
     (WL1
      "              instantly. These and any expressions are added to equations")
     (WL1 "              when calling crack")
     (WL1
      "            - in the linear case the system in form of replacements => and")
     (WL1 "              the linearized system in form of equations = .")
     (WL1 " fl      .. extra unknowns in eqnlist to be determined")
     (WL1
      " inelist .. a list, each element of it is a list with at least one of")
     (WL1 "            its elements being non-zero")
     (WL1 " mode    .. list of flags: ")
     (WL1 "            init: only initialization of global data")
     (WL1
      "            plain_com : direct computation of the commutator (for tests)")
     (WL1
      "            power_split_com : alternatice power splitting of commutator,")
     (WL1
      "                             (not if substitutions '=>' are present,")
     (WL1 "                              see below)")
     (WL1
      "            zerocoeff: all coefficients = 0 which do not appear in inelist")
     (WL1 "            filter: extra homogeneity weights as given in the list")
     (WL1 "                    hom_wei have to be satisfied by the symmetry")
     (WL1
      "            lin: find symmetry that is linear homogeneous in all fields ")
     (WL1 "                 with (index)>(maxindex/2), i.e. if lin then the")
     (WL1
      "                 number of fermions and boson fields must both be even")
     (WL1 "            tpar: t/nil, whether time variable t changes parity")
     (WL2
      "            spar: t/nil, whether symmetry variable s changes parity")
     (WL2 "-----------------")
     (WL1
      "The question whether ssym is to be used to compute a PDE(-system) with")
     (WL1 "symmetries, or to compute symmetries for a `given' PDE(-system) is")
     (WL1 "decided based on the form of the input eqnlist. ")
     (WL1 "If symmetries of a specific weight for a given PDE(-system) are to")
     (WL1 "be computed then eqnlist has to have the form")
     (WL1 "{df(f(1),t)=..., df(b(1),t)=...} with as many t-derivatives")
     (WL1 "of fermion fields as there are elements in afwlist and as many")
     (WL2 "t-derivatives of boson fields as there are elements in abwlist.")
     (WL1 "If the right hand sides of the PDE(s) in eqnlist involve any ")
     (WL1 "constants which are to be computed then these constants have to ")
     (WL1
      "be listed in the input list fl, like {p1,p2,...}. If these constant ")
     (WL1
      "coefficients are not given in the list fl then they are treated as ")
     (WL1 "independent parameters and symmetries are determined which are ")
     (WL1 "symmetries for generic values of these constant coefficients.")
     (WL1 "Also, when computing symmetries for a given PDE(-system) then ")
     (WL1 "dependencies of all the fields on t,x has to be declared")
     (WL1
      "beforehand in order to be able to input derivatives, like df(f(1),t) ")
     (WL2 "which otherwise would be automatically evaluated to zero.")
     (WL1 "If the input list eqnlist does not start with {df(..)=..} then")
     (WL1 "an ansatz for the PDE(-system) is generated and this system and")
     (WL1 "its symmetries are computed. In this case eqnlist can contain")
     (WL1 "substitutions, like p2=0, or expressions which are to be set to")
     (WL1 "zero, like p3*r4+p2*r3. Typically one would run the program first ")
     (WL1 "to see which ansatz for the PDE(-system) and the symmetry is ")
     (WL1 "generated and then start ssym again with the intended extra")
     (WL2 "conditions.")
     (WL1 "If a boson weight is non-positive or a fermion weight is negative")
     (WL1
      "then the global (algebraic mode) variable max_deg must have a positive")
     (WL2
      "integer value which is the highest degree of such a field in any ansatz.")
     (WL1
      "When determining symmetries (either for a given system of equations,")
     (WL1
      "or when determining equations and symmetries in one run) one has two")
     (WL1
      "cases: whether the symmetry variable s flips parity, or not, in other")
     (WL1
      "words, whether s itself is fermionic or bosonic. This has nothing to do")
     (WL1 "with the question whether s has an odd or even weight.")
     (WL1
      "Similarly, when determining equations and symmetries in one computation")
     (WL1
      "one can consider the two cases whether the `time' derivative changes")
     (WL1
      "parity or not. A parity changing t is specified through the flag `tpar'")
     (WL2 "and a parity changing s through the flag `spar'.")
     (WL2 "-----------------")
     (WL2 "Example for determining PDEs + symmetries:")
     (WL2 "ssym(1,4,5,{2},{2},{},{},{},{})$ ")
     (WL2 "Example for determining the symmetries of a given PDE-system:")
     (WL1 "ssym(1,4,5,{2},{2},")
     (WL1 "     {df(f(1),t)=df(f(1),x)*b(1)*p9,")
     (WL1 "      df(b(1),t)=df(b(1),x)*b(1)*p9 + df(f(1),x)*f(1)*p4},")
     (WL2 "     {},{},{})$ ")
     (WL2 "-----------------")
     (WL1 "Example for determining symmetries of given PDEs in the")
     (WL1 "presence of extra substitution rules which are given")
     (WL1 "using `=>' instead of `=' for the equations for which")
     (WL1 "matching symmetries are to be found:")
     (WL1 "lisp put('f,'prifn,nil)$  % from now on more than one fermion ")
     (WL1 "lisp put('b,'prifn,nil)$  % from now on more than one boson ")
     (WL1 "ssym(1,1,4,{1,1},{1,3,1,3}, ")
     (WL1 "     {df(f(1),t)=>-2*f(1)*b(1)*p1,  ")
     (WL1 "      df(b(1),t)=>b(1)**2*p1+d(1,f(1))*p2,  ")
     (WL1 "      d(1,b(2)) =>-df(b(1),x)*f(1),  ")
     (WL1 "      df(b(2),t)=>-2*d(1,b(1))*f(1)*b(1)*p1+d(1,df(b(1),t))*f(1)")
     (WL1 "                  -d(1,f(1))**2*p2/2-d(1,f(1))*b(1)**2*p1")
     (WL1 "                  +d(1,f(1))*df(b(1),t),")
     (WL1 "      df(f(2),t)=-2*f(2)*b(1)*p1-2*f(1)*b(3)*p1,  ")
     (WL1 "      df(b(3),t)=2*b(1)*b(3)*p1+d(1,f(2))*p2,  ")
     (WL1 "      d(1,b(4))=>-df(b(3),x)*f(1)-df(b(1),x)*f(2),  ")
     (WL1
      "      df(b(4),t)=>-2*d(1,b(3))*f(1)*b(1)*p1-2*d(1,b(1))*f(2)*b(1)*p1")
     (WL1 "                  -2*d(1,b(1))*f(1)*b(3)*p1+d(1,df(b(3),t))*f(1) ")
     (WL1 "                  +d(1,df(b(1),t))*f(2)-d(1,f(1))*d(1,f(2))*p2 ")
     (WL1 "                  -d(1,f(2))*b(1)**2*p1-2*d(1,f(1))*b(3)*b(1)*p1")
     (WL1 "                  +d(1,f(2))*df(b(1),t)+d(1,f(1))*df(b(3),t)},")
     (WL2 "     {},{},{});  ")
     (WL2 "-----------------")
     (WL1 "The same example again but now with the flag `lin' to generate a")
     (WL1 "symmetry that is linear homogeneous in all fields with an index")
     (WL1 "greater than half the maximum index for that type of field. When ")
     (WL1
      "setting the flag 'lin' the number of fermions and boson fields must")
     (WL1
      "both be even. In the following example that means, the symmetry will")
     (WL2 "be linear and homogeneous in f(2),b(3),b(4).")
     (WL1 "ssym(1,1,4,{1,1},{1,3,1,3},  ")
     (WL1 "     {df(f(1),t)=>-2*f(1)*b(1)*p1,  ")
     (WL1 "      df(b(1),t)=>b(1)**2*p1+d(1,f(1))*p2,  ")
     (WL1 "      d(1,b(2)) =>-df(b(1),x)*f(1),  ")
     (WL1 "      df(b(2),t)=>-2*d(1,b(1))*f(1)*b(1)*p1+d(1,df(b(1),t))*f(1)")
     (WL1 "                  -d(1,f(1))**2*p2/2-d(1,f(1))*b(1)**2*p1")
     (WL1 "                  +d(1,f(1))*df(b(1),t),")
     (WL1 "      df(f(2),t)=-2*f(2)*b(1)*p1-2*f(1)*b(3)*p1,  ")
     (WL1 "      df(b(3),t)=2*b(1)*b(3)*p1+d(1,f(2))*p2,  ")
     (WL1 "      d(1,b(4)) =>-df(b(3),x)*f(1)-df(b(1),x)*f(2),  ")
     (WL1
      "      df(b(4),t)=>-2*d(1,b(3))*f(1)*b(1)*p1-2*d(1,b(1))*f(2)*b(1)*p1")
     (WL1 "                  -2*d(1,b(1))*f(1)*b(3)*p1+d(1,df(b(3),t))*f(1) ")
     (WL1 "                  +d(1,df(b(1),t))*f(2)-d(1,f(1))*d(1,f(2))*p2")
     (WL1 "                  -d(1,f(2))*b(1)**2*p1-2*d(1,f(1))*b(3)*b(1)*p1")
     (WL1 "                  +d(1,f(2))*df(b(1),t)+d(1,f(1))*df(b(3),t)},")
     (WL2 "     {},{},{lin});  ")
     (WL2 "-----------------")
     (WL1
      "If one wants to find for a given system of equations df(..,t)=.. a ")
     (WL1
      "symmetry which satisfies in addition to a symmetry weight sw, a list ")
     (WL1
      "of fermion weights afwlist and list of boson weights abwlist other ")
     (WL1
      "homogeneities, then in the last parameter of the ssym call one adds the ")
     (WL1
      "flag `filter' and has a global variable hom_wei which is a list of ")
     (WL1
      "lists {sw,afwlist,abwlist} each being an additional set of homogeneity")
     (WL2 "weights.")
     (WL2 "Example: ")
     (WL2 "hom_wei:={{10,{3,3},{2,2}}}$")
     (WL1 "ssym(1,1,6,{1,1},{1,1},")
     (WL1 "     {df(f(1),t)=>-2*f(1)*b(1)*p1,")
     (WL1 "      df(b(1),t)=>b(1)*b(1)*p1+d(1,f(1))*p2,")
     (WL1 "      df(f(2),t)= - 2*f(2)*b(1)*p1 - 2*f(1)*b(2)*p1,")
     (WL1 "      df(b(2),t)=2*b(2)*b(1)*p1 + d(1,f(2))*p2},")
     (WL2 "     {},{},{lin,filter});")
     (WL2 "-----------------")
     (WL1
      "After a run of ssym() the substitution rules, like df(f(1),t)=> ..  ")
     (WL1 "are not active. If one wanted to check symmetries interactively  ")
     (WL1 "one would have to activate these rules which are stored under the")
     (WL1 "name `subli' by:                                    let subli$")
     (WL1
      "and if one would want to de-activate them then do:  clearrules subli$ ")
     (WL2
      "For example, to check symmetries after the above run one could do  ")
     (WL2 " let subli$  ")
     (WL1 " df(b(3),t):=2*b(3)*b(1)*p1 + d(1,f(2))*p2$  ")
     (WL2 " df(f(2),t):=- 2*f(2)*b(1)*p1 - 2*f(1)*b(3)*p1$  ")
     (WL1 " b3t:=df(b(3),t)$  ")
     (WL2 " f2t:=df(f(2),t)$  ")
     (WL1
      " df(b(3),s):=(1/2*d(1,b(3))*f(1)*b(1)**2*p1*p2 + 4/11*d(1,b(1))*f(2)  ")
     (WL1
      "  *b(1)**2*p1*p2 + d(1,b(1))*f(1)*b(3)*b(1)*p1*p2 + 7/22*d(1,f(1))**2 ")
     (WL1
      "  *b(3)*p2**2 + 7/11*d(1,f(1))*b(3)*b(1)**2*p1*p2 + 1/4*d(1,f(1))*  ")
     (WL1
      "  d(1,b(3))*f(1)*p2**2 + 5/44*d(1,f(1))*d(1,b(1))*f(2)*p2**2 + 1/44*  ")
     (WL1 "  df(b(1),x)*f(2)*f(1)*p2**2 + 1/22*df(f(1),x)*f(2)*b(1)*p2**2  ")
     (WL1 "  + 5/22*df(f(1),x)*f(1)*b(3)*p2**2)/(p1*p2)$  ")
     (WL1 " df(f(2),s):=(1/4*d(1,f(2))*d(1,f(1))*f(1)*p2**2 + 1/2*d(1,f(2))  ")
     (WL1
      "  *f(1)*b(1)**2*p1*p2 + 3/44*d(1,f(1))**2*f(2)*p2**2 + 3/22*d(1,f(1)) ")
     (WL2
      "  *f(2)*b(1)**2*p1*p2 + 1/44*df(f(1),x)*f(2)*f(1)*p2**2)/(p1*p2)$  ")
     (WL1 " b3s:=df(b(3),s)$  ")
     (WL2 " f2s:=df(f(2),s)$  ")
     (WL1 " write \"ZERO = \",df(b3t,s)-df(b3s,t)$  ")
     (WL2 " write \"ZERO = \",df(f2t,s)-df(f2s,t)$  ")
     (WL2 " clearrules subli$ ")
     NIL)) 
(PUT 'SSHELP6 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP6 'DEFINED-ON-LINE '3466) 
(PUT 'SSHELP6 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP6 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP6 NIL
    (PROGN
     (WL2 "To compute conservation laws the call is:")
     (WL2 "  ssconl(N,tw,mincw,maxcw,afwlist,abwlist,pdes)$")
     (WL2 "where")
     (WL1 " N       .. the number of superfields theta_i")
     (WL1
      " tw      .. weight(d_t) = 2 times the differential order of the equation")
     (WL1 " mincw   .. min weight of the conservation law")
     (WL1 " maxcw   .. max weight of the conservation law")
     (WL1 " afwlist .. (algebraic) list of weights of the fermion fields")
     (WL1 "            f(1), f(2), ... ")
     (WL1 " abwlist .. (algebraic) list of weights of the boson fields")
     (WL1 "            b(1), b(2), ...")
     (WL1
      " pdes    .. an algebraic list of the pde(s) for which a conservation")
     (WL1 "            law is to be found, for example")
     (WL1 "            {df(f(1),t)=df(f(1),x)*b(1)*p9,")
     (WL1 "             df(b(1),t)=b(1)**3*p3 + d(1,f(1))**2*p2 + ")
     (WL2 "                        df(b(1),x)*b(1)*p9 + df(f(1),x)*f(1)*p4}$")
     (WL1 "The number of elements in afwlist and abwlist ")
     (WL1
      "determines the number of fermion and boson fields. All weights are ")
     (WL1 "based on weight(x)=2.")
     (WL1 "Important: All fields must be made dependent on x and t before.")
     (WL2 "Fermion fields are called f(1),f(2),.. and boson fields b(1),.. .")
     (WL1 "If a boson weight is non-positive or a fermion weight is negative")
     (WL1
      "then the global (algebraic mode) variable max_deg must have a positive")
     (WL1
      "integer value which is the highest degree of such a field or any of")
     (WL2 "its derivatives in any ansatz.")
     (WL2 "-----------------")
     (WL2 "Example for determining conservation laws for a given PDE-system:")
     (WL1 "ssconl(1,4,10,15,{2},{2},")
     (WL1 "       {df(f(1),t)=df(f(1),x)*b(1)*p9,")
     (WL1
      "        df(b(1),t)=b(1)**3*p3 + d(1,f(1))**2*p2 + df(b(1),x)*b(1)*p9 ")
     (WL1 "                   + df(f(1),x)*f(1)*p4}")
     (WL2 "      );")
     NIL)) 
(PUT 'SSHELP7 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP7 'DEFINED-ON-LINE '3511) 
(PUT 'SSHELP7 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP7 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP7 NIL
    (PROGN
     (WL1 "To compute all possible homogeneities of a list of expressions")
     (WL1 "or equations, i.e. to determine for each homogeneity the weight")
     (WL2 "of d_t, of fields f(i),b(j) and other constants, the call is:")
     (WL2 "  FindSSWeights(N,nf,nb,exli,zerowei,verbose)$")
     (WL2 "where")
     (WL1 " N       .. the number of superfields theta_i")
     (WL1 " nf      .. number of fermion fields f(1) .. f(nf)")
     (WL1 " nb      .. number of boson   fields b(1) .. b(nb)")
     (WL1 " exli    .. list of equations or expressions")
     (WL1 " zerowei .. list of constants or other kernels that")
     (WL1 "            should have zero weight")
     (WL2 " verbose .. =t/nil whether detailed comments shall be made")
     (WL1
      "Weights are scaled such that weight of d_x is 2, i.e. the weight of")
     (WL2
      "any D is 1. Input expressions can be in field form or coordinate form.")
     (WL1 "The program returns a list of homogeneities,")
     (WL1 "each homogeneity being a list of")
     (WL1 "a list of the weights of f(1),b(2),.. and")
     (WL1 "a list of the weights of b(1),b(2),.. and")
     (WL2 "a list of the weights of equations/expressions in the input. ")
     (WL2 "-----------------")
     (WL1 "Example for determining all possible weights for a given")
     (WL2 "(artificial) system of equations: ")
     (WL1 "- th(i) denotes superfields theta_i,")
     (WL1 "- b's and f's with 3 indices are the coefficients in the")
     (WL1 "  Taylor expansions")
     (WL1 "  f(i)=f(i,0,0)+b(i,1,0)*th(1)+b(i,0,1)*th(2)+f(i,1,1)*th(1)*th(2)")
     (WL1 "- x,t,th(i) may occur not only as differentiation variables")
     (WL1 "  but also explicitly polynomially,")
     (WL2 "- p3,p5 are supposed to have zero weight.")
     (WL1 "FindSSWeights(2,2,0,")
     (WL1 "              {x*df(b(1,1,0),th(1),t)= d(1,d(2,f(2,0,0)))*p5 - ")
     (WL1
      "                                       df(f(1,1,1)*th(1)*th(2),x,2)*p3,")
     (WL1
      "               df(f(2),t)=(df(f(2),x,2)*p3*p5 - df(f(1),x,3)*p3**2)/p5},")
     (WL1 "              {p3,p5},")
     (WL2 "              t)$")
     NIL)) 
(PUT 'SSHELP8 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP8 'DEFINED-ON-LINE '3561) 
(PUT 'SSHELP8 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP8 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP8 NIL
    (PROGN
     (WL1 "To compute a linearization of a system of evolution equations,")
     (WL2 "the call is:")
     (WL2 "  linearize(pdes,nf,nb,tpar,spar)$")
     (WL2 "where")
     (WL1 " pdes   .. list of equations with a t-derivative on left hand side")
     (WL1 " nf     .. number of fermion fields f(1) .. f(nf)")
     (WL1 " nb     .. number of boson   fields b(1) .. b(nb)")
     (WL1 " tpar   .. t/nil, whether time variable t changes parity")
     (WL2 " spar   .. t/nil, whether symmetry variable s changes parity")
     (WL1 " linearize returns {list of relations like df(f(3),s) = f(6),")
     (WL1 "                    list of linearized equations}")
     (WL2 "Newly introduced fields depend on x,t,s.")
     (WL1 "A linearization is associated with a symmetry D_s. The linearized")
     (WL1
      "equation depends on whether s is parity changing or not and furthermore")
     (WL1
      "if s is parity changing then the sign of the lhs of the equation (or,")
     (WL2
      "equally of the rhs) depends on whether t is parity changing or not.")
     (WL2 "-----------------")
     (WL2 "An (artificial) example for a linearization:")
     (WL1 "linearize({df(f(1),t)=df(f(2),x)*b(1)**3*p1 ")
     (WL1 "                      + d(1,f(2))**5*df(f(1),x,2)*p2,")
     (WL1
      "           df(f(2),t)=2*d(1,df(b(1),x))*df(f(2),x,2)*df(f(1),x)*p3 ")
     (WL1 "                      + df(f(1),x,3)*p3**2,")
     (WL2 "           d(1,d(2,b(1)))=b(1)*d(1,d(2,f(1)))*f(1) },2,1,nil,nil)$")
     NIL)) 
(PUT 'SSHELP9 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP9 'DEFINED-ON-LINE '3597) 
(PUT 'SSHELP9 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP9 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP9 NIL
    (PROGN
     (WL1 "To make an ansatz for a polynom in a number of fermionic and/or")
     (WL1 "bosonic variables, and their x-derivatives and D(i,..) derivatives")
     (WL2 "The call is:")
     (WL2 "  GenSSPoly(N,wgtlist,cname,mode)$")
     (WL2 "where")
     (WL1 " N       .. the number of superfields theta_i")
     (WL1 " wgtlist .. an algebraic mode list of at least one alg. mode list")
     (WL1 "            {afwlist,abwlist,wgt} where")
     (WL1 "            afwlist .. (algebraic mode) list of weights of the")
     (WL1 "                       fermion fields f(1), f(2), ...")
     (WL1 "            abwlist .. (algebraic mode) list of weights of the")
     (WL1 "                       boson   fields b(1), b(2), ...")
     (WL1 "            wgt     .. the weight of each term in the generated")
     (WL1 "                       polynomial according to afwlist and abwlist")
     (WL1 "            The number of elements in afwlist, abwlist determines")
     (WL1 "            the number of fermion and boson fields.")
     (WL1 "            If the generated polynomial should have more than one")
     (WL1 "            homogeneity symmetry then each extra one is specified")
     (WL1 "            by another element {afwlist,abwlist,wgt} in wgtlist.")
     (WL1 " cname   .. name of the coefficients which gets added an index")
     (WL1 " mode    .. list of flags: ")
     (WL1
      "            lin: the generated polynomial has to be linear homogeneous")
     (WL1 "                 in all fields with (index)>(maxindex/2), i.e.")
     (WL1
      "                 if lin then the number of fermions and boson fields")
     (WL1 "                 must both be even")
     (WL1 "            fonly: only fermionic terms are generated")
     (WL1 "            bonly: only bosonic   terms are generated")
     (WL1 "            {forbid,d(1,f(1)),..}: a list of fermionic and bosonic")
     (WL1
      "                 variables and their derivatives that may not occur")
     (WL2 "The result is a list")
     (WL2 "{fermonic_polynomial, bosonic_polynomial, {coefficients}}.")
     (WL2 "Comments:")
     (WL1 "- The weight of d/dx is 2, the weight of d(i,..) is 1, no other")
     (WL2 "  derivatives occur.")
     (WL1
      "- If in the first homogeneity, i.e. in the first element of wgtlist")
     (WL1 "  a boson weight is non-positive or a fermion weight is negative")
     (WL1 "  then the global (algebraic mode) variable max_deg must have a")
     (WL1 "  positive integer value which is the highest degree of such a")
     (WL1 "  field in any ansatz. Therefore if there is a set of strictly")
     (WL1
      "  positive homogeneity weights, then they should come first, needing")
     (WL2
      "  no extra restriction through max_deg which might restrict generality.")
     NIL)) 
(PUT 'SSHELP10 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP10 'DEFINED-ON-LINE '3653) 
(PUT 'SSHELP10 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP10 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP10 NIL
    (PROGN
     (WL1
      "The N symmetry generators theta_1,..,theta_N are called th(1),..,th(N)")
     (WL1
      "in SSTools. As they are of odd parity only 2^N many products of their")
     (WL1 "powers exist. The name convention used in SSTools for coefficients")
     (WL1
      "in the Taylor expansion wrt. theta_i becomes apparent in the following")
     (WL2 "two expansions (here for N=2):")
     (WL1 "f(i)=f(i,0,0)+b(i,1,0)*th(1)+b(i,0,1)*th(2)+f(i,1,1)*th(1)*th(2)")
     (WL2 "b(i)=b(i,0,0)+f(i,1,0)*th(1)+f(i,0,1)*th(2)+b(i,1,1)*th(1)*th(2).")
     (WL2 "The theta expansion of derivatives D(i,P) is")
     (WL2 "D(i,P) = df(P,th(i))+th(i)*df(P,x)} .")
     (WL1 "These expansions can be used to convert a polynomial expression in")
     (WL1 "terms of f(i), b(j), D(k,..) (what we call `field form') into what")
     (WL2 "we call `coordinate form'. The call is:")
     (WL2 "  tocoo(N,nf,nb,ex)$")
     (WL2 "where")
     (WL1 " N      .. the number of superfields th(1) .. th(N)")
     (WL1 " nf     .. number of fermion fields f(1) .. f(nf)")
     (WL1 " nb     .. number of boson   fields b(1) .. b(nb)")
     (WL2 " ex     .. the expression to be converted")
     (WL2 "-----------------")
     (WL2 "Example: For N=2 and 2 fermion fields the call is")
     (WL1 "    hh := tocoo(2,2,0,")
     (WL1 "                f(1)*d(1,f(2))+d(2,d(1,DF(f(2),x)))+")
     (WL2 "                df(f(2),x,2)*p3*p5 - df(f(1),x,3)*p3**2);")
     NIL)) 
(PUT 'SSHELP11 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP11 'DEFINED-ON-LINE '3691) 
(PUT 'SSHELP11 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP11 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP11 NIL
    (PROGN
     (WL1 "The conversion back from coordinate form to field form is done")
     (WL1 "by the function tofield. For this function to be applicable ")
     (WL1 "the expression in coordinate form has to be homogeneous with ")
     (WL1 "suitable weights and in the current form only involve x- and ")
     (WL2 "th(i)-derivatives. The call is:")
     (WL2 "  tofield(N,nf,nb,ex,zerowei)$")
     (WL2 "where")
     (WL1 " N       .. the number of superfields theta_i")
     (WL1 " nf      .. number of fermion fields f(1) .. f(nf)")
     (WL1 " nb      .. number of boson   fields b(1) .. b(nb)")
     (WL1 " ex      .. the expression to be converted")
     (WL1 " zerowei .. list of constants or other kernels that")
     (WL2 "            should have zero weight")
     (WL2 "-----------------")
     (WL1 "Examples:")
     (WL2 "The following call inverts the above computation:")
     (WL2 "    tofield(2,2,0,hh,{p3,p5});")
     (WL1 "The next call shows what happens when the input does")
     (WL2 "not have a homogeneity:")
     (WL2
      "    tofield(1,1,0,f(1,0)+df(f(1,0),x)+f(1,0)*th(1)*df(f(1,0),x),{});")
     (WL1 "In the next call the input has a homogeneity but the resulting")
     (WL1 "homogeneity weight for f(1) is negative which does not prevent")
     (WL1 "the computation but makes it more expensive but that does not")
     (WL2 "matter in the following short example:")
     (WL1 "    tofield(1,2,0,f(2,0)*b(1,1) + f(2,0)*th(1)*df(f(1,0),x) + ")
     (WL2
      "                  f(1,0) + th(1)*b(2,1)*b(1,1) + th(1)*b(1,1),{}); ")
     (WL1 "In the next call the input does have a homogeneity, the resulting")
     (WL2 "weight for f(1) is positive but still a field form does not exist:")
     (WL2 "    tofield(1,1,0,df(f(1,0),x)+f(1,0)*th(1)*df(f(1,0),x),{});")
     NIL)) 
(FLAG '(SSHELP) 'OPFN) 
(PUT 'SSHELP 'NUMBER-OF-ARGS 0) 
(PUT 'SSHELP 'DEFINED-ON-LINE '3738) 
(PUT 'SSHELP 'DEFINED-IN-FILE 'SSTOOLS/SSTOOLS.RED) 
(PUT 'SSHELP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SSHELP NIL
    (PROG (PS S)
      (SETQ LINES_WRITTEN 0)
      (RDS NIL)
      (WRS NIL)
      (SETQ PS PROMPTSTRING*)
      (SETQ PROMPTSTRING* (REDFRONT_COLOR ""))
      (PROG ()
       REPEATLABEL
        (PROGN
         (PROGN
          (PRIN2
           "To read about the following topics input the corresponding number:")
          NIL)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "  Purpose                        (1)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Notation                       (2)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Initializations                (3)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Coefficients                   (4)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Symmetries                     (5)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Conservation laws              (6)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Homogeneities                  (7)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Linearizations                 (8)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Generating polynomials         (9)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Taylor expansions             (10)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Inverting Taylor expansions   (11)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  Exit help                     (12)") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "Choice: ") NIL)
         (SETQ S (READ))
         (COND (IFL* (RDS (CADR IFL*))))
         (COND (OFL* (WRS (CDR OFL*))))
         (WL2 "==========================================================")
         (COND ((EQUAL S 1) (SSHELP1)) ((EQUAL S 2) (SSHELP2))
               ((EQUAL S 3) (SSHELP3)) ((EQUAL S 4) (SSHELP4))
               ((EQUAL S 5) (SSHELP5)) ((EQUAL S 6) (SSHELP6))
               ((EQUAL S 7) (SSHELP7)) ((EQUAL S 8) (SSHELP8))
               ((EQUAL S 9) (SSHELP9)) ((EQUAL S 10) (SSHELP10))
               ((EQUAL S 11) (SSHELP11))
               ((NEQ S 12) (PROGN (PRIN2 "Wrong input, try again.") NIL)))
         NIL)
        (COND ((NOT (EQUAL S 12)) (GO REPEATLABEL))))
      (SETQ PROMPTSTRING* PS))) 
(PRIN2 "For help type:  sshelp()$  ") 
NIL 
(TERPRI) 
(ENDMODULE) 
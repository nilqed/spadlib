(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_WEAKLYNL)) 
(PUT 'MK_WNLOP 'NUMBER-OF-ARGS 3) 
(PUT 'MK_WNLOP 'DEFINED-ON-LINE '42) 
(PUT 'MK_WNLOP 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'MK_WNLOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_WNLOP (C W N_TAIL)
    (PROG ()
      (MKOP C)
      (MKOP W)
      (PUT 'WNL_COEFF C T)
      (PUT 'WNL_NARG C N_TAIL)
      (PUT 'WNL_TAIL W T)
      (PUT 'WNL_NARG W N_TAIL)
      (COND
       ((NOT (FIXP N_TAIL))
        (REDERR "Error: number of tail vectors must be an integer"))))) 
(FLAG '(MK_WNLOP) 'OPFN) 
(PUT 'WNL_COEFFP 'NUMBER-OF-ARGS 1) 
(PUT 'WNL_COEFFP 'DEFINED-ON-LINE '66) 
(PUT 'WNL_COEFFP 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'WNL_COEFFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WNL_COEFFP (C) (GET 'WNL_COEFF C)) 
(PUT 'WNL_TAILP 'NUMBER-OF-ARGS 1) 
(PUT 'WNL_TAILP 'DEFINED-ON-LINE '69) 
(PUT 'WNL_TAILP 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'WNL_TAILP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WNL_TAILP (W) (GET 'WNL_TAIL W)) 
(PUT 'CDWNL2VEC 'NUMBER-OF-ARGS 5) 
(PUT 'CDWNL2VEC 'DEFINED-ON-LINE '72) 
(PUT 'CDWNL2VEC 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'CDWNL2VEC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CDWNL2VEC (CDL_OP C W LOC_ARG NLOC_ARG)
    (PROG (LARGCD NTARGET NTAIL TEMPLOC TEMPNLOC)
      (SETQ LARGCD (CADR (GET 'CDLARG CDL_OP)))
      (SETQ NTARGET (GET 'CDTARGET CDL_OP))
      (SETQ NTAIL (GET 'WNL_NARG W))
      (RETURN
       (PROG (I FORALL-RESULT FORALL-ENDPTR)
         (SETQ I 1)
         (COND ((MINUSP (DIFFERENCE LARGCD I)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (PROGN
                           (SETQ TEMPLOC
                                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ J 1)
                                     (COND
                                      ((MINUSP (DIFFERENCE NTARGET J))
                                       (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      (REVAL1
                                                       (LIST CDL_OP I J
                                                             (NTH LOC_ARG J))
                                                       NIL)
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ J (PLUS2 J 1))
                                     (COND
                                      ((MINUSP (DIFFERENCE NTARGET J))
                                       (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              (REVAL1
                                               (LIST CDL_OP I J
                                                     (NTH LOC_ARG J))
                                               NIL)
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                           (SETQ TEMPNLOC
                                   (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ A 1)
                                    STARTOVER
                                     (COND
                                      ((MINUSP (DIFFERENCE NTAIL A))
                                       (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (PROG (B FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ B 1)
                                               (COND
                                                ((MINUSP (DIFFERENCE NTAIL B))
                                                 (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                (REVAL1
                                                                 (LIST 'TIMES
                                                                       (LIST C
                                                                             A
                                                                             B)
                                                                       (LIST W
                                                                             I
                                                                             A)
                                                                       (NTH
                                                                        NLOC_ARG
                                                                        B))
                                                                 NIL)
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ B (PLUS2 B 1))
                                               (COND
                                                ((MINUSP (DIFFERENCE NTAIL B))
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        (REVAL1
                                                         (LIST 'TIMES
                                                               (LIST C A B)
                                                               (LIST W I A)
                                                               (NTH NLOC_ARG
                                                                    B))
                                                         NIL)
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-RESULT))
                                     (SETQ A (PLUS2 A 1))
                                     (COND
                                      ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                    LOOPLABEL
                                     (COND
                                      ((MINUSP (DIFFERENCE NTAIL A))
                                       (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (PROG (B FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ B 1)
                                               (COND
                                                ((MINUSP (DIFFERENCE NTAIL B))
                                                 (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                (REVAL1
                                                                 (LIST 'TIMES
                                                                       (LIST C
                                                                             A
                                                                             B)
                                                                       (LIST W
                                                                             I
                                                                             A)
                                                                       (NTH
                                                                        NLOC_ARG
                                                                        B))
                                                                 NIL)
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ B (PLUS2 B 1))
                                               (COND
                                                ((MINUSP (DIFFERENCE NTAIL B))
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        (REVAL1
                                                         (LIST 'TIMES
                                                               (LIST C A B)
                                                               (LIST W I A)
                                                               (NTH NLOC_ARG
                                                                    B))
                                                         NIL)
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-ENDPTR))
                                     (SETQ A (PLUS2 A 1))
                                     (GO LOOPLABEL)))
                           (REVAL1 (CONS 'PLUS (APPEND TEMPLOC TEMPNLOC)) NIL))
                          NIL)))
        LOOPLABEL
         (SETQ I (PLUS2 I 1))
         (COND ((MINUSP (DIFFERENCE LARGCD I)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  (PROGN
                   (SETQ TEMPLOC
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J 1)
                             (COND
                              ((MINUSP (DIFFERENCE NTARGET J)) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              (REVAL1
                                               (LIST CDL_OP I J
                                                     (NTH LOC_ARG J))
                                               NIL)
                                              NIL)))
                            LOOPLABEL
                             (SETQ J (PLUS2 J 1))
                             (COND
                              ((MINUSP (DIFFERENCE NTARGET J))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      (REVAL1 (LIST CDL_OP I J (NTH LOC_ARG J))
                                              NIL)
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                   (SETQ TEMPNLOC
                           (PROG (A FORALL-RESULT FORALL-ENDPTR)
                             (SETQ A 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE NTAIL A)) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ B 1)
                                       (COND
                                        ((MINUSP (DIFFERENCE NTAIL B))
                                         (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        (REVAL1
                                                         (LIST 'TIMES
                                                               (LIST C A B)
                                                               (LIST W I A)
                                                               (NTH NLOC_ARG
                                                                    B))
                                                         NIL)
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ B (PLUS2 B 1))
                                       (COND
                                        ((MINUSP (DIFFERENCE NTAIL B))
                                         (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                (REVAL1
                                                 (LIST 'TIMES (LIST C A B)
                                                       (LIST W I A)
                                                       (NTH NLOC_ARG B))
                                                 NIL)
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ A (PLUS2 A 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE NTAIL A))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ B 1)
                                       (COND
                                        ((MINUSP (DIFFERENCE NTAIL B))
                                         (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        (REVAL1
                                                         (LIST 'TIMES
                                                               (LIST C A B)
                                                               (LIST W I A)
                                                               (NTH NLOC_ARG
                                                                    B))
                                                         NIL)
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ B (PLUS2 B 1))
                                       (COND
                                        ((MINUSP (DIFFERENCE NTAIL B))
                                         (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                (REVAL1
                                                 (LIST 'TIMES (LIST C A B)
                                                       (LIST W I A)
                                                       (NTH NLOC_ARG B))
                                                 NIL)
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ A (PLUS2 A 1))
                             (GO LOOPLABEL)))
                   (REVAL1 (CONS 'PLUS (APPEND TEMPLOC TEMPNLOC)) NIL))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'L_SUMMAND 'NUMBER-OF-ARGS 5) 
(PUT 'L_SUMMAND 'DEFINED-ON-LINE '95) 
(PUT 'L_SUMMAND 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'L_SUMMAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE L_SUMMAND (CDOP_LOC_CF LOC_ARG_1 VEC_OP2_2 LOC_ARG_3 ALL_PARDER)
    (PROG (OP_I OP_J TEMPLTERMS LIN_COEFF VAR_M VAR_ID M_IND)
      (RETURN
       (PROG (OP_ENTRY FORALL-RESULT FORALL-ENDPTR)
         (SETQ OP_ENTRY (CDR CDOP_LOC_CF))
        STARTOVER
         (COND ((NULL OP_ENTRY) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (OP_ENTRY)
                    (PROGN
                     (SETQ OP_I (CADR (CADR OP_ENTRY)))
                     (SETQ OP_J (CADDR (CADR OP_ENTRY)))
                     (SETQ TEMPLTERMS (CDDR OP_ENTRY))
                     (PROG (OP_TERM FORALL-RESULT FORALL-ENDPTR)
                       (SETQ OP_TERM TEMPLTERMS)
                      STARTOVER
                       (COND ((NULL OP_TERM) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (OP_TERM)
                                  (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ VAR ALL_PARDER)
                                    (COND ((NULL VAR) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (VAR)
                                                        (PROGN
                                                         (COND
                                                          ((CDE_DEPENDON
                                                            (CADR OP_TERM) VAR)
                                                           (PROGN
                                                            (SETQ LIN_COEFF
                                                                    (REVAL1
                                                                     (LIST 'DF
                                                                           (CADR
                                                                            OP_TERM)
                                                                           VAR)
                                                                     NIL))
                                                            (SETQ VAR_M
                                                                    (IDTOMIND 0
                                                                     VAR))
                                                            (SETQ VAR_ID
                                                                    (CAR
                                                                     VAR_M))
                                                            (SETQ M_IND
                                                                    (CONS 'LIST
                                                                          (CADR
                                                                           VAR_M)))
                                                            (REVAL1
                                                             (LIST 'TIMES
                                                                   LIN_COEFF
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      LOC_ARG_1
                                                                      OP_J)
                                                                     (CADDR
                                                                      OP_TERM))
                                                                    NIL)
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                                    NIL)
                                                                   (NTH
                                                                    LOC_ARG_3
                                                                    OP_I))
                                                             NIL))))))
                                                      (CAR VAR))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ VAR (CDR VAR))
                                    (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (VAR)
                                                (PROGN
                                                 (COND
                                                  ((CDE_DEPENDON (CADR OP_TERM)
                                                    VAR)
                                                   (PROGN
                                                    (SETQ LIN_COEFF
                                                            (REVAL1
                                                             (LIST 'DF
                                                                   (CADR
                                                                    OP_TERM)
                                                                   VAR)
                                                             NIL))
                                                    (SETQ VAR_M
                                                            (IDTOMIND 0 VAR))
                                                    (SETQ VAR_ID (CAR VAR_M))
                                                    (SETQ M_IND
                                                            (CONS 'LIST
                                                                  (CADR
                                                                   VAR_M)))
                                                    (REVAL1
                                                     (LIST 'TIMES LIN_COEFF
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   LOC_ARG_1
                                                                   OP_J)
                                                                  (CADDR
                                                                   OP_TERM))
                                                            NIL)
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   VEC_OP2_2
                                                                   (CDE_POSITION
                                                                    VAR_ID
                                                                    DEP_VAR*))
                                                                  M_IND)
                                                            NIL)
                                                           (NTH LOC_ARG_3
                                                                OP_I))
                                                     NIL))))))
                                              (CAR VAR))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR OP_TERM)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ OP_TERM (CDR OP_TERM))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL OP_TERM) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (OP_TERM)
                                  (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ VAR ALL_PARDER)
                                    (COND ((NULL VAR) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (VAR)
                                                        (PROGN
                                                         (COND
                                                          ((CDE_DEPENDON
                                                            (CADR OP_TERM) VAR)
                                                           (PROGN
                                                            (SETQ LIN_COEFF
                                                                    (REVAL1
                                                                     (LIST 'DF
                                                                           (CADR
                                                                            OP_TERM)
                                                                           VAR)
                                                                     NIL))
                                                            (SETQ VAR_M
                                                                    (IDTOMIND 0
                                                                     VAR))
                                                            (SETQ VAR_ID
                                                                    (CAR
                                                                     VAR_M))
                                                            (SETQ M_IND
                                                                    (CONS 'LIST
                                                                          (CADR
                                                                           VAR_M)))
                                                            (REVAL1
                                                             (LIST 'TIMES
                                                                   LIN_COEFF
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      LOC_ARG_1
                                                                      OP_J)
                                                                     (CADDR
                                                                      OP_TERM))
                                                                    NIL)
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                                    NIL)
                                                                   (NTH
                                                                    LOC_ARG_3
                                                                    OP_I))
                                                             NIL))))))
                                                      (CAR VAR))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ VAR (CDR VAR))
                                    (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (VAR)
                                                (PROGN
                                                 (COND
                                                  ((CDE_DEPENDON (CADR OP_TERM)
                                                    VAR)
                                                   (PROGN
                                                    (SETQ LIN_COEFF
                                                            (REVAL1
                                                             (LIST 'DF
                                                                   (CADR
                                                                    OP_TERM)
                                                                   VAR)
                                                             NIL))
                                                    (SETQ VAR_M
                                                            (IDTOMIND 0 VAR))
                                                    (SETQ VAR_ID (CAR VAR_M))
                                                    (SETQ M_IND
                                                            (CONS 'LIST
                                                                  (CADR
                                                                   VAR_M)))
                                                    (REVAL1
                                                     (LIST 'TIMES LIN_COEFF
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   LOC_ARG_1
                                                                   OP_J)
                                                                  (CADDR
                                                                   OP_TERM))
                                                            NIL)
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   VEC_OP2_2
                                                                   (CDE_POSITION
                                                                    VAR_ID
                                                                    DEP_VAR*))
                                                                  M_IND)
                                                            NIL)
                                                           (NTH LOC_ARG_3
                                                                OP_I))
                                                     NIL))))))
                                              (CAR VAR))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR OP_TERM)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ OP_TERM (CDR OP_TERM))
                       (GO LOOPLABEL))))
                  (CAR OP_ENTRY)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ OP_ENTRY (CDR OP_ENTRY))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL OP_ENTRY) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (OP_ENTRY)
                    (PROGN
                     (SETQ OP_I (CADR (CADR OP_ENTRY)))
                     (SETQ OP_J (CADDR (CADR OP_ENTRY)))
                     (SETQ TEMPLTERMS (CDDR OP_ENTRY))
                     (PROG (OP_TERM FORALL-RESULT FORALL-ENDPTR)
                       (SETQ OP_TERM TEMPLTERMS)
                      STARTOVER
                       (COND ((NULL OP_TERM) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (OP_TERM)
                                  (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ VAR ALL_PARDER)
                                    (COND ((NULL VAR) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (VAR)
                                                        (PROGN
                                                         (COND
                                                          ((CDE_DEPENDON
                                                            (CADR OP_TERM) VAR)
                                                           (PROGN
                                                            (SETQ LIN_COEFF
                                                                    (REVAL1
                                                                     (LIST 'DF
                                                                           (CADR
                                                                            OP_TERM)
                                                                           VAR)
                                                                     NIL))
                                                            (SETQ VAR_M
                                                                    (IDTOMIND 0
                                                                     VAR))
                                                            (SETQ VAR_ID
                                                                    (CAR
                                                                     VAR_M))
                                                            (SETQ M_IND
                                                                    (CONS 'LIST
                                                                          (CADR
                                                                           VAR_M)))
                                                            (REVAL1
                                                             (LIST 'TIMES
                                                                   LIN_COEFF
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      LOC_ARG_1
                                                                      OP_J)
                                                                     (CADDR
                                                                      OP_TERM))
                                                                    NIL)
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                                    NIL)
                                                                   (NTH
                                                                    LOC_ARG_3
                                                                    OP_I))
                                                             NIL))))))
                                                      (CAR VAR))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ VAR (CDR VAR))
                                    (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (VAR)
                                                (PROGN
                                                 (COND
                                                  ((CDE_DEPENDON (CADR OP_TERM)
                                                    VAR)
                                                   (PROGN
                                                    (SETQ LIN_COEFF
                                                            (REVAL1
                                                             (LIST 'DF
                                                                   (CADR
                                                                    OP_TERM)
                                                                   VAR)
                                                             NIL))
                                                    (SETQ VAR_M
                                                            (IDTOMIND 0 VAR))
                                                    (SETQ VAR_ID (CAR VAR_M))
                                                    (SETQ M_IND
                                                            (CONS 'LIST
                                                                  (CADR
                                                                   VAR_M)))
                                                    (REVAL1
                                                     (LIST 'TIMES LIN_COEFF
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   LOC_ARG_1
                                                                   OP_J)
                                                                  (CADDR
                                                                   OP_TERM))
                                                            NIL)
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   VEC_OP2_2
                                                                   (CDE_POSITION
                                                                    VAR_ID
                                                                    DEP_VAR*))
                                                                  M_IND)
                                                            NIL)
                                                           (NTH LOC_ARG_3
                                                                OP_I))
                                                     NIL))))))
                                              (CAR VAR))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR OP_TERM)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ OP_TERM (CDR OP_TERM))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL OP_TERM) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (OP_TERM)
                                  (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ VAR ALL_PARDER)
                                    (COND ((NULL VAR) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (VAR)
                                                        (PROGN
                                                         (COND
                                                          ((CDE_DEPENDON
                                                            (CADR OP_TERM) VAR)
                                                           (PROGN
                                                            (SETQ LIN_COEFF
                                                                    (REVAL1
                                                                     (LIST 'DF
                                                                           (CADR
                                                                            OP_TERM)
                                                                           VAR)
                                                                     NIL))
                                                            (SETQ VAR_M
                                                                    (IDTOMIND 0
                                                                     VAR))
                                                            (SETQ VAR_ID
                                                                    (CAR
                                                                     VAR_M))
                                                            (SETQ M_IND
                                                                    (CONS 'LIST
                                                                          (CADR
                                                                           VAR_M)))
                                                            (REVAL1
                                                             (LIST 'TIMES
                                                                   LIN_COEFF
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      LOC_ARG_1
                                                                      OP_J)
                                                                     (CADDR
                                                                      OP_TERM))
                                                                    NIL)
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                                    NIL)
                                                                   (NTH
                                                                    LOC_ARG_3
                                                                    OP_I))
                                                             NIL))))))
                                                      (CAR VAR))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ VAR (CDR VAR))
                                    (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (VAR)
                                                (PROGN
                                                 (COND
                                                  ((CDE_DEPENDON (CADR OP_TERM)
                                                    VAR)
                                                   (PROGN
                                                    (SETQ LIN_COEFF
                                                            (REVAL1
                                                             (LIST 'DF
                                                                   (CADR
                                                                    OP_TERM)
                                                                   VAR)
                                                             NIL))
                                                    (SETQ VAR_M
                                                            (IDTOMIND 0 VAR))
                                                    (SETQ VAR_ID (CAR VAR_M))
                                                    (SETQ M_IND
                                                            (CONS 'LIST
                                                                  (CADR
                                                                   VAR_M)))
                                                    (REVAL1
                                                     (LIST 'TIMES LIN_COEFF
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   LOC_ARG_1
                                                                   OP_J)
                                                                  (CADDR
                                                                   OP_TERM))
                                                            NIL)
                                                           (REVAL1
                                                            (LIST 'TD_MIND
                                                                  (NTH
                                                                   VEC_OP2_2
                                                                   (CDE_POSITION
                                                                    VAR_ID
                                                                    DEP_VAR*))
                                                                  M_IND)
                                                            NIL)
                                                           (NTH LOC_ARG_3
                                                                OP_I))
                                                     NIL))))))
                                              (CAR VAR))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR OP_TERM)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ OP_TERM (CDR OP_TERM))
                       (GO LOOPLABEL))))
                  (CAR OP_ENTRY)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ OP_ENTRY (CDR OP_ENTRY))
         (GO LOOPLABEL))))) 
(PUT 'NL_SUMMAND_1 'NUMBER-OF-ARGS 6) 
(PUT 'NL_SUMMAND_1 'DEFINED-ON-LINE '132) 
(PUT 'NL_SUMMAND_1 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'NL_SUMMAND_1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NL_SUMMAND_1 (C W VEC_OP2_2 NLOC_ARG_1 LOC_ARG_3 ALL_PARDER)
    (PROG (NTAIL LIN_COEFF VAR_M VAR_ID M_IND)
      (SETQ NTAIL (GET 'WNL_NARG C))
      (RETURN
       (PROG (ALPHA FORALL-RESULT FORALL-ENDPTR)
         (SETQ ALPHA 1)
        STARTOVER
         (COND ((MINUSP (DIFFERENCE NTAIL ALPHA)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (PROG (BETA FORALL-RESULT FORALL-ENDPTR)
                   (SETQ BETA 1)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ I (PLUS2 I 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ I (PLUS2 I 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ BETA (PLUS2 BETA 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ I (PLUS2 I 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ I (PLUS2 I 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ BETA (PLUS2 BETA 1))
                   (GO LOOPLABEL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ ALPHA (PLUS2 ALPHA 1))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((MINUSP (DIFFERENCE NTAIL ALPHA)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (PROG (BETA FORALL-RESULT FORALL-ENDPTR)
                   (SETQ BETA 1)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ I (PLUS2 I 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ I (PLUS2 I 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ BETA (PLUS2 BETA 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ I (PLUS2 I 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_3) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W I
                                                                      ALPHA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W I
                                                                           ALPHA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_1
                                                                       BETA)
                                                                      (NTH
                                                                       LOC_ARG_3
                                                                       I))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W I ALPHA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             I
                                                                             ALPHA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH NLOC_ARG_1
                                                                   BETA)
                                                              (NTH LOC_ARG_3
                                                                   I))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ I (PLUS2 I 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ BETA (PLUS2 BETA 1))
                   (GO LOOPLABEL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ ALPHA (PLUS2 ALPHA 1))
         (GO LOOPLABEL))))) 
(PUT 'NL_SUMMAND_2 'NUMBER-OF-ARGS 6) 
(PUT 'NL_SUMMAND_2 'DEFINED-ON-LINE '166) 
(PUT 'NL_SUMMAND_2 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'NL_SUMMAND_2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NL_SUMMAND_2 (C W NLOC_ARG_3 VEC_OP2_2 LOC_ARG_1 ALL_PARDER)
    (PROG (NTAIL LIN_COEFF VAR_M VAR_ID M_IND)
      (SETQ NTAIL (GET 'WNL_NARG C))
      (RETURN
       (PROG (ALPHA FORALL-RESULT FORALL-ENDPTR)
         (SETQ ALPHA 1)
        STARTOVER
         (COND ((MINUSP (DIFFERENCE NTAIL ALPHA)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (PROG (BETA FORALL-RESULT FORALL-ENDPTR)
                   (SETQ BETA 1)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ J (PLUS2 J 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ J (PLUS2 J 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ BETA (PLUS2 BETA 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ J (PLUS2 J 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ J (PLUS2 J 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ BETA (PLUS2 BETA 1))
                   (GO LOOPLABEL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ ALPHA (PLUS2 ALPHA 1))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((MINUSP (DIFFERENCE NTAIL ALPHA)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (PROG (BETA FORALL-RESULT FORALL-ENDPTR)
                   (SETQ BETA 1)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ J (PLUS2 J 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ J (PLUS2 J 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ BETA (PLUS2 BETA 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE NTAIL BETA)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J 1)
                            STARTOVER
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ J (PLUS2 J 1))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND
                              ((MINUSP (DIFFERENCE (LENGTH LOC_ARG_1) J))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ VAR ALL_PARDER)
                                       (COND ((NULL VAR) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (VAR)
                                                           (PROGN
                                                            (COND
                                                             ((CDE_DEPENDON
                                                               (REVAL1
                                                                (LIST W J BETA)
                                                                NIL)
                                                               VAR)
                                                              (PROGN
                                                               (SETQ LIN_COEFF
                                                                       (REVAL1
                                                                        (LIST
                                                                         'DF
                                                                         (REVAL1
                                                                          (LIST
                                                                           W J
                                                                           BETA)
                                                                          NIL)
                                                                         VAR)
                                                                        NIL))
                                                               (SETQ VAR_M
                                                                       (IDTOMIND
                                                                        0 VAR))
                                                               (SETQ VAR_ID
                                                                       (CAR
                                                                        VAR_M))
                                                               (SETQ M_IND
                                                                       (CONS
                                                                        'LIST
                                                                        (CADR
                                                                         VAR_M)))
                                                               (REVAL1
                                                                (LIST 'TIMES
                                                                      (MINUS 1)
                                                                      (REVAL1
                                                                       (LIST C
                                                                             ALPHA
                                                                             BETA)
                                                                       NIL)
                                                                      (NTH
                                                                       NLOC_ARG_3
                                                                       ALPHA)
                                                                      LIN_COEFF
                                                                      (REVAL1
                                                                       (LIST
                                                                        'TD_MIND
                                                                        (NTH
                                                                         VEC_OP2_2
                                                                         (CDE_POSITION
                                                                          VAR_ID
                                                                          DEP_VAR*))
                                                                        M_IND)
                                                                       NIL)
                                                                      (NTH
                                                                       LOC_ARG_1
                                                                       J))
                                                                NIL))))))
                                                         (CAR VAR))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ VAR (CDR VAR))
                                       (COND
                                        ((NULL VAR) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (VAR)
                                                   (PROGN
                                                    (COND
                                                     ((CDE_DEPENDON
                                                       (REVAL1 (LIST W J BETA)
                                                               NIL)
                                                       VAR)
                                                      (PROGN
                                                       (SETQ LIN_COEFF
                                                               (REVAL1
                                                                (LIST 'DF
                                                                      (REVAL1
                                                                       (LIST W
                                                                             J
                                                                             BETA)
                                                                       NIL)
                                                                      VAR)
                                                                NIL))
                                                       (SETQ VAR_M
                                                               (IDTOMIND 0
                                                                VAR))
                                                       (SETQ VAR_ID
                                                               (CAR VAR_M))
                                                       (SETQ M_IND
                                                               (CONS 'LIST
                                                                     (CADR
                                                                      VAR_M)))
                                                       (REVAL1
                                                        (LIST 'TIMES (MINUS 1)
                                                              (REVAL1
                                                               (LIST C ALPHA
                                                                     BETA)
                                                               NIL)
                                                              (NTH NLOC_ARG_3
                                                                   ALPHA)
                                                              LIN_COEFF
                                                              (REVAL1
                                                               (LIST 'TD_MIND
                                                                     (NTH
                                                                      VEC_OP2_2
                                                                      (CDE_POSITION
                                                                       VAR_ID
                                                                       DEP_VAR*))
                                                                     M_IND)
                                                               NIL)
                                                              (NTH LOC_ARG_1
                                                                   J))
                                                        NIL))))))
                                                 (CAR VAR))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ J (PLUS2 J 1))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ BETA (PLUS2 BETA 1))
                   (GO LOOPLABEL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ ALPHA (PLUS2 ALPHA 1))
         (GO LOOPLABEL))))) 
(PUT 'DUBROVIN_ZHANG_EXPR 'NUMBER-OF-ARGS 12) 
(PUT 'DUBROVIN_ZHANG_EXPR 'DEFINED-ON-LINE '203) 
(PUT 'DUBROVIN_ZHANG_EXPR 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'DUBROVIN_ZHANG_EXPR 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE DUBROVIN_ZHANG_EXPR
    (HAM1_L C W HAM2_L D Z DEP_VAR_EQU LOC_ARG_1 LOC_ARG_2 LOC_ARG_3 NLARGW
     NLARGZ)
    (PROG (NLARGW_1 NLARGW_2 NLARGW_3 NLARGZ_1 NLARGZ_2 NLARGZ_3 VEC_OP1_1
           VEC_OP1_2 VEC_OP1_3 VEC_OP2_1 VEC_OP2_2 VEC_OP2_3 HAM1_L_COEFF
           HAM2_L_COEFF ALL_PARDER L_H1_H2_123 L_H1_H2_312 L_H1_H2_231
           L_H2_H1_123 L_H2_H1_312 L_H2_H1_231)
      (SETQ NLARGW_1 (NTH NLARGW 1))
      (SETQ NLARGW_2 (NTH NLARGW 2))
      (SETQ NLARGW_3 (NTH NLARGW 3))
      (SETQ NLARGZ_1 (NTH NLARGZ 1))
      (SETQ NLARGZ_2 (NTH NLARGZ 2))
      (SETQ NLARGZ_3 (NTH NLARGZ 3))
      (SETQ VEC_OP1_1 (CDWNL2VEC HAM1_L C W LOC_ARG_1 NLARGW_1))
      (SETQ VEC_OP1_2 (CDWNL2VEC HAM1_L C W LOC_ARG_2 NLARGW_2))
      (SETQ VEC_OP1_3 (CDWNL2VEC HAM1_L C W LOC_ARG_3 NLARGW_3))
      (SETQ VEC_OP2_1 (CDWNL2VEC HAM2_L D Z LOC_ARG_1 NLARGZ_1))
      (SETQ VEC_OP2_2 (CDWNL2VEC HAM2_L D Z LOC_ARG_2 NLARGZ_2))
      (SETQ VEC_OP2_3 (CDWNL2VEC HAM2_L D Z LOC_ARG_3 NLARGZ_3))
      (SETQ HAM1_L_COEFF (ALL_COEFF_CDIFFOP0 HAM1_L))
      (SETQ HAM2_L_COEFF (ALL_COEFF_CDIFFOP0 HAM2_L))
      (SETQ ALL_PARDER
              (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                (SETQ VAR DEP_VAR_EQU)
               STARTOVER
                (COND ((NULL VAR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (VAR)
                           (REVERSE
                            (SELECT_ALL_DERS 0 VAR ALL_PARAMETRIC_DER*)))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ VAR (CDR VAR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (VAR)
                           (REVERSE
                            (SELECT_ALL_DERS 0 VAR ALL_PARAMETRIC_DER*)))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ VAR (CDR VAR))
                (GO LOOPLABEL)))
      (SETQ L_H1_H2_123
              (APPEND
               (L_SUMMAND HAM1_L_COEFF LOC_ARG_1 VEC_OP2_2 LOC_ARG_3
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 C W VEC_OP2_2 NLARGW_1 LOC_ARG_3 ALL_PARDER)
                (NL_SUMMAND_2 C W NLARGW_3 VEC_OP2_2 LOC_ARG_1 ALL_PARDER))))
      (SETQ L_H1_H2_312
              (APPEND
               (L_SUMMAND HAM1_L_COEFF LOC_ARG_3 VEC_OP2_1 LOC_ARG_2
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 C W VEC_OP2_1 NLARGW_3 LOC_ARG_2 ALL_PARDER)
                (NL_SUMMAND_2 C W NLARGW_2 VEC_OP2_1 LOC_ARG_3 ALL_PARDER))))
      (SETQ L_H1_H2_231
              (APPEND
               (L_SUMMAND HAM1_L_COEFF LOC_ARG_2 VEC_OP2_3 LOC_ARG_1
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 C W VEC_OP2_3 NLARGW_2 LOC_ARG_1 ALL_PARDER)
                (NL_SUMMAND_2 C W NLARGW_1 VEC_OP2_3 LOC_ARG_2 ALL_PARDER))))
      (SETQ L_H2_H1_123
              (APPEND
               (L_SUMMAND HAM2_L_COEFF LOC_ARG_1 VEC_OP1_2 LOC_ARG_3
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 D Z VEC_OP1_2 NLARGZ_1 LOC_ARG_3 ALL_PARDER)
                (NL_SUMMAND_2 D Z NLARGZ_3 VEC_OP1_2 LOC_ARG_1 ALL_PARDER))))
      (SETQ L_H2_H1_312
              (APPEND
               (L_SUMMAND HAM2_L_COEFF LOC_ARG_3 VEC_OP1_1 LOC_ARG_2
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 D Z VEC_OP1_1 NLARGZ_3 LOC_ARG_2 ALL_PARDER)
                (NL_SUMMAND_2 D Z NLARGZ_2 VEC_OP1_1 LOC_ARG_3 ALL_PARDER))))
      (SETQ L_H2_H1_231
              (APPEND
               (L_SUMMAND HAM2_L_COEFF LOC_ARG_2 VEC_OP1_3 LOC_ARG_1
                ALL_PARDER)
               (APPEND
                (NL_SUMMAND_1 D Z VEC_OP1_3 NLARGZ_2 LOC_ARG_1 ALL_PARDER)
                (NL_SUMMAND_2 D Z NLARGZ_1 VEC_OP1_3 LOC_ARG_2 ALL_PARDER))))
      (RETURN
       (REVAL1
        (CONS 'PLUS
              (APPEND L_H1_H2_123
                      (APPEND L_H1_H2_312
                              (APPEND L_H1_H2_231
                                      (APPEND L_H2_H1_123
                                              (APPEND L_H2_H1_312
                                                      L_H2_H1_231))))))
        NIL)))) 
(PUT 'INTEGRATE_BY_PARTS 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGRATE_BY_PARTS 'DEFINED-ON-LINE '279) 
(PUT 'INTEGRATE_BY_PARTS 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'INTEGRATE_BY_PARTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGRATE_BY_PARTS (BAD_TERM EL)
    (PROG (VAR_M VAR_ID MIND_VAL M_IND)
      (SETQ VAR_M (IDTOMIND 0 EL))
      (SETQ VAR_ID (CAR VAR_M))
      (SETQ MIND_VAL (CAADR VAR_M))
      (SETQ M_IND (CONS 'LIST (LIST MIND_VAL)))
      (RETURN
       (REVAL1
        (LIST 'TIMES VAR_ID (LIST 'EXPT (MINUS 1) MIND_VAL)
              (LIST 'TD_MIND BAD_TERM M_IND))
        NIL)))) 
(PUT 'NONLOCAL_REDUCTION 'NUMBER-OF-ARGS 4) 
(PUT 'NONLOCAL_REDUCTION 'DEFINED-ON-LINE '294) 
(PUT 'NONLOCAL_REDUCTION 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'NONLOCAL_REDUCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NONLOCAL_REDUCTION (SB_EXPR NLOC_ARG_1 LOC_ARG_2 LOC_ARG_3)
    (PROG (DER_ARG_3 TEMPDVAR TEMPTERM_1 TEMPTERM_2 REDUCED_EXPR)
      (SETQ REDUCED_EXPR NIL)
      (SETQ DER_ARG_3 NIL)
      (PROG (VAR)
        (SETQ VAR LOC_ARG_3)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROG (EL)
             (SETQ EL ALL_PARAMETRIC_DER*)
            LAB
             (COND ((NULL EL) (RETURN NIL)))
             ((LAMBDA (EL)
                (PROGN
                 (SETQ TEMPDVAR (IDTOMIND 0 EL))
                 (COND
                  ((AND (EQ (CAR TEMPDVAR) VAR) (LESSP 0 (CAADR TEMPDVAR)))
                   (SETQ DER_ARG_3 (CONS EL DER_ARG_3))))))
              (CAR EL))
             (SETQ EL (CDR EL))
             (GO LAB)))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (SETQ DER_ARG_3 (REVERSE DER_ARG_3))
      (PROG (NLA_1)
        (SETQ NLA_1 NLOC_ARG_1)
       LAB
        (COND ((NULL NLA_1) (RETURN NIL)))
        ((LAMBDA (NLA_1)
           (PROG (LA_2)
             (SETQ LA_2 LOC_ARG_2)
            LAB
             (COND ((NULL LA_2) (RETURN NIL)))
             ((LAMBDA (LA_2)
                (PROG (DLA_3)
                  (SETQ DLA_3 DER_ARG_3)
                 LAB
                  (COND ((NULL DLA_3) (RETURN NIL)))
                  ((LAMBDA (DLA_3)
                     (COND
                      ((NOT
                        (EQN
                         (SETQ TEMPTERM_1
                                 (COEFFN
                                  (COEFFN (COEFFN SB_EXPR DLA_3 1) LA_2 1)
                                  NLA_1 1))
                         0))
                       (PROGN
                        (SETQ TEMPTERM_2
                                (INTEGRATE_BY_PARTS
                                 (REVAL1 (LIST 'TIMES TEMPTERM_1 NLA_1 LA_2)
                                         NIL)
                                 DLA_3))
                        (SETQ REDUCED_EXPR
                                (REVAL1
                                 (LIST 'PLUS
                                       (LIST 'TIMES (MINUS 1) TEMPTERM_1 NLA_1
                                             LA_2 DLA_3)
                                       TEMPTERM_2 REDUCED_EXPR)
                                 NIL))))))
                   (CAR DLA_3))
                  (SETQ DLA_3 (CDR DLA_3))
                  (GO LAB)))
              (CAR LA_2))
             (SETQ LA_2 (CDR LA_2))
             (GO LAB)))
         (CAR NLA_1))
        (SETQ NLA_1 (CDR NLA_1))
        (GO LAB))
      (RETURN (REVAL1 (LIST 'PLUS REDUCED_EXPR SB_EXPR) NIL)))) 
(PUT 'LOCAL_REDUCTION 'NUMBER-OF-ARGS 4) 
(PUT 'LOCAL_REDUCTION 'DEFINED-ON-LINE '329) 
(PUT 'LOCAL_REDUCTION 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'LOCAL_REDUCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOCAL_REDUCTION (SB_EXPR LOC_ARG_1 LOC_ARG_2 LOC_ARG_3)
    (PROG (DER_ARG_3 DER_LOC_ARG_1 DER_LOC_ARG_2 TEMPDVAR TEMPTERM_1 TEMPTERM_2
           REDUCED_EXPR)
      (SETQ REDUCED_EXPR NIL)
      (SETQ DER_ARG_3 NIL)
      (PROG (VAR)
        (SETQ VAR LOC_ARG_3)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROG (EL)
             (SETQ EL ALL_PARAMETRIC_DER*)
            LAB
             (COND ((NULL EL) (RETURN NIL)))
             ((LAMBDA (EL)
                (PROGN
                 (SETQ TEMPDVAR (IDTOMIND 0 EL))
                 (COND
                  ((AND (EQ (CAR TEMPDVAR) VAR) (LESSP 0 (CAADR TEMPDVAR)))
                   (SETQ DER_ARG_3 (CONS EL DER_ARG_3))))))
              (CAR EL))
             (SETQ EL (CDR EL))
             (GO LAB)))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (SETQ DER_ARG_3 (REVERSE DER_ARG_3))
      (SETQ DER_LOC_ARG_1
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL LOC_ARG_1)
               STARTOVER
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (EL)
                           (SELECT_ALL_DERS 0 EL ALL_PARAMETRIC_DER*))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EL (CDR EL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (EL)
                           (SELECT_ALL_DERS 0 EL ALL_PARAMETRIC_DER*))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EL (CDR EL))
                (GO LOOPLABEL)))
      (SETQ DER_LOC_ARG_2
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL LOC_ARG_2)
               STARTOVER
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (EL)
                           (SELECT_ALL_DERS 0 EL ALL_PARAMETRIC_DER*))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EL (CDR EL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (EL)
                           (SELECT_ALL_DERS 0 EL ALL_PARAMETRIC_DER*))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EL (CDR EL))
                (GO LOOPLABEL)))
      (PROG (DLA_1)
        (SETQ DLA_1 DER_LOC_ARG_1)
       LAB
        (COND ((NULL DLA_1) (RETURN NIL)))
        ((LAMBDA (DLA_1)
           (PROG (DLA_2)
             (SETQ DLA_2 DER_LOC_ARG_2)
            LAB
             (COND ((NULL DLA_2) (RETURN NIL)))
             ((LAMBDA (DLA_2)
                (PROG (DLA_3)
                  (SETQ DLA_3 DER_ARG_3)
                 LAB
                  (COND ((NULL DLA_3) (RETURN NIL)))
                  ((LAMBDA (DLA_3)
                     (COND
                      ((NOT
                        (EQN
                         (SETQ TEMPTERM_1
                                 (COEFFN
                                  (COEFFN (COEFFN SB_EXPR DLA_3 1) DLA_2 1)
                                  DLA_1 1))
                         0))
                       (PROGN
                        (SETQ TEMPTERM_2
                                (INTEGRATE_BY_PARTS
                                 (REVAL1 (LIST 'TIMES TEMPTERM_1 DLA_1 DLA_2)
                                         NIL)
                                 DLA_3))
                        (SETQ REDUCED_EXPR
                                (REVAL1
                                 (LIST 'PLUS
                                       (LIST 'TIMES (MINUS 1) TEMPTERM_1 DLA_1
                                             DLA_2 DLA_3)
                                       TEMPTERM_2 REDUCED_EXPR)
                                 NIL))))))
                   (CAR DLA_3))
                  (SETQ DLA_3 (CDR DLA_3))
                  (GO LAB)))
              (CAR DLA_2))
             (SETQ DLA_2 (CDR DLA_2))
             (GO LAB)))
         (CAR DLA_1))
        (SETQ DLA_1 (CDR DLA_1))
        (GO LAB))
      (RETURN (REVAL1 (LIST 'PLUS REDUCED_EXPR SB_EXPR) NIL)))) 
(PUT 'GENERATE_NLARG 'NUMBER-OF-ARGS 2) 
(PUT 'GENERATE_NLARG 'DEFINED-ON-LINE '369) 
(PUT 'GENERATE_NLARG 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'GENERATE_NLARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENERATE_NLARG (NLOC_ARG_W I)
    (PROG (TPSI W NTAIL_W NLARG_I)
      (SETQ TPSI (CAR NLOC_ARG_W))
      (SETQ W (CADR NLOC_ARG_W))
      (SETQ NTAIL_W (GET 'WNL_NARG W))
      (PROG (ALP)
        (SETQ ALP 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NTAIL_W ALP)) (RETURN NIL)))
        (SETQ NLARG_I (CONS (CDE_LIST2ID (LIST TPSI '_ ALP I)) NLARG_I))
        (SETQ ALP (PLUS2 ALP 1))
        (GO LAB))
      (RETURN (REVERSE NLARG_I)))) 
(PUT 'SB_WNL_ALGORITHM 'NUMBER-OF-ARGS 9) 
(PUT 'SB_WNL_ALGORITHM 'DEFINED-ON-LINE '383) 
(PUT 'SB_WNL_ALGORITHM 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'SB_WNL_ALGORITHM 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE SB_WNL_ALGORITHM (HAM1_L C W HAM2_L D Z DEP_VAR_EQU LOC_ARG NLOC_ARG)
    (PROG (NLOC_ARG_W NLOC_ARG_Z LOC_ARG_1 LOC_ARG_2 LOC_ARG_3 NLARGW_1
           NLARGW_2 NLARGW_3 NLARGZ_1 NLARGZ_2 NLARGZ_3 NLARGW NLARGZ DZ
           FIRST_STEP)
      (SETQ NLOC_ARG_W (CDAR NLOC_ARG))
      (SETQ NLOC_ARG_Z (CDADR NLOC_ARG))
      (SETQ LOC_ARG_1
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL LOC_ARG)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (MKID EL 1)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (MKID EL 1)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LOC_ARG_2
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL LOC_ARG)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (MKID EL 2)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (MKID EL 2)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LOC_ARG_3
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL LOC_ARG)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (MKID EL 3)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (MKID EL 3)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NLARGW_1 (GENERATE_NLARG NLOC_ARG_W 1))
      (SETQ NLARGW_2 (GENERATE_NLARG NLOC_ARG_W 2))
      (SETQ NLARGW_3 (GENERATE_NLARG NLOC_ARG_W 3))
      (SETQ NLARGZ_1 (GENERATE_NLARG NLOC_ARG_Z 1))
      (SETQ NLARGZ_2 (GENERATE_NLARG NLOC_ARG_Z 2))
      (SETQ NLARGZ_3 (GENERATE_NLARG NLOC_ARG_Z 3))
      (SETQ NLARGW (LIST NLARGW_1 NLARGW_2 NLARGW_3))
      (SETQ NLARGZ (LIST NLARGZ_1 NLARGZ_2 NLARGZ_3))
      (PRIN2T "Step 0: calculating Schouten bracket")
      (SETQ DZ
              (DUBROVIN_ZHANG_EXPR HAM1_L C W HAM2_L D Z DEP_VAR_EQU LOC_ARG_1
               LOC_ARG_2 LOC_ARG_3 NLARGW NLARGZ))
      (PRIN2T "Step 1: reducing the nonlocal terms")
      (SETQ FIRST_STEP (NONLOCAL_REDUCTION DZ NLARGW_1 LOC_ARG_2 LOC_ARG_3))
      (SETQ FIRST_STEP
              (NONLOCAL_REDUCTION FIRST_STEP NLARGZ_1 LOC_ARG_2 LOC_ARG_3))
      (SETQ FIRST_STEP
              (NONLOCAL_REDUCTION FIRST_STEP NLARGW_2 LOC_ARG_3 LOC_ARG_1))
      (SETQ FIRST_STEP
              (NONLOCAL_REDUCTION FIRST_STEP NLARGZ_2 LOC_ARG_3 LOC_ARG_1))
      (SETQ FIRST_STEP
              (NONLOCAL_REDUCTION FIRST_STEP NLARGW_3 LOC_ARG_1 LOC_ARG_2))
      (SETQ FIRST_STEP
              (NONLOCAL_REDUCTION FIRST_STEP NLARGZ_3 LOC_ARG_1 LOC_ARG_2))
      (PRIN2T "Step 2: reducing the local terms")
      (RETURN (LOCAL_REDUCTION FIRST_STEP LOC_ARG_1 LOC_ARG_2 LOC_ARG_3)))) 
(PUT 'SCHOUTEN_BRACKET_WNL 'NUMBER-OF-ARGS 5) 
(PUT 'SCHOUTEN_BRACKET_WNL 'DEFINED-ON-LINE '449) 
(PUT 'SCHOUTEN_BRACKET_WNL 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'SCHOUTEN_BRACKET_WNL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SCHOUTEN_BRACKET_WNL (HAM1 HAM2 DEP_VAR_EQU LOC_ARG NLOC_ARG)
    (PROG (HAM1_L C W HAM2_L D Z NTAIL_W NTAIL_Z)
      (PRIN2T "Weakly nonlocal Poisson bracket: checking the Jacobi property")
      (PRIN2T "CDE module cde_weaklynl.red (2021)")
      (PRIN2T "by M. Casati, P. Lorenzoni, D. Valeri, R. Vitolo")
      (PRIN2T "see https://gdeq.org/Weakly_nonlocal_Poisson_brackets")
      (TERPRI)
      (SETQ HAM1_L (NTH HAM1 2))
      (SETQ C (NTH HAM1 3))
      (SETQ W (NTH HAM1 4))
      (SETQ HAM2_L (NTH HAM2 2))
      (SETQ D (NTH HAM2 3))
      (SETQ Z (NTH HAM2 4))
      (CHECK_CDIFF_ONEARG HAM1_L)
      (CHECK_CDIFF_ONEARG HAM2_L)
      (CHECK_CDIFF_SAMETYPE HAM1_L HAM2_L)
      (COND
       ((NOT (EQUAL (GET 'CDLARG HAM1_L) (GET 'CDLARG HAM2_L)))
        (REDERR "Error: non-conformant C-Differential operators")))
      (COND
       ((NOT (EQUAL (CADR (GET 'CDLARG HAM1_L)) (GET 'CDTARGET HAM1_L)))
        (REDERR "Non-conformant length of argument and dimension of target")))
      (COND
       ((NOT (EQUAL (CADR (GET 'CDLARG HAM1_L)) (GET 'CDTARGET HAM1_L)))
        (REDERR "Non-conformant length of argument and dimension of target")))
      (SETQ DEP_VAR_EQU (CDR DEP_VAR_EQU))
      (SETQ LOC_ARG (CDR LOC_ARG))
      (SETQ NLOC_ARG (CDR NLOC_ARG))
      (COND
       ((NOT (WNL_COEFFP C))
        (REDERR "Error: not a valid tail coefficient matrix")))
      (COND ((NOT (WNL_TAILP W)) (REDERR "Error: not a valid tail vector")))
      (COND
       ((NOT (WNL_COEFFP D))
        (REDERR "Error: not a valid tail coefficient matrix")))
      (COND ((NOT (WNL_TAILP Z)) (REDERR "Error: not a valid tail vector")))
      (COND
       ((NOT (EQN (LENGTH LOC_ARG) (LENGTH DEP_VAR_EQU)))
        (REDERR "Error: invalid list of arguments")))
      (SETQ NTAIL_W (GET 'WNL_NARG W))
      (COND
       ((NOT (EQN (GET 'WNL_NARG C) NTAIL_W))
        (REDERR "Error: non-conformant tail vector and coefficients")))
      (SETQ NTAIL_Z (GET 'WNL_NARG Z))
      (COND
       ((NOT (EQN (GET 'WNL_NARG D) NTAIL_Z))
        (REDERR "Error: non-conformant tail vector and coefficients")))
      (RETURN
       (SB_WNL_ALGORITHM HAM1_L C W HAM2_L D Z DEP_VAR_EQU LOC_ARG NLOC_ARG)))) 
(FLAG '(SCHOUTEN_BRACKET_WNL) 'OPFN) 
(PUT 'CDE_WEAKLYNL 'NUMBER-OF-ARGS 5) 
(PUT 'CDE_WEAKLYNL 'DEFINED-ON-LINE '509) 
(PUT 'CDE_WEAKLYNL 'DEFINED-IN-FILE 'CDE/CDE_WEAKLYNL.RED) 
(PUT 'CDE_WEAKLYNL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CDE_WEAKLYNL (INDEP_VAR DEP_VAR_EQU LOC_ARG NLOC_ARG TOTAL_ORDER)
    (PROG (N_DV LOC_ARG* LOC_ARG_3 NLOC_ARG_3 TAIL_VEC ALP NLVAR_TEMP PDER_TEMP
           DE_TEMP DEP_VAR_TOT DEP_VAR_JETSPACE PRINCIPAL_DER DE)
      (SETQ LOC_ARG* (CDR LOC_ARG))
      (SETQ N_DV (LENGTH (CDR DEP_VAR_EQU)))
      (SETQ NLOC_ARG_3 NIL)
      (SETQ PRINCIPAL_DER NIL)
      (SETQ DE NIL)
      (COND
       ((NOT (EQN N_DV (LENGTH LOC_ARG*)))
        (REDERR
         "Error: length of dependent variables and arguments mismatch")))
      (SETQ LOC_ARG_3
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                          (SETQ EL LOC_ARG*)
                          (COND ((NULL EL) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (EL) (MKID EL I)) (CAR EL))
                                           NIL)))
                         LOOPLABEL
                          (SETQ EL (CDR EL))
                          (COND ((NULL EL) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (EL) (MKID EL I)) (CAR EL))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                          (SETQ EL LOC_ARG*)
                          (COND ((NULL EL) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (EL) (MKID EL I)) (CAR EL))
                                           NIL)))
                         LOOPLABEL
                          (SETQ EL (CDR EL))
                          (COND ((NULL EL) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (EL) (MKID EL I)) (CAR EL))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
        (PROG (EL)
          (SETQ EL (CDR NLOC_ARG))
         LAB
          (COND ((NULL EL) (RETURN NIL)))
          ((LAMBDA (EL)
             (PROGN
              (SETQ TAIL_VEC (NTH EL 3))
              (SETQ ALP (NTH EL 4))
              (SETQ NLVAR_TEMP (CDE_LIST2ID (LIST (NTH EL 2) '_ ALP I)))
              (SETQ PDER_TEMP (MKID NLVAR_TEMP '_X))
              (SETQ DE_TEMP
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J 1)
                        (COND ((MINUSP (DIFFERENCE N_DV J)) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         (LIST 'TIMES (LIST TAIL_VEC J ALP)
                                               (MKID (NTH LOC_ARG* J) I))
                                         NIL)))
                       LOOPLABEL
                        (SETQ J (PLUS2 J 1))
                        (COND
                         ((MINUSP (DIFFERENCE N_DV J)) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 (LIST 'TIMES (LIST TAIL_VEC J ALP)
                                       (MKID (NTH LOC_ARG* J) I))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ NLOC_ARG_3 (CONS NLVAR_TEMP NLOC_ARG_3))
              (SETQ PRINCIPAL_DER (CONS PDER_TEMP PRINCIPAL_DER))
              (SETQ DE (CONS (REVAL1 (CONS 'PLUS DE_TEMP) NIL) DE))))
           (CAR EL))
          (SETQ EL (CDR EL))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ NLOC_ARG_3 (REVERSE NLOC_ARG_3))
      (SETQ PRINCIPAL_DER (CONS 'LIST (REVERSE PRINCIPAL_DER)))
      (SETQ DE (CONS 'LIST (REVERSE DE)))
      (SETQ DEP_VAR_JETSPACE
              (APPEND DEP_VAR_EQU (APPEND LOC_ARG_3 NLOC_ARG_3)))
      (CDE (LIST 'LIST INDEP_VAR DEP_VAR_JETSPACE (LIST 'LIST) TOTAL_ORDER)
       (LIST 'LIST PRINCIPAL_DER DE (LIST 'LIST) (LIST 'LIST)))
      (SETQ DEP_VAR_TOT
              (LIST 'LIST DEP_VAR_EQU (CONS 'LIST LOC_ARG_3)
                    (CONS 'LIST NLOC_ARG_3)))
      (RETURN DEP_VAR_TOT))) 
(FLAG '(CDE_WEAKLYNL) 'OPFN) 
(ENDMODULE) 
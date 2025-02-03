(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FRAMES)) 
(GLOBAL
 '(BASISFORML* BASISVECTORL* KEEPL* NATURALFRAME2COFRAME DBASEFORM2BASE2FORM
   DIMEX* INDXL* NATURALVECTOR2FRAMEVECTOR COORD* CURSYM* DETM*
   COMMUTATOR-OF-FRAMEVECTORS)) 
(FLUID '(METRICD* METRICU* ALGLIST* INDL KORD* SUBFG*)) 
(PUT 'COFRAMESTAT 'NUMBER-OF-ARGS 0) 
(PUT 'COFRAMESTAT 'DEFINED-ON-LINE '36) 
(PUT 'COFRAMESTAT 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'COFRAMESTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COFRAMESTAT NIL
    (PROG (FRAMEL METRIC)
      (FLAG '(WITH) 'DELIM)
      (SETQ FRAMEL (CDR (RLIS)))
      (REMFLAG '(WITH) 'DELIM)
      (COND ((EQ CURSYM* '*SEMICOL*) (GO A)))
      (COND ((EQ (SCAN) 'METRIC) (SETQ METRIC (XREAD T)))
            ((EQ CURSYM* 'SIGNATURE) (SETQ METRIC (RLIS)))
            (T (SYMERR 'COFRAME T)))
     A
      (COFRAM FRAMEL METRIC))) 
(PUT 'COFRAME 'STAT 'COFRAMESTAT) 
(PUT 'COFRAM 'NUMBER-OF-ARGS 2) 
(PUT 'COFRAM 'DEFINED-ON-LINE '53) 
(PUT 'COFRAM 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'COFRAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COFRAM (U V)
    (PROG (ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (RMSUBS)
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQ (CAR J) 'EQUAL) (CDR J))
                                          (T (LIST J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((EQ (CAR J) 'EQUAL) (CDR J)) (T (LIST J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PUTFORM (CAAR U) 1)
      (SETQ BASISFORML*
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (*A2K (CAR J))) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (*A2K (CAR J))) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ INDXL*
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J BASISFORML*)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CADR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CADR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIMEX* (LENGTH U))
      (SETQ BASISVECTORL* NIL)
      (COND ((NULL V) (SETQ METRICD* (NLIST 1 DIMEX*)))
            ((EQ (CAR V) 'SIGNATURE)
             (COND
              ((NEQ DIMEX* (LENGTH (CDR V)))
               (RERROR 'EXCALC 12
                       "Dimension of coframe and metric are inconsistent."))
              (T
               (SETQ METRICD*
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J (CDR V))
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
                                 (CONS ((LAMBDA (J) (REVAL1 J NIL)) (CAR J))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (COND
       ((OR (NULL V) (EQ (CAR V) 'SIGNATURE))
        (PROGN
         (SETQ DETM*
                 (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (SIMP (CAR METRICD*))))
         (PROG (J)
           (SETQ J (CDR METRICD*))
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (SETQ DETM*
                      (PROGN
                       (SETQ ALGLIST* (CONS NIL NIL))
                       (MULTSQ (SIMP J) DETM*))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (SETQ DETM* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (MK*SQ DETM*)))
         (SETQ METRICU*
                 (SETQ METRICD*
                         (PAIR INDXL*
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J (PAIR INDXL* METRICD*))
                                 (COND ((NULL J) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (J) (LIST J))
                                                   (CAR J))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (CDR J))
                                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS ((LAMBDA (J) (LIST J)) (CAR J))
                                               NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))))))
       (T (MKMETRIC V)))
      (COND ((FLAGP 'PARTDF 'NOXPND) (REMFLAG '(PARTDF) 'NOXPND)))
      (PUTFORM (CONS 'EPS INDXL*) 0)
      (PUT 'EPS 'INDXSYMMETRIES
           (LIST
            (LIST 'LAMBDA '(INDL)
                  (LIST 'TOT-ASYM-INDP
                        (LIST 'EVLIS
                              (MKQUOTE
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J 1)
                                 (COND
                                  ((MINUSP (DIFFERENCE DIMEX* J))
                                   (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS (LIST 'NTH 'INDL J)
                                                       NIL)))
                                LOOPLABEL
                                 (SETQ J (PLUS2 J 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE DIMEX* J))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS (LIST 'NTH 'INDL J) NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))))))
      (PUT 'EPS 'INDXSYMMETRIZE
           (LIST
            (LIST 'LAMBDA '(INDL)
                  (LIST 'ASYMMETRIZE-INDS
                        (MKQUOTE
                         (PROG (J FORALL-RESULT FORALL-ENDPTR)
                           (SETQ J 1)
                           (COND ((MINUSP (DIFFERENCE DIMEX* J)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS J NIL)))
                          LOOPLABEL
                           (SETQ J (PLUS2 J 1))
                           (COND
                            ((MINUSP (DIFFERENCE DIMEX* J))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS J NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                        'INDL))))
      (FLAG '(EPS) 'COVARIANT)
      (SETK
       (CONS 'EPS
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J INDXL*)
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J))
                                     NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
       1)
      (COND ((NULL (CDAR U)) (RETURN NIL)))
      (SETQ KEEPL*
              (APPEND
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J U)
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J) (CONS (*A2K (CAR J)) (CADR J)))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J) (CONS (*A2K (CAR J)) (CADR J))) (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               KEEPL*))
      (COFRAME1
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J U)
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (CADR J)) (CAR J)) NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (CADR J)) (CAR J)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'COFRAME1 'NUMBER-OF-ARGS 1) 
(PUT 'COFRAME1 'DEFINED-ON-LINE '95) 
(PUT 'COFRAME1 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'COFRAME1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COFRAME1 (U)
    (PROG (OSUBFG SCOORD V Y W METRICU* METRICD*)
      (SETQ OSUBFG SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ V
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ Y (PARTITOP J))
                                     (SETQ SCOORD (PICKUPCOORDS Y SCOORD))
                                     Y))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ Y (PARTITOP J))
                             (SETQ SCOORD (PICKUPCOORDS Y SCOORD))
                             Y))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NULL (ATOM (CAR SCOORD)))
        (PROGN
         (REMFLAG (LIST (CAAR SCOORD)) 'COVARIANT)
         (SETQ SCOORD
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J SCOORD)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (CAAAR (CAR (CDAR (PARTITOP J)))))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J) (CAAAR (CAR (CDAR (PARTITOP J)))))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ V
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J U)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (PARTITOP J)) (CAR J))
                                         NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (PARTITOP J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (COND
       ((NEQ (LENGTH SCOORD) DIMEX*) (RERROR 'EXCALC 3 "badly formed basis")))
      (SETQ W (*PF2MATWRTCOORDS V SCOORD))
      (SETQ NATURALVECTOR2FRAMEVECTOR V)
      (SETQ SUBFG* NIL)
      (SETQ NATURALFRAME2COFRAME
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J
                        (LNRSOLVE W
                                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ K BASISFORML*)
                                    (COND ((NULL K) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (K)
                                                        (LIST
                                                         (CONS
                                                          (LIST
                                                           (CONS (CONS K 1) 1))
                                                          1)))
                                                      (CAR K))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ K (CDR K))
                                    (COND ((NULL K) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (K)
                                                (LIST
                                                 (CONS
                                                  (LIST (CONS (CONS K 1) 1))
                                                  1)))
                                              (CAR K))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
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
      (SETQ SUBFG* OSUBFG)
      (SETQ COORD* SCOORD)
      (SETQ NATURALFRAME2COFRAME
              (PAIR SCOORD
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J NATURALFRAME2COFRAME)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (MK*SQPF (REPARTIT (PARTITSQ* J))))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (MK*SQPF (REPARTIT (PARTITSQ* J))))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ DBASEFORM2BASE2FORM
              (PAIR BASISFORML*
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J V)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (MK*SQPF (REPARTIT (EXDFPF J))))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (MK*SQPF (REPARTIT (EXDFPF J))))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))) 
(PUT 'PICKUPCOORDS 'NUMBER-OF-ARGS 2) 
(PUT 'PICKUPCOORDS 'DEFINED-ON-LINE '126) 
(PUT 'PICKUPCOORDS 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'PICKUPCOORDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PICKUPCOORDS (U V)
    (COND ((NULL U) V)
          ((NULL (EQCAR (CAAR U) 'D)) (RERROR 'EXCALC 4 "badly formed basis"))
          ((NULL V)
           (PROGN
            (PUTFORM (CADR (CAAR U)) 0)
            (PICKUPCOORDS (CDR U) (CONS (CADR (CAAR U)) NIL))))
          ((ORDOP (CADR (CAAR U)) (CAR V))
           (COND ((EQ (CADR (CAAR U)) (CAR V)) (PICKUPCOORDS (CDR U) V))
                 (T
                  (PROGN
                   (PUTFORM (CADR (CAAR U)) 0)
                   (PICKUPCOORDS (CDR U) (CONS (CADR (CAAR U)) V))))))
          (T
           (PICKUPCOORDS (CDR U)
            (CONS (CAR V)
                  (PICKUPCOORDS (CONS (CONS (CAAR U) (CONS 1 1)) NIL)
                   (CDR V))))))) 
(PUT '*PF2MATWRTCOORDS 'NUMBER-OF-ARGS 2) 
(PUT '*PF2MATWRTCOORDS 'DEFINED-ON-LINE '141) 
(PUT '*PF2MATWRTCOORDS 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT '*PF2MATWRTCOORDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *PF2MATWRTCOORDS (U V)
    (COND ((NULL U) NIL)
          (T (CONS (*PF2COLWRTCOORDS (CAR U) V) (*PF2MATWRTCOORDS (CDR U) V))))) 
(PUT '*PF2COLWRTCOORDS 'NUMBER-OF-ARGS 2) 
(PUT '*PF2COLWRTCOORDS 'DEFINED-ON-LINE '145) 
(PUT '*PF2COLWRTCOORDS 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT '*PF2COLWRTCOORDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *PF2COLWRTCOORDS (U V)
    (COND ((NULL V) NIL)
          ((AND U (EQ (CADR (CAAR U)) (CAR V)))
           (CONS (CDAR U) (*PF2COLWRTCOORDS (CDR U) (CDR V))))
          (T (CONS (CONS NIL 1) (*PF2COLWRTCOORDS U (CDR V)))))) 
(PUT 'COORDP 'NUMBER-OF-ARGS 1) 
(PUT 'COORDP 'DEFINED-ON-LINE '151) 
(PUT 'COORDP 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'COORDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COORDP (U) (MEMQ U COORD*)) 
(PUT 'MKMETRIC 'NUMBER-OF-ARGS 1) 
(PUT 'MKMETRIC 'DEFINED-ON-LINE '154) 
(PUT 'MKMETRIC 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKMETRIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMETRIC (U)
    (PROG (X Y Z OKORD)
      (PUTFORM (LIST (CADR U) NIL NIL) 0)
      (PUT (CADR U) 'INDXSYMMETRIES
           '((LAMBDA (INDL)
               (TOT-SYM-INDP (EVLIS '((NTH INDL 1) (NTH INDL 2)))))))
      (PUT (CADR U) 'INDXSYMMETRIZE
           '((LAMBDA (INDL) (SYMMETRIZE-INDS '(1 2) INDL))))
      (FLAG (LIST (CADR U)) 'COVARIANT)
      (SETQ OKORD KORD*)
      (SETQ KORD* BASISFORML*)
      (SETQ X (SIMP* (CADDR U)))
      (SETQ Y INDXL*)
      (SETQ METRICU* T)
      (PROG (J)
        (SETQ J INDXL*)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (PROG (K)
              (SETQ K Y)
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (SETK (LIST (CADR U) (LIST 'MINUS J) (LIST 'MINUS K)) 0))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))
            (SETQ Y (CDR Y))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (PROG (J)
        (SETQ J (PARTITSQ X 'BASEP))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         ((EQUAL (CDAAR (CAAR J)) 2)
          (SETK
           (LIST (CADR U) (LIST 'MINUS (CADR (CAAAR (CAAR J))))
                 (LIST 'MINUS (CADR (CAAAR (CAAR J)))))
           (MK*SQ (CDAR J))))
         (T
          (SETK
           (LIST (CADR U) (LIST 'MINUS (CADR (CAAAR (CAAR J))))
                 (LIST 'MINUS (CADR (CAAAR (CDAR (CAAR J))))))
           (MK*SQ (MULTSQ (CDAR J) (CONS 1 2))))))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ KORD* OKORD)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J INDXL*)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ K INDXL*)
                                      (COND ((NULL K) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (SIMPINDEXVAR
                                                           (LIST (CADR U)
                                                                 (LIST 'MINUS
                                                                       J)
                                                                 (LIST 'MINUS
                                                                       K))))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (SIMPINDEXVAR
                                                   (LIST (CADR U)
                                                         (LIST 'MINUS J)
                                                         (LIST 'MINUS K))))
                                                (CAR K))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROG (K FORALL-RESULT FORALL-ENDPTR)
                              (SETQ K INDXL*)
                              (COND ((NULL K) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (SIMPINDEXVAR
                                                   (LIST (CADR U)
                                                         (LIST 'MINUS J)
                                                         (LIST 'MINUS K))))
                                                (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K)
                                          (SIMPINDEXVAR
                                           (LIST (CADR U) (LIST 'MINUS J)
                                                 (LIST 'MINUS K))))
                                        (CAR K))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Z SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ Y (LNRSOLVE X (GENERATEIDENT (LENGTH INDXL*))))
      (SETQ SUBFG* Z)
      (SETQ METRICD* (MKASMETRIC X))
      (SETQ METRICU* (MKASMETRIC Y))
      (SETQ DETM* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (MK*SQ (DETQ X)))))) 
(PUT 'MKASMETRIC 'NUMBER-OF-ARGS 1) 
(PUT 'MKASMETRIC 'DEFINED-ON-LINE '195) 
(PUT 'MKASMETRIC 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKASMETRIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKASMETRIC (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J (PAIR INDXL* U))
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (CONS (CAR J)
                                (PROG (W Z)
                                  (SETQ W INDXL*)
                                  (PROG (K)
                                    (SETQ K (CDR J))
                                   LAB
                                    (COND ((NULL K) (RETURN NIL)))
                                    ((LAMBDA (K)
                                       (PROGN
                                        (COND
                                         ((CAR K)
                                          (SETQ Z
                                                  (CONS
                                                   (CONS (CAR W) (MK*SQ K))
                                                   Z))))
                                        (SETQ W (CDR W))))
                                     (CAR K))
                                    (SETQ K (CDR K))
                                    (GO LAB))
                                  (RETURN Z))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (CONS (CAR J)
                        (PROG (W Z)
                          (SETQ W INDXL*)
                          (PROG (K)
                            (SETQ K (CDR J))
                           LAB
                            (COND ((NULL K) (RETURN NIL)))
                            ((LAMBDA (K)
                               (PROGN
                                (COND
                                 ((CAR K)
                                  (SETQ Z (CONS (CONS (CAR W) (MK*SQ K)) Z))))
                                (SETQ W (CDR W))))
                             (CAR K))
                            (SETQ K (CDR K))
                            (GO LAB))
                          (RETURN Z))))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'FRAME 'NUMBER-OF-ARGS 1) 
(PUT 'FRAME 'DEFINED-ON-LINE '206) 
(PUT 'FRAME 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'FRAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FRAME (U)
    (PROG (Y)
      (PUTFORM (LIST (CAR U) NIL) (MINUS 1))
      (FLAG (LIST (CAR U)) 'COVARIANT)
      (SETQ BASISVECTORL*
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J INDXL*)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (*A2K (LIST (CAR U) (LIST 'MINUS J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (*A2K (LIST (CAR U) (LIST 'MINUS J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL DBASEFORM2BASE2FORM) (RETURN NIL)))
      (SETQ COMMUTATOR-OF-FRAMEVECTORS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (PICKUPWEDGES DBASEFORM2BASE2FORM))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS (LIST (CADADR J) (CADADR (CDR J)))
                                          (MK*SQPF
                                           (MKCOMMUTATORFV J
                                            DBASEFORM2BASE2FORM))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (CONS (LIST (CADADR J) (CADADR (CDR J)))
                                  (MK*SQPF
                                   (MKCOMMUTATORFV J DBASEFORM2BASE2FORM))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Y (PAIR BASISVECTORL* NATURALVECTOR2FRAMEVECTOR))
      (SETQ NATURALVECTOR2FRAMEVECTOR
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J COORD*)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS J (MK*SQPF (MKNAT2FRAMV J Y))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (CONS J (MK*SQPF (MKNAT2FRAMV J Y))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))) 
(PUT 'PICKUPWEDGES 'NUMBER-OF-ARGS 1) 
(PUT 'PICKUPWEDGES 'DEFINED-ON-LINE '223) 
(PUT 'PICKUPWEDGES 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'PICKUPWEDGES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PICKUPWEDGES (U) (PICKUPWEDGES1 U NIL)) 
(PUT 'PICKUPWEDGES1 'NUMBER-OF-ARGS 2) 
(PUT 'PICKUPWEDGES1 'DEFINED-ON-LINE '226) 
(PUT 'PICKUPWEDGES1 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'PICKUPWEDGES1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PICKUPWEDGES1 (U V)
    (COND ((NULL U) V) ((NULL (CDAR U)) (PICKUPWEDGES1 (CDR U) V))
          ((NULL V)
           (PICKUPWEDGES1 (CONS (CONS (CAAR U) (CDR (CDAR U))) (CDR U))
            (CONS (CAAR (CDAR U)) NIL)))
          ((MEMQ (CAAR (CDAR U)) V)
           (PICKUPWEDGES1
            (COND
             ((CDR (CDAR U)) (CONS (CONS (CAAR U) (CDR (CDAR U))) (CDR U)))
             (T (CDR U)))
            V))
          (T
           (PICKUPWEDGES1
            (COND
             ((CDR (CDAR U)) (CONS (CONS (CAAR U) (CDR (CDAR U))) (CDR U)))
             (T (CDR U)))
            (CONS (CAAR (CDAR U)) V))))) 
(PUT 'MKBASEVECTOR 'NUMBER-OF-ARGS 1) 
(PUT 'MKBASEVECTOR 'DEFINED-ON-LINE '239) 
(PUT 'MKBASEVECTOR 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKBASEVECTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKBASEVECTOR (U) (*A2K (LIST (CAAR BASISVECTORL*) (LIST 'MINUS U)))) 
(PUT 'MKCOMMUTATORFV 'NUMBER-OF-ARGS 2) 
(PUT 'MKCOMMUTATORFV 'DEFINED-ON-LINE '242) 
(PUT 'MKCOMMUTATORFV 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKCOMMUTATORFV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKCOMMUTATORFV (U V)
    (COND ((NULL V) NIL)
          (T
           (ADDPF (MKCOMMUTATORFV1 U (MKBASEVECTOR (CADAAR V)) (CDAR V))
            (MKCOMMUTATORFV U (CDR V)))))) 
(PUT 'MKCOMMUTATORFV1 'NUMBER-OF-ARGS 3) 
(PUT 'MKCOMMUTATORFV1 'DEFINED-ON-LINE '247) 
(PUT 'MKCOMMUTATORFV1 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKCOMMUTATORFV1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKCOMMUTATORFV1 (U V W)
    (COND ((NULL W) NIL)
          ((EQ U (CAAR W)) (CONS (CONS V (NEGSQ (SIMP* (CDAR W)))) NIL))
          ((ORDOP U (CAAR W)) NIL) (T (MKCOMMUTATORFV1 U V (CDR W))))) 
(PUT 'MKNAT2FRAMV 'NUMBER-OF-ARGS 2) 
(PUT 'MKNAT2FRAMV 'DEFINED-ON-LINE '254) 
(PUT 'MKNAT2FRAMV 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKNAT2FRAMV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKNAT2FRAMV (U V)
    (COND ((NULL V) NIL)
          (T
           (ADDPF (MKNAT2FRAMV1 U (CAAR V) (CDAR V)) (MKNAT2FRAMV U (CDR V)))))) 
(PUT 'MKNAT2FRAMV1 'NUMBER-OF-ARGS 3) 
(PUT 'MKNAT2FRAMV1 'DEFINED-ON-LINE '258) 
(PUT 'MKNAT2FRAMV1 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKNAT2FRAMV1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKNAT2FRAMV1 (U V W)
    (COND ((NULL W) NIL) ((EQ U (CADR (CAAR W))) (CONS (CONS V (CDAR W)) NIL))
          ((ORDOP U (CADR (CAAR W))) NIL) (T (MKNAT2FRAMV1 U V (CDR W))))) 
(PUT 'DUALFRAME 'NUMBER-OF-ARGS 1) 
(PUT 'DUALFRAME 'DEFINED-ON-LINE '265) 
(PUT 'DUALFRAME 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'DUALFRAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUALFRAME (U)
    (RERROR 'EXCALC 5 "Dualframe no longer supported - use frame instead")) 
(PUT 'RIEMANNCONX 'NUMBER-OF-ARGS 1) 
(PUT 'RIEMANNCONX 'DEFINED-ON-LINE '268) 
(PUT 'RIEMANNCONX 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'RIEMANNCONX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RIEMANNCONX (U) (RIEMCONNECTION (CAR U))) 
(PUT 'RIEMANNCONX 'STAT 'RLIS) 
(DE MKBASFORMSQ (U) (MKSQ (LIST (CAAR BASISFORML*) U) 1)) 
(PUT 'MKBASFORMSQ 'NUMBER-OF-ARGS 1) 
(PUT 'MKBASFORMSQ 'DEFINED-ON-LINE '273) 
(PUT 'MKBASFORMSQ 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKBASFORMSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MKBASFORMSQ 'INLINE '(LAMBDA (U) (MKSQ (LIST (CAAR BASISFORML*) U) 1))) 
(PUT 'RIEMCONNECTION 'NUMBER-OF-ARGS 1) 
(PUT 'RIEMCONNECTION 'DEFINED-ON-LINE '276) 
(PUT 'RIEMCONNECTION 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'RIEMCONNECTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RIEMCONNECTION (U)
    (PROG ()
      (PUTFORM (LIST U NIL NIL) 1)
      (FLAG (LIST U) 'COVARIANT)
      (PUT U 'INDXSYMMETRIES
           '((LAMBDA (INDL)
               (TOT-ASYM-INDP (EVLIS '((NTH INDL 1) (NTH INDL 2)))))))
      (PUT U 'INDXSYMMETRIZE '((LAMBDA (INDL) (ASYMMETRIZE-INDS '(1 2) INDL))))
      (PROG (J)
        (SETQ J INDXL*)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROG (K)
             (SETQ K INDXL*)
            LAB
             (COND ((NULL K) (RETURN NIL)))
             ((LAMBDA (K)
                (COND
                 ((AND (NEQ J K) (INDORDP J K))
                  (SETK (LIST U (LIST 'MINUS J) (LIST 'MINUS K)) 0))))
              (CAR K))
             (SETQ K (CDR K))
             (GO LAB)))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RIEMCONPART1 U)
      (RIEMCONPART2 U)
      (RIEMCONPART3 U))) 
(PUT 'RIEMCONPART1 'NUMBER-OF-ARGS 1) 
(PUT 'RIEMCONPART1 'DEFINED-ON-LINE '295) 
(PUT 'RIEMCONPART1 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'RIEMCONPART1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RIEMCONPART1 (U)
    (PROG (COVBASEFORM INDX1 INDX2 INDX3 VARL W Z)
      (PROG (L)
        (SETQ L DBASEFORM2BASE2FORM)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (PROGN
            (SETQ COVBASEFORM
                    (PARTITINDEXVAR (LIST (CAAR L) (LIST 'MINUS (CADAR L)))))
            (PROG (J)
              (SETQ J (CDR L))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              (PROGN
               (SETQ VARL (CDR (CAAR J)))
               (SETQ INDX1 (CADAR VARL))
               (SETQ INDX2 (CADADR VARL))
               (PROG (Y)
                 (SETQ Y COVBASEFORM)
                LAB
                 (COND ((NULL Y) (RETURN NIL)))
                 (PROGN
                  (SETQ W (LIST U (LIST 'MINUS INDX1) (LIST 'MINUS INDX2)))
                  (SETQ Z
                          (MULTSQ (CONS (MINUS 1) 2)
                                  (*PF2SQ
                                   (MULTPFSQ (CONS (CAR Y) NIL)
                                    (SIMP* (CDAR J))))))
                  (SETK W (MK*SQ (ADDSQ Z (MKSQ W 1))))
                  (SETQ INDX3 (CADR (CAAR Y)))
                  (SETQ Z
                          (MULTSQ (CONS (MINUS 1) 2)
                                  (MULTSQ (CDAR Y) (SIMP* (CDAR J)))))
                  (COND
                   ((NEQ INDX1 INDX3)
                    (COND
                     ((INDORDP INDX1 INDX3)
                      (PROGN
                       (SETQ W
                               (LIST U (LIST 'MINUS INDX1)
                                     (LIST 'MINUS INDX3)))
                       (SETK W
                             (MK*SQ
                              (ADDSQ
                               (MULTSQ Z
                                       (MKSQ (LIST (CAAR BASISFORML*) INDX2)
                                             1))
                               (MKSQ W 1))))))
                     (T
                      (PROGN
                       (SETQ W
                               (LIST U (LIST 'MINUS INDX3)
                                     (LIST 'MINUS INDX1)))
                       (SETK W
                             (MK*SQ
                              (ADDSQ
                               (MULTSQ (NEGSQ Z)
                                       (MKSQ (LIST (CAAR BASISFORML*) INDX2)
                                             1))
                               (MKSQ W 1)))))))))
                  (COND
                   ((NEQ INDX2 INDX3)
                    (COND
                     ((INDORDP INDX2 INDX3)
                      (PROGN
                       (SETQ W
                               (LIST U (LIST 'MINUS INDX2)
                                     (LIST 'MINUS INDX3)))
                       (SETK W
                             (MK*SQ
                              (ADDSQ
                               (MULTSQ (NEGSQ Z)
                                       (MKSQ (LIST (CAAR BASISFORML*) INDX1)
                                             1))
                               (MKSQ W 1))))))
                     (T
                      (PROGN
                       (SETQ W
                               (LIST U (LIST 'MINUS INDX3)
                                     (LIST 'MINUS INDX2)))
                       (SETK W
                             (MK*SQ
                              (ADDSQ
                               (MULTSQ Z
                                       (MKSQ (LIST (CAAR BASISFORML*) INDX1)
                                             1))
                               (MKSQ W 1))))))))))
                 (SETQ Y (CDR Y))
                 (GO LAB)))
              (SETQ J (CDR J))
              (GO LAB))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB)))) 
(PUT 'RIEMCONPART2 'NUMBER-OF-ARGS 1) 
(PUT 'RIEMCONPART2 'DEFINED-ON-LINE '333) 
(PUT 'RIEMCONPART2 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'RIEMCONPART2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RIEMCONPART2 (U)
    (PROG (DGKL INDX1 INDX2 VARL W Z)
      (COND ((NULL (SETQ DGKL (MKMETRICCONX2 METRICD*))) (RETURN NIL)))
      (PROG (J)
        (SETQ J DGKL)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROG (Y)
             (SETQ Y (CDR J))
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             (PROGN
              (SETQ VARL (CAAR Y))
              (SETQ INDX1 (CADAR VARL))
              (SETQ INDX2 (CADADR VARL))
              (SETQ W (LIST U (LIST 'MINUS INDX1) (LIST 'MINUS INDX2)))
              (SETQ Z
                      (MULTSQ (CONS (MINUS 1) 2)
                              (MULTSQ (CONS (LIST (CONS (CONS (CAR J) 1) 1)) 1)
                                      (CDAR Y))))
              (SETK W (MK*SQ (ADDSQ Z (MKSQ W 1)))))
             (SETQ Y (CDR Y))
             (GO LAB)))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB)))) 
(PUT 'MKMETRICCONX2 'NUMBER-OF-ARGS 1) 
(PUT 'MKMETRICCONX2 'DEFINED-ON-LINE '347) 
(PUT 'MKMETRICCONX2 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKMETRICCONX2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMETRICCONX2 (U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND
               (X
                (CONS
                 (CONS (CAAR (MKUPF (LIST (CAAR BASISFORML*) (CAAR U)))) X)
                 (MKMETRICCONX2 (CDR U))))
               (T (MKMETRICCONX2 (CDR U)))))
            (MKMETRICCONX21 (CDAR U)))))) 
(PUT 'MKMETRICCONX21 'NUMBER-OF-ARGS 1) 
(PUT 'MKMETRICCONX21 'DEFINED-ON-LINE '354) 
(PUT 'MKMETRICCONX21 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKMETRICCONX21 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMETRICCONX21 (U)
    (COND ((NULL U) NIL)
          (T
           (ADDPF
            (WEDGEPF2 (EXDF0 (SIMP* (CDAR U)))
             (CONS
              (CONS (LIST (CAAR (MKUPF (LIST (CAAR BASISFORML*) (CAAR U)))))
                    (CONS 1 1))
              NIL))
            (MKMETRICCONX21 (CDR U)))))) 
(PUT 'RIEMCONPART3 'NUMBER-OF-ARGS 1) 
(PUT 'RIEMCONPART3 'DEFINED-ON-LINE '360) 
(PUT 'RIEMCONPART3 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'RIEMCONPART3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RIEMCONPART3 (U)
    (PROG (DG DGK DGKL W X Z)
      (COND ((NULL (SETQ DG (MKMETRICCONX3 METRICD*))) (RETURN NIL)))
      (REMPROP U 'INDXSYMMETRIES)
      (REMPROP U 'INDXSYMMETRIZE)
      (PROG (J)
        (SETQ J INDXL*)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (COND ((AND DG (SETQ DGK (ATSOC J DG))) (SETQ DGK (CDR DGK)))
                  (T (SETQ DGK NIL)))
            (PROG (K)
              (SETQ K INDXL*)
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (COND
                  ((INDORDP J K)
                   (PROGN
                    (SETQ W (LIST U (LIST 'MINUS J) (LIST 'MINUS K)))
                    (SETQ X (COND ((EQ J K) (CONS NIL 1)) (T (MKSQ W 1))))
                    (COND
                     ((AND DGK (SETQ DGKL (ATSOC K DGK)))
                      (SETQ DGKL (CDR DGKL)))
                     (T (SETQ DGKL (CONS NIL 1))))
                    (SETQ Z (MULTSQ (CONS 1 2) DGKL))
                    (SETK W (MK*SQ (ADDSQ Z X)))
                    (SETQ W (LIST U (LIST 'MINUS K) (LIST 'MINUS J)))
                    (SETK W (MK*SQ (ADDSQ Z (NEGSQ X))))))))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB)))) 
(PUT 'MKMETRICCONX3 'NUMBER-OF-ARGS 1) 
(PUT 'MKMETRICCONX3 'DEFINED-ON-LINE '383) 
(PUT 'MKMETRICCONX3 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKMETRICCONX3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMETRICCONX3 (U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND (X (CONS (CONS (CAAR U) X) (MKMETRICCONX3 (CDR U))))
                    (T (MKMETRICCONX3 (CDR U)))))
            (MKMETRICCONX31 (CDAR U)))))) 
(PUT 'MKMETRICCONX31 'NUMBER-OF-ARGS 1) 
(PUT 'MKMETRICCONX31 'DEFINED-ON-LINE '389) 
(PUT 'MKMETRICCONX31 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'MKMETRICCONX31 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMETRICCONX31 (U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND (X (CONS (CONS (CAAR U) X) (MKMETRICCONX31 (CDR U))))
                    (T (MKMETRICCONX31 (CDR U)))))
            (*PF2SQ (EXDF0 (SIMP* (CDAR U)))))))) 
(PUT 'BASEP 'NUMBER-OF-ARGS 1) 
(PUT 'BASEP 'DEFINED-ON-LINE '395) 
(PUT 'BASEP 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'BASEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BASEP (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (OR
            (COND ((SFP (CAAAR U)) (BASEP (CAAAR U)))
                  (T (EQCAR (CAAAR U) (CAAR BASISFORML*))))
            (BASEP (CDAR U)) (BASEP (CDR U)))))) 
(PUT 'WEDGEFP 'NUMBER-OF-ARGS 1) 
(PUT 'WEDGEFP 'DEFINED-ON-LINE '402) 
(PUT 'WEDGEFP 'DEFINED-IN-FILE 'EXCALC/FRAMES.RED) 
(PUT 'WEDGEFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WEDGEFP (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (OR
            (COND ((SFP (CAAAR U)) (WEDGEFP (CAAAR U)))
                  (T (EQCAR (CAAAR U) 'WEDGE)))
            (WEDGEFP (CDAR U)) (WEDGEFP (CDR U)))))) 
(ENDMODULE) 
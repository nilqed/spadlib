(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PIDENT)) 
(REVISION 'PIDENT "$Id: pident.red 3961 2017-03-19 08:24:03Z thomas-sturm $") 
(COPYRIGHT 'PIDENT "(c) 2010-2017 T. Sturm") 
(SWITCH (LIST 'PIDENTMORE)) 
(LOAD-PACKAGE 'REDLOG) 
(LOAD-PACKAGE 'OFSF) 
(RL_SET '(DCFSF)) 
(FLAG '(PIDENT) 'OPFN) 
(PUT 'PIDENT 'NUMBER-OF-ARGS 4) 
(PUT 'PIDENT 'DEFINED-ON-LINE '41) 
(PUT 'PIDENT 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PIDENT (SYS IVL OVL PVL)
    (PROG (RES I)
      (SETQ I 0)
      (SETQ SYS (RL_SIMP SYS))
      (SETQ IVL (CDR IVL))
      (SETQ OVL (CDR OVL))
      (SETQ PVL (CDR PVL))
      (SETQ RES (PIDENT_PIDENT SYS IVL OVL PVL))
      (SETQ RES
              (COND
               (*PIDENTMORE
                (CONS 'LIST
                      (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                        (SETQ CASE RES)
                        (COND ((NULL CASE) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (CASE)
                                            (PROGN
                                             (SETQ I (PLUS I 1))
                                             (LIST 'LIST
                                                   (ID2STRING (MKID 'CASE I))
                                                   (CONS 'LIST
                                                         (PROG (EQN
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ EQN
                                                                   (CAR CASE))
                                                           (COND
                                                            ((NULL EQN)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  EQN)
                                                                               (RL_MK*FOF
                                                                                EQN))
                                                                             (CAR
                                                                              EQN))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ EQN (CDR EQN))
                                                           (COND
                                                            ((NULL EQN)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (EQN)
                                                                (RL_MK*FOF
                                                                 EQN))
                                                              (CAR EQN))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                   (CONS 'LIST
                                                         (PROG (NE
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ NE
                                                                   (CADR CASE))
                                                           (COND
                                                            ((NULL NE)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  NE)
                                                                               (RL_MK*FOF
                                                                                NE))
                                                                             (CAR
                                                                              NE))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ NE (CDR NE))
                                                           (COND
                                                            ((NULL NE)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (NE)
                                                                (RL_MK*FOF NE))
                                                              (CAR NE))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                   (CONS 'LIST
                                                         (PROG (PINFO
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ PINFO
                                                                   (CADDR
                                                                    CASE))
                                                           (COND
                                                            ((NULL PINFO)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  PINFO)
                                                                               (CONS
                                                                                'LIST
                                                                                PINFO))
                                                                             (CAR
                                                                              PINFO))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ PINFO
                                                                   (CDR PINFO))
                                                           (COND
                                                            ((NULL PINFO)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (PINFO)
                                                                (CONS 'LIST
                                                                      PINFO))
                                                              (CAR PINFO))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                   (CONS 'LIST
                                                         (PROG (C FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ C
                                                                   (CADDDR
                                                                    CASE))
                                                           (COND
                                                            ((NULL C)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  C)
                                                                               (MK*SQ
                                                                                (CONS
                                                                                 C
                                                                                 1)))
                                                                             (CAR
                                                                              C))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ C (CDR C))
                                                           (COND
                                                            ((NULL C)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (C)
                                                                (MK*SQ
                                                                 (CONS C 1)))
                                                              (CAR C))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL))))))
                                          (CAR CASE))
                                         NIL)))
                       LOOPLABEL
                        (SETQ CASE (CDR CASE))
                        (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CASE)
                                    (PROGN
                                     (SETQ I (PLUS I 1))
                                     (LIST 'LIST (ID2STRING (MKID 'CASE I))
                                           (CONS 'LIST
                                                 (PROG (EQN FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EQN (CAR CASE))
                                                   (COND
                                                    ((NULL EQN) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EQN)
                                                                       (RL_MK*FOF
                                                                        EQN))
                                                                     (CAR EQN))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EQN (CDR EQN))
                                                   (COND
                                                    ((NULL EQN)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EQN)
                                                               (RL_MK*FOF EQN))
                                                             (CAR EQN))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (CONS 'LIST
                                                 (PROG (NE FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ NE (CADR CASE))
                                                   (COND
                                                    ((NULL NE) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (NE)
                                                                       (RL_MK*FOF
                                                                        NE))
                                                                     (CAR NE))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ NE (CDR NE))
                                                   (COND
                                                    ((NULL NE)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (NE)
                                                               (RL_MK*FOF NE))
                                                             (CAR NE))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (CONS 'LIST
                                                 (PROG (PINFO FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ PINFO (CADDR CASE))
                                                   (COND
                                                    ((NULL PINFO)
                                                     (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (
                                                                          PINFO)
                                                                       (CONS
                                                                        'LIST
                                                                        PINFO))
                                                                     (CAR
                                                                      PINFO))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ PINFO (CDR PINFO))
                                                   (COND
                                                    ((NULL PINFO)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (PINFO)
                                                               (CONS 'LIST
                                                                     PINFO))
                                                             (CAR PINFO))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (CONS 'LIST
                                                 (PROG (C FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ C (CADDDR CASE))
                                                   (COND
                                                    ((NULL C) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (C)
                                                                       (MK*SQ
                                                                        (CONS C
                                                                              1)))
                                                                     (CAR C))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ C (CDR C))
                                                   (COND
                                                    ((NULL C)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (C)
                                                               (MK*SQ
                                                                (CONS C 1)))
                                                             (CAR C))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))))))
                                  (CAR CASE))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
               (T
                (CONS 'LIST
                      (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                        (SETQ CASE RES)
                        (COND ((NULL CASE) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (CASE)
                                            (PROGN
                                             (SETQ I (PLUS I 1))
                                             (LIST 'LIST
                                                   (ID2STRING (MKID 'CASE I))
                                                   (CONS 'LIST
                                                         (PROG (EQN
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ EQN
                                                                   (CAR CASE))
                                                           (COND
                                                            ((NULL EQN)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  EQN)
                                                                               (RL_MK*FOF
                                                                                EQN))
                                                                             (CAR
                                                                              EQN))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ EQN (CDR EQN))
                                                           (COND
                                                            ((NULL EQN)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (EQN)
                                                                (RL_MK*FOF
                                                                 EQN))
                                                              (CAR EQN))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                   (CONS 'LIST
                                                         (PROG (NE
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ NE
                                                                   (CADR CASE))
                                                           (COND
                                                            ((NULL NE)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  NE)
                                                                               (RL_MK*FOF
                                                                                NE))
                                                                             (CAR
                                                                              NE))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ NE (CDR NE))
                                                           (COND
                                                            ((NULL NE)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (NE)
                                                                (RL_MK*FOF NE))
                                                              (CAR NE))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                   (CONS 'LIST
                                                         (PROG (PINFO
                                                                FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ PINFO
                                                                   (CADDR
                                                                    CASE))
                                                           (COND
                                                            ((NULL PINFO)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  PINFO)
                                                                               (CONS
                                                                                'LIST
                                                                                PINFO))
                                                                             (CAR
                                                                              PINFO))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ PINFO
                                                                   (CDR PINFO))
                                                           (COND
                                                            ((NULL PINFO)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (PINFO)
                                                                (CONS 'LIST
                                                                      PINFO))
                                                              (CAR PINFO))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL))))))
                                          (CAR CASE))
                                         NIL)))
                       LOOPLABEL
                        (SETQ CASE (CDR CASE))
                        (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CASE)
                                    (PROGN
                                     (SETQ I (PLUS I 1))
                                     (LIST 'LIST (ID2STRING (MKID 'CASE I))
                                           (CONS 'LIST
                                                 (PROG (EQN FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EQN (CAR CASE))
                                                   (COND
                                                    ((NULL EQN) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EQN)
                                                                       (RL_MK*FOF
                                                                        EQN))
                                                                     (CAR EQN))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EQN (CDR EQN))
                                                   (COND
                                                    ((NULL EQN)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EQN)
                                                               (RL_MK*FOF EQN))
                                                             (CAR EQN))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (CONS 'LIST
                                                 (PROG (NE FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ NE (CADR CASE))
                                                   (COND
                                                    ((NULL NE) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (NE)
                                                                       (RL_MK*FOF
                                                                        NE))
                                                                     (CAR NE))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ NE (CDR NE))
                                                   (COND
                                                    ((NULL NE)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (NE)
                                                               (RL_MK*FOF NE))
                                                             (CAR NE))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (CONS 'LIST
                                                 (PROG (PINFO FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ PINFO (CADDR CASE))
                                                   (COND
                                                    ((NULL PINFO)
                                                     (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (
                                                                          PINFO)
                                                                       (CONS
                                                                        'LIST
                                                                        PINFO))
                                                                     (CAR
                                                                      PINFO))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ PINFO (CDR PINFO))
                                                   (COND
                                                    ((NULL PINFO)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (PINFO)
                                                               (CONS 'LIST
                                                                     PINFO))
                                                             (CAR PINFO))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))))))
                                  (CAR CASE))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))
      (RETURN RES))) 
(PUT 'PIDENT_MAXORDERL 'NUMBER-OF-ARGS 2) 
(PUT 'PIDENT_MAXORDERL 'DEFINED-ON-LINE '69) 
(PUT 'PIDENT_MAXORDERL 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_MAXORDERL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PIDENT_MAXORDERL (FL VL)
    (PROG (W M)
      (SETQ M 0)
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROG (F)
             (SETQ F FL)
            LAB
             (COND ((NULL F) (RETURN NIL)))
             ((LAMBDA (F)
                (PROGN
                 (SETQ W (DCFSF_ORDDEGF F V))
                 (COND ((GREATERP (CAR W) M) (SETQ M (CAR W))))))
              (CAR F))
             (SETQ F (CDR F))
             (GO LAB)))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN M))) 
(PUT 'PIDENT_PIDENT 'NUMBER-OF-ARGS 4) 
(PUT 'PIDENT_PIDENT 'DEFINED-ON-LINE '83) 
(PUT 'PIDENT_PIDENT 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_PIDENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PIDENT_PIDENT (SYS IVL OVL PVL)
    (PROG (CONTEXT THEO SOL DSOL CASEL VL RES)
      (PIDENT_CHECKVARS SYS IVL OVL PVL)
      (SETQ THEO
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PVL)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (DCFSF_0MK2 'EQUAL
                                     (CAR (SIMP (LIST 'D P 1)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (DCFSF_0MK2 'EQUAL (CAR (SIMP (LIST 'D P 1)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (X)
        (SETQ X IVL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ SYS (LIST 'EX X SYS))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ SOL (RL_QE SYS THEO))
      (SETQ DSOL (RL_DNF SOL))
      (SETQ CASEL
              (COND
               ((EQ (COND ((ATOM DSOL) DSOL) (T (CAR DSOL))) 'OR) (CDR DSOL))
               (T (LIST DSOL))))
      (SETQ VL (APPEND IVL OVL))
      (SETQ RES
              (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                (SETQ CASE CASEL)
                (COND ((NULL CASE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CASE) (PIDENT_DOIT1 CASE VL PVL))
                                  (CAR CASE))
                                 NIL)))
               LOOPLABEL
                (SETQ CASE (CDR CASE))
                (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CASE) (PIDENT_DOIT1 CASE VL PVL))
                          (CAR CASE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'PIDENT_CHECKVARS 'NUMBER-OF-ARGS 4) 
(PUT 'PIDENT_CHECKVARS 'DEFINED-ON-LINE '101) 
(PUT 'PIDENT_CHECKVARS 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_CHECKVARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PIDENT_CHECKVARS (SYS IVL OVL PVL)
    (PROG (V)
      (SETQ V (RL_FVARL SYS))
     LAB
      (COND ((NULL V) (RETURN NIL)))
      ((LAMBDA (V)
         (COND
          ((NOT (OR (MEMBER V IVL) (MEMBER V OVL) (MEMBER V PVL)))
           (REDERR (LIST "unspecified variable" V)))))
       (CAR V))
      (SETQ V (CDR V))
      (GO LAB))) 
(PUT 'PIDENT_DOIT1 'NUMBER-OF-ARGS 3) 
(PUT 'PIDENT_DOIT1 'DEFINED-ON-LINE '106) 
(PUT 'PIDENT_DOIT1 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_DOIT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PIDENT_DOIT1 (SOL VL PVL)
    (PROG (PSI CL EQL NEQL CVL SUBL PSIP HU HUHU RES)
      (PROG (G122)
        (SETQ G122
                (LTO_PARTITION (RL_ATL SOL)
                               (FUNCTION
                                (LAMBDA (AT)
                                  (EQ (COND ((ATOM AT) AT) (T (CAR AT)))
                                      'EQUAL)))))
        (SETQ EQL (CAR G122))
        (SETQ NEQL (CDR G122))
        (RETURN G122))
      (SETQ CL (PIDENT_CL EQL VL))
      (RL_SET '(R))
      (PROG (C)
        (SETQ C CL)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROG (V)
             (SETQ V (KERNELS C))
            LAB
             (COND ((NULL V) (RETURN NIL)))
             ((LAMBDA (V) (SETQ CVL (LTO_INSERTQ V CVL))) (CAR V))
             (SETQ V (CDR V))
             (GO LAB)))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (SETQ SUBL
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V CVL)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (CONS V (MKID V '_STAR)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CONS V (MKID V '_STAR))) (CAR V))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PSI
              ((LAMBDA (G124)
                 (COND ((AND G124 (CDR G124)) (CONS 'AND G124))
                       ((NULL G124) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G124))))
               (PROG (C FORALL-RESULT FORALL-ENDPTR)
                 (SETQ C CL)
                 (COND ((NULL C) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (C)
                                     (LIST 'EQUAL
                                           (ADDF C (NEGF (CAR (SUBF C SUBL))))
                                           NIL))
                                   (CAR C))
                                  NIL)))
                LOOPLABEL
                 (SETQ C (CDR C))
                 (COND ((NULL C) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (C)
                             (LIST 'EQUAL (ADDF C (NEGF (CAR (SUBF C SUBL))))
                                   NIL))
                           (CAR C))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (PROG (AT)
        (SETQ AT NEQL)
       LAB
        (COND ((NULL AT) (RETURN NIL)))
        ((LAMBDA (AT)
           (COND
            ((PIDENT_SUBSET (CL_FVARL1 AT) PVL)
             (SETQ PSI (CONS 'AND (LIST PSI AT))))))
         (CAR AT))
        (SETQ AT (CDR AT))
        (GO LAB))
      (SETQ RES
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PVL)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ PSIP
                                             (LIST 'EQUAL
                                                   (CAR
                                                    (SIMP
                                                     (LIST 'DIFFERENCE P
                                                           (MKID P '_STAR))))
                                                   NIL))
                                     (SETQ HU
                                             (RL_ALL (LIST 'IMPL PSI PSIP)
                                                     NIL))
                                     (SETQ HUHU (RL_QE HU NIL))
                                     (LIST P HUHU)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ PSIP
                                     (LIST 'EQUAL
                                           (CAR
                                            (SIMP
                                             (LIST 'DIFFERENCE P
                                                   (MKID P '_STAR))))
                                           NIL))
                             (SETQ HU (RL_ALL (LIST 'IMPL PSI PSIP) NIL))
                             (SETQ HUHU (RL_QE HU NIL))
                             (LIST P HUHU)))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RL_SET '(DCFSF))
      (RETURN (LIST EQL NEQL RES CL)))) 
(PUT 'PIDENT_CL 'NUMBER-OF-ARGS 2) 
(PUT 'PIDENT_CL 'DEFINED-ON-LINE '143) 
(PUT 'PIDENT_CL 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_CL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PIDENT_CL (EQL VL)
    (PROG (RES LHS KL)
      (PROG (AT)
        (SETQ AT EQL)
       LAB
        (COND ((NULL AT) (RETURN NIL)))
        ((LAMBDA (AT)
           (PROGN
            (SETQ LHS (DCFSF_ARG2L AT))
            (SETQ KL (PIDENT_VL2KL VL (PIDENT_MAXORDERL (LIST LHS) VL)))
            (PROG (C)
              (SETQ C (SFTO_ALLCOEFFS LHS KL))
             LAB
              (COND ((NULL C) (RETURN NIL)))
              ((LAMBDA (C)
                 (COND
                  ((NOT (OR (ATOM C) (ATOM (CAR C))))
                   (SETQ RES (LTO_INSERT (SFTO_DPRPARTF C) RES)))))
               (CAR C))
              (SETQ C (CDR C))
              (GO LAB))
            NIL))
         (CAR AT))
        (SETQ AT (CDR AT))
        (GO LAB))
      (RETURN RES))) 
(PUT 'PIDENT_VL2KL 'NUMBER-OF-ARGS 2) 
(PUT 'PIDENT_VL2KL 'DEFINED-ON-LINE '160) 
(PUT 'PIDENT_VL2KL 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_VL2KL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PIDENT_VL2KL (VL N)
    (PROG (V FORALL-RESULT FORALL-ENDPTR)
      (SETQ V VL)
     STARTOVER
      (COND ((NULL V) (RETURN NIL)))
      (SETQ FORALL-RESULT ((LAMBDA (V) (DCFSF_MKKL V N)) (CAR V)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ V (CDR V))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL V) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR ((LAMBDA (V) (DCFSF_MKKL V N)) (CAR V)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ V (CDR V))
      (GO LOOPLABEL))) 
(PUT 'PIDENT_SUBSET 'NUMBER-OF-ARGS 2) 
(PUT 'PIDENT_SUBSET 'DEFINED-ON-LINE '165) 
(PUT 'PIDENT_SUBSET 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'PIDENT_SUBSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PIDENT_SUBSET (M1 M2)
    (OR (NOT M1) (AND (MEMBER (CAR M1) M2) (PIDENT_SUBSET (CDR M1) M2)))) 
(PUT 'LTO_PARTITION 'NUMBER-OF-ARGS 2) 
(PUT 'LTO_PARTITION 'DEFINED-ON-LINE '168) 
(PUT 'LTO_PARTITION 'DEFINED-IN-FILE 'PIDENT/PIDENT.RED) 
(PUT 'LTO_PARTITION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LTO_PARTITION (L P)
    (PROG (L1 L2)
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((APPLY P (LIST X)) (SETQ L1 (CONS X L1)))
                 (T (SETQ L2 (CONS X L2)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS (REVERSIP L1) (REVERSIP L2))))) 
(ENDMODULE) 
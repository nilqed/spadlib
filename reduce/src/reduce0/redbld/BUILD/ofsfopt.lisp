(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFOPT)) 
(REVISION 'OFSFOPT "$Id: ofsfopt.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFOPT "(c) 1995-2009 A. Dolzmann, T. Sturm, 2016 T. Sturm") 
(DE OFSF_CVL (X) (CAR X)) 
(PUT 'OFSF_CVL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_CVL 'DEFINED-ON-LINE '32) 
(PUT 'OFSF_CVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_CVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'OFSF_CVL 'INLINE '(LAMBDA (X) (CAR X))) 
(DE OFSF_AL (X) (CADR X)) 
(PUT 'OFSF_AL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_AL 'DEFINED-ON-LINE '35) 
(PUT 'OFSF_AL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_AL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'OFSF_AL 'INLINE '(LAMBDA (X) (CADR X))) 
(DE OFSF_PL (X) (CADDR X)) 
(PUT 'OFSF_PL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PL 'DEFINED-ON-LINE '38) 
(PUT 'OFSF_PL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_PL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'OFSF_PL 'INLINE '(LAMBDA (X) (CADDR X))) 
(DE OFSF_AN (X) (CDDDR X)) 
(PUT 'OFSF_AN 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_AN 'DEFINED-ON-LINE '41) 
(PUT 'OFSF_AN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_AN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'OFSF_AN 'INLINE '(LAMBDA (X) (CDDDR X))) 
(DE OFSF_MKENTRY (CVL AL PL AN) (CONS CVL (CONS AL (CONS PL AN)))) 
(PUT 'OFSF_MKENTRY 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_MKENTRY 'DEFINED-ON-LINE '44) 
(PUT 'OFSF_MKENTRY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_MKENTRY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_MKENTRY 'INLINE
      '(LAMBDA (CVL AL PL AN) (CONS CVL (CONS AL (CONS PL AN))))) 
(PUT 'OFSF_SENDTASK 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SENDTASK 'DEFINED-ON-LINE '47) 
(PUT 'OFSF_SENDTASK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_SENDTASK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SENDTASK (HD P Z)
    (REMOTE_CALL* HD 'OFSF_OPT2
     (LIST (CAR P) (CADR P) (CADDR P) (CDDDR P) Z NIL NIL) 1)) 
(PUT 'OFSF_OPTMASTER 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_OPTMASTER 'DEFINED-ON-LINE '51) 
(PUT 'OFSF_OPTMASTER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTMASTER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTMASTER (CVL AL Z NPROC)
    (PROG (W PBASE FINL FP HDL P PENDING SOL HD)
      ((LAMBDA (*RLQEDFS)
         (SETQ PBASE (OFSF_OPT2 CVL AL NIL NIL Z (TIMES 3 NPROC))))
       NIL)
      (REMOTE_PROCESS NIL)
      (SETQ HDL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NPROC I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (PLUS (PVM_MYTID) I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE NPROC I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (PLUS (PVM_MYTID) I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PVM_INITSEND 1)
      (SETQ FP HDL)
      (SETQ PENDING NIL)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T (LIST "initial pbase size is " (LENGTH PBASE)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (OR PBASE PENDING)) (RETURN NIL)))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND PBASE FP)) (RETURN NIL)))
           (PROGN
            (SETQ HD (CAR FP))
            (SETQ FP (CDR FP))
            (SETQ P (CAR PBASE))
            (SETQ PBASE (CDR PBASE))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2T (LIST "sending task to " (LAND HD 4095)))))
            (SETQ PENDING (CONS (CONS (OFSF_SENDTASK HD P Z) HD) PENDING)))
           (GO WHILELABEL))
         (COND
          (PENDING
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2
               (LIST (LENGTH PBASE) " problems left, waiting for "
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X PENDING)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (LAND (CDR X) 4095))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (LAND (CDR X) 4095)) (CAR X))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     " ... "))))
            (SETQ FINL (REMOTE_WAIT))
            (COND (*RLVERBOSE (IOTO_PRIN2T "ready")))
            (PROG (FIN)
              (SETQ FIN FINL)
             LAB
              (COND ((NULL FIN) (RETURN NIL)))
              ((LAMBDA (FIN)
                 (PROGN
                  (COND
                   ((AND (SETQ W (REMOTE_RECEIVE FIN))
                         (OR (NULL SOL)
                             (MINUSF (CAR (ADDSQ (CAR W) (NEGSQ (CAR SOL)))))))
                    (SETQ SOL W)))
                  (SETQ FP (CONS (CDR (ASSOC FIN PENDING)) FP))
                  (SETQ PENDING (DELASC FIN PENDING))))
               (CAR FIN))
              (SETQ FIN (CDR FIN))
              (GO LAB))))))
        (GO WHILELABEL))
      (RETURN SOL))) 
(PUT 'OFSF_OPT 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_OPT 'DEFINED-ON-LINE '105) 
(PUT 'OFSF_OPT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPT (CL TARG PARML NPROC)
    (PROG (SVRLQEDFS W)
      (COND (*RLQEHEU (PROGN (SETQ SVRLQEDFS *RLQEDFS) (SETQ *RLQEDFS T))))
      (SETQ W (OFSF_OPT0 CL TARG PARML NPROC))
      (COND (*RLQEHEU (SETQ *RLQEDFS SVRLQEDFS)))
      (RETURN W))) 
(PUT 'OFSF_OPT0 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_OPT0 'DEFINED-ON-LINE '117) 
(PUT 'OFSF_OPT0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPT0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPT0 (CL TARG PARML NPROC)
    (PROG (W *RLSIATADV *RLSISO)
      (SETQ W (OFSF_OPT1 CL TARG PARML))
      (COND
       (*RLPARALLEL
        (RETURN
         (OFSF_OPTMKANS
          (OFSF_OPTMASTER (CAR W) (CADR W) (CADDR W) (DIFFERENCE NPROC 1))))))
      (RETURN
       (OFSF_OPTMKANS (OFSF_OPT2 (CAR W) (CADR W) NIL NIL (CADDR W) NIL))))) 
(PUT 'OFSF_OPT1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_OPT1 'DEFINED-ON-LINE '131) 
(PUT 'OFSF_OPT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPT1 (CL TARG PARML)
    (PROG (Z QVL)
      (SETQ Z (INTERN (GENSYM)))
      (SETQ CL
              (CONS
               (LIST 'GEQ
                     (ADDF
                      ((LAMBDA (G251)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR TARG) G251))
                               (T (POLY-MULTF (CDR TARG) G251))))
                       (CAR (SIMP Z)))
                      (NEGF (CAR TARG)))
                     NIL)
               CL))
      (SETQ QVL (SETDIFF (OFSF_VARL CL) (CONS Z PARML)))
      (RETURN (LIST QVL CL Z)))) 
(PUT 'OFSF_VARL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_VARL 'DEFINED-ON-LINE '140) 
(PUT 'OFSF_VARL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_VARL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_VARL (L)
    (PROG (W)
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ W (UNION W (OFSF_VARLAT X)))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN W))) 
(PUT 'OFSF_OPT2 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_OPT2 'DEFINED-ON-LINE '147) 
(PUT 'OFSF_OPT2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPT2 (CVL AL PL AN Z SZ)
    (PROG (W CO ANSL JUNCT BEST M THEO C VLV NODES DPTH)
      (SETQ C 0)
      (SETQ VLV 0)
      (SETQ NODES 0)
      (SETQ DPTH 0)
      (COND ((AND *RLVERBOSE *RLPARALLEL) (IOTO_TPRIN2 "entering opt2 ... ")))
      (COND
       ((AND *RLVERBOSE *RLQEDFS (NOT *RLPARALLEL))
        (PROGN
         (SETQ DPTH (LENGTH CVL))
         (SETQ VLV (QUOTIENT DPTH 4))
         (IOTO_TPRIN2T
          (LIST "+++ Depth is " DPTH ", watching level "
                (DIFFERENCE DPTH VLV))))))
      (SETQ CO (OFSF_SAVE CO (LIST (CONS CVL (CONS AL (CONS PL AN))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT CO) (RETURN NIL)))
        (PROGN
         (SETQ W (OFSF_GET CO))
         (SETQ CO (CDR W))
         (SETQ W (CAR W))
         (SETQ CVL (CAR W))
         (SETQ AL (CADR W))
         (SETQ PL (CADDR W))
         (SETQ AN (CDDDR W))
         (COND
          ((AND *RLVERBOSE (NOT *RLPARALLEL)) (SETQ NODES (PLUS NODES 1))))
         (COND
          ((AND *RLVERBOSE *RLQEDFS (EQN VLV (LENGTH CVL)) (NOT *RLPARALLEL))
           (IOTO_TPRIN2T (LIST "+++ Crossing level " (DIFFERENCE DPTH VLV)))))
         (COND
          ((AND *RLVERBOSE (NULL *RLQEDFS) (NOT *RLPARALLEL))
           (PROGN
            (COND
             ((EQN C 0)
              (PROGN
               (SETQ C (PLUS (OFSF_COLENGTH CO) 1))
               (IOTO_TPRIN2T
                (LIST "+++ " (LENGTH CVL) " variables left for this block")))))
            (IOTO_PRIN2 (LIST "[" C))
            (SETQ C (DIFFERENCE C 1)))))
         (COND
          ((AND *RLVERBOSE *RLQEDFS (NOT *RLPARALLEL))
           (IOTO_PRIN2 (LIST "[" (DIFFERENCE DPTH (LENGTH CVL))))))
         (SETQ JUNCT (OFSF_QEVAR CVL AL PL AN Z THEO))
         (COND ((EQ JUNCT 'BREAK) (SETQ CO NIL))
               ((AND JUNCT (CAR (CAR JUNCT))) (SETQ CO (OFSF_SAVE CO JUNCT)))
               (T
                (PROGN
                 (COND
                  ((AND *RLVERBOSE (NOT *RLPARALLEL) (NULL JUNCT))
                   (IOTO_PRIN2 "#")))
                 (PROG (X)
                   (SETQ X JUNCT)
                  LAB
                   (COND ((NULL X) (RETURN NIL)))
                   ((LAMBDA (X)
                      (PROGN
                       (COND
                        ((SETQ M (OFSF_GETVALUE (CADR X) (CADDR X)))
                         (PROGN
                          (COND
                           ((AND ANSL
                                 (OR
                                  (MINUSF
                                   (SETQ W (CAR (ADDSQ M (NEGSQ BEST)))))
                                  (AND (NULL W) *RLOPT1S)))
                            (SETQ ANSL NIL)))
                          (COND
                           ((OR (NULL ANSL) (NULL W))
                            (PROGN
                             (SETQ BEST M)
                             (SETQ THEO
                                     (LIST
                                      (LIST 'LEQ
                                            (ADDF
                                             ((LAMBDA (G253)
                                                (COND
                                                 (*PHYSOP-LOADED
                                                  (PHYSOP-MULTF (CDR BEST)
                                                   G253))
                                                 (T
                                                  (POLY-MULTF (CDR BEST)
                                                              G253))))
                                              (CAR (SIMP Z)))
                                             (NEGF (CAR BEST)))
                                            NIL)))
                             (SETQ ANSL (CONS (CDDDR X) ANSL))
                             (COND
                              ((AND *RLVERBOSE (NOT *RLPARALLEL))
                               (IOTO_TPRIN2T
                                (LIST "min=" (OR (CAR M) 0) "/"
                                      (CDR M)))))))))))))
                    (CAR X))
                   (SETQ X (CDR X))
                   (GO LAB))
                 (COND
                  ((AND *RLVERBOSE *RLQEDFS (NOT *RLPARALLEL))
                   (IOTO_PRIN2 "."))))))
         (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "] ")))
         (COND
          ((AND SZ (GEQ (OFSF_COLENGTH CO) SZ))
           (PROGN
            (COND
             (ANSL
              (REDERR "ofsf_opt2: found solutions during pbase generation")))
            (SETQ JUNCT 'PBASE)
            (SETQ ANSL (CDR CO))
            (SETQ CO NIL)))))
        (GO WHILELABEL))
      (COND ((EQ JUNCT 'PBASE) (SETQ W ANSL))
            ((EQ JUNCT 'BREAK) (SETQ W JUNCT))
            (T
             (PROGN
              (SETQ W NIL)
              (PROG (X)
                (SETQ X ANSL)
               LAB
                (COND ((NULL X) (RETURN NIL)))
                ((LAMBDA (X) (SETQ W (LTO_INSERT (OFSF_BACKSUB X Z BEST) W)))
                 (CAR X))
                (SETQ X (CDR X))
                (GO LAB))
              (SETQ W (LIST BEST W)))))
      (COND
       ((AND *RLVERBOSE (NOT *RLPARALLEL))
        (IOTO_TPRIN2T (LIST "+++ " NODES " nodes computed"))))
      (COND ((AND *RLVERBOSE *RLPARALLEL) (IOTO_PRIN2T "exiting opt2")))
      (RETURN W))) 
(PUT 'OFSF_QEVAR 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QEVAR 'DEFINED-ON-LINE '229) 
(PUT 'OFSF_QEVAR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_QEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEVAR (CVL AL PL AN Z THEO)
    (PROG (W V ESET)
      (COND ((SETQ W (OFSF_OPTGAUSS CVL AL PL AN THEO)) (RETURN (CDR W))))
      (SETQ W (OFSF_OPTESET CVL AL PL Z))
      (SETQ V (CAR W))
      (SETQ ESET (CDR W))
      (COND
       (ESET
        (PROGN
         (COND
          ((EQ V Z)
           (PROGN
            (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "z")))
            (RETURN (OFSF_ZESETSUBST CVL AL PL AN ESET THEO)))))
         (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "e")))
         (RETURN (OFSF_ESETSUBST CVL AL PL AN V ESET THEO)))))
      (COND (*RLVERBOSE (IOTO_PRIN2 "*")))
      (COND
       ((MEMQ V (OFSF_VARL PL))
        (PROGN
         (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "!")))
         (RETURN NIL))))
      (RETURN (LIST (CONS (LTO_DELQ V CVL) (CONS AL (CONS PL AN))))))) 
(PUT 'OFSF_OPTGAUSS 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_OPTGAUSS 'DEFINED-ON-LINE '254) 
(PUT 'OFSF_OPTGAUSS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTGAUSS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTGAUSS (CVL AL PL AN THEO)
    (PROG (V W SC)
      (SETQ SC CVL)
      (PROG ()
       WHILELABEL
        (COND ((NOT SC) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR SC))
         (SETQ SC (CDR SC))
         (COND ((SETQ W (OFSF_OPTFINDEQSOL AL V)) (SETQ SC NIL))))
        (GO WHILELABEL))
      (COND
       (W
        (PROGN
         (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "g")))
         (RETURN (CONS T (OFSF_ESETSUBST CVL AL PL AN V (LIST W) THEO)))))))) 
(PUT 'OFSF_OPTFINDEQSOL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_OPTFINDEQSOL 'DEFINED-ON-LINE '268) 
(PUT 'OFSF_OPTFINDEQSOL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTFINDEQSOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTFINDEQSOL (AL V)
    (PROG (A W)
      (SETQ A (CAR AL))
      (COND
       ((AND (EQ (CAR A) 'EQUAL) (MEMQ V (OFSF_VARLAT A)))
        (PROGN
         (SETQ W (OFSF_OPTMKSOL (CADR A) V))
         (RETURN
          (CONS A (MULTSQ (CONS (CAR W) 1) (INVSQ (CONS (CDR W) 1))))))))
      (COND ((CDR AL) (RETURN (OFSF_OPTFINDEQSOL (CDR AL) V)))))) 
(PUT 'OFSF_ESETSUBST 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_ESETSUBST 'DEFINED-ON-LINE '283) 
(PUT 'OFSF_ESETSUBST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_ESETSUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_ESETSUBST (CVL AL PL AN V ESET THEO)
    (PROG (W SCPL ZONLY NAL NPL JUNCT X)
      (SETQ CVL (LTO_DELQ V CVL))
      (PROG ()
       WHILELABEL
        (COND ((NOT ESET) (RETURN NIL)))
        (PROGN
         (SETQ X (CAR ESET))
         (SETQ ESET (CDR ESET))
         (COND
          ((MEMQ (CDR X) '(PINF MINF))
           (PROGN
            (SETQ NAL
                    (OFSF_SIMPL
                     (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                       (SETQ ATF AL)
                       (COND ((NULL ATF) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ATF)
                                           (OFSF_QESUBIAT ATF V (CDR X)))
                                         (CAR ATF))
                                        NIL)))
                      LOOPLABEL
                       (SETQ ATF (CDR ATF))
                       (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ATF) (OFSF_QESUBIAT ATF V (CDR X)))
                                 (CAR ATF))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     THEO))
            (SETQ NPL
                    (OFSF_SIMPL
                     (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                       (SETQ ATF PL)
                       (COND ((NULL ATF) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ATF)
                                           (OFSF_QESUBIAT ATF V (CDR X)))
                                         (CAR ATF))
                                        NIL)))
                      LOOPLABEL
                       (SETQ ATF (CDR ATF))
                       (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ATF) (OFSF_QESUBIAT ATF V (CDR X)))
                                 (CAR ATF))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     THEO))))
          (T
           (PROGN
            (SETQ NAL
                    (OFSF_SIMPL
                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                       (SETQ Y AL)
                       (COND ((NULL Y) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (Y)
                                           (OFSF_OPTSUBSTAT Y (CDR X) V))
                                         (CAR Y))
                                        NIL)))
                      LOOPLABEL
                       (SETQ Y (CDR Y))
                       (COND ((NULL Y) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (Y) (OFSF_OPTSUBSTAT Y (CDR X) V))
                                 (CAR Y))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     THEO))
            (SETQ NPL
                    (OFSF_SIMPL
                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                       (SETQ Y PL)
                       (COND ((NULL Y) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (Y)
                                           (OFSF_OPTSUBSTAT Y (CDR X) V))
                                         (CAR Y))
                                        NIL)))
                      LOOPLABEL
                       (SETQ Y (CDR Y))
                       (COND ((NULL Y) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (Y) (OFSF_OPTSUBSTAT Y (CDR X) V))
                                 (CAR Y))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     THEO))
            (SETQ AL (LTO_DELQ (CAR X) AL))
            (SETQ PL (CONS (CAR X) PL)))))
         (COND
          ((AND (NULL NAL) (NULL NPL))
           (PROGN (SETQ JUNCT 'BREAK) (SETQ ESET NIL)))
          ((NULL NAL)
           (PROGN
            (SETQ ZONLY T)
            (SETQ SCPL PL)
            (PROG ()
             WHILELABEL
              (COND ((NOT SCPL) (RETURN NIL)))
              (PROGN
               (SETQ W (OFSF_VARLAT (CAR SCPL)))
               (SETQ SCPL (CDR SCPL))
               (COND ((NEQ W '(Z)) (SETQ SCPL (SETQ ZONLY NIL)))))
              (GO WHILELABEL))
            (COND (ZONLY (REDERR "BUG IN OFSF_ESETSUBST")))
            (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "!")))))
          ((AND (NEQ NAL 'FALSE) (NEQ NPL 'FALSE))
           (SETQ JUNCT
                   (CONS
                    (CONS CVL (CONS NAL (CONS NPL (CONS (CONS V (CDR X)) AN))))
                    JUNCT)))))
        (GO WHILELABEL))
      (RETURN JUNCT))) 
(PUT 'OFSF_ZESETSUBST 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_ZESETSUBST 'DEFINED-ON-LINE '326) 
(PUT 'OFSF_ZESETSUBST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_ZESETSUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_ZESETSUBST (CVL AL PL AN ZESET THEO)
    (PROG (W SCPL ZONLY NAL NPL JUNCT X V)
      (PROG ()
       WHILELABEL
        (COND ((NOT ZESET) (RETURN NIL)))
        (PROGN
         (SETQ X (CAR ZESET))
         (SETQ ZESET (CDR ZESET))
         (SETQ V (CADR X))
         (SETQ NAL
                 (OFSF_SIMPL
                  (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                    (SETQ Y AL)
                    (COND ((NULL Y) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (Y)
                                        (OFSF_OPTSUBSTAT Y (CDDR X) V))
                                      (CAR Y))
                                     NIL)))
                   LOOPLABEL
                    (SETQ Y (CDR Y))
                    (COND ((NULL Y) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (Y) (OFSF_OPTSUBSTAT Y (CDDR X) V))
                              (CAR Y))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  THEO))
         (SETQ NPL
                 (OFSF_SIMPL
                  (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                    (SETQ Y PL)
                    (COND ((NULL Y) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (Y)
                                        (OFSF_OPTSUBSTAT Y (CDDR X) V))
                                      (CAR Y))
                                     NIL)))
                   LOOPLABEL
                    (SETQ Y (CDR Y))
                    (COND ((NULL Y) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (Y) (OFSF_OPTSUBSTAT Y (CDDR X) V))
                              (CAR Y))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  THEO))
         (SETQ AL (LTO_DELQ (CAR X) AL))
         (SETQ PL (CONS (CAR X) PL))
         (COND
          ((AND (NULL NAL) (NULL NPL))
           (PROGN (SETQ JUNCT 'BREAK) (SETQ ZESET NIL)))
          ((NULL NAL)
           (PROGN
            (SETQ ZONLY T)
            (SETQ SCPL PL)
            (PROG ()
             WHILELABEL
              (COND ((NOT SCPL) (RETURN NIL)))
              (PROGN
               (SETQ W (OFSF_VARLAT (CAR SCPL)))
               (SETQ SCPL (CDR SCPL))
               (COND ((NEQ W '(Z)) (SETQ SCPL (SETQ ZONLY NIL)))))
              (GO WHILELABEL))
            (COND (ZONLY (REDERR "BUG IN OFSF_ZESETSUBST")))
            (COND ((AND *RLVERBOSE (NOT *RLPARALLEL)) (IOTO_PRIN2 "!")))))
          ((AND (NEQ NAL 'FALSE) (NEQ NPL 'FALSE))
           (SETQ JUNCT
                   (CONS
                    (CONS (LTO_DELQ V CVL)
                          (CONS NAL (CONS NPL (CONS (CDR X) AN))))
                    JUNCT)))))
        (GO WHILELABEL))
      (RETURN JUNCT))) 
(PUT 'OFSF_OPTSUBSTAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_OPTSUBSTAT 'DEFINED-ON-LINE '357) 
(PUT 'OFSF_OPTSUBSTAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTSUBSTAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTSUBSTAT (ATF SQ V)
    (PROG (W)
      (COND ((NULL (SETQ W (OFSF_OPTSPLITTERM (CADR ATF) V))) (RETURN ATF)))
      (RETURN
       (LIST (CAR ATF)
             (ADDF
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR W) (CAR SQ)))
                    (T (POLY-MULTF (CAR W) (CAR SQ))))
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR W) (CDR SQ)))
                    (T (POLY-MULTF (CDR W) (CDR SQ)))))
             NIL)))) 
(PUT 'OFSF_OPTSPLITTERM 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_OPTSPLITTERM 'DEFINED-ON-LINE '366) 
(PUT 'OFSF_OPTSPLITTERM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTSPLITTERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTSPLITTERM (U V)
    (PROG (W)
      (SETQ U (SFTO_REORDER U V))
      (COND ((EQUAL (SETQ W (DEGR U V)) 0) (RETURN NIL)))
      (COND
       ((GREATERP W 1)
        (REDERR (LIST "ofsf_optsplitterm:" V "has degree" W "in" U))))
      (RETURN (CONS (REORDER (CDAR U)) (REORDER (CDR U)))))) 
(PUT 'OFSF_SIMPL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPL 'DEFINED-ON-LINE '377) 
(PUT 'OFSF_SIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_SIMPL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPL (L THEO)
    (PROG (W OP *RLSIEXPLA)
      (SETQ W
              (CL_SIMPL
               (COND ((AND L (CDR L)) (CONS 'AND L))
                     ((NULL L) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR L)))
               THEO (MINUS 1)))
      (COND ((EQ W 'FALSE) (RETURN 'FALSE)))
      (COND ((EQ W 'TRUE) (RETURN NIL)))
      (SETQ OP (COND ((ATOM W) W) (T (CAR W))))
      (COND ((EQ OP 'AND) (RETURN (CDR W))))
      (COND
       ((OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
            (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
            (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
        (REDERR (LIST "BUG IN OFSF_SIMPL" OP))))
      (RETURN (LIST W)))) 
(SWITCH (LIST 'BOESE 'USEOLD 'USEZ)) 
(ON1 'USEOLD) 
(ON1 'USEZ) 
(PUT 'OFSF_OPTESET 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_OPTESET 'DEFINED-ON-LINE '394) 
(PUT 'OFSF_OPTESET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTESET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTESET (CVL AL PL Z)
    (PROG (W V SEL LBL UBL ESET UB LB BEST)
      (SETQ UB 0)
      (SETQ LB 0)
      (SETQ BEST 0)
      (COND
       ((NOT (OR *USEZ *USEOLD)) (REDERR "select usez or useold as method")))
      (COND
       (*USEZ
        (PROGN
         (PROG (X)
           (SETQ X AL)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (COND
               ((SETQ W (OFSF_ZBOUNDCHK X Z))
                (PROGN
                 (SETQ BEST (PLUS BEST 1))
                 (SETQ ESET (CONS (CONS X W) ESET))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (COND (ESET (SETQ SEL Z))))))
      (COND
       ((OR *USEOLD (NULL SEL))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT CVL) (RETURN NIL)))
           (PROGN
            (SETQ V (CAR CVL))
            (SETQ CVL (CDR CVL))
            (SETQ LB (SETQ UB 0))
            (SETQ LBL (SETQ UBL NIL))
            (PROG (X)
              (SETQ X AL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (COND
                  ((SETQ W (OFSF_BOUNDCHK X V))
                   (COND
                    ((EQ (CAR W) 'LB)
                     (PROGN
                      (SETQ LB (PLUS LB 1))
                      (SETQ LBL (CONS (CONS X (CDR W)) LBL))))
                    ((EQ (CAR W) 'UB)
                     (PROGN
                      (SETQ UB (PLUS UB 1))
                      (SETQ UBL (CONS (CONS X (CDR W)) UBL))))
                    (T (REDERR "BUG 2 IN ofsf_opteset"))))))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (COND ((AND (NULL LBL) UBL) (SETQ LBL '((NIL . MINF)))))
            (COND ((AND (NULL UBL) LBL) (SETQ UBL '((NIL . PINF)))))
            (COND ((LEQ UB LB) (PROGN (SETQ LB UB) (SETQ LBL UBL))))
            (COND
             ((OR (NULL SEL) (LESSP LB BEST)
                  (AND (NULL *BOESE) (EQUAL LB BEST)))
              (PROGN (SETQ BEST LB) (SETQ ESET LBL) (SETQ SEL V))))
            (COND ((AND (NULL LBL) (NULL UBL)) (SETQ CVL NIL))))
           (GO WHILELABEL)))))
      (RETURN (CONS SEL ESET)))) 
(PUT 'OFSF_BOUNDCHK 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BOUNDCHK 'DEFINED-ON-LINE '442) 
(PUT 'OFSF_BOUNDCHK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_BOUNDCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BOUNDCHK (ATF V)
    (PROG (U OLDORDER OP SOL)
      (SETQ OLDORDER (SETKORDER (LIST V)))
      (SETQ U (REORDER (CADR ATF)))
      (SETKORDER OLDORDER)
      (COND ((OR (OR (ATOM U) (ATOM (CAR U))) (NEQ (CAAAR U) V)) (RETURN NIL)))
      (COND
       ((NEQ (CDAAR U) 1) (REDERR (LIST "ofsf_boundchk:" V "not linear"))))
      (SETQ SOL
              (MULTSQ (CONS (NEGF (REORDER (CDR U))) 1)
                      (INVSQ (CONS (REORDER (CDAR U)) 1))))
      (SETQ OP (CAR ATF))
      (COND ((EQ OP 'EQUAL) (RETURN (CONS 'EQUAL SOL))))
      (COND
       ((OFSF_XOR (EQ OP 'GEQ) (MINUSF (CDAR U))) (RETURN (CONS 'LB SOL))))
      (RETURN (CONS 'UB SOL)))) 
(PUT 'OFSF_ZBOUNDCHK 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ZBOUNDCHK 'DEFINED-ON-LINE '456) 
(PUT 'OFSF_ZBOUNDCHK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_ZBOUNDCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ZBOUNDCHK (ATF Z)
    (PROG (U V OLDORDER OP)
      (SETQ OP (CAR ATF))
      (SETQ U (CADR ATF))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN NIL)))
      (SETQ OLDORDER (SETKORDER (LIST Z)))
      (SETQ U (REORDER U))
      (SETKORDER OLDORDER)
      (COND
       ((NEQ (CDAAR U) 1) (REDERR (LIST "ofsf_zboundchk:" Z "not linear"))))
      (COND
       ((OR (NEQ (CAAAR U) Z) (OR (ATOM (CDR U)) (ATOM (CAR (CDR U)))))
        (RETURN NIL)))
      (COND
       ((NOT (OR (EQ OP 'EQUAL) (OFSF_XOR (EQ OP 'GEQ) (MINUSF (CDAR U)))))
        (RETURN NIL)))
      (SETQ V (CAAAR (CDR U)))
      (SETQ OLDORDER (SETKORDER (LIST V)))
      (SETQ U (REORDER U))
      (SETKORDER OLDORDER)
      (RETURN
       (CONS V
             (MULTSQ (CONS (NEGF (REORDER (CDR U))) 1)
                     (INVSQ (CONS (REORDER (CDAR U)) 1))))))) 
(PUT 'OFSF_XOR 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_XOR 'DEFINED-ON-LINE '478) 
(PUT 'OFSF_XOR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_XOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_XOR (A B) (AND (OR A B) (NOT (AND A B)))) 
(PUT 'OFSF_GETVALUE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETVALUE 'DEFINED-ON-LINE '481) 
(PUT 'OFSF_GETVALUE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_GETVALUE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETVALUE (AL PL)
    (PROG (ATF W)
      (SETQ ATF (OFSF_SIMPL (APPEND AL PL) NIL))
      (COND ((EQ ATF 'FALSE) (RETURN NIL)))
      (COND
       ((CDR ATF)
        (PROGN
         (COND ((CDDR ATF) (REDERR (LIST "BUG 1 IN OFSF_GETVALUE" ATF))))
         (SETQ W (CADR ATF))
         (COND
          ((OFSF_OPTLBP W)
           (PROGN
            (SETQ W (CAR ATF))
            (COND
             ((OFSF_OPTLBP W) (REDERR (LIST "BUG 2 IN OFSF_GETVALUE" ATF))))
            (SETQ ATF (CDR ATF))))))))
      (SETQ ATF (CAR ATF))
      (COND
       ((NOT (OFSF_OPTLBP ATF)) (REDERR (LIST "BUG 3 IN OFSF_GETVALUE" ATF))))
      (SETQ W (CADR ATF))
      (RETURN (MULTSQ (CONS (NEGF (CDR W)) 1) (INVSQ (CONS (CDAR W) 1)))))) 
(PUT 'OFSF_OPTLBP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_OPTLBP 'DEFINED-ON-LINE '502) 
(PUT 'OFSF_OPTLBP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTLBP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_OPTLBP (ATF) (MEMQ (CAR ATF) '(EQUAL GEQ))) 
(PUT 'OFSF_BACKSUB 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_BACKSUB 'DEFINED-ON-LINE '505) 
(PUT 'OFSF_BACKSUB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_BACKSUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BACKSUB (AN Z MIN)
    (SORT (OFSF_BACKSUB1 AN Z MIN)
          (FUNCTION (LAMBDA (X Y) (ORDP (CAR X) (CAR Y)))))) 
(PUT 'OFSF_BACKSUB1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_BACKSUB1 'DEFINED-ON-LINE '508) 
(PUT 'OFSF_BACKSUB1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_BACKSUB1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BACKSUB1 (AN Z MIN)
    (OFSF_BACKSUB2
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X AN)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (CONS (CAR X)
                                 (OFSF_OPTSUBSQ (CDR X) (LIST (CONS Z MIN)))))
                         (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (X)
                   (CONS (CAR X) (OFSF_OPTSUBSQ (CDR X) (LIST (CONS Z MIN)))))
                 (CAR X))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'OFSF_BACKSUB2 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_BACKSUB2 'DEFINED-ON-LINE '512) 
(PUT 'OFSF_BACKSUB2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_BACKSUB2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_BACKSUB2 (AN)
    (COND
     (AN
      (CONS (CAR AN)
            (OFSF_BACKSUB2
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (CDR AN))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (CONS (CAR X)
                                         (OFSF_OPTSUBSQ (CDR X)
                                          (LIST (CONS (CAAR AN) (CDAR AN))))))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (CONS (CAR X)
                                 (OFSF_OPTSUBSQ (CDR X)
                                  (LIST (CONS (CAAR AN) (CDAR AN))))))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))) 
(PUT 'OFSF_OPTSUBSQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_OPTSUBSQ 'DEFINED-ON-LINE '516) 
(PUT 'OFSF_OPTSUBSQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTSUBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTSUBSQ (SQ AL)
    (COND ((OR (MEMQ (CDAR AL) '(MINF PINF)) (MEMQ SQ '(MINF PINF))) SQ)
          (T (SUBSQ SQ (LIST (CONS (CAAR AL) (PREPSQ (CDAR AL)))))))) 
(PUT 'OFSF_OPTMKANS 'NUMBER-OF-ARGS 1) 
(DE OFSF_OPTMKANS (ANS)
    (PROG (W)
      (COND ((EQUAL ANS '(NIL NIL)) (RETURN 'INFEASIBLE)))
      (COND ((EQ ANS 'BREAK) (RETURN (LIST (SIMP '(MINUS INFINITY)) NIL))))
      (RETURN
       (CONS (MK*SQ (CAR ANS))
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (CADR ANS))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ Y X)
                                     (COND ((NULL Y) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (Y)
                                                         (PROGN
                                                          (SETQ W
                                                                  (ATSOC
                                                                   (CDR Y)
                                                                   '((MINF
                                                                      MINUS
                                                                      INFINITY)
                                                                     (PINF
                                                                      . INFINITY))))
                                                          (SETQ W
                                                                  (COND
                                                                   (W (CDR W))
                                                                   (T
                                                                    (MK*SQ
                                                                     (CDR
                                                                      Y)))))
                                                          (CONS (CAR Y) W)))
                                                       (CAR Y))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ Y (CDR Y))
                                     (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (PROGN
                                                  (SETQ W
                                                          (ATSOC (CDR Y)
                                                                 '((MINF MINUS
                                                                    INFINITY)
                                                                   (PINF
                                                                    . INFINITY))))
                                                  (SETQ W
                                                          (COND (W (CDR W))
                                                                (T
                                                                 (MK*SQ
                                                                  (CDR Y)))))
                                                  (CONS (CAR Y) W)))
                                               (CAR Y))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Y X)
                             (COND ((NULL Y) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (PROGN
                                                  (SETQ W
                                                          (ATSOC (CDR Y)
                                                                 '((MINF MINUS
                                                                    INFINITY)
                                                                   (PINF
                                                                    . INFINITY))))
                                                  (SETQ W
                                                          (COND (W (CDR W))
                                                                (T
                                                                 (MK*SQ
                                                                  (CDR Y)))))
                                                  (CONS (CAR Y) W)))
                                               (CAR Y))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Y (CDR Y))
                             (COND ((NULL Y) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Y)
                                         (PROGN
                                          (SETQ W
                                                  (ATSOC (CDR Y)
                                                         '((MINF MINUS
                                                            INFINITY)
                                                           (PINF . INFINITY))))
                                          (SETQ W
                                                  (COND (W (CDR W))
                                                        (T (MK*SQ (CDR Y)))))
                                          (CONS (CAR Y) W)))
                                       (CAR Y))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'OFSF_OPTMKSOL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_OPTMKSOL 'DEFINED-ON-LINE '532) 
(PUT 'OFSF_OPTMKSOL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_OPTMKSOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_OPTMKSOL (U V)
    (PROG (W)
      (SETQ W (SETKORDER (LIST V)))
      (SETQ U (REORDER U))
      (SETKORDER W)
      (COND ((NEQ (DEGR U V) 1) (REDERR (LIST "ofsf_mksol:" V "not linear"))))
      (RETURN (CONS (NEGF (REORDER (CDR U))) (REORDER (CDAR U)))))) 
(PUT 'OFSF_SAVE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SAVE 'DEFINED-ON-LINE '544) 
(PUT 'OFSF_SAVE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_SAVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SAVE (CO DOL)
    (COND (*RLQEDFS (OFSF_PUSH CO DOL)) (T (OFSF_ENQUEUE CO DOL)))) 
(PUT 'OFSF_PUSH 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PUSH 'DEFINED-ON-LINE '550) 
(PUT 'OFSF_PUSH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_PUSH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PUSH (CO DOL)
    (PROGN
     (PROG (X)
       (SETQ X DOL)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (SETQ CO (OFSF_COINSERT CO X))) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     CO)) 
(PUT 'OFSF_COINSERT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_COINSERT 'DEFINED-ON-LINE '559) 
(PUT 'OFSF_COINSERT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_COINSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_COINSERT (CO CE) (COND ((OFSF_COMEMBER CE CO) CO) (T (CONS CE CO)))) 
(PUT 'OFSF_ENQUEUE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ENQUEUE 'DEFINED-ON-LINE '564) 
(PUT 'OFSF_ENQUEUE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_ENQUEUE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ENQUEUE (CO DOL)
    (PROGN
     (COND
      ((AND (NULL CO) DOL)
       (PROGN
        (SETQ CO (LIST NIL (CAR DOL)))
        (SETCAR CO (CDR CO))
        (SETQ DOL (CDR DOL)))))
     (PROG (X)
       (SETQ X DOL)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (COND
           ((NOT (OFSF_COMEMBER X (CDR CO)))
            (SETCAR CO (SETCDR (CAR CO) (LIST X))))))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     CO)) 
(PUT 'OFSF_GET 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GET 'DEFINED-ON-LINE '580) 
(PUT 'OFSF_GET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_GET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GET (CO) (COND (*RLQEDFS (OFSF_POP CO)) (T (OFSF_DEQUEUE CO)))) 
(PUT 'OFSF_POP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_POP 'DEFINED-ON-LINE '586) 
(PUT 'OFSF_POP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_POP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_POP (CO) CO) 
(PUT 'OFSF_DEQUEUE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_DEQUEUE 'DEFINED-ON-LINE '592) 
(PUT 'OFSF_DEQUEUE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_DEQUEUE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_DEQUEUE (CO)
    (COND (CO (CONS (CADR CO) (COND ((CDDR CO) (CONS (CAR CO) (CDDR CO)))))))) 
(PUT 'OFSF_COLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_COLENGTH 'DEFINED-ON-LINE '598) 
(PUT 'OFSF_COLENGTH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_COLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_COLENGTH (CO)
    (COND ((OR *RLQEDFS (NULL CO)) (LENGTH CO)) (T (DIFFERENCE (LENGTH CO) 1)))) 
(PUT 'OFSF_COMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_COMEMBER 'DEFINED-ON-LINE '603) 
(PUT 'OFSF_COMEMBER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFOPT.RED) 
(PUT 'OFSF_COMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_COMEMBER (CE L)
    (PROG (A)
      (COND ((NULL L) (RETURN NIL)))
      (SETQ A (CAR L))
      (COND
       ((AND (EQUAL (CADR CE) (CADR A)) (EQUAL (CADDR CE) (CADDR A))
             (EQUAL (CAR CE) (CAR A)))
        (RETURN L)))
      (RETURN (OFSF_COMEMBER CE (CDR L))))) 
(ENDMODULE) 
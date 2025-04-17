(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFLP)) 
(REVISION 'OFSFLP "$Id: ofsflp.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFLP "(c) 2013-2014 T. Sturm") 
(SWITCH (LIST 'LPKEEPFILES)) 
(SWITCH (LIST 'RLFFI)) 
(FLUID '(*ECHO)) 
(FLUID '(*RLGUROBI)) 
(FLUID '(*RLFFI)) 
(FLUID '(LP_MODEL*)) 
(FLUID '(LP_MODELCACHE*)) 
(FLUID '(LP_VARL*)) 
(FLUID '(LP_DIM*)) 
(FLUID '(LP_RDIM*)) 
(FLUID '(LP_ZDIM*)) 
(GLOBAL '(*LP_CSLP)) 
(SETQ *LP_CSLP (MEMQ 'CSL LISPSYSTEM*)) 
(PUT 'LP_NEWMODEL 'NUMBER-OF-ARGS 2) 
(PUT 'LP_NEWMODEL 'DEFINED-ON-LINE '51) 
(PUT 'LP_NEWMODEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_NEWMODEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LP_NEWMODEL (N M)
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_NEWMODEL N M))
          (T
           (PROGN
            (SETQ LP_MODEL* (SETQ LP_MODELCACHE* NIL))
            (SETQ LP_VARL*
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 0)
                      (COND
                       ((MINUSP (DIFFERENCE (PLUS N (DIFFERENCE M 1)) I))
                        (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (MKID 'C I) NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE (PLUS N (DIFFERENCE M 1)) I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (MKID 'C I) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ LP_DIM* (PLUS N M))
            (SETQ LP_RDIM* N)
            (SETQ LP_ZDIM* M))))) 
(PUT 'LP_ADDCONSTRAINT 'NUMBER-OF-ARGS 3) 
(PUT 'LP_ADDCONSTRAINT 'DEFINED-ON-LINE '62) 
(PUT 'LP_ADDCONSTRAINT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_ADDCONSTRAINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LP_ADDCONSTRAINT (REL L R)
    (PROG (LHS VL)
      (COND
       ((AND *RLGUROBI *RLFFI *LP_CSLP)
        (PROGN (GUROBI_ADDCONSTRAINTFAST REL L R) (RETURN NIL))))
      (SETQ VL LP_VARL*)
      (SETQ LHS
              (CONS 'PLUS
                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                      (SETQ C L)
                      (COND ((NULL C) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (C)
                                          (LIST 'TIMES C
                                                (PROG1 (CAR VL)
                                                  (SETQ VL (CDR VL)))))
                                        (CAR C))
                                       NIL)))
                     LOOPLABEL
                      (SETQ C (CDR C))
                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (C)
                                  (LIST 'TIMES C
                                        (PROG1 (CAR VL) (SETQ VL (CDR VL)))))
                                (CAR C))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ LP_MODELCACHE* (CONS (LIST REL LHS R) LP_MODELCACHE*)))) 
(PUT 'LP_UPDATEMODEL 'NUMBER-OF-ARGS 0) 
(PUT 'LP_UPDATEMODEL 'DEFINED-ON-LINE '73) 
(PUT 'LP_UPDATEMODEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_UPDATEMODEL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_UPDATEMODEL NIL
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_UPDATEMODEL))
          (T
           (PROGN
            (SETQ LP_MODEL* (NCONC LP_MODEL* (REVERSIP LP_MODELCACHE*)))
            (SETQ LP_MODELCACHE* NIL))))) 
(PUT 'LP_NEGCONSTR1 'NUMBER-OF-ARGS 0) 
(PUT 'LP_NEGCONSTR1 'DEFINED-ON-LINE '81) 
(PUT 'LP_NEGCONSTR1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_NEGCONSTR1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_NEGCONSTR1 NIL (LP_NEGCONSTR 0)) 
(PUT 'LP_NEGCONSTR 'NUMBER-OF-ARGS 1) 
(PUT 'LP_NEGCONSTR 'DEFINED-ON-LINE '84) 
(PUT 'LP_NEGCONSTR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_NEGCONSTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LP_NEGCONSTR (N)
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_NEGCONSTR N))
          (T
           (PROGN
            (LP_UPDATEMODEL)
            (LTO_APPLY2NTHIP LP_MODEL* (PLUS N 1) (FUNCTION LP_DONEGCONSTR)
                             NIL))))) 
(PUT 'LP_DONEGCONSTR 'NUMBER-OF-ARGS 1) 
(PUT 'LP_DONEGCONSTR 'DEFINED-ON-LINE '92) 
(PUT 'LP_DONEGCONSTR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_DONEGCONSTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LP_DONEGCONSTR (C)
    (LIST (CAR C)
          (CONS 'PLUS
                (PROG (S FORALL-RESULT FORALL-ENDPTR)
                  (SETQ S (CDR (CADR C)))
                  (COND ((NULL S) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (S)
                                      (LIST 'TIMES (MINUS (CADR S)) (CADDR S)))
                                    (CAR S))
                                   NIL)))
                 LOOPLABEL
                  (SETQ S (CDR S))
                  (COND ((NULL S) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (S)
                              (LIST 'TIMES (MINUS (CADR S)) (CADDR S)))
                            (CAR S))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
          (CADDR C))) 
(PUT 'LP_DELCONSTR1 'NUMBER-OF-ARGS 0) 
(PUT 'LP_DELCONSTR1 'DEFINED-ON-LINE '95) 
(PUT 'LP_DELCONSTR1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_DELCONSTR1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_DELCONSTR1 NIL (LP_DELCONSTR 0)) 
(PUT 'LP_DELCONSTR 'NUMBER-OF-ARGS 1) 
(PUT 'LP_DELCONSTR 'DEFINED-ON-LINE '98) 
(PUT 'LP_DELCONSTR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_DELCONSTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LP_DELCONSTR (N)
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_DELCONSTR N))
          (T (PROGN (LP_UPDATEMODEL) (LTO_DELNTHIP LP_MODEL* (PLUS N 1)))))) 
(PUT 'LP_FREEMODEL 'NUMBER-OF-ARGS 0) 
(PUT 'LP_FREEMODEL 'DEFINED-ON-LINE '106) 
(PUT 'LP_FREEMODEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_FREEMODEL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_FREEMODEL NIL
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_FREEMODEL))
          (T (SETQ LP_MODEL* (SETQ LP_MODELCACHE* (SETQ LP_VARL* NIL)))))) 
(PUT 'LP_OPTIMIZE 'NUMBER-OF-ARGS 0) 
(PUT 'LP_OPTIMIZE 'DEFINED-ON-LINE '112) 
(PUT 'LP_OPTIMIZE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_OPTIMIZE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_OPTIMIZE NIL
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_OPTIMIZE))
          ((AND *RLGUROBI (NOT (AND *RLFFI *LP_CSLP))) (LP_RUNGUROBI))
          (T (LP_RUNLINALG)))) 
(PUT 'LP_OPTACTION 'NUMBER-OF-ARGS 0) 
(PUT 'LP_OPTACTION 'DEFINED-ON-LINE '120) 
(PUT 'LP_OPTACTION 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_OPTACTION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_OPTACTION NIL
    (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) "using Gurobi via FFI")
          ((AND *RLGUROBI (NOT (AND *RLFFI *LP_CSLP)))
           "using Gurobi via file interface")
          (T "using Reduce simplex"))) 
(PUT 'LP_RUNGUROBI 'NUMBER-OF-ARGS 0) 
(PUT 'LP_RUNGUROBI 'DEFINED-ON-LINE '128) 
(PUT 'LP_RUNGUROBI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_RUNGUROBI 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_RUNGUROBI NIL
    (PROGN
     (LP_UPDATEMODEL)
     (LP_RUNGUROBI1 LP_MODEL* LP_VARL* LP_DIM* LP_RDIM*))) 
(PUT 'LP_RUNGUROBI1 'NUMBER-OF-ARGS 4) 
(DE LP_RUNGUROBI1 (CL VL D RD)
    (PROG (BFN LP SOL LOG CALL RES)
      (SETQ BFN
              (LTO_SCONCAT
               (LIST "/tmp/reduce-lp-" (LTO_AT2STR (GETPID)) "-"
                     (GETENV "USER") "-" (LTO_AT2STR (RANDOM (EXPT 2 16))))))
      (SETQ LP (LTO_SCONCAT (LIST BFN ".lp")))
      (SETQ SOL (LTO_SCONCAT (LIST BFN ".sol")))
      (SETQ LOG (LTO_SCONCAT (LIST BFN ".log")))
      (LP_WRITELP LP (LIST 'TIMES 0 (CAR VL)) CL VL RD)
      (SETQ CALL
              (LTO_SCONCAT
               (LIST "gurobi_cl ResultFile=" SOL " " LP " > " LOG)))
      (SYSTEM CALL)
      (SETQ RES (LP_READSOL SOL VL D))
      (COND
       ((NOT *LPKEEPFILES)
        (PROG (FN)
          (SETQ FN (LIST LP SOL LOG))
         LAB
          (COND ((NULL FN) (RETURN NIL)))
          ((LAMBDA (FN) (SYSTEM (LTO_SCONCAT (LIST "rm -f " FN)))) (CAR FN))
          (SETQ FN (CDR FN))
          (GO LAB))))
      (RETURN RES))) 
(PUT 'LP_WRITELP 'NUMBER-OF-ARGS 5) 
(DE LP_WRITELP (FN OBJ CL VL RD)
    (PROG (OLDPRTCH W OLDSEMIC OLDECHO OLDUTF8 OLDFANCY)
      (SETQ OLDPRTCH (GET 'TIMES 'PRTCH))
      (PUT 'TIMES 'PRTCH '| |)
      (SETQ OLDSEMIC SEMIC*)
      (SETQ OLDECHO *ECHO)
      (SETQ OLDUTF8 *UTF8)
      (SETQ OLDFANCY *FANCY)
      (SETQ *ECHO NIL)
      (SETQ *UTF8 NIL)
      (SETQ *FANCY NIL)
      (COND (FN (OUT (LIST FN))))
      (SETQ W
              (ERRORSET
               (LIST 'LP_WRITELP1 (MKQUOTE OBJ) (MKQUOTE CL) (MKQUOTE VL)
                     (MKQUOTE RD))
               NIL *BACKTRACE))
      (COND (FN (SHUT (LIST FN))))
      (SETQ *FANCY OLDFANCY)
      (SETQ *UTF8 OLDUTF8)
      (SETQ *ECHO OLDECHO)
      (SETQ SEMIC* OLDSEMIC)
      (PUT 'TIMES 'PRTCH OLDPRTCH)
      (COND ((ERRORP W) (REDERR EMSG*))))) 
(PUT 'LP_WRITELP1 'NUMBER-OF-ARGS 4) 
(DE LP_WRITELP1 (OBJ CL VL RD)
    (PROGN
     (PRIN2* "Minimize")
     (TERPRI* NIL)
     (MAPRIN OBJ)
     (TERPRI* NIL)
     (PRIN2* "Subject To")
     (TERPRI* NIL)
     (PROG (C)
       (SETQ C CL)
      LAB
       (COND ((NULL C) (RETURN NIL)))
       ((LAMBDA (C)
          (PROGN
           (MAPRIN (LIST (CAR C) (REVAL1 (CADR C) T) (CADDR C)))
           (TERPRI* NIL)))
        (CAR C))
       (SETQ C (CDR C))
       (GO LAB))
     (PRIN2* "Bounds")
     (TERPRI* NIL)
     (PROG (V)
       (SETQ V VL)
      LAB
       (COND ((NULL V) (RETURN NIL)))
       ((LAMBDA (V) (PROGN (MAPRIN (LIST 'GEQ V '-INF)) (TERPRI* NIL)))
        (CAR V))
       (SETQ V (CDR V))
       (GO LAB))
     (PROG (I)
       (SETQ I 1)
      LAB
       (COND ((MINUSP (DIFFERENCE RD I)) (RETURN NIL)))
       (PROG1 (CAR VL) (SETQ VL (CDR VL)))
       (SETQ I (PLUS2 I 1))
       (GO LAB))
     (COND
      (VL
       (PROGN
        (PRIN2* "Integers")
        (TERPRI* NIL)
        (PROG (RVL)
          (SETQ RVL VL)
         LAB
          (COND ((NULL RVL) (RETURN NIL)))
          (PROGN (PRIN2* (CAR RVL)) (COND ((CDR RVL) (PRIN2* " "))))
          (SETQ RVL (CDR RVL))
          (GO LAB))
        (TERPRI* NIL))))
     (PRIN2* "End")
     (TERPRI* NIL))) 
(PUT 'LP_READSOL 'NUMBER-OF-ARGS 3) 
(DE LP_READSOL (FN VL D)
    (PROG (CH TOK RES)
      (SETQ CH (OPEN FN 'INPUT))
      (RDS CH)
      (SETQ TOK (READ))
      (COND ((NEQ TOK '|#|) (PROGN (RDS NIL) (CLOSE CH) (RETURN 'INFEASIBLE))))
      (PROG ()
       REPEATLABEL
        (SETQ TOK (READ))
        (COND ((NOT (EQUAL TOK 0)) (GO REPEATLABEL))))
      (SETQ RES
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (CONS (READ) (READ)) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (CONS (READ) (READ)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RDS NIL)
      (CLOSE CH)
      (SETQ RES
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VL)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (CDR (ATSOC V RES))) (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CDR (ATSOC V RES))) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'LP_RUNLINALG 'NUMBER-OF-ARGS 0) 
(PUT 'LP_RUNLINALG 'DEFINED-ON-LINE '227) 
(PUT 'LP_RUNLINALG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_RUNLINALG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_RUNLINALG NIL (LP_RUNLINALG1 LP_MODEL* LP_VARL* LP_DIM*)) 
(PUT 'LP_RUNLINALG1 'NUMBER-OF-ARGS 3) 
(DE LP_RUNLINALG1 (CL VL D)
    (PROG (W BOUNDS)
      (SETQ BOUNDS
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VL)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (CONS V '((MINUS INFINITY) INFINITY)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (CONS V '((MINUS INFINITY) INFINITY)))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (FS_SIMPLEX2 'MIN (SC_SIMP 0) CL BOUNDS))
      (COND ((EQ W 'INFEASIBLE) (RETURN 'INFEASIBLE)))
      (RETURN
       (PROG (E FORALL-RESULT FORALL-ENDPTR)
         (SETQ E (CDR W))
         (COND ((NULL E) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (E) (CADDR E)) (CAR E)) NIL)))
        LOOPLABEL
         (SETQ E (CDR E))
         (COND ((NULL E) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (E) (CADDR E)) (CAR E)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'LP_DUMPMODEL 'NUMBER-OF-ARGS 0) 
(PUT 'LP_DUMPMODEL 'DEFINED-ON-LINE '239) 
(PUT 'LP_DUMPMODEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFLP.RED) 
(PUT 'LP_DUMPMODEL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LP_DUMPMODEL NIL
    (PROGN
     (LP_UPDATEMODEL)
     (COND ((AND *RLGUROBI *RLFFI *LP_CSLP) (GUROBI_DUMPMODEL))
           (T (LP_WRITELP NIL NIL LP_MODEL* LP_VARL* LP_RDIM*))))) 
(ENDMODULE) 
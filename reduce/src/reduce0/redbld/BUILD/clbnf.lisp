(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLBNF)) 
(REVISION 'CLBNF "$Id: clbnf.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'CLBNF
           "(c) 1995-2009 A. Dolzmann, A. Seidl, T. Sturm, 2017 T. Sturm") 
(RL_PROVIDESERVICE 'RL_DNF 'CL_DNF
                   '(RL_SACATLP RL_SACAT RL_BNFSIMPL RL_SUBSUMPTION)) 
(PUT 'CL_DNF 'NUMBER-OF-ARGS 1) 
(PUT 'CL_DNF 'DEFINED-ON-LINE '43) 
(PUT 'CL_DNF 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_DNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_DNF (F) (RL_SIMPL (CL_GDNF0 F 'OR) NIL (MINUS 1))) 
(RL_PROVIDESERVICE 'RL_CNF 'CL_CNF
                   '(RL_SACATLP RL_SACAT RL_BNFSIMPL RL_SUBSUMPTION)) 
(PUT 'CL_CNF 'NUMBER-OF-ARGS 1) 
(PUT 'CL_CNF 'DEFINED-ON-LINE '51) 
(PUT 'CL_CNF 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_CNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_CNF (F) (RL_SIMPL (CL_GDNF0 F 'AND) NIL (MINUS 1))) 
(PUT 'CL_GDNF0 'NUMBER-OF-ARGS 2) 
(PUT 'CL_GDNF0 'DEFINED-ON-LINE '56) 
(PUT 'CL_GDNF0 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_GDNF0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_GDNF0 (F GOR)
    (PROG (QL VL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (OR (EQ (COND ((ATOM F) F) (T (CAR F))) 'EX)
               (EQ (COND ((ATOM F) F) (T (CAR F))) 'ALL)))
          (RETURN NIL)))
        (PROGN
         (SETQ QL (CONS (COND ((ATOM F) F) (T (CAR F))) QL))
         (SETQ VL (CONS (CADR F) VL))
         (SETQ F (CADDR F)))
        (GO WHILELABEL))
      (SETQ F (CL_GDNF F GOR))
      (PROG ()
       WHILELABEL
        (COND ((NOT QL) (RETURN NIL)))
        (PROGN
         (SETQ F (LIST (CAR QL) (CAR VL) F))
         (SETQ QL (CDR QL))
         (SETQ VL (CDR VL)))
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'CL_GDNF 'NUMBER-OF-ARGS 2) 
(PUT 'CL_GDNF 'DEFINED-ON-LINE '74) 
(PUT 'CL_GDNF 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_GDNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_GDNF (F GOR)
    (PROG (STRICTGDNF GDNF SVRLSIEXPLA)
      (SETQ F (RL_SIMPL (RL_NNF F) NIL (MINUS 1)))
      (SETQ SVRLSIEXPLA *RLSIEXPLA)
      (SETQ *RLSIEXPLA NIL)
      ((LAMBDA (*RLBNFSM) (SETQ STRICTGDNF (CL_STRICT-GDNF F GOR))) NIL)
      (COND
       (*RLBNFSM
        (SETQ STRICTGDNF (CONS GOR (CL_SUBSUME (CDR STRICTGDNF) GOR)))))
      (SETQ *RLSIEXPLA SVRLSIEXPLA)
      (SETQ GDNF (CL_UNSTRICT STRICTGDNF GOR))
      (RETURN GDNF))) 
(PUT 'CL_STRICT-GDNF 'NUMBER-OF-ARGS 2) 
(PUT 'CL_STRICT-GDNF 'DEFINED-ON-LINE '89) 
(PUT 'CL_STRICT-GDNF 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_STRICT-GDNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_STRICT-GDNF (F GOR)
    (PROG (W)
      (SETQ W
              (CL_MKSTRICT (RL_SIMPL (CL_STRICT-GDNF1 F GOR) NIL (MINUS 1))
               GOR))
      (RETURN (RL_BNFSIMPL W GOR)))) 
(PUT 'CL_SUBSUME 'NUMBER-OF-ARGS 2) 
(PUT 'CL_SUBSUME 'DEFINED-ON-LINE '98) 
(PUT 'CL_SUBSUME 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SUBSUME 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_SUBSUME (GCL GOR)
    (PROG (W)
      (COND ((OR (NULL GCL) (NULL (CDR GCL))) (RETURN GCL)))
      (SETQ W (CL_SUBSUME1 GCL GOR))
      (COND
       ((CAR W)
        (PROGN (SETCDR (CDR W) (CL_SUBSUME (CDDR W) GOR)) (RETURN (CDR W)))))
      (RETURN (CL_SUBSUME (CDR W) GOR)))) 
(PUT 'CL_SUBSUME1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_SUBSUME1 'DEFINED-ON-LINE '112) 
(PUT 'CL_SUBSUME1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SUBSUME1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_SUBSUME1 (GCL GOR)
    (PROG (A W X SCGCL OSCGCL)
      (SETQ X (CDAR GCL))
      (SETQ OSCGCL GCL)
      (SETQ SCGCL (CDR GCL))
      (PROG ()
       WHILELABEL
        (COND ((NOT SCGCL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCGCL))
         (SETQ SCGCL (CDR SCGCL))
         (SETQ W
                 (COND (*RLBNFSM (RL_SUBSUMPTION X (CDR A) GOR))
                       (T (CL_SETREL X (CDR A) GOR))))
         (COND ((EQ W 'KEEP1) (SETCDR OSCGCL SCGCL))
               ((EQ W 'KEEP2) (SETQ X (SETQ SCGCL NIL)))
               (T (SETQ OSCGCL (CDR OSCGCL)))))
        (GO WHILELABEL))
      (COND ((NULL X) (SETQ GCL (CDR GCL))))
      (RETURN (CONS X GCL)))) 
(PUT 'CL_SETREL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_SETREL 'DEFINED-ON-LINE '143) 
(PUT 'CL_SETREL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SETREL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SETREL (L1 L2 GOR)
    (PROG (KSTATE A1 HLP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L1 L2 (EQ (CAR L1) (CAR L2)))) (RETURN NIL)))
        (PROGN (SETQ L1 (CDR L1)) (SETQ L2 (CDR L2)))
        (GO WHILELABEL))
      (COND
       ((NULL (AND L1 L2))
        (PROGN
         (COND ((NULL (OR L1 L2)) (RETURN 'KEEP1)))
         (RETURN (OR (AND L2 'KEEP1) 'KEEP2)))))
      (SETQ KSTATE 'KEEP1)
      (COND
       ((RL_ORDATP (CAR L1) (CAR L2))
        (PROGN (SETQ HLP L1) (SETQ L1 L2) (SETQ L2 HLP) (SETQ KSTATE 'KEEP2))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ A1 (CAR L1))
         (SETQ L1 (CDR L1))
         (SETQ L2 (MEMQ A1 L2))
         (COND ((NULL L2) (SETQ A1 (SETQ L1 NIL)))))
        (COND ((NOT (NULL L1)) (GO REPEATLABEL))))
      (RETURN (AND A1 KSTATE)))) 
(PUT 'CL_STRICT-GDNF1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_STRICT-GDNF1 'DEFINED-ON-LINE '171) 
(PUT 'CL_STRICT-GDNF1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_STRICT-GDNF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_STRICT-GDNF1 (F GOR)
    (PROG (GAND OP SUBGDNFL NOOP NOOPGDNF)
      (SETQ GAND (COND ((EQ GOR 'OR) 'AND) (T 'OR)))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((EQ OP GOR)
        (RETURN
         (CONS GOR
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR F))
                STARTOVER
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (SUBF) (CDR (CL_STRICT-GDNF SUBF GOR)))
                          (CAR SUBF)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ SUBF (CDR SUBF))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (SUBF) (CDR (CL_STRICT-GDNF SUBF GOR)))
                          (CAR SUBF)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ SUBF (CDR SUBF))
                 (GO LOOPLABEL))))))
      (COND
       ((EQ OP GAND)
        (PROGN
         (SETQ SUBGDNFL
                 (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SUBF (CDR F))
                   (COND ((NULL SUBF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SUBF) (CL_STRICT-GDNF SUBF GOR))
                                     (CAR SUBF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SUBF (CDR SUBF))
                   (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBF) (CL_STRICT-GDNF SUBF GOR))
                             (CAR SUBF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ NOOP
                 (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SUBF SUBGDNFL)
                   (COND ((NULL SUBF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SUBF)
                                       (PROG (GCONJ FORALL-RESULT
                                              FORALL-ENDPTR)
                                         (SETQ GCONJ (CDR SUBF))
                                         (COND ((NULL GCONJ) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (GCONJ)
                                                             (CDR GCONJ))
                                                           (CAR GCONJ))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ GCONJ (CDR GCONJ))
                                         (COND
                                          ((NULL GCONJ)
                                           (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (GCONJ) (CDR GCONJ))
                                                   (CAR GCONJ))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR SUBF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SUBF (CDR SUBF))
                   (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBF)
                               (PROG (GCONJ FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ GCONJ (CDR SUBF))
                                 (COND ((NULL GCONJ) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (GCONJ) (CDR GCONJ))
                                                   (CAR GCONJ))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ GCONJ (CDR GCONJ))
                                 (COND ((NULL GCONJ) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (GCONJ) (CDR GCONJ))
                                           (CAR GCONJ))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR SUBF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ NOOPGDNF (LTO_CARTPROD NOOP))
         (RETURN
          (CONS GOR
                (PROG (GCONJ FORALL-RESULT FORALL-ENDPTR)
                  (SETQ GCONJ NOOPGDNF)
                  (COND ((NULL GCONJ) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (GCONJ)
                                      (CONS GAND
                                            (PROG (X FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ X GCONJ)
                                             STARTOVER
                                              (COND ((NULL X) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      ((LAMBDA (X)
                                                         (APPEND X NIL))
                                                       (CAR X)))
                                              (SETQ FORALL-ENDPTR
                                                      (LASTPAIR FORALL-RESULT))
                                              (SETQ X (CDR X))
                                              (COND
                                               ((ATOM FORALL-ENDPTR)
                                                (GO STARTOVER)))
                                             LOOPLABEL
                                              (COND
                                               ((NULL X)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      ((LAMBDA (X)
                                                         (APPEND X NIL))
                                                       (CAR X)))
                                              (SETQ FORALL-ENDPTR
                                                      (LASTPAIR FORALL-ENDPTR))
                                              (SETQ X (CDR X))
                                              (GO LOOPLABEL))))
                                    (CAR GCONJ))
                                   NIL)))
                 LOOPLABEL
                  (SETQ GCONJ (CDR GCONJ))
                  (COND ((NULL GCONJ) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (GCONJ)
                              (CONS GAND
                                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ X GCONJ)
                                     STARTOVER
                                      (COND ((NULL X) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              ((LAMBDA (X) (APPEND X NIL))
                                               (CAR X)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-RESULT))
                                      (SETQ X (CDR X))
                                      (COND
                                       ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                     LOOPLABEL
                                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              ((LAMBDA (X) (APPEND X NIL))
                                               (CAR X)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-ENDPTR))
                                      (SETQ X (CDR X))
                                      (GO LOOPLABEL))))
                            (CAR GCONJ))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (COND
       ((AND
         (OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
             (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                 (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
             (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
         (NOT (OR (EQ OP 'TRUE) (EQ OP 'FALSE))))
        (REDERR
         (LIST "cl_strict!-gdnf: illegal operator" OP "in BNF computation"))))
      (RETURN (CONS GOR (LIST (CONS GAND (LIST F))))))) 
(PUT 'CL_MKSTRICT 'NUMBER-OF-ARGS 2) 
(PUT 'CL_MKSTRICT 'DEFINED-ON-LINE '202) 
(PUT 'CL_MKSTRICT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_MKSTRICT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_MKSTRICT (F GOR)
    (PROG (OP GAND)
      (SETQ GAND (CL_FLIP GOR))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR
         (NOT
          (OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
              (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                  (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
              (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL))))
         (OR (EQ OP 'TRUE) (EQ OP 'FALSE)))
        (RETURN (CONS GOR (LIST (CONS GAND (LIST F)))))))
      (COND ((EQ OP GAND) (RETURN (CONS GOR (LIST F)))))
      (COND ((NEQ OP GOR) (REDERR (LIST "BUG IN cl_mkstrict"))))
      (RETURN
       (CONS GOR
             (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
               (SETQ SUBF (CDR F))
               (COND ((NULL SUBF) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (SUBF)
                                   (COND
                                    ((EQ
                                      (COND ((ATOM SUBF) SUBF) (T (CAR SUBF)))
                                      GAND)
                                     SUBF)
                                    (T (CONS GAND (LIST SUBF)))))
                                 (CAR SUBF))
                                NIL)))
              LOOPLABEL
               (SETQ SUBF (CDR SUBF))
               (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (SUBF)
                           (COND
                            ((EQ (COND ((ATOM SUBF) SUBF) (T (CAR SUBF))) GAND)
                             SUBF)
                            (T (CONS GAND (LIST SUBF)))))
                         (CAR SUBF))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'CL_UNSTRICT 'NUMBER-OF-ARGS 2) 
(PUT 'CL_UNSTRICT 'DEFINED-ON-LINE '218) 
(PUT 'CL_UNSTRICT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_UNSTRICT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_UNSTRICT (SGDNF GOR)
    ((LAMBDA (G132)
       (COND ((AND G132 (CDR G132)) (CONS GOR G132))
             ((NULL G132) (COND ((EQ GOR 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G132))))
     (PROG (CONJ FORALL-RESULT FORALL-ENDPTR)
       (SETQ CONJ (CDR SGDNF))
       (COND ((NULL CONJ) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (CONJ)
                           (COND ((CDR (CDR CONJ)) CONJ) (T (CAR (CDR CONJ)))))
                         (CAR CONJ))
                        NIL)))
      LOOPLABEL
       (SETQ CONJ (CDR CONJ))
       (COND ((NULL CONJ) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (CONJ)
                   (COND ((CDR (CDR CONJ)) CONJ) (T (CAR (CDR CONJ)))))
                 (CAR CONJ))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'CL_BNFSIMPL 'NUMBER-OF-ARGS 2) 
(PUT 'CL_BNFSIMPL 'DEFINED-ON-LINE '225) 
(PUT 'CL_BNFSIMPL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_BNFSIMPL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_BNFSIMPL (SGDNF GOR) (COND (*RLBNFSAC (CL_SAC SGDNF GOR)) (T SGDNF))) 
(PUT 'CL_SAC 'NUMBER-OF-ARGS 2) 
(PUT 'CL_SAC 'DEFINED-ON-LINE '232) 
(PUT 'CL_SAC 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SAC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_SAC (SGDNF GOR)
    (PROG (W GAND)
      (COND
       (((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
         (CAR (CDR (CAR (CDR SGDNF)))))
        (RETURN SGDNF)))
      (SETQ GAND (CL_FLIP GOR))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR SGDNF))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (CL_APPLYSAC W GOR))
      (COND
       ((EQ W 'BREAK)
        (RETURN
         (CONS GOR (LIST (CONS GAND (LIST (CL_CFLIP 'TRUE (EQ GOR 'OR)))))))))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X W)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND (X (LIST (CONS GAND X))) (T NIL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND (X (LIST (CONS GAND X))) (T NIL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (COND
       ((NULL W)
        (RETURN
         (CONS GOR (LIST (CONS GAND (LIST (CL_CFLIP 'TRUE (EQ GOR 'OR)))))))))
      (RETURN (CONS GOR W)))) 
(PUT 'CL_APPLYSAC 'NUMBER-OF-ARGS 2) 
(PUT 'CL_APPLYSAC 'DEFINED-ON-LINE '258) 
(PUT 'CL_APPLYSAC 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_APPLYSAC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_APPLYSAC (L GOR)
    (PROG (W LL RES)
      (SETQ LL L)
      (PROG ()
       WHILELABEL
        (COND ((NOT LL) (RETURN NIL)))
        (PROGN
         (SETQ W (CL_APPLYSAC1 (CAR LL) RES GOR))
         (COND ((EQ W 'BREAK) (PROGN (SETQ LL NIL) (SETQ RES 'BREAK)))
               (T
                (PROGN
                 (SETQ LL (CDR LL))
                 (COND ((CAR W) (SETQ RES (CONS (CDAR W) (CDR W))))
                       (T (SETQ RES (CDR W))))))))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'CL_APPLYSAC1 'NUMBER-OF-ARGS 3) 
(PUT 'CL_APPLYSAC1 'DEFINED-ON-LINE '285) 
(PUT 'CL_APPLYSAC1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_APPLYSAC1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_APPLYSAC1 (C L GOR)
    (PROG (W FLG)
      (SETQ FLG T)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ W (CL_APPLYSAC2 C L GOR))
         (COND ((EQ W 'BREAK) (PROGN (SETQ W '(NIL)) (SETQ FLG 'BREAK))))
         (COND
          ((AND (CAR W) (NULL (CAAR W)))
           (PROGN (SETQ FLG NIL) (SETQ C (CDAR W)) (SETQ L (CDR W)))))
         NIL)
        (COND ((NOT (OR (NULL (CAR W)) (CAAR W))) (GO REPEATLABEL))))
      (COND ((EQ FLG 'BREAK) (RETURN 'BREAK)))
      (COND ((NULL (CAR W)) (RETURN W)))
      (RETURN (CONS (CONS FLG (CDAR W)) (CDR W))))) 
(PUT 'CL_APPLYSAC2 'NUMBER-OF-ARGS 3) 
(PUT 'CL_APPLYSAC2 'DEFINED-ON-LINE '321) 
(PUT 'CL_APPLYSAC2 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_APPLYSAC2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_APPLYSAC2 (C L GOR)
    (PROG (W LL)
      (COND ((NULL L) (RETURN (CONS (CONS T C) NIL))))
      (SETQ LL L)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND LL (EQ (SETQ W (CL_SUBANDCUT C (CAR LL) GOR)) 'KEEP1)))
          (RETURN NIL)))
        (SETQ LL (CDR LL))
        (GO WHILELABEL))
      (COND ((NULL W) (RETURN 'BREAK)))
      (COND ((NULL LL) (RETURN (CONS (CONS T C) NIL))))
      (COND ((EQ W 'KEEP2) (RETURN (CONS NIL LL))))
      (COND ((NEQ W 'FAILED) (RETURN (CONS (CONS NIL W) (CDR LL)))))
      (SETQ W (CL_APPLYSAC2 C (CDR LL) GOR))
      (COND ((EQ W 'BREAK) (RETURN 'BREAK)))
      (SETCDR LL (CDR W))
      (RETURN (CONS (CAR W) LL)))) 
(PUT 'CL_SUBANDCUT 'NUMBER-OF-ARGS 3) 
(PUT 'CL_SUBANDCUT 'DEFINED-ON-LINE '356) 
(PUT 'CL_SUBANDCUT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SUBANDCUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SUBANDCUT (L1 L2 GOR)
    (PROG (KSTATE W X C)
      (SETQ C 0)
      (SETQ X L1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L1 L2 (EQUAL (CAR L1) (CAR L2)))) (RETURN NIL)))
        (PROGN (SETQ C (PLUS C 1)) (SETQ L1 (CDR L1)) (SETQ L2 (CDR L2)))
        (GO WHILELABEL))
      (COND
       ((NULL (AND L1 L2))
        (PROGN
         (COND ((NULL (OR L1 L2)) (RETURN 'KEEP1)))
         (RETURN (OR (AND L2 'KEEP1) 'KEEP2)))))
      (SETQ KSTATE 'KEEP1)
      (SETQ W (RL_SACAT (CAR L1) (CAR L2) GOR))
      (COND
       ((EQ W 'KEEP2)
        (PROGN
         (SETQ KSTATE 'KEEP2)
         (SETQ W (CDR L1))
         (SETQ L1 (CDR L2))
         (SETQ L2 W)))
       ((EQ W 'KEEP1) (PROGN (SETQ L1 (CDR L1)) (SETQ L2 (CDR L2))))
       (W (RETURN (CL_TRYCUT X C W (CDR L1) (CDR L2))))
       ((RL_ORDATP (CAR L1) (CAR L2))
        (PROGN (SETQ KSTATE 'KEEP2) (SETQ W L1) (SETQ L1 L2) (SETQ L2 W))))
      (PROG ()
       WHILELABEL
        (COND ((NOT L1) (RETURN NIL)))
        (PROGN
         (SETQ W (CL_SACATL (CAR L1) L2 GOR))
         (SETQ L2 (CDR W))
         (SETQ W (CAR W))
         (SETQ L1 (CDR L1))
         (COND ((NEQ W 'KEEP1) (SETQ L1 NIL))))
        (GO WHILELABEL))
      (COND ((EQ W 'KEEP1) (RETURN KSTATE)))
      (RETURN 'FAILED))) 
(PUT 'CL_TRYCUT 'NUMBER-OF-ARGS 5) 
(PUT 'CL_TRYCUT 'DEFINED-ON-LINE '405) 
(PUT 'CL_TRYCUT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_TRYCUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_TRYCUT (L C AT L1 L2)
    (PROG (A)
      (COND
       ((AND (NULL L1) (NULL L2))
        (PROGN
         (SETQ L
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                   (COND ((MINUSP (DIFFERENCE C I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (PROGN (SETQ A (CAR L)) (SETQ L (CDR L)) A)
                                    NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND ((MINUSP (DIFFERENCE C I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS (PROGN (SETQ A (CAR L)) (SETQ L (CDR L)) A)
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND ((EQ AT 'DROP) (RETURN (SORT L 'RL_ORDATP))))
         (RETURN (SORT (CONS AT L) 'RL_ORDATP)))))
      (COND ((NEQ L1 L2) (RETURN 'FAILED)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE C I)) (RETURN NIL)))
        (PROGN (SETQ L1 (CONS (CAR L) L1)) (SETQ L (CDR L)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NEQ AT 'DROP) (SETQ L1 (CONS AT L1))))
      (RETURN (SORT L1 'RL_ORDATP)))) 
(PUT 'CL_SACATL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_SACATL 'DEFINED-ON-LINE '434) 
(PUT 'CL_SACATL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SACATL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SACATL (A L GOR)
    (PROG (W)
      (COND ((NULL L) (RETURN '(NIL))))
      (COND ((NOT (RL_SACATLP A L)) (RETURN (CONS NIL L))))
      (SETQ W (RL_SACAT A (CAR L) GOR))
      (COND ((NOT W) (RETURN (CL_SACATL A (CDR L) GOR))))
      (COND ((MEMQ W '(KEEP1 KEEP)) (RETURN (CONS 'KEEP1 (CDR L)))))
      (COND ((EQ W 'KEEP2) (RETURN (CONS NIL (CDR L)))))
      (RETURN (CONS W (CDR L))))) 
(PUT 'CL_SACATLP 'NUMBER-OF-ARGS 2) 
(PUT 'CL_SACATLP 'DEFINED-ON-LINE '460) 
(PUT 'CL_SACATLP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SACATLP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_SACATLP (A L) T) 
(PUT 'CL_SACAT 'NUMBER-OF-ARGS 3) 
(PUT 'CL_SACAT 'DEFINED-ON-LINE '467) 
(PUT 'CL_SACAT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SACAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SACAT (A1 A2 GOR) (COND ((EQUAL A1 A2) 'KEEP) (T NIL))) 
(RL_PROVIDESERVICE 'RL_QUINE 'CL_QUINE
                   '(RL_QSSIMPL RL_QSSUSUA RL_QSTAUTP RL_QSSUBSUMEPD
                                RL_QSIMPLTESTCCL RL_QSSIADD RL_QSTRYCONS
                                RL_QSCONSENS RL_QSSUBAT RL_QSCSAAT RL_NEGATEAT)) 
(PUT 'CL_QUINE 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QUINE 'DEFINED-ON-LINE '537) 
(PUT 'CL_QUINE 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QUINE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QUINE (F)
    (PROG (W OP S)
      (SETQ W (CL_BNF2SET F))
      (SETQ OP (CAR W))
      (COND ((NOT OP) (RETURN F)))
      (COND ((EQ OP 'AND) (SETQ S (CL_QSNOT (CDR W)))) (T (SETQ S (CDR W))))
      (SETQ S (CL_QS S 'OR))
      (COND ((EQ OP 'AND) (SETQ S (CL_QSNOT S))))
      (RETURN (CL_SET2BNF S OP)))) 
(PUT 'CL_QSNOT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSNOT 'DEFINED-ON-LINE '555) 
(PUT 'CL_QSNOT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSNOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSNOT (S)
    (PROG (C FORALL-RESULT FORALL-ENDPTR)
      (SETQ C S)
      (COND ((NULL C) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (C) (CL_QSNOT1 C)) (CAR C)) NIL)))
     LOOPLABEL
      (SETQ C (CDR C))
      (COND ((NULL C) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (C) (CL_QSNOT1 C)) (CAR C)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_QSNOT1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSNOT1 'DEFINED-ON-LINE '558) 
(PUT 'CL_QSNOT1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSNOT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSNOT1 (C)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A C)
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (A) (RL_NEGATEAT A)) (CAR A)) NIL)))
     LOOPLABEL
      (SETQ A (CDR A))
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (RL_NEGATEAT A)) (CAR A)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_BNF2SET 'NUMBER-OF-ARGS 1) 
(PUT 'CL_BNF2SET 'DEFINED-ON-LINE '561) 
(PUT 'CL_BNF2SET 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_BNF2SET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_BNF2SET (F) (CL_BNF2SET1 F NIL)) 
(PUT 'CL_BNF2SET1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_BNF2SET1 'DEFINED-ON-LINE '564) 
(PUT 'CL_BNF2SET1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_BNF2SET1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_BNF2SET1 (F XOP)
    (PROG (OP W)
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN (CONS NIL F))))
      (COND ((NOT (CL_CXFP F)) (RETURN (CONS NIL (LIST (LIST F))))))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((NOT (MEMQ OP '(AND OR)))
        (REDERR (LIST "cl_bnf2set: not in bnf: " F))))
      (SETQ W (CL_BNF2SET2 (CDR F) OP))
      (COND
       ((NOT (CAR W))
        (COND ((EQ OP XOP) (RETURN (CONS OP W)))
              ((EQ (CL_FLIP OP) XOP) (RETURN (CONS OP (LIST (CDR F)))))
              (T (RETURN (CONS NIL F))))))
      (RETURN (CONS OP (CDR W))))) 
(PUT 'CL_BNF2SET2 'NUMBER-OF-ARGS 2) 
(PUT 'CL_BNF2SET2 'DEFINED-ON-LINE '591) 
(PUT 'CL_BNF2SET2 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_BNF2SET2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_BNF2SET2 (FL OP)
    (PROG (XOP FLG W)
      (SETQ XOP (CL_FLIP OP))
      (SETQ W
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FL)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (PROGN
                                     (COND ((NOT (CL_CXFP F)) (LIST F))
                                           ((EQ (COND ((ATOM F) F) (T (CAR F)))
                                                XOP)
                                            (PROGN
                                             (SETQ FLG T)
                                             (SORT (LTO_LIST2SET (CDR F))
                                                   'RL_ORDATP)))
                                           (T
                                            (REDERR
                                             (LIST "cl_bnf2set1: not in bnf: "
                                                   F))))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (PROGN
                             (COND ((NOT (CL_CXFP F)) (LIST F))
                                   ((EQ (COND ((ATOM F) F) (T (CAR F))) XOP)
                                    (PROGN
                                     (SETQ FLG T)
                                     (SORT (LTO_LIST2SET (CDR F)) 'RL_ORDATP)))
                                   (T
                                    (REDERR
                                     (LIST "cl_bnf2set1: not in bnf: " F))))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS FLG (LTO_LIST2SET W))))) 
(PUT 'CL_SET2BNF 'NUMBER-OF-ARGS 2) 
(PUT 'CL_SET2BNF 'DEFINED-ON-LINE '611) 
(PUT 'CL_SET2BNF 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SET2BNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_SET2BNF (LL OP)
    (PROG (FLOP)
      (SETQ FLOP (CL_FLIP OP))
      (RETURN
       ((LAMBDA (G134)
          (COND ((AND G134 (CDR G134)) (CONS OP G134))
                ((NULL G134) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G134))))
        (PROG (L FORALL-RESULT FORALL-ENDPTR)
          (SETQ L LL)
          (COND ((NULL L) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (L)
                              (COND ((AND L (CDR L)) (CONS FLOP L))
                                    ((NULL L)
                                     (COND ((EQ FLOP 'AND) 'TRUE) (T 'FALSE)))
                                    (T (CAR L))))
                            (CAR L))
                           NIL)))
         LOOPLABEL
          (SETQ L (CDR L))
          (COND ((NULL L) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (L)
                      (COND ((AND L (CDR L)) (CONS FLOP L))
                            ((NULL L) (COND ((EQ FLOP 'AND) 'TRUE) (T 'FALSE)))
                            (T (CAR L))))
                    (CAR L))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'CL_QS 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QS 'DEFINED-ON-LINE '618) 
(PUT 'CL_QS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QS (S OP)
    (PROG (W)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "[Quine: " (CL_QSSIZE S) "/" (LENGTH S)))))
      (SETQ W (CL_QSCPI S OP))
      (COND
       ((EQ W 'BREAK)
        (PROGN (IOTO_PRIN2T (LIST " -> 0/0]")) (RETURN (LIST (LIST))))))
      (COND
       (*RLVERBOSE (IOTO_PRIN2 (LIST " -> " (CL_QSSIZE W) "/" (LENGTH W)))))
      (RETURN (CL_QSSELECT W OP)))) 
(PUT 'CL_QSCPI 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSCPI 'DEFINED-ON-LINE '634) 
(PUT 'CL_QSCPI 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSCPI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSCPI (CL OP)
    (PROG (FIRSTL SCFIRSTL FIRST SECONDL SCSECONDL SECOND NEWL W)
      (SETQ NEWL CL)
      (PROG ()
       WHILELABEL
        (COND ((NOT NEWL) (RETURN NIL)))
        (PROGN
         (SETQ NEWL (CL_QSSISU NEWL OP))
         (SETQ FIRSTL (CL_QSSISUTWO FIRSTL NEWL OP))
         (SETQ SECONDL NEWL)
         (SETQ NEWL NIL)
         (SETQ SCFIRSTL FIRSTL)
         (PROG ()
          WHILELABEL
           (COND ((NOT SCFIRSTL) (RETURN NIL)))
           (PROGN
            (SETQ FIRST (CAR SCFIRSTL))
            (SETQ SCFIRSTL (CDR SCFIRSTL))
            (SETQ SCSECONDL SECONDL)
            (PROG ()
             WHILELABEL
              (COND ((NOT SCSECONDL) (RETURN NIL)))
              (PROGN
               (SETQ SECOND (CAR SCSECONDL))
               (SETQ SCSECONDL (CDR SCSECONDL))
               (COND
                ((NOT (EQ SECOND FIRST))
                 (PROGN
                  (SETQ W (RL_QSCONSENS FIRST SECOND OP))
                  (COND
                   ((EQ W 'BREAK)
                    (SETQ NEWL (SETQ SCSECONDL (SETQ SCFIRSTL NIL))))
                   (T
                    (PROG (CS)
                      (SETQ CS W)
                     LAB
                      (COND ((NULL CS) (RETURN NIL)))
                      ((LAMBDA (CS)
                         (COND
                          ((AND CS (NOT (CL_QSSUBSUMELP CS FIRSTL OP))
                                (NOT (MEMBER CS NEWL)))
                           (SETQ NEWL (CONS CS NEWL)))))
                       (CAR CS))
                      (SETQ CS (CDR CS))
                      (GO LAB))))))))
              (GO WHILELABEL)))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (COND ((EQ W 'BREAK) (RETURN 'BREAK)))
      (RETURN FIRSTL))) 
(PUT 'CL_QSSISU 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSISU 'DEFINED-ON-LINE '672) 
(PUT 'CL_QSSISU 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSISU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSISU (L OP)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X)
                 (COND ((NOT (CL_QSSUBSUMELP X (LTO_DELQ X L) OP)) (LIST X))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X)
                 (COND ((NOT (CL_QSSUBSUMELP X (LTO_DELQ X L) OP)) (LIST X))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'CL_QSSISUTWO 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSISUTWO 'DEFINED-ON-LINE '679) 
(PUT 'CL_QSSISUTWO 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSISUTWO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSISUTWO (L1 L2 OP)
    (PROG (W)
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X L1)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND ((NOT (CL_QSSUBSUMELP X L2 OP)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND ((NOT (CL_QSSUBSUMELP X L2 OP)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (NCONC W L2)))) 
(PUT 'CL_QSSUBSUMELP 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUBSUMELP 'DEFINED-ON-LINE '689) 
(PUT 'CL_QSSUBSUMELP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUBSUMELP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUBSUMELP (C CL OP)
    (PROG (R)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CL (NOT R))) (RETURN NIL)))
        (PROGN (SETQ R (CL_QSSUBSUMEP C (CAR CL) OP)) (SETQ CL (CDR CL)) NIL)
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSUBSUMEP 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUBSUMEP 'DEFINED-ON-LINE '699) 
(PUT 'CL_QSSUBSUMEP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUBSUMEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUBSUMEP (C1 C2 OP)
    (COND ((EQ OP 'OR) (RL_QSSUBSUMEP C1 C2 OP)) (T (RL_QSSUBSUMEP C2 C1 OP)))) 
(SWITCH (LIST 'PSEN)) 
(ON1 'PSEN) 
(PUT 'CL_QSSELECT 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSELECT 'DEFINED-ON-LINE '708) 
(PUT 'CL_QSSELECT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSELECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSELECT (S OP)
    (PROG (W CORE DCS CSIZE WSIZE CLEN WLEN)
      (SETQ CSIZE 0)
      (SETQ WSIZE 0)
      (SETQ CLEN 0)
      (SETQ WLEN 0)
      (SETQ W (CL_QSSPLTCORE S OP))
      (SETQ CORE (CAR W))
      (SETQ DCS (CDR W))
      (COND
       (*RLVERBOSE
        (PROGN
         (SETQ CSIZE (CL_QSSIZE CORE))
         (SETQ CLEN (LENGTH CORE))
         (IOTO_PRIN2
          (LIST " -> " CSIZE "/" CLEN "+" (CL_QSSIZE DCS) "/" (LENGTH DCS))))))
      (SETQ DCS (CL_QSRMABSDISP CORE DCS OP))
      (COND
       (*RLVERBOSE
        (IOTO_PRIN2
         (LIST " -> " CSIZE "/" CLEN "+" (CL_QSSIZE DCS) "/" (LENGTH DCS)))))
      (COND (*PSEN (SETQ W (CL_QSSELECT2 CORE DCS OP)))
            (T (SETQ W (CL_QSSELECT1 CORE DCS (CL_SUBSETS DCS) OP))))
      (COND
       (*RLVERBOSE
        (PROGN
         (SETQ WSIZE (CL_QSSIZE W))
         (SETQ WLEN (LENGTH W))
         (IOTO_PRIN2T
          (LIST " -> " CSIZE "/" CLEN "+" WSIZE "/" WLEN " = "
                (PLUS CSIZE WSIZE) "/" (PLUS CLEN WLEN) "]")))))
      (SETQ W (NCONC W CORE))
      (RETURN W))) 
(PUT 'CL_QSSELECT1 'NUMBER-OF-ARGS 4) 
(PUT 'CL_QSSELECT1 'DEFINED-ON-LINE '735) 
(PUT 'CL_QSSELECT1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSELECT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSELECT1 (CORE DCS POTDCS OP)
    (PROG (R W)
      (SETQ POTDCS (CDR (REVERSIP POTDCS)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND POTDCS (NOT R))) (RETURN NIL)))
        (PROGN
         (SETQ W (SETDIFF DCS (CAR POTDCS)))
         (COND
          ((CL_QSIMPLTESTCLCL (CAR POTDCS) (APPEND CORE W) OP) (SETQ R W)))
         (SETQ POTDCS (CDR POTDCS))
         NIL)
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSELECT2 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSELECT2 'DEFINED-ON-LINE '747) 
(PUT 'CL_QSSELECT2 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSELECT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSELECT2 (CORE DCS OP)
    (PROG (R W KSTATE POTDCS)
      (SETQ KSTATE (CONS DCS NIL))
      (CL_PS KSTATE)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NEQ (SETQ POTDCS (CL_PS KSTATE)) 'FINAL) (NOT R)))
          (RETURN NIL)))
        (PROGN
         (SETQ W (SETDIFF DCS POTDCS))
         (COND ((CL_QSIMPLTESTCLCL W (APPEND CORE POTDCS) OP) (SETQ R POTDCS)))
         NIL)
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSPLTCORE 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSPLTCORE 'DEFINED-ON-LINE '759) 
(PUT 'CL_QSSPLTCORE 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSPLTCORE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSPLTCORE (S OP)
    (PROG (CORE DCS)
      (PROG (X)
        (SETQ X S)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((RL_QSIMPLTESTCCL X (LTO_DELQ X S) OP) (SETQ DCS (CONS X DCS)))
            (T (SETQ CORE (CONS X CORE)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS CORE DCS)))) 
(PUT 'CL_QSRMABSDISP 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSRMABSDISP 'DEFINED-ON-LINE '769) 
(PUT 'CL_QSRMABSDISP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSRMABSDISP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSRMABSDISP (CORE DCS OP)
    (PROG (C FORALL-RESULT FORALL-ENDPTR)
      (SETQ C DCS)
     STARTOVER
      (COND ((NULL C) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (C)
                 (COND ((NOT (RL_QSIMPLTESTCCL C CORE OP)) (LIST C))))
               (CAR C)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ C (CDR C))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL C) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (C)
                 (COND ((NOT (RL_QSIMPLTESTCCL C CORE OP)) (LIST C))))
               (CAR C)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ C (CDR C))
      (GO LOOPLABEL))) 
(PUT 'CL_QSIMPLTESTCLCL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSIMPLTESTCLCL 'DEFINED-ON-LINE '774) 
(PUT 'CL_QSIMPLTESTCLCL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSIMPLTESTCLCL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSIMPLTESTCLCL (PL CL OP)
    (PROG (R)
      (SETQ R T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PL R)) (RETURN NIL)))
        (PROGN
         (SETQ R (RL_QSIMPLTESTCCL (CAR PL) CL OP))
         (SETQ PL (CDR PL))
         NIL)
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_SUBSETS 'NUMBER-OF-ARGS 1) 
(PUT 'CL_SUBSETS 'DEFINED-ON-LINE '788) 
(PUT 'CL_SUBSETS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SUBSETS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_SUBSETS (L) (SORT (CL_SUBSETS1 L) 'CL_LENGTHP)) 
(PUT 'CL_LENGTHP 'NUMBER-OF-ARGS 2) 
(PUT 'CL_LENGTHP 'DEFINED-ON-LINE '791) 
(PUT 'CL_LENGTHP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_LENGTHP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_LENGTHP (L1 L2) (LESSP (LENGTH L1) (LENGTH L2))) 
(PUT 'CL_SUBSETS1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_SUBSETS1 'DEFINED-ON-LINE '794) 
(PUT 'CL_SUBSETS1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_SUBSETS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_SUBSETS1 (L)
    (PROG (W R X)
      (COND ((NULL L) (RETURN (LIST NIL))))
      (SETQ W (CL_SUBSETS1 (CDR L)))
      (SETQ X (CAR L))
      (PROG (Y)
        (SETQ Y W)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (SETQ R (CONS (CONS X Y) R))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (NCONC R W)))) 
(PUT 'CL_QSSIZE 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSSIZE 'DEFINED-ON-LINE '805) 
(PUT 'CL_QSSIZE 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSSIZE (S)
    (PROG (X FORALL-RESULT)
      (SETQ X S)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (X) (LENGTH X)) (CAR X)) FORALL-RESULT))
      (SETQ X (CDR X))
      (GO LAB1))) 
(PUT 'CL_QSSUSUBYSI 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYSI 'DEFINED-ON-LINE '812) 
(PUT 'CL_QSSUSUBYSI 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYSI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYSI (C1 C2 OP) (REDERR "Out of order -> ueber rl_simpl")) 
(PUT 'CL_QSSUSUBYIT 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYIT 'DEFINED-ON-LINE '817) 
(PUT 'CL_QSSUSUBYIT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYIT (C1 C2 OP) (EQ (CL_QSIMPLTESTCC C1 C2 OP) 'TRUE)) 
(PUT 'CL_QSSUSUBYMEM 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYMEM 'DEFINED-ON-LINE '821) 
(PUT 'CL_QSSUSUBYMEM 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYMEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYMEM (C1 C2 OP)
    (PROG (L1 L2)
      (SETQ L1 (LENGTH C1))
      (SETQ L2 (LENGTH C2))
      (COND ((NOT (GEQ L1 L2)) (RETURN NIL)))
      (RETURN (CL_QSSUSUBYMEM1 C1 C2)))) 
(PUT 'CL_QSSUSUBYMEM1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSUSUBYMEM1 'DEFINED-ON-LINE '832) 
(PUT 'CL_QSSUSUBYMEM1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYMEM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYMEM1 (C1 C2)
    (PROG (R)
      (SETQ R T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C2 R)) (RETURN NIL)))
        (PROGN
         (COND ((NOT (MEMBER (CAR C2) C1)) (SETQ R NIL)))
         (SETQ C2 (CDR C2))
         NIL)
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSUSUBYTAB 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYTAB 'DEFINED-ON-LINE '843) 
(PUT 'CL_QSSUSUBYTAB 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYTAB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYTAB (C1 C2 OP)
    (PROG (L1 L2)
      (SETQ L1 (LENGTH C1))
      (SETQ L2 (LENGTH C2))
      (COND ((NOT (GEQ L1 L2)) (RETURN NIL)))
      (RETURN (CL_QSSUSUBYTAB1 C1 C2 OP)))) 
(PUT 'CL_QSSUSUBYTAB1 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYTAB1 'DEFINED-ON-LINE '854) 
(PUT 'CL_QSSUSUBYTAB1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYTAB1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYTAB1 (C1 C2 OP)
    (PROG (R)
      (SETQ R T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C2 R)) (RETURN NIL)))
        (PROGN (SETQ R (CL_QSSUSUBYTAB2 C1 (CAR C2) OP)) (SETQ C2 (CDR C2)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSUSUBYTAB2 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSUSUBYTAB2 'DEFINED-ON-LINE '866) 
(PUT 'CL_QSSUSUBYTAB2 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUSUBYTAB2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUSUBYTAB2 (C1 A OP)
    (PROG (R)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C1 (NOT R))) (RETURN NIL)))
        (PROGN (SETQ R (RL_QSSUSUAT (CAR C1) A OP)) (SETQ C1 (CDR C1)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'CL_QSSIMPL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSIMPL 'DEFINED-ON-LINE '880) 
(PUT 'CL_QSSIMPL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSIMPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSIMPL (S THEO OP)
    (PROG (R A W ATS)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND S (NOT (OR (EQ R 'TRUE) (EQ R 'FALSE))))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR S))
         (SETQ W (CL_QSSIMPLC A THEO OP))
         (COND ((EQ W 'TRUE) (SETQ R 'TRUE))
               ((NEQ W 'FALSE) (SETQ R (CONS W R))))
         (SETQ S (CDR S))
         NIL)
        (GO WHILELABEL))
      (COND ((EQ R 'TRUE) 'TRUE))
      (SETQ W (NCONC ATS R))
      (RETURN (COND ((NULL W) 'FALSE) (T W))))) 
(PUT 'CL_QSSIMPLC 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSIMPLC 'DEFINED-ON-LINE '899) 
(PUT 'CL_QSSIMPLC 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSIMPLC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSIMPLC (C THEO OP)
    (PROG (R A)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C (NEQ R 'FALSE))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR C))
         (COND ((EQ A 'FALSE) (SETQ R 'FALSE))
               ((NEQ A 'TRUE) (SETQ R (RL_QSSIADD A R THEO OP))))
         (SETQ C (CDR C))
         NIL)
        (GO WHILELABEL))
      (RETURN (COND ((NULL R) 'TRUE) (T R))))) 
(PUT 'CL_QSSIBYSIMPL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSSIBYSIMPL 'DEFINED-ON-LINE '918) 
(PUT 'CL_QSSIBYSIMPL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSIBYSIMPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSSIBYSIMPL (S THEO OP)
    (PROG (*RLSIEXPLA *RLSIEXPL F W)
      (SETQ F (RL_SIMPL (CL_SET2BNF S OP) NIL (MINUS 1)))
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (SETQ W (CL_BNF2SET1 F 'OR))
      (RETURN (CDR W)))) 
(PUT 'CL_QSIMPLTESTCCL 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSIMPLTESTCCL 'DEFINED-ON-LINE '931) 
(PUT 'CL_QSIMPLTESTCCL 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSIMPLTESTCCL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSIMPLTESTCCL (CP CLC OP)
    (PROG (W R SL)
      (SETQ SL (CL_QSCSA CP))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CLC (NEQ R 'TRUE))) (RETURN NIL)))
        (PROGN
         (SETQ W (CL_QSIMPLTESTCC1 SL CP (CAR CLC) OP))
         (COND ((EQ W 'TRUE) (SETQ R 'TRUE))
               ((NEQ W 'FALSE) (SETQ R (CONS W R))))
         (SETQ CLC (CDR CLC))
         NIL)
        (GO WHILELABEL))
      (RETURN (RL_QSTAUTP R)))) 
(PUT 'CL_QSIMPLTESTCC 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSIMPLTESTCC 'DEFINED-ON-LINE '949) 
(PUT 'CL_QSIMPLTESTCC 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSIMPLTESTCC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSIMPLTESTCC (CP CC OP) (CL_QSIMPLTESTCC1 (CL_QSCSA CP) CP CC OP)) 
(PUT 'CL_QSIMPLTESTCC1 'NUMBER-OF-ARGS 4) 
(PUT 'CL_QSIMPLTESTCC1 'DEFINED-ON-LINE '952) 
(PUT 'CL_QSIMPLTESTCC1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSIMPLTESTCC1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSIMPLTESTCC1 (SL CP CC OP)
    (PROG (W)
      (SETQ W (RL_QSSIMPL (LIST (CL_QSSUBC SL CC)) NIL OP))
      (RETURN
       (COND ((OR (EQ W 'TRUE) (EQ W 'FALSE)) W)
             ((CDR W)
              (REDERR
               (LIST "cl_qssimpltestcc1: Unexpected complex formula" W)))
             (T (CAR W)))))) 
(PUT 'CL_QSTAUTP 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSTAUTP 'DEFINED-ON-LINE '963) 
(PUT 'CL_QSTAUTP 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSTAUTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSTAUTP (F)
    (COND ((EQ F 'TRUE) T) ((EQ F 'FALSE) NIL)
          (T (EQ (CL_QSCPI F 'OR) 'BREAK)))) 
(PUT 'CL_QSCSA 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSCSA 'DEFINED-ON-LINE '971) 
(PUT 'CL_QSCSA 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSCSA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSCSA (C)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A C)
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (A) (RL_QSCSAAT A)) (CAR A)) NIL)))
     LOOPLABEL
      (SETQ A (CDR A))
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (RL_QSCSAAT A)) (CAR A)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_QSSUB 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSUB 'DEFINED-ON-LINE '976) 
(PUT 'CL_QSSUB 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUB (PL S)
    (PROG (C FORALL-RESULT FORALL-ENDPTR)
      (SETQ C S)
      (COND ((NULL C) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (C) (CL_QSSUBC PL C)) (CAR C)) NIL)))
     LOOPLABEL
      (SETQ C (CDR C))
      (COND ((NULL C) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (C) (CL_QSSUBC PL C)) (CAR C)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_QSSUBC 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSUBC 'DEFINED-ON-LINE '980) 
(PUT 'CL_QSSUBC 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUBC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUBC (PL C)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A C)
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (A) (RL_QSSUBAT PL A)) (CAR A)) NIL)))
     LOOPLABEL
      (SETQ A (CDR A))
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (A) (RL_QSSUBAT PL A)) (CAR A)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_QSCSAAT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QSCSAAT 'DEFINED-ON-LINE '988) 
(PUT 'CL_QSCSAAT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSCSAAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QSCSAAT (A) A) 
(PUT 'CL_QSSUBAT 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QSSUBAT 'DEFINED-ON-LINE '995) 
(PUT 'CL_QSSUBAT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSSUBAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QSSUBAT (PL A)
    (COND ((MEMBER A PL) 'TRUE) ((MEMBER (RL_NEGATEAT A) PL) 'FALSE) (T A))) 
(PUT 'CL_QS1CONSENS 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QS1CONSENS 'DEFINED-ON-LINE '1009) 
(PUT 'CL_QS1CONSENS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QS1CONSENS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QS1CONSENS (C1 C2 OP)
    (PROG (L1 L2 W)
      (SETQ L1 (LENGTH C1))
      (SETQ L2 (LENGTH C2))
      (SETQ W
              (COND ((LESSP L1 L2) (CL_QS1CONSENS1 C1 C2 OP))
                    (T (CL_QS1CONSENS1 C2 C1 OP))))
      (RETURN (COND ((EQ W 'BREAK) 'BREAK) (T (LIST W)))))) 
(PUT 'CL_QS1CONSENS1 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QS1CONSENS1 'DEFINED-ON-LINE '1023) 
(PUT 'CL_QS1CONSENS1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QS1CONSENS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QS1CONSENS1 (C1 C2 OP)
    (PROG (W SC1)
      (SETQ SC1 C1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SC1 (NOT W))) (RETURN NIL)))
        (PROGN (SETQ W (RL_QSTRYCONS (CAR SC1) C1 C2 OP)) (SETQ SC1 (CDR SC1)))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'CL_QSNCONSENS 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSNCONSENS 'DEFINED-ON-LINE '1035) 
(PUT 'CL_QSNCONSENS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSNCONSENS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSNCONSENS (C1 C2 OP)
    (PROG (L1 L2)
      (SETQ L1 (LENGTH C1))
      (SETQ L2 (LENGTH C2))
      (RETURN
       (COND ((LESSP L1 L2) (CL_QSNCONSENS1 C1 C2 OP))
             (T (CL_QSNCONSENS1 C2 C1 OP)))))) 
(PUT 'CL_QSNCONSENS1 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QSNCONSENS1 'DEFINED-ON-LINE '1048) 
(PUT 'CL_QSNCONSENS1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSNCONSENS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSNCONSENS1 (C1 C2 OP)
    (PROG (W R SC1)
      (SETQ SC1 C1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SC1 (NEQ W 'BREAK))) (RETURN NIL)))
        (PROGN
         (SETQ W (RL_QSTRYCONS (CAR SC1) C1 C2 OP))
         (COND (W (SETQ R (CONS W R))))
         (SETQ SC1 (CDR SC1))
         NIL)
        (GO WHILELABEL))
      (RETURN (COND ((EQ W 'BREAK) 'BREAK) (T R))))) 
(PUT 'CL_QSTRYCONS 'NUMBER-OF-ARGS 4) 
(PUT 'CL_QSTRYCONS 'DEFINED-ON-LINE '1064) 
(PUT 'CL_QSTRYCONS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_QSTRYCONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QSTRYCONS (A C1 C2 OP)
    (PROG (NA SC1 R CC1)
      (SETQ CC1 (DELETE A C1))
      (SETQ NA (RL_NEGATEAT A))
      (COND ((NOT (MEMBER NA C2)) (RETURN NIL)))
      (SETQ SC1 CC1)
      (SETQ R T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SC1 R)) (RETURN NIL)))
        (PROGN
         (COND ((MEMBER (RL_NEGATEAT (CAR SC1)) C2) (SETQ R NIL)))
         (SETQ SC1 (CDR SC1))
         NIL)
        (GO WHILELABEL))
      (COND ((NOT R) (RETURN NIL)))
      (SETQ R (SORT (LTO_LIST2SET (APPEND CC1 (DELETE NA C2))) 'RL_ORDATP))
      (COND ((NULL R) (RETURN 'BREAK)))
      (RETURN R))) 
(PUT 'CL_PS 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PS 'DEFINED-ON-LINE '1097) 
(PUT 'CL_PS 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_PS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PS (S)
    (PROG (V W R I N)
      (SETQ I 0)
      (SETQ N 0)
      (SETQ V (CDR S))
      (COND ((EQ (CDR S) 'FINAL) (RETURN 'FINAL)))
      (COND
       ((NULL (CDR S))
        (SETQ V (SETCDR S (MKVECT (DIFFERENCE (LENGTH (CAR S)) 1))))))
      (SETQ N (UPBV V))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (LEQ I N) (SETQ W (GETV V I)))) (RETURN NIL)))
        (PROGN (SETQ R (CONS (CAR W) R)) (SETQ I (PLUS I 1)) NIL)
        (GO WHILELABEL))
      (CL_PSNEXT S)
      (RETURN R))) 
(PUT 'CL_PSNEXT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PSNEXT 'DEFINED-ON-LINE '1114) 
(PUT 'CL_PSNEXT 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_PSNEXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PSNEXT (S) (CL_PSNEXT1 S 0)) 
(PUT 'CL_PSNEXT1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_PSNEXT1 'DEFINED-ON-LINE '1117) 
(PUT 'CL_PSNEXT1 'DEFINED-IN-FILE 'REDLOG/CL/CLBNF.RED) 
(PUT 'CL_PSNEXT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_PSNEXT1 (S N)
    (PROG (W V)
      (SETQ V (CDR S))
      (COND ((GREATERP N (UPBV V)) (RETURN (SETCDR S 'FINAL))))
      (SETQ W (GETV V N))
      (COND ((NULL W) (RETURN (PUTV V N (CAR S)))))
      (SETQ W (CDR W))
      (COND (W (RETURN (PUTV V N W))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ W (CL_PSNEXT1 S (PLUS N 1)))
         (COND ((NEQ W 'FINAL) (SETQ W (CDR (GETV V (PLUS N 1))))))
         NIL)
        (COND ((NOT W) (GO REPEATLABEL))))
      (COND ((EQ W 'FINAL) (RETURN 'FINAL)))
      (PUTV V N W))) 
(ENDMODULE) 
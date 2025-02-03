(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFBQBB)) 
(REVISION 'PASFBQBB "$Id: pasfbqbb.red 6288 2022-04-24 14:13:40Z fjwright $") 
(COPYRIGHT 'PASFBQBB "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'PASF_BQAPPLY2ATS1 'NUMBER-OF-ARGS 3) 
(DE PASF_BQAPPLY2ATS1 (F CLIENT XARGS)
    (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F)
          (CL_APPLY2ATS1 (CADDR F) CLIENT XARGS)
          (CL_APPLY2ATS1 (CADDDR F) CLIENT XARGS))) 
(PUT 'PASF_BQATNUM 'NUMBER-OF-ARGS 1) 
(DE PASF_BQATNUM (F) (PLUS (CL_ATNUM (CADDR F)) (CL_ATNUM (CADDDR F)))) 
(PUT 'PASF_BQQNUM 'NUMBER-OF-ARGS 1) 
(DE PASF_BQQNUM (F) (CL_QNUM (CADDR F))) 
(PUT 'PASF_BQDEPTH 'NUMBER-OF-ARGS 1) 
(DE PASF_BQDEPTH (F) (PLUS 1 (CL_DEPTH (CADDR F)))) 
(PUT 'PASF_BQF2ML 'NUMBER-OF-ARGS 2) 
(DE PASF_BQF2ML (F CLIENT)
    (LTO_ALMERGE (LIST (CL_F2ML (CADDR F) CLIENT) (CL_F2ML (CADDDR F) CLIENT))
                 'PLUS2)) 
(PUT 'PASF_BQSUBFOF1 'NUMBER-OF-ARGS 4) 
(DE PASF_BQSUBFOF1 (AL F ASGAL ALLVL)
    (PROG (OP V M B NEWV)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (SETQ V (CADR F))
      (SETQ M (CADDR F))
      (SETQ B (CADDDR F))
      (SETQ AL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X AL)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (SETQ ASGAL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X ASGAL)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (SETQ NEWV (CL_NEWV V M ASGAL ALLVL))
      (COND
       ((NEQ NEWV V)
        (PROGN
         (SETQ ALLVL (CONS NEWV ALLVL))
         (SETQ M (CL_SUBVARSUBSTAT NEWV V M)))))
      (RETURN
       (LIST OP NEWV (CL_SUBFOF1 AL M ASGAL ALLVL)
             (CL_SUBFOF1 AL B ASGAL ALLVL))))) 
(PUT 'PASF_BQREPLACE1 'NUMBER-OF-ARGS 2) 
(DE PASF_BQREPLACE1 (F SAL)
    (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) (CL_REPLACE (CADDR F) SAL)
          (CL_REPLACE (CADDDR F) SAL))) 
(PUT 'PASF_BQNNF1 'NUMBER-OF-ARGS 2) 
(DE PASF_BQNNF1 (F FLAG)
    (LIST (CL_CFLIP (COND ((ATOM F) F) (T (CAR F))) FLAG) (CADR F)
          (CL_NNF1 (CADDR F) FLAG) (CL_NNF1 (CADDDR F) T))) 
(PUT 'PASF_BQRENAME-VARS1 'NUMBER-OF-ARGS 2) 
(DE PASF_BQRENAME-VARS1 (F VL)
    (PROG (RNF W NVAR RNB)
      (PROG (G157)
        (SETQ G157 (CL_RENAME-VARS1 (CADDR F) VL))
        (SETQ RNF (CAR G157))
        (SETQ VL (CDR G157))
        (RETURN G157))
      (SETQ W (ASSOC (CADR F) (CDR VL)))
      (COND
       (W
        (PROGN
         (COND
          ((EQN (CDR W) 0)
           (PROGN
            (SETCDR W 1)
            (RETURN
             (CONS
              (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) RNF (CADDDR F))
              VL)))))
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ NVAR (MKID (CAR W) (CDR W)))
            (SETCDR W (PLUS (CDR W) 1)))
           (COND
            ((NOT (NOT (OR (MEMQ NVAR (CAR VL)) (GET NVAR 'AVALUE))))
             (GO REPEATLABEL))))
         (PROGN (SETCAR VL (CONS NVAR (CAR VL))) NVAR)
         (SETQ RNB
                 (CL_APPLY2ATS1 (CADDDR F) 'RL_VARSUBSTAT (LIST NVAR (CAR W))))
         (SETQ RNF (CL_APPLY2ATS1 RNF 'RL_VARSUBSTAT (LIST NVAR (CAR W))))
         (RETURN
          (CONS (LIST (COND ((ATOM F) F) (T (CAR F))) NVAR RNF RNB) VL)))))
      (RETURN
       (CONS (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) RNF (CADDDR F))
             VL)))) 
(PUT 'PASF_BQVARL2 'NUMBER-OF-ARGS 4) 
(DE PASF_BQVARL2 (F FVL CBVL BVL)
    (PROGN
     (SETQ CBVL (LTO_INSERTQ (CADR F) CBVL))
     (PROG (G158)
       (SETQ G158 (CL_VARL2 (CADDDR F) FVL CBVL BVL))
       (SETQ FVL (CAR G158))
       (SETQ BVL (CDR G158))
       (RETURN G158))
     (CL_VARL2 (CADDR F) FVL (LTO_INSERTQ (CADR F) CBVL) BVL))) 
(PUT 'PASF_BQQVARL1 'NUMBER-OF-ARGS 1) 
(DE PASF_BQQVARL1 (F) (LTO_INSERTQ (CADR F) (CL_QVARL1 (CADDR F)))) 
(PUT 'PASF_BQORDP 'NUMBER-OF-ARGS 2) 
(DE PASF_BQORDP (F1 F2)
    (COND
     ((NEQ (CADR F1) (CADR F2))
      (NOT (AND (ORDP (CADR F1) (CADR F2)) (NEQ (CADR F1) (CADR F2)))))
     ((NEQ (CADDDR F1) (CADDDR F2)) (CL_ORDP (CADDDR F1) (CADDDR F2)))
     (T (CL_ORDP (CADDR F1) (CADDR F2))))) 
(ENDMODULE) 
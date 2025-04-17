(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFHQE)) 
(REVISION 'OFSFHQE "$Id: ofsfhqe.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFHQE "(c) 2003-2009 A. Dolzmann, L. Gilch, 2016-2017 T. Sturm") 
(PUT 'OFSF_GHQE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GHQE 'DEFINED-ON-LINE '36) 
(PUT 'OFSF_GHQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GHQE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GHQE (F)
    (PROG (RES OFSF_HQETHEO* OFSF_HQEXVARS* *RLHQEGEN)
      (SETQ OFSF_HQETHEO* NIL)
      (SETQ OFSF_HQEXVARS* NIL)
      (SETQ *RLHQEGEN T)
      (SETQ RES (OFSF_HQE F))
      (SETQ *RLHQEGEN NIL)
      (SETQ OFSF_HQEXVARS* NIL)
      (RETURN (CONS (RL_THSIMPL OFSF_HQETHEO*) RES)))) 
(PUT 'OFSF_HQE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_HQE 'DEFINED-ON-LINE '49) 
(PUT 'OFSF_HQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_HQE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_HQE (PHI)
    (PROG (W SVRLHQEVB SVCGBVERBOSE)
      (SETQ SVRLHQEVB *RLHQEVB)
      (SETQ SVCGBVERBOSE *CGBVERBOSE)
      (COND ((NOT *RLVERBOSE) (OFF1 'RLHQEVB)))
      (COND ((NOT (AND *CGBVERBOSE *RLVERBOSE *RLHQEVB)) (OFF1 'CGBVERBOSE)))
      (SETQ W (OFSF_HQE0 PHI))
      (ONOFF 'RLHQEVB SVRLHQEVB)
      (ONOFF 'CGBVERBOSE SVCGBVERBOSE)
      (RETURN W))) 
(PUT 'OFSF_HQE0 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_HQE0 'DEFINED-ON-LINE '66) 
(PUT 'OFSF_HQE0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_HQE0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_HQE0 (PHI)
    (PROG (PHI1 QL FIRSTBLOCK N)
      (SETQ N 0)
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "+++++ simplifying input formula of RRCQE with " (CL_ATNUM PHI)
               " atomic formulas... "))))
      (SETQ PHI1 (CL_SIMPL PHI NIL (MINUS 1)))
      (COND
       (*RLHQEVB
        (IOTO_PRIN2
         (LIST "done. Number of atomic formulas now: " (CL_ATNUM PHI1)))))
      (COND ((MEMBER PHI1 (LIST 'TRUE 'FALSE)) (RETURN PHI1)))
      (COND (*RLHQEVB (IOTO_TPRIN2 "+++++ building prenex normal form... ")))
      (SETQ PHI1 (CL_PNF PHI1))
      (COND (*RLHQEVB (IOTO_PRIN2 "done.")))
      (SETQ QL (CL_SPLT PHI1))
      (SETQ PHI1 (CADR QL))
      (SETQ QL (CAR QL))
      (COND
       ((AND *RLHQEGEN (NULL OFSF_HQEXVARS*))
        (SETQ OFSF_HQEXVARS* (OFSF_GENVAR QL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT (NULL QL)) (NOT FIRSTBLOCK))) (RETURN NIL)))
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN
            (SETQ N (PLUS N 1))
            (IOTO_TPRIN2
             (LIST "+++++ Eliminating " (CAR QL) " ("
                   (PLUS (DIFFERENCE (LENGTH QL) N) 1)
                   (IOTO_CPLU " block" (PLUS (DIFFERENCE (LENGTH QL) N) 1))
                   " left)")))))
         (SETQ PHI1 (OFSF_RRCNFBLOCKQE (CAR QL) PHI1))
         (COND ((MEMBER PHI1 (LIST 'TRUE 'FALSE)) (SETQ QL NIL))
               (T
                (PROGN
                 (COND (*RLHQEDIM0 (SETQ FIRSTBLOCK T)))
                 (SETQ QL (CDR QL))))))
        (GO WHILELABEL))
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "+++++ leaving RRCQE: " (CL_ATNUM PHI1) " atomic formulas"))))
      (RETURN (OFSF_RQREQUANTIFY QL PHI1)))) 
(PUT 'OFSF_RQREQUANTIFY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RQREQUANTIFY 'DEFINED-ON-LINE '109) 
(PUT 'OFSF_RQREQUANTIFY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RQREQUANTIFY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RQREQUANTIFY (QL PHI)
    (COND ((OR (NOT *RLHQEDIM0) (NULL QL)) PHI)
          (T (OFSF_RQREQUANTIFY2 QL PHI)))) 
(PUT 'OFSF_RQREQUANTIFY2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RQREQUANTIFY2 'DEFINED-ON-LINE '119) 
(PUT 'OFSF_RQREQUANTIFY2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RQREQUANTIFY2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RQREQUANTIFY2 (QL PHI)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL QL))) (RETURN NIL)))
        (PROGN
         (SETQ PHI (OFSF_RQREQUANTIFY3 (CAAR QL) (CDAR QL) PHI))
         (SETQ QL (CDR QL)))
        (GO WHILELABEL))
      (RETURN PHI))) 
(PUT 'OFSF_RQREQUANTIFY3 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_RQREQUANTIFY3 'DEFINED-ON-LINE '132) 
(PUT 'OFSF_RQREQUANTIFY3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RQREQUANTIFY3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_RQREQUANTIFY3 (Q VARL PHI)
    (COND ((NULL VARL) PHI)
          (T (OFSF_RQREQUANTIFY3 Q (CDR VARL) (LIST Q (CAR VARL) PHI))))) 
(PUT 'OFSF_RRCNFBLOCKQE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RRCNFBLOCKQE 'DEFINED-ON-LINE '141) 
(PUT 'OFSF_RRCNFBLOCKQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RRCNFBLOCKQE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RRCNFBLOCKQE (QL PHI)
    (PROG (PSI GAMMA I)
      (SETQ I 0)
      (COND
       (*RLHQEVB (IOTO_TPRIN2 "+++++ building positive normal form ... ")))
      (COND ((EQ (CAR QL) 'ALL) (SETQ PSI (CL_NNFNOT PHI)))
            (T (SETQ PSI (CL_NNF PHI))))
      (COND
       (*RLHQEVB
        (PROGN
         (IOTO_PRIN2 (LIST "done, now " (CL_ATNUM PSI) " atomic formulas"))
         (IOTO_TPRIN2 "+++++ building RRC normal forms ..."))))
      (SETQ GAMMA (OFSF_CONNECT (OFSF_RRCNF PSI (CDR QL))))
      (COND ((MEMBER GAMMA (LIST 'TRUE 'FALSE)) (SETQ PSI GAMMA))
            (T
             (PROGN
              (COND (*RLHQEVB (IOTO_PRIN2 (LIST "done."))))
              (SETQ PSI 'FALSE)
              (PROG (ELEM)
                (SETQ ELEM GAMMA)
               LAB
                (COND ((NULL ELEM) (RETURN NIL)))
                ((LAMBDA (ELEM)
                   (PROGN
                    (COND
                     (*RLVERBOSE
                      (PROGN
                       (SETQ I (PLUS I 1))
                       (IOTO_TPRIN2
                        (LIST "++++ " (PLUS (DIFFERENCE (LENGTH GAMMA) I) 1)
                              (IOTO_CPLU " conjunction"
                                         (NEQ
                                          (PLUS (DIFFERENCE (LENGTH GAMMA) I)
                                                1)
                                          1))
                              " left")))))
                    (SETQ PSI
                            (OFSF_RQSIMPL
                             ((LAMBDA (G378)
                                (COND ((AND G378 (CDR G378)) (CONS 'OR G378))
                                      ((NULL G378)
                                       (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                      (T (CAR G378))))
                              (LIST PSI (OFSF_RRCNFELIMINATE ELEM (CDR QL))))))
                    NIL))
                 (CAR ELEM))
                (SETQ ELEM (CDR ELEM))
                (GO LAB)))))
      (COND ((EQ (CAR QL) 'ALL) (SETQ PSI (CL_NNFNOT PSI))))
      (RETURN PSI))) 
(PUT 'OFSF_RRCNFELIMINATE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RRCNFELIMINATE 'DEFINED-ON-LINE '183) 
(PUT 'OFSF_RRCNFELIMINATE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RRCNFELIMINATE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RRCNFELIMINATE (RRCNF VARL)
    (COND
     ((AND (NULL (CAR RRCNF)) (NULL (CADR RRCNF)) (NULL (CADDR RRCNF)))
      (OFSF_RQSIMPL (OFSF_SMKN 'AND (CADDDR RRCNF))))
     (*RLHQECONNECT
      (COND
       ((OFSF_CONJP (CADDDR RRCNF))
        (OFSF_RQSIMPL
         ((LAMBDA (G380)
            (COND ((AND G380 (CDR G380)) (CONS 'AND G380))
                  ((NULL G380) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G380))))
          (LIST (CADDDR RRCNF)
                (OFSF_QENF (CADDDR RRCNF) (CAR RRCNF) (CADR RRCNF)
                 (CADDR RRCNF) VARL)))))
       (T
        (OFSF_RQSIMPL
         ((LAMBDA (G382)
            (COND ((AND G382 (CDR G382)) (CONS 'AND G382))
                  ((NULL G382) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G382))))
          (LIST (CADDDR RRCNF)
                (OFSF_QENF 'TRUE (CAR RRCNF) (CADR RRCNF) (CADDR RRCNF)
                 VARL)))))))
     (T
      (OFSF_RQSIMPL
       ((LAMBDA (G388)
          (COND ((AND G388 (CDR G388)) (CONS 'AND G388))
                ((NULL G388) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G388))))
        (LIST
         ((LAMBDA (G384)
            (COND ((AND G384 (CDR G384)) (CONS 'AND G384))
                  ((NULL G384) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G384))))
          (CADDDR RRCNF))
         (OFSF_QENF
          ((LAMBDA (G386)
             (COND ((AND G386 (CDR G386)) (CONS 'AND G386))
                   ((NULL G386) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                   (T (CAR G386))))
           (CADDDR RRCNF))
          (CAR RRCNF) (CADR RRCNF) (CADDR RRCNF) VARL))))))) 
(PUT 'OFSF_CONNECT 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_CONNECT 'DEFINED-ON-LINE '200) 
(PUT 'OFSF_CONNECT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CONNECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_CONNECT (RRCNFL)
    (PROG (SAMEPHI NEWLIST L)
      (COND
       ((OR (MEMBER RRCNFL (LIST 'TRUE 'FALSE)) (NOT *RLHQECONNECT))
        (RETURN RRCNFL)))
      (SETQ L
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM RRCNFL)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (LIST (SORT (CAR ELEM) 'ORDP)
                                          (SORT (CADR ELEM) 'ORDP)
                                          (SORT (CADDR ELEM) 'ORDP)
                                          ((LAMBDA (G390)
                                             (COND
                                              ((AND G390 (CDR G390))
                                               (CONS 'AND G390))
                                              ((NULL G390)
                                               (COND ((EQ 'AND 'AND) 'TRUE)
                                                     (T 'FALSE)))
                                              (T (CAR G390))))
                                           (CADDDR ELEM))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM)
                            (LIST (SORT (CAR ELEM) 'ORDP)
                                  (SORT (CADR ELEM) 'ORDP)
                                  (SORT (CADDR ELEM) 'ORDP)
                                  ((LAMBDA (G390)
                                     (COND
                                      ((AND G390 (CDR G390)) (CONS 'AND G390))
                                      ((NULL G390)
                                       (COND ((EQ 'AND 'AND) 'TRUE)
                                             (T 'FALSE)))
                                      (T (CAR G390))))
                                   (CADDDR ELEM))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL L))) (RETURN NIL)))
        (PROGN
         (SETQ SAMEPHI (OFSF_SAMEPHI (CAR L) (CDR L)))
         (SETQ NEWLIST (CONS (CAR SAMEPHI) NEWLIST))
         (SETQ L (CADR SAMEPHI)))
        (GO WHILELABEL))
      (RETURN NEWLIST))) 
(PUT 'OFSF_SAMEPHI 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SAMEPHI 'DEFINED-ON-LINE '217) 
(PUT 'OFSF_SAMEPHI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SAMEPHI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SAMEPHI (RRCNF RRCNFL)
    (PROG (RRCNF1 RRCNFL1)
      (SETQ RRCNF1 RRCNF)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL RRCNFL))) (RETURN NIL)))
        (PROGN
         (COND
          ((OFSF_SAMEPHIP RRCNF (CAR RRCNFL))
           (SETQ RRCNF1 (OFSF_CONNECTRRCNF RRCNF (CAR RRCNFL))))
          (T (SETQ RRCNFL1 (CONS (CAR RRCNFL) RRCNFL1))))
         (SETQ RRCNFL (CDR RRCNFL)))
        (GO WHILELABEL))
      (RETURN (LIST RRCNF1 RRCNFL1)))) 
(PUT 'OFSF_SAMEPHIP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SAMEPHIP 'DEFINED-ON-LINE '234) 
(PUT 'OFSF_SAMEPHIP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SAMEPHIP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SAMEPHIP (R1 R2)
    (AND (EQUAL (CAR R1) (CAR R2)) (EQUAL (CADR R1) (CADR R2))
         (EQUAL (CADDR R1) (CADDR R2)))) 
(PUT 'OFSF_CONNECTRRCNF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CONNECTRRCNF 'DEFINED-ON-LINE '239) 
(PUT 'OFSF_CONNECTRRCNF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CONNECTRRCNF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CONNECTRRCNF (R1 R2)
    (COND
     ((OR (EQUAL (CADDDR R1) 'TRUE) (EQUAL (CADDDR R2) 'TRUE))
      (LIST (CAR R1) (CADR R1) (CADDR R1) 'TRUE))
     (T
      (LIST (CAR R1) (CADR R1) (CADDR R1)
            ((LAMBDA (G392)
               (COND ((AND G392 (CDR G392)) (CONS 'OR G392))
                     ((NULL G392) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR G392))))
             (LIST (CADDDR R1) (CADDDR R2))))))) 
(PUT 'OFSF_SMKN 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SMKN 'DEFINED-ON-LINE '248) 
(PUT 'OFSF_SMKN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SMKN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMKN (OP PHI)
    (COND
     ((NOT *RLHQECONNECT)
      (COND ((AND PHI (CDR PHI)) (CONS OP PHI))
            ((NULL PHI) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE))) (T (CAR PHI))))
     (T PHI))) 
(PUT 'OFSF_CONJP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_CONJP 'DEFINED-ON-LINE '257) 
(PUT 'OFSF_CONJP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CONJP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_CONJP (F)
    (COND ((AND (LISTP F) (EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)) NIL) (T T))) 
(PUT 'OFSF_GENVAR 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GENVAR 'DEFINED-ON-LINE '265) 
(PUT 'OFSF_GENVAR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GENVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GENVAR (L)
    (COND ((NULL L) NIL) (T (APPEND (CDAR L) (OFSF_GENVAR (CDR L)))))) 
(PUT 'OFSF_RRCNF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RRCNF 'DEFINED-ON-LINE '278) 
(PUT 'OFSF_RRCNF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RRCNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RRCNF (M VL)
    (PROG (W)
      (COND
       (*RLHQEVB
        (LIST "++++ computing DNF of formula with " (CL_ATNUM M)
              " atomic formulas")))
      (SETQ W (CL_DNF M))
      (COND
       (*RLHQEVB
        (LIST "++++ finished DNF Computation: " (CL_ATNUM W)
              " atomic formulas")))
      (COND ((OR (EQ W 'TRUE) (EQ W 'FALSE)) (RETURN W)))
      (SETQ W
              (COND ((EQ (COND ((ATOM W) W) (T (CAR W))) 'OR) (CDR W))
                    (T (LIST W))))
      (RETURN
       (PROG (BR FORALL-RESULT FORALL-ENDPTR)
         (SETQ BR W)
        STARTOVER
         (COND ((NULL BR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (BR)
                    (COND
                     ((EQ (COND ((ATOM BR) BR) (T (CAR BR))) 'AND)
                      (OFSF_RRCNF1 (CDR BR) VL))
                     (T (OFSF_RRCNF1 (LIST BR) VL))))
                  (CAR BR)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ BR (CDR BR))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL BR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (BR)
                    (COND
                     ((EQ (COND ((ATOM BR) BR) (T (CAR BR))) 'AND)
                      (OFSF_RRCNF1 (CDR BR) VL))
                     (T (OFSF_RRCNF1 (LIST BR) VL))))
                  (CAR BR)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ BR (CDR BR))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_NEW 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_NEW 'DEFINED-ON-LINE '312) 
(PUT 'OFSF_NEW 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_NEW 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OFSF_NEW NIL (LIST (LIST NIL NIL NIL NIL))) 
(PUT 'OFSF_ADDEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDEQUAL 'DEFINED-ON-LINE '315) 
(PUT 'OFSF_ADDEQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDEQUAL (RRCNF LHS)
    (PROGN
     (PROG (V)
       (SETQ V RRCNF)
      LAB
       (COND ((NULL V) (RETURN NIL)))
       ((LAMBDA (V) (SETCAR V (CONS LHS (CAR V)))) (CAR V))
       (SETQ V (CDR V))
       (GO LAB))
     RRCNF)) 
(PUT 'OFSF_ADDGREATERP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDGREATERP 'DEFINED-ON-LINE '322) 
(PUT 'OFSF_ADDGREATERP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDGREATERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDGREATERP (RRCNF LHS)
    (PROGN
     (PROG (V)
       (SETQ V RRCNF)
      LAB
       (COND ((NULL V) (RETURN NIL)))
       ((LAMBDA (V) (SETCAR (CDR V) (CONS LHS (CADR V)))) (CAR V))
       (SETQ V (CDR V))
       (GO LAB))
     RRCNF)) 
(PUT 'OFSF_ADDLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDLESSP 'DEFINED-ON-LINE '329) 
(PUT 'OFSF_ADDLESSP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDLESSP (RRCNF LHS)
    (PROG (NEGLHS)
      (SETQ NEGLHS (NEGF LHS))
      (PROG (V)
        (SETQ V RRCNF)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETCAR (CDR V) (CONS NEGLHS (CADR V)))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN RRCNF))) 
(PUT 'OFSF_ADDNEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDNEQ 'DEFINED-ON-LINE '337) 
(PUT 'OFSF_ADDNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDNEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDNEQ (RRCNF LHS)
    (PROGN
     (PROG (V)
       (SETQ V RRCNF)
      LAB
       (COND ((NULL V) (RETURN NIL)))
       ((LAMBDA (V) (SETCAR (CDDR V) (CONS LHS (CADDR V)))) (CAR V))
       (SETQ V (CDR V))
       (GO LAB))
     RRCNF)) 
(PUT 'OFSF_ADDGEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDGEQ 'DEFINED-ON-LINE '344) 
(PUT 'OFSF_ADDGEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDGEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDGEQ (RRCNF LHS)
    (PROG (V FORALL-RESULT FORALL-ENDPTR)
      (SETQ V RRCNF)
     STARTOVER
      (COND ((NULL V) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (V)
                 (LIST (LIST (CONS LHS (CAR V)) (CADR V) (CADDR V) (CADDDR V))
                       (LIST (CAR V) (CONS LHS (CADR V)) (CADDR V)
                             (CADDDR V))))
               (CAR V)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ V (CDR V))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL V) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (V)
                 (LIST (LIST (CONS LHS (CAR V)) (CADR V) (CADDR V) (CADDDR V))
                       (LIST (CAR V) (CONS LHS (CADR V)) (CADDR V)
                             (CADDDR V))))
               (CAR V)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ V (CDR V))
      (GO LOOPLABEL))) 
(PUT 'OFSF_ADDLEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDLEQ 'DEFINED-ON-LINE '349) 
(PUT 'OFSF_ADDLEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDLEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDLEQ (RRCNF LHS)
    (PROG (NEGLHS)
      (SETQ NEGLHS (NEGF LHS))
      (RETURN
       (PROG (V FORALL-RESULT FORALL-ENDPTR)
         (SETQ V RRCNF)
        STARTOVER
         (COND ((NULL V) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (V)
                    (LIST
                     (LIST (CONS LHS (CAR V)) (CADR V) (CADDR V) (CADDDR V))
                     (LIST (CAR V) (CONS NEGLHS (CADR V)) (CADDR V)
                           (CADDDR V))))
                  (CAR V)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ V (CDR V))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL V) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (V)
                    (LIST
                     (LIST (CONS LHS (CAR V)) (CADR V) (CADDR V) (CADDDR V))
                     (LIST (CAR V) (CONS NEGLHS (CADR V)) (CADDR V)
                           (CADDDR V))))
                  (CAR V)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ V (CDR V))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_ADDTHEO 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ADDTHEO 'DEFINED-ON-LINE '357) 
(PUT 'OFSF_ADDTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ADDTHEO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ADDTHEO (RRCNF AT)
    (PROGN
     (PROG (V)
       (SETQ V RRCNF)
      LAB
       (COND ((NULL V) (RETURN NIL)))
       ((LAMBDA (V) (SETCAR (CDDDR V) (CONS AT (CADDDR V)))) (CAR V))
       (SETQ V (CDR V))
       (GO LAB))
     RRCNF)) 
(PUT 'OFSF_RRCNF1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_RRCNF1 'DEFINED-ON-LINE '364) 
(PUT 'OFSF_RRCNF1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RRCNF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_RRCNF1 (ATL VL)
    (PROG (RESL OP LHS)
      (SETQ RESL (OFSF_NEW))
      (PROG (AT)
        (SETQ AT ATL)
       LAB
        (COND ((NULL AT) (RETURN NIL)))
        ((LAMBDA (AT)
           (PROGN
            (SETQ OP (COND ((ATOM AT) AT) (T (CAR AT))))
            (SETQ LHS (CADR AT))
            (SETQ RESL
                    (COND
                     ((NULL (INTERSECTION (KERNELS LHS) VL))
                      (OFSF_ADDTHEO RESL AT))
                     ((EQ OP 'EQUAL) (OFSF_ADDEQUAL RESL LHS))
                     ((EQ OP 'GREATERP) (OFSF_ADDGREATERP RESL LHS))
                     ((EQ OP 'LESSP) (OFSF_ADDLESSP RESL LHS))
                     ((EQ OP 'NEQ) (OFSF_ADDNEQ RESL LHS))
                     ((EQ OP 'GEQ) (OFSF_ADDGEQ RESL LHS))
                     ((EQ OP 'LEQ) (OFSF_ADDLEQ RESL LHS))))))
         (CAR AT))
        (SETQ AT (CDR AT))
        (GO LAB))
      (RETURN RESL))) 
(PUT 'OFSF_QENF 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QENF 'DEFINED-ON-LINE '409) 
(PUT 'OFSF_QENF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_QENF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QENF (XI FLIST GLIST HLIST VARL)
    (PROG ()
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "+++ entering QENF: theo:" (LENGTH XI) " r:" (LENGTH FLIST)
               " s:" (LENGTH GLIST) " t:" (LENGTH HLIST)))))
      (COND
       ((EQUAL XI 'FALSE)
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "+++ leaving QENF (0)"))))
         (RETURN 'FALSE)))
       ((AND (NULL FLIST) (NULL GLIST) (NULL HLIST))
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "+++ leaving QENF (0)"))))
         (RETURN 'TRUE)))
       ((NULL FLIST) (RETURN (OFSF_CASER0 XI VARL GLIST HLIST)))
       (T (RETURN (OFSF_ELIMINATEGSYS XI FLIST GLIST HLIST VARL)))))) 
(PUT 'OFSF_ELIMINATEGSYS 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_ELIMINATEGSYS 'DEFINED-ON-LINE '433) 
(PUT 'OFSF_ELIMINATEGSYS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ELIMINATEGSYS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMINATEGSYS (XI FLIST GLIST HLIST VARL)
    (PROG (S ITA GB DIM PSI I)
      (SETQ I 0)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "++ computing green Groebner system ... "))))
      (SETQ S (OFSF_GGSYS FLIST VARL XI))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "done"))))
      (SETQ PSI 'FALSE)
      (PROG (BRANCH)
        (SETQ BRANCH S)
       LAB
        (COND ((NULL BRANCH) (RETURN NIL)))
        ((LAMBDA (BRANCH)
           (PROGN
            (COND
             (*RLVERBOSE
              (PROGN
               (SETQ I (PLUS I 1))
               (IOTO_TPRIN2
                (LIST "++ " (PLUS (DIFFERENCE (LENGTH S) I) 1)
                      " branch(es) left")))))
            (SETQ ITA (OFSF_MKCONJ (CAR BRANCH)))
            (SETQ GB (CADR BRANCH))
            (COND
             (*RLHQEVB
              (IOTO_TPRIN2 (LIST "++ computing dimension of branch ... "))))
            (SETQ DIM (OFSF_DIM GB VARL))
            (COND (*RLHQEVB (IOTO_TPRIN2 "done")))
            (SETQ PSI
                    (OFSF_OR PSI
                     (OFSF_ELIMINATEDIM GB GLIST HLIST VARL DIM ITA XI)))))
         (CAR BRANCH))
        (SETQ BRANCH (CDR BRANCH))
        (GO LAB))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ leaving QENF (" (CL_ATNUM PSI) ")"))))
      (RETURN PSI))) 
(PUT 'OFSF_ELIMINATEDIM 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_ELIMINATEDIM 'DEFINED-ON-LINE '464) 
(PUT 'OFSF_ELIMINATEDIM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ELIMINATEDIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_ELIMINATEDIM (GB GLIST HLIST VARL DIM ITA XI)
    (COND ((EQUAL (CAR DIM) (MINUS 1)) 'FALSE)
          ((EQUAL (CAR DIM) 0)
           (OFSF_AND ITA (OFSF_D0MAIN GB VARL GLIST HLIST)))
          ((EQUAL (CAR DIM) (LENGTH VARL))
           (OFSF_AND ITA (OFSF_CASEDIMN ITA XI VARL GLIST HLIST)))
          (T
           (OFSF_AND ITA
            (OFSF_CASEDIM ITA XI (OFSF_REMVARL VARL (CADR DIM)) (CADR DIM) GB
             GLIST HLIST))))) 
(PUT 'OFSF_CASER0 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_CASER0 'DEFINED-ON-LINE '481) 
(PUT 'OFSF_CASER0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CASER0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CASER0 (XI VARL GLIST HLIST)
    (PROG (XN REMVARL PSI PHI12 PHI3 PHI4 NEQLIST)
      (COND
       (*RLHQEVARSEL
        (PROGN
         (SETQ XN (OFSF_SELECTXN VARL GLIST))
         (SETQ REMVARL (SETDIFF VARL (LIST XN)))))
       (T (PROGN (SETQ XN (CAR VARL)) (SETQ REMVARL (CDR VARL)))))
      (COND
       (*RLHQEVB (IOTO_TPRIN2 "++ transforming Matrix in case #f = 0 ...")))
      (SETQ PSI (OFSF_TRANSFORMMATRIX XN GLIST HLIST))
      (COND (*RLHQEVB (IOTO_TPRIN2 " done")))
      (SETQ NEQLIST (CAR PSI))
      (SETQ PHI12 (CADR PSI))
      (SETQ PHI3 (CADDR PSI))
      (SETQ PHI4 (CADDDR PSI))
      (SETQ PSI (OFSF_AND NEQLIST PHI12))
      (COND
       ((EQUAL PSI 'TRUE)
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "+++ leaving QENF [r=0] (0)"))))
         (RETURN PSI))))
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "++ Eliminating phi3: " (LENGTH PHI3) " subformulas"))))
      (SETQ PSI (OFSF_OR PSI (OFSF_ELIMINATEPHI34 XI PHI3 XN GLIST NEQLIST)))
      (COND
       (*RLHQEVB
        (PROGN
         (IOTO_TPRIN2 (LIST "++ phi3 eliminated."))
         (IOTO_TPRIN2
          (LIST "++ eliminating phi4: " (LENGTH PHI4) " subformulas")))))
      (SETQ PSI (OFSF_OR PSI (OFSF_ELIMINATEPHI34 XI PHI4 XN GLIST NEQLIST)))
      (COND (*RLHQEVB (IOTO_TPRIN2 (LIST "++ phi4 eliminated."))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ leaving QENF [r=0] (" (CL_ATNUM PSI) ")"))))
      (COND ((OR (EQUAL PSI 'TRUE) (EQUAL PSI 'FALSE)) (RETURN PSI))
            ((NOT (NULL REMVARL))
             (COND (*RLHQEDIM0 (RETURN (OFSF_MKNEWF2 REMVARL PSI)))
                   (T
                    (RETURN
                     (OFSF_HQE0 (OFSF_AND XI (OFSF_MKNEWF2 REMVARL PSI)))))))
            (T (RETURN PSI))))) 
(PUT 'OFSF_ELIMINATEPHI34 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_ELIMINATEPHI34 'DEFINED-ON-LINE '536) 
(PUT 'OFSF_ELIMINATEPHI34 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_ELIMINATEPHI34 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMINATEPHI34 (XI PHI34 XN GLIST NEQLIST)
    (PROG (FOUNDTRUE CONDL C PHI)
      (SETQ PHI 'FALSE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT (NULL PHI34)) (NOT FOUNDTRUE))) (RETURN NIL)))
        (PROGN
         (SETQ CONDL (CAAR PHI34))
         (COND
          (*RLHQEVB
           (IOTO_TPRIN2
            (LIST "+ Eliminating subformula of phi3/phi4, " (LENGTH CONDL)
                  " cases..."))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (NOT (NULL CONDL)) (NOT FOUNDTRUE))) (RETURN NIL)))
           (PROGN
            (COND (*RLHQEVB (IOTO_TPRIN2 "checking consistence ... ")))
            (SETQ C (OFSF_CONSISTENT XI (CAR CONDL)))
            (COND (*RLHQEVB (IOTO_PRIN2 "done.")))
            (COND
             ((CAR C)
              (SETQ PHI
                      (OFSF_OR PHI
                       (OFSF_AND NEQLIST
                        (OFSF_AND (CADR C)
                         (OFSF_QENFCASE0 (CAR CONDL) XN GLIST
                          (CADAR PHI34))))))))
            (COND ((EQUAL PHI 'TRUE) (SETQ FOUNDTRUE T)))
            (SETQ CONDL (CDR CONDL)))
           (GO WHILELABEL))
         (SETQ PHI34 (CDR PHI34)))
        (GO WHILELABEL))
      (RETURN PHI))) 
(PUT 'OFSF_SELECTXN 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SELECTXN 'DEFINED-ON-LINE '567) 
(PUT 'OFSF_SELECTXN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SELECTXN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SELECTXN (VARL GLIST)
    (PROG (RES)
      (COND (*RLHQEVB (IOTO_PRIN2 "selecting Xn in case r=0 ...")))
      (COND (*RLHQEVARSELX (SETQ RES (OFSF_SELECTXN2 VARL GLIST)))
            (T (SETQ RES (OFSF_SELECTXN1 VARL GLIST))))
      (COND (*RLHQEVB (IOTO_PRIN2 " done.")))
      (RETURN RES))) 
(PUT 'OFSF_SELECTXN1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SELECTXN1 'DEFINED-ON-LINE '583) 
(PUT 'OFSF_SELECTXN1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SELECTXN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SELECTXN1 (VARL GLIST)
    (PROG (VL2 D OLDORDER D2 DU ELEM1 D1)
      (SETQ D1 0)
      (COND ((OR (NULL GLIST) (NULL (CDR VARL))) (RETURN (CAR VARL))))
      (SETQ VL2 VARL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL VL2))) (RETURN NIL)))
        (PROGN
         (SETQ OLDORDER (SETKORDER (LIST (CAR VL2))))
         (SETQ D1 0)
         (SETQ DU NIL)
         (PROG (ELEM)
           (SETQ ELEM GLIST)
          LAB
           (COND ((NULL ELEM) (RETURN NIL)))
           ((LAMBDA (ELEM)
              (PROGN
               (SETQ ELEM1 (REORDER ELEM))
               (COND
                ((OR (OR (ATOM ELEM1) (ATOM (CAR ELEM1)))
                     (NEQ (CAAAR ELEM1) (CAR VL2)))
                 (SETQ D2 0))
                (T (SETQ D2 (CDAAR ELEM1))))
               (COND ((GREATERP D2 D1) (SETQ D1 D2)))
               (COND ((OR (NULL DU) (LESSP D2 DU)) (SETQ DU D2)))
               NIL))
            (CAR ELEM))
           (SETQ ELEM (CDR ELEM))
           (GO LAB))
         (COND
          ((OR (NULL D) (LEQ D1 (CAR D)))
           (PROGN (SETQ D (LIST DU D1 (CAR VL2))) (SETQ VL2 (CDR VL2))))
          (T (PROGN (SETQ VL2 NIL) (SETQ D NIL))))
         (SETKORDER OLDORDER))
        (GO WHILELABEL))
      (COND ((NOT (NULL D)) (RETURN (CADDR D))) (T (RETURN (CAR VARL)))))) 
(PUT 'OFSF_SELECTXN2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SELECTXN2 'DEFINED-ON-LINE '621) 
(PUT 'OFSF_SELECTXN2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SELECTXN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SELECTXN2 (VARL GLIST)
    (PROG (VL2 L XL OLDORDER ELEM1 DL)
      (COND ((OR (NULL GLIST) (NULL (CDR VARL))) (RETURN (CAR VARL))))
      (SETQ VL2 VARL)
      (SETQ DL (OFSF_DIFFERENCE GLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL VL2))) (RETURN NIL)))
        (PROGN
         (SETQ OLDORDER (SETKORDER (LIST (CAR VL2))))
         (SETQ XL (OFSF_GETHEXPONENT (CAR VL2) GLIST))
         (PROG (ELEM)
           (SETQ ELEM DL)
          LAB
           (COND ((NULL ELEM) (RETURN NIL)))
           ((LAMBDA (ELEM)
              (PROGN
               (SETQ ELEM1 (REORDER ELEM))
               (COND
                ((OR (OR (ATOM ELEM1) (ATOM (CAR ELEM1)))
                     (NEQ (CAAAR ELEM1) (CAR VL2)))
                 (SETQ XL (CONS 0 XL)))
                (T (SETQ XL (CONS (CDAAR ELEM1) XL))))))
            (CAR ELEM))
           (SETQ ELEM (CDR ELEM))
           (GO LAB))
         (SETQ L (CONS (CONS (CAR VL2) (SORT XL 'GEQ)) L))
         (SETKORDER OLDORDER)
         (SETQ VL2 (CDR VL2)))
        (GO WHILELABEL))
      (RETURN (OFSF_GETMINVAR L)))) 
(PUT 'OFSF_DIFFERENCE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_DIFFERENCE 'DEFINED-ON-LINE '646) 
(PUT 'OFSF_DIFFERENCE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_DIFFERENCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_DIFFERENCE (L)
    (PROG (RES)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL (CDR L)))) (RETURN NIL)))
        (PROGN
         (PROG (ELEM)
           (SETQ ELEM (CDR L))
          LAB
           (COND ((NULL ELEM) (RETURN NIL)))
           ((LAMBDA (ELEM)
              (SETQ RES (LTO_INSERT (ADDF (CAR L) (NEGF ELEM)) RES)))
            (CAR ELEM))
           (SETQ ELEM (CDR ELEM))
           (GO LAB))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'OFSF_GETHEXPONENT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETHEXPONENT 'DEFINED-ON-LINE '658) 
(PUT 'OFSF_GETHEXPONENT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETHEXPONENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETHEXPONENT (X L)
    (PROG (XL ELEM1)
      (PROG (ELEM)
        (SETQ ELEM L)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (PROGN
            (SETQ ELEM1 (REORDER ELEM))
            (COND
             ((OR (OR (ATOM ELEM1) (ATOM (CAR ELEM1))) (NEQ (CAAAR ELEM1) X))
              (SETQ XL (CONS 0 XL)))
             (T (SETQ XL (CONS (DIFFERENCE (CDAAR ELEM1) 1) XL))))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (RETURN XL))) 
(PUT 'OFSF_GETMINVAR 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GETMINVAR 'DEFINED-ON-LINE '672) 
(PUT 'OFSF_GETMINVAR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETMINVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GETMINVAR (L)
    (PROG (RES M)
      (COND ((NULL (CDAR L)) (RETURN (CAAR L))))
      (PROG (ELEM)
        (SETQ ELEM L)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (PROGN
            (COND
             ((OR (NULL RES) (LESSP (CADR ELEM) M))
              (PROGN
               (SETQ RES (LIST (CONS (CAR ELEM) (CDDR ELEM))))
               (SETQ M (CADR ELEM))))
             ((EQUAL (CADR ELEM) M)
              (SETQ RES (CONS (CONS (CAR ELEM) (CDDR ELEM)) RES))))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (COND ((EQUAL (LENGTH RES) 1) (RETURN (CAAR RES)))
            (T (RETURN (OFSF_GETMINVAR RES)))))) 
(PUT 'OFSF_CONSISTENT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CONSISTENT 'DEFINED-ON-LINE '692) 
(PUT 'OFSF_CONSISTENT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CONSISTENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CONSISTENT (XI COND)
    (PROG (XI2 COND2)
      (COND ((EQ XI 'TRUE) (RETURN (LIST T COND)))
            ((EQ COND 'TRUE) (RETURN (LIST T 'TRUE))))
      (SETQ XI2
              (COND ((NEQ (COND ((ATOM XI) XI) (T (CAR XI))) 'AND) (LIST XI))
                    (T (CDR XI))))
      (SETQ COND2
              (COND
               ((NEQ (COND ((ATOM COND) COND) (T (CAR COND))) 'AND)
                (LIST COND))
               (T (CDR COND))))
      (RETURN (OFSF_CONSISTENT1 (LTO_LIST2SET XI2) COND2)))) 
(PUT 'OFSF_CONSISTENT1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CONSISTENT1 'DEFINED-ON-LINE '712) 
(PUT 'OFSF_CONSISTENT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CONSISTENT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CONSISTENT1 (XI COND)
    (PROG (FOUND XI1 COND1)
      (SETQ XI1 XI)
      (SETQ COND1 COND)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT FOUND) (NOT (NULL XI1)))) (RETURN NIL)))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (NULL COND1))) (RETURN NIL)))
           (PROGN
            (COND
             ((AND
               (EQ (COND ((ATOM (CAR XI1)) (CAR XI1)) (T (CAR (CAR XI1))))
                   'EQUAL)
               (EQ
                (COND ((ATOM (CAR COND1)) (CAR COND1)) (T (CAR (CAR COND1))))
                'NEQ)
               ((LAMBDA (*EXP) (QUOTF1 (CADR (CAR COND1)) (CADR (CAR XI1))))
                T))
              (PROGN (SETQ COND1 NIL) (SETQ FOUND T)))
             ((AND
               (EQ
                (COND ((ATOM (CAR COND1)) (CAR COND1)) (T (CAR (CAR COND1))))
                'EQUAL)
               (MEMBER (COND ((ATOM (CAR XI1)) (CAR XI1)) (T (CAR (CAR XI1))))
                       (LIST 'NEQ 'GREATERP 'LESSP))
               ((LAMBDA (*EXP) (QUOTF1 (CADR (CAR XI1)) (CADR (CAR COND1))))
                T))
              (PROGN (SETQ COND1 NIL) (SETQ FOUND T)))
             (T (SETQ COND1 (CDR COND1)))))
           (GO WHILELABEL))
         (SETQ COND1 COND)
         (SETQ XI1 (CDR XI1)))
        (GO WHILELABEL))
      (COND (FOUND (RETURN (LIST (NOT FOUND))))
            (T
             (RETURN
              (LIST (NOT FOUND)
                    ((LAMBDA (G394)
                       (COND ((AND G394 (CDR G394)) (CONS 'AND G394))
                             ((NULL G394)
                              (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                             (T (CAR G394))))
                     (SETDIFF COND XI)))))))) 
(PUT 'OFSF_TRANSFORMMATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TRANSFORMMATRIX 'DEFINED-ON-LINE '744) 
(PUT 'OFSF_TRANSFORMMATRIX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRANSFORMMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSFORMMATRIX (XN GL HL)
    (PROG (NEQLIST GLIST PHI1 PHI2 PHI3 PHI4 PHI3PHI4 PHI1ORPHI2)
      (SETQ NEQLIST HL)
      (SETQ GLIST GL)
      (COND
       ((NOT (NULL GLIST))
        (PROGN
         (SETQ PHI3PHI4 (OFSF_GETPHI3PHI4 XN GLIST))
         (SETQ PHI3 (CAR PHI3PHI4))
         (SETQ PHI4 (CADR PHI3PHI4))
         (SETQ PHI1 (OFSF_BUILDPHI1 XN GLIST))
         (SETQ PHI2 (OFSF_BUILDPHI2 XN GLIST)))))
      (SETQ NEQLIST (OFSF_BUILDHKNEQ0 XN NEQLIST))
      (COND ((NULL PHI1) (SETQ PHI1ORPHI2 'TRUE))
            (T (SETQ PHI1ORPHI2 (OFSF_OR PHI1 PHI2))))
      (RETURN (LIST NEQLIST PHI1ORPHI2 PHI3 PHI4)))) 
(PUT 'OFSF_GETPHI3PHI4 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETPHI3PHI4 'DEFINED-ON-LINE '767) 
(PUT 'OFSF_GETPHI3PHI4 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETPHI3PHI4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETPHI3PHI4 (XN PHI)
    (PROG (CONJ PHI3 PHI4)
      (PROG (FORMULAL)
        (SETQ FORMULAL PHI)
       LAB
        (COND ((NULL FORMULAL) (RETURN NIL)))
        (PROGN
         (SETQ CONJ (OFSF_GETCONJ3 XN (CAR FORMULAL)))
         (COND ((NEQ CONJ 'FALSE) (SETQ PHI3 (CONS CONJ PHI3))))
         (PROG (FORMULA)
           (SETQ FORMULA (CDR FORMULAL))
          LAB
           (COND ((NULL FORMULA) (RETURN NIL)))
           ((LAMBDA (FORMULA)
              (PROGN
               (SETQ CONJ (OFSF_GETCONJ4 XN (CAR FORMULAL) FORMULA))
               (COND ((NEQ CONJ 'FALSE) (SETQ PHI4 (CONS CONJ PHI4))))))
            (CAR FORMULA))
           (SETQ FORMULA (CDR FORMULA))
           (GO LAB)))
        (SETQ FORMULAL (CDR FORMULAL))
        (GO LAB))
      (SETQ PHI3 (LTO_LIST2SET PHI3))
      (SETQ PHI4 (LTO_LIST2SET PHI4))
      (RETURN (LIST PHI3 PHI4)))) 
(PUT 'OFSF_GETCONJ3 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETCONJ3 'DEFINED-ON-LINE '788) 
(PUT 'OFSF_GETCONJ3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETCONJ3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETCONJ3 (XN F)
    (PROG (D NOTCONST)
      (SETQ D (CAR (DIFFF F XN)))
      (SETQ NOTCONST (OFSF_GETNOTCONST XN D))
      (COND ((EQUAL NOTCONST 'TRUE) (RETURN (LIST (LIST 'TRUE) D)))
            ((EQUAL NOTCONST 'FALSE) (RETURN 'FALSE))
            (T (RETURN (LIST NOTCONST D)))))) 
(PUT 'OFSF_GETNEQ0F 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETNEQ0F 'DEFINED-ON-LINE '806) 
(PUT 'OFSF_GETNEQ0F 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETNEQ0F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETNEQ0F (XN F)
    (PROG (RES DF OLDORDER)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ DF (REORDER F))
      (SETKORDER OLDORDER)
      (COND ((NULL DF) (RETURN 'FALSE)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL DF))) (RETURN NIL)))
        (COND
         ((OR (ATOM DF) (ATOM (CAR DF))) (PROGN (SETQ DF NIL) (SETQ RES NIL)))
         ((NEQ (CAAAR DF) XN)
          (PROGN (SETQ RES (CONS (LIST 'NEQ DF NIL) RES)) (SETQ DF NIL)))
         ((OR (ATOM (CDAR DF)) (ATOM (CAR (CDAR DF))))
          (PROGN (SETQ DF NIL) (SETQ RES NIL)))
         (T
          (PROGN
           (SETQ RES (CONS (LIST 'NEQ (CDAR DF) NIL) RES))
           (SETQ DF (CDR DF)))))
        (GO WHILELABEL))
      (COND ((NULL RES) (RETURN 'TRUE)) (T (RETURN (LTO_LIST2SET RES)))))) 
(PUT 'OFSF_GETNEQ0FGEN 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETNEQ0FGEN 'DEFINED-ON-LINE '839) 
(PUT 'OFSF_GETNEQ0FGEN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETNEQ0FGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETNEQ0FGEN (XN F)
    (PROG (RES DF OLDORDER)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ DF (REORDER F))
      (SETKORDER OLDORDER)
      (COND ((NULL DF) (RETURN 'FALSE)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL DF))) (RETURN NIL)))
        (COND
         ((OR (ATOM DF) (ATOM (CAR DF))) (PROGN (SETQ DF NIL) (SETQ RES NIL)))
         ((NEQ (CAAAR DF) XN)
          (PROGN (SETQ RES (CONS (LIST 'NEQ DF NIL) RES)) (SETQ DF NIL)))
         ((OR (ATOM (CDAR DF)) (ATOM (CAR (CDAR DF))))
          (PROGN (SETQ DF NIL) (SETQ RES NIL)))
         ((NOT (INTERSECTION (KERNELS (CDAR DF)) OFSF_HQEXVARS*))
          (PROGN
           (SETQ OFSF_HQETHEO* (CONS (LIST 'NEQ (CDAR DF) NIL) OFSF_HQETHEO*))
           (SETQ DF NIL)
           (SETQ RES NIL)))
         (T
          (PROGN
           (SETQ RES (CONS (LIST 'NEQ (CDAR DF) NIL) RES))
           (SETQ DF (CDR DF)))))
        (GO WHILELABEL))
      (COND ((NULL RES) (RETURN 'TRUE)) (T (RETURN (LTO_LIST2SET RES)))))) 
(PUT 'OFSF_GETCONJ4 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GETCONJ4 'DEFINED-ON-LINE '876) 
(PUT 'OFSF_GETCONJ4 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETCONJ4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETCONJ4 (XN F1 F2)
    (PROG (S NOTCONST)
      (SETQ S (ADDF F1 (NEGF F2)))
      (SETQ NOTCONST (OFSF_GETNOTCONST XN S))
      (COND ((EQUAL NOTCONST 'TRUE) (RETURN (LIST (LIST 'TRUE) S)))
            ((EQUAL NOTCONST 'FALSE) (RETURN 'FALSE))
            (T (RETURN (LIST NOTCONST S)))))) 
(PUT 'OFSF_GETNOTCONST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETNOTCONST 'DEFINED-ON-LINE '894) 
(PUT 'OFSF_GETNOTCONST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETNOTCONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETNOTCONST (XN F)
    (PROG (RES DF OLDORDER)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ DF (REORDER F))
      (COND (*RLHQEGEN (SETQ RES (OFSF_GETNOTCONSTFGEN XN DF NIL)))
            (T (SETQ RES (OFSF_GETNOTCONSTF XN DF NIL))))
      (SETKORDER OLDORDER)
      (RETURN RES))) 
(PUT 'OFSF_GETNOTCONSTF 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GETNOTCONSTF 'DEFINED-ON-LINE '909) 
(PUT 'OFSF_GETNOTCONSTF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETNOTCONSTF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETNOTCONSTF (XN F L)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) XN))
      (COND ((NULL L) 'FALSE) (T (CDR L))))
     ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F)))) (COND ((NULL L) 'TRUE) (T L)))
     ((NULL L)
      (OFSF_GETNOTCONSTF XN (CDR F)
       (LIST (LIST 'EQUAL (OFSF_NORMCOND (CDAR F)) NIL)
             (LIST 'NEQ (OFSF_NORMCOND (CDAR F)) NIL))))
     ((EQ (CAR L) 'FALSE) (CDR L)) ((EQ (CAR L) 'TRUE) L)
     ((OFSF_QUOTTEST (SFTO_SQFPARTF (CDAR F)) (CAR L))
      (OFSF_GETNOTCONSTF XN (CDR F) L))
     (T
      (OFSF_GETNOTCONSTF XN (CDR F)
       (CONS (OFSF_AND1 (LIST 'EQUAL (OFSF_NORMCOND (CDAR F)) NIL) (CAR L))
             (CONS (OFSF_AND1 (LIST 'NEQ (OFSF_NORMCOND (CDAR F)) NIL) (CAR L))
                   (CDR L))))))) 
(PUT 'OFSF_NORMCOND 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NORMCOND 'DEFINED-ON-LINE '938) 
(PUT 'OFSF_NORMCOND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_NORMCOND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NORMCOND (F) (SFTO_SQFPARTF (SFTO_DPRPARTF F))) 
(PUT 'OFSF_GETNOTCONSTFGEN 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GETNOTCONSTFGEN 'DEFINED-ON-LINE '943) 
(PUT 'OFSF_GETNOTCONSTFGEN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETNOTCONSTFGEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETNOTCONSTFGEN (XN F L)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) XN))
      (COND ((NULL L) 'FALSE) (T (CDR L))))
     ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F)))) (COND ((NULL L) 'TRUE) (T L)))
     ((NOT (INTERSECTION (KERNELS (CDAR F)) OFSF_HQEXVARS*))
      (PROGN
       (SETQ OFSF_HQETHEO* (CONS (LIST 'NEQ (CDAR F) NIL) OFSF_HQETHEO*))
       (COND ((NULL L) 'TRUE) (T L))))
     ((NULL L)
      (OFSF_GETNOTCONSTF XN (CDR F)
       (LIST (LIST 'EQUAL (OFSF_NORMCOND (CDAR F)) NIL)
             (LIST 'NEQ (OFSF_NORMCOND (CDAR F)) NIL))))
     ((EQ (CAR L) 'FALSE) (CDR L)) ((EQ (CAR L) 'TRUE) L)
     ((OFSF_QUOTTEST (SFTO_SQFPARTF (CDAR F)) (CAR L))
      (OFSF_GETNOTCONSTF XN (CDR F) L))
     (T
      (OFSF_GETNOTCONSTF XN (CDR F)
       (CONS (OFSF_AND1 (LIST 'EQUAL (OFSF_NORMCOND (CDAR F)) NIL) (CAR L))
             (CONS (OFSF_AND1 (LIST 'NEQ (OFSF_NORMCOND (CDAR F)) NIL) (CAR L))
                   (CDR L))))))) 
(PUT 'OFSF_QUOTTEST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QUOTTEST 'DEFINED-ON-LINE '977) 
(PUT 'OFSF_QUOTTEST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_QUOTTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QUOTTEST (F1 F2)
    (COND
     ((EQ (COND ((ATOM F2) F2) (T (CAR F2))) 'EQUAL)
      (OFSF_QUOTTEST1 F1 (LIST (CADR F2))))
     (T
      (OFSF_QUOTTEST1 F1
       (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
         (SETQ ELEM (CDR F2))
         (COND ((NULL ELEM) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (ELEM) (CADR ELEM)) (CAR ELEM)) NIL)))
        LOOPLABEL
         (SETQ ELEM (CDR ELEM))
         (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (ELEM) (CADR ELEM)) (CAR ELEM)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'OFSF_QUOTTEST1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QUOTTEST1 'DEFINED-ON-LINE '985) 
(PUT 'OFSF_QUOTTEST1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_QUOTTEST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QUOTTEST1 (F1 F2)
    (COND ((NULL F2) NIL) (((LAMBDA (*EXP) (QUOTF1 F1 (CAR F2))) T) T)
          (T (OFSF_QUOTTEST1 F1 (CDR F2))))) 
(PUT 'OFSF_INF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_INF 'DEFINED-ON-LINE '995) 
(PUT 'OFSF_INF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_INF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_INF (XN F)
    (PROG (RES OLDORDER)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ RES (OFSF_INF1 XN (REORDER F)))
      (SETKORDER OLDORDER)
      (RETURN RES))) 
(PUT 'OFSF_INF1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_INF1 'DEFINED-ON-LINE '1005) 
(PUT 'OFSF_INF1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_INF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_INF1 (XN F)
    (COND ((NULL F) 'FALSE)
          ((OR (ATOM F) (ATOM (CAR F)))
           (COND ((GREATERP F 0) 'TRUE) (T 'FALSE)))
          ((NEQ (CAAAR F) XN) (LIST 'GREATERP F NIL))
          ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F))))
           (COND ((GREATERP (CDAR F) 0) 'TRUE) (T 'FALSE)))
          (T
           (OFSF_OR (LIST 'GREATERP (CDAR F) NIL)
            (OFSF_AND (LIST 'EQUAL (CDAR F) NIL) (OFSF_INF1 XN (CDR F))))))) 
(PUT 'OFSF_MINF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MINF 'DEFINED-ON-LINE '1027) 
(PUT 'OFSF_MINF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MINF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MINF (XN F)
    (PROG (RES OLDORDER)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ RES (OFSF_MINF1 XN (REORDER F)))
      (SETKORDER OLDORDER)
      (RETURN RES))) 
(PUT 'OFSF_MINF1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MINF1 'DEFINED-ON-LINE '1037) 
(PUT 'OFSF_MINF1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MINF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MINF1 (XN F)
    (COND ((NULL F) 'FALSE)
          ((OR (ATOM F) (ATOM (CAR F)))
           (COND ((GREATERP F 0) 'TRUE) (T 'FALSE)))
          ((NEQ (CAAAR F) XN) (LIST 'GREATERP F NIL))
          ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F))))
           (COND
            ((GREATERP
              ((LAMBDA (G396)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR F) G396))
                       (T (POLY-MULTF (CDAR F) G396))))
               (EXPTF (CAR (SIMP '(MINUS 1))) (CDAAR F)))
              0)
             'TRUE)
            (T 'FALSE)))
          (T
           (OFSF_OR
            (LIST 'GREATERP
                  ((LAMBDA (G398)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR F) G398))
                           (T (POLY-MULTF (CDAR F) G398))))
                   (EXPTF (CAR (SIMP '(MINUS 1))) (CDAAR F)))
                  NIL)
            (OFSF_AND (LIST 'EQUAL (CDAR F) NIL) (OFSF_MINF1 XN (CDR F))))))) 
(PUT 'OFSF_BUILDPHI1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDPHI1 'DEFINED-ON-LINE '1059) 
(PUT 'OFSF_BUILDPHI1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDPHI1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDPHI1 (XN GLIST)
    (PROG (GLIST2 CONJ1 PHI1)
      (SETQ GLIST2 GLIST)
      (SETQ PHI1 'TRUE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL GLIST2))) (RETURN NIL)))
        (PROGN
         (SETQ CONJ1 (OFSF_INF XN (CAR GLIST2)))
         (COND
          ((EQUAL CONJ1 'FALSE) (PROGN (SETQ GLIST2 NIL) (SETQ PHI1 'FALSE)))
          (T
           (PROGN
            (SETQ GLIST2 (CDR GLIST2))
            (SETQ PHI1 (OFSF_AND PHI1 CONJ1))))))
        (GO WHILELABEL))
      (RETURN PHI1))) 
(PUT 'OFSF_BUILDPHI2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDPHI2 'DEFINED-ON-LINE '1079) 
(PUT 'OFSF_BUILDPHI2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDPHI2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDPHI2 (XN GLIST)
    (PROG (PHI2 CONJ1 GLIST2)
      (SETQ GLIST2 GLIST)
      (SETQ PHI2 'TRUE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL GLIST2))) (RETURN NIL)))
        (PROGN
         (SETQ CONJ1 (OFSF_MINF XN (CAR GLIST2)))
         (COND
          ((EQUAL CONJ1 'FALSE) (PROGN (SETQ GLIST2 NIL) (SETQ PHI2 'FALSE)))
          (T
           (PROGN
            (SETQ GLIST2 (CDR GLIST2))
            (SETQ PHI2 (OFSF_AND PHI2 CONJ1))))))
        (GO WHILELABEL))
      (RETURN PHI2))) 
(PUT 'OFSF_BUILDHKNEQ0 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDHKNEQ0 'DEFINED-ON-LINE '1099) 
(PUT 'OFSF_BUILDHKNEQ0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDHKNEQ0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDHKNEQ0 (XN NEQLIST)
    (PROG (RES N2 CONJ1)
      (SETQ N2 NEQLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL N2))) (RETURN NIL)))
        (PROGN
         (COND (*RLHQEGEN (SETQ CONJ1 (OFSF_GETNEQ0FGEN XN (CAR N2))))
               (T (SETQ CONJ1 (OFSF_GETNEQ0F XN (CAR N2)))))
         (COND ((EQUAL CONJ1 'TRUE) (SETQ N2 (CDR N2)))
               ((EQUAL CONJ1 'FALSE) (PROGN (SETQ RES 'FALSE) (SETQ N2 NIL)))
               (T
                (PROGN
                 (SETQ RES
                         (CONS
                          (COND ((AND CONJ1 (CDR CONJ1)) (CONS 'OR CONJ1))
                                ((NULL CONJ1)
                                 (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                (T (CAR CONJ1)))
                          RES))
                 (SETQ N2 (CDR N2))))))
        (GO WHILELABEL))
      (COND ((MEMBER RES (LIST 'TRUE 'FALSE)) (RETURN RES))
            (T
             (RETURN
              (OFSF_RQSIMPL
               (COND ((AND RES (CDR RES)) (CONS 'AND RES))
                     ((NULL RES) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR RES))))))))) 
(PUT 'OFSF_AND 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_AND 'DEFINED-ON-LINE '1127) 
(PUT 'OFSF_AND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_AND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_AND (F1 F2)
    (PROG (PHI)
      (COND (*RLHQEVB (IOTO_TPRIN2 (LIST "simplifying a conjunction ... "))))
      (SETQ PHI
              (CL_SIMPL
               ((LAMBDA (G400)
                  (COND ((AND G400 (CDR G400)) (CONS 'AND G400))
                        ((NULL G400) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G400))))
                (LIST F1 F2))
               NIL (MINUS 1)))
      (COND (*RLHQEVB (IOTO_PRIN2 "done")))
      (RETURN PHI))) 
(PUT 'OFSF_OR 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_OR 'DEFINED-ON-LINE '1139) 
(PUT 'OFSF_OR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_OR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_OR (F1 F2)
    (PROG (*RLSIEXPLA *RLSIPW PHI)
      (COND (*RLHQEVB (IOTO_TPRIN2 (LIST "simplifying a disjunction ... "))))
      (SETQ PHI
              (CL_SIMPL
               ((LAMBDA (G402)
                  (COND ((AND G402 (CDR G402)) (CONS 'OR G402))
                        ((NULL G402) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G402))))
                (LIST F1 F2))
               NIL (MINUS 1)))
      (COND (*RLHQEVB (IOTO_PRIN2 "done")))
      (RETURN PHI))) 
(PUT 'OFSF_AND1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_AND1 'DEFINED-ON-LINE '1151) 
(PUT 'OFSF_AND1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_AND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_AND1 (F1 F2)
    (PROG (*RLSIEXPLA *RLSIPW PHI)
      (COND (*RLHQEVB (IOTO_TPRIN2 (LIST "simplifying a disjunction ... "))))
      (SETQ PHI
              (CL_SIMPL
               ((LAMBDA (G404)
                  (COND ((AND G404 (CDR G404)) (CONS 'AND G404))
                        ((NULL G404) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G404))))
                (LIST F1 F2))
               NIL (MINUS 1)))
      (COND (*RLHQEVB (IOTO_PRIN2 "done.")))
      (RETURN PHI))) 
(PUT 'OFSF_MKNEWF2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKNEWF2 'DEFINED-ON-LINE '1163) 
(PUT 'OFSF_MKNEWF2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MKNEWF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKNEWF2 (VARL F)
    (COND ((NULL VARL) F)
          (T (OFSF_MKNEWF2 (CDR VARL) (LIST 'EX (CAR VARL) F))))) 
(PUT 'OFSF_QENFCASE0 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QENFCASE0 'DEFINED-ON-LINE '1170) 
(PUT 'OFSF_QENFCASE0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_QENFCASE0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QENFCASE0 (CONDL XN GL F)
    (PROG (OLDORDER PSI SF_CONDL SF_F)
      (SETQ OLDORDER (SETKORDER (CONS XN KORD*)))
      (SETQ SF_F (REORDER F))
      (COND ((EQUAL CONDL 'TRUE) (SETQ SF_CONDL NIL))
            ((NEQ (COND ((ATOM CONDL) CONDL) (T (CAR CONDL))) 'AND)
             (SETQ SF_CONDL (LIST CONDL)))
            (T (SETQ SF_CONDL (CDR CONDL))))
      (SETQ SF_F (OFSF_DELETEMON XN SF_F SF_CONDL SF_CONDL))
      (SETKORDER OLDORDER)
      (SETQ SF_F (REORDER SF_F))
      (SETQ PSI (OFSF_D0MAIN (LIST SF_F) (LIST XN) GL NIL))
      (RETURN PSI))) 
(PUT 'OFSF_DELETEMON 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_DELETEMON 'DEFINED-ON-LINE '1194) 
(PUT 'OFSF_DELETEMON 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_DELETEMON 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DELETEMON (XN F L1 L2)
    (COND ((OR (NULL L2) (OR (ATOM F) (ATOM (CAR F)))) F)
          ((EQ (COND ((ATOM (CAR L2)) (CAR L2)) (T (CAR (CAR L2)))) 'EQUAL)
           (COND
            ((NEQ (CAAAR F) XN)
             (COND (((LAMBDA (*EXP) (QUOTF1 F (CADR (CAR L2)))) T) NIL)
                   (T (OFSF_DELETEMON XN F L1 (CDR L2)))))
            (((LAMBDA (*EXP) (QUOTF1 (CDAR F) (CADR (CAR L2)))) T)
             (OFSF_DELETEMON XN (CDR F) L1 L1))
            (T (OFSF_DELETEMON XN F L1 (CDR L2)))))
          (T (OFSF_DELETEMON XN F L1 (CDR L2))))) 
(PUT 'OFSF_GGSYS 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GGSYS 'DEFINED-ON-LINE '1213) 
(PUT 'OFSF_GGSYS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GGSYS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GGSYS (L VARL XI)
    (PROG (*CGBREAL *CGBFAITHFUL CDL GSYS)
      (SETQ *CGBREAL T)
      (COND (*RLHQETHEORY (SETQ CDL (OFSF_MKCONDLIST XI))))
      (COND
       (*RLHQEGEN
        (PROGN
         (SETQ GSYS (CGB_GGSYSF L CDL OFSF_HQEXVARS* VARL 'REVGRADLEX NIL))
         (SETQ OFSF_HQETHEO*
                 (APPEND (OFSF_BUILDTHEORY GSYS CDL) OFSF_HQETHEO*))
         (SETQ GSYS (OFSF_BUILDGENGGSYS GSYS))))
       (T (SETQ GSYS (CGB_GSYSF L CDL VARL 'REVGRADLEX NIL))))
      (COND (*RLHQEGBRED (RETURN (OFSF_GBRED GSYS VARL))) (T (RETURN GSYS))))) 
(PUT 'OFSF_GBRED 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GBRED 'DEFINED-ON-LINE '1238) 
(PUT 'OFSF_GBRED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GBRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GBRED (GSYS VARL)
    (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
      (SETQ ELEM GSYS)
      (COND ((NULL ELEM) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (ELEM)
                          (LIST (CAR ELEM) (OFSF_GBRED1 (CADR ELEM) VARL)))
                        (CAR ELEM))
                       NIL)))
     LOOPLABEL
      (SETQ ELEM (CDR ELEM))
      (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (ELEM)
                  (LIST (CAR ELEM) (OFSF_GBRED1 (CADR ELEM) VARL)))
                (CAR ELEM))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_GBRED1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GBRED1 'DEFINED-ON-LINE '1243) 
(PUT 'OFSF_GBRED1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GBRED1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GBRED1 (GB VARL)
    (PROG (OLD GB1)
      (SETQ OLD (DIP_INIT VARL 'REVGRADLEX NIL))
      (SETQ GB1
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GB)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GB1 (GB_TRAVERSO-FINAL GB1))
      (SETQ GB1
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GB1)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (VDP_CLEANUP OLD)
      (RETURN GB1))) 
(PUT 'OFSF_MKCONJ 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_MKCONJ 'DEFINED-ON-LINE '1255) 
(PUT 'OFSF_MKCONJ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MKCONJ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_MKCONJ (CONDLIST)
    (COND ((OR (NULL CONDLIST) (EQ CONDLIST 'TRUE)) 'TRUE)
          ((NULL (CDR CONDLIST)) (CAR CONDLIST))
          (T
           (COND ((AND CONDLIST (CDR CONDLIST)) (CONS 'AND CONDLIST))
                 ((NULL CONDLIST) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR CONDLIST)))))) 
(PUT 'OFSF_CASEDIMN 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_CASEDIMN 'DEFINED-ON-LINE '1264) 
(PUT 'OFSF_CASEDIMN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CASEDIMN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CASEDIMN (ITA XI VARL GLIST HLIST)
    (COND (*RLHQEDIM0 (OFSF_MKNEWF2 VARL (OFSF_SFL2F NIL GLIST HLIST)))
          (T (OFSF_QENF (OFSF_AND1 ITA XI) NIL GLIST HLIST VARL)))) 
(PUT 'OFSF_SFL2F 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SFL2F 'DEFINED-ON-LINE '1273) 
(PUT 'OFSF_SFL2F 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SFL2F 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SFL2F (FL GL HL)
    (PROG (FL1 GL1 HL1)
      (SETQ FL1
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM FL)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (LIST 'EQUAL ELEM NIL))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (LIST 'EQUAL ELEM NIL)) (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GL1
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GL)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (LIST 'GREATERP ELEM NIL))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (LIST 'GREATERP ELEM NIL)) (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ HL1
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM HL)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (LIST 'NEQ ELEM NIL))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELEM) (LIST 'NEQ ELEM NIL)) (CAR ELEM))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       ((LAMBDA (G406)
          (COND ((AND G406 (CDR G406)) (CONS 'AND G406))
                ((NULL G406) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G406))))
        (APPEND FL1 (APPEND GL1 HL1)))))) 
(PUT 'OFSF_REMVARL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_REMVARL 'DEFINED-ON-LINE '1283) 
(PUT 'OFSF_REMVARL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_REMVARL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_REMVARL (VARL IVARL) (SETDIFF VARL IVARL)) 
(PUT 'OFSF_CASEDIM 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_CASEDIM 'DEFINED-ON-LINE '1289) 
(PUT 'OFSF_CASEDIM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CASEDIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_CASEDIM (ITA XI BVARL IVARL GB GLIST HLIST)
    (PROG (NEWPSI NEWGB NEWGLIST NEWHLIST THEO)
      (COND
       (*RLHQEDIM0
        (RETURN
         (OFSF_MKNEWF2 (APPEND BVARL IVARL) (OFSF_SFL2F GB GLIST HLIST)))))
      (SETQ NEWGB (OFSF_SORT GB BVARL))
      (SETQ NEWGLIST (OFSF_SORT GLIST BVARL))
      (SETQ NEWHLIST (OFSF_SORT HLIST BVARL))
      (SETQ THEO (OFSF_MKTHEO (CAR NEWGB) (CAR NEWGLIST) (CAR NEWHLIST)))
      (SETQ NEWPSI
              (OFSF_RQSIMPL
               ((LAMBDA (G408)
                  (COND ((AND G408 (CDR G408)) (CONS 'AND G408))
                        ((NULL G408) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G408))))
                (LIST THEO
                      (OFSF_QENF (OFSF_AND1 THEO (OFSF_AND1 ITA XI))
                       (CADR NEWGB) (CADR NEWGLIST) (CADR NEWHLIST) BVARL)))))
      (RETURN (OFSF_HQE0 (OFSF_MKNEWF2 IVARL NEWPSI))))) 
(PUT 'OFSF_RQSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_RQSIMPL 'DEFINED-ON-LINE '1306) 
(PUT 'OFSF_RQSIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_RQSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_RQSIMPL (PHI)
    (PROG (PHI1)
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "simplifying formula with " (CL_ATNUM PHI)
               " atomic formulas ... "))))
      (SETQ PHI1 (CL_SIMPL PHI NIL (MINUS 1)))
      (COND (*RLHQEVB (IOTO_PRIN2 (LIST "done (" (CL_ATNUM PHI1) ")"))))
      (RETURN PHI1))) 
(PUT 'OFSF_SORT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SORT 'DEFINED-ON-LINE '1319) 
(PUT 'OFSF_SORT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SORT (L VARL)
    (PROG (L1 IL DL)
      (SETQ L1 L)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL L1))) (RETURN NIL)))
        (PROGN
         (COND
          ((INTERSECTION (KERNELS (CAR L1)) VARL) (SETQ DL (CONS (CAR L1) DL)))
          (T (SETQ IL (CONS (CAR L1) IL))))
         (SETQ L1 (CDR L1)))
        (GO WHILELABEL))
      (RETURN (LIST IL DL)))) 
(PUT 'OFSF_MKTHEO 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MKTHEO 'DEFINED-ON-LINE '1336) 
(PUT 'OFSF_MKTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MKTHEO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKTHEO (FL GL HL)
    (PROG (RES)
      (SETQ RES
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM FL)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (LIST 'EQUAL ELEM NIL))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (LIST 'EQUAL ELEM NIL)) (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES
              (APPEND
               (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ELEM GL)
                 (COND ((NULL ELEM) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELEM) (LIST 'GREATERP ELEM NIL))
                                   (CAR ELEM))
                                  NIL)))
                LOOPLABEL
                 (SETQ ELEM (CDR ELEM))
                 (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ELEM) (LIST 'GREATERP ELEM NIL))
                           (CAR ELEM))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               RES))
      (SETQ RES
              (APPEND
               (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ELEM HL)
                 (COND ((NULL ELEM) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELEM) (LIST 'NEQ ELEM NIL))
                                   (CAR ELEM))
                                  NIL)))
                LOOPLABEL
                 (SETQ ELEM (CDR ELEM))
                 (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ELEM) (LIST 'NEQ ELEM NIL)) (CAR ELEM))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               RES))
      (RETURN
       (COND ((AND RES (CDR RES)) (CONS 'AND RES))
             ((NULL RES) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR RES)))))) 
(PUT 'OFSF_MKCONDLIST 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_MKCONDLIST 'DEFINED-ON-LINE '1346) 
(PUT 'OFSF_MKCONDLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MKCONDLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_MKCONDLIST (CONJ)
    (PROG (L CDL CD)
      (COND ((EQ CONJ 'TRUE) (RETURN NIL)))
      (SETQ L
              (COND
               ((NEQ (COND ((ATOM CONJ) CONJ) (T (CAR CONJ))) 'AND)
                (LIST CONJ))
               (T (CDR CONJ))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL L))) (RETURN NIL)))
        (PROGN
         (SETQ CD (OFSF_GETCDFORM (CAR L)))
         (COND ((NOT (NULL CD)) (SETQ CDL (CONS CD CDL))))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN CDL))) 
(PUT 'OFSF_GETCDFORM 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GETCDFORM 'DEFINED-ON-LINE '1366) 
(PUT 'OFSF_GETCDFORM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETCDFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GETCDFORM (AF)
    (COND ((MEMBER (COND ((ATOM AF) AF) (T (CAR AF))) (LIST 'EQUAL 'NEQ)) AF)
          ((MEMBER (COND ((ATOM AF) AF) (T (CAR AF))) (LIST 'GREATERP 'LESSP))
           (LIST 'NEQ (CADR AF) NIL)))) 
(PUT 'OFSF_BUILDTHEORY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDTHEORY 'DEFINED-ON-LINE '1374) 
(PUT 'OFSF_BUILDTHEORY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDTHEORY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDTHEORY (GSYS ICD)
    (PROG (CDL RES)
      (SETQ CDL
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GSYS)
               STARTOVER
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (ELEM) (CAR ELEM)) (CAR ELEM)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ ELEM (CDR ELEM))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (ELEM) (CAR ELEM)) (CAR ELEM)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ ELEM (CDR ELEM))
                (GO LOOPLABEL)))
      (SETQ CDL (LTO_SETMINUS CDL (RL_THSIMPL ICD)))
      (PROG (ELEM)
        (SETQ ELEM CDL)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (PROGN
            (COND
             ((AND (EQ (COND ((ATOM ELEM) ELEM) (T (CAR ELEM))) 'NEQ)
                   (NOT (INTERSECTION (KERNELS (CADR ELEM)) OFSF_HQEXVARS*)))
              (SETQ RES (CONS ELEM RES))))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (RETURN (RL_THSIMPL RES)))) 
(PUT 'OFSF_BUILDGENGGSYS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_BUILDGENGGSYS 'DEFINED-ON-LINE '1388) 
(PUT 'OFSF_BUILDGENGGSYS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDGENGGSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_BUILDGENGGSYS (GSYS)
    (PROG (BRANCH FORALL-RESULT FORALL-ENDPTR)
      (SETQ BRANCH GSYS)
      (COND ((NULL BRANCH) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (BRANCH)
                          (COND ((EQCAR BRANCH 'TRUE) BRANCH)
                                (T
                                 (LIST
                                  (LTO_SETMINUS (CAR BRANCH) OFSF_HQETHEO*)
                                  (CADR BRANCH)))))
                        (CAR BRANCH))
                       NIL)))
     LOOPLABEL
      (SETQ BRANCH (CDR BRANCH))
      (COND ((NULL BRANCH) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (BRANCH)
                  (COND ((EQCAR BRANCH 'TRUE) BRANCH)
                        (T
                         (LIST (LTO_SETMINUS (CAR BRANCH) OFSF_HQETHEO*)
                               (CADR BRANCH)))))
                (CAR BRANCH))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_D0MAIN 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_D0MAIN 'DEFINED-ON-LINE '1409) 
(PUT 'OFSF_D0MAIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_D0MAIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_D0MAIN (GB VARL GREATERLIST NEQLIST)
    (PROG (BETA CHI HELIST Y COEFFL QF VDP_GB VSBASIS OLD OLDORDER I)
      (SETQ I 0)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "+ begin d0: r:" (LENGTH GB) " n:" (LENGTH VARL) " s:"
               (LENGTH GREATERLIST) " t:" (LENGTH NEQLIST)))))
      (SETQ HELIST (OFSF_BUILDHELIST GREATERLIST NEQLIST))
      (SETQ OLD (DIP_INIT VARL 'REVGRADLEX NIL))
      (SETQ VDP_GB
              (OFSF_REDGROEBNER
               (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ELEM GB)
                 (COND ((NULL ELEM) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                                  NIL)))
                LOOPLABEL
                 (SETQ ELEM (CDR ELEM))
                 (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         "+ computing residue class ring vector space basis ... ")))
      (SETQ VSBASIS (OFSF_GVSBASIS (OFSF_GB2GLTB VDP_GB) VARL))
      (COND
       (*RLHQEVB (IOTO_PRIN2 (LIST "done. Dimension: " (LENGTH VSBASIS)))))
      (COND
       (*RLHQESTRCONST
        (PROGN
         (SETQ VSBASIS
                 (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELEM VSBASIS)
                   (COND ((NULL ELEM) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELEM) (VDP_F2VDP ELEM))
                                     (CAR ELEM))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELEM (CDR ELEM))
                   (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ BETA (GBSC_STRCONST VSBASIS VDP_GB 4)))))
      (SETQ Y (INTERN (GENSYM)))
      (SETQ OLDORDER (SETKORDER (CONS Y KORD*)))
      (SETQ CHI (COND ((NOT *RLHQETFCSPLIT) (SIMP 1))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "computing characteristic "
               (IOTO_CPLU "polynomial" (NEQ (LENGTH HELIST) 1)) ":"))))
      (PROG (ELEM)
        (SETQ ELEM HELIST)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (PROGN
            (COND
             (*RLVERBOSE
              (PROGN
               (SETQ I (PLUS I 1))
               (IOTO_PRIN2 (LIST " " (PLUS (DIFFERENCE (LENGTH HELIST) I) 1)))
               NIL)))
            (COND
             (*RLHQETFCSPLIT
              (SETQ CHI (CONS (OFSF_D0MAIN1 VDP_GB VSBASIS BETA ELEM Y) CHI)))
             (T
              (SETQ CHI
                      (MULTSQ CHI
                              (OFSF_D0MAIN1 VDP_GB VSBASIS BETA ELEM Y)))))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (COND (*RLVERBOSE (IOTO_PRIN2 " done.")))
      (COND
       ((NOT *RLHQETFCSPLIT)
        (PROGN
         (SETQ COEFFL (REVERSIP (OFSF_COEFFLIST CHI Y)))
         (COND (*RLHQEVB (IOTO_PRIN2 "Done.")))
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2
            (LIST "computing type formula of length " (LENGTH COEFFL)
                  " ... "))))
         (SETQ QF (OFSF_TFC COEFFL))))
       (T
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_PRIN2 "constructing disjunction of type formulas ... ")
            NIL)))
         (SETQ QF (OFSF_TFCMAIN CHI Y))
         NIL)))
      (SETKORDER OLDORDER)
      (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "done (" (CL_ATNUM QF) ")"))))
      (SETQ QF (CL_SIMPL (LIST 'NOT QF) NIL (MINUS 1)))
      (COND
       (*RLVERBOSE (IOTO_TPRIN2 (LIST "+ end of d0 (" (CL_ATNUM QF) ")"))))
      (VDP_CLEANUP OLD)
      (RETURN QF))) 
(PUT 'OFSF_D0MAIN1 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_D0MAIN1 'DEFINED-ON-LINE '1474) 
(PUT 'OFSF_D0MAIN1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_D0MAIN1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_D0MAIN1 (VDPGB VSBASIS BETA HE CHARX)
    (OFSF_CHARPOLY (CONS 'MAT (OFSF_BUILDQ VDPGB HE VSBASIS BETA)) CHARX)) 
(PUT 'OFSF_GB2GLTB 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GB2GLTB 'DEFINED-ON-LINE '1481) 
(PUT 'OFSF_GB2GLTB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GB2GLTB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GB2GLTB (VDPGB)
    (PROG (BASIS2)
      (SETQ BASIS2
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM VDPGB)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (VDP_FMON (SIMP* 1) (CADR ELEM)))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (VDP_FMON (SIMP* 1) (CADR ELEM)))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BASIS2
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM BASIS2)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN BASIS2))) 
(PUT 'OFSF_GVSBASIS 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GVSBASIS 'DEFINED-ON-LINE '1491) 
(PUT 'OFSF_GVSBASIS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GVSBASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GVSBASIS (LTB VARL)
    (PROG (HTL BASIS BASIS2 V D TT)
      (SETQ HTL (OFSF_MVP LTB VARL))
      (COND
       ((NEQ (LENGTH HTL) (LENGTH VARL))
        (REDERR "ideal not zero dimensional")))
      (SETQ BASIS (LIST (CAR (SIMP 1))))
      (PROG (TERM)
        (SETQ TERM HTL)
       LAB
        (COND ((NULL TERM) (RETURN NIL)))
        ((LAMBDA (TERM)
           (PROGN
            (SETQ V (CAR TERM))
            (SETQ D (CDR TERM))
            (SETQ BASIS2 BASIS)
            (PROG (ELEM)
              (SETQ ELEM BASIS)
             LAB
              (COND ((NULL ELEM) (RETURN NIL)))
              ((LAMBDA (ELEM)
                 (PROG (I)
                   (SETQ I 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE D 1) I)) (RETURN NIL)))
                   (PROGN
                    (SETQ TT
                            ((LAMBDA (G410)
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF ELEM G410))
                                     (T (POLY-MULTF ELEM G410))))
                             (EXPTF (CAR (SIMP V)) I)))
                    (COND
                     ((NOT (OFSF_REDP TT LTB)) (SETQ BASIS (CONS TT BASIS)))))
                   (SETQ I (PLUS2 I 1))
                   (GO LAB)))
               (CAR ELEM))
              (SETQ ELEM (CDR ELEM))
              (GO LAB))))
         (CAR TERM))
        (SETQ TERM (CDR TERM))
        (GO LAB))
      (RETURN BASIS))) 
(PUT 'OFSF_MVP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MVP 'DEFINED-ON-LINE '1514) 
(PUT 'OFSF_MVP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_MVP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MVP (LTB VARL)
    (PROG (HTLIST VAR V D W)
      (PROG (TERM)
        (SETQ TERM LTB)
       LAB
        (COND ((NULL TERM) (RETURN NIL)))
        ((LAMBDA (TERM)
           (PROGN
            (SETQ VAR (KERNELS TERM))
            (COND
             ((AND (EQUAL (LENGTH VAR) 1) (MEMBER (CAR VAR) VARL))
              (PROGN
               (SETQ V (CAR VAR))
               (SETQ D (CDAAR TERM))
               (SETQ W (ASSOC V HTLIST))
               (COND (W (SETCDR W (MIN (CDR W) D)))
                     (T (SETQ HTLIST (CONS (CONS V D) HTLIST)))))))))
         (CAR TERM))
        (SETQ TERM (CDR TERM))
        (GO LAB))
      (RETURN HTLIST))) 
(PUT 'OFSF_REDP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_REDP 'DEFINED-ON-LINE '1534) 
(PUT 'OFSF_REDP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_REDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_REDP (TT LTB)
    (PROG (C)
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C LTB)) (RETURN NIL)))
        (PROGN
         (COND ((NULL (CDR (QREMF TT (CAR LTB)))) (SETQ C NIL))
               (T (SETQ LTB (CDR LTB)))))
        (GO WHILELABEL))
      (RETURN (NOT C)))) 
(PUT 'OFSF_TRACE 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRACE 'DEFINED-ON-LINE '1548) 
(PUT 'OFSF_TRACE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRACE (VDPGB HE VI VJ VSBASIS)
    (PROG (RES)
      (SETQ RES (SIMP 0))
      (PROG (ELEM)
        (SETQ ELEM VSBASIS)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (SETQ RES (ADDSQ RES (OFSF_TRACE1 VDPGB HE VI VJ ELEM))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (RETURN RES))) 
(PUT 'OFSF_TRACE1 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRACE1 'DEFINED-ON-LINE '1558) 
(PUT 'OFSF_TRACE1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRACE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRACE1 (VDPGB HE VI VJ ELEM)
    (OFSF_GETCOEFF (OFSF_PROD HE VI VJ ELEM VDPGB) (VDP_F2VDP ELEM))) 
(PUT 'OFSF_PROD 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PROD 'DEFINED-ON-LINE '1563) 
(PUT 'OFSF_PROD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_PROD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PROD (HE VI VJ ELEM VDPGB)
    (GB_REDUCE
     (VDP_F2VDP
      ((LAMBDA (G414)
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF HE G414))
               (T (POLY-MULTF HE G414))))
       ((LAMBDA (G412)
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF VI G412))
                (T (POLY-MULTF VI G412))))
        (COND (*PHYSOP-LOADED (PHYSOP-MULTF VJ ELEM))
              (T (POLY-MULTF VJ ELEM))))))
     VDPGB)) 
(PUT 'OFSF_GETCOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETCOEFF 'DEFINED-ON-LINE '1568) 
(PUT 'OFSF_GETCOEFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETCOEFF (F VI)
    (COND ((NULL (CAR (CDDDR F))) (SIMP 0))
          ((EQUAL (CADR F) (CADR VI)) (CADDR F))
          (T (OFSF_GETCOEFF (VDP_MRED F) VI)))) 
(PUT 'OFSF_CHARPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CHARPOLY 'DEFINED-ON-LINE '1577) 
(PUT 'OFSF_CHARPOLY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_CHARPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CHARPOLY (Q X) (SIMP (REVAL1 (LIST 'CHAR_POLY Q X) NIL))) 
(PUT 'OFSF_COEFFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_COEFFLIST 'DEFINED-ON-LINE '1582) 
(PUT 'OFSF_COEFFLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_COEFFLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_COEFFLIST (P X)
    (PROG (RES Q1 Q2 D)
      (SETQ Q1 (REORDER (CAR P)))
      (SETQ Q2 (CDR P))
      (SETQ D (CDAAR Q1))
      (SETQ RES (OFSF_COEFFLIST1 Q1 X D))
      (SETQ RES
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM RES)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (MULTSQ (SIMP (PREPF ELEM))
                                            (INVSQ (SIMP (PREPF Q2)))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM)
                            (MULTSQ (SIMP (PREPF ELEM))
                                    (INVSQ (SIMP (PREPF Q2)))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'OFSF_COEFFLIST1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_COEFFLIST1 'DEFINED-ON-LINE '1596) 
(PUT 'OFSF_COEFFLIST1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_COEFFLIST1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_COEFFLIST1 (P X D)
    (COND
     ((AND (OR (OR (ATOM P) (ATOM (CAR P))) (NEQ (CAAAR P) X)) (EQUAL D 0))
      (LIST P))
     ((AND (NOT (OR (ATOM P) (ATOM (CAR P)))) (EQUAL (CDAAR P) D))
      (CONS (CDAR P) (OFSF_COEFFLIST1 (CDR P) X (DIFFERENCE D 1))))
     (T (CONS NIL (OFSF_COEFFLIST1 P X (DIFFERENCE D 1)))))) 
(PUT 'OFSF_BUILDQ 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_BUILDQ 'DEFINED-ON-LINE '1606) 
(PUT 'OFSF_BUILDQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDQ (VDPGB HE VSBASIS BETA)
    (COND (*RLHQESTRCONST (OFSF_BUILDQSC VDPGB HE VSBASIS BETA))
          (T (OFSF_BUILDQ1 VDPGB HE VSBASIS)))) 
(PUT 'OFSF_BUILDQ1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_BUILDQ1 'DEFINED-ON-LINE '1615) 
(PUT 'OFSF_BUILDQ1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDQ1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDQ1 (VDPGB HE VSBASIS)
    (PROG (REDHE Q DIM I)
      (SETQ I 0)
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "computing matrix Q of dimension " (LENGTH VSBASIS)))))
      (SETQ REDHE HE)
      (SETQ Q
              (PROG (VLIST FORALL-RESULT FORALL-ENDPTR)
                (SETQ VLIST VSBASIS)
                (COND ((NULL VLIST) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (VJ FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ VJ VLIST)
                                   (COND ((NULL VJ) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (VJ)
                                                       (PREPSQ
                                                        (OFSF_TRACE VDPGB REDHE
                                                         (CAR VLIST) VJ
                                                         VSBASIS)))
                                                     (CAR VJ))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ VJ (CDR VJ))
                                   (COND ((NULL VJ) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (VJ)
                                               (PREPSQ
                                                (OFSF_TRACE VDPGB REDHE
                                                 (CAR VLIST) VJ VSBASIS)))
                                             (CAR VJ))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ VLIST (CDR VLIST))
                (COND ((NULL VLIST) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (VJ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ VJ VLIST)
                           (COND ((NULL VJ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (VJ)
                                               (PREPSQ
                                                (OFSF_TRACE VDPGB REDHE
                                                 (CAR VLIST) VJ VSBASIS)))
                                             (CAR VJ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ VJ (CDR VJ))
                           (COND ((NULL VJ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (VJ)
                                       (PREPSQ
                                        (OFSF_TRACE VDPGB REDHE (CAR VLIST) VJ
                                         VSBASIS)))
                                     (CAR VJ))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q
              (PROG (LINE FORALL-RESULT FORALL-ENDPTR)
                (SETQ LINE Q)
                (COND ((NULL LINE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (LINE)
                                    (PROGN
                                     (SETQ I (PLUS I 1))
                                     (NCONC
                                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ J 1)
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DIFFERENCE I 1) J))
                                          (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS NIL NIL)))
                                       LOOPLABEL
                                        (SETQ J (PLUS2 J 1))
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DIFFERENCE I 1) J))
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))
                                      LINE)))
                                  (CAR LINE))
                                 NIL)))
               LOOPLABEL
                (SETQ LINE (CDR LINE))
                (COND ((NULL LINE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (LINE)
                            (PROGN
                             (SETQ I (PLUS I 1))
                             (NCONC
                              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                (SETQ J 1)
                                (COND
                                 ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                  (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR (CONS NIL NIL)))
                               LOOPLABEL
                                (SETQ J (PLUS2 J 1))
                                (COND
                                 ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))
                              LINE)))
                          (CAR LINE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIM (LENGTH VSBASIS))
      (PROG (Y)
        (SETQ Y 2)
       LAB
        (COND ((MINUSP (DIFFERENCE DIM Y)) (RETURN NIL)))
        (PROG (X)
          (SETQ X 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (DIFFERENCE Y 1) X)) (RETURN NIL)))
          (SETCAR (PNTH (NTH Q Y) X) (NTH (NTH Q X) Y))
          (SETQ X (PLUS2 X 1))
          (GO LAB))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (COND (*RLHQEVB (IOTO_TPRIN2 " done")))
      (RETURN Q))) 
(PUT 'OFSF_BUILDHELIST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDHELIST 'DEFINED-ON-LINE '1639) 
(PUT 'OFSF_BUILDHELIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDHELIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDHELIST (GREATERLIST NEQLIST)
    (PROG (HELIST CHI1 GLIST)
      (COND ((AND (NULL GREATERLIST) (NULL NEQLIST)) (RETURN (LIST 1))))
      (SETQ CHI1 1)
      (PROG (ELEM)
        (SETQ ELEM NEQLIST)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (SETQ CHI1
                   ((LAMBDA (G416)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF CHI1 G416))
                            (T (POLY-MULTF CHI1 G416))))
                    (EXPTF ELEM 2))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (SETQ GLIST (OFSF_BUILDGLIST GREATERLIST))
      (SETQ HELIST
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GLIST)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (COND
                                     (*PHYSOP-LOADED (PHYSOP-MULTF ELEM CHI1))
                                     (T (POLY-MULTF ELEM CHI1))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF ELEM CHI1))
                                  (T (POLY-MULTF ELEM CHI1))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN HELIST))) 
(PUT 'OFSF_BUILDGLIST 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_BUILDGLIST 'DEFINED-ON-LINE '1654) 
(PUT 'OFSF_BUILDGLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDGLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_BUILDGLIST (GREATERLIST)
    (PROG (RECRES)
      (COND ((NULL GREATERLIST) (RETURN (LIST 1)))
            ((EQUAL (LENGTH GREATERLIST) 1)
             (RETURN (LIST (EXPTF (CAR GREATERLIST) 2) (CAR GREATERLIST)))))
      (SETQ RECRES (OFSF_BUILDGLIST (CDR GREATERLIST)))
      (RETURN
       (APPEND (OFSF_BUILDGLIST1 (EXPTF (CAR GREATERLIST) 2) RECRES)
               (OFSF_BUILDGLIST1 (CAR GREATERLIST) RECRES))))) 
(PUT 'OFSF_BUILDGLIST1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BUILDGLIST1 'DEFINED-ON-LINE '1667) 
(PUT 'OFSF_BUILDGLIST1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDGLIST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDGLIST1 (POL POLLIST)
    (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
      (SETQ POLY POLLIST)
      (COND ((NULL POLY) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (POLY)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF POL POLY))
                                (T (POLY-MULTF POL POLY))))
                        (CAR POLY))
                       NIL)))
     LOOPLABEL
      (SETQ POLY (CDR POLY))
      (COND ((NULL POLY) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (POLY)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF POL POLY))
                        (T (POLY-MULTF POL POLY))))
                (CAR POLY))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_REDGROEBNER 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_REDGROEBNER 'DEFINED-ON-LINE '1676) 
(PUT 'OFSF_REDGROEBNER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_REDGROEBNER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_REDGROEBNER (VDPGB)
    (PROG (H F F0 EVF0)
      (SETQ F VDPGB)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL F))) (RETURN NIL)))
        (PROGN
         (SETQ F0 (CAR F))
         (SETQ EVF0 (CADR F0))
         (SETQ F (CDR F))
         (COND
          ((AND (NOT (GB_SEARCHINLIST EVF0 F)) (NOT (GB_SEARCHINLIST EVF0 H)))
           (SETQ H (CONS F0 H)))))
        (GO WHILELABEL))
      (RETURN H))) 
(PUT 'OFSF_BUILDQSC 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_BUILDQSC 'DEFINED-ON-LINE '1693) 
(PUT 'OFSF_BUILDQSC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_BUILDQSC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BUILDQSC (VDPGB HE VSBASIS BETA)
    (PROG (REDHE Q DIM I)
      (SETQ I 0)
      (COND
       (*RLHQEVB
        (IOTO_TPRIN2
         (LIST "computing matrix Q of dimension " (LENGTH VSBASIS)))))
      (SETQ REDHE (GB_REDUCE (VDP_F2VDP HE) VDPGB))
      (SETQ Q
              (PROG (VLIST FORALL-RESULT FORALL-ENDPTR)
                (SETQ VLIST VSBASIS)
                (COND ((NULL VLIST) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (VJ FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ VJ VLIST)
                                   (COND ((NULL VJ) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (VJ)
                                                       (MK*SQ
                                                        (OFSF_TRACESC REDHE
                                                         (CAR VLIST) VJ VSBASIS
                                                         BETA)))
                                                     (CAR VJ))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ VJ (CDR VJ))
                                   (COND ((NULL VJ) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (VJ)
                                               (MK*SQ
                                                (OFSF_TRACESC REDHE (CAR VLIST)
                                                 VJ VSBASIS BETA)))
                                             (CAR VJ))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ VLIST (CDR VLIST))
                (COND ((NULL VLIST) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (VJ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ VJ VLIST)
                           (COND ((NULL VJ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (VJ)
                                               (MK*SQ
                                                (OFSF_TRACESC REDHE (CAR VLIST)
                                                 VJ VSBASIS BETA)))
                                             (CAR VJ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ VJ (CDR VJ))
                           (COND ((NULL VJ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (VJ)
                                       (MK*SQ
                                        (OFSF_TRACESC REDHE (CAR VLIST) VJ
                                         VSBASIS BETA)))
                                     (CAR VJ))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q
              (PROG (LINE FORALL-RESULT FORALL-ENDPTR)
                (SETQ LINE Q)
                (COND ((NULL LINE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (LINE)
                                    (PROGN
                                     (SETQ I (PLUS I 1))
                                     (NCONC
                                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ J 1)
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DIFFERENCE I 1) J))
                                          (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS NIL NIL)))
                                       LOOPLABEL
                                        (SETQ J (PLUS2 J 1))
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DIFFERENCE I 1) J))
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))
                                      LINE)))
                                  (CAR LINE))
                                 NIL)))
               LOOPLABEL
                (SETQ LINE (CDR LINE))
                (COND ((NULL LINE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (LINE)
                            (PROGN
                             (SETQ I (PLUS I 1))
                             (NCONC
                              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                (SETQ J 1)
                                (COND
                                 ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                  (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR (CONS NIL NIL)))
                               LOOPLABEL
                                (SETQ J (PLUS2 J 1))
                                (COND
                                 ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))
                              LINE)))
                          (CAR LINE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIM (LENGTH VSBASIS))
      (PROG (Y)
        (SETQ Y 2)
       LAB
        (COND ((MINUSP (DIFFERENCE DIM Y)) (RETURN NIL)))
        (PROG (X)
          (SETQ X 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (DIFFERENCE Y 1) X)) (RETURN NIL)))
          (SETCAR (PNTH (NTH Q Y) X) (NTH (NTH Q X) Y))
          (SETQ X (PLUS2 X 1))
          (GO LAB))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (COND (*RLHQEVB (IOTO_TPRIN2 " done")))
      (RETURN Q))) 
(PUT 'OFSF_TRACESC 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRACESC 'DEFINED-ON-LINE '1718) 
(PUT 'OFSF_TRACESC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRACESC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRACESC (REDHE VI VJ VDPVSBASIS BETA)
    (PROG (RES)
      (SETQ RES (SIMP 0))
      (PROG (BK)
        (SETQ BK VDPVSBASIS)
       LAB
        (COND ((NULL BK) (RETURN NIL)))
        ((LAMBDA (BK)
           (SETQ RES
                   (ADDSQ RES (OFSF_TRACESC1 REDHE VI VJ BK VDPVSBASIS BETA))))
         (CAR BK))
        (SETQ BK (CDR BK))
        (GO LAB))
      (RETURN RES))) 
(PUT 'OFSF_TRACESC1 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_TRACESC1 'DEFINED-ON-LINE '1728) 
(PUT 'OFSF_TRACESC1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRACESC1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRACESC1 (REDHE VI VJ BK VDPVSBASIS BETA)
    (PROG (TRACEELEM VIVJBK)
      (SETQ TRACEELEM (SIMP 0))
      (SETQ VIVJBK (VDP_PROD VI (VDP_PROD VJ BK)))
      (PROG (BL)
        (SETQ BL VDPVSBASIS)
       LAB
        (COND ((NULL BL) (RETURN NIL)))
        ((LAMBDA (BL)
           (SETQ TRACEELEM
                   (ADDSQ TRACEELEM (OFSF_TRACESC2 REDHE BK BL VIVJBK BETA))))
         (CAR BL))
        (SETQ BL (CDR BL))
        (GO LAB))
      (RETURN TRACEELEM))) 
(PUT 'OFSF_TRACESC2 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRACESC2 'DEFINED-ON-LINE '1739) 
(PUT 'OFSF_TRACESC2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_TRACESC2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRACESC2 (REDHE BK BL VIVJBK BETA)
    (MULTSQ (GBSC_GETLINCOMBC BL REDHE)
            (GBSC_BETAGET BETA (VDP_PROD BL VIVJBK) BK))) 
(PUT 'OFSF_DIM 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DIM 'DEFINED-ON-LINE '1755) 
(PUT 'OFSF_DIM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_DIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DIM (GB VARL)
    (COND ((NULL GB) (LIST (LENGTH VARL) VARL))
          ((OFSF_PROPER GB VARL) (OFSF_DIMMAIN GB VARL))
          (T (LIST (MINUS 1) NIL)))) 
(PUT 'OFSF_PROPER 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PROPER 'DEFINED-ON-LINE '1767) 
(PUT 'OFSF_PROPER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_PROPER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PROPER (GB VARL)
    (COND ((NULL GB) T)
          ((INTERSECTION (KERNELS (CAR GB)) VARL) (OFSF_PROPER (CDR GB) VARL))
          (T NIL))) 
(PUT 'OFSF_DIMMAIN 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DIMMAIN 'DEFINED-ON-LINE '1778) 
(PUT 'OFSF_DIMMAIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_DIMMAIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DIMMAIN (GB VARL)
    (PROG (HTL M)
      (SETQ HTL (OFSF_HTL GB VARL))
      (SETQ M (OFSF_DIMREC HTL VARL 1 NIL NIL))
      (COND (*RLHQEGBDIMMIN (RETURN (OFSF_GETMIN M)))
            (T (RETURN (OFSF_GETMAX M)))))) 
(PUT 'OFSF_HTL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_HTL 'DEFINED-ON-LINE '1792) 
(PUT 'OFSF_HTL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_HTL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_HTL (GB VARL)
    (PROG (OLD VDPGB RES)
      (SETQ OLD (DIP_INIT VARL 'REVGRADLEX NIL))
      (SETQ VDPGB
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM GB)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELEM) (VDP_F2VDP ELEM)) (CAR ELEM))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM VDPGB)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (VDP_FMON (SIMP* 1) (CADR ELEM)))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (VDP_FMON (SIMP* 1) (CADR ELEM)))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM RES)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (DIP_2F (CAR (CDDDR ELEM))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (VDP_CLEANUP OLD)
      (RETURN RES))) 
(PUT 'OFSF_DIMREC 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_DIMREC 'DEFINED-ON-LINE '1805) 
(PUT 'OFSF_DIMREC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_DIMREC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DIMREC (S VARL K U M)
    (PROG (M2)
      (SETQ M2 M)
      (PROG (I)
        (SETQ I K)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VARL) I)) (RETURN NIL)))
        (COND
         ((NOT
           (OFSF_INTERSECTIONP (LTO_LIST2SET (CONS (OFSF_GETXI VARL I) U)) S))
          (PROGN
           (SETQ M2
                   (OFSF_DIMREC S VARL (PLUS I 1)
                    (LTO_LIST2SET (CONS (OFSF_GETXI VARL I) U)) M2)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NOT (OFSF_SUBSETP U M2)) (SETQ M2 (CONS U M2))))
      (RETURN M2))) 
(PUT 'OFSF_GETXI 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GETXI 'DEFINED-ON-LINE '1822) 
(PUT 'OFSF_GETXI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETXI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETXI (VARL I) (NTH VARL I)) 
(PUT 'OFSF_GETMAX 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GETMAX 'DEFINED-ON-LINE '1827) 
(PUT 'OFSF_GETMAX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETMAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GETMAX (M)
    (PROG (U D LENGTHU)
      (SETQ D 0)
      (SETQ LENGTHU 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL M))) (RETURN NIL)))
        (PROGN
         (SETQ LENGTHU (LENGTH (CAR M)))
         (COND
          ((GREATERP LENGTHU D) (PROGN (SETQ D LENGTHU) (SETQ U (CAR M)))))
         (SETQ M (CDR M)))
        (GO WHILELABEL))
      (RETURN (LIST D U)))) 
(PUT 'OFSF_GETMIN 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GETMIN 'DEFINED-ON-LINE '1844) 
(PUT 'OFSF_GETMIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_GETMIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GETMIN (M)
    (PROG (U D DU LENGTHU)
      (SETQ D 0)
      (SETQ DU 0)
      (SETQ LENGTHU 0)
      (COND
       ((NOT (NULL M))
        (PROGN
         (SETQ D (LENGTH (CAR M)))
         (SETQ DU D)
         (SETQ U (CAR M))
         (SETQ M (CDR M)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL M))) (RETURN NIL)))
        (PROGN
         (SETQ LENGTHU (LENGTH (CAR M)))
         (COND ((GREATERP LENGTHU D) (SETQ D LENGTHU))
               ((LESSP LENGTHU DU) (PROGN (SETQ DU LENGTHU) (SETQ U (CAR M)))))
         (SETQ M (CDR M)))
        (GO WHILELABEL))
      (RETURN (LIST D U)))) 
(PUT 'OFSF_INTERSECTIONP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_INTERSECTIONP 'DEFINED-ON-LINE '1869) 
(PUT 'OFSF_INTERSECTIONP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_INTERSECTIONP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_INTERSECTIONP (VL S)
    (PROG (NEWS FOUND)
      (SETQ NEWS S)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL NEWS))) (RETURN NIL)))
        (PROGN
         (COND
          ((SUBSETP (KERNELS (CAR NEWS)) VL)
           (PROGN (SETQ NEWS NIL) (SETQ FOUND T)))
          (T (SETQ NEWS (CDR NEWS)))))
        (GO WHILELABEL))
      (RETURN FOUND))) 
(PUT 'OFSF_SUBSETP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SUBSETP 'DEFINED-ON-LINE '1886) 
(PUT 'OFSF_SUBSETP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFHQE.RED) 
(PUT 'OFSF_SUBSETP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBSETP (U M2)
    (PROG (NEWM2 FOUND)
      (SETQ NEWM2 M2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL NEWM2))) (RETURN NIL)))
        (PROGN
         (COND
          ((SUBSETP U (CAR NEWM2)) (PROGN (SETQ NEWM2 NIL) (SETQ FOUND T)))
          (T (SETQ NEWM2 (CDR NEWM2)))))
        (GO WHILELABEL))
      (RETURN FOUND))) 
(ENDMODULE) 
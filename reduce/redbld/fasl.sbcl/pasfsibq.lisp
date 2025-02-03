(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFSIBQ)) 
(REVISION 'PASFSIBQ
          "$Id: pasfsibq.red 6013 2021-09-09 08:22:53Z thomas-sturm $") 
(COPYRIGHT 'PASFSIBQ "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'PASF_SIMPLIFYBOUNDEDQUANTIFIER 'NUMBER-OF-ARGS 3) 
(DE PASF_SIMPLIFYBOUNDEDQUANTIFIER (F KNOWL N)
    (PROG (B MTX)
      (SETQ KNOWL (RL_SMRMKNOWL KNOWL (CADR F)))
      (SETQ B (PASF_SIMPLB (CADDDR F) (CADR F)))
      (SETQ KNOWL (RL_SMUPDKNOWL 'AND (PASF_B2ATL B (CADR F)) KNOWL N))
      (SETQ MTX
              (CL_SIMPL1 (CADDR F) KNOWL (DIFFERENCE N 1)
                         (COND ((ATOM F) F) (T (CAR F)))))
      (SETQ F (PASF_SIMPLTB (COND ((ATOM F) F) (T (CAR F))) (CADR F) B MTX))
      (COND
       (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
         (COND ((ATOM F) F) (T (CAR F))))
        (SETQ F
                (PASF_SIMPLSTRB (COND ((ATOM F) F) (T (CAR F))) (CADR F)
                 (CADDDR F) (CADDR F)))))
      (RETURN
       (COND
        ((AND (NOT (OR (EQ F 'TRUE) (EQ F 'FALSE)))
              (NOT
               ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
                (COND ((ATOM F) F) (T (CAR F))))))
         (CL_SIMPL1 F NIL (MINUS 1) NIL))
        (T F))))) 
(PUT 'PASF_SIMPLTB 'NUMBER-OF-ARGS 4) 
(DE PASF_SIMPLTB (OP VAR B MTX)
    (PROG (BFVL)
      (COND ((EQ B 'FALSE) (RETURN (COND ((EQ OP 'BEX) 'FALSE) (T 'TRUE)))))
      (COND
       ((OR (AND (EQ MTX 'FALSE) (EQ OP 'BEX))
            (AND (EQ MTX 'TRUE) (EQ OP 'BALL)))
        (RETURN MTX)))
      (COND
       ((AND (NOT (MEMQ VAR (RL_FVARL MTX))) (PASF_BSATP B VAR)) (RETURN MTX)))
      (SETQ BFVL (CL_FVARL B))
      (COND
       ((AND (EQ (COND ((ATOM B) B) (T (CAR B))) 'EQUAL) (NULL (CDR BFVL))
             (EQCAR BFVL VAR))
        (RETURN (CL_SUBFOF (LIST (CONS VAR (CAR (PASF_B2TERML B VAR)))) MTX))))
      (RETURN (LIST OP VAR MTX B)))) 
(PUT 'PASF_SIMPLSTRB 'NUMBER-OF-ARGS 4) 
(DE PASF_SIMPLSTRB (OP VAR B MTX)
    (PROG (NEG ST LV VFL)
      (COND
       ((AND (CL_ATFP MTX) (EQUAL (LENGTH (CL_FVARL MTX)) 1)
             (EQ (CAR (CL_FVARL MTX)) VAR) (NOT (PASF_UNIVNLFP MTX VAR)))
        (PROGN
         (COND
          ((EQ OP 'BALL)
           (PROGN
            (SETQ B
                    (PASF_SIMPLB
                     ((LAMBDA (G148)
                        (COND ((AND G148 (CDR G148)) (CONS 'AND G148))
                              ((NULL G148)
                               (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                              (T (CAR G148))))
                      (CONS (CONS 'NOT (LIST MTX)) (LIST B)))
                     VAR))
            (SETQ MTX 'FALSE))))
         (COND
          ((EQ OP 'BEX)
           (PROGN
            (SETQ B
                    (PASF_SIMPLB
                     ((LAMBDA (G150)
                        (COND ((AND G150 (CDR G150)) (CONS 'AND G150))
                              ((NULL G150)
                               (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                              (T (CAR G150))))
                      (CONS MTX (LIST B)))
                     VAR))
            (SETQ MTX 'TRUE))))
         (RETURN (PASF_SIMPLTB OP VAR B MTX)))))
      (COND
       ((AND (EQ OP 'BALL) (EQ (COND ((ATOM MTX) MTX) (T (CAR MTX))) 'OR))
        (SETQ NEG 'NEGATE)))
      (COND
       ((AND (EQ OP 'BEX) (EQ (COND ((ATOM MTX) MTX) (T (CAR MTX))) 'AND))
        (SETQ NEG 'LEAVE)))
      (COND
       (NEG
        (PROGN
         (PROG (ARG)
           (SETQ ARG (CDR MTX))
          LAB
           (COND ((NULL ARG) (RETURN NIL)))
           ((LAMBDA (ARG)
              (PROGN
               (SETQ VFL (CL_FVARL ARG))
               (COND
                ((AND (EQUAL (LENGTH VFL) 1) (EQ (CAR VFL) VAR) (CL_ATFP ARG)
                      (NOT (PASF_UNIVNLFP ARG VAR)))
                 (SETQ ST
                         (CONS
                          (COND ((EQ NEG 'NEGATE) (CONS 'NOT (LIST ARG)))
                                (T ARG))
                          ST)))
                (T (SETQ LV (CONS ARG LV))))
               NIL))
            (CAR ARG))
           (SETQ ARG (CDR ARG))
           (GO LAB))
         (SETQ B
                 (PASF_SIMPLB
                  ((LAMBDA (G152)
                     (COND ((AND G152 (CDR G152)) (CONS 'AND G152))
                           ((NULL G152)
                            (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR G152))))
                   (CONS B ST))
                  VAR))
         (SETQ MTX
                 (COND
                  (LV
                   ((LAMBDA (G153)
                      (COND ((AND LV (CDR LV)) (CONS G153 LV))
                            ((NULL LV)
                             (COND ((EQ G153 'AND) 'TRUE) (T 'FALSE)))
                            (T (CAR LV))))
                    (COND ((ATOM MTX) MTX) (T (CAR MTX)))))
                  ((EQ OP 'BALL) 'FALSE) (T 'TRUE)))
         (RETURN (PASF_SIMPLTB OP VAR B MTX)))))
      (RETURN (LIST OP VAR MTX B)))) 
(PUT 'PASF_SIMPLB 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SIMPLB 'DEFINED-ON-LINE '128) 
(PUT 'PASF_SIMPLB 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIBQ.RED) 
(PUT 'PASF_SIMPLB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SIMPLB (F VAR)
    (PROG (SB NSB FLG ARGN ARGNA)
      (SETQ F (PASF_DNF (CL_SIMPL F NIL (MINUS 1))))
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (COND
       ((OR (GREATERP (LENGTH (CL_FVARL F)) 1) (PASF_UNIVNLFP F VAR))
        (RETURN F)))
      (SETQ ARGN
              (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR) (CDR F))
                    (T (LIST F))))
      (PROG (ARG)
        (SETQ ARG ARGN)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (PROGN
            (SETQ FLG NIL)
            (SETQ ARGNA
                    (COND
                     ((EQ (COND ((ATOM ARG) ARG) (T (CAR ARG))) 'AND)
                      (CDR ARG))
                     (T (LIST ARG))))
            (PROG (A)
              (SETQ A ARGNA)
             LAB
              (COND ((NULL A) (RETURN NIL)))
              ((LAMBDA (A)
                 (COND
                  ((AND (PAIRP A) (PAIRP (CAR A))
                        (MEMQ (CAAR A) '(CONG NCONG)))
                   (SETQ FLG T))))
               (CAR A))
              (SETQ A (CDR A))
              (GO LAB))
            (COND ((NULL FLG) (SETQ SB (CONS ARG SB)))
                  (T (SETQ NSB (CONS ARG NSB))))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (SETQ SB
              (PASF_IVL2QFF
               (PASF_QFF2IVL
                (COND ((AND SB (CDR SB)) (CONS 'OR SB))
                      ((NULL SB) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                      (T (CAR SB))))
               VAR))
      (RETURN
       (CL_SIMPL
        ((LAMBDA (G156)
           (COND ((AND G156 (CDR G156)) (CONS 'OR G156))
                 ((NULL G156) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR G156))))
         (CONS SB NSB))
        NIL (MINUS 1))))) 
(PUT 'PASF_B2ATL 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_B2ATL 'DEFINED-ON-LINE '155) 
(PUT 'PASF_B2ATL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIBQ.RED) 
(PUT 'PASF_B2ATL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_B2ATL (B K)
    (COND
     ((OR (EQ B 'TRUE) (EQ B 'FALSE))
      (COND ((EQ B 'FALSE) (LIST)) (T (REDERR "pasf_b2atl: infinite bound"))))
     ((CL_ATFP B) (LIST B))
     ((EQ (COND ((ATOM B) B) (T (CAR B))) 'AND) (CDR B)))) 
(ENDMODULE) 
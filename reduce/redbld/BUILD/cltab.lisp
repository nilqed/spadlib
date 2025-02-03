(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLTAB)) 
(REVISION 'CLTAB "$Id: cltab.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'CLTAB "(c) 1995-2009 A. Dolzmann, T. Sturm, 2016-2017 T. Sturm") 
(RL_PROVIDESERVICE 'RL_TAB 'CL_TAB '(RL_A2CDL)) 
(PUT 'CL_TAB 'NUMBER-OF-ARGS 4) 
(DE CL_TAB (F CDL ITERATE ITERATEB)
    (COND (CDL (CL_TAB0 F CDL)) (ITERATE (CL_ITAB F ITERATEB)) (T (CL_ATAB F)))) 
(PUT 'CL_TAB0 'NUMBER-OF-ARGS 2) 
(DE CL_TAB0 (F CDL) (CL_MKTF (CL_TAB1 F CDL))) 
(PUT 'CL_TAB1 'NUMBER-OF-ARGS 2) 
(DE CL_TAB1 (F CDL)
    (PROG (W FF RESL)
      (PROG (ATF)
        (SETQ ATF CDL)
       LAB
        (COND ((NULL ATF) (RETURN NIL)))
        ((LAMBDA (ATF)
           (PROGN
            (SETQ FF (RL_SIMPL F (LIST ATF) (MINUS 1)))
            (COND
             ((AND (NEQ FF 'INCTHEO) (NEQ FF 'FALSE))
              (COND
               ((SETQ W (ASSOC FF RESL))
                (SETCDR W
                        (RL_SIMPL (CONS 'OR (LIST ATF (CDR W))) NIL
                                  (MINUS 1))))
               (T (SETQ RESL (CONS (CONS FF ATF) RESL))))))))
         (CAR ATF))
        (SETQ ATF (CDR ATF))
        (GO LAB))
      (RETURN RESL))) 
(PUT 'CL_MKTF 'NUMBER-OF-ARGS 1) 
(DE CL_MKTF (RESL)
    (PROG (W FLG)
      (SETQ W RESL)
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (COND
         ((OR (OR (EQ (CAAR W) 'TRUE) (EQ (CAAR W) 'FALSE)) (CL_ATFP (CAAR W)))
          (PROGN (SETQ W NIL) (SETQ FLG T)))
         (T (SETQ W (CDR W))))
        (GO WHILELABEL))
      (RETURN
       (COND
        (FLG
         (RL_SIMPL
          ((LAMBDA (G166)
             (COND ((AND G166 (CDR G166)) (CONS 'OR G166))
                   ((NULL G166) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                   (T (CAR G166))))
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X RESL)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (CONS 'AND (LIST (CDR X) (CAR X))))
                               (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (X) (CONS 'AND (LIST (CDR X) (CAR X)))) (CAR X))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          NIL (MINUS 1)))
        (T
         ((LAMBDA (G168)
            (COND ((AND G168 (CDR G168)) (CONS 'OR G168))
                  ((NULL G168) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G168))))
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X RESL)
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X) (CONS 'AND (LIST (CDR X) (CAR X))))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X) (CONS 'AND (LIST (CDR X) (CAR X)))) (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))))))) 
(PUT 'CL_ATAB 'NUMBER-OF-ARGS 1) 
(DE CL_ATAB (F)
    (PROG (W) (SETQ W (CL_ATAB1 F)) (RETURN (COND (W (CL_MKTF W)) (T F))))) 
(PUT 'CL_ATAB1 'NUMBER-OF-ARGS 1) 
(DE CL_ATAB1 (F)
    (PROG (CDL CDLL ATNUM ATNUMOLD ATNUMNF NRESL RESL DPTH)
      (SETQ ATNUM (CL_ATNUM F))
      (SETQ ATNUMOLD ATNUM)
      (SETQ CDLL (RL_A2CDL (CL_ATML F)))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T (LIST ATNUM " = 100%"))
         (SETQ DPTH (LENGTH CDLL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT CDLL) (RETURN NIL)))
        (PROGN
         (SETQ CDL (CAR CDLL))
         (SETQ CDLL (CDR CDLL))
         (SETQ NRESL (CL_TAB1 F CDL))
         (SETQ ATNUMNF (CL_ATNUM (CL_MKTF NRESL)))
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_PRIN2 (LIST "[" DPTH ": " ATNUMNF "] "))
            (SETQ DPTH (DIFFERENCE DPTH 1)))))
         (COND
          ((LESSP ATNUMNF ATNUM)
           (PROGN (SETQ RESL NRESL) (SETQ ATNUM ATNUMNF)))))
        (GO WHILELABEL))
      (COND
       (*RLVERBOSE
        (COND
         ((LESSP ATNUM ATNUMOLD)
          (IOTO_TPRIN2T (LIST "Success: " ATNUMOLD " -> " ATNUM)))
         (T
          (IOTO_TPRIN2T
           (LIST "No success, returning the original formula"))))))
      (RETURN RESL))) 
(PUT 'CL_ITAB 'NUMBER-OF-ARGS 2) 
(DE CL_ITAB (F ITERATEB) (COND (ITERATEB (CL_ITAB2 F)) (T (CL_ITAB1 F)))) 
(PUT 'CL_ITAB1 'NUMBER-OF-ARGS 1) 
(DE CL_ITAB1 (F)
    (PROG (W RES)
      (SETQ W (CL_ATAB1 F))
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (PROGN
         (SETQ RES (CL_MKTF W))
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "Recomputing tableau."))))
         (SETQ W (CL_ATAB1 RES)))
        (GO WHILELABEL))
      (RETURN (OR RES F)))) 
(PUT 'CL_ITAB2 'NUMBER-OF-ARGS 1) 
(DE CL_ITAB2 (F)
    (PROG (W)
      (SETQ W (CL_ATAB1 F))
      (RETURN
       (COND
        (W
         (CL_MKTF
          (PROG (RES FORALL-RESULT FORALL-ENDPTR)
            (SETQ RES W)
            (COND ((NULL RES) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (RES)
                                (CONS (CL_ITAB2 (CAR RES)) (CDR RES)))
                              (CAR RES))
                             NIL)))
           LOOPLABEL
            (SETQ RES (CDR RES))
            (COND ((NULL RES) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (RES) (CONS (CL_ITAB2 (CAR RES)) (CDR RES)))
                      (CAR RES))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))
        (T F))))) 
(RL_PROVIDESERVICE 'RL_GENTHEO 'CL_GENTHEO '(RL_GETINEQ)) 
(PUT 'CL_GENTHEO 'NUMBER-OF-ARGS 3) 
(DE CL_GENTHEO (THEO F BVL)
    (PROG (W)
      (SETQ W (CL_GENTHEO0 F BVL))
      (RETURN (CONS (RL_THSIMPL (UNION THEO (CAR W))) (CDR W))))) 
(PUT 'CL_GENTHEO0 'NUMBER-OF-ARGS 2) 
(DE CL_GENTHEO0 (F BVL)
    (PROG (W RES THEO)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR (SETQ W (CL_GENTHEO1 F BVL)))) (RETURN NIL)))
        (PROGN
         (SETQ RES (CDR W))
         (SETQ THEO (CONS (CDR RES) THEO))
         (SETQ F (CAR RES)))
        (GO WHILELABEL))
      (RETURN (CONS THEO F)))) 
(PUT 'CL_GENTHEO1 'NUMBER-OF-ARGS 2) 
(DE CL_GENTHEO1 (F BVL)
    (PROG (CDL RESULT NRES FLAG THEO)
      (SETQ RESULT F)
      (SETQ CDL (RL_GETINEQ (RL_NNF F) BVL))
      (PROG (INEQ)
        (SETQ INEQ CDL)
       LAB
        (COND ((NULL INEQ) (RETURN NIL)))
        ((LAMBDA (INEQ)
           (PROGN
            (SETQ NRES (CONS (RL_SIMPL F (LIST INEQ) (MINUS 1)) INEQ))
            (COND
             ((NOT (CL_CMPFP RESULT (CAR NRES)))
              (PROGN
               (SETQ RESULT (CAR NRES))
               (SETQ THEO (CDR NRES))
               (SETQ FLAG T))))))
         (CAR INEQ))
        (SETQ INEQ (CDR INEQ))
        (GO LAB))
      (RETURN (CONS FLAG (CONS RESULT THEO))))) 
(PUT 'CL_CMPFP 'NUMBER-OF-ARGS 2) 
(DE CL_CMPFP (F NF) (LESSP (CL_ATNUM F) (CL_ATNUM NF))) 
(ENDMODULE) 
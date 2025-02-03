(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFDET)) 
(REVISION 'OFSFDET "$Id: ofsfdet.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFDET "(c) 2003-2009 A. Dolzmann, A. Seidl, and T. Sturm") 
(PUT 'OFSF_DET 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_DET 'DEFINED-ON-LINE '32) 
(PUT 'OFSF_DET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_DET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_DET (M) (COND (*RLOURDET (OFSF_NEWBAREISS2 M)) (T (OFSF_BAREISS M)))) 
(PUT 'OFSF_BAREISS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_BAREISS 'DEFINED-ON-LINE '41) 
(PUT 'OFSF_BAREISS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_BAREISS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_BAREISS (NU)
    (PROG (BU N OK V *EXP)
      (SETQ *EXP T)
      (SETQ NU
              (PROG (LINE FORALL-RESULT FORALL-ENDPTR)
                (SETQ LINE NU)
                (COND ((NULL LINE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (LINE)
                                    (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ ELEM LINE)
                                      (COND ((NULL ELEM) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (ELEM) ELEM)
                                                        (CAR ELEM))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ ELEM (CDR ELEM))
                                      (COND
                                       ((NULL ELEM) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELEM) ELEM)
                                                (CAR ELEM))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR LINE))
                                 NIL)))
               LOOPLABEL
                (SETQ LINE (CDR LINE))
                (COND ((NULL LINE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (LINE)
                            (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                              (SETQ ELEM LINE)
                              (COND ((NULL ELEM) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELEM) ELEM)
                                                (CAR ELEM))
                                               NIL)))
                             LOOPLABEL
                              (SETQ ELEM (CDR ELEM))
                              (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (ELEM) ELEM) (CAR ELEM))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR LINE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NU (SORT NU 'OFSF_LINESORT3))
      (SETQ N (LENGTH NU))
      (COND ((EQN N 1) (RETURN (CAAR NU))))
      (SETQ V
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (GENSYM) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (GENSYM) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ OK (SETKORDER (APPEND V KORD*)))
      (SETQ NU
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R NU)
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (R) (PRSUM V R)) (CAR R)) NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (R) (PRSUM V R)) (CAR R)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BU (CDR (SPARSE_BAREISS NU V BAREISS-STEP-SIZE*)))
      (SETQ BU (COND ((EQUAL (LENGTH BU) N) (CDAR (CAR BU))) (T NIL)))
      (SETKORDER OK)
      (RETURN BU))) 
(PUT 'OFSF_LINESORT1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_LINESORT1 'DEFINED-ON-LINE '67) 
(PUT 'OFSF_LINESORT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_LINESORT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_LINESORT1 (L1 L2)
    (COND ((NULL L1) T) ((AND (NULL (CAR L1)) (NOT (NULL (CAR L2)))) T)
          ((AND (NOT (NULL (CAR L1))) (NULL (CAR L2))) NIL)
          (T (OFSF_LINESORT1 (CDR L1) (CDR L2))))) 
(PUT 'OFSF_LINESORT2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_LINESORT2 'DEFINED-ON-LINE '76) 
(PUT 'OFSF_LINESORT2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_LINESORT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_LINESORT2 (L1 L2)
    (PROG (Z1 Z2)
      (SETQ Z1
              (PROG (X FORALL-RESULT)
                (SETQ X L1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (X) (COND ((NULL X) 1) (T 0))) (CAR X))
                              FORALL-RESULT))
                (SETQ X (CDR X))
                (GO LAB1)))
      (SETQ Z2
              (PROG (X FORALL-RESULT)
                (SETQ X L2)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (X) (COND ((NULL X) 1) (T 0))) (CAR X))
                              FORALL-RESULT))
                (SETQ X (CDR X))
                (GO LAB1)))
      (COND ((GREATERP Z1 Z2) (RETURN T)))
      (COND ((GREATERP Z2 Z1) (RETURN NIL)))
      (RETURN (OFSF_LINESORT1 L1 L2)))) 
(PUT 'OFSF_LINESORT3 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_LINESORT3 'DEFINED-ON-LINE '87) 
(PUT 'OFSF_LINESORT3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_LINESORT3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_LINESORT3 (L1 L2) (NOT (OFSF_LINESORT2 L1 L2))) 
(PUT 'OFSF_NEWBAREISS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NEWBAREISS 'DEFINED-ON-LINE '90) 
(PUT 'OFSF_NEWBAREISS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_NEWBAREISS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NEWBAREISS (M)
    (PROG (VM W MIK MKK MK1K1 N)
      (SETQ N 0)
      (SETQ N (LENGTH M))
      (SETQ VM (VMAT_MK M))
      (VMAT_PUT VM 0 0 (CAR (SIMP 1)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K)) (RETURN NIL)))
        (PROGN
         (IOTO_PRIN2 (LIST "[" (DIFFERENCE (DIFFERENCE N 1) K) "] "))
         (SETQ W (OFSF_GOODLCPAIR VM K N))
         (COND ((NOT W) (REDERR "zero determinant")))
         (COND
          ((NOT (EQN K (CDR W)))
           (PROGN
            (VMAT_SWAPC VM K (CDR W))
            (IOTO_PRIN2 (LIST "(" (CDR W) "<-c->" K ")")))))
         (COND
          ((NOT (EQN K (CAR W)))
           (PROGN
            (VMAT_SWAPL VM K (CAR W))
            (IOTO_PRIN2 (LIST "(" (CAR W) "<-l->" K ")")))))
         (SETQ MKK (VMAT_GET VM K K))
         (SETQ MK1K1 (VMAT_GET VM (DIFFERENCE K 1) (DIFFERENCE K 1)))
         (PROG (I)
           (SETQ I (PLUS K 1))
          LAB
           (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
           (PROGN
            (SETQ MIK (VMAT_GET VM I K))
            (PROG (J)
              (SETQ J (PLUS K 1))
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (PROGN
               (SETQ W
                       (ADDF
                        ((LAMBDA (G339)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G339 MKK))
                                 (T (POLY-MULTF G339 MKK))))
                         (VMAT_GET VM I J))
                        (NEGF
                         ((LAMBDA (G342)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF MIK G342))
                                  (T (POLY-MULTF MIK G342))))
                          (VMAT_GET VM K J)))))
               (COND (W (SETQ W (QUOTFX W MK1K1))))
               (VMAT_PUT VM I J W))
              (SETQ J (PLUS2 J 1))
              (GO LAB)))
           (SETQ I (PLUS2 I 1))
           (GO LAB)))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (RETURN (VMAT_GET VM N N)))) 
(PUT 'OFSF_NEWBAREISS2 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NEWBAREISS2 'DEFINED-ON-LINE '128) 
(PUT 'OFSF_NEWBAREISS2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_NEWBAREISS2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NEWBAREISS2 (M)
    (PROG (VM SIGN W K CNT DOIT N)
      (SETQ N 0)
      (SETQ N (LENGTH M))
      (SETQ VM (VMAT_MK M))
      (SETQ SIGN (CAR (SIMP 1)))
      (VMAT_PUT VM 0 0 SIGN)
      (COND (*RLVMATVB (IOTO_CTERPRI)))
      (SETQ CNT T)
      (SETQ K 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CNT (LESSP K N))) (RETURN NIL)))
        (PROGN
         (COND
          (*RLVMATVB (IOTO_PRIN2 (LIST "[" (DIFFERENCE (DIFFERENCE N 1) K)))))
         (SETQ W (OFSF_GOODLCPAIR VM K N))
         (COND
          ((NOT W)
           (PROGN (COND (*RLVMATVB (IOTO_PRIN2 "zero]"))) (SETQ CNT NIL)))
          (T
           (PROGN
            (SETQ SIGN (OFSF_BAREISS-PIVOT VM K W SIGN))
            (COND (DOIT (OFSF_BAREISS-STEP VM K N)))
            (COND (*RLVMATVB (IOTO_PRIN2 "] ")))
            (SETQ DOIT (NOT DOIT))
            (SETQ K (PLUS K 1))))))
        (GO WHILELABEL))
      (COND ((NOT CNT) (RETURN NIL)))
      (COND
       (DOIT
        (PROGN
         (COND (*RLVMATVB (IOTO_PRIN2 "[final")))
         (SETQ W
                 (OFSF_CDET2 (VMAT_GET VM (DIFFERENCE N 1) (DIFFERENCE N 1))
                  (VMAT_GET VM (DIFFERENCE N 1) N)
                  (VMAT_GET VM N (DIFFERENCE N 1)) (VMAT_GET VM N N)))
         (COND
          (W
           (SETQ W
                   (QUOTFX W
                           (VMAT_GET VM (DIFFERENCE N 2) (DIFFERENCE N 2))))))
         (VMAT_PUT VM N N W)
         (COND (*RLVMATVB (IOTO_PRIN2 "]"))))))
      (RETURN
       ((LAMBDA (G344)
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF SIGN G344))
                (T (POLY-MULTF SIGN G344))))
        (VMAT_GET VM N N))))) 
(PUT 'OFSF_BAREISS-PIVOT 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_BAREISS-PIVOT 'DEFINED-ON-LINE '170) 
(PUT 'OFSF_BAREISS-PIVOT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_BAREISS-PIVOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BAREISS-PIVOT (VM K W SIGN)
    (PROGN
     (COND
      ((NOT (EQN K (CDR W)))
       (PROGN
        (VMAT_SWAPC VM K (CDR W))
        (SETQ SIGN (NEGF SIGN))
        (COND (*RLVMATVB (IOTO_PRIN2 (LIST "(" (CDR W) "<-c->" K ")")))))))
     (COND
      ((NOT (EQN K (CAR W)))
       (PROGN
        (VMAT_SWAPL VM K (CAR W))
        (SETQ SIGN (NEGF SIGN))
        (COND (*RLVMATVB (IOTO_PRIN2 (LIST "(" (CAR W) "<-l->" K ")")))))))
     SIGN)) 
(PUT 'OFSF_BAREISS-STEP 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_BAREISS-STEP 'DEFINED-ON-LINE '187) 
(PUT 'OFSF_BAREISS-STEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_BAREISS-STEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_BAREISS-STEP (VM K N)
    (PROG (C0 CI1 CI2 W)
      (SETQ C0
              (OFSF_CDET2 (VMAT_GET VM (DIFFERENCE K 1) (DIFFERENCE K 1))
               (VMAT_GET VM (DIFFERENCE K 1) K)
               (VMAT_GET VM K (DIFFERENCE K 1)) (VMAT_GET VM K K)))
      (COND
       (C0
        (SETQ C0 (QUOTFX C0 (VMAT_GET VM (DIFFERENCE K 2) (DIFFERENCE K 2))))))
      (PROG (I)
        (SETQ I (PLUS K 1))
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ CI1
                 (NEGF
                  (OFSF_CDET2 (VMAT_GET VM (DIFFERENCE K 1) (DIFFERENCE K 1))
                   (VMAT_GET VM (DIFFERENCE K 1) K)
                   (VMAT_GET VM I (DIFFERENCE K 1)) (VMAT_GET VM I K))))
         (COND
          (CI1
           (SETQ CI1
                   (QUOTFX CI1
                           (VMAT_GET VM (DIFFERENCE K 2) (DIFFERENCE K 2))))))
         (SETQ CI2
                 (OFSF_CDET2 (VMAT_GET VM K (DIFFERENCE K 1)) (VMAT_GET VM K K)
                  (VMAT_GET VM I (DIFFERENCE K 1)) (VMAT_GET VM I K)))
         (COND
          (CI2
           (SETQ CI2
                   (QUOTFX CI2
                           (VMAT_GET VM (DIFFERENCE K 2) (DIFFERENCE K 2))))))
         (PROG (J)
           (SETQ J (PLUS K 1))
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (SETQ W
                    (ADDF
                     (ADDF
                      ((LAMBDA (G345)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G345 C0))
                               (T (POLY-MULTF G345 C0))))
                       (VMAT_GET VM I J))
                      ((LAMBDA (G347)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G347 CI1))
                               (T (POLY-MULTF G347 CI1))))
                       (VMAT_GET VM K J)))
                     ((LAMBDA (G349)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G349 CI2))
                              (T (POLY-MULTF G349 CI2))))
                      (VMAT_GET VM (DIFFERENCE K 1) J))))
            (COND
             (W
              (SETQ W
                      (QUOTFX W
                              (VMAT_GET VM (DIFFERENCE K 2)
                               (DIFFERENCE K 2))))))
            (VMAT_PUT VM I J W))
           (SETQ J (PLUS2 J 1))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ W
              (OFSF_CDET2 (VMAT_GET VM (DIFFERENCE K 1) (DIFFERENCE K 1))
               (VMAT_GET VM (DIFFERENCE K 1) K)
               (VMAT_GET VM K (DIFFERENCE K 1)) (VMAT_GET VM K K)))
      (COND
       (W (SETQ W (QUOTFX W (VMAT_GET VM (DIFFERENCE K 2) (DIFFERENCE K 2))))))
      (VMAT_PUT VM K K W))) 
(PUT 'OFSF_CDET2 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_CDET2 'DEFINED-ON-LINE '219) 
(PUT 'OFSF_CDET2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_CDET2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CDET2 (A11 A12 A21 A22)
    (ADDF
     (COND (*PHYSOP-LOADED (PHYSOP-MULTF A11 A22)) (T (POLY-MULTF A11 A22)))
     (NEGF
      (COND (*PHYSOP-LOADED (PHYSOP-MULTF A12 A21)) (T (POLY-MULTF A12 A21)))))) 
(PUT 'OFSF_GOODLINE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GOODLINE 'DEFINED-ON-LINE '222) 
(PUT 'OFSF_GOODLINE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_GOODLINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GOODLINE (M K N)
    (PROG (BESTL MAXZ Z)
      (SETQ BESTL 0)
      (SETQ MAXZ 0)
      (SETQ Z 0)
      (SETQ MAXZ (MINUS 1))
      (PROG (L)
        (SETQ L K)
       LAB
        (COND ((MINUSP (DIFFERENCE N L)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (NULL (VMAT_GET M L K)))
           (PROGN
            (SETQ Z
                    (PROG (J FORALL-RESULT)
                      (SETQ J (PLUS K 1))
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (PLUS (COND ((NULL (VMAT_GET M L J)) 1) (T 0))
                                    FORALL-RESULT))
                      (SETQ J (PLUS2 J 1))
                      (GO LAB1)))
            (COND ((GREATERP Z MAXZ) (PROGN (SETQ MAXZ Z) (SETQ BESTL L))))))))
        (SETQ L (PLUS2 L 1))
        (GO LAB))
      (COND ((NOT (EQN MAXZ (MINUS 1))) (RETURN BESTL))))) 
(PUT 'OFSF_GOODCOLUMN 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GOODCOLUMN 'DEFINED-ON-LINE '238) 
(PUT 'OFSF_GOODCOLUMN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_GOODCOLUMN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GOODCOLUMN (M K N)
    (PROG (BESTC MAXZ Z)
      (SETQ BESTC 0)
      (SETQ MAXZ 0)
      (SETQ Z 0)
      (SETQ MAXZ (MINUS 1))
      (PROG (C)
        (SETQ C K)
       LAB
        (COND ((MINUSP (DIFFERENCE N C)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (NULL (VMAT_GET M K C)))
           (PROGN
            (SETQ Z
                    (PROG (I FORALL-RESULT)
                      (SETQ I (PLUS K 1))
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (PLUS (COND ((NULL (VMAT_GET M I C)) 1) (T 0))
                                    FORALL-RESULT))
                      (SETQ I (PLUS2 I 1))
                      (GO LAB1)))
            (COND ((GREATERP Z MAXZ) (PROGN (SETQ MAXZ Z) (SETQ BESTC C))))))))
        (SETQ C (PLUS2 C 1))
        (GO LAB))
      (COND ((NOT (EQN MAXZ (MINUS 1))) (RETURN BESTC))))) 
(PUT 'OFSF_GOODLCPAIR 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GOODLCPAIR 'DEFINED-ON-LINE '273) 
(PUT 'OFSF_GOODLCPAIR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_GOODLCPAIR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GOODLCPAIR (M K N)
    (PROG (BESTLC MINZ MIN1Z CZ LZ Z)
      (SETQ MINZ 0)
      (SETQ MIN1Z 0)
      (SETQ CZ 0)
      (SETQ LZ 0)
      (SETQ Z 0)
      (SETQ MINZ (SETQ MIN1Z (PLUS (TIMES 6 N) 1)))
      (PROG (I)
        (SETQ I K)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J K)
         LAB
          (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
          (COND
           ((VMAT_GET M I J)
            (PROGN
             (SETQ LZ
                     (PROG (JJ FORALL-RESULT)
                       (SETQ JJ (PLUS K 1))
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((MINUSP (DIFFERENCE N JJ)) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (PLUS (OFSF_QUALITY (VMAT_GET M I JJ))
                                     FORALL-RESULT))
                       (SETQ JJ (PLUS2 JJ 1))
                       (GO LAB1)))
             (SETQ CZ
                     (PROG (II FORALL-RESULT)
                       (SETQ II (PLUS K 1))
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((MINUSP (DIFFERENCE N II)) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (PLUS (OFSF_QUALITY (VMAT_GET M II J))
                                     FORALL-RESULT))
                       (SETQ II (PLUS2 II 1))
                       (GO LAB1)))
             (SETQ Z (PLUS LZ CZ))
             (COND
              ((OR (LESSP Z MINZ) (AND (EQN Z MINZ) (LESSP (MAX LZ CZ) MIN1Z)))
               (PROGN
                (SETQ MINZ Z)
                (SETQ MIN1Z (MAX LZ CZ))
                (SETQ BESTLC (CONS I J))))))))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NOT (EQN MINZ (PLUS (TIMES 6 N) 1))) (RETURN BESTLC))))) 
(PUT 'OFSF_QUALITY 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QUALITY 'DEFINED-ON-LINE '292) 
(PUT 'OFSF_QUALITY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'OFSF_QUALITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QUALITY (F) (COND ((NULL F) 1) ((NUMBERP F) 0) (T 0))) 
(PUT 'VMAT_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'VMAT_PRINT 'DEFINED-ON-LINE '300) 
(PUT 'VMAT_PRINT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VMAT_PRINT (VM) (MATHPRINT (VMAT_PREP VM))) 
(PUT 'VMAT_PREP 'NUMBER-OF-ARGS 1) 
(PUT 'VMAT_PREP 'DEFINED-ON-LINE '303) 
(PUT 'VMAT_PREP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_PREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VMAT_PREP (VM)
    (PROG (N)
      (SETQ N 0)
      (SETQ N (DIFFERENCE (UPBV (GETV VM 0)) 1))
      (RETURN
       (CONS 'MAT
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I 1)
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ J 1)
                                  (COND
                                   ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   (PREPF (VMAT_GET VM I J))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ J (PLUS2 J 1))
                                  (COND
                                   ((MINUSP (DIFFERENCE N J))
                                    (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS (PREPF (VMAT_GET VM I J)) NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))
                                NIL)))
              LOOPLABEL
               (SETQ I (PLUS2 I 1))
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J 1)
                          (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS (PREPF (VMAT_GET VM I J))
                                                NIL)))
                         LOOPLABEL
                          (SETQ J (PLUS2 J 1))
                          (COND
                           ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS (PREPF (VMAT_GET VM I J)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'VMAT_MK 'NUMBER-OF-ARGS 1) 
(PUT 'VMAT_MK 'DEFINED-ON-LINE '311) 
(PUT 'VMAT_MK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_MK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VMAT_MK (M)
    (PROG (LINE VMAT N I J)
      (SETQ N 0)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ N (LENGTH M))
      (SETQ VMAT (MKVECT (PLUS N 1)))
      (SETQ LINE (MKVECT (PLUS N 1)))
      (PUTV VMAT 0 LINE)
      (SETQ LINE (MKVECT (PLUS N 1)))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PUTV LINE J J)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PUTV VMAT (PLUS N 1) LINE)
      (PROG (L)
        (SETQ L M)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (PROGN
            (SETQ I (PLUS I 1))
            (SETQ LINE (MKVECT (PLUS N 1)))
            (SETQ J 0)
            (PROG (C)
              (SETQ C L)
             LAB
              (COND ((NULL C) (RETURN NIL)))
              ((LAMBDA (C) (PROGN (SETQ J (PLUS J 1)) (PUTV LINE J C)))
               (CAR C))
              (SETQ C (CDR C))
              (GO LAB))
            (PUTV VMAT I LINE)))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (RETURN VMAT))) 
(PUT 'VMAT_GET 'NUMBER-OF-ARGS 3) 
(PUT 'VMAT_GET 'DEFINED-ON-LINE '335) 
(PUT 'VMAT_GET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_GET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VMAT_GET (M I J) (GETV (GETV M I) (VMAT_CMAP M J))) 
(PUT 'VMAT_PUT 'NUMBER-OF-ARGS 4) 
(PUT 'VMAT_PUT 'DEFINED-ON-LINE '338) 
(PUT 'VMAT_PUT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_PUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VMAT_PUT (M I J C) (PUTV (GETV M I) (VMAT_CMAP M J) C)) 
(PUT 'VMAT_CMAP 'NUMBER-OF-ARGS 2) 
(PUT 'VMAT_CMAP 'DEFINED-ON-LINE '341) 
(PUT 'VMAT_CMAP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_CMAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VMAT_CMAP (M J) (GETV (GETV M (UPBV M)) J)) 
(PUT 'VMAT_SWAPL 'NUMBER-OF-ARGS 3) 
(PUT 'VMAT_SWAPL 'DEFINED-ON-LINE '344) 
(PUT 'VMAT_SWAPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_SWAPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VMAT_SWAPL (M I1 I2)
    (PROG (W) (SETQ W (GETV M I1)) (PUTV M I1 (GETV M I2)) (PUTV M I2 W))) 
(PUT 'VMAT_SWAPC 'NUMBER-OF-ARGS 3) 
(PUT 'VMAT_SWAPC 'DEFINED-ON-LINE '351) 
(PUT 'VMAT_SWAPC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'VMAT_SWAPC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VMAT_SWAPC (M J1 J2)
    (PROG (W MAP)
      (SETQ MAP (GETV M (UPBV M)))
      (SETQ W (GETV MAP J1))
      (PUTV MAP J1 (GETV MAP J2))
      (PUTV MAP J2 W))) 
(FLAG '(BDET) 'OPFN) 
(PUT 'BDET1 'NUMBER-OF-ARGS 1) 
(PUT 'BDET1 'DEFINED-ON-LINE '361) 
(PUT 'BDET1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'BDET1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BDET1 (M)
    (PREPF
     (OFSF_NEWBAREISS
      (PROG (L FORALL-RESULT FORALL-ENDPTR)
        (SETQ L (CDR M))
        (COND ((NULL L) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (L)
                            (PROG (C FORALL-RESULT FORALL-ENDPTR)
                              (SETQ C L)
                              (COND ((NULL C) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (C) (CAR (SIMP C)))
                                                (CAR C))
                                               NIL)))
                             LOOPLABEL
                              (SETQ C (CDR C))
                              (COND ((NULL C) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (C) (CAR (SIMP C))) (CAR C))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR L))
                         NIL)))
       LOOPLABEL
        (SETQ L (CDR L))
        (COND ((NULL L) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (L)
                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                      (SETQ C L)
                      (COND ((NULL C) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (C) (CAR (SIMP C))) (CAR C))
                                       NIL)))
                     LOOPLABEL
                      (SETQ C (CDR C))
                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (C) (CAR (SIMP C))) (CAR C)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR L))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(FLAG '(BDET1) 'OPFN) 
(PUT 'BDET 'NUMBER-OF-ARGS 1) 
(PUT 'BDET 'DEFINED-ON-LINE '368) 
(PUT 'BDET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'BDET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BDET (M)
    (PREPF
     (OFSF_NEWBAREISS2
      (PROG (L FORALL-RESULT FORALL-ENDPTR)
        (SETQ L (CDR M))
        (COND ((NULL L) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (L)
                            (PROG (C FORALL-RESULT FORALL-ENDPTR)
                              (SETQ C L)
                              (COND ((NULL C) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (C) (CAR (SIMP C)))
                                                (CAR C))
                                               NIL)))
                             LOOPLABEL
                              (SETQ C (CDR C))
                              (COND ((NULL C) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (C) (CAR (SIMP C))) (CAR C))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR L))
                         NIL)))
       LOOPLABEL
        (SETQ L (CDR L))
        (COND ((NULL L) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (L)
                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                      (SETQ C L)
                      (COND ((NULL C) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (C) (CAR (SIMP C))) (CAR C))
                                       NIL)))
                     LOOPLABEL
                      (SETQ C (CDR C))
                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (C) (CAR (SIMP C))) (CAR C)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR L))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(FLAG '(GMAT) 'OPFN) 
(PUT 'GMAT 'NUMBER-OF-ARGS 1) 
(PUT 'GMAT 'DEFINED-ON-LINE '375) 
(PUT 'GMAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDET.RED) 
(PUT 'GMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GMAT (N)
    (CONS 'MAT
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I 1)
            (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J 1)
                               (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS (MKID (MKID 'A I) J)
                                                     NIL)))
                              LOOPLABEL
                               (SETQ J (PLUS2 J 1))
                               (COND
                                ((MINUSP (DIFFERENCE N J))
                                 (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS (MKID (MKID 'A I) J) NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             NIL)))
           LOOPLABEL
            (SETQ I (PLUS2 I 1))
            (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                       (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS (MKID (MKID 'A I) J) NIL)))
                      LOOPLABEL
                       (SETQ J (PLUS2 J 1))
                       (COND
                        ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS (MKID (MKID 'A I) J) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(ENDMODULE) 
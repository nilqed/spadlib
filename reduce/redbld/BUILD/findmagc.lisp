(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FINDMAGC)) 
(FLUID '(*TRA MAGICLIST)) 
(PUT 'FINDMAGIC 'NUMBER-OF-ARGS 1) 
(PUT 'FINDMAGIC 'DEFINED-ON-LINE '32) 
(PUT 'FINDMAGIC 'DEFINED-IN-FILE 'ALGINT/FINDMAGC.RED) 
(PUT 'FINDMAGIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDMAGIC (L)
    (PROG (P N PVEC M INTVEC MCOUNT TEMP)
      (SETQ L
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U L)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (PROGN
                                     (MAPC MAGICLIST
                                           (FUNCTION
                                            (LAMBDA (V)
                                              (COND
                                               ((INVOLVESF (CDR U) V)
                                                (INTERR "Hard findmagic"))))))
                                     (CAR U)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (PROGN
                             (MAPC MAGICLIST
                                   (FUNCTION
                                    (LAMBDA (V)
                                      (COND
                                       ((INVOLVESF (CDR U) V)
                                        (INTERR "Hard findmagic"))))))
                             (CAR U)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "We must make the following non-zero:")
          (TERPRI)
          "We must make the following non-zero:")
         (MAPC L (FUNCTION PRINTSF))
         (PRINC "by suitable choice of ")
         ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) MAGICLIST))))
      (SETQ P 0)
      (SETQ N (ISUB1 (LENGTH MAGICLIST)))
      (SETQ PVEC (MKVECT N))
      (PUTV PVEC 0 2)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV PVEC I (NEXTPRIME (GETV PVEC (ISUB1 I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ INTVEC (MKVECT N))
     LOOP
      (SETQ P (IADD1 P))
      (COND
       (*TRA
        (PROGN (PRINC "We try the number ") (PROGN (PRIN2 P) (TERPRI) P))))
      (SETQ M P)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ MCOUNT 0)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (EQUAL (CDR (SETQ TEMP (DIVIDE M (GETV PVEC I)))) 0))
             (RETURN NIL)))
           (PROGN (SETQ MCOUNT (IADD1 MCOUNT)) (SETQ M (CAR TEMP)))
           (GO WHILELABEL))
         (PUTV INTVEC I MCOUNT))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NEQ M 1) (GO LOOP)))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "which corresponds to ")
          (TERPRI)
          "which corresponds to ")
         (SUPERPRINT INTVEC))))
      (SETQ M NIL)
      (SETQ TEMP MAGICLIST)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ M (CONS (CONS (CAR TEMP) (GETV INTVEC I)) M))
         (SETQ TEMP (CDR TEMP)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ TEMP L)
     LOOP2
      (COND ((NULL (CAR (ALGINT-SUBF (CAR TEMP) M))) (GO LOOP)))
      (SETQ TEMP (CDR TEMP))
      (COND (TEMP (GO LOOP2)))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "which corresponds to the values:")
          (TERPRI)
          "which corresponds to the values:")
         (SUPERPRINT M))))
      (RETURN M))) 
(ENDMODULE) 
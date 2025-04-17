(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GAUSS)) 
(FLUID '(*NOEQUIV ACCURACY* *EXPTEXPAND)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC ERFG*)) 
(PUT 'IGETV 'SETQFN '(LAMBDA (V I X) (IPUTV V I X))) 
(PUT 'S 'NUMBER-OF-ARGS 2) 
(PUT 'S 'DEFINED-ON-LINE '37) 
(PUT 'S 'DEFINED-IN-FILE 'NUMERIC/GAUSS.RED) 
(PUT 'S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'S 'SMACRO '(LAMBDA (I J) (IGETV (IGETV M I) J))) 
(PUT 'RDSOLVELIN 'NUMBER-OF-ARGS 2) 
(PUT 'RDSOLVELIN 'DEFINED-ON-LINE '39) 
(PUT 'RDSOLVELIN 'DEFINED-IN-FILE 'NUMERIC/GAUSS.RED) 
(PUT 'RDSOLVELIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RDSOLVELIN (U B)
    (PROG (N N1 K M W R X Y ERR)
      (SETQ N 0)
      (SETQ N1 0)
      (SETQ K 0)
      (SETQ N (LENGTH B))
      (SETQ N1 (IPLUS2 N 1))
      (SETQ M (MKVECT (IPLUS2 N 1)))
      (SETQ R (MKVECT (IPLUS2 N 1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ W (MKVECT (IPLUS2 N 2)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (IPUTV W J (NTH (NTH U I) J))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (IPUTV W N1 (NTH B I))
         (IPUTV M I W)
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I)) (RETURN NIL)))
        (COND
         ((NOT ERR)
          (PROGN
           (SETQ X (ABSF (IGETV (IGETV M I) I)))
           (SETQ K I)
           (PROG (J)
             (SETQ J (PLUS I 1))
            LAB
             (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
             (COND
              (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
                (SETQ Y (ABSF (IGETV (IGETV M J) I))) X)
               (PROGN (SETQ X Y) (SETQ K J))))
             (SETQ J (PLUS2 J 1))
             (GO LAB))
           (COND
            ((NEQ I K)
             (PROGN
              (SETQ Y (IGETV M I))
              (IPUTV M I (IGETV M K))
              (IPUTV M K Y))))
           (COND ((NULL (IGETV (IGETV M I) I)) (SETQ ERR T))
                 (T
                  (PROGN
                   (SETQ X (|::QUOTIENT| 1 (IGETV (IGETV M I) I)))
                   (PROG (J)
                     (SETQ J (PLUS I 1))
                    LAB
                     (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                     (PROGN
                      (SETQ Y (|:TIMESN| (IGETV (IGETV M J) I) X))
                      (PROG (K)
                        (SETQ K I)
                       LAB
                        (COND ((MINUSP (DIFFERENCE N1 K)) (RETURN NIL)))
                        (IPUTV (IGETV M J) K
                               (|:DIFFERENCE| (IGETV (IGETV M J) K)
                                              (|:TIMESN| Y
                                                         (IGETV (IGETV M I)
                                                                K))))
                        (SETQ K (PLUS2 K 1))
                        (GO LAB))
                      NIL)
                     (SETQ J (PLUS2 J 1))
                     (GO LAB)))))
           NIL)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I N)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (COND
         ((NOT ERR)
          (PROGN
           (SETQ W (IGETV (IGETV M I) N1))
           (PROG (J)
             (SETQ J (IPLUS2 I 1))
            LAB
             (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
             (SETQ W
                     (|:DIFFERENCE| W
                                    (|:TIMESN| (IGETV (IGETV M I) J)
                                               (IGETV R J))))
             (SETQ J (PLUS2 J 1))
             (GO LAB))
           (COND ((NULL (IGETV (IGETV M I) I)) (SETQ ERR T))
                 (T (IPUTV R I (|::QUOTIENT| W (IGETV (IGETV M I) I)))))
           NIL)))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN
       (COND (ERR NIL)
             (T
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (IGETV R I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (IGETV R I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(REMPROP 'S 'SMACRO) 
(REMPROP 'S 'NUMBER-OF-ARGS) 
(ENDMODULE) 
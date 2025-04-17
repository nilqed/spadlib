(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HSUB)) 
(FLUID '(*TRHARM)) 
(SWITCH (LIST 'TRHAM)) 
(PUT 'HSUB1 'NUMBER-OF-ARGS 5) 
(PUT 'HSUB1 'DEFINED-ON-LINE '34) 
(PUT 'HSUB1 'DEFINED-IN-FILE 'CAMAL/HSUB.RED) 
(PUT 'HSUB1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HSUB1 (X U V A N)
    (PROG (ANS C TMP |FS:ZERO-GENERATED|)
      (SETQ ANS (|FS:SUBANG| X U V))
      (SETQ C (CAR A))
      (COND (C (SETQ C (CDR C))))
      (SETQ A C)
      (COND
       (*TRHAM
        (PROGN (PRINT "A") (COND ((NULL A) (PRINT 0)) (T (|FS:PRIN| A))))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (COND (*TRHAM (PROGN (PRINT "i=") (PRINT I))))
         (SETQ X (HDIFF X U))
         (COND
          (*TRHAM
           (PROGN
            (PRIN2* "df(x,u,i)=")
            (|FS:PRIN| X)
            (TERPRI* T)
            (PRIN2* "A^i =")
            (|FS:PRIN| C)
            (TERPRI* T))))
         (SETQ C (|FS:TIMES| (CDR (*SQ2FOURIER (CONS 1 I))) C))
         (COND
          (*TRHAM (PROGN (PRIN2* "A^i/fact(i) =") (|FS:PRIN| C) (TERPRI* T))))
         (SETQ TMP (|FS:TIMES| (|FS:SUBANG| X U V) C))
         (COND
          (*TRHAM
           (PROGN (PRIN2* "f'(0)*A^i/fact i = ") (|FS:PRIN| TMP) (TERPRI* T))))
         (SETQ ANS (|FS:PLUS| ANS TMP))
         (COND
          (*TRHAM
           (PROGN (PRIN2* "partial sum =") (|FS:PRIN| ANS) (TERPRI* T))))
         (COND ((NOT (EQUAL I N)) (SETQ C (|FS:TIMES| C A))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ANS))) 
(PUT '|FS:SUBANG| 'NUMBER-OF-ARGS 3) 
(PUT '|FS:SUBANG| 'DEFINED-ON-LINE '64) 
(PUT '|FS:SUBANG| 'DEFINED-IN-FILE 'CAMAL/HSUB.RED) 
(PUT '|FS:SUBANG| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |FS:SUBANG| (X U V)
    (COND ((NULL X) NIL)
          (T
           (PROG (VV N)
             (SETQ VV (MKVECT 7))
             (SETQ N (GETV (GETV X 2) U))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE 7 I)) (RETURN NIL)))
               (COND ((EQUAL I U) (PUTV VV I (TIMES N (GETV V I))))
                     (T
                      (PUTV VV I
                            (PLUS (GETV (GETV X 2) I) (TIMES N (GETV V I))))))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN
              (|FS:PLUS| (|FS:SUBANG| (GETV X 3) U V)
               (MAKE-TERM (GETV X 1) VV (GETV X 0)))))))) 
(PUT '|FS:SUB| 'NUMBER-OF-ARGS 2) 
(PUT '|FS:SUB| 'DEFINED-ON-LINE '76) 
(PUT '|FS:SUB| 'DEFINED-IN-FILE 'CAMAL/HSUB.RED) 
(PUT '|FS:SUB| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |FS:SUB| (X U)
    (COND ((NULL X) NIL)
          (T
           (PROG (ANS)
             (SETQ ANS (REVAL1 (PREPSQ (GETV X 0)) NIL))
             (COND ((NOT (FIXP ANS)) (SETQ ANS (SUBSQ (CADR ANS) U)))
                   (T (SETQ ANS (GETV X 0))))
             (COND ((EQCAR (CAR ANS) '|:FS:|) (SETQ ANS (CDAR ANS)))
                   (T (SETQ ANS (CDR (*SQ2FOURIER ANS)))))
             (SETQ ANS
                     (|FS:TIMES| (MAKE-TERM (GETV X 1) (GETV X 2) (CONS 1 1))
                      ANS))
             (RETURN (|FS:PLUS| (|FS:SUB| (GETV X 3) U) ANS)))))) 
(PUT 'SIMPHSUB 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPHSUB 'DEFINED-ON-LINE '88) 
(PUT 'SIMPHSUB 'DEFINED-IN-FILE 'CAMAL/HSUB.RED) 
(PUT 'SIMPHSUB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPHSUB (UU)
    (PROG (X U V VV A N DMODE*)
      (SETQ DMODE* '|:FS:|)
      (COND
       ((EQUAL (LENGTH UU) 5)
        (PROGN
         (SETQ X (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ U (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ V (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ A (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ N (CAR UU))))
       ((EQUAL (LENGTH UU) 3)
        (PROGN
         (SETQ X (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ U (CAR UU))
         (SETQ UU (CDR UU))
         (SETQ V (CAR UU))
         (SETQ UU (CDR UU))
         (COND
          ((NOT (HARMONICP U))
           (PROGN
            ((LAMBDA (WTL*)
               (SETQ A
                       (CONS
                        (CONS (GET 'FOURIER 'TAG)
                              (|FS:SUB| (CDAR (SIMP X)) (LIST (CONS U V))))
                        1)))
             (DELASC U WTL*))
            (RETURN A)
            NIL)))
         (SETQ A 0)
         (SETQ N 0))))
      (COND ((NOT (HARMONICP U)) (RERROR 'FOURIER 7 "Not an angle in HSUB")))
      (SETQ X (CDAR (SIMP X)))
      (COND
       ((NOT (ANGLE-EXPRESSION-P V))
        (RERROR 'FOURIER 8 "Not an angle expression in HSUB")))
      (SETQ VV (MKVECT 7))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 7 I)) (RETURN NIL)))
        (PUTV VV I 0)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COMPILE-ANGLE-EXPRESSION V VV)
      (SETQ A (SIMP* A))
      (SETQ N (SIMP* N))
      (COND ((NULL (CAR N)) (SETQ N (CONS 0 1)))
            ((NOT (AND (FIXP (CAR N)) (EQUAL (CDR N) 1)))
             (RERROR 'FOURIER 9 "Non integer expansion in HSUB")))
      (SETQ N (CAR N))
      (RETURN
       (CONS (CONS (GET 'FOURIER 'TAG) (HSUB1 X (GET U 'FOURIER-ANGLE) VV A N))
             1)))) 
(PUT 'HSUB 'SIMPFN 'SIMPHSUB) 
(ENDMODULE) 
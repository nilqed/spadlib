(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GHYPER)) 
(FLAG '(HYPERGEOMETIC) 'SPECFN) 
(PUT 'HYPERGEOMTRIC 'NUMBER-OF-ARGS 3) 
(PUT 'GHF 'SIMPFN 'SIMPGHF) 
(PUT 'SIMPGHF 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGHF 'DEFINED-ON-LINE '44) 
(PUT 'SIMPGHF 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'SIMPGHF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGHF (U)
    (COND
     ((NULL (CDDR U))
      (RERROR 'SPECIALF 125 "WRONG NUMBER OF ARGUMENTS TO GHF-FUNCTION"))
     ((OR (NOT (NUMBERP (CAR U))) (NOT (NUMBERP (CADR U))))
      (RERROR 'SPECIALF 126 "INVALID AS INTEGER"))
     (T
      (PROG (VV V)
        (SETQ V (REDPAR1 (CDDR U) (CAR U)))
        (SETQ VV (REDPAR1 (CDR V) (CADR U)))
        (COND
         ((NULL (CDDR VV))
          (RETURN
           (GHFSQ (LIST (CAR U) (CADR U))
            (PROG (UU FORALL-RESULT FORALL-ENDPTR)
              (SETQ UU (CAR V))
              (COND ((NULL UU) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL)))
             LOOPLABEL
              (SETQ UU (CDR UU))
              (COND ((NULL UU) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (PROG (UU FORALL-RESULT FORALL-ENDPTR)
              (SETQ UU (CAR VV))
              (COND ((NULL UU) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL)))
             LOOPLABEL
              (SETQ UU (CDR UU))
              (COND ((NULL UU) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (SIMP (CADR VV))))))
        (RETURN
         (RERROR 'SPECIALF 127 "WRONG NUMBER OF ARGUMENTS TO GHF-FUNCTION")))))) 
(PUT 'GHFEXIT 'NUMBER-OF-ARGS 3) 
(PUT 'GHFEXIT 'DEFINED-ON-LINE '62) 
(PUT 'GHFEXIT 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFEXIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHFEXIT (A B Z)
    (PROG (AA BB)
      (SETQ AA
              (CONS 'LIST
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU A)
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (UU) (PREPSQ UU)) (CAR UU))
                                       NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (PREPSQ UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ BB
              (CONS 'LIST
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU B)
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (UU) (PREPSQ UU)) (CAR UU))
                                       NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (PREPSQ UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN
       (CONS
        (LIST
         (CONS (CONS (CAR (FKERN (LIST 'HYPERGEOMETRIC AA BB (PREPSQ Z)))) 1)
               1))
        1)))) 
(PUT 'LISTMAXSQ 'NUMBER-OF-ARGS 1) 
(PUT 'LISTMAXSQ 'DEFINED-ON-LINE '73) 
(PUT 'LISTMAXSQ 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'LISTMAXSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTMAXSQ (U)
    (COND ((NULL (CDR U)) (CAR U)) ((NULL (CAAR U)) (CAR U))
          ((NULL (CAADR U)) (CADR U))
          ((OR (GREATERP (CAAR U) (CAADR U)) (EQUAL (CAR U) (CADR U)))
           (LISTMAXSQ (CONS (CAR U) (CDDR U))))
          (T (LISTMAXSQ (CONS (CADR U) (CDDR U)))))) 
(PUT 'GHFPOLYNOMP 'NUMBER-OF-ARGS 2) 
(PUT 'GHFPOLYNOMP 'DEFINED-ON-LINE '83) 
(PUT 'GHFPOLYNOMP 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFPOLYNOMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GHFPOLYNOMP (U A)
    (PROG (W1 W2)
     M1
      (COND
       ((NULL U)
        (COND ((NULL W1) (PROGN (SETQ U W2) (RETURN (CONS NIL A))))
              (T
               (PROGN
                (SETQ U (LISTMAXSQ W1))
                (SETQ A (CONS U (APPEND (DELETE U W1) W2)))
                (RETURN (CONS T A))))))
       ((PARFOOL (CAR U)) (SETQ W1 (CONS (CAR U) W1)))
       (T (SETQ W2 (CONS (CAR U) W2))))
      (SETQ U (CDR U))
      (GO M1))) 
(PUT 'POLYNOM 'NUMBER-OF-ARGS 4) 
(PUT 'POLYNOM 'DEFINED-ON-LINE '98) 
(PUT 'POLYNOM 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'POLYNOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYNOM (U A B Z)
    (PROG (S K)
      (SETQ K 0)
      (COND ((NULL (CAAR U)) (RETURN '(1 . 1))) (T (SETQ S (GHFPOLYNOMP B A))))
      (SETQ A (CDR S))
      (COND
       ((CAR S)
        (COND
         ((OR (NULL (CAAR A)) (GREATERP (CAAR A) (CAAR U)))
          (PROGN (SETQ B A) (SETQ A U) (RETURN (GHFEXIT A B Z)) NIL))
         (T (SETQ B A)))))
      (SETQ K 1)
      (SETQ S (CONS 1 1))
     M
      (SETQ S
              (ADDSQ S
                     (MULTSQ (MULTSQ (MULTPOCHH U (SIMP K)) (EXPTSQ Z K))
                             (INVSQ
                              (MULTPOCHH (APPEND (LIST '(1 . 1)) B)
                                         (SIMP K))))))
      (SETQ K (PLUS K 1))
      (COND ((GREATERP K (CAR (NEGSQ (CAR U)))) (RETURN S)) (T (GO M))))) 
(PUT 'GHFLOWERING1P 'NUMBER-OF-ARGS 0) 
(PUT 'GHFLOWERING1P 'DEFINED-ON-LINE '125) 
(PUT 'GHFLOWERING1P 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFLOWERING1P 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'GHFLOWERING1P 'SMACRO
      '(LAMBDA ()
         (PROG (SA SB W1 W2)
           (SETQ SA A)
           (SETQ SB B)
          M1
           (COND ((NULL B) (PROGN (SETQ A SA) (SETQ B SB) (RETURN NIL))))
          M2
           (COND
            ((NULL A)
             (PROGN
              (SETQ W2 (CONS (CAR B) W2))
              (SETQ B (CDR B))
              (SETQ A SA)
              (SETQ W1 NIL)
              (GO M1)))
            ((AND (NUMBERP (PREPSQ (ADDSQ (CAR A) (NEGSQ (CAR B)))))
                  (GREATERP (CAR (ADDSQ (CAR A) (NEGSQ (CAR B)))) 0))
             (PROGN
              (SETQ B (CONS (CAR B) (APPEND W2 (CDR B))))
              (SETQ A
                      (CONS (ADDSQ (CAR A) (NEGSQ (CAR B)))
                            (APPEND W1 (CDR A))))
              (RETURN T)))
            (T (PROGN (SETQ W1 (CONS (CAR A) W1)) (SETQ A (CDR A)) (GO M2))))))) 
(PUT 'LOWERING1 'NUMBER-OF-ARGS 4) 
(PUT 'LOWERING1 'DEFINED-ON-LINE '150) 
(PUT 'LOWERING1 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'LOWERING1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOWERING1 (X Y U Z)
    (ADDSQ
     (GHFSQ U
      (APPEND (LIST (ADDSQ (ADDSQ (CAR X) (CAR Y)) (NEGSQ '(1 . 1)))) (CDR X))
      (APPEND (LIST (CAR Y)) (CDR Y)) Z)
     (MULTSQ
      (GHFSQ U
       (APPEND (LIST (ADDSQ (CAR X) (CAR Y)))
               (SPECFN-LISTPLUS (CDR X) '(1 . 1)))
       (APPEND (LIST (ADDSQ (CAR Y) '(1 . 1)))
               (SPECFN-LISTPLUS (CDR Y) '(1 . 1)))
       Z)
      (MULTSQ
       (MULTSQ Z
               ((LAMBDA (P)
                  (PROGN
                   (PROG (PP)
                     (SETQ PP (CDR X))
                    LAB
                     (COND ((NULL PP) (RETURN NIL)))
                     ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
                     (SETQ PP (CDR PP))
                     (GO LAB))
                   P))
                '(1 . 1)))
       (INVSQ
        (MULTSQ (CAR Y)
                ((LAMBDA (P)
                   (PROGN
                    (PROG (PP)
                      (SETQ PP (CDR Y))
                     LAB
                      (COND ((NULL PP) (RETURN NIL)))
                      ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
                      (SETQ PP (CDR PP))
                      (GO LAB))
                    P))
                 '(1 . 1)))))))) 
(PUT 'GHFLOWERING2P 'NUMBER-OF-ARGS 0) 
(PUT 'GHFLOWERING2P 'DEFINED-ON-LINE '164) 
(PUT 'GHFLOWERING2P 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFLOWERING2P 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'GHFLOWERING2P 'SMACRO
      '(LAMBDA ()
         (PROG (SA SB W1 WA FL)
           (COND ((EQUAL Z '(1 . 1)) (RETURN NIL)))
           (SETQ SA A)
           (SETQ SB B)
          M1
           (COND
            ((NULL B)
             (PROGN
              (SETQ B SB)
              (COND (FL (SETQ A (CONS WA SA))) (T (SETQ A SA)))
              (RETURN NIL))))
          M2
           (COND
            ((NULL A)
             (PROGN (SETQ B (CDR B)) (SETQ A SA) (SETQ W1 NIL) (GO M1)))
            ((AND (NUMBERP (PREPSQ (ADDSQ (CAR B) (NEGSQ (CAR A)))))
                  (LESSP (CAR (ADDSQ (CAR A) (NEGSQ (CAR B)))) 0))
             (COND
              (FL
               (COND
                ((NOT (EQUAL WA (CAR A)))
                 (PROGN
                  (SETQ B SB)
                  (SETQ A (CONS (LIST WA (CAR A)) (APPEND W1 (CDR A))))
                  (RETURN T)))
                (T
                 (PROGN
                  (SETQ W1 (CONS (CAR A) W1))
                  (SETQ A (CDR A))
                  (GO M2)))))
              (T
               (PROGN
                (SETQ FL T)
                (SETQ SA (APPEND W1 (CDR A)))
                (SETQ WA (CAR A))
                (SETQ B (CDR B))
                (SETQ A SA)
                (SETQ W1 NIL)
                (GO M1)))))
            (T (PROGN (SETQ W1 (CONS (CAR A) W1)) (SETQ A (CDR A)) (GO M2))))))) 
(PUT 'LOWERING2 'NUMBER-OF-ARGS 4) 
(PUT 'LOWERING2 'DEFINED-ON-LINE '207) 
(PUT 'LOWERING2 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'LOWERING2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOWERING2 (X B U Z)
    (ADDSQ
     (MULTSQ
      (GHFSQ U (APPEND (LIST (CAAR X) (ADDSQ '(1 . 1) (CADAR X))) (CDR X)) B Z)
      (MULTSQ (CADAR X) (INVSQ (ADDSQ (CADAR X) (NEGSQ (CAAR X))))))
     (NEGSQ
      (MULTSQ
       (GHFSQ U (APPEND (LIST (ADDSQ '(1 . 1) (CAAR X)) (CADAR X)) (CDR X)) B
        Z)
       (MULTSQ (CAAR X) (INVSQ (ADDSQ (CADAR X) (NEGSQ (CAAR X))))))))) 
(PUT 'GHFLOWERING3P 'NUMBER-OF-ARGS 0) 
(PUT 'GHFLOWERING3P 'DEFINED-ON-LINE '217) 
(PUT 'GHFLOWERING3P 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFLOWERING3P 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'GHFLOWERING3P 'SMACRO
      '(LAMBDA ()
         (PROG (SA W MMM)
           (SETQ SA A)
          M1
           (COND ((NULL A) (PROGN (SETQ A SA) (RETURN NIL)))
                 ((NOT (NUMBERP (PREPSQ (CAR A))))
                  (PROGN (SETQ W (CONS (CAR A) W)) (SETQ A (CDR A)) (GO M1))))
           (COND
            ((MEMBER '(1 . 1) A)
             (PROGN (SETQ MMM '(1 . 1)) (SETQ A (DELETE '(1 . 1) A))))
            (T (PROGN (SETQ MMM (CAR A)) (SETQ A (CDR A)))))
          M2
           (COND
            ((NULL A)
             (COND ((LISTNUMBERP B) (PROGN (SETQ A (CONS MMM W)) (RETURN T)))
                   (T (PROGN (SETQ A SA) (RETURN NIL)))))
            ((EQUAL (CAR A) '(1 . 1)) (PROGN (SETQ A SA) (RETURN NIL)))
            (T (PROGN (SETQ W (CONS (CAR A) W)) (SETQ A (CDR A)) (GO M2))))))) 
(PUT 'LISTNUMBERP 'NUMBER-OF-ARGS 1) 
(PUT 'LISTNUMBERP 'DEFINED-ON-LINE '243) 
(PUT 'LISTNUMBERP 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'LISTNUMBERP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTNUMBERP (V)
    (COND ((NULL V) NIL) ((NUMBERP (PREPSQ (CAR V))) T)
          (T (LISTNUMBERP (CDR V))))) 
(PUT 'LOWERING3 'NUMBER-OF-ARGS 4) 
(PUT 'LOWERING3 'DEFINED-ON-LINE '251) 
(PUT 'LOWERING3 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'LOWERING3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOWERING3 (A B U Z)
    (MULTSQ
     (MULTSQ
      ((LAMBDA (P)
         (PROGN
          (PROG (PP)
            (SETQ PP
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU B)
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (UU)
                                          (ADDSQ UU (NEGSQ '(1 . 1))))
                                        (CAR UU))
                                       NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1))))
                                (CAR UU))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
           LAB
            (COND ((NULL PP) (RETURN NIL)))
            ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
            (SETQ PP (CDR PP))
            (GO LAB))
          P))
       '(1 . 1))
      (INVSQ
       (MULTSQ Z
               ((LAMBDA (P)
                  (PROGN
                   (PROG (PP)
                     (SETQ PP
                             (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                               (SETQ UU (CDR A))
                               (COND ((NULL UU) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (UU)
                                                   (ADDSQ UU (NEGSQ '(1 . 1))))
                                                 (CAR UU))
                                                NIL)))
                              LOOPLABEL
                               (SETQ UU (CDR UU))
                               (COND ((NULL UU) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (UU)
                                           (ADDSQ UU (NEGSQ '(1 . 1))))
                                         (CAR UU))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                    LAB
                     (COND ((NULL PP) (RETURN NIL)))
                     ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
                     (SETQ PP (CDR PP))
                     (GO LAB))
                   P))
                '(1 . 1)))))
     (ADDSQ
      (GHFSQ U
       (CONS (CAR A)
             (PROG (UU FORALL-RESULT FORALL-ENDPTR)
               (SETQ UU (CDR A))
               (COND ((NULL UU) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1))))
                                 (CAR UU))
                                NIL)))
              LOOPLABEL
               (SETQ UU (CDR UU))
               (COND ((NULL UU) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
       (PROG (UU FORALL-RESULT FORALL-ENDPTR)
         (SETQ UU B)
         (COND ((NULL UU) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                          NIL)))
        LOOPLABEL
         (SETQ UU (CDR UU))
         (COND ((NULL UU) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))
       Z)
      (NEGSQ
       (GHFSQ U
        (APPEND (LIST (ADDSQ (CAR A) (NEGSQ '(1 . 1))))
                (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                  (SETQ UU (CDR A))
                  (COND ((NULL UU) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1))))
                                    (CAR UU))
                                   NIL)))
                 LOOPLABEL
                  (SETQ UU (CDR UU))
                  (COND ((NULL UU) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROG (UU FORALL-RESULT FORALL-ENDPTR)
          (SETQ UU B)
          (COND ((NULL UU) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                           NIL)))
         LOOPLABEL
          (SETQ UU (CDR UU))
          (COND ((NULL UU) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1)))) (CAR UU))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        Z))))) 
(PUT 'GHFSQ 'NUMBER-OF-ARGS 4) 
(PUT 'GHFSQ 'DEFINED-ON-LINE '263) 
(PUT 'GHFSQ 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHFSQ (U A B Z)
    (PROG (C AAA)
      (SETQ U (REDPAR A B))
      (SETQ A (CAR U))
      (SETQ B (CADR U))
      (SETQ U (LIST (LENGTH A) (LENGTH B)))
      (COND ((NULL (CAR Z)) (RETURN '(1 . 1)))
            ((AND (LISTPARFOOL B (CONS NIL 1))
                  (NOT (LISTPARFOOL A (CONS NIL 1))))
             (RETURN (GHFEXIT A B Z)))
            (T (SETQ AAA (GHFPOLYNOMP A A))))
      (SETQ A (CDR AAA))
      (COND ((CAR AAA) (RETURN (POLYNOM A A B Z)))
            ((PROG (SA SB W1 W2)
               (SETQ SA A)
               (SETQ SB B)
              M1
               (COND ((NULL B) (PROGN (SETQ A SA) (SETQ B SB) (RETURN NIL))))
              M2
               (COND
                ((NULL A)
                 (PROGN
                  (SETQ W2 (CONS (CAR B) W2))
                  (SETQ B (CDR B))
                  (SETQ A SA)
                  (SETQ W1 NIL)
                  (GO M1)))
                ((AND (NUMBERP (PREPSQ (ADDSQ (CAR A) (NEGSQ (CAR B)))))
                      (GREATERP (CAR (ADDSQ (CAR A) (NEGSQ (CAR B)))) 0))
                 (PROGN
                  (SETQ B (CONS (CAR B) (APPEND W2 (CDR B))))
                  (SETQ A
                          (CONS (ADDSQ (CAR A) (NEGSQ (CAR B)))
                                (APPEND W1 (CDR A))))
                  (RETURN T)))
                (T
                 (PROGN
                  (SETQ W1 (CONS (CAR A) W1))
                  (SETQ A (CDR A))
                  (GO M2)))))
             (RETURN (LOWERING1 A B U Z)))
            ((PROG (SA SB W1 WA FL)
               (COND ((EQUAL Z '(1 . 1)) (RETURN NIL)))
               (SETQ SA A)
               (SETQ SB B)
              M1
               (COND
                ((NULL B)
                 (PROGN
                  (SETQ B SB)
                  (COND (FL (SETQ A (CONS WA SA))) (T (SETQ A SA)))
                  (RETURN NIL))))
              M2
               (COND
                ((NULL A)
                 (PROGN (SETQ B (CDR B)) (SETQ A SA) (SETQ W1 NIL) (GO M1)))
                ((AND (NUMBERP (PREPSQ (ADDSQ (CAR B) (NEGSQ (CAR A)))))
                      (LESSP (CAR (ADDSQ (CAR A) (NEGSQ (CAR B)))) 0))
                 (COND
                  (FL
                   (COND
                    ((NOT (EQUAL WA (CAR A)))
                     (PROGN
                      (SETQ B SB)
                      (SETQ A (CONS (LIST WA (CAR A)) (APPEND W1 (CDR A))))
                      (RETURN T)))
                    (T
                     (PROGN
                      (SETQ W1 (CONS (CAR A) W1))
                      (SETQ A (CDR A))
                      (GO M2)))))
                  (T
                   (PROGN
                    (SETQ FL T)
                    (SETQ SA (APPEND W1 (CDR A)))
                    (SETQ WA (CAR A))
                    (SETQ B (CDR B))
                    (SETQ A SA)
                    (SETQ W1 NIL)
                    (GO M1)))))
                (T
                 (PROGN
                  (SETQ W1 (CONS (CAR A) W1))
                  (SETQ A (CDR A))
                  (GO M2)))))
             (RETURN (LOWERING2 A B U Z)))
            ((PROG (SA W MMM)
               (SETQ SA A)
              M1
               (COND ((NULL A) (PROGN (SETQ A SA) (RETURN NIL)))
                     ((NOT (NUMBERP (PREPSQ (CAR A))))
                      (PROGN
                       (SETQ W (CONS (CAR A) W))
                       (SETQ A (CDR A))
                       (GO M1))))
               (COND
                ((MEMBER '(1 . 1) A)
                 (PROGN (SETQ MMM '(1 . 1)) (SETQ A (DELETE '(1 . 1) A))))
                (T (PROGN (SETQ MMM (CAR A)) (SETQ A (CDR A)))))
              M2
               (COND
                ((NULL A)
                 (COND
                  ((LISTNUMBERP B) (PROGN (SETQ A (CONS MMM W)) (RETURN T)))
                  (T (PROGN (SETQ A SA) (RETURN NIL)))))
                ((EQUAL (CAR A) '(1 . 1)) (PROGN (SETQ A SA) (RETURN NIL)))
                (T
                 (PROGN (SETQ W (CONS (CAR A) W)) (SETQ A (CDR A)) (GO M2)))))
             (RETURN (LOWERING3 A B U Z)))
            ((AND (EQUAL (CAR U) 0) (EQUAL (CADR U) 0))
             (RETURN (EXPDEG (SIMP 'E) Z)))
            ((AND (EQUAL (CAR U) 0) (EQUAL (CADR U) 1)) (RETURN (GHF01 A B Z)))
            ((AND (EQUAL (CAR U) 1) (EQUAL (CADR U) 0))
             (COND ((EQUAL Z '(1 . 1)) (RETURN (GHFEXIT A B Z)))
                   (T
                    (RETURN
                     (EXPDEG (ADDSQ '(1 . 1) (NEGSQ Z))
                             (COND ((NULL A) '(NIL . 1))
                                   (T (NEGSQ (CAR A)))))))))
            ((AND (EQUAL (CAR U) 1) (EQUAL (CADR U) 1)) (RETURN (GHF11 A B Z)))
            ((AND (EQUAL (CAR U) 1) (EQUAL (CADR U) 2)) (RETURN (GHF12 A B Z)))
            ((AND (EQUAL (CAR U) 2) (EQUAL (CADR U) 1)) (RETURN (GHF21 A B Z)))
            ((EQUAL (CAR U) (PLUS (CADR U) 1))
             (COND
              ((EQUAL (SETQ C (GHFMID A B Z)) 'FAIL) (RETURN (GHFEXIT A B Z)))
              (T (RETURN C)))))
      (COND ((LEQ (CAR U) (CADR U)) (RETURN (GHFEXIT A B Z))))
      (RETURN (RERROR 'SPECIALF 131 "hypergeometric series diverges")))) 
(PUT 'GHFMID 'NUMBER-OF-ARGS 3) 
(PUT 'GHFMID 'DEFINED-ON-LINE '307) 
(PUT 'GHFMID 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHFMID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHFMID (A B Z)
    (PROG (C)
      (SETQ C
              (REDPAR A
                      (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                        (SETQ UU B)
                        (COND ((NULL UU) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (UU)
                                            (ADDSQ UU (NEGSQ '(1 . 1))))
                                          (CAR UU))
                                         NIL)))
                       LOOPLABEL
                        (SETQ UU (CDR UU))
                        (COND ((NULL UU) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(1 . 1))))
                                  (CAR UU))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (COND
       ((OR (GREATERP (LENGTH (CADR C)) 0) (GREATERP (LENGTH (CAR C)) 1))
        (RETURN 'FAIL))
       (T
        (RETURN
         (FORMULAFORMIDCASE (LENGTH B) (CAAR C)
          (ADDSQ (CAR B) (NEGSQ '(1 . 1))) Z)))))) 
(PUT 'FORMULAFORMIDCASE 'NUMBER-OF-ARGS 4) 
(PUT 'FORMULAFORMIDCASE 'DEFINED-ON-LINE '316) 
(PUT 'FORMULAFORMIDCASE 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'FORMULAFORMIDCASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMULAFORMIDCASE (P B A Z)
    (COND
     ((AND (NOT (EQUAL P 1)) (EQUAL B '(1 . 1)) (EQUAL Z '(1 . 1)))
      (MULTSQ (SIMPX1 (PREPSQ (MULTSQ '(-1 . 1) A)) P 1)
              (MULTSQ
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'POLYGAMMA (PREPSQ (SIMP (DIFFERENCE P 1)))
                           (PREPSQ A))))
                   1)
                  1))
                1)
               (INVSQ
                (CONS
                 (LIST
                  (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP P))))) 1)
                        1))
                 1)))))
     ((AND (EQUAL B '(1 . 1)) (EQUAL Z '(-1 . 1)))
      (MULTSQ
       (MULTSQ (SIMPX1 (PREPSQ (MULTSQ '(-1 . 2) A)) P 1)
               (ADDSQ
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'POLYGAMMA (PREPSQ (SIMP (DIFFERENCE P 1)))
                            (PREPSQ (MULTSQ A '(1 . 2))))))
                    1)
                   1))
                 1)
                (NEGSQ
                 (CONS
                  (LIST
                   (CONS
                    (CONS
                     (CAR
                      (FKERN
                       (LIST 'POLYGAMMA (PREPSQ (SIMP (DIFFERENCE P 1)))
                             (PREPSQ (MULTSQ (ADDSQ '(1 . 1) A) '(1 . 2))))))
                     1)
                    1))
                  1))))
       (INVSQ
        (CONS
         (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP P))))) 1) 1))
         1))))
     ((AND (EQUAL Z '(1 . 1)) (NOT (NUMBERP (PREPSQ B))))
      (MULTSQ
       (SUBSQNEW
        (DERIVATIVESQ
         (MULTSQ
          (CONS
           (LIST
            (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP 'R))))) 1) 1))
           1)
          (INVSQ
           (CONS
            (LIST
             (CONS
              (CONS
               (CAR
                (FKERN
                 (LIST 'GAMMA
                       (PREPSQ (ADDSQ (SIMP 'R) (ADDSQ '(1 . 1) (NEGSQ B)))))))
               1)
              1))
            1)))
         'R (SIMP (DIFFERENCE P 1)))
        A 'R)
       (MULTSQ
        (MULTSQ (MULTSQ (SIMPX1 (PREPSQ (MULTSQ '(-1 . 1) A)) P 1) '(-1 . 1))
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN (LIST 'GAMMA (PREPSQ (ADDSQ '(1 . 1) (NEGSQ B))))))
                    1)
                   1))
                 1))
        (INVSQ
         (CONS
          (LIST
           (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP P))))) 1) 1))
          1)))))
     ((AND (EQUAL Z '(-1 . 1)) (NUMBERP (PREPSQ B)))
      (PROG (C K)
        (SETQ K 0)
        (RETURN
         (MULTSQ
          (SUBSQNEW
           (DERIVATIVESQ
            (ADDSQ
             (PROGN
              (SETQ K (DIFFERENCE (PREPSQ B) 1))
              (SETQ C '(NIL . 1))
              (PROG ()
               WHILELABEL
                (COND ((NOT (GREATERP (PREPSQ K) 0)) (RETURN NIL)))
                (PROGN
                 (SETQ C
                         (ADDSQ C
                                (MULTSQ
                                 (CONS
                                  (LIST
                                   (CONS
                                    (CONS
                                     (CAR (FKERN (LIST 'GAMMA (PREPSQ B)))) 1)
                                    1))
                                  1)
                                 (CONS
                                  (LIST
                                   (CONS
                                    (CONS
                                     (CAR
                                      (FKERN
                                       (LIST 'POCHHAMMER
                                             (PREPSQ
                                              (ADDSQ (SIMP (PLUS 1 K))
                                                     (NEGSQ (SIMP 'R))))
                                             (PREPSQ
                                              (SIMP
                                               (DIFFERENCE
                                                (DIFFERENCE (PREPSQ B) 1)
                                                K))))))
                                     1)
                                    1))
                                  1))))
                 (SETQ K (DIFFERENCE K 1)))
                (GO WHILELABEL))
              C)
             (MULTSQ
              (MULTSQ
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN (LIST 'GAMMA (PREPSQ (ADDSQ B (NEGSQ (SIMP 'R)))))))
                   1)
                  1))
                1)
               (ADDSQ
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'PSI
                            (PREPSQ
                             (MULTSQ (ADDSQ (SIMP 'R) '(1 . 1)) '(1 . 2))))))
                    1)
                   1))
                 1)
                (NEGSQ
                 (CONS
                  (LIST
                   (CONS
                    (CONS
                     (CAR
                      (FKERN (LIST 'PSI (PREPSQ (MULTSQ (SIMP 'R) '(1 . 2))))))
                     1)
                    1))
                  1))))
              (INVSQ
               (MULTSQ (CONS 2 1)
                       (CONS
                        (LIST
                         (CONS
                          (CONS
                           (CAR
                            (FKERN
                             (LIST 'GAMMA
                                   (PREPSQ
                                    (ADDSQ '(1 . 1) (NEGSQ (SIMP 'R)))))))
                           1)
                          1))
                        1)))))
            'R (DIFFERENCE P 1))
           A 'R)
          (MULTSQ (MULTSQ (SIMPX1 (PREPSQ (MULTSQ '(-1 . 1) A)) P 1) '(-1 . 1))
                  (INVSQ
                   (MULTSQ
                    (CONS
                     (LIST
                      (CONS
                       (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP P))))) 1)
                       1))
                     1)
                    (CONS
                     (LIST
                      (CONS
                       (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (SIMP B))))) 1)
                       1))
                     1))))))))
     (T 'FAIL))) 
(PUT 'GHF01 'NUMBER-OF-ARGS 3) 
(PUT 'GHF01 'DEFINED-ON-LINE '372) 
(PUT 'GHF01 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHF01 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHF01 (A B Z)
    (COND
     ((ZNAK Z)
      (MULTSQ
       (CONS
        (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (CAR B))))) 1) 1))
        1)
       (MULTSQ
        (CONS
         (LIST
          (CONS
           (CONS
            (CAR
             (FKERN
              (LIST 'BESSELI (PREPSQ (ADDSQ (CAR B) (NEGSQ '(1 . 1))))
                    (PREPSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
            1)
           1))
         1)
        (EXPDEG Z (MULTSQ (ADDSQ '(1 . 1) (NEGSQ (CAR B))) '(1 . 2))))))
     (T
      (MULTSQ
       (CONS
        (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (CAR B))))) 1) 1))
        1)
       (MULTSQ
        (CONS
         (LIST
          (CONS
           (CONS
            (CAR
             (FKERN
              (LIST 'BESSELJ (PREPSQ (ADDSQ (CAR B) (NEGSQ '(1 . 1))))
                    (PREPSQ
                     (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2))))))
            1)
           1))
         1)
        (EXPDEG (NEGSQ Z)
                (MULTSQ (ADDSQ '(1 . 1) (NEGSQ (CAR B))) '(1 . 2)))))))) 
(PUT 'GHF11 'NUMBER-OF-ARGS 3) 
(PUT 'GHF11 'DEFINED-ON-LINE '382) 
(PUT 'GHF11 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHF11 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHF11 (A B Z)
    (COND
     ((EQUAL (CAR B) (MULTSQ '(2 . 1) (CAR A)))
      (MULTSQ
       (MULTSQ
        (CONS
         (LIST
          (CONS
           (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (ADDSQ '(1 . 2) (CAR A))))))
                 1)
           1))
         1)
        (EXPDEG (SIMP 'E) (MULTSQ Z '(1 . 2))))
       (MULTSQ (EXPDEG (MULTSQ Z '(1 . 4)) (ADDSQ '(1 . 2) (NEGSQ (CAR A))))
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'BESSELI (PREPSQ (ADDSQ (CAR A) (NEGSQ '(1 . 2))))
                           (PREPSQ (MULTSQ Z '(1 . 2))))))
                   1)
                  1))
                1))))
     ((AND (EQUAL (CAR A) '(1 . 2)) (EQUAL (CAR B) '(3 . 2)))
      (MULTSQ (MULTSQ (SIMPX1 'PI 1 2) '(1 . 2))
              (COND
               ((ZNAK Z)
                (MULTSQ (SIMPFUNC 'ERFI (SIMPX1 (PREPSQ Z) 1 2))
                        (INVSQ (SIMPX1 (PREPSQ Z) 1 2))))
               (T
                (MULTSQ (SIMPFUNC 'ERF (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2))
                        (INVSQ (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2)))))))
     ((AND (EQUAL (CAR A) '(1 . 1)) (EQUAL (CAR B) '(3 . 2)) (ZNAK Z))
      (MULTSQ (MULTSQ '(1 . 2) (EXPDEG (SIMP 'E) Z))
              (MULTSQ (SIMPFUNC 'ERF (SIMPX1 (PREPSQ Z) 1 2))
                      (SIMPX1 (PREPSQ (MULTSQ (SIMP 'PI) (INVSQ Z))) 1 2))))
     (T (GHFEXIT A B Z)))) 
(PUT 'GHF21 'NUMBER-OF-ARGS 3) 
(PUT 'GHF21 'DEFINED-ON-LINE '403) 
(PUT 'GHF21 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHF21 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHF21 (A B Z)
    (COND
     ((AND (EQUAL (CAR A) '(1 . 2)) (EQUAL (CADR A) '(1 . 2))
           (EQUAL (CAR B) '(3 . 2)) (ZNAK Z))
      (MULTSQ (SIMPFUNC 'ASIN (SIMPX1 (PREPSQ Z) 1 2))
              (INVSQ (SIMPX1 (PREPSQ Z) 1 2))))
     ((AND
       (OR (AND (EQUAL (CAR A) '(1 . 2)) (EQUAL (CADR A) '(1 . 1)))
           (AND (EQUAL (CAR A) '(1 . 1)) (EQUAL (CADR A) '(1 . 2))))
       (EQUAL (CAR B) '(3 . 2)))
      (PROGN
       (COND
        ((NOT (ZNAK Z))
         (MULTSQ (SIMPFUNC 'ATAN (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2))
                 (INVSQ (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2))))
        ((NOT (EQUAL Z '(1 . 1)))
         (MULTSQ
          (SIMPFUNC 'LOG
                    (MULTSQ (ADDSQ '(1 . 1) (SIMPX1 (PREPSQ Z) 1 2))
                            (INVSQ
                             (ADDSQ '(1 . 1)
                                    (NEGSQ (SIMPX1 (PREPSQ Z) 1 2))))))
          (INVSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2)))))
        (T (GHFEXIT A B Z)))))
     ((AND (EQUAL (CAR A) '(1 . 1)) (EQUAL (CADR A) '(1 . 1))
           (EQUAL (CAR B) '(2 . 1)) (NOT (EQUAL Z '(1 . 1))))
      (MULTSQ (SIMPFUNC 'LOG (ADDSQ '(1 . 1) (NEGSQ Z))) (INVSQ (NEGSQ Z))))
     ((AND (EQUAL (ADDSQ (ADDSQ (CAR A) (CADR A)) (NEGSQ (CAR B))) '(-1 . 2))
           (OR (EQUAL (MULTSQ '(2 . 1) (CAR A)) (CAR B))
               (EQUAL (MULTSQ '(2 . 1) (CADR A)) (CAR B))))
      (MULTSQ
       (EXPDEG
        (ADDSQ '(1 . 1) (SIMPX1 (PREPSQ (ADDSQ '(1 . 1) (NEGSQ Z))) 1 2))
        (ADDSQ '(1 . 1) (NEGSQ (CAR B))))
       (EXPDEG '(2 . 1) (ADDSQ (CAR B) '(-1 . 1)))))
     ((AND (EQUAL Z '(1 . 1))
           (OR
            (NOT
             (NUMBERP
              (PREPSQ (ADDSQ (CAR B) (NEGSQ (ADDSQ (CAR A) (CADR A)))))))
            (GREATERP (PREPSQ (ADDSQ (CAR B) (NEGSQ (ADDSQ (CAR A) (CADR A)))))
                      0)))
      (MULTSQ
       (MULTSQ
        (CONS
         (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (CAR B))))) 1) 1))
         1)
        (CONS
         (LIST
          (CONS
           (CONS
            (CAR
             (FKERN
              (LIST 'GAMMA
                    (PREPSQ
                     (ADDSQ (CAR B) (NEGSQ (ADDSQ (CAR A) (CADR A))))))))
            1)
           1))
         1))
       (INVSQ
        (MULTSQ
         (CONS
          (LIST
           (CONS
            (CONS
             (CAR
              (FKERN (LIST 'GAMMA (PREPSQ (ADDSQ (CAR B) (NEGSQ (CAR A)))))))
             1)
            1))
          1)
         (CONS
          (LIST
           (CONS
            (CONS
             (CAR
              (FKERN (LIST 'GAMMA (PREPSQ (ADDSQ (CAR B) (NEGSQ (CADR A)))))))
             1)
            1))
          1)))))
     ((AND (EQUAL (CAR A) '(1 . 1)) (EQUAL (CADR A) '(1 . 1))
           (NUMBERP (PREPSQ (CAR B))) (GREATERP (PREPSQ (CAR B)) 0)
           (NOT (EQUAL Z '(1 . 1))))
      (FORMULA136 (PREPSQ (CAR B)) Z))
     (T (GHFEXIT A B Z)))) 
(PUT 'FORMULA136 'NUMBER-OF-ARGS 2) 
(PUT 'FORMULA136 'DEFINED-ON-LINE '455) 
(PUT 'FORMULA136 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'FORMULA136 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMULA136 (M Z)
    (PROG (C K)
      (SETQ K 0)
      (SETQ C '(NIL . 1))
      (SETQ K 2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ K (DIFFERENCE M 1))) (RETURN NIL)))
        (PROGN
         (SETQ C
                 (ADDSQ C
                        (MULTSQ (EXPTSQ (ADDSQ Z (NEGSQ '(1 . 1))) K)
                                (INVSQ
                                 (MULTSQ (EXPTSQ Z K)
                                         (SIMP (DIFFERENCE M K)))))))
         (SETQ K (PLUS K 1)))
        (GO WHILELABEL))
      (SETQ C
              (ADDSQ C
                     (NEGSQ
                      (MULTSQ
                       (EXPTSQ (MULTSQ (ADDSQ Z (NEGSQ '(1 . 1))) (INVSQ Z)) M)
                       (SIMPFUNC 'LOG (ADDSQ '(1 . 1) (NEGSQ Z)))))))
      (RETURN
       (MULTSQ C
               (MULTSQ (MULTSQ (SIMP (DIFFERENCE M 1)) Z)
                       (INVSQ (EXPTSQ (ADDSQ Z (NEGSQ '(1 . 1))) 2))))))) 
(PUT 'GHF12 'NUMBER-OF-ARGS 3) 
(PUT 'GHF12 'DEFINED-ON-LINE '470) 
(PUT 'GHF12 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHF12 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GHF12 (A B Z)
    (COND
     ((AND (EQUAL (CAR A) '(3 . 4))
           (OR (AND (EQUAL (CAR B) '(3 . 2)) (EQUAL (CADR B) '(7 . 4)))
               (AND (EQUAL (CAR B) '(7 . 4)) (EQUAL (CADR B) '(3 . 2))))
           (NOT (ZNAK Z)))
      (PROGN
       (SETQ Z (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2)))
       (MULTSQ
        (MULTSQ (MULTSQ '(3 . 1) (SIMPX1 'PI 1 2))
                (INVSQ (MULTSQ (SIMPX1 2 1 2) (SIMPX1 (PREPSQ Z) 3 2))))
        (SIMPFUNC 'INTFS Z))))
     ((AND (EQUAL (CAR A) '(1 . 4))
           (OR (AND (EQUAL (CAR B) '(1 . 2)) (EQUAL (CADR B) '(5 . 4)))
               (AND (EQUAL (CAR B) '(5 . 4)) (EQUAL (CADR B) '(1 . 2))))
           (NOT (ZNAK Z)))
      (PROGN
       (SETQ Z (MULTSQ (CONS 2 1) (SIMPX1 (PREPSQ (NEGSQ Z)) 1 2)))
       (MULTSQ
        (MULTSQ (SIMPX1 'PI 1 2)
                (INVSQ (MULTSQ (SIMPX1 2 1 2) (SIMPX1 (PREPSQ Z) 1 2))))
        (SIMPFUNC 'INTFC Z))))
     (T (GHFEXIT A B Z)))) 
(DE GHYPER_FEHLERF NIL
    (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")) 
(PUT 'GHYPER_FEHLERF 'NUMBER-OF-ARGS 0) 
(PUT 'GHYPER_FEHLERF 'DEFINED-ON-LINE '487) 
(PUT 'GHYPER_FEHLERF 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'GHYPER_FEHLERF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'GHYPER_FEHLERF 'INLINE
      '(LAMBDA () (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric"))) 
(PUT 'HYPERGEOM 'NUMBER-OF-ARGS 1) 
(PUT 'HYPERGEOM 'DEFINED-ON-LINE '490) 
(PUT 'HYPERGEOM 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'HYPERGEOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPERGEOM (U)
    (PROG (LIST1 LIST2 RES RES1)
      (COND
       ((NOT (EQUAL (LENGTH U) 3))
        (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (COND ((PAIRP U) (SETQ LIST1 (CAR U)))
            (T (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (COND ((PAIRP (CDR U)) (SETQ LIST2 (CADR U)))
            (T (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (COND
       ((NOT (PAIRP (CDDR U)))
        (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (COND
       ((NOT (EQCAR LIST1 'LIST))
        (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (COND
       ((NOT (EQCAR LIST2 'LIST))
        (RERROR 'SPECIALF 139 "Wrong arguments to hypergeometric")))
      (SETQ LIST1
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR LIST1))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LIST2
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR LIST2))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES
              (GHFSQ (LIST (LENGTH LIST1) (LENGTH LIST2)) LIST1 LIST2
               (SIMP (CADDR U))))
      (SETQ RES1 (PREPSQ RES))
      (RETURN (COND ((EQCAR RES1 'HYPERGEOMETRIC) RES) (T (SIMP RES1)))))) 
(REMFLAG '(HYPERGEOMETRIC) 'FULL) 
(PUT 'HYPERGEOMETRIC 'SIMPFN 'HYPERGEOM) 
(PUT 'DFFORM_HYPERGEOMETRIC 'NUMBER-OF-ARGS 3) 
(PUT 'DFFORM_HYPERGEOMETRIC 'DEFINED-ON-LINE '515) 
(PUT 'DFFORM_HYPERGEOMETRIC 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'DFFORM_HYPERGEOMETRIC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFFORM_HYPERGEOMETRIC (GHFFORM DFVAR N)
    (PROG (A B VAR FCT RESULT)
      (SETQ A (CDR (CADR GHFFORM)))
      (SETQ B (CDR (CADDR GHFFORM)))
      (SETQ VAR (CADDDR GHFFORM))
      (COND
       ((OR (DEPENDS A DFVAR) (DEPENDS B DFVAR))
        (SETQ RESULT
                (CONS
                 (LIST (CONS (GETPOWER (FKERN (LIST 'DF GHFFORM DFVAR)) 1) 1))
                 1)))
       (T
        (PROGN
         (SETQ FCT (SIMP* (LIST 'QUOTIENT (RETIMES A) (RETIMES B))))
         (SETQ RESULT
                 (SIMP*
                  (LIST 'HYPERGEOMETRIC
                        (CONS 'LIST
                              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL A)
                                (COND ((NULL EL) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL)
                                                    (LIST 'PLUS EL 1))
                                                  (CAR EL))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL (CDR EL))
                                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL) (LIST 'PLUS EL 1))
                                          (CAR EL))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                        (CONS 'LIST
                              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL B)
                                (COND ((NULL EL) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL)
                                                    (LIST 'PLUS EL 1))
                                                  (CAR EL))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL (CDR EL))
                                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL) (LIST 'PLUS EL 1))
                                          (CAR EL))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                        VAR)))
         (SETQ RESULT (MULTSQ FCT RESULT))
         (COND
          ((NEQ DFVAR VAR)
           (SETQ RESULT (MULTSQ RESULT (SIMP* (LIST 'DF VAR DFVAR)))))))))
      (COND
       ((NEQ N 1)
        (SETQ RESULT
                (MULTSQ
                 (CONS (LIST (CONS (CONS GHFFORM (DIFFERENCE N 1)) N)) 1)
                 RESULT))))
      (RETURN RESULT))) 
(PUT 'HYPERGEOMETRIC 'DFFORM 'DFFORM_HYPERGEOMETRIC) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (HYPERGEOMETRIC (LIST (QUOTIENT 1 2) (QUOTIENT 1 2))
       (LIST (QUOTIENT 3 2)) (MINUS (EXPT (~ X) 2)))
      (QUOTIENT (ASINH X) X)))))) 
(AEVAL
 (LET
  '((REPLACEBY
     (HYPERGEOMETRIC (LIST (~ A) (~ B)) (LIST (~ C))
      (MINUS (QUOTIENT (~ Z) (DIFFERENCE 1 (~ Z)))))
     (TIMES (HYPERGEOMETRIC (LIST A (DIFFERENCE C B)) (LIST C) Z)
            (EXPT (DIFFERENCE 1 Z) A)))))) 
(FLAG '(PERMUTATIONOF) 'BOOLEAN) 
(PUT 'PERMUTATIONOF 'NUMBER-OF-ARGS 2) 
(PUT 'PERMUTATIONOF 'DEFINED-ON-LINE '548) 
(PUT 'PERMUTATIONOF 'DEFINED-IN-FILE 'SPECFN/GHYPER.RED) 
(PUT 'PERMUTATIONOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PERMUTATIONOF (SET1 SET2)
    (AND (EQUAL (LENGTH SET1) (LENGTH SET2)) (NOT (SETDIFF SET1 SET2)))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 3 (TIMES 32 (SQRT 2) (EXPT (MINUS Z) (QUOTIENT 3 4))))
              (DIFFERENCE
               (TIMES
                (COSH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
                (SIN (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))
               (TIMES
                (SINH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
                (COS (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))))
       (AND
        (PERMUTATIONOF LOWERIND
         (LIST (QUOTIENT 5 4) (QUOTIENT 3 2) (QUOTIENT 7 4)))
        (NUMBERP Z) (LESSP Z 0))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 1 (TIMES 4 (EXPT (MINUS (TIMES 4 Z)) (QUOTIENT 1 4))))
              (PLUS
               (TIMES
                (SINH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
                (COS (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))
               (TIMES
                (COSH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
                (SIN (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))))
       (AND
        (PERMUTATIONOF LOWERIND
         (LIST (QUOTIENT 5 4) (QUOTIENT 1 2) (QUOTIENT 3 4)))
        (NUMBERP Z) (LESSP Z 0))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 1 (TIMES 8 (EXPT (MINUS Z) (QUOTIENT 1 2))))
              (SINH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
              (SIN (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))
       (AND
        (PERMUTATIONOF LOWERIND
         (LIST (QUOTIENT 3 4) (QUOTIENT 5 4) (QUOTIENT 3 2)))
        (NUMBERP Z) (LESSP Z 0))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (COSH (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4))))
              (COS (TIMES 2 (EXPT (MINUS (TIMES Z 4)) (QUOTIENT 1 4)))))
       (AND
        (PERMUTATIONOF LOWERIND
         (LIST (QUOTIENT 1 4) (QUOTIENT 1 2) (QUOTIENT 3 4)))
        (NUMBERP Z) (LESSP Z 0))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 3 (TIMES 64 (EXPT Z (QUOTIENT 3 4))))
              (DIFFERENCE (SINH (TIMES 4 (EXPT Z (QUOTIENT 1 4))))
                          (SIN (TIMES 4 (EXPT Z (QUOTIENT 1 4))))))
       (PERMUTATIONOF LOWERIND
        (LIST (QUOTIENT 5 4) (QUOTIENT 3 2) (QUOTIENT 7 4)))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 1 (TIMES 8 (EXPT Z (QUOTIENT 1 4))))
              (PLUS (SINH (TIMES 4 (EXPT Z (QUOTIENT 1 4))))
                    (SIN (TIMES 4 (EXPT Z (QUOTIENT 1 4))))))
       (PERMUTATIONOF LOWERIND
        (LIST (QUOTIENT 5 4) (QUOTIENT 1 2) (QUOTIENT 3 4)))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 1 (TIMES 16 (EXPT Z (QUOTIENT 1 2))))
              (DIFFERENCE (COSH (TIMES 4 (EXPT Z (QUOTIENT 1 4))))
                          (COS (TIMES 4 (EXPT Z (QUOTIENT 1 4))))))
       (PERMUTATIONOF LOWERIND
        (LIST (QUOTIENT 3 4) (QUOTIENT 5 4) (QUOTIENT 3 2)))))
     (REPLACEBY (HYPERGEOMETRIC (LIST) (~ LOWERIND) (~ Z))
      (WHEN
       (TIMES (QUOTIENT 1 2)
              (PLUS (COSH (TIMES 4 (EXPT Z (QUOTIENT 1 4))))
                    (COS (TIMES 4 (EXPT Z (QUOTIENT 1 4))))))
       (PERMUTATIONOF LOWERIND
        (LIST (QUOTIENT 1 4) (QUOTIENT 1 2) (QUOTIENT 3 4))))))))) 
(SETK 'HYPERGEOMETRIC_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST '~ 'A)) (LIST 'LIST)
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'EXPT (LIST 'DIFFERENCE 1 'X) (LIST 'MINUS 'A))
                         (LIST 'NOT
                               (LIST 'AND (LIST 'NUMBERP 'X)
                                     (LIST 'EQUAL 'X 1)))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2))
                         (LIST 'LIST (LIST 'QUOTIENT 5 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 3 (LIST 'TIMES 4 'X))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS 1 (LIST 'TIMES 2 'X)) 2)
                                     (LIST 'SQRT (LIST 'QUOTIENT 'PI 'X))
                                     (LIST 'ERFI (LIST 'SQRT 'X)))
                               (LIST 'EXPT 'E 'X))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 1 2)) (LIST '~ 'X))
                   (LIST 'PLUS 1
                         (LIST 'TIMES (LIST 'SQRT (LIST 'TIMES 'PI 'X))
                               (LIST 'EXPT 'E 'X)
                               (LIST 'ERF (LIST 'SQRT 'X)))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 3 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'SQRT (LIST 'QUOTIENT 'PI 'X))
                         (LIST 'EXPT 'E 'X) (LIST 'ERF (LIST 'SQRT 'X))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 5 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 3 (LIST 'TIMES 2 'X))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                     (LIST 'SQRT (LIST 'QUOTIENT 'PI 'X))
                                     (LIST 'EXPT 'E 'X)
                                     (LIST 'ERF (LIST 'SQRT 'X)))
                               1)))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 7 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 5 (LIST 'TIMES 4 (LIST 'EXPT 'X 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                           (LIST 'SQRT (LIST 'QUOTIENT 'PI 'X))
                                           (LIST 'EXPT 'E 'X)
                                           (LIST 'ERF (LIST 'SQRT 'X)))
                                     3)
                               (LIST 'TIMES 2 'X))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 3 2))
                         (LIST 'LIST (LIST 'QUOTIENT 5 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'EXPT 'E (LIST 'MINUS 'X))
                         (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                               (LIST 'LIST (LIST 'QUOTIENT 5 2)) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 3 2))
                         (LIST 'LIST (LIST 'QUOTIENT 5 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 3 (LIST 'TIMES 2 'X))
                         (LIST 'DIFFERENCE (LIST 'EXPT 'E 'X)
                               (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                     (LIST 'SQRT (LIST 'QUOTIENT 'PI 'X))
                                     (LIST 'ERFI (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 5 2))
                         (LIST 'LIST (LIST 'QUOTIENT 7 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'EXPT 'E (LIST 'MINUS 'X))
                         (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                               (LIST 'LIST (LIST 'QUOTIENT 7 2)) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 7 2))
                         (LIST 'LIST (LIST 'QUOTIENT 9 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'EXPT 'E (LIST 'MINUS 'X))
                         (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                               (LIST 'LIST (LIST 'QUOTIENT 9 2)) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST '~ 'A))
                         (LIST 'LIST (LIST '~ 'B)) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'A
                               (LIST 'EXPT (LIST 'MINUS 'X) (LIST 'MINUS 'A))
                               (LIST 'M_GAMMA 'A (LIST 'MINUS 'X)))
                         (LIST 'EQUAL 'B (LIST 'PLUS 'A 1))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST '~ 'A))
                         (LIST 'LIST (LIST '~ 'B)) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'PLUS 'A 1)
                               (LIST 'PLUS (LIST 'EXPT 'E 'X)
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE (LIST 'MINUS 'X)
                                                 'A)
                                           (LIST 'EXPT (LIST 'MINUS 'X)
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'MINUS 'A) 1))
                                           (LIST 'M_GAMMA (LIST 'PLUS 'A 1)
                                                 (LIST 'MINUS 'X)))))
                         (LIST 'EQUAL 'B (LIST 'PLUS 'A 2))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 1)
                         (LIST 'LIST (LIST 'QUOTIENT 3 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'PLUS 1 'X)
                                     (LIST 'QUOTIENT
                                           (LIST 'ATAN (LIST 'SQRT 'X))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 1)
                         (LIST 'LIST (LIST 'QUOTIENT 3 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'DIFFERENCE 1 'X)
                                     (LIST 'QUOTIENT
                                           (LIST 'ATANH (LIST 'SQRT 'X))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2) 1)
                         (LIST 'LIST (LIST 'QUOTIENT 5 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES
                         (LIST 'TIMES (LIST 'QUOTIENT 3 2) (LIST 'MINUS 'X))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'PLUS 1 'X)
                                     (LIST 'QUOTIENT
                                           (LIST 'ATAN (LIST 'SQRT 'X))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2) 1)
                         (LIST 'LIST (LIST 'QUOTIENT 5 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'TIMES (LIST 'QUOTIENT 3 2) 'X)
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'DIFFERENCE 1 'X)
                                     (LIST 'QUOTIENT
                                           (LIST 'ATANH (LIST 'SQRT 'X))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST
                               (LIST 'PLUS (LIST '~ 'A) (LIST 'QUOTIENT 1 2))
                               (LIST '~ 'A))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'DIFFERENCE 1 'X) (LIST 'MINUS 'A))
                         (LIST 'COS
                               (LIST 'TIMES 2 'A
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'QUOTIENT 5 4) (LIST 'QUOTIENT 3 4))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'DIFFERENCE 1 'X)
                               (LIST 'MINUS (LIST 'QUOTIENT 3 4)))
                         (LIST 'COS
                               (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS (LIST '~ 'N) 1) 2)
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'N) 1) 2))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'DIFFERENCE 1 'X)
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (LIST 'PLUS 'N 1) 2)))
                         (LIST 'COS
                               (LIST 'TIMES (LIST 'PLUS 'N 1)
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'QUOTIENT 7 4) (LIST 'QUOTIENT 5 4))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2)) (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'DIFFERENCE 1 'X)
                                     (LIST 'MINUS (LIST 'QUOTIENT 3 4)))
                               (LIST 'SQRT (LIST 'MINUS 'X)))
                         (LIST 'SIN
                               (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST
                               (LIST 'PLUS (LIST '~ 'A) (LIST 'QUOTIENT 1 2))
                               (LIST '~ 'A))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'DIFFERENCE 1 'X)
                                     (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 2)
                                           'A))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE (LIST 'TIMES 2 'A) 1)
                                     (LIST 'SQRT (LIST 'MINUS 'X))))
                         (LIST 'SIN
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE (LIST 'TIMES 2 'A) 1)
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS (LIST '~ 'N) 2) 2)
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'N) 2) 2))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2)) (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'DIFFERENCE 1 'X)
                                     (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 2)
                                           (LIST 'QUOTIENT (LIST 'PLUS 'N 2)
                                                 2)))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 2) 2))
                                           1)
                                     (LIST 'SQRT (LIST 'MINUS 'X))))
                         (LIST 'SIN
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 2) 2))
                                           1)
                                     (LIST 'ATAN
                                           (LIST 'SQRT (LIST 'MINUS 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2) (LIST 'QUOTIENT 1 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'PLUS (LIST 'COS (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                         (LIST 'TIMES 2 (LIST 'SQRT 'X)
                               (LIST 'SI (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2) (LIST 'QUOTIENT 1 2))
                         (LIST '~ 'X))
                   (LIST 'DIFFERENCE
                         (LIST 'COSH (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                         (LIST 'TIMES 2 'X
                               (LIST 'SHI (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2) (LIST 'QUOTIENT 3 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS
                               (LIST 'COS (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'TIMES 2 (LIST 'SQRT 'X)
                                     (LIST 'SI
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 1 2) (LIST 'QUOTIENT 3 2))
                         (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS
                               (LIST 'COSH (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT
                                           (LIST 'SINH
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X)))
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)
                                           (LIST 'SHI
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X))))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2) (LIST 'QUOTIENT 3 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                         (LIST 'PLUS
                               (LIST 'COS (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'TIMES (LIST 'PLUS 1 (LIST 'TIMES 2 'X))
                                     (LIST 'QUOTIENT
                                           (LIST 'SI
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X)))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC
                         (LIST 'LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2) (LIST 'QUOTIENT 3 2))
                         (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                         (LIST 'PLUS
                               (LIST 'COSH (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'QUOTIENT
                                     (LIST 'SINH
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE 1 (LIST 'TIMES 2 'X))
                                     (LIST 'QUOTIENT
                                           (LIST 'SHI
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X)))
                                           (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2) (LIST 'QUOTIENT 3 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'QUOTIENT (LIST 'SI (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                         (LIST 'TIMES 2 (LIST 'SQRT 'X))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2))
                         (LIST 'LIST (LIST 'QUOTIENT 3 2) (LIST 'QUOTIENT 3 2))
                         (LIST '~ 'X))
                   (LIST 'QUOTIENT (LIST 'SHI (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                         (LIST 'TIMES 2 (LIST 'SQRT 'X))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2))
                         (LIST 'LIST (LIST 'QUOTIENT 5 2) (LIST 'QUOTIENT 3 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 3 (LIST 'TIMES 8 (LIST 'MINUS 'X)))
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)
                                           (LIST 'SI
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X))))
                                     (LIST 'COS
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X))))
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 1 2))
                         (LIST 'LIST (LIST 'QUOTIENT 5 2) (LIST 'QUOTIENT 3 2))
                         (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 3 (LIST 'TIMES 8 'X))
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X)
                                           (LIST 'SHI
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X))))
                                     (LIST 'COSH
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X))))
                               (LIST 'QUOTIENT
                                     (LIST 'SINH
                                           (LIST 'TIMES 2 (LIST 'SQRT 'X)))
                                     (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 3 4) (LIST 'QUOTIENT 5 4))
                         (LIST '~ 'X))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'SQRT
                               (LIST 'QUOTIENT 'PI
                                     (LIST 'SQRT (LIST 'MINUS 'X))))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))
                                     (LIST 'FRESNEL_C
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X)))))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))
                                     (LIST 'FRESNEL_S
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST 1)
                         (LIST 'LIST (LIST 'QUOTIENT 5 4) (LIST 'QUOTIENT 7 4))
                         (LIST '~ 'X))
                   (LIST 'TIMES 3
                         (LIST 'QUOTIENT (LIST 'SQRT 'PI)
                               (LIST 'TIMES 8
                                     (LIST 'EXPT (LIST 'SQRT (LIST 'MINUS 'X))
                                           (LIST 'QUOTIENT 3 2))))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))
                                     (LIST 'FRESNEL_C
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X)))))
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))
                                     (LIST 'FRESNEL_S
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS 'X))))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 5 2))
                         (LIST 'LIST (LIST 'QUOTIENT 7 2) (LIST 'QUOTIENT 7 2))
                         (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 75 (LIST 'TIMES 16 (LIST 'EXPT 'X 2)))
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 3
                                           (LIST 'QUOTIENT
                                                 (LIST 'SI
                                                       (LIST 'TIMES 2
                                                             (LIST 'SQRT 'X)))
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X))))
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'SIN
                                                       (LIST 'TIMES 2
                                                             (LIST 'SQRT 'X)))
                                                 (LIST 'SQRT 'X))))
                               (LIST 'COS (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST 'QUOTIENT 5 2))
                         (LIST 'LIST (LIST 'QUOTIENT 7 2) (LIST 'QUOTIENT 7 2))
                         (LIST '~ 'X))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 75 (LIST 'TIMES 16 (LIST 'EXPT 'X 2)))
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 3
                                           (LIST 'QUOTIENT
                                                 (LIST 'SHI
                                                       (LIST 'TIMES 2
                                                             (LIST 'SQRT 'X)))
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT 'X))))
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'SINH
                                                       (LIST 'TIMES 2
                                                             (LIST 'SQRT 'X)))
                                                 (LIST 'SQRT 'X))))
                               (LIST 'COSH (LIST 'TIMES 2 (LIST 'SQRT 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST '~ 'A))
                         (LIST 'LIST (LIST '~ 'B) (LIST 'QUOTIENT 3 2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'EXPT 2
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'TIMES 2 'A)))
                                     'A
                                     (LIST 'EXPT (LIST 'SQRT (LIST 'MINUS 'X))
                                           (LIST 'MINUS (LIST 'TIMES 2 'A)))
                                     (LIST 'PLUS
                                           (LIST 'TIMES
                                                 (LIST 'GAMMA
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES 2 'A)
                                                             1))
                                                 (LIST 'COS
                                                       (LIST 'TIMES 'A 'PI)))
                                           (LIST 'FRESNEL_S
                                                 (LIST 'TIMES 2
                                                       (LIST 'SQRT
                                                             (LIST 'MINUS 'X)))
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 'A)
                                                       1)))))
                         (LIST 'EQUAL 'B (LIST 'PLUS 'A 1))))
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOMETRIC (LIST 'LIST (LIST '~ 'A))
                         (LIST 'LIST (LIST '~ 'B) (LIST 'QUOTIENT 1 2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT 2
                                     (LIST 'DIFFERENCE 1 (LIST 'TIMES 2 'A)))
                               'A
                               (LIST 'EXPT (LIST 'SQRT (LIST 'MINUS 'X))
                                     (LIST 'MINUS (LIST 'TIMES 2 'A)))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'GAMMA (LIST 'TIMES 2 'A))
                                           (LIST 'COS (LIST 'TIMES 'A 'PI)))
                                     (LIST 'FRESNEL_C
                                           (LIST 'TIMES 2
                                                 (LIST 'SQRT (LIST 'MINUS 'X)))
                                           (LIST 'TIMES 2 'A))))
                         (LIST 'EQUAL 'B (LIST 'PLUS 'A 1))))))) 
(AEVAL (LET '(HYPERGEOMETRIC_RULES))) 
(AEVAL (OPERATOR (LIST 'POISSON-CHARLIER 'TORONTO))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (TORONTO (~ M) (~ N) (~ X))
      (TIMES
       (QUOTIENT (GAMMA (PLUS (QUOTIENT M 2) (QUOTIENT 1 2))) (FACTORIAL N))
       (EXPT X (PLUS (DIFFERENCE (TIMES 2 N) (TIMES 2 M)) 1))
       (EXP (MINUS (EXPT X 2)))
       (KUMMERM (PLUS (QUOTIENT M 2) (QUOTIENT 1 2)) (PLUS 1 N) (EXPT X 2))))
     (REPLACEBY (POISSON-CHARLIER (~ N) (~ NU) (~ X))
      (TIMES
       (QUOTIENT (POCHHAMMER (PLUS 1 (DIFFERENCE NU N)) N)
                 (TIMES (SQRT (FACTORIAL N)) (EXPT X (QUOTIENT N 2))))
       (SUM
        (TIMES (POCHHAMMER (MINUS N) I)
               (QUOTIENT (EXPT X I)
                         (TIMES (POCHHAMMER (PLUS 1 (DIFFERENCE NU N)) I)
                                (FACTORIAL I))))
        I 0 N))))))) 
(AEVAL 'NIL) 
(ENDMODULE) 
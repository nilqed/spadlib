(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MEIJERG)) 
(FLAG '(MEIJERG) 'SPECFN) 
(PUT 'MEIJERG 'NUMBER-OF-ARGS 3) 
(DE MEIJERG_FEHLER NIL
    (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")) 
(PUT 'MEIJERG_FEHLER 'NUMBER-OF-ARGS 0) 
(PUT 'MEIJERG_FEHLER 'DEFINED-ON-LINE '39) 
(PUT 'MEIJERG_FEHLER 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'MEIJERG_FEHLER 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'MEIJERG_FEHLER 'INLINE
      '(LAMBDA () (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG"))) 
(PUT 'SIMPMEIJERG 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPMEIJERG 'DEFINED-ON-LINE '42) 
(PUT 'SIMPMEIJERG 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SIMPMEIJERG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPMEIJERG (U)
    (PROG (LIST1 LIST2 LIST1A LIST2A)
      (COND ((PAIRP U) (SETQ LIST1 (CAR U)))
            (T (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")))
      (COND ((PAIRP (CDR U)) (SETQ LIST2 (CADR U)))
            (T (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")))
      (COND
       ((NOT (PAIRP (CDDR U)))
        (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")))
      (COND
       ((NOT (EQCAR LIST1 'LIST))
        (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")))
      (COND
       ((NOT (EQCAR LIST2 'LIST))
        (RERROR 'SPECIALF 140 "Wrong arguments to operator MeijerG")))
      (SETQ LIST1A
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDADR LIST1))
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
      (SETQ LIST2A
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDADR LIST2))
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
      (SETQ LIST1
              (CONS LIST1A
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (CDDR LIST1))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X) (SIMP (REVAL1 X T)))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ LIST2
              (CONS LIST2A
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (CDDR LIST2))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X) (SIMP (REVAL1 X T)))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ LIST1 (GFMSQ LIST1 LIST2 (SIMP (CADDR U))))
      (COND ((EQUAL LIST1 'FAIL) (RETURN (SIMP 'FAIL)))
            (T (SETQ LIST1 (PREPSQ LIST1))))
      (COND ((EQCAR LIST1 'MEIJERG) (RETURN LIST1)) (T (RETURN (SIMP LIST1)))))) 
(REMFLAG '(MEIJERG) 'FULL) 
(PUT 'MEIJERG 'SIMPFN 'SIMPMEIJERG) 
(COND
 ((NOT (GETD 'SIMPMEIJERG))
  (FLAG
   '(F6 F8 F9 F10 F11 F12 F13 F14 F26 F27 F28 F29 F30 F31 F32 F33 F34 F35 F36
     F37 F38 F39)
   'INTERNALFUNCTION))) 
(SWITCH (LIST 'TRACESPECFNS)) 
(PUT 'GFMSQ 'NUMBER-OF-ARGS 3) 
(PUT 'GFMSQ 'DEFINED-ON-LINE '77) 
(PUT 'GFMSQ 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMSQ (A B Z)
    (PROG (V1 V2 M N P Q AA BB)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ P 0)
      (SETQ Q 0)
      (SETQ AA 0)
      (SETQ BB 0)
      (SETQ V1 (REDPAR (CAR B) (CDR A)))
      (SETQ V2 (REDPAR (CDR B) (CAR A)))
      (SETQ AA (CONS (CADR V2) (CADR V1)))
      (SETQ BB (CONS (CAR V1) (CAR V2)))
      (SETQ A (APPEND (CAR AA) (CDR AA)))
      (SETQ B (APPEND (CAR BB) (CDR BB)))
      (SETQ M (LENGTH (CAR V1)))
      (SETQ N (LENGTH (CADR V2)))
      (SETQ Q (PLUS M (LENGTH (CAR V2))))
      (SETQ P (PLUS N (LENGTH (CADR V1))))
      (COND
       (*TRACESPECFNS
        (PROGN
         (PRIN2
          (LIST "MeijerG<" M N P Q ">" A "|" B "|" Z "|aa=" AA "|bb=" BB))
         (TERPRI))))
      (COND
       ((AND (EQUAL P 0) (EQUAL Q 0))
        (RETURN (PROGN (RERROR 'SPECIALF 141 "DIVERGENT INTEGRAL") 'FAIL))))
      (COND ((GREATERP P Q) (RETURN (GFMINVERS AA BB Z)))
            ((OR (GREATERP Q 3) (GREATERP P 3)) (RETURN (SIMPGTOH AA BB Z)))
            ((AND (EQUAL Q 3) (EQUAL P 1)) (GO Q3))
            ((AND (EQUAL Q 2) (OR (EQUAL P 0) (EQUAL P 1))) (GO Q2))
            ((EQUAL Q 1) (GO Q1)) (T (RETURN (SIMPGTOH AA BB Z))))
     Q1
      (COND
       ((AND (EQUAL P 0) (EQUAL N 0) (EQUAL M 1))
        (RETURN (MULTSQ (EXPDEG Z (CAR B)) (EXPDEG (SIMP* 'E) (NEGSQ Z)))))
       ((AND (EQUAL P 1) (EQUAL N 0) (EQUAL M 1) (NULL (CAAR B))
             (EQUAL (CAR A) '(1 . 1)))
        (RETURN (GFMEXIT AA BB Z)))
       ((AND (EQUAL P 1) (EQUAL N 0) (EQUAL M 1))
        (RETURN
         (MULTSQ
          (CONS
           (LIST
            (CONS
             (CONS
              (CAR
               (FKERN (LIST 'HEAVISIDE (PREPSQ (ADDSQ '(1 . 1) (NEGSQ Z))))))
              1)
             1))
           1)
          (MULTSQ
           (MULTSQ (EXPDEG Z (CAR B))
                   (EXPDEG (ADDSQ '(1 . 1) (NEGSQ Z))
                           (ADDSQ (CAR A) (NEGSQ (ADDSQ '(1 . 1) (CAR B))))))
           (INVSQ
            (CONS
             (LIST
              (CONS
               (CONS
                (CAR
                 (FKERN
                  (LIST 'GAMMA (PREPSQ (ADDSQ (CAR A) (NEGSQ (CAR B)))))))
                1)
               1))
             1))))))
       ((AND (EQUAL P 1) (EQUAL N 1) (EQUAL M 0) (NULL (CAAR B))
             (EQUAL (CAR A) '(1 . 1)))
        (RETURN (GFMEXIT AA BB Z)))
       ((AND (EQUAL P 1) (EQUAL N 1) (EQUAL M 0))
        (RETURN
         (MULTSQ
          (CONS
           (LIST
            (CONS
             (CONS
              (CAR
               (FKERN (LIST 'HEAVISIDE (PREPSQ (ADDSQ Z (NEGSQ '(1 . 1)))))))
              1)
             1))
           1)
          (MULTSQ
           (MULTSQ (EXPDEG Z (CAR B))
                   (EXPDEG (ADDSQ Z (NEGSQ '(1 . 1)))
                           (ADDSQ (CAR A) (NEGSQ (ADDSQ '(1 . 1) (CAR B))))))
           (INVSQ
            (CONS
             (LIST
              (CONS
               (CONS
                (CAR
                 (FKERN
                  (LIST 'GAMMA (PREPSQ (ADDSQ (CAR A) (NEGSQ (CAR B)))))))
                1)
               1))
             1))))))
       ((AND (EQUAL P 1) (EQUAL N 1) (EQUAL M 1))
        (RETURN
         (MULTSQ
          (CONS
           (LIST
            (CONS
             (CONS
              (CAR
               (FKERN
                (LIST 'GAMMA
                      (PREPSQ
                       (ADDSQ '(1 . 1)
                              (NEGSQ (ADDSQ (CAR A) (NEGSQ (CAR B)))))))))
              1)
             1))
           1)
          (MULTSQ (EXPDEG Z (CAR B))
                  (EXPDEG (ADDSQ '(1 . 1) Z)
                          (ADDSQ (CAR A) (NEGSQ (ADDSQ '(1 . 1) (CAR B)))))))))
       (T
        (RETURN (RERROR 'SPECIALF 142 "***** parameter error in G-function"))))
     Q2
      (COND ((EQUAL P 2) (RETURN (SIMPGTOH AA BB Z))) ((EQUAL P 1) (GO Q2P1))
            ((AND (EQUAL P 0) (EQUAL M 1)) (RETURN (F6 (CAR B) (CADR B) Z)))
            ((AND (EQUAL P 0) (EQUAL M 2)) (RETURN (F8 (CAR B) (CADR B) Z)))
            (T
             (RETURN
              (RERROR 'SPECIALF 143 "***** parameter error in G-function"))))
     Q2P1
      (COND ((AND (EQUAL M 1) (EQUAL N 0)) (RETURN (Q2P1M1N0 A B Z)))
            ((AND (EQUAL M 2) (EQUAL N 0)) (RETURN (Q2P1M2N0 A B Z)))
            ((AND (EQUAL M 2) (EQUAL N 1)) (RETURN (Q2P1M2N1 A B Z)))
            (T (RETURN (SIMPGTOH AA BB Z))))
     Q3
      (COND ((EQUAL P 1) (GO Q3P1)) (T (RETURN (SIMPGTOH AA BB Z))))
     Q3P1
      (COND ((AND (EQUAL M 1) (EQUAL N 1)) (RETURN (Q3P1M1N1 A B Z)))
            ((AND (EQUAL M 2) (EQUAL N 0)) (RETURN (Q3P1M2N0 A B Z)))
            ((AND (EQUAL M 2) (EQUAL N 1)) (RETURN (Q3P1M2N1 A B Z)))
            ((AND (EQUAL M 3) (EQUAL N 0)) (RETURN (Q3P1M3N0 A B Z)))
            ((AND (EQUAL M 3) (EQUAL N 1)) (RETURN (Q3P1M3N1 A B Z)))
            (T (RETURN (SIMPGTOH AA BB Z)))))) 
(PUT 'GFMINVERS 'NUMBER-OF-ARGS 3) 
(PUT 'GFMINVERS 'DEFINED-ON-LINE '156) 
(PUT 'GFMINVERS 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMINVERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMINVERS (A B Z)
    (GFMSQ
     (CONS
      ((LAMBDA (G125)
         (PROG (VV FORALL-RESULT FORALL-ENDPTR)
           (SETQ VV (CAR B))
           (COND ((NULL VV) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (VV) (ADDSQ G125 (NEGSQ VV))) (CAR VV))
                            NIL)))
          LOOPLABEL
           (SETQ VV (CDR VV))
           (COND ((NULL VV) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (VV) (ADDSQ G125 (NEGSQ VV))) (CAR VV)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
       (CONS '1 1))
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CDR B))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (CONS
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CAR A))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CDR A))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (INVSQ Z))) 
(PUT 'F6 'NUMBER-OF-ARGS 3) 
(PUT 'F6 'DEFINED-ON-LINE '161) 
(PUT 'F6 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F6 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F6 (A B Z)
    (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ A B)))
            (CONS
             (LIST
              (CONS
               (CONS
                (CAR
                 (FKERN
                  (LIST 'BESSELJ (PREPSQ (ADDSQ A (NEGSQ B)))
                        (PREPSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                1)
               1))
             1))) 
(PUT 'F8 'NUMBER-OF-ARGS 3) 
(PUT 'F8 'DEFINED-ON-LINE '165) 
(PUT 'F8 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F8 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F8 (A B Z)
    (MULTSQ '(2 . 1)
            (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ A B)))
                    (CONS
                     (LIST
                      (CONS
                       (CONS
                        (CAR
                         (FKERN
                          (LIST 'BESSELK (PREPSQ (ADDSQ A (NEGSQ B)))
                                (PREPSQ
                                 (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                        1)
                       1))
                     1)))) 
(PUT 'SIMPGTOH 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPGTOH 'DEFINED-ON-LINE '173) 
(PUT 'SIMPGTOH 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SIMPGTOH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPGTOH (A B Z)
    (COND
     ((GEQ (PLUS (LENGTH (CAR B)) (LENGTH (CDR B)))
           (PLUS (LENGTH (CAR A)) (LENGTH (CDR A))))
      (FROMGTOH A B Z))
     (T
      (FROMGTOH
       (CONS
        (PROG (VV FORALL-RESULT FORALL-ENDPTR)
          (SETQ VV (CAR B))
          (COND ((NULL VV) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                           NIL)))
         LOOPLABEL
          (SETQ VV (CDR VV))
          (COND ((NULL VV) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        (PROG (VV FORALL-RESULT FORALL-ENDPTR)
          (SETQ VV (CDR B))
          (COND ((NULL VV) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                           NIL)))
         LOOPLABEL
          (SETQ VV (CDR VV))
          (COND ((NULL VV) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))
       (CONS
        (PROG (VV FORALL-RESULT FORALL-ENDPTR)
          (SETQ VV (CAR A))
          (COND ((NULL VV) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                           NIL)))
         LOOPLABEL
          (SETQ VV (CDR VV))
          (COND ((NULL VV) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        (PROG (VV FORALL-RESULT FORALL-ENDPTR)
          (SETQ VV (CDR A))
          (COND ((NULL VV) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                           NIL)))
         LOOPLABEL
          (SETQ VV (CDR VV))
          (COND ((NULL VV) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))
       (INVSQ Z))))) 
(PUT 'FROMGTOH 'NUMBER-OF-ARGS 3) 
(PUT 'FROMGTOH 'DEFINED-ON-LINE '207) 
(PUT 'FROMGTOH 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'FROMGTOH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FROMGTOH (A B Z)
    (COND ((NULL (CAR B)) (GFMEXIT A B Z))
          ((AND (NOT (NULL A))
                (LISTFOOLTWO
                 (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                   (SETQ UU (CAR B))
                   (COND ((NULL UU) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1))))
                                     (CAR UU))
                                    NIL)))
                  LOOPLABEL
                   (SETQ UU (CDR UU))
                   (COND ((NULL UU) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1))))
                             (CAR UU))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (CAR A)))
           'FAIL)
          ((LISTFOOL (CAR B)) (GFMLOGCASE A B Z))
          ((LEQ (PLUS (LENGTH (CAR A)) (LENGTH (CDR A)))
                (PLUS (LENGTH (CAR B)) (LENGTH (CDR B))))
           (ALLSIMPLPOLES (CAR B) A B Z))
          (T (ALLSIMPLPOLES (CAR A) A B Z)))) 
(PUT 'GFMEXIT 'NUMBER-OF-ARGS 3) 
(PUT 'GFMEXIT 'DEFINED-ON-LINE '223) 
(PUT 'GFMEXIT 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMEXIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMEXIT (A B Z)
    (PROG (MNPQ AA BB)
      (COND
       ((GREATERP (PLUS (LENGTH (CAR A)) (LENGTH (CDR A)))
                  (PLUS (LENGTH (CAR B)) (LENGTH (CDR B))))
        (RETURN (GFMEXITINVERS A B Z))))
      (SETQ MNPQ
              (CONS 'LST
                    (LIST (LENGTH (CAR B)) (LENGTH (CAR A))
                          (PLUS (LENGTH (CAR A)) (LENGTH (CDR A)))
                          (PLUS (LENGTH (CAR B)) (LENGTH (CDR B))))))
      (SETQ AA
              (CONS 'LST
                    (APPEND
                     (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                       (SETQ UU (CAR A))
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
                       (GO LOOPLABEL))
                     (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                       (SETQ UU (CDR A))
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
                       (GO LOOPLABEL)))))
      (SETQ BB
              (CONS 'LST
                    (APPEND
                     (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                       (SETQ UU (CAR B))
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
                       (GO LOOPLABEL))
                     (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                       (SETQ UU (CDR B))
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
                       (GO LOOPLABEL)))))
      (RETURN
       (CONS
        (LIST
         (CONS (CONS (CAR (FKERN (CONS 'GFM (LIST MNPQ AA BB (PREPSQ Z))))) 1)
               1))
        1)))) 
(PUT 'GFMEXITINVERS 'NUMBER-OF-ARGS 3) 
(PUT 'GFMEXITINVERS 'DEFINED-ON-LINE '236) 
(PUT 'GFMEXITINVERS 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMEXITINVERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMEXITINVERS (A B Z)
    (GFMEXIT
     (CONS
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CAR B))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CDR B))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (CONS
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CAR A))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
        (SETQ VV (CDR A))
        (COND ((NULL VV) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                         NIL)))
       LOOPLABEL
        (SETQ VV (CDR VV))
        (COND ((NULL VV) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (VV) (ADDSQ '(1 . 1) (NEGSQ VV))) (CAR VV))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (INVSQ Z))) 
(PUT 'ALLSIMPLPOLES 'NUMBER-OF-ARGS 4) 
(PUT 'ALLSIMPLPOLES 'DEFINED-ON-LINE '241) 
(PUT 'ALLSIMPLPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'ALLSIMPLPOLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ALLSIMPLPOLES (V A B Z)
    (COND ((NULL V) '(NIL . 1))
          (T
           (ADDSQ
            (INFINITYSIMPLPOLES A
             (CONS (CAR (REDPAR (CAR B) (LIST (CAR V)))) (CDR B)) (CAR V) Z)
            (ALLSIMPLPOLES (CDR V) A B Z))))) 
(PUT 'INFINITYSIMPLPOLES 'NUMBER-OF-ARGS 4) 
(PUT 'INFINITYSIMPLPOLES 'DEFINED-ON-LINE '247) 
(PUT 'INFINITYSIMPLPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'INFINITYSIMPLPOLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INFINITYSIMPLPOLES (A B V Z)
    (PROG (COEFGAM)
      (SETQ COEFGAM
              (MULTSQ
               (MULTSQ
                ((LAMBDA (P)
                   (PROGN
                    (PROG (PP)
                      (SETQ PP
                              (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                                (SETQ UU (CAR B))
                                (COND ((NULL UU) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (UU)
                                                    (ADDSQ UU (NEGSQ V)))
                                                  (CAR UU))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ UU (CDR UU))
                                (COND ((NULL UU) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (UU) (ADDSQ UU (NEGSQ V)))
                                          (CAR UU))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                     LAB
                      (COND ((NULL PP) (RETURN NIL)))
                      ((LAMBDA (PP)
                         (PROGN
                          (SETQ P
                                  (MULTSQ
                                   (CONS
                                    (LIST
                                     (CONS
                                      (CONS
                                       (CAR (FKERN (LIST 'GAMMA (PREPSQ PP))))
                                       1)
                                      1))
                                    1)
                                   P))))
                       (CAR PP))
                      (SETQ PP (CDR PP))
                      (GO LAB))
                    P))
                 '(1 . 1))
                (COND ((OR (NULL A) (NULL (CAR A))) '(1 . 1))
                      (T
                       ((LAMBDA (P)
                          (PROGN
                           (PROG (PP)
                             (SETQ PP
                                     ((LAMBDA (G127)
                                        (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ VV (CAR A))
                                          (COND ((NULL VV) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (VV)
                                                              (ADDSQ G127
                                                                     (NEGSQ
                                                                      VV)))
                                                            (CAR VV))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ VV (CDR VV))
                                          (COND
                                           ((NULL VV) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (VV)
                                                      (ADDSQ G127 (NEGSQ VV)))
                                                    (CAR VV))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                      (ADDSQ '(1 . 1) V)))
                            LAB
                             (COND ((NULL PP) (RETURN NIL)))
                             ((LAMBDA (PP)
                                (PROGN
                                 (SETQ P
                                         (MULTSQ
                                          (CONS
                                           (LIST
                                            (CONS
                                             (CONS
                                              (CAR
                                               (FKERN
                                                (LIST 'GAMMA (PREPSQ PP))))
                                              1)
                                             1))
                                           1)
                                          P))))
                              (CAR PP))
                             (SETQ PP (CDR PP))
                             (GO LAB))
                           P))
                        '(1 . 1)))))
               (INVSQ
                (MULTSQ
                 (COND ((NULL (CDR B)) '(1 . 1))
                       (T
                        ((LAMBDA (P)
                           (PROGN
                            (PROG (PP)
                              (SETQ PP
                                      ((LAMBDA (G129)
                                         (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ VV (CDR B))
                                           (COND ((NULL VV) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (VV)
                                                               (ADDSQ G129
                                                                      (NEGSQ
                                                                       VV)))
                                                             (CAR VV))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ VV (CDR VV))
                                           (COND
                                            ((NULL VV) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (VV)
                                                       (ADDSQ G129 (NEGSQ VV)))
                                                     (CAR VV))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (ADDSQ '(1 . 1) V)))
                             LAB
                              (COND ((NULL PP) (RETURN NIL)))
                              ((LAMBDA (PP)
                                 (PROGN
                                  (SETQ P
                                          (MULTSQ
                                           (CONS
                                            (LIST
                                             (CONS
                                              (CONS
                                               (CAR
                                                (FKERN
                                                 (LIST 'GAMMA (PREPSQ PP))))
                                               1)
                                              1))
                                            1)
                                           P))))
                               (CAR PP))
                              (SETQ PP (CDR PP))
                              (GO LAB))
                            P))
                         '(1 . 1))))
                 (COND ((OR (NULL A) (NULL (CDR A))) '(1 . 1))
                       (T
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
                                                            (ADDSQ UU
                                                                   (NEGSQ V)))
                                                          (CAR UU))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ UU (CDR UU))
                                        (COND
                                         ((NULL UU) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (UU)
                                                    (ADDSQ UU (NEGSQ V)))
                                                  (CAR UU))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                             LAB
                              (COND ((NULL PP) (RETURN NIL)))
                              ((LAMBDA (PP)
                                 (PROGN
                                  (SETQ P
                                          (MULTSQ
                                           (CONS
                                            (LIST
                                             (CONS
                                              (CONS
                                               (CAR
                                                (FKERN
                                                 (LIST 'GAMMA (PREPSQ PP))))
                                               1)
                                              1))
                                            1)
                                           P))))
                               (CAR PP))
                              (SETQ PP (CDR PP))
                              (GO LAB))
                            P))
                         '(1 . 1))))))))
      (RETURN
       (MULTSQ (MULTSQ COEFGAM (EXPDEG Z V))
               (GHFSQ
                (LIST (PLUS (LENGTH (CAR A)) (LENGTH (CDR A)))
                      (PLUS (LENGTH (CAR B)) (LENGTH (CDR B))))
                (COND ((NULL A) NIL)
                      ((NULL (CAR A))
                       ((LAMBDA (G131)
                          (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                            (SETQ VV (CDR A))
                            (COND ((NULL VV) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (VV)
                                                (ADDSQ G131 (NEGSQ VV)))
                                              (CAR VV))
                                             NIL)))
                           LOOPLABEL
                            (SETQ VV (CDR VV))
                            (COND ((NULL VV) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (VV) (ADDSQ G131 (NEGSQ VV)))
                                      (CAR VV))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                        (ADDSQ '(1 . 1) V)))
                      (T
                       (APPEND
                        ((LAMBDA (G133)
                           (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                             (SETQ VV (CAR A))
                             (COND ((NULL VV) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (VV)
                                                 (ADDSQ G133 (NEGSQ VV)))
                                               (CAR VV))
                                              NIL)))
                            LOOPLABEL
                             (SETQ VV (CDR VV))
                             (COND ((NULL VV) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (VV) (ADDSQ G133 (NEGSQ VV)))
                                       (CAR VV))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (ADDSQ '(1 . 1) V))
                        ((LAMBDA (G135)
                           (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                             (SETQ VV (CDR A))
                             (COND ((NULL VV) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (VV)
                                                 (ADDSQ G135 (NEGSQ VV)))
                                               (CAR VV))
                                              NIL)))
                            LOOPLABEL
                             (SETQ VV (CDR VV))
                             (COND ((NULL VV) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (VV) (ADDSQ G135 (NEGSQ VV)))
                                       (CAR VV))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (ADDSQ '(1 . 1) V)))))
                (COND
                 ((NULL (CDR B))
                  ((LAMBDA (G137)
                     (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                       (SETQ VV (CAR B))
                       (COND ((NULL VV) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (VV) (ADDSQ G137 (NEGSQ VV)))
                                         (CAR VV))
                                        NIL)))
                      LOOPLABEL
                       (SETQ VV (CDR VV))
                       (COND ((NULL VV) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (VV) (ADDSQ G137 (NEGSQ VV)))
                                 (CAR VV))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
                   (ADDSQ '(1 . 1) V)))
                 (T
                  (APPEND
                   ((LAMBDA (G139)
                      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                        (SETQ VV (CAR B))
                        (COND ((NULL VV) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (VV) (ADDSQ G139 (NEGSQ VV)))
                                          (CAR VV))
                                         NIL)))
                       LOOPLABEL
                        (SETQ VV (CDR VV))
                        (COND ((NULL VV) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (VV) (ADDSQ G139 (NEGSQ VV)))
                                  (CAR VV))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                    (ADDSQ '(1 . 1) V))
                   ((LAMBDA (G141)
                      (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                        (SETQ VV (CDR B))
                        (COND ((NULL VV) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (VV) (ADDSQ G141 (NEGSQ VV)))
                                          (CAR VV))
                                         NIL)))
                       LOOPLABEL
                        (SETQ VV (CDR VV))
                        (COND ((NULL VV) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (VV) (ADDSQ G141 (NEGSQ VV)))
                                  (CAR VV))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                    (ADDSQ '(1 . 1) V)))))
                (MULTSQ Z
                        (EXPTSQ '(-1 . 1)
                                (PLUS 1
                                      (DIFFERENCE (LENGTH (CDR A))
                                                  (LENGTH (CAR B))))))))))) 
(PUT 'Q2P1M1N0 'NUMBER-OF-ARGS 3) 
(PUT 'Q2P1M1N0 'DEFINED-ON-LINE '281) 
(PUT 'Q2P1M1N0 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q2P1M1N0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q2P1M1N0 (A B Z)
    (PROG (V)
      (SETQ V (ADDEND A B '(1 . 2)))
      (COND
       ((NULL (CAR (ADDSQ (CADR V) (CADDR V))))
        (RETURN (F7 (CAR V) (CADR V) Z)))
       (T (RETURN (SIMPGTOH (CONS NIL A) (REDPAR1 B 1) Z)))))) 
(PUT 'F7 'NUMBER-OF-ARGS 3) 
(PUT 'F7 'DEFINED-ON-LINE '289) 
(PUT 'F7 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F7 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F7 (A B Z)
    (MULTSQ
     (MULTSQ (SIMPFUNC 'COS (MULTSQ B (SIMP* 'PI))) (INVSQ (SIMPX1 'PI 1 2)))
     (MULTSQ (EXPDEG Z A)
             (MULTSQ (EXPDEG (SIMP* 'E) (MULTSQ Z '(1 . 2)))
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELI (PREPSQ B)
                                 (PREPSQ (MULTSQ Z '(1 . 2))))))
                         1)
                        1))
                      1))))) 
(PUT 'Q2P1M2N0 'NUMBER-OF-ARGS 3) 
(PUT 'Q2P1M2N0 'DEFINED-ON-LINE '294) 
(PUT 'Q2P1M2N0 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q2P1M2N0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q2P1M2N0 (A B Z)
    (PROG (V)
      (SETQ V (ADDEND A B '(1 . 2)))
      (COND
       ((NULL (CAR (ADDSQ (CADR V) (CADDR V))))
        (RETURN (F9 (CAR V) (CADR V) Z)))
       (T (RETURN (F11 (CAR A) (CAR B) (CADR B) Z)))))) 
(PUT 'F9 'NUMBER-OF-ARGS 3) 
(PUT 'F9 'DEFINED-ON-LINE '302) 
(PUT 'F9 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F9 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F9 (A B Z)
    (MULTSQ (MULTSQ (EXPDEG Z A) (INVSQ (SIMPX1 'PI 1 2)))
            (MULTSQ (EXPDEG (SIMP* 'E) (MULTSQ '(1 . 2) (NEGSQ Z)))
                    (CONS
                     (LIST
                      (CONS
                       (CONS
                        (CAR
                         (FKERN
                          (LIST 'BESSELK (PREPSQ B)
                                (PREPSQ (MULTSQ Z '(1 . 2))))))
                        1)
                       1))
                     1)))) 
(PUT 'F11 'NUMBER-OF-ARGS 4) 
(PUT 'F11 'DEFINED-ON-LINE '307) 
(PUT 'F11 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F11 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE F11 (A B C Z)
    (MULTSQ (EXPDEG Z B)
            (MULTSQ (EXPDEG (SIMP* 'E) (NEGSQ Z))
                    (CONS
                     (LIST
                      (CONS
                       (CONS
                        (CAR
                         (FKERN
                          (LIST 'KUMMERU (PREPSQ (ADDSQ A (NEGSQ C)))
                                (PREPSQ (ADDSQ '(1 . 1) (ADDSQ B (NEGSQ C))))
                                (PREPSQ Z))))
                        1)
                       1))
                     1)))) 
(PUT 'Q2P1M2N1 'NUMBER-OF-ARGS 3) 
(PUT 'Q2P1M2N1 'DEFINED-ON-LINE '311) 
(PUT 'Q2P1M2N1 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q2P1M2N1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q2P1M2N1 (A B Z)
    (PROG (V)
      (SETQ V (ADDEND A B '(1 . 2)))
      (COND
       ((AND (NULL (CAR (ADDSQ (CADR V) (CADDR V))))
             (OR (AND (EQUAL (CDADR V) 2) (NOT (NUMBERP (CADAR V))))
                 (NOT (EQUAL (CDADR V) 2))))
        (RETURN (F10 (CAR V) (CADR V) Z)))
       (T (RETURN (SIMPGTOH (CONS A NIL) (CONS B NIL) Z)))))) 
(PUT 'F10 'NUMBER-OF-ARGS 3) 
(PUT 'F10 'DEFINED-ON-LINE '321) 
(PUT 'F10 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F10 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F10 (A B Z)
    (MULTSQ
     (MULTSQ (SIMPX1 'PI 1 2) (INVSQ (SIMPFUNC 'COS (MULTSQ (SIMP* 'PI) B))))
     (MULTSQ (EXPDEG Z A)
             (MULTSQ (EXPDEG (SIMP* 'E) (MULTSQ '(1 . 2) Z))
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELK (PREPSQ B)
                                 (PREPSQ (MULTSQ '(1 . 2) Z)))))
                         1)
                        1))
                      1))))) 
(PUT 'Q3P1M2N1 'NUMBER-OF-ARGS 3) 
(PUT 'Q3P1M2N1 'DEFINED-ON-LINE '330) 
(PUT 'Q3P1M2N1 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q3P1M2N1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q3P1M2N1 (A B Z)
    (PROG (V V1)
      (COND
       ((EQUAL (ADDSQ (CAR A) (NEGSQ (CADDR B))) '(1 . 2))
        (COND
         ((AND (EQUAL (CAR A) (CAR B))
               (OR
                (AND (EQUAL (CDR (ADDSQ (CADR B) (NEGSQ (CADDR B)))) 2)
                     (NOT (NUMBERP (CAR (ADDSQ (CADR B) (NEGSQ (CADDR B)))))))
                (NOT (EQUAL (CDR (ADDSQ (CADR B) (NEGSQ (CADDR B)))) 2))))
          (RETURN (F34 (CADDR B) (CADR B) Z)))
         ((AND (EQUAL (CAR A) (CADR B))
               (OR
                (AND (EQUAL (CDR (ADDSQ (CAR B) (NEGSQ (CADDR B)))) 2)
                     (NOT (NUMBERP (CAR (ADDSQ (CAR B) (NEGSQ (CADDR B)))))))
                (NOT (EQUAL (CDR (ADDSQ (CAR B) (NEGSQ (CADDR B)))) 2))))
          (RETURN (F34 (CADDR B) (CAR B) Z)))
         (T (GO M)))))
      (COND
       ((AND (EQUAL (ADDSQ (CAR A) (NEGSQ (CAR B))) '(1 . 2))
             (EQUAL (CAR A) (CADR B)))
        (RETURN (F35 (CAR B) (CADDR B) Z)))
       ((AND (EQUAL (ADDSQ (CAR A) (NEGSQ (CADR B))) '(1 . 2))
             (EQUAL (CAR A) (CAR B)))
        (RETURN (F35 (CADR B) (CADDR B) Z)))
       (T (RETURN (SIMPGTOH (CONS A NIL) (REDPAR1 B 2) Z))))
     M
      (SETQ V (ADDEND A B '(1 . 2)))
      (SETQ V1 (CDR V))
      (COND
       ((AND (NULL (CAAR V1)) (NULL (CAR (ADDSQ (CADR V1) (CADDR V1)))))
        (RETURN (F32 (CAR V) (CADR V1) Z)))
       ((AND (NULL (CAADR V1)) (NULL (CAR (ADDSQ (CAR V1) (CADDR V1)))))
        (RETURN (F32 (CAR V) (CAR V1) Z)))
       ((AND (NULL (CAADDR V1)) (NULL (CAR (ADDSQ (CAR V1) (CADR V1))))
             (OR (AND (NOT (EQUAL (CDAR V1) 1)) (NOT (EQUAL (CDAR V1) 2)))
                 (NOT (NUMBERP (CAAR V1)))))
        (RETURN (F33 (CAR V) (CAR V1) Z))))
      (RETURN (SIMPGTOH (CONS A NIL) (REDPAR1 B 2) Z)))) 
(PUT 'F34 'NUMBER-OF-ARGS 3) 
(PUT 'F34 'DEFINED-ON-LINE '360) 
(PUT 'F34 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F34 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F34 (A B Z)
    (MULTSQ
     (MULTSQ (SIMP* 'PI)
             (INVSQ (SIMPFUNC 'COS (MULTSQ (SIMP* 'PI) (ADDSQ B (NEGSQ A))))))
     (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ A B)))
             (ADDSQ
              (CONS
               (LIST
                (CONS
                 (CONS
                  (CAR
                   (FKERN
                    (LIST 'BESSELI (PREPSQ (ADDSQ B (NEGSQ A)))
                          (PREPSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
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
                     (LIST 'STRUVEL (PREPSQ (ADDSQ A (NEGSQ B)))
                           (PREPSQ
                            (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                   1)
                  1))
                1)))))) 
(PUT 'F35 'NUMBER-OF-ARGS 3) 
(PUT 'F35 'DEFINED-ON-LINE '368) 
(PUT 'F35 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F35 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F35 (A B Z)
    (MULTSQ (SIMP* 'PI)
            (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ A B)))
                    (ADDSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELI (PREPSQ (ADDSQ A (NEGSQ B)))
                                 (PREPSQ
                                  (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
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
                            (LIST 'STRUVEL (PREPSQ (ADDSQ A (NEGSQ B)))
                                  (PREPSQ
                                   (MULTSQ '(2 . 1)
                                           (SIMPX1 (PREPSQ Z) 1 2))))))
                          1)
                         1))
                       1)))))) 
(PUT 'F33 'NUMBER-OF-ARGS 3) 
(PUT 'F33 'DEFINED-ON-LINE '375) 
(PUT 'F33 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F33 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F33 (C A Z)
    (MULTSQ
     (MULTSQ (SIMPX1 'PI 3 2)
             (INVSQ (SIMPFUNC 'SIN (MULTSQ '(2 . 1) (MULTSQ A (SIMP* 'PI))))))
     (MULTSQ (EXPDEG Z C)
             (ADDSQ
              (MULTSQ
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'BESSELI (PREPSQ (NEGSQ A))
                           (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                   1)
                  1))
                1)
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'BESSELI (PREPSQ (NEGSQ A))
                           (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                   1)
                  1))
                1))
              (NEGSQ
               (MULTSQ
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'BESSELI (PREPSQ A)
                            (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                    1)
                   1))
                 1)
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'BESSELI (PREPSQ A)
                            (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                    1)
                   1))
                 1))))))) 
(PUT 'F32 'NUMBER-OF-ARGS 3) 
(PUT 'F32 'DEFINED-ON-LINE '383) 
(PUT 'F32 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F32 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F32 (C A Z)
    (MULTSQ (MULTSQ '(2 . 1) (SIMPX1 'PI 1 2))
            (MULTSQ (EXPDEG Z C)
                    (MULTSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELI (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1)
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELK (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1))))) 
(PUT 'Q3P1M2N0 'NUMBER-OF-ARGS 3) 
(PUT 'Q3P1M2N0 'DEFINED-ON-LINE '387) 
(PUT 'Q3P1M2N0 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q3P1M2N0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q3P1M2N0 (A B Z)
    (PROG (V V1)
      (COND
       ((EQUAL (CAR A) (CADDR B))
        (COND
         ((EQUAL (ADDSQ (CAR B) (NEGSQ (CAR A))) '(1 . 2))
          (RETURN (F29 (CAR B) (CADR B) Z)))
         ((EQUAL (ADDSQ (CADR B) (NEGSQ (CAR A))) '(1 . 2))
          (RETURN (F29 (CADR B) (CAR B) Z))))))
      (SETQ V (ADDEND A B '(1 . 2)))
      (SETQ V1 (CDR V))
      (COND
       ((AND (NULL (CAAR V1)) (NULL (CAR (ADDSQ (CADR V1) (CADDR V1)))))
        (RETURN (F31 (CAR V) (CADR V1) Z)))
       ((AND (NULL (CAADR V1)) (NULL (CAR (ADDSQ (CAR V1) (CADDR V1)))))
        (RETURN (F31 (CAR V) (CAR V1) Z)))
       ((AND (NULL (CAADDR V1)) (NULL (CAR (ADDSQ (CADR V1) (CAR V1))))
             (OR (AND (EQUAL (CDAR V1) 1) (NOT (NUMBERP (CAAR V1))))
                 (NOT (EQUAL (CDAR V1) 1))))
        (RETURN (F30 (CAR V) (CAR V1) Z))))
      (RETURN (SIMPGTOH (CONS NIL A) (REDPAR1 B 2) Z)))) 
(PUT 'F29 'NUMBER-OF-ARGS 3) 
(PUT 'F29 'DEFINED-ON-LINE '406) 
(PUT 'F29 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F29 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F29 (A B Z)
    (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ A B)))
            (CONS
             (LIST
              (CONS
               (CONS
                (CAR
                 (FKERN
                  (LIST 'BESSELY (PREPSQ (ADDSQ B (NEGSQ A)))
                        (PREPSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                1)
               1))
             1))) 
(PUT 'F30 'NUMBER-OF-ARGS 3) 
(PUT 'F30 'DEFINED-ON-LINE '410) 
(PUT 'F30 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F30 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F30 (C A Z)
    (MULTSQ
     (MULTSQ (SIMPX1 'PI 1 2)
             (INVSQ (MULTSQ '(2 . 1) (SIMPFUNC 'SIN (MULTSQ A (SIMP* 'PI))))))
     (MULTSQ (EXPDEG Z C)
             (ADDSQ
              (MULTSQ
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'BESSELJ (PREPSQ (NEGSQ A))
                           (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                   1)
                  1))
                1)
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'BESSELJ (PREPSQ (NEGSQ A))
                           (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                   1)
                  1))
                1))
              (NEGSQ
               (MULTSQ
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'BESSELJ (PREPSQ A)
                            (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                    1)
                   1))
                 1)
                (CONS
                 (LIST
                  (CONS
                   (CONS
                    (CAR
                     (FKERN
                      (LIST 'BESSELJ (PREPSQ A)
                            (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                    1)
                   1))
                 1))))))) 
(PUT 'F31 'NUMBER-OF-ARGS 3) 
(PUT 'F31 'DEFINED-ON-LINE '416) 
(PUT 'F31 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F31 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F31 (C A Z)
    (MULTSQ (NEGSQ (SIMPX1 'PI 1 2))
            (MULTSQ (EXPDEG Z C)
                    (MULTSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELJ (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1)
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELY (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1))))) 
(PUT 'Q3P1M1N1 'NUMBER-OF-ARGS 3) 
(PUT 'Q3P1M1N1 'DEFINED-ON-LINE '420) 
(PUT 'Q3P1M1N1 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q3P1M1N1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q3P1M1N1 (A B Z)
    (PROG (V V1)
      (COND
       ((EQUAL (CAR A) (CAR B))
        (COND
         ((EQUAL (ADDSQ (CAR A) (NEGSQ (CADDR B))) '(1 . 2))
          (RETURN (F28 (CAR A) (CADR B) Z)))
         ((EQUAL (ADDSQ (CAR A) (NEGSQ (CADR B))) '(1 . 2))
          (RETURN (F28 (CAR A) (CADDR B) Z))))))
      (SETQ V (ADDEND A B '(1 . 2)))
      (SETQ V1 (CDR V))
      (COND
       ((AND (NULL (CAAR V1)) (NULL (CAR (ADDSQ (CADR V1) (CADDR V1)))))
        (RETURN (F26 (CAR V) (CADR V1) Z)))
       ((AND (OR (NULL (CAADR V1)) (NULL (CAADDR V1)))
             (OR (NULL (CAR (ADDSQ (CAR V1) (CADR V1))))
                 (NULL (CAR (ADDSQ (CAR V1) (CADDR V1))))))
        (RETURN (F27 (CAR V) (CAR V1) Z))))
      (RETURN (SIMPGTOH (CONS A NIL) (REDPAR1 B 1) Z)))) 
(PUT 'F26 'NUMBER-OF-ARGS 3) 
(PUT 'F26 'DEFINED-ON-LINE '436) 
(PUT 'F26 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F26 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F26 (C A Z)
    (MULTSQ (SIMPX1 'PI 1 2)
            (MULTSQ (EXPDEG Z C)
                    (MULTSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELJ (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1)
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELJ (PREPSQ (NEGSQ A))
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1))))) 
(PUT 'F27 'NUMBER-OF-ARGS 3) 
(PUT 'F27 'DEFINED-ON-LINE '441) 
(PUT 'F27 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F27 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F27 (C A Z)
    (MULTSQ (SIMPX1 'PI 1 2)
            (MULTSQ (EXPDEG Z C)
                    (MULTSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELJ (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1)
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELJ (PREPSQ A)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1))))) 
(PUT 'F28 'NUMBER-OF-ARGS 3) 
(PUT 'F28 'DEFINED-ON-LINE '445) 
(PUT 'F28 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F28 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F28 (A B Z)
    (MULTSQ (EXPDEG Z (MULTSQ '(1 . 2) (ADDSQ (ADDSQ A B) (NEGSQ '(1 . 2)))))
            (CONS
             (LIST
              (CONS
               (CONS
                (CAR
                 (FKERN
                  (LIST 'STRUVEH (PREPSQ (ADDSQ A (NEGSQ (ADDSQ B '(1 . 2)))))
                        (PREPSQ (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                1)
               1))
             1))) 
(PUT 'Q3P1M3N0 'NUMBER-OF-ARGS 3) 
(PUT 'Q3P1M3N0 'DEFINED-ON-LINE '450) 
(PUT 'Q3P1M3N0 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q3P1M3N0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q3P1M3N0 (A B Z)
    (PROG (V V1)
      (SETQ V (ADDEND A B '(1 . 2)))
      (SETQ V1 (CDR V))
      (COND
       ((OR (AND (NULL (CAR (ADDSQ (CAR V1) (CADR V1)))) (NULL (CAADDR V1)))
            (AND (NULL (CAR (ADDSQ (CAR V1) (CADDR V1)))) (NULL (CAADR V1))))
        (RETURN (F36 (CAR V) (CAR V1) Z)))
       ((AND (NULL (CAR (ADDSQ (CADR V1) (CADDR V1)))) (NULL (CAAR V1)))
        (RETURN (F36 (CAR V) (CADR V1) Z))))
      (RETURN (SIMPGTOH (CONS NIL A) (CONS B NIL) Z)))) 
(PUT 'F36 'NUMBER-OF-ARGS 3) 
(PUT 'F36 'DEFINED-ON-LINE '461) 
(PUT 'F36 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F36 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F36 (A B Z)
    (MULTSQ (MULTSQ '(2 . 1) (INVSQ (SIMPX1 'PI 1 2)))
            (MULTSQ (EXPDEG Z A)
                    (MULTSQ
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELK (PREPSQ B)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1)
                     (CONS
                      (LIST
                       (CONS
                        (CONS
                         (CAR
                          (FKERN
                           (LIST 'BESSELK (PREPSQ B)
                                 (PREPSQ (SIMPX1 (PREPSQ Z) 1 2)))))
                         1)
                        1))
                      1))))) 
(PUT 'Q3P1M3N1 'NUMBER-OF-ARGS 3) 
(PUT 'Q3P1M3N1 'DEFINED-ON-LINE '465) 
(PUT 'Q3P1M3N1 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'Q3P1M3N1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Q3P1M3N1 (A B Z)
    (COND
     ((AND (EQUAL (CAR A) (CAR B)) (NULL (CAR (ADDSQ (CADR B) (CADDR B)))))
      (F38 (CAR A) (CADR B) Z))
     ((OR (AND (EQUAL (CAR A) (CADR B)) (NULL (CAR (ADDSQ (CAR B) (CADDR B)))))
          (AND (EQUAL (CAR A) (CADDR B))
               (NULL (CAR (ADDSQ (CAR B) (CADR B))))))
      (F38 (CAR A) (CAR B) Z))
     ((AND (EQUAL (ADDSQ (CAR A) (NEGSQ (CADDR B))) '(1 . 2))
           (NULL
            (CAR
             (ADDSQ (ADDSQ (CAR B) (CADR B))
                    (CONS
                     ((LAMBDA (G143 G144)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G143 G144))
                              (T (POLY-MULTF G143 G144))))
                      (MINUS 2) (CAADDR B))
                     (CDADDR B))))))
      (F39 (CADDR B) (CAR B) Z))
     ((AND (EQUAL (ADDSQ (CAR A) (NEGSQ (CADR B))) '(1 . 2))
           (NULL
            (CAR
             (ADDSQ (ADDSQ (CAR B) (CADDR B))
                    (CONS
                     ((LAMBDA (G145 G146)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G145 G146))
                              (T (POLY-MULTF G145 G146))))
                      (MINUS 2) (CAADR B))
                     (CDADR B))))))
      (F39 (CADR B) (CAR B) Z))
     ((AND (EQUAL (ADDSQ (CAR A) (NEGSQ (CAR B))) '(1 . 2))
           (NULL
            (CAR
             (ADDSQ (ADDSQ (CADR B) (CADDR B))
                    (CONS
                     ((LAMBDA (G147)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G147 (CAAR B)))
                              (T (POLY-MULTF G147 (CAAR B)))))
                      (MINUS 2))
                     (CDAR B))))))
      (F39 (CAR B) (CADR B) Z))
     (T (SIMPGTOH (CONS A NIL) (CONS B NIL) Z)))) 
(PUT 'F38 'NUMBER-OF-ARGS 3) 
(PUT 'F38 'DEFINED-ON-LINE '483) 
(PUT 'F38 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F38 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F38 (A B Z)
    (COND
     ((OR (PARFOOL (ADDSQ '(1 . 1) (NEGSQ (ADDSQ A B))))
          (PARFOOL (ADDSQ '(1 . 1) (ADDSQ B (NEGSQ A)))))
      (SIMPGTOH (CONS (LIST A) NIL) (CONS (LIST A B (NEGSQ B)) NIL) Z))
     (T
      (MULTSQ (EXPDEG '(4 . 1) (ADDSQ '(1 . 1) (NEGSQ A)))
              (MULTSQ
               ((LAMBDA (P)
                  (PROGN
                   (PROG (PP)
                     (SETQ PP
                             (LIST (ADDSQ '(1 . 1) (NEGSQ (ADDSQ A B)))
                                   (ADDSQ B (ADDSQ '(1 . 1) (NEGSQ A)))))
                    LAB
                     (COND ((NULL PP) (RETURN NIL)))
                     ((LAMBDA (PP)
                        (PROGN
                         (SETQ P
                                 (MULTSQ
                                  (CONS
                                   (LIST
                                    (CONS
                                     (CONS
                                      (CAR (FKERN (LIST 'GAMMA (PREPSQ PP))))
                                      1)
                                     1))
                                   1)
                                  P))))
                      (CAR PP))
                     (SETQ PP (CDR PP))
                     (GO LAB))
                   P))
                '(1 . 1))
               (CONS
                (LIST
                 (CONS
                  (CONS
                   (CAR
                    (FKERN
                     (LIST 'LOMMEL2
                           (PREPSQ
                            (ADDSQ (MULTSQ '(2 . 1) A) (NEGSQ '(1 . 1))))
                           (PREPSQ (MULTSQ '(2 . 1) B))
                           (PREPSQ
                            (MULTSQ '(2 . 1) (SIMPX1 (PREPSQ Z) 1 2))))))
                   1)
                  1))
                1)))))) 
(PUT 'F39 'NUMBER-OF-ARGS 3) 
(PUT 'F39 'DEFINED-ON-LINE '494) 
(PUT 'F39 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'F39 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE F39 (A B Z)
    (COND
     ((OR (NOT (NUMBERP (CAR (ADDSQ A (NEGSQ B)))))
          (NOT (EQUAL (CDR (ADDSQ A (NEGSQ B))) 2)))
      (MULTSQ
       (MULTSQ (MULTSQ (SIMPX1 'PI 5 2) (EXPDEG Z A))
               (INVSQ
                (MULTSQ '(2 . 1)
                        (SIMPFUNC 'COS
                                  (MULTSQ (SIMP* 'PI) (ADDSQ B (NEGSQ A)))))))
       (MULTSQ (HANKEL1SQ (ADDSQ B (NEGSQ A)) (SIMPX1 (PREPSQ Z) 1 2))
               (HANKEL2SQ (ADDSQ B (NEGSQ A)) (SIMPX1 (PREPSQ Z) 1 2)))))
     (T
      (SIMPGTOH (CONS (LIST (ADDSQ A '(1 . 2))) NIL)
       (CONS (LIST B A (ADDSQ (MULTSQ '(2 . 1) A) (NEGSQ B))) NIL) Z)))) 
(FLUID '(*INFINITYMULTPOLE)) 
(DE PRIZNAK (U V)
    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
      (SETQ UU U)
      (COND ((NULL UU) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (UU) (CONS UU V)) (CAR UU)) NIL)))
     LOOPLABEL
      (SETQ UU (CDR UU))
      (COND ((NULL UU) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (UU) (CONS UU V)) (CAR UU)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PRIZNAK 'NUMBER-OF-ARGS 2) 
(PUT 'PRIZNAK 'DEFINED-ON-LINE '510) 
(PUT 'PRIZNAK 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'PRIZNAK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'PRIZNAK 'INLINE
      '(LAMBDA (U V)
         (PROG (UU FORALL-RESULT FORALL-ENDPTR)
           (SETQ UU U)
           (COND ((NULL UU) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (UU) (CONS UU V)) (CAR UU)) NIL)))
          LOOPLABEL
           (SETQ UU (CDR UU))
           (COND ((NULL UU) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (UU) (CONS UU V)) (CAR UU)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))) 
(PUT 'GFMLOGCASE 'NUMBER-OF-ARGS 3) 
(PUT 'GFMLOGCASE 'DEFINED-ON-LINE '513) 
(PUT 'GFMLOGCASE 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMLOGCASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMLOGCASE (A B Z)
    (PROG (W)
      (SETQ W
              (ALLPOLES
               (LOGCASE
                (APPEND
                 (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                   (SETQ UU (CDR A))
                   (COND ((NULL UU) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (UU) (CONS UU 'N)) (CAR UU))
                                         NIL)))
                  LOOPLABEL
                   (SETQ UU (CDR UU))
                   (COND ((NULL UU) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (UU) (CONS UU 'N)) (CAR UU)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                   (SETQ UU (CAR B))
                   (COND ((NULL UU) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (UU) (CONS UU 'P)) (CAR UU))
                                         NIL)))
                  LOOPLABEL
                   (SETQ UU (CDR UU))
                   (COND ((NULL UU) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (UU) (CONS UU 'P)) (CAR UU)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (SETQ W (SORTPOLES W))
      (COND ((NULL *INFINITYMULTPOLE) (RETURN (GFMLOGCASEMULT W A B Z)))
            (T (PROGN (SETQ *INFINITYMULTPOLE NIL) (RETURN 'FAIL)))))) 
(ARRAYFN 'SYMBOLIC (LIST (LIST 'RES 5))) 
(PUT 'ALLPOLES 'NUMBER-OF-ARGS 1) 
(PUT 'ALLPOLES 'DEFINED-ON-LINE '526) 
(PUT 'ALLPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'ALLPOLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLPOLES (UU)
    (PROG (U FORALL-RESULT FORALL-ENDPTR)
      (SETQ U UU)
     STARTOVER
      (COND ((NULL U) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (U)
                 (PROG (W KR)
                   (SETQ KR 0)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT U) (RETURN NIL)))
                     (PROGN
                      (COND ((EQUAL (CDAR U) 'N) (SETQ KR (DIFFERENCE KR 1)))
                            (T (SETQ KR (PLUS KR 1))))
                      (COND
                       ((GREATERP KR 0)
                        (COND
                         ((NOT (NULL (CDR U)))
                          (COND
                           ((NOT (EQUAL (CAADR U) (CAAR U)))
                            (SETQ W
                                    (CONS
                                     (LIST KR
                                           (PREPSQ
                                            (ADDSQ (CAADR U) (NEGSQ (CAAR U))))
                                           (NEGSQ (CAAR U)))
                                     W)))
                           (T (SETQ W W))))
                         (T
                          (PROGN
                           (SETQ W
                                   (CONS (LIST KR 'INFINITY (NEGSQ (CAAR U)))
                                         W))
                           (COND
                            ((NOT (EQN KR 1)) (SETQ *INFINITYMULTPOLE T))))))))
                      (SETQ U (CDR U))
                      NIL)
                     (GO WHILELABEL))
                   (RETURN W)))
               (CAR U)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ U (CDR U))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL U) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (U)
                 (PROG (W KR)
                   (SETQ KR 0)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT U) (RETURN NIL)))
                     (PROGN
                      (COND ((EQUAL (CDAR U) 'N) (SETQ KR (DIFFERENCE KR 1)))
                            (T (SETQ KR (PLUS KR 1))))
                      (COND
                       ((GREATERP KR 0)
                        (COND
                         ((NOT (NULL (CDR U)))
                          (COND
                           ((NOT (EQUAL (CAADR U) (CAAR U)))
                            (SETQ W
                                    (CONS
                                     (LIST KR
                                           (PREPSQ
                                            (ADDSQ (CAADR U) (NEGSQ (CAAR U))))
                                           (NEGSQ (CAAR U)))
                                     W)))
                           (T (SETQ W W))))
                         (T
                          (PROGN
                           (SETQ W
                                   (CONS (LIST KR 'INFINITY (NEGSQ (CAAR U)))
                                         W))
                           (COND
                            ((NOT (EQN KR 1)) (SETQ *INFINITYMULTPOLE T))))))))
                      (SETQ U (CDR U))
                      NIL)
                     (GO WHILELABEL))
                   (RETURN W)))
               (CAR U)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ U (CDR U))
      (GO LOOPLABEL))) 
(PUT 'LOGCASE 'NUMBER-OF-ARGS 1) 
(PUT 'LOGCASE 'DEFINED-ON-LINE '547) 
(PUT 'LOGCASE 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'LOGCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOGCASE (U)
    (PROG (BLOG BLOGNEW SB)
      (SETQ SB U)
      (SETQ U (CDR SB))
     M1
      (COND ((NULL SB) (RETURN BLOGNEW)))
     M2
      (COND
       ((NULL U)
        (PROGN
         (COND
          ((NOT (NULL BLOG))
           (PROGN (SETQ BLOGNEW (CONS BLOG BLOGNEW)) (SETQ BLOG NIL)))
          (T (SETQ BLOGNEW (CONS (LIST (CAR SB)) BLOGNEW))))
         (SETQ SB (CDR SB))
         (COND (SB (SETQ U (CDR SB))))
         (GO M1)))
       ((OR (EQUAL (CAAR SB) (CAAR U))
            (AND (NUMBERP (CAR (ADDSQ (CAAR SB) (NEGSQ (CAAR U)))))
                 (EQUAL (CDR (ADDSQ (CAAR SB) (NEGSQ (CAAR U)))) 1)))
        (PROGN
         (COND
          ((NULL BLOG)
           (COND
            ((OR (EQUAL (CAAR SB) (CAAR U))
                 (LESSP (CAR (ADDSQ (CAAR SB) (NEGSQ (CAAR U)))) 0))
             (SETQ BLOG (LIST (CAR SB) (CAR U))))
            (T (SETQ BLOG (LIST (CAR U) (CAR SB))))))
          (T (SETQ BLOG (ORDERN (CAR U) BLOG))))
         (SETQ SB (DELETE (CAR U) SB))
         (COND (U (SETQ U (CDR U))))
         (GO M2)))
       (T (PROGN (COND (U (SETQ U (CDR U)))) (GO M2) NIL))))) 
(PUT 'ORDERN 'NUMBER-OF-ARGS 2) 
(PUT 'ORDERN 'DEFINED-ON-LINE '584) 
(PUT 'ORDERN 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'ORDERN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDERN (U V)
    (COND ((NULL V) (LIST U))
          ((OR (EQUAL (CAR U) (CAAR V))
               (GREATERP (CAR (ADDSQ (CAR U) (NEGSQ (CAAR V)))) 0))
           (CONS (CAR V) (ORDERN U (CDR V))))
          (T (CONS U V)))) 
(PUT 'SORTPOLES 'NUMBER-OF-ARGS 1) 
(PUT 'SORTPOLES 'DEFINED-ON-LINE '594) 
(PUT 'SORTPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SORTPOLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORTPOLES (W)
    (PROG (W1 W2)
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (PROG ()
          (COND ((EQUAL (CADAR W) 'INFINITY) (SETQ W1 (CONS (CAR W) W1)))
                (T (SETQ W2 (CONS (CAR W) W2))))
          (SETQ W (CDR W)))
        (GO WHILELABEL))
      (RETURN (APPEND W2 W1)))) 
(PUT 'GFMLOGCASEMULT 'NUMBER-OF-ARGS 4) 
(PUT 'GFMLOGCASEMULT 'DEFINED-ON-LINE '604) 
(PUT 'GFMLOGCASEMULT 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GFMLOGCASEMULT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFMLOGCASEMULT (W A B Z)
    (COND ((NULL W) (CONS NIL 1))
          (T
           (ADDSQ (GROUPRESUDES (CAR W) A B Z)
                  (GFMLOGCASEMULT (CDR W) A B Z))))) 
(PUT 'GROUPRESUDES 'NUMBER-OF-ARGS 4) 
(PUT 'GROUPRESUDES 'DEFINED-ON-LINE '610) 
(PUT 'GROUPRESUDES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GROUPRESUDES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROUPRESUDES (W A B Z)
    (COND ((NOT (EQUAL (CADR W) 'INFINITY)) (MULTPOLES W A B Z))
          ((AND (EQUAL (CADR W) 'INFINITY) (EQUAL (CAR W) 1))
           (SIMPLEPOLES (CADDR W) A B Z))
          (T 'FAIL))) 
(PUT 'SIMPLEPOLES 'NUMBER-OF-ARGS 4) 
(PUT 'SIMPLEPOLES 'DEFINED-ON-LINE '619) 
(PUT 'SIMPLEPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SIMPLEPOLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLEPOLES (AT A B Z)
    (COND
     ((MEMBER AT (CAR B))
      (INFINITYSIMPLPOLES A (CONS (CAR (REDPAR (CAR B) (LIST AT))) (CDR B))
       (NEGSQ AT) Z))
     (T (SPECIALTRANSF AT A B Z)))) 
(PUT 'SPECIALTRANSF 'NUMBER-OF-ARGS 4) 
(PUT 'SPECIALTRANSF 'DEFINED-ON-LINE '625) 
(PUT 'SPECIALTRANSF 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SPECIALTRANSF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPECIALTRANSF (AT A B Z)
    (COND
     ((LISTFOOLTWO (CAR B) (CDR A))
      (PROG (C CC)
        (SETQ C (REDPAR (CAR B) (CDR A)))
        (SETQ CC (REDPAR (CDR B) (CAR A)))
        (SETQ A (CONS (CADR CC) (CADR C)))
        (SETQ B (CONS (CAR C) (CAR CC)))
        (COND
         ((LISTFOOLTWO (CAR B) (CDR A))
          (PROGN
           (SETQ C (FINDTWOPARAMS (CAR B) (CDR A)))
           (SETQ A
                   (CONS (CONS (CAR C) (CAR A))
                         (CAR (REDPAR (CDR A) (LIST (CAR C))))))
           (SETQ B
                   (CONS (CAR (REDPAR (CAR B) (LIST (CADR C))))
                         (CONS (CADR C) (CDR B))))
           (RETURN
            (MULTSQ (EXPDEG '(-1 . 1) (ADDSQ (CAR C) (NEGSQ (CADR C))))
                    (SPECIALTRANSF AT A B Z)))))
         (T (RETURN (INFINITYSIMPLPOLES A B (NEGSQ AT) Z))))))
     (T
      (PROG (C)
        (SETQ C (REDPAR (CDR B) (CAR A)))
        (SETQ A (CONS (CADR C) (CDR A)))
        (SETQ B (CONS (CAR B) (CAR C)))
        (RETURN (INFINITYSIMPLPOLES A B (NEGSQ AT) Z)))))) 
(PUT 'FINDTWOPARAMS 'NUMBER-OF-ARGS 2) 
(PUT 'FINDTWOPARAMS 'DEFINED-ON-LINE '653) 
(PUT 'FINDTWOPARAMS 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'FINDTWOPARAMS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDTWOPARAMS (U V)
    (PROG (C)
      (PROG (UU)
        (SETQ UU U)
       LAB
        (COND ((NULL UU) (RETURN NIL)))
        ((LAMBDA (UU)
           (PROG (VV)
             (SETQ VV V)
            LAB
             (COND ((NULL VV) (RETURN NIL)))
             ((LAMBDA (VV)
                (COND
                 ((PARFOOL (ADDSQ UU (NEGSQ VV)))
                  (PROGN (SETQ C (LIST VV UU)) (SETQ U NIL) (SETQ V NIL)))))
              (CAR VV))
             (SETQ VV (CDR VV))
             (GO LAB)))
         (CAR UU))
        (SETQ UU (CDR UU))
        (GO LAB))
      (RETURN C))) 
(PUT 'MULTPOLES 'NUMBER-OF-ARGS 4) 
(PUT 'MULTPOLES 'DEFINED-ON-LINE '663) 
(PUT 'MULTPOLES 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'MULTPOLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTPOLES (U A B Z)
    (COND ((EQUAL (CADR U) 0) (CONS NIL 1))
          (T
           (ADDSQ (MULTRESUDE (LIST (CAR U) (CADDR U)) A B Z)
                  (MULTPOLES
                   (LIST (CAR U) (DIFFERENCE (CADR U) 1)
                         (ADDSQ (CADDR U) (NEGSQ '(1 . 1))))
                   A B Z))))) 
(PUT 'MULTRESUDE 'NUMBER-OF-ARGS 4) 
(PUT 'MULTRESUDE 'DEFINED-ON-LINE '670) 
(PUT 'MULTRESUDE 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'MULTRESUDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTRESUDE (U A B Z)
    (PROGN
     (PROG (I)
       (SETQ I 0)
      LAB
       (COND ((MINUSP (DIFFERENCE 5 I)) (RETURN NIL)))
       (SETEL (LIST 'RES (*S2I I)) '(NIL . 1))
       (SETQ I (PLUS2 I 1))
       (GO LAB))
     (FINDRESUDE
      (MULTLISTASYM
       (LIST
        (LISTTAYLORNOM (SPECFN-LISTPLUS (CAR B) (CADR U)) (SIMP 'EPS) (CAR U))
        (LISTTAYLORNOM
         ((LAMBDA (G150)
            (PROG (VV FORALL-RESULT FORALL-ENDPTR)
              (SETQ VV (CAR A))
              (COND ((NULL VV) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (VV) (ADDSQ G150 (NEGSQ VV))) (CAR VV))
                               NIL)))
             LOOPLABEL
              (SETQ VV (CDR VV))
              (COND ((NULL VV) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (VV) (ADDSQ G150 (NEGSQ VV))) (CAR VV))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
          (ADDSQ '(1 . 1) (NEGSQ (CADR U))))
         (NEGSQ (SIMP 'EPS)) (CAR U))
        (LISTTAYLORDEN (SPECFN-LISTPLUS (CDR A) (CADR U)) (SIMP 'EPS) (CAR U))
        (LISTTAYLORDEN
         ((LAMBDA (G152)
            (PROG (VV FORALL-RESULT FORALL-ENDPTR)
              (SETQ VV (CDR B))
              (COND ((NULL VV) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (VV) (ADDSQ G152 (NEGSQ VV))) (CAR VV))
                               NIL)))
             LOOPLABEL
              (SETQ VV (CDR VV))
              (COND ((NULL VV) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (VV) (ADDSQ G152 (NEGSQ VV))) (CAR VV))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
          (ADDSQ '(1 . 1) (NEGSQ (CADR U))))
         (NEGSQ (SIMP 'EPS)) (CAR U))
        (COND ((EQUAL Z '(1 . 1)) '(1 . 1))
              (T
               (MULTSQ (EXPDEG Z (NEGSQ (CADR U)))
                       (SERIESFORDEGREE (CAR U) (SIMP 'EPS) Z)))))
       (CAR U))))) 
(PUT 'FINDRESUDE 'NUMBER-OF-ARGS 1) 
(PUT 'FINDRESUDE 'DEFINED-ON-LINE '688) 
(PUT 'FINDRESUDE 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'FINDRESUDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDRESUDE (U)
    (PROG (S CC)
      (SETQ CC (PREPSQ (CONS (CDR U) 1)))
      (SETQ CC (CDR (AEVAL (LIST 'COEFF CC 'EPS))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (CAR CC) 0)) (RETURN NIL)))
        (SETQ CC (CDR CC))
        (GO WHILELABEL))
      (SETQ S (COND ((NUMBERP (CAR CC)) (SIMP (CAR CC))) (T (CADR (CAR CC)))))
      (SETQ CC (PREPSQ (CONS (CAR U) 1)))
      (SETQ CC (CDR (AEVAL (LIST 'COEFF CC 'EPS))))
      (RETURN
       (MULTSQ (INVSQ S)
               (COND ((NUMBERP (CAR CC)) (SIMP (CAR CC)))
                     (T (CADR (CAR CC)))))))) 
(PUT 'SERIESFORDEGREE 'NUMBER-OF-ARGS 3) 
(PUT 'SERIESFORDEGREE 'DEFINED-ON-LINE '701) 
(PUT 'SERIESFORDEGREE 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'SERIESFORDEGREE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SERIESFORDEGREE (N V Z)
    (COND ((EQUAL N 1) '(1 . 1))
          (T
           (ADDSQ
            (MULTSQ
             (MULTSQ (EXPTSQ (NEGSQ V) (DIFFERENCE N 1))
                     (EXPTSQ (SIMPFUNC 'LOG Z) (DIFFERENCE N 1)))
             (INVSQ
              (CONS
               (LIST
                (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (CONS N 1))))) 1)
                      1))
               1)))
            (SERIESFORDEGREE (DIFFERENCE N 1) V Z))))) 
(PUT 'LISTTAYLORNOM 'NUMBER-OF-ARGS 3) 
(PUT 'LISTTAYLORNOM 'DEFINED-ON-LINE '707) 
(PUT 'LISTTAYLORNOM 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'LISTTAYLORNOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LISTTAYLORNOM (U V N)
    (COND ((NULL U) '(1 . 1))
          (T (MULTASYM (TAYLORNOM (CAR U) V N) (LISTTAYLORNOM (CDR U) V N) N)))) 
(PUT 'MULTLISTASYM 'NUMBER-OF-ARGS 2) 
(PUT 'MULTLISTASYM 'DEFINED-ON-LINE '714) 
(PUT 'MULTLISTASYM 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'MULTLISTASYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTLISTASYM (U N)
    (COND ((NULL U) '(1 . 1))
          (T (MULTASYM (CAR U) (MULTLISTASYM (CDR U) N) N)))) 
(PUT 'MULTASYM 'NUMBER-OF-ARGS 3) 
(PUT 'MULTASYM 'DEFINED-ON-LINE '719) 
(PUT 'MULTASYM 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'MULTASYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTASYM (U V N)
    (PROG (K)
      (SETQ K 0)
      (COND ((OR (NULL (CAR U)) (NULL (CAR V))) (RETURN '(NIL . 1))))
      (SETQ U (MULTSQ U V))
      (COND ((NOT (OLDPOLSTACK (CONS (CAR U) 1))) (RETURN U)))
      (SETQ V (GETEL (LIST 'RES 0)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP (SETQ K (PLUS K 1)) N)) (RETURN NIL)))
        (SETQ V
                (ADDSQ V
                       (MULTSQ (GETEL (LIST 'RES (*S2I K)))
                               (EXPTSQ (SIMP 'EPS) K))))
        (GO WHILELABEL))
      (RETURN (MULTSQ V (CONS 1 (CDR U)))))) 
(PUT 'OLDPOLSTACK 'NUMBER-OF-ARGS 1) 
(PUT 'OLDPOLSTACK 'DEFINED-ON-LINE '730) 
(PUT 'OLDPOLSTACK 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'OLDPOLSTACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OLDPOLSTACK (U)
    (PROG (CC K)
      (SETQ K 0)
      (SETQ CC (PREPSQ U))
      (SETQ CC (CDR (AEVAL (LIST 'COEFF CC 'EPS))))
      (COND ((NULL CC) (RETURN NIL)) (T (SETQ K 0)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL CC))) (RETURN NIL)))
        (PROGN
         (SETEL (LIST 'RES (*S2I K))
                (COND ((NUMBERP (CAR CC)) (SIMP (CAR CC)))
                      (T (CADR (CAR CC)))))
         (SETQ CC (CDR CC))
         (SETQ K (PLUS K 1))
         NIL)
        (GO WHILELABEL))
      (RETURN T))) 
(PUT 'LISTTAYLORDEN 'NUMBER-OF-ARGS 3) 
(PUT 'LISTTAYLORDEN 'DEFINED-ON-LINE '744) 
(PUT 'LISTTAYLORDEN 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'LISTTAYLORDEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LISTTAYLORDEN (U V N)
    (COND ((NULL U) '(1 . 1))
          (T (MULTASYM (TAYLORDEN (CAR U) V N) (LISTTAYLORDEN (CDR U) V N) N)))) 
(PUT 'TAYLORNOM 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLORNOM 'DEFINED-ON-LINE '751) 
(PUT 'TAYLORNOM 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'TAYLORNOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLORNOM (U V N)
    (COND ((NULL (CAR U)) (MULTSQ (INVSQ V) (TAYLORGAMMA '(1 . 1) V N)))
          ((PARFOOL U)
           (MULTLISTASYM
            (LIST
             (EXPTSQ '(-1 . 1)
                     (COND ((NULL (CAR (NEGSQ U))) 0) (T (CAR (NEGSQ U)))))
             (INVSQ V) (TAYLORNOM '(1 . 1) V N)
             (TAYLORNOM '(1 . 1) (NEGSQ V) N)
             (TAYLORDEN (ADDSQ '(1 . 1) (NEGSQ U)) (NEGSQ V) N))
            N))
          (T
           (MULTSQ
            (CONS
             (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ U)))) 1) 1)) 1)
            (TAYLORGAMMA U V N))))) 
(PUT 'TAYLORDEN 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLORDEN 'DEFINED-ON-LINE '765) 
(PUT 'TAYLORDEN 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'TAYLORDEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLORDEN (U V N)
    (COND
     ((PARFOOL U)
      (MULTLISTASYM
       (LIST
        (EXPTSQ '(-1 . 1)
                (COND ((NULL (CAR (NEGSQ U))) 0) (T (CAR (NEGSQ U)))))
        V (TAYLORNOM (ADDSQ '(1 . 1) (NEGSQ U)) (NEGSQ V) N)
        (TAYLORDEN '(1 . 1) V N) (TAYLORDEN '(1 . 1) (NEGSQ V) N))
       N))
     (T
      (MULTSQ (INVERSEPOL (TAYLORGAMMA U V N) N)
              (INVSQ
               (CONS
                (LIST (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ U)))) 1) 1))
                1)))))) 
(PUT 'INVERSEPOL 'NUMBER-OF-ARGS 2) 
(PUT 'INVERSEPOL 'DEFINED-ON-LINE '778) 
(PUT 'INVERSEPOL 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'INVERSEPOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVERSEPOL (U N)
    (PROG (SSTACK C W K M)
      (SETQ K 0)
      (SETQ M 0)
      (COND ((EQUAL N 1) (RETURN '(1 . 1))))
      (COND ((NULL (OLDPOLSTACK (CONS (CAR U) 1))) (RETURN U)))
      (SETQ SSTACK (LIST '(1 . 1)))
      (SETQ K 2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ K N)) (RETURN NIL)))
        (PROG ()
          (SETQ W SSTACK)
          (SETQ M 2)
          (SETQ C (CONS NIL 1))
          (PROG ()
           WHILELABEL
            (COND ((NOT (LEQ M K)) (RETURN NIL)))
            (PROG ()
              (SETQ C
                      (ADDSQ C
                             (MULTSQ
                              (GETEL (LIST 'RES (*S2I (DIFFERENCE M 1))))
                              (CAR W))))
              (SETQ M (PLUS M 1))
              (SETQ W (CDR W)))
            (GO WHILELABEL))
          (SETQ SSTACK (CONS (NEGSQ C) SSTACK))
          (SETQ K (PLUS K 1)))
        (GO WHILELABEL))
      (SETQ W (CONS NIL 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT SSTACK) (RETURN NIL)))
        (PROG ()
          (SETQ W
                  (ADDSQ W
                         (MULTSQ (CAR SSTACK)
                                 (EXPTSQ (SIMP 'EPS) (DIFFERENCE N 1)))))
          (SETQ SSTACK (CDR SSTACK))
          (SETQ N (DIFFERENCE N 1)))
        (GO WHILELABEL))
      (RETURN
       (MULTSQ (CONS (CDR U) 1) (MULTSQ W (INVSQ (GETEL (LIST 'RES 0)))))))) 
(PUT 'TAYLORGAMMA 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLORGAMMA 'DEFINED-ON-LINE '801) 
(PUT 'TAYLORGAMMA 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'TAYLORGAMMA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLORGAMMA (U V N)
    (COND ((EQUAL N 1) '(1 . 1))
          (T
           (ADDSQ
            (MULTSQ
             (MULTSQ (EXPTSQ V (DIFFERENCE N 1))
                     (GAMMATOPSI U (DIFFERENCE N 1)))
             (INVSQ
              (CONS
               (LIST
                (CONS (CONS (CAR (FKERN (LIST 'GAMMA (PREPSQ (CONS N 1))))) 1)
                      1))
               1)))
            (TAYLORGAMMA U V (DIFFERENCE N 1)))))) 
(PUT 'GAMMATOPSI 'NUMBER-OF-ARGS 2) 
(PUT 'GAMMATOPSI 'DEFINED-ON-LINE '809) 
(PUT 'GAMMATOPSI 'DEFINED-IN-FILE 'SPECFN/MEIJERG.RED) 
(PUT 'GAMMATOPSI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GAMMATOPSI (U N)
    (COND
     ((EQUAL N 1)
      (CONS (LIST (CONS (CONS (CAR (FKERN (LIST 'PSI (PREPSQ U)))) 1) 1)) 1))
     (T
      (ADDSQ
       (MULTSQ
        (CONS (LIST (CONS (CONS (CAR (FKERN (LIST 'PSI (PREPSQ U)))) 1) 1)) 1)
        (GAMMATOPSI U (DIFFERENCE N 1)))
       (DIFFSQ (GAMMATOPSI U (DIFFERENCE N 1)) (PREPSQ U)))))) 
(AEVAL (OPERATOR (LIST 'LST 'GFM))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (GFM (LST 1 0 1 1) (LST 1) (LST 0) (~ Z))
      (QUOTIENT (PLUS (SIGN (PLUS 1 Z)) (SIGN (DIFFERENCE 1 Z))) 2))
     (REPLACEBY (GFM (LST 0 1 1 1) (LST 1) (LST 0) (~ Z))
      (DIFFERENCE 1
                  (QUOTIENT (PLUS (SIGN (PLUS 1 Z)) (SIGN (DIFFERENCE 1 Z)))
                            2))))))) 
(AEVAL
 (LET
  (LIST
   (LIST 'REPLACEBY
         (LIST 'MEIJERG (LIST 'LIST (LIST 'LIST) 1)
               (LIST 'LIST (LIST 'LIST 0 0) (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
               (LIST '~ 'X))
         (LIST 'QUOTIENT
               (LIST 'G_FRESNEL_S (LIST 'TIMES 2 (LIST 'SQRT 'X)) (MINUS 1))
               (LIST 'TIMES (LIST 'EXPT 2 (MINUS 2)) (LIST 'SQRT 'PI))))))) 
(ENDMODULE) 
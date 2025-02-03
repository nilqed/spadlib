(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RSOLVE)) 
(CREATE-PACKAGE '(RSOLVE) '(SOLVE)) 
(SWITCH (LIST 'MULTIPLICITIES)) 
(GLOBAL '(!ARBINT MULTIPLICITIES*)) 
(SHARE (LIST 'MULTIPLICITIES*)) 
(PUT 'ARBRAT 'SIMPFN 'SIMPIDEN) 
(FLUID '(*I_SOLVE)) 
(PUT 'I_SOLVE 'PSOPFN 'I_SOLVE-EVAL) 
(PUT 'I_SOLVE-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'I_SOLVE-EVAL 'DEFINED-ON-LINE '99) 
(PUT 'I_SOLVE-EVAL 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'I_SOLVE-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE I_SOLVE-EVAL (U) ((LAMBDA (*I_SOLVE) (R_SOLVE-EVAL U)) T)) 
(PUT 'R_SOLVE 'PSOPFN 'R_SOLVE-EVAL) 
(PUT 'R_SOLVE-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'R_SOLVE-EVAL 'DEFINED-ON-LINE '105) 
(PUT 'R_SOLVE-EVAL 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'R_SOLVE-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R_SOLVE-EVAL (U)
    (COND ((NULL U) (REDERR "r/i_solve called with no equations"))
          (T
           (PROG (VAR MUL NOEQS)
             (SETQ MUL (COND (*MULTIPLICITIES 'EXPAND) (T 'SEPARATE)))
             (PROG (MAYBE_VAR)
               (SETQ MAYBE_VAR T)
               (PROG (X)
                 (SETQ X (CDR U))
                LAB
                 (COND ((NULL X) (RETURN NIL)))
                 ((LAMBDA (X)
                    (PROGN
                     (COND ((EQ X 'SEPARATE) (SETQ MUL 'SEPARATE))
                           ((MEMQ X '(EXPAND MULTIPLICITIES))
                            (SETQ MUL 'EXPAND))
                           ((EQ X 'TOGETHER) (SETQ MUL T))
                           ((EQ X 'NOMUL) (SETQ MUL NIL))
                           ((EQ X 'NOEQS) (SETQ NOEQS T))
                           (MAYBE_VAR (SETQ VAR X))
                           (T (TYPERR X "optional r/i_solve argument")))
                     (SETQ MAYBE_VAR NIL)))
                  (CAR X))
                 (SETQ X (CDR X))
                 (GO LAB)))
             (RETURN
              ((LAMBDA (OLD_DMODE)
                 (PROG (RESULT)
                   (COND
                    (OLD_DMODE ((LAMBDA (*MSG) (SETDMODE OLD_DMODE NIL)) NIL)))
                   (SETQ RESULT
                           (ERRORSET*
                            (LIST 'R_SOLVE-EVAL1 (MKQUOTE (CAR U))
                                  (MKQUOTE VAR) (MKQUOTE MUL) (MKQUOTE NOEQS))
                            T))
                   (COND
                    (OLD_DMODE ((LAMBDA (*MSG) (SETDMODE OLD_DMODE T)) NIL)))
                   (COND ((ERRORP RESULT) (ERROR1))
                         (T (RETURN (CAR RESULT))))))
               DMODE*)))))) 
(SWITCH (LIST 'TRSOLVE)) 
(DEFLIST '((TR_WRITE RLIS)) 'STAT) 
(PUT 'TR_WRITE 'NUMBER-OF-ARGS 1) 
(PUT 'TR_WRITE 'DEFINED-ON-LINE '158) 
(PUT 'TR_WRITE 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'TR_WRITE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TR_WRITE (U)
    (COND
     (*TRSOLVE
      (PROGN
       (PROG (EL)
         (SETQ EL U)
        LAB
         (COND ((NULL EL) (RETURN NIL)))
         ((LAMBDA (EL) (PRIN2 EL)) (CAR EL))
         (SETQ EL (CDR EL))
         (GO LAB))
       (TERPRI))))) 
(PUT 'R_SOLVE-EVAL1 'NUMBER-OF-ARGS 4) 
(PUT 'R_SOLVE-EVAL1 'DEFINED-ON-LINE '162) 
(PUT 'R_SOLVE-EVAL1 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'R_SOLVE-EVAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE R_SOLVE-EVAL1 (F VAR MUL NOEQS)
    (PROG (X U)
      (SETQ F (CAR (SIMP* (*EQN2A (REVAL1 F T)))))
      (TR_WRITE (LIST "Simplified poly to solve: " F))
      (COND
       ((NOT (ATOM F)) (COND ((EQCAR (SETQ X (CAAAR F)) 'LIST) (GO ERROR)))))
      (COND
       (VAR
        (COND
         ((NOT (EQ (SETQ VAR (*A2K VAR)) X))
          (COND
           (X
            (COND ((NOT (SMEMBER VAR F)) (RETURN (CONS 'LIST NIL)))
                  (T (GO ERROR))))
           ((NULL F)
            (PROGN
             (SETQ MULTIPLICITIES*
                     (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST NIL)))
             (RETURN
              (CONS 'LIST
                    (LIST
                     (MAKEQN-MAYBE VAR
                      (LIST (COND (*I_SOLVE 'ARBINT) (T 'ARBRAT))
                            (SETQ !ARBINT (PLUS !ARBINT 1)))
                      NOEQS)))))))))))
      (COND ((ATOM F) (RETURN (CONS 'LIST NIL))))
      (SETQ U F)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ATOM U))) (RETURN NIL)))
        (SETQ U
                (COND ((AND (EQ (CAAAR U) X) (FIXP (CDAR U))) (CDR U))
                      (T 'ERROR)))
        (GO WHILELABEL))
      (COND
       ((NOT (EQ U 'ERROR)) (RETURN (R_SOLVE-OUTPUT F (R_SOLVE F) MUL NOEQS))))
     ERROR
      (TYPERR (PREPF F) "univariate polynomial over Z"))) 
(PUT 'R_SOLVE-OUTPUT 'NUMBER-OF-ARGS 4) 
(PUT 'R_SOLVE-OUTPUT 'DEFINED-ON-LINE '192) 
(PUT 'R_SOLVE-OUTPUT 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'R_SOLVE-OUTPUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE R_SOLVE-OUTPUT (F ZEROS MUL NOEQS)
    (PROG (X)
      (SETQ X (CAAAR F))
      (RETURN
       (CONS 'LIST
             (COND
              ((NULL MUL)
               (PROGN
                (SETQ MULTIPLICITIES*
                        (PROGN
                         (SETQ ALGLIST* (CONS NIL NIL))
                         (CONS 'LIST NIL)))
                (PROG (S FORALL-RESULT FORALL-ENDPTR)
                  (SETQ S ZEROS)
                  (COND ((NULL S) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (S)
                                      (MAKEQN-MAYBE X (*N2A S) NOEQS))
                                    (CAR S))
                                   NIL)))
                 LOOPLABEL
                  (SETQ S (CDR S))
                  (COND ((NULL S) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (S) (MAKEQN-MAYBE X (*N2A S) NOEQS))
                            (CAR S))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
              (T
               (PROG (SOLNS M)
                 (SETQ M 0)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT ZEROS) (RETURN NIL)))
                   (PROGN
                    (SETQ F (DIFF F X))
                    (SETQ M (PLUS M 1))
                    (SETQ ZEROS
                            (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Z ZEROS)
                             STARTOVER
                              (COND ((NULL Z) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (Z)
                                         (COND
                                          ((COND
                                            (*I_SOLVE
                                             (ZEROP (EVAL_UNI_POLY F Z)))
                                            (T
                                             (NULL
                                              (CAR (EVAL_UNI_POLY_Q F Z)))))
                                           (LIST Z))
                                          (T
                                           (PROGN
                                            (SETQ SOLNS
                                                    (CONS (CONS Z M) SOLNS))
                                            NIL))))
                                       (CAR Z)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ Z (CDR Z))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL Z) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (Z)
                                         (COND
                                          ((COND
                                            (*I_SOLVE
                                             (ZEROP (EVAL_UNI_POLY F Z)))
                                            (T
                                             (NULL
                                              (CAR (EVAL_UNI_POLY_Q F Z)))))
                                           (LIST Z))
                                          (T
                                           (PROGN
                                            (SETQ SOLNS
                                                    (CONS (CONS Z M) SOLNS))
                                            NIL))))
                                       (CAR Z)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ Z (CDR Z))
                              (GO LOOPLABEL))))
                   (GO WHILELABEL))
                 (SETQ SOLNS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S SOLNS)
                           (COND ((NULL S) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (S)
                                               (CONS
                                                (MAKEQN-MAYBE X (*N2A (CAR S))
                                                 NOEQS)
                                                (CDR S)))
                                             (CAR S))
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (CDR S))
                           (COND ((NULL S) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (S)
                                       (CONS
                                        (MAKEQN-MAYBE X (*N2A (CAR S)) NOEQS)
                                        (CDR S)))
                                     (CAR S))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ MULTIPLICITIES*
                         (PROGN (SETQ ALGLIST* (CONS NIL NIL)) NIL))
                 (COND
                  ((EQ MUL 'SEPARATE)
                   (PROGN
                    (SETQ SOLNS
                            (PROG (S FORALL-RESULT FORALL-ENDPTR)
                              (SETQ S SOLNS)
                              (COND ((NULL S) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S)
                                                  (PROGN
                                                   (SETQ MULTIPLICITIES*
                                                           (PROGN
                                                            (SETQ ALGLIST*
                                                                    (CONS NIL
                                                                          NIL))
                                                            (CONS (CDR S)
                                                                  MULTIPLICITIES*)))
                                                   (CAR S)))
                                                (CAR S))
                                               NIL)))
                             LOOPLABEL
                              (SETQ S (CDR S))
                              (COND ((NULL S) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (S)
                                          (PROGN
                                           (SETQ MULTIPLICITIES*
                                                   (PROGN
                                                    (SETQ ALGLIST*
                                                            (CONS NIL NIL))
                                                    (CONS (CDR S)
                                                          MULTIPLICITIES*)))
                                           (CAR S)))
                                        (CAR S))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    (SETQ MULTIPLICITIES*
                            (PROGN
                             (SETQ ALGLIST* (CONS NIL NIL))
                             (REVERSE MULTIPLICITIES*)))))
                  ((EQ MUL 'EXPAND)
                   (SETQ SOLNS
                           (PROG (S FORALL-RESULT FORALL-ENDPTR)
                             (SETQ S SOLNS)
                            STARTOVER
                             (COND ((NULL S) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     ((LAMBDA (S)
                                        (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ I 1)
                                          (COND
                                           ((MINUSP (DIFFERENCE (CDR S) I))
                                            (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS (CAR S) NIL)))
                                         LOOPLABEL
                                          (SETQ I (PLUS2 I 1))
                                          (COND
                                           ((MINUSP (DIFFERENCE (CDR S) I))
                                            (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS (CAR S) NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                      (CAR S)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ S (CDR S))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND ((NULL S) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     ((LAMBDA (S)
                                        (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ I 1)
                                          (COND
                                           ((MINUSP (DIFFERENCE (CDR S) I))
                                            (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS (CAR S) NIL)))
                                         LOOPLABEL
                                          (SETQ I (PLUS2 I 1))
                                          (COND
                                           ((MINUSP (DIFFERENCE (CDR S) I))
                                            (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS (CAR S) NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                      (CAR S)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ S (CDR S))
                             (GO LOOPLABEL))))
                  (T
                   (SETQ SOLNS
                           (PROG (S FORALL-RESULT FORALL-ENDPTR)
                             (SETQ S SOLNS)
                             (COND ((NULL S) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (S)
                                                 (CONS 'LIST
                                                       (LIST (CAR S) (CDR S))))
                                               (CAR S))
                                              NIL)))
                            LOOPLABEL
                             (SETQ S (CDR S))
                             (COND ((NULL S) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (S)
                                         (CONS 'LIST (LIST (CAR S) (CDR S))))
                                       (CAR S))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))))
                 (SETQ MULTIPLICITIES*
                         (PROGN
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (CONS 'LIST MULTIPLICITIES*)))
                 (RETURN SOLNS)))))))) 
(PUT 'MAKEQN-MAYBE 'NUMBER-OF-ARGS 3) 
(PUT 'MAKEQN-MAYBE 'DEFINED-ON-LINE '235) 
(PUT 'MAKEQN-MAYBE 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'MAKEQN-MAYBE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKEQN-MAYBE (X S NOEQS) (COND (NOEQS S) (T (LIST 'EQUAL X S)))) 
(PUT '*N2A 'NUMBER-OF-ARGS 1) 
(PUT '*N2A 'DEFINED-ON-LINE '238) 
(PUT '*N2A 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT '*N2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *N2A (N) (COND ((FIXP N) N) (T (*Q2A1 N *NOSQ)))) 
(PUT '|MOD#| 'NUMBER-OF-ARGS 1) 
(PUT '|MOD#| 'DEFINED-ON-LINE '248) 
(PUT '|MOD#| 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT '|MOD#| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |MOD#| (N)
    ((LAMBDA (N_M) (COND ((LESSP N_M 0) (PLUS N_M CURRENT-MODULUS)) (T N_M)))
     (REMAINDER N CURRENT-MODULUS))) 
(DE MOD+ (A B) (GENERAL-MODULAR-PLUS A B)) 
(PUT 'MOD+ 'NUMBER-OF-ARGS 2) 
(PUT 'MOD+ 'DEFINED-ON-LINE '254) 
(PUT 'MOD+ 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'MOD+ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'MOD+ 'INLINE '(LAMBDA (A B) (GENERAL-MODULAR-PLUS A B))) 
(DE RSOLVE_MOD* (A B) (REMAINDER (TIMES A B) CURRENT-MODULUS)) 
(PUT 'RSOLVE_MOD* 'NUMBER-OF-ARGS 2) 
(PUT 'RSOLVE_MOD* 'DEFINED-ON-LINE '258) 
(PUT 'RSOLVE_MOD* 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'RSOLVE_MOD* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'RSOLVE_MOD* 'INLINE
      '(LAMBDA (A B) (REMAINDER (TIMES A B) CURRENT-MODULUS))) 
(PUT 'MOD/ 'NUMBER-OF-ARGS 2) 
(PUT 'MOD/ 'DEFINED-ON-LINE '262) 
(PUT 'MOD/ 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'MOD/ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD/ (A B)
    (REMAINDER (TIMES A (GENERAL-RECIPROCAL-BY-GCD CURRENT-MODULUS B 0 1))
               CURRENT-MODULUS)) 
(DE MOD^ (A N) (GENERAL-MODULAR-EXPT A N)) 
(PUT 'MOD^ 'NUMBER-OF-ARGS 2) 
(PUT 'MOD^ 'DEFINED-ON-LINE '266) 
(PUT 'MOD^ 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'MOD^ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'MOD^ 'INLINE '(LAMBDA (A N) (GENERAL-MODULAR-EXPT A N))) 
(FLUID '(*EZGCD *GCD CURRENT-MODULUS MODULUS/2)) 
(PUT 'R_SOLVE 'NUMBER-OF-ARGS 1) 
(PUT 'R_SOLVE 'DEFINED-ON-LINE '276) 
(PUT 'R_SOLVE 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'R_SOLVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R_SOLVE (F)
    (PROG (X F1 FM F1M L A_N A_0 P B N P^N |2^R| |2^R1| P2R P2R1 *EZGCD *GCD
           CURRENT-MODULUS MODULUS/2)
      (SETQ X (CAAAR F))
      (SETQ *GCD T)
      (SETQ F ((LAMBDA (*EZGCD) (QUOTF* F (GCDF F (DIFF F X)))) T))
      (SETQ F1 (DIFF F X))
      (TR_WRITE (LIST "Squarefree poly: " F))
      (SETQ A_N (ABS (CDAR F)))
      (SETQ A_0 F)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT (ATOM A_0)) (CDR A_0))) (RETURN NIL)))
        (SETQ A_0 (CDR A_0))
        (GO WHILELABEL))
      (COND ((NOT (ATOM A_0)) (SETQ A_0 (CDAR A_0))))
      (SETQ A_0 (ABS A_0))
      (TR_WRITE (LIST "Abs leading coeff: " A_N ",  abs trailing coeff: " A_0))
      (PROG (OLD_MOD DMODE*)
        (SETQ DMODE* '|:MOD:|)
        (SETQ OLD_MOD (SETMOD (SETQ P 2)))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (OR (ZEROP (REMAINDER A_N P))
                 (NEQ (GCDF (SETQ FM (RESIMPF F)) (RESIMPF F1)) 1)))
            (RETURN NIL)))
          (SETMOD (SETQ P (NEXTPRIME P)))
          (GO WHILELABEL))
        (SETMOD OLD_MOD))
      (SETQ FM (RESIMPF FM))
      (TR_WRITE (LIST "Prime modulus: " P))
      (TR_WRITE (LIST "Poly mod " P ": " FM))
      (SETQ CURRENT-MODULUS P)
      (PROG (XX)
        (SETQ XX 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE P 1) XX)) (RETURN NIL)))
        (COND ((ZEROP (MOD_EVAL_UNI_POLY FM XX)) (SETQ L (CONS XX L))))
        (SETQ XX (PLUS2 XX 1))
        (GO LAB))
      (TR_WRITE (LIST "Zero set mod " P ": " L))
      (COND ((NULL L) (RETURN NIL)))
      (SETQ B (TIMES 2 (COND (*I_SOLVE A_0) (T (TIMES A_N A_0)))))
      (SETQ N 1)
      (SETQ P^N P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ P^N B)) (RETURN NIL)))
        (PROGN (SETQ N (PLUS N 1)) (SETQ P^N (TIMES P P^N)))
        (GO WHILELABEL))
      (TR_WRITE (LIST "Modular prime power bound: " N))
      (SETQ |2^R| 1)
      (SETQ P2R P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP |2^R| N)) (RETURN NIL)))
        (PROGN
         (SETQ P2R1
                 (COND ((LESSP (SETQ |2^R1| (TIMES 2 |2^R|)) N) (EXPT P2R 2))
                       (T (PROGN (SETQ |2^R1| N) P^N))))
         (TR_WRITE (LIST "****************************************"))
         (TR_WRITE (LIST "Lift modulo prime power " P "^" |2^R1| " = " P2R1))
         (SETQ L
                 (PROG (ALPHA_R FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ALPHA_R L)
                   (COND ((NULL ALPHA_R) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ALPHA_R)
                                       (PROG (FM B_R F1M Y)
                                         (SETQ FM 0)
                                         (SETQ B_R 0)
                                         (SETQ F1M 0)
                                         (SETQ Y 0)
                                         (TR_WRITE
                                          (LIST "Current zero mod " P "^" |2^R|
                                                ": " ALPHA_R))
                                         (SETQ CURRENT-MODULUS P2R1)
                                         (SETQ FM
                                                 (MOD_EVAL_UNI_POLY F ALPHA_R))
                                         (TR_WRITE (LIST "fm: " FM))
                                         (COND ((ZEROP FM) (RETURN ALPHA_R)))
                                         (SETQ B_R (QUOTIENT FM P2R))
                                         (SETQ CURRENT-MODULUS P2R)
                                         (SETQ F1M
                                                 (MOD_EVAL_UNI_POLY F1
                                                  ALPHA_R))
                                         (TR_WRITE
                                          (LIST "b_r: " B_R ",  f1m: " F1M))
                                         (SETQ Y
                                                 (MOD/ (DIFFERENCE P2R B_R)
                                                  F1M))
                                         (TR_WRITE
                                          (LIST "y = -b_r/f1m mod " P2R ": "
                                                Y))
                                         (RETURN
                                          (REMAINDER
                                           (PLUS ALPHA_R (TIMES P2R Y))
                                           P2R1))))
                                     (CAR ALPHA_R))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ALPHA_R (CDR ALPHA_R))
                   (COND ((NULL ALPHA_R) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ALPHA_R)
                               (PROG (FM B_R F1M Y)
                                 (SETQ FM 0)
                                 (SETQ B_R 0)
                                 (SETQ F1M 0)
                                 (SETQ Y 0)
                                 (TR_WRITE
                                  (LIST "Current zero mod " P "^" |2^R| ": "
                                        ALPHA_R))
                                 (SETQ CURRENT-MODULUS P2R1)
                                 (SETQ FM (MOD_EVAL_UNI_POLY F ALPHA_R))
                                 (TR_WRITE (LIST "fm: " FM))
                                 (COND ((ZEROP FM) (RETURN ALPHA_R)))
                                 (SETQ B_R (QUOTIENT FM P2R))
                                 (SETQ CURRENT-MODULUS P2R)
                                 (SETQ F1M (MOD_EVAL_UNI_POLY F1 ALPHA_R))
                                 (TR_WRITE (LIST "b_r: " B_R ",  f1m: " F1M))
                                 (SETQ Y (MOD/ (DIFFERENCE P2R B_R) F1M))
                                 (TR_WRITE
                                  (LIST "y = -b_r/f1m mod " P2R ": " Y))
                                 (RETURN
                                  (REMAINDER (PLUS ALPHA_R (TIMES P2R Y))
                                             P2R1))))
                             (CAR ALPHA_R))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (TR_WRITE (LIST "Zero set mod " P "^" |2^R1| ": " L))
         (SETQ |2^R| |2^R1|)
         (SETQ P2R P2R1))
        (GO WHILELABEL))
      (RETURN
       (COND
        (*I_SOLVE
         (PROG (ALPHA FORALL-RESULT FORALL-ENDPTR)
           (SETQ ALPHA L)
          STARTOVER
           (COND ((NULL ALPHA) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   ((LAMBDA (ALPHA)
                      ((LAMBDA (A)
                         (COND ((ZEROP (EVAL_UNI_POLY F A)) (LIST A))))
                       (BALANCE_MOD ALPHA P2R)))
                    (CAR ALPHA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ ALPHA (CDR ALPHA))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL ALPHA) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   ((LAMBDA (ALPHA)
                      ((LAMBDA (A)
                         (COND ((ZEROP (EVAL_UNI_POLY F A)) (LIST A))))
                       (BALANCE_MOD ALPHA P2R)))
                    (CAR ALPHA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ ALPHA (CDR ALPHA))
           (GO LOOPLABEL)))
        (T
         (PROG (ALPHA FORALL-RESULT FORALL-ENDPTR)
           (SETQ ALPHA L)
          STARTOVER
           (COND ((NULL ALPHA) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   ((LAMBDA (ALPHA)
                      (PROGN
                       (COND ((ZEROP ALPHA) (SETQ ALPHA (CONS NIL 1)))
                             (T
                              (PROGN
                               (SETQ ALPHA
                                       (BALANCE_MOD
                                        (REMAINDER (TIMES ALPHA A_N) P2R) P2R))
                               (SETQ ALPHA
                                       ((LAMBDA (G)
                                          (CONS (QUOTIENT ALPHA G)
                                                (QUOTIENT A_N G)))
                                        (GCDN ALPHA A_N))))))
                       (COND
                        ((NULL (CAR (EVAL_UNI_POLY_Q F ALPHA)))
                         (LIST ALPHA)))))
                    (CAR ALPHA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ ALPHA (CDR ALPHA))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL ALPHA) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   ((LAMBDA (ALPHA)
                      (PROGN
                       (COND ((ZEROP ALPHA) (SETQ ALPHA (CONS NIL 1)))
                             (T
                              (PROGN
                               (SETQ ALPHA
                                       (BALANCE_MOD
                                        (REMAINDER (TIMES ALPHA A_N) P2R) P2R))
                               (SETQ ALPHA
                                       ((LAMBDA (G)
                                          (CONS (QUOTIENT ALPHA G)
                                                (QUOTIENT A_N G)))
                                        (GCDN ALPHA A_N))))))
                       (COND
                        ((NULL (CAR (EVAL_UNI_POLY_Q F ALPHA)))
                         (LIST ALPHA)))))
                    (CAR ALPHA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ ALPHA (CDR ALPHA))
           (GO LOOPLABEL))))))) 
(PUT 'RESIMPF 'NUMBER-OF-ARGS 1) 
(PUT 'RESIMPF 'DEFINED-ON-LINE '388) 
(PUT 'RESIMPF 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'RESIMPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESIMPF (F) ((LAMBDA (VARSTACK*) (CAR (SUBF1 F NIL))) NIL)) 
(PUT 'BALANCE_MOD 'NUMBER-OF-ARGS 2) 
(PUT 'BALANCE_MOD 'DEFINED-ON-LINE '392) 
(PUT 'BALANCE_MOD 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'BALANCE_MOD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BALANCE_MOD (N M) (COND ((GREATERP (PLUS N N) M) (DIFFERENCE N M)) (T N))) 
(PUT 'EVAL_UNI_POLY 'NUMBER-OF-ARGS 2) 
(PUT 'EVAL_UNI_POLY 'DEFINED-ON-LINE '398) 
(PUT 'EVAL_UNI_POLY 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'EVAL_UNI_POLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVAL_UNI_POLY (F A)
    (COND ((ATOM F) F)
          (T
           (PROG (R D)
             (SETQ R (CDAR F))
             (SETQ D (CDAAR F))
             (SETQ F (CDR F))
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (ATOM F))) (RETURN NIL)))
               (PROGN
                (SETQ R
                        (PLUS (TIMES R (EXPT A (DIFFERENCE D (CDAAR F))))
                              (CDAR F)))
                (SETQ D (CDAAR F))
                (SETQ F (CDR F)))
               (GO WHILELABEL))
             (SETQ R (TIMES R (EXPT A D)))
             (COND (F (SETQ R (PLUS R F))))
             (RETURN R))))) 
(PUT 'EVAL_UNI_POLY_Q 'NUMBER-OF-ARGS 2) 
(PUT 'EVAL_UNI_POLY_Q 'DEFINED-ON-LINE '414) 
(PUT 'EVAL_UNI_POLY_Q 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'EVAL_UNI_POLY_Q 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVAL_UNI_POLY_Q (F A)
    (COND ((ATOM F) (CONS F 1))
          (T
           (PROG (R D)
             (SETQ R (CONS (CDAR F) 1))
             (SETQ D (CDAAR F))
             (SETQ F (CDR F))
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (ATOM F))) (RETURN NIL)))
               (PROGN
                (SETQ R
                        (ADDSQ (MULTSQ R (EXPTSQ A (DIFFERENCE D (CDAAR F))))
                               (CONS (CDAR F) 1)))
                (SETQ D (CDAAR F))
                (SETQ F (CDR F)))
               (GO WHILELABEL))
             (RETURN (ADDSQ (MULTSQ R (EXPTSQ A D)) (CONS F 1))))))) 
(PUT 'MOD_EVAL_UNI_POLY 'NUMBER-OF-ARGS 2) 
(PUT 'MOD_EVAL_UNI_POLY 'DEFINED-ON-LINE '429) 
(PUT 'MOD_EVAL_UNI_POLY 'DEFINED-IN-FILE 'SOLVE/RSOLVE.RED) 
(PUT 'MOD_EVAL_UNI_POLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD_EVAL_UNI_POLY (F A)
    (COND ((ATOM F) (|MOD#| F))
          (T
           (PROG (R D)
             (SETQ R (|MOD#| (CDAR F)))
             (SETQ D (CDAAR F))
             (SETQ F (CDR F))
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (ATOM F))) (RETURN NIL)))
               (PROGN
                (SETQ R
                        (GENERAL-MODULAR-PLUS
                         (REMAINDER
                          (TIMES R
                                 (GENERAL-MODULAR-EXPT A
                                                       (DIFFERENCE D
                                                                   (CDAAR F))))
                          CURRENT-MODULUS)
                         (|MOD#| (CDAR F))))
                (SETQ D (CDAAR F))
                (SETQ F (CDR F)))
               (GO WHILELABEL))
             (SETQ R
                     (REMAINDER (TIMES R (GENERAL-MODULAR-EXPT A D))
                                CURRENT-MODULUS))
             (COND (F (SETQ R (GENERAL-MODULAR-PLUS R (|MOD#| F)))))
             (RETURN R))))) 
(ENDMODULE) 
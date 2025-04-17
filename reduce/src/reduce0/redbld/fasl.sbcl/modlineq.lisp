(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MODLINEQ)) 
(FLUID '(*TRA *TRMIN CURRENT-MODULUS SQRTS-MOD-PRIME)) 
(GLOBAL '(LIST-OF-MEDIUM-PRIMES SQRTS-MOD-8)) 
(EXPORTS (LIST 'CHECK-LINEQ)) 
(SETQ LIST-OF-MEDIUM-PRIMES '(101 103 107 109)) 
(SETQ SQRTS-MOD-8 (MKVECT 7)) 
(PUTV SQRTS-MOD-8 0 T) 
(PUTV SQRTS-MOD-8 1 T) 
(PUTV SQRTS-MOD-8 4 T) 
(PUT 'MODP-NTH-ROOT 'NUMBER-OF-ARGS 3) 
(PUT 'MODP-NTH-ROOT 'DEFINED-ON-LINE '46) 
(PUT 'MODP-NTH-ROOT 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'MODP-NTH-ROOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODP-NTH-ROOT (M N P)
    (PROG (J P2)
      (SETQ P2 (QUOTIENT P 2))
      (PROG (I)
        (SETQ I (MINUS P2))
       LAB
        (COND ((MINUSP (DIFFERENCE P2 I)) (RETURN NIL)))
        (COND ((IEQUAL (MODULAR-EXPT I N) M) (PROGN (SETQ J I) (SETQ I P2))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN J))) 
(PUT 'MODP-SQRT 'NUMBER-OF-ARGS 2) 
(PUT 'MODP-SQRT 'DEFINED-ON-LINE '57) 
(PUT 'MODP-SQRT 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'MODP-SQRT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODP-SQRT (N P)
    (PROG (P2 S TT)
      (SETQ P2 (QUOTIENT P 2))
      (COND ((LESSP N 0) (SETQ N (PLUS N P))))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE P2 I)) (RETURN NIL)))
        (PROG ()
          (SETQ TT (PLUS N (TIMES P I)))
          (COND ((NULL (GETV SQRTS-MOD-8 (IREMAINDER TT 8))) (RETURN NIL)))
          (COND ((GREATERP (IADD1 (IREMAINDER TT 5)) 2) (RETURN NIL)))
          (SETQ S (INT-SQRT TT))
          (COND ((FIXP S) (PROGN (SETQ P2 0) (RETURN NIL)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((OR (NOT (FIXP S)) (NULL S)) (RETURN NIL)) (T (RETURN S))))) 
(PUT 'CHECK-LINEQ 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK-LINEQ 'DEFINED-ON-LINE '81) 
(PUT 'CHECK-LINEQ 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'CHECK-LINEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK-LINEQ (M RIGHTSIDE)
    (PROG (VLIST N1 N2 U PRIMELIST M1 V MODP-SUBS ATOMS)
      (SETQ N1 (UPBV M))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (PROGN
         (SETQ U (GETV M I))
         (COND
          (U
           (PROG (J)
             (SETQ J 0)
            LAB
             (COND ((MINUSP (DIFFERENCE (SETQ N2 (UPBV U)) J)) (RETURN NIL)))
             (SETQ VLIST (VARSINSQ (GETV U J) VLIST))
             (SETQ J (PLUS2 J 1))
             (GO LAB)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ U VLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR U))
         (SETQ U (CDR U))
         (COND ((ATOM V) (SETQ ATOMS (CONS V ATOMS)))
               ((OR (EQ (CAR V) 'SQRT) (EQ (CAR V) 'EXPT))
                (PROG (W)
                  (SETQ W (VARSINSF (*Q2F (SIMP (CADR V))) NIL))
                 LAB
                  (COND ((NULL W) (RETURN NIL)))
                  ((LAMBDA (W)
                     (COND
                      ((NOT (MEMBER W VLIST))
                       (PROGN (SETQ U (CONS W U)) (SETQ VLIST (CONS W VLIST))))
                      (T NIL)))
                   (CAR W))
                  (SETQ W (CDR W))
                  (GO LAB)))
               (T (INTERR "Unexpected item"))))
        (GO WHILELABEL))
      (COND
       ((AND SQRTS-MOD-PRIME
             (SUBSETP VLIST
                      (PROG (U FORALL-RESULT FORALL-ENDPTR)
                        (SETQ U (CDR SQRTS-MOD-PRIME))
                        (COND ((NULL U) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (U) (CAR U)) (CAR U))
                                              NIL)))
                       LOOPLABEL
                        (SETQ U (CDR U))
                        (COND ((NULL U) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CAR U)) (CAR U)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (GO END-OF-LOOP)))
      (SETQ VLIST (SETDIFF VLIST ATOMS))
      (SETQ U NIL)
      (PROG (V)
        (SETQ V VLIST)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((NEQ (CAR V) 'SQRT) (SETQ U (CONS V U))))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ VLIST (NCONC U (SORTSQRTS (SETDIFF VLIST U) NIL)))
      (SETQ PRIMELIST LIST-OF-MEDIUM-PRIMES)
      (SET-MODULUS (CAR PRIMELIST))
      (SETQ ATOMS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U ATOMS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (CONS U
                                          (MODULAR-NUMBER
                                           (RANDOM (CAR PRIMELIST)))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (CONS U (MODULAR-NUMBER (RANDOM (CAR PRIMELIST)))))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GO TRY-PRIME)
     NEXT-PRIME
      (SETQ PRIMELIST (CDR PRIMELIST))
      (COND
       ((AND (NULL PRIMELIST) *TRA)
        (PROGN
         (PRIN2 "Ran out of primes in check!-lineq")
         (TERPRI)
         "Ran out of primes in check!-lineq")))
      (COND ((NULL PRIMELIST) (RETURN T)))
      (SET-MODULUS (CAR PRIMELIST))
     TRY-PRIME
      (SETQ MODP-SUBS ATOMS)
      (SETQ V VLIST)
     LOOP
      (COND ((NULL V) (GO END-OF-LOOP)))
      (SETQ U (MODP-SUBST (SIMP (CADR (CAR V))) MODP-SUBS))
      (COND ((EQ (CAAR V) 'SQRT) (SETQ U (MODP-SQRT U (CAR PRIMELIST))))
            ((EQ (CAAR V) 'EXPT)
             (SETQ U
                     (MODP-NTH-ROOT (MODULAR-EXPT U (CADR (CADDR (CAR V))))
                      (CADDR (CADDR (CAR V))) (CAR PRIMELIST))))
            (T (INTERR "Unexpected item")))
      (COND ((NULL U) (GO NEXT-PRIME)))
      (SETQ MODP-SUBS (CONS (CONS (CAR V) U) MODP-SUBS))
      (SETQ V (CDR V))
      (GO LOOP)
     END-OF-LOOP
      (COND
       ((NULL PRIMELIST)
        (PROGN
         (SETMOD (CAR SQRTS-MOD-PRIME))
         (SETQ MODP-SUBS (CDR SQRTS-MOD-PRIME))))
       (T (SETQ SQRTS-MOD-PRIME (CONS (CAR PRIMELIST) MODP-SUBS))))
      (SETQ M1 (MKVECT N1))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (PROG ()
          (SETQ U (GETV M I))
          (COND ((NULL U) (RETURN NIL)))
          (PUTV M1 I (SETQ V (MKVECT N2)))
          (PROG (J)
            (SETQ J 0)
           LAB
            (COND ((MINUSP (DIFFERENCE N2 J)) (RETURN NIL)))
            (PUTV V J (MODP-SUBST (GETV U J) MODP-SUBS))
            (SETQ J (PLUS2 J 1))
            (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ V (MKVECT N1))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (PUTV V I (MODP-SUBST (GETV RIGHTSIDE I) MODP-SUBS))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ U (MOD-JHDSOLVE M1 V))
      (COND
       ((AND (EQ U 'FAILED) (OR *TRA *TRMIN))
        (PROGN
         (PRINC "Proved insoluble mod ")
         ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) (CAR SQRTS-MOD-PRIME)))))
      (RETURN U))) 
(PUT 'VARSINSQ 'NUMBER-OF-ARGS 2) 
(PUT 'VARSINSQ 'DEFINED-ON-LINE '172) 
(PUT 'VARSINSQ 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'VARSINSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VARSINSQ (SQ VL) (VARSINSF (CAR SQ) (VARSINSF (CDR SQ) VL))) 
(PUT 'MODP-SUBST 'NUMBER-OF-ARGS 2) 
(PUT 'MODP-SUBST 'DEFINED-ON-LINE '175) 
(PUT 'MODP-SUBST 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'MODP-SUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODP-SUBST (SQ SLIST)
    (REMAINDER
     (TIMES (MODP-SUBF (CAR SQ) SLIST)
            (MODULAR-RECIPROCAL (MODP-SUBF (CDR SQ) SLIST)))
     CURRENT-MODULUS)) 
(PUT 'MODP-SUBF 'NUMBER-OF-ARGS 2) 
(PUT 'MODP-SUBF 'DEFINED-ON-LINE '180) 
(PUT 'MODP-SUBF 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'MODP-SUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODP-SUBF (SF SLIST)
    (COND ((ATOM SF) (COND ((NULL SF) 0) (T (MODULAR-NUMBER SF))))
          (T
           (PROG (U)
             (SETQ U (ASSOC (CAAAR SF) SLIST))
             (COND ((NULL U) (INTERR "Unexpected variable")))
             (RETURN
              (PROG (RESULT)
                (SETQ RESULT
                        (IPLUS2
                         (REMAINDER
                          (TIMES (MODULAR-EXPT (CDR U) (CDAAR SF))
                                 (MODP-SUBF (CDAR SF) SLIST))
                          CURRENT-MODULUS)
                         (MODP-SUBF (CDR SF) SLIST)))
                (COND
                 ((NOT (ILESSP RESULT CURRENT-MODULUS))
                  (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                (RETURN RESULT))))))) 
(PUT 'MOD-JHDSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'MOD-JHDSOLVE 'DEFINED-ON-LINE '196) 
(PUT 'MOD-JHDSOLVE 'DEFINED-IN-FILE 'ALGINT/MODLINEQ.RED) 
(PUT 'MOD-JHDSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD-JHDSOLVE (M RIGHTSIDE)
    (PROG (II N1 N2 ANS U ROW SWAPFLG SWAPS)
      (SETQ N1 (UPBV M))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (COND ((SETQ U (GETV M I)) (SETQ N2 (UPBV U))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ SWAPS (MKVECT N2))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N2 I)) (RETURN NIL)))
        (PUTV SWAPS I (DIFFERENCE N2 I))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (ISUB1 N1) I)) (RETURN NIL)))
        (PROG (K V PIVOT)
         TRYAGAIN
          (SETQ ROW (GETV M I))
          (COND ((NULL ROW) (GO INTERCHANGE)))
          (SETQ K (MINUS 1))
          (PROG (J)
            (SETQ J 0)
           LAB
            (COND ((MINUSP (DIFFERENCE N2 J)) (RETURN NIL)))
            (COND
             ((NEQ (SETQ PIVOT (GETV ROW J)) 0)
              (PROGN (SETQ K J) (SETQ J N2))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (COND ((NEQ K (MINUS 1)) (GO NEWROW)))
          (COND
           ((NEQ (GETV RIGHTSIDE I) 0)
            (PROGN (SETQ M 'FAILED) (SETQ I (SUB1 N1)) (GO FINISHED))))
         INTERCHANGE
          (SWAP M I N1)
          (SWAP RIGHTSIDE I N1)
          (SETQ N1 (ISUB1 N1))
          (COND ((IEQUAL I N1) (GO FINISHED)) (T (GO TRYAGAIN)))
         NEWROW
          (COND
           ((NEQ I K)
            (PROGN
             (SETQ SWAPFLG T)
             (SWAP SWAPS I K)
             (PROG (L)
               (SETQ L 0)
              LAB
               (COND ((MINUSP (DIFFERENCE N1 L)) (RETURN NIL)))
               (SWAP (GETV M L) I K)
               (SETQ L (PLUS2 L 1))
               (GO LAB)))))
          (SETQ PIVOT
                  ((LAMBDA (A)
                     (COND ((EQUAL A 0) A)
                           (T (IDIFFERENCE CURRENT-MODULUS A))))
                   (MODULAR-RECIPROCAL PIVOT)))
          (PROG (J)
            (SETQ J (IADD1 I))
           LAB
            (COND ((MINUSP (DIFFERENCE N1 J)) (RETURN NIL)))
            (PROG ()
              (SETQ U (GETV M J))
              (COND ((NULL U) (RETURN NIL)))
              (SETQ V (REMAINDER (TIMES (GETV U I) PIVOT) CURRENT-MODULUS))
              (COND
               ((NEQ V 0)
                (PROGN
                 (PUTV RIGHTSIDE J
                       (PROG (RESULT)
                         (SETQ RESULT
                                 (IPLUS2 (GETV RIGHTSIDE J)
                                         (REMAINDER
                                          (TIMES V (GETV RIGHTSIDE I))
                                          CURRENT-MODULUS)))
                         (COND
                          ((NOT (ILESSP RESULT CURRENT-MODULUS))
                           (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                         (RETURN RESULT)))
                 (PROG (L)
                   (SETQ L 0)
                  LAB
                   (COND ((MINUSP (DIFFERENCE N2 L)) (RETURN NIL)))
                   (PUTV U L
                         (PROG (RESULT)
                           (SETQ RESULT
                                   (IPLUS2 (GETV U L)
                                           (REMAINDER (TIMES V (GETV ROW L))
                                                      CURRENT-MODULUS)))
                           (COND
                            ((NOT (ILESSP RESULT CURRENT-MODULUS))
                             (SETQ RESULT
                                     (IDIFFERENCE RESULT CURRENT-MODULUS))))
                           (RETURN RESULT)))
                   (SETQ L (PLUS2 L 1))
                   (GO LAB))))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
         FINISHED)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((EQ M 'FAILED) (GO FAILED)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NULL (SETQ ROW (GETV M N1)))) (RETURN NIL)))
        (SETQ N1 (ISUB1 N1))
        (GO WHILELABEL))
      (SETQ U NIL)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N2 I)) (RETURN NIL)))
        (COND ((NEQ (GETV ROW I) 0) (SETQ U 'T)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((NULL U)
        (COND ((NEQ (GETV RIGHTSIDE N1) 0) (GO FAILED))
              (T (SETQ N1 (ISUB1 N1))))))
      (COND ((GREATERP N1 N2) (GO FAILED)))
      (SETQ ANS (MKVECT N2))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N2 I)) (RETURN NIL)))
        (PUTV ANS I 0)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ II N2)
      (PROG (I)
        (SETQ I N1)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
        (PROG ()
          (SETQ ROW (GETV M I))
          (PROG ()
           WHILELABEL
            (COND ((NOT (EQUAL (GETV ROW II) 0)) (RETURN NIL)))
            (SETQ II (ISUB1 II))
            (GO WHILELABEL))
          (COND ((NULL ROW) (RETURN NIL)))
          (SETQ U (GETV RIGHTSIDE I))
          (PROG (J)
            (SETQ J (IADD1 II))
           LAB
            (COND ((MINUSP (DIFFERENCE N2 J)) (RETURN NIL)))
            (SETQ U
                    (PROG (RESULT)
                      (SETQ RESULT
                              (IPLUS2 U
                                      (REMAINDER
                                       (TIMES (GETV ROW J)
                                              ((LAMBDA (A)
                                                 (COND ((EQUAL A 0) A)
                                                       (T
                                                        (IDIFFERENCE
                                                         CURRENT-MODULUS A))))
                                               (GETV ANS J)))
                                       CURRENT-MODULUS)))
                      (COND
                       ((NOT (ILESSP RESULT CURRENT-MODULUS))
                        (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                      (RETURN RESULT)))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (PUTV ANS II
                (REMAINDER (TIMES U (MODULAR-RECIPROCAL (GETV ROW II)))
                           CURRENT-MODULUS))
          (SETQ II (ISUB1 II)))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (COND (SWAPFLG (VECSORT SWAPS (LIST ANS))))
      (RETURN ANS)
     FAILED
      (COND
       (*TRA
        (PROGN
         (PRIN2 "Unable to force correct zeroes")
         (TERPRI)
         "Unable to force correct zeroes")))
      (RETURN 'FAILED))) 
(ENDMODULE) 
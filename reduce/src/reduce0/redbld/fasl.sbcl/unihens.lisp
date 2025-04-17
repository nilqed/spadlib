(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'UNIHENS)) 
(FLUID
 '(*LINEAR *OVERSHOOT *OVERVIEW *TRFAC ALPHALIST ALPHAVEC COEFFTBD
   CURRENT-FACTOR-PRODUCT CURRENT-MODULUS DELFVEC DELTAM FACTOR-LEVEL
   FACTOR-TRACE-LIST FACTORS-DONE FACTORVEC FACVEC FHATVEC HENSEL-GROWTH-SIZE
   HENSEL-POLY IRREDUCIBLE M-IMAGE-VARIABLE MODFVEC MULTIVARIATE-INPUT-POLY
   NON-MONIC NUMBER-OF-FACTORS POLYZERO PRIME-BASE RECONSTRUCTING-GCD)) 
(GLOBAL '(LARGEST-SMALL-MODULUS)) 
(PUT 'UHENSEL.EXTEND 'NUMBER-OF-ARGS 4) 
(PUT 'UHENSEL.EXTEND 'DEFINED-ON-LINE '64) 
(PUT 'UHENSEL.EXTEND 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'UHENSEL.EXTEND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE UHENSEL.EXTEND (POLY BEST-FLIST LCLIST P)
    (PROG (W K OLD-MODULUS ALPHAVEC MODULAR-FLIST FACTORVEC MODFVEC COEFFTBD
           FCOUNT FHATVEC DELTAM MOD-SYMM-FLIST CURRENT-FACTOR-PRODUCT FACVEC
           FACTORS-DONE HENSEL-POLY)
      (SETQ PRIME-BASE P)
      (SETQ OLD-MODULUS (SET-MODULUS P))
      (SETQ NUMBER-OF-FACTORS (LENGTH BEST-FLIST))
      (SETQ W (EXPT (CDAR POLY) (DIFFERENCE NUMBER-OF-FACTORS 1)))
      (COND
       ((LESSP (CDAR POLY) 0) (ERRORF (LIST "LC should not be -ve" POLY))))
      (SETQ COEFFTBD
              (MAX 110 (PLUS P 1)
                   (TIMES (CDAR POLY) (GET-COEFFT-BOUND POLY (CDAAR POLY)))))
      (SETQ POLY
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF POLY W))
                    (T (POLY-MULTF POLY W))))
      (SETQ MODULAR-FLIST
              (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                (SETQ FF BEST-FLIST)
                (COND ((NULL FF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FF) (REDUCE-MOD-P FF)) (CAR FF))
                                 NIL)))
               LOOPLABEL
                (SETQ FF (CDR FF))
                (COND ((NULL FF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (FF) (REDUCE-MOD-P FF)) (CAR FF)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NOT (EQUAL W 1))
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN (PRIN2* "Altered univariate polynomial: ") (PRINTSF POLY))
             (WRS STREAM)))))))
      (SETQ MOD-SYMM-FLIST
              (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                (SETQ FF MODULAR-FLIST)
                (COND ((NULL FF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FF) (MAKE-MODULAR-SYMMETRIC FF))
                                  (CAR FF))
                                 NIL)))
               LOOPLABEL
                (SETQ FF (CDR FF))
                (COND ((NULL FF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FF) (MAKE-MODULAR-SYMMETRIC FF)) (CAR FF))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PRIN2* "The factors mod ")
              (PRIN2* P)
              (PROGN (PRIN2* " to start from are:") (TERPRI* NIL))
              (SETQ FCOUNT 1)
              (PROG (FF)
                (SETQ FF MOD-SYMM-FLIST)
               LAB
                (COND ((NULL FF) (RETURN NIL)))
                ((LAMBDA (FF)
                   (PROGN
                    (PRIN2* "   f(")
                    (PRIN2* FCOUNT)
                    (PRIN2* ")=")
                    (PRINTSF FF)
                    (SETQ FCOUNT (IADD1 FCOUNT))))
                 (CAR FF))
                (SETQ FF (CDR FF))
                (GO LAB))
              (TERPRI* NIL))
             (WRS STREAM)))))))
      (SETQ ALPHALIST (ALPHAS NUMBER-OF-FACTORS MODULAR-FLIST 1))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PROGN
               (PRIN2*
                "The following modular polynomials are chosen such that:")
               (TERPRI* NIL))
              (TERPRI)
              (PRIN2* "   a(1)*h(1) + ... + a(")
              (PRIN2* NUMBER-OF-FACTORS)
              (PRIN2* ")*h(")
              (PRIN2* NUMBER-OF-FACTORS)
              (PRIN2* ") = 1 mod ")
              (PROGN (PRIN2* P) (TERPRI* NIL))
              (TERPRI)
              (PROGN
               (PRIN2* "  where h(i)=(product of all f(j) [see below])/f(i)")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "    and degree of a(i) < degree of f(i).")
               (TERPRI* NIL))
              (SETQ FCOUNT 1)
              (PROG (A)
                (SETQ A MODULAR-FLIST)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A)
                   (PROGN
                    (PRIN2* "   a(")
                    (PRIN2* FCOUNT)
                    (PRIN2* ")=")
                    (PRINTSF (CDR (GET-ALPHA A)))
                    (PRIN2* "   f(")
                    (PRIN2* FCOUNT)
                    (PRIN2* ")=")
                    (PRINTSF A)
                    (SETQ FCOUNT (IADD1 FCOUNT))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB)))
             (WRS STREAM)))))))
      (SETQ K 0)
      (SETQ FACTORVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ MODFVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ ALPHAVEC (MKVECT NUMBER-OF-FACTORS))
      (PROG (MODSYMMF)
        (SETQ MODSYMMF MOD-SYMM-FLIST)
       LAB
        (COND ((NULL MODSYMMF) (RETURN NIL)))
        ((LAMBDA (MODSYMMF)
           (PROGN
            (PUTV FACTORVEC (SETQ K (PLUS K 1))
                  (FORCE-LC MODSYMMF (CAR LCLIST)))
            (SETQ LCLIST (CDR LCLIST))))
         (CAR MODSYMMF))
        (SETQ MODSYMMF (CDR MODSYMMF))
        (GO LAB))
      (SETQ K 0)
      (PROG (MODFACTOR)
        (SETQ MODFACTOR MODULAR-FLIST)
       LAB
        (COND ((NULL MODFACTOR) (RETURN NIL)))
        ((LAMBDA (MODFACTOR)
           (PROGN
            (PUTV MODFVEC (SETQ K (PLUS K 1)) MODFACTOR)
            (PUTV ALPHAVEC K (CDR (GET-ALPHA MODFACTOR)))
            NIL))
         (CAR MODFACTOR))
        (SETQ MODFACTOR (CDR MODFACTOR))
        (GO LAB))
      (SETQ FHATVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ W (HENSEL-MOD-P POLY MODFVEC FACTORVEC COEFFTBD NIL P))
      (COND ((EQUAL (CAR W) 'OVERSHOT) (SETQ W (UHENSEL.EXTEND1 POLY W)))
            (T (SETQ W (UHENSEL.EXTEND2 W))))
      (SET-MODULUS OLD-MODULUS)
      (COND
       (IRREDUCIBLE
        (PROGN
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              (PROGN
               (PRIN2* "Two factors and overshooting means irreducible")
               (TERPRI* NIL))
              (WRS STREAM)))))
         (RETURN T))))
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROG (K)
             (SETQ K 0)
             (PROGN
              (PRIN2* "Univariate factors, possibly with adjusted leading ")
              (TERPRI* NIL))
             (PROGN (PRIN2* "coefficients, are:") (TERPRI* NIL))
             (PROG (WW)
               (SETQ WW (CDR W))
              LAB
               (COND ((NULL WW) (RETURN NIL)))
               ((LAMBDA (WW)
                  (PROGN
                   (PRIN2* " f(")
                   (PRIN2* (SETQ K (IPLUS2 K 1)))
                   (PRIN2* ")=")
                   (PRINTSF WW)))
                (CAR WW))
               (SETQ WW (CDR WW))
               (GO LAB)))
           (WRS STREAM)))))
      (RETURN
       (COND
        (NON-MONIC (CONS (CAR W) (PRIMITIVE.PARTS (CDR W) M-IMAGE-VARIABLE T)))
        (T W))))) 
(PUT 'UHENSEL.EXTEND1 'NUMBER-OF-ARGS 2) 
(PUT 'UHENSEL.EXTEND1 'DEFINED-ON-LINE '157) 
(PUT 'UHENSEL.EXTEND1 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'UHENSEL.EXTEND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UHENSEL.EXTEND1 (POLY W)
    (PROG (OKLIST BADLIST M R FF OM POL)
      (SETQ M (CADR W))
      (SETQ R (GETV FACTORVEC 0))
      (COND ((EQUAL R 2) (RETURN (SETQ IRREDUCIBLE T))))
      (COND
       (FACTORS-DONE
        (PROGN
         (SETQ POLY HENSEL-POLY)
         (PROG (WW)
           (SETQ WW FACTORS-DONE)
          LAB
           (COND ((NULL WW) (RETURN NIL)))
           ((LAMBDA (WW)
              (SETQ POLY
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF POLY WW))
                            (T (POLY-MULTF POLY WW)))))
            (CAR WW))
           (SETQ WW (CDR WW))
           (GO LAB)))))
      (SETQ POL POLY)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ ALPHALIST NIL)
      (PROG (I)
        (SETQ I R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (SETQ ALPHALIST
                (CONS
                 (CONS (REDUCE-MOD-P (GETV FACTORVEC I)) (GETV ALPHAVEC I))
                 ALPHALIST))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SET-MODULUS OM)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ FF (GETV FACTORVEC I))
         (COND
          ((NOT (NULL (SETQ W ((LAMBDA (*EXP) (QUOTF1 POL FF)) T))))
           (PROGN (SETQ OKLIST (CONS FF OKLIST)) (SETQ POL W)))
          (T (SETQ BADLIST (CONS (CONS I FF) BADLIST)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NULL BADLIST) (SETQ W (CONS 'OK OKLIST)))
            (T
             (PROGN
              (COND
               ((NOT *OVERVIEW)
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN
                      (PROGN (PRIN2* "Overshot factors are:") (TERPRI* NIL))
                      (PROG (F)
                        (SETQ F BADLIST)
                       LAB
                        (COND ((NULL F) (RETURN NIL)))
                        ((LAMBDA (F)
                           (PROGN
                            (PRIN2* " f(")
                            (PRIN2* (CAR F))
                            (PRIN2* ")=")
                            (PRINTSF (CDR F))))
                         (CAR F))
                        (SETQ F (CDR F))
                        (GO LAB)))
                     (WRS STREAM)))))))
              (SETQ W (TRY.COMBINING BADLIST POL M NIL))
              (COND
               ((EQUAL (CAR W) '|ONE BAD FACTOR|)
                (PROG (X)
                  (SETQ W (APPEND OKLIST (CDR W)))
                  (SETQ X 1)
                  (PROG (V)
                    (SETQ V W)
                   LAB
                    (COND ((NULL V) (RETURN NIL)))
                    ((LAMBDA (V)
                       (SETQ X
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF X V))
                                     (T (POLY-MULTF X V)))))
                     (CAR V))
                    (SETQ V (CDR V))
                    (GO LAB))
                  (SETQ W (CONS 'OK (CONS (QUOTFAIL POL X) W)))))
               (T (SETQ W (CONS 'OK (APPEND OKLIST W))))))))
      (COND
       ((AND (NOT *LINEAR) MULTIVARIATE-INPUT-POLY)
        (PROGN
         (SETQ POLY 1)
         (SETQ NUMBER-OF-FACTORS 0)
         (PROG (FACC)
           (SETQ FACC (CDR W))
          LAB
           (COND ((NULL FACC) (RETURN NIL)))
           ((LAMBDA (FACC)
              (PROGN
               (SETQ POLY
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF POLY FACC))
                             (T (POLY-MULTF POLY FACC))))
               (SETQ NUMBER-OF-FACTORS (IPLUS2 1 NUMBER-OF-FACTORS))))
            (CAR FACC))
           (SETQ FACC (CDR FACC))
           (GO LAB))
         (RESET-QUADRATIC-STEP-FLUIDS POLY (CDR W) NUMBER-OF-FACTORS)
         (COND
          ((EQUAL M DELTAM)
           (ERRORF (LIST "Coefft bound < prime ?" COEFFTBD M))))
         (SETQ M (TIMES DELTAM DELTAM))
         (PROG ()
          WHILELABEL
           (COND ((NOT (LESSP M LARGEST-SMALL-MODULUS)) (RETURN NIL)))
           (PROGN
            (QUADRATIC-STEP M NUMBER-OF-FACTORS)
            (SETQ M (TIMES M DELTAM)))
           (GO WHILELABEL))
         (SETQ HENSEL-GROWTH-SIZE DELTAM)
         (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
         (SETQ ALPHALIST NIL)
         (PROG (I)
           (SETQ I NUMBER-OF-FACTORS)
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
           (SETQ ALPHALIST
                   (CONS
                    (CONS (REDUCE-MOD-P (GETV FACTORVEC I)) (GETV ALPHAVEC I))
                    ALPHALIST))
           (SETQ I (PLUS2 I (MINUS 1)))
           (GO LAB))
         (SET-MODULUS OM))))
      (RETURN W))) 
(PUT 'UHENSEL.EXTEND2 'NUMBER-OF-ARGS 1) 
(PUT 'UHENSEL.EXTEND2 'DEFINED-ON-LINE '224) 
(PUT 'UHENSEL.EXTEND2 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'UHENSEL.EXTEND2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UHENSEL.EXTEND2 (W)
    (PROG (R FACLIST OM)
      (SETQ R (GETV FACTORVEC 0))
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ ALPHALIST NIL)
      (PROG (I)
        (SETQ I R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (SETQ ALPHALIST
                (CONS
                 (CONS (REDUCE-MOD-P (GETV FACTORVEC I)) (GETV ALPHAVEC I))
                 ALPHALIST))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SET-MODULUS OM)
      (PROG (I)
        (SETQ I R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (SETQ FACLIST (CONS (GETV FACTORVEC I) FACLIST))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN (CONS (CAR W) FACLIST)))) 
(PUT 'GET-COEFFT-BOUND 'NUMBER-OF-ARGS 2) 
(PUT 'GET-COEFFT-BOUND 'DEFINED-ON-LINE '239) 
(PUT 'GET-COEFFT-BOUND 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'GET-COEFFT-BOUND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET-COEFFT-BOUND (POLY DDEG)
    (TIMES (BINOMIAL-COEFFT (QUOTIENT DDEG 2) (QUOTIENT DDEG 4))
           (ROOT-SQUARES POLY 0))) 
(PUT 'BINOMIAL-COEFFT 'NUMBER-OF-ARGS 2) 
(PUT 'BINOMIAL-COEFFT 'DEFINED-ON-LINE '244) 
(PUT 'BINOMIAL-COEFFT 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'BINOMIAL-COEFFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BINOMIAL-COEFFT (N R)
    (COND ((LESSP N R) NIL) ((EQUAL N R) 1) ((EQUAL R 1) N)
          (T
           (PROG (N-C-R B)
             (SETQ N-C-R 1)
             (SETQ B (MIN R (DIFFERENCE N R)))
             (PROG (I)
               (SETQ I 1)
              LAB
               (COND ((MINUSP (DIFFERENCE B I)) (RETURN NIL)))
               (SETQ N-C-R
                       (QUOTIENT (TIMES N-C-R (PLUS (DIFFERENCE N I) 1)) I))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN N-C-R))))) 
(PUT 'FIND-ALPHAS-IN-A-RING 'NUMBER-OF-ARGS 4) 
(PUT 'FIND-ALPHAS-IN-A-RING 'DEFINED-ON-LINE '256) 
(PUT 'FIND-ALPHAS-IN-A-RING 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'FIND-ALPHAS-IN-A-RING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-ALPHAS-IN-A-RING (N MFLIST FHATLIST GAMMA)
    (PROG (GG M PPOW I GG-MOD-P MODFLIST WVEC ALPHA ALPHAZEROS W)
      (COND
       ((NULL PRIME-BASE)
        (ERRORF
         (LIST "Prime base not set for finding alphas" CURRENT-MODULUS N
               MFLIST))))
      (SETQ M (SET-MODULUS PRIME-BASE))
      (SETQ MODFLIST
              (COND ((EQUAL M PRIME-BASE) MFLIST)
                    (T
                     (PROG (FTHING FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FTHING MFLIST)
                       (COND ((NULL FTHING) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FTHING)
                                           (REDUCE-MOD-P FTHING))
                                         (CAR FTHING))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FTHING (CDR FTHING))
                       (COND ((NULL FTHING) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FTHING) (REDUCE-MOD-P FTHING))
                                 (CAR FTHING))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ ALPHALIST (ALPHAS N MODFLIST GAMMA))
      (COND ((EQUAL M PRIME-BASE) (PROGN (SET-MODULUS M) (RETURN ALPHALIST))))
      (SETQ I 0)
      (SETQ ALPHAZEROS (MKVECT N))
      (SETQ WVEC (MKVECT N))
      (PROG (MODFTHING)
        (SETQ MODFTHING MODFLIST)
       LAB
        (COND ((NULL MODFTHING) (RETURN NIL)))
        ((LAMBDA (MODFTHING)
           (PROGN
            (PUTV MODFVEC (SETQ I (IADD1 I)) MODFTHING)
            (PUTV ALPHAVEC I (SETQ ALPHA (CDR (GET-ALPHA MODFTHING))))
            (PUTV ALPHAZEROS I ALPHA)
            (PUTV WVEC I ALPHA)
            (PUTV FHATVEC I (CAR FHATLIST))
            (SETQ FHATLIST (CDR FHATLIST))))
         (CAR MODFTHING))
        (SETQ MODFTHING (CDR MODFTHING))
        (GO LAB))
      (SETQ GG GAMMA)
      (SETQ PPOW PRIME-BASE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP PPOW M)) (RETURN NIL)))
        (PROGN
         (SET-MODULUS M)
         (SETQ GG
                 (QUOTFAIL
                  (DIFFERENCE-MOD-P GG
                   (FORM-SUM-AND-PRODUCT-MOD-M WVEC FHATVEC N))
                  PRIME-BASE))
         (SET-MODULUS PRIME-BASE)
         (SETQ GG-MOD-P (REDUCE-MOD-P GG))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
           (PROGN
            (PUTV WVEC K
                  (SETQ W
                          (REMAINDER-MOD-P
                           (TIMES-MOD-P (GETV ALPHAZEROS K) GG-MOD-P)
                           (GETV MODFVEC K))))
            (PUTV ALPHAVEC K
                  (ADDF (GETV ALPHAVEC K)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF W PPOW))
                              (T (POLY-MULTF W PPOW))))))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (SETQ PPOW (TIMES PPOW PRIME-BASE)))
        (GO WHILELABEL))
      (SET-MODULUS M)
      (SETQ I 0)
      (RETURN
       (PROG (FTHING FORALL-RESULT FORALL-ENDPTR)
         (SETQ FTHING MFLIST)
         (COND ((NULL FTHING) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (FTHING)
                             (CONS FTHING (GETV ALPHAVEC (SETQ I (IADD1 I)))))
                           (CAR FTHING))
                          NIL)))
        LOOPLABEL
         (SETQ FTHING (CDR FTHING))
         (COND ((NULL FTHING) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (FTHING)
                     (CONS FTHING (GETV ALPHAVEC (SETQ I (IADD1 I)))))
                   (CAR FTHING))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'ALPHAS 'NUMBER-OF-ARGS 3) 
(PUT 'ALPHAS 'DEFINED-ON-LINE '301) 
(PUT 'ALPHAS 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'ALPHAS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ALPHAS (N FLIST GAMMA)
    (COND ((EQUAL N 1) (LIST (CONS (CAR FLIST) GAMMA)))
          (T
           (PROG (K W F1 F2 I GAMMA1 GAMMA2)
             (SETQ K (QUOTIENT N 2))
             (SETQ F1 1)
             (SETQ F2 1)
             (SETQ I 1)
             (PROG (F)
               (SETQ F FLIST)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ F2 (TIMES-MOD-P F F2)))
                         (T (SETQ F1 (TIMES-MOD-P F F1))))
                   (SETQ I (PLUS I 1))))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ W (XGCD-MOD-P F1 F2 1 POLYZERO POLYZERO 1))
             (COND ((ATOM W) (RETURN '|FACTORS NOT COPRIME|)))
             (SETQ GAMMA1 (REMAINDER-MOD-P (TIMES-MOD-P (CDR W) GAMMA) F1))
             (SETQ GAMMA2 (REMAINDER-MOD-P (TIMES-MOD-P (CAR W) GAMMA) F2))
             (SETQ I 1)
             (SETQ F1 NIL)
             (SETQ F2 NIL)
             (PROG (F)
               (SETQ F FLIST)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ F2 (CONS F F2)))
                         (T (SETQ F1 (CONS F F1))))
                   (SETQ I (PLUS I 1))))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (RETURN
              (APPEND (ALPHAS K F1 GAMMA1)
                      (ALPHAS (DIFFERENCE N K) F2 GAMMA2))))))) 
(PUT 'XGCD-MOD-P 'NUMBER-OF-ARGS 6) 
(PUT 'XGCD-MOD-P 'DEFINED-ON-LINE '334) 
(PUT 'XGCD-MOD-P 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'XGCD-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE XGCD-MOD-P (A B X1 Y1 X2 Y2)
    (COND ((NULL B) NIL)
          ((OR (ATOM B) (ATOM (CAR B)))
           (PROG ()
             (SETQ B (MODULAR-RECIPROCAL B))
             (SETQ X2 (MULTIPLY-BY-CONSTANT-MOD-P X2 B))
             (SETQ Y2 (MULTIPLY-BY-CONSTANT-MOD-P Y2 B))
             (RETURN (CONS X2 Y2))))
          (T
           (PROG (Q)
             (SETQ Q (QUOTIENT-MOD-P A B))
             (RETURN
              (XGCD-MOD-P B (DIFFERENCE-MOD-P A (TIMES-MOD-P B Q)) X2 Y2
               (DIFFERENCE-MOD-P X1 (TIMES-MOD-P X2 Q))
               (DIFFERENCE-MOD-P Y1 (TIMES-MOD-P Y2 Q)))))))) 
(PUT 'HENSEL-MOD-P 'NUMBER-OF-ARGS 6) 
(PUT 'HENSEL-MOD-P 'DEFINED-ON-LINE '351) 
(PUT 'HENSEL-MOD-P 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'HENSEL-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HENSEL-MOD-P (POLY MVEC FVEC CBD VSET P)
    (PROG (W U0 DELFVEC OLD.MOD RES M)
      (SETQ U0 (INITIALIZE-HENSEL NUMBER-OF-FACTORS P POLY MVEC FVEC CBD))
      (SETQ M P)
      (SETQ OLD.MOD (SET-MODULUS NIL))
      (COND
       ((EQUAL NUMBER-OF-FACTORS 1)
        (PROGN
         (PUTV FVEC 1 (SETQ CURRENT-FACTOR-PRODUCT POLY))
         (RETURN (HENSEL-EXIT M OLD.MOD P VSET W)))))
      (HENSEL-MSG1 P U0)
      (SETQ OLD.MOD (SET-MODULUS P))
      (SETQ RES (ADDF HENSEL-POLY (NEGF U0)))
      (SETQ M P)
     A
      (COND ((NULL RES) (RETURN (HENSEL-EXIT M OLD.MOD P VSET W))))
      (COND
       ((GREATERP (QUOTIENT M 2) COEFFTBD)
        (PROGN
         (COND
          (*OVERSHOOT
           (PROGN
            (PRIN2 (COND ((NULL VSET) "Univariate ") (T "Multivariate ")))
            (PRIN2T "coefft bound overshoot"))))
         (COND
          ((NOT *OVERVIEW)
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PRIN2* "We have overshot the coefficient bound")
                 (TERPRI* NIL))
                (WRS STREAM)))))))
         (RETURN (HENSEL-EXIT M OLD.MOD P VSET 'OVERSHOT)))))
      (SETQ RES (QUOTFAIL RES DELTAM))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PRIN2* "Residue divided by ")
              (PRIN2* M)
              (PRIN2* " is ")
              (PRINTSF RES))
             (WRS STREAM)))))))
      (COND
       ((AND (NOT *LINEAR) (NULL VSET) (LEQ M LARGEST-SMALL-MODULUS)
             (GREATERP M P))
        (QUADRATIC-STEP M NUMBER-OF-FACTORS)))
      (SETQ W (REDUCE-MOD-P RES))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PRIN2* "Next term in residue to kill is:")
              (PRINSF W)
              (PRIN2* " which is of size ")
              (PRINTSF (TIMES DELTAM M))
              NIL)
             (WRS STREAM)))))))
      (SOLVE-FOR-CORRECTIONS W FHATVEC MODFVEC DELFVEC VSET)
      (MAKE-VEC-MODULAR-SYMMETRIC DELFVEC NUMBER-OF-FACTORS)
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PROGN (PRIN2* "Correction terms are:") (TERPRI* NIL))
              (SETQ W 1)
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                (PROGN
                 (PRIN2* "  To f(")
                 (PRIN2* W)
                 (PRIN2* "): ")
                 (PRINTSF
                  ((LAMBDA (G637)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF M G637))
                           (T (POLY-MULTF M G637))))
                   (GETV DELFVEC I)))
                 (SETQ W (IADD1 W)))
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              NIL)
             (WRS STREAM)))))))
      (SETQ W (TERMS-DONE FACTORVEC DELFVEC M))
      (SETQ RES (ADDF RES (NEGF W)))
      (SETQ CURRENT-FACTOR-PRODUCT
              (ADDF CURRENT-FACTOR-PRODUCT
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF M W))
                          (T (POLY-MULTF M W)))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PUTV FACTORVEC I
              (ADDF (GETV FACTORVEC I)
                    ((LAMBDA (G638)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G638 M))
                             (T (POLY-MULTF G638 M))))
                     (GETV DELFVEC I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PROGN (PRIN2* "   giving new factors as:") (TERPRI* NIL))
              (SETQ W 1)
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                (PROGN
                 (PRIN2* " f(")
                 (PRIN2* W)
                 (PRIN2* ")=")
                 (PRINTSF (GETV FACTORVEC I))
                 (SETQ W (IADD1 W)))
                (SETQ I (PLUS2 I 1))
                (GO LAB)))
             (WRS STREAM)))))))
      (SETQ M (TIMES M DELTAM))
      (COND
       ((AND (NOT (NULL RES)) (NULL VSET) (NOT RECONSTRUCTING-GCD))
        (PROG (J U FAC)
          (SETQ J 0)
          (PROG ()
           WHILELABEL
            (COND
             ((NOT (LEQ (SETQ J (IPLUS2 J 1)) NUMBER-OF-FACTORS))
              (RETURN NIL)))
            (COND
             ((NOT
               (NULL
                (SETQ U
                        ((LAMBDA (*EXP)
                           (QUOTF1 HENSEL-POLY (SETQ FAC (GETV FACTORVEC J))))
                         T))))
              (PROGN
               (SETQ HENSEL-POLY U)
               (SETQ RES (ADJUST-GROWTH FAC J M))
               (SETQ J NUMBER-OF-FACTORS))))
            (GO WHILELABEL)))))
      (GO A))) 
(PUT 'HENSEL-EXIT 'NUMBER-OF-ARGS 5) 
(PUT 'HENSEL-EXIT 'DEFINED-ON-LINE '457) 
(PUT 'HENSEL-EXIT 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'HENSEL-EXIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HENSEL-EXIT (M OLD.MOD P VSET W)
    (PROG ()
      (COND
       (FACTORS-DONE
        (PROGN
         (COND ((NOT (EQUAL W 'OVERSHOT)) (SETQ M (TIMES P P))))
         (SET-HENSEL-FLUIDS-BACK P))))
      (COND
       ((AND (NOT (EQUAL W 'OVERSHOT)) (NULL VSET) (NOT *LINEAR)
             MULTIVARIATE-INPUT-POLY)
        (PROG ()
         WHILELABEL
          (COND ((NOT (LESSP M LARGEST-SMALL-MODULUS)) (RETURN NIL)))
          (PROGN
           (COND ((NOT (EQUAL M DELTAM)) (QUADRATIC-STEP M NUMBER-OF-FACTORS)))
           (SETQ M (TIMES M DELTAM)))
          (GO WHILELABEL))))
      (SET-MODULUS OLD.MOD)
      (SETQ HENSEL-GROWTH-SIZE DELTAM)
      (PUTV FACTORVEC 0 NUMBER-OF-FACTORS)
      (RETURN
       (COND ((EQUAL W 'OVERSHOT) (LIST 'OVERSHOT M FACTORVEC))
             (T (CONS 'OK FACTORVEC)))))) 
(PUT 'HENSEL-MSG1 'NUMBER-OF-ARGS 2) 
(PUT 'HENSEL-MSG1 'DEFINED-ON-LINE '478) 
(PUT 'HENSEL-MSG1 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'HENSEL-MSG1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HENSEL-MSG1 (P U0)
    (PROG (W)
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROGN
            (PROGN
             (PRIN2*
              "We are now ready to use the Hensel construction to grow ")
             (TERPRI* NIL))
            (PRIN2* "in powers of ")
            (PROGN (PRIN2* CURRENT-MODULUS) (TERPRI* NIL))
            (COND
             ((NOT *OVERVIEW)
              (PROGN
               (PRIN2* "Polynomial to factor (=U): ")
               (PRINTSF HENSEL-POLY))))
            (PRIN2* "Initial factors mod ")
            (PRIN2* P)
            (PROGN (PRIN2* " with some correct coefficients:") (TERPRI* NIL))
            (SETQ W 1)
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
              (PROGN
               (PRIN2* " f(")
               (PRIN2* W)
               (PRIN2* ")=")
               (PRINTSF (GETV FACTORVEC I))
               (SETQ W (IADD1 W)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND
             ((NOT *OVERVIEW)
              (PROGN
               (PRIN2* "Coefficient bound = ")
               (PRIN2* COEFFTBD)
               (TERPRI* NIL)
               (PRIN2* "The product of factors over the integers is ")
               (PRINTSF U0)
               (PROGN
                (PRIN2*
                 "In each step below, the residue is U - (product of the ")
                (TERPRI* NIL))
               (PROGN
                (PRIN2*
                 "factors as far as we know them). The correction to each ")
                (TERPRI* NIL))
               (PROGN
                (PRIN2* "factor, f(i), is (a(i)*v) mod f0(i) where f0(i) is ")
                (TERPRI* NIL))
               (PRIN2* "f(i) mod ")
               (PRIN2* P)
               (PROGN
                (PRIN2* "(ie. the f(i) used in calculating the a(i))")
                (TERPRI* NIL))))))
           (WRS STREAM))))))) 
(PUT 'INITIALIZE-HENSEL 'NUMBER-OF-ARGS 6) 
(PUT 'INITIALIZE-HENSEL 'DEFINED-ON-LINE '505) 
(PUT 'INITIALIZE-HENSEL 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'INITIALIZE-HENSEL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INITIALIZE-HENSEL (R P POLY MVEC FVEC CBD)
    (PROG (U0)
      (SETQ DELFVEC (MKVECT R))
      (SETQ FACVEC (MKVECT R))
      (SETQ HENSEL-POLY POLY)
      (SETQ MODFVEC MVEC)
      (SETQ FACTORVEC FVEC)
      (SETQ COEFFTBD CBD)
      (SETQ FACTORS-DONE NIL)
      (SETQ DELTAM P)
      (SETQ U0 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (SETQ U0
                ((LAMBDA (G640)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G640 U0))
                         (T (POLY-MULTF G640 U0))))
                 (GETV FACTORVEC I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ CURRENT-FACTOR-PRODUCT U0)
      (RETURN U0))) 
(PUT 'RESET-QUADRATIC-STEP-FLUIDS 'NUMBER-OF-ARGS 3) 
(PUT 'RESET-QUADRATIC-STEP-FLUIDS 'DEFINED-ON-LINE '541) 
(PUT 'RESET-QUADRATIC-STEP-FLUIDS 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'RESET-QUADRATIC-STEP-FLUIDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESET-QUADRATIC-STEP-FLUIDS (POLY FACLIST N)
    (PROG (I OM FACPAIRLIST CFP-MOD-P FHATLIST)
      (SETQ CURRENT-FACTOR-PRODUCT POLY)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ CFP-MOD-P (REDUCE-MOD-P CURRENT-FACTOR-PRODUCT))
      (SETQ I 0)
      (SETQ FACPAIRLIST
              (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                (SETQ FAC FACLIST)
                (COND ((NULL FAC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FAC)
                                    (PROGN
                                     (SETQ I (IPLUS2 I 1))
                                     (CONS FAC (REDUCE-MOD-P FAC))))
                                  (CAR FAC))
                                 NIL)))
               LOOPLABEL
                (SETQ FAC (CDR FAC))
                (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FAC)
                            (PROGN
                             (SETQ I (IPLUS2 I 1))
                             (CONS FAC (REDUCE-MOD-P FAC))))
                          (CAR FAC))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FHATLIST
              (PROG (FACC FORALL-RESULT FORALL-ENDPTR)
                (SETQ FACC FACPAIRLIST)
                (COND ((NULL FACC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FACC)
                                    (QUOTFAIL-MOD-P CFP-MOD-P (CDR FACC)))
                                  (CAR FACC))
                                 NIL)))
               LOOPLABEL
                (SETQ FACC (CDR FACC))
                (COND ((NULL FACC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FACC) (QUOTFAIL-MOD-P CFP-MOD-P (CDR FACC)))
                          (CAR FACC))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (FACTORS-DONE
        (SETQ ALPHALIST
                (FIND-ALPHAS-IN-A-RING I
                 (PROG (FACPR FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FACPR FACPAIRLIST)
                   (COND ((NULL FACPR) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (FACPR) (CDR FACPR)) (CAR FACPR))
                                    NIL)))
                  LOOPLABEL
                   (SETQ FACPR (CDR FACPR))
                   (COND ((NULL FACPR) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (FACPR) (CDR FACPR)) (CAR FACPR))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 FHATLIST 1))))
      (SETQ I 0)
      (PROG (FACPAIR)
        (SETQ FACPAIR FACPAIRLIST)
       LAB
        (COND ((NULL FACPAIR) (RETURN NIL)))
        ((LAMBDA (FACPAIR)
           (PROGN
            (PUTV FACTORVEC (SETQ I (IADD1 I)) (CAR FACPAIR))
            (PUTV MODFVEC I (CDR FACPAIR))
            (PUTV ALPHAVEC I (CDR (GET-ALPHA (CDR FACPAIR))))))
         (CAR FACPAIR))
        (SETQ FACPAIR (CDR FACPAIR))
        (GO LAB))
      (SET-MODULUS OM))) 
(PUT 'QUADRATIC-STEP 'NUMBER-OF-ARGS 2) 
(PUT 'QUADRATIC-STEP 'DEFINED-ON-LINE '573) 
(PUT 'QUADRATIC-STEP 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'QUADRATIC-STEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUADRATIC-STEP (M R)
    (PROG (W S CFP-MOD-P)
      (SET-MODULUS M)
      (SETQ CFP-MOD-P (REDUCE-MOD-P CURRENT-FACTOR-PRODUCT))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PUTV FACVEC I (REDUCE-MOD-P (GETV FACTORVEC I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PUTV FHATVEC I (QUOTFAIL-MOD-P CFP-MOD-P (GETV FACVEC I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ W (FORM-SUM-AND-PRODUCT-MOD-M ALPHAVEC FHATVEC R))
      (SETQ W (PLUS-MOD-P 1 (MINUS-MOD-P W)))
      (SETQ S (QUOTFAIL W DELTAM))
      (SET-MODULUS DELTAM)
      (SETQ S S)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ W
                 (REMAINDER-MOD-P (TIMES-MOD-P S (GETV ALPHAVEC I))
                  (GETV MODFVEC I)))
         (PUTV ALPHAVEC I
               (ADDF (GETV ALPHAVEC I)
                     ((LAMBDA (G643)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G643))
                              (T (POLY-MULTF W G643))))
                      DELTAM))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ S MODFVEC)
      (SETQ MODFVEC FACVEC)
      (SETQ FACVEC S)
      (SETQ DELTAM M)
      (SET-MODULUS DELTAM)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN (PUTV FACVEC I "RUBBISH") (PUTV ALPHAVEC I (GETV ALPHAVEC I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PROGN
               (PRIN2* "The new modular polynomials are chosen such that:")
               (TERPRI* NIL))
              (TERPRI)
              (PRIN2* "   a(1)*h(1) + ... + a(")
              (PRIN2* R)
              (PRIN2* ")*h(")
              (PRIN2* R)
              (PRIN2* ") = 1 mod ")
              (PROGN (PRIN2* M) (TERPRI* NIL))
              (TERPRI)
              (PROGN
               (PRIN2* "  where h(i)=(product of all f(j) [see below])/f(i)")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "    and degree of a(i) < degree of f(i).")
               (TERPRI* NIL))
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
                (PROGN
                 (PRIN2* "  a(")
                 (PRIN2* I)
                 (PRIN2* ")=")
                 (PRINTSF (GETV ALPHAVEC I))
                 (PRIN2* "   f(")
                 (PRIN2* I)
                 (PRIN2* ")=")
                 (PRINTSF (GETV MODFVEC I)))
                (SETQ I (PLUS2 I 1))
                (GO LAB)))
             (WRS STREAM))))))))) 
(PUT 'TERMS-DONE 'NUMBER-OF-ARGS 3) 
(PUT 'TERMS-DONE 'DEFINED-ON-LINE '624) 
(PUT 'TERMS-DONE 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'TERMS-DONE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMS-DONE (FVEC DELFVEC M)
    (PROG (FLIST DELFLIST)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ FLIST (CONS (GETV FVEC I) FLIST))
         (SETQ DELFLIST (CONS (GETV DELFVEC I) DELFLIST)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN
       (TERMS.DONE NUMBER-OF-FACTORS FLIST DELFLIST NUMBER-OF-FACTORS M)))) 
(PUT 'TERMS.DONE 'NUMBER-OF-ARGS 5) 
(PUT 'TERMS.DONE 'DEFINED-ON-LINE '633) 
(PUT 'TERMS.DONE 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'TERMS.DONE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMS.DONE (N FLIST DELFLIST R M)
    (COND ((EQUAL N 1) (CONS (CAR FLIST) (CAR DELFLIST)))
          (T
           (PROG (K I F1 F2 DELF1 DELF2)
             (SETQ K (QUOTIENT N 2))
             (SETQ I 1)
             (PROG (F)
               (SETQ F FLIST)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ F2 (CONS F F2)))
                         (T (SETQ F1 (CONS F F1))))
                   (SETQ I (PLUS I 1))))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ I 1)
             (PROG (DELF)
               (SETQ DELF DELFLIST)
              LAB
               (COND ((NULL DELF) (RETURN NIL)))
               ((LAMBDA (DELF)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ DELF2 (CONS DELF DELF2)))
                         (T (SETQ DELF1 (CONS DELF DELF1))))
                   (SETQ I (PLUS I 1))))
                (CAR DELF))
               (SETQ DELF (CDR DELF))
               (GO LAB))
             (SETQ F1 (TERMS.DONE K F1 DELF1 R M))
             (SETQ DELF1 (CDR F1))
             (SETQ F1 (CAR F1))
             (SETQ F2 (TERMS.DONE (DIFFERENCE N K) F2 DELF2 R M))
             (SETQ DELF2 (CDR F2))
             (SETQ F2 (CAR F2))
             (SETQ DELF1
                     (ADDF
                      (ADDF
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF F1 DELF2))
                             (T (POLY-MULTF F1 DELF2)))
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF F2 DELF1))
                             (T (POLY-MULTF F2 DELF1))))
                      ((LAMBDA (G644)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G644 DELF2))
                               (T (POLY-MULTF G644 DELF2))))
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF DELF1 M))
                             (T (POLY-MULTF DELF1 M))))))
             (COND ((EQUAL N R) (RETURN DELF1)))
             (RETURN
              (CONS
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF F1 F2))
                     (T (POLY-MULTF F1 F2)))
               DELF1)))))) 
(PUT 'TRY.COMBINING 'NUMBER-OF-ARGS 4) 
(PUT 'TRY.COMBINING 'DEFINED-ON-LINE '659) 
(PUT 'TRY.COMBINING 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'TRY.COMBINING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRY.COMBINING (L POLY M SOFAR) (TRY.COMBINING1 L POLY M SOFAR 2)) 
(PUT 'TRY.COMBINING1 'NUMBER-OF-ARGS 5) 
(PUT 'TRY.COMBINING1 'DEFINED-ON-LINE '670) 
(PUT 'TRY.COMBINING1 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'TRY.COMBINING1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRY.COMBINING1 (L POLY M SOFAR K)
    (COND
     ((EQUAL POLY 1)
      (COND ((NULL L) SOFAR) (T (ERRORF (LIST "too many bad factors:" L)))))
     (T
      (PROG (N RES FF V W W1 COMBINED.FACTORS LL LCFINV OLDMOD)
        (SETQ N (LENGTH L))
        (COND
         ((EQUAL N 1)
          (COND
           ((GREATERP (CDAAR (CAR L)) (QUOTIENT (CDAAR POLY) 2))
            (RETURN (CONS '|ONE BAD FACTOR| SOFAR)))
           (T (ERRORF (LIST "one bad factor does not fit:" L))))))
        (COND
         ((OR (EQUAL N 2) (EQUAL N 3))
          (PROGN
           (SETQ W (CDAR (CDAR L)))
           (PROG ()
            WHILELABEL
             (COND ((NOT (NOT (EQUAL W (CDAR POLY)))) (RETURN NIL)))
             (SETQ POLY (QUOTFAIL POLY W))
             (GO WHILELABEL))
           (COND
            ((NOT *OVERVIEW)
             (PROG (STREAM)
               (COND
                ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                 (SETQ STREAM (CONS NIL NIL)))
                (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
               (COND
                (STREAM
                 (PROGN
                  (SETQ STREAM (WRS (CDR STREAM)))
                  (PROGN
                   (PROGN (PRIN2* "We combine:") (TERPRI* NIL))
                   (PROG (LF)
                     (SETQ LF L)
                    LAB
                     (COND ((NULL LF) (RETURN NIL)))
                     ((LAMBDA (LF) (PRINTSF (CDR LF))) (CAR LF))
                     (SETQ LF (CDR LF))
                     (GO LAB))
                   (PRIN2* " mod ")
                   (PRIN2* M)
                   (PROGN (PRIN2* " to give correct factor:") (TERPRI* NIL))
                   (PRINTSF POLY))
                  (WRS STREAM)))))))
           (COMBINE.ALPHAS L T)
           (RETURN (CONS POLY SOFAR)))))
        (SETQ LL
                (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FF L)
                  (COND ((NULL FF) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FF) (CONS (CDR FF) (CAR FF)))
                                    (CAR FF))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FF (CDR FF))
                  (COND ((NULL FF) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (FF) (CONS (CDR FF) (CAR FF))) (CAR FF))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ OLDMOD (SET-GENERAL-MODULUS M))
        (SETQ LCFINV (GENERAL-MODULAR-RECIPROCAL (CDAR (CDAR L))))
        (SET-GENERAL-MODULUS OLDMOD)
       LOOP1
        (COND ((GREATERP K (QUOTIENT N 2)) (GO EXIT)))
        (SETQ W (KOUTOF K (COND ((EQUAL (TIMES 2 K) N) (CDR L)) (T L)) NIL))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (AND W
                  (EQUAL (SETQ V (FACTOR-TRIALDIV POLY (CAR W) M LL LCFINV))
                         'DIDNTGO)))
            (RETURN NIL)))
          (PROGN
           (SETQ W (CDR W))
           (PROG ()
            WHILELABEL
             (COND
              ((NOT
                (AND W
                     (OR (EQUAL (CAR W) '*LAZYADJOIN)
                         (EQUAL (CAR W) '*LAZYKOUTOF))))
               (RETURN NIL)))
             (COND
              ((EQUAL (CAR W) '*LAZYADJOIN)
               (SETQ W (LAZY-ADJOIN (CADR W) (CADDR W) (CADR (CDDR W)))))
              (T (SETQ W (KOUTOF (CADR W) (CADDR W) (CADR (CDDR W))))))
             (GO WHILELABEL)))
          (GO WHILELABEL))
        (COND
         ((NOT (EQUAL V 'DIDNTGO))
          (PROGN
           (SETQ FF (CAR V))
           (SETQ V (CDR V))
           (COND
            ((NOT *OVERVIEW)
             (PROG (STREAM)
               (COND
                ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                 (SETQ STREAM (CONS NIL NIL)))
                (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
               (COND
                (STREAM
                 (PROGN
                  (SETQ STREAM (WRS (CDR STREAM)))
                  (PROGN
                   (PROGN (PRIN2* "We combine:") (TERPRI* NIL))
                   (PROG (A)
                     (SETQ A (CAR W))
                    LAB
                     (COND ((NULL A) (RETURN NIL)))
                     ((LAMBDA (A) (PRINTSF A)) (CAR A))
                     (SETQ A (CDR A))
                     (GO LAB))
                   (PRIN2* " mod ")
                   (PRIN2* M)
                   (PROGN (PRIN2* " to give correct factor:") (TERPRI* NIL))
                   (PRINTSF FF))
                  (WRS STREAM)))))))
           (PROG (A)
             (SETQ A (CAR W))
            LAB
             (COND ((NULL A) (RETURN NIL)))
             ((LAMBDA (A)
                (PROGN
                 (SETQ W1 L)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (NOT (EQUAL A (CDAR W1)))) (RETURN NIL)))
                   (SETQ W1 (CDR W1))
                   (GO WHILELABEL))
                 (SETQ COMBINED.FACTORS (CONS (CAR W1) COMBINED.FACTORS))
                 (SETQ L (DELETE (CAR W1) L))))
              (CAR A))
             (SETQ A (CDR A))
             (GO LAB))
           (COMBINE.ALPHAS COMBINED.FACTORS T)
           (SETQ RES (TRY.COMBINING1 L V M (CONS FF SOFAR) K))
           (GO EXIT))))
        (SETQ K (PLUS K 1))
        (GO LOOP1)
       EXIT
        (COND (RES (RETURN RES))
              (T
               (PROGN
                (SETQ W (CDAR (CDAR L)))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (NOT (EQUAL W (CDAR POLY)))) (RETURN NIL)))
                  (SETQ POLY (QUOTFAIL POLY W))
                  (GO WHILELABEL))
                (COND
                 ((NOT *OVERVIEW)
                  (PROG (STREAM)
                    (COND
                     ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                      (SETQ STREAM (CONS NIL NIL)))
                     (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                    (COND
                     (STREAM
                      (PROGN
                       (SETQ STREAM (WRS (CDR STREAM)))
                       (PROGN
                        (PROGN (PRIN2* "We combine:") (TERPRI* NIL))
                        (PROG (FF)
                          (SETQ FF L)
                         LAB
                          (COND ((NULL FF) (RETURN NIL)))
                          ((LAMBDA (FF) (PRINTSF (CDR FF))) (CAR FF))
                          (SETQ FF (CDR FF))
                          (GO LAB))
                        (PRIN2* " mod ")
                        (PRIN2* M)
                        (PROGN
                         (PRIN2* " to give correct factor:")
                         (TERPRI* NIL))
                        (PRINTSF POLY))
                       (WRS STREAM)))))))
                (COMBINE.ALPHAS L T)
                (RETURN (CONS POLY SOFAR))))))))) 
(PUT 'KOUTOF 'NUMBER-OF-ARGS 3) 
(PUT 'KOUTOF 'DEFINED-ON-LINE '754) 
(PUT 'KOUTOF 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'KOUTOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE KOUTOF (K L SOFAR)
    (COND
     ((EQUAL K 1)
      (APPEND
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F L)
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (LIST (CDR F))) (CAR F)) NIL)))
        LOOPLABEL
         (SETQ F (CDR F))
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (F) (LIST (CDR F))) (CAR F)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))
       SOFAR))
     ((GREATERP K (LENGTH L)) SOFAR)
     (T
      (PROGN
       (PROG ()
        WHILELABEL
         (COND
          ((NOT (OR (EQCAR L '*LAZYADJOIN) (EQCAR L '*LAZYKOUTOF)))
           (RETURN NIL)))
         (COND
          ((EQUAL (CAR L) '*LAZYADJOIN)
           (SETQ L (LAZY-ADJOIN (CADR L) (CADDR L) (CADR (CDDR L)))))
          (T (SETQ L (KOUTOF (CADR L) (CADDR L) (CADR (CDDR L))))))
         (GO WHILELABEL))
       (COND
        ((EQUAL K (LENGTH L))
         (CONS
          (PROG (LL FORALL-RESULT FORALL-ENDPTR)
            (SETQ LL L)
            (COND ((NULL LL) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (LL) (CDR LL)) (CAR LL)) NIL)))
           LOOPLABEL
            (SETQ LL (CDR LL))
            (COND ((NULL LL) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (LL) (CDR LL)) (CAR LL)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          SOFAR))
        (T
         (KOUTOF K (CDR L)
          (LIST '*LAZYADJOIN (CDAR L)
                (LIST '*LAZYKOUTOF (DIFFERENCE K 1) (CDR L) NIL) SOFAR)))))))) 
(PUT 'LAZY-ADJOIN 'NUMBER-OF-ARGS 3) 
(PUT 'LAZY-ADJOIN 'DEFINED-ON-LINE '775) 
(PUT 'LAZY-ADJOIN 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'LAZY-ADJOIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LAZY-ADJOIN (ITEM L TAIL)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT (OR (EQCAR L '*LAZYADJOIN) (EQCAR L '*LAZYKOUTOF)))
         (RETURN NIL)))
       (COND
        ((EQUAL (CAR L) '*LAZYADJOIN)
         (SETQ L (LAZY-ADJOIN (CADR L) (CADDR L) (CADR (CDDR L)))))
        (T (SETQ L (KOUTOF (CADR L) (CADDR L) (CADR (CDDR L))))))
       (GO WHILELABEL))
     (COND ((NULL L) TAIL)
           (T
            (CONS (CONS ITEM (CAR L))
                  (COND ((NULL (CDR L)) TAIL)
                        (T (LIST '*LAZYADJOIN ITEM (CDR L) TAIL)))))))) 
(PUT 'FACTOR-TRIALDIV 'NUMBER-OF-ARGS 5) 
(PUT 'FACTOR-TRIALDIV 'DEFINED-ON-LINE '787) 
(PUT 'FACTOR-TRIALDIV 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'FACTOR-TRIALDIV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FACTOR-TRIALDIV (POLY FLIST M LLIST LCFINV)
    (COND ((NULL POLY) (ERRORF "Test dividing into zero?"))
          (T
           (PROG (D Q TCPOLY TCOEFF X OLDMOD W POLY1 TRY1)
             (PROG (STREAM)
               (COND
                ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                 (SETQ STREAM (CONS NIL NIL)))
                (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
               (COND
                (STREAM
                 (PROGN
                  (SETQ STREAM (WRS (CDR STREAM)))
                  (PROGN
                   (PRIN2* "We combine factors ")
                   (PROG (FF)
                     (SETQ FF FLIST)
                    LAB
                     (COND ((NULL FF) (RETURN NIL)))
                     ((LAMBDA (FF)
                        (PROGN
                         (SETQ W (ASSOC FF LLIST))
                         (PRIN2* "f(")
                         (PRIN2* (CDR W))
                         (PRIN2* "), ")))
                      (CAR FF))
                     (SETQ FF (CDR FF))
                     (GO LAB))
                   (PRIN2* "and try dividing : "))
                  (WRS STREAM)))))
             (SETQ X (CAAAR POLY))
             (SETQ TCPOLY (TRAILING.COEFFT POLY X))
             (SETQ TCOEFF (TRAILING.COEFFT (CAR FLIST) X))
             (SETQ OLDMOD (SET-GENERAL-MODULUS M))
             (PROG (FAC)
               (SETQ FAC (CDR FLIST))
              LAB
               (COND ((NULL FAC) (RETURN NIL)))
               ((LAMBDA (FAC)
                  (SETQ TCOEFF
                          (GENERAL-MODULAR-TIMES
                           (GENERAL-MODULAR-TIMES TCOEFF LCFINV)
                           (TRAILING.COEFFT FAC X))))
                (CAR FAC))
               (SETQ FAC (CDR FAC))
               (GO LAB))
             (COND
              ((NOT
                (ZEROP
                 (REMAINDER TCPOLY
                            (SETQ W (GENERAL-MAKE-MODULAR-SYMMETRIC TCOEFF)))))
               (PROGN
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN (PRIN2* " it didn't go (tc test)") (TERPRI* NIL))
                     (WRS STREAM)))))
                (SET-GENERAL-MODULUS OLDMOD)
                (RETURN 'DIDNTGO))))
             (SETQ POLY1 (EVAL-AT-1 POLY))
             (SETQ TRY1 (EVAL-AT-1 (CAR FLIST)))
             (PROG (FAC)
               (SETQ FAC (CDR FLIST))
              LAB
               (COND ((NULL FAC) (RETURN NIL)))
               ((LAMBDA (FAC)
                  (SETQ TRY1
                          (GENERAL-MODULAR-TIMES
                           (GENERAL-MODULAR-TIMES TRY1 LCFINV)
                           (EVAL-AT-1 FAC))))
                (CAR FAC))
               (SETQ FAC (CDR FAC))
               (GO LAB))
             (COND
              ((OR (AND (ZEROP TRY1) (NOT (ZEROP POLY1)))
                   (NOT
                    (ZEROP
                     (REMAINDER POLY1 (GENERAL-MAKE-MODULAR-SYMMETRIC TRY1)))))
               (PROGN
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN (PRIN2* " it didn't go (test at 1)") (TERPRI* NIL))
                     (WRS STREAM)))))
                (SET-GENERAL-MODULUS OLDMOD)
                (RETURN 'DIDNTGO))))
             (SET-GENERAL-MODULUS OLDMOD)
             (SETQ D (COMBINE FLIST M LLIST LCFINV))
             (COND
              ((NULL (SETQ Q ((LAMBDA (*EXP) (QUOTF1 POLY (CAR D))) T)))
               (PROGN
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN
                      (PRIN2* " it didn't go (division fail)")
                      (TERPRI* NIL))
                     (WRS STREAM)))))
                (RETURN 'DIDNTGO)))
              (T
               (PROGN
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN (PRIN2* " it worked !") (TERPRI* NIL))
                     (WRS STREAM)))))
                (RETURN
                 (CONS (CAR D) ((LAMBDA (*EXP) (QUOTF1 Q (CDR D))) T)))))))))) 
(PUT 'EVAL-AT-1 'NUMBER-OF-ARGS 1) 
(PUT 'EVAL-AT-1 'DEFINED-ON-LINE '848) 
(PUT 'EVAL-AT-1 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'EVAL-AT-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVAL-AT-1 (F) (COND ((ATOM F) F) (T (PLUS (CDAR F) (EVAL-AT-1 (CDR F)))))) 
(PUT 'COMBINE 'NUMBER-OF-ARGS 4) 
(PUT 'COMBINE 'DEFINED-ON-LINE '853) 
(PUT 'COMBINE 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'COMBINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMBINE (FLIST M L LCFINV)
    (PROG (OM RES LCF LCFPROD)
      (SETQ LCF (CDAR (CAR FLIST)))
      (SETQ LCFPROD 1)
      (COND
       ((GREATERP M LARGEST-SMALL-MODULUS)
        (PROGN
         (SETQ OM (SET-GENERAL-MODULUS M))
         (SETQ RES (GENERAL-REDUCE-MOD-P (CAR FLIST)))
         (PROG (FF)
           (SETQ FF (CDR FLIST))
          LAB
           (COND ((NULL FF) (RETURN NIL)))
           ((LAMBDA (FF)
              (PROGN
               (COND ((NOT (EQUAL LCF (CDAR FF))) (ERRORF "bad lc in flist")))
               (SETQ RES
                       (GENERAL-TIMES-MOD-P
                        (GENERAL-TIMES-MOD-P LCFINV (GENERAL-REDUCE-MOD-P FF))
                        RES))
               (SETQ LCFPROD (TIMES LCFPROD LCF))))
            (CAR FF))
           (SETQ FF (CDR FF))
           (GO LAB))
         (SETQ RES (GENERAL-MAKE-MODULAR-SYMMETRIC RES))
         (SET-MODULUS OM)
         (RETURN (CONS RES LCFPROD))))
       (T
        (PROGN
         (SETQ OM (SET-MODULUS M))
         (SETQ LCFINV (MODULAR-RECIPROCAL LCF))
         (SETQ RES (REDUCE-MOD-P (CAR FLIST)))
         (PROG (FF)
           (SETQ FF (CDR FLIST))
          LAB
           (COND ((NULL FF) (RETURN NIL)))
           ((LAMBDA (FF)
              (PROGN
               (COND ((NOT (EQUAL LCF (CDAR FF))) (ERRORF "bad lc in flist")))
               (SETQ RES
                       (TIMES-MOD-P (TIMES-MOD-P LCFINV (REDUCE-MOD-P FF))
                        RES))
               (SETQ LCFPROD (TIMES LCFPROD LCF))))
            (CAR FF))
           (SETQ FF (CDR FF))
           (GO LAB))
         (SETQ RES (MAKE-MODULAR-SYMMETRIC RES))
         (SET-MODULUS OM)
         (RETURN (CONS RES LCFPROD))))))) 
(PUT 'COMBINE.ALPHAS 'NUMBER-OF-ARGS 2) 
(PUT 'COMBINE.ALPHAS 'DEFINED-ON-LINE '887) 
(PUT 'COMBINE.ALPHAS 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'COMBINE.ALPHAS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMBINE.ALPHAS (FLIST FIXLCS)
    (PROG (F1 A1 FF AA OLDM LCFAC LCFINV SAVEFLIST)
      (SETQ OLDM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ FLIST
              (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                (SETQ FAC FLIST)
                (COND ((NULL FAC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FAC)
                                    (PROGN
                                     (SETQ SAVEFLIST
                                             (CONS (REDUCE-MOD-P (CDR FAC))
                                                   SAVEFLIST))
                                     (CONS (CAR FAC) (CAR SAVEFLIST))))
                                  (CAR FAC))
                                 NIL)))
               LOOPLABEL
                (SETQ FAC (CDR FAC))
                (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FAC)
                            (PROGN
                             (SETQ SAVEFLIST
                                     (CONS (REDUCE-MOD-P (CDR FAC)) SAVEFLIST))
                             (CONS (CAR FAC) (CAR SAVEFLIST))))
                          (CAR FAC))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (FIXLCS
        (PROGN
         (SETQ LCFINV (MODULAR-RECIPROCAL (CDAR (CDAR FLIST))))
         (SETQ LCFAC
                 (MODULAR-EXPT (CDAR (CDAR FLIST)) (SUB1 (LENGTH FLIST))))))
       (T (PROGN (SETQ LCFINV 1) (SETQ LCFAC 1))))
      (SETQ FF (CDAR FLIST))
      (SETQ AA (CDR (GET-ALPHA FF)))
      (SETQ FLIST (CDR FLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT FLIST) (RETURN NIL)))
        (PROGN
         (SETQ F1 (CDAR FLIST))
         (SETQ A1 (CDR (GET-ALPHA F1)))
         (SETQ FLIST (CDR FLIST))
         (SETQ AA (PLUS-MOD-P (TIMES-MOD-P AA F1) (TIMES-MOD-P A1 FF)))
         (SETQ FF (TIMES-MOD-P FF F1)))
        (GO WHILELABEL))
      (PROG (A)
        (SETQ A ALPHALIST)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NOT (MEMBER (CAR A) SAVEFLIST))
             (SETQ FLIST
                     (CONS (CONS (CAR A) (TIMES-MOD-P (CDR A) LCFAC))
                           FLIST)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ ALPHALIST (CONS (CONS (QUOTIENT-MOD-P FF LCFAC) AA) FLIST))
      (SET-MODULUS OLDM))) 
(PUT 'ADJUST-GROWTH 'NUMBER-OF-ARGS 3) 
(PUT 'ADJUST-GROWTH 'DEFINED-ON-LINE '927) 
(PUT 'ADJUST-GROWTH 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'ADJUST-GROWTH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADJUST-GROWTH (FACDONE K M)
    (PROG (W U BOUND-SCALE MODFLIST FACTORLIST FHATLIST MODFDONE B)
      (SETQ FACTORLIST (VEC2LIST-WITHOUT-K FACTORVEC K))
      (SETQ MODFLIST (VEC2LIST-WITHOUT-K MODFVEC K))
      (SETQ FHATLIST (VEC2LIST-WITHOUT-K FHATVEC K))
      (SETQ W NUMBER-OF-FACTORS)
      (SETQ MODFDONE (GETV MODFVEC K))
     TOP
      (SETQ FACTORS-DONE (CONS FACDONE FACTORS-DONE))
      (COND
       ((EQUAL (SETQ NUMBER-OF-FACTORS (IDIFFERENCE NUMBER-OF-FACTORS 1)) 1)
        (PROGN
         (SETQ FACTORS-DONE (CONS HENSEL-POLY FACTORS-DONE))
         (SETQ NUMBER-OF-FACTORS 0)
         (SETQ HENSEL-POLY 1)
         (COND
          ((NOT *OVERVIEW)
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PROGN (PRIN2* "    All factors found:") (TERPRI* NIL))
                 (PROG (FD)
                   (SETQ FD FACTORS-DONE)
                  LAB
                   (COND ((NULL FD) (RETURN NIL)))
                   ((LAMBDA (FD) (PRINTSF FD)) (CAR FD))
                   (SETQ FD (CDR FD))
                   (GO LAB)))
                (WRS STREAM)))))))
         (RETURN POLYZERO))))
      (SETQ FHATLIST
              (PROG (FHAT FORALL-RESULT FORALL-ENDPTR)
                (SETQ FHAT FHATLIST)
                (COND ((NULL FHAT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FHAT)
                                    (QUOTFAIL-MOD-P
                                     (COND ((NULL FHAT) POLYZERO) (T FHAT))
                                     MODFDONE))
                                  (CAR FHAT))
                                 NIL)))
               LOOPLABEL
                (SETQ FHAT (CDR FHAT))
                (COND ((NULL FHAT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FHAT)
                            (QUOTFAIL-MOD-P
                             (COND ((NULL FHAT) POLYZERO) (T FHAT)) MODFDONE))
                          (CAR FHAT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (COMFAC FACDONE))
      (COND
       ((CAR U)
        (ERRORF (LIST "Factor divisible by main variable: " FACDONE (CAR U)))))
      (SETQ FACDONE (QUOTFAIL FACDONE (CDR U)))
      (SETQ BOUND-SCALE (CDR U))
      (COND
       ((NOT (EQUAL (SETQ B (CDAR FACDONE)) 1))
        (PROG (B-INV OLD-M)
          (SETQ HENSEL-POLY (QUOTFAIL HENSEL-POLY (EXPT B NUMBER-OF-FACTORS)))
          (SETQ B-INV (MODULAR-RECIPROCAL (MODULAR-NUMBER B)))
          (SETQ MODFLIST
                  (PROG (MODF FORALL-RESULT FORALL-ENDPTR)
                    (SETQ MODF MODFLIST)
                    (COND ((NULL MODF) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (MODF) (TIMES-MOD-P B-INV MODF))
                                      (CAR MODF))
                                     NIL)))
                   LOOPLABEL
                    (SETQ MODF (CDR MODF))
                    (COND ((NULL MODF) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (MODF) (TIMES-MOD-P B-INV MODF))
                              (CAR MODF))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (COND
           ((GREATERP M LARGEST-SMALL-MODULUS)
            (PROGN
             (SETQ OLD-M (SET-GENERAL-MODULUS M))
             (SETQ FACTORLIST
                     (PROG (FACC FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FACC FACTORLIST)
                       (COND ((NULL FACC) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FACC)
                                           ((LAMBDA (G647 G648)
                                              (COND ((NULL G647) G648)
                                                    (T
                                                     (CONS
                                                      (CONS (CAAR FACC) G647)
                                                      G648))))
                                            (QUOTFAIL (CDAR FACC) B)
                                            (GENERAL-MAKE-MODULAR-SYMMETRIC
                                             (GENERAL-TIMES-MOD-P
                                              (GENERAL-MODULAR-RECIPROCAL
                                               (GENERAL-MODULAR-NUMBER B))
                                              (GENERAL-REDUCE-MOD-P
                                               (CDR FACC))))))
                                         (CAR FACC))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FACC (CDR FACC))
                       (COND ((NULL FACC) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FACC)
                                   ((LAMBDA (G647 G648)
                                      (COND ((NULL G647) G648)
                                            (T
                                             (CONS (CONS (CAAR FACC) G647)
                                                   G648))))
                                    (QUOTFAIL (CDAR FACC) B)
                                    (GENERAL-MAKE-MODULAR-SYMMETRIC
                                     (GENERAL-TIMES-MOD-P
                                      (GENERAL-MODULAR-RECIPROCAL
                                       (GENERAL-MODULAR-NUMBER B))
                                      (GENERAL-REDUCE-MOD-P (CDR FACC))))))
                                 (CAR FACC))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
           (T
            (PROGN
             (SETQ OLD-M (SET-MODULUS M))
             (SETQ FACTORLIST
                     (PROG (FACC FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FACC FACTORLIST)
                       (COND ((NULL FACC) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FACC)
                                           ((LAMBDA (G650 G651)
                                              (COND ((NULL G650) G651)
                                                    (T
                                                     (CONS
                                                      (CONS (CAAR FACC) G650)
                                                      G651))))
                                            (QUOTFAIL (CDAR FACC) B)
                                            (MAKE-MODULAR-SYMMETRIC
                                             (TIMES-MOD-P
                                              (MODULAR-RECIPROCAL
                                               (MODULAR-NUMBER B))
                                              (REDUCE-MOD-P (CDR FACC))))))
                                         (CAR FACC))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FACC (CDR FACC))
                       (COND ((NULL FACC) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FACC)
                                   ((LAMBDA (G650 G651)
                                      (COND ((NULL G650) G651)
                                            (T
                                             (CONS (CONS (CAAR FACC) G650)
                                                   G651))))
                                    (QUOTFAIL (CDAR FACC) B)
                                    (MAKE-MODULAR-SYMMETRIC
                                     (TIMES-MOD-P
                                      (MODULAR-RECIPROCAL (MODULAR-NUMBER B))
                                      (REDUCE-MOD-P (CDR FACC))))))
                                 (CAR FACC))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))
          (SET-MODULUS OLD-M)
          (SETQ FHATLIST
                  (PROG (FHAT FORALL-RESULT FORALL-ENDPTR)
                    (SETQ FHAT FHATLIST)
                    (COND ((NULL FHAT) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (FHAT)
                                        (TIMES-MOD-P
                                         (MODULAR-EXPT B-INV
                                                       (IDIFFERENCE
                                                        NUMBER-OF-FACTORS 1))
                                         FHAT))
                                      (CAR FHAT))
                                     NIL)))
                   LOOPLABEL
                    (SETQ FHAT (CDR FHAT))
                    (COND ((NULL FHAT) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (FHAT)
                                (TIMES-MOD-P
                                 (MODULAR-EXPT B-INV
                                               (IDIFFERENCE NUMBER-OF-FACTORS
                                                            1))
                                 FHAT))
                              (CAR FHAT))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))
     TRY-ANOTHER-FACTOR
      (COND
       ((GREATERP (SETQ W (IDIFFERENCE W 1)) 0)
        (COND
         ((NOT
           (NULL
            (SETQ U
                    ((LAMBDA (*EXP)
                       (QUOTF1 HENSEL-POLY (SETQ FACDONE (CAR FACTORLIST))))
                     T))))
          (PROGN
           (SETQ HENSEL-POLY U)
           (SETQ FACTORLIST (CDR FACTORLIST))
           (SETQ MODFDONE (CAR MODFLIST))
           (SETQ MODFLIST (CDR MODFLIST))
           (SETQ FHATLIST (CDR FHATLIST))
           (GO TOP)))
         (T
          (PROGN
           (SETQ FACTORLIST (APPEND (CDR FACTORLIST) (LIST (CAR FACTORLIST))))
           (SETQ MODFLIST (APPEND (CDR MODFLIST) (LIST (CAR MODFLIST))))
           (SETQ FHATLIST (APPEND (CDR FHATLIST) (LIST (CAR FHATLIST))))
           (GO TRY-ANOTHER-FACTOR))))))
      (SET-FLUIDS-FOR-NEWHENSEL FACTORLIST FHATLIST MODFLIST)
      (SETQ BOUND-SCALE
              (TIMES BOUND-SCALE
                     (GET-COEFFT-BOUND
                      (QUOTFAIL HENSEL-POLY
                       (EXPT BOUND-SCALE (IDIFFERENCE NUMBER-OF-FACTORS 1)))
                      (CDAAR HENSEL-POLY))))
      (COND ((LESSP BOUND-SCALE COEFFTBD) (SETQ COEFFTBD BOUND-SCALE)))
      (SETQ W
              (QUOTFAIL (ADDF HENSEL-POLY (NEGF CURRENT-FACTOR-PRODUCT))
               (QUOTIENT M DELTAM)))
      (COND
       ((NOT *OVERVIEW)
        (PROG (STREAM)
          (COND
           ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
            (SETQ STREAM (CONS NIL NIL)))
           (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
          (COND
           (STREAM
            (PROGN
             (SETQ STREAM (WRS (CDR STREAM)))
             (PROGN
              (PROGN (PRIN2* "    Factors found to be correct:") (TERPRI* NIL))
              (PROG (FD)
                (SETQ FD FACTORS-DONE)
               LAB
                (COND ((NULL FD) (RETURN NIL)))
                ((LAMBDA (FD) (PRINTSF FD)) (CAR FD))
                (SETQ FD (CDR FD))
                (GO LAB))
              (PROGN (PRIN2* "Remaining factors are:") (TERPRI* NIL))
              (EZGCD_PRINTVEC "    f(" NUMBER-OF-FACTORS ") = " FACTORVEC)
              (PRIN2* "New coefficient bound is ")
              (PROGN (PRIN2* COEFFTBD) (TERPRI* NIL))
              (PRIN2* " and the residue is now ")
              (PRINTSF W))
             (WRS STREAM)))))))
      (RETURN W))) 
(PUT 'VEC2LIST-WITHOUT-K 'NUMBER-OF-ARGS 2) 
(PUT 'VEC2LIST-WITHOUT-K 'DEFINED-ON-LINE '1023) 
(PUT 'VEC2LIST-WITHOUT-K 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'VEC2LIST-WITHOUT-K 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VEC2LIST-WITHOUT-K (V K)
    (PROG (W)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (COND ((NOT (EQUAL I K)) (SETQ W (CONS (GETV V I) W))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN W))) 
(PUT 'SET-FLUIDS-FOR-NEWHENSEL 'NUMBER-OF-ARGS 3) 
(PUT 'SET-FLUIDS-FOR-NEWHENSEL 'DEFINED-ON-LINE '1031) 
(PUT 'SET-FLUIDS-FOR-NEWHENSEL 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'SET-FLUIDS-FOR-NEWHENSEL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SET-FLUIDS-FOR-NEWHENSEL (FLIST FHATLIST MODFLIST)
    (PROGN
     (SETQ CURRENT-FACTOR-PRODUCT 1)
     (SETQ ALPHALIST
             (FIND-ALPHAS-IN-A-RING NUMBER-OF-FACTORS MODFLIST FHATLIST 1))
     (PROG (I)
       (SETQ I NUMBER-OF-FACTORS)
      LAB
       (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
       (PROGN
        (PUTV FACTORVEC I (CAR FLIST))
        (PUTV MODFVEC I (CAR MODFLIST))
        (PUTV FHATVEC I (CAR FHATLIST))
        (PUTV ALPHAVEC I (CDR (GET-ALPHA (CAR MODFLIST))))
        (SETQ CURRENT-FACTOR-PRODUCT
                ((LAMBDA (G653)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR FLIST) G653))
                         (T (POLY-MULTF (CAR FLIST) G653))))
                 CURRENT-FACTOR-PRODUCT))
        (SETQ FLIST (CDR FLIST))
        (SETQ MODFLIST (CDR MODFLIST))
        (SETQ FHATLIST (CDR FHATLIST)))
       (SETQ I (PLUS2 I (MINUS 1)))
       (GO LAB)))) 
(PUT 'SET-HENSEL-FLUIDS-BACK 'NUMBER-OF-ARGS 1) 
(PUT 'SET-HENSEL-FLUIDS-BACK 'DEFINED-ON-LINE '1046) 
(PUT 'SET-HENSEL-FLUIDS-BACK 'DEFINED-IN-FILE 'FACTOR/UNIHENS.RED) 
(PUT 'SET-HENSEL-FLUIDS-BACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SET-HENSEL-FLUIDS-BACK (P)
    (PROG (N FD MODFLIST FULLF MODF)
      (SET-MODULUS P)
      (SETQ DELTAM P)
      (SETQ N (IPLUS2 NUMBER-OF-FACTORS (LENGTH (SETQ FD FACTORS-DONE))))
      (SETQ CURRENT-FACTOR-PRODUCT HENSEL-POLY)
      (PROG (I)
        (SETQ I (IPLUS2 NUMBER-OF-FACTORS 1))
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (PUTV FACTORVEC I (SETQ FULLF (CAR FD)))
         (PUTV MODFVEC I (SETQ MODF (REDUCE-MOD-P FULLF)))
         (SETQ CURRENT-FACTOR-PRODUCT
                 ((LAMBDA (G655)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF FULLF G655))
                          (T (POLY-MULTF FULLF G655))))
                  CURRENT-FACTOR-PRODUCT))
         (SETQ MODFLIST (CONS MODF MODFLIST))
         (SETQ FD (CDR FD)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ MODF (REDUCE-MOD-P (GETV MODFVEC I)))
         (SETQ MODFLIST (CONS MODF MODFLIST))
         (PUTV MODFVEC I MODF))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ ALPHALIST (ALPHAS N MODFLIST 1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV ALPHAVEC I (CDR (GET-ALPHA (GETV MODFVEC I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ NUMBER-OF-FACTORS N))) 
(ENDMODULE) 
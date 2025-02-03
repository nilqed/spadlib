(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYREVRT)) 
(EXPORTS (LIST 'TAYLORREVERT)) 
(IMPORTS
 (LIST '*A2K '*Q2A 'INVSQ 'LASTPAIR 'MVAR 'NEQ 'NTH 'NUMR 'OVER 'REVAL 'SIMP*
       'TYPERR '*TAYEXP2Q 'CST-TAYLOR* 'MAKE-CST-COEFFICIENT 'MAKE-TAYLOR*
       'MULTTAYLORSQ 'PREPTAYEXP 'PRUNE-COEFFLIST 'SET-TAYTEMPLATE 'TAYCFPL
       'TAYCFSQ 'TAYCOEFFLIST 'TAYEXP-QUOTIENT '|TAYLOR:| 'TAYMAKECOEFF
       'TAYLOR-KERNEL-SQ-P 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER
       'TAYTPELPOINT 'TAYTPELVARS 'DELETE-NTH 'TAYLOR-ERROR 'ADDTAYLOR
       'INVTAYLOR 'MULTTAYLOR 'NEGTAYLOR 'EXPTTAYRAT 'ENTER-SORTED
       'SMALLEST-INCREMENT)) 
(PUT 'TAYREVERT 'NUMBER-OF-ARGS 3) 
(PUT 'TAYREVERT 'DEFINED-ON-LINE '64) 
(PUT 'TAYREVERT 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYREVERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYREVERT (TAY OKRNL KRNL)
    (PROG (TP CFL X I)
      (SETQ I 0)
      (SETQ CFL (CADR TAY))
      (SETQ TP (CADDR TAY))
      (SETQ X TP)
      (SETQ I 1)
     LOOP
      (COND
       ((MEMBER OKRNL (CAR (CAR X)))
        (PROGN
         (COND
          ((NOT (NULL (CDR (CAR (CAR X)))))
           (PROGN
            (TAYLOR-ERROR 'TAYREVERT
             (LIST "Kernel" OKRNL "appears in homogenous template" (CAR X)))
            (RETURN NIL)))
          (T (GO FOUND)))
         NIL))
       (T
        (PROGN
         (SETQ X (CDR X))
         (SETQ I (PLUS I 1))
         (COND ((NOT (NULL X)) (GO LOOP))))))
      (TAYLOR-ERROR 'TAYREVERT (LIST "Kernel" OKRNL "not found in template"))
      (RETURN NIL)
     FOUND
      (RETURN (TAYREVERT1 TP CFL (CAR X) I OKRNL KRNL)))) 
(PUT 'TAYREVERTREORDER 'NUMBER-OF-ARGS 2) 
(PUT 'TAYREVERTREORDER 'DEFINED-ON-LINE '104) 
(PUT 'TAYREVERTREORDER 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYREVERTREORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYREVERTREORDER (CF I)
    (PROG (CF1 PL SQ)
      (PROG (PP)
        (SETQ PP CF)
       LAB
        (COND ((NULL PP) (RETURN NIL)))
        ((LAMBDA (PP)
           (PROGN
            (SETQ PL (CAR PP))
            (SETQ SQ (CDR PP))
            (SETQ PL (CONS (NTH PL I) (DELETE-NTH PL I)))
            (SETQ CF1 (ENTER-SORTED (CONS PL SQ) CF1))))
         (CAR PP))
        (SETQ PP (CDR PP))
        (GO LAB))
      (RETURN CF1))) 
(PUT 'TAYREVERTFIRSTDEGREECOEFFS 'NUMBER-OF-ARGS 2) 
(PUT 'TAYREVERTFIRSTDEGREECOEFFS 'DEFINED-ON-LINE '120) 
(PUT 'TAYREVERTFIRSTDEGREECOEFFS 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYREVERTFIRSTDEGREECOEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYREVERTFIRSTDEGREECOEFFS (CF N)
    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
      (SETQ EL CF)
     STARTOVER
      (COND ((NULL EL) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (EL)
                 (COND
                  ((AND (EQUAL (CAR (CAR (CAR EL))) N)
                        (NOT (NULL (CAR (CDR EL)))))
                   (LIST (CONS (CDR (CAR EL)) (CDR EL))))
                  (T NIL)))
               (CAR EL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ EL (CDR EL))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL EL) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (EL)
                 (COND
                  ((AND (EQUAL (CAR (CAR (CAR EL))) N)
                        (NOT (NULL (CAR (CDR EL)))))
                   (LIST (CONS (CDR (CAR EL)) (CDR EL))))
                  (T NIL)))
               (CAR EL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ EL (CDR EL))
      (GO LOOPLABEL))) 
(PUT 'TAYREVERT1 'NUMBER-OF-ARGS 6) 
(PUT 'TAYREVERT1 'DEFINED-ON-LINE '129) 
(PUT 'TAYREVERT1 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYREVERT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYREVERT1 (TP CF EL I OKRNL KRNL)
    (PROG (FIRST INCR NEWTP NEWCF NEWPOINT U U-K V W X X1 N EXPO UPPER)
      (SETQ NEWCF
              ((LAMBDA (CFLIS)
                 (PROGN
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT
                       (AND (NOT (NULL CFLIS)) (NULL (CAR (CDR (CAR CFLIS))))))
                      (RETURN NIL)))
                    (SETQ CFLIS (CDR CFLIS))
                    (GO WHILELABEL))
                  CFLIS))
               (TAYREVERTREORDER CF I)))
      (SETQ NEWTP (DELETE-NTH TP I))
      (SETQ N (CAR (CAR (CAR (CAR NEWCF)))))
      (COND
       ((TAYEXP-LESSP N 0) (TAYREVERT1POLE TP CF EL I OKRNL KRNL NEWCF NEWTP)))
      (COND
       ((EQUAL N 0)
        (COND
         ((NOT (NULL NEWTP))
          (PROG (XX)
            (SETQ XX (TAYREVERTFIRSTDEGREECOEFFS NEWCF 0))
            (COND
             ((TAYEXP-GREATERP (LENGTH XX) 1)
              (TAYLOR-ERROR 'TAYREVERT
               "Term with power 0 is a Taylor series")))
            (SETQ XX (CAR XX))
            (PROG (EL)
              (SETQ EL (CAR XX))
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL)
                 (PROG (EL2)
                   (SETQ EL2 EL)
                  LAB
                   (COND ((NULL EL2) (RETURN NIL)))
                   ((LAMBDA (EL2)
                      (COND
                       ((NEQ EL2 0)
                        (TAYLOR-ERROR 'TAYREVERT
                         "Term with power 0 is a Taylor series"))))
                    (CAR EL2))
                   (SETQ EL2 (CDR EL2))
                   (GO LAB)))
               (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))
            (SETQ NEWPOINT (*Q2A1 (CDR XX) *NOSQ))))
         (T
          (PROGN
           (SETQ NEWPOINT (*Q2A1 (CDR (CAR NEWCF)) *NOSQ))
           (SETQ NEWCF
                   ((LAMBDA (CFLIS)
                      (PROGN
                       (PROG ()
                        WHILELABEL
                         (COND
                          ((NOT
                            (AND (NOT (NULL CFLIS))
                                 (NULL (CAR (CDR (CAR CFLIS))))))
                           (RETURN NIL)))
                         (SETQ CFLIS (CDR CFLIS))
                         (GO WHILELABEL))
                       CFLIS))
                    (CDR NEWCF)))
           (SETQ N (CAR (CAR (CAR (CAR NEWCF)))))))))
       (T (SETQ NEWPOINT 0)))
      (SETQ TP (CONS (LIST (LIST KRNL) NEWPOINT (CADDR EL) (CADDDR EL)) NEWTP))
      (SETQ FIRST (TAYEXP-QUOTIENT 1 N))
      (SETQ INCR (CAR (SMALLEST-INCREMENT NEWCF)))
      (SETQ EXPO (TAYEXP-TIMES FIRST INCR))
      (COND
       ((NOT (EQUAL EXPO 1))
        ((LAMBDA (NEWTAY)
           (PROGN
            (SETQ NEWCF (CADR NEWTAY))
            (SETQ TP (CADDR NEWTAY))
            (SETQ NEWTP (CDR TP))
            (SETQ TP
                    (CONS
                     (LIST (CAR (CAR TP))
                           (REVAL1
                            (LIST 'EXPT (CADR (CAR TP)) (PREPTAYEXP EXPO)) T)
                           (TAYEXP-TIMES (CADDR (CAR TP)) EXPO)
                           (TAYEXP-TIMES (CADDDR (CAR TP)) EXPO))
                     NEWTP))))
         (EXPTTAYRAT (LIST 'TAYLOR* NEWCF TP NIL NIL) (*TAYEXP2Q EXPO)))))
      (SETQ UPPER
              (TAYEXP-DIFFERENCE (TAYEXP-QUOTIENT (CADDDR (CAR TP)) INCR) 1))
      (SETQ X (TAYREVERTFIRSTDEGREECOEFFS NEWCF INCR))
      (SETQ X1 (SETQ X (INVTAYLOR (LIST 'TAYLOR* X NEWTP NIL NIL))))
      (SETQ W
              (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                (SETQ PP (CADR X1))
                (COND ((NULL PP) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PP)
                                    (CONS (CONS (LIST EXPO) (CAR PP))
                                          (CDR PP)))
                                  (CAR PP))
                                 NIL)))
               LOOPLABEL
                (SETQ PP (CDR PP))
                (COND ((NULL PP) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PP)
                            (CONS (CONS (LIST EXPO) (CAR PP)) (CDR PP)))
                          (CAR PP))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V (MKVECT UPPER))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE UPPER J)) (RETURN NIL)))
        (PUTV V J
              (MULTTAYLOR X
               (LIST 'TAYLOR*
                     (TAYREVERTFIRSTDEGREECOEFFS NEWCF (TAYEXP-TIMES J INCR))
                     NEWTP NIL NIL)))
        (SETQ J (TAYEXP-PLUS2 J 1))
        (GO LAB))
      (SETQ U (MKVECT UPPER))
      (PUTV U 0
            ((LAMBDA (G205)
               (LIST 'TAYLOR*
                     (LIST
                      (CONS
                       (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                         (SETQ EL NEWTP)
                         (COND ((NULL EL) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (EL)
                                             (NLIST 0 (LENGTH (CAR EL))))
                                           (CAR EL))
                                          NIL)))
                        LOOPLABEL
                         (SETQ EL (CDR EL))
                         (COND ((NULL EL) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                   (CAR EL))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       G205))
                     NEWTP G205 NIL))
             (CONS 1 1)))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE UPPER J)) (RETURN NIL)))
        (PROGN
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND
            ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE (TAYEXP-DIFFERENCE J 2) K))
             (RETURN NIL)))
           (PROG ()
             (SETQ U-K (GETV U K))
             (PROG (L)
               (SETQ L (TAYEXP-DIFFERENCE K 1))
              LAB
               (COND
                ((TAYEXP-MINUSP
                  (TAYEXP-TIMES (TAYEXP-MINUS 1) (TAYEXP-DIFFERENCE 0 L)))
                 (RETURN NIL)))
               (SETQ U-K
                       (ADDTAYLOR U-K
                        (NEGTAYLOR
                         (MULTTAYLOR (GETV U L)
                          (GETV V (TAYEXP-PLUS (TAYEXP-DIFFERENCE K L) 1))))))
               (SETQ L (TAYEXP-PLUS2 L (TAYEXP-MINUS 1)))
               (GO LAB))
             (PUTV U K U-K))
           (SETQ K (TAYEXP-PLUS2 K 1))
           (GO LAB))
         (SETQ U-K (MULTTAYLORSQ (GETV V J) (*TAYEXP2Q J)))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND
            ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE (TAYEXP-DIFFERENCE J 2) K))
             (RETURN NIL)))
           (SETQ U-K
                   (ADDTAYLOR
                    (MULTTAYLORSQ
                     (MULTTAYLOR (GETV U K) (GETV V (TAYEXP-DIFFERENCE J K)))
                     (*TAYEXP2Q (TAYEXP-DIFFERENCE J K)))
                    U-K))
           (SETQ K (TAYEXP-PLUS2 K 1))
           (GO LAB))
         (SETQ U-K (NEGTAYLOR U-K))
         (PUTV U (TAYEXP-DIFFERENCE J 1) U-K)
         (SETQ X1 (MULTTAYLOR X1 X))
         (PROG (PP)
           (SETQ PP
                   (CADR
                    (MULTTAYLOR
                     (MULTTAYLORSQ (GETV U (TAYEXP-DIFFERENCE J 1))
                      (INVSQ (*TAYEXP2Q J)))
                     X1)))
          LAB
           (COND ((NULL PP) (RETURN NIL)))
           ((LAMBDA (PP)
              (SETQ W
                      (ENTER-SORTED
                       (CONS (CONS (LIST (TAYEXP-TIMES J EXPO)) (CAR PP))
                             (CDR PP))
                       W)))
            (CAR PP))
           (SETQ PP (CDR PP))
           (GO LAB))
         NIL)
        (SETQ J (TAYEXP-PLUS2 J 1))
        (GO LAB))
      (SETQ NEWTP (CONS (CAR TP) NEWTP))
      (SETQ W
              (ENTER-SORTED
               (CONS
                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EL NEWTP)
                  (COND ((NULL EL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                    (CAR EL))
                                   NIL)))
                 LOOPLABEL
                  (SETQ EL (CDR EL))
                  (COND ((NULL EL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))
                (SIMP* (CADR EL)))
               W))
      (SETQ W (LIST 'TAYLOR* W NEWTP NIL NIL))
      (RETURN
       (COND ((EQUAL INCR 1) W) (T (EXPTTAYRAT W (INVSQ (*TAYEXP2Q INCR)))))))) 
(PUT 'TAYREVERT1POLE 'NUMBER-OF-ARGS 8) 
(PUT 'TAYREVERT1POLE 'DEFINED-ON-LINE '261) 
(PUT 'TAYREVERT1POLE 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYREVERT1POLE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TAYREVERT1POLE (TP CF EL I OKRNL KRNL NEWCF NEWTP)
    (PROG (X Y)
      (SETQ CF (CADR (INVTAYLOR (LIST 'TAYLOR* CF TP NIL NIL))))
      (SETQ X (TAYREVERT1 TP CF EL I OKRNL KRNL))
      (SETQ Y (CADDR X))
      (COND
       ((NEQ (CADR (CAR Y)) 0)
        (TAYLOR-ERROR 'NOT-IMPLEMENTED "(Taylor series reversion)"))
       (T
        (PROGN
         (RPLACA (CDDR X)
                 (CONS (LIST (LIST KRNL) 'INFINITY (CADDR (CAR Y))) (CDR Y)))
         (RETURN X)))))) 
(PUT 'TAYLORREVERT 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLORREVERT 'DEFINED-ON-LINE '278) 
(PUT 'TAYLORREVERT 'DEFINED-IN-FILE 'TAYLOR/TAYREVRT.RED) 
(PUT 'TAYLORREVERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLORREVERT (U OKRNL NKRNL)
    ((LAMBDA (SQ)
       (COND
        ((NOT (AND (KERNP SQ) (EQCAR (CAAAR (CAR SQ)) 'TAYLOR*)))
         (TYPERR U "Taylor kernel"))
        (T (TAYREVERT (CAAAR (CAR SQ)) (*A2K OKRNL) (*A2K NKRNL)))))
     (SIMP* U))) 
(FLAG '(TAYLORREVERT) 'OPFN) 
(ENDMODULE) 
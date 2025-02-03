(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYUTILS)) 
(EXPORTS
 (LIST 'ADD-DEGREES 'ADD.COMP.TP. 'CHECK-FOR-CST-TAYLOR 'COMP.TP.-P
       'DELETE-SUPERFLUOUS-COEFFS 'ENTER-SORTED 'EXCEEDS-ORDER
       'EXCEEDS-ORDER-VARIANT 'FIND-NON-ZERO 'GET-CST-COEFF 'INV.TP. 'IS-NEG-PL
       'MIN2-ORDER 'MULT.COMP.TP. 'RAT-KERN-POW 'REPLACE-NEXT 'SUBTR-DEGREES
       'SUBTR-TP-ORDER 'TAYDEGREE< 'TAYDEGREE-STRICT<= 'TAYMINCOEFF
       'TAYMINPOWERLIST 'TAYLOR*-CONSTANTP 'TAYLOR*-NZCONSTANTP 'TAYLOR*-ONEP
       'TAYLOR*-ZEROP 'TAYTPMIN2 'TP-GREATERP 'TRUNCATE-COEFFLIST
       'TRUNCATE-TAYLOR*)) 
(IMPORTS
 (LIST 'OVER 'DOMAINP 'EQCAR 'GCDN 'GEQ 'LASTPAIR 'LC 'LDEG 'MKRN 'MVAR 'NEQ
       'NTH 'NUMR 'RED 'REVERSIP 'SIMPRN '*TAY2Q 'GET-DEGREE 'GET-DEGREELIST
       'MAKE-CST-POWERLIST 'MAKE-TAYLOR* 'NZEROLIST 'PRUNE-COEFFLIST 'TAYCFPL
       'TAYCFSQ 'TAYCOEFFLIST 'TAYGETCOEFF 'TAYFLAGS '|TAYLOR:| 'TAYORIG
       'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER 'TAYTPELPOINT 'TAYTPELVARS
       'TPDEGREELIST 'TPNEXTLIST 'CONFUSION 'TAYCOEFFLIST-ZEROP)) 
(PUT 'ADD-DEGREES 'NUMBER-OF-ARGS 2) 
(PUT 'ADD-DEGREES 'DEFINED-ON-LINE '67) 
(PUT 'ADD-DEGREES 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'ADD-DEGREES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD-DEGREES (DL1 DL2)
    (PROG (DL U V W)
      (PROG ()
       WHILELABEL
        (COND ((NOT DL1) (RETURN NIL)))
        (PROGN
         (SETQ U (CAR DL1))
         (SETQ V (CAR DL2))
         (SETQ W NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT U) (RETURN NIL)))
           (PROGN
            (SETQ W (CONS (TAYEXP-PLUS (CAR U) (CAR V)) W))
            (SETQ U (CDR U))
            (SETQ V (CDR V)))
           (GO WHILELABEL))
         (SETQ DL (CONS (REVERSIP W) DL))
         (SETQ DL1 (CDR DL1))
         (SETQ DL2 (CDR DL2)))
        (GO WHILELABEL))
      (RETURN (REVERSIP DL)))) 
(PUT 'SUBTR-DEGREES 'NUMBER-OF-ARGS 2) 
(PUT 'SUBTR-DEGREES 'DEFINED-ON-LINE '87) 
(PUT 'SUBTR-DEGREES 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'SUBTR-DEGREES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBTR-DEGREES (DL1 DL2)
    (PROG (DL U V W)
      (PROG ()
       WHILELABEL
        (COND ((NOT DL1) (RETURN NIL)))
        (PROGN
         (SETQ U (CAR DL1))
         (SETQ V (CAR DL2))
         (SETQ W NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT U) (RETURN NIL)))
           (PROGN
            (SETQ W (CONS (TAYEXP-DIFFERENCE (CAR U) (CAR V)) W))
            (SETQ U (CDR U))
            (SETQ V (CDR V)))
           (GO WHILELABEL))
         (SETQ DL (CONS (REVERSIP W) DL))
         (SETQ DL1 (CDR DL1))
         (SETQ DL2 (CDR DL2)))
        (GO WHILELABEL))
      (RETURN (REVERSIP DL)))) 
(PUT 'FIND-NON-ZERO 'NUMBER-OF-ARGS 1) 
(PUT 'FIND-NON-ZERO 'DEFINED-ON-LINE '107) 
(PUT 'FIND-NON-ZERO 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'FIND-NON-ZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND-NON-ZERO (PL)
    (PROG (U N M)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ N 1)
     LOOP
      (SETQ M 1)
      (SETQ U (CAR PL))
     LOOP2
      (COND ((NOT (EQUAL (CAR U) 0)) (RETURN (CONS N M))))
      (SETQ U (CDR U))
      (SETQ M (PLUS M 1))
      (COND ((NOT (NULL U)) (GO LOOP2)))
      (SETQ PL (CDR PL))
      (SETQ N (PLUS N 1))
      (COND ((NULL PL) (CONFUSION 'FIND-NON-ZERO)))
      (GO LOOP))) 
(DE GET-DENOM (EXPO) (COND ((ATOM EXPO) 1) (T (CDDR EXPO)))) 
(PUT 'GET-DENOM 'NUMBER-OF-ARGS 1) 
(PUT 'GET-DENOM 'DEFINED-ON-LINE '135) 
(PUT 'GET-DENOM 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'GET-DENOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'GET-DENOM 'INLINE
      '(LAMBDA (EXPO) (COND ((ATOM EXPO) 1) (T (CDDR EXPO))))) 
(PUT 'GET-DENOM-L 'NUMBER-OF-ARGS 1) 
(PUT 'GET-DENOM-L 'DEFINED-ON-LINE '138) 
(PUT 'GET-DENOM-L 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'GET-DENOM-L 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-DENOM-L (EXPOL)
    ((LAMBDA (RESULT)
       (PROGN
        (PROG (EL)
          (SETQ EL (CDR EXPOL))
         LAB
          (COND ((NULL EL) (RETURN NIL)))
          ((LAMBDA (EL)
             (SETQ RESULT (LCMN RESULT (COND ((ATOM EL) 1) (T (CDDR EL))))))
           (CAR EL))
          (SETQ EL (CDR EL))
          (GO LAB))
        RESULT))
     (COND ((ATOM (CAR EXPOL)) 1) (T (CDDR (CAR EXPOL)))))) 
(PUT 'GET-DENOM-LL 'NUMBER-OF-ARGS 2) 
(PUT 'GET-DENOM-LL 'DEFINED-ON-LINE '144) 
(PUT 'GET-DENOM-LL 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'GET-DENOM-LL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET-DENOM-LL (DL PL)
    (COND ((NULL DL) NIL)
          (T
           (CONS (LCMN (CAR DL) (GET-DENOM-L (CAR PL)))
                 (GET-DENOM-LL (CDR DL) (CDR PL)))))) 
(PUT 'SMALLEST-INCREMENT 'NUMBER-OF-ARGS 1) 
(PUT 'SMALLEST-INCREMENT 'DEFINED-ON-LINE '149) 
(PUT 'SMALLEST-INCREMENT 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'SMALLEST-INCREMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SMALLEST-INCREMENT (CFL)
    (COND ((NULL CFL) (CONFUSION 'SMALLEST-INCREMENT))
          (T
           (PROG (RESULT)
             (SETQ RESULT
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L (CAR (CAR CFL)))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L) (GET-DENOM-L L)) (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (L) (GET-DENOM-L L)) (CAR L))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (PROG (EL)
               (SETQ EL (CDR CFL))
              LAB
               (COND ((NULL EL) (RETURN NIL)))
               ((LAMBDA (EL) (SETQ RESULT (GET-DENOM-LL RESULT (CAR EL))))
                (CAR EL))
               (SETQ EL (CDR EL))
               (GO LAB))
             (RETURN
              (PROG (N FORALL-RESULT FORALL-ENDPTR)
                (SETQ N RESULT)
                (COND ((NULL N) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (N)
                                    (COND ((EQUAL N 1) N) (T (MKRN 1 N))))
                                  (CAR N))
                                 NIL)))
               LOOPLABEL
                (SETQ N (CDR N))
                (COND ((NULL N) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (N) (COND ((EQUAL N 1) N) (T (MKRN 1 N))))
                          (CAR N))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'COMMON-INCREMENT 'NUMBER-OF-ARGS 2) 
(PUT 'COMMON-INCREMENT 'DEFINED-ON-LINE '159) 
(PUT 'COMMON-INCREMENT 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'COMMON-INCREMENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMMON-INCREMENT (DL1 DL2)
    (PROG (RESULT L)
     LOOP
      (SETQ L
              (LCMN (COND ((ATOM (CAR DL1)) 1) (T (CDDR (CAR DL1))))
                    (COND ((ATOM (CAR DL2)) 1) (T (CDDR (CAR DL2))))))
      (SETQ RESULT (CONS (COND ((EQUAL L 1) L) (T (MKRN 1 L))) RESULT))
      (SETQ DL1 (CDR DL1))
      (SETQ DL2 (CDR DL2))
      (COND ((NOT (NULL DL1)) (GO LOOP))
            ((NOT (NULL DL2)) (CONFUSION 'COMMON-INCREMENT))
            (T (RETURN (REVERSIP RESULT)))))) 
(PUT 'MIN2-ORDER 'NUMBER-OF-ARGS 3) 
(PUT 'MIN2-ORDER 'DEFINED-ON-LINE '171) 
(PUT 'MIN2-ORDER 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'MIN2-ORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MIN2-ORDER (NEXTLIS ORDLIS DL)
    (COND ((NULL NEXTLIS) NIL)
          (T
           (CONS
            ((LAMBDA (DG)
               (COND
                ((TAYEXP-GREATERP DG (CAR ORDLIS))
                 (TAYEXP-MIN2 DG (CAR NEXTLIS)))
                (T (CAR NEXTLIS))))
             (PROG (N FORALL-RESULT)
               (SETQ N (CAR DL))
               (SETQ FORALL-RESULT 0)
              LAB1
               (COND ((NULL N) (RETURN FORALL-RESULT)))
               (SETQ FORALL-RESULT
                       (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
               (SETQ N (CDR N))
               (GO LAB1)))
            (MIN2-ORDER (CDR NEXTLIS) (CDR ORDLIS) (CDR DL)))))) 
(PUT 'REPLACE-NEXT 'NUMBER-OF-ARGS 2) 
(PUT 'REPLACE-NEXT 'DEFINED-ON-LINE '187) 
(PUT 'REPLACE-NEXT 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'REPLACE-NEXT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPLACE-NEXT (TP NL)
    (COND ((NULL TP) NIL)
          (T
           (CONS
            (LIST (CAR (CAR TP)) (CADR (CAR TP)) (CADDR (CAR TP)) (CAR NL))
            (REPLACE-NEXT (CDR TP) (CDR NL)))))) 
(PUT 'COMP.TP.-P 'NUMBER-OF-ARGS 2) 
(PUT 'COMP.TP.-P 'DEFINED-ON-LINE '199) 
(PUT 'COMP.TP.-P 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'COMP.TP.-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMP.TP.-P (U V)
    (PROG ()
      (SETQ U (CADDR U))
      (SETQ V (CADDR V))
      (COND ((NEQ (LENGTH U) (LENGTH V)) (RETURN NIL)))
     LOOP
      (COND
       ((NOT
         (AND (EQUAL (CAR (CAR U)) (CAR (CAR V)))
              (EQUAL (CADR (CAR U)) (CADR (CAR V)))))
        (RETURN NIL)))
      (SETQ U (CDR U))
      (SETQ V (CDR V))
      (COND ((NULL U) (RETURN T)))
      (GO LOOP))) 
(PUT 'ADD.COMP.TP. 'NUMBER-OF-ARGS 2) 
(PUT 'ADD.COMP.TP. 'DEFINED-ON-LINE '217) 
(PUT 'ADD.COMP.TP. 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'ADD.COMP.TP. 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD.COMP.TP. (U V)
    (PROG (W)
      (SETQ U (CADDR U))
      (SETQ V (CADDR V))
      (COND ((NEQ (LENGTH U) (LENGTH V)) (RETURN NIL)))
      (COND ((NULL U) (RETURN (LIST NIL))))
     LOOP
      (COND
       ((NOT
         (AND (EQUAL (CAR (CAR U)) (CAR (CAR V)))
              (EQUAL (CADR (CAR U)) (CADR (CAR V)))))
        (RETURN NIL))
       (T
        (SETQ W
                (CONS
                 (LIST (CAR (CAR U)) (CADR (CAR U))
                       (TAYEXP-MIN2 (CADDR (CAR U)) (CADDR (CAR V)))
                       (TAYEXP-MIN2 (CADDDR (CAR U)) (CADDDR (CAR V))))
                 W))))
      (SETQ U (CDR U))
      (SETQ V (CDR V))
      (COND ((NULL U) (RETURN (LIST (REVERSIP W)))))
      (GO LOOP))) 
(PUT 'TAYMINDEGREEL 'NUMBER-OF-ARGS 2) 
(PUT 'TAYMINDEGREEL 'DEFINED-ON-LINE '245) 
(PUT 'TAYMINDEGREEL 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYMINDEGREEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYMINDEGREEL (PL DL)
    (COND ((NULL PL) NIL)
          (T
           (CONS
            (TAYEXP-MIN2
             (PROG (N FORALL-RESULT)
               (SETQ N (CAR PL))
               (SETQ FORALL-RESULT 0)
              LAB1
               (COND ((NULL N) (RETURN FORALL-RESULT)))
               (SETQ FORALL-RESULT
                       (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
               (SETQ N (CDR N))
               (GO LAB1))
             (CAR DL))
            (TAYMINDEGREEL (CDR PL) (CDR DL)))))) 
(PUT 'GET-MIN-DEGREELIST 'NUMBER-OF-ARGS 1) 
(PUT 'GET-MIN-DEGREELIST 'DEFINED-ON-LINE '251) 
(PUT 'GET-MIN-DEGREELIST 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'GET-MIN-DEGREELIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-MIN-DEGREELIST (CFL)
    (COND ((NULL CFL) (CONFUSION 'GET-MIN-DEGREELIST))
          ((NULL (CDR CFL))
           (PROG (DG FORALL-RESULT FORALL-ENDPTR)
             (SETQ DG (CAR (CAR CFL)))
             (COND ((NULL DG) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (DG)
                                 (PROG (N FORALL-RESULT)
                                   (SETQ N DG)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND ((NULL N) (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (TAYEXP-PLUS
                                            ((LAMBDA (N) N) (CAR N))
                                            FORALL-RESULT))
                                   (SETQ N (CDR N))
                                   (GO LAB1)))
                               (CAR DG))
                              NIL)))
            LOOPLABEL
             (SETQ DG (CDR DG))
             (COND ((NULL DG) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (DG)
                         (PROG (N FORALL-RESULT)
                           (SETQ N DG)
                           (SETQ FORALL-RESULT 0)
                          LAB1
                           (COND ((NULL N) (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N))
                                                FORALL-RESULT))
                           (SETQ N (CDR N))
                           (GO LAB1)))
                       (CAR DG))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          (T (TAYMINDEGREEL (CAR (CAR CFL)) (GET-MIN-DEGREELIST (CDR CFL)))))) 
(PUT 'MULT.COMP.TP. 'NUMBER-OF-ARGS 3) 
(PUT 'MULT.COMP.TP. 'DEFINED-ON-LINE '258) 
(PUT 'MULT.COMP.TP. 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'MULT.COMP.TP. 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULT.COMP.TP. (U V DIV?)
    (PROG (CF1 CF2 NEXT1 NEXT2 ORD1 ORD2 W |#TERMS-1| |#TERMS-NEXT| DL1 DL2
           MINDG)
      (SETQ CF1
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
               (CADR U)))
      (COND ((NULL CF1) (SETQ DL1 (NLIST 0 (LENGTH (CADDR U)))))
            (T (SETQ DL1 (GET-MIN-DEGREELIST CF1))))
      (SETQ CF2
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
               (CADR V)))
      (COND ((NULL CF2) (SETQ DL2 (NLIST 0 (LENGTH (CADDR V)))))
            (T (SETQ DL2 (GET-MIN-DEGREELIST CF2))))
      (SETQ U (CADDR U))
      (SETQ V (CADDR V))
      (COND ((NEQ (LENGTH U) (LENGTH V)) (RETURN NIL)))
      (COND ((NULL U) (RETURN (LIST NIL NIL NIL NIL NIL))))
     LOOP
      (COND
       ((NOT
         (AND (EQUAL (CAR (CAR U)) (CAR (CAR V)))
              (EQUAL (CADR (CAR U)) (CADR (CAR V)))))
        (RETURN NIL)))
      (SETQ MINDG
              (COND (DIV? (TAYEXP-DIFFERENCE (CAR DL1) (CAR DL2)))
                    (T (TAYEXP-PLUS (CAR DL1) (CAR DL2)))))
      (SETQ |#TERMS-1|
              (TAYEXP-MIN2 (TAYEXP-DIFFERENCE (CADDR (CAR U)) (CAR DL1))
                           (TAYEXP-DIFFERENCE (CADDR (CAR V)) (CAR DL2))))
      (SETQ |#TERMS-NEXT|
              (TAYEXP-MIN2 (TAYEXP-DIFFERENCE (CADDDR (CAR U)) (CAR DL1))
                           (TAYEXP-DIFFERENCE (CADDDR (CAR V)) (CAR DL2))))
      (SETQ ORD1 (CONS (TAYEXP-PLUS |#TERMS-1| (CAR DL1)) ORD1))
      (SETQ ORD2 (CONS (TAYEXP-PLUS |#TERMS-1| (CAR DL2)) ORD2))
      (SETQ NEXT1 (CONS (TAYEXP-PLUS |#TERMS-NEXT| (CAR DL1)) NEXT1))
      (SETQ NEXT2 (CONS (TAYEXP-PLUS |#TERMS-NEXT| (CAR DL2)) NEXT2))
      (SETQ W
              (CONS
               (LIST (CAR (CAR U)) (CADR (CAR U))
                     (TAYEXP-PLUS MINDG |#TERMS-1|)
                     (TAYEXP-PLUS MINDG |#TERMS-NEXT|))
               W))
      (SETQ U (CDR U))
      (SETQ V (CDR V))
      (SETQ DL1 (CDR DL1))
      (SETQ DL2 (CDR DL2))
      (COND
       ((NULL U)
        (RETURN
         (LIST (REVERSIP W) (REVERSIP ORD1) (REVERSIP ORD2) (REVERSIP NEXT1)
               (REVERSIP NEXT2)))))
      (GO LOOP))) 
(PUT 'INV.TP. 'NUMBER-OF-ARGS 1) 
(PUT 'INV.TP. 'DEFINED-ON-LINE '304) 
(PUT 'INV.TP. 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'INV.TP. 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INV.TP. (U)
    (PROG (W CF |#TERMS-1| |#TERMS-NEXT| DL MINDG)
      (SETQ CF
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
               (CADR U)))
      (COND ((NULL CF) (SETQ DL (NLIST 0 (LENGTH (CADDR U)))))
            (T
             (SETQ DL
                     (PROG (DG FORALL-RESULT FORALL-ENDPTR)
                       (SETQ DG (CAR (CAR CF)))
                       (COND ((NULL DG) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (DG)
                                           (PROG (N FORALL-RESULT)
                                             (SETQ N DG)
                                             (SETQ FORALL-RESULT 0)
                                            LAB1
                                             (COND
                                              ((NULL N)
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (TAYEXP-PLUS
                                                      ((LAMBDA (N) N) (CAR N))
                                                      FORALL-RESULT))
                                             (SETQ N (CDR N))
                                             (GO LAB1)))
                                         (CAR DG))
                                        NIL)))
                      LOOPLABEL
                       (SETQ DG (CDR DG))
                       (COND ((NULL DG) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (DG)
                                   (PROG (N FORALL-RESULT)
                                     (SETQ N DG)
                                     (SETQ FORALL-RESULT 0)
                                    LAB1
                                     (COND ((NULL N) (RETURN FORALL-RESULT)))
                                     (SETQ FORALL-RESULT
                                             (TAYEXP-PLUS
                                              ((LAMBDA (N) N) (CAR N))
                                              FORALL-RESULT))
                                     (SETQ N (CDR N))
                                     (GO LAB1)))
                                 (CAR DG))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ U (CADDR U))
      (COND ((NULL U) (RETURN (LIST NIL NIL))))
     LOOP
      (SETQ MINDG (TAYEXP-MINUS (CAR DL)))
      (SETQ |#TERMS-1| (TAYEXP-DIFFERENCE (CADDR (CAR U)) (CAR DL)))
      (SETQ |#TERMS-NEXT| (TAYEXP-DIFFERENCE (CADDDR (CAR U)) (CAR DL)))
      (SETQ W
              (CONS
               (LIST (CAR (CAR U)) (CADR (CAR U))
                     (TAYEXP-PLUS MINDG |#TERMS-1|)
                     (TAYEXP-PLUS MINDG |#TERMS-NEXT|))
               W))
      (SETQ U (CDR U))
      (SETQ DL (CDR DL))
      (COND ((NULL U) (RETURN (LIST (REVERSIP W)))))
      (GO LOOP))) 
(DE TAYCOEFF-BEFORE (CC1 CC2) (TAYDEGREE< (CAR CC1) (CAR CC2))) 
(PUT 'TAYCOEFF-BEFORE 'NUMBER-OF-ARGS 2) 
(PUT 'TAYCOEFF-BEFORE 'DEFINED-ON-LINE '330) 
(PUT 'TAYCOEFF-BEFORE 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYCOEFF-BEFORE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'TAYCOEFF-BEFORE 'INLINE
      '(LAMBDA (CC1 CC2) (TAYDEGREE< (CAR CC1) (CAR CC2)))) 
(PUT 'TAYDEGREE< 'NUMBER-OF-ARGS 2) 
(PUT 'TAYDEGREE< 'DEFINED-ON-LINE '339) 
(PUT 'TAYDEGREE< 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYDEGREE< 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYDEGREE< (U V)
    (PROG (U1 V1)
     LOOP
      (SETQ U1 (CAR U))
      (SETQ V1 (CAR V))
     LOOP2
      (COND ((TAYEXP-GREATERP (CAR U1) (CAR V1)) (RETURN NIL))
            ((TAYEXP-LESSP (CAR U1) (CAR V1)) (RETURN T)))
      (SETQ U1 (CDR U1))
      (SETQ V1 (CDR V1))
      (COND ((NOT (NULL U1)) (GO LOOP2)))
      (SETQ U (CDR U))
      (SETQ V (CDR V))
      (COND ((NOT (NULL U)) (GO LOOP))))) 
(PUT 'TAYDEGREE-STRICT<= 'NUMBER-OF-ARGS 2) 
(PUT 'TAYDEGREE-STRICT<= 'DEFINED-ON-LINE '361) 
(PUT 'TAYDEGREE-STRICT<= 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYDEGREE-STRICT<= 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYDEGREE-STRICT<= (U V)
    (PROG (U1 V1)
     LOOP
      (SETQ U1 (CAR U))
      (SETQ V1 (CAR V))
     LOOP2
      (COND ((TAYEXP-GREATERP (CAR U1) (CAR V1)) (RETURN NIL)))
      (SETQ U1 (CDR U1))
      (SETQ V1 (CDR V1))
      (COND ((NOT (NULL U1)) (GO LOOP2)))
      (SETQ U (CDR U))
      (SETQ V (CDR V))
      (COND ((NOT (NULL U)) (GO LOOP)))
      (RETURN T))) 
(PUT 'EXCEEDS-ORDER 'NUMBER-OF-ARGS 2) 
(PUT 'EXCEEDS-ORDER 'DEFINED-ON-LINE '384) 
(PUT 'EXCEEDS-ORDER 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'EXCEEDS-ORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXCEEDS-ORDER (ORDLIS CF)
    (COND ((NULL ORDLIS) NIL)
          (T
           (OR
            (TAYEXP-GEQ
             (PROG (N FORALL-RESULT)
               (SETQ N (CAR CF))
               (SETQ FORALL-RESULT 0)
              LAB1
               (COND ((NULL N) (RETURN FORALL-RESULT)))
               (SETQ FORALL-RESULT
                       (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
               (SETQ N (CDR N))
               (GO LAB1))
             (CAR ORDLIS))
            (EXCEEDS-ORDER (CDR ORDLIS) (CDR CF)))))) 
(PUT 'EXCEEDS-ORDER-VARIANT 'NUMBER-OF-ARGS 2) 
(PUT 'EXCEEDS-ORDER-VARIANT 'DEFINED-ON-LINE '395) 
(PUT 'EXCEEDS-ORDER-VARIANT 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'EXCEEDS-ORDER-VARIANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXCEEDS-ORDER-VARIANT (ORDLIS CF)
    (COND ((NULL ORDLIS) NIL)
          (T
           (OR
            (TAYEXP-GREATERP
             (PROG (N FORALL-RESULT)
               (SETQ N (CAR CF))
               (SETQ FORALL-RESULT 0)
              LAB1
               (COND ((NULL N) (RETURN FORALL-RESULT)))
               (SETQ FORALL-RESULT
                       (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
               (SETQ N (CDR N))
               (GO LAB1))
             (CAR ORDLIS))
            (EXCEEDS-ORDER-VARIANT (CDR ORDLIS) (CDR CF)))))) 
(PUT 'ENTER-SORTED 'NUMBER-OF-ARGS 2) 
(PUT 'ENTER-SORTED 'DEFINED-ON-LINE '406) 
(PUT 'ENTER-SORTED 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'ENTER-SORTED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ENTER-SORTED (U ALIST)
    (COND ((NULL ALIST) (LIST U))
          ((TAYDEGREE< (CAR U) (CAR (CAR ALIST))) (CONS U ALIST))
          (T (CONS (CAR ALIST) (ENTER-SORTED U (CDR ALIST)))))) 
(PUT 'DELETE-SUPERFLUOUS-COEFFS 'NUMBER-OF-ARGS 3) 
(PUT 'DELETE-SUPERFLUOUS-COEFFS 'DEFINED-ON-LINE '417) 
(PUT 'DELETE-SUPERFLUOUS-COEFFS 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'DELETE-SUPERFLUOUS-COEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DELETE-SUPERFLUOUS-COEFFS (CFLIS POS N)
    (PROG (CC FORALL-RESULT FORALL-ENDPTR)
      (SETQ CC CFLIS)
     STARTOVER
      (COND ((NULL CC) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (CC)
                 (COND
                  ((TAYEXP-GREATERP
                    (PROG (N FORALL-RESULT)
                      (SETQ N (NTH (CAR CC) POS))
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((NULL N) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N))
                                           FORALL-RESULT))
                      (SETQ N (CDR N))
                      (GO LAB1))
                    N)
                   NIL)
                  (T (LIST CC))))
               (CAR CC)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ CC (CDR CC))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL CC) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (CC)
                 (COND
                  ((TAYEXP-GREATERP
                    (PROG (N FORALL-RESULT)
                      (SETQ N (NTH (CAR CC) POS))
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((NULL N) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N))
                                           FORALL-RESULT))
                      (SETQ N (CDR N))
                      (GO LAB1))
                    N)
                   NIL)
                  (T (LIST CC))))
               (CAR CC)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ CC (CDR CC))
      (GO LOOPLABEL))) 
(PUT 'TRUNCATE-COEFFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'TRUNCATE-COEFFLIST 'DEFINED-ON-LINE '428) 
(PUT 'TRUNCATE-COEFFLIST 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TRUNCATE-COEFFLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRUNCATE-COEFFLIST (CFLIS DL)
    (PROG (L)
      (PROG (CF)
        (SETQ CF CFLIS)
       LAB
        (COND ((NULL CF) (RETURN NIL)))
        ((LAMBDA (CF)
           (COND ((NOT (EXCEEDS-ORDER DL (CAR CF))) (SETQ L (CONS CF L)))))
         (CAR CF))
        (SETQ CF (CDR CF))
        (GO LAB))
      (RETURN (REVERSIP L)))) 
(PUT 'TAYTP-MIN2 'NUMBER-OF-ARGS 2) 
(PUT 'TAYTP-MIN2 'DEFINED-ON-LINE '441) 
(PUT 'TAYTP-MIN2 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYTP-MIN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYTP-MIN2 (TP1 TP2)
    (COND ((NULL TP1) NIL)
          ((NOT
            (AND (EQUAL (CAR (CAR TP1)) (CAR (CAR TP2)))
                 (EQUAL (CADR (CAR TP1)) (CADR (CAR TP2)))))
           (CONFUSION 'TAYTPMIN2))
          (T
           (CONS
            (LIST (CAR (CAR TP1)) (CADR (CAR TP2))
                  (TAYEXP-MIN2 (CADDR (CAR TP1)) (CADDR (CAR TP2)))
                  (TAYEXP-MIN2 (CADDDR (CAR TP1)) (CADDDR (CAR TP2))))
            (TAYTP-MIN2 (CDR TP1) (CDR TP2)))))) 
(PUT 'TRUNCATE-TAYLOR* 'NUMBER-OF-ARGS 2) 
(PUT 'TRUNCATE-TAYLOR* 'DEFINED-ON-LINE '457) 
(PUT 'TRUNCATE-TAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TRUNCATE-TAYLOR* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRUNCATE-TAYLOR* (TAY NTP)
    (PROG (NL OL L TP TCL OTP)
      (SETQ TCL (CADR TAY))
      (SETQ OTP (CADDR TAY))
      (SETQ TP
              (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                (SETQ PP (PAIR NTP OTP))
                (COND ((NULL PP) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PP)
                                    (LIST (CAR (CAR PP)) (CADR (CAR PP))
                                          (TAYEXP-MIN2 (CADDR (CAR PP))
                                                       (CADDR (CDR PP)))
                                          (TAYEXP-MIN2 (CADDDR (CAR PP))
                                                       (CADDDR (CDR PP)))))
                                  (CAR PP))
                                 NIL)))
               LOOPLABEL
                (SETQ PP (CDR PP))
                (COND ((NULL PP) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PP)
                            (LIST (CAR (CAR PP)) (CADR (CAR PP))
                                  (TAYEXP-MIN2 (CADDR (CAR PP))
                                               (CADDR (CDR PP)))
                                  (TAYEXP-MIN2 (CADDDR (CAR PP))
                                               (CADDDR (CDR PP)))))
                          (CAR PP))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TP)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ OL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TP)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (CF)
        (SETQ CF TCL)
       LAB
        (COND ((NULL CF) (RETURN NIL)))
        ((LAMBDA (CF)
           (COND
            ((NOT (NULL (CAR (CDR CF))))
             ((LAMBDA (PL)
                (COND
                 ((NOT (EXCEEDS-ORDER-VARIANT OL PL)) (SETQ L (CONS CF L)))
                 (T (SETQ NL (MIN2-ORDER NL OL PL)))))
              (CAR CF)))))
         (CAR CF))
        (SETQ CF (CDR CF))
        (GO LAB))
      (RETURN
       (LIST 'TAYLOR* (REVERSIP L) (REPLACE-NEXT TP NL) (CADDDR TAY)
             (CAR (CDDDDR TAY)))))) 
(PUT 'TP-GREATERP 'NUMBER-OF-ARGS 2) 
(PUT 'TP-GREATERP 'DEFINED-ON-LINE '484) 
(PUT 'TP-GREATERP 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TP-GREATERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TP-GREATERP (TP1 TP2)
    (COND ((NULL TP1) NIL)
          (T
           (OR (TAYEXP-GREATERP (CADDR (CAR TP1)) (CADDR (CAR TP2)))
               (TP-GREATERP (CDR TP1) (CDR TP2)))))) 
(PUT 'SUBTR-TP-ORDER 'NUMBER-OF-ARGS 2) 
(PUT 'SUBTR-TP-ORDER 'DEFINED-ON-LINE '494) 
(PUT 'SUBTR-TP-ORDER 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'SUBTR-TP-ORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBTR-TP-ORDER (TP1 TP2)
    (COND ((NULL TP1) NIL)
          (T
           (CONS (TAYEXP-DIFFERENCE (CADDR (CAR TP1)) (CADDR (CAR TP2)))
                 (SUBTR-TP-ORDER (CDR TP1) (CDR TP2)))))) 
(PUT 'ADDTO-ALL-TAYTPELORDERS 'NUMBER-OF-ARGS 2) 
(PUT 'ADDTO-ALL-TAYTPELORDERS 'DEFINED-ON-LINE '508) 
(PUT 'ADDTO-ALL-TAYTPELORDERS 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'ADDTO-ALL-TAYTPELORDERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDTO-ALL-TAYTPELORDERS (TP NL)
    (COND ((NULL TP) NIL)
          (T
           (CONS
            (LIST (CAR (CAR TP)) (CADR (CAR TP))
                  (TAYEXP-PLUS (CADDR (CAR TP)) (CAR NL))
                  (TAYEXP-PLUS (CADDDR (CAR TP)) (CAR NL)))
            (ADDTO-ALL-TAYTPELORDERS (CDR TP) (CDR NL)))))) 
(PUT 'TAYMINCOEFF 'NUMBER-OF-ARGS 1) 
(PUT 'TAYMINCOEFF 'DEFINED-ON-LINE '517) 
(PUT 'TAYMINCOEFF 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYMINCOEFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYMINCOEFF (CFLIS)
    (COND ((NULL CFLIS) 0)
          ((NOT (NULL (CAR (CDR (CAR CFLIS)))))
           (PROG (N FORALL-RESULT)
             (SETQ N (CAR (CAR (CAR CFLIS))))
             (SETQ FORALL-RESULT 0)
            LAB1
             (COND ((NULL N) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT
                     (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
             (SETQ N (CDR N))
             (GO LAB1)))
          (T (TAYMINCOEFF (CDR CFLIS))))) 
(PUT 'TAYMINPOWERLIST 'NUMBER-OF-ARGS 1) 
(PUT 'TAYMINPOWERLIST 'DEFINED-ON-LINE '527) 
(PUT 'TAYMINPOWERLIST 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYMINPOWERLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYMINPOWERLIST (CFLIS)
    (COND ((NULL CFLIS) (CONFUSION 'TAYMINPOWERLIST))
          (T (TAYMINPOWERLIST1 CFLIS (LENGTH (CAR (CAR CFLIS))))))) 
(PUT 'TAYMINPOWERLIST1 'NUMBER-OF-ARGS 2) 
(PUT 'TAYMINPOWERLIST1 'DEFINED-ON-LINE '535) 
(PUT 'TAYMINPOWERLIST1 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYMINPOWERLIST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYMINPOWERLIST1 (CFLIS L)
    (COND ((NULL CFLIS) (NLIST 0 L))
          ((NULL (CAR (CDR (CAR CFLIS)))) (TAYMINPOWERLIST1 (CDR CFLIS) L))
          (T
           (PROG (DG FORALL-RESULT FORALL-ENDPTR)
             (SETQ DG (CAR (CAR CFLIS)))
             (COND ((NULL DG) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (DG)
                                 (PROG (N FORALL-RESULT)
                                   (SETQ N DG)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND ((NULL N) (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (TAYEXP-PLUS
                                            ((LAMBDA (N) N) (CAR N))
                                            FORALL-RESULT))
                                   (SETQ N (CDR N))
                                   (GO LAB1)))
                               (CAR DG))
                              NIL)))
            LOOPLABEL
             (SETQ DG (CDR DG))
             (COND ((NULL DG) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (DG)
                         (PROG (N FORALL-RESULT)
                           (SETQ N DG)
                           (SETQ FORALL-RESULT 0)
                          LAB1
                           (COND ((NULL N) (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N))
                                                FORALL-RESULT))
                           (SETQ N (CDR N))
                           (GO LAB1)))
                       (CAR DG))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GET-CST-COEFF 'NUMBER-OF-ARGS 1) 
(PUT 'GET-CST-COEFF 'DEFINED-ON-LINE '541) 
(PUT 'GET-CST-COEFF 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'GET-CST-COEFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-CST-COEFF (TAY)
    ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
     (ASSOC
      (PROG (EL FORALL-RESULT FORALL-ENDPTR)
        (SETQ EL (CADDR TAY))
        (COND ((NULL EL) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                         NIL)))
       LOOPLABEL
        (SETQ EL (CDR EL))
        (COND ((NULL EL) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (CADR TAY)))) 
(PUT 'TAYLOR*-CONSTANTP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLOR*-CONSTANTP 'DEFINED-ON-LINE '544) 
(PUT 'TAYLOR*-CONSTANTP 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR*-CONSTANTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLOR*-CONSTANTP (TAY)
    (TAYLOR*-CONSTANTP1
     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
       (SETQ EL (CADDR TAY))
       (COND ((NULL EL) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                        NIL)))
      LOOPLABEL
       (SETQ EL (CDR EL))
       (COND ((NULL EL) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     (CADR TAY))) 
(PUT 'TAYLOR*-CONSTANTP1 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOR*-CONSTANTP1 'DEFINED-ON-LINE '548) 
(PUT 'TAYLOR*-CONSTANTP1 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR*-CONSTANTP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOR*-CONSTANTP1 (PL TCF)
    (COND ((NULL TCF) T)
          ((EQUAL (CAR (CAR TCF)) PL) (TAYCOEFFLIST-ZEROP (CDR TCF)))
          ((NOT (NULL (CAR (CDR (CAR TCF))))) NIL)
          (T (TAYLOR*-CONSTANTP1 PL (CDR TCF))))) 
(PUT 'CHECK-FOR-CST-TAYLOR 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-FOR-CST-TAYLOR 'DEFINED-ON-LINE '555) 
(PUT 'CHECK-FOR-CST-TAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'CHECK-FOR-CST-TAYLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-FOR-CST-TAYLOR (TAY)
    (PROG (PL TC)
      (SETQ PL
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL (CADDR TAY))
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
                (GO LOOPLABEL)))
      (SETQ TC (CADR TAY))
      (RETURN
       (COND
        ((TAYLOR*-CONSTANTP1 PL TC)
         ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
          (ASSOC PL TC)))
        (T (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1)))))) 
(PUT 'TAYLOR*-NZCONSTANTP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLOR*-NZCONSTANTP 'DEFINED-ON-LINE '563) 
(PUT 'TAYLOR*-NZCONSTANTP 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR*-NZCONSTANTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLOR*-NZCONSTANTP (TAY)
    (TAYLOR*-NZCONSTANTP1
     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
       (SETQ EL (CADDR TAY))
       (COND ((NULL EL) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                        NIL)))
      LOOPLABEL
       (SETQ EL (CDR EL))
       (COND ((NULL EL) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     (CADR TAY))) 
(PUT 'TAYLOR*-NZCONSTANTP1 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOR*-NZCONSTANTP1 'DEFINED-ON-LINE '567) 
(PUT 'TAYLOR*-NZCONSTANTP1 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR*-NZCONSTANTP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOR*-NZCONSTANTP1 (PL TCF)
    (COND ((NULL TCF) NIL)
          ((EQUAL (CAR (CAR TCF)) PL)
           (COND ((NULL (CAR (CDR (CAR TCF)))) NIL)
                 (T (TAYCOEFFLIST-ZEROP (CDR TCF)))))
          ((AND (NEQ (CAR (CAR TCF)) PL) (NOT (NULL (CAR (CDR (CAR TCF))))))
           NIL)
          (T (TAYLOR*-NZCONSTANTP1 PL (CDR TCF))))) 
(PUT 'TAYLOR*-ONEP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLOR*-ONEP 'DEFINED-ON-LINE '577) 
(PUT 'TAYLOR*-ONEP 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR*-ONEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLOR*-ONEP (TAY)
    (TAYLOR-ONEP1
     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
       (SETQ EL (CADDR TAY))
       (COND ((NULL EL) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                        NIL)))
      LOOPLABEL
       (SETQ EL (CDR EL))
       (COND ((NULL EL) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     (CADR TAY))) 
(PUT 'TAYLOR-ONEP1 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOR-ONEP1 'DEFINED-ON-LINE '580) 
(PUT 'TAYLOR-ONEP1 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'TAYLOR-ONEP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOR-ONEP1 (PL TCF)
    (COND ((NULL TCF) NIL)
          ((EQUAL (CAR (CAR TCF)) PL)
           (COND
            ((EQUAL (CDR (CAR TCF)) (CONS 1 1)) (TAYCOEFFLIST-ZEROP (CDR TCF)))
            (T NIL)))
          ((NULL (CAR (CDR (CAR TCF)))) (TAYLOR*-NZCONSTANTP1 PL (CDR TCF)))
          (T NIL))) 
(PUT 'IS-NEG-PL 'NUMBER-OF-ARGS 1) 
(PUT 'IS-NEG-PL 'DEFINED-ON-LINE '590) 
(PUT 'IS-NEG-PL 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'IS-NEG-PL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS-NEG-PL (PL)
    (COND ((NULL PL) NIL)
          ((TAYEXP-LESSP
            (PROG (N FORALL-RESULT)
              (SETQ N (CAR PL))
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND ((NULL N) (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (TAYEXP-PLUS ((LAMBDA (N) N) (CAR N)) FORALL-RESULT))
              (SETQ N (CDR N))
              (GO LAB1))
            0)
           T)
          (T (IS-NEG-PL (CDR PL))))) 
(PUT 'RAT-KERN-POW 'NUMBER-OF-ARGS 2) 
(PUT 'RAT-KERN-POW 'DEFINED-ON-LINE '599) 
(PUT 'RAT-KERN-POW 'DEFINED-IN-FILE 'TAYLOR/TAYUTILS.RED) 
(PUT 'RAT-KERN-POW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RAT-KERN-POW (X POS)
    (PROG (Y N)
      (SETQ N 0)
      (COND
       ((OR (OR (ATOM X) (ATOM (CAR X))) (NOT (NULL (CDR X)))
            (NOT (EQUAL (CDAR X) 1)))
        (RETURN NIL)))
      (SETQ N (CDAAR X))
      (SETQ X (CAAAR X))
      (COND
       ((EQCAR X 'SQRT) (RETURN (TAYEXP-TIMES (CONS (CADR X) (MKRN 1 2)) N)))
       ((AND (EQCAR X 'EXPT) (SETQ Y (SIMPRN (LIST (CADDR X)))))
        (COND
         ((OR (NULL POS) (TAYEXP-GREATERP (SETQ Y (CAR Y)) 0))
          (RETURN (CONS (CADR X) (TAYEXP-TIMES Y N))))
         (T (RETURN NIL))))
       (T (RETURN (CONS X N)))))) 
(ENDMODULE) 
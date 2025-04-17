(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYINTRF)) 
(EXPORTS (LIST 'SIMPTAYLOR 'SIMPTAYLOR* 'TAYAPPLYOPFN)) 
(IMPORTS
 (LIST '*F2Q 'ACONC* 'DENR 'DEPENDS 'DIFFSQ 'EQCAR 'ERRORSET* 'KERNP 'LASTPAIR
       'LEQ 'LPRIM 'MKQUOTE 'MKSQ 'MULTSQ 'MVAR 'NEQ 'NTH 'NUMR 'ORDAD 'OVER
       'PREPSQ 'RESETKLIST 'REVLIS 'REVERSIP 'SIMP 'SIMP* 'SUBS2 'SUBSQ 'TYPERR
       'UNION '*TAY2Q 'GET-DEGREE 'HAS-TAYLOR* 'HAS-TAYVARS 'MAKE-TAYLOR*
       'MULTINTOCOEFFLIST 'RESIMPTAYLOR 'TAYCFPL 'TAYCFSQ 'TAYCOEFFLIST
       'TAYFLAGS 'TAYMAKECOEFF 'TAYORIG 'TAYTEMPLATE 'TAYTPELORDER
       'TAYTPELPOINT 'TAYLOR-KERNEL-SQ-P 'TAYMINCOEFF 'REPLACE-NTH
       'TAYLOR-ERROR 'VAR-IS-NTH 'TAYLOREXPAND 'DELETE-SUPERFLUOUS-COEFFS
       'INVTAYLOR1 'QUOTTAYLOR1 'TAYSIMPSQ 'PREPTAYLOR*)) 
(FLUID
 '(*BACKTRACE *PRECISE *TAYINTERNAL* *TAYLORKEEPORIGINAL *TAYLORAUTOCOMBINE
   TAYNOMUL* FRLIS* SUBFG*)) 
(GLOBAL '(KPROPS* MUL*)) 
(PUT 'TAYLORAUTOCOMBINE 'SIMPFG '((T (RMSUBS)))) 
(PUT 'TAYFKERN 'NUMBER-OF-ARGS 1) 
(PUT 'TAYFKERN 'DEFINED-ON-LINE '86) 
(PUT 'TAYFKERN 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYFKERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYFKERN (U)
    (PROG (X Y)
      (COND (*TAYINTERNAL* (RETURN U)))
      (SETQ Y (GET 'TAYLOR* 'KLIST))
      (SETQ X (GETHASH U KERNHASH))
      (COND
       ((NULL X)
        (PROGN
         (SETQ X (LIST U NIL))
         (PUTHASH U KERNHASH X)
         (SETQ Y (ORDAD X Y))
         (SETQ KPROPS* (UNION '(TAYLOR*) KPROPS*))
         (PUT 'TAYLOR* 'KLIST Y))))
      (RETURN X))) 
(PUT 'TAYLOR* 'FKERNFN 'TAYFKERN) 
(PUT 'TAYSIMPSQ-FROM-MUL 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPSQ-FROM-MUL 'DEFINED-ON-LINE '106) 
(PUT 'TAYSIMPSQ-FROM-MUL 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYSIMPSQ-FROM-MUL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPSQ-FROM-MUL (U) ((LAMBDA (TAYNOMUL*) (TAYSIMPSQ U)) T)) 
(PUT 'SIMPTAYLOR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTAYLOR 'DEFINED-ON-LINE '109) 
(PUT 'SIMPTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'SIMPTAYLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTAYLOR (U)
    (COND
     ((NEQ (REMAINDER (LENGTH U) 3) 1) (TAYLOR-ERROR 'WRONG-NO-ARGS 'TAYLOR))
     ((NULL SUBFG*) (MKSQ (CONS 'TAYLOR U) 1))
     (T
      (PROG (*PRECISE ARGLIST DEGREE F LL RESULT VAR VAR0)
        (COND
         ((AND *TAYLORAUTOCOMBINE (NOT TAYNOMUL*) (NOT (MEMQ 'TAYSIMPSQ MUL*)))
          (SETQ MUL* (ACONC* MUL* 'TAYSIMPSQ-FROM-MUL))))
        (SETQ F (SIMP* (CAR U)))
        (SETQ U (REVLIS (CDR U)))
        (SETQ ARGLIST U)
        (PROG ()
         WHILELABEL
          (COND ((NOT (NOT (NULL ARGLIST))) (RETURN NIL)))
          (PROGN
           (SETQ VAR (CAR ARGLIST))
           (SETQ VAR (COND ((EQCAR VAR 'LIST) (CDR VAR)) (T (LIST VAR))))
           (PROG (EL FORALL-RESULT FORALL-ENDPTR)
             (SETQ EL VAR)
             (COND ((NULL EL) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (EL)
                                 (PROG ()
                                   (SETQ EL (SIMP* EL))
                                   (COND ((KERNP EL) (RETURN (CAAAR (CAR EL))))
                                         (T (TYPERR (PREPSQ EL) 'KERNEL)))))
                               (CAR EL))
                              NIL)))
            LOOPLABEL
             (SETQ EL (CDR EL))
             (COND ((NULL EL) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (EL)
                         (PROG ()
                           (SETQ EL (SIMP* EL))
                           (COND ((KERNP EL) (RETURN (CAAAR (CAR EL))))
                                 (T (TYPERR (PREPSQ EL) 'KERNEL)))))
                       (CAR EL))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))
           (SETQ VAR0 (CADR ARGLIST))
           (SETQ DEGREE (CADDR ARGLIST))
           (COND
            ((NOT (FIXP DEGREE)) (TYPERR DEGREE "order of Taylor expansion")))
           (SETQ ARGLIST (CDDDR ARGLIST))
           (SETQ LL (CONS (LIST VAR VAR0 DEGREE (PLUS DEGREE 1)) LL)))
          (GO WHILELABEL))
        (SETQ RESULT (TAYLOREXPAND F (REVERSIP LL)))
        (RETURN
         (COND ((SMEMBER 'TAYLOR* RESULT) RESULT)
               (T (MKSQ (CONS 'TAYLOR (CONS (PREPSQ F) U)) 1)))))))) 
(PUT 'TAYLOR 'SIMPFN 'SIMPTAYLOR) 
(PUT 'TAYLOR1 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLOR1 'DEFINED-ON-LINE '193) 
(PUT 'TAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR1 (F VARLIS VAR0 N)
    (TAYLOR1SQ
     (COND
      ((EQ VAR0 'INFINITY)
       (SUBSQ F
              (PROG (KRNL FORALL-RESULT FORALL-ENDPTR)
                (SETQ KRNL VARLIS)
                (COND ((NULL KRNL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (KRNL)
                                    (CONS KRNL (LIST 'QUOTIENT 1 KRNL)))
                                  (CAR KRNL))
                                 NIL)))
               LOOPLABEL
                (SETQ KRNL (CDR KRNL))
                (COND ((NULL KRNL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (KRNL) (CONS KRNL (LIST 'QUOTIENT 1 KRNL)))
                          (CAR KRNL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))
      (T F))
     VARLIS VAR0 N)) 
(PUT 'TAYLOR1SQ 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLOR1SQ 'DEFINED-ON-LINE '209) 
(PUT 'TAYLOR1SQ 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR1SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR1SQ (F VARLIS VAR0 N)
    (COND
     ((AND (KERNP F) (EQCAR (CAAAR (CAR F)) 'TAYLOR*))
      (COND
       ((SMEMBERLP
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X (CADDR (CAAAR (CAR F))))
          STARTOVER
           (COND ((NULL X) (RETURN NIL)))
           (SETQ FORALL-RESULT ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ X (CDR X))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL X) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ X (CDR X))
           (GO LOOPLABEL))
         VARLIS)
        (TAYLORSAMEVAR (CAAAR (CAR F)) VARLIS VAR0 N))
       (T
        (PROG (Y Z)
          (SETQ F (CAAAR (CAR F)))
          (SETQ Z
                  (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                    (SETQ CC (CADR F))
                   STARTOVER
                    (COND ((NULL CC) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (CC)
                               (PROG (CC2 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ CC2 (TAYLOR2 (CDR CC) VARLIS VAR0 N))
                                 (COND ((NULL CC2) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (CC2)
                                                     (CONS
                                                      (APPEND (CAR CC)
                                                              (CAR CC2))
                                                      (CDR CC2)))
                                                   (CAR CC2))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ CC2 (CDR CC2))
                                 (COND ((NULL CC2) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (CC2)
                                             (CONS (APPEND (CAR CC) (CAR CC2))
                                                   (CDR CC2)))
                                           (CAR CC2))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR CC)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ CC (CDR CC))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL CC) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (CC)
                               (PROG (CC2 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ CC2 (TAYLOR2 (CDR CC) VARLIS VAR0 N))
                                 (COND ((NULL CC2) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (CC2)
                                                     (CONS
                                                      (APPEND (CAR CC)
                                                              (CAR CC2))
                                                      (CDR CC2)))
                                                   (CAR CC2))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ CC2 (CDR CC2))
                                 (COND ((NULL CC2) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (CC2)
                                             (CONS (APPEND (CAR CC) (CAR CC2))
                                                   (CDR CC2)))
                                           (CAR CC2))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR CC)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ CC (CDR CC))
                    (GO LOOPLABEL)))
          (SETQ Y (APPEND (CADDR F) (LIST (LIST VARLIS VAR0 N (PLUS N 1)))))
          (RETURN (LIST 'TAYLOR* Z Y (CADDDR F) (CAR (CDDDDR F))))))))
     (T
      (LIST 'TAYLOR* (TAYLOR2 F VARLIS VAR0 N)
            (LIST (LIST VARLIS VAR0 N (PLUS N 1)))
            (COND (*TAYLORKEEPORIGINAL F) (T NIL)) NIL)))) 
(PUT 'TAYLOR2 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLOR2 'DEFINED-ON-LINE '254) 
(PUT 'TAYLOR2 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR2 (F VARLIS VAR0 N)
    (PROG (RESULT OLDKLIST)
      (SETQ OLDKLIST (GET 'TAYLOR* 'KLIST))
      (SETQ RESULT
              (ERRORSET*
               (LIST 'TAYLOR2E (MKQUOTE F) (MKQUOTE VARLIS) (MKQUOTE VAR0)
                     (MKQUOTE N))
               NIL))
      (RESETKLIST 'TAYLOR* OLDKLIST)
      (COND ((ATOM RESULT) (TAYLOR-ERROR 'EXPANSION "(possible singularity!)"))
            (T (RETURN (CAR RESULT)))))) 
(PUT 'TAYLOR2E 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLOR2E 'DEFINED-ON-LINE '264) 
(PUT 'TAYLOR2E 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR2E 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR2E (F VARLIS VAR0 N)
    (COND ((CDR VARLIS) (TAYLOR2HOM F VARLIS VAR0 N))
          ((EQUAL (CDR F) 1) (TAYLOR2F (CAR F) (CAR VARLIS) VAR0 N T))
          ((NOT (DEPENDS (CDR F) (CAR VARLIS)))
           ((LAMBDA (G127)
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (TAYLOR2F (CAR F) (CAR VARLIS) VAR0 N T))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS (CAR P)
                                          (RESIMP
                                           (SUBS2* (MULTSQ (CDR P) G127)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (CAR P)
                                  (RESIMP (SUBS2* (MULTSQ (CDR P) G127)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
            (CONS 1 (CDR F))))
          ((EQUAL (CAR F) 1)
           (DELETE-SUPERFLUOUS-COEFFS
            (INVTAYLOR1 (LIST VARLIS VAR0 N (PLUS N 1))
             (TAYLOR2F (CDR F) (CAR VARLIS) VAR0 N NIL))
            1 N))
          ((NOT (DEPENDS (CAR F) (CAR VARLIS)))
           ((LAMBDA (G129)
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P
                        (INVTAYLOR1 (LIST VARLIS VAR0 N (PLUS N 1))
                         (TAYLOR2F (CDR F) (CAR VARLIS) VAR0 N NIL)))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS (CAR P)
                                          (RESIMP
                                           (SUBS2* (MULTSQ (CDR P) G129)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (CAR P)
                                  (RESIMP (SUBS2* (MULTSQ (CDR P) G129)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
            (CONS (CAR F) 1)))
          (T
           (PROG (DENOM N1)
             (SETQ N1 0)
             (SETQ DENOM (TAYLOR2F (CDR F) (CAR VARLIS) VAR0 N NIL))
             (SETQ N1 (PLUS N (TAYMINCOEFF DENOM)))
             (RETURN
              (DELETE-SUPERFLUOUS-COEFFS
               (QUOTTAYLOR1 (LIST VARLIS VAR0 N1 (PLUS N1 1))
                (TAYLOR2F (CAR F) (CAR VARLIS) VAR0 N1 T) DENOM)
               1 N)))))) 
(PUT 'TAYLOR2F 'NUMBER-OF-ARGS 5) 
(PUT 'TAYLOR2F 'DEFINED-ON-LINE '312) 
(PUT 'TAYLOR2F 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR2F 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR2F (F VAR VAR0 N FLG)
    (PROG (X Y Z K)
      (SETQ K 0)
      (COND ((EQ VAR0 'INFINITY) (SETQ VAR0 0)))
      (SETQ X (LIST (CONS VAR VAR0)))
      (SETQ Y (SIMP (LIST 'DIFFERENCE VAR VAR0)))
      (SETQ F (CONS F 1))
      (SETQ Z (SUBS2 (SUBSQ F X)))
      (COND ((AND (NULL (CAR Z)) (NOT FLG)) (SETQ N (PLUS N 1)))
            (T (SETQ FLG T)))
      (SETQ Y (LIST (CONS (LIST (LIST 0)) Z)))
      (SETQ K 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (LEQ K N) (NOT (NULL (CAR F))))) (RETURN NIL)))
        (PROGN
         (SETQ F (MULTSQ (DIFFSQ F VAR) (CONS 1 K)))
         (SETQ Z (SUBS2 (SUBSQ F X)))
         (COND ((AND (NULL (CAR Z)) (NOT FLG)) (SETQ N (PLUS N 2)))
               (T (SETQ FLG T)))
         (COND
          ((NOT (NULL (CAR Z))) (SETQ Y (CONS (CONS (LIST (LIST K)) Z) Y))))
         (SETQ K (PLUS K 1)))
        (GO WHILELABEL))
      (RETURN (REVERSIP Y)))) 
(PUT 'TAYLOR2HOM 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLOR2HOM 'DEFINED-ON-LINE '366) 
(PUT 'TAYLOR2HOM 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLOR2HOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR2HOM (F VARLIS VAR0 N)
    (COND ((NULL (CDR VARLIS)) (TAYLOR2E F (LIST (CAR VARLIS)) VAR0 N))
          (T
           (PROG (U FORALL-RESULT FORALL-ENDPTR)
             (SETQ U (TAYLOR2E F (LIST (CAR VARLIS)) VAR0 N))
            STARTOVER
             (COND ((NULL U) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (U)
                        (PROG (V FORALL-RESULT FORALL-ENDPTR)
                          (SETQ V
                                  (TAYLOR2HOM (CDR U) (CDR VARLIS) VAR0
                                   (DIFFERENCE N
                                               (PROG (N FORALL-RESULT)
                                                 (SETQ N (CAR (CAR U)))
                                                 (SETQ FORALL-RESULT 0)
                                                LAB1
                                                 (COND
                                                  ((NULL N)
                                                   (RETURN FORALL-RESULT)))
                                                 (SETQ FORALL-RESULT
                                                         (TAYEXP-PLUS
                                                          ((LAMBDA (N) N)
                                                           (CAR N))
                                                          FORALL-RESULT))
                                                 (SETQ N (CDR N))
                                                 (GO LAB1)))))
                          (COND ((NULL V) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (V)
                                              (CONS
                                               (LIST
                                                (CONS (CAR (CAR (CAR U)))
                                                      (CAR (CAR V))))
                                               (CDR V)))
                                            (CAR V))
                                           NIL)))
                         LOOPLABEL
                          (SETQ V (CDR V))
                          (COND ((NULL V) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (CONS
                                       (LIST
                                        (CONS (CAR (CAR (CAR U)))
                                              (CAR (CAR V))))
                                       (CDR V)))
                                    (CAR V))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR U)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ U (CDR U))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL U) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (U)
                        (PROG (V FORALL-RESULT FORALL-ENDPTR)
                          (SETQ V
                                  (TAYLOR2HOM (CDR U) (CDR VARLIS) VAR0
                                   (DIFFERENCE N
                                               (PROG (N FORALL-RESULT)
                                                 (SETQ N (CAR (CAR U)))
                                                 (SETQ FORALL-RESULT 0)
                                                LAB1
                                                 (COND
                                                  ((NULL N)
                                                   (RETURN FORALL-RESULT)))
                                                 (SETQ FORALL-RESULT
                                                         (TAYEXP-PLUS
                                                          ((LAMBDA (N) N)
                                                           (CAR N))
                                                          FORALL-RESULT))
                                                 (SETQ N (CDR N))
                                                 (GO LAB1)))))
                          (COND ((NULL V) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (V)
                                              (CONS
                                               (LIST
                                                (CONS (CAR (CAR (CAR U)))
                                                      (CAR (CAR V))))
                                               (CDR V)))
                                            (CAR V))
                                           NIL)))
                         LOOPLABEL
                          (SETQ V (CDR V))
                          (COND ((NULL V) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (CONS
                                       (LIST
                                        (CONS (CAR (CAR (CAR U)))
                                              (CAR (CAR V))))
                                       (CDR V)))
                                    (CAR V))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR U)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ U (CDR U))
             (GO LOOPLABEL))))) 
(PUT 'TAYLORSAMEVAR 'NUMBER-OF-ARGS 4) 
(PUT 'TAYLORSAMEVAR 'DEFINED-ON-LINE '379) 
(PUT 'TAYLORSAMEVAR 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYLORSAMEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLORSAMEVAR (U VARLIS VAR0 N)
    (PROG (TP MDEG POS)
      (SETQ MDEG 0)
      (SETQ POS 0)
      (COND
       ((CDR VARLIS)
        (TAYLOR-ERROR 'NOT-IMPLEMENTED
         "(homogeneous expansion in TAYLORSAMEVAR)")))
      (SETQ TP (CADDR U))
      (SETQ POS (CAR (VAR-IS-NTH TP (CAR VARLIS))))
      (SETQ TP (NTH TP POS))
      (COND
       ((NEQ (CADR TP) VAR0)
        (RETURN
         (TAYLOR1
          (COND ((NOT (NULL (CADDDR U))) (CADDDR U))
                (T (SIMP* (PREPTAYLOR* U))))
          VARLIS VAR0 N))))
      (SETQ MDEG (CADDR TP))
      (COND ((EQUAL N MDEG) (RETURN U))
            ((GREATERP N MDEG)
             (PROGN (LPRIM "Cannot expand further... truncated.") (RETURN U))))
      (RETURN
       (LIST 'TAYLOR*
             (PROG (CC FORALL-RESULT FORALL-ENDPTR)
               (SETQ CC (CADR U))
              STARTOVER
               (COND ((NULL CC) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (CC)
                          (COND ((GREATERP (NTH (NTH (CAR CC) POS) 1) N) NIL)
                                (T (LIST CC))))
                        (CAR CC)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ CC (CDR CC))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL CC) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (CC)
                          (COND ((GREATERP (NTH (NTH (CAR CC) POS) 1) N) NIL)
                                (T (LIST CC))))
                        (CAR CC)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ CC (CDR CC))
               (GO LOOPLABEL))
             (REPLACE-NTH (CADDR U) POS (LIST VARLIS (CADR TP) N (PLUS N 1)))
             (CADDDR U) (CAR (CDDDDR U)))))) 
(PUT 'SIMPTAYLOR* 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTAYLOR* 'DEFINED-ON-LINE '413) 
(PUT 'SIMPTAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'SIMPTAYLOR* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTAYLOR* (U)
    (COND (CONVERT-TAYLOR* (SIMP (PREPTAYLOR* U)))
          ((OR (MEMQ (CADR U) FRLIS*) (EQCAR (CADR U) '~))
           (CONS (LIST (CONS (GETPOWER (FKERN U) 1) 1)) 1))
          (T
           (PROGN
            (COND
             ((AND *TAYLORAUTOCOMBINE (NOT TAYNOMUL*)
                   (NOT (MEMQ 'TAYSIMPSQ MUL*)))
              (SETQ MUL* (ACONC* MUL* 'TAYSIMPSQ-FROM-MUL))))
            (CONS
             (LIST
              (CONS
               (GETPOWER
                (FKERN
                 (LIST 'TAYLOR*
                       (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                         (SETQ CC (CADR U))
                         (COND ((NULL CC) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (CC)
                                             (CONS (CAR CC)
                                                   (SUBS2 (RESIMP (CDR CC)))))
                                           (CAR CC))
                                          NIL)))
                        LOOPLABEL
                         (SETQ CC (CDR CC))
                         (COND ((NULL CC) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (CC)
                                     (CONS (CAR CC) (SUBS2 (RESIMP (CDR CC)))))
                                   (CAR CC))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (CADDR U)
                       (COND
                        ((AND *TAYLORKEEPORIGINAL (CADDDR U))
                         (RESIMP (CADDDR U)))
                        (T NIL))
                       (CAR (CDDDDR U))))
                1)
               1))
             1))))) 
(FLAG '(TAYLOR*) 'FULL) 
(PUT 'TAYLOR* 'SIMPFN 'SIMPTAYLOR*) 
(FLAG '(TAYLOR*) 'SIMP0FN) 
(PUT 'TAYAPPLYOPFN 'NUMBER-OF-ARGS 2) 
(PUT 'TAYAPPLYOPFN 'DEFINED-ON-LINE '443) 
(PUT 'TAYAPPLYOPFN 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYAPPLYOPFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYAPPLYOPFN (TAY FNC)
    (LIST 'TAYLOR*
          (PROG (CC FORALL-RESULT FORALL-ENDPTR)
            (SETQ CC (CADR TAY))
            (COND ((NULL CC) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (CC)
                                (CONS (CAR CC)
                                      (SIMP* (APPLY1 FNC (MK*SQ (CDR CC))))))
                              (CAR CC))
                             NIL)))
           LOOPLABEL
            (SETQ CC (CDR CC))
            (COND ((NULL CC) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (CC)
                        (CONS (CAR CC) (SIMP* (APPLY1 FNC (MK*SQ (CDR CC))))))
                      (CAR CC))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (CADDR TAY)
          (COND
           ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
            (SIMP* (APPLY1 FNC (MK*SQ (CADDDR TAY)))))
           (T NIL))
          (CAR (CDDDDR TAY)))) 
(PUT 'TAYAPPLYOPFN2 'NUMBER-OF-ARGS 3) 
(PUT 'TAYAPPLYOPFN2 'DEFINED-ON-LINE '455) 
(PUT 'TAYAPPLYOPFN2 'DEFINED-IN-FILE 'TAYLOR/TAYINTRF.RED) 
(PUT 'TAYAPPLYOPFN2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYAPPLYOPFN2 (TAY FNC EXTRA)
    (LIST 'TAYLOR*
          (PROG (CC FORALL-RESULT FORALL-ENDPTR)
            (SETQ CC (CADR TAY))
            (COND ((NULL CC) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (CC)
                                (CONS (CAR CC)
                                      (SIMP*
                                       (APPLY2 FNC (MK*SQ (CDR CC)) EXTRA))))
                              (CAR CC))
                             NIL)))
           LOOPLABEL
            (SETQ CC (CDR CC))
            (COND ((NULL CC) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (CC)
                        (CONS (CAR CC)
                              (SIMP* (APPLY2 FNC (MK*SQ (CDR CC)) EXTRA))))
                      (CAR CC))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (CADDR TAY)
          (COND
           ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
            (SIMP* (APPLY2 FNC (MK*SQ (CADDDR TAY)) EXTRA)))
           (T NIL))
          (CAR (CDDDDR TAY)))) 
(ENDMODULE) 
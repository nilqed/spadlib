(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYDIFF)) 
(EXPORTS (LIST 'DIFFTAYLORWRTTAYVAR)) 
(IMPORTS
 (LIST '*N2F 'DEPENDS 'DIFFSQ 'LASTPAIR 'LDEPENDS 'MULTSQ 'NEGSQ 'NTH 'OVER
       'SIMP* '*TAY2Q '*TAYEXP2Q 'MAKE-CST-COEFFLIS 'MAKE-TAYLOR* 'TAYCFPL
       'TAYCFSQ 'TAYCOEFFLIST 'TAYFLAGS '|TAYLOR:| 'TAYMAKECOEFF 'TAYORIG
       'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER 'TAYTPELPOINT 'TAYTPELVARS
       'TAYVARS 'REPLACE-NTH 'REPLACE-NTH-NTH 'VAR-IS-NTH 'ADDTAYLOR
       'MULTTAYLOR 'MULTTAYLORSQ 'EXPTTAYI)) 
(FLUID '(*TAYLORKEEPORIGINAL)) 
(PUT 'TAYLOR* 'DFFORM 'TAYDIFFP) 
(PUT 'TAYDIFFP 'NUMBER-OF-ARGS 3) 
(PUT 'TAYDIFFP 'DEFINED-ON-LINE '63) 
(PUT 'TAYDIFFP 'DEFINED-IN-FILE 'TAYLOR/TAYDIFF.RED) 
(PUT 'TAYDIFFP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYDIFFP (U V N)
    (CONS
     (LIST
      (CONS
       (GETPOWER
        (FKERN
         ((LAMBDA (UV)
            (COND ((EQUAL N 1) UV)
                  (T
                   (MULTTAYLOR
                    (MULTTAYLORSQ (EXPTTAYI U (DIFFERENCE N 1))
                     (CONS (COND ((ZEROP N) NIL) (T N)) 1))
                    UV))))
          (DIFFTAYLOR U V)))
        1)
       1))
     1)) 
(PUT 'DIFFTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFTAYLOR 'DEFINED-ON-LINE '73) 
(PUT 'DIFFTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYDIFF.RED) 
(PUT 'DIFFTAYLOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFTAYLOR (U KERNEL)
    (PROG (D)
      (SETQ D
              (COND
               ((NOT
                 (LDEPENDS
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X (CADDR U))
                   STARTOVER
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ X (CDR X))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ X (CDR X))
                    (GO LOOPLABEL))
                  KERNEL))
                (LIST 'TAYLOR*
                      (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                        (SETQ CC (CADR U))
                        (COND ((NULL CC) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (CC)
                                            (CONS (CAR CC)
                                                  (DIFFSQ (CDR CC) KERNEL)))
                                          (CAR CC))
                                         NIL)))
                       LOOPLABEL
                        (SETQ CC (CDR CC))
                        (COND ((NULL CC) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CC)
                                    (CONS (CAR CC) (DIFFSQ (CDR CC) KERNEL)))
                                  (CAR CC))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      (CADDR U)
                      (COND
                       ((AND *TAYLORKEEPORIGINAL (CADDDR U))
                        (DIFFSQ (CADDDR U) KERNEL))
                       (T NIL))
                      (CAR (CDDDDR U))))
               (T (DIFFTAYLORWRTTAYVAR U KERNEL))))
      (PROG (EL)
        (SETQ EL (CADDR U))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((DEPENDS (CADR EL) KERNEL)
             (PROG (F)
               (SETQ F (NEGSQ (DIFFSQ (SIMP* (CADR EL)) KERNEL)))
               (PROG (VAR)
                 (SETQ VAR (CAR EL))
                LAB
                 (COND ((NULL VAR) (RETURN NIL)))
                 ((LAMBDA (VAR)
                    (SETQ D
                            (ADDTAYLOR D
                             (MULTTAYLORSQ (DIFFTAYLORWRTTAYVAR U VAR) F))))
                  (CAR VAR))
                 (SETQ VAR (CDR VAR))
                 (GO LAB))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN D))) 
(PUT 'DIFFTAYLORWRTTAYVAR 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFTAYLORWRTTAYVAR 'DEFINED-ON-LINE '97) 
(PUT 'DIFFTAYLORWRTTAYVAR 'DEFINED-IN-FILE 'TAYLOR/TAYDIFF.RED) 
(PUT 'DIFFTAYLORWRTTAYVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFTAYLORWRTTAYVAR (U KERNEL)
    (PROG (V W W1 N M)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ V (CADDR U))
      (SETQ W (VAR-IS-NTH V KERNEL))
      (SETQ N (CAR W))
      (SETQ M (CDR W))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CADR U))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ W (NTH (CAR X) N))
                            (SETQ W1 (NTH W M))
                            (COND ((EQUAL W1 0) NIL)
                                  (T
                                   (LIST
                                    (COND
                                     ((EQ (CADR (NTH V N)) 'INFINITY)
                                      (CONS
                                       (REPLACE-NTH-NTH (CAR X) N M
                                        (TAYEXP-PLUS W1 1))
                                       (MULTSQ (CDR X)
                                               (*TAYEXP2Q (TAYEXP-MINUS W1)))))
                                     (T
                                      (CONS
                                       (REPLACE-NTH-NTH (CAR X) N M
                                        (TAYEXP-DIFFERENCE W1 1))
                                       (MULTSQ (CDR X) (*TAYEXP2Q W1))))))))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ W (NTH (CAR X) N))
                            (SETQ W1 (NTH W M))
                            (COND ((EQUAL W1 0) NIL)
                                  (T
                                   (LIST
                                    (COND
                                     ((EQ (CADR (NTH V N)) 'INFINITY)
                                      (CONS
                                       (REPLACE-NTH-NTH (CAR X) N M
                                        (TAYEXP-PLUS W1 1))
                                       (MULTSQ (CDR X)
                                               (*TAYEXP2Q (TAYEXP-MINUS W1)))))
                                     (T
                                      (CONS
                                       (REPLACE-NTH-NTH (CAR X) N M
                                        (TAYEXP-DIFFERENCE W1 1))
                                       (MULTSQ (CDR X) (*TAYEXP2Q W1))))))))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN
       (LIST 'TAYLOR*
             (COND
              ((NULL W)
               (LIST
                (CONS
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL V)
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
                            ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (CONS NIL 1))))
              (T W))
             (REPLACE-NTH V N
              ((LAMBDA (W1)
                 (LIST (CAR W1) (CADR W1)
                       (COND
                        ((EQ (CADR W1) 'INFINITY) (TAYEXP-PLUS (CADDR W1) 1))
                        (T (TAYEXP-DIFFERENCE (CADDR W1) 1)))
                       (COND
                        ((EQ (CADR W1) 'INFINITY) (TAYEXP-PLUS (CADDDR W1) 1))
                        (T (TAYEXP-DIFFERENCE (CADDDR W1) 1)))))
               (NTH V N)))
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR U)) (DIFFSQ (CADDDR U) KERNEL))
              (T NIL))
             (CAR (CDDDDR U)))))) 
(ENDMODULE) 
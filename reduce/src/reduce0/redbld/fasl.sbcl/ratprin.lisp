(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RATPRIN)) 
(FLUID
 '(*FORT *LIST *MCD *NAT *RATPRI DMODE* YCOORD* YMIN* YMAX* ORIG* PLINE* POSN*
   P**)) 
(GLOBAL '(SPARE*)) 
(SWITCH (LIST (LIST 'EQUAL 'RATPRI 'ON))) 
(PUT 'QUOTIENT 'PRIFN 'QUOTPRI) 
(PUT 'QUOTPRI 'EXPT 'INBRACKETS) 
(PUT 'QUOTPRI 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTPRI 'DEFINED-ON-LINE '53) 
(PUT 'QUOTPRI 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'QUOTPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTPRI (U)
    (PROG (DMODE)
      (COND
       ((OR (NULL *RATPRI) (NULL *NAT) *FORT *LIST (NULL *MCD))
        (RETURN 'FAILED))
       ((FLAGP DMODE* 'RATMODE) (PROGN (SETQ DMODE DMODE*) (SETQ DMODE* NIL))))
      (SETQ U (RATFUNPRI1 U))
      (COND (DMODE (SETQ DMODE* DMODE)))
      (RETURN U))) 
(PUT 'RATFUNPRI1 'NUMBER-OF-ARGS 1) 
(PUT 'RATFUNPRI1 'DEFINED-ON-LINE '65) 
(PUT 'RATFUNPRI1 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'RATFUNPRI1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATFUNPRI1 (U)
    (PROG (X Y CH PLN PLD HEIGHTNUM HEIGHTDEN ORGNUM ORGDEN FL W)
      (SETQ HEIGHTNUM 0)
      (SETQ HEIGHTDEN 0)
      (SETQ ORGNUM 0)
      (SETQ ORGDEN 0)
      (SETQ FL 0)
      (SETQ W 0)
      (SETQ SPARE* (PLUS SPARE* 2))
      (COND
       ((AND (SETQ PLN (LAYOUT-FORMULA (CADR U) 0 NIL))
             (SETQ PLD (LAYOUT-FORMULA (CADDR U) 0 NIL)))
        (PROGN
         (SETQ SPARE* (DIFFERENCE SPARE* 2))
         (SETQ FL (PLUS 2 (MAX (CDAR PLN) (CDAR PLD))))
         (COND
          ((GREATERP FL
                     (DIFFERENCE (DIFFERENCE (LINELENGTH NIL) SPARE*) POSN*))
           (TERPRI* T)))
         (SETQ W (DIFFERENCE (CDAR PLN) (CDAR PLD)))
         (COND
          ((GREATERP W 0) (PROGN (SETQ ORGNUM 0) (SETQ ORGDEN (QUOTIENT W 2))))
          (T (PROGN (SETQ ORGNUM (QUOTIENT (MINUS W) 2)) (SETQ ORGDEN 0))))
         (SETQ HEIGHTNUM (PLUS (DIFFERENCE (CDDR PLN) (CADR PLN)) 1))
         (SETQ HEIGHTDEN (PLUS (DIFFERENCE (CDDR PLD) (CADR PLD)) 1))
         (SETQ PLINE*
                 (APPEND
                  (UPDATE-PLINE (PLUS ORGNUM POSN* (DIFFERENCE 1 ORIG*))
                   (PLUS (DIFFERENCE 1 (CADR PLN)) YCOORD*) (CAAR PLN))
                  (APPEND
                   (UPDATE-PLINE (PLUS ORGDEN POSN* (DIFFERENCE 1 ORIG*))
                    (DIFFERENCE (DIFFERENCE YCOORD* (CDDR PLD)) 1) (CAAR PLD))
                   PLINE*)))
         (SETQ YMIN* (MIN YMIN* (DIFFERENCE YCOORD* HEIGHTDEN)))
         (SETQ YMAX* (MAX YMAX* (PLUS YCOORD* HEIGHTNUM)))
         (SETQ CH (SYMBOL 'BAR))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE FL J)) (RETURN NIL)))
           (PRIN2* CH)
           (SETQ J (PLUS2 J 1))
           (GO LAB))))
       (T
        (PROGN
         (SETQ SPARE* (DIFFERENCE SPARE* 2))
         (SETQ U (CDR U))
         (SETQ X (GET 'QUOTIENT 'INFIX))
         (COND (P** (SETQ Y (GREATERP P** X))) (T (SETQ Y NIL)))
         (COND (Y (PRIN2* "(")))
         (MAPRINT (CAR U) X)
         (OPRIN 'QUOTIENT)
         (MAPRINT (NEGNUMBERCHK (CADR U)) X)
         (COND (Y (PRIN2* ")")))))))) 
(PUT 'LAYOUT-FORMULA 'NUMBER-OF-ARGS 3) 
(PUT 'LAYOUT-FORMULA 'DEFINED-ON-LINE '105) 
(PUT 'LAYOUT-FORMULA 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'LAYOUT-FORMULA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LAYOUT-FORMULA (U P OP)
    (PROG (YCOORD* YMIN* YMAX* POSN* PLINE* TESTING-WIDTH* OVERFLOWED*)
      (SETQ PLINE* (SETQ OVERFLOWED* NIL))
      (SETQ YCOORD* (SETQ YMIN* (SETQ YMAX* 0)))
      (SETQ POSN* ORIG*)
      (SETQ TESTING-WIDTH* T)
      (COND
       (OP
        (PROGN (COND ((EQUAL OP 'INBRACKETS) (PRIN2* "(")) (T (OPRIN OP))))))
      (MAPRINT U P)
      (COND ((EQUAL OP 'INBRACKETS) (PRIN2* ")")))
      (COND (OVERFLOWED* (RETURN NIL))
            (T
             (RETURN
              (CONS (CONS PLINE* (DIFFERENCE POSN* ORIG*))
                    (CONS YMIN* YMAX*))))))) 
(PUT 'UPDATE-PLINE 'NUMBER-OF-ARGS 3) 
(PUT 'UPDATE-PLINE 'DEFINED-ON-LINE '127) 
(PUT 'UPDATE-PLINE 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'UPDATE-PLINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPDATE-PLINE (X Y PLINE)
    (COND ((AND (EQUAL X 0) (EQUAL Y 0)) PLINE)
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J PLINE)
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (J)
                                 (CONS
                                  (CONS
                                   (CONS (IPLUS2 (CAAAR J) X)
                                         (IPLUS2 (CDAAR J) X))
                                   (IPLUS2 (CDAR J) Y))
                                  (CDR J)))
                               (CAR J))
                              NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (J)
                         (CONS
                          (CONS
                           (CONS (IPLUS2 (CAAAR J) X) (IPLUS2 (CDAAR J) X))
                           (IPLUS2 (CDAR J) Y))
                          (CDR J)))
                       (CAR J))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'PRINFIT 'NUMBER-OF-ARGS 3) 
(PUT 'PRINFIT 'DEFINED-ON-LINE '133) 
(PUT 'PRINFIT 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'PRINFIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINFIT (U P OP)
    (PROG (W)
      (COND
       ((OR (NOT *NAT) TESTING-WIDTH*)
        (PROGN (COND (OP (OPRIN OP))) (RETURN (MAPRINT U P)))))
      (SETQ W (LAYOUT-FORMULA U P OP))
      (COND
       ((EQUAL W NIL) (PROGN (COND (OP (OPRIN OP))) (RETURN (MAPRINT U P)))))
      (PUTPLINE W))) 
(PUT 'PUTPLINE 'NUMBER-OF-ARGS 1) 
(PUT 'PUTPLINE 'DEFINED-ON-LINE '148) 
(PUT 'PUTPLINE 'DEFINED-IN-FILE 'MATHPR/RATPRIN.RED) 
(PUT 'PUTPLINE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PUTPLINE (W)
    (PROG ()
      (COND
       ((GREATERP (IPLUS2 POSN* (CDAR W))
                  (IDIFFERENCE (LINELENGTH NIL) SPARE*))
        (TERPRI* T)))
      (SETQ PLINE*
              (APPEND (UPDATE-PLINE (IDIFFERENCE POSN* ORIG*) YCOORD* (CAAR W))
                      PLINE*))
      (SETQ POSN* (IPLUS2 POSN* (CDAR W)))
      (SETQ YMIN* (MIN YMIN* (IPLUS2 (CADR W) YCOORD*)))
      (SETQ YMAX* (MAX YMAX* (IPLUS2 (CDDR W) YCOORD*))))) 
(ENDMODULE) 
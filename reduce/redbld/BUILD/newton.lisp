(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NEWTON)) 
(FLUID '(*NOEQUIV ACCURACY* *INVJACOBI *ROUNDED)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC ERFG*)) 
(PUT 'RDNEWTON0 'NUMBER-OF-ARGS 4) 
(PUT 'RDNEWTON0 'DEFINED-ON-LINE '49) 
(PUT 'RDNEWTON0 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'RDNEWTON0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RDNEWTON0 (E VARS P N)
    ((LAMBDA (*ROUNDBF)
       (PROG (JAC X OLDMODE *NOEQUIV PREC)
         (SETQ PREC 0)
         (COND
          ((NOT (MEMQ DMODE* '(|:RD:| |:CR:|)))
           (PROGN (SETQ OLDMODE T) (SETDMODE 'ROUNDED (SETQ *ROUNDED T)))))
         (SETQ PREC (PRECISION 0))
         (SETQ P
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X P)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (FORCE-TO-DM (CAR (SIMP X))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (FORCE-TO-DM (CAR (SIMP X)))) (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND (*TRNUMERIC (LPRIM "computing symbolic Jacobian")))
         (EVAL (LIST 'MATRIX (MKQUOTE (LIST (LIST 'JACOBIAN N N)))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
             (SETMATELEM (LIST 'JACOBIAN I J)
              (REVAL1 (LIST 'DF (NTH E I) (NTH VARS J)) T))
             (SETQ J (PLUS2 J 1))
             (GO LAB))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((AND *TRNUMERIC *INVJACOBI) (LPRIM "inverting symbolic Jacobian")))
         (SETQ JAC
                 (CDR
                  (REVAL1
                   (COND (*INVJACOBI '(QUOTIENT 1 JACOBIAN)) (T 'JACOBIAN))
                   T)))
         (SETQ JAC
                 (PROG (R FORALL-RESULT FORALL-ENDPTR)
                   (SETQ R JAC)
                   (COND ((NULL R) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (R)
                                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ C R)
                                         (COND ((NULL C) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (C)
                                                             (REVAL1 C T))
                                                           (CAR C))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ C (CDR C))
                                         (COND
                                          ((NULL C) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (C) (REVAL1 C T))
                                                   (CAR C))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR R))
                                    NIL)))
                  LOOPLABEL
                   (SETQ R (CDR R))
                   (COND ((NULL R) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (R)
                               (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ C R)
                                 (COND ((NULL C) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (C) (REVAL1 C T))
                                                   (CAR C))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ C (CDR C))
                                 (COND ((NULL C) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (C) (REVAL1 C T)) (CAR C))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR R))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ *NOEQUIV T)
         (SETQ X (RDNEWTON1 E JAC VARS P 'ROOT))
         (COND (OLDMODE (SETDMODE 'ROUNDED (SETQ *ROUNDED NIL))))
         (PRECISION PREC)
         (COND ((NULL X) (REDERR "no solution found")))
         (RETURN
          (CONS 'LIST
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P (PAIR VARS X))
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P))) (CAR P))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))))
     *ROUNDBF)) 
(PUT 'RDNEWTON1 'NUMBER-OF-ARGS 5) 
(PUT 'RDNEWTON1 'DEFINED-ON-LINE '79) 
(PUT 'RDNEWTON1 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'RDNEWTON1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RDNEWTON1 (F JAC VARS X MODE)
    (PROG (R ACC)
      (COND (*TRNUMERIC (LPRIM "starting Newton iteration")))
      (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
      (SETQ R (RDNEWTON2 F JAC VARS ACC X MODE NIL NIL))
      (SETQ R
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X R)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (PREPF X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (PREPF X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN R))) 
(PUT 'RDNEWTON2 'NUMBER-OF-ARGS 8) 
(PUT 'RDNEWTON2 'DEFINED-ON-LINE '88) 
(PUT 'RDNEWTON2 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'RDNEWTON2 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE RDNEWTON2 (F JAC VARS ACC X MODE LOW HIGH)
    (PROG (N0 N1 E0 E1 DX DX2 X1 G DMP DELTA H DXOLD DX2OLD DMPOLD COUNT)
      (SETQ COUNT 0)
      (SETQ ACC (ROUNDED-FLOAT ACC))
      (COND (*TRNUMERIC (LPRIM "Newton iteration")))
      (SETQ MODE NIL)
      (COND (*TRNUMERIC (LPRIM "evaluate function at the initial point")))
      (SETQ E0 (LIST-EVALUATE F VARS X))
     LOOP
      (SETQ COUNT (ADD1 COUNT))
      (COND
       ((GREATERP COUNT ITERATIONS*)
        (PROGN
         (LPRIM "requested accuracy not reached within iteration limit")
         (GO READY))))
      (COND (*TRNUMERIC (LPRIM "evaluate Jacobian (or its inverse)")))
      (SETQ G (MATRIX-EVALUATE JAC VARS X))
      (COND (*TRNUMERIC (LPRIM "compute the next point")))
      (SETQ DX (COND (*INVJACOBI (MAT*LIST G E0)) (T (RDSOLVELIN G E0))))
      (COND ((NULL DX) (GO JACERR)))
      (SETQ N0 (MAX_ABS_NUMBER DX))
      (SETQ DMP 1)
     STEP
      (SETQ X1 (LIST-LIST X (SCAL*LIST DMP DX)))
      (COND (*TRNUMERIC (LPRIM "evaluate function at the next point")))
      ((LAMBDA (*MSG *PROTFG)
         (SETQ E1
                 (ERRORSET
                  (LIST 'LIST-EVALUATE (MKQUOTE F) (MKQUOTE VARS) (MKQUOTE X1))
                  NIL NIL)))
       NIL T)
      (COND ((ERRORP E1) (GO CONTRACT)) (T (SETQ E1 (CAR E1))))
      (COND (*TRNUMERIC (LPRIM "compute the point difference")))
      (SETQ DX2 (COND (*INVJACOBI (MAT*LIST G E1)) (T (RDSOLVELIN G E1))))
      (COND ((NULL DX2) (GO CONTRACT)))
      (COND (*TRNUMERIC (LPRIM "compute the size of the point difference")))
      (SETQ N1 (MAX_ABS_NUMBER DX2))
      (COND ((OR (EQUAL N1 0) (LESSP N1 N0)) (GO ACCEPT)))
      (COND ((NULL DMPOLD) (GO CONTRACT)))
      (SETQ H
              (TIMES DMPOLD
                     (QUOTIENT
                      (TIMES (MAX_ABS_NUMBER (LIST-LIST DX2OLD DX))
                             (MAX_ABS_NUMBER DX))
                      (TIMES (MAX_ABS_NUMBER DXOLD) (MAX_ABS_NUMBER DX2OLD)))))
      (COND
       ((GREATERP H 1)
        (PROGN
         (SETQ DMP (COND ((LESSP H 10) (QUOTIENT 1 H)) (T 0.1)))
         (SETQ DMPOLD NIL)
         (GO STEP))))
     CONTRACT
      (COND
       (*TRNUMERIC
        (LPRIM "reduce the difference limit iteration to its half")))
      (SETQ DMP (TIMES DMP 0.5))
      (COND ((LESSP DMP ACC) (REDERR "Newton method does not converge")))
      (GO STEP)
     ACCEPT
      (SETQ DELTA (CONS '|:RD:| (TIMES DMP N0)))
      (SETQ X X1)
      (SETQ E0 E1)
      (SETQ N0 N1)
      (COND
       ((AND LOW HIGH (OR (GREATERP LOW (CAR X)) (LESSP HIGH (CAR X))))
        (RETURN NIL)))
      (SETQ DMPOLD DMP)
      (SETQ DXOLD DX)
      (SETQ DX2OLD DX2)
      (RDNEWTONPRINTPOINT COUNT X DELTA E0)
      (COND
       ((OR (GREATERP N1 ACC) (LESSP DMP 1))
        (PROGN (UPDATE-PRECISION (CONS DELTA E0)) (GO LOOP))))
     READY
      (SETQ X (LIST-LIST X DX2))
      (RETURN X)
     JACERR
      (REDERR "singular Jacobian"))) 
(PUT 'RDNEWTONPRINTPOINT 'NUMBER-OF-ARGS 4) 
(PUT 'RDNEWTONPRINTPOINT 'DEFINED-ON-LINE '179) 
(PUT 'RDNEWTONPRINTPOINT 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'RDNEWTONPRINTPOINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RDNEWTONPRINTPOINT (COUNT X DX E0)
    (COND
     (*TRNUMERIC
      (PROG ()
        (WRITEPRI COUNT 'FIRST)
        (WRITEPRI ". residue=" NIL)
        (PRINTSFLIST E0)
        (WRITEPRI ", step length=" NIL)
        (WRITEPRI (MKQUOTE (PREPF DX)) 'LAST)
        (WRITEPRI " at " NIL)
        (PRINTSFLIST X)
        (WRITEPRI " " 'LAST))))) 
(PUT 'MAX_ABS_NUMBER 'NUMBER-OF-ARGS 1) 
(PUT 'MAX_ABS_NUMBER 'DEFINED-ON-LINE '195) 
(PUT 'MAX_ABS_NUMBER 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'MAX_ABS_NUMBER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAX_ABS_NUMBER (V)
    (PROG (M Y)
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ Y (POSITIVE-ROUNDED-FLOAT X))
            (COND
             ((NULL Y)
              (PROGN
               (COND
                (*MSG
                 (PROGN
                  (WRITEPRI "***** max_abs_number, test:" 'ONLY)
                  (WRITEPRI (MKQUOTE (MKQUOTE X)) 'ONLY)
                  (WRITEPRI "***** objects:" 'ONLY)
                  (PROG (Z)
                    (SETQ Z V)
                   LAB
                    (COND ((NULL Z) (RETURN NIL)))
                    ((LAMBDA (Z) (WITEPRI (MKQUOTE (MKQUOTE Z)) 'ONLY))
                     (CAR Z))
                    (SETQ Z (CDR Z))
                    (GO LAB)))))
               (REDERR "compute the (positive) maximum of numbers"))))
            (COND ((NULL M) (SETQ M Y)) ((GREATERP Y M) (SETQ M Y)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN M))) 
(PUT 'POSITIVE-ROUNDED-FLOAT 'NUMBER-OF-ARGS 1) 
(PUT 'POSITIVE-ROUNDED-FLOAT 'DEFINED-ON-LINE '216) 
(PUT 'POSITIVE-ROUNDED-FLOAT 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'POSITIVE-ROUNDED-FLOAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POSITIVE-ROUNDED-FLOAT (X)
    (PROGN
     (SETQ X (ROUNDED-FLOAT X))
     (COND ((NULL X) X) ((LESSP X 0) (MINUS X)) (T X)))) 
(PUT 'ROUNDED-FLOAT 'NUMBER-OF-ARGS 1) 
(PUT 'ROUNDED-FLOAT 'DEFINED-ON-LINE '220) 
(PUT 'ROUNDED-FLOAT 'DEFINED-IN-FILE 'NUMERIC/NEWTON.RED) 
(PUT 'ROUNDED-FLOAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROUNDED-FLOAT (X)
    (COND ((NULL X) 0.0)
          (T
           (PROG (Y Z)
             (COND ((AND (PAIRP X) (EQ (CAR X) '|:RD:|)) (SETQ X (CDR X))))
             (COND ((FLOATP X) T) ((FIXP X) (SETQ X (FLOAT X)))
                   ((PAIRP X)
                    (PROGN
                     (SETQ Y (CAR X))
                     (SETQ Z (CDR X))
                     (COND
                      ((AND (NUMBERP Y) (NUMBERP (CDR X)))
                       (SETQ X
                               (COND ((GREATERP Z 100) 1.0e100)
                                     (T (TIMES Y (EXPT 2.0 Z))))))
                      ((EQ Y '|:CR:|)
                       (SETQ X
                               (MAX (POSITIVE-ROUNDED-FLOAT (CADR X))
                                    (POSITIVE-ROUNDED-FLOAT (CDDR X)))))
                      ((EQ Y '*SQ)
                       (SETQ X
                               (QUOTIENT (POSITIVE-ROUNDED-FLOAT (CAADR X))
                                         (POSITIVE-ROUNDED-FLOAT (CDADR X)))))
                      (T (SETQ X NIL))))))
             (RETURN X))))) 
(ENDMODULE) 
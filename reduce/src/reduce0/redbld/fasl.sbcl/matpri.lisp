(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATPRI)) 
(FLUID '(*NAT OBRKP* ORIG* PLINE* POSN* YCOORD* YMAX* YMIN*)) 
(PUT 'SETMATPRI 'NUMBER-OF-ARGS 2) 
(PUT 'SETMATPRI 'DEFINED-ON-LINE '33) 
(PUT 'SETMATPRI 'DEFINED-IN-FILE 'MATRIX/MATPRI.RED) 
(PUT 'SETMATPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETMATPRI (U V) (MATPRI1 (CDR V) U)) 
(PUT 'MAT 'SETPRIFN 'SETMATPRI) 
(PUT 'MATPRI 'NUMBER-OF-ARGS 1) 
(PUT 'MATPRI 'DEFINED-ON-LINE '38) 
(PUT 'MATPRI 'DEFINED-IN-FILE 'MATRIX/MATPRI.RED) 
(PUT 'MATPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATPRI (U)
    (COND ((NULL (CDR U)) (REDERR "Empty matrix")) (T (MATPRI1 (CDR U) NIL)))) 
(PUT 'MATPRI1 'NUMBER-OF-ARGS 2) 
(PUT 'MATPRI1 'DEFINED-ON-LINE '42) 
(PUT 'MATPRI1 'DEFINED-IN-FILE 'MATRIX/MATPRI.RED) 
(PUT 'MATPRI1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATPRI1 (U X)
    (PROG (M N R L W E LL OK NAME NW WIDTHS FIRSTFLAG TOPROW LBAR RBAR
           REALORIG)
      (COND
       (*FORT
        (PROGN
         (SETQ M 1)
         (COND ((NULL X) (SETQ X "MAT")))
         (PROG (Y)
           (SETQ Y U)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROGN
               (SETQ N 1)
               (PROG (Z)
                 (SETQ Z Y)
                LAB
                 (COND ((NULL Z) (RETURN NIL)))
                 ((LAMBDA (Z)
                    (PROGN
                     (ASSGNPRI Z (LIST (LIST X M N)) 'ONLY)
                     (SETQ N (PLUS N 1))))
                  (CAR Z))
                 (SETQ Z (CDR Z))
                 (GO LAB))
               (SETQ M (PLUS M 1))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (RETURN NIL))))
      (TERPRI* T)
      (COND
       ((AND X *NAT)
        (PROGN
         (SETQ NAME (LAYOUT-FORMULA X 0 NIL))
         (COND (NAME (PROGN (SETQ NW (PLUS (CDAR NAME) 4)) (SETQ OK *NAT))))))
       (T (PROGN (SETQ NW 0) (SETQ OK *NAT))))
      (SETQ LL
              (DIFFERENCE
               (DIFFERENCE (DIFFERENCE (LINELENGTH NIL) SPARE*) ORIG*) NW))
      (SETQ M (LENGTH (CAR U)))
      (SETQ WIDTHS (MKVECT (PLUS 1 M)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
        (PUTV WIDTHS I 1)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ REALORIG ORIG*)
      (SETQ ORIG* 0)
      (COND
       (OK
        (PROG (Y)
          (SETQ Y U)
         LAB
          (COND ((NULL Y) (RETURN NIL)))
          ((LAMBDA (Y)
             (PROGN
              (SETQ N 1)
              (SETQ L NIL)
              (SETQ W 0)
              (COND
               (OK
                (PROG (Z)
                  (SETQ Z Y)
                 LAB
                  (COND ((NULL Z) (RETURN NIL)))
                  ((LAMBDA (Z)
                     (COND
                      (OK
                       (PROGN
                        (SETQ E (LAYOUT-FORMULA Z 0 NIL))
                        (COND ((NULL E) (SETQ OK NIL))
                              (T
                               (PROG (COL)
                                 (SETQ COL (MAX (GETV WIDTHS N) (CDAR E)))
                                 (COND
                                  ((GREATERP (SETQ W (PLUS W COL 2)) LL)
                                   (SETQ OK NIL))
                                  (T
                                   (PROGN
                                    (SETQ L (CONS E L))
                                    (PUTV WIDTHS N COL)))))))
                        (SETQ N (PLUS N 1))))))
                   (CAR Z))
                  (SETQ Z (CDR Z))
                  (GO LAB))))
              (SETQ R (CONS (REVERSE L) R))))
           (CAR Y))
          (SETQ Y (CDR Y))
          (GO LAB))))
      (COND
       (OK
        (PROGN
         (SETQ FIRSTFLAG (SETQ TOPROW T))
         (SETQ R
                 (PROG (PY FORALL-RESULT FORALL-ENDPTR)
                   (SETQ PY (REVERSE R))
                   (COND ((NULL PY) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (PROG (Y YMIN YMAX POS PL K W)
                                      (SETQ YMIN (SETQ YMAX 0))
                                      (SETQ POS 1)
                                      (SETQ K 1)
                                      (SETQ PL NIL)
                                      (SETQ Y (CAR PY))
                                      (PROG (Z)
                                        (SETQ Z Y)
                                       LAB
                                        (COND ((NULL Z) (RETURN NIL)))
                                        ((LAMBDA (Z)
                                           (PROGN
                                            (SETQ W (GETV WIDTHS K))
                                            (SETQ PL
                                                    (APPEND
                                                     (UPDATE-PLINE
                                                      (PLUS POS
                                                            (QUOTIENT
                                                             (DIFFERENCE W
                                                                         (CDAR
                                                                          Z))
                                                             2))
                                                      0 (CAAR Z))
                                                     PL))
                                            (SETQ POS (PLUS POS W 2))
                                            (SETQ K (PLUS K 1))
                                            (SETQ YMIN (MIN YMIN (CADR Z)))
                                            (SETQ YMAX (MAX YMAX (CDDR Z)))))
                                         (CAR Z))
                                        (SETQ Z (CDR Z))
                                        (GO LAB))
                                      (SETQ K NIL)
                                      (COND (FIRSTFLAG (SETQ FIRSTFLAG NIL))
                                            (T (SETQ YMAX (PLUS YMAX 1))))
                                      (PROG (H)
                                        (SETQ H YMAX)
                                       LAB
                                        (COND
                                         ((MINUSP
                                           (TIMES (MINUS 1)
                                                  (DIFFERENCE YMIN H)))
                                          (RETURN NIL)))
                                        (PROGN
                                         (COND
                                          (TOPROW
                                           (PROGN
                                            (SETQ LBAR (SYMBOL 'MAT-TOP-L))
                                            (SETQ RBAR (SYMBOL 'MAT-TOP-R))
                                            (SETQ TOPROW NIL)))
                                          ((AND (EQUAL H YMIN) (NULL (CDR PY)))
                                           (PROGN
                                            (SETQ LBAR (SYMBOL 'MAT-LOW-L))
                                            (SETQ RBAR (SYMBOL 'MAT-LOW-R))))
                                          (T
                                           (PROGN
                                            (SETQ LBAR (SYMBOL 'MAT-MID-L))
                                            (SETQ RBAR (SYMBOL 'MAT-MID-R)))))
                                         (SETQ PL
                                                 (CONS
                                                  (CONS
                                                   (CONS
                                                    (CONS (DIFFERENCE POS 2)
                                                          (DIFFERENCE POS 1))
                                                    H)
                                                   RBAR)
                                                  PL))
                                         (SETQ K
                                                 (CONS
                                                  (CONS (CONS (CONS 0 1) H)
                                                        LBAR)
                                                  K)))
                                        (SETQ H (PLUS2 H (MINUS 1)))
                                        (GO LAB))
                                      (RETURN
                                       (CONS (CONS (APPEND PL K) POS)
                                             (CONS YMIN YMAX))))
                                    NIL)))
                  LOOPLABEL
                   (SETQ PY (CDR PY))
                   (COND ((NULL PY) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (PROG (Y YMIN YMAX POS PL K W)
                              (SETQ YMIN (SETQ YMAX 0))
                              (SETQ POS 1)
                              (SETQ K 1)
                              (SETQ PL NIL)
                              (SETQ Y (CAR PY))
                              (PROG (Z)
                                (SETQ Z Y)
                               LAB
                                (COND ((NULL Z) (RETURN NIL)))
                                ((LAMBDA (Z)
                                   (PROGN
                                    (SETQ W (GETV WIDTHS K))
                                    (SETQ PL
                                            (APPEND
                                             (UPDATE-PLINE
                                              (PLUS POS
                                                    (QUOTIENT
                                                     (DIFFERENCE W (CDAR Z))
                                                     2))
                                              0 (CAAR Z))
                                             PL))
                                    (SETQ POS (PLUS POS W 2))
                                    (SETQ K (PLUS K 1))
                                    (SETQ YMIN (MIN YMIN (CADR Z)))
                                    (SETQ YMAX (MAX YMAX (CDDR Z)))))
                                 (CAR Z))
                                (SETQ Z (CDR Z))
                                (GO LAB))
                              (SETQ K NIL)
                              (COND (FIRSTFLAG (SETQ FIRSTFLAG NIL))
                                    (T (SETQ YMAX (PLUS YMAX 1))))
                              (PROG (H)
                                (SETQ H YMAX)
                               LAB
                                (COND
                                 ((MINUSP
                                   (TIMES (MINUS 1) (DIFFERENCE YMIN H)))
                                  (RETURN NIL)))
                                (PROGN
                                 (COND
                                  (TOPROW
                                   (PROGN
                                    (SETQ LBAR (SYMBOL 'MAT-TOP-L))
                                    (SETQ RBAR (SYMBOL 'MAT-TOP-R))
                                    (SETQ TOPROW NIL)))
                                  ((AND (EQUAL H YMIN) (NULL (CDR PY)))
                                   (PROGN
                                    (SETQ LBAR (SYMBOL 'MAT-LOW-L))
                                    (SETQ RBAR (SYMBOL 'MAT-LOW-R))))
                                  (T
                                   (PROGN
                                    (SETQ LBAR (SYMBOL 'MAT-MID-L))
                                    (SETQ RBAR (SYMBOL 'MAT-MID-R)))))
                                 (SETQ PL
                                         (CONS
                                          (CONS
                                           (CONS
                                            (CONS (DIFFERENCE POS 2)
                                                  (DIFFERENCE POS 1))
                                            H)
                                           RBAR)
                                          PL))
                                 (SETQ K
                                         (CONS (CONS (CONS (CONS 0 1) H) LBAR)
                                               K)))
                                (SETQ H (PLUS2 H (MINUS 1)))
                                (GO LAB))
                              (RETURN
                               (CONS (CONS (APPEND PL K) POS)
                                     (CONS YMIN YMAX))))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ ORIG* REALORIG)
         (SETQ W 0)
         (PROG (Y)
           (SETQ Y R)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y) (SETQ W (PLUS W (DIFFERENCE (CDDR Y) (CADR Y)) 1)))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (SETQ N (QUOTIENT W 2))
         (SETQ U NIL)
         (PROG (Y)
           (SETQ Y R)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROGN
               (SETQ U
                       (APPEND
                        (UPDATE-PLINE 0 (DIFFERENCE N (CDDR Y)) (CAAR Y)) U))
               (SETQ N
                       (DIFFERENCE N
                                   (PLUS (DIFFERENCE (CDDR Y) (CADR Y)) 1)))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (COND (X (PROGN (MAPRIN X) (OPRIN 'SETQ))))
         (SETQ PLINE* (APPEND (UPDATE-PLINE POSN* YCOORD* U) PLINE*))
         (SETQ YMAX* (MAX (PLUS YCOORD* (QUOTIENT W 2)) YMAX*))
         (SETQ YMIN* (MIN (PLUS YCOORD* (DIFFERENCE (QUOTIENT W 2) W)) YMIN*))
         (TERPRI* (NOT *NAT))))
       (T (PROGN (COND (X (PROGN (MAPRIN X) (OPRIN 'SETQ)))) (MATPRI2 U)))))) 
(PUT 'MATPRI2 'NUMBER-OF-ARGS 1) 
(PUT 'MATPRI2 'DEFINED-ON-LINE '146) 
(PUT 'MATPRI2 'DEFINED-IN-FILE 'MATRIX/MATPRI.RED) 
(PUT 'MATPRI2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATPRI2 (U)
    (PROG (Y)
      (PRIN2* 'MAT)
      (PRIN2* "(")
      (SETQ OBRKP* NIL)
      (SETQ Y ORIG*)
      (SETQ ORIG* (COND ((LESSP POSN* 18) POSN*) (T (PLUS ORIG* 3))))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (PRIN2* "(")
         (SETQ ORIG* (PLUS ORIG* 1))
         (INPRINT '*COMMA* 0 (CAR U))
         (PRIN2* ")")
         (COND
          ((CDR U)
           (PROGN
            (OPRIN '*COMMA*)
            (SETQ ORIG* (DIFFERENCE ORIG* 1))
            (TERPRI* *NAT))))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (SETQ OBRKP* T)
      (SETQ ORIG* Y)
      (PRIN2* ")")
      (COND ((NULL *NAT) (PRIN2* "$")))
      (TERPRI* T))) 
(ENDMODULE) 
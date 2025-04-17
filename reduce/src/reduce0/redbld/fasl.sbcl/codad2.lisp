(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODAD2)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL '(ROWMIN ROWMAX)) 
(DE FIND+VAR (VAR FA IV) (GETCIND VAR 'VARLST+ 'PLUS FA IV)) 
(PUT 'FIND+VAR 'NUMBER-OF-ARGS 3) 
(PUT 'FIND+VAR 'DEFINED-ON-LINE '114) 
(PUT 'FIND+VAR 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'FIND+VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FIND+VAR 'INLINE
      '(LAMBDA (VAR FA IV) (GETCIND VAR 'VARLST+ 'PLUS FA IV))) 
(DE FIND*VAR (VAR FA IV) (GETCIND VAR 'VARLST* 'TIMES FA IV)) 
(PUT 'FIND*VAR 'NUMBER-OF-ARGS 3) 
(PUT 'FIND*VAR 'DEFINED-ON-LINE '117) 
(PUT 'FIND*VAR 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'FIND*VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FIND*VAR 'INLINE
      '(LAMBDA (VAR FA IV) (GETCIND VAR 'VARLST* 'TIMES FA IV))) 
(PUT 'GETCIND 'NUMBER-OF-ARGS 5) 
(PUT 'GETCIND 'DEFINED-ON-LINE '120) 
(PUT 'GETCIND 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'GETCIND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETCIND (VAR VARLST OP FA IV)
    (PROG (Y Z)
      (COND
       ((NULL (SETQ Y (GET VAR VARLST)))
        (PROGN
         (SETQ Y (SETQ ROWMIN (DIFFERENCE ROWMIN 1)))
         (PUT VAR VARLST Y)
         (SETROW Y OP VAR NIL NIL))))
      (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4
            (INSZZZN
             (SETQ Z
                     (COND ((OR (IDP IV) (CONSTP IV)) (CONS FA (CONS IV NIL)))
                           (T (CONS FA IV))))
             (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4)))
      (RETURN
       (COND ((OR (IDP (CDR Z)) (CONSTP (CDR Z))) (CONS Y (CONS (CDR Z) NIL)))
             (T (CONS Y (CDR Z))))))) 
(PUT 'POWEROFSUMS 'NUMBER-OF-ARGS 0) 
(PUT 'POWEROFSUMS 'DEFINED-ON-LINE '149) 
(PUT 'POWEROFSUMS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'POWEROFSUMS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE POWEROFSUMS NIL
    (PROG (VAR Z RMAX)
      (SETQ RMAX ROWMAX)
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE RMAX X)) (RETURN NIL)))
        (COND
         ((AND (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
               (GREATERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 6) 1)
               (NOT (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1))))
          (PROGN
           (SETQ VAR (FNEWSYM))
           (SETROW (SETQ ROWMAX (PLUS ROWMAX 1)) 'PLUS VAR
            (LIST (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
            (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
           (PUT VAR 'ROWINDEX ROWMAX)
           (PROG (Z)
             (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
            LAB
             (COND ((NULL Z) (RETURN NIL)))
             ((LAMBDA (Z)
                (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                      (CONS
                       ((LAMBDA (G163)
                          (COND
                           ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                            (CONS G163 (CONS (CDR Z) NIL)))
                           (T (CONS G163 (CDR Z)))))
                        ROWMAX)
                       (DELYZZ X
                        (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4)))))
              (CAR Z))
             (SETQ Z (CDR Z))
             (GO LAB))
           (PROG (CH)
             (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
            LAB
             (COND ((NULL CH) (RETURN NIL)))
             ((LAMBDA (CH) (PUTV (GETV CODMAT (PLUS MAXVAR CH)) 3 ROWMAX))
              (CAR CH))
             (SETQ CH (CDR CH))
             (GO LAB))
           (SETPREV X ROWMAX)
           (SETROW (SETQ ROWMAX (PLUS ROWMAX 1)) 'TIMES X (LIST NIL)
            (LIST
             (SETQ Z
                     ((LAMBDA (G165 G166)
                        (COND
                         ((OR (IDP G166) (CONSTP G166))
                          (CONS G165 (CONS G166 NIL)))
                         (T (CONS G165 G166))))
                      (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
                      (GETV (GETV CODMAT (PLUS MAXVAR X)) 6)))))
           (SETROW ROWMIN 'TIMES VAR NIL
            (LIST
             ((LAMBDA (G167)
                (COND
                 ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                  (CONS G167 (CONS (CDR Z) NIL)))
                 (T (CONS G167 (CDR Z)))))
              ROWMAX)))
           (PUTV (GETV CODMAT (PLUS MAXVAR X)) 5 (LIST ROWMAX))
           (PUT VAR 'VARLST* ROWMIN)
           (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 NIL)
           (PUTV (GETV CODMAT (PLUS MAXVAR X)) 6 1))))
        (SETQ X (PLUS2 X 1))
        (GO LAB)))) 
(PUT 'REMREPMULTVARS 'NUMBER-OF-ARGS 0) 
(PUT 'REMREPMULTVARS 'DEFINED-ON-LINE '200) 
(PUT 'REMREPMULTVARS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMREPMULTVARS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REMREPMULTVARS NIL
    (PROG (RMIN VAR PRODCOLI PCOLINF MMULT SROWS TCOLINF RINDX NVAR Z ZZ ZZ1)
      (SETQ RMIN ROWMIN)
      (PROG (Y)
        (SETQ Y RMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND
           (NOT (NUMBERP (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3))))
           (NEQ VAR '+ONE) (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'PLUS))
          (PROGN
           (SETQ PRODCOLI (GET VAR 'VARLST*))
           (SETQ PCOLINF NIL)
           (PROG (Z)
             (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
            LAB
             (COND ((NULL Z) (RETURN NIL)))
             ((LAMBDA (Z)
                (COND
                 ((NOT (|:ONEP| (DM-ABS (CAR (CDR Z)))))
                  (SETQ PCOLINF
                          (INSPCVV
                           (CONS (CAR Z)
                                 (COND ((|:MINUSP| (CAR (CDR Z))) (MINUS 1))
                                       (T 1)))
                           (DM-ABS (CAR (CDR Z))) PCOLINF)))))
              (CAR Z))
             (SETQ Z (CDR Z))
             (GO LAB))
           (PROG (CSEINFO)
             (SETQ CSEINFO PCOLINF)
            LAB
             (COND ((NULL CSEINFO) (RETURN NIL)))
             ((LAMBDA (CSEINFO)
                (PROGN
                 (SETQ MMULT (CAR CSEINFO))
                 (SETQ SROWS (CDR CSEINFO))
                 (SETQ TCOLINF NIL)
                 (COND
                  (PRODCOLI
                   (PROG (Z)
                     (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR PRODCOLI)) 4))
                    LAB
                     (COND ((NULL Z) (RETURN NIL)))
                     ((LAMBDA (Z)
                        (PROGN
                         (SETQ RINDX (CAR Z))
                         (COND
                          ((DM-EQ
                            (DM-ABS (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 6))
                            MMULT)
                           (SETQ TCOLINF (CONS (CONS RINDX Z) TCOLINF))))))
                      (CAR Z))
                     (SETQ Z (CDR Z))
                     (GO LAB))))
                 (COND
                  ((GREATERP (PLUS (LENGTH SROWS) (LENGTH TCOLINF)) 1)
                   (PROGN
                    (SETQ Z
                            (COND
                             ((OR (IDP MMULT) (CONSTP MMULT))
                              (CONS Y (CONS MMULT NIL)))
                             (T (CONS Y MMULT))))
                    (SETQ NVAR (FNEWSYM))
                    (SETQ ROWMAX (PLUS ROWMAX 1))
                    (SETROW ROWMAX 'PLUS NVAR (LIST NIL) (LIST Z))
                    (PUT NVAR 'ROWINDEX ROWMAX)
                    (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
                    (SETQ ZZ NIL)
                    (PROG (ROWINF)
                      (SETQ ROWINF SROWS)
                     LAB
                      (COND ((NULL ROWINF) (RETURN NIL)))
                      ((LAMBDA (ROWINF)
                         (PROGN
                          (SETQ RINDX (CAR ROWINF))
                          (SETQ ZZ
                                  (CONS
                                   (COND
                                    ((OR (IDP (CDR ROWINF))
                                         (CONSTP (CDR ROWINF)))
                                     (CONS RINDX (CONS (CDR ROWINF) NIL)))
                                    (T (CONS RINDX (CDR ROWINF))))
                                   ZZ))
                          (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 4
                                (CONS
                                 ((LAMBDA (G169 G170)
                                    (COND
                                     ((OR (IDP G170) (CONSTP G170))
                                      (CONS G169 (CONS G170 NIL)))
                                     (T (CONS G169 G170))))
                                  ROWMIN (CDR (CAR ZZ)))
                                 (DELYZZ Y
                                  (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 4))))
                          (SETPREV RINDX ROWMAX)))
                       (CAR ROWINF))
                      (SETQ ROWINF (CDR ROWINF))
                      (GO LAB))
                    (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4
                          (CONS
                           ((LAMBDA (G171)
                              (COND
                               ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                                (CONS G171 (CONS (CDR Z) NIL)))
                               (T (CONS G171 (CDR Z)))))
                            ROWMAX)
                           (REMZZZZ ZZ
                            (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))))
                    (SETROW ROWMIN 'PLUS NVAR NIL ZZ)
                    (PUT NVAR 'VARLST+ ROWMIN)
                    (COND
                     (TCOLINF
                      (PROGN
                       (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
                       (SETQ ZZ1 (SETQ ZZ NIL))
                       (PROG (ROWINF)
                         (SETQ ROWINF TCOLINF)
                        LAB
                         (COND ((NULL ROWINF) (RETURN NIL)))
                         ((LAMBDA (ROWINF)
                            (PROGN
                             (SETQ RINDX (CAR ROWINF))
                             (SETQ Z (CDR ROWINF))
                             (SETQ ZZ
                                     (CONS
                                      (COND
                                       ((OR (IDP 1) (CONSTP 1))
                                        (CONS RINDX (CONS 1 NIL)))
                                       (T (CONS RINDX 1)))
                                      ZZ))
                             (COND
                              ((GREATERP (CAR (CDR Z)) 1)
                               (RPLACA (CDR Z) (DIFFERENCE (CAR (CDR Z)) 1)))
                              (T
                               (PROGN
                                (SETQ ZZ1 (CONS (CAR ZZ) ZZ1))
                                (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 4
                                      (DELYZZ PRODCOLI
                                       (GETV (GETV CODMAT (PLUS MAXVAR RINDX))
                                             4))))))
                             (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 4
                                   (CONS
                                    ((LAMBDA (G173 G174)
                                       (COND
                                        ((OR (IDP G174) (CONSTP G174))
                                         (CONS G173 (CONS G174 NIL)))
                                        (T (CONS G173 G174))))
                                     ROWMIN (CDR (CAR ZZ)))
                                    (GETV (GETV CODMAT (PLUS MAXVAR RINDX))
                                          4)))
                             (SETPREV RINDX ROWMAX)
                             (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 6
                                   (DM-QUOTIENT
                                    (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 6)
                                    MMULT))))
                          (CAR ROWINF))
                         (SETQ ROWINF (CDR ROWINF))
                         (GO LAB))
                       (PUTV (GETV CODMAT (PLUS MAXVAR PRODCOLI)) 4
                             (REMZZZZ ZZ1
                              (GETV (GETV CODMAT (PLUS MAXVAR PRODCOLI)) 4)))
                       (SETROW ROWMIN 'TIMES NVAR NIL ZZ)
                       (PUT NVAR 'VARLST* ROWMIN)))))))))
              (CAR CSEINFO))
             (SETQ CSEINFO (CDR CSEINFO))
             (GO LAB)))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB)))) 
(PUT 'UPDATEMONOMIALS 'NUMBER-OF-ARGS 0) 
(PUT 'UPDATEMONOMIALS 'DEFINED-ON-LINE '327) 
(PUT 'UPDATEMONOMIALS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'UPDATEMONOMIALS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE UPDATEMONOMIALS NIL
    (PROG (Y)
      (SETQ Y ROWMIN)
     LAB
      (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
      (COND
       ((AND (NOT (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3)))
             (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'TIMES))
        (PROG (GCEL)
          (SETQ GCEL (MKGCLSTMON Y))
         LAB
          (COND ((NULL GCEL) (RETURN NIL)))
          ((LAMBDA (GCEL) (REMGCMON GCEL Y)) (CAR GCEL))
          (SETQ GCEL (CDR GCEL))
          (GO LAB))))
      (SETQ Y (PLUS2 Y 1))
      (GO LAB))) 
(PUT 'MKGCLSTMON 'NUMBER-OF-ARGS 1) 
(PUT 'MKGCLSTMON 'DEFINED-ON-LINE '340) 
(PUT 'MKGCLSTMON 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'MKGCLSTMON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKGCLSTMON (Y)
    (PROG (GCLST COF INDXSGN)
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((NOT
              (|:ONEP|
               (DM-ABS
                (SETQ COF (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 6)))))
             (PROGN
              (SETQ INDXSGN
                      (CONS (CAR Z) (COND ((|:MINUSP| COF) (MINUS 1)) (T 1))))
              (SETQ GCLST (INSGCLST COF INDXSGN GCLST 1))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN GCLST))) 
(PUT 'REMGCMON 'NUMBER-OF-ARGS 2) 
(PUT 'REMGCMON 'DEFINED-ON-LINE '356) 
(PUT 'REMGCMON 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMGCMON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMGCMON (GCEL Y)
    (PROG (X NVAR GC ZEL ZZY ZZGC IVALZ)
      (COND
       ((GREATERP (LENGTH (CADR GCEL)) 1)
        (PROGN
         (SETQ GC (CAR GCEL))
         (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
         (SETQ ROWMAX (PLUS ROWMAX 1))
         (SETQ NVAR (FNEWSYM))
         (SETQ ZEL
                 (COND ((OR (IDP 1) (CONSTP 1)) (CONS Y (CONS 1 NIL)))
                       (T (CONS Y 1))))
         (SETROW ROWMAX 'TIMES NVAR (LIST NIL GC) (LIST ZEL))
         (PUT NVAR 'ROWINDEX ROWMAX)
         (SETQ ZZY
                 (CONS
                  ((LAMBDA (G175)
                     (COND
                      ((OR (IDP (CDR ZEL)) (CONSTP (CDR ZEL)))
                       (CONS G175 (CONS (CDR ZEL) NIL)))
                      (T (CONS G175 (CDR ZEL)))))
                   ROWMAX)
                  (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4)))
         (SETQ ZZGC NIL)
         (PROG (Z)
           (SETQ Z (CADR GCEL))
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (PROGN
               (SETQ X (CAR Z))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 6 1)
               (SETPREV X ROWMAX)
               (SETQ ZEL (CAR (PNTHXZZ X ZZY)))
               (COND
                ((GREATERP (CAR (CDR ZEL)) 1)
                 (PROGN
                  (SETQ ZZY
                          (INSZZZ
                           ((LAMBDA (G178)
                              (COND
                               ((OR (IDP G178) (CONSTP G178))
                                (CONS X (CONS G178 NIL)))
                               (T (CONS X G178))))
                            (SETQ IVALZ (DM-DIFFERENCE (CAR (CDR ZEL)) 1)))
                           (DELYZZ X ZZY)))
                  (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                        (INSZZZR
                         (COND
                          ((OR (IDP IVALZ) (CONSTP IVALZ))
                           (CONS Y (CONS IVALZ NIL)))
                          (T (CONS Y IVALZ)))
                         (DELYZZ Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))))))
                (T
                 (PROGN
                  (SETQ ZZY (DELYZZ X ZZY))
                  (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                        (DELYZZ Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))))))
               (SETQ ZZGC
                       (INSZZZ
                        (SETQ ZEL
                                (COND
                                 ((OR (IDP 1) (CONSTP 1))
                                  (CONS X (CONS 1 NIL)))
                                 (T (CONS X 1))))
                        ZZGC))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                     (CONS
                      ((LAMBDA (G179)
                         (COND
                          ((OR (IDP (CDR ZEL)) (CONSTP (CDR ZEL)))
                           (CONS G179 (CONS (CDR ZEL) NIL)))
                          (T (CONS G179 (CDR ZEL)))))
                       ROWMIN)
                      (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4 ZZY)
         (SETROW ROWMIN 'TIMES NVAR NIL ZZGC)
         (PUT NVAR 'VARLST* ROWMIN))))
      (COND
       ((CDDR GCEL)
        (PROG (ITEM)
          (SETQ ITEM (CDDR GCEL))
         LAB
          (COND ((NULL ITEM) (RETURN NIL)))
          ((LAMBDA (ITEM) (REMGCMON ITEM Y)) (CAR ITEM))
          (SETQ ITEM (CDR ITEM))
          (GO LAB)))))) 
(PUT 'CODGCD 'NUMBER-OF-ARGS 0) 
(PUT 'CODGCD 'DEFINED-ON-LINE '444) 
(PUT 'CODGCD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'CODGCD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CODGCD NIL
    (PROG (PRESENTROWMAX)
      (SETQ PRESENTROWMAX ROWMAX)
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE PRESENTROWMAX X)) (RETURN NIL)))
        (COND
         ((NOT (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1)))
          (PROG (GCEL)
            (SETQ GCEL (MKGCLST X))
           LAB
            (COND ((NULL GCEL) (RETURN NIL)))
            ((LAMBDA (GCEL) (REMGC GCEL X)) (CAR GCEL))
            (SETQ GCEL (CDR GCEL))
            (GO LAB))))
        (SETQ X (PLUS2 X 1))
        (GO LAB)))) 
(PUT 'MKGCLST 'NUMBER-OF-ARGS 1) 
(PUT 'MKGCLST 'DEFINED-ON-LINE '456) 
(PUT 'MKGCLST 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'MKGCLST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKGCLST (X)
    (PROG (GCLST IV OPV)
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((NOT (|:ONEP| (DM-ABS (SETQ IV (CAR (CDR Z))))))
             (COND
              ((|:MINUSP| IV)
               (SETQ GCLST
                       (INSGCLST (DM-MINUS IV) (CONS (CAR Z) (MINUS 1)) GCLST
                        1)))
              (T (SETQ GCLST (INSGCLST IV (CONS (CAR Z) 1) GCLST 1)))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (SETQ OPV (GETV (GETV CODMAT (PLUS MAXVAR X)) 2))
      (PROG (CH)
        (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
       LAB
        (COND ((NULL CH) (RETURN NIL)))
        ((LAMBDA (CH)
           (COND
            ((AND (NOT (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR CH)) 2) OPV))
                  (NOT
                   (|:ONEP|
                    (DM-ABS
                     (SETQ IV (GETV (GETV CODMAT (PLUS MAXVAR CH)) 6))))))
             (COND
              ((|:MINUSP| IV)
               (SETQ GCLST
                       (INSGCLST (DM-MINUS IV) (CONS CH (MINUS 1)) GCLST 1)))
              (T (SETQ GCLST (INSGCLST IV (CONS CH 1) GCLST 1)))))))
         (CAR CH))
        (SETQ CH (CDR CH))
        (GO LAB))
      (RETURN GCLST))) 
(PUT 'INSGCLST 'NUMBER-OF-ARGS 4) 
(PUT 'INSGCLST 'DEFINED-ON-LINE '488) 
(PUT 'INSGCLST 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'INSGCLST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSGCLST (IV Y GCLST GC0)
    (PROG (GC CGCL)
      (RETURN
       (COND ((NULL GCLST) (LIST (CONS IV (CONS (LIST Y) NIL))))
             ((DM-EQ (CAAR GCLST) IV)
              (CONS (CONS IV (CONS (CONS Y (CADAR GCLST)) (CDDAR GCLST)))
                    (CDR GCLST)))
             ((OR (FLOATPROP IV) (FLOATPROP (CAAR GCLST))
                  (LEQ (SETQ GC (GCD2 IV (CAAR GCLST))) GC0))
              (CONS (CAR GCLST) (INSGCLST IV Y (CDR GCLST) GC0)))
             ((EQUAL GC (CAAR GCLST))
              (CONS
               (APPEND (LIST GC (CADAR GCLST)) (INSDIFF IV Y (CDDAR GCLST)))
               (CDR GCLST)))
             ((EQUAL GC IV)
              (PROGN
               (COND
                ((NULL (CADAR GCLST))
                 (LIST (APPEND (LIST GC (LIST Y)) (CDDAR GCLST))))
                ((AND (CDDAR GCLST) (CADDAR GCLST))
                 (CONS
                  (APPEND (LIST GC (LIST Y) (LIST (CAAR GCLST) (CADAR GCLST)))
                          (CDDAR GCLST))
                  (CDR GCLST)))
                (T
                 (CONS (CONS GC (CONS (LIST Y) (LIST (CAR GCLST))))
                       (CDR GCLST))))))
             (T
              (CONS
               (CONS GC
                     (CONS NIL
                           (APPEND (LIST (CONS IV (CONS (LIST Y) NIL)))
                                   (COND
                                    ((CDDAR GCLST)
                                     (APPEND
                                      (LIST (LIST (CAAR GCLST) (CADAR GCLST)))
                                      (CDDAR GCLST)))
                                    (T
                                     (LIST
                                      (LIST (CAAR GCLST) (CADAR GCLST))))))))
               (CDR GCLST))))))) 
(PUT 'INSDIFF 'NUMBER-OF-ARGS 3) 
(PUT 'INSDIFF 'DEFINED-ON-LINE '587) 
(PUT 'INSDIFF 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'INSDIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSDIFF (IV Y GLSTS)
    (PROG (B RLST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND GLSTS (NOT B))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (CAAR GLSTS) IV)
           (PROGN
            (SETQ RLST (CONS (LIST IV (APPEND (LIST Y) (CADAR GLSTS))) RLST))
            (SETQ B T)))
          (T (SETQ RLST (CONS (CAR GLSTS) RLST))))
         (SETQ GLSTS (CDR GLSTS)))
        (GO WHILELABEL))
      (RETURN
       (COND (B (APPEND (REVERSE RLST) GLSTS))
             (T
              (APPEND (LIST (CONS IV (CONS (LIST Y) NIL))) (REVERSE RLST))))))) 
(PUT 'REMGC 'NUMBER-OF-ARGS 2) 
(PUT 'REMGC 'DEFINED-ON-LINE '609) 
(PUT 'REMGC 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMGC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMGC (GCEL X)
    (PROG (ZZCH ZZCHL ZZR CHR ZZ CH NSUM NPROD NS NP OPV GC COF COFLOC IV VAR1
           VAR2)
      (COND
       ((OR
         (AND (EQ (SETQ OPV (GETV (GETV CODMAT (PLUS MAXVAR X)) 2)) 'TIMES)
              (OR (GREATERP (LENGTH (CADR GCEL)) 1) (CDDR GCEL)))
         (AND (EQ OPV 'PLUS) (GREATERP (LENGTH (CADR GCEL)) 1)))
        (PROGN
         (COND
          ((EQ OPV 'TIMES)
           (PROGN
            (SETQ NSUM (SETQ ROWMAX (PLUS ROWMAX 1)))
            (SETQ VAR1 (FNEWSYM))
            (PUT VAR1 'ROWINDEX NSUM)
            (SETPREV X NSUM)
            (SETROW (SETQ ROWMIN (DIFFERENCE ROWMIN 1)) 'TIMES VAR1 NIL
             (LIST
              (SETQ IV
                      ((LAMBDA (G182)
                         (COND
                          ((OR (IDP G182) (CONSTP G182))
                           (CONS X (CONS G182 NIL)))
                          (T (CONS X G182))))
                       (SETQ GC (CAR GCEL))))))
            (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                  (INSZZZR
                   ((LAMBDA (G183)
                      (COND
                       ((OR (IDP (CDR IV)) (CONSTP (CDR IV)))
                        (CONS G183 (CONS (CDR IV) NIL)))
                       (T (CONS G183 (CDR IV)))))
                    ROWMIN)
                   (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))
            (PUT VAR1 'VARLST* ROWMIN)
            (SETROW NSUM 'TIMES VAR1 (LIST NIL) NIL)))
          (T
           (PROGN
            (SETQ NPROD (PLUS ROWMAX 1))
            (SETQ NSUM (SETQ ROWMAX (PLUS ROWMAX 2)))
            (PUTV (GETV CODMAT (PLUS MAXVAR X)) 5
                  (CONS NPROD (GETV (GETV CODMAT (PLUS MAXVAR X)) 5)))
            (SETROW NPROD (COND ((EQ OPV 'PLUS) 'TIMES) (T 'PLUS)) X
             (LIST (LIST NSUM) (SETQ GC (CAR GCEL))) NIL)
            (SETROW NSUM OPV NPROD (LIST NIL) NIL))))
         (SETQ ZZCH (UPDATEROWINF X NSUM 1 (CADR GCEL) ZZR CHR))
         (PROG (Y)
           (SETQ Y (CDDR GCEL))
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROGN
               (SETQ COF (DM-QUOTIENT (CAR Y) GC))
               (SETQ COFLOC (CADR Y))
               (COND
                ((CDR COFLOC)
                 (PROGN
                  (COND
                   ((EQ OPV 'PLUS)
                    (PROGN
                     (SETQ NP (PLUS ROWMAX 1))
                     (SETQ NS (SETQ ROWMAX (PLUS ROWMAX 2)))
                     (SETROW NP (COND ((EQ OPV 'PLUS) 'TIMES) (T 'PLUS)) NSUM
                      (LIST (LIST NS) COF) NIL)
                     (SETROW NS OPV NP (LIST NIL) NIL)
                     (PUTV (GETV CODMAT (PLUS MAXVAR NSUM)) 5
                           (CONS NP
                                 (GETV (GETV CODMAT (PLUS MAXVAR NSUM)) 5)))))
                   (T
                    (PROGN
                     (SETQ NS (SETQ ROWMAX (PLUS ROWMAX 1)))
                     (SETQ VAR2 (FNEWSYM))
                     (PUT VAR2 'ROWINDEX NS)
                     (SETPREV (GET VAR1 'ROWINDEX) NS)
                     (SETROW (SETQ ROWMIN (DIFFERENCE ROWMIN 1)) 'TIMES VAR2
                      NIL
                      (LIST
                       (SETQ IV
                               (COND
                                ((OR (IDP COF) (CONSTP COF))
                                 (CONS NSUM (CONS COF NIL)))
                                (T (CONS NSUM COF))))))
                     (PUTV (GETV CODMAT (PLUS MAXVAR NSUM)) 4
                           (INSZZZR
                            ((LAMBDA (G185)
                               (COND
                                ((OR (IDP (CDR IV)) (CONSTP (CDR IV)))
                                 (CONS G185 (CONS (CDR IV) NIL)))
                                (T (CONS G185 (CDR IV)))))
                             ROWMIN)
                            (GETV (GETV CODMAT (PLUS MAXVAR NSUM)) 4)))
                     (PUT VAR2 'VARLST* ROWMIN)
                     (SETROW NS 'TIMES VAR2 (LIST NIL) NIL))))
                  (SETQ ZZ (SETQ CH NIL))
                  (SETQ ZZCHL (UPDATEROWINF X NS 1 COFLOC ZZ CH))
                  (PUTV (GETV CODMAT (PLUS MAXVAR NS)) 4 (CAR ZZCHL))
                  (PUTV (GETV CODMAT (PLUS MAXVAR NS)) 5 (CDR ZZCHL))))
                (T
                 (SETQ ZZCH
                         (UPDATEROWINF X NSUM COF COFLOC (CAR ZZCH)
                          (CDR ZZCH)))))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (PROG (ZEL)
           (SETQ ZEL (CAR ZZCH))
          LAB
           (COND ((NULL ZEL) (RETURN NIL)))
           ((LAMBDA (ZEL)
              (PUTV (GETV CODMAT (PLUS MAXVAR NSUM)) 4
                    (INSZZZR ZEL (GETV (GETV CODMAT (PLUS MAXVAR NSUM)) 4))))
            (CAR ZEL))
           (SETQ ZEL (CDR ZEL))
           (GO LAB))
         (PUTV (GETV CODMAT (PLUS MAXVAR NSUM)) 5
               (COND
                ((GETV (GETV CODMAT (PLUS MAXVAR NSUM)) 5)
                 (APPEND (GETV (GETV CODMAT (PLUS MAXVAR NSUM)) 5) (CDR ZZCH)))
                (T (CDR ZZCH))))))
       (T
        (PROG (ITEM)
          (SETQ ITEM (CDDR GCEL))
         LAB
          (COND ((NULL ITEM) (RETURN NIL)))
          ((LAMBDA (ITEM) (REMGC ITEM X)) (CAR ITEM))
          (SETQ ITEM (CDR ITEM))
          (GO LAB)))))) 
(PUT 'UPDATEROWINF 'NUMBER-OF-ARGS 6) 
(PUT 'UPDATEROWINF 'DEFINED-ON-LINE '711) 
(PUT 'UPDATEROWINF 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'UPDATEROWINF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPDATEROWINF (X NROW COF INFOLST ZZ CH)
    (PROG (INDX IV MZ DYZ)
      (PROG (ITEM)
        (SETQ ITEM INFOLST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (SETQ INDX (CAR ITEM))
            (COND
             ((LESSP INDX 0)
              (PROGN
               (SETQ ZZ
                       (INSZZZR
                        (SETQ IV
                                ((LAMBDA (G188)
                                   (COND
                                    ((OR (IDP G188) (CONSTP G188))
                                     (CONS INDX (CONS G188 NIL)))
                                    (T (CONS INDX G188))))
                                 (DM-TIMES COF (CDR ITEM))))
                        ZZ))
               (PUTV (GETV CODMAT (PLUS MAXVAR INDX)) 4
                     (INSZZZ
                      (COND
                       ((OR (IDP (CDR IV)) (CONSTP (CDR IV)))
                        (CONS NROW (CONS (CDR IV) NIL)))
                       (T (CONS NROW (CDR IV))))
                      (DELYZZ X (GETV (GETV CODMAT (PLUS MAXVAR INDX)) 4))))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                     (DELYZZ INDX (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))))
             (T
              (PROGN
               (SETQ CH (CONS INDX CH))
               (CHDEL X INDX)
               (PUTV (GETV CODMAT (PLUS MAXVAR INDX)) 3 NROW)
               (PUTV (GETV CODMAT (PLUS MAXVAR INDX)) 6
                     (DM-TIMES COF (CDR ITEM))))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (RETURN (CONS ZZ CH)))) 
(GLOBAL '(KVARLST QLHS QRHS QLKVL)) 
(PUT 'TCHSCHEME2 'NUMBER-OF-ARGS 0) 
(PUT 'TCHSCHEME2 'DEFINED-ON-LINE '743) 
(PUT 'TCHSCHEME2 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'TCHSCHEME2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TCHSCHEME2 NIL
    (PROG ()
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (PROGN (REMOVECHILD X) (TO*SCHEME X))
        (SETQ X (PLUS2 X 1))
        (GO LAB)))) 
(PUT 'TO*SCHEME 'NUMBER-OF-ARGS 1) 
(PUT 'TO*SCHEME 'DEFINED-ON-LINE '756) 
(PUT 'TO*SCHEME 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'TO*SCHEME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TO*SCHEME (X)
    (PROG (Z YI EXP)
      (COND
       ((AND (NOT (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))
             (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
             (EQUAL (LENGTH (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)) 1)
             (NULL (GETV (GETV CODMAT (PLUS MAXVAR X)) 5)))
        (PROGN
         (SETQ Z (CAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))
         (SETQ YI (CAR Z))
         (SETQ EXP (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 6 (DM-EXPT (CAR (CDR Z)) EXP))
         (SETQ Z
                 (GETCIND (GETV (GETV CODMAT (PLUS MAXVAR YI)) 3) 'VARLST*
                  'TIMES X (CONS EXP (CDR (CDR Z)))))
         (PUTV (GETV CODMAT (PLUS MAXVAR YI)) 4
               (DELYZZ X (GETV (GETV CODMAT (PLUS MAXVAR YI)) 4)))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 (LIST Z))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 2 'TIMES)
         NIL))))) 
(PUT 'REMOVECHILD 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVECHILD 'DEFINED-ON-LINE '774) 
(PUT 'REMOVECHILD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMOVECHILD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVECHILD (X)
    (PROG (CH EXP IV)
      (COND
       ((AND (NOT (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))
             (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'PLUS)
             (NULL (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
             (EQUAL (LENGTH (GETV (GETV CODMAT (PLUS MAXVAR X)) 5)) 1))
        (PROGN
         (SETQ CH (CAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 5)))
         (SETQ EXP (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))
         (PROG (Z)
           (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR CH)) 4))
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (PROGN
               (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                     (DELYZZ CH (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4)))
               (SETQ IV (DM-TIMES (CAR (CDR Z)) EXP))
               (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                     (INSZZZ
                      (COND ((OR (IDP IV) (CONSTP IV)) (CONS X (CONS IV NIL)))
                            (T (CONS X IV)))
                      (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4)))
               (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                     (INSZZZR
                      (COND
                       ((OR (IDP IV) (CONSTP IV)) (CONS (CAR Z) (CONS IV NIL)))
                       (T (CONS (CAR Z) IV)))
                      (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (PROG (CHLD)
           (SETQ CHLD (GETV (GETV CODMAT (PLUS MAXVAR CH)) 5))
          LAB
           (COND ((NULL CHLD) (RETURN NIL)))
           ((LAMBDA (CHLD) (PUTV (GETV CODMAT (PLUS MAXVAR CHLD)) 3 X))
            (CAR CHLD))
           (SETQ CHLD (CDR CHLD))
           (GO LAB))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 2 'TIMES)
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 6
               (DM-TIMES (GETV (GETV CODMAT (PLUS MAXVAR CH)) 6) EXP))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 5
               (GETV (GETV CODMAT (PLUS MAXVAR CH)) 5))
         (CLEARROW CH)
         NIL))))) 
(PUT 'SEARCHCSEQUOTIENTS 'NUMBER-OF-ARGS 0) 
(PUT 'SEARCHCSEQUOTIENTS 'DEFINED-ON-LINE '797) 
(PUT 'SEARCHCSEQUOTIENTS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'SEARCHCSEQUOTIENTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SEARCHCSEQUOTIENTS NIL
    (PROG (RES CONTINUESEARCH)
      (TCHSCHEME2)
      (SETQ RES (SETQ CONTINUESEARCH (SEARCHCSEQUOTIENTS2)))
      (PROG ()
       WHILELABEL
        (COND ((NOT CONTINUESEARCH) (RETURN NIL)))
        (SETQ CONTINUESEARCH (SEARCHCSEQUOTIENTS2))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'SEARCHCSEQUOTIENTS2 'NUMBER-OF-ARGS 0) 
(PUT 'SEARCHCSEQUOTIENTS2 'DEFINED-ON-LINE '807) 
(PUT 'SEARCHCSEQUOTIENTS2 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'SEARCHCSEQUOTIENTS2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SEARCHCSEQUOTIENTS2 NIL
    (PROG (J QUOTIENTS DMLST DM NUMERINFOL NRLST SELECTEDDMS SELECTEDNRS
           QUOTLST B QUOTS PROFIT QCSE CSELST VAR S)
      (SETQ QLKVL (LENGTH KVARLST))
      (SETQ QLHS (MKVECT QLKVL))
      (SETQ QRHS (MKVECT QLKVL))
      (SETQ J 0)
      (SETQ QUOTIENTS NIL)
      (PROG (ITEM)
        (SETQ ITEM KVARLST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (PUTV QLHS (SETQ J (PLUS J 1)) (CAR ITEM))
            (PUTV QRHS J (CDR ITEM))
            (COND
             ((RELQUOTTEST (GETV QRHS J)) (SETQ QUOTIENTS (CONS J QUOTIENTS))))
            NIL))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (COND
       (QUOTIENTS
        (PROGN
         (PROG (INDX)
           (SETQ INDX QUOTIENTS)
          LAB
           (COND ((NULL INDX) (RETURN NIL)))
           ((LAMBDA (INDX)
              (SETQ DMLST (INSERTIN DMLST (CADDR (GETV QRHS INDX)) INDX)))
            (CAR INDX))
           (SETQ INDX (CDR INDX))
           (GO LAB))
         (SETQ DMLST (ADDMATNORDS DMLST))
         (SETQ SELECTEDDMS (SELECTMOSTFREQNORD DMLST))
         (COND
          ((AND SELECTEDDMS (GREATERP (LENGTH (CDR SELECTEDDMS)) 1))
           (PROGN
            (SETQ DM (CAR SELECTEDDMS))
            (SETQ NUMERINFOL (CDR SELECTEDDMS))
            (SETQ NRLST NIL)
            (PROG (INDX)
              (SETQ INDX NUMERINFOL)
             LAB
              (COND ((NULL INDX) (RETURN NIL)))
              ((LAMBDA (INDX)
                 (SETQ NRLST (INSERTIN NRLST (CADR (GETV QRHS INDX)) INDX)))
               (CAR INDX))
              (SETQ INDX (CDR INDX))
              (GO LAB))
            (SETQ NRLST (ADDMATNORDS NRLST))
            (COND
             ((SETQ SELECTEDNRS (SELECTMOSTFREQNORD NRLST))
              (COND
               ((GREATERP (LENGTH (CDR SELECTEDNRS)) 1)
                (SETQ QUOTLST
                        (CONS
                         (CONS (CONS (CAR SELECTEDNRS) DM) (CDR SELECTEDNRS))
                         QUOTLST)))))))))
         (COND
          (QUOTLST
           (PROGN
            (SETQ QUOTS (MKVECT QLKVL))
            (PROG (ITEM)
              (SETQ ITEM QUOTLST)
             LAB
              (COND ((NULL ITEM) (RETURN NIL)))
              ((LAMBDA (ITEM)
                 (PROGN
                  (SETQ PROFIT (QPROFIT ITEM))
                  (COND
                   ((OR (GEQ (CDR PROFIT) 0)
                        (GEQ (PLUS (CAR PROFIT) (TIMES 2 (CDR PROFIT))) 0))
                    (PROGN
                     (SETQ B T)
                     (SETQ QCSE (LIST 'QUOTIENT (CAAR ITEM) (CDAR ITEM)))
                     (COND
                      ((SETQ VAR
                               (ASSOC QCSE (SETQ S (GET (CAR QCSE) 'KVARLST))))
                       (SETQ QCSE (CONS (CDR VAR) QCSE)))
                      (T
                       (PROGN
                        (SETQ VAR (FNEWSYM))
                        (PUT (CAR QCSE) 'KVARLST (CONS (CONS QCSE VAR) S))
                        (SETQ QCSE (CONS VAR QCSE))
                        (SETQ CSELST (CONS QCSE CSELST)))))
                     (PROG (INDX)
                       (SETQ INDX (CDR ITEM))
                      LAB
                       (COND ((NULL INDX) (RETURN NIL)))
                       ((LAMBDA (INDX)
                          (COND
                           ((NEQ (CAR QCSE) (GETV QLHS INDX))
                            (SUBSTQCSE QCSE INDX))))
                        (CAR INDX))
                       (SETQ INDX (CDR INDX))
                       (GO LAB)))))))
               (CAR ITEM))
              (SETQ ITEM (CDR ITEM))
              (GO LAB))
            (SETQ KVARLST NIL)
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE QLKVL J)) (RETURN NIL)))
              (COND
               ((GETV QLHS J)
                (SETQ KVARLST
                        (APPEND KVARLST
                                (LIST (CONS (GETV QLHS J) (GETV QRHS J)))))))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (SETQ KVARLST (APPEND KVARLST CSELST))
            NIL))))))
      (SETQ QLKVL (SETQ QLHS (SETQ QRHS NIL)))
      (RETURN B))) 
(PUT 'RELQUOTTEST 'NUMBER-OF-ARGS 1) 
(PUT 'RELQUOTTEST 'DEFINED-ON-LINE '921) 
(PUT 'RELQUOTTEST 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'RELQUOTTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RELQUOTTEST (ITEM)
    (AND (EQCAR ITEM 'QUOTIENT) (RELVSTR (CADR ITEM)) (RELVSTR (CADDR ITEM)))) 
(PUT 'RELVSTR 'NUMBER-OF-ARGS 1) 
(PUT 'RELVSTR 'DEFINED-ON-LINE '929) 
(PUT 'RELVSTR 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'RELVSTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RELVSTR (ITEM) (PROG (ROWINDX) (RETURN (OR (CONSTP ITEM) (IDP ITEM))))) 
(PUT 'ADDMATNORDS 'NUMBER-OF-ARGS 1) 
(PUT 'ADDMATNORDS 'DEFINED-ON-LINE '940) 
(PUT 'ADDMATNORDS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'ADDMATNORDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADDMATNORDS (NORDLST)
    (PROG (MATNORDS TEMPLST ROWINDX)
      (PROG (NORD)
        (SETQ NORD NORDLST)
       LAB
        (COND ((NULL NORD) (RETURN NIL)))
        ((LAMBDA (NORD)
           (PROG (INDX)
             (SETQ INDX (CDR NORD))
            LAB
             (COND ((NULL INDX) (RETURN NIL)))
             ((LAMBDA (INDX)
                (COND
                 ((AND (SETQ ROWINDX (GET (CAR NORD) 'ROWINDEX))
                       (EQ (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 2)
                           'TIMES))
                  (PROGN
                   (PROG (Z)
                     (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4))
                    LAB
                     (COND ((NULL Z) (RETURN NIL)))
                     ((LAMBDA (Z)
                        (SETQ MATNORDS
                                (INSERTIN MATNORDS
                                 (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3)
                                 INDX)))
                      (CAR Z))
                     (SETQ Z (CDR Z))
                     (GO LAB))
                   (COND
                    ((NEQ (ABS (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6)) 1)
                     (SETQ MATNORDS
                             (INSERTIN MATNORDS
                              (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6)
                              INDX))))))))
              (CAR INDX))
             (SETQ INDX (CDR INDX))
             (GO LAB)))
         (CAR NORD))
        (SETQ NORD (CDR NORD))
        (GO LAB))
      (PROG (NORD)
        (SETQ NORD NORDLST)
       LAB
        (COND ((NULL NORD) (RETURN NIL)))
        ((LAMBDA (NORD)
           (PROGN
            (COND
             ((GREATERP (LENGTH (CDR NORD)) 1)
              (PROG (INDX)
                (SETQ INDX (CDR NORD))
               LAB
                (COND ((NULL INDX) (RETURN NIL)))
                ((LAMBDA (INDX)
                   (SETQ TEMPLST (INSERTIN TEMPLST (CAR NORD) INDX)))
                 (CAR INDX))
                (SETQ INDX (CDR INDX))
                (GO LAB)))
             ((ASSOC (CAR NORD) MATNORDS)
              (SETQ TEMPLST (INSERTIN TEMPLST (CAR NORD) (CADR NORD))))
             ((AND (SETQ ROWINDX (GET (CAR NORD) 'ROWINDEX))
                   (EQ (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 2) 'TIMES)
                   (EQUAL (NOFNORDOCC (CAR NORD)) 1))
              (PROGN
               (PROG (Z)
                 (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4))
                LAB
                 (COND ((NULL Z) (RETURN NIL)))
                 ((LAMBDA (Z)
                    (SETQ TEMPLST
                            (INSERTIN TEMPLST
                             (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3)
                             (CADR NORD))))
                  (CAR Z))
                 (SETQ Z (CDR Z))
                 (GO LAB))
               (SETQ TEMPLST
                       (INSERTIN TEMPLST
                        (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6)
                        (CADR NORD))))))))
         (CAR NORD))
        (SETQ NORD (CDR NORD))
        (GO LAB))
      (RETURN TEMPLST))) 
(PUT 'NOFNORDOCC 'NUMBER-OF-ARGS 1) 
(PUT 'NOFNORDOCC 'DEFINED-ON-LINE '983) 
(PUT 'NOFNORDOCC 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'NOFNORDOCC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOFNORDOCC (NORD)
    (PROG (NOFOCC)
      (SETQ NOFOCC (NOFMATNORDS NORD))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE QLKVL I)) (RETURN NIL)))
        (SETQ NOFOCC (PLUS NOFOCC (NUMBEROFOCC NORD (GETV QRHS I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NOFOCC))) 
(PUT 'NUMBEROFOCC 'NUMBER-OF-ARGS 2) 
(PUT 'NUMBEROFOCC 'DEFINED-ON-LINE '994) 
(PUT 'NUMBEROFOCC 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'NUMBEROFOCC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUMBEROFOCC (VAR EXPRESSION)
    (COND
     ((OR (CONSTP EXPRESSION) (IDP EXPRESSION))
      (COND ((EQUAL VAR EXPRESSION) 1) (T 0)))
     (T
      (PLUS (COND ((CDR EXPRESSION) (NUMBEROFOCC VAR (CDR EXPRESSION))) (T 0))
            (COND ((EQUAL VAR (CAR EXPRESSION)) 1)
                  ((NOT (ATOM (CAR EXPRESSION)))
                   (NUMBEROFOCC VAR (CAR EXPRESSION)))
                  (T 0)))))) 
(PUT 'NOFMATNORDS 'NUMBER-OF-ARGS 1) 
(PUT 'NOFMATNORDS 'DEFINED-ON-LINE '1014) 
(PUT 'NOFMATNORDS 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'NOFMATNORDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOFMATNORDS (NORD)
    (PROG (NOFOCC COLINDX)
      (SETQ NOFOCC 0)
      (COND
       ((SETQ COLINDX (GET NORD 'VARLST*))
        (SETQ NOFOCC (LENGTH (GETV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4)))))
      (COND
       ((SETQ COLINDX (GET NORD 'VARLST+))
        (SETQ NOFOCC
                (PLUS NOFOCC
                      (LENGTH (GETV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4))))))
      (RETURN NOFOCC))) 
(PUT 'INSERTIN 'NUMBER-OF-ARGS 3) 
(PUT 'INSERTIN 'DEFINED-ON-LINE '1024) 
(PUT 'INSERTIN 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'INSERTIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSERTIN (NORDLST ITEM INDX)
    (PROG (PR)
      (RETURN
       (COND ((|:ONEP| (DM-ABS ITEM)) NORDLST)
             ((SETQ PR (ASSOC ITEM NORDLST))
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL NORDLST)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (COND
                                     ((EQUAL (CAR EL) ITEM)
                                      (CONS ITEM
                                            (APPEND (CDR PR) (LIST INDX))))
                                     (T EL)))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (COND
                             ((EQUAL (CAR EL) ITEM)
                              (CONS ITEM (APPEND (CDR PR) (LIST INDX))))
                             (T EL)))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
             (T (APPEND (LIST (CONS ITEM (LIST INDX))) NORDLST)))))) 
(PUT 'SELECTMOSTFREQNORD 'NUMBER-OF-ARGS 1) 
(PUT 'SELECTMOSTFREQNORD 'DEFINED-ON-LINE '1039) 
(PUT 'SELECTMOSTFREQNORD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'SELECTMOSTFREQNORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SELECTMOSTFREQNORD (NORDLST)
    (PROG (TEMPLST TEMP SELECTEDPR LMAX)
      (COND
       (NORDLST
        (PROGN
         (SETQ SELECTEDPR (CAR NORDLST))
         (SETQ LMAX (LENGTH (CDR SELECTEDPR)))
         (SETQ TEMPLST (CDR NORDLST))
         (PROG (PR)
           (SETQ PR TEMPLST)
          LAB
           (COND ((NULL PR) (RETURN NIL)))
           ((LAMBDA (PR)
              (PROGN
               (COND
                ((LESSP LMAX (SETQ TEMP (LENGTH (CDAR TEMPLST))))
                 (PROGN (SETQ LMAX TEMP) (SETQ SELECTEDPR (CAR TEMPLST)))))
               (SETQ TEMPLST (CDR TEMPLST))))
            (CAR PR))
           (SETQ PR (CDR PR))
           (GO LAB)))))
      (RETURN SELECTEDPR))) 
(PUT 'QPROFIT 'NUMBER-OF-ARGS 1) 
(PUT 'QPROFIT 'DEFINED-ON-LINE '1061) 
(PUT 'QPROFIT 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'QPROFIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QPROFIT (ITEM)
    (PROG (NBOF* NBOF/ TEMPQUOT H F TF IL)
      (SETQ IL (CDR ITEM))
      (PROG ()
       WHILELABEL
        (COND ((NOT IL) (RETURN NIL)))
        (PROGN
         (SETQ H (CAR IL))
         (SETQ IL (CDR IL))
         (SETQ F (CONS H F))
         (PROG (INDX)
           (SETQ INDX IL)
          LAB
           (COND ((NULL INDX) (RETURN NIL)))
           ((LAMBDA (INDX)
              (PROGN (COND ((NEQ INDX H) (SETQ TF (CONS INDX TF))))))
            (CAR INDX))
           (SETQ INDX (CDR INDX))
           (GO LAB))
         (COND ((NOT (NULL TF)) (PROGN (SETQ IL (REVERSE TF)) (SETQ TF NIL)))
               (T (SETQ IL NIL))))
        (GO WHILELABEL))
      (COND
       ((EQUAL (LENGTH (SETQ IL (REVERSE F))) 1)
        (PROGN (SETQ NBOF* 0) (SETQ NBOF/ (MINUS 1))))
       (T
        (PROGN
         (SETQ NBOF* 0)
         (SETQ NBOF/ (MINUS 1))
         (PROG (SGNINDX)
           (SETQ SGNINDX IL)
          LAB
           (COND ((NULL SGNINDX) (RETURN NIL)))
           ((LAMBDA (SGNINDX)
              (PROGN
               (SETQ TEMPQUOT (GETV QRHS SGNINDX))
               (COND
                ((EQUAL (CDAR ITEM) (CADDR TEMPQUOT))
                 (SETQ NBOF/ (PLUS 1 NBOF/)))
                (T (SETQ NBOF* (PLUS 1 NBOF*))))
               NIL))
            (CAR SGNINDX))
           (SETQ SGNINDX (CDR SGNINDX))
           (GO LAB)))))
      (RETURN (CONS NBOF* NBOF/)))) 
(PUT 'SUBSTQCSE 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSTQCSE 'DEFINED-ON-LINE '1107) 
(PUT 'SUBSTQCSE 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'SUBSTQCSE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSTQCSE (CSEPAIR INDX)
    (PROG (VAR VAL DM NR PNR PDM NINROW DINROW EXPO)
      (SETQ VAR (CAR CSEPAIR))
      (SETQ VAL (CDR CSEPAIR))
      (SETQ NR (CADR VAL))
      (SETQ DM (CADDR VAL))
      (SETQ PNR (CADR (GETV QRHS INDX)))
      (SETQ PDM (CADDR (GETV QRHS INDX)))
      (SETQ NINROW (COND ((NEQ NR PNR) (GET PNR 'ROWINDEX)) (T NIL)))
      (SETQ DINROW (COND ((NEQ DM PDM) (GET PDM 'ROWINDEX)) (T NIL)))
      (SETQ EXPO (MIN (NORDEXPO NR PNR) (NORDEXPO DM PDM)))
      (SETQ PNR (REMNORD NR EXPO PNR INDX))
      (SETQ PNR (INSNORD VAR EXPO PNR INDX))
      (SETQ PDM (REMNORD DM EXPO PDM INDX))
      (SETQ PNR (CHECKNORD PNR NINROW INDX))
      (SETQ PDM (CHECKNORD PDM DINROW INDX))
      (COND
       ((AND (|:ONEP| PDM) (UNPROTECTED (GETV QLHS INDX)))
        (PROGN (REMQUOTIENT PNR INDX) (PUTV QLHS INDX NIL)))
       (T
        (PUTV QRHS INDX
              (COND ((|:ONEP| PDM) PNR) (T (LIST 'QUOTIENT PNR PDM)))))))) 
(PUT 'UNPROTECTED 'NUMBER-OF-ARGS 1) 
(PUT 'UNPROTECTED 'DEFINED-ON-LINE '1138) 
(PUT 'UNPROTECTED 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'UNPROTECTED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNPROTECTED (VAR) (AND (FLAGP VAR 'NEWSYM) (NOT (GET VAR 'ALIAS)))) 
(PUT 'NORDEXPO 'NUMBER-OF-ARGS 2) 
(PUT 'NORDEXPO 'DEFINED-ON-LINE '1142) 
(PUT 'NORDEXPO 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'NORDEXPO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NORDEXPO (X Y)
    (COND ((CONSTP X) 1)
          ((IDP X)
           (COND ((EQUAL X Y) 1)
                 (T
                  (PROG (RES)
                    (COND
                     ((SETQ RES
                              (ASSOC (GET X 'VARLST*)
                                     (GETV
                                      (GETV CODMAT
                                            (PLUS MAXVAR (GET Y 'ROWINDEX)))
                                      4)))
                      (SETQ RES (CAR (CDR RES))))
                     (T (SETQ RES 0)))
                    (RETURN RES))))))) 
(PUT 'REMNORD 'NUMBER-OF-ARGS 4) 
(PUT 'REMNORD 'DEFINED-ON-LINE '1160) 
(PUT 'REMNORD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMNORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REMNORD (ITEM EXPO DEST INDX)
    (PROG (ROWINDX COLINDX Z)
      (RETURN
       (COND ((CONSTP DEST) (DM-QUOTIENT DEST (DM-EXPT ITEM EXPO)))
             ((EQUAL ITEM DEST)
              (PROGN
               (REMQUOTORDR INDX ITEM)
               (COND
                ((SETQ ROWINDX (GET ITEM 'ROWINDEX))
                 (REMQUOTORDR INDX ROWINDX)))
               1))
             (T
              (PROGN
               (SETQ ROWINDX (GET DEST 'ROWINDEX))
               (COND
                ((CONSTP ITEM)
                 (PROGN
                  (COND
                   ((EQUAL (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 2) 'TIMES)
                    (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6
                          (DM-QUOTIENT
                           (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6)
                           (DM-EXPT ITEM EXPO))))
                   (T
                    (PROGN
                     (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4
                           (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Z
                                     (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX))
                                           4))
                             (COND ((NULL Z) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Z)
                                                 ((LAMBDA (G191)
                                                    (COND
                                                     ((OR (IDP G191)
                                                          (CONSTP G191))
                                                      (CONS (CAR Z)
                                                            (CONS G191 NIL)))
                                                     (T (CONS (CAR Z) G191))))
                                                  (CONS
                                                   (DM-QUOTIENT (CAR (CDR Z))
                                                    (DM-EXPT ITEM EXPO))
                                                   (CDR (CDR Z)))))
                                               (CAR Z))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Z (CDR Z))
                             (COND ((NULL Z) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Z)
                                         ((LAMBDA (G191)
                                            (COND
                                             ((OR (IDP G191) (CONSTP G191))
                                              (CONS (CAR Z) (CONS G191 NIL)))
                                             (T (CONS (CAR Z) G191))))
                                          (CONS
                                           (DM-QUOTIENT (CAR (CDR Z))
                                            (DM-EXPT ITEM EXPO))
                                           (CDR (CDR Z)))))
                                       (CAR Z))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                     (PROG (Z)
                       (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4))
                      LAB
                       (COND ((NULL Z) (RETURN NIL)))
                       ((LAMBDA (Z)
                          (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                                (INSZZZ
                                 (COND
                                  ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                                   (CONS ROWINDX (CONS (CDR Z) NIL)))
                                  (T (CONS ROWINDX (CDR Z))))
                                 (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z)))
                                       4))))
                        (CAR Z))
                       (SETQ Z (CDR Z))
                       (GO LAB)))))
                  DEST))
                (T
                 (PROGN
                  (SETQ COLINDX (GET ITEM 'VARLST*))
                  (SETQ Z
                          (ASSOC COLINDX
                                 (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4)))
                  (PUTV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4
                        (DELYZZ ROWINDX
                         (GETV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4)))
                  (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4
                        (DELETE Z
                                (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4)))
                  (COND
                   ((EQUAL (CAR (CDR Z)) EXPO)
                    (PROGN
                     (REMPREV ROWINDX ITEM)
                     (COND
                      ((GET ITEM 'ROWINDEX)
                       (REMPREV ROWINDX (GET ITEM 'ROWINDEX))))))
                   (T
                    (PROGN
                     (PUTV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4
                           (INSZZZ
                            ((LAMBDA (G193)
                               (COND
                                ((OR (IDP G193) (CONSTP G193))
                                 (CONS ROWINDX (CONS G193 NIL)))
                                (T (CONS ROWINDX G193))))
                             (CONS (DIFFERENCE (CAR (CDR Z)) EXPO)
                                   (CDR (CDR Z))))
                            (GETV (GETV CODMAT (PLUS MAXVAR COLINDX)) 4)))
                     (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4
                           (INSZZZR
                            ((LAMBDA (G195)
                               (COND
                                ((OR (IDP G195) (CONSTP G195))
                                 (CONS COLINDX (CONS G195 NIL)))
                                (T (CONS COLINDX G195))))
                             (CONS (DIFFERENCE (CAR (CDR Z)) EXPO)
                                   (CDR (CDR Z))))
                            (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4))))))
                  DEST))))))))) 
(PUT 'INSNORD 'NUMBER-OF-ARGS 4) 
(PUT 'INSNORD 'DEFINED-ON-LINE '1217) 
(PUT 'INSNORD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'INSNORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSNORD (ITEM EXPO DEST INDX)
    (PROG (ROWINDX)
      (RETURN
       (COND
        ((CONSTP DEST)
         (COND ((CONSTP ITEM) (DM-TIMES DEST (DM-EXPT ITEM EXPO)))
               (T (PROGN ITEM))))
        (T
         (PROGN
          (SETQ ROWINDX (GET DEST 'ROWINDEX))
          (COND
           ((CONSTP ITEM)
            (PROGN
             (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6
                   (DM-TIMES (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 6)
                    (DM-EXPT ITEM EXPO)))
             DEST))
           (T
            (PROGN
             (PUTV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4
                   (INSZZZR
                    ((LAMBDA (G196)
                       (COND
                        ((OR (IDP EXPO) (CONSTP EXPO))
                         (CONS G196 (CONS EXPO NIL)))
                        (T (CONS G196 EXPO))))
                     (CAR (GETCIND ITEM 'VARLST* 'TIMES ROWINDX EXPO)))
                    (GETV (GETV CODMAT (PLUS MAXVAR ROWINDX)) 4)))
             (COND
              ((GET ITEM 'ROWINDEX) (SETPREV ROWINDX (GET ITEM 'ROWINDEX)))
              (T (SETPREV ROWINDX ITEM)))
             DEST))))))))) 
(PUT 'INSQUOTORDR 'NUMBER-OF-ARGS 2) 
(PUT 'INSQUOTORDR 'DEFINED-ON-LINE '1255) 
(PUT 'INSQUOTORDR 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'INSQUOTORDR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSQUOTORDR (INDX ORD)
    (PROG (COL)
      (COND
       ((SETQ COL (GET (GETV QLHS INDX) 'VARLST+))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (SETPREV (CAR Z) ORD)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))))
      (COND
       ((SETQ COL (GET (GETV QLHS INDX) 'VARLST*))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (SETPREV (CAR Z) ORD)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB)))))) 
(PUT 'REMQUOTORDR 'NUMBER-OF-ARGS 2) 
(PUT 'REMQUOTORDR 'DEFINED-ON-LINE '1269) 
(PUT 'REMQUOTORDR 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMQUOTORDR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMQUOTORDR (INDX ORD)
    (PROG (COL)
      (COND
       ((SETQ COL (GET (GETV QLHS INDX) 'VARLST+))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (REMPREV (CAR Z) ORD)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))))
      (COND
       ((SETQ COL (GET (GETV QLHS INDX) 'VARLST*))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (REMPREV (CAR Z) ORD)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB)))))) 
(PUT 'REMPREV 'NUMBER-OF-ARGS 2) 
(PUT 'REMPREV 'DEFINED-ON-LINE '1283) 
(PUT 'REMPREV 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMPREV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMPREV (X Y)
    (COND
     ((NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3))
      (REMPREV (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) Y))
     (T
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 8
            (REMORDR Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 8)))))) 
(PUT 'CHECKNORD 'NUMBER-OF-ARGS 3) 
(PUT 'CHECKNORD 'DEFINED-ON-LINE '1292) 
(PUT 'CHECKNORD 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'CHECKNORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECKNORD (NORD INROW INDX)
    (PROG ()
      (COND
       (INROW
        (PROGN
         (COND
          ((AND (NULL (GETV (GETV CODMAT (PLUS MAXVAR INROW)) 4))
                (NULL (GETV (GETV CODMAT (PLUS MAXVAR INROW)) 5)))
           (PROGN
            (SETQ NORD (GETV (GETV CODMAT (PLUS MAXVAR INROW)) 6))
            (REMQUOTORDR INDX INROW)
            (REMQUOTORDR INDX (GETV (GETV CODMAT (PLUS MAXVAR INROW)) 3))
            (CLEARROW INROW)))
          (T (INSQUOTORDR INDX (GET NORD 'ROWINDEX)))))))
      (RETURN NORD))) 
(PUT 'REMQUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'REMQUOTIENT 'DEFINED-ON-LINE '1315) 
(PUT 'REMQUOTIENT 'DEFINED-IN-FILE 'SCOPE/CODAD2.RED) 
(PUT 'REMQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMQUOTIENT (PNR INDX)
    (PROG (VAR COL ROWINDX)
      (SETQ VAR (GETV QLHS INDX))
      (COND
       ((SETQ COL (GET VAR 'VARLST+))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (REMPREV (CAR Z) VAR)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))))
      (COND
       ((SETQ COL (GET VAR 'VARLST*))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (REMPREV (CAR Z) VAR)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))))
      (TSHRINKCOL (GETV QLHS INDX) PNR 'VARLST+)
      (TSHRINKCOL (GETV QLHS INDX) PNR 'VARLST*)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE QLKVL I)) (RETURN NIL)))
        (PUTV QRHS I (SUBST PNR (GETV QLHS INDX) (GETV QRHS I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((SETQ ROWINDX (GET PNR 'ROWINDEX)) (SETQ PNR ROWINDX)))
      (COND
       ((SETQ COL (GET PNR 'VARLST+))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (SETPREV (CAR Z) PNR)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))))
      (COND
       ((SETQ COL (GET PNR 'VARLST*))
        (PROG (Z)
          (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z) (SETPREV (CAR Z) PNR)) (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB)))))) 
(ENDMODULE) 
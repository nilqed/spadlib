(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'COATESID)) 
(FLUID '(*TRA INTVAR MAGICLIST TAYLORASSLIST TAYLORVARIABLE)) 
(EXPORTS (LIST 'COATESSOLVE 'VECPROD 'COATES-LINEQ)) 
(IMPORTS
 (LIST '*INVSQ '*MULTSQ 'NEGSQ '*ADDSQ 'SWAP 'CHECK-LINEQ 'NON-NULL-VEC
       'PRINTSQ 'SQRT2TOP 'MAPVEC 'MKSP 'VECSORT 'ADDSQ 'MKILIST 'MKVEC 'MAPPLY
       'TAYLORFORMP 'XSUBSTITUTESQ 'TAYLORFORM 'TAYLOREVALUATE 'MULTSQ 'INVSQ
       'REMOVECMSQ)) 
(PUT 'COATESSOLVE 'NUMBER-OF-ARGS 4) 
(PUT 'COATESSOLVE 'DEFINED-ON-LINE '39) 
(PUT 'COATESSOLVE 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'COATESSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATESSOLVE (MZERO PZERO BASIS NORMALS)
    (PROG (M N RIGHTSIDE NNN)
      (COND ((NULL MZERO) (RETURN 'FAILED)))
      (SETQ NNN (MAX (LENGTH NORMALS) 1))
      (SETQ BASIS (MKVEC BASIS))
      (SETQ M (COATESMATRIX MZERO PZERO BASIS NORMALS))
      (SETQ N (UPBV M))
      (SETQ RIGHTSIDE (MKVECT N))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV RIGHTSIDE (DIFFERENCE N I)
              (CONS (COND ((LESSP I NNN) 1) (T NIL)) 1))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ N (COATES-LINEQ M RIGHTSIDE))
      (COND ((EQ N 'FAILED) (RETURN 'FAILED)))
      (SETQ N (REMOVECMSQ (VECPROD N BASIS)))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Answer from linear equation solving is ")
          (TERPRI)
          "Answer from linear equation solving is ")
         (PRINTSQ N))))
      (RETURN N))) 
(PUT 'COATESMATRIX 'NUMBER-OF-ARGS 4) 
(PUT 'COATESMATRIX 'DEFINED-ON-LINE '68) 
(PUT 'COATESMATRIX 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'COATESMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATESMATRIX (MZERO PZERO BASIS NORMALS)
    (PROG (ANS N1 N2 J W SAVE NEXTFLAG SAVE-TAYLORS X-FACTORS NORMALS-OK TEMP)
      (SETQ SAVE-TAYLORS (MKVECT (ISUB1 (LENGTH PZERO))))
      (SETQ SAVE TAYLORASSLIST)
      (SETQ NORMALS-OK NIL)
      (SETQ N1 (UPBV BASIS))
      (SETQ N2
              (PLUS (ISUB1 (MAPPLY (FUNCTION PLUS2) MZERO))
                    (MAX (LENGTH NORMALS) 1)))
      (SETQ TAYLORVARIABLE INTVAR)
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Basis for the functions with precisely the correct poles")
          (TERPRI)
          "Basis for the functions with precisely the correct poles")
         (MAPVEC BASIS (FUNCTION PRINTSQ)))))
      (SETQ ANS (MKVECT N2))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N2 I)) (RETURN NIL)))
        (PUTV ANS I (MKVECT N1))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (PROG (XMZ XPZ K)
          (SETQ XMZ MZERO)
          (SETQ K (SETQ J 0))
          (SETQ XPZ PZERO)
          (PROG ()
           WHILELABEL
            (COND ((NOT XPZ) (RETURN NIL)))
            (PROGN
             (NEWPLACE (BASICPLACE (CAR XPZ)))
             (COND
              (NEXTFLAG
               (SETQ W
                       (TAYLORFORMP
                        (LIST 'BINARYTIMES (GETV SAVE-TAYLORS K)
                              (GETV X-FACTORS K)))))
              ((NOT *TRA)
               (SETQ W (TAYLORFORM (XSUBSTITUTESQ (GETV BASIS I) (CAR XPZ)))))
              (T
               (PROG (FLG U SLISTS)
                 (SETQ U (XSUBSTITUTESQ (GETV BASIS I) (BASICPLACE (CAR XPZ))))
                 (SETQ SLISTS (EXTENPLACE (CAR XPZ)))
                 (PROG (W)
                   (SETQ W (SQRTSINSQ U INTVAR))
                  LAB
                   (COND ((NULL W) (RETURN NIL)))
                   ((LAMBDA (W)
                      (COND ((NOT (ASSOC W SLISTS)) (SETQ FLG (CONS W FLG)))))
                    (CAR W))
                   (SETQ W (CDR W))
                   (GO LAB))
                 (COND
                  (FLG
                   (PROGN
                    (PROGN
                     (PRIN2 "The following square roots were not expected")
                     (TERPRI)
                     "The following square roots were not expected")
                    (MAPC FLG (FUNCTION SUPERPRINT))
                    (PROGN
                     (PRIN2 "in the substitution")
                     (TERPRI)
                     "in the substitution")
                    (SUPERPRINT (CAR XPZ))
                    (PRINTSQ (GETV BASIS I)))))
                 (SETQ W (TAYLORFORM (XSUBSTITUTESQ U SLISTS))))))
             (PUTV SAVE-TAYLORS K W)
             (SETQ K (IADD1 K))
             (PROG (L)
               (SETQ L 0)
              LAB
               (COND ((MINUSP (DIFFERENCE (ISUB1 (CAR XMZ)) L)) (RETURN NIL)))
               (PROGN (ASTORE ANS J I (TAYLOREVALUATE W L)) (SETQ J (IADD1 J)))
               (SETQ L (PLUS2 L 1))
               (GO LAB))
             (COND
              ((AND (NULL NORMALS) (EQUAL J N2))
               (PROGN
                (SETQ TEMP (TAYLOREVALUATE W (CAR XMZ)))
                (ASTORE ANS J I TEMP)
                (SETQ NORMALS-OK (OR NORMALS-OK (CAR TEMP))))))
             (SETQ XPZ (CDR XPZ))
             (SETQ XMZ (CDR XMZ)))
            (GO WHILELABEL))
          (SETQ NEXTFLAG
                  (AND (LESSP I N1)
                       (EQUAL (GETV BASIS I)
                              (MULTSQ
                               (CONS
                                (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)) 1)
                               (GETV BASIS (PLUS I 1))))))
          (COND
           ((AND NEXTFLAG (NULL X-FACTORS))
            (PROGN
             (SETQ X-FACTORS (MKVECT (UPBV SAVE-TAYLORS)))
             (SETQ XPZ PZERO)
             (SETQ K 0)
             (SETQ XMZ
                     (INVSQ
                      (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)) 1)))
             (PROG ()
              WHILELABEL
               (COND ((NOT XPZ) (RETURN NIL)))
               (PROGN
                (PUTV X-FACTORS K (TAYLORFORM (XSUBSTITUTESQ XMZ (CAR XPZ))))
                (SETQ XPZ (CDR XPZ))
                (SETQ K (IADD1 K)))
               (GO WHILELABEL))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((AND (NULL NORMALS) (NULL NORMALS-OK))
        (PROGN
         (COND
          (*TRA
           (PROGN
            (PRIN2 "Our default normalisation condition was vacuous")
            (TERPRI)
            "Our default normalisation condition was vacuous")))
         (ASTORE ANS N2 N1 (CONS 1 1)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT NORMALS) (RETURN NIL)))
        (PROGN
         (SETQ W (CAR NORMALS))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND ((MINUSP (DIFFERENCE N1 K)) (RETURN NIL)))
           (PROGN (ASTORE ANS J K (CAR W)) (SETQ W (CDR W)))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (SETQ J (IADD1 J))
         (SETQ NORMALS (CDR NORMALS)))
        (GO WHILELABEL))
     NIL
      (RETURN ANS))) 
(PUT 'PRINTMATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'PRINTMATRIX 'DEFINED-ON-LINE '158) 
(PUT 'PRINTMATRIX 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'PRINTMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINTMATRIX (ANS N2 N1)
    (COND
     (*TRA
      (PROGN
       (PROGN
        (PRIN2 "Equations to be solved:")
        (TERPRI)
        "Equations to be solved:")
       (PROG (I)
         (SETQ I 0)
        LAB
         (COND ((MINUSP (DIFFERENCE N2 I)) (RETURN NIL)))
         (PROG ()
           (COND ((NULL (GETV ANS I)) (RETURN NIL)))
           (PRINC "Row number ")
           (PRINC I)
           (PROG (J)
             (SETQ J 0)
            LAB
             (COND ((MINUSP (DIFFERENCE N1 J)) (RETURN NIL)))
             (PRINTSQ (GETV (GETV ANS I) J))
             (SETQ J (PLUS2 J 1))
             (GO LAB)))
         (SETQ I (PLUS2 I 1))
         (GO LAB)))))) 
(PUT 'VECPROD 'NUMBER-OF-ARGS 2) 
(PUT 'VECPROD 'DEFINED-ON-LINE '173) 
(PUT 'VECPROD 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'VECPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECPROD (U V)
    (PROG (W N)
      (SETQ W (CONS NIL 1))
      (SETQ N (UPBV U))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ W (*ADDSQ W (*MULTSQ (GETV U I) (GETV V I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN W))) 
(PUT 'COATES-LINEQ 'NUMBER-OF-ARGS 2) 
(PUT 'COATES-LINEQ 'DEFINED-ON-LINE '185) 
(PUT 'COATES-LINEQ 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'COATES-LINEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COATES-LINEQ (M RIGHTSIDE)
    (PROG (NNN N)
      (SETQ NNN (DESPARSE M RIGHTSIDE))
      (COND ((EQ NNN 'FAILED) (RETURN 'FAILED)))
      (SETQ M (CAR NNN))
      (COND ((NULL M) (PROGN (SETQ N (CDDR NNN)) (GO VECPROD))))
      (SETQ RIGHTSIDE (CADR NNN))
      (SETQ NNN (CDDR NNN))
      (SETQ N (CHECK-LINEQ M RIGHTSIDE))
      (COND ((EQ N 'FAILED) (RETURN N)))
      (SETQ N (JHDSOLVE M RIGHTSIDE (NON-NULL-VEC NNN)))
      (COND ((EQ N 'FAILED) (RETURN N)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV N) I)) (RETURN NIL)))
        (COND ((SETQ M (GETV NNN I)) (PUTV N I M)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
     VECPROD
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV N) I)) (RETURN NIL)))
        (COND ((NULL (GETV N I)) (PUTV N I (CONS NIL 1))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN N))) 
(PUT 'JHDSOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'JHDSOLVE 'DEFINED-ON-LINE '215) 
(PUT 'JHDSOLVE 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'JHDSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JHDSOLVE (M RIGHTSIDE IGNORE)
    (PROG (II N1 N2 ANS U ROW SWAPFLG SWAPS)
      (SETQ N1 (UPBV M))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (COND ((SETQ U (GETV M I)) (SETQ N2 (UPBV U))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PRINTMATRIX M N1 N2)
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
             ((CAR (SETQ PIVOT (GETV ROW J))) (PROGN (SETQ K J) (SETQ J N2))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (COND ((NEQ K (MINUS 1)) (GO NEWROW)))
          (COND
           ((CAR (GETV RIGHTSIDE I))
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
          (SETQ PIVOT (SQRT2TOP (NEGSQ (*INVSQ PIVOT))))
          (PROG (J)
            (SETQ J (IADD1 I))
           LAB
            (COND ((MINUSP (DIFFERENCE N1 J)) (RETURN NIL)))
            (PROG ()
              (SETQ U (GETV M J))
              (COND ((NULL U) (RETURN NIL)))
              (SETQ V (*MULTSQ (GETV U I) PIVOT))
              (COND
               ((CAR V)
                (PROGN
                 (PUTV RIGHTSIDE J
                       (*ADDSQ (GETV RIGHTSIDE J)
                               (*MULTSQ V (GETV RIGHTSIDE I))))
                 (PROG (L)
                   (SETQ L 0)
                  LAB
                   (COND ((MINUSP (DIFFERENCE N2 L)) (RETURN NIL)))
                   (PUTV U L (*ADDSQ (GETV U L) (*MULTSQ V (GETV ROW L))))
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
        (COND ((CAR (GETV ROW I)) (SETQ U 'T)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((NULL U)
        (COND ((CAR (GETV RIGHTSIDE N1)) (GO FAILED))
              (T (SETQ N1 (ISUB1 N1))))))
      (COND ((GREATERP N1 N2) (GO FAILED)))
      (SETQ ANS (MKVECT N2))
      (SETQ N2 (DIFFERENCE N2 IGNORE))
      (SETQ II N1)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N1 I)) (RETURN NIL)))
        (COND ((NULL (GETV M I)) (SETQ II (IADD1 II))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((LESSP II N2)
        (PROGN
         (COND
          (*TRA
           (PROGN
            (PROGN
             (PRIN2 "The equations do not completely determine the functions")
             (TERPRI)
             "The equations do not completely determine the functions")
            (PROGN (PRIN2 "Matrix:") (TERPRI) "Matrix:")
            (MAPVEC M (FUNCTION SUPERPRINT))
            (PROGN (PRIN2 "Right-hand side:") (TERPRI) "Right-hand side:")
            (SUPERPRINT RIGHTSIDE)
            ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
             (LIST "Adding new symbols for " (IADD1 II) " ... " N2)))))
         (COND
          (*TRA
           (PROGN
            (PRIN2 "If in doubt consult an expert")
            (TERPRI)
            "If in doubt consult an expert"))))))
      (PROG (I)
        (SETQ I N1)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
        (PROG ()
          (SETQ ROW (GETV M I))
          (COND ((NULL ROW) (RETURN NIL)))
          (SETQ II 0)
          (PROG ()
           WHILELABEL
            (COND ((NOT (NULL (CAR (GETV ROW II)))) (RETURN NIL)))
            (SETQ II (IADD1 II))
            (GO WHILELABEL))
          (SETQ U (GETV RIGHTSIDE I))
          (PROG (J)
            (SETQ J (IADD1 II))
           LAB
            (COND ((MINUSP (DIFFERENCE N2 J)) (RETURN NIL)))
            (PROGN
             (COND
              ((NULL (GETV ANS J))
               (PROGN
                (SETQ MAGICLIST (CONS (GENSYM) MAGICLIST))
                (PUTV ANS J
                      (CONS
                       (LIST (CONS (GETPOWER (FKERN (CAR MAGICLIST)) 1) 1))
                       1)))))
             (SETQ U (*ADDSQ U (*MULTSQ (GETV ROW J) (NEGSQ (GETV ANS J))))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (PUTV ANS II (*MULTSQ U (SQRT2TOP (*INVSQ (GETV ROW II))))))
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
(PUT 'DESPARSE 'NUMBER-OF-ARGS 2) 
(PUT 'DESPARSE 'DEFINED-ON-LINE '350) 
(PUT 'DESPARSE 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'DESPARSE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DESPARSE (MATRX RIGHTSIDE)
    (PROG (VECT CHANGED N M ZERO FAILED)
      (SETQ ZERO (CONS NIL 1))
      (SETQ N (UPBV MATRX))
      (SETQ M (UPBV (GETV MATRX 0)))
      (SETQ VECT (MKVECT M))
      (SETQ CHANGED T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CHANGED (NOT FAILED))) (RETURN NIL)))
        (PROG ()
          (SETQ CHANGED NIL)
          (PROG (I)
            (SETQ I 0)
           LAB
            (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
            (COND ((OR CHANGED FAILED) (SETQ I N))
                  (T
                   (PROG (NZCOUNT ROW PIVOT)
                     (SETQ ROW (GETV MATRX I))
                     (COND ((NULL ROW) (RETURN NIL)))
                     (SETQ NZCOUNT 0)
                     (PROG (J)
                       (SETQ J 0)
                      LAB
                       (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                       (COND
                        ((CAR (GETV ROW J))
                         (PROGN
                          (SETQ NZCOUNT (IADD1 NZCOUNT))
                          (SETQ PIVOT J))))
                       (SETQ J (PLUS2 J 1))
                       (GO LAB))
                     (COND
                      ((EQUAL NZCOUNT 0)
                       (COND
                        ((NULL (CAR (GETV RIGHTSIDE I)))
                         (RETURN (PUTV MATRX I NIL)))
                        (T (RETURN (SETQ FAILED 'FAILED))))))
                     (COND ((GREATERP NZCOUNT 1) (RETURN NIL)))
                     (SETQ NZCOUNT (GETV RIGHTSIDE I))
                     (COND
                      ((NULL (CAR NZCOUNT))
                       (PROGN (PUTV VECT PIVOT ZERO) (GO WAS-ZERO))))
                     (SETQ NZCOUNT (*MULTSQ NZCOUNT (*INVSQ (GETV ROW PIVOT))))
                     (PUTV VECT PIVOT NZCOUNT)
                     (SETQ NZCOUNT (NEGSQ NZCOUNT))
                     (PROG (I)
                       (SETQ I 0)
                      LAB
                       (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                       (COND
                        ((SETQ ROW (GETV MATRX I))
                         (COND
                          ((CAR (SETQ ROW (GETV ROW PIVOT)))
                           (PUTV RIGHTSIDE I
                                 (*ADDSQ (GETV RIGHTSIDE I)
                                         (*MULTSQ ROW NZCOUNT)))))))
                       (SETQ I (PLUS2 I 1))
                       (GO LAB))
                    WAS-ZERO
                     (PROG (I)
                       (SETQ I 0)
                      LAB
                       (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                       (COND ((SETQ ROW (GETV MATRX I)) (PUTV ROW PIVOT ZERO)))
                       (SETQ I (PLUS2 I 1))
                       (GO LAB))
                     (SETQ CHANGED T)
                     (PUTV MATRX I NIL)
                     (SWAP MATRX I N)
                     (SWAP RIGHTSIDE I N))))
            (SETQ I (PLUS2 I 1))
            (GO LAB)))
        (GO WHILELABEL))
      (COND (FAILED (RETURN 'FAILED)))
      (SETQ CHANGED T)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (COND ((GETV MATRX I) (SETQ CHANGED NIL)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND (CHANGED (SETQ MATRX NIL)))
      (RETURN (CONS MATRX (CONS RIGHTSIDE VECT))))) 
(PUT 'ASTORE 'NUMBER-OF-ARGS 4) 
(PUT 'ASTORE 'DEFINED-ON-LINE '417) 
(PUT 'ASTORE 'DEFINED-IN-FILE 'ALGINT/COATESID.RED) 
(PUT 'ASTORE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ASTORE (A I J VAL) (PUTV (GETV A I) J VAL)) 
(ENDMODULE) 
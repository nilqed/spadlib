(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODOPT)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL
 '(PSI JSI NPSI NJSI WSI RCOCCUP ROCCUP1 ROCCUP2 NEWJSI NEWNJSI CODHISTO
   HEADHISTO ROWMIN ROWMAX)) 
(SETQ RCOCCUP (SETQ ROCCUP1 (SETQ ROCCUP2 NIL))) 
(PUT 'EXTBRSEA 'NUMBER-OF-ARGS 0) 
(PUT 'EXTBRSEA 'DEFINED-ON-LINE '214) 
(PUT 'EXTBRSEA 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'EXTBRSEA 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXTBRSEA NIL
    (PROG (FURTHER)
      (PROG (X)
        (SETQ X ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (COND
         ((OR (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1))
              (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 2)))
          (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL))
         (T (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)))
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (INITBRSEA)
      (EXTBRSEA1)
      (PROG ()
       REPEATLABEL
        (PROGN
         (EXPANDPROD)
         (PROG (X)
           (SETQ X ROWMIN)
          LAB
           (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
           (COND
            ((AND
              (NOT (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1)))
              (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) 'TIMES))
             (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T))
            (T (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL)))
           (SETQ X (PLUS2 X 1))
           (GO LAB))
         (INITBRSEA)
         (EXTBRSEA1)
         (SETQ FURTHER (SHRINKPROD))
         NIL)
        (COND ((NOT (NOT FURTHER)) (GO REPEATLABEL)))))) 
(PUT 'INITBRSEA 'NUMBER-OF-ARGS 0) 
(PUT 'INITBRSEA 'DEFINED-ON-LINE '272) 
(PUT 'INITBRSEA 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'INITBRSEA 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INITBRSEA NIL
    (PROG (HLEN)
      (SETQ HLEN 200)
      (PROG (X)
        (SETQ X ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (COND ((GETV (GETV CODMAT (PLUS MAXVAR X)) 0) (INITWGHT X)))
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (REDCODMAT)
      (COND
       (CODHISTO
        (PROG (X)
          (SETQ X 0)
         LAB
          (COND ((MINUSP (DIFFERENCE 200 X)) (RETURN NIL)))
          (PUTV CODHISTO X NIL)
          (SETQ X (PLUS2 X 1))
          (GO LAB)))
       (T (SETQ CODHISTO (MKVECT HLEN))))
      (SETQ HEADHISTO 0)
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (INSHISTO X)
        (SETQ X (PLUS2 X 1))
        (GO LAB)))) 
(PUT 'REDCODMAT 'NUMBER-OF-ARGS 0) 
(PUT 'REDCODMAT 'DEFINED-ON-LINE '307) 
(PUT 'REDCODMAT 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'REDCODMAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDCODMAT NIL
    (PROG (X)
      (SETQ X ROWMIN)
     LAB
      (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
      (TESTRED X)
      (SETQ X (PLUS2 X 1))
      (GO LAB))) 
(PUT 'TESTRED 'NUMBER-OF-ARGS 1) 
(PUT 'TESTRED 'DEFINED-ON-LINE '314) 
(PUT 'TESTRED 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'TESTRED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTRED (X)
    (COND
     ((AND (GETV (GETV CODMAT (PLUS MAXVAR X)) 0)
           (LESSP (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 2))
      (PROGN
       (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL)
       (PROG (Z)
         (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
        LAB
         (COND ((NULL Z) (RETURN NIL)))
         ((LAMBDA (Z)
            (PROGN (DOWNWGHT1 (CAR Z) (CAR (CDR Z))) (TESTRED (CAR Z))))
          (CAR Z))
         (SETQ Z (CDR Z))
         (GO LAB)))))) 
(PUT 'EXTBRSEA1 'NUMBER-OF-ARGS 0) 
(PUT 'EXTBRSEA1 'DEFINED-ON-LINE '337) 
(PUT 'EXTBRSEA1 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'EXTBRSEA1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXTBRSEA1 NIL
    (PROG (HR HC X)
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ HR (FINDHR))) (RETURN NIL)))
        (COND
         ((SETQ HC (FINDHC HR))
          (PROGN
           (SETQ WSI 0)
           (PROG ()
            WHILELABEL
             (COND
              ((NOT
                (NOT
                 (NULL
                  (SETQ X (FINDOPTROW HR HC (PLUS (QUOTIENT WSI NPSI) 1))))))
               (RETURN NIL)))
             (BRUPDATE X)
             (GO WHILELABEL))
           (PROG (X)
             (SETQ X ROCCUP1)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X) (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)) (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (PROG (X)
             (SETQ X ROCCUP2)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X) (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)) (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (SETQ ROCCUP1 (SETQ ROCCUP2 NIL))
           (COND
            ((GREATERP WSI 0)
             (PROGN
              (PROG (X)
                (SETQ X RCOCCUP)
               LAB
                (COND ((NULL X) (RETURN NIL)))
                ((LAMBDA (X) (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)) (CAR X))
                (SETQ X (CDR X))
                (GO LAB))
              (SETQ RCOCCUP NIL)
              (ADDCSE)))
            ((EQUAL NPSI 1)
             (PROGN
              (PUTV (GETV CODMAT (PLUS MAXVAR HC)) 0 NIL)
              (SETQ RCOCCUP (CONS HC RCOCCUP)))))))
         (T
          (PROGN
           (PROG (X)
             (SETQ X RCOCCUP)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X) (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)) (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (SETQ RCOCCUP NIL)
           (ROWDEL HR)
           (TESTREDZZ HR))))
        (GO WHILELABEL)))) 
(PUT 'FINDHR 'NUMBER-OF-ARGS 0) 
(PUT 'FINDHR 'DEFINED-ON-LINE '437) 
(PUT 'FINDHR 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'FINDHR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FINDHR NIL
    (PROG (X)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (GREATERP HEADHISTO 0)
                (NULL (SETQ X (GETV CODHISTO HEADHISTO)))))
          (RETURN NIL)))
        (SETQ HEADHISTO (DIFFERENCE HEADHISTO 1))
        (GO WHILELABEL))
      (COND
       (X
        (PROGN
         (SETQ PSI (LIST X))
         (SETQ NPSI 1)
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL)
         (SETQ RCOCCUP (CONS X RCOCCUP)))))
      (RETURN X))) 
(PUT 'FINDHC 'NUMBER-OF-ARGS 1) 
(PUT 'FINDHC 'DEFINED-ON-LINE '461) 
(PUT 'FINDHC 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'FINDHC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDHC (HR)
    (PROG (Y Y1 AW AWMAX)
      (SETQ AWMAX (SETQ NJSI 0))
      (SETQ JSI NIL)
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR HR)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((GETV (GETV CODMAT (PLUS MAXVAR (SETQ Y1 (CAR Z)))) 0)
             (PROGN
              (SETQ JSI (CONS Y1 JSI))
              (SETQ NJSI (PLUS NJSI 1))
              (COND
               ((GREATERP
                 (SETQ AW (CAAR (GETV (GETV CODMAT (PLUS MAXVAR Y1)) 1)))
                 AWMAX)
                (PROGN (SETQ AWMAX AW) (SETQ Y Y1))))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (SETQ JSI (REVERSE JSI))
      (RETURN Y))) 
(PUT 'FINDOPTROW 'NUMBER-OF-ARGS 3) 
(PUT 'FINDOPTROW 'DEFINED-ON-LINE '492) 
(PUT 'FINDOPTROW 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'FINDOPTROW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FINDOPTROW (HR HC LMAX)
    (PROG (L S X X1 BIL)
      (SETQ BIL
              (CAR
               (CDR
                (CAR (PNTHXZZ HC (GETV (GETV CODMAT (PLUS MAXVAR HR)) 4))))))
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR HC)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((GETV (GETV CODMAT (PLUS MAXVAR (SETQ X1 (CAR Z)))) 0)
             (PROGN
              (COND
               ((NULL (CDR (SETQ S (TESTPR X1 HR (CAR (CDR Z)) BIL))))
                (SETQ ROCCUP1 (CONS X1 ROCCUP1)))
               (T
                (PROGN
                 (COND
                  ((GREATERP (SETQ L (LENGTH S)) LMAX)
                   (PROGN
                    (SETQ NEWNJSI (SETQ LMAX L))
                    (SETQ X X1)
                    (SETQ NEWJSI S))))
                 (SETQ ROCCUP2 (CONS X1 ROCCUP2)))))
              (PUTV (GETV CODMAT (PLUS MAXVAR X1)) 0 NIL)))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN X))) 
(PUT 'TESTPR 'NUMBER-OF-ARGS 4) 
(PUT 'TESTPR 'DEFINED-ON-LINE '537) 
(PUT 'TESTPR 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'TESTPR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TESTPR (X HR BKL BIL)
    (PROG (ZZ ZZHR X1 Y P LJSI CLJSI)
      (SETQ LJSI JSI)
      (SETQ ZZ (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
      (SETQ ZZHR (GETV (GETV CODMAT (PLUS MAXVAR HR)) 4))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LJSI ZZ)) (RETURN NIL)))
        (COND
         ((EQUAL (SETQ CLJSI (CAR LJSI)) (SETQ X1 (CAR (CAR ZZ))))
          (PROGN
           (SETQ ZZHR (PNTHXZZ CLJSI ZZHR))
           (COND
            ((ZEROPP
              (DM-DIFFERENCE (DM-TIMES (CAR (CDR (CAR ZZ))) BIL)
               (DM-TIMES (CAR (CDR (CAR ZZHR))) BKL)))
             (SETQ P (CONS CLJSI P))))
           (SETQ LJSI (CDR LJSI))
           (SETQ ZZ (CDR ZZ))))
         ((GREATERP CLJSI X1) (SETQ ZZ (CDR ZZ))) (T (SETQ LJSI (CDR LJSI))))
        (GO WHILELABEL))
      (RETURN P))) 
(PUT 'BRUPDATE 'NUMBER-OF-ARGS 1) 
(PUT 'BRUPDATE 'DEFINED-ON-LINE '599) 
(PUT 'BRUPDATE 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'BRUPDATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BRUPDATE (X)
    (PROGN
     (SETQ PSI (CONS X PSI))
     (SETQ NPSI (PLUS NPSI 1))
     (SETQ JSI (REVERSE NEWJSI))
     (SETQ NJSI NEWNJSI)
     (SETQ WSI (TIMES (DIFFERENCE NJSI 1) (DIFFERENCE NPSI 1)))
     (PROG (X)
       (SETQ X ROCCUP2)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (SETQ ROCCUP2 NIL)
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL)
     (SETQ RCOCCUP (CONS X RCOCCUP)))) 
(PUT 'ADDCSE 'NUMBER-OF-ARGS 0) 
(PUT 'ADDCSE 'DEFINED-ON-LINE '625) 
(PUT 'ADDCSE 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'ADDCSE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ADDCSE NIL
    (PROG (ZZ ZZR ZZC LZZR LZZC OPV VAR GC FLT MIN)
      (SETQ ZZR (SETQ LZZR (RZSTRTCSE)))
      (SETQ LZZC (CZSTRTCSE (CAR (CDR (CAR ZZR)))))
      (SETQ GC (DM-ABS (CAR (CDR (CAR LZZC)))))
      (SETQ MIN GC)
      (SETQ FLT (FLOATPROP GC))
      (PROG (ZZ)
        (SETQ ZZ LZZC)
       LAB
        (COND ((NULL ZZ) (RETURN NIL)))
        ((LAMBDA (ZZ)
           (PROGN
            (SETQ FLT (OR FLT (FLOATPROP (CAR (CDR ZZ)))))
            (SETQ MIN (DM-MIN MIN (DM-ABS (CAR (CDR ZZ)))))
            (COND ((NOT FLT) (SETQ GC (GCD2 GC (ABS (CAR (CDR ZZ)))))))))
         (CAR ZZ))
        (SETQ ZZ (CDR ZZ))
        (GO LAB))
      (COND (FLT (SETQ GC MIN)))
      (COND
       ((NOT (|:ONEP| GC))
        (PROGN
         (SETQ ZZ NIL)
         (PROG (Z)
           (SETQ Z ZZR)
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (SETQ ZZ
                      (CONS
                       ((LAMBDA (G142)
                          (COND
                           ((OR (IDP G142) (CONSTP G142))
                            (CONS (CAR Z) (CONS G142 NIL)))
                           (T (CONS (CAR Z) G142))))
                        (DM-TIMES (CAR (CDR Z)) GC))
                       ZZ)))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (SETQ ZZR (SETQ LZZR (REVERSE ZZ)))
         (SETQ ZZ NIL)
         (PROG (Z)
           (SETQ Z LZZC)
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (SETQ ZZ
                      (CONS
                       ((LAMBDA (G144)
                          (COND
                           ((OR (IDP G144) (CONSTP G144))
                            (CONS (CAR Z) (CONS G144 NIL)))
                           (T (CONS (CAR Z) G144))))
                        (DM-QUOTIENT (CAR (CDR Z)) GC))
                       ZZ)))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (SETQ LZZC (REVERSE ZZ)))))
      (SETQ ZZ NIL)
      (SETQ VAR (FNEWSYM))
      (SETQ ROWMAX (PLUS ROWMAX 1))
      (SETROW ROWMAX (SETQ OPV (GETV (GETV CODMAT (PLUS MAXVAR (CAR JSI))) 2))
       VAR (LIST NIL) ZZR)
      (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
      (SETROW ROWMIN OPV VAR NIL NIL)
      (COND ((EQ OPV 'PLUS) (PUT VAR 'VARLST+ ROWMIN))
            (T (PUT VAR 'VARLST* ROWMIN)))
      (PUT VAR 'ROWINDEX ROWMAX)
      (PROG (X)
        (SETQ X PSI)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ ZZ (REMZZZZ ZZR (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))
            (SETQ ZZC (CONS (CAR LZZC) ZZC))
            (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                  (CONS
                   ((LAMBDA (G145 G146)
                      (COND
                       ((OR (IDP G146) (CONSTP G146))
                        (CONS G145 (CONS G146 NIL)))
                       (T (CONS G145 G146))))
                    ROWMIN (CDR (CAR LZZC)))
                   ZZ))
            (DELHISTO X)
            (INITWGHT X)
            (INSHISTO X)
            (SETPREV X ROWMAX)
            (SETQ LZZC (CDR LZZC))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (Y)
        (SETQ Y JSI)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4
                  (CONS
                   ((LAMBDA (G147 G148)
                      (COND
                       ((OR (IDP G148) (CONSTP G148))
                        (CONS G147 (CONS G148 NIL)))
                       (T (CONS G147 G148))))
                    ROWMAX (CDR (CAR LZZR)))
                   (REMZZZZ ZZC (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))))
            (SETQ LZZR (CDR LZZR))
            (INITWGHT Y)))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (PUTV (GETV CODMAT (PLUS MAXVAR ROWMIN)) 4 ZZC)
      (INITWGHT ROWMAX)
      (INSHISTO ROWMAX)
      (INITWGHT ROWMIN)
      (PROG (X)
        (SETQ X JSI)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (TESTREDH X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X PSI)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (TESTREDH X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'RZSTRTCSE 'NUMBER-OF-ARGS 0) 
(PUT 'RZSTRTCSE 'DEFINED-ON-LINE '741) 
(PUT 'RZSTRTCSE 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'RZSTRTCSE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RZSTRTCSE NIL
    (PROG (LJSI ZZ ZZCSE GC FLT MIN)
      (SETQ ZZ
              (PNTHXZZ (CAR JSI)
               (GETV (GETV CODMAT (PLUS MAXVAR (CAR PSI))) 4)))
      (SETQ ZZCSE (LIST (CAR ZZ)))
      (SETQ GC (DM-ABS (CAR (CDR (CAR ZZ)))))
      (SETQ MIN GC)
      (SETQ FLT (FLOATPROP GC))
      (PROG (LJSI)
        (SETQ LJSI (CDR JSI))
       LAB
        (COND ((NULL LJSI) (RETURN NIL)))
        ((LAMBDA (LJSI)
           (PROGN
            (SETQ ZZ (PNTHXZZ LJSI ZZ))
            (SETQ FLT (OR FLT (FLOATPROP (CAR (CDR (CAR ZZ))))))
            (SETQ MIN (DM-MIN MIN (DM-ABS (CAR (CDR (CAR ZZ))))))
            (COND ((NOT FLT) (SETQ GC (GCD2 GC (ABS (CAR (CDR (CAR ZZ))))))))
            (SETQ ZZCSE (CONS (CAR ZZ) ZZCSE))))
         (CAR LJSI))
        (SETQ LJSI (CDR LJSI))
        (GO LAB))
      (COND (FLT (SETQ GC MIN)))
      (RETURN
       (COND ((OR (|:ONEP| GC) (EXPSHRTEST)) (REVERSE ZZCSE))
             (T
              (PROGN
               (SETQ ZZ NIL)
               (PROG (Z)
                 (SETQ Z ZZCSE)
                LAB
                 (COND ((NULL Z) (RETURN NIL)))
                 ((LAMBDA (Z)
                    (SETQ ZZ
                            (CONS
                             ((LAMBDA (G150)
                                (COND
                                 ((OR (IDP G150) (CONSTP G150))
                                  (CONS (CAR Z) (CONS G150 NIL)))
                                 (T (CONS (CAR Z) G150))))
                              (DM-QUOTIENT (CAR (CDR Z)) GC))
                             ZZ)))
                  (CAR Z))
                 (SETQ Z (CDR Z))
                 (GO LAB))
               ZZ)))))) 
(PUT 'GCD2 'NUMBER-OF-ARGS 2) 
(PUT 'GCD2 'DEFINED-ON-LINE '809) 
(PUT 'GCD2 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'GCD2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCD2 (A1 A2)
    (PROG (A3)
      (SETQ A3 (REMAINDER A1 A2))
      (RETURN (COND ((EQUAL A3 0) A2) (T (GCD2 A2 A3)))))) 
(PUT 'EXPSHRTEST 'NUMBER-OF-ARGS 0) 
(PUT 'EXPSHRTEST 'DEFINED-ON-LINE '822) 
(PUT 'EXPSHRTEST 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'EXPSHRTEST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXPSHRTEST NIL
    (PROG (LJSI FURTHER)
      (COND
       ((NOT (EQ (GETV (GETV CODMAT (PLUS MAXVAR (CAR JSI))) 2) 'PLUS))
        (PROGN
         (SETQ LJSI JSI)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND LJSI (NOT FURTHER))) (RETURN NIL)))
           (PROGN
            (SETQ FURTHER
                    (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR (CAR LJSI))) 3)
                           (MINUS 2)))
            (SETQ LJSI (CDR LJSI)))
           (GO WHILELABEL)))))
      (RETURN FURTHER))) 
(PUT 'CZSTRTCSE 'NUMBER-OF-ARGS 1) 
(PUT 'CZSTRTCSE 'DEFINED-ON-LINE '839) 
(PUT 'CZSTRTCSE 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'CZSTRTCSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CZSTRTCSE (IV)
    (PROG (LPSI ZZ ZZCSE)
      (SETQ ZZ (GETV (GETV CODMAT (PLUS MAXVAR (CAR JSI))) 4))
      (SETQ LPSI (ORDN PSI))
      (SETQ PSI NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT LPSI) (RETURN NIL)))
        (PROGN
         (SETQ ZZ (PNTHXZZ (CAR LPSI) ZZ))
         (SETQ ZZCSE
                 (CONS
                  ((LAMBDA (G152)
                     (COND
                      ((OR (IDP G152) (CONSTP G152))
                       (CONS (CAR LPSI) (CONS G152 NIL)))
                      (T (CONS (CAR LPSI) G152))))
                   (DM-QUOTIENT (CAR (CDR (CAR ZZ))) IV))
                  ZZCSE))
         (SETQ PSI (CONS (CAR LPSI) PSI))
         (SETQ LPSI (CDR LPSI)))
        (GO WHILELABEL))
      (RETURN ZZCSE))) 
(PUT 'TESTREDZZ 'NUMBER-OF-ARGS 1) 
(PUT 'TESTREDZZ 'DEFINED-ON-LINE '879) 
(PUT 'TESTREDZZ 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'TESTREDZZ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTREDZZ (X)
    (PROG (Z)
      (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
     LAB
      (COND ((NULL Z) (RETURN NIL)))
      ((LAMBDA (Z) (TESTREDH (CAR Z))) (CAR Z))
      (SETQ Z (CDR Z))
      (GO LAB))) 
(PUT 'TESTREDH 'NUMBER-OF-ARGS 1) 
(PUT 'TESTREDH 'DEFINED-ON-LINE '887) 
(PUT 'TESTREDH 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'TESTREDH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTREDH (X)
    (COND
     ((AND (GETV (GETV CODMAT (PLUS MAXVAR X)) 0)
           (LESSP (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 2))
      (PROGN (ROWDEL X) (TESTREDZZ X))))) 
(PUT 'EXPANDPROD 'NUMBER-OF-ARGS 0) 
(PUT 'EXPANDPROD 'DEFINED-ON-LINE '898) 
(PUT 'EXPANDPROD 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'EXPANDPROD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXPANDPROD NIL
    (PROG (VAR PCVARY PCDVAR ZZR IVALZ N M NPCDVAR NPCDV COL* RELCOLS)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'TIMES)
               (NOT (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3)))
               (TESTREL Y))
          (SETQ RELCOLS (CONS Y RELCOLS))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (PROG (Y)
        (SETQ Y RELCOLS)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3))
            (SETQ PCVARY (SETQ PCDVAR (SETQ ZZR NIL)))
            (PROG (ZEL)
              (SETQ ZEL (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
             LAB
              (COND ((NULL ZEL) (RETURN NIL)))
              ((LAMBDA (ZEL)
                 (COND
                  ((NOT (EQUAL (SETQ IVALZ (CAR (CDR ZEL))) 1))
                   (PROGN
                    (RPLACA (CDR ZEL) 1)
                    (SETQ PCVARY
                            (INSPCVV (CAR ZEL) (DIFFERENCE IVALZ 1)
                             PCVARY))))))
               (CAR ZEL))
              (SETQ ZEL (CDR ZEL))
              (GO LAB))
            (SETQ PCDVAR (INSPCVV Y 1 PCDVAR))
            (SETQ N 0)
            (SETQ NPCDV (SETQ NPCDVAR (GET VAR 'NPCDVAR)))
            (PROG (PC)
              (SETQ PC PCVARY)
             LAB
              (COND ((NULL PC) (RETURN NIL)))
              ((LAMBDA (PC)
                 (PROGN
                  (COND
                   (NPCDVAR
                    (PROGN
                     (SETQ COL* (CAR NPCDVAR))
                     (SETQ NPCDVAR (CDR NPCDVAR))
                     NIL))
                   (T
                    (PROGN
                     (SETQ COL* (SETQ ROWMIN (DIFFERENCE ROWMIN 1)))
                     (SETQ NPCDV (CONS COL* NPCDV))
                     NIL)))
                  (SETQ ZZR
                          (CONS
                           ((LAMBDA (G154)
                              (COND
                               ((OR (IDP G154) (CONSTP G154))
                                (CONS COL* (CONS G154 NIL)))
                               (T (CONS COL* G154))))
                            (DIFFERENCE (CAR PC) N))
                           ZZR))
                  (SETROW COL* 'TIMES (MINUS 2) NIL NIL)
                  (PROG (X)
                    (SETQ X (CDR PC))
                   LAB
                    (COND ((NULL X) (RETURN NIL)))
                    ((LAMBDA (X)
                       (PROG (Z)
                         (SETQ Z ZZR)
                        LAB
                         (COND ((NULL Z) (RETURN NIL)))
                         ((LAMBDA (Z)
                            (PROGN
                             (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4
                                   (INSZZZR Z
                                    (GETV (GETV CODMAT (PLUS MAXVAR X)) 4)))
                             (PUTV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 4
                                   (INSZZZ
                                    (COND
                                     ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                                      (CONS X (CONS (CDR Z) NIL)))
                                     (T (CONS X (CDR Z))))
                                    (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z)))
                                          4)))))
                          (CAR Z))
                         (SETQ Z (CDR Z))
                         (GO LAB)))
                     (CAR X))
                    (SETQ X (CDR X))
                    (GO LAB))
                  (SETQ PCDVAR (INSPCVV COL* (DIFFERENCE (CAR PC) N) PCDVAR))
                  (SETQ N (CAR PC))
                  NIL))
               (CAR PC))
              (SETQ PC (CDR PC))
              (GO LAB))
            (PUT VAR 'PCDVAR PCDVAR)
            (PUT VAR 'NPCDVAR NPCDV)
            NIL))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB)))) 
(PUT 'TESTREL 'NUMBER-OF-ARGS 1) 
(PUT 'TESTREL 'DEFINED-ON-LINE '1068) 
(PUT 'TESTREL 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'TESTREL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTREL (COLINDEX)
    (PROG (BTST MN RCOL RELCOLS RELROW ONEROWS OROWS)
      (COND
       ((SETQ BTST
                (FLAGP (LIST (GETV (GETV CODMAT (PLUS MAXVAR COLINDEX)) 3))
                       'EXPSHR))
        (REMFLAG (LIST (GETV (GETV CODMAT (PLUS MAXVAR COLINDEX)) 3)) 'EXPSHR))
       (T
        (PROGN
         (SETQ MN 0)
         (PROG (Z)
           (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COLINDEX)) 4))
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (COND
               ((GREATERP (CAR (CDR Z)) 1)
                (PROGN
                 (SETQ MN (PLUS MN 1))
                 (COND ((EQUAL MN 1) (SETQ RELROW (CAR Z))))))
               (T (SETQ ONEROWS (CONS (CAR Z) ONEROWS)))))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (COND
          ((AND (NOT (SETQ BTST (GREATERP MN 1))) (EQUAL MN 1) ONEROWS
                (GREATERP (LENGTH (GETV (GETV CODMAT (PLUS MAXVAR RELROW)) 4))
                          1))
           (PROGN
            (SETQ MN 0)
            (PROG (Z)
              (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR RELROW)) 4))
             LAB
              (COND ((NULL Z) (RETURN NIL)))
              ((LAMBDA (Z)
                 (COND
                  ((NEQ (CAR Z) COLINDEX)
                   (PROGN
                    (SETQ MN (PLUS MN 1))
                    (SETQ RELCOLS (CONS (CAR Z) RELCOLS))))))
               (CAR Z))
              (SETQ Z (CDR Z))
              (GO LAB))
            (COND
             ((GREATERP MN 0)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND RELCOLS (NOT BTST))) (RETURN NIL)))
                (PROGN
                 (SETQ RCOL (CAR RELCOLS))
                 (SETQ RELCOLS (CDR RELCOLS))
                 (SETQ OROWS ONEROWS)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND OROWS (NOT BTST))) (RETURN NIL)))
                   (PROGN
                    (SETQ BTST
                            (PNTHXZZ (CAR OROWS)
                             (GETV (GETV CODMAT (PLUS MAXVAR RCOL)) 4)))
                    (SETQ OROWS (CDR OROWS)))
                   (GO WHILELABEL)))
                (GO WHILELABEL))))))))))
      (RETURN BTST))) 
(PUT 'INSPCVV 'NUMBER-OF-ARGS 3) 
(PUT 'INSPCVV 'DEFINED-ON-LINE '1113) 
(PUT 'INSPCVV 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'INSPCVV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INSPCVV (X IV S)
    (COND ((NULL S) (LIST (CONS IV (LIST X))))
          ((DM-EQ IV (CAAR S)) (CONS (CONS IV (CONS X (CDAR S))) (CDR S)))
          ((DM-LT IV (CAAR S)) (CONS (CONS IV (LIST X)) S))
          (T (CONS (CAR S) (INSPCVV X IV (CDR S)))))) 
(PUT 'SHRINKPROD 'NUMBER-OF-ARGS 0) 
(PUT 'SHRINKPROD 'DEFINED-ON-LINE '1132) 
(PUT 'SHRINKPROD 'DEFINED-IN-FILE 'SCOPE/CODOPT.RED) 
(PUT 'SHRINKPROD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SHRINKPROD NIL
    (PROG (VAR PCDVAR ZZ ZSTREET EL EXP COLLST INDX FURTHER)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND
           (NOT (NUMBERP (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3))))
           (SETQ PCDVAR (GET VAR 'PCDVAR))
           (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'TIMES))
          (PROGN
           (SETQ ZSTREET (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
           (PROG (PCD)
             (SETQ PCD PCDVAR)
            LAB
             (COND ((NULL PCD) (RETURN NIL)))
             ((LAMBDA (PCD)
                (PROGN
                 (SETQ EXP (CAR PCD))
                 (SETQ COLLST (DELETE Y (CDR PCD)))
                 (PROG (COL)
                   (SETQ COL COLLST)
                  LAB
                   (COND ((NULL COL) (RETURN NIL)))
                   ((LAMBDA (COL)
                      (PROGN
                       (PROG (Z)
                         (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR COL)) 4))
                        LAB
                         (COND ((NULL Z) (RETURN NIL)))
                         ((LAMBDA (Z)
                            (PROGN
                             (SETQ INDX (CAR Z))
                             (COND
                              ((SETQ EL (ASSOC INDX ZSTREET))
                               (RPLACA (CDR EL) (PLUS (CAR (CDR EL)) EXP)))
                              (T
                               (PROGN
                                (SETQ ZSTREET
                                        (INSZZZ
                                         (SETQ EL
                                                 (COND
                                                  ((OR (IDP EXP) (CONSTP EXP))
                                                   (CONS INDX (CONS EXP NIL)))
                                                  (T (CONS INDX EXP))))
                                         ZSTREET))
                                (PUTV (GETV CODMAT (PLUS MAXVAR INDX)) 4
                                      (INSZZZR
                                       (COND
                                        ((OR (IDP (CDR EL)) (CONSTP (CDR EL)))
                                         (CONS Y (CONS (CDR EL) NIL)))
                                        (T (CONS Y (CDR EL))))
                                       (GETV (GETV CODMAT (PLUS MAXVAR INDX))
                                             4))))))
                             (PUTV (GETV CODMAT (PLUS MAXVAR INDX)) 4
                                   (DELYZZ COL
                                    (GETV (GETV CODMAT (PLUS MAXVAR INDX))
                                          4)))))
                          (CAR Z))
                         (SETQ Z (CDR Z))
                         (GO LAB))
                       (CLEARROW COL)))
                    (CAR COL))
                   (SETQ COL (CDR COL))
                   (GO LAB))))
              (CAR PCD))
             (SETQ PCD (CDR PCD))
             (GO LAB))
           (PUTV (GETV CODMAT (PLUS MAXVAR Y)) 4 ZSTREET)
           (REMPROP VAR 'PCDVAR)
           (COND
            ((TESTREL Y)
             (PROGN (SETQ FURTHER T) (FLAG (LIST VAR) 'EXPSHR)))))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (RETURN FURTHER))) 
(ENDMODULE) 
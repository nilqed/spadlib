(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REDFRONT)) 
(REVISION 'REDFRONT
          "$Id: redfront.red 6781 2024-04-25 09:15:44Z arthurcnorman $") 
(COPYRIGHT 'REDFRONT "(c) 1999-2009 A. Dolzmannm, T. Sturm, 2010-2017 T. Sturm") 
(GLOBAL '(STATCOUNTER)) 
(FLUID '(POSN* ORIG*)) 
(PUT 'REDFRONT_OH 'NUMBER-OF-ARGS 2) 
(PUT 'REDFRONT_OH 'DEFINED-ON-LINE '36) 
(PUT 'REDFRONT_OH 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_OH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDFRONT_OH (M L)
    (PROG (OUTPUTHANDLER*)
      (COND
       ((EQ M 'MAPRIN)
        (COND ((OR OFL* (NEQ POSN* ORIG*)) (MAPRIN L))
              (T (PROGN (REDFRONT_ON) (ASSGNPRI L NIL NIL) (REDFRONT_OFF)))))
       ((EQ M 'PRIN2*) (PRIN2* L)) ((EQ M 'TERPRI) (TERPRI* L))
       ((EQ M 'ASSGNPRI)
        (PROGN (REDFRONT_ON) (ASSGNPRI (CAR L) NIL NIL) (REDFRONT_OFF)))
       (T (REDERR (LIST "unknown method " M " in redfront_oh")))))) 
(PUT 'REDFRONT_ON 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_ON 'DEFINED-ON-LINE '59) 
(PUT 'REDFRONT_ON 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_ON 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_ON NIL (PROGN (TERPRI* NIL) (PRIN2 (INT2ID 3)) (TERPRI* NIL))) 
(PUT 'REDFRONT_OFF 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_OFF 'DEFINED-ON-LINE '66) 
(PUT 'REDFRONT_OFF 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_OFF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_OFF NIL (PROGN (TERPRI* NIL) (PRIN2 (INT2ID 4)))) 
(PUT 'REDFRONT_FORMWRITE 'NUMBER-OF-ARGS 3) 
(PUT 'REDFRONT_FORMWRITE 'DEFINED-ON-LINE '72) 
(PUT 'REDFRONT_FORMWRITE 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_FORMWRITE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDFRONT_FORMWRITE (U VARS MODE)
    (PROG (Z)
      (SETQ Z (FORMWRITE U VARS MODE))
      (COND ((NULL Z) (RETURN NIL)))
      (RETURN
       (LIST 'COND
             (LIST
              (LIST 'AND (LIST 'EQ 'OUTPUTHANDLER* ''REDFRONT_OH) '(NOT OFL*))
              (LIST 'PROG '(OUTPUTHANDLER*) '(REDFRONT_ON) Z '(REDFRONT_OFF)))
             (LIST T Z))))) 
(PUT 'WRITE 'FORMFN 'REDFRONT_FORMWRITE) 
(SETQ OUTPUTHANDLER* 'REDFRONT_OH) 
(FLUID
 '(PROMPTSTRING* REDFRONT_SWITCHES* REDFRONT_SWITCHES-THIS-SL* LISPSYSTEM*
   BREAKLEVEL* INPUT-LIBRARIES OUTPUT-LIBRARY)) 
(SETQ REDFRONT_SWITCHES* (LIST *MSG *OUTPUT)) 
(OFF1 'MSG) 
(OFF1 'OUTPUT) 
(PUT 'REDFRONT_PSLP 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_PSLP 'DEFINED-ON-LINE '96) 
(PUT 'REDFRONT_PSLP 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_PSLP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_PSLP NIL (MEMQ 'PSL LISPSYSTEM*)) 
(COND
 ((REDFRONT_PSLP)
  (PROGN (SETQ REDFRONT_SWITCHES-THIS-SL* (LIST *USERMODE)) (OFF1 'USERMODE)))) 
(PUT 'REDFRONT_COLOR 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_COLOR 'DEFINED-ON-LINE '104) 
(PUT 'REDFRONT_COLOR 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_COLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_COLOR (C)
    (COND
     ((STRINGP C)
      (COMPRESS
       (CONS '|"|
             (CONS (INT2ID 1)
                   (REVERSIP
                    (CONS '|"|
                          (CONS (INT2ID 2)
                                (CDR (REVERSIP (CDR (EXPLODE C)))))))))))
     (T
      (INTERN
       (COMPRESS (CONS (INT2ID 1) (NCONC (EXPLODE C) (LIST (INT2ID 2))))))))) 
(PUT 'REDFRONT_UNCOLOR 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_UNCOLOR 'DEFINED-ON-LINE '111) 
(PUT 'REDFRONT_UNCOLOR 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_UNCOLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_UNCOLOR (C)
    (COND
     ((STRINGP C)
      (COMPRESS
       (CONS '|"|
             (REVERSIP (CONS '|"| (CDDR (REVERSIP (CDDR (EXPLODE C)))))))))
     (T
      (INTERN
       (COMPRESS (CONS '! (REVERSIP (CDR (REVERSIP (CDR (EXPLODE C))))))))))) 
(PUT 'REDFRONT_SETPCHAR-PSL 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_SETPCHAR-PSL 'DEFINED-ON-LINE '117) 
(PUT 'REDFRONT_SETPCHAR-PSL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SETPCHAR-PSL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_SETPCHAR-PSL (C)
    (PROG (W)
      (SETQ W (REDFRONT_SETPCHAR-ORIG C))
      (SETQ PROMPTSTRING* (REDFRONT_COLOR PROMPTSTRING*))
      (RETURN (REDFRONT_UNCOLOR W)))) 
(PUT 'REDFRONT_SETPCHAR-CSL 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_SETPCHAR-CSL 'DEFINED-ON-LINE '124) 
(PUT 'REDFRONT_SETPCHAR-CSL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SETPCHAR-CSL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_SETPCHAR-CSL (C)
    (REDFRONT_UNCOLOR (REDFRONT_SETPCHAR-ORIG (REDFRONT_COLOR C)))) 
(GLOBAL '(REDFRONT_SETPCHAR_REDEFINED*)) 
(COND
 ((NOT REDFRONT_SETPCHAR_REDEFINED*) (COPYD 'REDFRONT_SETPCHAR-ORIG 'SETPCHAR))) 
(COND ((REDFRONT_PSLP) (COPYD 'SETPCHAR 'REDFRONT_SETPCHAR-PSL))
      (T (COPYD 'SETPCHAR 'REDFRONT_SETPCHAR-CSL))) 
(SETQ REDFRONT_SETPCHAR_REDEFINED* T) 
(PUT 'REDFRONT_YESP-PSL 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_YESP-PSL 'DEFINED-ON-LINE '139) 
(PUT 'REDFRONT_YESP-PSL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_YESP-PSL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_YESP-PSL (U)
    (PROG (IFL OFL X Y)
      (COND
       (IFL*
        (PROGN
         (SETQ IFL (SETQ IFL* (LIST (CAR IFL*) (CADR IFL*) CURLINE*)))
         (RDS NIL))))
      (COND (OFL* (PROGN (SETQ OFL OFL*) (WRS NIL))))
      (COND ((NULL *LESSSPACE) (TERPRI)))
      (COND ((ATOM U) (PRIN2 U)) (T (LPRI U)))
      (COND ((NULL *LESSSPACE) (TERPRI)))
      (SETQ Y (SETPCHAR "?"))
      (SETQ X (YESP1))
      (SETPCHAR Y)
      (COND (OFL (WRS (CDR OFL))))
      (COND (IFL (RDS (CADR IFL))))
      (SETQ CURSYM* '*SEMICOL*)
      (RETURN X))) 
(COND
 ((REDFRONT_PSLP)
  (PROGN
   (REMFLAG '(YESP) 'LOSE)
   (COPYD 'REDFRONT_YESP-ORIG 'YESP)
   (COPYD 'YESP 'REDFRONT_YESP-PSL)
   (FLAG '(YESP) 'LOSE)))) 
(PUT 'REDFRONT_COMPUTE-PROMPT-STRING 'NUMBER-OF-ARGS 2) 
(PUT 'REDFRONT_COMPUTE-PROMPT-STRING 'DEFINED-ON-LINE '175) 
(PUT 'REDFRONT_COMPUTE-PROMPT-STRING 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_COMPUTE-PROMPT-STRING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDFRONT_COMPUTE-PROMPT-STRING (COUNT LEVEL)
    (REDFRONT_COLOR (REDFRONT_COMPUTE-PROMPT-STRING-ORIG COUNT LEVEL))) 
(COND
 ((REDFRONT_PSLP)
  (PROGN
   (COPYD 'REDFRONT_COMPUTE-PROMPT-STRING-ORIG 'COMPUTE-PROMPT-STRING)
   (COPYD 'COMPUTE-PROMPT-STRING 'REDFRONT_COMPUTE-PROMPT-STRING)))) 
(PUT 'REDFRONT_BREAK_PROMPT 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_BREAK_PROMPT 'DEFINED-ON-LINE '183) 
(PUT 'REDFRONT_BREAK_PROMPT 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_BREAK_PROMPT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_BREAK_PROMPT NIL
    (PROGN
     (PRIN2 "break[")
     (PRIN2 BREAKLEVEL*)
     (PRIN2 "]")
     (SETQ PROMPTSTRING* (REDFRONT_COLOR PROMPTSTRING*)))) 
(COND
 ((REDFRONT_PSLP)
  (PROGN
   (COPYD 'BREAK_PROMPT 'REDFRONT_BREAK_PROMPT)
   (FLAG '(BREAK_PROMPT) 'LOSE)
   NIL))) 
(COND ((REDFRONT_PSLP) (ONOFF 'USERMODE (CAR REDFRONT_SWITCHES-THIS-SL*)))) 
(PUT 'REDFRONT_LEARNCOLOR 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_LEARNCOLOR 'DEFINED-ON-LINE '199) 
(PUT 'REDFRONT_LEARNCOLOR 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_LEARNCOLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_LEARNCOLOR (C)
    (COND
     ((STRINGP C)
      (COMPRESS
       (CONS '|"|
             (CONS (INT2ID 5)
                   (REVERSIP
                    (CONS '|"|
                          (CONS (INT2ID 6)
                                (CDR (REVERSIP (CDR (EXPLODE C)))))))))))
     (T
      (INTERN
       (COMPRESS (CONS (INT2ID 5) (NCONC (EXPLODE C) (LIST (INT2ID 6))))))))) 
(PUT 'REDFRONT_OBLIST 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_OBLIST 'DEFINED-ON-LINE '223) 
(PUT 'REDFRONT_OBLIST 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_OBLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_OBLIST NIL (OBLIST)) 
(PUT 'REDFRONT_SWL 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_SWL 'DEFINED-ON-LINE '227) 
(PUT 'REDFRONT_SWL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SWL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_SWL NIL
    (PROG (SWL)
      (SETQ SWL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (REDFRONT_OBLIST))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((FLAGP X 'SWITCH) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((FLAGP X 'SWITCH) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (SORT SWL 'ORDP)))) 
(PUT 'REDFRONT_SEND-SWITCHES 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_SEND-SWITCHES 'DEFINED-ON-LINE '233) 
(PUT 'REDFRONT_SEND-SWITCHES 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SEND-SWITCHES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_SEND-SWITCHES NIL
    (PROGN
     (PROG (SW)
       (SETQ SW (REDFRONT_SWL))
      LAB
       (COND ((NULL SW) (RETURN NIL)))
       ((LAMBDA (SW) (PRIN2T (REDFRONT_LEARNCOLOR SW))) (CAR SW))
       (SETQ SW (CDR SW))
       (GO LAB))
     (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
     NIL)) 
(PUT 'REDFRONT_MODL 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_MODL 'DEFINED-ON-LINE '241) 
(PUT 'REDFRONT_MODL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_MODL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_MODL NIL
    (PROG (LIBL L)
      (COND ((REDFRONT_PSLP) (RETURN NIL)))
      (SETQ LIBL INPUT-LIBRARIES)
      (COND (OUTPUT-LIBRARY (SETQ LIBL (CONS OUTPUT-LIBRARY LIBL))))
      (SETQ L
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X LIBL)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (X) (LIBRARY-MEMBERS X)) (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (LIBRARY-MEMBERS X)) (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (SORT L 'ORDP)))) 
(PUT 'REDFRONT_SEND-MODULES 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_SEND-MODULES 'DEFINED-ON-LINE '252) 
(PUT 'REDFRONT_SEND-MODULES 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SEND-MODULES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_SEND-MODULES NIL
    (PROGN
     (PROG (MOD)
       (SETQ MOD (REDFRONT_MODL))
      LAB
       (COND ((NULL MOD) (RETURN NIL)))
       ((LAMBDA (MOD) (PRIN2T (REDFRONT_LEARNCOLOR MOD))) (CAR MOD))
       (SETQ MOD (CDR MOD))
       (GO LAB))
     (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
     NIL)) 
(PUT 'REDFRONT_PACKAGE_NAMES 'DEFINED-ON-LINE '273) 
(PUT 'REDFRONT_PACKAGE_NAMES 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_PACKAGE_NAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM REDFRONT_PACKAGE_NAMES (U)
    (PROG (FN I W E BASEL EXTRAL)
      (SETQ FN "$reduce/packages/package.map")
      (COND
       ((MEMQ 'PSL LISPSYSTEM*)
        (PROG (R1 R2 R3)
          (SETQ R2 (EXPLODE2 (SETQ R1 (GETENV "reduce"))))
          (SETQ R3 (EXPLODE2 "/cygdrive/"))
          (PROG ()
           WHILELABEL
            (COND ((NOT (AND R2 R3 (EQUAL (CAR R2) (CAR R3)))) (RETURN NIL)))
            (PROGN (SETQ R2 (CDR R2)) (SETQ R3 (CDR R3)))
            (GO WHILELABEL))
          (COND
           ((NULL R3)
            (SETQ R1
                    (LIST2STRING
                     (CONS (CAR R2) (CONS '|:| (CONS '/ (CDDR R2))))))))
          (SETQ FN (CONCAT R1 "/packages/package.map")))))
      (PRIN2 "**** File name for packages = ")
      (PRINT FN)
      (SETQ I FN)
      (SETQ I (OPEN I 'INPUT))
      (SETQ I (RDS I))
      (SETQ E *ECHO)
      (SETQ *ECHO NIL)
      (SETQ W (READ))
      (SETQ *ECHO E)
      (SETQ I (RDS I))
      (CLOSE I)
      (SETQ BASEL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X W)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND ((MEMBER 'CORE (CDDR X)) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND ((MEMBER 'CORE (CDDR X)) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (SETQ EXTRAL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X W)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND
                            ((NOT (MEMBER 'CORE (CDDR X))) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND
                            ((NOT (MEMBER 'CORE (CDDR X))) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (MKQUOTE (CONS BASEL EXTRAL))))) 
(PUT 'REDFRONT_SEND-PACKAGES 'NUMBER-OF-ARGS 1) 
(PUT 'REDFRONT_SEND-PACKAGES 'DEFINED-ON-LINE '308) 
(PUT 'REDFRONT_SEND-PACKAGES 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SEND-PACKAGES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFRONT_SEND-PACKAGES (FN)
    (PROGN
     (PROG (PACK)
       (SETQ PACK
               (CDR
                '((SUPPORT REVISION RLISP ALG POLY RTOOLS ARITH MATHPR EZGCD
                   FACTOR HEPHYS INT MATRIX RLISP88 RPRINT FMPRINT PRETTY SOLVE
                   DESIR INEQ MODSR RSOLVE ALGINT ARNUM ASSIST DUMMY CANTENS
                   ATENSOR AVECTOR INVBASE MISC BOOLEAN CEDIT RCREF RESET CALI
                   CAMAL CHANGEVR COMPACT DFPART LIE)
                  ASSERT ODESOLVE PF TRIGD CVIT NONCOM2 PHYSOP EXCALC GENTRAN
                  FIDE1 FIDE NUMERIC ECONOMISE RANDPOLY REACTEQN ROOTS RLFI
                  ROOTS2 SETS XIDEAL EDS DIPOLY GROEBNER GROEBNR2 IDEALS LINALG
                  NCPOLY NORMFORM ORTHOVEC PLOT GNUPLOT LAPLACE PM QSUM SCOPE
                  SPARSE SPDE ELLIPFN SPECFN SPECFN2 SPECFAUX SPECBESS SFGAMMA
                  TPS LIMITS DEFINT FPS TRIGINT RATINT MATHML MATHMLOM
                  RLSUPPORT RLTOOLS REDLOG CGB CL OFSF DVFSF ACFSF DCFSF IBALP
                  PASF QQE QQE_OFSF MRI MRI_OFSF MRI_PASF REDFRONT REDUCE4
                  TABLES TALP SUM ZEILBERG SYMAUX SYMMETRY TAYLOR MRVLIMIT
                  RESIDUE SUSY2 TRI TRIGSIMP CRACK LIEPDE APPLYSYM CONLAW
                  V3TOOLS XCOLOR WU ZTRANS GEOPROVER RATAPRX RTRACE TMPRINT
                  LIBREDUCE UTF8 LPDO GUARDIAN BREDUCE CDIFF BIBASIS CLPRL
                  GCREF TURTLE LOGOTURTLE PROFILE PIDENT PGAUSS QHULL SMT
                  GUROBI Z3 CUBA NLOPT RUBI_RED LALR RANUM LISTVECOPS CDE
                  SSTOOLS COEFF2 GRINDER F5)))
      LAB
       (COND ((NULL PACK) (RETURN NIL)))
       ((LAMBDA (PACK) (PRIN2T (REDFRONT_LEARNCOLOR PACK))) (CAR PACK))
       (SETQ PACK (CDR PACK))
       (GO LAB))
     (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
     NIL)) 
(PUT 'REDFRONT_FWL 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_FWL 'DEFINED-ON-LINE '316) 
(PUT 'REDFRONT_FWL 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_FWL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_FWL NIL
    (PROG (FWL)
      (SETQ FWL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (REDFRONT_OBLIST))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND
                            ((OR (GET X 'PSOPFN) (GET X 'OPFN) (GET X 'POLYFN))
                             (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND
                            ((OR (GET X 'PSOPFN) (GET X 'OPFN) (GET X 'POLYFN))
                             (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (SORT FWL 'ORDP)))) 
(PUT 'REDFRONT_SEND-FUNCTIONS 'NUMBER-OF-ARGS 0) 
(PUT 'REDFRONT_SEND-FUNCTIONS 'DEFINED-ON-LINE '324) 
(PUT 'REDFRONT_SEND-FUNCTIONS 'DEFINED-IN-FILE 'REDFRONT/REDFRONT.RED) 
(PUT 'REDFRONT_SEND-FUNCTIONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDFRONT_SEND-FUNCTIONS NIL
    (PROGN
     (PROG (FW)
       (SETQ FW (REDFRONT_FWL))
      LAB
       (COND ((NULL FW) (RETURN NIL)))
       ((LAMBDA (FW) (PRIN2T (REDFRONT_LEARNCOLOR FW))) (CAR FW))
       (SETQ FW (CDR FW))
       (GO LAB))
     (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
     NIL)) 
(ONOFF 'MSG (CAR REDFRONT_SWITCHES*)) 
(ONOFF 'OUTPUT (CADR REDFRONT_SWITCHES*)) 
(SETQ CRBUF* NIL) 
(SETQ INPUTBUFLIS* NIL) 
(SETQ STATCOUNTER 0) 
(ENDMODULE) 
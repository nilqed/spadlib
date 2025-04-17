(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODDEC)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL '(FORTCONV* OPTLANG*)) 
(FLUID '(*DOUBLE)) 
(SWITCH (LIST 'DOUBLE)) 
(PUT 'TYPEALL 'NUMBER-OF-ARGS 1) 
(PUT 'TYPEALL 'DEFINED-ON-LINE '65) 
(PUT 'TYPEALL 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'TYPEALL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPEALL (FORMS)
    (PROG (B DECLST NFORMS)
      (ON-DOUBLE FORMS)
      (SETQ DECLST (SYMTABGET NIL '*DECS*))
      (COND
       ((EQUAL OPTLANG* 'FORTRAN2)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND DECLST (NOT B))) (RETURN NIL)))
           (PROGN
            (SETQ B
                    (OR (EQUAL (CADAR DECLST) 'COMPLEX)
                        (EQUAL (CADAR DECLST) '|IMPLICIT COMPLEX|)))
            (SETQ DECLST (CDR DECLST)))
           (GO WHILELABEL))
         (COND
          (B
           (SETQ FORTCONV*
                   '(UNKNOWN (INTEGER REAL COMPLEX ALL) (BOOL ALL)
                     (CHAR STRING ALL))))
          (T
           (SETQ FORTCONV*
                   '(UNKNOWN (INTEGER REAL ALL) (BOOL ALL)
                     (CHAR STRING ALL))))))))
      (PROG (ASS)
        (SETQ ASS FORMS)
       LAB
        (COND ((NULL ASS) (RETURN NIL)))
        ((LAMBDA (ASS)
           (PROGN
            (SETQ ASS
                    (CONS (CAR ASS)
                          (COND ((COMPLEXP ASS) (CIREVAL (CDR ASS)))
                                (T (CDR ASS)))))
            (ASSTYPE (CAR ASS) (CDR ASS))
            (SETQ NFORMS (CONS (CONS (CAR ASS) (CDR ASS)) NFORMS))
            NIL))
         (CAR ASS))
        (SETQ ASS (CDR ASS))
        (GO LAB))
      (APPLY1 'ARESTORE AVARLST)
      (SETQ NFORMS (REVERSE NFORMS))
      (FINISH-TYPING NFORMS)
      (FIX-IMPLICIT)
      (RETURN NFORMS))) 
(PUT 'ON-DOUBLE 'NUMBER-OF-ARGS 1) 
(PUT 'ON-DOUBLE 'DEFINED-ON-LINE '103) 
(PUT 'ON-DOUBLE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'ON-DOUBLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ON-DOUBLE (FORMS)
    (PROG (NEWTYPE)
      (PROG (DEC)
        (SETQ DEC (SYMTABGET NIL '*DECS*))
       LAB
        (COND ((NULL DEC) (RETURN NIL)))
        ((LAMBDA (DEC)
           (COND
            ((SETQ NEWTYPE
                     (ASSOC (CADR DEC)
                            '((REAL*8 . REAL) (COMPLEX*16 . COMPLEX)
                              (|IMPLICIT REAL*8| . |IMPLICIT REAL|)
                              (|IMPLICIT COMPLEX*16| . |IMPLICIT COMPLEX|))))
             (PROGN
              (SYMTABPUT NIL (CAR DEC) (LIST (CDR NEWTYPE)))
              (SETQ *DOUBLE T)))))
         (CAR DEC))
        (SETQ DEC (CDR DEC))
        (GO LAB))
      (ON-DOUBLE1 FORMS))) 
(PUT 'ON-DOUBLE1 'NUMBER-OF-ARGS 1) 
(PUT 'ON-DOUBLE1 'DEFINED-ON-LINE '129) 
(PUT 'ON-DOUBLE1 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'ON-DOUBLE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ON-DOUBLE1 (FORMS)
    (COND
     ((PAIRP FORMS)
      (COND ((DOUBLEP (CAR FORMS)) (SETQ *DOUBLE 'T))
            (T (PROGN (ON-DOUBLE1 (CAR FORMS)) (ON-DOUBLE1 (CDR FORMS)))))))) 
(PUT 'FIX-IMPLICIT 'NUMBER-OF-ARGS 0) 
(PUT 'FIX-IMPLICIT 'DEFINED-ON-LINE '137) 
(PUT 'FIX-IMPLICIT 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'FIX-IMPLICIT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIX-IMPLICIT NIL
    (PROG (DECL TYPE)
      (PROG (DECL)
        (SETQ DECL (SYMTABGET NIL '*DECS*))
       LAB
        (COND ((NULL DECL) (RETURN NIL)))
        ((LAMBDA (DECL)
           (COND
            ((AND (NOT (ISIMPLICIT (CADR DECL)))
                  (SETQ TYPE (IMPLICITDEC (CAR DECL))))
             (PROGN
              (COND
               ((GREATERTYPE TYPE (CDR DECL))
                (TYPERROR 8 (CONS (CDR DECL) TYPE))))
              (SYMTABREM NIL (CAR DECL))))))
         (CAR DECL))
        (SETQ DECL (CDR DECL))
        (GO LAB)))) 
(PUT 'GETDEC 'NUMBER-OF-ARGS 1) 
(PUT 'GETDEC 'DEFINED-ON-LINE '161) 
(PUT 'GETDEC 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'GETDEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETDEC (VNAME)
    (PROG (DECL)
      (SETQ DECL (SYMTABGET NIL VNAME))
      (COND ((NOT DECL) (SETQ DECL (IMPLICITDEC VNAME))))
      (RETURN DECL))) 
(PUT 'IMPLICITDEC 'NUMBER-OF-ARGS 1) 
(PUT 'IMPLICITDEC 'DEFINED-ON-LINE '175) 
(PUT 'IMPLICITDEC 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'IMPLICITDEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IMPLICITDEC (VNAME)
    (PROG (DECL DECS)
      (SETQ DECL NIL)
      (SETQ DECS (SYMTABGET NIL '*DECS*))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT DECL) DECS)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (ISIMPLICIT (CADAR DECS)) (FIRSTMATCH VNAME (CAAR DECS)))
           (SETQ DECL (LIST VNAME (IMPLICITTYPE (CADAR DECS))))))
         (SETQ DECS (CDR DECS)))
        (GO WHILELABEL))
      (RETURN DECL))) 
(PUT 'FIRSTMATCH 'NUMBER-OF-ARGS 2) 
(PUT 'FIRSTMATCH 'DEFINED-ON-LINE '193) 
(PUT 'FIRSTMATCH 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'FIRSTMATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIRSTMATCH (VNAME IMPLICIT)
    (PROG (FIRST)
      (SETQ FIRST (ID2INT (CAR (EXPLODE VNAME))))
      (RETURN
       (AND (GEQ FIRST (ID2INT (CAR (EXPLODE IMPLICIT))))
            (LEQ FIRST (ID2INT (CADDDR (EXPLODE IMPLICIT)))))))) 
(PUT 'ISIMPLICIT 'NUMBER-OF-ARGS 1) 
(PUT 'ISIMPLICIT 'DEFINED-ON-LINE '206) 
(PUT 'ISIMPLICIT 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'ISIMPLICIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISIMPLICIT (TYPE)
    (PROG (IMPLICIT RESULT ETYPE)
      (SETQ IMPLICIT (EXPLODE '|IMPLICIT |))
      (SETQ ETYPE (EXPLODE TYPE))
      (SETQ RESULT 'T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND RESULT IMPLICIT)) (RETURN NIL)))
        (PROGN
         (SETQ RESULT (EQUAL (CAR ETYPE) (CAR IMPLICIT)))
         (SETQ IMPLICIT (CDR IMPLICIT))
         (SETQ ETYPE (CDR ETYPE)))
        (GO WHILELABEL))
      (RETURN RESULT))) 
(PUT 'IMPLICITTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'IMPLICITTYPE 'DEFINED-ON-LINE '224) 
(PUT 'IMPLICITTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'IMPLICITTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IMPLICITTYPE (IMPLICIT) (INTERN (COMPRESS (PNTH (EXPLODE IMPLICIT) 11)))) 
(PUT 'ASSTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'ASSTYPE 'DEFINED-ON-LINE '232) 
(PUT 'ASSTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'ASSTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASSTYPE (LHS RHS)
    (PROG (LHSTYPE)
      (SETQ LHSTYPE
              (TYPECHECK (DETTYPE LHS 'UNKNOWN) (DETTYPE RHS 'UNKNOWN) RHS))
      (COND ((ATOM LHS) (SYMTABPUT NIL LHS (LIST LHSTYPE)))
            ((SUBSCRIPTEDVARP (CAR LHS))
             (SYMTABPUT NIL (CAR LHS) (LIST LHSTYPE)))
            (T
             (SYMTABPUT NIL (CAR LHS)
                        (APPEND
                         (LIST
                          (COND ((ATOM LHSTYPE) (LIST LHSTYPE)) (T LHSTYPE)))
                         (PROG (NDX FORALL-RESULT FORALL-ENDPTR)
                           (SETQ NDX (CDR LHS))
                           (COND ((NULL NDX) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS ((LAMBDA (NDX) 'N) (CAR NDX))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ NDX (CDR NDX))
                           (COND ((NULL NDX) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (NDX) 'N) (CAR NDX)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))))) 
(PUT 'DETTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'DETTYPE 'DEFINED-ON-LINE '254) 
(PUT 'DETTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'DETTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DETTYPE (XPR MINIMUMTYPE)
    (PROG (TYPE DTYPE OPTYPE MTYPE MNTYPE MXTYPE)
      (RETURN
       (COND
        ((ATOM XPR)
         (COND ((NUMBERP XPR) (COND ((FLOATP XPR) 'REAL) (T 'INTEGER)))
               ((AND (SETQ TYPE (GETDEC XPR)) (SETQ TYPE (CADR TYPE)))
                (COND
                 ((GREATERTYPE MINIMUMTYPE (MINTYPE TYPE))
                  (COND
                   ((GREATERTYPE MINIMUMTYPE (MAXTYPE TYPE)) (TYPERROR 1 XPR))
                   (T
                    (PROGN
                     (SYMTABPUT NIL XPR
                                (LIST
                                 (SETQ TYPE
                                         (RETURNTYPE
                                          (LIST MINIMUMTYPE (MAXTYPE TYPE))))))
                     TYPE))))
                 (T TYPE)))
               (T
                (PROGN
                 (SYMTABPUT NIL XPR (LIST (LIST MINIMUMTYPE 'ALL)))
                 (LIST MINIMUMTYPE 'ALL)))))
        ((MEMQ (CAR XPR) DOMAINLIST*)
         (COND ((MEMQ (CAR XPR) '(|:RD:| |:RN:|)) 'REAL)
               ((MEMQ (CAR XPR) '(|:GI:| |:CR:| |:CRN:|)) 'COMPLEX)
               (T (TYPERROR 5 (CAR XPR)))))
        ((SUBSCRIPTEDVARP2 (CAR XPR))
         (PROGN
          (PROG (NDX)
            (SETQ NDX (CDR XPR))
           LAB
            (COND ((NULL NDX) (RETURN NIL)))
            ((LAMBDA (NDX) (TYPECHECK 'INTEGER (DETTYPE NDX 'INTEGER) NDX))
             (CAR NDX))
            (SETQ NDX (CDR NDX))
            (GO LAB))
          (CADR (GETDEC (CAR XPR)))))
        ((SMEMBER 'ARGTYPE (CAR (OR (SETQ OPTYPE (OPCHECK XPR)) '(NIL))))
         (PROGN
          (SETQ MTYPE
                  (SETQ MNTYPE
                          (SETQ MXTYPE (CAR (EVAL (GET (CAR XPR) 'ARGTYPE))))))
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG)
               (PROGN
                (SETQ DTYPE (DETTYPE ARG MTYPE))
                (COND
                 ((GREATERTYPE (SETQ TYPE (MAXTYPE DTYPE)) MXTYPE)
                  (SETQ MXTYPE TYPE)))
                (COND
                 ((GREATERTYPE (SETQ TYPE (MINTYPE DTYPE)) MNTYPE)
                  (SETQ MNTYPE TYPE)))))
             (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          (COND
           ((ATOM (CDR OPTYPE))
            (PROGN
             (COND
              ((EQUAL (CDR OPTYPE) 'ARGTYPE) (RETURNTYPE (LIST MNTYPE MXTYPE)))
              (T (CDR OPTYPE)))))
           ((GREATERTYPE MXTYPE (CADR OPTYPE))
            (PROGN
             (COND ((GREATERTYPE MNTYPE (CADR OPTYPE)) (LIST MNTYPE MXTYPE))
                   (T (LIST (CADR OPTYPE) MXTYPE)))))
           (T (CADR OPTYPE)))))
        (OPTYPE
         (PROGN
          (SETQ TYPE (CAR OPTYPE))
          (COND ((ATOM TYPE) (SETQ TYPE (LIST TYPE))))
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG)
               (PROGN
                (SETQ MTYPE (FIRSTINCLASS (CAR TYPE)))
                (TYPECHECK (CAR TYPE) (DETTYPE ARG MTYPE) ARG)
                (SETQ TYPE (CDR TYPE))))
             (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          (CDR OPTYPE)))
        (T
         (PROGN
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG) (DETTYPE ARG 'UNKNOWN)) (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          (LIST MINIMUMTYPE 'ALL))))))) 
(PUT 'TYPECHECK 'NUMBER-OF-ARGS 3) 
(PUT 'TYPECHECK 'DEFINED-ON-LINE '344) 
(PUT 'TYPECHECK 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'TYPECHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TYPECHECK (LHSTYPE RHSTYPE RHS)
    (PROG (TYPE)
      (COND
       ((GREATERTYPE (MINTYPE LHSTYPE) (MAXTYPE RHSTYPE)) (MINTYPE LHSTYPE))
       (T
        (PROGN
         (SETQ TYPE (TYPEINTERSEC LHSTYPE RHSTYPE))
         (COND
          ((GREATERTYPE (MAXTYPE RHSTYPE) (MAXTYPE TYPE))
           (COND
            ((NOT (PUTMAXTYPE RHS (MAXTYPE TYPE)))
             (TYPERROR 2 (CONS LHSTYPE RHSTYPE)))))))))
      (RETURN TYPE))) 
(PUT 'TYPEINTERSEC 'NUMBER-OF-ARGS 2) 
(PUT 'TYPEINTERSEC 'DEFINED-ON-LINE '379) 
(PUT 'TYPEINTERSEC 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'TYPEINTERSEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPEINTERSEC (TYPE1 TYPE2)
    (PROG (MINT MAXT)
      (SETQ MINT
              (COND
               ((GREATERTYPE (MINTYPE TYPE1) (MINTYPE TYPE2)) (MINTYPE TYPE1))
               (T (MINTYPE TYPE2))))
      (SETQ MAXT
              (COND
               ((GREATERTYPE (MAXTYPE TYPE1) (MAXTYPE TYPE2)) (MAXTYPE TYPE2))
               (T (MAXTYPE TYPE1))))
      (COND ((GREATERTYPE MINT MAXT) (TYPERROR 2 (CONS TYPE1 TYPE2))))
      (RETURN (RETURNTYPE (LIST MINT MAXT))))) 
(PUT 'MINTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'MINTYPE 'DEFINED-ON-LINE '399) 
(PUT 'MINTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'MINTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINTYPE (TYPE) (COND ((ATOM TYPE) TYPE) (T (CAR TYPE)))) 
(PUT 'MAXTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'MAXTYPE 'DEFINED-ON-LINE '408) 
(PUT 'MAXTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'MAXTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAXTYPE (TYPE)
    (COND ((ATOM TYPE) TYPE) ((PAIRP (CDR TYPE)) (CADR TYPE)) (T (CAR TYPE)))) 
(PUT 'RETURNTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'RETURNTYPE 'DEFINED-ON-LINE '416) 
(PUT 'RETURNTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'RETURNTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RETURNTYPE (TYPE)
    (COND ((EQUAL (MINTYPE TYPE) (MAXTYPE TYPE)) (MINTYPE TYPE))
          ((GREATERTYPE (MINTYPE TYPE) (MAXTYPE TYPE)) (TYPERROR 7 NIL))
          (T TYPE))) 
(PUT 'PUTMAXTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'PUTMAXTYPE 'DEFINED-ON-LINE '428) 
(PUT 'PUTMAXTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'PUTMAXTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PUTMAXTYPE (XPR TYPE)
    (PROG (RESTYPE B)
      (RETURN
       (COND ((NULL XPR) T)
             ((ATOM XPR)
              (COND ((NUMBERP XPR) (GEQTYPE TYPE (DETTYPE XPR 'INTEGER)))
                    ((SETQ RESTYPE (CADR (GETDEC XPR)))
                     (COND ((ATOM RESTYPE) (GEQTYPE TYPE RESTYPE))
                           ((GEQTYPE TYPE (MINTYPE RESTYPE))
                            (PROGN
                             (COND
                              ((EQUAL TYPE (MINTYPE RESTYPE))
                               (SYMTABPUT NIL XPR (LIST TYPE)))
                              (T
                               (SYMTABPUT NIL XPR
                                          (LIST
                                           (LIST (MINTYPE RESTYPE) TYPE)))))
                             T))
                           (T NIL)))
                    (T (TYPERROR 3 XPR))))
             ((SUBSCRIPTEDVARP (CAR XPR))
              (GEQTYPE TYPE (CADR (GETDEC (CAR XPR)))))
             ((OR
               (EQUAL (SETQ RESTYPE (CDR (OR (OPCHECK XPR) '(NIL)))) 'ARGTYPE)
               (LISTP RESTYPE))
              (PROGN
               (SETQ B T)
               (PROG (ARG)
                 (SETQ ARG (CDR XPR))
                LAB
                 (COND ((NULL ARG) (RETURN NIL)))
                 ((LAMBDA (ARG) (SETQ B (AND B (PUTMAXTYPE ARG TYPE))))
                  (CAR ARG))
                 (SETQ ARG (CDR ARG))
                 (GO LAB))
               B))
             (RESTYPE (GEQTYPE TYPE RESTYPE)) (T (GEQTYPE TYPE 'UNKNOWN)))))) 
(GLOBAL '(FORTCONV* CCONV* RATCONV* PASCONV* F90CONV* OPTLANG*)) 
(PUT 'FORTRAN 'CONVERSION 'FORTCONV*) 
(PUT 'F90 'CONVERSION 'F90CONV*) 
(PUT 'C 'CONVERSION 'CCONV*) 
(PUT 'RATFOR 'CONVERSION 'RATCONV*) 
(PUT 'PASCAL 'CONVERSION 'PASCONV*) 
(SETQ FORTCONV*
        '(UNKNOWN (INTEGER REAL COMPLEX ALL) (BOOL ALL) (CHAR STRING ALL))) 
(SETQ F90CONV*
        '(UNKNOWN (INTEGER REAL COMPLEX ALL) (BOOL ALL) (CHAR STRING ALL))) 
(SETQ CCONV*
        (SETQ RATCONV*
                (SETQ PASCONV*
                        '(UNKNOWN (INTEGER REAL ALL) (BOOL ALL)
                          (CHAR STRING ALL))))) 
(PUT 'GETNUM 'NUMBER-OF-ARGS 0) 
(PUT 'GETNUM 'DEFINED-ON-LINE '524) 
(PUT 'GETNUM 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'GETNUM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GETNUM NIL
    (PROG (CONV FOUND)
      (SETQ CONV
              (EVAL (GET (COND (OPTLANG* OPTLANG*) (T 'FORTRAN)) 'CONVERSION)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT FOUND) (SETQ CONV (CDR CONV)))) (RETURN NIL)))
        (COND ((EQUAL (CAAR CONV) 'INTEGER) (SETQ FOUND T)))
        (GO WHILELABEL))
      (RETURN (CAR CONV)))) 
(PUT 'GREATERTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'GREATERTYPE 'DEFINED-ON-LINE '538) 
(PUT 'GREATERTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'GREATERTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GREATERTYPE (T1 T2)
    (PROG (CONV CLASS FOUND FOUND1 FOUND2 F)
      (SETQ CONV
              (EVAL (GET (COND (OPTLANG* OPTLANG*) (T 'FORTRAN)) 'CONVERSION)))
      (COND ((EQUAL (CAR CONV) T2) (SETQ F T))
            ((EQUAL (CAR CONV) T1) (SETQ F NIL))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND (SETQ CONV (CDR CONV)) (NOT FOUND))) (RETURN NIL)))
                (PROGN
                 (SETQ CLASS (CAR CONV))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND CLASS (NOT FOUND2))) (RETURN NIL)))
                   (PROGN
                    (COND ((EQUAL (CAR CLASS) T1) (SETQ FOUND1 T)))
                    (COND ((EQUAL (CAR CLASS) T2) (SETQ FOUND2 T))
                          (T (SETQ CLASS (CDR CLASS)))))
                   (GO WHILELABEL))
                 (COND
                  (FOUND2
                   (PROGN
                    (SETQ CLASS (CDR CLASS))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT (AND CLASS (NOT F))) (RETURN NIL)))
                      (COND ((EQUAL (CAR CLASS) T1) (SETQ FOUND1 (SETQ F T)))
                            (T (SETQ CLASS (CDR CLASS))))
                      (GO WHILELABEL))
                    NIL)))
                 (COND
                  ((OR (AND FOUND1 (NOT FOUND2)) (AND (NOT FOUND1) FOUND2))
                   (TYPERROR 4 (CONS T1 T2)))
                  ((AND FOUND1 FOUND2) (SETQ FOUND T))))
                (GO WHILELABEL)))))
      (RETURN F))) 
(PUT 'GEQTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'GEQTYPE 'DEFINED-ON-LINE '581) 
(PUT 'GEQTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'GEQTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GEQTYPE (T1 T2)
    (PROG (CONV CLASS FOUND FOUND1 FOUND2 F)
      (SETQ CONV
              (EVAL (GET (COND (OPTLANG* OPTLANG*) (T 'FORTRAN)) 'CONVERSION)))
      (COND ((EQUAL (CAR CONV) T2) (SETQ F T)) ((EQUAL (CAR CONV) T1) NIL)
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND (SETQ CONV (CDR CONV)) (NOT FOUND))) (RETURN NIL)))
                (PROGN
                 (SETQ CLASS (CAR CONV))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND CLASS (NOT FOUND2))) (RETURN NIL)))
                   (PROGN
                    (COND ((EQUAL (CAR CLASS) T1) (SETQ FOUND1 T)))
                    (COND ((EQUAL (CAR CLASS) T2) (SETQ FOUND2 T))
                          (T (SETQ CLASS (CDR CLASS)))))
                   (GO WHILELABEL))
                 (COND
                  (FOUND2
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (AND CLASS (NOT F))) (RETURN NIL)))
                     (COND ((EQUAL (CAR CLASS) T1) (SETQ FOUND1 (SETQ F T)))
                           (T (SETQ CLASS (CDR CLASS))))
                     (GO WHILELABEL))))
                 (COND
                  ((OR (AND FOUND1 (NOT FOUND2)) (AND (NOT FOUND1) FOUND2))
                   (TYPERROR 4 (CONS T1 T2)))
                  ((AND FOUND1 FOUND2) (SETQ FOUND T))))
                (GO WHILELABEL)))))
      (RETURN F))) 
(PUT 'LESSTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'LESSTYPE 'DEFINED-ON-LINE '620) 
(PUT 'LESSTYPE 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'LESSTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LESSTYPE (T1 T2) (GREATERTYPE T2 T1)) 
(PUT 'FIRSTINCLASS 'NUMBER-OF-ARGS 1) 
(PUT 'FIRSTINCLASS 'DEFINED-ON-LINE '623) 
(PUT 'FIRSTINCLASS 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'FIRSTINCLASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIRSTINCLASS (TYPE)
    (PROG (CONV FOUND CLASS MCLASS)
      (SETQ CONV
              (EVAL (GET (COND (OPTLANG* OPTLANG*) (T 'FORTRAN)) 'CONVERSION)))
      (RETURN
       (COND ((OR (EQUAL TYPE 'ALL) (EQUAL TYPE 'UNKNOWN)) 'UNKNOWN)
             (T
              (PROGN
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND (SETQ CONV (CDR CONV)) (NOT FOUND)))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ MCLASS (CAR (SETQ CLASS (CAR CONV))))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND CLASS (NOT FOUND))) (RETURN NIL)))
                    (PROGN
                     (COND ((EQUAL (CAR CLASS) TYPE) (SETQ FOUND T)))
                     (SETQ CLASS (CDR CLASS)))
                    (GO WHILELABEL)))
                 (GO WHILELABEL))
               (COND (FOUND MCLASS) (T (TYPERROR 5 TYPE))))))))) 
(PUT 'LASTINCLASS 'NUMBER-OF-ARGS 1) 
(PUT 'LASTINCLASS 'DEFINED-ON-LINE '647) 
(PUT 'LASTINCLASS 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'LASTINCLASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LASTINCLASS (TYPE)
    (PROG (CONV FOUND CLASS)
      (SETQ CONV
              (EVAL (GET (COND (OPTLANG* OPTLANG*) (T 'FORTRAN)) 'CONVERSION)))
      (COND
       ((NEQ TYPE 'ALL)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (SETQ CONV (CDR CONV)) (NOT FOUND))) (RETURN NIL)))
          (PROGN
           (SETQ CLASS (CAR CONV))
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND CLASS (NOT FOUND))) (RETURN NIL)))
             (COND
              ((EQUAL (CAR CLASS) TYPE)
               (PROGN
                (SETQ FOUND T)
                (PROG ()
                 REPEATLABEL
                  (SETQ TYPE (CAR CLASS))
                  (COND
                   ((NOT (EQUAL (SETQ CLASS (CDR CLASS)) '(ALL)))
                    (GO REPEATLABEL))))))
              (T (SETQ CLASS (CDR CLASS))))
             (GO WHILELABEL)))
          (GO WHILELABEL))))
      (RETURN TYPE))) 
(PROG (OP)
  (SETQ OP '(TIMES PLUS DIFFERENCE))
 LAB
  (COND ((NULL OP) (RETURN NIL)))
  ((LAMBDA (OP)
     (PROGN
      (PUT OP 'CHKTYPE '((ARGTYPE) . ARGTYPE))
      (PUT OP 'ARGTYPE '(GETNUM))))
   (CAR OP))
  (SETQ OP (CDR OP))
  (GO LAB)) 
(PUT 'QUOTIENT 'CHKTYPE '((ARGTYPE ARGTYPE) REAL)) 
(PUT 'QUOTIENT 'ARGTYPE '(GETNUM)) 
(PUT 'EXPT 'CHKTYPE '((ARGTYPE ARGTYPE) . ARGTYPE)) 
(PUT 'EXPT 'ARGTYPE '(GETNUM)) 
(PUT 'MINUS 'CHKTYPE '(ARGTYPE . ARGTYPE)) 
(PUT 'MINUS 'ARGTYPE '(GETNUM)) 
(PROG (OP)
  (SETQ OP '(OR AND))
 LAB
  (COND ((NULL OP) (RETURN NIL)))
  ((LAMBDA (OP) (PUT OP 'CHKTYPE '((BOOL) . BOOL))) (CAR OP))
  (SETQ OP (CDR OP))
  (GO LAB)) 
(PUT 'NOT 'CHKTYPE '(BOOL . BOOL)) 
(PROG (OP)
  (SETQ OP '(EQ LEQ GEQ GREATERP LESSP NEQ))
 LAB
  (COND ((NULL OP) (RETURN NIL)))
  ((LAMBDA (OP)
     (PROGN
      (PUT OP 'CHKTYPE '((ARGTYPE ARGTYPE) . BOOL))
      (PUT OP 'ARGTYPE '(GETNUM))))
   (CAR OP))
  (SETQ OP (CDR OP))
  (GO LAB)) 
(PROG (OP)
  (SETQ OP
          '(SIN COS TAN ASIN ACOS ATAN SINH COSH TANH ASINH ACOSH ATANH COT LOG
                SQRT))
 LAB
  (COND ((NULL OP) (RETURN NIL)))
  ((LAMBDA (OP) (PUT OP 'CHKTYPE '(REAL . REAL))) (CAR OP))
  (SETQ OP (CDR OP))
  (GO LAB)) 
(PUT 'OPCHECK 'NUMBER-OF-ARGS 1) 
(PUT 'OPCHECK 'DEFINED-ON-LINE '733) 
(PUT 'OPCHECK 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'OPCHECK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPCHECK (OP)
    (PROG (OPTYPE)
      (RETURN
       (COND ((NOT (SETQ OPTYPE (GET (CAR OP) 'CHKTYPE))) 'NIL)
             ((ATOM (CAR OPTYPE))
              (COND ((EQUAL (LENGTH (CDR OP)) 1) OPTYPE)
                    (T (TYPERROR 6 (CAR OP)))))
             ((CDAR OPTYPE)
              (COND ((EQUAL (LENGTH (CDR OP)) (LENGTH (CAR OPTYPE))) OPTYPE)
                    (T (TYPERROR 6 (CAR OP)))))
             ((GEQ (LENGTH (CDR OP)) 2) OPTYPE) (T (TYPERROR 6 (CAR OP))))))) 
(PUT 'FINISH-TYPING 'NUMBER-OF-ARGS 1) 
(PUT 'FINISH-TYPING 'DEFINED-ON-LINE '764) 
(PUT 'FINISH-TYPING 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'FINISH-TYPING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINISH-TYPING (PRFLST)
    (PROG (LTYPE RTYPE)
      (PROG (ITEM)
        (SETQ ITEM PRFLST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (COND
            ((SETQ LTYPE (DET&BIND (CAR ITEM) 'ALL))
             (PROGN
              (COND
               ((EQUAL LTYPE 'ALL)
                (COND
                 ((EQUAL (SETQ RTYPE (DET&BIND (CDR ITEM) LTYPE)) 'ALL)
                  (PROGN
                   (PRIN2 (LIST "Unknown type for operator" (CDR ITEM)))
                   NIL))
                 (T (SETQ LTYPE (LASTINCLASS RTYPE)))))
               (T (SETQ RTYPE (DET&BIND (CDR ITEM) LTYPE))))
              (COND ((GREATERTYPE RTYPE LTYPE) (TYPERROR 2 ITEM))
                    ((ATOM (CAR ITEM)) (SYMTABPUT NIL (CAR ITEM) (LIST LTYPE)))
                    (T (SYMTABPUT NIL (CAAR ITEM) (LIST LTYPE))))))
            (T (SYMTABPUT NIL (CAR ITEM) (LIST (DET&BIND (CDR ITEM) 'ALL))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB)))) 
(PUT 'DET&BIND 'NUMBER-OF-ARGS 2) 
(PUT 'DET&BIND 'DEFINED-ON-LINE '793) 
(PUT 'DET&BIND 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'DET&BIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DET&BIND (XPR MAXIMUMTYPE)
    (PROG (TYPE MTYPE OPTYPE)
      (RETURN
       (COND
        ((OR (IDP XPR) (CONSTP XPR))
         (COND ((CONSTP XPR) (DETTYPE XPR 'INTEGER))
               (T (DET&BINDMAX XPR MAXIMUMTYPE))))
        ((SUBSCRIPTEDVARP (CAR XPR))
         (PROGN
          (PROG (NDX)
            (SETQ NDX (CDR XPR))
           LAB
            (COND ((NULL NDX) (RETURN NIL)))
            ((LAMBDA (NDX) (DET&BIND NDX 'INTEGER)) (CAR NDX))
            (SETQ NDX (CDR NDX))
            (GO LAB))
          (DET&BINDMAX (CAR XPR) MAXIMUMTYPE)))
        ((SMEMBER 'ARGTYPE (CAR (OR (SETQ OPTYPE (OPCHECK XPR)) '(NIL))))
         (PROGN
          (SETQ MTYPE 'UNKNOWN)
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG)
               (COND
                ((GREATERTYPE (SETQ TYPE (DET&BIND ARG MAXIMUMTYPE)) MTYPE)
                 (SETQ MTYPE TYPE))))
             (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          (COND
           ((ATOM (CDR OPTYPE))
            (PROGN
             (COND ((EQUAL (CDR OPTYPE) 'ARGTYPE) MTYPE) (T (CDR OPTYPE)))))
           ((GREATERTYPE MTYPE (CADR OPTYPE)) MTYPE) (T (CADR OPTYPE)))))
        (OPTYPE
         (PROGN
          (SETQ TYPE (CAR OPTYPE))
          (COND ((ATOM TYPE) (SETQ TYPE (LIST TYPE))))
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG)
               (PROGN (DET&BIND ARG (CAR TYPE)) (SETQ TYPE (CDR TYPE))))
             (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          (CDR OPTYPE)))
        (T
         (PROGN
          (PROG (ARG)
            (SETQ ARG (CDR XPR))
           LAB
            (COND ((NULL ARG) (RETURN NIL)))
            ((LAMBDA (ARG) (DET&BIND ARG 'ALL)) (CAR ARG))
            (SETQ ARG (CDR ARG))
            (GO LAB))
          MAXIMUMTYPE)))))) 
(PUT 'DET&BINDMAX 'NUMBER-OF-ARGS 2) 
(PUT 'DET&BINDMAX 'DEFINED-ON-LINE '851) 
(PUT 'DET&BINDMAX 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'DET&BINDMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DET&BINDMAX (XPR MAXIMUMTYPE)
    (PROG (TYPE)
      (COND
       ((PAIRP (SETQ TYPE (CADR (GETDEC XPR))))
        (COND
         ((EQUAL (MAXTYPE TYPE) 'ALL)
          (COND
           ((EQUAL (MINTYPE TYPE) 'UNKNOWN)
            (PROGN
             (SETQ TYPE MAXIMUMTYPE)
             (SYMTABPUT NIL XPR (LIST MAXIMUMTYPE))))
           (T
            (PROGN
             (SETQ TYPE (LASTINCLASS (MINTYPE TYPE)))
             (COND ((GREATERTYPE TYPE MAXIMUMTYPE) (SETQ TYPE MAXIMUMTYPE)))
             (SYMTABPUT NIL XPR (LIST TYPE))))))
         (T (SYMTABPUT NIL XPR (LIST (SETQ TYPE (MAXTYPE TYPE))))))))
      (RETURN TYPE))) 
(PUT 'TYPERROR 'NUMBER-OF-ARGS 2) 
(PUT 'TYPERROR 'DEFINED-ON-LINE '870) 
(PUT 'TYPERROR 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'TYPERROR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPERROR (ERRORNR XPR)
    (COND
     ((EQUAL ERRORNR 6) (REDERR (LIST "Wrong number of arguments for" XPR)))
     (T
      (PROGN
       (TERPRI* T)
       (PROGN (PRIN2 "***** Type error:") NIL)
       (TERPRI* T)
       (PRINTDECS)
       (COND ((EQUAL ERRORNR 1) (REDERR (LIST "Wrong type for variable" XPR)))
             ((EQUAL ERRORNR 2)
              (PROGN
               (ASSGNPRI (CDR XPR) (LIST (CAR XPR)) T)
               (REDERR (LIST "Wrong typing"))))
             ((EQUAL ERRORNR 3) (REDERR (LIST XPR "not checked on type")))
             ((EQUAL ERRORNR 4)
              (REDERR
               (LIST (CAR XPR) "and" (CDR XPR) "in different type classes")))
             ((EQUAL ERRORNR 5) (REDERR (LIST XPR "is an unknown type")))
             ((EQUAL ERRORNR 7) (REDERR (LIST "Wrong reasoning")))
             ((EQUAL ERRORNR 8)
              (REDERR (LIST (CAR XPR) "cannot be redeclared to" (CDR XPR))))
             (T (REDERR (LIST "Unknown type error")))))))) 
(PUT 'SUBSCRIPTEDVARP 'NUMBER-OF-ARGS 1) 
(PUT 'SUBSCRIPTEDVARP 'DEFINED-ON-LINE '900) 
(PUT 'SUBSCRIPTEDVARP 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'SUBSCRIPTEDVARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBSCRIPTEDVARP (V)
    (OR (GREATERP (LENGTH (SYMTABGET NIL V)) 2) (FLAGP V 'SUBSCRIPTED))) 
(PUT 'SUBSCRIPTEDVARP2 'NUMBER-OF-ARGS 1) 
(PUT 'SUBSCRIPTEDVARP2 'DEFINED-ON-LINE '908) 
(PUT 'SUBSCRIPTEDVARP2 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'SUBSCRIPTEDVARP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBSCRIPTEDVARP2 (V) (GREATERP (LENGTH (SYMTABGET NIL V)) 2)) 
(GLOBAL '(*SYMBOLTABLE*)) 
(PUT 'DUMPSYMTAB 'NUMBER-OF-ARGS 0) 
(PUT 'DUMPSYMTAB 'DEFINED-ON-LINE '917) 
(PUT 'DUMPSYMTAB 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'DUMPSYMTAB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DUMPSYMTAB NIL
    (PROG (RES)
      (SETQ RES
              (PROG (PN FORALL-RESULT FORALL-ENDPTR)
                (SETQ PN *SYMBOLTABLE*)
               STARTOVER
                (COND ((NULL PN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PN)
                           (LIST
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*TYPE*)
                                  (MKQUOTE (SYMTABGET PN '*TYPE*)))
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*PARAMS*)
                                  (MKQUOTE (SYMTABGET PN '*PARAMS*)))
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*DECS*)
                                  (MKQUOTE (SYMTABGET PN '*DECS*)))))
                         (CAR PN)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PN (CDR PN))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PN)
                           (LIST
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*TYPE*)
                                  (MKQUOTE (SYMTABGET PN '*TYPE*)))
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*PARAMS*)
                                  (MKQUOTE (SYMTABGET PN '*PARAMS*)))
                            (LIST 'SYMTABPUT (MKQUOTE PN) (MKQUOTE '*DECS*)
                                  (MKQUOTE (SYMTABGET PN '*DECS*)))))
                         (CAR PN)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PN (CDR PN))
                (GO LOOPLABEL)))
      (SETQ RES
              (CONS 'PROGN
                    (CONS (LIST 'SETQ '*SYMBOLTABLE* (MKQUOTE *SYMBOLTABLE*))
                          RES)))
      (RETURN RES))) 
(PUT 'FIRSTMATCH 'NUMBER-OF-ARGS 2) 
(PUT 'FIRSTMATCH 'DEFINED-ON-LINE '937) 
(PUT 'FIRSTMATCH 'DEFINED-IN-FILE 'SCOPE/CODDEC.RED) 
(PUT 'FIRSTMATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIRSTMATCH (VNAME IMPLICIT)
    (PROG (FIRST)
      (SETQ FIRST (ID2INT (CAR (EXPLODE VNAME))))
      (COND
       ((FREEOF (EXPLODE IMPLICIT) '-)
        (RETURN (EQUAL FIRST (ID2INT (CAR (EXPLODE IMPLICIT))))))
       (T
        (RETURN
         (AND (GEQ FIRST (ID2INT (CAR (EXPLODE IMPLICIT))))
              (LEQ FIRST (ID2INT (CADDDR (EXPLODE IMPLICIT)))))))))) 
(ENDMODULE) 
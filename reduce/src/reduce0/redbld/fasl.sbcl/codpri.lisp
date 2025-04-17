(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODPRI)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(FLUID '(PREPREFIXLIST)) 
(GLOBAL
 '(CODBEXL* ROWMAX ROWMIN LINTLST KVARLST ENDMAT RHSALIASES AVARLST
   MIN-EXPR-LENGTH* *VECTORC)) 
(GLOBAL '(CODMAT MAXVAR)) 
(DE TESTPROW (Y OPV)
    (AND (GETV (GETV CODMAT (PLUS MAXVAR Y)) 0)
         (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) OPV))) 
(PUT 'TESTPROW 'NUMBER-OF-ARGS 2) 
(PUT 'TESTPROW 'DEFINED-ON-LINE '150) 
(PUT 'TESTPROW 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'TESTPROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'TESTPROW 'INLINE
      '(LAMBDA (Y OPV)
         (AND (GETV (GETV CODMAT (PLUS MAXVAR Y)) 0)
              (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) OPV)))) 
(PUT 'PRIMAT 'NUMBER-OF-ARGS 0) 
(PUT 'PRIMAT 'DEFINED-ON-LINE '158) 
(PUT 'PRIMAT 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRIMAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRIMAT NIL
    (PROG (L)
      (SETQ L (LINELENGTH 120))
      (TERPRI)
      (PRIN2 "Sumscheme :")
      (PRISCHEME 'PLUS)
      (TERPRI)
      (TERPRI)
      (TERPRI)
      (PRIN2 "Productscheme :")
      (PRISCHEME 'TIMES)
      (LINELENGTH L))) 
(GLOBAL '(FREEVEC FREETEST)) 
(SETQ FREETEST NIL) 
(PUT 'PRIMAT1 'NUMBER-OF-ARGS 0) 
(PUT 'PRIMAT1 'DEFINED-ON-LINE '181) 
(PUT 'PRIMAT1 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRIMAT1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRIMAT1 NIL
    (PROG (FREEVEC1 RMIN RMAX)
      (SETQ RMIN ROWMIN)
      (SETQ RMAX ROWMAX)
      (COND
       ((OR (NULL FREETEST) (LESSP FREETEST MAXVAR))
        (PROGN
         (SETQ FREETEST MAXVAR)
         (SETQ FREEVEC1 (MKVECT (TIMES 2 MAXVAR)))
         (SETQ FREEVEC FREEVEC1))))
      (PROG (J)
        (SETQ J RMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE RMAX J)) (RETURN NIL)))
        (PROGN
         (PUTV FREEVEC (PLUS J MAXVAR) (GETV (GETV CODMAT (PLUS MAXVAR J)) 0))
         (PUTV (GETV CODMAT (PLUS MAXVAR J)) 0 T))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PRIMAT)
      (PROG (J)
        (SETQ J RMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE RMAX J)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (GETV FREEVEC (PLUS J MAXVAR)))
           (PUTV (GETV CODMAT (PLUS MAXVAR J)) 0 NIL)))
         (TERPRI)
         (COND
          ((LESSP J 0)
           (PROGN
            (PRIN2 "col(")
            (PRIN2 J)
            (PRIN2 ")=")
            (PRIN2 (GETV CODMAT (PLUS MAXVAR J)))
            NIL))
          (T
           (PROGN
            (PRIN2 "scope_row(")
            (PRIN2 J)
            (PRIN2 ")=")
            (PRIN2 (GETV CODMAT (PLUS MAXVAR J)))
            NIL))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (TERPRI))) 
(PUT 'PRISCHEME 'NUMBER-OF-ARGS 1) 
(PUT 'PRISCHEME 'DEFINED-ON-LINE '200) 
(PUT 'PRISCHEME 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRISCHEME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRISCHEME (OPV)
    (PROG (N YL)
      (SETQ N 0)
      (SETQ LINTLST NIL)
      (TERPRI)
      (TERPRI)
      (PRIN2 "   |")
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND (GETV (GETV CODMAT (PLUS MAXVAR Y)) 0)
               (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) OPV))
          (PROGN
           (PRINUMB (PLUS Y (ABS ROWMIN)))
           (SETQ YL (CONS Y YL))
           (SETQ N (PLUS N 1)))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (PRIN2 "| EC|Far")
      (TERPRI)
      (SETQ N (PLUS (TIMES 3 N) 12))
      (COND
       ((GREATERP N 120)
        (PROGN (PRIN2 "Scheme to large to be printed") (RETURN NIL))))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PRIN2 "-")
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ YL (REVERSE YL))
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (COND
         ((AND (GETV (GETV CODMAT (PLUS MAXVAR X)) 0)
               (EQ (GETV (GETV CODMAT (PLUS MAXVAR X)) 2) OPV))
          (PRIROW X OPV YL)))
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (TERPRI)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PRIN2 "-")
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PRILINT)
      (TERPRI)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND (GETV (GETV CODMAT (PLUS MAXVAR Y)) 0)
               (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) OPV))
          (PROGN
           (PRIN2 (SETQ YL (PLUS Y (ABS ROWMIN))))
           (COND ((LESSP YL 10) (PRIN2 "  : ")) (T (PRIN2 " : ")))
           (PRIVAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3))
           (COND
            ((SETQ N (ASSOC (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3) KVARLST))
             (PROGN (PRIN2 "=") (PRIVAR (CDR N)))))
           (TERPRI))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB)))) 
(PUT 'PRIROW 'NUMBER-OF-ARGS 3) 
(PUT 'PRIROW 'DEFINED-ON-LINE '239) 
(PUT 'PRIROW 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRIROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRIROW (X OPV YL)
    (PROG ()
      (TERPRI)
      (PRINUMB X)
      (PRIN2 "|")
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((AND (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 0)
                  (EQ (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 2) OPV))
             (PROGN (SETQ YL (MEMQP (CAR Z) YL)) (PRINUMB (CAR (CDR Z)))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH YL) J)) (RETURN NIL)))
        (PRIN2 "   ")
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PRIN2 "|")
      (PRINUMB (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))
      (PRIN2 "| ")
      (PRIVAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))) 
(PUT 'MEMQP 'NUMBER-OF-ARGS 2) 
(PUT 'MEMQP 'DEFINED-ON-LINE '261) 
(PUT 'MEMQP 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'MEMQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MEMQP (Y YL)
    (COND ((EQUAL Y (CAR YL)) (CDR YL))
          (T (PROGN (PRIN2 "   ") (MEMQP Y (CDR YL)))))) 
(PUT 'PRINUMB 'NUMBER-OF-ARGS 1) 
(PUT 'PRINUMB 'DEFINED-ON-LINE '278) 
(PUT 'PRINUMB 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRINUMB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINUMB (N)
    (PROGN
     (COND
      ((AND (PAIRP N) (MEMQ (CAR N) DOMAINLIST*))
       (PROGN (SETQ LINTLST (CONS N LINTLST)) (SETQ N "  X")))
      ((MINUSP N)
       (COND ((GREATERP N (MINUS 10)) (PRIN2 " "))
             ((LEQ N (MINUS 100))
              (PROGN (SETQ LINTLST (CONS N LINTLST)) (SETQ N "  X")))))
      (T
       (COND ((LESSP N 10) (PRIN2 "  ")) ((LESSP N 100) (PRIN2 " "))
             ((GEQ N 1000)
              (PROGN (SETQ LINTLST (CONS N LINTLST)) (SETQ N "  X"))))))
     (PRIN2 N)
     NIL)) 
(PUT 'PRILINT 'NUMBER-OF-ARGS 0) 
(PUT 'PRILINT 'DEFINED-ON-LINE '308) 
(PUT 'PRILINT 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRILINT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRILINT NIL
    (COND
     (LINTLST
      (PROGN
       (TERPRI)
       (PRIN2
        "The following numbers ought to replace the X-entries of the matrix")
       (TERPRI)
       (PRIN2 "in a left-to-right-and top-down order : ")
       (PROG (N)
         (SETQ N (REVERSE LINTLST))
        LAB
         (COND ((NULL N) (RETURN NIL)))
         ((LAMBDA (N) (PROGN (DM-PRINT N) (PRIN2 "  "))) (CAR N))
         (SETQ N (CDR N))
         (GO LAB))
       NIL)))) 
(PUT 'PRIVAR 'NUMBER-OF-ARGS 1) 
(PUT 'PRIVAR 'DEFINED-ON-LINE '323) 
(PUT 'PRIVAR 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRIVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIVAR (VAR)
    (COND ((ATOM VAR) (PRIN2 VAR))
          (T
           (PROGN
            (PRIN2 (CAR VAR))
            (PRIN2 "(")
            (SETQ VAR (CDR VAR))
            (PROG ()
             WHILELABEL
              (COND ((NOT VAR) (RETURN NIL)))
              (PROGN
               (DM-PRINT (CAR VAR))
               (COND ((SETQ VAR (CDR VAR)) (PRIN2 ","))))
              (GO WHILELABEL))
            (PRIN2 ")")
            NIL)))) 
(GLOBAL '(*PREFIX *AGAIN)) 
(FLUID '(PREFIXLIST)) 
(SETQ PREFIXLIST NIL) 
(PUT 'PRFEXP 'NUMBER-OF-ARGS 2) 
(PUT 'PRFEXP 'DEFINED-ON-LINE '409) 
(PUT 'PRFEXP 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRFEXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRFEXP (X PREFIXLIST)
    (PROG (XX NEX)
      (COND
       ((GETV (GETV CODMAT (PLUS MAXVAR X)) 0)
        (PROGN
         (PROG (Y)
           (SETQ Y (REVERSE (GETV (GETV CODMAT (PLUS MAXVAR X)) 8)))
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (COND ((CONSTP Y) (SETQ PREFIXLIST (PRFEXP Y PREFIXLIST)))
                    (T
                     (PROGN
                      (SETQ PREFIXLIST (PRFKVAR Y PREFIXLIST))
                      (COND
                       ((GET Y 'NVARLST)
                        (PROGN
                         (SETQ PREFIXLIST (PRFPOWL Y PREFIXLIST))
                         (REMPROP Y 'NVARLST))))))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (COND
          ((NOT (EQUAL (GET (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) 'BEXL) X))
           (COND
            ((SETQ NEX (GET (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) 'NEX))
             (PROGN
              (PROG (ARG)
                (SETQ ARG (CDR NEX))
               LAB
                (COND ((NULL ARG) (RETURN NIL)))
                ((LAMBDA (ARG)
                   (COND
                    ((SETQ XX (GET ARG 'ROWINDEX))
                     (SETQ PREFIXLIST (PRFEXP XX PREFIXLIST)))
                    (T (SETQ PREFIXLIST (PRFKVAR ARG PREFIXLIST)))))
                 (CAR ARG))
                (SETQ ARG (CDR ARG))
                (GO LAB))
              (REMPROP (CAR NEX) 'KVARLST)
              (SETQ PREFIXLIST (CONS (CONS NEX (CONSTREXP X)) PREFIXLIST))
              (SYMTABREM NIL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3))))
            (T
             (SETQ PREFIXLIST
                     (CONS
                      (CONS (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)
                            (CONSTREXP X))
                      PREFIXLIST)))))
          (T (REMPROP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) 'BEXL)))
         (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL))))
      (RETURN PREFIXLIST))) 
(PUT 'CONSTREXP 'NUMBER-OF-ARGS 1) 
(PUT 'CONSTREXP 'DEFINED-ON-LINE '458) 
(PUT 'CONSTREXP 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'CONSTREXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONSTREXP (X)
    (PROG (S EC OPV CH LS)
      (COND
       ((EQ (SETQ OPV (GETV (GETV CODMAT (PLUS MAXVAR X)) 2)) 'TIMES)
        (PROGN
         (SETQ S
                 (APPEND (PRFMEX (GETV (GETV CODMAT (PLUS MAXVAR X)) 4) 'TIMES)
                         (COMPEX (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))))
         (COND ((NULL S) (SETQ S (LIST 0))))
         (SETQ EC (GETV (GETV CODMAT (PLUS MAXVAR X)) 6))
         (SETQ LS (LENGTH S))
         (COND
          ((|:ONEP| EC)
           (COND ((GREATERP LS 1) (SETQ S (CONS 'TIMES S)))
                 (T (SETQ S (CAR S)))))
          ((|:ONEP| (DM-MINUS EC))
           (SETQ S
                   (COND ((GREATERP LS 1) (LIST 'MINUS (CONS 'TIMES S)))
                         (T (LIST 'MINUS (CAR S))))))
          ((|:MINUSP| EC)
           (SETQ S (LIST 'MINUS (CONS 'TIMES (CONS (DM-MINUS EC) S)))))
          (T (SETQ S (CONS 'TIMES (CONS EC S)))))))
       ((EQ OPV 'PLUS)
        (PROGN
         (SETQ S
                 (APPEND (PRFMEX (GETV (GETV CODMAT (PLUS MAXVAR X)) 4) 'PLUS)
                         (COMPEX (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))))
         (COND ((NULL S) (SETQ S (LIST 0))))
         (COND ((GREATERP (LENGTH S) 1) (SETQ S (CONS 'PLUS (SHIFTMINUS S))))
               (T (SETQ S (CAR S))))
         (COND
          ((GREATERP (SETQ EC (GETV (GETV CODMAT (PLUS MAXVAR X)) 6)) 1)
           (SETQ S (LIST 'EXPT S EC))))))
       (T
        (PROGN
         (SETQ CH (GETV (GETV CODMAT (PLUS MAXVAR X)) 5))
         (PROG (Z)
           (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (COND
               ((NULL Z)
                (PROGN
                 (SETQ S (CONS (CONSTREXP (CAR CH)) S))
                 (SETQ CH (CDR CH))))
               (T (SETQ S (CONS Z S)))))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (SETQ S (CONS (CAR OPV) (REVERSE S)))
         (PROG (OP)
           (SETQ OP (CDR OPV))
          LAB
           (COND ((NULL OP) (RETURN NIL)))
           ((LAMBDA (OP) (SETQ S (LIST OP S))) (CAR OP))
           (SETQ OP (CDR OP))
           (GO LAB))
         (COND
          ((GREATERP (SETQ EC (GETV (GETV CODMAT (PLUS MAXVAR X)) 6)) 1)
           (SETQ S (LIST 'EXPT S EC)))))))
      (RETURN S))) 
(PUT 'SHIFTMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'SHIFTMINUS 'DEFINED-ON-LINE '505) 
(PUT 'SHIFTMINUS 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'SHIFTMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHIFTMINUS (S)
    (PROG (TS HEAD)
      (SETQ TS S)
      (SETQ HEAD NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND TS (PAIRP (CAR TS)) (EQ (CAAR TS) 'MINUS))) (RETURN NIL)))
        (PROGN (SETQ HEAD (CONS (CAR TS) HEAD)) (SETQ TS (CDR TS)))
        (GO WHILELABEL))
      (RETURN (COND (TS (APPEND TS (REVERSE HEAD))) (T S))))) 
(PUT 'PRFMEX 'NUMBER-OF-ARGS 2) 
(PUT 'PRFMEX 'DEFINED-ON-LINE '513) 
(PUT 'PRFMEX 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRFMEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRFMEX (ZZ OP)
    (PROG (Z FORALL-RESULT FORALL-ENDPTR)
      (SETQ Z ZZ)
      (COND ((NULL Z) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (Z)
                          (PROG (VAR NEX)
                            (SETQ VAR
                                    (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z)))
                                          3))
                            (COND
                             ((SETQ NEX (GET VAR 'NEX))
                              (PROGN (SETQ VAR NEX) (SYMTABREM NIL VAR))))
                            (COND
                             ((EQ VAR '+ONE)
                              (COND
                               ((|:MINUSP| (CAR (CDR Z)))
                                (RETURN
                                 (LIST 'MINUS (DM-MINUS (CAR (CDR Z))))))
                               (T (RETURN (CAR (CDR Z)))))))
                            (COND
                             ((NOT (|:ONEP| (DM-ABS (CAR (CDR Z)))))
                              (COND
                               ((EQ OP 'PLUS)
                                (SETQ VAR
                                        (LIST 'TIMES (DM-ABS (CAR (CDR Z)))
                                              VAR)))
                               ((CDR (CDR Z)) (SETQ VAR (CDR (CDR Z))))
                               (T (SETQ VAR (LIST 'EXPT VAR (CAR (CDR Z))))))))
                            (COND
                             ((|:MINUSP| (CAR (CDR Z)))
                              (SETQ VAR (LIST 'MINUS VAR))))
                            (RETURN VAR)))
                        (CAR Z))
                       NIL)))
     LOOPLABEL
      (SETQ Z (CDR Z))
      (COND ((NULL Z) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (Z)
                  (PROG (VAR NEX)
                    (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 3))
                    (COND
                     ((SETQ NEX (GET VAR 'NEX))
                      (PROGN (SETQ VAR NEX) (SYMTABREM NIL VAR))))
                    (COND
                     ((EQ VAR '+ONE)
                      (COND
                       ((|:MINUSP| (CAR (CDR Z)))
                        (RETURN (LIST 'MINUS (DM-MINUS (CAR (CDR Z))))))
                       (T (RETURN (CAR (CDR Z)))))))
                    (COND
                     ((NOT (|:ONEP| (DM-ABS (CAR (CDR Z)))))
                      (COND
                       ((EQ OP 'PLUS)
                        (SETQ VAR (LIST 'TIMES (DM-ABS (CAR (CDR Z))) VAR)))
                       ((CDR (CDR Z)) (SETQ VAR (CDR (CDR Z))))
                       (T (SETQ VAR (LIST 'EXPT VAR (CAR (CDR Z))))))))
                    (COND
                     ((|:MINUSP| (CAR (CDR Z))) (SETQ VAR (LIST 'MINUS VAR))))
                    (RETURN VAR)))
                (CAR Z))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'COMPEX 'NUMBER-OF-ARGS 1) 
(PUT 'COMPEX 'DEFINED-ON-LINE '541) 
(PUT 'COMPEX 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'COMPEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPEX (CHR)
    (PROG (CH FORALL-RESULT FORALL-ENDPTR)
      (SETQ CH CHR)
      (COND ((NULL CH) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (CH) (CONSTREXP CH)) (CAR CH)) NIL)))
     LOOPLABEL
      (SETQ CH (CDR CH))
      (COND ((NULL CH) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (CH) (CONSTREXP CH)) (CAR CH)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PRFKVAR 'NUMBER-OF-ARGS 2) 
(PUT 'PRFKVAR 'DEFINED-ON-LINE '550) 
(PUT 'PRFKVAR 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRFKVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRFKVAR (KV PREFIXLIST)
    (PROG (KVL X KVL1 NEX)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND KVARLST (NOT (EQUAL KV (CAAR KVARLST))))) (RETURN NIL)))
        (PROGN
         (SETQ KVL (CONS (CAR KVARLST) KVL))
         (SETQ KVARLST (CDR KVARLST)))
        (GO WHILELABEL))
      (COND
       ((NULL KVARLST)
        (PROGN
         (SETQ KVARLST KVL)
         (COND
          ((SETQ NEX (GET KV 'NEX))
           (SETQ PREFIXLIST
                   (CONS (CONS KV NEX) (NEXCHECK KV NEX PREFIXLIST)))))))
       (T
        (PROGN
         (SETQ KVL1 (CAR KVARLST))
         (SETQ KVARLST (APPEND KVL (CDR KVARLST)))
         (PROG (VAR)
           (SETQ VAR (CDDR KVL1))
          LAB
           (COND ((NULL VAR) (RETURN NIL)))
           ((LAMBDA (VAR)
              (COND
               ((SETQ X (GET VAR 'ROWINDEX))
                (SETQ PREFIXLIST (PRFEXP X PREFIXLIST)))
               (T (SETQ PREFIXLIST (PRFKVAR VAR PREFIXLIST)))))
            (CAR VAR))
           (SETQ VAR (CDR VAR))
           (GO LAB))
         (COND
          ((SETQ NEX (GET KV 'NEX))
           (PROGN
            (SETQ PREFIXLIST (NEXCHECK KV NEX PREFIXLIST))
            (SETQ KV NEX))))
         (SETQ PREFIXLIST (CONS (CONS KV (CDR KVL1)) PREFIXLIST))
         (FLAG (LIST KV) 'DONE))))
      (RETURN PREFIXLIST))) 
(PUT 'NEXCHECK 'NUMBER-OF-ARGS 3) 
(PUT 'NEXCHECK 'DEFINED-ON-LINE '593) 
(PUT 'NEXCHECK 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'NEXCHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEXCHECK (KV NEX PREFIXLIST)
    (PROG (X)
      (COND
       ((NOT (OR (FLAGP KV 'DONE) (AND *VECTORC (SUBSCRIPTEDVARP (CAR NEX)))))
        (PROG (ARG)
          (SETQ ARG (CDR NEX))
         LAB
          (COND ((NULL ARG) (RETURN NIL)))
          ((LAMBDA (ARG)
             (COND
              ((SETQ X (GET ARG 'ROWINDEX))
               (SETQ PREFIXLIST (PRFEXP X PREFIXLIST)))
              (T (SETQ PREFIXLIST (PRFKVAR ARG PREFIXLIST)))))
           (CAR ARG))
          (SETQ ARG (CDR ARG))
          (GO LAB))))
      (SYMTABREM NIL KV)
      (REMPROP KV 'NEX)
      (RETURN PREFIXLIST))) 
(PUT 'EVALPARTPREFIXLIST 'NUMBER-OF-ARGS 1) 
(PUT 'EVALPARTPREFIXLIST 'DEFINED-ON-LINE '609) 
(PUT 'EVALPARTPREFIXLIST 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'EVALPARTPREFIXLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALPARTPREFIXLIST (PREFIXLIST)
    (PROG (NEWPREFIXLIST PAIR TEMP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL PREFIXLIST))) (RETURN NIL)))
        (PROGN
         (COND
          ((SETQ PAIR (EVALPART1 (CAR PREFIXLIST)))
           (SETQ NEWPREFIXLIST (CONS PAIR NEWPREFIXLIST))))
         (SETQ PREFIXLIST (CDR PREFIXLIST)))
        (GO WHILELABEL))
      (PROG (ITEM)
        (SETQ ITEM (GET 'EVALPART1 'SETKLIST))
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (REMPROP ITEM 'AVALUE)
            (COND
             ((SETQ TEMP (GET ITEM 'TAVAL))
              (PROGN
               (SETK ITEM (MK*SQ (SIMP* TEMP)))
               (REMPROP ITEM 'TAVAL))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (REMPROP 'EVALPART1 'SETKLIST)
      (RETURN (REVERSE NEWPREFIXLIST)))) 
(PUT 'EVALPART1 'NUMBER-OF-ARGS 1) 
(PUT 'EVALPART1 'DEFINED-ON-LINE '630) 
(PUT 'EVALPART1 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'EVALPART1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALPART1 (PAIR)
    (PROG (CARPAIR EXP RES X)
      (SETQ EXP *EXP)
      (SETQ *EXP T)
      (SETQ CARPAIR (CAR PAIR))
      (SETQ X (REVAL1 (CDR PAIR) T))
      (COND
       ((AND (NOT (OR (ATOM X) (MEMQ (CAR X) '(PLUS DIFFERENCE))))
             (FLAGP CARPAIR 'NEWSYM))
        (PROGN
         (COND
          ((AND (GET CARPAIR 'AVALUE) (NOT (GET CARPAIR 'TAVAL)))
           (PUT CARPAIR 'TAVAL (PREPSQ (CADADR (GET CARPAIR 'AVALUE))))))
         (SETK CARPAIR (REVAL1 X NIL))))
       (T (SETQ RES (CONS CARPAIR X))))
      (COND
       ((NULL RES)
        (PUT 'EVALPART1 'SETKLIST (CONS CARPAIR (GET 'EVALPART1 'SETKLIST)))))
      (SETQ *EXP EXP)
      (RETURN RES))) 
(PUT 'REMOVEARRAYSUBSTITUTES 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVEARRAYSUBSTITUTES 'DEFINED-ON-LINE '648) 
(PUT 'REMOVEARRAYSUBSTITUTES 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'REMOVEARRAYSUBSTITUTES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVEARRAYSUBSTITUTES (PREFIXLIST)
    (PROG (NEWPREFIXLIST PAIR)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL PREFIXLIST))) (RETURN NIL)))
        (PROGN
         (SETQ PAIR (CAR PREFIXLIST))
         (SETQ PREFIXLIST (CDR PREFIXLIST))
         (COND
          ((AND (FLAGP (CAR PAIR) 'NEWSYM) (PAIRP (CDR PAIR))
                (SUBSCRIPTEDVARP (CADR PAIR)))
           (SETQ PREFIXLIST
                   (PROG (ITEM FORALL-RESULT FORALL-ENDPTR)
                     (SETQ ITEM PREFIXLIST)
                     (COND ((NULL ITEM) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (ITEM)
                                         (SUBST (CDR PAIR) (CAR PAIR) ITEM))
                                       (CAR ITEM))
                                      NIL)))
                    LOOPLABEL
                     (SETQ ITEM (CDR ITEM))
                     (COND ((NULL ITEM) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (ITEM)
                                 (SUBST (CDR PAIR) (CAR PAIR) ITEM))
                               (CAR ITEM))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
          (T (SETQ NEWPREFIXLIST (CONS PAIR NEWPREFIXLIST))))
         NIL)
        (GO WHILELABEL))
      (RETURN (REVERSE NEWPREFIXLIST)))) 
(PUT 'PREPPOWLS 'NUMBER-OF-ARGS 0) 
(PUT 'PREPPOWLS 'DEFINED-ON-LINE '740) 
(PUT 'PREPPOWLS 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PREPPOWLS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PREPPOWLS NIL
    (PROG (VAR NVAR NVARLST RINDX)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE (MINUS 1) Y)) (RETURN NIL)))
        (COND
         ((AND
           (NOT (NUMBERP (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3))))
           (EQ (GETV (GETV CODMAT (PLUS MAXVAR Y)) 2) 'TIMES))
          (PROGN
           (PROG (Z)
             (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR Y)) 4))
            LAB
             (COND ((NULL Z) (RETURN NIL)))
             ((LAMBDA (Z)
                (COND ((EQUAL (CAR (CDR Z)) 1) (RPLACD (CDR Z) VAR))
                      (T
                       (PROGN
                        (SETQ RINDX (CAR Z))
                        (SETPREV RINDX VAR)
                        (COND
                         ((NOT NVARLST) (SETQ NVARLST (LIST (CONS 1 VAR)))))
                        (COND
                         ((OR
                           (NUMBERP
                            (SETQ NVAR
                                    (GETV (GETV CODMAT (PLUS MAXVAR RINDX))
                                          3)))
                           (PAIRP NVAR)
                           (NOT
                            (AND
                             (NULL
                              (CDR (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 4)))
                             (NULL (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 5))
                             (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 6)
                                    1))))
                          (SETQ NVAR (FNEWSYM)))
                         (T (PUT NVAR 'BEXL RINDX)))
                        (RPLACD (CDR Z) NVAR)
                        (PUTV (GETV CODMAT (PLUS MAXVAR RINDX)) 4
                              (INSZZZR
                               ((LAMBDA (G209)
                                  (COND
                                   ((OR (IDP G209) (CONSTP G209))
                                    (CONS Y (CONS G209 NIL)))
                                   (T (CONS Y G209))))
                                (CONS (CAR (CDR Z)) NVAR))
                               (DELYZZ Y
                                (GETV (GETV CODMAT (PLUS MAXVAR RINDX)) 4))))
                        (SETQ NVARLST
                                (INSEXPLST (CONS (CAR (CDR Z)) NVAR) NVARLST))
                        NIL))))
              (CAR Z))
             (SETQ Z (CDR Z))
             (GO LAB))
           (COND
            (NVARLST (PROGN (PUT VAR 'NVARLST NVARLST) (SETQ NVARLST NIL)))))))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (TERPRI))) 
(PUT 'PRFPOWL 'NUMBER-OF-ARGS 2) 
(PUT 'PRFPOWL 'DEFINED-ON-LINE '788) 
(PUT 'PRFPOWL 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PRFPOWL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRFPOWL (Y PREFIXLIST)
    (PROG (NVARLST EXPLST FIRST CFIRST CSECOND DIFF VAR POWLST VAR1 VAR2)
      (SETQ NVARLST (SETQ EXPLST (GET Y 'NVARLST)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ FIRST (CAR EXPLST))
         (SETQ CFIRST (CAR FIRST))
         (SETQ CSECOND (CAAR (SETQ EXPLST (CDR EXPLST))))
         (SETQ DIFF (DIFFERENCE CFIRST CSECOND))
         (COND
          ((GREATERP DIFF CSECOND)
           (PROGN
            (COND
             ((EQUAL (REMAINDER CFIRST 2) 1)
              (PROGN
               (SETQ CFIRST (DIFFERENCE CFIRST 1))
               (SETQ VAR (FNEWSYM))
               (SETQ POWLST
                       (CONS (CONS (CDR FIRST) (LIST 'TIMES Y VAR)) POWLST))
               (SETQ FIRST (CONS CFIRST VAR))
               (SETQ NVARLST (CONS FIRST NVARLST)))))
            (SETQ DIFF (SETQ CSECOND (QUOTIENT CFIRST 2)))
            NIL)))
         (COND
          ((NULL (ASSOC DIFF NVARLST))
           (PROGN
            (SETQ VAR (FNEWSYM))
            (SETQ NVARLST (CONS (CONS DIFF VAR) NVARLST)))))
         (SETQ VAR1 (CDR (ASSOC DIFF NVARLST)))
         (SETQ VAR2 (CDR (ASSOC CSECOND NVARLST)))
         (SETQ POWLST (CONS (CONS (CDR FIRST) (LIST 'TIMES VAR1 VAR2)) POWLST))
         (SETQ EXPLST (INSEXPLST (CONS DIFF VAR1) EXPLST))
         NIL)
        (COND
         ((NOT (AND (EQUAL DIFF CSECOND) (EQUAL CSECOND 1)))
          (GO REPEATLABEL))))
      (SETQ PREFIXLIST (APPEND (REVERSE POWLST) PREFIXLIST))
      (RETURN PREFIXLIST))) 
(PUT 'INSEXPLST 'NUMBER-OF-ARGS 2) 
(PUT 'INSEXPLST 'DEFINED-ON-LINE '832) 
(PUT 'INSEXPLST 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'INSEXPLST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSEXPLST (EL EXPLST)
    (COND
     ((OR (NULL EXPLST) (GREATERP (CAR EL) (CAAR EXPLST))) (CONS EL EXPLST))
     ((EQUAL (CAR EL) (CAAR EXPLST)) EXPLST)
     (T (CONS (CAR EXPLST) (INSEXPLST EL (CDR EXPLST)))))) 
(GLOBAL '(CODBEXL*)) 
(PUT 'ALIASBACKSUBST 'NUMBER-OF-ARGS 1) 
(PUT 'ALIASBACKSUBST 'DEFINED-ON-LINE '859) 
(PUT 'ALIASBACKSUBST 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'ALIASBACKSUBST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALIASBACKSUBST (PFL)
    (PROG (BACKSUBSTLIST ORIGINAL LHS NPFL)
      (SETQ BACKSUBSTLIST RHSALIASES)
      (PROG (STAT)
        (SETQ STAT (REVERSE PFL))
       LAB
        (COND ((NULL STAT) (RETURN NIL)))
        ((LAMBDA (STAT)
           (PROGN
            (COND
             ((SETQ ORIGINAL (GET (SETQ LHS (CAR STAT)) 'ALIAS))
              (COND
               ((OR (ATOM ORIGINAL)
                    (EQ LHS
                        (CADR
                         (ASSOC ORIGINAL (GET (CAR ORIGINAL) 'FINALALIAS))))
                    (VECTORVARP (CAR ORIGINAL)))
                (SETQ BACKSUBSTLIST (CONS (CONS LHS ORIGINAL) BACKSUBSTLIST)))
               (T (SETQ ORIGINAL NIL)))))
            (SETQ NPFL
                    (CONS
                     (CONS (COND (ORIGINAL ORIGINAL) (T LHS))
                           (RECALIASBACKSUBST (CDR STAT) BACKSUBSTLIST))
                     NPFL))
            NIL))
         (CAR STAT))
        (SETQ STAT (CDR STAT))
        (GO LAB))
      (RETURN (REVERSE NPFL)))) 
(PUT 'RECALIASBACKSUBST 'NUMBER-OF-ARGS 2) 
(PUT 'RECALIASBACKSUBST 'DEFINED-ON-LINE '891) 
(PUT 'RECALIASBACKSUBST 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'RECALIASBACKSUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RECALIASBACKSUBST (EX AL)
    (COND
     ((OR (ATOM EX) (CONSTP EX))
      (COND ((ASSOC EX AL) (CDR (ASSOC EX AL))) (T EX)))
     (T
      (PROG (EL FORALL-RESULT FORALL-ENDPTR)
        (SETQ EL EX)
        (COND ((NULL EL) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL) (RECALIASBACKSUBST EL AL)) (CAR EL))
                         NIL)))
       LOOPLABEL
        (SETQ EL (CDR EL))
        (COND ((NULL EL) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (EL) (RECALIASBACKSUBST EL AL)) (CAR EL)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'REINSERTRATEXPS 'NUMBER-OF-ARGS 2) 
(PUT 'REINSERTRATEXPS 'DEFINED-ON-LINE '901) 
(PUT 'REINSERTRATEXPS 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'REINSERTRATEXPS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REINSERTRATEXPS (PPL PFL)
    (PROG (KEYS NPFL)
      (SETQ KEYS
              (PROG (RE FORALL-RESULT FORALL-ENDPTR)
                (SETQ RE PPL)
                (COND ((NULL RE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (RE) (CAR RE)) (CAR RE)) NIL)))
               LOOPLABEL
                (SETQ RE (CDR RE))
                (COND ((NULL RE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (RE) (CAR RE)) (CAR RE)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (STAT)
        (SETQ STAT PFL)
       LAB
        (COND ((NULL STAT) (RETURN NIL)))
        ((LAMBDA (STAT)
           (PROGN
            (PROG (K)
              (SETQ K KEYS)
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (COND
                  ((NOT (FREEOF (CDR STAT) K))
                   (PROGN
                    (SETQ NPFL (CONS (ASSOC K PPL) NPFL))
                    (SETQ KEYS (DELETE K KEYS))))))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))
            (SETQ NPFL (CONS STAT NPFL))))
         (CAR STAT))
        (SETQ STAT (CDR STAT))
        (GO LAB))
      (RETURN (REVERSE NPFL)))) 
(PUT 'CLEANUPVARS 'NUMBER-OF-ARGS 2) 
(PUT 'CLEANUPVARS 'DEFINED-ON-LINE '921) 
(PUT 'CLEANUPVARS 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'CLEANUPVARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLEANUPVARS (P PFL)
    (PROG (CSENOW LP DP PN SV)
      (SETQ CSENOW (FNEWSYM))
      (SETQ LP (LETTERPART CSENOW))
      (SETQ DP (DIGITPART CSENOW))
      (SETQ PN
              (PROG (IDX FORALL-RESULT FORALL-ENDPTR)
                (SETQ IDX 0)
                (COND ((MINUSP (DIFFERENCE DP IDX)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (MKID LP IDX) NIL)))
               LOOPLABEL
                (SETQ IDX (PLUS2 IDX 1))
                (COND ((MINUSP (DIFFERENCE DP IDX)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (MKID LP IDX) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND *AGAIN (NOT *VECTORC))
        (PROGN
         (PROG (F)
           (SETQ F PFL)
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (COND
               ((AND (ATOM (CAR F)) (FLAGP (CAR F) 'INALIAS))
                (PROGN
                 (REMFLAG (LIST (CAR F)) 'INALIAS)
                 (REMFLAG (LIST (CAR F)) 'NEWSYM)))))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))))
       ((NOT *AGAIN)
        (PROGN
         (PROG (V)
           (SETQ V PN)
          LAB
           (COND ((NULL V) (RETURN NIL)))
           ((LAMBDA (V)
              (COND
               ((SETQ SV (GET V 'ALIAS))
                (PROGN
                 (COND ((PAIRP SV) (SETQ SV (CAR SV))))
                 (REMPROP SV 'FINALALIAS)
                 (PROG (A)
                   (SETQ A (GET SV 'ALIASLIST))
                  LAB
                   (COND ((NULL A) (RETURN NIL)))
                   ((LAMBDA (A)
                      (PROGN
                       (REMPROP A 'ALIAS)
                       (PROG (A2)
                         (SETQ A2 (GET A 'ALIASLIST))
                        LAB
                         (COND ((NULL A2) (RETURN NIL)))
                         ((LAMBDA (A2) (REMPROP A2 'ALIAS)) (CAR A2))
                         (SETQ A2 (CDR A2))
                         (GO LAB))
                       (REMPROP A 'FINALALIAS)
                       NIL))
                    (CAR A))
                   (SETQ A (CDR A))
                   (GO LAB))
                 (REMPROP SV 'ALIASLIST)
                 NIL))))
            (CAR V))
           (SETQ V (CDR V))
           (GO LAB))
         (SETQ PN (APPEND PN P))
         (REMFLAG PN 'SUBSCRIPTED)
         (REMFLAG PN 'INALIAS)
         (REMFLAG PN 'ALIASNEWSYM)
         NIL))))) 
(PUT 'LISTEQ 'NUMBER-OF-ARGS 2) 
(PUT 'LISTEQ 'DEFINED-ON-LINE '961) 
(PUT 'LISTEQ 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'LISTEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTEQ (A B)
    (COND ((ATOM A) (EQ A B)) ((ATOM B) NIL)
          (T (AND (LISTEQ (CAR A) (CAR B)) (LISTEQ (CDR A) (CDR B)))))) 
(DE PROTECTED (A PN) (MEMBER (COND ((ATOM A) A) (T (CAR A))) PN)) 
(PUT 'PROTECTED 'NUMBER-OF-ARGS 2) 
(PUT 'PROTECTED 'DEFINED-ON-LINE '969) 
(PUT 'PROTECTED 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PROTECTED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'PROTECTED 'INLINE
      '(LAMBDA (A PN) (MEMBER (COND ((ATOM A) A) (T (CAR A))) PN))) 
(DE PROTECT (N PN)
    (COND ((MEMBER (COND ((ATOM N) N) (T (CAR N))) PN) PN)
          (T (CONS (COND ((ATOM N) N) (T (CAR N))) PN)))) 
(PUT 'PROTECT 'NUMBER-OF-ARGS 2) 
(PUT 'PROTECT 'DEFINED-ON-LINE '972) 
(PUT 'PROTECT 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'PROTECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'PROTECT 'INLINE
      '(LAMBDA (N PN)
         (COND ((MEMBER (COND ((ATOM N) N) (T (CAR N))) PN) PN)
               (T (CONS (COND ((ATOM N) N) (T (CAR N))) PN))))) 
(PUT 'CLEANUPPREFIXLIST 'NUMBER-OF-ARGS 1) 
(PUT 'CLEANUPPREFIXLIST 'DEFINED-ON-LINE '977) 
(PUT 'CLEANUPPREFIXLIST 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'CLEANUPPREFIXLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEANUPPREFIXLIST (PREFIXLIST)
    (PROG (LPL PROTECTEDNAMES J ITEM SUBSTLST DELLST SE OSE R DEFVARLST RHSOCC
           OCC VAR SGN LHS RHS)
      (COND
       (PREPREFIXLIST
        (SETQ PREFIXLIST (REINSERTRATEXPS PREPREFIXLIST PREFIXLIST))))
      (SETQ PREFIXLIST (ALIASBACKSUBST (REVERSE PREFIXLIST)))
      (SETQ LPL (LENGTH PREFIXLIST))
      (SETQ LHS (MKVECT LPL))
      (SETQ RHS (MKVECT LPL))
      (PROG (INDX)
        (SETQ INDX CODBEXL*)
       LAB
        (COND ((NULL INDX) (RETURN NIL)))
        ((LAMBDA (INDX)
           (COND
            ((NUMBERP INDX)
             (PROGN
              (COND
               ((SETQ VAR (GET (GETV (GETV CODMAT (PLUS MAXVAR INDX)) 3) 'NEX))
                (SETQ PROTECTEDNAMES
                        (COND
                         ((MEMBER (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                                  PROTECTEDNAMES)
                          PROTECTEDNAMES)
                         (T
                          (CONS (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                                PROTECTEDNAMES)))))
               ((NOT
                 (FLAGP (GETV (GETV CODMAT (PLUS MAXVAR INDX)) 3)
                        'ALIASNEWSYM))
                (SETQ PROTECTEDNAMES
                        ((LAMBDA (G211)
                           (COND
                            ((MEMBER (COND ((ATOM G211) G211) (T (CAR G211)))
                                     PROTECTEDNAMES)
                             PROTECTEDNAMES)
                            (T
                             (CONS (COND ((ATOM G211) G211) (T (CAR G211)))
                                   PROTECTEDNAMES))))
                         (GETV (GETV CODMAT (PLUS MAXVAR INDX)) 3))))
               ((SETQ VAR
                        (GET (GETV (GETV CODMAT (PLUS MAXVAR INDX)) 3) 'ALIAS))
                (SETQ PROTECTEDNAMES
                        (COND
                         ((MEMBER (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                                  PROTECTEDNAMES)
                          PROTECTEDNAMES)
                         (T
                          (CONS (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                                PROTECTEDNAMES))))))))
            ((IDP INDX)
             (COND
              ((NOT (FLAGP INDX 'ALIASNEWSYM))
               (SETQ PROTECTEDNAMES
                       (COND
                        ((MEMBER (COND ((ATOM INDX) INDX) (T (CAR INDX)))
                                 PROTECTEDNAMES)
                         PROTECTEDNAMES)
                        (T
                         (CONS (COND ((ATOM INDX) INDX) (T (CAR INDX)))
                               PROTECTEDNAMES)))))
              ((SETQ VAR (GET INDX 'ALIAS))
               (SETQ PROTECTEDNAMES
                       (COND
                        ((MEMBER (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                                 PROTECTEDNAMES)
                         PROTECTEDNAMES)
                        (T
                         (CONS (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                               PROTECTEDNAMES)))))))))
         (CAR INDX))
        (SETQ INDX (CDR INDX))
        (GO LAB))
      (SETQ J 0)
      (PROG (ITEM)
        (SETQ ITEM PREFIXLIST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (PUTV LHS (SETQ J (PLUS J 1)) (CAR ITEM))
            (PUTV RHS J (CDR ITEM))
            (SETQ SE NIL)
            (COND
             ((AND (PAIRP (CDR ITEM)) (GET (SETQ SE (CADR ITEM)) 'KVARLST))
              (REMPROP SE 'KVARLST)))
            (COND ((FLAGP SE 'DONE) (REMFLAG (LIST SE) 'DONE)))
            (SETQ DEFVARLST (CONS (CONS (CAR ITEM) J) DEFVARLST))
            (COND
             ((PAIRP (CAR ITEM)) (SETQ RHSOCC (INSOCCS (CAR ITEM) RHSOCC))))
            (SETQ RHSOCC (INSOCCS (CDR ITEM) RHSOCC))
            (SETQ SGN NIL)
            (COND
             ((EQCAR (CDR ITEM) 'MINUS)
              (PROGN (SETQ SGN T) (SETQ ITEM (CONS (CAR ITEM) (CADDR ITEM))))))
            (COND
             ((AND (IDP (CDR ITEM))
                   (AND
                    (MEMBER
                     (COND ((ATOM (CAR ITEM)) (CAR ITEM)) (T (CAR (CAR ITEM))))
                     PROTECTEDNAMES)
                    (NOT
                     (MEMBER
                      (COND ((ATOM (CDR ITEM)) (CDR ITEM))
                            (T (CAR (CDR ITEM))))
                      PROTECTEDNAMES)))
                   (NOT (AND (GET (CAR ITEM) 'FINALALIAS) (PAIRP (CAR ITEM))))
                   (SETQ R (ASSOC (CDR ITEM) DEFVARLST))
                   (NOT (ASSOC (CDR ITEM) SUBSTLST)) (MOVABLE ITEM DEFVARLST))
              (PROGN
               (SETQ DELLST (CONS (CAR ITEM) DELLST))
               (SETQ SUBSTLST
                       (CONS (SUBSTEL (CONS (CDR ITEM) (CAR ITEM)) SGN)
                             SUBSTLST))
               (COND
                ((AND SGN R)
                 (PROGN
                  (PUTV RHS (CDR R) (CONS 'MINUS (LIST (GETV RHS (CDR R)))))
                  (PUTV LHS (CDR R) (GETV LHS J))
                  (PUTV RHS J (GETV LHS J))))))))
            NIL))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LPL J)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (MEMBER (GETV LHS J) DELLST)
                (OR
                 (NOT
                  ((LAMBDA (G213)
                     (MEMBER (COND ((ATOM G213) G213) (T (CAR G213)))
                             PROTECTEDNAMES))
                   (GETV LHS J)))
                 (EQ (GETV LHS J) (GETV RHS J))))
           (PROGN (PUTV LHS J NIL) (PUTV RHS J NIL)))
          (T
           (PROGN
            (COND
             ((PAIRP (GETV LHS J))
              (PUTV LHS J (REPLACEIN (GETV LHS J) SUBSTLST))))
            (PUTV RHS J (REPLACEIN (GETV RHS J) SUBSTLST))
            (SETQ ITEM (CONS (GETV LHS J) (GETV RHS J)))
            (SETQ SGN NIL)
            (COND
             ((EQCAR (CDR ITEM) 'MINUS)
              (PROGN (SETQ SGN T) (SETQ ITEM (CONS (CAR ITEM) (CADDR ITEM))))))
            (SETQ SE NIL)
            (COND
             ((AND (LISTEQ (CAR ITEM) (CDR ITEM)) (NOT SGN))
              (PROGN (PUTV LHS J NIL) (PUTV RHS J NIL)))
             (T
              (PROGN
               (COND
                ((AND (CONSTP (CDR ITEM))
                      (NOT
                       (MEMBER
                        (COND ((ATOM (CAR ITEM)) (CAR ITEM))
                              (T (CAR (CAR ITEM))))
                        PROTECTEDNAMES)))
                 (SETQ SE (SUBSTEL ITEM SGN)))
                ((AND
                  (COND ((ATOM (CDR ITEM)) (IDP (CDR ITEM)))
                        (T (SUBSCRIPTEDVARP (CADR ITEM))))
                  (NOT
                   (MEMBER
                    (COND ((ATOM (CAR ITEM)) (CAR ITEM)) (T (CAR (CAR ITEM))))
                    PROTECTEDNAMES)))
                 (SETQ SE (SUBSTEL ITEM SGN)))
                ((AND
                  (NOT
                   (MEMBER
                    (COND ((ATOM (CAR ITEM)) (CAR ITEM)) (T (CAR (CAR ITEM))))
                    PROTECTEDNAMES))
                  (SETQ OCC (ASSOC (CAR ITEM) RHSOCC)) (EQUAL (CDR OCC) 1))
                 (SETQ SE (SUBSTEL ITEM SGN))))
               NIL)))
            (COND
             (SE
              (PROGN
               (COND
                ((SETQ OSE (ASSOC (CAR SE) SUBSTLST))
                 (PROGN
                  (SETQ SUBSTLST (DELETE OSE SUBSTLST))
                  (SETQ SUBSTLST
                          (DELETE (SUBSTEL (CONS (CDR OSE) (CDR OSE)) T)
                                  SUBSTLST))
                  (SETQ DELLST (DELETE (CDR OSE) DELLST)))))
               (SETQ SUBSTLST (CONS SE SUBSTLST))
               (PUTV LHS J NIL)
               (PUTV RHS J NIL)))
             ((AND (SETQ SE (ASSOC (CAR ITEM) SUBSTLST))
                   (NOT
                    (AND
                     (MEMBER
                      (COND ((ATOM (CAR ITEM)) (CAR ITEM))
                            (T (CAR (CAR ITEM))))
                      PROTECTEDNAMES)
                     (EQ J (CDR (ASSOC (CAR ITEM) DEFVARLST))))))
              (PUTV LHS J (CDR SE)))
             (SE (SETQ SUBSTLST (DELETE SE SUBSTLST))))
            NIL))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ PREFIXLIST NIL)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LPL J)) (RETURN NIL)))
        (COND
         ((GETV LHS J)
          (SETQ PREFIXLIST
                  (CONS (CONS (GETV LHS J) (GETV RHS J)) PREFIXLIST))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (COND
       (MIN-EXPR-LENGTH*
        (SETQ PREFIXLIST
                (MAKE_MIN_LENGTH (REVERSE PREFIXLIST) PROTECTEDNAMES)))
       (T (SETQ PREFIXLIST (REVERSE PREFIXLIST))))
      (APPLY1 'ARESTORE AVARLST)
      (CLEANUPVARS PROTECTEDNAMES PREFIXLIST)
      (RETURN PREFIXLIST))) 
(PUT 'MOVABLE 'NUMBER-OF-ARGS 2) 
(PUT 'MOVABLE 'DEFINED-ON-LINE '1227) 
(PUT 'MOVABLE 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'MOVABLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOVABLE (V DEFL)
    (COND
     ((PAIRP (CAR V))
      (NOT
       (MEMBER NIL
               (PROG (IDX FORALL-RESULT FORALL-ENDPTR)
                 (SETQ IDX (CDAR V))
                 (COND ((NULL IDX) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (IDX)
                                     (OR (NUMBERP IDX)
                                         (COND
                                          ((ASSOC IDX DEFL)
                                           (LESSP (CDR (ASSOC IDX DEFL))
                                                  (CDR (ASSOC (CDR V) DEFL))))
                                          (T T))))
                                   (CAR IDX))
                                  NIL)))
                LOOPLABEL
                 (SETQ IDX (CDR IDX))
                 (COND ((NULL IDX) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (IDX)
                             (OR (NUMBERP IDX)
                                 (COND
                                  ((ASSOC IDX DEFL)
                                   (LESSP (CDR (ASSOC IDX DEFL))
                                          (CDR (ASSOC (CDR V) DEFL))))
                                  (T T))))
                           (CAR IDX))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
     (T T))) 
(PUT 'INSOCCS 'NUMBER-OF-ARGS 2) 
(PUT 'INSOCCS 'DEFINED-ON-LINE '1242) 
(PUT 'INSOCCS 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'INSOCCS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSOCCS (X OCCLST)
    (PROG ()
      (COND
       ((OR (IDP X) (SUBSCRIPTEDVARP X)
            (AND (PAIRP X) (SUBSCRIPTEDVARP (CAR X))))
        (SETQ OCCLST (INSERTOCC OCCLST X))))
      (COND
       ((AND (NOT (IDP X)) (NOT (CONSTP X)))
        (PROG (ARG)
          (SETQ ARG (CDR X))
         LAB
          (COND ((NULL ARG) (RETURN NIL)))
          ((LAMBDA (ARG) (SETQ OCCLST (INSOCCS ARG OCCLST))) (CAR ARG))
          (SETQ ARG (CDR ARG))
          (GO LAB))))
      (RETURN OCCLST))) 
(PUT 'INSERTOCC 'NUMBER-OF-ARGS 2) 
(PUT 'INSERTOCC 'DEFINED-ON-LINE '1254) 
(PUT 'INSERTOCC 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'INSERTOCC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSERTOCC (OCCLST VAR)
    (PROG (OLDEL)
      (COND
       ((SETQ OLDEL (ASSOC VAR OCCLST))
        (SETQ OCCLST (SUBST (CONS VAR (PLUS (CDR OLDEL) 1)) OLDEL OCCLST)))
       (T (SETQ OCCLST (CONS (CONS VAR 1) OCCLST))))
      (RETURN OCCLST))) 
(PUT 'SUBSTEL 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSTEL 'DEFINED-ON-LINE '1263) 
(PUT 'SUBSTEL 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'SUBSTEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSTEL (OLDNEW SIGN)
    (CONS (CAR OLDNEW)
          (COND (SIGN (LIST 'MINUS (CDR OLDNEW))) (T (CDR OLDNEW))))) 
(PUT 'REPLACEIN 'NUMBER-OF-ARGS 2) 
(PUT 'REPLACEIN 'DEFINED-ON-LINE '1266) 
(PUT 'REPLACEIN 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'REPLACEIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPLACEIN (EXPR1 SL)
    (PROG (NEXPR ISZERO)
      (RETURN
       (COND
        ((OR (IDP EXPR1) (SUBSCRIPTEDVARP EXPR1))
         (COND ((SETQ NEXPR (ASSOC EXPR1 SL)) (CDR NEXPR)) (T EXPR1)))
        ((CONSTP EXPR1) EXPR1)
        (T
         (PROGN
          (SETQ NEXPR
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL (CDR EXPR1))
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (REPLACEIN EL SL)) (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (EL) (REPLACEIN EL SL)) (CAR EL))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ EXPR1 (APPEND (LIST (CAR EXPR1)) NEXPR))
          (COND
           ((AND (EQCAR EXPR1 'MINUS) (EQCAR (CADR EXPR1) 'MINUS))
            (SETQ EXPR1 (CADADR EXPR1))))
          (COND
           ((EQCAR EXPR1 'PLUS)
            (PROGN
             (SETQ NEXPR '(PLUS))
             (PROG (EL)
               (SETQ EL (CDR EXPR1))
              LAB
               (COND ((NULL EL) (RETURN NIL)))
               ((LAMBDA (EL)
                  (COND
                   ((NOT (AND (CONSTP EL) (|:ZEROP| EL)))
                    (SETQ NEXPR
                            (APPEND NEXPR
                                    (COND ((EQCAR EL 'PLUS) (CDR EL))
                                          (T (LIST EL))))))))
                (CAR EL))
               (SETQ EL (CDR EL))
               (GO LAB))
             (SETQ EXPR1 NEXPR)))
           ((EQCAR EXPR1 'TIMES)
            (PROGN
             (SETQ ISZERO NIL)
             (SETQ NEXPR '(TIMES))
             (PROG (EL)
               (SETQ EL (CDR EXPR1))
              LAB
               (COND ((NULL EL) (RETURN NIL)))
               ((LAMBDA (EL)
                  (PROGN
                   (COND
                    ((NOT (AND (CONSTP EL) (|:ONEP| EL)))
                     (SETQ NEXPR
                             (APPEND NEXPR
                                     (COND ((EQCAR EL 'TIMES) (CDR EL))
                                           (T (LIST EL)))))))
                   (COND ((AND (CONSTP EL) (|:ZEROP| EL)) (SETQ ISZERO T)))))
                (CAR EL))
               (SETQ EL (CDR EL))
               (GO LAB))
             (SETQ EXPR1 (COND (ISZERO 0) (T NEXPR)))))
           ((AND (EQCAR EXPR1 'QUOTIENT) (CONSTP (CADDR EXPR1))
                 (|:ONEP| (CADDR EXPR1)))
            (SETQ EXPR1 (CADR EXPR1)))
           ((EQCAR EXPR1 'QUOTIENT) (SETQ EXPR1 (QQSTR? EXPR1)))
           ((AND (EQCAR EXPR1 'MINUS) (CONSTP (CADR EXPR1))
                 (|:ZEROP| (CADR EXPR1)))
            (SETQ EXPR1 0))
           ((AND (EQCAR EXPR1 'EXPT) (CONSTP (CADDR EXPR1)))
            (COND ((|:ZEROP| (CADDR EXPR1)) (SETQ EXPR1 1))
                  ((|:ONEP| (CADDR EXPR1)) (SETQ EXPR1 (CADR EXPR1))))))
          (COND
           ((AND (PAIRP EXPR1) (MEMQ (CAR EXPR1) '(TIMES PLUS)))
            (COND ((EQUAL (LENGTH EXPR1) 2) (SETQ EXPR1 (CADR EXPR1)))
                  ((EQUAL (LENGTH EXPR1) 1)
                   (SETQ EXPR1 (COND ((EQUAL EXPR1 'PLUS) 0) (T 1)))))))
          EXPR1)))))) 
(PUT 'QQSTR? 'NUMBER-OF-ARGS 1) 
(PUT 'QQSTR? 'DEFINED-ON-LINE '1333) 
(PUT 'QQSTR? 'DEFINED-IN-FILE 'SCOPE/CODPRI.RED) 
(PUT 'QQSTR? 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QQSTR? (EXPR1)
    (PROG (NR DM NR2 DM2)
      (SETQ NR (CADR EXPR1))
      (SETQ DM (CADDR EXPR1))
      (COND
       ((EQCAR NR 'QUOTIENT) (PROGN (SETQ DM2 (CADDR NR)) (SETQ NR (CADR NR))))
       ((EQCAR NR 'TIMES)
        (SETQ NR
                (PROG (FCT FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FCT NR)
                  (COND ((NULL FCT) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FCT)
                                      (COND
                                       ((EQCAR FCT 'QUOTIENT)
                                        (PROGN
                                         (SETQ DM2 (CADDR FCT))
                                         (CADR FCT)))
                                       (T FCT)))
                                    (CAR FCT))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FCT (CDR FCT))
                  (COND ((NULL FCT) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (FCT)
                              (COND
                               ((EQCAR FCT 'QUOTIENT)
                                (PROGN (SETQ DM2 (CADDR FCT)) (CADR FCT)))
                               (T FCT)))
                            (CAR FCT))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       ((EQCAR DM 'QUOTIENT) (PROGN (SETQ NR2 (CADDR DM)) (SETQ DM (CADR DM))))
       ((EQCAR DM 'TIMES)
        (SETQ DM
                (PROG (FCT FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FCT DM)
                  (COND ((NULL FCT) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FCT)
                                      (COND
                                       ((EQCAR FCT 'QUOTIENT)
                                        (PROGN
                                         (SETQ NR2 (CADDR FCT))
                                         (CADR FCT)))
                                       (T FCT)))
                                    (CAR FCT))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FCT (CDR FCT))
                  (COND ((NULL FCT) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (FCT)
                              (COND
                               ((EQCAR FCT 'QUOTIENT)
                                (PROGN (SETQ NR2 (CADDR FCT)) (CADR FCT)))
                               (T FCT)))
                            (CAR FCT))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       (DM2
        (SETQ DM
                (APPEND (LIST 'TIMES DM2)
                        (COND ((EQCAR DM 'TIMES) (CDR DM)) (T (LIST DM)))))))
      (COND
       (NR2
        (SETQ NR
                (APPEND (LIST 'TIMES NR2)
                        (COND ((EQCAR NR 'TIMES) (CDR NR)) (T (LIST NR)))))))
      (RETURN (LIST 'QUOTIENT NR DM)))) 
(ENDMODULE) 
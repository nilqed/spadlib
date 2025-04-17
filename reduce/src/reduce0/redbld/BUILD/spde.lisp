(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPDE)) 
(CREATE-PACKAGE '(SPDE) '(CONTRIB SPDE)) 
(AEVAL (OPERATOR (LIST 'X 'U 'XI 'ETA 'C 'XI* 'ETA*))) 
(AEVAL (OPERATOR (LIST 'DEQ 'DX 'DU 'GL 'GEN 'SDER 'RULE))) 
(FLUID '(DEPL*)) 
(FLUID '(PCLASS MM NN NUM-CGEN NUM-DGEN)) 
(SHARE (LIST 'PCLASS 'MM 'NN)) 
(SETQ PCLASS
        (PROGN
         (SETQ ALGLIST* (CONS NIL NIL))
         (SETQ MM
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (SETQ NN
                          (PROGN
                           (SETQ ALGLIST* (CONS NIL NIL))
                           (SETQ NUM-CGEN (SETQ NUM-DGEN 0)))))))) 
(FLAG '(SIMPSYS RESULT PRSYS PRSYS*) 'OPFN) 
(FLUID '(*LIST KORD*)) 
(FLUID '(UHF DFSUB CSUB CZERO RDEP *RATIONAL)) 
(FLUID '(LIST-M LIST-DEQ LIST-PQ)) 
(DE PRLOAD NIL NIL) 
(PUT 'PRLOAD 'NUMBER-OF-ARGS 0) 
(PUT 'PRLOAD 'DEFINED-ON-LINE '68) 
(PUT 'PRLOAD 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRLOAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'PRLOAD 'INLINE '(LAMBDA () NIL)) 
(REMFLAG '(ORDP ORDPA) 'LOSE) 
(PUT 'ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'ORDP 'DEFINED-ON-LINE '76) 
(PUT 'ORDP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDP (U V)
    (COND ((NULL U) (NULL V)) ((NULL V) T)
          ((OR (EQ U 'DF) (AND (EQ U 'ETA) (NOT (EQ V 'DF)))
               (AND (EQ U 'XI) (NOT (OR (EQ V 'DF) (EQ V 'ETA))))
               (AND (EQ U 'C) (NOT (OR (EQ V 'DF) (EQ V 'ETA) (EQ V 'XI)))))
           T)
          ((OR (AND (EQ U 'ETA) (EQ V 'DF))
               (AND (EQ U 'XI) (OR (EQ V 'DF) (EQ V 'ETA)))
               (AND (EQ U 'C) (OR (EQ V 'DF) (EQ V 'ETA) (EQ V 'XI)))
               (EQ V 'DF) (EQ V 'ETA) (EQ V 'XI) (EQ V 'C))
           NIL)
          ((ATOM U)
           (COND
            ((ATOM V)
             (COND ((NUMBERP U) (AND (NUMBERP V) (NOT (LESSP U V))))
                   ((NUMBERP V) T) (T (ORDERP U V))))
            (T NIL)))
          ((ATOM V) T) ((EQUAL (CAR U) (CAR V)) (ORDP (CDR U) (CDR V)))
          (T (ORDP (CAR U) (CAR V))))) 
(PUT 'ORDPA 'NUMBER-OF-ARGS 2) 
(PUT 'ORDPA 'DEFINED-ON-LINE '93) 
(PUT 'ORDPA 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'ORDPA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDPA (U V) (ORDP U V)) 
(PUT 'MAKESET 'NUMBER-OF-ARGS 1) 
(PUT 'MAKESET 'DEFINED-ON-LINE '95) 
(PUT 'MAKESET 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MAKESET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKESET (U)
    (COND ((NOT U) NIL) ((MEMBER (CAR U) (CDR U)) (MAKESET (CDR U)))
          (T (CONS (CAR U) (MAKESET (CDR U)))))) 
(PUT 'LASTMEM 'NUMBER-OF-ARGS 1) 
(PUT 'LASTMEM 'DEFINED-ON-LINE '100) 
(PUT 'LASTMEM 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LASTMEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LASTMEM (U) (COND ((CDR U) (LASTMEM (CDR U))) (T (CAR U)))) 
(DE XMEMBER (U V) (REVERSE (MEMBER U (REVERSE V)))) 
(PUT 'XMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'XMEMBER 'DEFINED-ON-LINE '103) 
(PUT 'XMEMBER 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'XMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'XMEMBER 'INLINE '(LAMBDA (U V) (REVERSE (MEMBER U (REVERSE V))))) 
(PUT 'SACAR 'NUMBER-OF-ARGS 2) 
(PUT 'SACAR 'DEFINED-ON-LINE '105) 
(PUT 'SACAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SACAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SACAR (A U)
    (COND ((ATOM U) NIL) ((AND (EQ A (CAR U)) (CDR U)) (LIST U))
          (T (APPEND (SACAR A (CAR U)) (SACAR A (CDR U)))))) 
(PUT 'SCAR 'NUMBER-OF-ARGS 2) 
(PUT 'SCAR 'DEFINED-ON-LINE '110) 
(PUT 'SCAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SCAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SCAR (A U)
    (COND ((ATOM U) NIL) ((EQUAL A (CAR U)) U)
          (T (OR (SCAR A (CAR U)) (SCAR A (CDR U)))))) 
(PUT 'INTER 'NUMBER-OF-ARGS 2) 
(PUT 'INTER 'DEFINED-ON-LINE '114) 
(PUT 'INTER 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'INTER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTER (U V)
    (COND ((NOT U) NIL) ((MEMBER (CAR U) V) (CONS (CAR U) (INTER (CDR U) V)))
          (T (INTER (CDR U) V)))) 
(PUT 'COMPL 'NUMBER-OF-ARGS 2) 
(PUT 'COMPL 'DEFINED-ON-LINE '119) 
(PUT 'COMPL 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'COMPL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPL (U V)
    (COND ((NOT U) NIL) ((MEMBER (CAR U) V) (COMPL (CDR U) V))
          (T (CONS (CAR U) (COMPL (CDR U) V))))) 
(PUT 'VLIST 'NUMBER-OF-ARGS 1) 
(PUT 'VLIST 'DEFINED-ON-LINE '123) 
(PUT 'VLIST 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'VLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VLIST (U)
    (COND ((NOT U) NIL) ((NUMBERP (CAR U)) (VLIST (CDR U)))
          (T (CONS (CAR U) (VLIST (CDR U)))))) 
(PUT 'DELNIL 'NUMBER-OF-ARGS 1) 
(PUT 'DELNIL 'DEFINED-ON-LINE '128) 
(PUT 'DELNIL 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'DELNIL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELNIL (U)
    (COND ((NOT U) NIL) ((CAR U) (CONS (CAR U) (DELNIL (CDR U))))
          (T (DELNIL (CDR U))))) 
(PUT 'PRLIST 'NUMBER-OF-ARGS 1) 
(PUT 'PRLIST 'DEFINED-ON-LINE '133) 
(PUT 'PRLIST 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRLIST (U)
    (COND ((NOT U) NIL) ((PAIRP (CAR U)) (CONS (CAR U) (PRLIST (CDR U))))
          (T (PRLIST (CDR U))))) 
(DE APPENDS (U V W) (APPEND U (APPEND V W))) 
(PUT 'APPENDS 'NUMBER-OF-ARGS 3) 
(PUT 'APPENDS 'DEFINED-ON-LINE '138) 
(PUT 'APPENDS 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'APPENDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'APPENDS 'INLINE '(LAMBDA (U V W) (APPEND U (APPEND V W)))) 
(PUT 'PROPA 'NUMBER-OF-ARGS 2) 
(PUT 'PROPA 'DEFINED-ON-LINE '140) 
(PUT 'PROPA 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PROPA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROPA (FN U)
    (PROG (IND)
      (SETQ IND T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND IND U)) (RETURN NIL)))
        (PROGN (SETQ IND (APPLY1 FN (CAR U))) (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN IND))) 
(PUT 'SORTX 'NUMBER-OF-ARGS 2) 
(PUT 'SORTX 'DEFINED-ON-LINE '149) 
(PUT 'SORTX 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SORTX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORTX (FN U)
    (PROG (V W)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ V (MAXMEM FN U))
         (SETQ U (DELETE V U))
         (SETQ W (CONS V W)))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'MAXMEM 'NUMBER-OF-ARGS 2) 
(PUT 'MAXMEM 'DEFINED-ON-LINE '155) 
(PUT 'MAXMEM 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MAXMEM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXMEM (FN U)
    (PROG (V)
      (SETQ V (CAR U))
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((GREATERP (APPLY1 FN X) (APPLY1 FN V)) (SETQ V X))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN V))) 
(PUT 'MAXL 'NUMBER-OF-ARGS 1) 
(PUT 'MAXL 'DEFINED-ON-LINE '165) 
(PUT 'MAXL 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MAXL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAXL (U) (COND ((NOT U) (MINUS 10000)) (T (MAX (CAR U) (MAXL (CDR U)))))) 
(PUT 'SUML 'NUMBER-OF-ARGS 1) 
(PUT 'SUML 'DEFINED-ON-LINE '169) 
(PUT 'SUML 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SUML 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUML (U) (COND ((NOT U) 0) (T (PLUS2 (CAR U) (SUML (CDR U)))))) 
(PUT 'SPDE-SUBSETP 'NUMBER-OF-ARGS 2) 
(PUT 'SPDE-SUBSETP 'DEFINED-ON-LINE '173) 
(PUT 'SPDE-SUBSETP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SPDE-SUBSETP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPDE-SUBSETP (U V)
    (COND ((NOT U) T) (T (AND (MEMBER (CAR U) V) (SPDE-SUBSETP (CDR U) V))))) 
(PUT 'PRODUCT-SET2 'NUMBER-OF-ARGS 2) 
(PUT 'PRODUCT-SET2 'DEFINED-ON-LINE '179) 
(PUT 'PRODUCT-SET2 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRODUCT-SET2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRODUCT-SET2 (U V)
    (PROG (W)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (Y)
             (SETQ Y V)
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y) (SETQ W (CONS (LIST X Y) W))) (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN W))) 
(DE LEQGRT (L I J) (OR (AND (LEQ I J) (EQN L I)) (GEQ I (ADD1 J)))) 
(PUT 'LEQGRT 'NUMBER-OF-ARGS 3) 
(PUT 'LEQGRT 'DEFINED-ON-LINE '187) 
(PUT 'LEQGRT 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LEQGRT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'LEQGRT 'INLINE
      '(LAMBDA (L I J) (OR (AND (LEQ I J) (EQN L I)) (GEQ I (ADD1 J))))) 
(DE FIDEP (U) (AND (ASSOC U DEPL*) (CDR (ASSOC U DEPL*)))) 
(PUT 'FIDEP 'NUMBER-OF-ARGS 1) 
(PUT 'FIDEP 'DEFINED-ON-LINE '190) 
(PUT 'FIDEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'FIDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FIDEP 'INLINE '(LAMBDA (U) (AND (ASSOC U DEPL*) (CDR (ASSOC U DEPL*))))) 
(PUT 'MKDEP 'NUMBER-OF-ARGS 1) 
(PUT 'MKDEP 'DEFINED-ON-LINE '193) 
(PUT 'MKDEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MKDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKDEP (U)
    (PROG (X)
      (SETQ X (CDR U))
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X) (DEPEND1 (CAR U) X T)) (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'RMDEP 'NUMBER-OF-ARGS 1) 
(PUT 'RMDEP 'DEFINED-ON-LINE '196) 
(PUT 'RMDEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RMDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMDEP (U)
    (PROGN
     (RMSUBS)
     (PROG (X)
       (SETQ X (CDR U))
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (DEPEND1 (CAR U) X NIL)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB)))) 
(PUT 'BLANKS 'NUMBER-OF-ARGS 1) 
(PUT 'BLANKS 'DEFINED-ON-LINE '199) 
(PUT 'BLANKS 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'BLANKS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BLANKS (L)
    (PROG (U)
      (SETQ U '(|"|))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L K)) (RETURN NIL)))
        (SETQ U (CONS BLANK U))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (RETURN (COMPRESS (CONS '|"| U))))) 
(DE TERPRI2 NIL (PROGN (TERPRI) (TERPRI))) 
(PUT 'TERPRI2 'NUMBER-OF-ARGS 0) 
(PUT 'TERPRI2 'DEFINED-ON-LINE '206) 
(PUT 'TERPRI2 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'TERPRI2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'TERPRI2 'INLINE '(LAMBDA () (PROGN (TERPRI) (TERPRI)))) 
(DE LCF (U) (AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (CDAR U))) 
(PUT 'LCF 'NUMBER-OF-ARGS 1) 
(PUT 'LCF 'DEFINED-ON-LINE '212) 
(PUT 'LCF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LCF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LCF 'INLINE
      '(LAMBDA (U) (AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (CDAR U)))) 
(DE MINUS-F (U) (MINUSF (CAR (SIMP (REVAL1 U T))))) 
(PUT 'MINUS-F 'NUMBER-OF-ARGS 1) 
(PUT 'MINUS-F 'DEFINED-ON-LINE '214) 
(PUT 'MINUS-F 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MINUS-F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MINUS-F 'INLINE '(LAMBDA (U) (MINUSF (CAR (SIMP (REVAL1 U T)))))) 
(FLAG '(MINUS-F) 'OPFN) 
(PUT 'LENGTHN 'NUMBER-OF-ARGS 1) 
(PUT 'LENGTHN 'DEFINED-ON-LINE '220) 
(PUT 'LENGTHN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LENGTHN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LENGTHN (U)
    (COND ((NOT U) 0)
          ((NUMBERP (CAR U)) (PLUS (SUB1 (CAR U)) (LENGTHN (CDR U))))
          (T (PLUS 1 (LENGTHN (CDR U)))))) 
(PUT 'LENGTHF 'NUMBER-OF-ARGS 1) 
(PUT 'LENGTHF 'DEFINED-ON-LINE '225) 
(PUT 'LENGTHF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LENGTHF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LENGTHF (U)
    (COND ((NOT U) 0) ((ATOM U) (FLATSIZEC U))
          ((EQCAR U 'PLUS)
           (PLUS (TIMES 3 (SUB1 (LENGTH (CDR U)))) (LENGTHF (CDR U))))
          ((OR (EQCAR U 'TIMES) (EQCAR U 'MINUS))
           (PLUS (SUB1 (LENGTH (CDR U))) (LENGTHF (CDR U))))
          ((EQCAR U 'QUOTIENT)
           (COND
            (*RATIONAL
             (ADD1 (ADD1 (MAX (FLATSIZEC (CADR U)) (FLATSIZEC (CADDR U))))))
            (T (ADD1 (PLUS (FLATSIZEC (CADR U)) (FLATSIZEC (CADDR U)))))))
          ((EQCAR U 'EXPT) (ADD1 (FLATSIZEC (CADR U))))
          ((OR (EQCAR U 'DX) (EQCAR U 'DU)) (PLUS (FLATSIZEC (CADR U)) 4))
          ((OR (EQCAR U 'XI) (EQCAR U 'ETA) (EQCAR U 'C) (EQCAR U 'X)
               (EQCAR U 'U))
           (TIMES 2 (LENGTH U)))
          ((EQCAR U 'DF) (PLUS 4 (LENGTHF (CADR U)) (LENGTHF (CDDR U))))
          (T (PLUS (LENGTHF (CAR U)) (LENGTHF (CDR U)))))) 
(FLAG '(LENGTHF) 'OPFN) 
(DE DIFORD (U) (LENGTHN (CDDR U))) 
(PUT 'DIFORD 'NUMBER-OF-ARGS 1) 
(PUT 'DIFORD 'DEFINED-ON-LINE '245) 
(PUT 'DIFORD 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'DIFORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DIFORD 'INLINE '(LAMBDA (U) (LENGTHN (CDDR U)))) 
(PUT 'ADIFF 'NUMBER-OF-ARGS 2) 
(PUT 'ADIFF 'DEFINED-ON-LINE '247) 
(PUT 'ADIFF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'ADIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADIFF (U V)
    (COND ((NOT (MEMBER V U)) U)
          ((AND (EQUAL (LENGTH U) 3) (MEMBER V U)) (CADR U))
          ((OR (NOT (CDR (MEMBER V U))) (NOT (NUMBERP (CADR (MEMBER V U)))))
           (DELETE V U))
          ((EQUAL (CADR (MEMBER V U)) 2)
           (APPEND (REVERSE (MEMBER V (REVERSE U))) (CDDR (MEMBER V U))))
          (T
           (APPEND (REVERSE (MEMBER V (REVERSE U)))
                   (CONS (SUB1 (CADR (MEMBER V U))) (CDDR (MEMBER V U))))))) 
(PUT 'SUB-INT-DF 'NUMBER-OF-ARGS 1) 
(PUT 'SUB-INT-DF 'DEFINED-ON-LINE '258) 
(PUT 'SUB-INT-DF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SUB-INT-DF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUB-INT-DF (U)
    (COND
     ((AND (EQCAR (CADR U) 'DF) (MEMBER (LASTMEM U) (CADR U)))
      (ADIFF (CADR U) (LASTMEM U)))
     (T U))) 
(PUT 'SUBINTF 'NUMBER-OF-ARGS 1) 
(PUT 'SUBINTF 'DEFINED-ON-LINE '264) 
(PUT 'SUBINTF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SUBINTF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBINTF (U)
    (PROG ()
      (PROG (X)
        (SETQ X (MAKESET (SACAR 'INT U)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ U (SUBST (SUB-INT-DF X) X U))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CAR (SIMP (PREPF U)))))) 
(PUT 'MONOP 'NUMBER-OF-ARGS 1) 
(PUT 'MONOP 'DEFINED-ON-LINE '273) 
(PUT 'MONOP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MONOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MONOP (U)
    (OR (OR (ATOM U) (ATOM (CAR U))) (AND (NOT (CDR U)) (MONOP (CDAR U))))) 
(DE SOLVEF (U V) (CAR (SOLVE0 (PREPF U) V))) 
(PUT 'SOLVEF 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEF 'DEFINED-ON-LINE '277) 
(PUT 'SOLVEF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SOLVEF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'SOLVEF 'INLINE '(LAMBDA (U V) (CAR (SOLVE0 (PREPF U) V)))) 
(DE COMFACN (U) (LNC (CKRN U))) 
(PUT 'COMFACN 'NUMBER-OF-ARGS 1) 
(PUT 'COMFACN 'DEFINED-ON-LINE '279) 
(PUT 'COMFACN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'COMFACN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'COMFACN 'INLINE '(LAMBDA (U) (LNC (CKRN U)))) 
(DE REMFACN (U) ((LAMBDA (*EXP) (QUOTF1 U (LNC (CKRN U)))) T)) 
(PUT 'REMFACN 'NUMBER-OF-ARGS 1) 
(PUT 'REMFACN 'DEFINED-ON-LINE '281) 
(PUT 'REMFACN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'REMFACN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'REMFACN 'INLINE
      '(LAMBDA (U) ((LAMBDA (*EXP) (QUOTF1 U (LNC (CKRN U)))) T))) 
(PUT 'LDF-MVAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-MVAR 'DEFINED-ON-LINE '288) 
(PUT 'LDF-MVAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-MVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-MVAR (U) ((LAMBDA (X) (COND ((EQCAR X 'DF) (CADR X)) (T X))) (CAAAR U))) 
(PUT 'LDF-FVAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-FVAR 'DEFINED-ON-LINE '292) 
(PUT 'LDF-FVAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-FVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-FVAR (U)
    (MAKESET
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LDT-TVAR X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (LDT-TVAR X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LDF-FVAR-PART 'NUMBER-OF-ARGS 2) 
(PUT 'LDF-FVAR-PART 'DEFINED-ON-LINE '296) 
(PUT 'LDF-FVAR-PART 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-FVAR-PART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDF-FVAR-PART (U V)
    (PROG (W)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((EQ (LDT-TVAR X) V) (SETQ W (CONS X W))))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (REVERSE W)))) 
(PUT 'LDF-DEP-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-DEP-VAR 'DEFINED-ON-LINE '304) 
(PUT 'LDF-DEP-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-DEP-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-DEP-VAR (U)
    (PROG (V)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((ASSOC (LDT-TVAR X) DEPL*)
             (SETQ V (APPEND (CDR (ASSOC (LDT-TVAR X) DEPL*)) V)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (MAKESET V)))) 
(PUT 'LDF-POW-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-POW-VAR 'DEFINED-ON-LINE '313) 
(PUT 'LDF-POW-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-POW-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-POW-VAR (U)
    (PROG (V Z)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ V (APPEND V (KERNELS (CDR X))))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (Y)
        (SETQ Y (PRLIST (MAKESET V)))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (COND ((OR (EQCAR Y 'X) (EQCAR Y 'U)) (SETQ Z (CONS Y Z)))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (MAKESET Z)))) 
(PUT 'LDF-DEG 'NUMBER-OF-ARGS 2) 
(PUT 'LDF-DEG 'DEFINED-ON-LINE '322) 
(PUT 'LDF-DEG 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-DEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDF-DEG (U V)
    (MAXL
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (DEGREEF (CDR X) V)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (DEGREEF (CDR X) V)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LDF-SPF-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SPF-VAR 'DEFINED-ON-LINE '326) 
(PUT 'LDF-SPF-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SPF-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SPF-VAR (U)
    (PROG (V Z)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ V (APPEND V (KERNELS (CDR X))))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (Y)
        (SETQ Y (PRLIST (MAKESET V)))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (COND
            ((AND (NOT (EQCAR Y 'X)) (NOT (EQCAR Y 'U)))
             (SETQ Z
                     (APPEND (SACAR 'X (CDR Y))
                             (APPEND (SACAR 'U (CDR Y)) Z))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (MAKESET Z)))) 
(PUT 'LDF-ALL-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-ALL-VAR 'DEFINED-ON-LINE '337) 
(PUT 'LDF-ALL-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-ALL-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-ALL-VAR (U)
    (MAKESET (APPEND (LDF-DEP-VAR U) (APPEND (LDF-POW-VAR U) (LDF-SPF-VAR U))))) 
(PUT 'LDF-SEP-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SEP-VAR 'DEFINED-ON-LINE '341) 
(PUT 'LDF-SEP-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SEP-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SEP-VAR (U)
    (COMPL (COMPL (LDF-POW-VAR U) (LDF-DEP-VAR U)) (LDF-SPF-VAR U))) 
(PUT 'LDF-INT-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-INT-VAR 'DEFINED-ON-LINE '345) 
(PUT 'LDF-INT-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-INT-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-INT-VAR (U)
    (COND
     ((EQCAR (CAAAR U) 'DF)
      (PROG (V)
        (SETQ V (LDF-ALL-VAR U))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND V U)) (RETURN NIL)))
          (PROGN
           (SETQ V (COMPL V (COMPL (LDT-DEP (CAR U)) (LDT-DFVAR (CAR U)))))
           (SETQ U (CDR U)))
          (GO WHILELABEL))
        (RETURN V))))) 
(PUT 'LDF-INT 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-INT 'DEFINED-ON-LINE '355) 
(PUT 'LDF-INT 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-INT (U)
    (PROG (V W Z TEST NFUN)
      (SETQ NFUN 0)
     A
      (SETQ TEST NIL)
      (SETQ W (LDF-INT-VAR U))
      (SETQ NFUN (FIND-NFUN))
      (PROG (X)
        (SETQ X W)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((OR
              (NOT
               (SMEMBER 'INT (SETQ Z (CAADR (AEVAL (LIST 'INT (PREPF U) X))))))
              (NOT (SMEMBER 'INT (SETQ Z (SUBINTF Z)))))
             (PROGN
              (SETQ V (*A2K (LIST 'C (SETQ NFUN (PLUS NFUN 1)))))
              (SETQ TEST T)
              (MKDEP (CONS V (DELETE X (LDF-ALL-VAR U))))
              (SETQ U (ADDF Z (LIST (CONS (CONS V 1) 1))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND (TEST (GO A)))
      (RETURN U))) 
(PUT 'LDF-DF-DIFF 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-DF-DIFF 'DEFINED-ON-LINE '373) 
(PUT 'LDF-DF-DIFF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-DF-DIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-DF-DIFF (U)
    (PROG (DFVAR DFSUB V W Z0 Z N0 NMAX)
      (SETQ N0 0)
      (SETQ NMAX 0)
      (SETQ V (COMPL (LDF-DEP-VAR U) (LDF-SPF-VAR U)))
      (COND ((NOT V) (RETURN NIL)))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X V)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (LIST X (ADD1 (LDF-DEG U X))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (LIST X (ADD1 (LDF-DEG U X)))) (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NMAX
              (MAXL
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X W)
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LEQ (SETQ N0 (PLUS N0 1)) NMAX) (NOT (SETQ Z0 NIL))))
          (RETURN NIL)))
        (PROGN
         (PROG (X)
           (SETQ X W)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X) (COND ((GEQ (CADR X) N0) (SETQ Z0 (CONS (CAR X) Z0)))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (SETQ Z (CONS Z0 Z)))
        (GO WHILELABEL))
      (SETQ Z (REVERSE Z))
      (SETQ DFVAR
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CAR Z))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (X)
        (SETQ X (CDR Z))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ DFVAR
                   (APPEND DFVAR
                           (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Y DFVAR)
                             (COND ((NULL Y) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (CAR (PRODUCT-SET2 X Y)))
                                               (CAR Y))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Y (CDR Y))
                             (COND ((NULL Y) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Y) (CAR (PRODUCT-SET2 X Y)))
                                       (CAR Y))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X DFVAR)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (P Q)
             (SETQ P X)
             (SETQ Q U)
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P Q (CDR Q))) (RETURN NIL)))
               (PROGN
                (SETQ Q (LDF-SIMP (CAR (DIFFF Q (CAR P)))))
                (SETQ P (CDR P)))
               (GO WHILELABEL))
             (COND
              ((AND (PAIRP Q) (NOT (CDR Q)) (EQCAR (CAAAR Q) 'DF))
               (SETQ DFSUB (CONS (CAAAR Q) DFSUB))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (MAKESET DFSUB)))) 
(PUT 'LDF-SUB-VAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SUB-VAR 'DEFINED-ON-LINE '399) 
(PUT 'LDF-SUB-VAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SUB-VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SUB-VAR (U)
    (PROG (V W Z)
      (SETQ W (LDF-ALL-VAR U))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((AND (NOT V) (NOT (EQCAR (SETQ Z (CAAR X)) 'DF)) (MONOP (CDR X))
                  (SPDE-SUBSETP W (LDT-DEP X)) (NOT (SMEMBER Z (DELETE X U))))
             (SETQ V Z))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN V))) 
(PUT 'LDF-SIMP 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SIMP 'DEFINED-ON-LINE '409) 
(PUT 'LDF-SIMP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SIMP (U)
    (COND ((NOT U) NIL)
          ((NOT (CDR U))
           (CAR (SIMP (PREPF (LIST (CONS (CONS (CAAAR U) 1) 1))))))
          (T
           (PROG (V)
             (SETQ V (CAR (SIMP (PREPF U))))
             (COND
              ((NOT (OR (ATOM V) (ATOM (CAR V))))
               (SETQ V ((LAMBDA (*EXP) (QUOTF1 V (CDR (COMFAC V)))) T))))
             (RETURN (ABSF V)))))) 
(PUT 'LDF-SEP 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SEP 'DEFINED-ON-LINE '419) 
(PUT 'LDF-SEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SEP (U)
    (PROG (V K)
      (SETQ K 0)
      (COND ((NOT (SETQ V (LDF-SEP-VAR U))) (RETURN (LIST U))))
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ U (SUBST (LIST 'UX 1 (SETQ K (PLUS K 1))) X U)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (COEFF-ALL U 'UX))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (LDF-SIMP (CAR (SIMP (PREPF X)))))
                           (CAR X))
                          NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (LDF-SIMP (CAR (SIMP (PREPF X))))) (CAR X))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'LDF-SUBF0 'NUMBER-OF-ARGS 1) 
(PUT 'LDF-SUBF0 'DEFINED-ON-LINE '428) 
(PUT 'LDF-SUBF0 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDF-SUBF0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDF-SUBF0 (U)
    (LDF-SIMP
     (DELNIL
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X U)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (LDT-SUBT0 X)) (CAR X)) NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (LDT-SUBT0 X)) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'LDT-TVAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDT-TVAR 'DEFINED-ON-LINE '437) 
(PUT 'LDT-TVAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDT-TVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDT-TVAR (U) ((LAMBDA (X) (COND ((EQCAR X 'DF) (CADR X)) (T X))) (CAAR U))) 
(PUT 'LDT-DFVAR 'NUMBER-OF-ARGS 1) 
(PUT 'LDT-DFVAR 'DEFINED-ON-LINE '441) 
(PUT 'LDT-DFVAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDT-DFVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDT-DFVAR (U)
    ((LAMBDA (X) (COND ((EQCAR X 'DF) (VLIST (CDDR X))) (T NIL))) (CAAR U))) 
(PUT 'LDT-DEP 'NUMBER-OF-ARGS 1) 
(PUT 'LDT-DEP 'DEFINED-ON-LINE '445) 
(PUT 'LDT-DEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDT-DEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDT-DEP (U)
    ((LAMBDA (X) (COND (X (CDR X)) (T NIL))) (ASSOC (LDT-TVAR U) DEPL*))) 
(PUT 'LDT-SUBT0 'NUMBER-OF-ARGS 1) 
(PUT 'LDT-SUBT0 'DEFINED-ON-LINE '450) 
(PUT 'LDT-SUBT0 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'LDT-SUBT0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDT-SUBT0 (U) (COND ((NOT (MEMBER (LDT-TVAR U) CZERO)) U) (T NIL))) 
(PUT 'CRESYS 'NUMBER-OF-ARGS 1) 
(PUT 'CRESYS 'DEFINED-ON-LINE '458) 
(PUT 'CRESYS 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'CRESYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CRESYS (U)
    (PROG (R V W LGL LSUB DEPL** LIST-SDER)
      (REMPROP 'DF 'KVALUE)
      (REMKLIST 'DF)
      (REMPROP 'C 'KVALUE)
      (REMKLIST 'C)
     NIL
      (RMSUBS)
      (SETQ DEPL* NIL)
      (COND
       ((CAR U)
        (SETQ LIST-DEQ
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X U)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (ASSOC X (GET (CAR X) 'KVALUE)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X) (ASSOC X (GET (CAR X) 'KVALUE)))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (T (SETQ LIST-DEQ (GET 'DEQ 'KVALUE))))
      (COND
       ((EQN (LENGTH LIST-DEQ) 1)
        (PROG (P)
          (SETQ P (MAXMEM (FUNCTION LENGTH) (MAKESET (SACAR 'U LIST-DEQ))))
          (SETQ P (MK*SQ (CONS (LIST (CONS (CONS P 1) 1)) 1)))
          (SETQ LIST-SDER (LIST (LIST (LIST 'SDER (CADAAR LIST-DEQ)) P)))))
       ((CAR U)
        (SETQ LIST-SDER
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X LIST-DEQ)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (ASSOC (LIST 'SDER (CADAR X))
                                             (GET 'SDER 'KVALUE)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (ASSOC (LIST 'SDER (CADAR X))
                                     (GET 'SDER 'KVALUE)))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (T (SETQ LIST-SDER (GET 'SDER 'KVALUE))))
      (COND
       ((NOT LIST-DEQ) (RERROR 'SPDE 1 "Differential equations not defined")))
      (COND
       ((NOT LIST-SDER)
        (RERROR 'SPDE 2 "Substitutions for derivatives not defined")))
      (SETQ MM (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (FIND-M LIST-DEQ)))
      (SETQ NN (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (FIND-N LIST-DEQ)))
      (SETQ LIST-M
              (MAKESET
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (SACAR 'U LIST-DEQ))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
        (PROGN
         (SETQ W (CONS (*A2K (LIST 'XI K)) W))
         (SETQ V (CONS (*A2K (LIST 'X K)) V)))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
        (COND
         ((MEMBER K LIST-M)
          (PROGN
           (SETQ W (CONS (*A2K (LIST 'ETA K)) W))
           (SETQ V (CONS (*A2K (LIST 'U K)) V)))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
        (SETQ R (CONS (*A2K (LIST 'DX K)) R))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
        (SETQ R (CONS (*A2K (LIST 'DU K)) R))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
        (SETQ DEPL** (CONS (CONS (*A2K (LIST 'ETA K)) V) DEPL**))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
        (SETQ DEPL** (CONS (CONS (*A2K (LIST 'XI K)) V) DEPL**))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ DEPL* DEPL**)
      (SETQ KORD* (REVERSE R))
      (PROG (X)
        (SETQ X LIST-SDER)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ LSUB
                   (CONS
                    (CONS (CAAAR (CAADR (CADR X)))
                          (PREPSQ
                           (CAAR
                            (CAR
                             (SOLVE0
                              (PREPF
                               (CAADR
                                (CADR (ASSOC (LIST 'DEQ (CADAR X)) LIST-DEQ))))
                              (CAAAR (CAADR (CADR X))))))))
                    LSUB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X LIST-DEQ)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (S Z LX LU)
             (SETQ Z (CAADR (CADR X)))
             (SETQ LX (MAKESET (SACAR 'X Z)))
             (SETQ LU (MAKESET (SACAR 'U Z)))
             (PROG (Y)
               (SETQ Y LX)
              LAB
               (COND ((NULL Y) (RETURN NIL)))
               ((LAMBDA (Y)
                  (SETQ S
                          (ADDF S
                                ((LAMBDA (G127 G128)
                                   (COND
                                    (*PHYSOP-LOADED (PHYSOP-MULTF G127 G128))
                                    (T (POLY-MULTF G127 G128))))
                                 (LIST
                                  (CONS (CONS (*A2K (LIST 'XI (CADR Y))) 1) 1))
                                 (CAR (SIMP (PREPSQ (DIFFF Z Y))))))))
                (CAR Y))
               (SETQ Y (CDR Y))
               (GO LAB))
             (PROG (Y)
               (SETQ Y LU)
              LAB
               (COND ((NULL Y) (RETURN NIL)))
               ((LAMBDA (Y)
                  (COND
                   ((EQUAL (LENGTH Y) 2)
                    (SETQ S
                            (ADDF S
                                  ((LAMBDA (G129 G130)
                                     (COND
                                      (*PHYSOP-LOADED (PHYSOP-MULTF G129 G130))
                                      (T (POLY-MULTF G129 G130))))
                                   (LIST
                                    (CONS (CONS (*A2K (LIST 'ETA (CADR Y))) 1)
                                          1))
                                   (CAR (SIMP (PREPSQ (DIFFF Z Y))))))))
                   (T
                    (SETQ S
                            (ADDF S
                                  ((LAMBDA (G131 G132)
                                     (COND
                                      (*PHYSOP-LOADED (PHYSOP-MULTF G131 G132))
                                      (T (POLY-MULTF G131 G132))))
                                   (CAR (ZETA* (CDR Y)))
                                   (CAR (SIMP (PREPSQ (DIFFF Z Y))))))))))
                (CAR Y))
               (SETQ Y (CDR Y))
               (GO LAB))
             (SETQ S (CAR (SUBF S LSUB)))
             (SETQ S (CAR (SUBF S LSUB)))
             (SETQ LGL (APPEND (COEFF-ALL S 'U) LGL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ UHF
              (LIST (MAKESET LGL)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (REVERSE W))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (CONS (LIST (CONS (CONS X 1) 1)) 1))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS (LIST (CONS (CONS X 1) 1)) 1))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))) 
(RLISTAT '(CRESYS)) 
(PUT 'TOTDER 'NUMBER-OF-ARGS 2) 
(PUT 'TOTDER 'DEFINED-ON-LINE '516) 
(PUT 'TOTDER 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'TOTDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTDER (U I)
    (PROG (Z V W)
      (SETQ V (CAR (DIFFF U (*A2K (LIST 'X I)))))
      (SETQ Z (MAKESET (SACAR 'U U)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
        (COND ((MEMBER K LIST-M) (SETQ Z (CONS (*A2K (LIST 'U K)) Z))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (X)
        (SETQ X (MAKESET Z))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ W
                   (ADDF W
                         ((LAMBDA (G133 G134)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G133 G134))
                                  (T (POLY-MULTF G133 G134))))
                          (LIST (CONS (CONS (*A2K (APPEND X (LIST I))) 1) 1))
                          (CAR (DIFFF U X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CAR (SIMP (PREPF (ADDF V W))))))) 
(PUT 'ZETA* 'NUMBER-OF-ARGS 1) 
(PUT 'ZETA* 'DEFINED-ON-LINE '527) 
(PUT 'ZETA* 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'ZETA* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZETA* (U)
    (COND
     ((AND (NOT (GET 'DEQ 'KVALUE)) (OR (EQN MM 0) (EQN NN 0)))
      (RERROR 'SPDE 3 "Number of variables not defined"))
     ((GEQ (LENGTH U) 3)
      (PROG (V W)
       NIL
        (COND
         ((EQN NN 0)
          (SETQ NN (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (FIND-N LIST-DEQ)))))
        (SETQ V
                (TOTDER (CAR (ZETA* (REVERSE (CDR (REVERSE U)))))
                 (CAR (REVERSE U))))
        (PROG (S)
          (SETQ S 1)
         LAB
          (COND ((MINUSP (DIFFERENCE NN S)) (RETURN NIL)))
          (SETQ W
                  (ADDF W
                        ((LAMBDA (G135 G136)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G135 G136))
                                 (T (POLY-MULTF G135 G136))))
                         (LIST
                          (CONS
                           (CONS
                            (*A2K
                             (CONS 'U
                                   (APPEND (REVERSE (CDR (REVERSE U)))
                                           (LIST S))))
                            1)
                           1))
                         (TOTDER (LIST (CONS (CONS (*A2K (LIST 'XI S)) 1) 1))
                          (CAR (REVERSE U))))))
          (SETQ S (PLUS2 S 1))
          (GO LAB))
        (RETURN (SIMP (PREPSQ (CONS (ADDF V (NEGF W)) 1))))))
     (T
      (PROG (V W)
       NIL
        (COND
         ((EQN NN 0)
          (PROGN
           (SETQ NN (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (FIND-N LIST-DEQ)))
           (SETQ MM (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (FIND-M LIST-DEQ)))))
         (T
          (PROG (P Z)
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
              (SETQ Z (CONS K Z))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
              (SETQ P (CONS (*A2K (LIST 'X K)) P))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
              (SETQ P (CONS (*A2K (LIST 'U K)) P))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
              (MKDEP (CONS (*A2K (LIST 'XI K)) P))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
              (MKDEP (CONS (*A2K (LIST 'ETA K)) P))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETQ LIST-M Z))))
        (SETQ V
                (TOTDER (LIST (CONS (CONS (*A2K (LIST 'ETA (CAR U))) 1) 1))
                 (CADR U)))
        (PROG (S)
          (SETQ S 1)
         LAB
          (COND ((MINUSP (DIFFERENCE NN S)) (RETURN NIL)))
          (SETQ W
                  (ADDF W
                        ((LAMBDA (G137 G138)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G137 G138))
                                 (T (POLY-MULTF G137 G138))))
                         (LIST (CONS (CONS (*A2K (LIST 'U (CAR U) S)) 1) 1))
                         (TOTDER (LIST (CONS (CONS (*A2K (LIST 'XI S)) 1) 1))
                          (CAR (REVERSE U))))))
          (SETQ S (PLUS2 S 1))
          (GO LAB))
        (RETURN (SIMP (PREPSQ (CONS (ADDF V (NEGF W)) 1)))))))) 
(PUT 'SIMPU 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPU 'DEFINED-ON-LINE '559) 
(PUT 'SIMPU 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPU (U)
    (CONS
     (LIST
      (CONS
       (GETPOWER (FKERN (CONS 'U (CONS (CAR U) (REVERSE (ORDN (CDR U)))))) 1)
       1))
     1)) 
(PUT 'U 'SIMPFN 'SIMPU) 
(PUT 'ZETA 'SIMPFN 'ZETA*) 
(PUT 'COEFF-ALL 'NUMBER-OF-ARGS 2) 
(PUT 'COEFF-ALL 'DEFINED-ON-LINE '565) 
(PUT 'COEFF-ALL 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'COEFF-ALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFF-ALL (U V)
    (PROG (Z)
      (SETQ LIST-PQ NIL)
      (SPLITREC U V 1 NIL)
      (PROG (X)
        (SETQ X LIST-PQ)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ Z (CONS (LDF-SIMP (CAR (SIMP (PREPF (CDR X))))) Z)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (MAKESET Z)))) 
(PUT 'SPLITREC 'NUMBER-OF-ARGS 4) 
(PUT 'SPLITREC 'DEFINED-ON-LINE '574) 
(PUT 'SPLITREC 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SPLITREC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPLITREC (U V P Q)
    (COND
     ((OR (ATOM U) (ATOM (CAR U)))
      (PROG (Y)
        (SETQ P
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF U P))
                      (T (POLY-MULTF U P))))
        (COND ((SETQ Y (ASSOC Q LIST-PQ)) (RPLACD Y (ADDF (CDR Y) P)))
              (T (SETQ LIST-PQ (CONS (CONS Q P) LIST-PQ))))))
     (T
      (PROG ()
        (COND
         ((AND (EQCAR (CAAAR U) V) (GREATERP (LENGTH (CAAAR U)) 2))
          (SPLITREC (CDAR U) V P (CONS (CAAR U) Q)))
         (T (SPLITREC (CDAR U) V (LIST (CONS (CAAR U) P)) Q)))
        (COND ((CDR U) (SPLITREC (CDR U) V P Q))))))) 
(PUT 'FIND-M 'NUMBER-OF-ARGS 1) 
(PUT 'FIND-M 'DEFINED-ON-LINE '588) 
(PUT 'FIND-M 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'FIND-M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND-M (U)
    (MAXL
     (MAKESET
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (SACAR 'U U))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'FIND-N 'NUMBER-OF-ARGS 1) 
(PUT 'FIND-N 'DEFINED-ON-LINE '591) 
(PUT 'FIND-N 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'FIND-N 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND-N (U)
    (PROG (VX VU WX WU)
      (SETQ VX (MAKESET (SACAR 'X U)))
      (SETQ VU (MAKESET (SACAR 'U U)))
      (PROG (X)
        (SETQ X VX)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ WX (CONS (CADR X) WX))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X VU)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((GEQ (LENGTH X) 3) (SETQ WU (APPEND (CDDR X) WU)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (MAX (MAXL WX) (MAXL WU))))) 
(PUT 'RULE0 'NUMBER-OF-ARGS 0) 
(PUT 'RULE0 'DEFINED-ON-LINE '605) 
(PUT 'RULE0 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULE0 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RULE0 NIL
    (COND
     (UHF
      (PROG (X)
        (SETQ X (CAR UHF))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((AND (NOT (CDR X)) (NOT (EQCAR (CAAAR X) 'DF)))
             (SETQ CZERO (CONS (CAAAR X) CZERO)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))))) 
(PUT 'RULE1 'NUMBER-OF-ARGS 0) 
(PUT 'RULE1 'DEFINED-ON-LINE '611) 
(PUT 'RULE1 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULE1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RULE1 NIL
    (COND
     ((AND UHF (CAR UHF))
      (PROG (DFSUB)
        (PROG (X)
          (SETQ X (CAR UHF))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND
              ((AND (NOT (CDR X)) (EQCAR (CAAAR X) 'DF)
                    (EQN (LENGTHN (CDDR (CAAAR X))) 1))
               (SETQ RDEP (CONS (CAAAR X) RDEP)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (COND (RDEP (RETURN T))))))) 
(PUT 'RULE1-DIFF 'NUMBER-OF-ARGS 0) 
(PUT 'RULE1-DIFF 'DEFINED-ON-LINE '622) 
(PUT 'RULE1-DIFF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULE1-DIFF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RULE1-DIFF NIL
    (COND
     ((AND UHF (CAR UHF))
      (PROG (U V Z)
        (PROG (X)
          (SETQ X (CAR UHF))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X) (COND ((SETQ Z (LDF-DF-DIFF X)) (SETQ U (APPEND Z U)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (PROG (X)
          (SETQ X U)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X) (COND ((EQN (LENGTHN (CDDR X)) 1) (SETQ V (CONS X V)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (SETQ RDEP (MAKESET V))
        (COND (RDEP (RETURN T))))))) 
(PUT 'RULEC 'NUMBER-OF-ARGS 1) 
(PUT 'RULEC 'DEFINED-ON-LINE '635) 
(PUT 'RULEC 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RULEC (L)
    (COND
     ((AND UHF (CAR UHF))
      (PROG (V)
        (PROG (U)
          (SETQ U (CAR UHF))
         LAB
          (COND ((NULL U) (RETURN NIL)))
          ((LAMBDA (U)
             (COND
              ((AND (OR (AND (LEQ L 4) (EQN (LENGTH U) L)) (GEQ L (ADD1 4)))
                    (SETQ V (LDF-SUB-VAR U)) (NOT (SMEMBER V CSUB))
                    (NOT
                     (INTER
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X CSUB)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (X) (CAR X)) (CAR X))
                                              NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      (LDF-FVAR U))))
               (SETQ CSUB
                       (CONS
                        (CONS V (PREPSQ (CAAR (CAR (SOLVE0 (PREPF U) V)))))
                        CSUB)))))
           (CAR U))
          (SETQ U (CDR U))
          (GO LAB))
        (COND (CSUB (RETURN T))))))) 
(PUT 'RULEDF 'NUMBER-OF-ARGS 1) 
(PUT 'RULEDF 'DEFINED-ON-LINE '647) 
(PUT 'RULEDF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULEDF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RULEDF (L)
    (COND
     ((AND UHF (CAR UHF))
      (PROG (DFSUB)
        (PROG (X)
          (SETQ X (CAR UHF))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND
              ((AND (NOT (CDR X)) (EQCAR (CAAAR X) 'DF)
                    (EQN (LENGTHN (CDDR (CAAAR X))) L)
                    (NOT (SMEMBER (LDF-MVAR X) DFSUB)))
               (SETQ DFSUB (CONS (CAAAR X) DFSUB)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (SETQ CSUB
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X DFSUB)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (CONS (CADR X) (CREPOL X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X) (CONS (CADR X) (CREPOL X))) (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (COND (CSUB (RETURN T))))))) 
(PUT 'RULEDF-DIFF 'NUMBER-OF-ARGS 1) 
(PUT 'RULEDF-DIFF 'DEFINED-ON-LINE '660) 
(PUT 'RULEDF-DIFF 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULEDF-DIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RULEDF-DIFF (L)
    (COND
     ((AND UHF (CAR UHF))
      (PROG (V DFSUB)
        (PROG (U)
          (SETQ U (CAR UHF))
         LAB
          (COND ((NULL U) (RETURN NIL)))
          ((LAMBDA (U) (SETQ V (APPEND V (LDF-DF-DIFF U)))) (CAR U))
          (SETQ U (CDR U))
          (GO LAB))
        (COND ((NOT (SETQ V (MAKESET V))) (RETURN NIL)))
        (PROG (X)
          (SETQ X V)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND ((EQN (LENGTHN (CDDR X)) L) (SETQ DFSUB (CONS X DFSUB)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (COND ((NOT DFSUB) (RETURN NIL)))
        (SETQ CSUB (CONS (CONS (CADAR DFSUB) (CREPOL (CAR DFSUB))) CSUB))
        (COND (CSUB (RETURN T))))))) 
(PUT 'RULE-INT 'NUMBER-OF-ARGS 1) 
(PUT 'RULE-INT 'DEFINED-ON-LINE '675) 
(PUT 'RULE-INT 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RULE-INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RULE-INT (L)
    (COND
     ((AND UHF (CAR UHF))
      (PROG (V W)
        (PROG (U)
          (SETQ U (CAR UHF))
         LAB
          (COND ((NULL U) (RETURN NIL)))
          ((LAMBDA (U)
             (COND
              ((AND (NOT CSUB)
                    (OR (AND (LEQ L 4) (EQN (LENGTH U) L)) (GEQ L (ADD1 4)))
                    (SETQ V (LDF-SUB-VAR (SETQ W (LDF-INT U)))))
               (SETQ CSUB
                       (LIST
                        (CONS V
                              (PREPSQ (CAAR (CAR (SOLVE0 (PREPF W) V))))))))))
           (CAR U))
          (SETQ U (CDR U))
          (GO LAB))
        (COND (CSUB (RETURN T))))))) 
(PUT 'SIMPSYS0 'NUMBER-OF-ARGS 0) 
(PUT 'SIMPSYS0 'DEFINED-ON-LINE '687) 
(PUT 'SIMPSYS0 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPSYS0 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SIMPSYS0 NIL
    (PROG (U V)
      (COND
       ((EQUAL PCLASS 2)
        (PROGN
         (PROGN (PRIN2 "Entering SIMPSYS0") NIL)
         (PROGN (TERPRI) (TERPRI)))))
      (SETQ U
              (DELNIL
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CAR UHF))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (LDF-SUBF0 X)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (LDF-SUBF0 X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ V
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CADR UHF))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (CONS (LDF-SUBF0 (CAR X)) (CDR X)))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (CONS (LDF-SUBF0 (CAR X)) (CDR X)))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ UHF (LIST (MAKESET U) V))
      (COND
       ((EQUAL PCLASS 1)
        (PROG ()
          (PROGN (TERPRI) (TERPRI))
          (COND ((EQN (LENGTH CZERO) 1) (PROGN (PRIN2 "Substitution") NIL))
                (T (PROGN (PRIN2 "Substitutions") NIL)))
          (TERPRI)
          (PROG (X)
            (SETQ X CZERO)
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (PROGN
                (ASSGNPRI (REVAL1 X NIL) NIL 'FIRST)
                (ASSGNPRI (AEVAL ":=0") NIL 'LAST)))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (TERPRI))))
      (COND
       ((EQUAL PCLASS 2)
        (PROGN (PROGN (PRIN2 "CZERO:=") NIL) (PRETTYPRINT CZERO) (TERPRI))))
      (SETQ CZERO NIL)
      (COND
       ((EQUAL PCLASS 2)
        (PROGN
         (PROGN (PRIN2 "Leaving SIMPSYS0") NIL)
         (PROGN (TERPRI) (TERPRI))))))) 
(PUT 'SIMPSYS-RDEP 'NUMBER-OF-ARGS 0) 
(PUT 'SIMPSYS-RDEP 'DEFINED-ON-LINE '709) 
(PUT 'SIMPSYS-RDEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPSYS-RDEP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SIMPSYS-RDEP NIL
    (PROG (U V)
      (COND
       ((EQUAL PCLASS 2)
        (PROGN
         (PROGN (PRIN2 "Entering SIMPSYS!-RDEP") NIL)
         (PROGN (TERPRI) (TERPRI)))))
      (PROG (X)
        (SETQ X RDEP)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (RMDEP (CDR X))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ U
              (MAKESET
               (DELNIL
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CAR UHF))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (LDF-SIMP X)) (CAR X))
                                        NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (LDF-SIMP X)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ V
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CADR UHF))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (SIMP (PREPSQ X))) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (SIMP (PREPSQ X))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ UHF (LIST U V))
      (COND
       ((EQUAL PCLASS 1)
        (PROG ()
          (TERPRI)
          (PROGN (PRIN2 "Dependencies removed") NIL)
          (PROGN (TERPRI) (TERPRI))
          (PROG (X)
            (SETQ X RDEP)
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (PROGN
                (MAPRIN (CADR X))
                (PRIN2* " independent of ")
                (MAPRIN (CADDR X))
                (TERPRI* T)
                NIL))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (TERPRI))))
      (COND
       ((EQUAL PCLASS 2)
        (PROGN (PROGN (PRIN2 "RDEP:='") NIL) (PRETTYPRINT RDEP) (TERPRI))))
      (COND
       ((EQUAL PCLASS 2)
        (PROGN
         (PROGN (PRIN2 "Leaving SIMPSYS!-RDEP") NIL)
         (PROGN (TERPRI) (TERPRI))))))) 
(PUT 'SIMPSYS-SEP 'NUMBER-OF-ARGS 0) 
(PUT 'SIMPSYS-SEP 'DEFINED-ON-LINE '730) 
(PUT 'SIMPSYS-SEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPSYS-SEP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SIMPSYS-SEP NIL
    (COND
     ((AND UHF (CAR UHF))
      (PROG (U V TEST)
        (COND
         ((EQUAL PCLASS 2)
          (PROGN
           (PROGN (PRIN2 "Entering SIMPSYS!-SEP") NIL)
           (PROGN (TERPRI) (TERPRI)))))
        (PROG (X)
          (SETQ X (CAR UHF))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND ((EQN (LENGTH (SETQ V (LDF-SEP X))) 1) (SETQ U (CONS X U)))
                   (T
                    (PROG ()
                      (SETQ U (APPEND V U))
                      (COND
                       ((OR (EQUAL PCLASS 1) (EQUAL PCLASS 2))
                        (PROG (Z L)
                          (SETQ L 0)
                          (TERPRI)
                          (SETQ L
                                  (PLUS
                                   (DIFFERENCE (LENGTH (CAR UHF))
                                               (LENGTH (MEMBER X (CAR UHF))))
                                   1))
                          (PROGN
                           (PRIN2 "Equation ")
                           (PRIN2 L)
                           (PRIN2 " separated into the terms")
                           NIL)
                          (TERPRI)
                          (COND
                           ((EQUAL PCLASS 1)
                            (PROG (K)
                              (SETQ K 1)
                             LAB
                              (COND
                               ((MINUSP (DIFFERENCE (LENGTH V) K))
                                (RETURN NIL)))
                              (PROG ()
                                (SETQ Z (PREPF (NTH V K)))
                                (SETQ *LIST (GEQ (LENGTHF Z) 50))
                                (PROGN
                                 (ASSGNPRI (AEVAL* "Term   ") NIL 'FIRST)
                                 (ASSGNPRI K NIL NIL)
                                 (ASSGNPRI (AEVAL* "  ") NIL NIL)
                                 (ASSGNPRI (AEVAL* Z) NIL 'LAST)))
                              (SETQ K (PLUS2 K 1))
                              (GO LAB))))
                          (COND
                           ((EQUAL PCLASS 2)
                            (PROG (Y)
                              (SETQ Y V)
                             LAB
                              (COND ((NULL Y) (RETURN NIL)))
                              ((LAMBDA (Y) (PRETTYPRINT Y)) (CAR Y))
                              (SETQ Y (CDR Y))
                              (GO LAB)))))))
                      (SETQ TEST T)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (SETQ *LIST NIL)
        (COND (TEST (SETQ UHF (LIST (REVERSE (MAKESET U)) (CADR UHF)))))
        (COND
         ((EQUAL PCLASS 2)
          (PROGN
           (PROGN (PRIN2 "Leaving SIMPSYS!-SEP") NIL)
           (PROGN (TERPRI) (TERPRI))))))))) 
(PUT 'SIMPSYS-SUB 'NUMBER-OF-ARGS 0) 
(PUT 'SIMPSYS-SUB 'DEFINED-ON-LINE '760) 
(PUT 'SIMPSYS-SUB 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPSYS-SUB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SIMPSYS-SUB NIL
    (COND
     ((AND UHF (CAR UHF))
      (PROG (U V)
        (COND
         ((EQUAL PCLASS 2)
          (PROGN
           (PROGN (PRIN2 "Entering SIMPSYS!-SUB") NIL)
           (PROGN (TERPRI) (TERPRI)))))
        (COND ((EQUAL PCLASS 1) (PRRULE CSUB)))
        (COND
         ((EQUAL PCLASS 2)
          (PROGN (PROGN (PRIN2 "CSUB:='") NIL) (PRETTYPRINT CSUB) (TERPRI))))
        (SETQ U
                (MAKESET
                 (DELNIL
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X (CAR UHF))
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X)
                                        (LDF-SIMP (CAR (SUBF X CSUB))))
                                      (CAR X))
                                     NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X) (LDF-SIMP (CAR (SUBF X CSUB))))
                              (CAR X))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
        (SETQ V
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CADR UHF))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (SUBSQ X CSUB)) (CAR X))
                                        NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (SUBSQ X CSUB)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ UHF (LIST U V))
        (SETQ CSUB NIL)
        (COND
         ((EQUAL PCLASS 2)
          (PROGN
           (PROGN (PRIN2 "Leaving SIMPSYS!-SUB") NIL)
           (PROGN (TERPRI) (TERPRI))))))))) 
(PUT 'SIMPSYS 'NUMBER-OF-ARGS 0) 
(PUT 'SIMPSYS 'DEFINED-ON-LINE '775) 
(PUT 'SIMPSYS 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'SIMPSYS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SIMPSYS NIL
    (COND ((NOT UHF) (RERROR 'SPDE 4 "The determining system is not defined"))
          ((NOT (CAR UHF))
           (RERROR 'SPDE 5 "The determining system completely solved"))
          (T
           (PROG (U V NFUN)
             (SETQ NFUN 0)
            NIL
             (SETQ U
                     (MAKESET
                      (DELNIL
                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                         (SETQ X (CAR UHF))
                         (COND ((NULL X) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X) (LDF-SIMP X)) (CAR X))
                                          NIL)))
                        LOOPLABEL
                         (SETQ X (CDR X))
                         (COND ((NULL X) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (LDF-SIMP X)) (CAR X))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
             (SETQ V
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CADR UHF))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (SIMP (PREPSQ X)))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (SIMP (PREPSQ X))) (CAR X))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ UHF (LIST U V))
            MARK0
             (COND ((EQUAL PCLASS 1) (PROGN (PRSYS* "Entering main loop")))
                   ((EQUAL PCLASS 2) (PRTLIST "Entering main loop")))
             (SETQ CZERO (SETQ CSUB (SETQ RDEP NIL)))
             (SIMPSYS-SEP)
             (RULE0)
             (COND (CZERO (PROGN (SIMPSYS0) (GO MARK0))))
             (COND
              ((OR (RULE1) (RULE1-DIFF)) (PROGN (SIMPSYS-RDEP) (GO MARK0))))
             (COND
              ((OR (RULEDF 2) (RULEC 2) (RULE-INT 2) (RULEDF-DIFF 2) (RULEDF 3)
                   (RULEC 3) (RULE-INT 3) (RULEDF-DIFF 3) (RULEDF 4) (RULEC 4)
                   (RULE-INT 4) (RULEDF-DIFF 4) (RULEDF 5) (RULEC 5)
                   (RULE-INT 5) (RULEDF-DIFF 5))
               (PROGN (SIMPSYS-SUB) (GO MARK0))))
             (COND
              ((CAR UHF)
               (PROGN
                (PROGN
                 (PRIN2 "Determining system is not completely solved")
                 NIL)
                (PROGN (TERPRI) (TERPRI))
                (PRSYS* "The remaining equations are")
                (COND
                 ((NOT (ZEROP (SETQ NFUN (FIND-NFUN))))
                  (PROGN
                   (PRIN2 "Number of functions is ")
                   (PRIN2 NFUN)
                   NIL)))))))))) 
(PUT 'CREPOL 'NUMBER-OF-ARGS 1) 
(PUT 'CREPOL 'DEFINED-ON-LINE '805) 
(PUT 'CREPOL 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'CREPOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CREPOL (U)
    (PROG (L1 F POW NFUN)
      (SETQ POW 0)
      (SETQ NFUN 0)
      (SETQ NFUN (FIND-NFUN))
      (SETQ L1 (CDR (ASSOC (CAR (SETQ U (CDR U))) DEPL*)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ U (CDR U))) (RETURN NIL)))
        (PROG (V)
          (SETQ V (CAR U))
          (COND
           ((OR (EQUAL (LENGTH U) 1) (NOT (NUMBERP (CADR U)))) (SETQ POW 1))
           (T (PROGN (SETQ POW (CADR U)) (SETQ U (DELETE POW U)) NIL)))
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((MINUSP (DIFFERENCE POW K)) (RETURN NIL)))
            (PROG (W)
              (SETQ W (*A2K (LIST 'C (SETQ NFUN (PLUS NFUN 1)))))
              (MKDEP (CONS W (DELETE V L1)))
              (COND ((EQUAL K 1) (SETQ F (CONS W F))))
              (COND ((EQUAL K 2) (SETQ F (CONS (LIST 'TIMES W V) F))))
              (COND
               ((GEQ K 3)
                (SETQ F
                        (CONS (LIST 'TIMES W (LIST 'EXPT V (DIFFERENCE K 1)))
                              F)))))
            (SETQ K (PLUS2 K 1))
            (GO LAB)))
        (GO WHILELABEL))
      (RETURN (APPEND '(PLUS) F)))) 
(PUT 'CPAR 'NUMBER-OF-ARGS 1) 
(PUT 'CPAR 'DEFINED-ON-LINE '831) 
(PUT 'CPAR 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'CPAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CPAR (U)
    (PROG (V)
      (SETQ V
              (MAKESET
               (APPEND (SACAR 'XI U) (APPEND (SACAR 'ETA U) (SACAR 'C U)))))
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((NOT (ASSOC X DEPL*)) (SETQ V (DELETE X V)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN V))) 
(PUT 'MAKESET-C-X 'NUMBER-OF-ARGS 1) 
(PUT 'MAKESET-C-X 'DEFINED-ON-LINE '838) 
(PUT 'MAKESET-C-X 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MAKESET-C-X 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKESET-C-X (U)
    (COND ((NOT U) NIL) ((MEMBER-C-X (CAR U) (CDR U)) (MAKESET-C-X (CDR U)))
          (T (CONS (CAR U) (MAKESET-C-X (CDR U)))))) 
(PUT 'MEMBER-C-X 'NUMBER-OF-ARGS 2) 
(PUT 'MEMBER-C-X 'DEFINED-ON-LINE '843) 
(PUT 'MEMBER-C-X 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'MEMBER-C-X 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MEMBER-C-X (U V)
    (COND ((NOT V) NIL) ((EQUAL-C-X U (CAR V)) V) (T (MEMBER-C-X U (CDR V))))) 
(PUT 'EQUAL-C-X 'NUMBER-OF-ARGS 2) 
(PUT 'EQUAL-C-X 'DEFINED-ON-LINE '847) 
(PUT 'EQUAL-C-X 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'EQUAL-C-X 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUAL-C-X (U V)
    (PROG (P Q)
      (SETQ P (OR (SCAR 'C U) (SCAR 'XI U) (SCAR 'ETA U)))
      (SETQ Q (OR (SCAR 'C V) (SCAR 'XI V) (SCAR 'ETA V)))
      (RETURN (EQUAL (SUBST 'CXX P U) (SUBST 'CXX Q V))))) 
(DE NUMGEN NIL (LENGTH (GET 'GEN 'KVALUE))) 
(PUT 'NUMGEN 'NUMBER-OF-ARGS 0) 
(PUT 'NUMGEN 'DEFINED-ON-LINE '854) 
(PUT 'NUMGEN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'NUMGEN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'NUMGEN 'INLINE '(LAMBDA () (LENGTH (GET 'GEN 'KVALUE)))) 
(FLAG '(NUMGEN) 'OPFN) 
(PUT 'GENGEN 'NUMBER-OF-ARGS 0) 
(PUT 'GENGEN 'DEFINED-ON-LINE '858) 
(PUT 'GENGEN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'GENGEN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENGEN NIL
    (PROG (U Z CGEN DGEN NGEN)
      (SETQ NGEN 0)
      (REMPROP 'GEN 'KVALUE)
      (REMKLIST 'GEN)
      (PROG (X)
        (SETQ X (CADR UHF))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ U (APPEND (LDF-FVAR (CAR X)) U))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X (MAKESET U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (V W)
             (SETQ W (CONS NIL 1))
             (COND
              ((ASSOC X DEPL*)
               (SETQ V
                       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Y (CADR UHF))
                         (COND ((NULL Y) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Y)
                                             (SIMP
                                              (PREPSQ
                                               (CONS (LDF-FVAR-PART (CAR Y) X)
                                                     (CDR Y)))))
                                           (CAR Y))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Y (CDR Y))
                         (COND ((NULL Y) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (Y)
                                     (SIMP
                                      (PREPSQ
                                       (CONS (LDF-FVAR-PART (CAR Y) X)
                                             (CDR Y)))))
                                   (CAR Y))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
              (T
               (SETQ V
                       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Y (CADR UHF))
                         (COND ((NULL Y) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Y)
                                             (SIMP
                                              (PREPSQ
                                               (CONS
                                                ((LAMBDA (U)
                                                   (AND
                                                    (NOT
                                                     (OR (ATOM U)
                                                         (ATOM (CAR U))))
                                                    (CDAR U)))
                                                 (LDF-FVAR-PART (CAR Y) X))
                                                (CDR Y)))))
                                           (CAR Y))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Y (CDR Y))
                         (COND ((NULL Y) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (Y)
                                     (SIMP
                                      (PREPSQ
                                       (CONS
                                        ((LAMBDA (U)
                                           (AND
                                            (NOT (OR (ATOM U) (ATOM (CAR U))))
                                            (CDAR U)))
                                         (LDF-FVAR-PART (CAR Y) X))
                                        (CDR Y)))))
                                   (CAR Y))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
               (COND
                ((CAR (NTH V K))
                 (SETQ W
                         (ADDSQ
                          (MULTSQ (NTH V K)
                                  (CONS
                                   (LIST (CONS (CONS (*A2K (LIST 'DX K)) 1) 1))
                                   1))
                          W))))
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
               (COND
                ((CAR (NTH V (PLUS NN K)))
                 (SETQ W
                         (ADDSQ
                          (MULTSQ (NTH V (PLUS NN K))
                                  (CONS
                                   (LIST (CONS (CONS (*A2K (LIST 'DU K)) 1) 1))
                                   1))
                          W))))
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (COND
              ((ASSOC X DEPL*)
               (SETQ CGEN
                       (CONS
                        (ABSF
                         ((LAMBDA (U)
                            ((LAMBDA (*EXP) (QUOTF1 U (LNC (CKRN U)))) T))
                          (CAR (SIMP (PREPF (CAR W))))))
                        CGEN)))
              (T
               (SETQ DGEN
                       (CONS
                        (ABSF
                         ((LAMBDA (U)
                            ((LAMBDA (*EXP) (QUOTF1 U (LNC (CKRN U)))) T))
                          (CAR (SIMP (PREPF (CAR W))))))
                        DGEN))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ DGEN (MAKESET DGEN))
      (SETQ CGEN (MAKESET-C-X CGEN))
      (SETQ NUM-DGEN (LENGTH DGEN))
      (SETQ NUM-CGEN (LENGTH CGEN))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN K)) (RETURN NIL)))
        (COND
         ((MEMBER (SETQ Z (LIST (CONS (CONS (*A2K (LIST 'DX K)) 1) 1))) DGEN)
          (PROGN
           (SETK (LIST 'GEN (SETQ NGEN (ADD1 NGEN))) (PREPF Z))
           (SETQ DGEN (DELETE Z DGEN)))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MM K)) (RETURN NIL)))
        (COND
         ((MEMBER (SETQ Z (LIST (CONS (CONS (*A2K (LIST 'DU K)) 1) 1))) DGEN)
          (PROGN
           (SETK (LIST 'GEN (SETQ NGEN (ADD1 NGEN))) (PREPF Z))
           (SETQ DGEN (DELETE Z DGEN)))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ DGEN (SORTX (FUNCTION LENGTH) DGEN))
      (PROG (X)
        (SETQ X DGEN)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETK (LIST 'GEN (SETQ NGEN (ADD1 NGEN))) (PREPF X)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ CGEN (SORTX (FUNCTION LENGTH) CGEN))
      (PROG (X)
        (SETQ X CGEN)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETK (LIST 'GEN (SETQ NGEN (ADD1 NGEN))) (PREPF X)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(FLAG '(GENGEN) 'OPFN) 
(PUT 'COMM 'NUMBER-OF-ARGS 2) 
(FLAG '(COMM) 'OPFN) 
(PUT 'COMM 'DEFINED-ON-LINE '892) 
(PUT 'COMM 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'COMM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMM (A B)
    (PROG (Z)
      (COND
       ((EVALEQUAL (AEVAL (LENGTH LIST-DEQ)) 0)
        (PROGN
         (ASSGNPRI (AEVAL "Differential equations not defined") NIL 'ONLY)
         (RETURN (AEVAL 'NIL)))))
      (SETQ Z
              (AEVAL
               (LIST 'PLUS
                     (PROG (K FORALL-RESULT)
                       (SETQ K 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NN) K))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES
                                                   (LIST 'DF A (LIST 'DX K))
                                                   (LIST 'DF B (LIST 'X K)))
                                             (LIST 'TIMES
                                                   (LIST 'DF B (LIST 'DX K))
                                                   (LIST 'DF A (LIST 'X K)))))
                                      FORALL-RESULT)))
                       (SETQ K
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                K))
                       (GO LAB1))
                     (PROG (K FORALL-RESULT)
                       (SETQ K 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MM) K))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES
                                                   (LIST 'DF A (LIST 'DU K))
                                                   (LIST 'DF B (LIST 'U K)))
                                             (LIST 'TIMES
                                                   (LIST 'DF B (LIST 'DU K))
                                                   (LIST 'DF A (LIST 'U K)))))
                                      FORALL-RESULT)))
                       (SETQ K
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                K))
                       (GO LAB1)))))
      (RETURN (AEVAL Z)))) 
(PUT 'RESULT 'NUMBER-OF-ARGS 0) 
(FLAG '(RESULT) 'OPFN) 
(PUT 'RESULT 'DEFINED-ON-LINE '901) 
(PUT 'RESULT 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'RESULT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESULT NIL
    (PROG (L)
      (SETQ L 0)
      (COND
       ((EVALEQUAL (SETQ L (AEVAL (LENGTH LIST-DEQ))) 1)
        (ASSGNPRI (AEVAL "The differential equation") NIL 'ONLY))
       (T (ASSGNPRI (AEVAL "The differential equations") NIL 'ONLY)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L J)) (RETURN NIL)))
        (PROG (Z I K)
          (SETQ I 0)
          (SETQ K 0)
          (SETQ Z (CAR (CADADR (NTH LIST-DEQ J))))
          (SETQ I (AEVAL* (CADAR (NTH LIST-DEQ J))))
          (SETQ K (AEVAL* (LENGTHF (PREPF Z))))
          (SETQ *LIST (GREATERP K 40))
          (PROGN
           (ASSGNPRI (AEVAL* "DEQ(") NIL 'FIRST)
           (ASSGNPRI I NIL NIL)
           (ASSGNPRI (AEVAL* "):=") NIL NIL)
           (ASSGNPRI (AEVAL* (PREPF Z)) NIL 'LAST)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETK '*LIST (AEVAL 'NIL))
      (COND
       ((EVALNEQ (AEVAL (LENGTH (CAR UHF))) 0)
        (AEVAL
         (LIST 'PRSYS* "The determining system is not completely solved")))
       (T
        (PROGN
         (AEVAL (GENGEN))
         (AEVAL (LIST 'PRGEN))
         (AEVAL (LIST 'COMM-TAB))))))) 
(PUT 'PRSYS* 'NUMBER-OF-ARGS 1) 
(PUT 'PRSYS* 'DEFINED-ON-LINE '924) 
(PUT 'PRSYS* 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRSYS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRSYS* (U)
    (COND
     ((AND UHF (CAR UHF))
      (PROGN (TERPRI) (PROGN (PRIN2 U) NIL) (TERPRI) (PRSYS) (TERPRI))))) 
(PUT 'PRSYS 'NUMBER-OF-ARGS 0) 
(PUT 'PRSYS 'DEFINED-ON-LINE '928) 
(PUT 'PRSYS 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRSYS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRSYS NIL
    (PROG (V)
      (TERPRI)
      (REMPROP 'GL 'KVALUE)
      (REMKLIST 'GL)
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH (CAR UHF)) K)) (RETURN NIL)))
        (PROG (Z L)
          (SETQ L 0)
          (SETQ Z (PREPF (NTH (CAR UHF) K)))
          (SETQ L (LENGTHF (PREPF (NTH (CAR UHF) K))))
          (SETQ *LIST (GREATERP L 50))
          (PROGN
           (ASSGNPRI (AEVAL* "GL(") NIL 'FIRST)
           (ASSGNPRI K NIL NIL)
           (ASSGNPRI (AEVAL* "):=") NIL NIL)
           (ASSGNPRI (AEVAL* Z) NIL 'LAST))
          (SETK (LIST 'GL K) Z))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROGN (TERPRI) (TERPRI))
      (PROGN (PRIN2 "The remaining dependencies") NIL)
      (PROGN (TERPRI) (TERPRI))
      (SETQ V
              (MAKESET
               (APPEND (SACAR 'XI (CAR UHF))
                       (APPEND (SACAR 'ETA (CAR UHF)) (SACAR 'C (CAR UHF))))))
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (WRITE-DEP X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ *LIST NIL))) 
(PUT 'PRRULE 'NUMBER-OF-ARGS 1) 
(PUT 'PRRULE 'DEFINED-ON-LINE '949) 
(PUT 'PRRULE 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRRULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRRULE (U)
    (PROG ()
      (PROGN (TERPRI) (TERPRI))
      (COND ((EQN (LENGTH U) 1) (PROGN (PRIN2 "Substitution") NIL))
            (T (PROGN (PRIN2 "Substitutions") NIL)))
      (PROGN (TERPRI) (TERPRI))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (MAPRIN (CAR X))
            (PRIN2* " = ")
            (MAPRIN (CDR X))
            (TERPRI* T)
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI)
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (Y)
             (SETQ Y (SACAR 'C (CDR X)))
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y) (WRITE-DEP Y)) (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'PRTLIST 'NUMBER-OF-ARGS 1) 
(PUT 'PRTLIST 'DEFINED-ON-LINE '961) 
(PUT 'PRTLIST 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRTLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRTLIST (U)
    (PROGN
     (PROGN (PRIN2 U) NIL)
     (PROGN (TERPRI) (TERPRI))
     (PROGN (PRIN2 "DEPL!*:='") NIL)
     (PRETTYPRINT DEPL*)
     (PROGN (PRIN2 "UHF:='") NIL)
     (PRETTYPRINT UHF))) 
(PUT 'WRITE-DF-SUB 'NUMBER-OF-ARGS 0) 
(PUT 'WRITE-DF-SUB 'DEFINED-ON-LINE '965) 
(PUT 'WRITE-DF-SUB 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'WRITE-DF-SUB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE WRITE-DF-SUB NIL
    (COND
     ((GET 'DF 'KVALUE)
      (PROG (W)
        (SETQ W (GET 'DF 'KVALUE))
        (REMPROP 'DF 'KVALUE)
        (TERPRI)
        (COND ((EQUAL (LENGTH W) 1) (PROGN (PRIN2 "Constraint") NIL))
              (T (PROGN (PRIN2 "Constraints") NIL)))
        (PROGN (TERPRI) (TERPRI))
        (PROG (X)
          (SETQ X W)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROG (U V)
               (SETQ U (CAR X))
               (SETQ V (CADADR X))
               (PROGN
                (ASSGNPRI (AEVAL U) NIL 'FIRST)
                (ASSGNPRI (AEVAL ":=") NIL NIL)
                (ASSGNPRI (AEVAL (PREPSQ V)) NIL 'LAST))
               (PROGN (TERPRI) (TERPRI))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (PUT 'DF 'KVALUE W))))) 
(PUT 'PRGEN 'NUMBER-OF-ARGS 0) 
(FLAG '(PRGEN) 'OPFN) 
(PUT 'PRGEN 'DEFINED-ON-LINE '983) 
(PUT 'PRGEN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'PRGEN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRGEN NIL
    (PROG (LCPAR)
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NN) K)) (RETURN NIL)))
        (PROGN
         (AEVAL* (ORDER (LIST (LIST 'DX K))))
         (AEVAL* (FACTOR (LIST (LIST 'DX K)))))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MM) K)) (RETURN NIL)))
        (AEVAL* (FACTOR (LIST (LIST 'DU K))))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ LCPAR (CPAR (GET 'GEN 'KVALUE)))
      (ASSGNPRI (AEVAL "The symmetry generators are") NIL 'ONLY)
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'NUMGEN)) K))
          (RETURN NIL)))
        (COND
         ((EVALLEQ (AEVAL* (LENGTHF (REVAL1 (LIST 'GEN K) T))) 60)
          (PROGN
           (SETQ *LIST NIL)
           (PROGN
            (ASSGNPRI (AEVAL* "GEN(") NIL 'FIRST)
            (ASSGNPRI K NIL NIL)
            (ASSGNPRI (AEVAL* "):=") NIL NIL)
            (ASSGNPRI (AEVAL* (LIST 'GEN K)) NIL 'LAST))))
         (T
          (PROG (Z R S NT)
            (SETQ R 0)
            (SETQ S 0)
            (SETQ NT 0)
            (AEVAL* (OPERATOR (LIST 'GEN*)))
            (SETQ NT
                    (AEVAL*
                     (LENGTH (SETQ Z (CAR (SIMP (REVAL1 (LIST 'GEN K) T)))))))
            (SETQ R
                    (AEVAL*
                     (MAXL
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X Z)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (ABS (LNC (CKRN (LIST X)))))
                                          (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (ABS (LNC (CKRN (LIST X)))))
                                  (CAR X))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
            (COND ((EQUAL R 1) (SETQ R (AEVAL* 0)))
                  (T (SETQ R (AEVAL* (FLATSIZEC R)))))
            (PROG (L)
              (SETQ L 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NT L)) (RETURN NIL)))
              (SETK (LIST 'GEN* L) (AEVAL* (PREPF (LIST (NTH Z L)))))
              (SETQ L (PLUS2 L 1))
              (GO LAB))
            (PROG (L)
              (SETQ L 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NT L)) (RETURN NIL)))
              (PROG ()
                (SETQ *LIST (GEQ (LENGTHF (PREPF (CDR (NTH Z L)))) 56))
                (SETQ S (AEVAL* (ABS (LNC (CKRN (LIST (NTH Z L)))))))
                (COND ((EQUAL R 0) (SETQ S (AEVAL* 0)))
                      ((EQUAL S 1) (SETQ S (AEVAL* (MINUS 1))))
                      (T (SETQ S (AEVAL* (FLATSIZEC S)))))
                (COND
                 ((EQUAL L 1)
                  (PROGN
                   (ASSGNPRI (AEVAL* "GEN(") NIL 'FIRST)
                   (ASSGNPRI K NIL NIL)
                   (ASSGNPRI (AEVAL* "):=") NIL NIL)
                   (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 1))) NIL
                             NIL)
                   (ASSGNPRI (AEVAL* (LIST 'GEN* 1)) NIL 'LAST)))
                 ((BOOLVALUE* (REVALX (LIST 'MINUS-F (LIST 'GEN* L))))
                  (PROGN
                   (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 6))) NIL
                             'FIRST)
                   (ASSGNPRI (AEVAL* (LIST 'GEN* L)) NIL 'LAST)))
                 (T
                  (PROGN
                   (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 6))) NIL
                             'FIRST)
                   (ASSGNPRI (AEVAL* " + ") NIL NIL)
                   (ASSGNPRI (AEVAL* (LIST 'GEN* L)) NIL 'LAST)))))
              (SETQ L (PLUS2 L 1))
              (GO LAB))
            (AEVAL* (CLEAR (LIST 'GEN*)))
            (SETQ *LIST NIL))))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (COND
       ((EVALNEQ (AEVAL (LENGTH LCPAR)) 0)
        (PROGN
         (ASSGNPRI (AEVAL "The remaining dependencies") NIL 'ONLY)
         (AEVAL (TERPRI)))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LENGTH LCPAR)) K))
          (RETURN NIL)))
        (PROGN (AEVAL* (WRITE-DEP (NTH LCPAR K))) (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (COND ((EVALNEQ (AEVAL (LENGTH LCPAR)) 0) (AEVAL (TERPRI))))
      (WRITE-DF-SUB))) 
(PUT 'COMM-TAB 'NUMBER-OF-ARGS 0) 
(FLAG '(COMM-TAB) 'OPFN) 
(PUT 'COMM-TAB 'DEFINED-ON-LINE '1019) 
(PUT 'COMM-TAB 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'COMM-TAB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COMM-TAB NIL
    (COND
     ((EVALGEQ (AEVAL NUM-DGEN) 2)
      (PROG (ND V)
        (SETQ ND 0)
        (SETQ ND (AEVAL NUM-DGEN))
        (ASSGNPRI
         (AEVAL "The non-vanishing commutators of the finite subgroup") NIL
         'ONLY)
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (DIFFERENCE ND 1) I)) (RETURN NIL)))
          (PROG (J)
            (SETQ J (PLUS I 1))
           LAB
            (COND ((MINUSP (DIFFERENCE ND J)) (RETURN NIL)))
            (COND
             ((EVALNEQ
               (SETQ V (AEVAL* (LIST 'COMM (LIST 'GEN I) (LIST 'GEN J)))) 0)
              (COND
               ((EVALLEQ (AEVAL* (LENGTHF (REVAL1 V T))) 60)
                (PROGN
                 (SETQ *LIST NIL)
                 (PROGN
                  (ASSGNPRI (AEVAL* "COMM(") NIL 'FIRST)
                  (ASSGNPRI I NIL NIL)
                  (ASSGNPRI (AEVAL* ",") NIL NIL)
                  (ASSGNPRI J NIL NIL)
                  (ASSGNPRI (AEVAL* "):= ") NIL NIL)
                  (ASSGNPRI (AEVAL* V) NIL 'LAST))))
               (T
                (PROG (R S NT Z)
                  (SETQ R 0)
                  (SETQ S 0)
                  (SETQ NT 0)
                  (AEVAL* (OPERATOR (LIST 'GEN*)))
                  (SETQ NT
                          (AEVAL* (LENGTH (SETQ Z (CAR (SIMP (REVAL1 V T)))))))
                  (SETQ R
                          (AEVAL*
                           (MAXL
                            (PROG (X FORALL-RESULT FORALL-ENDPTR)
                              (SETQ X Z)
                              (COND ((NULL X) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (X)
                                                  (ABS (LNC (CKRN (LIST X)))))
                                                (CAR X))
                                               NIL)))
                             LOOPLABEL
                              (SETQ X (CDR X))
                              (COND ((NULL X) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (ABS (LNC (CKRN (LIST X)))))
                                        (CAR X))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))))
                  (COND ((EQUAL R 1) (SETQ R (AEVAL* 0)))
                        (T (SETQ R (AEVAL* (FLATSIZEC R)))))
                  (PROG (I)
                    (SETQ I 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE NT I)) (RETURN NIL)))
                    (SETK (LIST 'GEN* I) (AEVAL* (PREPF (LIST (NTH Z I)))))
                    (SETQ I (PLUS2 I 1))
                    (GO LAB))
                  (PROG (L)
                    (SETQ L 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE NT L)) (RETURN NIL)))
                    (PROG ()
                      (SETQ *LIST (GEQ (LENGTHF (REVAL1 (LIST 'GEN* L) T)) 63))
                      (SETQ S (AEVAL* (ABS (LNC (CKRN (LIST (NTH Z L)))))))
                      (COND ((EQUAL R 0) (SETQ S (AEVAL* 0)))
                            ((EQUAL S 1) (SETQ S (AEVAL* (MINUS 1))))
                            (T (SETQ S (AEVAL* (FLATSIZEC S)))))
                      (COND
                       ((EQUAL L 1)
                        (PROGN
                         (ASSGNPRI (AEVAL* "COMM(") NIL 'FIRST)
                         (ASSGNPRI I NIL NIL)
                         (ASSGNPRI (AEVAL* ",") NIL NIL)
                         (ASSGNPRI J NIL NIL)
                         (ASSGNPRI (AEVAL* "):=") NIL NIL)
                         (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 1)))
                                   NIL NIL)
                         (ASSGNPRI (AEVAL* (LIST 'GEN* 1)) NIL 'LAST)))
                       ((BOOLVALUE* (REVALX (LIST 'MINUS-F (LIST 'GEN* L))))
                        (PROGN
                         (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 9)))
                                   NIL 'FIRST)
                         (ASSGNPRI (AEVAL* (LIST 'GEN* L)) NIL 'LAST)))
                       (T
                        (PROGN
                         (ASSGNPRI (AEVAL* (BLANKS (PLUS (DIFFERENCE R S) 9)))
                                   NIL 'FIRST)
                         (ASSGNPRI (AEVAL* " + ") NIL NIL)
                         (ASSGNPRI (AEVAL* (LIST 'GEN* L)) NIL 'LAST)))))
                    (SETQ L (PLUS2 L 1))
                    (GO LAB))
                  (AEVAL* (CLEAR (LIST 'GEN*))))))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ *LIST NIL))))) 
(PUT 'WRITE-DEP 'NUMBER-OF-ARGS 1) 
(PUT 'WRITE-DEP 'DEFINED-ON-LINE '1050) 
(PUT 'WRITE-DEP 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'WRITE-DEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITE-DEP (U)
    (COND
     ((ASSOC (REVAL1 U T) DEPL*)
      (PROG (V)
        (SETQ V (CDR (ASSOC U DEPL*)))
        (PROGN
         (PRIN2 (CAR U))
         (PRIN2 "(")
         (PRIN2 (CADR U))
         (PRIN2 ") depends on ")
         NIL)
        (PROGN (PRIN2 (CAAR V)) (PRIN2 "(") (PRIN2 (CADAR V)) (PRIN2 ")") NIL)
        (PROG (X)
          (SETQ X (CDR V))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROGN
              (PRIN2 ",")
              (PRIN2 (CAR X))
              (PRIN2 "(")
              (PRIN2 (CADR X))
              (PRIN2 ")")
              NIL))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (PROGN (TERPRI) (TERPRI)))))) 
(FLAG '(WRITE-DEP) 'OPFN) 
(PUT 'FIND-NFUN 'NUMBER-OF-ARGS 0) 
(PUT 'FIND-NFUN 'DEFINED-ON-LINE '1062) 
(PUT 'FIND-NFUN 'DEFINED-IN-FILE 'SPDE/SPDE.RED) 
(PUT 'FIND-NFUN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIND-NFUN NIL
    (COND ((NOT (GET 'C 'KLIST)) 0)
          (T
           (MAXL
            (MAKESET
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (GET 'C 'KLIST))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (CADAR X)) (CAR X)) NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (CADAR X)) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))) 
(ENDMODULE) 
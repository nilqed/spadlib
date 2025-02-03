(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LAPLACE)) 
(CREATE-PACKAGE '(LAPLACE) '(CONTRIB MISC)) 
(FLUID
 '(*EXP *LIMITEDFACTORS *MCD *PRECISE *ROUNDED DEPL* KORD* SUBFG* TRANSVAR*
   VARSTACK*)) 
(GLOBAL
 '(LPSM* LPCM* LPSHM* LPCHM* LPSE* LPCE* LPSHE* LPCHE* LPEXPT* ILE1* ILE2*
   ILE3* ILE4* ILE5* LPVAR* ILVAR* LPSHIFT* *LMSG *LMON *LTRIG *LHYP *LDONE
   *LIONE)) 
(SWITCH (LIST 'LHYP 'LMON 'LTRIG)) 
(SETQ *LMSG T) 
(COND ((NOT (EQUAL (GETTYPE 'INTL) 'OPERATOR)) (AEVAL (OPERATOR (LIST 'INTL))))) 
(COND ((NOT (EQUAL (GETTYPE 'ONE) 'OPERATOR)) (AEVAL (OPERATOR (LIST 'ONE))))) 
(COND
 ((NOT (EQUAL (GETTYPE 'DELTA) 'OPERATOR)) (AEVAL (OPERATOR (LIST 'DELTA))))) 
(PUT 'LAP-SAVE-ENVIRONMENT 'NUMBER-OF-ARGS 0) 
(PUT 'LAP-SAVE-ENVIRONMENT 'DEFINED-ON-LINE '91) 
(PUT 'LAP-SAVE-ENVIRONMENT 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LAP-SAVE-ENVIRONMENT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LAP-SAVE-ENVIRONMENT NIL
    (PROG (U)
      (SETQ U
              (LIST *EXP *MCD KORD* DEPL* (GET 'EXPT 'OPMTCH)
                    (GET 'SIN 'OPMTCH) (GET 'COS 'OPMTCH) (GET 'SINH 'OPMTCH)
                    (GET 'COSH 'OPMTCH) (GET 'ONE 'SIMPFN) (GET 'DELTA 'SIMPFN)
                    (GET 'INTL 'SIMPFN) (GET 'LAPLACE 'SIMPFN)
                    (GET 'INVLAP 'SIMPFN)))
      (SETQ KORD* (APPEND KORD* NIL))
      (SETQ DEPL*
              (PROG (D FORALL-RESULT FORALL-ENDPTR)
                (SETQ D DEPL*)
                (COND ((NULL D) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (D) (APPEND D NIL)) (CAR D))
                                      NIL)))
               LOOPLABEL
                (SETQ D (CDR D))
                (COND ((NULL D) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (D) (APPEND D NIL)) (CAR D)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN U))) 
(PUT 'LAP-RESTORE-ENVIRONMENT 'NUMBER-OF-ARGS 1) 
(PUT 'LAP-RESTORE-ENVIRONMENT 'DEFINED-ON-LINE '111) 
(PUT 'LAP-RESTORE-ENVIRONMENT 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LAP-RESTORE-ENVIRONMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAP-RESTORE-ENVIRONMENT (U)
    (PROG ()
      (SETQ *EXP (CAR U))
      (SETQ U (CDR U))
      (SETQ *MCD (CAR U))
      (SETQ U (CDR U))
      (SETQ KORD* (CAR U))
      (SETQ U (CDR U))
      (SETQ DEPL* (CAR U))
      (SETQ U (CDR U))
      (PUT 'EXPT 'OPMTCH (CAR U))
      (SETQ U (CDR U))
      (PUT 'SIN 'OPMTCH (CAR U))
      (SETQ U (CDR U))
      (PUT 'COS 'OPMTCH (CAR U))
      (SETQ U (CDR U))
      (PUT 'SINH 'OPMTCH (CAR U))
      (SETQ U (CDR U))
      (PUT 'COSH 'OPMTCH (CAR U))
      (SETQ U (CDR U))
      (PUT 'ONE 'SIMPFN (CAR U))
      (SETQ U (CDR U))
      (PUT 'DELTA 'SIMPFN (CAR U))
      (SETQ U (CDR U))
      (PUT 'INTL 'SIMPFN (CAR U))
      (SETQ U (CDR U))
      (PUT 'LAPLACE 'SIMPFN (CAR U))
      (SETQ U (CDR U))
      (PUT 'INVLAP 'SIMPFN (CAR U))
      (SETQ U (CDR U)))) 
(PUT 'LAPLACE 'SIMPFN 'SIMPLAPLACE) 
(SETQ LPSM*
        '(((MINUS =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (MINUS (TIMES (ONE (MINUS =X)) (SIN =X))) NIL)) 
(SETQ LPCM*
        '(((MINUS =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE (MINUS =X)) (COS =X)) NIL)) 
(SETQ LPSHM*
        '(((MINUS =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (MINUS (TIMES (ONE (MINUS =X)) (SINH =X))) NIL)) 
(SETQ LPCHM*
        '(((MINUS =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE (MINUS =X)) (COSH =X)) NIL)) 
(SETQ LPSE*
        '((=X) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE =X)
                 (QUOTIENT
                  (DIFFERENCE (EXPT E (TIMES I =X))
                              (EXPT E (MINUS (TIMES I =X))))
                  (TIMES 2 I)))
          NIL)) 
(SETQ LPCE*
        '((=X) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE =X)
                 (QUOTIENT
                  (PLUS (EXPT E (TIMES I =X)) (EXPT E (MINUS (TIMES I =X))))
                  2))
          NIL)) 
(SETQ LPSHE*
        '((=X) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE =X)
                 (QUOTIENT (DIFFERENCE (EXPT E =X) (EXPT E (MINUS =X))) 2))
          NIL)) 
(SETQ LPCHE*
        '((=X) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (TIMES (ONE =X) (QUOTIENT (PLUS (EXPT E =X) (EXPT E (MINUS =X))) 2))
          NIL)) 
(SETQ LPEXPT*
        '((E (PLUS =X =Y)) (NIL . T)
          (TIMES (EXPT E =X) (EXPT E =Y) (ONE (PLUS =X =Y))) NIL)) 
(PUT 'SIMPLAPLACE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLAPLACE 'DEFINED-ON-LINE '181) 
(PUT 'SIMPLAPLACE 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'SIMPLAPLACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLAPLACE (U)
    (PROG (E R)
      (SETQ E (LAP-SAVE-ENVIRONMENT))
      (SETQ R (ERRORSET (LIST 'SIMPLAPLACE* (MKQUOTE U)) NIL NIL))
      (LAP-RESTORE-ENVIRONMENT E)
      (COND ((ERRORP R) (TYPERR (CONS 'LAPLACE U) "Laplace form"))
            (T (RETURN (LAPLACE_FIXUP (CAR R))))))) 
(PUT 'LAPLACE_FIXUP 'NUMBER-OF-ARGS 1) 
(PUT 'LAPLACE_FIXUP 'DEFINED-ON-LINE '190) 
(PUT 'LAPLACE_FIXUP 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LAPLACE_FIXUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAPLACE_FIXUP (U)
    ((LAMBDA (VARSTACK*)
       (PROGN
        (PUT 'LAPLACE 'SIMPFN 'SIMPIDEN)
        (SETQ U (SIMP (AEVAL* (PREPSQ U))))
        (PUT 'LAPLACE 'SIMPFN 'SIMPLAPLACE)
        U))
     NIL)) 
(PUT 'SIMPLAPLACE* 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLAPLACE* 'DEFINED-ON-LINE '198) 
(PUT 'SIMPLAPLACE* 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'SIMPLAPLACE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLAPLACE* (U)
    (PROG (*EXP *MCD V W TRANSVAR* *PRECISE)
      (COND ((NULL SUBFG*) (RETURN (MKSQ (CONS 'LAPLACE U) 1))))
      (COND
       ((OR (AND (CDDR U) (NULL (IDP (SETQ W (CADDR U)))))
            (NULL (IDP (SETQ V (CADR U)))))
        (GO ERR)))
      (SETQ V (CAAAR (CAR (SIMP V))))
      (SETQ TRANSVAR* W)
      (COND ((NULL TRANSVAR*) (SETQ TRANSVAR* 'IL&)))
      (COND ((NULL (IDP V)) (GO ERR)))
      (SETQ U (CAR U))
      (SETQ *MCD (SETQ *EXP T))
      (SETQ KORD* (CONS 'LP& (CONS 'IL& KORD*)))
      (PUT 'ONE 'SIMPFN 'LPSIMP1)
      (COND (*LDONE (PUT 'EXPT 'OPMTCH (CONS LPEXPT* (GET 'EXPT 'OPMTCH)))))
      (COND
       (*LMON
        (PROGN
         (PUT 'SIN 'OPMTCH (CONS LPSE* (GET 'SIN 'OPMTCH)))
         (PUT 'COS 'OPMTCH (CONS LPCE* (GET 'COS 'OPMTCH)))
         (PUT 'SINH 'OPMTCH (CONS LPSHE* (GET 'SINH 'OPMTCH)))
         (PUT 'COSH 'OPMTCH (CONS LPCHE* (GET 'COSH 'OPMTCH)))))
       (T
        (PROGN
         (PUT 'SIN 'OPMTCH (CONS LPSM* (GET 'SIN 'OPMTCH)))
         (PUT 'COS 'OPMTCH (CONS LPCM* (GET 'COS 'OPMTCH)))
         (PUT 'SINH 'OPMTCH (CONS LPSHM* (GET 'SINH 'OPMTCH)))
         (PUT 'COSH 'OPMTCH (CONS LPCHM* (GET 'COSH 'OPMTCH))))))
      (SETQ LPVAR* V)
      (SETQ LPSHIFT* T)
      (COND ((NEQ V 'LP&) (SETQ KORD* (CONS V KORD*))))
      (PROG (X)
        (SETQ X DEPL*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((MEMQ V (CDR X)) (RPLACD X (CONS 'LP& (CDR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (OFF (LIST 'MCD))
      (SETQ U (LAPLACE1 (LIST U V)))
      (COND (W (SETQ U (SUBF (CAR U) (LIST (CONS 'IL& W))))))
      (PROG (X)
        (SETQ X DEPL*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((MEMQ 'LP& (CDR X)) (RPLACD X (DELETE 'LP& (CDR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PUT 'ONE 'SIMPFN 'SIMPIDEN)
      (SETQ KORD* (CDDR KORD*))
      (PUT 'SIN 'OPMTCH (CDR (GET 'SIN 'OPMTCH)))
      (PUT 'COS 'OPMTCH (CDR (GET 'COS 'OPMTCH)))
      (PUT 'SINH 'OPMTCH (CDR (GET 'SINH 'OPMTCH)))
      (PUT 'COSH 'OPMTCH (CDR (GET 'COSH 'OPMTCH)))
      (COND (*LDONE (PUT 'EXPT 'OPMTCH (CDR (GET 'EXPT 'OPMTCH)))))
      (COND (ERFG* (SETQ ERFG* NIL)))
      (RETURN U)
     ERR
      (MSGPRI "Laplace operator incorrect" NIL NIL NIL T))) 
(PUT 'SIN 'LPFN '(QUOTIENT K (PLUS (EXPT IL& 2) (EXPT K 2)))) 
(PUT 'COS 'LPFN '(QUOTIENT IL& (PLUS (EXPT IL& 2) (EXPT K 2)))) 
(PUT 'SINH 'LPFN '(QUOTIENT K (PLUS (EXPT IL& 2) (MINUS (EXPT K 2))))) 
(PUT 'COSH 'LPFN '(QUOTIENT IL& (PLUS (EXPT IL& 2) (MINUS (EXPT K 2))))) 
(PUT 'ONE 'LPFN '(QUOTIENT 1 IL&)) 
(PUT 'EXPT 'LPFN
     '(QUOTIENT (TIMES (EXPT K D) (GAMMA (PLUS D 1))) (EXPT IL& (PLUS D 1)))) 
(PUT 'DELTA 'LPFN 1) 
(PUT 'LAPLACE1 'NUMBER-OF-ARGS 1) 
(PUT 'LAPLACE1 'DEFINED-ON-LINE '268) 
(PUT 'LAPLACE1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LAPLACE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAPLACE1 (U)
    (PROG (V W Z)
      (SETQ V (CADR U))
      (SETQ U (CAR U))
      (SETQ Z (SIMP* U))
      (COND ((NEQ (CDR Z) 1) (SETQ Z (SIMP (PREPSQ Z)))))
      (COND ((NEQ (CDR Z) 1) (REDERR (LIST U "has non-trivial denominator"))))
      (SETQ Z (CAR Z))
      (COND
       ((NEQ V 'LP&)
        (PROGN
         (SETQ KORD* (CDR KORD*))
         (SETQ Z (LPCHVAR (LIST (CONS V 'LP&)) Z)))))
      (COND
       (ERFG*
        (RETURN
         (CONS
          (LIST
           (CONS
            (GETPOWER
             (FKERN
              (LIST 'LAPLACE (SUBLA (LIST (CONS 'LP& LPVAR*)) U) LPVAR*
                    TRANSVAR*))
             1)
            1))
          1))))
      (SETQ W (CONS NIL 1))
      (SETQ U Z)
      (SETQ *EXP NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (COND
         ((OR (ATOM U) (ATOM (CAR U)))
          (PROGN (SETQ W (ADDSQ W (LPDOM U))) (SETQ U NIL)))
         (T
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (COND ((SETQ Z (LPTERMX (CAR U))) Z)
                                (T
                                 (CONS
                                  (LIST
                                   (CONS
                                    (GETPOWER
                                     (FKERN
                                      (LIST 'LAPLACE
                                            (SUBLA (LIST (CONS 'LP& LPVAR*))
                                                   (PREPSQ
                                                    (CONS (LIST (CAR U)) 1)))
                                            LPVAR* TRANSVAR*))
                                     1)
                                    1))
                                  1)))))
           (SETQ U (CDR U)))))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'LPCHVAR 'NUMBER-OF-ARGS 2) 
(PUT 'LPCHVAR 'DEFINED-ON-LINE '293) 
(PUT 'LPCHVAR 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPCHVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPCHVAR (U V)
    (COND ((OR (ATOM V) (ATOM (CAR V))) V)
          (T
           (ADDF
            ((LAMBDA (G124 G125)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF G124 G125))
                     (T (POLY-MULTF G124 G125))))
             (LIST (CONS (GETPOWER (FKERN (SUBLA U (CAAAR V))) (CDAAR V)) 1))
             (LPCHVAR U (CDAR V)))
            (LPCHVAR U (CDR V)))))) 
(PUT 'LPTERMX 'NUMBER-OF-ARGS 1) 
(PUT 'LPTERMX 'DEFINED-ON-LINE '298) 
(PUT 'LPTERMX 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPTERMX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPTERMX (U)
    (PROG (W N)
      (SETQ N 0)
      (COND ((NEQ (CAAR U) 'LP&) (RETURN (LPTERM U)))
            ((FIXP (CDAR U))
             (COND ((GREATERP (SETQ N (CDAR U)) 0) NIL)
                   (T (RETURN (LPUNKNOWN U)))))
            (T
             (RETURN
              (LPTERM
               (CONS (CONS (LIST 'EXPT 'LP& (PREPSQ (CONS (CDAR U) 1))) 1)
                     (CDR U))))))
      (COND ((SETQ W (LPFORM (CDR U))) NIL) (T (RETURN NIL)))
     A
      (COND ((EQUAL N 0) (RETURN W)))
      (SETQ W (NEGSQ (DIFFSQ W 'IL&)))
      (SETQ N (DIFFERENCE N 1))
      (GO A))) 
(PUT 'LPDOM 'NUMBER-OF-ARGS 1) 
(PUT 'LPDOM 'DEFINED-ON-LINE '316) 
(PUT 'LPDOM 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPDOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDOM (U) (CONS (LIST (CONS (CONS 'IL& (MINUS 1)) U)) 1)) 
(PUT 'LPFORM 'NUMBER-OF-ARGS 1) 
(PUT 'LPFORM 'DEFINED-ON-LINE '321) 
(PUT 'LPFORM 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPFORM (U)
    (PROG (Y Z)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (LPDOM U)))
            ((CDR U)
             (RETURN
              (COND
               ((AND (SETQ Y (LPTERM (CAR U))) (SETQ Z (LPFORM (CDR U))))
                (ADDSQ Y Z))
               (T NIL))))
            (T (RETURN (LPTERM (CAR U))))))) 
(PUT 'LPTERM 'NUMBER-OF-ARGS 1) 
(PUT 'LPTERM 'DEFINED-ON-LINE '334) 
(PUT 'LPTERM 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPTERM (U)
    (PROG (V W W1 Y Z)
      (SETQ V (CAR U))
      (SETQ W (CDR U))
      (COND
       ((OR (ATOM (SETQ Y (CAR V))) (ATOM (CAR Y)))
        (COND
         ((NOT (DEPENDS Y 'LP&))
          (RETURN
           (COND ((SETQ Z (LPFORM W)) (MULTSQ (CONS (LIST (CONS V 1)) 1) Z))
                 (T NIL))))
         ((ATOM Y) (RETURN (LPUNKNOWN U)))
         ((EQUAL (CAR Y) 'EXPT) (RETURN (LPEXPT V NIL W))) (T NIL)))
       (T
        (RETURN
         (COND
          ((NOT (DEPENDS (PREPSQ (CONS Y 1)) 'LP&))
           (COND ((SETQ Z (LPFORM W)) (MULTSQ (CONS (LIST (CONS V 1)) 1) Z))
                 (T NIL)))
          (T (LPUNKNOWN U))))))
      (COND ((OR (ATOM W) (ATOM (CAR W))) (RETURN (LPFUNC V W))))
      (COND
       ((CDR W)
        (RETURN
         (COND
          ((AND (SETQ Y (LPTERM (LIST V (CAR W))))
                (SETQ Z (LPTERM (CONS V (CDR W)))))
           (ADDSQ Y Z))
          (T NIL)))))
      (SETQ W1 (CDAR W))
      (SETQ W (CAAR W))
      (COND
       ((NOT
         (DEPENDS
          (COND ((NOT (SFP (SETQ Y (CAR W)))) Y) (T (PREPSQ (CONS Y 1))))
          'LP&))
        (RETURN
         (COND
          ((SETQ Z (LPTERM (CONS V W1))) (MULTSQ (CONS (LIST (CONS W 1)) 1) Z))
          (T NIL))))
       ((EQUAL (CAR Y) 'EXPT) (RETURN (LPEXPT W V W1))))
      (COND
       ((AND (EQUAL (CAAR V) 'ONE) (EQUAL (CAAR W) 'ONE))
        (RETURN (LPMULT1 V W W1)))
       (T (RETURN (LPUNKNOWN U)))))) 
(PUT 'LPUNKNOWN 'NUMBER-OF-ARGS 1) 
(PUT 'LPUNKNOWN 'DEFINED-ON-LINE '367) 
(PUT 'LPUNKNOWN 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPUNKNOWN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPUNKNOWN (U)
    (PROG (D Z W)
      (COND
       ((AND ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ D (CDR U)))
             (NOT (|:ONEP| D)))
        (SETQ U (CONS (LIST (CONS (CAR U) 1)) 1)))
       (T (SETQ U (CONS (LIST U) 1))))
      (SETQ U (LIST 'LAPLACE (PREPSQ U) 'LP& TRANSVAR*))
      (SETQ W (LIST 'LAPLACE (CADR U) 'LP&))
      (COND
       ((AND (GET 'LAPLACE 'OPMTCH)
             (OR (SETQ Z (OPMTCH U)) (SETQ Z (OPMTCH W))))
        (PROGN
         (SETQ *EXP T)
         (PUT 'LAPLACE 'SIMPFN 'LAPLACE1)
         (SETQ Z (SIMP Z))
         (SETQ *EXP NIL)
         (PUT 'LAPLACE 'SIMPFN 'SIMPLAPLACE))))
      (COND
       ((NULL Z)
        (RETURN
         (COND
          (*LMSG
           (MSGPRI "Laplace for" (SUBLA (LIST (CONS 'LP& LPVAR*)) (CADR U))
                   (COND ((OR *LMON (ATOM (CADR U))) "not known")
                         (T "not known - try ON LMON"))
                   NIL NIL))
          (T NIL)))))
      (SETQ Z (SUBLA (LIST (CONS 'LP& LPVAR*)) Z))
      (RETURN
       (COND
        ((AND (OR (ATOM D) (ATOM (CAR D))) (NOT (|:ONEP| D)))
         (MULTSQ Z (CONS D 1)))
        (T Z))))) 
(PUT 'LPSIMP1 'NUMBER-OF-ARGS 1) 
(PUT 'LPSIMP1 'DEFINED-ON-LINE '391) 
(PUT 'LPSIMP1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPSIMP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPSIMP1 (U)
    (PROG (V L R)
      (SETQ V (SUBLA (LIST (CONS LPVAR* 'LP&)) U))
      (COND ((NOT (DEPENDS (CAR V) 'LP&)) (RETURN (CONS 1 1))))
      (SETQ V (CAR (SIMP (CAR V))))
      (COND
       ((NEQ (CAAAR V) 'LP&)
        (PROGN
         (SETQ *MCD T)
         (SETQ V (SUBF V NIL))
         (SETQ *MCD NIL)
         (SETQ V
                 ((LAMBDA (G127)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR V) G127))
                          (T (POLY-MULTF (CAR V) G127))))
                  (RECIPF* (CDR V)))))))
      (COND ((NOT (AND (EQ (CAAAR V) 'LP&) (|:ONEP| (CDAAR V)))) (GO ERR)))
      (SETQ L (CDAR V))
      (SETQ R (CDR V))
      (COND ((NULL R) (COND ((MINUSF L) (GO ERR)) (T (RETURN (CONS 1 1))))))
      (SETQ V
              (COND ((MINUSF L) (MULTSQ (CONS (NEGF R) 1) (CONS 1 (NEGF L))))
                    (T (MULTSQ (CONS R 1) (CONS 1 L)))))
      (COND ((NOT (MINUSF (CAR V))) (RETURN (CONS 1 1))))
      (COND ((NULL LPSHIFT*) (GO ERR))
            (T
             (RETURN
              (MKSQ
               (LIST 'ONE
                     (PREPSQ (ADDSQ (CONS (LIST (CONS (CONS 'LP& 1) 1)) 1) V)))
               1))))
     ERR
      (COND
       (*LMSG
        (MSGPRI "Laplace induces" (CONS 'ONE U) " which is not allowed" NIL
                'HOLD)))
      (RETURN (CONS NIL 1)))) 
(PUT 'LPMULT1 'NUMBER-OF-ARGS 3) 
(PUT 'LPMULT1 'DEFINED-ON-LINE '414) 
(PUT 'LPMULT1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPMULT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPMULT1 (U V W)
    (PROG (U1 V1 L1 R1 L2 R2)
      (SETQ U1 (CAR (SIMP (CADAR U))))
      (SETQ V1 (CAR (SIMP (CADAR V))))
      (SETQ L1 (CDAR U1))
      (SETQ L2 (CDAR V1))
      (SETQ R1 (CDR U1))
      (SETQ R2 (CDR V1))
      (COND
       ((AND (OR (ATOM L1) (ATOM (CAR L1))) (OR (ATOM L2) (ATOM (CAR L2)))
             (OR (ATOM R1) (ATOM (CAR R1))) (OR (ATOM R2) (ATOM (CAR R2))))
        (COND
         ((|:MINUSP| (ADDDM (MULTDM R1 L2) (|:MINUS| (MULTDM R2 L1))))
          (RETURN (LPTERM (CONS U W))))
         (T (RETURN (LPTERM (CONS V W))))))
       (T (RETURN (LPUNKNOWN (LIST U (CONS V W)))))))) 
(PUT 'LPEXPT 'NUMBER-OF-ARGS 3) 
(PUT 'LPEXPT 'DEFINED-ON-LINE '431) 
(PUT 'LPEXPT 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPEXPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPEXPT (U V W)
    (PROG (P Q R Z L LA)
      (SETQ R (CDR U))
      (SETQ P (CADAR U))
      (SETQ Q (CADDAR U))
      (COND ((DEPENDS P 'LP&) (GO GAMMA)))
      (SETQ *EXP T)
      (SETQ Q (CAR (SIMP Q)))
      (COND
       ((NEQ (CAAAR Q) 'LP&)
        (PROGN
         (SETQ *MCD T)
         (SETQ Q (SUBF Q NIL))
         (SETQ *MCD NIL)
         (SETQ Q
                 ((LAMBDA (G129)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR Q) G129))
                          (T (POLY-MULTF (CAR Q) G129))))
                  (RECIPF* (CDR Q)))))))
      (COND
       ((NOT (|:ONEP| R))
        (SETQ Q
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF Q R))
                      (T (POLY-MULTF Q R))))))
      (SETQ *EXP NIL)
      (COND
       ((NOT (AND (EQ (CAAAR Q) 'LP&) (|:ONEP| (CDAAR Q))))
        (RETURN
         (COND ((NULL V) (LPUNKNOWN (CONS U W)))
               (T (LPUNKNOWN (LIST U (CONS V W))))))))
      (COND
       ((SETQ R (CDR Q))
        (PROGN
         (COND
          (*LDONE
           (PROGN
            (SETQ *EXP T)
            (SETQ W
                    ((LAMBDA (G131)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G131))
                             (T (POLY-MULTF W G131))))
                     (CAR (LPSIMP1 (LIST (PREPSQ (CONS Q 1)))))))
            (SETQ *EXP NIL))))
         (SETQ Q (LIST (CAR Q)))
         (SETQ R
                 (CONS
                  (LIST (CONS (CONS (LIST 'EXPT P (PREPSQ (CONS R 1))) 1) 1))
                  1)))))
      (COND
       ((NEQ P 'E)
        (SETQ Q
                ((LAMBDA (G133)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF Q G133))
                         (T (POLY-MULTF Q G133))))
                 (LIST (CONS (GETPOWER (FKERN (LIST 'LOG P)) 1) 1))))))
      (SETQ Z (COND ((NULL V) (LPFORM W)) (T (LPTERM (CONS V W)))))
      (COND ((NULL Z) (RETURN NIL)))
      (SETQ L (PREPSQ (CONS (CDAR Q) 1)))
      (SETQ LA (LIST (CONS 'IL& (LIST 'DIFFERENCE 'IL& L))))
      (COND
       ((NOT (EQ TRANSVAR* 'IL&))
        (SETQ Z (SUBSQ Z (LIST (CONS TRANSVAR* 'IL&))))))
      (SETQ Z (SUBF (CAR Z) LA))
      (RETURN (COND (R (MULTSQ R Z)) (T Z)))
     GAMMA
      (RETURN
       (COND
        ((NULL V)
         (COND ((OR (ATOM W) (ATOM (CAR W))) (LPFUNC U W))
               ((NOT (DEPENDS (SETQ L (CAAAR W)) 'LP&))
                (COND
                 ((SETQ Z (LPEXPT U NIL (CDAR W)))
                  (MULTSQ (CONS (LIST (CONS (CAAR W) 1)) 1) Z))
                 (T NIL)))
               ((AND (NOT (ATOM L)) (EQUAL (CAR L) 'EXPT))
                (LPEXPT (CAAR W) U (CDAR W)))
               (T (LPUNKNOWN (CONS U W)))))
        (T (LPUNKNOWN (LIST U (CONS V W)))))))) 
(PUT 'LPFUNC 'NUMBER-OF-ARGS 2) 
(PUT 'LPFUNC 'DEFINED-ON-LINE '479) 
(PUT 'LPFUNC 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'LPFUNC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPFUNC (U V)
    (PROG (LD FN W VAR EX K TAU C)
      (SETQ LD (CDR U))
      (SETQ W (CAR U))
      (SETQ FN (CAR W))
     LINTL
      (COND ((NEQ FN 'INTL) (GO LEXPT)))
      (COND
       ((NOT
         (AND (|:ONEP| LD) (EQUAL (CADDDR W) 0) (EQUAL (CAR (CDDDDR W)) 'LP&)
              (IDP (SETQ VAR (CADDR W)))))
        (RETURN
         (COND
          (*LMSG
           (MSGPRI "Laplace integral"
                   (SUBLA (LIST (CONS 'LP& LPVAR*))
                          (PREPSQ (CONS (LIST (CONS U 1)) 1)))
                   "not allowed" NIL NIL))
          (T NIL)))))
      (SETQ EX (SUBLA (LIST (CONS VAR 'LP&)) (CADR W)))
      (SETQ LPSHIFT* NIL)
      (SETQ W (LAPLACE1 (LIST EX 'LP&)))
      (SETQ LPSHIFT* T)
      (RETURN
       (COND
        (W (MULTSQ (CONS (MULTD V (LIST (CONS (CONS 'IL& (MINUS 1)) 1))) 1) W))
        (T NIL)))
     LEXPT
      (COND ((NEQ FN 'EXPT) (GO LFUNC)))
      (SETQ LD
              ((LAMBDA (G135)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF LD G135))
                       (T (POLY-MULTF LD G135))))
               (CAR (SIMP (CADDR W)))))
      (COND
       ((OR (MINUSF (ADDD 1 LD)) (DEPENDS (PREPSQ (CONS LD 1)) 'LP&))
        (RETURN (LPUNKNOWN (CONS U V)))))
      (SETQ LD (PREPSQ (CONS LD 1)))
     LFUNC
      (COND ((OR (EQUAL FN 'EXPT) (EQUAL FN 'ONE) (|:ONEP| LD)) NIL)
            (T (RETURN (LPUNKNOWN (CONS U V)))))
      (SETQ *EXP T)
      (SETQ EX (CAR (SIMP (CADR W))))
      (SETQ *EXP NIL)
      (COND
       ((NOT (AND (EQUAL (CAAAR EX) 'LP&) (|:ONEP| (CDAAR EX))))
        (RETURN (LPUNKNOWN (CONS U V)))))
      (SETQ K (CDAR EX))
      (SETQ TAU (CDR EX))
      (COND
       ((OR (MINUSF K) (AND (NULL LPSHIFT*) TAU))
        (RETURN
         (COND
          (*LMSG
           (MSGPRI "Laplace for" (SUBLA (LIST (CONS 'LP& LPVAR*)) W)
                   "not allowed" NIL NIL))
          (T NIL)))))
      (COND ((AND TAU (NOT (MINUSF TAU))) (RETURN (LPUNKNOWN (CONS U V)))))
      (SETQ C (PREPSQ (CONS K 1)))
      (COND
       ((SETQ W (GET FN 'LPFN))
        (SETQ W (CAR (SIMP (SUBLA (LIST (CONS 'K C) (CONS 'D LD)) W))))))
      (RETURN
       (COND ((NULL W) (LPUNKNOWN (CONS U V)))
             ((NULL TAU) (CONS (MULTD V W) 1))
             (T
              (CONS
               (MULTD V
                      ((LAMBDA (G137)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G137))
                               (T (POLY-MULTF W G137))))
                       (LIST
                        (CONS
                         (GETPOWER
                          (FKERN
                           (LIST 'EXPT 'E
                                 (PREPSQ
                                  (MULTSQ
                                   (CONS (LIST (CONS (CONS 'IL& 1) 1)) 1)
                                   (MULTSQ (CONS TAU 1) (INVSQ (CONS K 1)))))))
                          1)
                         1))))
               1)))))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(FORALL
 (LIST '(X Y Z) 'T
       '(LET00
         '((EQUAL (LAPLACE (TIMES Z (DELTA X)) X Y) (SUB (EQUAL X 0) Z)))))) 
(FORALL
 (LIST '(K X Y Z) 'T
       '(LET00
         '((EQUAL (LAPLACE (TIMES Z (DELTA (PLUS X (MINUS K)))) X Y)
                  (TIMES (EXPT E (TIMES Y (MINUS K))) (SUB (EQUAL X K) Z))))))) 
(FORALL (LIST '(X Y) 'T '(LET00 '((EQUAL (LAPLACE (DF (DELTA X) X) X Y) Y))))) 
(FORALL
 (LIST '(N X Y) 'T
       '(LET00 '((EQUAL (LAPLACE (DF (DELTA X) X N) X Y) (EXPT Y N)))))) 
(FORALL
 (LIST '(K X Y) 'T
       '(LET00
         '((EQUAL (LAPLACE (DF (DELTA (PLUS X (MINUS K))) X) X Y)
                  (TIMES Y (EXPT E (MINUS (TIMES K Y))))))))) 
(FORALL
 (LIST '(K N X Y) 'T
       '(LET00
         '((EQUAL (LAPLACE (DF (DELTA (PLUS X (MINUS K))) X N) X Y)
                  (TIMES (EXPT Y N) (EXPT E (MINUS (TIMES K Y))))))))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'INVLAP 'SIMPFN 'SIMPINVLAP) 
(SETQ ILE1*
        '((E (TIMES I =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (PLUS (COS =X) (TIMES I (SIN =X))) NIL)) 
(SETQ ILE2*
        '((E (MINUS (TIMES I =X))) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (DIFFERENCE (COS =X) (TIMES I (SIN =X))) NIL)) 
(SETQ ILE3*
        '((E =X) (NIL DEPENDS (REVAL '=X) LPVAR*) (PLUS (COSH =X) (SINH =X))
          NIL)) 
(SETQ ILE4*
        '((E (MINUS =X)) (NIL DEPENDS (REVAL '=X) LPVAR*)
          (DIFFERENCE (COSH =X) (SINH =X)) NIL)) 
(SETQ ILE5*
        '((E (PLUS =X =Y))
          (NIL AND (NOT (DEPENDS (REVAL '=X) 'I)) (DEPENDS (REVAL '=Y) 'I))
          (TIMES (EXPT E =X) (EXPT E =Y)) NIL)) 
(PUT 'SIMPINVLAP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINVLAP 'DEFINED-ON-LINE '579) 
(PUT 'SIMPINVLAP 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'SIMPINVLAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINVLAP (U)
    (PROG (R E)
      (SETQ E (LAP-SAVE-ENVIRONMENT))
      (SETQ R (ERRORSET (LIST 'SIMPINVLAP* (MKQUOTE U)) NIL NIL))
      (LAP-RESTORE-ENVIRONMENT E)
      (COND ((ERRORP R) (TYPERR (CONS 'INVLAP U) "Laplace form"))
            (T (RETURN (INVLAP_FIXUP (CAR R))))))) 
(PUT 'INVLAP_FIXUP 'NUMBER-OF-ARGS 1) 
(PUT 'INVLAP_FIXUP 'DEFINED-ON-LINE '588) 
(PUT 'INVLAP_FIXUP 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'INVLAP_FIXUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVLAP_FIXUP (U)
    ((LAMBDA (VARSTACK*)
       (PROGN
        (PUT 'INVLAP 'SIMPFN 'SIMPIDEN)
        (SETQ U (SIMP (AEVAL* (PREPSQ U))))
        (PUT 'INVLAP 'SIMPFN 'SIMPINVLAP)
        U))
     NIL)) 
(PUT 'SIMPINVLAP* 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINVLAP* 'DEFINED-ON-LINE '596) 
(PUT 'SIMPINVLAP* 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'SIMPINVLAP* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINVLAP* (U)
    (PROG (*EXP *MCD V W *PRECISE)
      (COND ((NULL SUBFG*) (RETURN (MKSQ (CONS 'INVLAP U) 1))))
      (COND ((AND (CDDR U) (NULL (IDP (SETQ W (CADDR U))))) (GO ERR)))
      (SETQ V (CAAAR (CAR (SIMP (CADR U)))))
      (SETQ TRANSVAR* W)
      (COND ((NULL (IDP V)) (GO ERR)))
      (SETQ U (CAR U))
      (SETQ *EXP (SETQ *MCD NIL))
      (SETQ KORD* (CONS 'IL& (CONS 'LP& KORD*)))
      (PUT 'ONE 'SIMPFN 'ILSIMP1)
      (SETQ ILVAR* V)
      (COND ((NEQ V 'IL&) (SETQ KORD* (CONS V KORD*))))
      (PROG (X)
        (SETQ X DEPL*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((MEMQ V (CDR X)) (RPLACD X (CONS 'IL& (CDR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ U (INVLAP1 (LIST U V)))
      (PUT 'INVLAP 'SIMPFN 'SIMPIDEN)
      (COND (W (PROGN (SETQ LPVAR* W) (SETQ U (SUBLA (LIST (CONS 'LP& W)) U))))
            (T (SETQ LPVAR* 'LP&)))
      (COND
       ((OR *LTRIG *LHYP)
        (PROGN
         (SETQ *EXP T)
         (COND
          (*LHYP
           (PUT 'EXPT 'OPMTCH (CONS ILE3* (CONS ILE4* (GET 'EXPT 'OPMTCH))))))
         (COND
          (*LTRIG
           (PUT 'EXPT 'OPMTCH (CONS ILE1* (CONS ILE2* (GET 'EXPT 'OPMTCH))))))
         (PUT 'EXPT 'OPMTCH (CONS ILE5* (GET 'EXPT 'OPMTCH)))
         (SETQ U (SIMP (PREPSQ U)))
         (COND
          ((AND *LTRIG *LHYP)
           (PUT 'EXPT 'OPMTCH (CDR (CDDDDR (GET 'EXPT 'OPMTCH)))))
          (T (PUT 'EXPT 'OPMTCH (CDDDR (GET 'EXPT 'OPMTCH)))))))
       (T (SETQ U (RESIMP U))))
      (PROG (X)
        (SETQ X DEPL*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((MEMQ 'IL& (CDR X)) (RPLACD X (DELETE 'IL& (CDR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PUT 'ONE 'SIMPFN 'SIMPIDEN)
      (SETQ KORD* (CDDR KORD*))
      (RETURN U)
     ERR
      (MSGPRI "Invlap operator incorrect" NIL NIL NIL T))) 
(PUT 'INVLAP1 'NUMBER-OF-ARGS 1) 
(PUT 'INVLAP1 'DEFINED-ON-LINE '640) 
(PUT 'INVLAP1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'INVLAP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVLAP1 (U)
    (PROG (V W Z)
      (SETQ V (CADR U))
      (SETQ U (CAR U))
      (SETQ Z (SIMP* U))
      (COND ((NEQ (CDR Z) 1) (SETQ Z (SIMP (PREPSQ Z)))))
      (COND ((NEQ (CDR Z) 1) (REDERR (LIST U "has non-trivial denominator"))))
      (SETQ Z (CAR Z))
      (SETQ U Z)
      (COND
       ((NEQ V 'IL&)
        (PROGN
         (SETQ KORD* (CDR KORD*))
         (SETQ U (LPCHVAR (LIST (CONS V 'IL&)) U)))))
      (SETQ W (CONS NIL 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (COND
         ((OR (ATOM U) (ATOM (CAR U)))
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (CONS
                           (LIST
                            (CONS (GETPOWER (FKERN (LIST 'DELTA 'LP&)) 1) U))
                           1)))
           (SETQ U NIL)))
         (T
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (COND ((SETQ Z (ILTERM (CAR U) 1 1 NIL)) Z)
                                (T
                                 (CONS
                                  (LIST
                                   (CONS
                                    (GETPOWER
                                     (FKERN
                                      (LIST 'INVLAP
                                            (SUBLA (LIST (CONS 'IL& ILVAR*))
                                                   (PREPSQ
                                                    (CONS (LIST (CAR U)) 1)))
                                            ILVAR* TRANSVAR*))
                                     1)
                                    1))
                                  1)))))
           (SETQ U (CDR U)))))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'ILTERM 'NUMBER-OF-ARGS 4) 
(PUT 'ILTERM 'DEFINED-ON-LINE '665) 
(PUT 'ILTERM 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILTERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILTERM (U NUMF DENF ROOTL)
    (PROG (V V1 V2 W Y Z P P1)
      (SETQ V (CAR U))
      (SETQ W (CDR U))
      (SETQ V1 (CAR V))
      (SETQ V2 (CDR V))
      (COND
       ((NOT
         (DEPENDS (COND ((NOT (SFP V1)) V1) (T (PREPSQ (CONS V1 1)))) 'IL&))
        (RETURN
         (COND
          ((SETQ Z (ILFORM W NUMF DENF ROOTL))
           (MULTSQ (CONS (LIST (CONS V 1)) 1) Z))
          (T NIL)))))
      (COND
       ((ATOM V1)
        (COND ((NOT (EQUAL V1 'IL&)) (RETURN (ILUNKNOWN U NUMF DENF)))))
       ((ATOM (CAR V1))
        (RETURN
         (COND ((EQUAL (CAR V1) 'EXPT) (ILEXPT V NIL W NUMF DENF ROOTL))
               ((OR (ATOM W) (ATOM (CAR W))) (ILEXPTFN V W NUMF DENF))
               ((CDR W)
                (COND
                 ((AND (SETQ Y (ILTERM (LIST V (CAR W)) NUMF DENF ROOTL))
                       (SETQ Z (ILTERM (CONS V (CDR W)) NUMF DENF ROOTL)))
                  (ADDSQ Y Z))
                 (T NIL)))
               (T
                (ILTERM (LIST (CAAR W) (CONS V (CDAR W))) NUMF DENF ROOTL))))))
      (COND
       ((AND (NUMBERP V2) (FIXP V2))
        (COND
         ((GREATERP V2 0)
          (COND
           ((ATOM V1)
            (RETURN
             (ILFORM W
              ((LAMBDA (G138)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G138 NUMF))
                       (T (POLY-MULTF G138 NUMF))))
               (LIST (CONS V 1)))
              DENF ROOTL)))
           (T NIL)))
         (T (RETURN (ILROOT V W NUMF DENF ROOTL)))))
       (T
        (RETURN
         (ILEXPT
          (CONS
           (LIST 'EXPT (COND ((NOT (SFP V1)) V1) (T (PREPSQ (CONS V1 1))))
                 (PREPSQ (CONS V2 1)))
           1)
          NIL W NUMF DENF ROOTL))))
      (SETQ V (COND ((|:ONEP| V2) V1) (T (LIST (CONS V 1)))))
      (COND
       ((CDR V1)
        (PROGN
         (SETQ *EXP T)
         (SETQ Y (CAR (SUBF V NIL)))
         (SETQ Z Y)
         (PROG ()
          WHILELABEL
           (COND ((NOT Z) (RETURN NIL)))
           (COND ((OR (ATOM Z) (ATOM (CAR Z))) (SETQ Z NIL))
                 ((LESSP (CDAAR Z) 0)
                  (COND
                   ((DEPENDS
                     (COND ((NOT (SFP (SETQ P1 (CAAAR Z)))) P1)
                           (T (PREPSQ (CONS P1 1))))
                     'IL&)
                    (PROGN (SETQ P T) (SETQ Z NIL)))
                   (T (SETQ Z (ADDF (CDAR Z) (CDR Z))))))
                 (T (SETQ Z (ADDF (CDAR Z) (CDR Z)))))
           (GO WHILELABEL))
         (COND
          (P
           (SETQ W
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y W))
                         (T (POLY-MULTF Y W)))))
          (T
           (SETQ NUMF
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF V NUMF))
                         (T (POLY-MULTF V NUMF))))))
         (SETQ *EXP NIL)))
       (T
        (SETQ NUMF
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF V NUMF))
                      (T (POLY-MULTF V NUMF))))))
      (RETURN (ILFORM W NUMF DENF ROOTL)))) 
(PUT 'ILFORM 'NUMBER-OF-ARGS 4) 
(PUT 'ILFORM 'DEFINED-ON-LINE '725) 
(PUT 'ILFORM 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILFORM (U NUMF DENF ROOTL)
    (PROG (Y Z)
      (RETURN
       (COND
        ((OR (ATOM U) (ATOM (CAR U)))
         (COND ((SETQ Z (ILRESID NUMF DENF ROOTL)) (MULTSQ (CONS U 1) Z))
               (T NIL)))
        ((NULL (CDR U)) (ILTERM (CAR U) NUMF DENF ROOTL))
        ((AND (SETQ Y (ILTERM (CAR U) NUMF DENF ROOTL))
              (SETQ Z (ILFORM (CDR U) NUMF DENF ROOTL)))
         (ADDSQ Y Z))
        (T NIL))))) 
(PUT 'ILUNKNOWN 'NUMBER-OF-ARGS 3) 
(PUT 'ILUNKNOWN 'DEFINED-ON-LINE '739) 
(PUT 'ILUNKNOWN 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILUNKNOWN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILUNKNOWN (U NUMF DENF)
    (PROG (D Z W)
      (COND
       (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ D (CDR U)))
        (COND ((|:ONEP| D) (SETQ U (CONS (LIST U) 1)))
              (T (SETQ U (CONS (LIST (CONS (CAR U) 1)) 1)))))
       (T (SETQ U (CONS (LIST U) 1))))
      (COND ((NEQ NUMF 1) (SETQ U (MULTSQ U (CONS NUMF 1)))))
      (COND ((NEQ DENF 1) (SETQ U (MULTSQ U (CONS 1 DENF)))))
      (SETQ U (LIST 'INVLAP (PREPSQ U) 'IL& TRANSVAR*))
      (SETQ W (LIST 'INVLAP (CADR U) 'IL&))
      (COND
       ((AND (GET 'INVLAP 'OPMTCH)
             (OR (SETQ Z (OPMTCH U)) (SETQ Z (OPMTCH W))))
        (PROGN
         (SETQ *EXP T)
         (PUT 'INVLAP 'SIMPFN 'INVLAP1)
         (SETQ Z (SIMP Z))
         (SETQ *EXP NIL)
         (PUT 'INVLAP 'SIMPFN 'SIMPINVLAP))))
      (COND
       ((AND (NULL Z) *LMSG)
        (MSGPRI "Invlap for" (SUBLA (LIST (CONS 'IL& ILVAR*)) (CADR U))
                "not known" NIL NIL)))
      (RETURN
       (COND ((NULL Z) NIL)
             ((AND (OR (ATOM D) (ATOM (CAR D))) (NOT (|:ONEP| D)))
              (MULTSQ Z (CONS D 1)))
             (T Z))))) 
(PUT 'ILSIMP1 'NUMBER-OF-ARGS 1) 
(PUT 'ILSIMP1 'DEFINED-ON-LINE '766) 
(PUT 'ILSIMP1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILSIMP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ILSIMP1 (U) (COND ((ATOM (CAR U)) (CONS 1 1)) (T (MKSQ (CONS 'ONE U) 1)))) 
(PUT 'ILEXPT 'NUMBER-OF-ARGS 6) 
(PUT 'ILEXPT 'DEFINED-ON-LINE '771) 
(PUT 'ILEXPT 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILEXPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILEXPT (U V W NUMF DENF ROOTL)
    (PROG (P Q R Z L)
      (SETQ R (CDR U))
      (SETQ P (CADAR U))
      (SETQ Q (CADDAR U))
      (COND ((DEPENDS P 'IL&) (GO GAMMA)))
      (SETQ *EXP T)
      (SETQ Q (CAR (SIMP Q)))
      (COND
       ((NEQ (CAAAR Q) 'IL&)
        (PROGN
         (SETQ *MCD T)
         (SETQ Q (SUBF Q NIL))
         (SETQ *MCD NIL)
         (SETQ Q
                 ((LAMBDA (G141)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR Q) G141))
                          (T (POLY-MULTF (CAR Q) G141))))
                  (RECIPF* (CDR Q)))))))
      (COND
       ((NOT (|:ONEP| R))
        (SETQ Q
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF Q R))
                      (T (POLY-MULTF Q R))))))
      (SETQ *EXP NIL)
      (COND
       ((NOT
         (AND (EQUAL (CAAAR Q) 'IL&) (|:ONEP| (CDAAR Q)) (MINUSF (CDAR Q))))
        (RETURN
         (COND ((NULL V) (ILUNKNOWN (CONS U W) NUMF DENF))
               (T (ILUNKNOWN (LIST U (CONS V W)) NUMF DENF))))))
      (COND
       ((SETQ R (CDR Q))
        (PROGN
         (SETQ Q (LIST (CAR Q)))
         (SETQ R
                 (CONS
                  (LIST (CONS (CONS (LIST 'EXPT P (PREPSQ (CONS R 1))) 1) 1))
                  1)))))
      (COND
       ((NEQ P 'E)
        (SETQ Q
                ((LAMBDA (G143)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF Q G143))
                         (T (POLY-MULTF Q G143))))
                 (LIST (CONS (GETPOWER (FKERN (LIST 'LOG P)) 1) 1))))))
      (SETQ Z
              (COND ((NULL V) (ILFORM W NUMF DENF ROOTL))
                    (T (ILTERM (CONS V W) NUMF DENF ROOTL))))
      (COND ((NULL Z) (RETURN NIL)))
      (SETQ L (LIST 'PLUS 'LP& (PREPSQ (CONS (CDAR Q) 1))))
      (SETQ Z (SUBF (CAR Z) (LIST (CONS 'LP& L))))
      (COND
       (*LIONE
        (SETQ Z
                (MULTSQ Z
                        (CONS
                         (LIST (CONS (GETPOWER (FKERN (LIST 'ONE L)) 1) 1))
                         1)))))
      (RETURN (COND (R (MULTSQ R Z)) (T Z)))
     GAMMA
      (RETURN
       (COND
        ((NULL V)
         (COND ((OR (ATOM W) (ATOM (CAR W))) (ILEXPTFN U W NUMF DENF))
               ((CDR W)
                (COND
                 ((AND (SETQ Z (ILEXPT U NIL (LIST (CAR W)) NUMF DENF ROOTL))
                       (SETQ L (ILEXPT U NIL (CDR W) NUMF DENF ROOTL)))
                  (ADDSQ Z L))
                 (T NIL)))
               ((NOT
                 (DEPENDS
                  (COND
                   (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                     (SETQ L (CAAAR W)))
                    L)
                   (T (PREPSQ (CONS L 1))))
                  'IL&))
                (COND
                 ((SETQ Z (ILEXPT U NIL (CDAR W) NUMF DENF ROOTL))
                  (MULTSQ (CONS (LIST (CONS (CAAR W) 1)) 1) Z))
                 (T NIL)))
               ((AND (NOT (ATOM L)) (EQUAL (CAR L) 'EXPT))
                (ILEXPT (CAAR W) U (CDAR W) NUMF DENF ROOTL))
               ((OR (ATOM L) (NOT (ATOM (CAR L))))
                (ILTERM (LIST (CAAR W) (CONS U (CDAR W))) NUMF DENF ROOTL))
               (T (ILUNKNOWN (CONS U W) NUMF DENF))))
        (T (ILUNKNOWN (LIST U (CONS V W)) NUMF DENF)))))) 
(PUT 'ILEXPTFN 'NUMBER-OF-ARGS 4) 
(PUT 'ILEXPTFN 'DEFINED-ON-LINE '821) 
(PUT 'ILEXPTFN 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILEXPTFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILEXPTFN (U V NUMF DENF)
    (PROG (EX DG FN K A B Y D)
      (SETQ EX (CAR U))
      (SETQ DG (CDR U))
      (SETQ FN (CAR EX))
      (COND ((NEQ FN 'EXPT) (GO UNK)))
      (SETQ D (CADDR EX))
      (COND ((ATOM (SETQ EX (CADR EX))) (SETQ K T)))
      (SETQ *EXP T)
      (SETQ EX (CAR (SIMP EX)))
      (SETQ DG (MULTD DG (CAR (SIMP D))))
      (SETQ A (CDAR EX))
      (COND
       ((NOT (AND (OR (ATOM A) (ATOM (CAR A))) (|:ONEP| A)))
        (PROGN
         (SETQ EX
                 ((LAMBDA (G145)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF EX G145))
                          (T (POLY-MULTF EX G145))))
                  (RECIPF* A)))
         (SETQ A
                 (LIST
                  (CONS
                   (GETPOWER
                    (FKERN
                     (LIST 'EXPT (PREPSQ (CONS A 1)) (PREPSQ (CONS DG 1))))
                    1)
                   1))))))
      (SETQ B (CDR EX))
      (SETQ *EXP NIL)
      (COND
       ((OR (NEQ (CAAAR EX) 'IL&) (NEQ (CDAAR EX) 1)
            (DEPENDS (PREPSQ (CONS B 1)) 'IL&))
        (GO UNK)))
      (COND ((AND (EQUAL NUMF 1) (EQUAL DENF 1)) (GO RET)))
      (SETQ Y
              ((LAMBDA (G148 G149)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G148 G149))
                       (T (POLY-MULTF G148 G149))))
               ((LAMBDA (G147)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF NUMF G147))
                        (T (POLY-MULTF NUMF G147))))
                (LIST
                 (CONS
                  (GETPOWER
                   (FKERN
                    (LIST 'EXPT (PREPSQ (CONS EX 1)) (PREPSQ (CONS DG 1))))
                   1)
                  1)))
               (RECIPF* DENF)))
      (COND
       ((OR (CDR Y) (NEQ (CDAR (CDAR Y)) 1) (NEQ (CAR (CAAAR (CDAR Y))) 'EXPT)
            (AND (NOT K) (NEQ (CAAAR Y) EX))
            (AND K (NEQ (CAAAR Y) (CAAAR EX))))
        (GO UNK)))
      (SETQ DG (ADDD (CDAAR Y) DG))
     RET
      (COND ((MINUSF DG) (SETQ D (PREPSQ (CONS (NEGF DG) 1)))) (T (GO UNK)))
      (COND
       ((SETQ Y (GET FN 'ILFN))
        (SETQ Y (CAR (SIMP (SUBLA (LIST (CONS 'D D)) Y)))))
       (T (GO UNK)))
      (COND
       (B
        (SETQ Y
                (MULTD V
                       ((LAMBDA (G153)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y G153))
                                (T (POLY-MULTF Y G153))))
                        (LIST
                         (CONS
                          (GETPOWER
                           (FKERN
                            (LIST 'EXPT 'E
                                  (PREPSQ
                                   (CONS
                                    ((LAMBDA (G150 G151)
                                       (COND
                                        (*PHYSOP-LOADED
                                         (PHYSOP-MULTF G150 G151))
                                        (T (POLY-MULTF G150 G151))))
                                     (LIST (CONS (CONS 'LP& 1) 1)) (NEGF B))
                                    1))))
                           1)
                          1))))))
       (T (SETQ Y (MULTD V Y))))
      (RETURN
       (COND ((AND (OR (ATOM A) (ATOM (CAR A))) (|:ONEP| A)) (CONS Y 1))
             (T
              (CONS
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF A Y)) (T (POLY-MULTF A Y)))
               1))))
     UNK
      (RETURN (ILUNKNOWN (CONS U V) NUMF DENF)))) 
(PUT 'EXPT 'ILFN '(QUOTIENT (EXPT LP& (PLUS D (MINUS 1))) (GAMMA D))) 
(PUT 'ADDROOTL 'NUMBER-OF-ARGS 3) 
(PUT 'ADDROOTL 'DEFINED-ON-LINE '857) 
(PUT 'ADDROOTL 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ADDROOTL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDROOTL (ROOT MLTPL ROOTL)
    (PROG (PARR)
      (SETQ PARR (ASSOC ROOT ROOTL))
      (COND
       (PARR
        (PROGN
         (SETQ MLTPL (PLUS MLTPL (CDR PARR)))
         (SETQ ROOTL (DELETE PARR ROOTL)))))
      (RETURN (CONS (CONS ROOT MLTPL) ROOTL)))) 
(PUT 'RECIPF* 'NUMBER-OF-ARGS 1) 
(PUT 'RECIPF* 'DEFINED-ON-LINE '866) 
(PUT 'RECIPF* 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'RECIPF* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RECIPF* (U)
    (PROG (D)
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (COND ((|:ONEP| U) (RETURN 1)) ((|:ONEP| (NEGF U)) (RETURN (MINUS 1)))
              ((FIELDP U) NIL)
              ((SETQ D (GET DMODE* 'I2D)) (SETQ U (APPLY1 D U)))
              (T (SETQ U (MKRATNUM U)))))
       (T
        (RETURN
         (COND ((CDR U) (LIST (CONS (CONS U (MINUS 1)) 1)))
               (T
                ((LAMBDA (G154 G155)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G154 G155))
                         (T (POLY-MULTF G154 G155))))
                 (LIST (CONS (CONS (CAAAR U) (MINUS (CDAAR U))) 1))
                 (RECIPF* (CDAR U))))))))
      (RETURN (DCOMBINE 1 U 'QUOTIENT)))) 
(PUT 'ILROOT 'NUMBER-OF-ARGS 5) 
(PUT 'ILROOT 'DEFINED-ON-LINE '879) 
(PUT 'ILROOT 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILROOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILROOT (U V NUMF DENF ROOTL)
    (PROG (DG EX A B C Z X1 X2)
      (SETQ DG (MINUS (CDR U)))
      (SETQ EX (CAR U))
      (COND
       ((ATOM EX)
        (RETURN
         (ILFORM V NUMF
          ((LAMBDA (G156)
             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G156 DENF))
                   (T (POLY-MULTF G156 DENF))))
           (LIST (CONS (CONS 'IL& DG) 1)))
          (ADDROOTL NIL DG ROOTL)))))
      (COND ((ATOM (CAR EX)) (RETURN (ILUNKNOWN (CONS U V) NUMF DENF))))
      ((LAMBDA (*EXP) (SETQ EX (SUBF EX NIL))) T)
      (COND
       ((NOT (DEPENDS (PREPSQ EX) 'IL&))
        (RETURN
         (COND
          ((SETQ Z (ILFORM V NUMF DENF ROOTL))
           (MULTSQ (CONS (LIST (CONS U 1)) 1) Z))
          (T NIL)))))
      (SETQ EX (CAR EX))
      (COND ((GREATERP (CDAAR EX) 2) (RETURN (IL3POL U V NUMF DENF ROOTL))))
      (SETQ A (CDAR EX))
      (COND
       ((DEPENDS (PREPSQ (CONS A 1)) 'IL&)
        (RETURN (ILUNKNOWN (CONS U V) NUMF DENF))))
      (COND
       ((NOT (AND (OR (ATOM A) (ATOM (CAR A))) (|:ONEP| A)))
        ((LAMBDA (*EXP)
           (PROGN
            (SETQ A (RECIPF* A))
            (SETQ EX
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF EX A))
                          (T (POLY-MULTF EX A))))
            (COND ((GREATERP DG 1) (SETQ A (EXPTF A DG))))))
         T)))
      (COND
       ((EQUAL (CDAAR EX) 2)
        (PROGN
         (COND
          ((SETQ B (CDR EX))
           (COND ((OR (ATOM B) (ATOM (CAR B))) (PROGN (SETQ C B) (SETQ B NIL)))
                 ((EQUAL (CAAAR B) 'IL&)
                  (PROGN (SETQ C (CDR B)) (SETQ B (CDAR B))))
                 (T (PROGN (SETQ C B) (SETQ B NIL)))))
          (T (SETQ C NIL)))
         (COND
          ((OR (DEPENDS (PREPSQ (CONS B 1)) 'IL&)
               (DEPENDS (PREPSQ (CONS C 1)) 'IL&))
           (RETURN (ILUNKNOWN (CONS U V) NUMF DENF))))
         (COND
          ((AND (NULL B) (NULL C))
           (PROGN
            (SETQ ROOTL (ADDROOTL NIL (TIMES 2 DG) ROOTL))
            (SETQ DENF
                    ((LAMBDA (G544)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 DENF))
                             (T (POLY-MULTF G544 DENF))))
                     (LIST (CONS (CONS 'IL& (TIMES 2 DG)) 1))))))
          (T
           (PROGN
            ((LAMBDA (*EXP)
               (PROGN
                (SETQ B (MULTD (CONS '|:RN:| (CONS (MINUS 1) 2)) B))
                (SETQ C
                        (SIMP
                         (LIST 'SQRT
                               (PREPSQ
                                (CONS
                                 (ADDF
                                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF B B))
                                        (T (POLY-MULTF B B)))
                                  (NEGF C))
                                 1)))))
                (COND
                 ((FIXP (CDR C))
                  (SETQ C (MULTD (CONS '|:RN:| (CONS 1 (CDR C))) (CAR C))))
                 (T (REDERR (LIST "invalid laplace denominator" (CDR C)))))
                (SETQ X1 (ADDF B C))
                (SETQ X2 (ADDF B (NEGF C)))))
             T)
            (COND
             ((EQUAL X1 X2)
              (PROGN
               (SETQ ROOTL (ADDROOTL X1 (TIMES 2 DG) ROOTL))
               (SETQ X1 (CONS (CONS (CONS 'IL& 1) 1) (NEGF X1)))
               (SETQ DENF
                       ((LAMBDA (G544)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 DENF))
                                (T (POLY-MULTF G544 DENF))))
                        (LIST (CONS (CONS X1 (TIMES 2 DG)) 1))))))
             (T
              (PROGN
               (SETQ ROOTL (ADDROOTL X2 DG (ADDROOTL X1 DG ROOTL)))
               (SETQ X1 (CONS (CONS (CONS 'IL& 1) 1) (NEGF X1)))
               (SETQ X2 (CONS (CONS (CONS 'IL& 1) 1) (NEGF X2)))
               (COND
                ((NOT (|:ONEP| DG))
                 (PROGN
                  (SETQ X1 (LIST (CONS (CONS X1 DG) 1)))
                  (SETQ X2 (LIST (CONS (CONS X2 DG) 1))))))
               (SETQ DENF
                       ((LAMBDA (G163)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF X2 G163))
                                (T (POLY-MULTF X2 G163))))
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF X1 DENF))
                              (T (POLY-MULTF X1 DENF)))))))))))))
       (T
        (PROGN
         (COND
          ((SETQ B (CDR EX))
           (PROGN
            (SETQ ROOTL (ADDROOTL (NEGF B) DG ROOTL))
            (SETQ DENF
                    (COND
                     ((|:ONEP| DG)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF EX DENF))
                            (T (POLY-MULTF EX DENF))))
                     (T
                      ((LAMBDA (G544)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 DENF))
                               (T (POLY-MULTF G544 DENF))))
                       (LIST (CONS (CONS EX DG) 1))))))))
          (T
           (PROGN
            (SETQ ROOTL (ADDROOTL NIL DG ROOTL))
            (SETQ DENF
                    ((LAMBDA (G544)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 DENF))
                             (T (POLY-MULTF G544 DENF))))
                     (LIST (CONS (CONS 'IL& DG) 1))))))))))
      (SETQ Z (ILFORM V NUMF DENF ROOTL))
      (RETURN
       (COND ((AND (OR (ATOM A) (ATOM (CAR A))) (|:ONEP| A)) Z) ((NULL Z) NIL)
             (T (MULTSQ (CONS A 1) Z)))))) 
(PUT 'IL3POL 'NUMBER-OF-ARGS 5) 
(PUT 'IL3POL 'DEFINED-ON-LINE '946) 
(PUT 'IL3POL 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'IL3POL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE IL3POL (U V NUMF DENF ROOTL)
    ((LAMBDA (*LIMITEDFACTORS)
       (PROG (A D P Y Z W)
         (COND (*ROUNDED (GO UNK)))
         (SETQ D (MINUS (CDR U)))
         (SETQ P (CAR U))
         (SETQ *EXP T)
         (SETQ *MCD T)
         (SETQ *LIMITEDFACTORS T)
         (SETQ Y P)
         (SETQ P (CONS NIL 1))
         (REMFLAG '(|:RN:|) 'FIELD)
         (PROG ()
          WHILELABEL
           (COND ((NOT Y) (RETURN NIL)))
           (COND
            ((OR (ATOM Y) (ATOM (CAR Y)))
             (PROGN (SETQ P (ADDSQ P (*D2Q1 Y))) (SETQ Y NIL)))
            (T
             (PROGN
              (SETQ A 1)
              (SETQ Z (LIST (CAR Y)))
              (PROG ()
               WHILELABEL
                (COND ((NOT (NOT (OR (ATOM Z) (ATOM (CAR Z))))) (RETURN NIL)))
                (PROGN
                 (SETQ W (CAAR Z))
                 (SETQ W
                         (COND ((KERNLP (CAR W)) (LIST (CONS W 1)))
                               (T (EXPTF (CAR W) (CDR W)))))
                 (SETQ A
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF A W))
                               (T (POLY-MULTF A W))))
                 (SETQ Z (CDAR Z)))
                (GO WHILELABEL))
              (SETQ P (ADDSQ P (MULTSQ (CONS A 1) (*D2Q1 Z))))
              (SETQ Y (CDR Y)))))
           (GO WHILELABEL))
         (FLAG '(|:RN:|) 'FIELD)
         (COND ((AND (NEQ (SETQ A (CDR P)) 1) (NEQ D 1)) (SETQ A (EXPTF A D))))
         (SETQ Z (FCTRF (CAR P)))
         (SETQ *EXP NIL)
         (SETQ *MCD NIL)
         (COND ((AND (EQUAL (LENGTH Z) 2) (EQUAL (CDR (CADR Z)) 1)) (GO UNK)))
         (COND ((NEQ (CAR Z) 1) (ERRACH (LIST (CAR Z) "found in IL3POL"))))
         (SETQ Z (CDR Z))
         (SETQ Y V)
         (PROG ()
          WHILELABEL
           (COND ((NOT Z) (RETURN NIL)))
           (PROGN
            (SETQ P (CAAR Z))
            (COND ((NEQ (CDR (CAR Z)) 1) (SETQ P (EXPTF P (CDR (CAR Z))))))
            (COND ((NEQ D 1) (SETQ P (EXPTF P D))))
            (SETQ Y
                    ((LAMBDA (G169)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y G169))
                             (T (POLY-MULTF Y G169))))
                     (RECIPF* P)))
            (SETQ Z (CDR Z)))
           (GO WHILELABEL))
         (SETQ Y (ILFORM Y NUMF DENF ROOTL))
         (COND ((NULL Y) (GO UNK))
               (T (RETURN (COND ((EQUAL A 1) Y) (T (MULTSQ (CONS A 1) Y))))))
        UNK
         (RETURN (ILUNKNOWN (CONS U V) NUMF DENF))))
     *LIMITEDFACTORS)) 
(PUT 'ILRESID 'NUMBER-OF-ARGS 3) 
(PUT 'ILRESID 'DEFINED-ON-LINE '990) 
(PUT 'ILRESID 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT 'ILRESID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ILRESID (NUMF DENF ROOTL)
    (PROG (N D NDEG DDEG M X Y Z W)
      (SETQ *EXP T)
      (SETQ N (CAR (SUBF NUMF NIL)))
      (SETQ *EXP NIL)
      (SETQ Z (CONS NIL 1))
      (SETQ W (CONS NIL 1))
      (SETQ X N)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND X (NOT (OR (ATOM X) (ATOM (CAR X)))))) (RETURN NIL)))
        (PROGN
         (SETQ Y (CAR X))
         (SETQ X (CDR X))
         (COND
          ((OR (DEPENDS (PREPSQ (CONS (CDR Y) 1)) 'IL&)
               (AND (NEQ (CAAR Y) 'IL&) (DEPENDS (CAAR Y) 'IL&)))
           (COND
            ((SETQ Z (ILTERM Y 1 DENF ROOTL))
             (PROGN (SETQ W (ADDSQ W Z)) (SETQ N (DELETE Y N))))
            (T (SETQ X NIL))))))
        (GO WHILELABEL))
      (COND ((NULL Z) (RETURN NIL)))
      (SETQ NDEG
              (COND
               ((AND (NOT (OR (ATOM N) (ATOM (CAR N)))) (EQUAL (CAAAR N) 'IL&))
                (CDAAR N))
               (T 0)))
      (SETQ *EXP T)
      (SETQ D (CAR (SUBF DENF NIL)))
      (SETQ *EXP NIL)
      (SETQ DDEG
              (COND
               ((AND (NOT (OR (ATOM D) (ATOM (CAR D)))) (EQUAL (CAAAR D) 'IL&))
                (CDAAR D))
               (T 0)))
      (COND ((LESSP NDEG DDEG) (GO RESID)))
      (SETQ *EXP T)
      (SETQ Y (QREMF N D))
      (SETQ *EXP NIL)
      (SETQ N (CDR Y))
      (SETQ X (CAR Y))
      (PROG ()
       WHILELABEL
        (COND ((NOT X) (RETURN NIL)))
        (COND
         ((OR (ATOM X) (ATOM (CAR X)))
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (CONS
                           (LIST (CONS (GETPOWER (FKERN '(DELTA LP&)) 1) X))
                           1)))
           (SETQ X NIL)))
         ((NEQ (CAAAR X) 'IL&)
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (MULTSQ
                           (CONS
                            (LIST (CONS (GETPOWER (FKERN '(DELTA LP&)) 1) 1))
                            1)
                           (CONS (LIST (CAR X)) 1))))
           (SETQ X (CDR X))))
         (T
          (PROGN
           (SETQ W
                   (ADDSQ W
                          (MULTSQ
                           (CONS
                            (LIST
                             (CONS
                              (GETPOWER
                               (FKERN
                                (LIST 'DF (LIST 'DELTA 'LP&) 'LP& (CDAAR X)))
                               1)
                              1))
                            1)
                           (CONS (CDAR X) 1))))
           (SETQ X (CDR X)))))
        (GO WHILELABEL))
     RESID
      (COND ((NULL ROOTL) (RETURN W)))
      (SETQ X (CAAR ROOTL))
      (SETQ M (CDAR ROOTL))
      (COND ((NULL X) (SETQ Y (LIST (CONS (CONS 'IL& M) 1))))
            (T
             (PROGN
              (SETQ Y (CONS (CONS (CONS 'IL& 1) 1) (NEGF X)))
              (COND ((NEQ M 1) (SETQ Y (LIST (CONS (CONS Y M) 1))))))))
      (SETQ *EXP T)
      (SETQ Y (CAR (SUBF Y NIL)))
      (SETQ Y (CAR (QREMF D Y)))
      (SETQ *EXP NIL)
      (SETQ Z
              ((LAMBDA (G544)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 N))
                       (T (POLY-MULTF G544 N))))
               (LIST (CONS (CONS '(EXPT E (TIMES IL& LP&)) 1) 1))))
      (SETQ Y (RECIPF* Y))
      (SETQ Z
              (CONS
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z Y)) (T (POLY-MULTF Z Y)))
               1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (SETQ M (DIFFERENCE M 1)) 0)) (RETURN NIL)))
        (SETQ Z (DIFFSQ Z 'IL&))
        (GO WHILELABEL))
      (SETQ X (COND ((NULL X) 0) (T (PREPSQ (CONS X 1)))))
      (SETQ *EXP T)
      (SETQ Z (SUBF (CAR Z) (LIST (CONS 'IL& X))))
      (COND
       ((NOT (DEPENDS (PREPSQ Z) 'LP&))
        (SETQ Z
                (MULTSQ Z
                        (CONS (LIST (CONS (GETPOWER (FKERN '(ONE LP&)) 1) 1))
                              1)))))
      (COND
       ((GREATERP (SETQ M (CDAR ROOTL)) 2)
        (PROG ()
         WHILELABEL
          (COND ((NOT (GREATERP (SETQ M (DIFFERENCE M 1)) 1)) (RETURN NIL)))
          (SETQ Z
                  (CONS
                   ((LAMBDA (G173)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR Z) G173))
                            (T (POLY-MULTF (CAR Z) G173))))
                    (CONS '|:RN:| (CONS 1 M)))
                   1))
          (GO WHILELABEL))))
      (SETQ W (ADDSQ W Z))
      (SETQ *EXP NIL)
      (SETQ ROOTL (CDR ROOTL))
      (GO RESID))) 
(PUT '*D2Q1 'NUMBER-OF-ARGS 1) 
(PUT '*D2Q1 'DEFINED-ON-LINE '1039) 
(PUT '*D2Q1 'DEFINED-IN-FILE 'LAPLACE/LAPLACE.RED) 
(PUT '*D2Q1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *D2Q1 (U)
    (COND ((NUMBERP U) (COND ((ZEROP U) (CONS NIL 1)) (T (CONS U 1))))
          ((AND (EQCAR U '|:RN:|) *MCD) (CDR U)) ((|:ZEROP| U) (CONS NIL 1))
          (T (CONS U 1)))) 
(ENDMODULE) 
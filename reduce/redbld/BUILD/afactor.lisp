(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'AFACTOR)) 
(FLUID
 '(*GALOIS *NOEXTEND *SQFREE *TRFIELD AFACTORVAR LISTOFNEWSQRTS MONICPART
   VARLIST ZLIST SQRTLIST SQRTFLAG INDEXLIST)) 
(SWITCH (LIST 'TRFIELD)) 
(EXPORTS (LIST 'AFACTOR 'JFACTOR)) 
(IMPORTS
 (LIST 'EXPTF 'ORDOP '*MULTF 'ADDF 'MAKEMAINVAR 'ALGEBRAICSF 'DIVSF 'CONTENTS)) 
(IMPORTS (LIST 'QUOTF* 'NEGF 'SQFR-NORM2 'PREPF 'ALGINT-SUBF '*Q2F)) 
(IMPORTS (LIST 'PRINTSF)) 
(PUT 'AFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'AFACTOR 'DEFINED-ON-LINE '46) 
(PUT 'AFACTOR 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'AFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AFACTOR (U V)
    (PROG (AFACTORVAR *NOEXTEND *SQFREE)
      (SETQ *NOEXTEND T)
      (SETQ AFACTORVAR V)
      (COND
       (*TRFIELD
        (PROGN
         (PRINC "We must factorise the following over: ")
         (PROG (U)
           (SETQ U LISTOFNEWSQRTS)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U) (PROGN (PRINC U) (PRINC " "))) (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (TERPRI)
         (PRINTSF U))))
      (SETQ V (ALGFACTOR U))
      (COND
       (*TRFIELD
        (PROGN
         (PROGN (PRIN2 "factorizes as ") (TERPRI) "factorizes as ")
         (MAPC V (FUNCTION PRINTSF)))))
      (RETURN V))) 
(PUT 'ALGFACTOR2 'NUMBER-OF-ARGS 2) 
(PUT 'ALGFACTOR2 'DEFINED-ON-LINE '67) 
(PUT 'ALGFACTOR2 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'ALGFACTOR2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGFACTOR2 (F A)
    (COND
     ((NULL A)
      (PROG (U FORALL-RESULT FORALL-ENDPTR)
        (SETQ U (CDR (FCTRF F)))
        (COND ((NULL U) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (COND ((EQUAL (CDR U) 1) (CAR U))
                                  (T
                                   (INTERR
                                    "repeated factors found while processing algebraics"))))
                          (CAR U))
                         NIL)))
       LOOPLABEL
        (SETQ U (CDR U))
        (COND ((NULL U) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (U)
                    (COND ((EQUAL (CDR U) 1) (CAR U))
                          (T
                           (INTERR
                            "repeated factors found while processing algebraics"))))
                  (CAR U))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     ((ALGEBRAICSF F) (ALGFACTOR3 F A))
     (T
      (PROG (W)
        (COND
         (*TRFIELD
          (PROGN
           (PRINC "to be factorized over ")
           (PROG (U)
             (SETQ U A)
            LAB
             (COND ((NULL U) (RETURN NIL)))
             ((LAMBDA (U) (PROGN (PRINC U) (PRINC " "))) (CAR U))
             (SETQ U (CDR U))
             (GO LAB))
           (TERPRI)
           (PRINTSF F))))
        (COND
         ((AND (NEQ *GALOIS 2) (NUMBERP (CDR F))
               (NOT (NUMBERP (CADR (CAR A)))))
          (RETURN (ALGFACTOR2 F (CDR A)))))
        (SETQ W (ALGFACTOR2 F NIL))
        (COND ((AND W (NULL (CDR W))) (RETURN (ALGFACTOR3 F A)))
              (T (RETURN (CONS 'PARTIAL W)))))))) 
(PUT 'ALGFACTOR3 'NUMBER-OF-ARGS 2) 
(PUT 'ALGFACTOR3 'DEFINED-ON-LINE '94) 
(PUT 'ALGFACTOR3 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'ALGFACTOR3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGFACTOR3 (F A)
    (PROG (FF W GG H P)
      (SETQ W (SQFR-NORM2 F (CAAAR F) (CAR A)))
      (SETQ *SQFREE (CAR W))
      (SETQ W (CDR W))
      (SETQ FF (ALGFACTOR2 *SQFREE (CDR A)))
      (COND ((NULL FF) (RETURN (LIST F)))
            ((EQ (CAR FF) 'PARTIAL)
             (PROGN (SETQ P 'PARTIAL) (SETQ FF (CDR FF)))))
      (COND ((NULL (CDR FF)) (RETURN (LIST F))))
      (SETQ A (CAR A))
      (SETQ GG (CADR W))
      (SETQ W (LIST (LIST AFACTORVAR 'PLUS AFACTORVAR (PREPF (CAR W)))))
      (SETQ H
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U FF)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (*Q2F
                                     (ALGINT-SUBF (GCDINONEVAR U GG AFACTORVAR)
                                      W)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (*Q2F
                             (ALGINT-SUBF (GCDINONEVAR U GG AFACTORVAR) W)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((EQ P 'PARTIAL) (SETQ H (CONS P H))))
      (RETURN H))) 
(PUT 'ALGFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'ALGFACTOR 'DEFINED-ON-LINE '113) 
(PUT 'ALGFACTOR 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'ALGFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGFACTOR (U)
    (PROG (A AA Z W MONICPART)
      (SETQ Z (MAKEMAINVAR U AFACTORVAR))
      (COND ((IEQUAL (CDAAR Z) 1) (RETURN (LIST U))))
      (SETQ Z (CDAR Z))
      (COND ((IEQUAL Z 1) (GO MONIC)))
      (COND ((ALGEBRAICSF Z) (SETQ U (*MULTF U (CAR (SQRT2TOP (CONS 1 Z)))))))
      (SETQ U (QUOTF* U (CONTENTS U AFACTORVAR)))
      (SETQ Z (MAKEMAINVAR U AFACTORVAR))
      (COND
       ((NEQ (CDAR Z) 1)
        (COND ((IEQUAL (CDAR Z) (MINUS 1)) (SETQ U (NEGF U)))
              (T (PROGN (SETQ W (CDAR Z)) (SETQ U (MAKEMONIC Z)))))))
     MONIC
      (SETQ AA LISTOFNEWSQRTS)
      (COND ((ALGEBRAICSF U) (GO NORMAL)))
      (SETQ A (CDR AA))
      (SETQ Z T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A Z)) (RETURN NIL)))
        (PROG (ALG V)
          (SETQ ALG (CAR A))
          (SETQ A (CDR A))
          (SETQ V (ALGFACTOR3 U (LIST ALG)))
          (COND ((NULL (CDR V)) (RETURN NIL)))
          (COND ((EQ (CAR V) 'PARTIAL) (SETQ V (CDR V))))
          (SETQ A (MAPCAN V (FUNCTION ALGFACTOR)))
          (SETQ Z NIL))
        (GO WHILELABEL))
      (SETQ MONICPART W)
      (COND
       ((NULL Z)
        (COND ((NULL W) (RETURN A))
              (T
               (RETURN
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J A)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (DEMONISE J)) (CAR J))
                                        NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (DEMONISE J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
     NORMAL
      (SETQ Z (ALGFACTOR2 U AA))
      (SETQ MONICPART W)
      (COND
       ((OR (NULL (CDR Z)) (NEQ (CAR Z) 'PARTIAL))
        (COND ((NULL W) (RETURN Z))
              (T
               (RETURN
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J Z)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (DEMONISE J)) (CAR J))
                                        NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (DEMONISE J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (COND ((NULL W) (RETURN (MAPCAN (CDR Z) (FUNCTION ALGFACTOR))))
            (T
             (RETURN
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U Z)
               STARTOVER
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (U) (ALGFACTOR (DEMONISE U))) (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ U (CDR U))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (U) (ALGFACTOR (DEMONISE U))) (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ U (CDR U))
                (GO LOOPLABEL))))))) 
(PUT 'DEMONISE 'NUMBER-OF-ARGS 1) 
(PUT 'DEMONISE 'DEFINED-ON-LINE '174) 
(PUT 'DEMONISE 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'DEMONISE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEMONISE (U)
    (COND ((ATOM U) U)
          ((EQ AFACTORVAR (CAAAR U))
           (ADDF (DEMONISE (CDR U))
                 (*MULTF (CONS (CAR U) NIL) (EXPTF MONICPART (CDAAR U)))))
          ((ORDOP AFACTORVAR (CAAAR U)) U)
          (T
           (ADDF (DEMONISE (CDR U))
                 (*MULTF (LIST (CONS (CAAR U) 1)) (DEMONISE (CDAR U))))))) 
(PUT 'GCDINONEVAR 'NUMBER-OF-ARGS 3) 
(PUT 'GCDINONEVAR 'DEFINED-ON-LINE '186) 
(PUT 'GCDINONEVAR 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'GCDINONEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GCDINONEVAR (U V X)
    (COND ((NULL U) V) ((NULL V) U)
          (T
           (PROG (U1 V1 Z W)
             (SETQ U1 (STT U X))
             (SETQ V1 (STT V X))
            LOOP
             (COND ((GREATERP (CAR U1) (CAR V1)) (GO OK)))
             (SETQ Z U1)
             (SETQ U1 V1)
             (SETQ V1 Z)
             (SETQ Z U)
             (SETQ U V)
             (SETQ V Z)
            OK
             (COND ((IEQUAL (CAR V1) 0) (INTERR "Coprimeness in gcd")))
             (SETQ Z (GCDF (CDR U1) (CDR V1)))
             (SETQ W ((LAMBDA (*EXP) (QUOTF1 (CDR U1) Z)) T))
             (COND
              ((NEQ (CAR U1) (CAR V1))
               (SETQ W
                       (*MULTF W
                               (LIST
                                (CONS
                                 (GETPOWER (FKERN X)
                                           (DIFFERENCE (CAR U1) (CAR V1)))
                                 1))))))
             (SETQ U
                     (ADDF (*MULTF V W)
                           (*MULTF U
                                   (NEGF
                                    ((LAMBDA (*EXP) (QUOTF1 (CDR V1) Z)) T)))))
             (COND ((NULL U) (RETURN V)))
             (SETQ U1 (STT U X))
             (GO LOOP))))) 
(PUT 'MAKEMONIC 'NUMBER-OF-ARGS 1) 
(PUT 'MAKEMONIC 'DEFINED-ON-LINE '216) 
(PUT 'MAKEMONIC 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'MAKEMONIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKEMONIC (U)
    (PROG (V W X XX)
      (SETQ V (CAAAR U))
      (SETQ X (CDAR U))
      (SETQ XX 1)
      (SETQ W (LIST (CONS (CAAR U) 1)))
      (SETQ U (CDR U))
      (PROG (I)
        (SETQ I (ISUB1 (CDAAR W)))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROG ()
          (COND ((ATOM U) (GO NEXT)))
          (COND ((NEQ (CAAAR U) V) (GO NEXT)))
          (COND
           ((IEQUAL (CDAAR U) I)
            (SETQ W
                    (ADDF W
                          (*MULTF (CDAR U)
                                  (*MULTF (LIST (CONS (CAAR U) 1)) XX))))))
          (SETQ U (CDR U))
         NEXT
          (SETQ XX (*MULTF X XX)))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SETQ W (ADDF W (*MULTF U XX)))
      (RETURN W))) 
(PUT 'JFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'JFACTOR 'DEFINED-ON-LINE '241) 
(PUT 'JFACTOR 'DEFINED-IN-FILE 'ALGINT/AFACTOR.RED) 
(PUT 'JFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JFACTOR (SF VAR)
    (PROG (VARLIST ZLIST INDEXLIST SQRTLIST SQRTFLAG PRIM RES ANSWER U V X Y)
      (SETQ PRIM (JSQFREE SF VAR))
      (SETQ INDEXLIST (CREATEINDICES ZLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL PRIM))) (RETURN NIL)))
        (PROGN
         (SETQ X (CAR PRIM))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (NULL X))) (RETURN NIL)))
           (PROGN
            (SETQ Y (FACBYPP (CAR X) VARLIST))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (NULL Y))) (RETURN NIL)))
              (PROGN
               (SETQ RES (APPEND (INT-FAC (CAR Y)) RES))
               (SETQ Y (CDR Y)))
              (GO WHILELABEL))
            (SETQ X (CDR X)))
           (GO WHILELABEL))
         (SETQ PRIM (CDR PRIM)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT RES) (RETURN NIL)))
        (PROGN
         (COND
          ((EQ (CAAR RES) 'LOG)
           (PROGN
            (SETQ U (CDAR RES))
            (SETQ U
                    (*MULTSQ (CONS (CAR U) 1)
                             (CONS 1 (CDR (STT (CAR U) VAR)))))
            (SETQ V (CDR U))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (ATOM V))) (RETURN NIL)))
              (SETQ V (CDAR V))
              (GO WHILELABEL))
            (COND
             ((AND (NUMBERP V) (GREATERP 0 V))
              (SETQ U (CONS (NEGF (CAR U)) (NEGF (CDR U))))))
            (COND ((NEQ U '(1 . 1)) (SETQ ANSWER (CONS U ANSWER))))))
          ((EQ (CAAR RES) 'ATAN) NIL)
          (T (INTERR "Unexpected term in jfactor")))
         (SETQ RES (CDR RES)))
        (GO WHILELABEL))
      (RETURN ANSWER))) 
(ENDMODULE) 
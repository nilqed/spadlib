(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RESULTNT)) 
(FLUID '(*BEZOUT *EXP KORD*)) 
(SWITCH (LIST 'BEZOUT)) 
(PUT 'RESULTANT 'SIMPFN 'SIMPRESULTANT) 
(PUT 'SIMPRESULTANT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPRESULTANT 'DEFINED-ON-LINE '56) 
(PUT 'SIMPRESULTANT 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'SIMPRESULTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPRESULTANT (U)
    (COND
     ((NEQ (LENGTH U) 3)
      (RERROR 'MATRIX 19 "Resultant called with wrong number of arguments"))
     (T
      ((LAMBDA (*EXP)
         (RESULTANTSQ (SIMP* (CAR U)) (SIMP* (CADR U)) (*A2K (CADDR U))))
       T)))) 
(PUT 'RESULTANT 'NUMBER-OF-ARGS 3) 
(PUT 'RESULTANT 'DEFINED-ON-LINE '63) 
(PUT 'RESULTANT 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'RESULTANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESULTANT (U V VAR)
    (COND ((AND (OR (ATOM U) (ATOM (CAR U))) (OR (ATOM V) (ATOM (CAR V)))) 1)
          (T
           (PROG (X)
             (SETQ KORD* (CONS VAR KORD*))
             (COND
              ((AND (NULL (OR (ATOM U) (ATOM (CAR U))))
                    (NULL (EQ (CAAAR U) VAR)))
               (SETQ U (REORDER U))))
             (COND
              ((AND (NULL (OR (ATOM V) (ATOM (CAR V))))
                    (NULL (EQ (CAAAR V) VAR)))
               (SETQ V (REORDER V))))
             (SETQ X
                     (COND (*BEZOUT (BEZOUT_RESULTANT U V VAR))
                           (T (POLYRESULTANTF U V VAR))))
             (SETKORDER (CDR KORD*))
             (RETURN X))))) 
(PUT 'RESULTANTSQ 'NUMBER-OF-ARGS 3) 
(PUT 'RESULTANTSQ 'DEFINED-ON-LINE '77) 
(PUT 'RESULTANTSQ 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'RESULTANTSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESULTANTSQ (U V VAR)
    (COND
     ((AND (OR (ATOM (CAR U)) (ATOM (CAR (CAR U))))
           (OR (ATOM (CAR V)) (ATOM (CAR (CAR V)))) (EQUAL (CDR U) 1)
           (EQUAL (CDR V) 1))
      (CONS 1 1))
     (T
      (PROG (X Y Z)
        (SETQ KORD* (CONS VAR KORD*))
        (COND
         ((AND (NULL (OR (ATOM (CAR U)) (ATOM (CAR (CAR U)))))
               (NULL (EQ (CAAAR (CAR U)) VAR)))
          (SETQ U (REORDSQ U))))
        (COND
         ((AND (NULL (OR (ATOM (CAR V)) (ATOM (CAR (CAR V)))))
               (NULL (EQ (CAAAR (CAR V)) VAR)))
          (SETQ V (REORDSQ V))))
        (COND
         ((AND (NEQ (SETQ Y (CDR U)) 1) (SMEMBER VAR Y))
          (TYPERR (PREPF Y) 'POLYNOMIAL))
         ((AND (NEQ (SETQ Z (CDR V)) 1) (SMEMBER VAR Z))
          (TYPERR (PREPF Z) 'POLYNOMIAL)))
        (SETQ U (CAR U))
        (SETQ V (CAR V))
        (COND ((SMEMBER VAR (COEFFLIST U VAR)) (TYPERR (PREPF U) 'POLYNOMIAL))
              ((SMEMBER VAR (COEFFLIST V VAR)) (TYPERR (PREPF V) 'POLYNOMIAL)))
        (SETQ X
                (COND (*BEZOUT (BEZOUT_RESULTANT U V VAR))
                      (T (POLYRESULTANTF U V VAR))))
        (COND ((NEQ Y 1) (SETQ Y (EXPTF Y (DEGR V VAR)))))
        (COND
         ((NEQ Z 1)
          (SETQ Y
                  ((LAMBDA (G571)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y G571))
                           (T (POLY-MULTF Y G571))))
                   (EXPTF Z (DEGR U VAR))))))
        (SETKORDER (CDR KORD*))
        (RETURN (CONS X Y)))))) 
(PUT 'COEFFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'COEFFLIST 'DEFINED-ON-LINE '105) 
(PUT 'COEFFLIST 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'COEFFLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFFLIST (U VAR)
    (PROG (Z)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (EQUAL (CAAAR U) VAR)))
          (RETURN NIL)))
        (PROGN (SETQ Z (CONS (CONS (CDAAR U) (CDAR U)) Z)) (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN (COND ((NULL U) Z) (T (CONS (CONS 0 U) Z)))))) 
(PUT 'POLYRESULTANTF 'NUMBER-OF-ARGS 3) 
(PUT 'POLYRESULTANTF 'DEFINED-ON-LINE '113) 
(PUT 'POLYRESULTANTF 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'POLYRESULTANTF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYRESULTANTF (U V VAR)
    (PROG (BETA CD CN DELTA GAM R S TEMP X)
      (SETQ CD (SETQ CN (SETQ R (SETQ S 1))))
      (SETQ GAM (MINUS 1))
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (OR (ATOM V) (ATOM (CAR V))))
        (RETURN 1))
       ((LESSP (CDAAR U) (CDAAR V))
        (PROGN
         (SETQ S (EXPT (MINUS 1) (TIMES (CDAAR U) (CDAAR V))))
         (SETQ TEMP U)
         (SETQ U V)
         (SETQ V TEMP))))
      (PROG ()
       WHILELABEL
        (COND ((NOT V) (RETURN NIL)))
        (PROGN
         (SETQ DELTA (DIFFERENCE (CDAAR U) (LDEGR V VAR)))
         (SETQ BETA
                 (NEGF
                  ((LAMBDA (G573)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF R G573))
                           (T (POLY-MULTF R G573))))
                   (EXPTF GAM DELTA))))
         (SETQ R (LCR V VAR))
         (COND
          ((NEQ DELTA 0)
           (SETQ GAM
                   (QUOTF* (EXPTF (NEGF R) DELTA)
                           (EXPTF GAM (DIFFERENCE DELTA 1))))))
         (SETQ TEMP U)
         (SETQ U V)
         (COND
          ((AND (NOT (EVENP (CDAAR TEMP))) (NOT (EVENP (LDEGR U VAR))))
           (SETQ S (MINUS S))))
         (SETQ V ((LAMBDA (*EXP) (QUOTF1 (PSEUDO_REMF TEMP V VAR) BETA)) T))
         (COND
          (V
           (PROGN
            (SETQ CN
                    ((LAMBDA (G575)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF CN G575))
                             (T (POLY-MULTF CN G575))))
                     (EXPTF BETA (CDAAR U))))
            (SETQ CD
                    ((LAMBDA (G577)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF CD G577))
                             (T (POLY-MULTF CD G577))))
                     (EXPTF R
                            (PLUS
                             (DIFFERENCE (TIMES (PLUS 1 DELTA) (CDAAR U))
                                         (CDAAR TEMP))
                             (LDEGR V VAR)))))
            (COND
             ((SETQ X ((LAMBDA (*EXP) (QUOTF1 CD CN)) T))
              (PROGN (SETQ CN 1) (SETQ CD X))))))))
        (GO WHILELABEL))
      (RETURN
       (COND ((AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (EQ (CAAAR U) VAR)) NIL)
             ((NEQ (CDAAR TEMP) 1)
              ((LAMBDA (*EXP)
                 (QUOTF1
                  ((LAMBDA (G581)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF S G581))
                           (T (POLY-MULTF S G581))))
                   ((LAMBDA (G579)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF CN G579))
                            (T (POLY-MULTF CN G579))))
                    (EXPTF U (CDAAR TEMP))))
                  CD))
               T))
             (T U))))) 
(PUT 'LCR 'NUMBER-OF-ARGS 2) 
(PUT 'LCR 'DEFINED-ON-LINE '146) 
(PUT 'LCR 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'LCR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LCR (U VAR)
    (COND ((OR (OR (ATOM U) (ATOM (CAR U))) (NEQ (CAAAR U) VAR)) U)
          (T (CDAR U)))) 
(PUT 'LDEGR 'NUMBER-OF-ARGS 2) 
(PUT 'LDEGR 'DEFINED-ON-LINE '149) 
(PUT 'LDEGR 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'LDEGR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDEGR (U VAR)
    (COND ((OR (OR (ATOM U) (ATOM (CAR U))) (NEQ (CAAAR U) VAR)) 0)
          (T (CDAAR U)))) 
(PUT 'PSEUDO_REMF 'NUMBER-OF-ARGS 3) 
(PUT 'PSEUDO_REMF 'DEFINED-ON-LINE '152) 
(PUT 'PSEUDO_REMF 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'PSEUDO_REMF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PSEUDO_REMF (U V VAR)
    (*Q2F
     (SIMP
      (PSEUDO-REMAINDER (LIST (MK*SQ (CONS U 1)) (MK*SQ (CONS V 1)) VAR))))) 
(PUT 'BEZOUT_RESULTANT 'NUMBER-OF-ARGS 3) 
(PUT 'BEZOUT_RESULTANT 'DEFINED-ON-LINE '155) 
(PUT 'BEZOUT_RESULTANT 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT 'BEZOUT_RESULTANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BEZOUT_RESULTANT (U V W)
    (PROG (N NM AP EP UH UT VH VT)
      (SETQ N 0)
      (SETQ NM 0)
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NULL (EQ (CAAAR U) W)))
        (RETURN
         (COND
          ((AND (NOT (OR (ATOM V) (ATOM (CAR V)))) (EQ (CAAAR V) W))
           (EXPTF U (CDAAR V)))
          (T 1))))
       ((OR (OR (ATOM V) (ATOM (CAR V))) (NULL (EQ (CAAAR V) W)))
        (RETURN (COND ((EQ (CAAAR U) W) (EXPTF V (CDAAR U))) (T 1)))))
      (SETQ N (DIFFERENCE (CDAAR V) (CDAAR U)))
      (COND
       ((LESSP N 0)
        (RETURN
         (MULTD (EXPT (MINUS 1) (TIMES (CDAAR U) (CDAAR V)))
                (BEZOUT_RESULTANT V U W)))))
      (SETQ EP 1)
      (SETQ NM (CDAAR V))
      (SETQ UH (CDAR U))
      (SETQ VH (CDAR V))
      (SETQ UT
              (COND
               ((NEQ N 0)
                ((LAMBDA (G544)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 (CDR U)))
                         (T (POLY-MULTF G544 (CDR U)))))
                 (LIST (CONS (CONS W N) 1))))
               (T (CDR U))))
      (SETQ VT (CDR V))
      (SETQ AP
              (ADDF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF UH VT))
                     (T (POLY-MULTF UH VT)))
               (NEGF
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF VH UT))
                      (T (POLY-MULTF VH UT))))))
      (SETQ EP (|B:EXTMULT| (*SF2EXB AP W) EP))
      (PROG (J)
        (SETQ J (DIFFERENCE NM 1))
       LAB
        (COND
         ((MINUSP (TIMES (MINUS 1) (DIFFERENCE (PLUS N 1) J))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (DEGR UT W) J)
           (PROGN
            (SETQ UH
                    (ADDF (CDAR UT)
                          ((LAMBDA (G584)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G584 UH))
                                   (T (POLY-MULTF G584 UH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (SETQ UT (CDR UT))))
          (T
           (SETQ UH
                   ((LAMBDA (G586)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G586 UH))
                            (T (POLY-MULTF G586 UH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (COND
          ((EQUAL (DEGR VT W) J)
           (PROGN
            (SETQ VH
                    (ADDF (CDAR VT)
                          ((LAMBDA (G588)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G588 VH))
                                   (T (POLY-MULTF G588 VH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (SETQ VT (CDR VT))))
          (T
           (SETQ VH
                   ((LAMBDA (G590)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G590 VH))
                            (T (POLY-MULTF G590 VH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (SETQ EP
                 (|B:EXTMULT|
                  (*SF2EXB
                   (ADDF
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF UH VT))
                          (T (POLY-MULTF UH VT)))
                    (NEGF
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF VH UT))
                           (T (POLY-MULTF VH UT)))))
                   W)
                  EP)))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (COND
       ((NEQ N 0)
        (PROGN
         (SETQ EP (|B:EXTMULT| (*SF2EXB U W) EP))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) J)) (RETURN NIL)))
           (SETQ EP
                   (|B:EXTMULT|
                    (*SF2EXB
                     ((LAMBDA (G544)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 U))
                              (T (POLY-MULTF G544 U))))
                      (LIST (CONS (CONS W J) 1)))
                     W)
                    EP))
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (RETURN (COND ((NULL EP) NIL) (T (CDAR EP)))))) 
(PUT '*SF2EXB 'NUMBER-OF-ARGS 2) 
(PUT '*SF2EXB 'DEFINED-ON-LINE '195) 
(PUT '*SF2EXB 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT '*SF2EXB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *SF2EXB (U V)
    (COND
     ((EQUAL (DEGR U V) 0)
      (COND ((NULL U) NIL) (T (CONS (CONS (LIST 0) U) NIL))))
     (T (CONS (CONS (LIST (CDAAR U)) (CDAR U)) (*SF2EXB (CDR U) V))))) 
(PUT '|B:EXTMULT| 'NUMBER-OF-ARGS 2) 
(PUT '|B:EXTMULT| 'DEFINED-ON-LINE '223) 
(PUT '|B:EXTMULT| 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT '|B:EXTMULT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:EXTMULT| (U V)
    (PROG (STACK X W R)
     TOP
      (COND ((OR (NULL U) (NULL V)) (PROGN (SETQ R NIL) (GO EXIT)))
            ((EQUAL V 1) (PROGN (SETQ R U) (GO EXIT))))
      (SETQ STACK (CONS (CONS U V) STACK))
      (COND ((CDR U) (SETQ U (LIST (CAR U)))))
      (SETQ V (CDR V))
      (GO TOP)
     EXIT
      (PROG ()
       WHILELABEL
        (COND ((NOT STACK) (RETURN NIL)))
        (PROGN
         (SETQ U (CAAR STACK))
         (SETQ V (CDAR STACK))
         (SETQ STACK (CDR STACK))
         (SETQ X (|B:ORDEXN| (CAR (CAAR U)) (CAAR V)))
         (COND
          (X
           (PROGN
            (SETQ W
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) (CDAR V)))
                          (T (POLY-MULTF (CDAR U) (CDAR V)))))
            (COND ((CAR X) (SETQ W (NEGF W))))
            (SETQ R
                    (CONS (CONS (CDR X) W)
                          (|B:EXTADD| R (|B:EXTMULT| (CDR U) V))))))
          (T (SETQ R (|B:EXTADD| (|B:EXTMULT| (CDR U) V) R)))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT '|B:EXTADD| 'NUMBER-OF-ARGS 2) 
(PUT '|B:EXTADD| 'DEFINED-ON-LINE '263) 
(PUT '|B:EXTADD| 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT '|B:EXTADD| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:EXTADD| (U V)
    (PROG (R W X)
     TOP
      (COND ((NULL U) (PROGN (SETQ W V) (GO EXIT)))
            ((NULL V) (PROGN (SETQ W U) (GO EXIT)))
            ((EQUAL (CAAR U) (CAAR V))
             (PROGN
              (SETQ X (ADDF (CDAR U) (CDAR V)))
              (COND ((NOT (NULL X)) (SETQ R (CONS (CONS (CAAR U) X) R))))
              (SETQ U (CDR U))
              (SETQ V (CDR V))
              (GO TOP)))
            ((|B:ORDEXP| (CAAR U) (CAAR V))
             (PROGN (SETQ R (CONS (CAR U) R)) (SETQ U (CDR U)) (GO TOP)))
            (T (PROGN (SETQ R (CONS (CAR V) R)) (SETQ V (CDR V)) (GO TOP))))
     EXIT
      (PROG ()
       WHILELABEL
        (COND ((NOT R) (RETURN NIL)))
        (PROGN (SETQ U (CDR R)) (RPLACD R W) (SETQ W R) (SETQ R U))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT '|B:ORDEXP| 'NUMBER-OF-ARGS 2) 
(PUT '|B:ORDEXP| 'DEFINED-ON-LINE '298) 
(PUT '|B:ORDEXP| 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT '|B:ORDEXP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:ORDEXP| (UL VL)
    (OR (NULL UL) (GREATERP (CAR UL) (CAR VL))
        (AND (EQUAL (CAR UL) (CAR VL)) (|B:ORDEXP| (CDR UL) (CDR VL))))) 
(PUT '|B:ORDEXN| 'NUMBER-OF-ARGS 2) 
(PUT '|B:ORDEXN| 'DEFINED-ON-LINE '319) 
(PUT '|B:ORDEXN| 'DEFINED-IN-FILE 'MATRIX/RESULTNT.RED) 
(PUT '|B:ORDEXN| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:ORDEXN| (U VL)
    (PROG (S WL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (LESSP U (CAR VL)))) (RETURN NIL)))
        (PROGN
         (SETQ WL (CONS (CAR VL) WL))
         (SETQ VL (CDR VL))
         (SETQ S (NOT S)))
        (GO WHILELABEL))
      (COND ((NULL VL) (RETURN (CONS S (REVERSIP (CONS U WL))))))
      (COND ((EQN U (CAR VL)) (RETURN NIL)))
      (RETURN (CONS S (NCONC (REVERSIP (CONS U WL)) VL))))) 
(ENDMODULE) 
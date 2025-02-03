(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST (LIST 'CONS 'BRSLTNT_BMAP 'RED))) 
(FLUID '(*BEZOUT_TRY_FAC *BEZOUT_TRY_GCD)) 
(DE |BB:ORDEXP| (U V) (GREATERP U V)) 
(PUT '|BB:ORDEXP| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:ORDEXP| 'DEFINED-ON-LINE '42) 
(PUT '|BB:ORDEXP| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:ORDEXP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC '|BB:ORDEXP| 'INLINE '(LAMBDA (U V) (GREATERP U V))) 
(PUT '|BB:BEZOUT_RESULTANT| 'NUMBER-OF-ARGS 3) 
(PUT '|BB:BEZOUT_RESULTANT| 'DEFINED-ON-LINE '44) 
(PUT '|BB:BEZOUT_RESULTANT| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:BEZOUT_RESULTANT| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |BB:BEZOUT_RESULTANT| (U V W)
    (PROG (N NM EP CUH CVH CX CXF UH UT VH VT X)
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
                (|BB:BEZOUT_RESULTANT| V U W)))))
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
      (SETQ CUH UH)
      (SETQ CVH VH)
      (SETQ CXF '(1))
      (COND
       ((AND *BEZOUT_TRY_GCD (NEQ (SETQ CX (GCDF* UH VH)) 1))
        (PROGN
         (SETQ CXF (|BB:FAC-MERGE| CX CXF))
         (SETQ UH (QUOTF1 UH CX))
         (SETQ VH (QUOTF1 VH CX)))))
      (SETQ EP
              (*SF2EXBB
               (ADDF
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF UH VT))
                      (T (POLY-MULTF UH VT)))
                (NEGF
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF VH UT))
                       (T (POLY-MULTF VH UT)))))
               W))
      (COND ((CDR CXF) (|BB:TRY_PREVIOUS_FACTORS| EP (CDR CXF))))
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
                          ((LAMBDA (G597)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G597 UH))
                                   (T (POLY-MULTF G597 UH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((AND *BEZOUT_TRY_GCD (NULL (OR (EQUAL CUH 1) (EQUAL CVH 1))))
              (SETQ CUH (GCDF* (CDAR UT) CUH))))
            (SETQ UT (CDR UT))))
          (T
           (SETQ UH
                   ((LAMBDA (G599)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G599 UH))
                            (T (POLY-MULTF G599 UH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (COND
          ((EQUAL (DEGR VT W) J)
           (PROGN
            (SETQ VH
                    (ADDF (CDAR VT)
                          ((LAMBDA (G601)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G601 VH))
                                   (T (POLY-MULTF G601 VH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((AND *BEZOUT_TRY_GCD (NULL (OR (EQUAL CUH 1) (EQUAL CVH 1))))
              (SETQ CVH (GCDF* (CDAR VT) CVH))))
            (SETQ VT (CDR VT))))
          (T
           (SETQ VH
                   ((LAMBDA (G603)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G603 VH))
                            (T (POLY-MULTF G603 VH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (COND
          ((AND *BEZOUT_TRY_GCD (NEQ (SETQ CX (GCDF* CUH CVH)) 1))
           (PROGN
            (SETQ CXF (|BB:FAC-MERGE| CX CXF))
            (SETQ UH (QUOTF1 UH CX))
            (SETQ VH (QUOTF1 VH CX)))))
         (SETQ X
                 (*SF2EXBB
                  (ADDF
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF UH VT))
                         (T (POLY-MULTF UH VT)))
                   (NEGF
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF VH UT))
                          (T (POLY-MULTF VH UT)))))
                  W))
         (COND ((CDR CXF) (|BB:TRY_PREVIOUS_FACTORS| X (CDR CXF))))
         (SETQ CXF (|BB:NORMALIZE| X CXF))
         (SETQ EP (|BB:EXTMULT| X EP))
         (COND ((CDR CXF) (|BB:TRY_PREVIOUS_FACTORS| EP (CDR CXF))))
         (COND ((NEQ J 1) (SETQ CXF (|BB:NORMALIZE| EP CXF)))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (COND
       ((NEQ N 0)
        (PROGN
         (SETQ X (*SF2EXBB U W))
         (SETQ CXF (|BB:NORMALIZE| X CXF))
         (SETQ EP (|BB:EXTMULT| X EP))
         (SETQ CXF (|BB:NORMALIZE| EP CXF))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) J)) (RETURN NIL)))
           (PROGN
            (SETQ X
                    (*SF2EXBB
                     ((LAMBDA (G544)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 U))
                              (T (POLY-MULTF G544 U))))
                      (LIST (CONS (CONS W J) 1)))
                     W))
            (SETQ CXF (|BB:NORMALIZE| X CXF))
            (SETQ EP (|BB:EXTMULT| X EP))
            (SETQ CXF (|BB:NORMALIZE| EP CXF)))
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (COND
       (EP
        (PROGN
         (SETQ X
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR EP) (CAR CXF)))
                       (T (POLY-MULTF (CDAR EP) (CAR CXF)))))
         (PROG (J)
           (SETQ J (CDR CXF))
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (SETQ X
                      ((LAMBDA (G608)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF X G608))
                               (T (POLY-MULTF X G608))))
                       (EXPTF (CAR J) (CDR J)))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB)))))
      (RETURN (COND ((NULL EP) NIL) (T X))))) 
(PUT '*SF2EXBB 'NUMBER-OF-ARGS 2) 
(PUT '*SF2EXBB 'DEFINED-ON-LINE '115) 
(PUT '*SF2EXBB 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '*SF2EXBB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *SF2EXBB (U V)
    (COND ((NULL U) NIL) ((EQUAL (DEGR U V) 0) (CONS (CONS 1 U) NIL))
          (T (CONS (CONS (ASHIFT 1 (CDAAR U)) (CDAR U)) (*SF2EXBB (CDR U) V))))) 
(PUT '|BB:EXTMULT_| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:EXTMULT_| 'DEFINED-ON-LINE '121) 
(PUT '|BB:EXTMULT_| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:EXTMULT_| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:EXTMULT_| (U V)
    (PROG (EPTR R RES V0 X Y Z)
      (SETQ R (CONS NIL NIL))
      (SETQ RES (CONS NIL NIL))
     A0
      (RPLACD R NIL)
      (SETQ EPTR R)
      (SETQ V0 V)
      (SETQ X (CAAR U))
     A
      (SETQ Y (CAAR V0))
      (COND
       ((EQUAL (LAND X Y) 0)
        (PROGN
         (SETQ Z
                 (COND ((EVENP (LOGCOUNT (LAND Y (SUB1 X)))) (CDAR U))
                       (T (NEGF (CDAR U)))))
         (SETQ Z
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z (CDAR V0)))
                       (T (POLY-MULTF Z (CDAR V0)))))
         (SETQ EPTR (CDR (RPLACD EPTR (CONS (CONS (LOR X Y) Z) NIL)))))))
      (COND ((SETQ V0 (CDR V0)) (GO A)))
      (|BB:EXTADDIP| RES (CDR R))
      (COND ((SETQ U (CDR U)) (GO A0)))
      (RETURN (CDR RES)))) 
(PUT '|BB:EXTADDIP| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:EXTADDIP| 'DEFINED-ON-LINE '142) 
(PUT '|BB:EXTADDIP| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:EXTADDIP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:EXTADDIP| (U V)
    (PROG (X RU)
     A
      (SETQ RU (CDR U))
      (COND ((NULL V) (RETURN NIL)) ((NULL RU) (RETURN (RPLACD U V)))
            ((EQUAL (CAAR RU) (CAAR V))
             (PROGN
              (COND
               ((SETQ X (ADDF (CDAR RU) (CDAR V)))
                (PROGN (RPLACD (CAR RU) X) (SETQ U RU)))
               (T (RPLACD U (CDR RU))))
              (SETQ V (CDR V))))
            ((GREATERP (CAAR V) (CAAR RU))
             (PROGN
              (RPLACA (RPLACD RU (CONS (CAR RU) (CDR RU))) (CAR V))
              (SETQ V (CDR V))
              (SETQ U RU)))
            (T (SETQ U RU)))
      (GO A))) 
(PUT '|BB:EXTMULT| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:EXTMULT| 'DEFINED-ON-LINE '162) 
(PUT '|BB:EXTMULT| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:EXTMULT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:EXTMULT| (U V)
    (PROG (LC1 RES RF RRP RP X Y Z)
      (SETQ RES (SETQ RP (CONS NIL NIL)))
      (SETQ X (CAAR U))
      (SETQ LC1 (CDAR U))
      (PROG (Q)
        (SETQ Q V)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        (COND
         ((EQUAL (LAND X (SETQ Y (CAAR Q))) 0)
          (SETQ RP
                  (CDR
                   (RPLACD RP
                           (CONS
                            (CONS (LOR X Y)
                                  ((LAMBDA (G609)
                                     (COND
                                      (*PHYSOP-LOADED
                                       (PHYSOP-MULTF G609 (CDAR Q)))
                                      (T (POLY-MULTF G609 (CDAR Q)))))
                                   (COND
                                    ((EVENP (LOGCOUNT (LAND Y (SUB1 X)))) LC1)
                                    (T (NEGF LC1)))))
                            NIL))))))
        (SETQ Q (CDR Q))
        (GO LAB))
      (SETQ RF (COND ((EQUAL (LAND X (CAAR V)) 0) (CDR RES)) (T RES)))
      (PROG (P)
        (SETQ P (CDR U))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        (PROGN
         (SETQ X (CAAR P))
         (SETQ LC1 (CDAR P))
         (SETQ RP RF)
         (COND
          ((EQUAL (LAND X (SETQ Y (CAAR V))) 0)
           (PROG ()
             (SETQ Z
                     ((LAMBDA (G611)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G611 (CDAR V)))
                              (T (POLY-MULTF G611 (CDAR V)))))
                      (COND ((EVENP (LOGCOUNT (LAND Y (SUB1 X)))) LC1)
                            (T (NEGF LC1)))))
            A
             (SETQ RRP (CDR RP))
             (COND
              ((NULL RRP)
               (SETQ RP (CDR (RPLACD RP (CONS (CONS (LOR X Y) Z) NIL)))))
              ((EQUAL (LOR X Y) (CAAR RRP))
               (COND
                ((SETQ Z (ADDF (CDAR RRP) Z))
                 (PROGN (RPLACD (CAR RRP) Z) (SETQ RP RRP)))
                (T (RPLACD RP (CDR RRP)))))
              ((GREATERP (LOR X Y) (CAAR RRP))
               (SETQ RP
                       (RPLACA (RPLACD RRP (CONS (CAR RRP) (CDR RRP)))
                               (CONS (LOR X Y) Z))))
              (T (PROGN (SETQ RP RRP) (GO A)))))))
         (SETQ RF RP)
         (PROG (Q)
           (SETQ Q (CDR V))
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           (COND
            ((EQUAL (LAND X (SETQ Y (CAAR Q))) 0)
             (PROG ()
               (SETQ Z
                       ((LAMBDA (G613)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G613 (CDAR Q)))
                                (T (POLY-MULTF G613 (CDAR Q)))))
                        (COND ((EVENP (LOGCOUNT (LAND Y (SUB1 X)))) LC1)
                              (T (NEGF LC1)))))
              A
               (SETQ RRP (CDR RP))
               (COND
                ((NULL RRP)
                 (SETQ RP (CDR (RPLACD RP (CONS (CONS (LOR X Y) Z) NIL)))))
                ((EQUAL (LOR X Y) (CAAR RRP))
                 (COND
                  ((SETQ Z (ADDF (CDAR RRP) Z))
                   (PROGN (RPLACD (CAR RRP) Z) (SETQ RP RRP)))
                  (T (RPLACD RP (CDR RRP)))))
                ((GREATERP (LOR X Y) (CAAR RRP))
                 (SETQ RP
                         (RPLACA (RPLACD RRP (CONS (CAR RRP) (CDR RRP)))
                                 (CONS (LOR X Y) Z))))
                (T (PROGN (SETQ RP RRP) (GO A)))))))
           (SETQ Q (CDR Q))
           (GO LAB)))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (CDR RES)))) 
(PUT '|BB:COMFAC| 'NUMBER-OF-ARGS 1) 
(PUT '|BB:COMFAC| 'DEFINED-ON-LINE '222) 
(PUT '|BB:COMFAC| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:COMFAC| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |BB:COMFAC| (U)
    (PROG (*EZGCD X)
      (SETQ *EZGCD T)
      (COND ((NULL U) (RETURN 1)))
      (SETQ X (GCDF* (CDAR U) NIL))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN X)))
      (SETQ X (GCDF* (CDAR U) X))
      (GO A))) 
(PUT '|BB:CQUOT| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:CQUOT| 'DEFINED-ON-LINE '233) 
(PUT '|BB:CQUOT| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:CQUOT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:CQUOT| (U V)
    (PROG (X)
      (SETQ X U)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      (RPLACD (CAR X) (QUOTF1 (CDAR X) V))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT '|BB:CTRIALDIV| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:CTRIALDIV| 'DEFINED-ON-LINE '236) 
(PUT '|BB:CTRIALDIV| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:CTRIALDIV| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:CTRIALDIV| (U V)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (QUOTF1 (CDAR U) V))) (RETURN NIL)))
        (SETQ U (CDR U))
        (GO WHILELABEL))
      (RETURN (NULL U)))) 
(PUT '|BB:TRY_PREVIOUS_FACTORS| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:TRY_PREVIOUS_FACTORS| 'DEFINED-ON-LINE '242) 
(PUT '|BB:TRY_PREVIOUS_FACTORS| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:TRY_PREVIOUS_FACTORS| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:TRY_PREVIOUS_FACTORS| (U V)
    (PROG (B)
     A
      (COND
       ((SETQ B (|BB:CTRIALDIV| U (CAAR V)))
        (PROG (X)
          (SETQ X U)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          (RPLACD (CAR X) (QUOTF1 (CDAR X) (CAAR V)))
          (SETQ X (CDR X))
          (GO LAB))))
      (COND ((NULL B) (COND ((SETQ V (CDR V)) (GO A)) (T (RETURN NIL)))))
      (RPLACD (CAR V) (PLUS (CDAR V) 1))
      (GO A))) 
(PUT '|BB:FAC-MERGE| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:FAC-MERGE| 'DEFINED-ON-LINE '251) 
(PUT '|BB:FAC-MERGE| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:FAC-MERGE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:FAC-MERGE| (U V)
    (PROG (X)
      (SETQ X (COND (*BEZOUT_TRY_FAC (FCTRF U)) (T (LIST 1 (CONS U 1)))))
      (RETURN
       (COND
        ((NULL (CDR V))
         (CONS
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) (CAR V)))
                (T (POLY-MULTF (CAR X) (CAR V))))
          (CDR X)))
        (T
         (CONS
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) (CAR V)))
                (T (POLY-MULTF (CAR X) (CAR V))))
          (|BB:FAC-MERGE2| (CDR X) (CDR V)))))))) 
(PUT '|BB:FAC-MERGE2| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:FAC-MERGE2| 'DEFINED-ON-LINE '258) 
(PUT '|BB:FAC-MERGE2| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:FAC-MERGE2| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:FAC-MERGE2| (U V)
    (PROG (X Y R)
      (COND ((NULL U) (RETURN V)))
     C
      (SETQ X (CAR U))
      (SETQ Y V)
     B
      (COND
       ((EQUAL (CAR X) (CAAR Y))
        (PROGN (RPLACD (CAR Y) (PLUS (CDAR Y) (CDR X))) (GO A))))
      (SETQ Y (CDR Y))
      (COND (Y (GO B)))
      (SETQ R (CONS X R))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN (APPEND V R))))
      (GO C))) 
(PUT '|BB:NORMALIZE| 'NUMBER-OF-ARGS 2) 
(PUT '|BB:NORMALIZE| 'DEFINED-ON-LINE '274) 
(PUT '|BB:NORMALIZE| 'DEFINED-IN-FILE 'MATRIX/BRSLTNT_BMAP.RED) 
(PUT '|BB:NORMALIZE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |BB:NORMALIZE| (U V)
    (PROG (CX)
      (COND
       ((OR (NULL *BEZOUT_TRY_GCD) (EQUAL (SETQ CX (|BB:COMFAC| U)) 1))
        (RETURN V)))
      (SETQ V (|BB:FAC-MERGE| CX V))
      (|BB:CQUOT| U CX)
      (RETURN V))) 
(ENDMODULE) 
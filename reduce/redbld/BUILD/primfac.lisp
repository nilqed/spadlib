(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PRIMFAC)) 
(FLUID '(*INTFAC *SURDS DMODE* INTVAR KERNLIST* KNOWNDISCRIMSIGN)) 
(PUT 'PRSQFRFACF 'NUMBER-OF-ARGS 1) 
(PUT 'PRSQFRFACF 'DEFINED-ON-LINE '34) 
(PUT 'PRSQFRFACF 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'PRSQFRFACF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRSQFRFACF (U)
    (PROG (BOOL KNOWNDISCRIMSIGN V W)
      (COND (DMODE* (RETURN (LIST 1 U))))
      (SETQ V
              (COND (INTVAR (LIST INTVAR)) (KERNLIST* KERNLIST*)
                    (T (REVERSE (KERNORD-SORT (POWERS U))))))
      (SETQ W (SETKORDER V))
      (SETQ U (REORDER U))
      (COND ((MINUSF U) (PROGN (SETQ BOOL T) (SETQ U (NEGF U)))))
      (SETQ U (FACTOR-ORDERED-SQFREE-PRIM-F U))
      (SETKORDER W)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (PROG ()
                                      (SETQ V (REORDER X))
                                      (COND
                                       ((AND BOOL (MINUSF V))
                                        (PROGN
                                         (SETQ V (NEGF V))
                                         (SETQ BOOL NIL))))
                                      (RETURN V)))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROG ()
                              (SETQ V (REORDER X))
                              (COND
                               ((AND BOOL (MINUSF V))
                                (PROGN (SETQ V (NEGF V)) (SETQ BOOL NIL))))
                              (RETURN V)))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (BOOL (SETQ U (CONS (NEGF (CAR U)) (CDR U)))))
      (RETURN U))) 
(PUT 'FACTOR-ORDERED-SQFREE-PRIM-F 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR-ORDERED-SQFREE-PRIM-F 'DEFINED-ON-LINE '67) 
(PUT 'FACTOR-ORDERED-SQFREE-PRIM-F 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'FACTOR-ORDERED-SQFREE-PRIM-F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR-ORDERED-SQFREE-PRIM-F (POL)
    (PROG (N Q RES W)
      (SETQ N 0)
      (COND ((EQUAL (CDAAR POL) 1) (RETURN (FACTOR-COEFFS POL)))
            ((UNIVARIATEP POL)
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT (CAR (SETQ Q (LINFACF POL)))) (RETURN NIL)))
                (PROGN (SETQ RES (CONS (CAR Q) RES)) (SETQ POL (CDR Q)))
                (GO WHILELABEL))
              (PROG ()
               WHILELABEL
                (COND ((NOT (CAR (SETQ Q (QUADFACF POL)))) (RETURN NIL)))
                (PROGN (SETQ RES (CONS (CAR Q) RES)) (SETQ POL (CDR Q)))
                (GO WHILELABEL)))))
      (COND ((NULL POL) (RETURN (CONS 1 RES)))
            ((GREATERP (LENGTH (SETQ W (SPECIAL-CASE-FACTOR POL))) 2)
             (PROGN
              (SETQ RES (CONS (CAR W) RES))
              (PROG (J)
                (SETQ J (CDR W))
               LAB
                (COND ((NULL J) (RETURN NIL)))
                ((LAMBDA (J)
                   (SETQ RES (FAC-MERGE (FACTOR-ORDERED-SQFREE-PRIM-F J) RES)))
                 (CAR J))
                (SETQ J (CDR J))
                (GO LAB))
              (RETURN RES)))
            ((OR (LESSP (CDAAR POL) 4) (EQUAL (SETQ N (DEGREEGCD POL)) 1))
             (RETURN (CONS 1 (CONS POL RES)))))
      (SETQ W (CDR (SORT (DFACTORS N) (FUNCTION LESSP))))
      (SETQ KNOWNDISCRIMSIGN 'NEGATIVE)
     A
      (COND
       ((NULL W)
        (PROGN (SETQ KNOWNDISCRIMSIGN NIL) (RETURN (CONS 1 (CONS POL RES)))))
       ((GREATERP
         (LENGTH
          (SETQ Q (FACTOR-ORDERED-SQFREE-PRIM-F (DOWNPOWER POL (CAR W)))))
         2)
        (PROGN
         (SETQ RES (CONS (CAR Q) RES))
         (PROG (J)
           (SETQ J (CDR Q))
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (SETQ RES
                      (FAC-MERGE
                       (FACTOR-ORDERED-SQFREE-PRIM-F
                        (UPPOWER J (CAAAR POL) (CAR W)))
                       RES)))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (SETQ KNOWNDISCRIMSIGN NIL)
         (RETURN RES))))
      (SETQ W (CDR W))
      (GO A))) 
(PUT 'DOWNPOWER 'NUMBER-OF-ARGS 2) 
(PUT 'DOWNPOWER 'DEFINED-ON-LINE '106) 
(PUT 'DOWNPOWER 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'DOWNPOWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOWNPOWER (POL N) (DOWNPOWER1 POL (CAAAR POL) N)) 
(PUT 'DOWNPOWER1 'NUMBER-OF-ARGS 3) 
(PUT 'DOWNPOWER1 'DEFINED-ON-LINE '111) 
(PUT 'DOWNPOWER1 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'DOWNPOWER1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DOWNPOWER1 (POL MV N)
    (COND ((OR (OR (ATOM POL) (ATOM (CAR POL))) (NOT (EQ (CAAAR POL) MV))) POL)
          (T
           (CONS (CONS (CONS MV (QUOTIENT (CDAAR POL) N)) (CDAR POL))
                 (DOWNPOWER1 (CDR POL) MV N))))) 
(PUT 'UPPOWER 'NUMBER-OF-ARGS 3) 
(PUT 'UPPOWER 'DEFINED-ON-LINE '115) 
(PUT 'UPPOWER 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'UPPOWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPPOWER (POL VAR N)
    (COND ((EQUAL (CAAAR POL) VAR) (UPPOWER1 POL VAR N))
          (T (UPPOWER2 POL VAR N)))) 
(PUT 'UPPOWER1 'NUMBER-OF-ARGS 3) 
(PUT 'UPPOWER1 'DEFINED-ON-LINE '119) 
(PUT 'UPPOWER1 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'UPPOWER1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPPOWER1 (POL MV N)
    (COND ((OR (OR (ATOM POL) (ATOM (CAR POL))) (NOT (EQ (CAAAR POL) MV))) POL)
          (T
           (CONS (CONS (CONS MV (TIMES (CDAAR POL) N)) (CDAR POL))
                 (UPPOWER1 (CDR POL) MV N))))) 
(PUT 'UPPOWER2 'NUMBER-OF-ARGS 3) 
(PUT 'UPPOWER2 'DEFINED-ON-LINE '123) 
(PUT 'UPPOWER2 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'UPPOWER2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE UPPOWER2 (POL VAR N)
    (COND ((OR (ATOM POL) (ATOM (CAR POL))) POL)
          ((EQUAL (CAAAR POL) VAR)
           (CONS (CONS (CONS (CAAAR POL) (TIMES (CDAAR POL) N)) (CDAR POL))
                 (UPPOWER2 (CDR POL) VAR N)))
          (T
           (CONS (CONS (CAAR POL) (UPPOWER2 (CDAR POL) VAR N))
                 (UPPOWER2 (CDR POL) VAR N))))) 
(PUT 'UNIVARIATEP 'NUMBER-OF-ARGS 1) 
(PUT 'UNIVARIATEP 'DEFINED-ON-LINE '130) 
(PUT 'UNIVARIATEP 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'UNIVARIATEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIVARIATEP (POL)
    (AND (NOT (OR (ATOM POL) (ATOM (CAR POL)))) (UNIVARIATEP1 POL (CAAAR POL)))) 
(PUT 'UNIVARIATEP1 'NUMBER-OF-ARGS 2) 
(PUT 'UNIVARIATEP1 'DEFINED-ON-LINE '135) 
(PUT 'UNIVARIATEP1 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'UNIVARIATEP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNIVARIATEP1 (POL MV)
    (OR (OR (ATOM POL) (ATOM (CAR POL)))
        (AND (EQ (CAAAR POL) MV) (OR (ATOM (CDAR POL)) (ATOM (CAR (CDAR POL))))
             (UNIVARIATEP1 (CDR POL) MV)))) 
(PUT 'SPECIAL-CASE-FACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'SPECIAL-CASE-FACTOR 'DEFINED-ON-LINE '139) 
(PUT 'SPECIAL-CASE-FACTOR 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'SPECIAL-CASE-FACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPECIAL-CASE-FACTOR (POL)
    ((LAMBDA (DEGREE)
       (COND ((AND (EQUAL DEGREE 2) (NULL *INTFAC)) (QUADRATICF POL))
             ((EQUAL DEGREE 3) (CUBICF POL)) ((EQUAL DEGREE 4) (QUARTICF POL))
             (T (LIST 1 POL))))
     (CDAAR POL))) 
(PUT 'DEGREEGCD 'NUMBER-OF-ARGS 1) 
(PUT 'DEGREEGCD 'DEFINED-ON-LINE '149) 
(PUT 'DEGREEGCD 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'DEGREEGCD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEGREEGCD (POL)
    (PROG (N MV)
      (SETQ N 0)
      (SETQ MV (CAAAR POL))
      (SETQ N (CDAAR POL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (GREATERP N 1)
                (NOT
                 ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                  (SETQ POL (CDR POL))))
                (EQ (CAAAR POL) MV)))
          (RETURN NIL)))
        (SETQ N (GCDN N (CDAAR POL)))
        (GO WHILELABEL))
      (RETURN N))) 
(PUT 'FACTOR-COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR-COEFFS 'DEFINED-ON-LINE '159) 
(PUT 'FACTOR-COEFFS 'DEFINED-IN-FILE 'POLY/PRIMFAC.RED) 
(PUT 'FACTOR-COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR-COEFFS (U) (LIST 1 U)) 
(ENDMODULE) 
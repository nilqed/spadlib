(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MODIFY)) 
(FLUID '(*TRA INTVAR)) 
(EXPORTS (LIST 'MODIFY-SQRTS 'COMBINE-SQRTS)) 
(PUT 'MODIFY-SQRTS 'NUMBER-OF-ARGS 2) 
(PUT 'MODIFY-SQRTS 'DEFINED-ON-LINE '34) 
(PUT 'MODIFY-SQRTS 'DEFINED-IN-FILE 'ALGINT/MODIFY.RED) 
(PUT 'MODIFY-SQRTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODIFY-SQRTS (BASIS SQRTL)
    (PROG (SQRTL-IN-SF N U V F)
      (SETQ N (UPBV BASIS))
      (SETQ SQRTL-IN-SF
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U SQRTL)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG ()
          (SETQ U (GETV BASIS I))
          (SETQ V (SQRTSINSQ U INTVAR))
          (SETQ V (SETDIFF V SQRTL))
          (COND ((NULL V) (GO NOCHANGE)))
          (SETQ U (SQRT2TOP U))
          (SETQ U
                  (MULTSQ (CONS (MODIFY2 (CAR U) V SQRTL-IN-SF) 1)
                          (CONS 1 (MODIFY2 (CDR U) V SQRTL-IN-SF))))
          (SETQ V (SQRTSINSQ U INTVAR))
          (SETQ V (SETDIFF V SQRTL))
          (COND
           (V
            (PROGN
             (COND
              (*TRA
               (PROGN
                (PROGN
                 (PRIN2 "Discarding element")
                 (TERPRI)
                 "Discarding element")
                (PRINTSQ U))))
             (PUTV BASIS I (CONS 1 1))))
           (T (PUTV BASIS I (REMOVECMSQ U))))
          (SETQ F T)
         NOCHANGE)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ BASIS (MKUNIQUEVECT BASIS))
      (COND
       ((AND F *TRA)
        (PROGN
         (PROGN (PRIN2 "Basis replaced by") (TERPRI) "Basis replaced by")
         (MAPVEC BASIS (FUNCTION PRINTSQ)))))
      (RETURN BASIS))) 
(PUT 'COMBINE-SQRTS 'NUMBER-OF-ARGS 2) 
(PUT 'COMBINE-SQRTS 'DEFINED-ON-LINE '73) 
(PUT 'COMBINE-SQRTS 'DEFINED-IN-FILE 'ALGINT/MODIFY.RED) 
(PUT 'COMBINE-SQRTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMBINE-SQRTS (BASIS SQRTL)
    (PROG (SQRTL-IN-SF N U V F)
      (SETQ N (UPBV BASIS))
      (SETQ SQRTL-IN-SF
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U SQRTL)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG ()
          (SETQ U (GETV BASIS I))
          (SETQ V (SQRTSINSQ U INTVAR))
          (SETQ V (SETDIFF V SQRTL))
          (COND ((NULL V) (GO NOCHANGE)))
          (SETQ U
                  (MULTSQ (CONS (MODIFY2 (CAR U) V SQRTL-IN-SF) 1)
                          (CONS 1 (MODIFY2 (CDR U) V SQRTL-IN-SF))))
          (PUTV BASIS I U)
          (SETQ F T)
         NOCHANGE)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((AND F *TRA)
        (PROGN
         (PROGN (PRIN2 "Basis replaced by") (TERPRI) "Basis replaced by")
         (MAPVEC BASIS (FUNCTION PRINTSQ)))))
      (RETURN BASIS))) 
(PUT 'MODIFY2 'NUMBER-OF-ARGS 3) 
(PUT 'MODIFY2 'DEFINED-ON-LINE '101) 
(PUT 'MODIFY2 'DEFINED-IN-FILE 'ALGINT/MODIFY.RED) 
(PUT 'MODIFY2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODIFY2 (SF SQRTSIN REALSQRTS)
    (COND ((ATOM SF) SF) ((ATOM (CAAAR SF)) SF)
          ((AND (EQCAR (CAAAR SF) 'SQRT) (DEPENDSP (CAAAR SF) INTVAR))
           (PROG (U V W LCSF SQRTSIN2 W2 LCSF2 TEMP)
             (SETQ U (*Q2F (SIMP (CADR (CAAAR SF)))))
             (SETQ V REALSQRTS)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND V (NULL (SETQ W (MODIFY-QUOTF (CAR V) U)))))
                 (RETURN NIL)))
               (SETQ V (CDR V))
               (GO WHILELABEL))
             (COND
              ((NULL V)
               (PROGN
                (COND
                 (*TRA
                  (PROGN
                   (PROGN
                    (PRIN2 "Unable to modify (postponed)")
                    (TERPRI)
                    "Unable to modify (postponed)")
                   (PRINTSF (LIST (CONS (GETPOWER (FKERN (CAAAR SF)) 1) 1))))))
                (RETURN SF))))
             (SETQ V (CAR V))
             (SETQ LCSF (CDAR SF))
             (SETQ SQRTSIN2 (DELETE (CAAAR SF) SQRTSIN))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND SQRTSIN2 (NEQ W 1))) (RETURN NIL)))
               (PROGN
                (SETQ TEMP (*Q2F (SIMP (CADR (CAR SQRTSIN2)))))
                (COND
                 ((AND (SETQ W2 (MODIFY-QUOTF W TEMP))
                       (SETQ LCSF2
                               (MODIFY-QUOTF LCSF
                                (LIST
                                 (CONS (GETPOWER (FKERN (CAR SQRTSIN2)) 1)
                                       1)))))
                  (PROGN (SETQ W W2) (SETQ LCSF LCSF2))))
                (SETQ SQRTSIN2 (CDR SQRTSIN2)))
               (GO WHILELABEL))
             (COND
              ((EQUAL W 1)
               (RETURN
                (ADDF
                 ((LAMBDA (G559)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF LCSF G559))
                          (T (POLY-MULTF LCSF G559))))
                  (FORMSQRT V))
                 (MODIFY2 (CDR SF) SQRTSIN REALSQRTS)))))
             (RETURN
              (ADDF
               ((LAMBDA (G560 G561)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G560 G561))
                        (T (POLY-MULTF G560 G561))))
                (LIST (CONS (CAAR SF) 1))
                (MODIFY2 (CDAR SF) SQRTSIN REALSQRTS))
               (MODIFY2 (CDR SF) SQRTSIN REALSQRTS)))))
          (T
           (ADDF
            ((LAMBDA (G562 G563)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF G562 G563))
                     (T (POLY-MULTF G562 G563))))
             (LIST (CONS (CAAR SF) 1)) (MODIFY2 (CDAR SF) SQRTSIN REALSQRTS))
            (MODIFY2 (CDR SF) SQRTSIN REALSQRTS))))) 
(PUT 'MODIFY-QUOTF 'NUMBER-OF-ARGS 2) 
(PUT 'MODIFY-QUOTF 'DEFINED-ON-LINE '191) 
(PUT 'MODIFY-QUOTF 'DEFINED-IN-FILE 'ALGINT/MODIFY.RED) 
(PUT 'MODIFY-QUOTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODIFY-QUOTF (U V)
    (COND ((OR (ATOM V) (ATOM (CAAAR V))) ((LAMBDA (*EXP) (QUOTF1 U V)) T))
          ((EQUAL U V) 1)
          (T
           (PROG (SQ)
             (SETQ SQ (SQRT2TOP (CONS U V)))
             (COND ((INVOLVESF (CDR SQ) INTVAR) (RETURN NIL)))
             (COND
              ((NOT (ONEP (CDR SQ)))
               (COND
                ((NOT (NUMBERP (CDR SQ)))
                 (INTERR "Gauss' lemma violated in modify"))
                (*TRA
                 (PROGN
                  (PROGN
                   (PRIN2 "*** Denominator ignored in modify")
                   (TERPRI)
                   "*** Denominator ignored in modify")
                  (PROGN (PRIN2 (CDR SQ)) (TERPRI) (CDR SQ)))))))
             (RETURN (CAR SQ)))))) 
(ENDMODULE) 
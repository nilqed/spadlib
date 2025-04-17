(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFSIAT)) 
(REVISION 'PASFSIAT
          "$Id: pasfsiat.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'PASFSIAT
           "(c) 2002-2009 A. Dolzmann, A. Seidl, T. Sturm, 2020 T. Sturm") 
(PUT 'PASF_SIMPLAT1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SIMPLAT1 'DEFINED-ON-LINE '34) 
(PUT 'PASF_SIMPLAT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_SIMPLAT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SIMPLAT1 (ATF SOP)
    (PROG ()
      (SETQ ATF (PASF_VF (PASF_DT (PASF_MKPOS (PASF_ZCONG ATF)))))
      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) (RETURN ATF)))
      (COND
       ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
        (SETQ ATF (PASF_CECONG (PASF_VF (PASF_MR ATF)))))
       (T
        (COND
         ((MEMQ
           (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                 ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
           '(EQUAL NEQ))
          (SETQ ATF (PASF_CEEQ ATF)))
         (T (SETQ ATF (PASF_CEIN ATF))))))
      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) (RETURN ATF)))
      (SETQ ATF
              (COND
               ((MEMQ
                 (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                       ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
                 '(CONG NCONG))
                (PASF_SC ATF))
               ((MEMQ
                 (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                       ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
                 '(EQUAL NEQ))
                (PASF_SE ATF))
               (T (PASF_OR ATF))))
      (COND ((NOT *RLSIFAC) (RETURN ATF)))
      (RETURN (PASF_FACT ATF)))) 
(PUT 'PASF_ZCONG 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ZCONG 'DEFINED-ON-LINE '71) 
(PUT 'PASF_ZCONG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_ZCONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ZCONG (ATF)
    (COND
     ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
      (COND
       ((NULL (CDAR ATF))
        (LIST
         (COND
          ((EQ
            (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                  ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
            'CONG)
           'EQUAL)
          (T 'NEQ))
         (CADR ATF) NIL))
       ((AND (NULL (CADR ATF))
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'CONG))
        'TRUE)
       ((AND (NULL (CADR ATF))
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'NCONG))
        'FALSE)
       (T ATF)))
     (T ATF))) 
(PUT 'PASF_MKPOS 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_MKPOS 'DEFINED-ON-LINE '84) 
(PUT 'PASF_MKPOS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_MKPOS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_MKPOS (ATF)
    (PROG (RES)
      (SETQ RES
              (COND
               ((AND (NOT (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)))
                     (MINUSF (CADR ATF)))
                (PASF_ANEGATEAT ATF))
               (T ATF)))
      (COND
       ((AND
         (AND (PAIRP RES) (PAIRP (CAR RES)) (MEMQ (CAAR RES) '(CONG NCONG)))
         (MINUSF (CDAR RES)))
        (SETQ RES
                (LIST
                 (CONS
                  (COND ((OR (EQ RES 'TRUE) (EQ RES 'FALSE)) RES)
                        ((PAIRP (CAR RES)) (CAAR RES)) (T (CAR RES)))
                  (NEGF (CDAR RES)))
                 (CADR RES) NIL))))
      (RETURN RES))) 
(PUT 'PASF_VF 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_VF 'DEFINED-ON-LINE '100) 
(PUT 'PASF_VF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_VF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_VF (ATF)
    (PROG ()
      (COND
       ((AND (NOT (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)))
             (OR (ATOM (CADR ATF)) (ATOM (CAR (CADR ATF)))))
        (PROGN
         (COND
          ((AND
            (AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
            (NULL (OR (ATOM (CDAR ATF)) (ATOM (CAR (CDAR ATF))))))
           (COND ((NULL (CADR ATF)) (RETURN 'FALSE)) (T (RETURN ATF)))))
         (RETURN
          (COND ((PASF_EVALATP (CAR ATF) (CADR ATF)) 'TRUE) (T 'FALSE))))))
      (RETURN ATF))) 
(PUT 'PASF_DT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_DT 'DEFINED-ON-LINE '120) 
(PUT 'PASF_DT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_DT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_DT (ATF)
    (PROG (PDP OPN)
      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) (RETURN ATF)))
      (SETQ PDP (PASF_PDP (CADR ATF)))
      (SETQ OPN
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF))))
      (COND
       ((AND (EQ PDP 'PDEF) (MEMQ OPN '(EQUAL LESSP LEQ))) (RETURN 'FALSE)))
      (COND
       ((AND (EQ PDP 'NDEF) (MEMQ OPN '(EQUAL GREATERP GEQ))) (RETURN 'FALSE)))
      (COND
       ((AND (EQ PDP 'PDEF) (MEMQ OPN '(NEQ GREATERP GEQ))) (RETURN 'TRUE)))
      (COND ((AND (EQ PDP 'NDEF) (MEMQ OPN '(NEQ LESSP LEQ))) (RETURN 'TRUE)))
      (COND ((AND (EQ PDP 'PSDEF) (EQ OPN 'LESSP)) (RETURN 'FALSE)))
      (COND ((AND (EQ PDP 'NSDEF) (EQ OPN 'GREATERP)) (RETURN 'FALSE)))
      (COND ((AND (EQ PDP 'PSDEF) (EQ OPN 'GEQ)) (RETURN 'TRUE)))
      (COND ((AND (EQ PDP 'NSDEF) (EQ OPN 'LEQ)) (RETURN 'TRUE)))
      (COND
       ((AND (EQ PDP 'PSDEF) (EQ OPN 'NEQ))
        (RETURN (LIST 'GREATERP (CADR ATF) NIL))))
      (COND
       ((AND (EQ PDP 'NSDEF) (EQ OPN 'NEQ))
        (RETURN (LIST 'LESSP (CADR ATF) NIL))))
      (RETURN ATF))) 
(PUT 'PASF_MR 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_MR 'DEFINED-ON-LINE '145) 
(PUT 'PASF_MR 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_MR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_MR (ATF)
    (COND
     ((AND (NOT (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)))
           (AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
           (OR (ATOM (CDAR ATF)) (ATOM (CAR (CDAR ATF)))))
      (LIST (CAR ATF) (PASF_PREMF (CADR ATF) (CDAR ATF)) NIL))
     (T ATF))) 
(PUT 'PASF_PREMF 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_PREMF 'DEFINED-ON-LINE '155) 
(PUT 'PASF_PREMF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_PREMF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_PREMF (F M) (PASF_PREMF1 (REMF F M) M)) 
(PUT 'PASF_PREMF1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_PREMF1 'DEFINED-ON-LINE '159) 
(PUT 'PASF_PREMF1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_PREMF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_PREMF1 (R M)
    (PROG (C V D RR)
      (COND
       ((OR (ATOM R) (ATOM (CAR R)))
        (RETURN (COND ((MINUSF R) (ADDF R M)) (T R)))))
      (SETQ C (PASF_PREMF1 (CDAR R) M))
      (SETQ V (LIST (CONS (CONS (CAAAR R) 1) 1)))
      (SETQ D (CDAAR R))
      (SETQ RR (PASF_PREMF1 (CDR R) M))
      (RETURN
       (ADDF
        ((LAMBDA (G146)
           (COND (*PHYSOP-LOADED (PHYSOP-MULTF C G146))
                 (T (POLY-MULTF C G146))))
         (EXPTF V D))
        RR)))) 
(PUT 'PASF_CEEQ 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CEEQ 'DEFINED-ON-LINE '170) 
(PUT 'PASF_CEEQ 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_CEEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CEEQ (ATF)
    (PROG (G)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (MEMQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              '(EQUAL NEQ))))
        (RETURN ATF)))
      (SETQ G (SFTO_DCONTENTF (CADR ATF)))
      (RETURN (LIST (CAR ATF) (QUOTFX (CADR ATF) (CAR (SIMP G))) NIL)))) 
(PUT 'PASF_CEIN 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CEIN 'DEFINED-ON-LINE '183) 
(PUT 'PASF_CEIN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_CEIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CEIN (ATF)
    (PROG (G DECP)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (MEMQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              '(LEQ GREATERP GEQ LESSP))))
        (RETURN ATF)))
      (SETQ DECP (PASF_DECI (CADR ATF)))
      (SETQ G (SFTO_DCONTENTF (CAR DECP)))
      (RETURN
       (LIST (CAR ATF)
             (ADDF (QUOTFX (CAR DECP) (CAR (SIMP G)))
                   (COND
                    ((MEMQ
                      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                            ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
                      '(LEQ GREATERP))
                     (NEGF (PASF_FLOOR (MINUS (CDR DECP)) G)))
                    ((MEMQ
                      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                            ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
                      '(GEQ LESSP))
                     (NEGF (PASF_CEIL (MINUS (CDR DECP)) G)))))
             NIL)))) 
(PUT 'PASF_CECONG 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CECONG 'DEFINED-ON-LINE '201) 
(PUT 'PASF_CECONG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_CECONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CECONG (ATF)
    (PROG (INV M G)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (AND (PAIRP ATF) (PAIRP (CAR ATF))
                  (MEMQ (CAAR ATF) '(CONG NCONG)))))
        (RETURN ATF)))
      (SETQ M (CDAR ATF))
      (SETQ G (GCDF M (SFTO_DCONTENTF (CADR ATF))))
      (SETQ ATF
              (LIST
               (PASF_MKOP
                (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                      ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
                (QUOTFX M (CAR (SIMP G))))
               (QUOTFX (CADR ATF) (CAR (SIMP G))) NIL))
      (SETQ M (CDAR ATF))
      (SETQ G (SFTO_DCONTENTF (CADR ATF)))
      (SETQ INV (AND (OR (ATOM M) (ATOM (CAR M))) (EQUAL (GCDF M G) 1)))
      (RETURN
       (COND (INV (LIST (CAR ATF) (QUOTFX (CADR ATF) (CAR (SIMP G))) NIL))
             (T ATF))))) 
(PUT 'PASF_SE 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SE 'DEFINED-ON-LINE '224) 
(PUT 'PASF_SE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_SE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SE (ATF)
    (PROG (DECP G)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (MEMQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              '(NEQ EQUAL))))
        (RETURN ATF)))
      (SETQ DECP (PASF_DECI (CADR ATF)))
      (SETQ G (SFTO_DCONTENTF (CAR DECP)))
      (COND
       ((AND (NEQ (REMAINDER (CDR DECP) G) 0)
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'NEQ))
        (RETURN 'TRUE)))
      (COND
       ((AND (NEQ (REMAINDER (CDR DECP) G) 0)
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'EQUAL))
        (RETURN 'FALSE)))
      (RETURN ATF))) 
(PUT 'PASF_OR 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_OR 'DEFINED-ON-LINE '242) 
(PUT 'PASF_OR 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_OR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_OR (ATF)
    (PROG (DECP)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (MEMQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              '(LESSP GREATERP LEQ GEQ))))
        (RETURN ATF)))
      (SETQ DECP (PASF_DECI (CADR ATF)))
      (COND
       ((AND
         (EQ
          (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
          'LESSP)
         (LESSP (CDR DECP) 0))
        (RETURN (LIST 'LEQ (ADDF (CADR ATF) (CAR (SIMP 1))) NIL))))
      (COND
       ((AND
         (EQ
          (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
          'LEQ)
         (GREATERP (CDR DECP) 0))
        (RETURN (LIST 'LESSP (ADDF (CADR ATF) (NEGF (CAR (SIMP 1)))) NIL))))
      (COND
       ((AND
         (EQ
          (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
          'GREATERP)
         (GREATERP (CDR DECP) 0))
        (RETURN (LIST 'GEQ (ADDF (CADR ATF) (NEGF (CAR (SIMP 1)))) NIL))))
      (COND
       ((AND
         (EQ
          (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
          'GEQ)
         (LESSP (CDR DECP) 0))
        (RETURN (LIST 'GREATERP (ADDF (CADR ATF) (CAR (SIMP 1))) NIL))))
      (RETURN ATF))) 
(PUT 'PASF_SC 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SC 'DEFINED-ON-LINE '262) 
(PUT 'PASF_SC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_SC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SC (ATF)
    (PROG (G RES M DECP)
      (COND
       ((OR (OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
            (NOT
             (MEMQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              '(CONG NCONG)))
            (NULL (OR (ATOM (CDAR ATF)) (ATOM (CAR (CDAR ATF))))))
        (RETURN ATF)))
      (SETQ DECP (PASF_DECI (CADR ATF)))
      (SETQ G (SFTO_DCONTENTF (CAR DECP)))
      (SETQ M (CDAR ATF))
      (SETQ RES T)
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
        (SETQ RES
                (AND RES (NEQ (REMAINDER (PLUS (CDR DECP) (TIMES J G)) M) 0)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (COND
       ((AND RES
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'CONG))
        (RETURN 'FALSE)))
      (COND
       ((AND RES
             (EQ
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              'NCONG))
        (RETURN 'TRUE)))
      (RETURN ATF))) 
(PUT 'PASF_EVALATP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_EVALATP 'DEFINED-ON-LINE '288) 
(PUT 'PASF_EVALATP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_EVALATP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_EVALATP (REL LHS)
    (COND
     ((AND (PAIRP REL) (MEMQ (CAR REL) '(CONG NCONG)))
      (COND
       ((OR (ATOM (CDR REL)) (ATOM (CAR (CDR REL))))
        (PASF_EVALATPM (CAR REL) LHS (CDR REL)))
       (T (REDERR (LIST "pasf_evalatp : parametric modulus in input")))))
     (T (PASF_EVALATPM REL LHS NIL)))) 
(PUT 'PASF_EVALATPM 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_EVALATPM 'DEFINED-ON-LINE '299) 
(PUT 'PASF_EVALATPM 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_EVALATPM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_EVALATPM (REL LHS M)
    (COND ((EQ REL 'EQUAL) (OR (NULL LHS) (EQUAL LHS 0)))
          ((EQ REL 'NEQ) (NOT (OR (NULL LHS) (EQUAL LHS 0))))
          ((EQ REL 'LEQ) (OR (MINUSF LHS) (NULL LHS) (EQUAL LHS 0)))
          ((EQ REL 'GEQ) (NOT (MINUSF LHS))) ((EQ REL 'LESSP) (MINUSF LHS))
          ((EQ REL 'GREATERP) (NOT (OR (MINUSF LHS) (NULL LHS) (EQUAL LHS 0))))
          ((EQ REL 'CONG)
           (OR (OR (NULL LHS) (EQUAL LHS 0)) (EQUAL 0 (REMAINDER LHS M))))
          ((EQ REL 'NCONG)
           (NOT
            (OR (OR (NULL LHS) (EQUAL LHS 0)) (EQUAL 0 (REMAINDER LHS M)))))
          (T (REDERR (LIST "pasf_evalatp: unknown operator" REL))))) 
(PUT 'PASF_FACT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_FACT 'DEFINED-ON-LINE '316) 
(PUT 'PASF_FACT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_FACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_FACT (ATF)
    (PROG (FAC OP M)
      (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) (RETURN ATF)))
      (SETQ OP (CAR ATF))
      (SETQ FAC (FCTRF (CADR ATF)))
      (COND ((LESSP (LENGTH FAC) 3) (RETURN ATF)))
      (COND
       ((MEMQ OP '(EQUAL NEQ))
        (RETURN
         (CONS (COND ((EQ OP 'EQUAL) 'OR) (T 'AND))
               (PROG (FCT FORALL-RESULT FORALL-ENDPTR)
                 (SETQ FCT (CDR FAC))
                 (COND ((NULL FCT) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (FCT) (LIST OP (CAR FCT) NIL))
                                   (CAR FCT))
                                  NIL)))
                LOOPLABEL
                 (SETQ FCT (CDR FCT))
                 (COND ((NULL FCT) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (FCT) (LIST OP (CAR FCT) NIL)) (CAR FCT))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND
       ((MEMQ OP '(LEQ LESSP GEQ GREATERP))
        (RETURN
         (PASF_FACT1 (CDR FAC)
          (COND ((MINUSF (CAR FAC)) (PASF_ANEGREL OP)) (T OP))))))
      (RETURN ATF))) 
(PUT 'PASF_FACT1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_FACT1 'DEFINED-ON-LINE '337) 
(PUT 'PASF_FACT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFSIAT.RED) 
(PUT 'PASF_FACT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_FACT1 (FAC OP)
    (COND ((NULL (CDR FAC)) (LIST OP (CAAR FAC) NIL))
          ((NEQ (REMAINDER (CDAR FAC) 2) 0)
           (CONS 'OR
                 (LIST
                  (CONS 'AND
                        (LIST (LIST OP (CAAR FAC) NIL)
                              (COND
                               ((MEMQ OP '(GEQ GREATERP))
                                (PASF_FACT1 (CDR FAC) OP))
                               (T (PASF_FACT1 (CDR FAC) (PASF_ANEGREL OP))))))
                  (CONS 'AND
                        (LIST (LIST (PASF_ANEGREL OP) (CAAR FAC) NIL)
                              (COND
                               ((MEMQ OP '(GEQ GREATERP))
                                (PASF_FACT1 (CDR FAC) (PASF_ANEGREL OP)))
                               (T (PASF_FACT1 (CDR FAC) OP))))))))
          (T (PASF_FACT1 (CDR FAC) OP)))) 
(ENDMODULE) 
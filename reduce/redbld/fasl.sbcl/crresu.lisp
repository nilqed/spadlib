(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RESULTANTE)) 
(FLAG '(BRESULTANT) 'OPFN) 
(PUT 'ERR_CATCH_RESUL 'NUMBER-OF-ARGS 3) 
(PUT 'ERR_CATCH_RESUL 'DEFINED-ON-LINE '57) 
(PUT 'ERR_CATCH_RESUL 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'ERR_CATCH_RESUL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_RESUL (L1 L2 V)
    (PROG (H BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_ELIMIN))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_ELIMIN)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'BRESULTANT (MKQUOTE L1) (MKQUOTE L2) (MKQUOTE V)) NIL
                  NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'DEGFL 'NUMBER-OF-ARGS 2) 
(PUT 'DEGFL 'DEFINED-ON-LINE '72) 
(PUT 'DEGFL 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'DEGFL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGFL (L F)
    (PROG (DEL)
      (SETQ DEL (GET L 'DERIVS))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND DEL (NEQ F (CAAAR DEL)))) (RETURN NIL)))
        (SETQ DEL (CDR DEL))
        (GO WHILELABEL))
      (RETURN (CDAR DEL)))) 
(PUT 'ADD_RES_WITH 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_RES_WITH 'DEFINED-ON-LINE '79) 
(PUT 'ADD_RES_WITH 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'ADD_RES_WITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_RES_WITH (F DE1 DE2)
    (PROG (A B)
      (SETQ A (GET DE2 'RES_WITH))
      (SETQ B (ASSOC F A))
      (COND
       (B (PROGN (SETQ A (DELETE B A)) (SETQ B (CONS F (CONS DE1 (CDR B))))))
       (T (SETQ B (LIST F DE1))))
      (PUT DE2 'RES_WITH (CONS B A)))) 
(PUT 'ADD_BOTH_RES_WITH 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_BOTH_RES_WITH 'DEFINED-ON-LINE '88) 
(PUT 'ADD_BOTH_RES_WITH 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'ADD_BOTH_RES_WITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_BOTH_RES_WITH (F DE1 DE2)
    (PROG () (ADD_RES_WITH F DE1 DE2) (ADD_RES_WITH F DE2 DE1))) 
(PUT 'DO_ONE_RESULTANT 'NUMBER-OF-ARGS 1) 
(PUT 'DO_ONE_RESULTANT 'DEFINED-ON-LINE '96) 
(PUT 'DO_ONE_RESULTANT 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'DO_ONE_RESULTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DO_ONE_RESULTANT (ARGLIST)
    (PROG (PDES CPU GC P AVF DEL L L1 L2 WITH1 FCTS1 NVARS1 TERMS1 ALLVARFCTS1
           H FOUNDBETTERPAIR BEST1 BEST2 BESTF BESTNFCTS BESTSIZE D1 D2 LTD)
      (SETQ CPU (TIME))
      (SETQ GC (GCTIME))
      (SETQ PDES (CAR ARGLIST))
      (COND (EXPERT_MODE (SETQ L (SELECTPDES PDES 2))) (T (SETQ L PDES)))
      (PROG (P)
        (SETQ P L)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((NOT (PAIRP (GET P 'FAC)))
             (PROGN
              (SETQ AVF (GET P 'ALLVARFCTS))
              (COND
               (AVF
                (PROGN
                 (SETQ DEL (GET P 'DERIVS))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND DEL
                           (OR (NULL (CDAAR DEL)) (FREEOF AVF (CAAAR DEL)))))
                     (RETURN NIL)))
                   (SETQ DEL (CDR DEL))
                   (GO WHILELABEL))
                 (COND ((NULL DEL) (SETQ L1 (CONS P L1))))
                 NIL)))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND ((NULL L1) (RETURN NIL)))
      (SETQ L L1)
      (SETQ BESTNFCTS 10000)
      (SETQ L1 (CAR L))
      (SETQ L (CDR L))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ WITH1 (GET L1 'RES_WITH))
         (SETQ FCTS1 (GET L1 'FCTS))
         (SETQ NVARS1 (GET L1 'NVARS))
         (SETQ TERMS1 (GET L1 'TERMS))
         (SETQ ALLVARFCTS1 (GET L1 'ALLVARFCTS))
         (PROG (L2)
           (SETQ L2 L)
          LAB
           (COND ((NULL L2) (RETURN NIL)))
           ((LAMBDA (L2)
              (COND
               ((AND (NULL (MEMBER L2 WITH1)) (EQUAL NVARS1 (GET L2 'NVARS))
                     (EQUAL ALLVARFCTS1 (GET L2 'ALLVARFCTS)))
                (PROGN
                 (COND
                  ((LESSP (SETQ H (LENGTH (UNION FCTS1 (GET L2 'FCTS))))
                          BESTNFCTS)
                   (PROGN
                    (SETQ BESTNFCTS H)
                    (SETQ BESTSIZE (PLUS TERMS1 (GET L2 'TERMS)))
                    (SETQ FOUNDBETTERPAIR T)))
                  ((EQUAL H BESTNFCTS)
                   (PROGN
                    (SETQ H (PLUS TERMS1 (GET L2 'TERMS)))
                    (COND
                     ((LESSP H BESTSIZE)
                      (PROGN (SETQ BESTSIZE H) (SETQ FOUNDBETTERPAIR T)))
                     (T (SETQ FOUNDBETTERPAIR NIL)))
                    NIL))
                  (T (SETQ FOUNDBETTERPAIR NIL)))
                 (COND
                  (FOUNDBETTERPAIR
                   (PROGN
                    (SETQ BEST1 L1)
                    (SETQ BEST2 L2)
                    (SETQ BESTF (CAR ALLVARFCTS1))
                    (COND
                     (NIL
                      (PROGN
                       (SETQ LTD 10000)
                       (PROG (F)
                         (SETQ F ALLVARFCTS1)
                        LAB
                         (COND ((NULL F) (RETURN NIL)))
                         ((LAMBDA (F)
                            (PROGN
                             (SETQ D1 (DEGFL L1 F))
                             (SETQ D2 (DEGFL L2 F))
                             (COND ((GREATERP D2 D1) (SETQ D1 D2)))
                             (COND
                              ((LESSP D1 LTD)
                               (PROGN (SETQ LTD D1) (SETQ BESTF F))))))
                          (CAR F))
                         (SETQ F (CDR F))
                         (GO LAB)))))
                    NIL)))))))
            (CAR L2))
           (SETQ L2 (CDR L2))
           (GO LAB))
         (SETQ L1 (CAR L))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (COND
       ((LESSP BESTNFCTS 10000)
        (PROGN
         (SETQ PDES (ONE_RESULTANT PDES BEST1 BEST2 BESTF))
         (ADD_BOTH_RES_WITH BESTF BEST1 BEST2)))
       (T (SETQ PDES NIL)))
      (COND
       ((AND PRINT_ *TIME)
        (PROGN
         (PROGN
          (PRIN2 " time : ")
          (PRIN2 (DIFFERENCE (TIME) CPU))
          (PRIN2 " ms    GC time : ")
          (PRIN2 (DIFFERENCE (GCTIME) GC))
          (PRIN2 " ms.      ")
          NIL))))
      (RETURN (COND ((NULL PDES) NIL) (T (LIST PDES (CADR ARGLIST))))))) 
(PUT 'COMP_RESULTANT 'NUMBER-OF-ARGS 1) 
(PUT 'COMP_RESULTANT 'DEFINED-ON-LINE '242) 
(PUT 'COMP_RESULTANT 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'COMP_RESULTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMP_RESULTANT (PDES)
    (PROG (S L)
      (SETQ L (SELECTPDES PDES 2))
      (COND
       ((EQUAL (LENGTH (SETQ S (GET (CAR L) 'DERIVS))) 1)
        (PROGN
         (SETQ S (CAAR S))
         (SETQ S (COND ((NULL (CDR S)) (CAR S)) (T (CONS 'DF S))))
         (COND
          ((FREEOF (GET (CADR L) 'KERN) S)
           (PROGN
            (PROGN (PRIN2 "No common kernel in both equations.") NIL)
            (TERPRI))))))
       ((EQUAL (LENGTH (SETQ S (GET (CADR L) 'DERIVS))) 1)
        (PROGN
         (SETQ S (CAAR S))
         (SETQ S (COND ((NULL (CDR S)) (CAR S)) (T (CONS 'DF S))))
         (COND
          ((FREEOF (GET (CAR L) 'KERN) S)
           (PROGN
            (PROGN (PRIN2 "No common kernel in both equations.") NIL)
            (TERPRI))))))
       (T
        (PROGN
         (CHANGE_PROMPT_TO "Which function/derivative shall be eliminated? ")
         (SETQ S (TERMREAD))
         (RESTORE_INTERACTIVE_PROMPT)
         (COND
          ((OR (FREEOF (GET (CAR L) 'KERN) S) (FREEOF (GET (CADR L) 'KERN) S))
           (PROGN
            (PROGN (PRIN2 "This is not a kernel in both equations.") NIL)
            (TERPRI)))))))
      (SETQ L (ONE_RESULTANT PDES (CAR L) (CADR L) S))
      (RETURN (COND (L L) (T PDES))))) 
(PUT 'ONE_RESULTANT 'NUMBER-OF-ARGS 4) 
(PUT 'ONE_RESULTANT 'DEFINED-ON-LINE '276) 
(PUT 'ONE_RESULTANT 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'ONE_RESULTANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ONE_RESULTANT (PDES E1 E2 S)
    (PROG (RATIONAL_BAK EZGCD_BAK GCD_BAK H RES)
      (ON (LIST 'BEZOUT))
      (COND
       (*RATIONAL
        (PROGN (AEVAL (OFF (LIST 'RATIONAL))) (SETQ RATIONAL_BAK T))))
      (COND (*EZGCD (SETQ EZGCD_BAK T)) (T (AEVAL (ON (LIST 'EZGCD)))))
      (COND (*GCD (SETQ GCD_BAK T)) (T (AEVAL (ON (LIST 'GCD)))))
      (SETQ RES
              (ERR_CATCH_RESUL (LIST '*SQ (GET E1 'SQVAL) NIL)
               (LIST '*SQ (GET E2 'SQVAL) NIL) S))
      (COND ((NULL EZGCD_BAK) (AEVAL (OFF (LIST 'EZGCD)))))
      (COND ((NULL GCD_BAK) (AEVAL (OFF (LIST 'GCD)))))
      (COND (RATIONAL_BAK (AEVAL (ON (LIST 'RATIONAL)))))
      (COND ((NULL RES) (RETURN NIL)))
      (COND
       ((EQUAL RES '(LIST 0))
        (PROGN
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "Polynomials ")
             (PRIN2 E1)
             (PRIN2 ",")
             (PRIN2 E2)
             (PRIN2 " have a common factor and have now")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "at least each 2 factors recorded in their properties.")
             NIL)
            (TERPRI)
            NIL)))
         (SETQ RES
                 (ERR_CATCH_GCD (LIST '*SQ (GET E1 'SQVAL) NIL)
                  (LIST '*SQ (GET E2 'SQVAL) NIL)))
         (SETQ RES (SIMP RES))
         (SETQ H (GET E1 'FAC))
         (COND
          ((AND (OR (EQUAL H NIL) (FIXP H)) (NEQ RES (GET E1 'SQVAL)))
           (PUT E1 'FAC (LIST RES (MULTSQ (GET E1 'SQVAL) (INVSQ RES))))))
         (SETQ H (GET E2 'FAC))
         (COND
          ((AND (OR (EQUAL H NIL) (FIXP H)) (NEQ RES (GET E2 'SQVAL)))
           (PUT E2 'FAC (LIST RES (MULTSQ (GET E2 'SQVAL) (INVSQ RES))))))
         NIL))
       (T
        (PROGN
         (SETQ RES
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H (CDR RES))
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (H) (SIMP* (CADR H))) (CAR H))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (H) (SIMP* (CADR H))) (CAR H)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ RES
                 (MKEQSQ NIL RES NIL (UNION (GET E1 'FCTS) (GET E2 'FCTS))
                  (UNION (GET E1 'VARS) (GET E2 'VARS)) ALLFLAGS_ T (LIST 0)
                  NIL PDES))
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "Computing the resultant for ")
             (PRIN2 E1)
             (PRIN2 ", ")
             (PRIN2 E2)
             (PRIN2 " wrt. ")
             (PRIN2 S)
             (PRIN2 " gives the new equation ")
             (PRIN2 RES)
             (PRIN2 ".")
             NIL)
            (TERPRI)
            NIL)))
         (SETQ PDES (EQINSERT RES PDES)))))
      (RETURN PDES))) 
(PUT 'BRESULTANT 'NUMBER-OF-ARGS 3) 
(PUT 'BRESULTANT 'DEFINED-ON-LINE '326) 
(PUT 'BRESULTANT 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'BRESULTANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BRESULTANT (U V VAR)
    (PROG (X *EXP)
      (SETQ *EXP T)
      (SETQ U (CAR (SIMP* U)))
      (SETQ V (CAR (SIMP* V)))
      (SETQ VAR (*A2K VAR))
      (COND
       ((AND (OR (ATOM U) (ATOM (CAR U))) (OR (ATOM V) (ATOM (CAR V))))
        (RETURN (LIST 'LIST 1))))
      (SETQ KORD* (CONS VAR KORD*))
      (COND
       ((AND (NULL (OR (ATOM U) (ATOM (CAR U)))) (NULL (EQ (CAAAR U) VAR)))
        (SETQ U (REORDER U))))
      (COND
       ((AND (NULL (OR (ATOM V) (ATOM (CAR V)))) (NULL (EQ (CAAAR V) VAR)))
        (SETQ V (REORDER V))))
      (SETQ X (BEZOUT_RESULTANT_FAC U V VAR))
      (SETKORDER (CDR KORD*))
      (RETURN X))) 
(PUT 'BEZOUT_RESULTANT_FAC 'NUMBER-OF-ARGS 3) 
(PUT 'BEZOUT_RESULTANT_FAC 'DEFINED-ON-LINE '344) 
(PUT 'BEZOUT_RESULTANT_FAC 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'BEZOUT_RESULTANT_FAC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BEZOUT_RESULTANT_FAC (U V W)
    (PROG (N NM AP EP UH UT VH VT X CX CXF CEP CUH CVH RES_VERBOSE)
      (SETQ N 0)
      (SETQ NM 0)
      (SETQ RES_VERBOSE NIL)
      (COND
       (RES_VERBOSE (PROGN (PROGN (PRIN2 "entering bezout.") NIL) (TERPRI))))
      (SETQ *EXP T)
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NULL (EQ (CAAAR U) W)))
        (RETURN
         (COND
          ((AND (NOT (OR (ATOM V) (ATOM (CAR V)))) (EQ (CAAAR V) W))
           (LIST 'LIST (MK*SQ (CONS (EXPTF U (CDAAR V)) 1))))
          (T (LIST 'LIST 1)))))
       ((OR (OR (ATOM V) (ATOM (CAR V))) (NULL (EQ (CAAAR V) W)))
        (RETURN
         (COND
          ((EQ (CAAAR U) W) (LIST 'LIST (MK*SQ (CONS (EXPTF V (CDAAR U)) 1))))
          (T (LIST 'LIST 1))))))
      (SETQ N (DIFFERENCE (CDAAR V) (CDAAR U)))
      (SETQ CXF '(1))
      (COND
       ((LESSP N 0)
        (PROGN
         (SETQ X U)
         (SETQ U V)
         (SETQ V X)
         (SETQ CXF '(-1))
         (SETQ N (MINUS N)))))
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
      (SETQ CX 1)
      (SETQ CUH UH)
      (SETQ CVH VH)
      (SETQ CX (GCDF* UH VH))
      (COND
       ((EQUAL CX 1)
        (COND
         (RES_VERBOSE
          (PROGN
           (PROGN (PRIN2 "Found trivial factor from headpolys only. ") NIL)
           (TERPRI)))
         (T NIL)))
       (T
        (PROGN
         (COND
          (RES_VERBOSE
           (PROGN
            (PROGN
             (PRIN2 "Found factor from headpolys with ")
             (PRIN2 (TERMSF CX))
             (PRIN2 " terms.")
             NIL)
            (TERPRI))))
         (SETQ CXF (FCTRF CX)))))
      (SETQ AP
              (ADDF
               ((LAMBDA (G141)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G141 VT))
                        (T (POLY-MULTF G141 VT))))
                (QUOTF1 UH CX))
               (NEGF
                ((LAMBDA (G143)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G143 UT))
                         (T (POLY-MULTF G143 UT))))
                 (QUOTF1 VH CX)))))
      (SETQ X (*SF2EXB AP W))
      (COND ((CDR CXF) (SETQ X (|B:TRY_PREVIOUS_FACTORS| X (CDR CXF)))))
      (SETQ CX (|B:COMFAC| X))
      (COND
       ((NEQ CX 1)
        (PROGN
         (SETQ CXF (BFAC-MERGE (FCTRF CX) CXF))
         (COND
          (RES_VERBOSE
           (PROGN
            (PROGN
             (PRIN2 "commom factor cx found. ")
             (PRIN2 (TMSF CX))
             (PRIN2 " terms.")
             NIL)
            (TERPRI)))))))
      (SETQ X (|B:CQUOT| X CX))
      (SETQ EP (|B:EXTMULT| X EP))
      (COND ((CDR CXF) (SETQ EP (|B:TRY_PREVIOUS_FACTORS| EP (CDR CXF)))))
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
                          ((LAMBDA (G145)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G145 UH))
                                   (T (POLY-MULTF G145 UH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((NULL (OR (EQUAL CUH 1) (EQUAL CVH 1)))
              (SETQ CUH (GCDF* (CDAR UT) CUH))))
            (SETQ UT (CDR UT))))
          (T
           (SETQ UH
                   ((LAMBDA (G147)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G147 UH))
                            (T (POLY-MULTF G147 UH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (COND
          ((EQUAL (DEGR VT W) J)
           (PROGN
            (SETQ VH
                    (ADDF (CDAR VT)
                          ((LAMBDA (G149)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G149 VH))
                                   (T (POLY-MULTF G149 VH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((NULL (OR (EQUAL CUH 1) (EQUAL CVH 1)))
              (SETQ CVH (GCDF* (CDAR VT) CVH))))
            (SETQ VT (CDR VT))))
          (T
           (SETQ VH
                   ((LAMBDA (G151)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G151 VH))
                            (T (POLY-MULTF G151 VH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (SETQ CX 1)
         (SETQ CX (GCDF* CUH CVH))
         (COND
          ((EQUAL CX 1)
           (COND
            (RES_VERBOSE
             (PROGN
              (PROGN (PRIN2 "Found trivial factor from headpolys only. ") NIL)
              (TERPRI)))
            (T NIL)))
          (T
           (PROGN
            (COND
             (RES_VERBOSE
              (PROGN
               (PROGN
                (PRIN2 "Found factor from headpolys with ")
                (PRIN2 (TERMSF CX))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (BFAC-MERGE (FCTRF CX) CXF)))))
         (SETQ X
                 (*SF2EXB
                  (ADDF
                   ((LAMBDA (G153)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G153 VT))
                            (T (POLY-MULTF G153 VT))))
                    (QUOTF1 UH CX))
                   (NEGF
                    ((LAMBDA (G155)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G155 UT))
                             (T (POLY-MULTF G155 UT))))
                     (QUOTF1 VH CX))))
                  W))
         (COND ((CDR CXF) (SETQ X (|B:TRY_PREVIOUS_FACTORS| X (CDR CXF)))))
         (SETQ CX (|B:COMFAC| X))
         (COND
          ((NEQ CX 1)
           (PROGN
            (COND
             (RES_VERBOSE
              (PROGN
               (PROGN
                (PRIN2 "commom factor cx found. ")
                (PRIN2 (TMSF CX))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (BFAC-MERGE (FCTRF CX) CXF))
            NIL)))
         (SETQ X (|B:CQUOT| X CX))
         (SETQ EP (|B:EXTMULT| X EP))
         (COND ((CDR CXF) (SETQ EP (|B:TRY_PREVIOUS_FACTORS| EP (CDR CXF)))))
         (COND
          ((NEQ J 1)
           (PROGN
            (SETQ CEP (|B:COMFAC| EP))
            (COND
             ((NEQ CEP 1)
              (PROGN
               (SETQ CXF (BFAC-MERGE (FCTRF CEP) CXF))
               (COND
                (RES_VERBOSE
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cep found. ")
                   (PRIN2 (TMSF CEP))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI)))))))
            (SETQ EP (|B:CQUOT| EP CEP))))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (COND
       ((NEQ N 0)
        (PROGN
         (SETQ X (*SF2EXB U W))
         (SETQ CX (|B:COMFAC| X))
         (COND
          ((NEQ CX 1)
           (PROGN
            (COND
             (RES_VERBOSE
              (PROGN
               (PROGN
                (PRIN2 "commom factor cx found.")
                (PRIN2 (TMSF CX))
                (PRIN2 " terms")
                NIL)
               (TERPRI))))
            (SETQ CXF (BFAC-MERGE (FCTRF CX) CXF)))))
         (SETQ X (|B:CQUOT| X CX))
         (SETQ EP (|B:EXTMULT| X EP))
         (SETQ CEP (|B:COMFAC| EP))
         (COND
          ((NEQ CEP 1)
           (PROGN
            (COND
             (RES_VERBOSE
              (PROGN
               (PROGN
                (PRIN2 "commom factor cep found. ")
                (PRIN2 (TMSF CEP))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (BFAC-MERGE (FCTRF CEP) CXF)))))
         (SETQ EP (|B:CQUOT| EP CEP))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) J)) (RETURN NIL)))
           (PROGN
            (SETQ X
                    (*SF2EXB
                     ((LAMBDA (G544)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 U))
                              (T (POLY-MULTF G544 U))))
                      (LIST (CONS (CONS W J) 1)))
                     W))
            (SETQ CX (|B:COMFAC| X))
            (COND
             ((NEQ CX 1)
              (PROGN
               (COND
                (RES_VERBOSE
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cx found. ")
                   (PRIN2 (TMSF CX))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI))))
               (SETQ CXF (BFAC-MERGE (FCTRF CX) CXF)))))
            (SETQ X (|B:CQUOT| X CX))
            (SETQ EP (|B:EXTMULT| X EP))
            (SETQ CEP (|B:COMFAC| EP))
            (COND
             ((NEQ CEP 1)
              (PROGN
               (SETQ CXF (BFAC-MERGE (FCTRF CEP) CXF))
               (COND
                (RES_VERBOSE
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cep found. ")
                   (PRIN2 (TMSF CEP))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI)))))))
            (SETQ EP (|B:CQUOT| EP CEP))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (RETURN
       (COND ((NULL EP) (LIST 'LIST 0))
             ((OR (ATOM (CDAR EP)) (ATOM (CAR (CDAR EP))))
              (CONS 'LIST
                    (CONS
                     (LIST 'LIST
                           (COND
                            (*PHYSOP-LOADED (PHYSOP-MULTF (CAR CXF) (CDAR EP)))
                            (T (POLY-MULTF (CAR CXF) (CDAR EP)))))
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CDR CXF))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J)
                                           (LIST 'LIST (MK*SQ (CONS (CAR J) 1))
                                                 (CDR J)))
                                         (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (LIST 'LIST (MK*SQ (CONS (CAR J) 1))
                                         (CDR J)))
                                 (CAR J))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
             (T
              (CONS 'LIST
                    (CONS (LIST 'LIST (CAR CXF))
                          (CONS (LIST 'LIST (MK*SQ (CONS (CDAR EP) 1)) 1)
                                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ J (CDR CXF))
                                  (COND ((NULL J) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J)
                                                      (LIST 'LIST
                                                            (MK*SQ
                                                             (CONS (CAR J) 1))
                                                            (CDR J)))
                                                    (CAR J))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ J (CDR J))
                                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (J)
                                              (LIST 'LIST
                                                    (MK*SQ (CONS (CAR J) 1))
                                                    (CDR J)))
                                            (CAR J))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))))))))) 
(PUT '|B:COMFAC| 'NUMBER-OF-ARGS 1) 
(PUT '|B:COMFAC| 'DEFINED-ON-LINE '470) 
(PUT '|B:COMFAC| 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT '|B:COMFAC| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |B:COMFAC| (U)
    (PROG (*EZGCD X)
      (SETQ *EZGCD T)
      (COND ((NULL U) (RETURN 1)))
      (SETQ X (CDAR U))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (PROGN (RETURN X))))
      (SETQ X (GCDF* (CDAR U) X))
      (GO A))) 
(PUT '|B:CQUOT| 'NUMBER-OF-ARGS 2) 
(PUT '|B:CQUOT| 'DEFINED-ON-LINE '483) 
(PUT '|B:CQUOT| 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT '|B:CQUOT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:CQUOT| (U V)
    (COND ((NULL U) NIL)
          (T (CONS (CONS (CAAR U) (QUOTF1 (CDAR U) V)) (|B:CQUOT| (CDR U) V))))) 
(PUT '|B:TRY_PREVIOUS_FACTORS| 'NUMBER-OF-ARGS 2) 
(PUT '|B:TRY_PREVIOUS_FACTORS| 'DEFINED-ON-LINE '487) 
(PUT '|B:TRY_PREVIOUS_FACTORS| 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT '|B:TRY_PREVIOUS_FACTORS| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:TRY_PREVIOUS_FACTORS| (U V)
    (PROG (X)
     B
      (SETQ X (|B:CTRIALDIV| U (CAAR V)))
      (COND ((NULL X) (GO A)))
      (SETQ U X)
      (RPLACD (CAR V) (PLUS (CDAR V) 1))
      (GO B)
     A
      (SETQ V (CDR V))
      (COND ((NULL V) (RETURN U)))
      (GO B))) 
(PUT '|B:CTRIALDIV| 'NUMBER-OF-ARGS 2) 
(PUT '|B:CTRIALDIV| 'DEFINED-ON-LINE '500) 
(PUT '|B:CTRIALDIV| 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT '|B:CTRIALDIV| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |B:CTRIALDIV| (U V)
    (PROG (RES W X)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (QUOTF1 U V))))
      (COND
       ((SETQ X (QUOTF1 (CDAR U) V))
        (SETQ RES (SETQ W (CONS (CONS (CAAR U) X) NIL))))
       (T (RETURN NIL)))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN RES)))
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (COND ((SETQ X (QUOTF1 U V)) (PROGN (RPLACD W X) (RETURN RES)))
              (T (RETURN NIL)))))
      (COND
       ((SETQ X (QUOTF1 (CDAR U) V)) (RPLACD W (CONS (CONS (CAAR U) X) NIL)))
       (T (RETURN NIL)))
      (SETQ W (CDR W))
      (GO A))) 
(PUT 'BFAC-MERGE 'NUMBER-OF-ARGS 2) 
(PUT 'BFAC-MERGE 'DEFINED-ON-LINE '515) 
(PUT 'BFAC-MERGE 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'BFAC-MERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFAC-MERGE (U V)
    (COND
     ((NULL (CDR V))
      (CONS
       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) (CAR V)))
             (T (POLY-MULTF (CAR U) (CAR V))))
       (CDR U)))
     (T
      (CONS
       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) (CAR V)))
             (T (POLY-MULTF (CAR U) (CAR V))))
       (BFAC-MERGE2 (CDR U) (CDR V)))))) 
(PUT 'BFAC-MERGE2 'NUMBER-OF-ARGS 2) 
(PUT 'BFAC-MERGE2 'DEFINED-ON-LINE '519) 
(PUT 'BFAC-MERGE2 'DEFINED-IN-FILE 'CRACK/CRRESU.RED) 
(PUT 'BFAC-MERGE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFAC-MERGE2 (U V)
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
(ENDMODULE) 
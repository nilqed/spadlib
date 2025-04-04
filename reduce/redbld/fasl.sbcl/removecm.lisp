(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REMOVECM)) 
(FLUID '(INTVAR)) 
(EXPORTS (LIST 'REMOVECMSQ)) 
(IMPORTS
 (LIST 'ORDOP 'ADDF 'GCDN 'GCDF 'GCDK 'INVOLVESF 'DEPENDSP 'MAKEMAINVAR 'QUOTF)) 
(PUT 'REMOVECMSQ 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVECMSQ 'DEFINED-ON-LINE '42) 
(PUT 'REMOVECMSQ 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'REMOVECMSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVECMSQ (SQ) (CONS (REMOVECMSF (CAR SQ)) (REMOVECMSF (CDR SQ)))) 
(PUT 'REMOVECMSF 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVECMSF 'DEFINED-ON-LINE '45) 
(PUT 'REMOVECMSF 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'REMOVECMSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVECMSF (SF)
    (COND
     ((OR (ATOM SF) (NOT (ORDOP (CAAAR SF) INTVAR))
          (NOT (INVOLVESF SF INTVAR)))
      (COND (SF 1) (T NIL)))
     ((NULL (CDR SF))
      (COND
       ((DEPENDSP (CAAAR SF) INTVAR)
        (CONS (CONS (CAAR SF) (REMOVECMSF (CDAR SF))) NIL))
       (T (REMOVECMSF (CDAR SF)))))
     (T
      (PROG (U V)
        (SETQ U SF)
        (PROG ()
         WHILELABEL
          (COND ((NOT (SETQ V (INVOLVESF U INTVAR))) (RETURN NIL)))
          (SETQ U (CDAR (MAKEMAINVAR U V)))
          (GO WHILELABEL))
        (COND ((IEQUAL U 1) (RETURN SF)))
        (RETURN (QUOTF-FAIL SF (CMGCDF SF U))))))) 
(PUT 'CMGCDF 'NUMBER-OF-ARGS 2) 
(PUT 'CMGCDF 'DEFINED-ON-LINE '66) 
(PUT 'CMGCDF 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'CMGCDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CMGCDF (SF U)
    (COND
     ((NUMBERP U)
      (COND ((ATOM SF) (COND ((NULL SF) U) (T (GCDN SF U)))) ((EQUAL U 1) 1)
            (T (CMGCDF (CDR SF) (CMGCDF (CDAR SF) U)))))
     ((ATOM SF) (GCDF SF U))
     ((EQ (CAAAR U) (CAAAR SF))
      (COND ((ORDOP INTVAR (CAAAR U)) (GCDF SF U)) (T (CMGCDF2 SF U))))
     ((ORDOP (CAAAR SF) (CAAAR U)) (CMGCDF (CDR SF) (CMGCDF (CDAR SF) U)))
     (T (CMGCDF U SF)))) 
(PUT 'REMOVE-MAXDEG 'NUMBER-OF-ARGS 2) 
(PUT 'REMOVE-MAXDEG 'DEFINED-ON-LINE '85) 
(PUT 'REMOVE-MAXDEG 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'REMOVE-MAXDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE-MAXDEG (SF VAR)
    (COND ((ATOM SF) 0) ((EQ (CAAAR SF) VAR) (CDAAR SF))
          ((ORDOP VAR (CAAAR SF)) 0)
          (T (MAX (REMOVE-MAXDEG (CDAR SF) VAR) (REMOVE-MAXDEG (CDR SF) VAR))))) 
(PUT 'CMGCDF2 'NUMBER-OF-ARGS 2) 
(PUT 'CMGCDF2 'DEFINED-ON-LINE '94) 
(PUT 'CMGCDF2 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'CMGCDF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CMGCDF2 (SF U)
    (PROG (N)
      (SETQ N (REMOVE-MAXDEG SF INTVAR))
      (COND ((EQUAL N 0) (RETURN (GCDF SF U))))
     LOOP
      (COND ((EQUAL U 1) (RETURN 1)))
      (SETQ U (GCDF U (COLLECTTERMS SF INTVAR N)))
      (SETQ N (ISUB1 N))
      (COND ((LESSP N 0) (RETURN U)) (T (GO LOOP))))) 
(PUT 'COLLECTTERMS 'NUMBER-OF-ARGS 3) 
(PUT 'COLLECTTERMS 'DEFINED-ON-LINE '114) 
(PUT 'COLLECTTERMS 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'COLLECTTERMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COLLECTTERMS (SF VAR N)
    (COND ((ATOM SF) (COND ((EQUAL N 0) SF) (T NIL)))
          ((EQ (CAAAR SF) VAR)
           (COND ((EQUAL (CDAAR SF) N) (CDAR SF))
                 ((GREATERP (CDAAR SF) N) (COLLECTTERMS (CDR SF) VAR N))
                 (T NIL)))
          ((ORDOP VAR (CAAAR SF)) (COND ((EQUAL N 0) SF) (T NIL)))
          (T
           (PROG (V W)
             (SETQ V (COLLECTTERMS (CDAR SF) VAR N))
             (SETQ W (COLLECTTERMS (CDR SF) VAR N))
             (COND ((NULL V) (RETURN W))
                   (T (RETURN (ADDF W (CONS (CONS (CAAR SF) V) NIL))))))))) 
(PUT 'REMOVE-CONSTANTP 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE-CONSTANTP 'DEFINED-ON-LINE '154) 
(PUT 'REMOVE-CONSTANTP 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'REMOVE-CONSTANTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE-CONSTANTP (PF)
    (COND ((NUMBERP PF) T) ((ATOM PF) NIL)
          ((EQ (CAR PF) 'SQRT) (REMOVE-CONSTANTP (CADR PF)))
          ((OR (EQ (CAR PF) 'EXPT) (EQ (CAR PF) 'QUOTIENT))
           (AND (REMOVE-CONSTANTP (CADR PF)) (REMOVE-CONSTANTP (CADDR PF))))
          (T NIL))) 
(PUT 'REMOVE-CONST-CONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE-CONST-CONTENT 'DEFINED-ON-LINE '166) 
(PUT 'REMOVE-CONST-CONTENT 'DEFINED-IN-FILE 'ALGINT/REMOVECM.RED) 
(PUT 'REMOVE-CONST-CONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE-CONST-CONTENT (SF)
    (COND ((NUMBERP SF) SF)
          ((NULL (CDR SF))
           (COND
            ((REMOVE-CONSTANTP (CAAAR SF))
             (CONS (CONS (CAAR SF) (REMOVE-CONST-CONTENT (CDAR SF))) NIL))
            (T (REMOVE-CONST-CONTENT (CDAR SF)))))
          (T
           (PROG (U)
             (SETQ U (REMOVE-CONST-CONTENT (CDAR SF)))
             (COND ((EQUAL U 1) (RETURN U)))
             (RETURN (GCDF U (REMOVE-CONST-CONTENT (CDR SF)))))))) 
(ENDMODULE) 
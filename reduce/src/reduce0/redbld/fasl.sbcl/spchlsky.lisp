(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPCHLSKY)) 
(PUT 'SPCHOLESKY 'NUMBER-OF-ARGS 1) 
(PUT 'SPCHOLESKY 'DEFINED-ON-LINE '45) 
(PUT 'SPCHOLESKY 'DEFINED-IN-FILE 'SPARSE/SPCHLSKY.RED) 
(PUT 'SPCHOLESKY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCHOLESKY (MAT1)
    (PROG (COL X P IN_MAT L U I_TURNED_ROUNDED_ON VAL I J N)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ N 0)
      (COND
       ((NOT *ROUNDED)
        (PROGN (SETQ I_TURNED_ROUNDED_ON T) (ON (LIST 'ROUNDED)) NIL)))
      (COND
       ((NOT (MATRIXP MAT1))
        (REDERR "Error in spcholesky:  non matrix input.")))
      (COND
       ((NOT (SYMMETRICP MAT1))
        (REDERR "Error in spcholesky: input matrix is not symmetric.")))
      (SETQ IN_MAT (SP-COPY-VECT MAT1 NIL))
      (SETQ N (SPROW_DIM IN_MAT))
      (SETQ P (MKVECT N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW IN_MAT I))
         (COND ((EQUAL COL NIL) (SETQ COL (LIST (LIST NIL) (LIST NIL)))))
         (PROG (XX)
           (SETQ XX (CDR COL))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (COND
                ((EQUAL XX '(NIL))
                 (PROGN (SETQ J I) (SETQ VAL (FINDELEM2 IN_MAT I I))))
                (T (PROGN (SETQ J (CAR XX)) (SETQ VAL (CDR XX)) NIL)))
               (COND
                ((GEQ J I)
                 (PROGN
                  (SETQ X
                          (SPINNERPROD 1 1 (DIFFERENCE I 1) (LIST 'MINUS VAL)
                           COL (FINDROW IN_MAT J)))
                  (SETQ X (REVAL1 (LIST 'MINUS X) T))
                  (COND
                   ((EQUAL J I)
                    (PROGN
                     (COND
                      ((LEQ (GET_NUM_PART (COND ((FIXP X) X) (T (REVAL1 X T))))
                            0)
                       (REDERR
                        "Error in spcholesky: input matrix is not positive definite.")))
                     (PUTV P I (REVAL1 (LIST 'QUOTIENT 1 (LIST 'SQRT X)) T))
                     NIL))
                   (T
                    (PROGN
                     (LETMTR3 (LIST IN_MAT J I)
                      (REVAL1 (LIST 'TIMES X (GETV P I)) T) IN_MAT NIL)
                     NIL)))
                  NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ L (SPGET_L IN_MAT P N))
      (SETQ U (AEVAL (LIST 'TP L)))
      (COND (I_TURNED_ROUNDED_ON (OFF (LIST 'ROUNDED))))
      (RETURN (LIST 'LIST L U)))) 
(FLAG '(SPCHOLESKY) 'OPFN) 
(PUT 'SPGET_L 'NUMBER-OF-ARGS 3) 
(PUT 'SPGET_L 'DEFINED-ON-LINE '101) 
(PUT 'SPGET_L 'DEFINED-IN-FILE 'SPARSE/SPCHLSKY.RED) 
(PUT 'SPGET_L 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPGET_L (IN_MAT P SQ_SIZE)
    (PROG (L COL I J VAL)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ VAL 0)
      (SETQ L (MKEMPSPMAT SQ_SIZE (LIST 'SPM SQ_SIZE SQ_SIZE)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
        (PROGN
         (LETMTR3 (LIST L I I) (REVAL1 (LIST 'QUOTIENT 1 (GETV P I)) T) L NIL)
         (SETQ COL (FINDROW IN_MAT I))
         (PROG (XX)
           (SETQ XX (CDR COL))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ J (CAR XX))
               (SETQ VAL (CDR XX))
               (COND
                ((LESSP J I)
                 (PROGN
                  (COND ((EQUAL VAL 0) NIL)
                        (T (LETMTR3 (LIST L I J) VAL L NIL)))
                  NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN L))) 
(ENDMODULE) 
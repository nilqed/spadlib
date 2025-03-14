(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TALPBNF)) 
(REVISION 'TALPBNF "$Id: talpbnf.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'TALPBNF "Copyright (c) 2004-2009 by A. Dolzmann, T. Sturm") 
(PUT 'TALP_DNF 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_DNF 'DEFINED-ON-LINE '33) 
(PUT 'TALP_DNF 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_DNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_DNF (F)
    (COND (*RLBNFSAC ((LAMBDA (*RLSISO) (CL_DNF F)) T)) (T (CL_DNF F)))) 
(PUT 'TALP_CNF 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_CNF 'DEFINED-ON-LINE '41) 
(PUT 'TALP_CNF 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_CNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_CNF (F)
    (COND (*RLBNFSAC ((LAMBDA (*RLSISO) (CL_CNF F)) T)) (T (CL_CNF F)))) 
(PUT 'TALP_SUBSUMPTION 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SUBSUMPTION 'DEFINED-ON-LINE '49) 
(PUT 'TALP_SUBSUMPTION 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SUBSUMPTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SUBSUMPTION (L1 L2 GOR)
    (COND
     ((EQ GOR 'OR)
      (COND ((TALP_SUBSUMEP-AND L1 L2) 'KEEP2)
            ((TALP_SUBSUMEP-AND L2 L1) 'KEEP1)))
     ((TALP_SUBSUMEP-OR L1 L2) 'KEEP1) ((TALP_SUBSUMEP-OR L2 L1) 'KEEP2))) 
(PUT 'TALP_SUBSUMEP-AND 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SUBSUMEP-AND 'DEFINED-ON-LINE '63) 
(PUT 'TALP_SUBSUMEP-AND 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SUBSUMEP-AND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SUBSUMEP-AND (L1 L2)
    (PROG (A)
      (PROG ()
       WHILELABEL
        (COND ((NOT L2) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR L2))
         (SETQ L2 (CDR L2))
         (COND ((NEQ (CL_SIMPL A L1 (MINUS 1)) 'TRUE) (SETQ A (SETQ L2 NIL)))))
        (GO WHILELABEL))
      (RETURN A))) 
(PUT 'TALP_SUBSUMEP-OR 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SUBSUMEP-OR 'DEFINED-ON-LINE '75) 
(PUT 'TALP_SUBSUMEP-OR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SUBSUMEP-OR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SUBSUMEP-OR (L1 L2)
    (PROG (A)
      (PROG ()
       WHILELABEL
        (COND ((NOT L1) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR L1))
         (SETQ L1 (CDR L1))
         (COND
          ((NEQ
            (CL_SIMPL
             (COND ((AND L2 (CDR L2)) (CONS 'OR L2))
                   ((NULL L2) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                   (T (CAR L2)))
             (LIST A) (MINUS 1))
            'TRUE)
           (SETQ A (SETQ L1 NIL)))))
        (GO WHILELABEL))
      (RETURN A))) 
(PUT 'TALP_SACATLP 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SACATLP 'DEFINED-ON-LINE '87) 
(PUT 'TALP_SACATLP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SACATLP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SACATLP (A L)
    ((LAMBDA (W) (NOT (AND (NEQ (CADR A) (CADR W)) (ORDP (CADR A) (CADR W)))))
     (CAR L))) 
(PUT 'TALP_SACAT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SACAT 'DEFINED-ON-LINE '95) 
(PUT 'TALP_SACAT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SACAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SACAT (A1 A2 GOR)
    (PROG (W)
      (COND ((NEQ (CADR A1) (CADR A2)) (RETURN NIL)))
      (SETQ W (TALP_SACREL (CAR A1) (CAR A2) GOR))
      (COND ((MEMQ W '(DROP KEEP KEEP1 KEEP2)) (RETURN W)))
      (RETURN (LIST W (CADR A1) (CADDR A2))))) 
(PUT 'TALP_SACREL 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SACREL 'DEFINED-ON-LINE '113) 
(PUT 'TALP_SACREL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SACREL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SACREL (R1 R2 GOR)
    (COND ((EQ GOR 'OR) (TALP_SACREL-OR R1 R2)) (T (TALP_SACREL-AND R1 R2)))) 
(PUT 'TALP_SACREL-OR 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SACREL-OR 'DEFINED-ON-LINE '130) 
(PUT 'TALP_SACREL-OR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SACREL-OR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SACREL-OR (R1 R2)
    (PROG (W)
      (SETQ W
              '((EQUAL (EQUAL . KEEP) (NEQ . DROP))
                (NEQ (EQUAL . DROP) (NEQ . KEEP))))
      (RETURN (CDR (ATSOC R1 (CDR (ATSOC R2 W))))))) 
(PUT 'TALP_SACREL-AND 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SACREL-AND 'DEFINED-ON-LINE '147) 
(PUT 'TALP_SACREL-AND 'DEFINED-IN-FILE 'REDLOG/TALP/TALPBNF.RED) 
(PUT 'TALP_SACREL-AND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SACREL-AND (R1 R2)
    (PROG (W)
      (SETQ W
              '((EQUAL (EQUAL . KEEP) (NEQ . DROP))
                (NEQ (EQUAL . DROP) (NEQ . KEEP))))
      (RETURN (CDR (ATSOC R1 (CDR (ATSOC R2 W))))))) 
(ENDMODULE) 
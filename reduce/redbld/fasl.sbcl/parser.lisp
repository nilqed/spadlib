(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PARSER)) 
(FLUID '(*BACKTRACE)) 
(GLOBAL '(CURSYM*)) 
(PUT 'COMM1 'NUMBER-OF-ARGS 1) 
(PUT 'COMM1 'DEFINED-ON-LINE '45) 
(PUT 'COMM1 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'COMM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMM1 (U)
    (PROG (BOOL)
      (COND ((EQUAL U 'END) (SCAN)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (NOT
            (OR (EQUAL CURSYM* '*SEMICOL*)
                (AND (EQUAL U 'END)
                     (MEMQ CURSYM*
                           '(END ELSE THEN UNTIL *RPAR* *ENDGROUP*))))))
          (RETURN NIL)))
        (PROGN
         (COND
          ((AND (EQUAL U 'END) (NULL BOOL))
           (PROGN
            (LPRIM (LIST "END-COMMENT NO LONGER SUPPORTED"))
            (SETQ BOOL T))))
         (SCAN))
        (GO WHILELABEL)))) 
(PUT 'IFSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'IFSTAT 'DEFINED-ON-LINE '61) 
(PUT 'IFSTAT 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'IFSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE IFSTAT NIL
    (PROG (CONDX CONDIT)
     A
      (SETQ CONDX (XREAD T))
      (COND ((NOT (EQUAL CURSYM* 'THEN)) (SYMERR 'IF T)))
      (SETQ CONDIT (ACONC* CONDIT (LIST CONDX (XREAD T))))
      (COND ((NOT (EQUAL CURSYM* 'ELSE)) NIL) ((EQUAL (SCAN) 'IF) (GO A))
            (T (SETQ CONDIT (ACONC* CONDIT (LIST T (XREAD1 T))))))
      (RETURN (CONS 'COND CONDIT)))) 
(PUT 'IF 'STAT 'IFSTAT) 
(FLAG '(THEN ELSE) 'DELIM) 
(PUT 'FUNCTIONSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'FUNCTIONSTAT 'DEFINED-ON-LINE '79) 
(PUT 'FUNCTIONSTAT 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'FUNCTIONSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FUNCTIONSTAT NIL
    (PROG (X)
      (SETQ X (SCAN))
      (RETURN
       (LIST 'FUNCTION
             (COND ((EQUAL X '*LPAR*) (XREAD1 T))
                   ((AND (IDP X) (NULL (EQUAL X 'LAMBDA))) (PROGN (SCAN) X))
                   (T (SYMERR "Function" T))))))) 
(PUT 'FUNCTION 'STAT 'FUNCTIONSTAT) 
(PUT 'LAMSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'LAMSTAT 'DEFINED-ON-LINE '93) 
(PUT 'LAMSTAT 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'LAMSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LAMSTAT NIL
    (PROG (X Y)
      (SETQ X (XREAD 'LAMBDA))
      (COND (X (SETQ X (REMCOMMA X))))
      (SETQ Y (LIST 'LAMBDA X (XREAD T)))
      (RETURN Y))) 
(PUT 'LAMBDA 'STAT 'LAMSTAT) 
(PUT 'READPROGN 'NUMBER-OF-ARGS 0) 
(PUT 'READPROGN 'DEFINED-ON-LINE '106) 
(PUT 'READPROGN 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'READPROGN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READPROGN NIL
    (PROG (LST)
      (SETQ LST (LIST (XREAD 'GROUP)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL CURSYM* '*ENDGROUP*))) (RETURN NIL)))
        (SETQ LST (ACONC* LST (XREAD 'GROUP)))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (CONS 'PROGN LST)))) 
(PUT '*STARTGROUP* 'STAT 'READPROGN) 
(FLAG '(*STARTGROUP*) 'GO) 
(FLAG '(*ENDGROUP*) 'DELIM) 
(FLAG '(*ENDGROUP*) 'NODEL) 
(PUT 'READVECTOR 'NUMBER-OF-ARGS 0) 
(PUT 'READVECTOR 'DEFINED-ON-LINE '126) 
(PUT 'READVECTOR 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'READVECTOR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READVECTOR NIL
    (PROG (LST)
      (SETQ LST (LIST (XREAD 'GROUP)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL CURSYM* '*RSQBKT*))) (RETURN NIL)))
        (SETQ LST (ACONC* LST (XREAD 'GROUP)))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (CONS 'VECT LST)))) 
(PUT '*LSQBKT* 'STAT 'READVECTOR) 
(FLAG '(*LSQBKT*) 'GO) 
(FLAG '(*RSQBKT*) 'DELIM) 
(FLAG '(*RSQBKT*) 'NODEL) 
(PUT 'ENDSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'ENDSTAT 'DEFINED-ON-LINE '146) 
(PUT 'ENDSTAT 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'ENDSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ENDSTAT NIL (PROG (X) (SETQ X CURSYM*) (COMM1 'END) (RETURN (LIST X)))) 
(PUT 'END 'STAT 'ENDSTAT) 
(PUT 'ENDMODULE 'STAT 'ENDSTAT) 
(PUT 'BYE 'STAT 'ENDSTAT) 
(PUT 'QUIT 'STAT 'ENDSTAT) 
(FLAG '(BYE QUIT) 'EVAL) 
(PUT 'ENDSTAT1 'NUMBER-OF-ARGS 0) 
(PUT 'ENDSTAT1 'DEFINED-ON-LINE '166) 
(PUT 'ENDSTAT1 'DEFINED-IN-FILE 'RLISP/PARSER.RED) 
(PUT 'ENDSTAT1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ENDSTAT1 NIL
    (PROG (X OPTARG)
      (SETQ X CURSYM*)
      (SCAN)
      (COND ((STRINGP CURSYM*) (SETQ OPTARG CURSYM*)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (EQUAL CURSYM* '*SEMICOL*))
                (NOT (AND (IDP CURSYM*) (FLAGP CURSYM* 'DELIM)))))
          (RETURN NIL)))
        (SCAN)
        (GO WHILELABEL))
      (RETURN (LIST X OPTARG)))) 
(PUT 'SHOWTIME 'STAT 'ENDSTAT1) 
(PUT 'SHOWTIME1 'STAT 'ENDSTAT1) 
(PUT 'SHOWTIME2 'STAT 'ENDSTAT1) 
(PUT 'SHOWTIME3 'STAT 'ENDSTAT1) 
(PUT 'RESETTIME 'STAT 'ENDSTAT) 
(PUT 'RESETTIME1 'STAT 'ENDSTAT) 
(PUT 'RESETTIME2 'STAT 'ENDSTAT) 
(PUT 'RESETTIME3 'STAT 'ENDSTAT) 
(ENDMODULE) 
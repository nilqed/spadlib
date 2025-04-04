(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PATTDEFN)) 
(FLUID
 '(FREEVARS OP R P I UPB IDENTITY EXPAND ACONTRACT MCONTRACT COMB COUNT SYMM)) 
(DE BIND (U V) (PUT U 'BINDING (CONS V (GET U 'BINDING)))) 
(PUT 'BIND 'NUMBER-OF-ARGS 2) 
(PUT 'BIND 'DEFINED-ON-LINE '38) 
(PUT 'BIND 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'BIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'BIND 'INLINE '(LAMBDA (U V) (PUT U 'BINDING (CONS V (GET U 'BINDING))))) 
(DE BINDING (U) ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING))) 
(PUT 'BINDING 'NUMBER-OF-ARGS 1) 
(PUT 'BINDING 'DEFINED-ON-LINE '41) 
(PUT 'BINDING 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'BINDING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'BINDING 'INLINE
      '(LAMBDA (U) ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)))) 
(DE UNBIND (U) (PUT U 'BINDING (CDR (GET U 'BINDING)))) 
(PUT 'UNBIND 'NUMBER-OF-ARGS 1) 
(PUT 'UNBIND 'DEFINED-ON-LINE '44) 
(PUT 'UNBIND 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'UNBIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'UNBIND 'INLINE '(LAMBDA (U) (PUT U 'BINDING (CDR (GET U 'BINDING))))) 
(DE NEWENV (U) (PUT U 'BINDING (CONS 'UNBOUND (GET U 'BINDING)))) 
(PUT 'NEWENV 'NUMBER-OF-ARGS 1) 
(PUT 'NEWENV 'DEFINED-ON-LINE '47) 
(PUT 'NEWENV 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'NEWENV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'NEWENV 'INLINE
      '(LAMBDA (U) (PUT U 'BINDING (CONS 'UNBOUND (GET U 'BINDING))))) 
(DE RESTORENV (U) (PUT U 'BINDING (CDR (GET U 'BINDING)))) 
(PUT 'RESTORENV 'NUMBER-OF-ARGS 1) 
(PUT 'RESTORENV 'DEFINED-ON-LINE '50) 
(PUT 'RESTORENV 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'RESTORENV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'RESTORENV 'INLINE '(LAMBDA (U) (PUT U 'BINDING (CDR (GET U 'BINDING))))) 
(DE |PM:FREE| (U)
    (EQ ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)) 'UNBOUND)) 
(PUT '|PM:FREE| 'NUMBER-OF-ARGS 1) 
(PUT '|PM:FREE| 'DEFINED-ON-LINE '53) 
(PUT '|PM:FREE| 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT '|PM:FREE| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|PM:FREE| 'INLINE
      '(LAMBDA (U)
         (EQ ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)) 'UNBOUND))) 
(DE BOUND (U)
    ((LAMBDA (X) (AND X (NEQ X 'UNBOUND)))
     ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)))) 
(PUT 'BOUND 'NUMBER-OF-ARGS 1) 
(PUT 'BOUND 'DEFINED-ON-LINE '56) 
(PUT 'BOUND 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'BOUND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'BOUND 'INLINE
      '(LAMBDA (U)
         ((LAMBDA (X) (AND X (NEQ X 'UNBOUND)))
          ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING))))) 
(DE MEQ (U V)
    ((LAMBDA (X) (EQUAL (COND ((AND X (NEQ X 'UNBOUND)) X) (T U)) V))
     ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)))) 
(PUT 'MEQ 'NUMBER-OF-ARGS 2) 
(PUT 'MEQ 'DEFINED-ON-LINE '59) 
(PUT 'MEQ 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'MEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'MEQ 'INLINE
      '(LAMBDA (U V)
         ((LAMBDA (X) (EQUAL (COND ((AND X (NEQ X 'UNBOUND)) X) (T U)) V))
          ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING))))) 
(DE MVAL (U) U) 
(PUT 'MVAL 'NUMBER-OF-ARGS 1) 
(PUT 'MVAL 'DEFINED-ON-LINE '78) 
(PUT 'MVAL 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'MVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MVAL 'INLINE '(LAMBDA (U) U)) 
(PUT 'BSUBS 'NUMBER-OF-ARGS 1) 
(PUT 'BSUBS 'DEFINED-ON-LINE '82) 
(PUT 'BSUBS 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'BSUBS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BSUBS (U)
    (COND ((NULL U) U)
          ((ATOM U)
           (COND
            (((LAMBDA (X) (AND X (NEQ X 'UNBOUND)))
              ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)))
             ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)))
            (T U)))
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J U)
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (J) (BSUBS J)) (CAR J)) NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (BSUBS J)) (CAR J)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(DE IDENT (OP) (GET OP 'IDENTITY)) 
(PUT 'IDENT 'NUMBER-OF-ARGS 1) 
(PUT 'IDENT 'DEFINED-ON-LINE '89) 
(PUT 'IDENT 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'IDENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'IDENT 'INLINE '(LAMBDA (OP) (GET OP 'IDENTITY))) 
(DE GENP (U) (AND (ATOM U) (OR (GET U 'GEN) (MGENP U)))) 
(PUT 'GENP 'NUMBER-OF-ARGS 1) 
(PUT 'GENP 'DEFINED-ON-LINE '92) 
(PUT 'GENP 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'GENP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'GENP 'INLINE '(LAMBDA (U) (AND (ATOM U) (OR (GET U 'GEN) (MGENP U))))) 
(DE MGENP (U) (AND (ATOM U) (GET U 'MGEN))) 
(PUT 'MGENP 'NUMBER-OF-ARGS 1) 
(PUT 'MGENP 'DEFINED-ON-LINE '95) 
(PUT 'MGENP 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'MGENP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MGENP 'INLINE '(LAMBDA (U) (AND (ATOM U) (GET U 'MGEN)))) 
(DE SUCHP (U) (AND (NOT (ATOM U)) (EQ (CAR U) 'SUCH-THAT))) 
(PUT 'SUCHP 'NUMBER-OF-ARGS 1) 
(PUT 'SUCHP 'DEFINED-ON-LINE '98) 
(PUT 'SUCHP 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'SUCHP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SUCHP 'INLINE '(LAMBDA (U) (AND (NOT (ATOM U)) (EQ (CAR U) 'SUCH-THAT)))) 
(PUT 'CHK 'NUMBER-OF-ARGS 1) 
(PUT 'CHK 'DEFINED-ON-LINE '105) 
(PUT 'CHK 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'CHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHK (U)
    (OR (NULL U) (EQ U T)
        ((LAMBDA (X)
           (COND
            ((FREEXP X)
             ((LAMBDA (Y)
                (COND ((NULL Y) NIL) ((EQ Y T) (LIST X)) (T (CONS X Y))))
              (CHK (CDR U))))
            ((EQ (REVAL1 X T) T) (CHK (CDR U))) (T NIL)))
         (BSUBS (CAR U))))) 
(PUT 'FINDNEWVARS 'NUMBER-OF-ARGS 1) 
(PUT 'FINDNEWVARS 'DEFINED-ON-LINE '114) 
(PUT 'FINDNEWVARS 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'FINDNEWVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDNEWVARS (U)
    (COND
     ((ATOM U)
      (COND ((AND (ATOM U) (OR (GET U 'GEN) (MGENP U))) (LIST U)) (T NIL)))
     (T
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J U)
       STARTOVER
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT ((LAMBDA (J) (FINDNEWVARS J)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ J (CDR J))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR ((LAMBDA (J) (FINDNEWVARS J)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ J (CDR J))
        (GO LOOPLABEL))))) 
(PUT 'FREEXP 'NUMBER-OF-ARGS 1) 
(PUT 'FREEXP 'DEFINED-ON-LINE '118) 
(PUT 'FREEXP 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'FREEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREEXP (U)
    (COND
     ((ATOM U)
      (EQ ((LAMBDA (X) (COND (X (CAR X)))) (GET U 'BINDING)) 'UNBOUND))
     (T (OR (FREEXP (CAR U)) (FREEXP (CDR U)))))) 
(PUT 'GENEXP 'NUMBER-OF-ARGS 1) 
(PUT 'GENEXP 'DEFINED-ON-LINE '121) 
(PUT 'GENEXP 'DEFINED-IN-FILE 'PM/PATTDEFN.RED) 
(PUT 'GENEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENEXP (U)
    (COND ((ATOM U) (AND (ATOM U) (OR (GET U 'GEN) (MGENP U))))
          (T (OR (GENEXP (CAR U)) (GENEXP (CDR U)))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RECORDS)) 
(PUT 'RECORDSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'RECORDSTAT 'DEFINED-ON-LINE '43) 
(PUT 'RECORDSTAT 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'RECORDSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RECORDSTAT NIL
    (PROG (F STAT)
      (SETQ F (FLAGP 'HAS 'DELIM))
      (FLAG '(HAS) 'DELIM)
      (SETQ STAT (ERRORSET '(RECORDSTAT1) NIL NIL))
      (COND ((NOT F) (REMFLAG '(HAS) 'DELIM)))
      (COND
       ((ERRORP STAT)
        (PROG ()
         WHILELABEL
          (COND ((NOT (NEQ CURSYM* '*SEMICOL*)) (RETURN NIL)))
          (SCAN)
          (GO WHILELABEL)))
       (T (RETURN (CAR STAT)))))) 
(PUT 'RECORDSTAT1 'NUMBER-OF-ARGS 0) 
(PUT 'RECORDSTAT1 'DEFINED-ON-LINE '57) 
(PUT 'RECORDSTAT1 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'RECORDSTAT1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RECORDSTAT1 NIL
    (PROG (STRUCTNAME ANNOTATION FIELDS OPTIONS)
      (SETQ STRUCTNAME (SCAN))
      (COND ((NOT (IDP STRUCTNAME)) (SYMERR 'RECORD T)))
      (COND
       ((EQCAR (SCAN) '*COMMENT*)
        (PROGN (SETQ ANNOTATION (CADR CURSYM*)) (SCAN))))
      (COND ((EQ CURSYM* 'WITH) (SETQ FIELDS (REMCOMMA (XREAD NIL)))))
      (COND ((EQ CURSYM* 'HAS) (SETQ OPTIONS (REMCOMMA (XREAD NIL)))))
      (COND
       ((EQ CURSYM* '*SEMICOL*)
        (RETURN (LIST 'RECORD STRUCTNAME ANNOTATION FIELDS OPTIONS)))
       (T (SYMERR 'RECORD T))))) 
(PUT 'RECORD 'STAT 'RECORDSTAT) 
(PUT 'FORMRECORD 'NUMBER-OF-ARGS 3) 
(PUT 'FORMRECORD 'DEFINED-ON-LINE '72) 
(PUT 'FORMRECORD 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'FORMRECORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMRECORD (U VARS MODE)
    ((LAMBDA (FORM_FUNCTION) (APPLY FORM_FUNCTION (CDR U)))
     (FUNCTION
      (LAMBDA (RECORD_NAME ANNOTATION FIELDS OPTIONS)
        (PROG (STRUCTSPEC FIELDSPECS CONSTRUCTOR FORM)
          (SETQ STRUCTSPEC (FORM_STRUCTURE_SPECIFICATION RECORD_NAME OPTIONS))
          (SETQ FIELDSPECS (FORM_FIELD_SPECIFICATIONS FIELDS))
          (SETQ CONSTRUCTOR
                  (CDR
                   (ATSOC 'CONSTRUCTOR (GET_DEFSTRUCT_OPTIONS STRUCTSPEC))))
          (SETQ FORM (LIST NIL))
          (TCONC FORM 'PROGN)
          (COND
           (CONSTRUCTOR
            (PROGN
             (TCONC FORM
              (LIST 'PUT (MKQUOTE CONSTRUCTOR) ''FORMFN
                    ''FORM_RECORD_CONSTRUCTOR))
             (PUT CONSTRUCTOR 'FORMFN 'FORM_RECORD_CONSTRUCTOR))))
          (COND
           (ANNOTATION
            (TCONC FORM
             (LIST 'PUT (MKQUOTE RECORD_NAME) ''ANNOTATION ANNOTATION))))
          (TCONC FORM (CONS 'DEFSTRUCT (CONS STRUCTSPEC FIELDSPECS)))
          (RETURN (CAR FORM))))))) 
(PUT 'RECORD 'FORMFN 'FORMRECORD) 
(PUT 'TCONC 'NUMBER-OF-ARGS 2) 
(PUT 'TCONC 'DEFINED-ON-LINE '99) 
(PUT 'TCONC 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'TCONC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TCONC (PTR ELEM)
    (PROGN
     (SETQ ELEM (LIST ELEM))
     (COND ((NOT (PAIRP PTR)) (CONS ELEM ELEM))
           ((NULL (CDR PTR)) (RPLACA (RPLACD PTR ELEM) ELEM))
           (T (PROGN (RPLACD (CDR PTR) ELEM) (RPLACD PTR ELEM)))))) 
(PUT 'FORM_STRUCTURE_SPECIFICATION 'NUMBER-OF-ARGS 2) 
(PUT 'FORM_STRUCTURE_SPECIFICATION 'DEFINED-ON-LINE '108) 
(PUT 'FORM_STRUCTURE_SPECIFICATION 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'FORM_STRUCTURE_SPECIFICATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORM_STRUCTURE_SPECIFICATION (RECORD_NAME OPTIONS)
    ((LAMBDA (DEFAULTS)
       (APPEND DEFAULTS
               (PROG (ENTRY FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ENTRY OPTIONS)
                 (COND ((NULL ENTRY) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ENTRY)
                                     (COND ((ATOM ENTRY) ENTRY)
                                           ((AND (EQCAR ENTRY 'NO)
                                                 (EQUAL (LENGTH ENTRY) 2))
                                            (LIST (CADR ENTRY) NIL))
                                           ((AND (EQ (CAR ENTRY) 'EQUAL)
                                                 (EQUAL (LENGTH ENTRY) 3))
                                            (LIST (CADR ENTRY) (CADDR ENTRY)))
                                           (T
                                            (ERROR 0
                                                   (LIST "Bad RECORD option:"
                                                         ENTRY)))))
                                   (CAR ENTRY))
                                  NIL)))
                LOOPLABEL
                 (SETQ ENTRY (CDR ENTRY))
                 (COND ((NULL ENTRY) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ENTRY)
                             (COND ((ATOM ENTRY) ENTRY)
                                   ((AND (EQCAR ENTRY 'NO)
                                         (EQUAL (LENGTH ENTRY) 2))
                                    (LIST (CADR ENTRY) NIL))
                                   ((AND (EQ (CAR ENTRY) 'EQUAL)
                                         (EQUAL (LENGTH ENTRY) 3))
                                    (LIST (CADR ENTRY) (CADDR ENTRY)))
                                   (T
                                    (ERROR 0
                                           (LIST "Bad RECORD option:"
                                                 ENTRY)))))
                           (CAR ENTRY))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
     (LIST RECORD_NAME (LIST 'CONSTRUCTOR RECORD_NAME) 'PREDICATE))) 
(PUT 'FORM_FIELD_SPECIFICATIONS 'NUMBER-OF-ARGS 1) 
(PUT 'FORM_FIELD_SPECIFICATIONS 'DEFINED-ON-LINE '120) 
(PUT 'FORM_FIELD_SPECIFICATIONS 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'FORM_FIELD_SPECIFICATIONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORM_FIELD_SPECIFICATIONS (FIELD_LIST)
    (PROG (ENTRY FORALL-RESULT FORALL-ENDPTR)
      (SETQ ENTRY FIELD_LIST)
     STARTOVER
      (COND ((NULL ENTRY) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (ENTRY)
                 (COND
                  ((EQCAR ENTRY 'SETQ)
                   (LIST
                    (LIST (CADR ENTRY) (FORM1 (CADDR ENTRY) NIL 'SYMBOLIC))))
                  (T NIL)))
               (CAR ENTRY)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ ENTRY (CDR ENTRY))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL ENTRY) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (ENTRY)
                 (COND
                  ((EQCAR ENTRY 'SETQ)
                   (LIST
                    (LIST (CADR ENTRY) (FORM1 (CADDR ENTRY) NIL 'SYMBOLIC))))
                  (T NIL)))
               (CAR ENTRY)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ ENTRY (CDR ENTRY))
      (GO LOOPLABEL))) 
(PUT 'FORM_RECORD_CONSTRUCTOR 'NUMBER-OF-ARGS 3) 
(PUT 'FORM_RECORD_CONSTRUCTOR 'DEFINED-ON-LINE '127) 
(PUT 'FORM_RECORD_CONSTRUCTOR 'DEFINED-IN-FILE 'RLISP88/RECORDS.RED) 
(PUT 'FORM_RECORD_CONSTRUCTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORM_RECORD_CONSTRUCTOR (U VARS MODE)
    (PROG (CONSTRUCTOR ARGLIST)
      (SETQ CONSTRUCTOR (CAR U))
      (SETQ ARGLIST (LIST NIL))
      (PROG (ARG)
        (SETQ ARG (CDR U))
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (COND
            ((EQCAR ARG 'SETQ)
             (PROGN
              (TCONC ARGLIST (CADR ARG))
              (TCONC ARGLIST (FORM1 (CADDR ARG) VARS MODE))))
            (T
             (REDERR
              (LIST ARG "is not a proper initialization form for"
                    CONSTRUCTOR)))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (RETURN (CONS CONSTRUCTOR (CAR ARGLIST))))) 
(ENDMODULE) 
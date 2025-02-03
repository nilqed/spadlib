(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CHECKIND)) 
(GLOBAL '(SPACES*)) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON*)) 
(PUT 'SPLIT_FREE_DUM_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT_FREE_DUM_IDS 'DEFINED-ON-LINE '37) 
(PUT 'SPLIT_FREE_DUM_IDS 'DEFINED-IN-FILE 'ASSIST/CHECKIND.RED) 
(PUT 'SPLIT_FREE_DUM_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT_FREE_DUM_IDS (LTENS)
    (PROG (IND DUMLIST FREELIST)
      (SETQ IND (SPLIT_COV_CONT_IDS (ALL_INDEX_LST LTENS)))
      (SETQ IND (LIST (CLEAN_NUMID (CAR IND)) (CLEAN_NUMID (CADR IND))))
      (SETQ DUMLIST (INTERSECTION (CAR IND) (CADR IND)))
      ((LAMBDA (DUMMY_ID*) (VERIFY_TENS_IDS IND)) DUMLIST)
      (SETQ FREELIST
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y (SETDIFF (CAR IND) DUMLIST))
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (LIST 'MINUS Y)) (CAR Y))
                                      NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (LIST 'MINUS Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FREELIST (APPEND FREELIST (SETDIFF (CADR IND) DUMLIST)))
      (RETURN (LIST (ORDN FREELIST) DUMLIST)))) 
(PUT 'CHECK_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_IDS 'DEFINED-ON-LINE '55) 
(PUT 'CHECK_IDS 'DEFINED-IN-FILE 'ASSIST/CHECKIND.RED) 
(PUT 'CHECK_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_IDS (SF)
    (PROG (DUMLIST FREELIST Y)
      (SETQ FREELIST 'UNDEFINED)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM SF) (ATOM (CAR SF))))) (RETURN NIL)))
        (PROGN
         (SETQ Y (SEP_TENS_FROM_OTHER (CONS (CAR SF) NIL)))
         (COND
          ((GEQ (LENGTH (CAR Y)) 1)
           (PROGN
            (SETQ Y (SPLIT_FREE_DUM_IDS (CAR Y)))
            (COND ((EQUAL FREELIST 'UNDEFINED) (SETQ FREELIST (CAR Y)))
                  ((NEQ FREELIST (CAR Y))
                   (RERROR 'CANTENS 11
                           (LIST "mismatch in free indices : "
                                 (LIST (CAR Y) FREELIST)))))
            (SETQ DUMLIST (UNION DUMLIST (CADR Y)))))
          (FREELIST
           (COND ((EQUAL FREELIST 'UNDEFINED) (SETQ FREELIST NIL))
                 (T (RERROR 'CANTENS 11 "scalar added with tensor(s)")))))
         (SETQ SF (CDR SF)))
        (GO WHILELABEL))
      (COND
       ((NEQ FREELIST 'UNDEFINED)
        (COND
         ((AND (SETQ Y (REPEATS FREELIST)) (EXTRACT_DUMMY_IDS Y))
          (RERROR 'CANTENS 12 (LIST "wrong use of indices" Y))))))
      (RETURN
       (COND
        ((OR (EQUAL FREELIST 'UNDEFINED) (NULL FREELIST)) (LIST NIL DUMLIST))
        (SF (RERROR 'CANTENS 12 "scalar added with tensor(s)"))
        (T (LIST FREELIST DUMLIST)))))) 
(ENDMODULE) 
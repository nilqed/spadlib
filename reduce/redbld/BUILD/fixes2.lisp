(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REDUCE_PATCHES)) 
(PUT 'SUBEVAL0 'NUMBER-OF-ARGS 1) 
(PUT 'SUBEVAL0 'DEFINED-ON-LINE '338) 
(PUT 'SUBEVAL0 'DEFINED-IN-FILE 'CRACK/FIXES2.RED) 
(PUT 'SUBEVAL0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBEVAL0 (U)
    (PROG (X Y Z NS CADDRX)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR U)) (RETURN NIL)))
        (PROGN
         (COND ((NOT (EQCAR (CAR U) 'EQUAL)) (SETQ X (CONS (CAR U) X)))
               ((NOT (EQUAL (CADAR U) (SETQ Y (REVAL1 (CADDAR U) NIL))))
                (SETQ X (CONS (LIST (CAAR U) (CADAR U) Y) X))))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (COND ((NULL X) (RETURN (CAR U))) (T (SETQ U (REVERSIP2 X U))))
      (COND
       ((MEMBER U SUBLIST*)
        (RETURN
         (MK*SQ (CONS (LIST (CONS (GETPOWER (FKERN (CONS 'SUB U)) 1) 1)) 1))))
       (T (SETQ SUBLIST* (CONS U SUBLIST*))))
      (COND
       ((NULL (AND U (CDR U))) (REDERR "SUB requires at least 2 arguments")))
      ((LAMBDA (*EVALLHSEQP)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR U)) (RETURN NIL)))
           (PROGN
            (SETQ X (REVAL1 (CAR U) T))
            (COND ((EQ (GETRTYPE X) 'LIST) (SETQ U (APPEND (CDR X) (CDR U))))
                  (T
                   (PROGN
                    (COND ((NOT (EQEXPR X)) (ERRPRI2 (CAR U) T)))
                    (SETQ Y (CADR X))
                    (COND ((NULL (GETRTYPE Y)) (SETQ Y (*A2KWOWEIGHT Y))))
                    (SETQ CADDRX (REVAL1 (CADDR X) T))
                    (COND
                     ((GETRTYPE CADDRX) (SETQ NS (CONS (CONS Y CADDRX) NS)))
                     (T (SETQ Z (CONS (CONS Y CADDRX) Z))))
                    (SETQ U (CDR U))))))
           (GO WHILELABEL)))
       NIL)
      (SETQ X (REVAL1 (CAR U) NIL))
      (RETURN (SUBEVAL1 (APPEND NS Z) X)))) 
(LOAD_PACKAGE (LIST 'TRIGSIMP)) 
(ENDMODULE) 
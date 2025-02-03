(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DF2Q)) 
(FLUID '(INDEXLIST ZLIST)) 
(EXPORTS (LIST 'DF2Q)) 
(IMPORTS (LIST 'ADDF 'GCDF 'MKSP '*MULTF 'QUOTF)) 
(PUT 'DF2Q 'NUMBER-OF-ARGS 1) 
(PUT 'DF2Q 'DEFINED-ON-LINE '39) 
(PUT 'DF2Q 'DEFINED-IN-FILE 'INT/DF2Q.RED) 
(PUT 'DF2Q 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DF2Q (P)
    (PROG (N D W X Y Z)
      (COND ((NULL P) (RETURN (CONS NIL 1))))
      (SETQ D (CDR (CDAR P)))
      (SETQ W (CDR P))
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (PROGN
         (SETQ D
                 ((LAMBDA (G544)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF D G544))
                          (T (POLY-MULTF D G544))))
                  (QUOTF-FAIL (CDR (CDAR W)) (GCDF D (CDR (CDAR W))))))
         (SETQ W (CDR W)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT P) (RETURN NIL)))
        (PROG ()
          (SETQ W (SQRT2TOP (CDAR P)))
          (SETQ X
                  ((LAMBDA (G545 G546)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G545 G546))
                           (T (POLY-MULTF G545 G546))))
                   (XL2F (CAAR P) ZLIST INDEXLIST)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR W) D))
                         (T (POLY-MULTF (CAR W) D)))))
          (COND ((NULL X) (RETURN (SETQ P (CDR P)))))
          (SETQ Y (CDR W))
          (SETQ Z ((LAMBDA (*EXP) (QUOTF1 X Y)) T))
          (COND
           ((NULL Z)
            (PROGN
             (SETQ Z (RATIONALIZESQ (CONS X Y)))
             (COND
              ((NEQ (CDR Z) 1)
               (PROGN
                (SETQ D
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR Z) D))
                              (T (POLY-MULTF (CDR Z) D))))
                (SETQ N
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR Z) N))
                              (T (POLY-MULTF (CDR Z) N)))))))
             (SETQ Z (CAR Z)))))
          (SETQ N (ADDF N Z))
          (SETQ P (CDR P)))
        (GO WHILELABEL))
      (RETURN (TIDY-POWERSQ (CONS N D))))) 
(PUT 'TIDY-POWERSQ 'NUMBER-OF-ARGS 1) 
(PUT 'TIDY-POWERSQ 'DEFINED-ON-LINE '66) 
(PUT 'TIDY-POWERSQ 'DEFINED-IN-FILE 'INT/DF2Q.RED) 
(PUT 'TIDY-POWERSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TIDY-POWERSQ (X)
    (PROG (EXPTS *PRECISE *KEEPSQRTS)
      (SETQ *KEEPSQRTS T)
      (SETQ X (SUBS2Q X))
      (SETQ EXPTS (FIND-EXPTS (CAR X) (FIND-EXPTS (CDR X) NIL)))
      (COND ((NULL EXPTS) (RETURN X)))
      (SETQ X
              (SUBSQ X
                     (PROG (V FORALL-RESULT FORALL-ENDPTR)
                       (SETQ V EXPTS)
                       (COND ((NULL V) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (V)
                                           (CONS (CAR V)
                                                 (LIST 'EXPT (CADR V)
                                                       (CDDR V))))
                                         (CAR V))
                                        NIL)))
                      LOOPLABEL
                       (SETQ V (CDR V))
                       (COND ((NULL V) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (V)
                                   (CONS (CAR V)
                                         (LIST 'EXPT (CADR V) (CDDR V))))
                                 (CAR V))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
      (SETQ X
              (SUBSQ X
                     (PROG (V FORALL-RESULT FORALL-ENDPTR)
                       (SETQ V EXPTS)
                       (COND ((NULL V) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (V)
                                           (CONS (CADR V)
                                                 (LIST 'EXPT (CAR V)
                                                       (LIST 'QUOTIENT 1
                                                             (CDDR V)))))
                                         (CAR V))
                                        NIL)))
                      LOOPLABEL
                       (SETQ V (CDR V))
                       (COND ((NULL V) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (V)
                                   (CONS (CADR V)
                                         (LIST 'EXPT (CAR V)
                                               (LIST 'QUOTIENT 1 (CDDR V)))))
                                 (CAR V))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
      (RETURN X))) 
(PUT 'FIND-EXPTS 'NUMBER-OF-ARGS 2) 
(PUT 'FIND-EXPTS 'DEFINED-ON-LINE '83) 
(PUT 'FIND-EXPTS 'DEFINED-IN-FILE 'INT/DF2Q.RED) 
(PUT 'FIND-EXPTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND-EXPTS (FF L)
    (PROG (W)
      (COND ((OR (ATOM FF) (ATOM (CAR FF))) (RETURN L)))
      (SETQ L (FIND-EXPTS (CDAR FF) (FIND-EXPTS (CDR FF) L)))
      (SETQ FF (CAAAR FF))
      (COND ((EQCAR FF 'SQRT) (SETQ FF (LIST 'EXPT (CADR FF) '(QUOTIENT 1 2))))
            ((AND (EQCAR FF 'EXPT) (EQCAR (CADDR FF) 'QUOTIENT)
                  (NUMBERP (CADDR (CADDR FF))))
             (PROGN
              (SETQ W (ASSOC (CADR FF) L))
              (COND
               ((NULL W)
                (PROGN
                 (SETQ W (CONS (CADR FF) (CONS (GENSYM) 1)))
                 (SETQ L (CONS W L)))))
              (RPLACD (CDR W) (LCM (CDDR W) (CADDR (CADDR FF)))))))
      (RETURN L))) 
(PUT 'XL2F 'NUMBER-OF-ARGS 3) 
(PUT 'XL2F 'DEFINED-ON-LINE '99) 
(PUT 'XL2F 'DEFINED-IN-FILE 'INT/DF2Q.RED) 
(PUT 'XL2F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XL2F (L Z IL)
    (COND ((NULL Z) 1) ((EQUAL (CAR L) 0) (XL2F (CDR L) (CDR Z) (CDR IL)))
          ((NOT (ATOM (CAR L)))
           (PROG (TEMP)
             (COND ((EQUAL (CAAR L) 0) (SETQ TEMP (CAR IL)))
                   (T (SETQ TEMP (LIST 'PLUS (CAR IL) (CAAR L)))))
             (SETQ TEMP (GETPOWER (FKERN (LIST 'EXPT (CAR Z) TEMP)) 1))
             (RETURN
              (*MULTF (CONS (CONS TEMP 1) NIL)
               (XL2F (CDR L) (CDR Z) (CDR IL))))))
          (T
           (*MULTF (CONS (CONS (GETPOWER (FKERN (CAR Z)) (CAR L)) 1) NIL)
            (XL2F (CDR L) (CDR Z) (CDR IL)))))) 
(ENDMODULE) 
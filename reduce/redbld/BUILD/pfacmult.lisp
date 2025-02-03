(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PFACMULT)) 
(FLUID '(*TRFAC)) 
(PUT 'FCTRFKRONM 'NUMBER-OF-ARGS 1) 
(PUT 'FCTRFKRONM 'DEFINED-ON-LINE '40) 
(PUT 'FCTRFKRONM 'DEFINED-IN-FILE 'FACTOR/PFACMULT.RED) 
(PUT 'FCTRFKRONM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTRFKRONM (F)
    (PROG (SUB TRA K X XX X0 Y Z R Q F0 FL FS DMODE* D D0)
      (SETQ D 0)
      (SETQ D0 0)
      (SETQ K (KERNELS F))
      (SETQ DMODE* '|:MOD:|)
      (PROG (Z)
        (SETQ Z
                (DECOMPOSEDEGR F
                               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ X K)
                                 (COND ((NULL X) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (X) (CONS X 0))
                                                   (CAR X))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ X (CDR X))
                                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X) (CONS X 0)) (CAR X))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z) (COND ((GREATERP (CDR Z) D) (SETQ D (CDR Z))))) (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (SETQ D (PLUS D 1))
      (SETQ D0 D)
      (SETQ X0 (CAR K))
      (PROG (X)
        (SETQ X (CDR K))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ SUB (CONS (CONS X (LIST 'EXPT X0 D0)) SUB))
            (SETQ TRA (CONS (CONS X D0) TRA))
            (SETQ D0 (TIMES D0 D))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ FS (CAR (SUBF F SUB)))
      (COND
       (*TRFAC
        (PROGN
         (WRITEPRI "Kronecker mapped form:" 'FIRST)
         (WRITEPRI (MKQUOTE (PREPF FS)) 'LAST))))
      (SETQ FL (DECOMPOSEFCTRF FS))
      (COND ((NULL (CDR FL)) (RETURN (LIST 1 (CONS F 1)))))
      (SETQ F0 (CAR (RESIMP (CONS F 1))))
      (PROG (FC)
        (SETQ FC FL)
       LAB
        (COND ((NULL FC) (RETURN NIL)))
        ((LAMBDA (FC)
           (COND
            ((NOT (OR (ATOM F0) (ATOM (CAR F0))))
             (PROGN
              (SETQ Y (FCTRFMK1 FC TRA))
              (SETQ Y (CAR (RESIMP (CONS Y 1))))
              (SETQ X (FCTRFMK3 Y))
              (COND (X (SETQ Y ((LAMBDA (*EXP) (QUOTF1 Y X)) T))))
              (COND
               (*TRFAC
                (PROGN
                 (WRITEPRI "test next candidate " 'FIRST)
                 (WRITEPRI (MKQUOTE (PREPF Y)) 'LAST))))
              (COND
               ((SETQ Q ((LAMBDA (*EXP) (QUOTF1 F0 Y)) T))
                (PROGN
                 (SETQ F0 Q)
                 (COND ((SETQ Z (ASSOC Y R)) (SETCDR Z (PLUS (CDR Z) 1)))
                       (T (SETQ R (CONS (CONS Y 1) R)))))))))))
         (CAR FC))
        (SETQ FC (CDR FC))
        (GO LAB))
      (COND ((NULL R) (RETURN (LIST 1 (CONS F 1)))))
      (COND ((OR (ATOM F0) (ATOM (CAR F0))) (RETURN (CONS F0 R))))
      (COND
       (*TRFAC
        (PROGN
         (WRITEPRI "descend in recursion with" 'ONLY)
         (WRITEPRI (MKQUOTE (PREPF F0)) 'ONLY))))
      (SETQ FL (FCTRFKRONM F0))
      (COND
       (*TRFAC
        (PROGN
         (WRITEPRI "return from recursion; numeric factor " 'FIRST)
         (WRITEPRI (MKQUOTE (PREPF (CAR FL))) 'LAST)
         (PROG (FC)
           (SETQ FC (CDR FL))
          LAB
           (COND ((NULL FC) (RETURN NIL)))
           ((LAMBDA (FC)
              (PROGN
               (WRITEPRI "polynomial factor: " 'FIRST)
               (WRITEPRI (MKQUOTE (PREPF (CAR FC))) NIL)
               (WRITEPRI " multiplicity " NIL)
               (WRITEPRI (MKQUOTE (PREPF (CDR FC))) 'LAST)))
            (CAR FC))
           (SETQ FC (CDR FC))
           (GO LAB)))))
      (SETQ X (CAR FL))
      (SETQ XX (CDR FL))
      (COND
       ((AND (NULL (CDR XX)) (EQUAL (CDAR XX) 1) (FCTRFMK4 X))
        (PROGN
         (SETQ Y (FCTRFMK3 (CAR XX)))
         (COND
          (Y
           (PROGN
            (SETQ X Y)
            (SETQ XX (LIST (CONS ((LAMBDA (*EXP) (QUOTF1 (CAAR XX) X)) T) 1)))
            (COND
             (*TRFAC
              (PROGN
               (WRITEPRI "number correction; numeric factor " 'FIRST)
               (WRITEPRI (MKQUOTE X) 'LAST)
               (WRITEPRI "polynomial factor " 'FIRST)
               (WRITEPRI (MKQUOTE (PREPF (CAAR XX))) 'LAST))))))))))
      (PROG (FC)
        (SETQ FC XX)
       LAB
        (COND ((NULL FC) (RETURN NIL)))
        ((LAMBDA (FC)
           (PROGN
            (SETQ Y (CAR (RESIMP (CONS (CAR FC) 1))))
            (COND
             (*TRFAC
              (PROGN
               (WRITEPRI "next division: " 'FIRST)
               (WRITEPRI (MKQUOTE (PREPF Y)) 'LAST))))
            (SETQ F0 ((LAMBDA (*EXP) (QUOTF1 F0 Y)) T))
            (COND ((SETQ Z (ASSOC Y R)) (SETCDR Z (PLUS (CDR Z) (CDR FC))))
                  (T (SETQ R (CONS (CONS Y (CDR FC)) R))))))
         (CAR FC))
        (SETQ FC (CDR FC))
        (GO LAB))
      (SETQ X ((LAMBDA (*EXP) (QUOTF1 X F0)) T))
      (RETURN (CONS X R)))) 
(PUT 'FCTRFMK1 'NUMBER-OF-ARGS 2) 
(PUT 'FCTRFMK1 'DEFINED-ON-LINE '105) 
(PUT 'FCTRFMK1 'DEFINED-IN-FILE 'FACTOR/PFACMULT.RED) 
(PUT 'FCTRFMK1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FCTRFMK1 (F TRA)
    (COND ((OR (ATOM F) (ATOM (CAR F))) F)
          (T
           (ADDF
            ((LAMBDA (G630)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR F) G630))
                     (T (POLY-MULTF (CDAR F) G630))))
             (FCTRFMK2 (CAAAR F) (CDAAR F) TRA))
            (FCTRFMK1 (CDR F) TRA))))) 
(PUT 'FCTRFMK2 'NUMBER-OF-ARGS 3) 
(PUT 'FCTRFMK2 'DEFINED-ON-LINE '110) 
(PUT 'FCTRFMK2 'DEFINED-IN-FILE 'FACTOR/PFACMULT.RED) 
(PUT 'FCTRFMK2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FCTRFMK2 (X N TRA)
    (COND ((EQUAL N 0) 1) ((NULL TRA) (CONS (CONS (CONS X N) 1) NIL))
          ((GEQ N (CDAR TRA))
           ((LAMBDA (G631 G632)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF G631 G632))
                    (T (POLY-MULTF G631 G632))))
            (CONS (CONS (CONS (CAAR TRA) (QUOTIENT N (CDAR TRA))) 1) NIL)
            (FCTRFMK2 X (REMAINDER N (CDAR TRA)) (CDR TRA))))
          (T (FCTRFMK2 X N (CDR TRA))))) 
(PUT 'FCTRFMK3 'NUMBER-OF-ARGS 1) 
(PUT 'FCTRFMK3 'DEFINED-ON-LINE '117) 
(PUT 'FCTRFMK3 'DEFINED-IN-FILE 'FACTOR/PFACMULT.RED) 
(PUT 'FCTRFMK3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTRFMK3 (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) (COND ((FCTRFMK4 F) NIL) (T F)))
          (T (FCTRFMK3 (CDAR F))))) 
(PUT 'FCTRFMK4 'NUMBER-OF-ARGS 1) 
(PUT 'FCTRFMK4 'DEFINED-ON-LINE '121) 
(PUT 'FCTRFMK4 'DEFINED-IN-FILE 'FACTOR/PFACMULT.RED) 
(PUT 'FCTRFMK4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTRFMK4 (U)
    (OR (AND (NUMBERP U) (EQUAL U 1))
        (AND (NOT (ATOM U)) (EQUAL (CAR U) '|:MOD:|) (|MODONEP:| U)))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RAIV)) 
(PUT 'RA_ISOLATE0 'NUMBER-OF-ARGS 3) 
(DE RA_ISOLATE0 (F LB UB)
    (PROG (IVL L U FF)
      (PROG (G139)
        (SETQ G139 (RA_ISOLATINGIVL0 F LB UB))
        (SETQ F (CAR G139))
        (SETQ IVL (CDR G139))
        (RETURN G139))
      (RETURN
       (PROG (IV FORALL-RESULT FORALL-ENDPTR)
         (SETQ IV IVL)
         (COND ((NULL IV) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (IV)
                             (PROGN
                              (SETQ L (CAR IV))
                              (SETQ U (CDR IV))
                              (COND
                               ((EQUAL L U)
                                (PROGN
                                 (SETQ FF
                                         (COND
                                          ((NULL (CAR L))
                                           (LIST (CONS (CONS 'X 1) 1)))
                                          (T
                                           (CONS (CONS (CONS 'X 1) (CDR L))
                                                 (NEGF (CAR L))))))
                                 (SETQ L (ADDSQ (CAR IV) (NEGSQ (CONS 1 1))))
                                 (SETQ U (ADDSQ (CAR IV) (CONS 1 1)))
                                 (RA_QMK FF L U)))
                               (T
                                (RA_SIMPL0 (RA_NORMALIZE0 (RA_QMK F L U)))))))
                           (CAR IV))
                          NIL)))
        LOOPLABEL
         (SETQ IV (CDR IV))
         (COND ((NULL IV) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (IV)
                     (PROGN
                      (SETQ L (CAR IV))
                      (SETQ U (CDR IV))
                      (COND
                       ((EQUAL L U)
                        (PROGN
                         (SETQ FF
                                 (COND
                                  ((NULL (CAR L)) (LIST (CONS (CONS 'X 1) 1)))
                                  (T
                                   (CONS (CONS (CONS 'X 1) (CDR L))
                                         (NEGF (CAR L))))))
                         (SETQ L (ADDSQ (CAR IV) (NEGSQ (CONS 1 1))))
                         (SETQ U (ADDSQ (CAR IV) (CONS 1 1)))
                         (RA_QMK FF L U)))
                       (T (RA_SIMPL0 (RA_NORMALIZE0 (RA_QMK F L U)))))))
                   (CAR IV))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(DE RA_ISOLATE (A1 A2 A3) (RA_WRAPPER 'RA_ISOLATE0 (LIST A1 A2 A3))) 
(PUT 'RA_ISOLATE 'NUMBER-OF-ARGS 3) 
(PUT 'RA_ISOLATE$ 'NUMBER-OF-ARGS 1) 
(PUT 'RA_ISOLATE$ 'DEFINED-ON-LINE '52) 
(PUT 'RA_ISOLATE$ 'DEFINED-IN-FILE 'RANUM/RARCOUNT.RED) 
(PUT 'RA_ISOLATE$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RA_ISOLATE$ (ARGL)
    (PROG (LB UB)
      (COND
       ((CDR ARGL)
        (PROGN
         (SETQ LB (SIMP (CADR ARGL)))
         (COND
          ((NOT (RA_RATNUMP LB)) (REDERR "bounds must be rational numbers")))
         (COND
          ((CDDR ARGL)
           (PROGN
            (SETQ UB (SIMP (CADDR ARGL)))
            (COND
             ((NOT (RA_RATNUMP UB))
              (REDERR "bounds must be rational numbers")))))))))
      (RETURN
       (CONS 'LIST
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (RA_ISOLATE (CAR (SIMP (CAR ARGL))) LB UB))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (MK*SQ
                                    (CONS (SFTO_INT2SF (INT-EQUIV-CHK X)) 1)))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (MK*SQ (CONS (SFTO_INT2SF (INT-EQUIV-CHK X)) 1)))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'ISOLATE 'PSOPFN 'RA_ISOLATE$) 
(PUT 'RA_RATNUMP 'NUMBER-OF-ARGS 1) 
(DE RA_RATNUMP (Q)
    (AND (OR (NULL (CAR Q)) (NUMBERP (CAR Q))) (NUMBERP (CDR Q)))) 
(PUT 'RA_ISOLATINGIVL0 'NUMBER-OF-ARGS 3) 
(DE RA_ISOLATINGIVL0 (F LB UB)
    (PROG (IVL Z)
      (SETQ F (SFTO_DPRPARTF (SFTO_SQFPARTF F)))
      (COND
       ((NULL (SFTO_ABSSUMMAND F))
        (PROGN
         (SETQ F ((LAMBDA (*EXP) (QUOTF1 F (LIST (CONS (CONS 'X 1) 1)))) T))
         (SETQ Z T))))
      (COND ((NULL UB) (SETQ UB (ADDSQ (SFTO_LMQ F) (CONS 1 1)))))
      (COND
       ((NULL LB)
        (SETQ LB (NEGSQ (ADDSQ (SFTO_LMQ (RA_MIRROR F)) (CONS 1 1))))))
      (SETQ IVL (RA_VCA F LB UB))
      (COND (Z (SETQ IVL (RA_INSERTZERO IVL))))
      (RETURN (CONS F IVL)))) 
(DE RA_ISOLATINGIVL (A1 A2 A3) (RA_WRAPPER 'RA_ISOLATINGIVL0 (LIST A1 A2 A3))) 
(PUT 'RA_ISOLATINGIVL 'NUMBER-OF-ARGS 3) 
(PUT 'RA_INSERTZERO 'NUMBER-OF-ARGS 1) 
(DE RA_INSERTZERO (IVL)
    (COND ((NULL IVL) '(((NIL . 1) NIL . 1)))
          ((MINUSF (CAR (CAR (CAR IVL))))
           (CONS (CAR IVL) (RA_INSERTZERO (CDR IVL))))
          (T (CONS '((NIL . 1) NIL . 1) IVL)))) 
(PUT 'RA_ISOLATINGIVL$ 'NUMBER-OF-ARGS 1) 
(DE RA_ISOLATINGIVL$ (ARGL)
    (CONS 'LIST
          (PROG (IV FORALL-RESULT FORALL-ENDPTR)
            (SETQ IV (CDR (RA_ISOLATINGIVL (CAR (SIMP (CAR ARGL))) NIL NIL)))
            (COND ((NULL IV) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (IV)
                                (LIST 'LIST (MK*SQ (CAR IV)) (MK*SQ (CDR IV))))
                              (CAR IV))
                             NIL)))
           LOOPLABEL
            (SETQ IV (CDR IV))
            (COND ((NULL IV) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (IV)
                        (LIST 'LIST (MK*SQ (CAR IV)) (MK*SQ (CDR IV))))
                      (CAR IV))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'ISOLATINGIVL 'PSOPFN 'RA_ISOLATINGIVL$) 
(PUT 'RA_MIRROR 'NUMBER-OF-ARGS 1) 
(DE RA_MIRROR (F)
    (PROGN
     (SETQ F
             (SFTO_FSUB1 F
                         (LIST (CONS 'X (NEGF (LIST (CONS (CONS 'X 1) 1)))))))
     (COND ((MINUSF F) (NEGF F)) (T F)))) 
(PUT 'RA_HELP 'NUMBER-OF-ARGS 1) 
(DE RA_HELP (IVL)
    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
      (SETQ PR IVL)
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PR)
                          (LIST
                           (QUOTIENT (FLOAT (OR (CAAR PR) 0))
                                     (FLOAT (CDAR PR)))
                           (QUOTIENT (FLOAT (OR (CADR PR) 0))
                                     (FLOAT (CDDR PR)))))
                        (CAR PR))
                       NIL)))
     LOOPLABEL
      (SETQ PR (CDR PR))
      (COND ((NULL PR) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (PR)
                  (LIST (QUOTIENT (FLOAT (OR (CAAR PR) 0)) (FLOAT (CDAR PR)))
                        (QUOTIENT (FLOAT (OR (CADR PR) 0)) (FLOAT (CDDR PR)))))
                (CAR PR))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'RA_VCA 'NUMBER-OF-ARGS 3) 
(DE RA_VCA (F L U) (RA_VCA1 (RA_TRANSFORM F L U) L U)) 
(PUT 'RA_TRANSFORM 'NUMBER-OF-ARGS 3) 
(DE RA_TRANSFORM (F L U)
    (CAR
     (SFTO_QSUB1 F
                 (LIST
                  (CONS 'X
                        (ADDSQ
                         (MULTSQ (ADDSQ U (NEGSQ L))
                                 (CONS (LIST (CONS (CONS 'X 1) 1)) 1))
                         L)))))) 
(PUT 'RA_VCA1 'NUMBER-OF-ARGS 3) 
(DE RA_VCA1 (F A B)
    (PROG (FF FFF C RESL SV)
      (SETQ SV 0)
      (SETQ SV (RA_BUDAN-0-1 F))
      (COND ((EQN SV 0) (RETURN NIL)))
      (COND ((EQN SV 1) (RETURN (LIST (CONS A B)))))
      (SETQ C (MULTSQ (ADDSQ A B) (INVSQ (CONS 2 1))))
      (PROG (G140)
        (SETQ G140 (RA_VCATRANSFORM1 F))
        (SETQ FF (CAR G140))
        (SETQ FFF (CDR G140))
        (RETURN G140))
      (SETQ RESL (RA_VCA1 FF A C))
      (COND
       ((NULL (CAR (SFTO_QSUB1 F (LIST (CONS 'X (CONS 1 2))))))
        (SETQ RESL (APPEND RESL (LIST (CONS C C))))))
      (SETQ RESL (APPEND RESL (RA_VCA1 FFF C B)))
      (RETURN RESL))) 
(PUT 'RA_VCATRANSFORM1 'NUMBER-OF-ARGS 1) 
(DE RA_VCATRANSFORM1 (F)
    (PROG (XP1 FF FFF C D CC)
      (SETQ C 0)
      (SETQ D 0)
      (SETQ CC 0)
      (COND (NIL NIL))
      (SETQ XP1 (ADDF (LIST (CONS (CONS 'X 1) 1)) 1))
      (SETQ C 1)
      (SETQ D (CDAAR F))
      (SETQ FF (CONS (CONS (CONS 'X D) (CDAR F)) NIL))
      (SETQ FFF (CDAR F))
      (SETQ F (CDR F))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM F) (ATOM (CAR F))))) (RETURN NIL)))
        (PROGN
         (SETQ C (TIMES C (EXPT 2 (DIFFERENCE D (CDAAR F)))))
         (SETQ CC (TIMES C (CDAR F)))
         (SETQ FF (ADDF FF (CONS (CONS (CONS 'X (CDAAR F)) CC) NIL)))
         (SETQ FFF
                 (ADDF
                  ((LAMBDA (G142)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF FFF G142))
                           (T (POLY-MULTF FFF G142))))
                   (EXPTF XP1 (DIFFERENCE D (CDAAR F))))
                  CC))
         (SETQ D (CDAAR F))
         (SETQ F (CDR F)))
        (GO WHILELABEL))
      (SETQ C (TIMES C (EXPT 2 D)))
      (SETQ CC (TIMES C (SFTO_SF2INT F)))
      (SETQ FF (ADDF FF CC))
      (SETQ FFF
              (ADDF
               ((LAMBDA (G144)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF FFF G144))
                        (T (POLY-MULTF FFF G144))))
                (EXPTF XP1 D))
               CC))
      (RETURN (CONS FF FFF)))) 
(PUT 'RA_BUDAN-0-1 'NUMBER-OF-ARGS 1) 
(DE RA_BUDAN-0-1 (F)
    (PROG (FF C SIGN CURSIGN SIGNCHANGES)
      (SETQ SIGN 0)
      (SETQ CURSIGN 0)
      (SETQ SIGNCHANGES 0)
      (SETQ FF (RA_BUDAN-TRANSFORM F))
      (PROG ()
       WHILELABEL
        (COND ((NOT FF) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (ATOM FF) (ATOM (CAR FF))) (PROGN (SETQ C FF) (SETQ FF NIL)))
          (T (PROGN (SETQ C (CDAR FF)) (SETQ FF (CDR FF)))))
         (SETQ SIGN (COND ((MINUSF C) (MINUS 1)) (T 1)))
         (COND
          ((LESSP (TIMES SIGN CURSIGN) 0)
           (SETQ SIGNCHANGES (PLUS SIGNCHANGES 1))))
         (SETQ CURSIGN SIGN)
         NIL)
        (GO WHILELABEL))
      (RETURN SIGNCHANGES))) 
(PUT 'RA_BUDAN-TRANSFORM 'NUMBER-OF-ARGS 1) 
(DE RA_BUDAN-TRANSFORM (F)
    (PROG (XP1 FF RD)
      (SETQ XP1 (ADDF (LIST (CONS (CONS 'X 1) 1)) 1))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RETURN F)))
      (SETQ FF (RA_BUDAN-TRANSFORM (CDR F)))
      (SETQ RD
              (COND ((OR (ATOM (CDR F)) (ATOM (CAR (CDR F)))) 0)
                    (T (CDAAR (CDR F)))))
      (RETURN
       (ADDF (CDAR F)
             ((LAMBDA (G145)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G145 FF))
                      (T (POLY-MULTF G145 FF))))
              (EXPTF XP1 (DIFFERENCE (CDAAR F) RD))))))) 
(PUT 'RA_BUDANCOUNT 'NUMBER-OF-ARGS 3) 
(DE RA_BUDANCOUNT (F L U) (RA_BUDAN-0-1 (RA_TRANSFORM F L U))) 
(PUT 'RA_BUDAN-0-1_OLD 'NUMBER-OF-ARGS 1) 
(DE RA_BUDAN-0-1_OLD (F)
    (PROG (XP1 FF C SIGN CURSIGN SIGNCHANGES)
      (SETQ SIGN 0)
      (SETQ CURSIGN 0)
      (SETQ SIGNCHANGES 0)
      (SETQ XP1 (ADDF (LIST (CONS (CONS 'X 1) 1)) 1))
      (SETQ FF (SFTO_QSUB1 F (LIST (CONS 'X (CONS 1 XP1)))))
      (SETQ FF (MULTSQ (CONS (EXPTF XP1 (CDAAR F)) 1) FF))
      (SETQ FF (CAR FF))
      (PROG ()
       WHILELABEL
        (COND ((NOT FF) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (ATOM FF) (ATOM (CAR FF))) (PROGN (SETQ C FF) (SETQ FF NIL)))
          (T (PROGN (SETQ C (CDAR FF)) (SETQ FF (CDR FF)))))
         (SETQ SIGN (COND ((MINUSF C) (MINUS 1)) (T 1)))
         (COND
          ((LESSP (TIMES SIGN CURSIGN) 0)
           (SETQ SIGNCHANGES (PLUS SIGNCHANGES 1))))
         (SETQ CURSIGN SIGN)
         NIL)
        (GO WHILELABEL))
      (RETURN SIGNCHANGES))) 
(ENDMODULE) 
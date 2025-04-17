(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INVBCOMP)) 
(DE C_ZERO NIL NIL) 
(PUT 'C_ZERO 'NUMBER-OF-ARGS 0) 
(PUT 'C_ZERO 'DEFINED-ON-LINE '29) 
(PUT 'C_ZERO 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'C_ZERO 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'C_ZERO 'INLINE '(LAMBDA () NIL)) 
(DE CNEG (C) (NEGF C)) 
(PUT 'CNEG 'NUMBER-OF-ARGS 1) 
(PUT 'CNEG 'DEFINED-ON-LINE '31) 
(PUT 'CNEG 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'CNEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'CNEG 'INLINE '(LAMBDA (C) (NEGF C))) 
(DE CSUM (C1 C2) (ADDF C1 C2)) 
(PUT 'CSUM 'NUMBER-OF-ARGS 2) 
(PUT 'CSUM 'DEFINED-ON-LINE '34) 
(PUT 'CSUM 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'CSUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CSUM 'INLINE '(LAMBDA (C1 C2) (ADDF C1 C2))) 
(DE CPROD (C1 C2)
    (COND (*PHYSOP-LOADED (PHYSOP-MULTF C1 C2)) (T (POLY-MULTF C1 C2)))) 
(PUT 'CPROD 'NUMBER-OF-ARGS 2) 
(PUT 'CPROD 'DEFINED-ON-LINE '37) 
(PUT 'CPROD 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'CPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CPROD 'INLINE
      '(LAMBDA (C1 C2)
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF C1 C2)) (T (POLY-MULTF C1 C2))))) 
(DE CDIV (C1 C2) (CAR (RESIMP (CONS C1 C2)))) 
(PUT 'CDIV 'NUMBER-OF-ARGS 2) 
(PUT 'CDIV 'DEFINED-ON-LINE '40) 
(PUT 'CDIV 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'CDIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CDIV 'INLINE '(LAMBDA (C1 C2) (CAR (RESIMP (CONS C1 C2))))) 
(PUT 'TRASS 'NUMBER-OF-ARGS 2) 
(PUT 'TRASS 'DEFINED-ON-LINE '43) 
(PUT 'TRASS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'TRASS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRASS (ID VALUE)
    (PROGN
     (TERPRI)
     (PROGN (PRIN2 ID) NIL)
     (PROGN (PRIN2 " = ") NIL)
     (PROGN (PRIN2 VALUE) NIL)
     (TERPRI)
     NIL)) 
(PUT 'LEFTZEROS 'NUMBER-OF-ARGS 1) 
(PUT 'LEFTZEROS 'DEFINED-ON-LINE '46) 
(PUT 'LEFTZEROS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'LEFTZEROS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEFTZEROS (U)
    (COND ((OR (NULL U) (NEQ (CAR U) 0)) 0) (T (IPLUS2 1 (LEFTZEROS (CDR U)))))) 
(PUT 'CLASS 'NUMBER-OF-ARGS 1) 
(PUT 'CLASS 'DEFINED-ON-LINE '49) 
(PUT 'CLASS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'CLASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLASS (JET)
    (COND ((EQUAL (ORD JET) 0) 0)
          (T
           (IPLUS2 1
                   (LEFTZEROS
                    (REVERSE
                     (COND ((EQUAL ORDERING 'LEX) JET) (T (CDR JET))))))))) 
(PUT 'ORD 'NUMBER-OF-ARGS 1) 
(PUT 'ORD 'DEFINED-ON-LINE '53) 
(PUT 'ORD 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'ORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ORD (JET)
    (COND ((EQUAL ORDERING 'LEX) (EVAL (CONS 'PLUS JET))) (T (CAR JET)))) 
(DE LJET (P) (CAAR P)) 
(PUT 'LJET 'NUMBER-OF-ARGS 1) 
(PUT 'LJET 'DEFINED-ON-LINE '56) 
(PUT 'LJET 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'LJET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LJET 'INLINE '(LAMBDA (P) (CAAR P))) 
(PUT 'SUB01 'NUMBER-OF-ARGS 2) 
(PUT 'SUB01 'DEFINED-ON-LINE '58) 
(PUT 'SUB01 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'SUB01 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUB01 (V U)
    (COND (U (CONS (COND ((EQUAL (CAR U) V) 1) (T 0)) (SUB01 V (CDR U)))))) 
(PUT '*V2J 'NUMBER-OF-ARGS 1) 
(PUT '*V2J 'DEFINED-ON-LINE '62) 
(PUT '*V2J 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT '*V2J 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *V2J (V)
    (COND ((EQUAL ORDERING 'LEX) (SUB01 V VARLIST*))
          (T (CONS 1 (SUB01 V VARLIST*))))) 
(PUT 'NONMULT 'NUMBER-OF-ARGS 1) 
(PUT 'NONMULT 'DEFINED-ON-LINE '65) 
(PUT 'NONMULT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'NONMULT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NONMULT (CL)
    (REVERSE (CDR (MEMBER (NTH (REVERSE VJETS*) CL) (REVERSE VJETS*))))) 
(PUT 'INSERT 'NUMBER-OF-ARGS 2) 
(PUT 'INSERT 'DEFINED-ON-LINE '68) 
(PUT 'INSERT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'INSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSERT (X GG*)
    (PROG (GG1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND GG* (DLESS (CDR X) (CDAR GG*)))) (RETURN NIL)))
        (PROGN (SETQ GG1 (CONS (CAR GG*) GG1)) (SETQ GG* (CDR GG*)))
        (GO WHILELABEL))
      (RETURN (APPEND (REVERSIP GG1) (CONS X GG*))))) 
(PUT 'ADDNEW 'NUMBER-OF-ARGS 3) 
(PUT 'ADDNEW 'DEFINED-ON-LINE '75) 
(PUT 'ADDNEW 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'ADDNEW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDNEW (F IND FF)
    (PROGN
     (PUTV GV* IND F)
     (PUTV BV* IND T)
     (COND ((NULL F) FF) (T (SETQ FF (INSERT (CONS IND (CAAR F)) FF)))))) 
(PUT 'DLESSLEX 'NUMBER-OF-ARGS 2) 
(PUT 'DLESSLEX 'DEFINED-ON-LINE '82) 
(PUT 'DLESSLEX 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DLESSLEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DLESSLEX (D1 D2)
    (COND ((NULL D1) NIL) ((IGREATERP (CAR D1) (CAR D2)) NIL)
          ((ILESSP (CAR D1) (CAR D2)) T) (T (DLESSLEX (CDR D1) (CDR D2))))) 
(PUT 'DLESS 'NUMBER-OF-ARGS 2) 
(PUT 'DLESS 'DEFINED-ON-LINE '88) 
(PUT 'DLESS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DLESS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DLESS (D1 D2)
    (COND ((EQUAL ORDERING 'LEX) (DLESSLEX D1 D2))
          ((ILESSP (CAR D1) (CAR D2)) T) ((IGREATERP (CAR D1) (CAR D2)) NIL)
          ((EQUAL ORDERING 'GLEX) (DLESSLEX (CDR D1) (CDR D2)))
          ((EQUAL ORDERING 'GREV)
           (DLESSLEX (REVERSE (CDR D2)) (REVERSE (CDR D1)))))) 
(PUT 'DDMULT 'NUMBER-OF-ARGS 2) 
(PUT 'DDMULT 'DEFINED-ON-LINE '94) 
(PUT 'DDMULT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DDMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DDMULT (D1 D2)
    (COND ((NULL D1) NIL)
          (T (CONS (IPLUS2 (CAR D1) (CAR D2)) (DDMULT (CDR D1) (CDR D2)))))) 
(PUT 'DQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'DQUOT 'DEFINED-ON-LINE '97) 
(PUT 'DQUOT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DQUOT (D2 D1)
    (PROG (D3 N)
      (SETQ N 0)
     L1
      (SETQ N (DIFFERENCE (CAR D2) (CAR D1)))
      (COND ((ILESSP N 0) (RETURN NIL)))
      (SETQ D3 (CONS N D3))
      (SETQ D1 (CDR D1))
      (SETQ D2 (CDR D2))
      (COND (D1 (GO L1)))
      (RETURN (REVERSIP D3)))) 
(PUT 'PCMULT 'NUMBER-OF-ARGS 2) 
(PUT 'PCMULT 'DEFINED-ON-LINE '108) 
(PUT 'PCMULT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PCMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PCMULT (P C)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS (CAR X)
                                (COND (*PHYSOP-LOADED (PHYSOP-MULTF C (CDR X)))
                                      (T (POLY-MULTF C (CDR X))))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (CONS (CAR X)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF C (CDR X)))
                              (T (POLY-MULTF C (CDR X))))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PCDIV 'NUMBER-OF-ARGS 2) 
(PUT 'PCDIV 'DEFINED-ON-LINE '111) 
(PUT 'PCDIV 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PCDIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PCDIV (P C)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS (CAR X) (CAR (RESIMP (CONS (CDR X) C)))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X) (CONS (CAR X) (CAR (RESIMP (CONS (CDR X) C)))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PDMULT 'NUMBER-OF-ARGS 2) 
(PUT 'PDMULT 'DEFINED-ON-LINE '114) 
(PUT 'PDMULT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PDMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PDMULT (P D)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS
                           (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Y (PAIR (CAR X) D))
                             (COND ((NULL Y) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (IPLUS2 (CAR Y) (CDR Y)))
                                               (CAR Y))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Y (CDR Y))
                             (COND ((NULL Y) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Y) (IPLUS2 (CAR Y) (CDR Y)))
                                       (CAR Y))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))
                           (CDR X)))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (CONS
                   (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                     (SETQ Y (PAIR (CAR X) D))
                     (COND ((NULL Y) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Y) (IPLUS2 (CAR Y) (CDR Y)))
                                       (CAR Y))
                                      NIL)))
                    LOOPLABEL
                     (SETQ Y (CDR Y))
                     (COND ((NULL Y) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (Y) (IPLUS2 (CAR Y) (CDR Y))) (CAR Y))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))
                   (CDR X)))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PSUM 'NUMBER-OF-ARGS 2) 
(PUT 'PSUM 'DEFINED-ON-LINE '118) 
(PUT 'PSUM 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PSUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PSUM (P1 P2)
    (PROG (T1 T2 D2 C3 P3 SUM RET)
      (COND ((NULL P1) (SETQ SUM P2)) ((NULL P2) (SETQ SUM P1))
            (T
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P2 (NOT RET))) (RETURN NIL)))
               (PROGN
                (SETQ T2 (CAR P2))
                (SETQ D2 (CAR T2))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND P1 (DLESS D2 (CAAR P1)))) (RETURN NIL)))
                  (PROGN (SETQ P3 (CONS (CAR P1) P3)) (SETQ P1 (CDR P1)))
                  (GO WHILELABEL))
                (COND
                 ((NULL P1)
                  (PROGN (SETQ SUM (APPEND (REVERSE P3) P2)) (SETQ RET T)))
                 (T
                  (PROGN
                   (SETQ T1 (CAR P1))
                   (COND
                    ((EQUAL D2 (CAR T1))
                     (PROGN
                      (SETQ C3 (ADDF (CDR T1) (CDR T2)))
                      (COND ((NEQ C3 NIL) (SETQ P3 (CONS (CONS D2 C3) P3))))
                      (SETQ P1 (CDR P1))
                      (SETQ T1 (COND (P1 (CAR P1))))
                      NIL))
                    (T (SETQ P3 (CONS T2 P3))))
                   (SETQ P2 (CDR P2))
                   (COND ((NULL P2) (SETQ SUM (APPEND (REVERSE P3) P1))))))))
               (GO WHILELABEL))))
      (RETURN SUM))) 
(PUT 'PNEG 'NUMBER-OF-ARGS 1) 
(PUT 'PNEG 'DEFINED-ON-LINE '142) 
(PUT 'PNEG 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PNEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PNEG (P)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (CONS (CAR X) (NEGF (CDR X)))) (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (X) (CONS (CAR X) (NEGF (CDR X)))) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(DE PDIF (P1 P2) (PSUM P1 (PNEG P2))) 
(PUT 'PDIF 'NUMBER-OF-ARGS 2) 
(PUT 'PDIF 'DEFINED-ON-LINE '145) 
(PUT 'PDIF 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PDIF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'PDIF 'INLINE '(LAMBDA (P1 P2) (PSUM P1 (PNEG P2)))) 
(PUT 'DD 'NUMBER-OF-ARGS 2) 
(PUT 'DD 'DEFINED-ON-LINE '148) 
(PUT 'DD 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DD (D1 D2)
    (PROG (DQ LZ)
      (SETQ DQ (DQUOT D2 D1))
      (COND ((NOT DQ) (RETURN (COND ((DLESS D1 D2) 1) (T 0)))))
      (COND ((NEQ ORDERING 'LEX) (SETQ DQ (CDR DQ))))
      (SETQ LZ (LEFTZEROS DQ))
      (RETURN
       (COND
        ((AND (NOT NC*)
              (NOT (ILESSP LZ (IDIFFERENCE (LENGTH VARLIST*) (CLASS D1)))))
         3)
        ((AND NC* (NOT (ILESSP LZ (IDIFFERENCE (LENGTH VARLIST*) NC*)))) 4)
        (T 2))))) 
(PUT 'DLCM 'NUMBER-OF-ARGS 2) 
(PUT 'DLCM 'DEFINED-ON-LINE '163) 
(PUT 'DLCM 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DLCM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DLCM (D1 D2)
    (COND
     ((EQUAL ORDERING 'LEX)
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (PAIR D1 D2))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (MAX (CAR X) (CDR X))) (CAR X))
                              NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (MAX (CAR X) (CDR X))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (ADDGT
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (PAIR (CDR D1) (CDR D2)))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (MAX (CAR X) (CDR X))) (CAR X))
                               NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (MAX (CAR X) (CDR X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'NF 'NUMBER-OF-ARGS 3) 
(PUT 'NF 'DEFINED-ON-LINE '167) 
(PUT 'NF 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'NF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NF (H GG* SW)
    (COND ((NULL GG*) H)
          (T
           (PROG (F LPF G C CF CG NF G1 G2 U NR)
             (SETQ NR 0)
             (SETQ F H)
             (SETQ G1 GG*)
            NEXTLPF
             (COND ((NULL F) (GO EXIT)))
             (SETQ LPF (CAAR F))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND (NOT (NULL G1)) (DLESS LPF (CDAR G1))))
                 (RETURN NIL)))
               (SETQ G1 (CDR G1))
               (GO WHILELABEL))
             (COND ((NULL G1) (GO EXIT)))
             (SETQ G2 G1)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND G2 (ILESSP (DD (CDAR G2) LPF) (PLUS SW 2))))
                 (RETURN NIL)))
               (SETQ G2 (CDR G2))
               (GO WHILELABEL))
             (COND
              ((NULL G2)
               (COND
                (REDTAILS
                 (PROGN
                  (SETQ NF (CONS (CONS LPF (CDAR F)) NF))
                  (SETQ F (CDR F))))
                (T (GO EXIT))))
              (T
               (PROGN
                (SETQ G (GETV GV* (CAAR G2)))
                (SETQ C (GCDF* (CDAR F) (CDAR G)))
                (SETQ CF (CAR (RESIMP (CONS (CDAR F) C))))
                (SETQ CG (CAR (RESIMP (CONS (CDAR G) C))))
                (SETQ F (PCMULT F CG))
                (SETQ NF (PCMULT NF CG))
                (SETQ G (PCMULT G CF))
                (SETQ U (PDMULT (CDR G) (DQUOT LPF (CDAR G2))))
                (COND
                 (TRED
                  (PROGN
                   (TERPRI)
                   (PROGN
                    (PRIN2 "r e d u c t i o n :  ")
                    (PRIN2 LPF)
                    (PRIN2 "/")
                    (PRIN2 (CDAR G2))
                    NIL)
                   (TERPRI)
                   NIL)))
                (COND (STARS (PROGN (PRIN2 "*") NIL)))
                (SETQ NR (IPLUS2 NR 1))
                (SETQ F (PSUM (CDR F) (PNEG U)))
                NIL)))
             (GO NEXTLPF)
            EXIT
             (SETQ REDUCTIONS* (IPLUS2 REDUCTIONS* NR))
             (SETQ NFORMS* (IPLUS2 NFORMS* 1))
             (SETQ U (GCDOUT (APPEND (REVERSIP NF) F)))
             (COND ((NULL U) (SETQ ZEROS* (IPLUS2 ZEROS* 1))))
             (RETURN U))))) 
(PUT 'GCDOUT 'NUMBER-OF-ARGS 1) 
(PUT 'GCDOUT 'DEFINED-ON-LINE '215) 
(PUT 'GCDOUT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'GCDOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GCDOUT (P)
    (COND (*MODULAR P) ((NULL P) NIL) ((EQUAL (ORD (CAAR P)) 0) P)
          (T
           (PROG (C P1)
             (SETQ P1 P)
             (SETQ C (CDAR P1))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P1 (NEQ C 1))) (RETURN NIL)))
               (PROGN (SETQ C (GCDF* C (CDAR P1))) (SETQ P1 (CDR P1)))
               (GO WHILELABEL))
             (RETURN (COND ((EQUAL C 1) P) (T (PCDIV P C)))))))) 
(PUT 'NEWBASIS 'NUMBER-OF-ARGS 2) 
(PUT 'NEWBASIS 'DEFINED-ON-LINE '225) 
(PUT 'NEWBASIS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'NEWBASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEWBASIS (GG* SW)
    (PROG (G1 G2)
      (SETQ G1 (REVERSE GG*))
      (PROG ()
       WHILELABEL
        (COND ((NOT G1) (RETURN NIL)))
        (PROGN
         (PUTV GV* (CAAR G1) (NF (GETV GV* (CAAR G1)) G2 SW))
         (SETQ G2 (CONS (CAR G1) G2))
         (SETQ G1 (CDR G1))
         NIL)
        (GO WHILELABEL)))) 
(PUT '*F2DI 'NUMBER-OF-ARGS 2) 
(PUT '*F2DI 'DEFINED-ON-LINE '235) 
(PUT '*F2DI 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT '*F2DI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *F2DI (F VARLIST*)
    (COND ((NULL F) NIL)
          ((OR (ATOM F) (ATOM (CAR F)))
           (CONS
            (CONS
             (ADDGT
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VARLIST*)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) 0) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (V) 0) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
             F)
            NIL))
          (T
           (PSUM
            (COND
             ((MEMBER (CAAAR F) VARLIST*)
              (PDMULT (*F2DI (CDAR F) VARLIST*)
               (ADDGT
                (PROG (V FORALL-RESULT FORALL-ENDPTR)
                  (SETQ V VARLIST*)
                  (COND ((NULL V) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (COND ((EQUAL V (CAAAR F)) (CDAAR F))
                                            (T 0)))
                                    (CAR V))
                                   NIL)))
                 LOOPLABEL
                  (SETQ V (CDR V))
                  (COND ((NULL V) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (V)
                              (COND ((EQUAL V (CAAAR F)) (CDAAR F)) (T 0)))
                            (CAR V))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
             (T
              (PCMULT (*F2DI (CDAR F) VARLIST*) (CONS (CONS (CAAR F) 1) NIL))))
            (*F2DI (CDR F) VARLIST*))))) 
(PUT '*DI2Q0 'NUMBER-OF-ARGS 2) 
(PUT '*DI2Q0 'DEFINED-ON-LINE '248) 
(PUT '*DI2Q0 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT '*DI2Q0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *DI2Q0 (P VARLIST*)
    (COND ((NULL P) (CONS NIL 1))
          (T
           (ADDSQ
            ((LAMBDA (S U)
               (PROGN
                (PROG (X)
                  (SETQ X U)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (COND
                      ((NEQ (CDR X) 0)
                       (SETQ S (MULTSQ S (CONS (CONS (CONS X 1) NIL) 1))))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                S))
             (CDAR P)
             (PAIR VARLIST*
                   (COND ((EQUAL ORDERING 'LEX) (CAAR P)) (T (CDR (CAAR P))))))
            (*DI2Q0 (CDR P) VARLIST*))))) 
(PUT '*DI2Q 'NUMBER-OF-ARGS 2) 
(PUT '*DI2Q 'DEFINED-ON-LINE '258) 
(PUT '*DI2Q 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT '*DI2Q 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *DI2Q (P VARLIST*)
    (*DI2Q0
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X P)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X) (CONS (CAR X) (CONS (CDR X) 1))) (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (CONS (CAR X) (CONS (CDR X) 1))) (CAR X))
                     NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     VARLIST*)) 
(PUT 'SHOW 'NUMBER-OF-ARGS 2) 
(PUT 'SHOW 'DEFINED-ON-LINE '261) 
(PUT 'SHOW 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'SHOW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SHOW (STR P)
    (COND
     ((NULL P)
      (PROGN (ASSGNPRI STR NIL 'FIRST) (ASSGNPRI (AEVAL " := 0") NIL 'LAST)))
     (T
      (PROGN
       (ASSGNPRI STR NIL 'FIRST)
       (ASSGNPRI (AEVAL " := ") NIL NIL)
       (ASSGNPRI (AEVAL (PREPSQ (*DI2Q (LIST (CAR P)) VARLIST*))) NIL NIL)
       (ASSGNPRI (AEVAL " + ") NIL NIL)
       (ASSGNPRI (AEVAL (PREPSQ (*DI2Q (CDR P) VARLIST*))) NIL 'LAST))))) 
(DE ADDGT (U)
    (COND ((EQUAL ORDERING 'LEX) U) (T (CONS (EVAL (CONS 'PLUS U)) U)))) 
(PUT 'ADDGT 'NUMBER-OF-ARGS 1) 
(PUT 'ADDGT 'DEFINED-ON-LINE '267) 
(PUT 'ADDGT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'ADDGT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'ADDGT 'INLINE
      '(LAMBDA (U)
         (COND ((EQUAL ORDERING 'LEX) U) (T (CONS (EVAL (CONS 'PLUS U)) U))))) 
(PUT 'PRINTSYS 'NUMBER-OF-ARGS 2) 
(PUT 'PRINTSYS 'DEFINED-ON-LINE '270) 
(PUT 'PRINTSYS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'PRINTSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINTSYS (STR GG*)
    (PROG (I)
      (SETQ I 0)
      (PROG (X)
        (SETQ X GG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ I (PLUS I 1))
            (PROGN
             (ASSGNPRI STR NIL 'FIRST)
             (ASSGNPRI (AEVAL "(") NIL NIL)
             (ASSGNPRI (AEVAL I) NIL NIL)
             (ASSGNPRI (AEVAL ") := ") NIL NIL)
             (ASSGNPRI
              (AEVAL (PREPSQ (*DI2Q (LIST (CAR (GETV GV* (CAR X)))) VARLIST*)))
              NIL NIL)
             (ASSGNPRI (AEVAL " + ") NIL NIL)
             (ASSGNPRI
              (AEVAL (PREPSQ (*DI2Q (CDR (GETV GV* (CAR X))) VARLIST*))) NIL
              'LAST))
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'ANSWER 'NUMBER-OF-ARGS 1) 
(PUT 'ANSWER 'DEFINED-ON-LINE '280) 
(PUT 'ANSWER 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'ANSWER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANSWER (GG*)
    (PROGN
     (COND
      (TITLE*
       (PROGN
        (ASSGNPRI (AEVAL "% ") NIL 'FIRST)
        (ASSGNPRI (AEVAL TITLE*) NIL 'LAST))))
     (TRASS "% ordering" VARLIST*)
     (PRINTSYS "G" (REVERSE GG*))
     NIL)) 
(PUT 'WR 'NUMBER-OF-ARGS 2) 
(PUT 'WR 'DEFINED-ON-LINE '285) 
(PUT 'WR 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'WR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WR (FILE GG*)
    (PROGN
     (OFF (LIST 'NAT 'TIME))
     (OUT (LIST FILE))
     (PROGN (PRIN2 "algebraic$") NIL)
     (PROGN (PRIN2 "operator g$") NIL)
     (ANSWER GG*)
     (PROGN (PRIN2 "end;") NIL)
     (SHUT (LIST FILE))
     (ON (LIST 'NAT 'TIME)))) 
(PUT 'INVTEST* 'NUMBER-OF-ARGS 0) 
(PUT 'INVTEST* 'DEFINED-ON-LINE '291) 
(PUT 'INVTEST* 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'INVTEST* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INVTEST* NIL
    (PROG (G C)
      (SETQ C T)
      (COND (*TRINVBASE (TERPRI)))
      (PROG (X)
        (SETQ X GG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            (C
             (PROGN
              (SETQ G (GETV GV* (CAR X)))
              (PROG (VJ)
                (SETQ VJ (NONMULT (CLASS (CAAR G))))
               LAB
                (COND ((NULL VJ) (RETURN NIL)))
                ((LAMBDA (VJ)
                   (COND
                    ((AND C (NF (PDMULT G VJ) GG* 1))
                     (PROGN
                      (SETQ C NIL)
                      (COND (*TRINVBASE (PRIN2T "INV - t e s t  f a i l e d")))
                      NIL))))
                 (CAR VJ))
                (SETQ VJ (CDR VJ))
                (GO LAB))
              NIL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND ((AND C *TRINVBASE) (PRIN2T "I n v o l u t i v e  b a s i s")))
      (RETURN C))) 
(PUT 'REDALL 'NUMBER-OF-ARGS 3) 
(PUT 'REDALL 'DEFINED-ON-LINE '306) 
(PUT 'REDALL 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'REDALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDALL (GG* FF SW)
    (PROG (RR F F1 LJ K NEW)
      (SETQ RR FF)
      (SETQ THIRDWAY* (SETQ SHORTWAY* NIL))
      (SETQ NEW T)
      (PROG ()
       WHILELABEL
        (COND ((NOT RR) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR (REVERSE RR)))
         (SETQ RR (DELETE F RR))
         (SETQ K (CAR F))
         (SETQ F1 (GETV GV* K))
         (COND
          (PATH
           (PROGN
            (COND (NEW (PROGN (PRIN2 (CAAR F1)) (PRIN2 " ==> ") NIL))
                  (T (PROGN (PRIN2 (CAAR F1)) (PRIN2 " --> ") NIL)))
            NIL)))
         (SETQ F (PUTV GV* K (NF F1 GG* SW)))
         (SETQ LJ (COND (F (CAAR F)) (T 0)))
         (COND (PATH (PROGN (PROGN (PRIN2 LJ) NIL) (TERPRI))))
         (COND ((NULL F) NIL)
               ((EQUAL (ORD LJ) 0) (SETQ CONDS* (CONS F CONDS*)))
               (T
                (PROGN
                 (COND ((NEQ (CAAR F) (CAAR F1)) (SETQ SHORTWAY* T)))
                 (COND ((AND (NOT NEW) (NEQ F F1)) (SETQ THIRDWAY* T)))
                 (PROG (X)
                   (SETQ X GG*)
                  LAB
                   (COND ((NULL X) (RETURN NIL)))
                   ((LAMBDA (X)
                      (COND
                       ((GEQ (DD LJ (CDR X)) (PLUS SW 2))
                        (PROGN
                         (SETQ GG* (DELETE X GG*))
                         (SETQ RR (INSERT X RR))
                         (PUTV BV* (CAR X) T)
                         NIL))))
                    (CAR X))
                   (SETQ X (CDR X))
                   (GO LAB))
                 (SETQ GG* (INSERT (CONS K LJ) GG*))
                 (SETQ NEW NIL)
                 NIL))))
        (GO WHILELABEL))
      (RETURN GG*))) 
(PUT 'REMRED 'NUMBER-OF-ARGS 2) 
(PUT 'REMRED 'DEFINED-ON-LINE '335) 
(PUT 'REMRED 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'REMRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMRED (FF SW)
    (PROG (GG* GG1 F G P)
      (SETQ FF (REVERSE FF))
      (PROG ()
       WHILELABEL
        (COND ((NOT FF) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FF))
         (SETQ FF (CDR FF))
         (SETQ P T)
         (SETQ GG1 GG*)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND P GG1)) (RETURN NIL)))
           (PROGN
            (SETQ G (CAR GG1))
            (SETQ GG1 (CDR GG1))
            (COND ((GEQ (DD (CDR G) (CDR F)) (PLUS SW 2)) (SETQ P NIL)))
            NIL)
           (GO WHILELABEL))
         (COND (P (SETQ GG* (CONS F GG*))))
         NIL)
        (GO WHILELABEL))
      (RETURN GG*))) 
(PUT 'INVBASE* 'NUMBER-OF-ARGS 0) 
(PUT 'INVBASE* 'DEFINED-ON-LINE '350) 
(PUT 'INVBASE* 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'INVBASE* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INVBASE* NIL
    (PROG (GG1 G K NM F THIRDWAY* SHORTWAY* FIN P P0 LB R)
      (COND (*TRINVBASE (TERPRI)))
      (SETQ P (SETQ MAXORD* (MINUS 1)))
      (COND (PATH (TERPRI)))
      (SETQ GG* (REDALL NIL GG* 1))
      (NEWBASIS GG* 1)
      (SETQ LB 0)
      (PROG (X)
        (SETQ X GG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ LB (PLUS LB (ORD (CDR X))))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ LB (PLUS LB (DIFFERENCE (LENGTH VARLIST*) 1)))
     L
      (SETQ GG1 (REVERSE GG*))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND GG1 (NULL (GETV BV* (CAAR GG1))))) (RETURN NIL)))
        (SETQ GG1 (CDR GG1))
        (GO WHILELABEL))
      (COND
       (GG1
        (PROGN
         (COND
          ((EQUAL (CADAR GG1) (CADAR GG*))
           (PROGN
            (SETQ P0 P)
            (SETQ P (CADAR GG1))
            (COND
             ((AND *TRINVBASE (GREATERP P P0))
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "---------- ORDER = ")
                (PRIN2 (CADAR GG*))
                (PRIN2 " ----------")
                NIL)
               (TERPRI)
               (TERPRI)
               NIL)))
            (COND
             ((GREATERP P LB)
              (PROGN
               (SETQ GG* (REDALL NIL GG* 0))
               (NEWBASIS GG* 0)
               (SETQ INVTEMPBASIS
                       (PROGN
                        (SETQ ALGLIST* (CONS NIL NIL))
                        (CONS 'LIST
                              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                (SETQ X GG*)
                                (COND ((NULL X) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (X)
                                                    (CONS 'PLUS
                                                          (PROG (M
                                                                 FORALL-RESULT
                                                                 FORALL-ENDPTR)
                                                            (SETQ M
                                                                    (GETV GV*
                                                                          (CAR
                                                                           X)))
                                                            (COND
                                                             ((NULL M)
                                                              (RETURN NIL)))
                                                            (SETQ FORALL-RESULT
                                                                    (SETQ FORALL-ENDPTR
                                                                            (CONS
                                                                             ((LAMBDA
                                                                                  (
                                                                                   M)
                                                                                (PREPSQ
                                                                                 (*DI2Q
                                                                                  (LIST
                                                                                   M)
                                                                                  VARLIST*)))
                                                                              (CAR
                                                                               M))
                                                                             NIL)))
                                                           LOOPLABEL
                                                            (SETQ M (CDR M))
                                                            (COND
                                                             ((NULL M)
                                                              (RETURN
                                                               FORALL-RESULT)))
                                                            (RPLACD
                                                             FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (M)
                                                                 (PREPSQ
                                                                  (*DI2Q
                                                                   (LIST M)
                                                                   VARLIST*)))
                                                               (CAR M))
                                                              NIL))
                                                            (SETQ FORALL-ENDPTR
                                                                    (CDR
                                                                     FORALL-ENDPTR))
                                                            (GO LOOPLABEL))))
                                                  (CAR X))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ X (CDR X))
                                (COND ((NULL X) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (CONS 'PLUS
                                                  (PROG (M FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ M (GETV GV* (CAR X)))
                                                    (COND
                                                     ((NULL M) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (M)
                                                                        (PREPSQ
                                                                         (*DI2Q
                                                                          (LIST
                                                                           M)
                                                                          VARLIST*)))
                                                                      (CAR M))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ M (CDR M))
                                                    (COND
                                                     ((NULL M)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (M)
                                                                (PREPSQ
                                                                 (*DI2Q
                                                                  (LIST M)
                                                                  VARLIST*)))
                                                              (CAR M))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL))))
                                          (CAR X))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))
               (REDERR "Maximum degree bound exceeded.")
               NIL)))
            (SETQ MAXORD* (MAX MAXORD* (CADAR GG*)))
            (COND ((LESSP (CADAR GG*) MAXORD*) (SETQ FIN T)))
            NIL)))
         (COND (FIN (GO M)))
         (SETQ K (CAAR GG1))
         (SETQ G (GETV GV* K))
         (PUTV BV* K NIL)
         (SETQ NM (NONMULT (CLASS (CAAR G))))
         (PROG (VJ)
           (SETQ VJ NM)
          LAB
           (COND ((NULL VJ) (RETURN NIL)))
           ((LAMBDA (VJ)
              (PROGN
               (SETQ NG* (PLUS NG* 1))
               (SETQ F (PDMULT G VJ))
               (PUTV GV* NG* F)
               (PUTV BV* NG* T)
               (SETQ GG* (REDALL GG* (LIST (CONS NG* (CAAR F))) 1))
               (COND (THIRDWAY* (NEWBASIS GG* 1))
                     (SHORTWAY*
                      (PROG (Y)
                        (SETQ Y GG*)
                       LAB
                        (COND ((NULL Y) (RETURN NIL)))
                        ((LAMBDA (Y)
                           (COND
                            ((NEQ (CAR Y) NG*)
                             (PUTV GV* (CAR Y)
                                   (NF (GETV GV* (CAR Y))
                                    (LIST (CONS NG* (CAAR (GETV GV* NG*))))
                                    1)))))
                         (CAR Y))
                        (SETQ Y (CDR Y))
                        (GO LAB))))
               NIL))
            (CAR VJ))
           (SETQ VJ (CDR VJ))
           (GO LAB))
         (GO L)
         NIL)))
     M
      (STAT)
      (COND ((LEQ P LB) (DIM GG*))))) 
(DE NJETS (N Q) (COMBIN Q (PLUS Q (DIFFERENCE N 1)))) 
(PUT 'NJETS 'NUMBER-OF-ARGS 2) 
(PUT 'NJETS 'DEFINED-ON-LINE '400) 
(PUT 'NJETS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'NJETS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'NJETS 'INLINE '(LAMBDA (N Q) (COMBIN Q (PLUS Q (DIFFERENCE N 1))))) 
(PUT 'COMBIN 'NUMBER-OF-ARGS 2) 
(PUT 'COMBIN 'DEFINED-ON-LINE '403) 
(PUT 'COMBIN 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'COMBIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMBIN (M N)
    (COND ((GREATERP M N) 0)
          (T
           (PROG (I1 I2)
             (SETQ I1 0)
             (SETQ I2 0)
             (SETQ I1 (SETQ I2 1))
             (PROG (I)
               (SETQ I (PLUS (DIFFERENCE N M) 1))
              LAB
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (SETQ I1 (TIMES I1 I))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (PROG (I)
               (SETQ I 1)
              LAB
               (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
               (SETQ I2 (TIMES I2 I))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN (QUOTIENT I1 I2)))))) 
(PUT 'DIM 'NUMBER-OF-ARGS 1) 
(PUT 'DIM 'DEFINED-ON-LINE '410) 
(PUT 'DIM 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIM (GG*)
    (COND
     (*TRINVBASE
      (PROG (Q N CL S Y DIM DP MON)
        (SETQ Q 0)
        (SETQ N 0)
        (SETQ CL 0)
        (SETQ S 0)
        (SETQ Y 0)
        (SETQ DIM 0)
        (SETQ DP 0)
        (SETQ MON 0)
        (SETQ Q (CADAR GG*))
        (SETQ N (LENGTH VARLIST*))
        (SETQ DIM 0)
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
          (PUTV BETA* I 0)
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (PROG (X)
          (SETQ X GG*)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROGN
              (SETQ CL (CLASS (CDR X)))
              (PROG (I)
                (SETQ I CL)
               LAB
                (COND
                 ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
                (PROGN
                 (SETQ Y
                         ((LAMBDA (G543)
                            (COMBIN G543
                             (PLUS G543
                                   (DIFFERENCE (PLUS (DIFFERENCE CL I) 1) 1))))
                          (DIFFERENCE Q (ORD (CDR X)))))
                 (PUTV BETA* I (PLUS (GETV BETA* I) Y))
                 NIL)
                (SETQ I (PLUS2 I (MINUS 1)))
                (GO LAB))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (TERPRI)
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
          (PROGN
           (PUTV ALFA* I
                 (DIFFERENCE
                  (COMBIN (DIFFERENCE N I) (PLUS Q (DIFFERENCE N I)))
                  (GETV BETA* I)))
           (COND ((NEQ (GETV ALFA* I) 0) (SETQ DIM (PLUS DIM 1))))
           NIL)
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (TERPRI)
        (TERPRI)
        (PROGN (PRIN2 "D i m e n s i o n  =  ") (PRIN2 DIM) NIL)
        (TERPRI)
        (COND ((EQUAL DIM 0) (NROOTS GG*))))))) 
(PUT 'NROOTS 'NUMBER-OF-ARGS 1) 
(PUT 'NROOTS 'DEFINED-ON-LINE '432) 
(PUT 'NROOTS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'NROOTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NROOTS (GG*)
    (COND
     (GG*
      (PROG (D)
        (SETQ D 0)
        (PROG (X)
          (SETQ X GG*)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X) (SETQ D (PLUS D (CAR (REVERSE X))))) (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (TERPRI)
        (PROGN
         (PRIN2 "N u m b e r  o f  s o l u t i o n s  =  ")
         (PRIN2 D)
         NIL)
        (TERPRI))))) 
(PUT 'STAT 'NUMBER-OF-ARGS 0) 
(PUT 'STAT 'DEFINED-ON-LINE '440) 
(PUT 'STAT 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'STAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE STAT NIL
    (COND
     (*TRINVBASE
      (PROGN
       (TERPRI)
       (PROGN (PRIN2 "reductions = ") (PRIN2 REDUCTIONS*) NIL)
       (PROGN (PRIN2 "  zeros = ") (PRIN2 ZEROS*) NIL)
       (PROGN (PRIN2 "  maxord = ") (PRIN2 MAXORD*) NIL)
       (PROGN (PRIN2 "  order = ") (PRIN2 (CADAR GG*)) NIL)
       (PROGN (PRIN2 "  length = ") (PRIN2 (LENGTH GG*)) NIL)
       NIL)))) 
(PUT '*G2LEX 'NUMBER-OF-ARGS 1) 
(PUT '*G2LEX 'DEFINED-ON-LINE '448) 
(PUT '*G2LEX 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT '*G2LEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *G2LEX (P)
    (PROG (P1)
      (PROG (X)
        (SETQ X P)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ P1 (PSUM P1 (LIST (CONS (CDAR X) (CDR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN P1))) 
(PUT 'LEXORDER 'NUMBER-OF-ARGS 1) 
(PUT 'LEXORDER 'DEFINED-ON-LINE '456) 
(PUT 'LEXORDER 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'LEXORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEXORDER (LST)
    (PROG (LST1 LJ)
      (PROG (X)
        (SETQ X LST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ LJ
                    (CAAR
                     (PUTV GV* (CAR X) (GCDOUT (*G2LEX (GETV GV* (CAR X)))))))
            (SETQ LST1 (INSERT (CONS (CAR X) LJ) LST1))
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN LST1))) 
(PUT 'GI 'NUMBER-OF-ARGS 2) 
(PUT 'GI 'DEFINED-ON-LINE '466) 
(PUT 'GI 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'GI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GI (GG* I)
    (PROG (FF)
      (PROG (X)
        (SETQ X GG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((EQUAL (CLASS (CDR X)) I) (SETQ FF (CONS X FF)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN FF))) 
(PUT 'MONIC 'NUMBER-OF-ARGS 2) 
(PUT 'MONIC 'DEFINED-ON-LINE '472) 
(PUT 'MONIC 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'MONIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MONIC (JET CL)
    (PROG (U N)
      (SETQ JET (REVERSE JET))
      (SETQ N (LENGTH VARLIST*))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (COND ((NEQ I CL) (SETQ U (CONS (NTH JET I) U))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN
       (EQUAL U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (CDR VARLIST*))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) 0) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (V) 0) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))))) 
(PUT 'MONICMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'MONICMEMBER 'DEFINED-ON-LINE '480) 
(PUT 'MONICMEMBER 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'MONICMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MONICMEMBER (GG* CL)
    (PROG (P)
     L
      (COND ((NULL GG*) (RETURN NIL)))
      (COND ((MONIC (CDAR GG*) CL) (RETURN T)))
      (SETQ GG* (CDR GG*))
      (GO L))) 
(PUT 'MONTEST 'NUMBER-OF-ARGS 1) 
(PUT 'MONTEST 'DEFINED-ON-LINE '488) 
(PUT 'MONTEST 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'MONTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MONTEST (GG*)
    (PROG (P N)
      (SETQ P T)
      (SETQ N (LENGTH VARLIST*))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (COND
         ((NOT (MONICMEMBER GG* I)) (PROGN (SETQ P NIL) (SETQ I (PLUS N 1)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN P))) 
(PUT 'INVLEX* 'NUMBER-OF-ARGS 0) 
(PUT 'INVLEX* 'DEFINED-ON-LINE '496) 
(PUT 'INVLEX* 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'INVLEX* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INVLEX* NIL
    (PROG (GI N GGINV ORDERING)
      (SETQ N (LENGTH VARLIST*))
      (SETQ GGINV (MKVECT N))
      (SETQ ORDERING 'LEX)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV GGINV I (LEXORDER (GI GG* I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ GG* NIL)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ NC* I)
         (COND (PATH (PROGN (TRASS "i" I) (TERPRI))))
         (SETQ GG* (REDALL GG* (GETV GGINV I) 2))
         (COND ((MONTEST GG*) (SETQ I (PLUS N 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ NC* NIL)
      (SETQ GG* (REMRED GG* 0))
      (NEWBASIS GG* 0))) 
(PUT 'READSYS 'NUMBER-OF-ARGS 2) 
(PUT 'READSYS 'DEFINED-ON-LINE '513) 
(PUT 'READSYS 'DEFINED-IN-FILE 'INVBASE/INVBCOMP.RED) 
(PUT 'READSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE READSYS (ELIST VLIST)
    (PROG ()
      (SETQ VARLIST* (CDR VLIST))
      (SETQ NG* (SETQ REDUCTIONS* (SETQ NFORMS* (SETQ ZEROS* 0))))
      (SETQ ALFA* (MKVECT (LENGTH VARLIST*)))
      (SETQ BETA* (MKVECT (LENGTH VARLIST*)))
      (SETQ GG* NIL)
      (PROG (X)
        (SETQ X (CDR ELIST))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ GG*
                   (ADDNEW (GCDOUT (*F2DI (CAR (SIMP X)) VARLIST*))
                    (SETQ NG* (PLUS NG* 1)) GG*)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VJETS*
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VARLIST*)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (*V2J V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (*V2J V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))) 
(FLAG '(READSYS) 'OPFN) 
(SETQ ORDERING 'GREV) 
(SETQ REDTAILS T) 
(SETQ TRED (SETQ PATH (SETQ STARS NIL))) 
(ENDMODULE) 
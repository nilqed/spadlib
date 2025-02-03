(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFPSI)) 
(IMPORTS (LIST 'SQ2BF* 'SF*EVAL)) 
(EXPORTS
 (LIST 'RDPSI* 'CRPSI* 'DO*POLYGAMMA 'DO*TRIGAMMA*HALVES 'DO*ZETA
       'DO*ZETA*POS*INTCALC)) 
(GLOBAL '(BFZ* BFHALF* BFTWO*)) 
(FLUID '(COMPUTE-BERNOULLI **ROUNDBF *ROUNDBF)) 
(|SET:CONST| '|:EULER_GAMMA|
             '(-1 57721 566490 1532 86060 65120 90082 40243 10 42159 33593
               99235 98805 76723 48848 67726 7776 64670 93694 70632 91746 74951
               46314 47249 80708 24809 605040 1448 65428 36224 17399 76449
               23536 253500 3337 42937 33773 76739 42792 59525 82470 9491
               600873 52039 48165 67085 32331 51776 61152 86211 99501 50798
               47937 45085 7057400 299 21354 78614 66940 29604 32542 15190
               58775 53526 73313 992540 1296 74205 13754 13954 91116 85102
               80798 42348 77587 20503 84310 93997 36137 25530 60889 33126
               76001 72479 53783 67592 71351 57722 61027 34929 13940 798430
               1034 17771 77808 81549 57066 10750 10161 91663 34015 22789
               35868)) 
(PUT '|:EULER_GAMMA| 'NUMBER-OF-ARGS 1) 
(PUT '|:EULER_GAMMA| 'DEFINED-ON-LINE '84) 
(PUT '|:EULER_GAMMA| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|:EULER_GAMMA| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:EULER_GAMMA| (K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|:EULER_GAMMA|))
          (T
           (PROG (U)
             (SETQ U (|GET:CONST| '|:EULER_GAMMA| K))
             (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
             (SETQ U (|RDPSI:| BFONE* K))
             (COND
              ((OR (NEQ (CDR U) 1) (NOT (EQCAR U '|:RD:|)))
               (BFLERRMSG '|:EULER_GAMMA|))
              (T
               (PROGN
                (SETQ U (|:MINUS| (CAR U)))
                (|SAVE:CONST| '|:EULER_GAMMA| U)
                (RETURN U)))))))) 
(PUT 'RD_EULER* 'NUMBER-OF-ARGS 0) 
(PUT 'RD_EULER* 'DEFINED-ON-LINE '99) 
(PUT 'RD_EULER* 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'RD_EULER* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RD_EULER* NIL
    (MKROUND
     (COND (**ROUNDBF (|:EULER_GAMMA| |:BPREC:|))
           (T (BF2FLR (|:EULER_GAMMA| !NBFPD)))))) 
(PUT 'CR_EULER* 'NUMBER-OF-ARGS 0) 
(PUT 'CR_EULER* 'DEFINED-ON-LINE '102) 
(PUT 'CR_EULER* 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'CR_EULER* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CR_EULER* NIL (MKCR (RD_EULER*) (RDZERO*))) 
(FLAG
 '(POLYGAMMA*CALC TRIGAMMA*HALVES |POLYGAMMA:ERROR| POLYGAMMA_AUX |PSI:ERROR|)
 'OPFN) 
(PUT '|PSI:ERROR| 'NUMBER-OF-ARGS 1) 
(PUT '|PSI:ERROR| 'DEFINED-ON-LINE '229) 
(PUT '|PSI:ERROR| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|PSI:ERROR| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PSI:ERROR| (X)
    (RERROR 'SPECFN 0
            (LIST "Psi undefined for nonpositive integer argument" X))) 
(PUT '|POLYGAMMA:ERROR| 'NUMBER-OF-ARGS 2) 
(PUT '|POLYGAMMA:ERROR| 'DEFINED-ON-LINE '232) 
(PUT '|POLYGAMMA:ERROR| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|POLYGAMMA:ERROR| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |POLYGAMMA:ERROR| (N X)
    (COND
     ((NOT (FIXP N))
      (RERROR 'SPECFN 0
              (LIST "Index of Polygamma must be an integer >= 0, not" N)))
     (T
      (RERROR 'SPECFN 0
              (LIST "Polygamma undefined for nonpositive integer argument"
                    X))))) 
(PUT 'POLYGAMMA_AUX 'NUMBER-OF-ARGS 2) 
(FLAG '(POLYGAMMA_AUX) 'OPFN) 
(PUT 'POLYGAMMA_AUX 'DEFINED-ON-LINE '238) 
(PUT 'POLYGAMMA_AUX 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'POLYGAMMA_AUX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYGAMMA_AUX (N M)
    (PROG (II FORALL-RESULT)
      (SETQ II 1)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) II))
        (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (AEVAL*
               (LIST 'PLUS
                     (AEVAL*
                      (LIST 'QUOTIENT 1 (LIST 'EXPT II (LIST 'PLUS M 1))))
                     FORALL-RESULT)))
      (SETQ II
              ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
               II))
      (GO LAB1))) 
(PUT 'DO*POLYGAMMA 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*POLYGAMMA) 'OPFN) 
(PUT 'DO*POLYGAMMA 'DEFINED-ON-LINE '352) 
(PUT 'DO*POLYGAMMA 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'DO*POLYGAMMA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*POLYGAMMA (N Z) (SF*EVAL 'POLYGAMMA*CALC (LIST 'LIST N Z))) 
(PUT 'DO*TRIGAMMA*HALVES 'NUMBER-OF-ARGS 1) 
(FLAG '(DO*TRIGAMMA*HALVES) 'OPFN) 
(PUT 'DO*TRIGAMMA*HALVES 'DEFINED-ON-LINE '355) 
(PUT 'DO*TRIGAMMA*HALVES 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'DO*TRIGAMMA*HALVES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DO*TRIGAMMA*HALVES (Z) (SF*EVAL 'TRIGAMMA*HALVES (LIST 'LIST Z))) 
(PUT 'DO*ZETA 'NUMBER-OF-ARGS 1) 
(FLAG '(DO*ZETA) 'OPFN) 
(PUT 'DO*ZETA 'DEFINED-ON-LINE '358) 
(PUT 'DO*ZETA 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'DO*ZETA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DO*ZETA (Z)
    (COND
     ((AND (EVALLEQ (AEVAL Z) (AEVAL '(|:DN:| 15 . -1)))
           (EVALLEQ (AEVAL (LIST 'PRECISION 0))
                    (AEVAL (LIST 'FLOOR (LIST 'PLUS 4 (LIST 'TIMES 3 Z))))))
      (AEVAL (LIST 'RAW*ZETA Z)))
     ((EVALGREATERP (AEVAL (LIST 'TIMES 3 Z))
                    (AEVAL (LIST 'TIMES 10 (LIST 'PRECISION 0))))
      (AEVAL '(|:DN:| 10 . -1)))
     ((EVALGREATERP (AEVAL Z) 100) (AEVAL (SF*EVAL 'ZETA*CALC (LIST 'LIST Z))))
     (T (AEVAL (SF*EVAL 'ZETA*GENERAL*CALC (LIST 'LIST Z)))))) 
(PUT 'DO*ZETA*POS*INTCALC 'NUMBER-OF-ARGS 1) 
(FLAG '(DO*ZETA*POS*INTCALC) 'OPFN) 
(PUT 'DO*ZETA*POS*INTCALC 'DEFINED-ON-LINE '365) 
(PUT 'DO*ZETA*POS*INTCALC 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'DO*ZETA*POS*INTCALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DO*ZETA*POS*INTCALC (Z) (SF*EVAL 'ZETA*POS*INTCALC (LIST 'LIST Z))) 
(PUT 'RDPSI* 'NUMBER-OF-ARGS 1) 
(PUT 'RDPSI* 'DEFINED-ON-LINE '445) 
(PUT 'RDPSI* 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'RDPSI* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDPSI* (U)
    (COND ((|RD:ONEP| U) (|RD:MINUS| (RD_EULER*)))
          ((|RD:ONEP| (|RD:TIMES| BFTWO* U))
           (|RD:MINUS|
            (|RD:PLUS| (RD_EULER*) (|RD:TIMES| BFTWO* (RDLOG* (RDTWO*))))))
          (T ((LAMBDA (**ROUNDBF) (|RDPSI:| (CONVPREC U) |:BPREC:|)) T)))) 
(GLOBAL '(|BERN300:| |I300:|)) 
(SETQ |BERN300:| (|ROUND:MT| (|READ:LNUM| '(372 71738 33244 27)) 48)) 
(SETQ |I300:| (|READ:LNUM| '(-3 33333 33333 33333))) 
(PUT '|RDPSI:COMPUTE-LOWERBOUND| 'NUMBER-OF-ARGS 1) 
(PUT '|RDPSI:COMPUTE-LOWERBOUND| 'DEFINED-ON-LINE '484) 
(PUT '|RDPSI:COMPUTE-LOWERBOUND| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|RDPSI:COMPUTE-LOWERBOUND| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RDPSI:COMPUTE-LOWERBOUND| (EPS)
    (|EXP:|
     (NORMBF
      (|ROUND:MT| (|TIMES:| (|LOG:| (|DIVIDE:| |BERN300:| EPS 32) 32) |I300:|)
                  |:BPREC:|))
     32)) 
(PUT '|RDPSI:| 'NUMBER-OF-ARGS 2) 
(PUT '|RDPSI:| 'DEFINED-ON-LINE '488) 
(PUT '|RDPSI:| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|RDPSI:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RDPSI:| (Z K)
    (COND
     ((OR (|BFZEROP:| Z) (AND (|RD:MINUSP| Z) (|INTEGERP:| Z)))
      (BFLERRMSG '|RDPSI:|))
     (T
      (PROG (RESULT ADMISSABLE LB REFL PIVAL SHIFT K7)
        (SETQ SHIFT 0)
        (SETQ K7 0)
        (SETQ K7 (PLUS K 7))
        (COND
         ((|RD:MINUSP| Z)
          (PROGN
           (SETQ PIVAL (|:PI| K7))
           (SETQ REFL (|DIVIDE:| PIVAL (|TAN:| (|TIMES:| PIVAL Z) K7) K7))
           (SETQ Z (|DIFFERENCE:| BFONE* Z))
           NIL)))
        (SETQ ADMISSABLE (CONS '|:RD:| (CONS 1 (DIFFERENCE 6 K))))
        (SETQ LB (|RDPSI:COMPUTE-LOWERBOUND| ADMISSABLE))
        (COND
         ((|GREATERP:| LB Z)
          (PROGN
           (SETQ SHIFT (PLUS 20 (|CONV:BF2I| (|DIFFERENCE:| LB Z))))
           (SETQ Z (|PLUS:| Z (CONS '|:RD:| (CONS SHIFT 0)))))))
        (SETQ RESULT
                (|PLUS:|
                 (|DIFFERENCE:| (|LOG:| Z K7)
                                (|DIVIDE:| BFONE* (|TIMES:| BFTWO* Z) K7))
                 (|RDPSI:1| (|DIVIDE:| BFONE* (|TIMES:| Z Z) K7) K
                  ADMISSABLE)))
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((MINUSP (DIFFERENCE SHIFT N)) (RETURN NIL)))
          (SETQ RESULT
                  (|DIFFERENCE:| RESULT
                                 (|DIVIDE:| BFONE*
                                            (|DIFFERENCE:| Z
                                                           (CONS '|:RD:|
                                                                 (CONS N 0)))
                                            K7)))
          (SETQ N (PLUS2 N 1))
          (GO LAB))
        (COND (REFL (SETQ RESULT (|DIFFERENCE:| RESULT REFL))))
        (RETURN (|ROUND:MT| RESULT K)))))) 
(PUT '|RDPSI:1| 'NUMBER-OF-ARGS 3) 
(PUT '|RDPSI:1| 'DEFINED-ON-LINE '519) 
(PUT '|RDPSI:1| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|RDPSI:1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |RDPSI:1| (ZSQ KP ADMISSABLE)
    (PROG (RESULT ZSQP THIS BK K K7)
      (SETQ K 0)
      (SETQ K7 0)
      (SETQ K 2)
      (SETQ K7 (PLUS KP 7))
      (SETQ ZSQP ZSQ)
      (SETQ RESULT BFZ*)
      (PROG ()
       REPEATLABEL
        (PROGN
         ((LAMBDA (**ROUNDBF)
            (SETQ BK
                    ((LAMBDA (X)
                       (COND ((FLOATP X) (FL2BF X))
                             (T
                              (NORMBF
                               (COND ((NOT (ATOM X)) X)
                                     ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                     (T (|READ:NUM| X)))))))
                     ((LAMBDA (X)
                        (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                              (T
                               ((LAMBDA (Y)
                                  (COND
                                   ((NEQ (CAR Y) '|:RD:|)
                                    ((LAMBDA (U)
                                       (COND ((ATOM U) U)
                                             (T (CONS '|:RD:| U))))
                                     (CDR (*RN2RD Y))))
                                   (T
                                    (COND ((ATOM (CDR Y)) (CDR Y))
                                          (T (CONS '|:RD:| (CDR Y)))))))
                                (*Q2F (SIMP* X))))))
                      (BERNOULLI*CALC K)))))
          **ROUNDBF)
         (SETQ THIS
                 (|DIVIDE:| (|TIMES:| BK ZSQP) (CONS '|:RD:| (CONS K 0)) K7))
         (SETQ RESULT (|DIFFERENCE:| RESULT THIS))
         (SETQ K (PLUS K 2))
         (SETQ ZSQP (|CUT:MT| (|TIMES:| ZSQP ZSQ) K7)))
        (COND ((NOT (|LESSP:| (|ABS:| THIS) ADMISSABLE)) (GO REPEATLABEL))))
      (RETURN RESULT))) 
(PUT 'CRPSI* 'NUMBER-OF-ARGS 1) 
(PUT 'CRPSI* 'DEFINED-ON-LINE '536) 
(PUT 'CRPSI* 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'CRPSI* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CRPSI* (U)
    (COND ((|RD:ZEROP| (CONS '|:RD:| (CDDR U))) (*RD2CR (RDPSI* U)))
          ((|RD:MINUSP| (CONS '|:RD:| (CADR U)))
           (|CR:DIFFER|
            ((LAMBDA (**ROUNDBF)
               (|GFPSI:| (CRPRCD (|CR:DIFFER| (*RD2CR BFONE*) U)) |:BPREC:|))
             T)
            ((LAMBDA (CRPI) (|CR:QUOTIENT| CRPI (CRTAN* (|CR:TIMES| CRPI U))))
             (*RD2CR (PI*)))))
          (T ((LAMBDA (**ROUNDBF) (|GFPSI:| (CRPRCD U) |:BPREC:|)) T)))) 
(PUT '|GFPSI:| 'NUMBER-OF-ARGS 2) 
(PUT '|GFPSI:| 'DEFINED-ON-LINE '548) 
(PUT '|GFPSI:| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|GFPSI:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |GFPSI:| (Z K)
    (COND
     ((|RD:MINUSP| (CONS '|:RD:| (CADR Z)))
      (RERROR 'SPECFN 0
              (LIST
               "Internal error in psi computation: gfpsi!: argument is negative: "
               Z)))
     (T
      (PROG (RESULT ADMISSABLE GFNORM GFLOG LB SHIFT K7)
        (SETQ SHIFT 0)
        (SETQ K7 0)
        (SETQ K7 (PLUS K 7))
        (SETQ ADMISSABLE (CONS '|:RD:| (CONS 1 (DIFFERENCE 6 K))))
        (SETQ LB (|RDPSI:COMPUTE-LOWERBOUND| ADMISSABLE))
        (COND
         ((AND (|LESSP:| (CAR Z) LB) (|LESSP:| (|ABS:| (CDR Z)) LB))
          (PROGN
           (SETQ SHIFT
                   (|CONV:BF2I|
                    (|DIFFERENCE:|
                     (BFSQRT
                      (|DIFFERENCE:| (|TIMES:| LB LB)
                                     (|TIMES:| (CDR Z) (CDR Z))))
                     (CAR Z))))
           (COND
            ((GREATERP SHIFT 0)
             (PROGN
              (SETQ SHIFT (PLUS SHIFT 20))
              (SETQ Z
                      (GFPLUS Z
                              (CONS (CONS '|:RD:| (CONS SHIFT 0)) BFZ*)))))))))
        (SETQ GFNORM (RDHYPOT* (CAR Z) (CDR Z)))
        (SETQ GFLOG (CONS (|LOG:| GFNORM K7) (RDATAN2* (CDR Z) (CAR Z))))
        (SETQ RESULT
                (GFPLUS
                 (GFDIFFER GFLOG
                           (GFQUOTIENT
                            ((LAMBDA (X)
                               (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                             BFONE*)
                            (GFTIMES
                             ((LAMBDA (X)
                                (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                              BFTWO*)
                             Z)))
                 (|GFPSI:1|
                  (GFQUOTIENT
                   ((LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                    BFONE*)
                   (GFTIMES Z Z))
                  K ADMISSABLE)))
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((MINUSP (DIFFERENCE SHIFT N)) (RETURN NIL)))
          (SETQ RESULT
                  (GFDIFFER RESULT
                            (GFQUOTIENT
                             ((LAMBDA (X)
                                (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                              BFONE*)
                             (GFDIFFER Z
                                       (CONS (CONS '|:RD:| (CONS N 0))
                                             BFZ*)))))
          (SETQ N (PLUS2 N 1))
          (GO LAB))
        (RETURN (|GF2CR:| RESULT)))))) 
(PUT '|GFPSI:1| 'NUMBER-OF-ARGS 3) 
(PUT '|GFPSI:1| 'DEFINED-ON-LINE '579) 
(PUT '|GFPSI:1| 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT '|GFPSI:1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |GFPSI:1| (ZSQ KP ADMISSABLE)
    (PROG (RESULT ZSQP THIS BK K K7)
      (SETQ K 0)
      (SETQ K7 0)
      (SETQ K 2)
      (SETQ K7 (PLUS KP 7))
      (SETQ ZSQP ZSQ)
      (SETQ RESULT ((LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*)))) BFZ*))
      (PROG ()
       REPEATLABEL
        (PROGN
         ((LAMBDA (**ROUNDBF)
            (SETQ BK
                    ((LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                     ((LAMBDA (X)
                        (COND ((FLOATP X) (FL2BF X))
                              (T
                               (NORMBF
                                (COND ((NOT (ATOM X)) X)
                                      ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                      (T (|READ:NUM| X)))))))
                      ((LAMBDA (X)
                         (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                               (T
                                ((LAMBDA (Y)
                                   (COND
                                    ((NEQ (CAR Y) '|:RD:|)
                                     ((LAMBDA (U)
                                        (COND ((ATOM U) U)
                                              (T (CONS '|:RD:| U))))
                                      (CDR (*RN2RD Y))))
                                    (T
                                     (COND ((ATOM (CDR Y)) (CDR Y))
                                           (T (CONS '|:RD:| (CDR Y)))))))
                                 (*Q2F (SIMP* X))))))
                       (BERNOULLI*CALC K))))))
          **ROUNDBF)
         (SETQ THIS
                 (GFQUOTIENT (GFTIMES BK ZSQP)
                             ((LAMBDA (X)
                                (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                              (CONS '|:RD:| (CONS K 0)))))
         (SETQ RESULT (GFDIFFER RESULT THIS))
         (SETQ K (PLUS K 2))
         (SETQ ZSQP (GFTIMES ZSQP ZSQ)))
        (COND
         ((NOT (|LESSP:| (RDHYPOT* (CAR THIS) (CDR THIS)) ADMISSABLE))
          (GO REPEATLABEL))))
      (RETURN RESULT))) 
(PUT 'PSI '|:RD:| 'RDPSI*) 
(PUT 'PSI '|:CR:| 'CRPSI*) 
(REMFLAG '(DUMMY*ARG) 'RESERVED) 
(PUT 'POLYGAMMA*AUX 'NUMBER-OF-ARGS 2) 
(PUT 'POLYGAMMA*AUX 'DEFINED-ON-LINE '630) 
(PUT 'POLYGAMMA*AUX 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'POLYGAMMA*AUX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYGAMMA*AUX (N Z0)
    (PROG (POLY X Y PIVAL)
      (SETQ Z0
              (COND ((FIXP Z0) (CONS '|:RD:| (CONS Z0 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z0))))))
      (SETQ POLY (SIMP '(COT DUMMY*ARG)))
      (COND
       ((NOT (KERNP POLY))
        (RERROR 'SPECFN 0
                (LIST "Internal error in polygamma:" N Z0 (PREPSQ Y)))))
      (SETQ Y (CAAAR (CAR POLY)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
        (SETQ POLY (DIFFSQ POLY 'DUMMY*ARG))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ PIVAL (RDPI*))
      (SETQ X (RDCOT* (|RD:TIMES| PIVAL Z0)))
      (SETQ X (SUBSQ POLY (LIST (CONS Y X))))
      (COND
       ((AND (EQUAL (CDR X) 1) (OR (ATOM (CAR X)) (ATOM (CAR (CAR X)))))
        (RETURN (|RD:TIMES| (CAR X) (|TEXPT:| PIVAL N))))
       (T
        (RERROR 'SPECFN 0
                (LIST "Internal error in polygamma:" N Z0 (PREPSQ X))))))) 
(FLAG '(DUMMY*ARG) 'RESERVED) 
(PUT 'POLYGAMMA*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(POLYGAMMA*CALC) 'OPFN) 
(PUT 'POLYGAMMA*CALC 'DEFINED-ON-LINE '664) 
(PUT 'POLYGAMMA*CALC 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'POLYGAMMA*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYGAMMA*CALC (N Z)
    (COND
     ((AND (FIXP (REVALX Z)) (EVALLEQ (AEVAL Z) 0))
      (AEVAL (LIST '|POLYGAMMA:ERROR| N Z)))
     (T
      (PROG (RESULT Z0 PREPRE PRECOM)
        (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
        (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
        (COND
         ((EVALLESSP (AEVAL PREPRE) (AEVAL !NFPD))
          (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 3))))
         (T
          (AEVAL
           (LIST 'PRECISION
                 (LIST 'PLUS PREPRE 3
                       (LIST 'FLOOR (LIST 'QUOTIENT PREPRE 50)))))))
        (COND
         ((EVALGREATERP (AEVAL Z) 0)
          (PROGN
           (SETQ Z0 (AEVAL Z))
           (SETQ RESULT (AEVAL (POLYGAMMA*CALC*S N Z0)))))
         (T
          (PROGN
           (SETQ Z0 (AEVAL (LIST 'DIFFERENCE 1 Z)))
           (SETQ RESULT
                   (AEVAL
                    (LIST 'PLUS (LIST 'TIMES 'PI (POLYGAMMA*AUX N Z0))
                          (POLYGAMMA*CALC*S N Z0))))
           (COND
            ((NOT (BOOLVALUE* (REVALX (LIST 'EVENP N))))
             (SETQ RESULT (AEVAL (LIST 'MINUS RESULT))))))))
        (AEVAL (LIST 'PRECISION PREPRE))
        (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
        (RETURN (AEVAL RESULT)))))) 
(PUT 'POLYGAMMA*CALC*S 'NUMBER-OF-ARGS 2) 
(PUT 'POLYGAMMA*CALC*S 'DEFINED-ON-LINE '698) 
(PUT 'POLYGAMMA*CALC*S 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'POLYGAMMA*CALC*S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYGAMMA*CALC*S (N Z)
    (PROG (RESULT THIS ADMISSABLE PARTIAL ZEXP ZEXP1 ZSQ NFAC NFAC1 KFAC
           RESCALE SIGNER Z0 K NM1 NM2 RP ORDA MIN SCALE)
      (SETQ K 0)
      (SETQ NM1 0)
      (SETQ NM2 0)
      (SETQ RP 0)
      (SETQ ORDA 0)
      (SETQ MIN 0)
      (SETQ SCALE 0)
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ SIGNER (CONS '|:RD:| (CONS (EXPT (MINUS 1) (DIFFERENCE N 1)) 0)))
      (SETQ ADMISSABLE (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS 1 |:BPREC:|)) 8))
      (SETQ MIN
              (PLUS 10
                    (|CONV:BF2I|
                     (|EXP:|
                      (|TIMES:|
                       (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS (PLUS 300 N) 0))
                                  8)
                       (|LOG:|
                        (|DIVIDE:|
                         (NORMBF
                          (|ROUND:MT|
                           (|TIMES:|
                            (|ROUND:MT|
                             (CONS '|:RD:| (CONS (FACTORIAL (PLUS 300 N)) 0))
                             8)
                            (|ABS:|
                             ((LAMBDA (X)
                                (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                      (T
                                       ((LAMBDA (Y)
                                          (COND
                                           ((NEQ (CAR Y) '|:RD:|)
                                            ((LAMBDA (U)
                                               (COND ((ATOM U) U)
                                                     (T (CONS '|:RD:| U))))
                                             (CDR (*RN2RD Y))))
                                           (T
                                            (COND ((ATOM (CDR Y)) (CDR Y))
                                                  (T
                                                   (CONS '|:RD:| (CDR Y)))))))
                                        (*Q2F (SIMP* X))))))
                              (AEVAL (LIST 'BERNOULLI 300)))))
                           |:BPREC:|))
                         (|TIMES:| ADMISSABLE
                                   (|ROUND:MT|
                                    (CONS '|:RD:| (CONS (FACTORIAL 300) 0)) 8))
                         8)
                        8))
                      8))))
      (SETQ SCALE (DIFFERENCE MIN (PLUS 1 (|CONV:BF2I| Z))))
      (COND ((LESSP SCALE 0) (SETQ SCALE 0)))
      (SETQ Z0 (PLUBF Z (CONS '|:RD:| (CONS SCALE 0))))
      (SETQ NFAC
              (|ROUND:MT| (CONS '|:RD:| (CONS (FACTORIAL (DIFFERENCE N 1)) 0))
                          |:BPREC:|))
      (SETQ ZEXP (|TEXPT:ANY| Z0 N))
      (SETQ RESULT
              (PLUBF (NORMBF (|DIVIDE:| NFAC ZEXP |:BPREC:|))
                     (NORMBF
                      (|DIVIDE:|
                       (SETQ NFAC1
                               (NORMBF
                                (|ROUND:MT|
                                 (|TIMES:| (CONS '|:RD:| (CONS N 0)) NFAC)
                                 |:BPREC:|)))
                       (NORMBF
                        (|ROUND:MT|
                         (|TIMES:| BFTWO*
                                   (SETQ ZEXP1
                                           (NORMBF
                                            (|ROUND:MT| (|TIMES:| ZEXP Z0)
                                                        |:BPREC:|))))
                         |:BPREC:|))
                       |:BPREC:|))))
      (SETQ NFAC NFAC1)
      (SETQ ZEXP ZEXP1)
      (SETQ NM1 (DIFFERENCE N 1))
      (SETQ NM2 (DIFFERENCE N 2))
      (SETQ RP |:BPREC:|)
      (SETQ NFAC
              (NORMBF
               (|ROUND:MT| (|TIMES:| NFAC (CONS '|:RD:| (CONS (PLUS N 1) 0)))
                           |:BPREC:|)))
      (SETQ KFAC BFTWO*)
      (SETQ ZEXP (NORMBF (|ROUND:MT| (|TIMES:| ZEXP Z0) |:BPREC:|)))
      (SETQ ZSQ (NORMBF (|ROUND:MT| (|TIMES:| Z0 Z0) |:BPREC:|)))
      (SETQ PARTIAL
              (NORMBF
               (|DIVIDE:| NFAC
                          (NORMBF (|ROUND:MT| (|TIMES:| KFAC ZEXP) |:BPREC:|))
                          |:BPREC:|)))
      (SETQ K 2)
      (SETQ ORDA (DIFFERENCE (|ORDER:| ADMISSABLE) 5))
      (SETQ THIS BFONE*)
      (COND
       ((NULL COMPUTE-BERNOULLI)
        (PROGN (ERRORSET* '(LOAD_PACKAGE '(SPECFAUX)) NIL) NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ RESULT
                 (PLUBF RESULT
                        (SETQ THIS
                                (NORMBF
                                 (|ROUND:MT|
                                  (|TIMES:|
                                   ((LAMBDA (X)
                                      (COND
                                       ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                       (T
                                        ((LAMBDA (Y)
                                           (COND
                                            ((NEQ (CAR Y) '|:RD:|)
                                             ((LAMBDA (U)
                                                (COND ((ATOM U) U)
                                                      (T (CONS '|:RD:| U))))
                                              (CDR (*RN2RD Y))))
                                            (T
                                             (COND ((ATOM (CDR Y)) (CDR Y))
                                                   (T
                                                    (CONS '|:RD:| (CDR Y)))))))
                                         (*Q2F (SIMP* X))))))
                                    (RETRIEVE*BERN K))
                                   PARTIAL)
                                  |:BPREC:|)))))
         (SETQ K (PLUS K 2))
         (SETQ PARTIAL
                 (|DIVIDE:|
                  (NORMBF
                   (|ROUND:MT|
                    (|TIMES:| PARTIAL
                              (CONS '|:RD:|
                                    (CONS (TIMES (PLUS NM2 K) (PLUS NM1 K))
                                          0)))
                    |:BPREC:|))
                  (NORMBF
                   (|ROUND:MT|
                    (|TIMES:| ZSQ
                              (CONS '|:RD:|
                                    (CONS (TIMES (DIFFERENCE K 1) K) 0)))
                    |:BPREC:|))
                  RP))
         (SETQ RP (DIFFERENCE (|ORDER:| THIS) ORDA)))
        (GO WHILELABEL))
      (SETQ RESULT (|TIMES:| SIGNER RESULT))
      (COND
       ((NEQ SCALE 0)
        (PROGN
         (SETQ RESCALE BFZ*)
         (SETQ NFAC
                 (|ROUND:MT| (CONS '|:RD:| (CONS (FACTORIAL N) 0)) |:BPREC:|))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE SCALE K)) (RETURN NIL)))
           (PROGN
            (SETQ RESCALE
                    (|PLUS:| RESCALE
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:| NFAC
                                         (|TEXPT:| Z (DIFFERENCE (MINUS N) 1)))
                               |:BPREC:|))))
            (SETQ Z (PLUBF Z BFONE*)))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (SETQ RESULT (PLUBF RESULT (|TIMES:| SIGNER RESCALE))))))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(PUT 'TRIGAMMA*HALVES 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIGAMMA*HALVES) 'OPFN) 
(PUT 'TRIGAMMA*HALVES 'DEFINED-ON-LINE '765) 
(PUT 'TRIGAMMA*HALVES 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'TRIGAMMA*HALVES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIGAMMA*HALVES (X)
    (PROG (PREPRE RESULT ALGLIST*)
      (SETQ PREPRE 0)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ RESULT
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'QUOTIENT 1 2) (LIST 'EXPT 'PI 2))
                     (LIST 'TIMES 4
                           (PROG (K FORALL-RESULT)
                             (SETQ K 1)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE
                                      (AEVAL*
                                       (LIST 'ROUND
                                             (LIST 'DIFFERENCE X
                                                   (LIST 'QUOTIENT 1 2))))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (AEVAL*
                                             (LIST 'EXPT
                                                   (DIFFERENCE (TIMES 2 K) 1)
                                                   (MINUS 2)))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1))))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'ZETA*CALC 'NUMBER-OF-ARGS 1) 
(FLAG '(ZETA*CALC) 'OPFN) 
(PUT 'ZETA*CALC 'DEFINED-ON-LINE '785) 
(PUT 'ZETA*CALC 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'ZETA*CALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZETA*CALC (Z)
    (PROG (RESULT ADMISSABLE PRIMELIST PARTIALPL THIS MODIFY SPL ALGLIST*
           PREPRE J RFLAG THISPRIME NEXTI)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ J 0)
      (SETQ RFLAG 0)
      (SETQ THISPRIME 0)
      (SETQ NEXTI 0)
      (AEVAL (SHARE (LIST 'SPL)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PLUS (LIST 'PRECISION PREPRE) 3))
      (SETQ ADMISSABLE
              (AEVAL (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 2)))))
      (SETQ SPL *PRIMELIST*)
      (SETQ PRIMELIST (AEVAL (LIST 'LIST)))
      (SETQ RESULT (AEVAL 1))
      (SETQ MODIFY (AEVAL 1))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 10 K)) (RETURN NIL)))
        (PROGN
         (SETQ J (AEVAL* (CAR SPL)))
         (SETQ SPL (CDR SPL))
         (SETQ PRIMELIST (AEVAL* (LIST 'CONS J PRIMELIST)))
         (SETQ MODIFY
                 (AEVAL*
                  (LIST 'TIMES MODIFY
                        (LIST 'DIFFERENCE 1
                              (LIST 'QUOTIENT 1 (LIST 'EXPT J Z)))))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ MODIFY (AEVAL (LIST 'QUOTIENT 1 MODIFY)))
      (SETQ THIS (AEVAL (LIST 'PLUS ADMISSABLE 1)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (CDR (DIVIDE J 3)))))
        (SETQ J (AEVAL (PLUS J 2)))))
      (SETQ NEXTI
              (COND ((NOT (BOOLVALUE* (REVALX (CDR (DIVIDE (PLUS J 1) 3))))) 2)
                    (T 4)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS THIS)) (AEVAL* ADMISSABLE))
             (PROGN
              (SETQ RFLAG (AEVAL* 1))
              (SETQ PARTIALPL (AEVAL* PRIMELIST))
              (WHILE
               (AND (EVALNEQ (AEVAL* PARTIALPL) (AEVAL* (LIST 'LIST)))
                    (BOOLVALUE* RFLAG))
               (PROGN
                (SETQ THISPRIME (AEVAL* (LIST 'FIRST PARTIALPL)))
                (SETQ RFLAG (AEVAL* (CDR (DIVIDE J THISPRIME))))
                (SETQ PARTIALPL (AEVAL* (LIST 'REST PARTIALPL)))))
              (COND
               ((BOOLVALUE* RFLAG)
                (SETQ RESULT
                        (AEVAL*
                         (LIST 'PLUS RESULT
                               (SETQ THIS
                                       (AEVAL*
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT J Z)))))))))
              (SETQ J (AEVAL* (PLUS J NEXTI)))
              (SETQ NEXTI (AEVAL* (DIFFERENCE 6 NEXTI)))))
      (SETQ RESULT (AEVAL (LIST 'TIMES RESULT MODIFY)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'ZETA*POS*INTCALC 'NUMBER-OF-ARGS 1) 
(FLAG '(ZETA*POS*INTCALC) 'OPFN) 
(PUT 'ZETA*POS*INTCALC 'DEFINED-ON-LINE '821) 
(PUT 'ZETA*POS*INTCALC 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'ZETA*POS*INTCALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZETA*POS*INTCALC (M)
    (LIST 'PLUS
          (LIST 'TIMES (LIST 'EXPT (MINUS 1) M)
                (LIST 'QUOTIENT (LIST 'POLYGAMMA (LIST 'DIFFERENCE M 1) 3)
                      (LIST 'FACTORIAL (LIST 'DIFFERENCE M 1))))
          1 (LIST 'QUOTIENT 1 (LIST 'EXPT 2 M)))) 
(PUT 'ZETA*ERROR 'NUMBER-OF-ARGS 2) 
(FLAG '(ZETA*ERROR) 'OPFN) 
(PUT 'ZETA*ERROR 'DEFINED-ON-LINE '826) 
(PUT 'ZETA*ERROR 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'ZETA*ERROR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZETA*ERROR (Z TERMS)
    (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST 'PLUS TERMS 2))
          (LIST 'EXPT (LIST 'PLUS TERMS 1) Z))) 
(PUT 'ZETA*GENERAL*CALC 'NUMBER-OF-ARGS 1) 
(FLAG '(ZETA*GENERAL*CALC) 'OPFN) 
(PUT 'ZETA*GENERAL*CALC 'DEFINED-ON-LINE '830) 
(PUT 'ZETA*GENERAL*CALC 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'ZETA*GENERAL*CALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZETA*GENERAL*CALC (Z)
    (PROG (RESULT ZP ADMISSABLE Z0 PRE K)
      (SETQ PRE 0)
      (SETQ K 0)
      (SETQ PRE (AEVAL (LIST 'PRECISION 0)))
      (SETQ ADMISSABLE
              (AEVAL
               (MK*SQ
                (CONS
                 (MKROUND
                  (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS (EXPT 10 PRE) 0)) 8))
                 1))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'EXPT Z 2)) (AEVAL ADMISSABLE))
        (SETQ RESULT
                (AEVAL
                 (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                       (LIST 'QUOTIENT
                             (LIST 'TIMES (LIST 'LOG (LIST 'TIMES 2 'PI)) Z)
                             2)))))
       ((EVALLESSP PRE (AEVAL !NFPD))
        (PROG (SSTT STT)
          (SETQ SSTT
                  (PROG (K FORALL-RESULT)
                    (SETQ K 2)
                    (SETQ FORALL-RESULT 0)
                   LAB1
                    (COND
                     ((MINUSP (DIFFERENCE (DIFFERENCE PRE 1) K))
                      (RETURN FORALL-RESULT)))
                    (SETQ FORALL-RESULT
                            (AEVAL*
                             (LIST 'PLUS
                                   (AEVAL* (LIST 'EXPT K (LIST 'MINUS Z)))
                                   FORALL-RESULT)))
                    (SETQ K (PLUS2 K 1))
                    (GO LAB1)))
          (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 2)))
          (SETQ Z0 (AEVAL Z))
          (SETQ ZP (AEVAL (LIST 'EXPT PRE (LIST 'MINUS Z))))
          (SETQ STT (AEVAL (LIST 'PLUS SSTT 1)))
          (SETQ RESULT
                  (AEVAL (ZETA*GENERAL*CALC*SUB Z0 ZP ADMISSABLE PRE STT)))))
       (T
        (PROGN
         (SETQ Z0 (AEVAL Z))
         (SETQ ZP (AEVAL (LIST 'EXPT PRE (LIST 'MINUS Z))))
         (SETQ RESULT
                 (AEVAL (ZETA*GENERAL*CALC*SUB Z0 ZP ADMISSABLE PRE 'NIL))))))
      (AEVAL (LIST 'PRECISION PRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'ZETA*GENERAL*CALC*SUB 'NUMBER-OF-ARGS 5) 
(PUT 'ZETA*GENERAL*CALC*SUB 'DEFINED-ON-LINE '854) 
(PUT 'ZETA*GENERAL*CALC*SUB 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'ZETA*GENERAL*CALC*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ZETA*GENERAL*CALC*SUB (Z ZP ADMISSABLE PRE STT)
    (PROG (RESULT PRERE THIS FAC ZK1 ZK2 K)
      (SETQ K 0)
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ ZP
              (COND ((FIXP ZP) (CONS '|:RD:| (CONS ZP 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* ZP))))))
      (SETQ ADMISSABLE
              (COND ((FIXP ADMISSABLE) (CONS '|:RD:| (CONS ADMISSABLE 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* ADMISSABLE))))))
      (COND
       ((EQUAL STT NIL)
        (PROGN
         (SETQ RESULT BFONE*)
         (SETQ K 1)
         (SETQ THIS (|PLUS:| ADMISSABLE BFONE*))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (|GREATERP:| (|ABS:| THIS) ADMISSABLE)
                   (LESSP K (DIFFERENCE PRE 1))))
             (RETURN NIL)))
           (PROGN
            (SETQ K (PLUS K 1))
            (SETQ THIS (|TEXPT:ANY| (CONS '|:RD:| (CONS K 0)) (|MINUS:| Z)))
            (SETQ RESULT (PLUBF RESULT THIS)))
           (GO WHILELABEL))))
       (T
        (SETQ RESULT
                (COND ((FIXP STT) (CONS '|:RD:| (CONS STT 0)))
                      (T
                       ((LAMBDA (Y)
                          (COND
                           ((NEQ (CAR Y) '|:RD:|)
                            ((LAMBDA (U)
                               (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                             (CDR (*RN2RD Y))))
                           (T
                            (COND ((ATOM (CDR Y)) (CDR Y))
                                  (T (CONS '|:RD:| (CDR Y)))))))
                        (*Q2F (SIMP* STT))))))))
      (SETQ PRE (CONS '|:RD:| (CONS PRE 0)))
      (SETQ ZK1 (PLUBF Z BFTWO*))
      (SETQ ZK2 (PLUBF Z BFONE*))
      (SETQ RESULT
              (PLUBF RESULT
                     (NORMBF
                      (|ROUND:MT|
                       (|TIMES:| ZP
                                 (PLUBF BFHALF*
                                        (NORMBF
                                         (|DIVIDE:| PRE (DIFBF Z BFONE*)
                                                    |:BPREC:|))))
                       |:BPREC:|))))
      (SETQ FAC
              (NORMBF
               (|DIVIDE:| BFONE*
                          (NORMBF (|ROUND:MT| (|TIMES:| PRE PRE) |:BPREC:|))
                          |:BPREC:|)))
      (SETQ THIS
              (NORMBF
               (|ROUND:MT|
                (|TIMES:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                          (NORMBF (|DIVIDE:| ZP PRE |:BPREC:|)))
                |:BPREC:|)))
      (SETQ RESULT
              (PLUBF RESULT
                     (NORMBF
                      (|DIVIDE:| THIS (CONS '|:RD:| (CONS 6 0)) |:BPREC:|))))
      (SETQ K 4)
      (SETQ PRERE (PLUBF RESULT BFONE*))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (|GREATERP:| (|ABS:| (DIFBF PRERE RESULT)) ADMISSABLE))
          (RETURN NIL)))
        (PROGN
         (SETQ THIS
                 (NORMBF
                  (|DIVIDE:|
                   (NORMBF
                    (|ROUND:MT|
                     (|TIMES:| THIS
                               (NORMBF
                                (|ROUND:MT|
                                 (|TIMES:| FAC
                                           (NORMBF
                                            (|ROUND:MT| (|TIMES:| ZK1 ZK2)
                                                        |:BPREC:|)))
                                 |:BPREC:|)))
                     |:BPREC:|))
                   (CONS '|:RD:| (CONS (TIMES K (DIFFERENCE K 1)) 0))
                   |:BPREC:|)))
         (SETQ PRERE RESULT)
         (SETQ RESULT
                 (PLUBF RESULT
                        (NORMBF
                         (|ROUND:MT|
                          (|TIMES:|
                           ((LAMBDA (X)
                              (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                    (T
                                     ((LAMBDA (Y)
                                        (COND
                                         ((NEQ (CAR Y) '|:RD:|)
                                          ((LAMBDA (U)
                                             (COND ((ATOM U) U)
                                                   (T (CONS '|:RD:| U))))
                                           (CDR (*RN2RD Y))))
                                         (T
                                          (COND ((ATOM (CDR Y)) (CDR Y))
                                                (T (CONS '|:RD:| (CDR Y)))))))
                                      (*Q2F (SIMP* X))))))
                            (AEVAL* (LIST 'BERNOULLI*CALC K)))
                           THIS)
                          |:BPREC:|))))
         (SETQ ZK1 (|PLUS:| ZK1 BFTWO*))
         (SETQ ZK2 (|PLUS:| ZK2 BFTWO*))
         (SETQ K (PLUS K 2))
         NIL)
        (GO WHILELABEL))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(ARRAYFN 'ALGEBRAIC (LIST (LIST '|STIELTJES:| 5))) 
(SETEL (LIST '|STIELTJES:| 0)
       (AEVAL (LIST 'PLUS '(|:DN:| 577215664901532860606512 . -24)))) 
(SETEL (LIST '|STIELTJES:| 1)
       (AEVAL (LIST 'PLUS '(|:DN:| 72815845483676724860586 . -24)))) 
(SETEL (LIST '|STIELTJES:| 2)
       (AEVAL (LIST 'MINUS '(|:DN:| 484518159643616136422750173551 . -32)))) 
(SETEL (LIST '|STIELTJES:| 3)
       (AEVAL (LIST 'MINUS '(|:DN:| 342305736717224331350228894166 . -33)))) 
(SETEL (LIST '|STIELTJES:| 4)
       (AEVAL (LIST 'PLUS '(|:DN:| 968904193944708054646771285453 . -34)))) 
(SETEL (LIST '|STIELTJES:| 5)
       (AEVAL (LIST 'MINUS '(|:DN:| 661103181084218943121035468498 . -35)))) 
(PUT 'RAW*ZETA 'NUMBER-OF-ARGS 1) 
(FLAG '(RAW*ZETA) 'OPFN) 
(PUT 'RAW*ZETA 'DEFINED-ON-LINE '918) 
(PUT 'RAW*ZETA 'DEFINED-IN-FILE 'SPECFN/SFPSI.RED) 
(PUT 'RAW*ZETA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAW*ZETA (Z)
    (PROGN
     (SETQ Z (AEVAL (LIST 'DIFFERENCE Z 1)))
     (AEVAL
      (LIST 'PLUS (LIST 'QUOTIENT 1 Z)
            (PROG (M FORALL-RESULT)
              (SETQ M 0)
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND ((MINUSP (DIFFERENCE 5 M)) (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             (AEVAL*
                              (LIST 'TIMES (LIST '|STIELTJES:| M)
                                    (LIST 'EXPT Z M)))
                             FORALL-RESULT)))
              (SETQ M (PLUS2 M 1))
              (GO LAB1)))))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(ENDMODULE) 
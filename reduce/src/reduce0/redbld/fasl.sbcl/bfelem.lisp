(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BFELEM)) 
(EXPORTS
 (LIST '|:CBRT10| '|:CBRT2| '|:CBRT3| '|:CBRT5| '|:CBRTE| '|:CBRTPI| '|:E|
       '|:LOG10| '|:LOG2| '|:LOG3| '|:LOG5| '|:LOGPI| '|:PI| '|:SQRT10|
       '|:SQRT2| '|:SQRT3| '|:SQRT5| '|:SQRTE| '|:SQRTPI| 'ACOS* 'ASIN* 'ATAN*
       'COS* 'E* 'EXP* '|EXP:| '|GET:CONST| 'LOG* '|LOG:| 'PI* 'SIN* '|SQRT:|
       'TAN*)) 
(IMPORTS
 (LIST '*Q2F '|ABS:| 'BFLERRMSG '|BFP:| '|BFZEROP:| '|CONV:BF2I| '|CONV:MT|
       '|CUT:EP| '|CUT:MT| 'DECIMAL2INTERNAL '|DIFFERENCE:| '|DIVIDE:| '|EP:|
       '|EQUAL:| 'GEQ '|GREATERP:| '|I2BF:| 'LEQ '|LESSP:| 'LSHIFT '|MAKE:IBF|
       '|MINUS:| '|MINUSP:| 'MKSQ '|MT:| 'MULTD 'NEQ 'NUMR '|ORDER:| '|PLUS:|
       '|PRECI:| '|QUOTIENT:| '|ROUND:MT| 'SIMP '|TEXPT:| '|TEXPT:ANY|
       '|TIMES:|)) 
(FLUID '(|:PREC:| |:BPREC:| !SCLS !SCLC)) 
(GLOBAL '(BFSAVEPREC*)) 
(GLOBAL
 '(BFZ* BFHALF* BFONE* BFTWO* BFTHREE* BFFIVE* BFTEN* |:BF-0.0625| |:BF-0.25|
   |:BF0.419921875|)) 
(PUT 'ALLFIXP 'NUMBER-OF-ARGS 1) 
(PUT 'ALLFIXP 'DEFINED-ON-LINE '59) 
(PUT 'ALLFIXP 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'ALLFIXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLFIXP (L) (OR (NULL L) (AND (FIXP (CAR L)) (ALLFIXP (CDR L))))) 
(PUT '|READ:LNUM| 'NUMBER-OF-ARGS 1) 
(PUT '|READ:LNUM| 'DEFINED-ON-LINE '63) 
(PUT '|READ:LNUM| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|READ:LNUM| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |READ:LNUM| (L)
    (COND ((NOT (ALLFIXP L)) (BFLERRMSG '|READ:LNUM|))
          (T
           (PROG (MT EP K SIGN U V DCNT)
             (SETQ MT (SETQ DCNT 0))
             (SETQ U L)
             (SETQ EP (ADD1 (CAR U)))
             (SETQ SIGN (COND ((MINUSP (CADR L)) (MINUS 1)) (T 1)))
             (PROG ()
              WHILELABEL
               (COND ((NOT (SETQ U (CDR U))) (RETURN NIL)))
               (PROGN
                (SETQ K (LENGTH (EXPLODE (SETQ V (ABS (CAR U))))))
                (SETQ MT (PLUS (TIMES MT (EXPT 10 K)) V))
                (SETQ EP (DIFFERENCE EP K))
                (SETQ DCNT (PLUS DCNT K))
                (COND
                 ((AND BFSAVEPREC* (GREATERP DCNT BFSAVEPREC*))
                  (SETQ U '(NIL)))))
               (GO WHILELABEL))
             (RETURN
              ((LAMBDA (|:BPREC:|) (DECIMAL2INTERNAL (TIMES SIGN MT) EP))
               (|MSD:| MT))))))) 
(PUT 'EXP* 'NUMBER-OF-ARGS 1) 
(PUT 'EXP* 'DEFINED-ON-LINE '103) 
(PUT 'EXP* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'EXP* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXP* (U) (|EXP:| U |:BPREC:|)) 
(PUT 'LOG* 'NUMBER-OF-ARGS 1) 
(PUT 'LOG* 'DEFINED-ON-LINE '105) 
(PUT 'LOG* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'LOG* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOG* (U) (|LOG:| U |:BPREC:|)) 
(PUT 'SIN* 'NUMBER-OF-ARGS 1) 
(PUT 'SIN* 'DEFINED-ON-LINE '107) 
(PUT 'SIN* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'SIN* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIN* (U) (|SIN:| U |:BPREC:|)) 
(PUT 'COS* 'NUMBER-OF-ARGS 1) 
(PUT 'COS* 'DEFINED-ON-LINE '109) 
(PUT 'COS* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'COS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COS* (U) (|COS:| U |:BPREC:|)) 
(PUT 'TAN* 'NUMBER-OF-ARGS 1) 
(PUT 'TAN* 'DEFINED-ON-LINE '111) 
(PUT 'TAN* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'TAN* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAN* (U) (|TAN:| U |:BPREC:|)) 
(PUT 'ASIN* 'NUMBER-OF-ARGS 1) 
(PUT 'ASIN* 'DEFINED-ON-LINE '113) 
(PUT 'ASIN* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'ASIN* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASIN* (U) (|ASIN:| U |:BPREC:|)) 
(PUT 'ACOS* 'NUMBER-OF-ARGS 1) 
(PUT 'ACOS* 'DEFINED-ON-LINE '115) 
(PUT 'ACOS* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'ACOS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ACOS* (U) (|ACOS:| U |:BPREC:|)) 
(PUT 'ATAN* 'NUMBER-OF-ARGS 1) 
(PUT 'ATAN* 'DEFINED-ON-LINE '117) 
(PUT 'ATAN* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'ATAN* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ATAN* (U) (|ATAN:| U |:BPREC:|)) 
(PUT 'SQRT* 'NUMBER-OF-ARGS 1) 
(PUT 'SQRT* 'DEFINED-ON-LINE '119) 
(PUT 'SQRT* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'SQRT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRT* (U) (|SQRT:| U |:BPREC:|)) 
(PUT 'PI* 'NUMBER-OF-ARGS 0) 
(PUT 'PI* 'DEFINED-ON-LINE '121) 
(PUT 'PI* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'PI* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PI* NIL
    (COND ((GREATERP |:PREC:| 1000) (|:BIGPI| |:BPREC:|))
          (T (|:PI| |:BPREC:|)))) 
(PUT 'E* 'NUMBER-OF-ARGS 0) 
(PUT 'E* 'DEFINED-ON-LINE '124) 
(PUT 'E* 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT 'E* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE E* NIL (|:E| |:BPREC:|)) 
(PUT '|:PI| 'NUMBER-OF-ARGS 1) 
(PUT '|:PI| 'DEFINED-ON-LINE '133) 
(PUT '|:PI| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:PI| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:PI| (K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|:PI|))
          (T
           (PROG (K3 S SS M N X TEST U)
             (SETQ K3 0)
             (SETQ S 0)
             (SETQ SS 0)
             (SETQ M 0)
             (SETQ N 0)
             (SETQ X 0)
             (SETQ TEST 0)
             (SETQ U (|GET:CONST| '|:PI| K))
             (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
             (SETQ SS (SETQ N (QUOTIENT (EXPT 2 (SETQ K3 (PLUS K 3))) 5)))
             (SETQ X (MINUS (EXPT 5 2)))
             (SETQ M 1)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NEQ N 0)) (RETURN NIL)))
               (PROGN
                (SETQ N (QUOTIENT N X))
                (SETQ SS (PLUS SS (QUOTIENT N (SETQ M (PLUS M 2))))))
               (GO WHILELABEL))
             (SETQ S (SETQ N (QUOTIENT (EXPT 2 K3) 239)))
             (SETQ X (MINUS (EXPT 239 2)))
             (SETQ M 1)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NEQ N 0)) (RETURN NIL)))
               (PROGN
                (SETQ N (QUOTIENT N X))
                (SETQ S (PLUS S (QUOTIENT N (SETQ M (PLUS M 2))))))
               (GO WHILELABEL))
             (SETQ U
                     (|ROUND:MT|
                      (CONS '|:RD:|
                            (CONS
                             (SETQ TEST (DIFFERENCE (TIMES 16 SS) (TIMES 4 S)))
                             (MINUS K3)))
                      K))
             (|SAVE:CONST| '|:PI| U)
             (RETURN U))))) 
(PUT '|:BIGPI| 'NUMBER-OF-ARGS 1) 
(PUT '|:BIGPI| 'DEFINED-ON-LINE '165) 
(PUT '|:BIGPI| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:BIGPI| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:BIGPI| (K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|:BIGPI|))
          (T
           (PROG (K7 N DCUT HALF X Y U V)
             (SETQ K7 0)
             (SETQ N 0)
             (SETQ U (|GET:CONST| '|:PI| K))
             (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
             (SETQ K7 (PLUS K 7))
             (SETQ HALF BFHALF*)
             (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS K7))))
             (SETQ N 1)
             (SETQ X BFONE*)
             (SETQ Y (|DIVIDE:| X (|:SQRT2| K7) K7))
             (SETQ U |:BF-0.25|)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (|GREATERP:| (|ABS:| (|DIFFERENCE:| X Y)) DCUT))
                 (RETURN NIL)))
               (PROGN
                (SETQ V X)
                (SETQ X (|TIMES:| (|PLUS:| X Y) HALF))
                (SETQ Y (|SQRT:| (|CUT:EP| (|TIMES:| Y V) (MINUS K7)) K7))
                (SETQ V (|DIFFERENCE:| X V))
                (SETQ V (|TIMES:| (|TIMES:| V V) (CONS '|:RD:| (CONS N 0))))
                (SETQ U (|DIFFERENCE:| U (|CUT:EP| V (MINUS K7))))
                (SETQ N (TIMES 2 N)))
               (GO WHILELABEL))
             (SETQ V (|CUT:MT| (|TEXPT:| (|PLUS:| X Y) 2) K7))
             (SETQ U (|DIVIDE:| V (|TIMES:| (CONS '|:RD:| (CONS 4 0)) U) K))
             (|SAVE:CONST| '|:PI| U)
             (RETURN U))))) 
(PUT '|:E| 'NUMBER-OF-ARGS 1) 
(PUT '|:E| 'DEFINED-ON-LINE '198) 
(PUT '|:E| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:E| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:E| (K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|:E|))
          (T
           (PROG (K7 ANS M N U)
             (SETQ K7 0)
             (SETQ ANS 0)
             (SETQ M 0)
             (SETQ N 0)
             (SETQ U (|GET:CONST| '|:E| K))
             (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
             (SETQ K7 (PLUS K 7))
             (SETQ M 1)
             (SETQ N (ASHIFT 1 K7))
             (SETQ ANS 0)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NEQ N 0)) (RETURN NIL)))
               (SETQ ANS (PLUS ANS (SETQ N (QUOTIENT N (SETQ M (PLUS M 1))))))
               (GO WHILELABEL))
             (SETQ ANS (PLUS ANS (ASHIFT 1 (PLUS K7 1))))
             (SETQ U (|ROUND:MT| (CONS '|:RD:| (CONS ANS (MINUS K7))) K))
             (|SAVE:CONST| '|:E2| U)
             (RETURN U))))) 
(PUT '|:E0625| 'NUMBER-OF-ARGS 1) 
(PUT '|:E0625| 'DEFINED-ON-LINE '219) 
(PUT '|:E0625| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:E0625| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:E0625| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:E0625| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|EXP:| |:BF-0.0625| K))
      (|SAVE:CONST| '|:E0625| U)
      (RETURN U))) 
(PUT '|:LOG2| 'NUMBER-OF-ARGS 1) 
(PUT '|:LOG2| 'DEFINED-ON-LINE '232) 
(PUT '|:LOG2| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:LOG2| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:LOG2| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:LOG2| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|LOG:| BFTWO* K))
      (|SAVE:CONST| '|:LOG2| U)
      (RETURN U))) 
(PUT '|:LOG3| 'NUMBER-OF-ARGS 1) 
(PUT '|:LOG3| 'DEFINED-ON-LINE '244) 
(PUT '|:LOG3| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:LOG3| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:LOG3| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:LOG3| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|LOG:| BFTHREE* K))
      (|SAVE:CONST| '|:LOG3| U)
      (RETURN U))) 
(PUT '|:LOG5| 'NUMBER-OF-ARGS 1) 
(PUT '|:LOG5| 'DEFINED-ON-LINE '256) 
(PUT '|:LOG5| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:LOG5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:LOG5| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:LOG5| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|LOG:| BFFIVE* K))
      (|SAVE:CONST| '|:LOG5| U)
      (RETURN U))) 
(PUT '|:LOG10| 'NUMBER-OF-ARGS 1) 
(PUT '|:LOG10| 'DEFINED-ON-LINE '268) 
(PUT '|:LOG10| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:LOG10| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:LOG10| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:LOG10| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|LOG:| BFTEN* K))
      (|SAVE:CONST| '|:LOG10| U)
      (RETURN U))) 
(PUT '|:LOGPI| 'NUMBER-OF-ARGS 1) 
(PUT '|:LOGPI| 'DEFINED-ON-LINE '280) 
(PUT '|:LOGPI| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:LOGPI| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:LOGPI| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:LOGPI| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|LOG:| (|:PI| (PLUS K 2)) K))
      (|SAVE:CONST| '|:LOGPI| U)
      (RETURN U))) 
(PUT '|:SQRT2| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRT2| 'DEFINED-ON-LINE '292) 
(PUT '|:SQRT2| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRT2| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRT2| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRT2| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| BFTWO* K))
      (|SAVE:CONST| '|:SQRT2| U)
      (RETURN U))) 
(PUT '|:SQRT3| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRT3| 'DEFINED-ON-LINE '304) 
(PUT '|:SQRT3| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRT3| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRT3| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRT3| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| BFTHREE* K))
      (|SAVE:CONST| '|:SQRT3| U)
      (RETURN U))) 
(PUT '|:SQRT5| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRT5| 'DEFINED-ON-LINE '316) 
(PUT '|:SQRT5| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRT5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRT5| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRT5| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| BFFIVE* K))
      (|SAVE:CONST| '|:SQRT5| U)
      (RETURN U))) 
(PUT '|:SQRT10| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRT10| 'DEFINED-ON-LINE '328) 
(PUT '|:SQRT10| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRT10| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRT10| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRT10| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| BFTEN* K))
      (|SAVE:CONST| '|:SQRT10| U)
      (RETURN U))) 
(PUT '|:SQRTPI| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRTPI| 'DEFINED-ON-LINE '340) 
(PUT '|:SQRTPI| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRTPI| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRTPI| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRTPI| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| (|:PI| (PLUS K 2)) K))
      (|SAVE:CONST| '|:SQRTPI| U)
      (RETURN U))) 
(PUT '|:SQRTE| 'NUMBER-OF-ARGS 1) 
(PUT '|:SQRTE| 'DEFINED-ON-LINE '352) 
(PUT '|:SQRTE| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:SQRTE| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:SQRTE| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:SQRTE| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|SQRT:| (|:E| (PLUS K 2)) K))
      (|SAVE:CONST| '|:SQRTE| U)
      (RETURN U))) 
(PUT '|:CBRT2| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRT2| 'DEFINED-ON-LINE '364) 
(PUT '|:CBRT2| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRT2| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRT2| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRT2| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| BFTWO* K))
      (|SAVE:CONST| '|:CBRT2| U)
      (RETURN U))) 
(PUT '|:CBRT3| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRT3| 'DEFINED-ON-LINE '376) 
(PUT '|:CBRT3| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRT3| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRT3| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRT3| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| BFTHREE* K))
      (|SAVE:CONST| '|:CBRT3| U)
      (RETURN U))) 
(PUT '|:CBRT5| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRT5| 'DEFINED-ON-LINE '388) 
(PUT '|:CBRT5| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRT5| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRT5| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRT5| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| BFFIVE* K))
      (|SAVE:CONST| '|:CBRT5| U)
      (RETURN U))) 
(PUT '|:CBRT10| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRT10| 'DEFINED-ON-LINE '400) 
(PUT '|:CBRT10| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRT10| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRT10| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRT10| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| BFTEN* K))
      (|SAVE:CONST| '|:CBRT10| U)
      (RETURN U))) 
(PUT '|:CBRTPI| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRTPI| 'DEFINED-ON-LINE '412) 
(PUT '|:CBRTPI| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRTPI| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRTPI| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRTPI| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| (|:PI| (PLUS K 2)) K))
      (|SAVE:CONST| '|:CBRTPI| U)
      (RETURN U))) 
(PUT '|:CBRTE| 'NUMBER-OF-ARGS 1) 
(PUT '|:CBRTE| 'DEFINED-ON-LINE '424) 
(PUT '|:CBRTE| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|:CBRTE| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:CBRTE| (K)
    (PROG (U)
      (SETQ U (|GET:CONST| '|:CBRTE| K))
      (COND ((NEQ U 'NOT_FOUND) (RETURN U)))
      (SETQ U (|CBRT:| (|:E| (PLUS K 2)) K))
      (|SAVE:CONST| '|:CBRTE| U)
      (RETURN U))) 
(PUT '|GET:CONST| 'NUMBER-OF-ARGS 2) 
(PUT '|GET:CONST| 'DEFINED-ON-LINE '442) 
(PUT '|GET:CONST| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|GET:CONST| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |GET:CONST| (CNST K)
    (COND
     ((AND (ATOM CNST) (FIXP K) (GREATERP K 0))
      (PROG (U)
        (SETQ U (GET CNST '|SAVE:C|))
        (COND ((OR (NULL U) (LESSP (CAR U) K)) (RETURN 'NOT_FOUND))
              ((EQUAL (CAR U) K) (RETURN (CDR U)))
              (T (RETURN (|ROUND:MT| (CDR U) K))))))
     (T (BFLERRMSG '|GET:CONST|)))) 
(PUT '|SAVE:CONST| 'NUMBER-OF-ARGS 2) 
(PUT '|SAVE:CONST| 'DEFINED-ON-LINE '458) 
(PUT '|SAVE:CONST| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|SAVE:CONST| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SAVE:CONST| (CNST NMBR)
    (COND
     ((AND (ATOM CNST) (AND (EQCAR NMBR '|:RD:|) (NOT (ATOM (CDR NMBR)))))
      (PUT CNST '|SAVE:C| (CONS (|MSD:| (ABS (CADR NMBR))) NMBR)))
     (T (BFLERRMSG '|SAVE:CONST|)))) 
(PUT '|SET:CONST| 'NUMBER-OF-ARGS 2) 
(PUT '|SET:CONST| 'DEFINED-ON-LINE '467) 
(PUT '|SET:CONST| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|SET:CONST| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SET:CONST| (CNST L) (|SAVE:CONST| CNST (|READ:LNUM| L))) 
(|SET:CONST| '|:PI|
             '(0 3141 59265 35897 93238 46264 33832 79502 88419 71693 99375
               105820 9749 44592 30781 64062 86208 99862 80348 25342 11706
               79821 48086 51328 23066 47093 84460 95505 82231 72535 94081
               28481 1174 5028410 2701 93852 11055 59644 62294 89549 30381
               96442 88109 8)) 
(|SET:CONST| '|:E|
             '(0 2718 28182 84590 45235 36028 74713 52662 49775 72470 93699
               95957 49669 67627 72407 66303 53547 59457 13821 78525 16642
               74274 66391 93200 30599 21817 41359 66290 43572 90033 42952
               60595 63073 81323 28627 943490 7632 33829 88075 31952 510190
               1157 38341 9)) 
(|SET:CONST| '|:E0625|
             '(0 1064 49445 89178 59429 563390 5946 42889 673100 7254 43649
               35330 151930 7510 63556 39368 2816600 633 42934 35506 87662
               43755 1)) 
(|SET:CONST| '|:LOG2|
             '(-1 6931 47180 55994 53094 17232 12145 81765 68075 50013 43602
               55254 1206 800094 93393 62196 96947 15605 86332 69964 18687
               54200 2)) 
(|SET:CONST| '|:LOG3|
             '(0 1098 61228 866810 9691 39524 52369 22525 70464 74905 57822
               74945 17346 94333 63749 42932 18608 96687 36157 54813 73208
               87879 7)) 
(|SET:CONST| '|:LOG5|
             '(0 1609 43791 2434100 374 60075 93332 26187 63952 56013 54268
               51772 19126 47891 47417 898770 7657 764630 1338 78093 179610
               7999 7)) 
(|SET:CONST| '|:LOG10|
             '(0 2302 58509 29940 456840 1799 14546 84364 20760 11014 88628
               77297 60333 27900 96757 26096 77352 48023 599720 5089 59829
               83419 7)) 
(|SET:CONST| '|:LOGPI|
             '(0 1144 72988 5849400 174 14342 73513 53058 71164 72948 12915
               31157 15136 23071 47213 77698 848260 7978 36232 70275 48970
               77020 1)) 
(|SET:CONST| '|:SQRT2|
             '(0 1414 21356 23730 95048 80168 872420 96980 7856 96718 75376
               94807 31766 79737 99073 24784 621070 38850 3875 34327 64157
               27350 1)) 
(|SET:CONST| '|:SQRT3|
             '(0 17320 5080 75688 77293 52744 634150 5872 36694 28052 53810
               38062 805580 6979 45193 301690 88000 3708 11461 86757 24857
               56756 3)) 
(|SET:CONST| '|:SQRT5|
             '(0 22360 6797 74997 89696 40917 36687 31276 235440 6183 59611
               52572 42708 97245 4105 209256 37804 89941 441440 8378 78227
               49695 1)) 
(|SET:CONST| '|:SQRT10|
             '(0 3162 277660 1683 79331 99889 35444 32718 53371 95551 39325
               21682 685750 4852 79259 44386 39238 22134 424810 8379 30029
               51873 47)) 
(|SET:CONST| '|:SQRTPI|
             '(0 1772 453850 9055 16027 29816 74833 41145 18279 75494 56122
               38712 821380 7789 85291 12845 91032 18137 49506 56738 54466
               54162 3)) 
(|SET:CONST| '|:SQRTE|
             '(0 1648 721270 7001 28146 8486 507878 14163 57165 3776100 710
               14801 15750 79311 64066 10211 94215 60863 27765 20056 36664
               30028 7)) 
(|SET:CONST| '|:CBRT2|
             '(0 1259 92104 98948 73164 7672 106072 78228 350570 2514 64701
               5079800 819 75112 15529 96765 13959 48372 93965 62436 25509
               41543 1)) 
(|SET:CONST| '|:CBRT3|
             '(0 1442 249570 30740 8382 32163 83107 80109 58839 18692 53499
               35057 75464 16194 54168 75968 29997 33985 47554 79705 64525
               66868 4)) 
(|SET:CONST| '|:CBRT5|
             '(0 1709 97594 66766 96989 35310 88725 43860 10986 80551 105430
               5492 43828 61707 44429 592050 4173 21625 71870 10020 18900
               220450)) 
(|SET:CONST| '|:CBRT10|
             '(0 2154 4346900 318 83721 75929 35665 19350 49525 93449 42192
               10858 24892 35506 34641 11066 48340 80018 544150 3543 24327
               61012 6)) 
(|SET:CONST| '|:CBRTPI|
             '(0 1464 59188 75615 232630 2014 25272 63790 39173 85968 55627
               93717 43572 55937 13839 36497 98286 26614 56820 67820 353820
               89750)) 
(|SET:CONST| '|:CBRTE|
             '(0 1395 61242 50860 89528 62812 531960 2586 83759 79065 15199
               40698 26175 167060 3173 90156 45951 84696 97888 17295 83022
               41352 1)) 
(PUT '|SQRT:| 'NUMBER-OF-ARGS 2) 
(PUT '|SQRT:| 'DEFINED-ON-LINE '571) 
(PUT '|SQRT:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|SQRT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SQRT:| (X K)
    (COND ((OR (|MINUSP:| X) (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|SQRT:|))
          ((|BFZEROP:| X) BFZ*)
          (T
           (PROG (K7 NCUT NFIG DCUT HALF DY Y Y0 U)
             (SETQ K7 0)
             (SETQ NCUT 0)
             (SETQ NFIG 0)
             (SETQ K7 (PLUS K 7))
             (SETQ NCUT (DIFFERENCE K7 (QUOTIENT (PLUS (|ORDER:| X) 1) 2)))
             (SETQ HALF BFHALF*)
             (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS NCUT))))
             (SETQ DY (CONS '|:RD:| (CONS 4 (MINUS NCUT))))
             (SETQ Y0 (|CONV:MT| X 2))
             (COND
              ((EQUAL (REMAINDER (CDDR Y0) 2) 0)
               (SETQ Y0
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 2 (TIMES 3 (CADR Y0))) 5)
                                   (QUOTIENT (CDDR Y0) 2)))))
              (T
               (SETQ Y0
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 9 (TIMES 5 (CADR Y0))) 10)
                                   (QUOTIENT (DIFFERENCE (CDDR Y0) 1) 2))))))
             (SETQ NFIG 1)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (OR (LESSP NFIG K7) (|GREATERP:| (|ABS:| DY) DCUT)))
                 (RETURN NIL)))
               (PROGN
                (COND
                 ((GREATERP (SETQ NFIG (TIMES 2 NFIG)) K7) (SETQ NFIG K7)))
                (SETQ U (|DIVIDE:| X Y0 NFIG))
                (SETQ Y (|TIMES:| (|PLUS:| Y0 U) HALF))
                (SETQ DY (|DIFFERENCE:| Y Y0))
                (SETQ Y0 Y))
               (GO WHILELABEL))
             (RETURN (|ROUND:MT| Y K)))))) 
(PUT '|CBRT:| 'NUMBER-OF-ARGS 2) 
(PUT '|CBRT:| 'DEFINED-ON-LINE '601) 
(PUT '|CBRT:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|CBRT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |CBRT:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|CBRT:|))
          ((|BFZEROP:| X) BFZ*)
          ((|MINUSP:| X) (|MINUS:| (|CBRT:| (|MINUS:| X) K)))
          (T
           (PROG (K7 NCUT NFIG J DCUT THRE DY Y U)
             (SETQ K7 0)
             (SETQ NCUT 0)
             (SETQ NFIG 0)
             (SETQ J 0)
             (SETQ K7 (PLUS K 7))
             (SETQ NCUT (DIFFERENCE K7 (QUOTIENT (PLUS (|ORDER:| X) 2) 3)))
             (SETQ THRE BFTHREE*)
             (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS NCUT))))
             (SETQ DY (CONS '|:RD:| (CONS 4 (MINUS NCUT))))
             (SETQ Y (|CONV:MT| X 3))
             (COND
              ((EQUAL (SETQ J (REMAINDER (CDDR Y) 3)) 0)
               (SETQ Y
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 12 (CADR Y)) 10)
                                   (QUOTIENT (CDDR Y) 3)))))
              ((OR (EQUAL J 1) (EQUAL J (MINUS 2)))
               (SETQ Y
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 17 (TIMES 4 (CADR Y))) 16)
                                   (QUOTIENT (DIFFERENCE (CDDR Y) 1) 3)))))
              (T
               (SETQ Y
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 15 (TIMES 4 (CADR Y))) 12)
                                   (QUOTIENT (DIFFERENCE (CDDR Y) 2) 3))))))
             (SETQ NFIG 1)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (OR (LESSP NFIG K7) (|GREATERP:| (|ABS:| DY) DCUT)))
                 (RETURN NIL)))
               (PROGN
                (COND
                 ((GREATERP (SETQ NFIG (TIMES 2 NFIG)) K7) (SETQ NFIG K7)))
                (SETQ U (|CUT:MT| (|TIMES:| Y Y) NFIG))
                (SETQ U (|DIVIDE:| X U NFIG))
                (SETQ J
                        (PLUS (|ORDER:| (SETQ U (|DIFFERENCE:| U Y)))
                              (DIFFERENCE NCUT K7)))
                (SETQ DY (|DIVIDE:| U THRE (MAX 1 (PLUS NFIG J))))
                (SETQ Y (|PLUS:| Y DY)))
               (GO WHILELABEL))
             (RETURN (|ROUND:MT| Y K)))))) 
(PUT '|EXP:| 'NUMBER-OF-ARGS 2) 
(PUT '|EXP:| 'DEFINED-ON-LINE '634) 
(PUT '|EXP:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|EXP:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |EXP:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|EXP:|))
          ((|BFZEROP:| X) BFONE*)
          (T
           (PROG (K7 M Q R Y YQ YR)
             (SETQ K7 0)
             (SETQ M 0)
             (SETQ Q
                     (CONS '|:RD:|
                           (CONS (SETQ M (|CONV:BF2I| (SETQ Y (|ABS:| X))))
                                 0)))
             (SETQ R (|DIFFERENCE:| Y Q))
             (SETQ K7 (PLUS K (|MSD:| M) 7))
             (SETQ R (|DIFFERENCE:| Y Q))
             (COND ((|BFZEROP:| Q) (SETQ YQ BFONE*))
                   (T
                    ((LAMBDA (|:BPREC:|) (SETQ YQ (|TEXPT:| (|:E| K7) M)))
                     K7)))
             (COND ((|BFZEROP:| R) (SETQ YR BFONE*))
                   (T
                    (PROG (J N DCUT FCTRIAL RI TM)
                      (SETQ J 0)
                      (SETQ N 0)
                      (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS K7))))
                      (SETQ YR (SETQ RI (SETQ TM BFONE*)))
                      (SETQ M 1)
                      (SETQ J 0)
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT (|GREATERP:| TM DCUT)) (RETURN NIL)))
                        (PROGN
                         (SETQ FCTRIAL
                                 (CONS '|:RD:|
                                       (CONS
                                        (SETQ M (TIMES M (SETQ J (PLUS J 1))))
                                        0)))
                         (SETQ RI (|CUT:EP| (|TIMES:| RI R) (MINUS K7)))
                         (SETQ N
                                 (MAX 1
                                      (PLUS (DIFFERENCE K7 (|ORDER:| FCTRIAL))
                                            (|ORDER:| RI))))
                         (SETQ TM (|DIVIDE:| RI FCTRIAL N))
                         (SETQ YR (|PLUS:| YR TM))
                         (COND
                          ((EQUAL (REMAINDER J 10) 0)
                           (SETQ YR (|CUT:EP| YR (MINUS K7))))))
                        (GO WHILELABEL)))))
             (SETQ Y (|CUT:MT| (|TIMES:| YQ YR) (PLUS K 1)))
             (RETURN
              (COND ((|MINUSP:| X) (|DIVIDE:| BFONE* Y K))
                    (T (|ROUND:MT| Y K)))))))) 
(PUT '|LOG:| 'NUMBER-OF-ARGS 2) 
(PUT '|LOG:| 'DEFINED-ON-LINE '670) 
(PUT '|LOG:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|LOG:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |LOG:| (X K)
    (COND
     ((OR (|MINUSP:| X) (|BFZEROP:| X) (NOT (FIXP K)) (LEQ K 0))
      (BFLERRMSG '|LOG:|))
     ((|EQUAL:| X BFONE*) BFZ*)
     (T
      (PROG (K7 M EEE ES SIGN L Y Z)
        (SETQ K7 0)
        (SETQ M 0)
        (SETQ K7 (PLUS K 7))
        (SETQ EEE (|:E| K7))
        (SETQ ES (|:E0625| K7))
        (COND ((|GREATERP:| X BFONE*) (PROGN (SETQ SIGN BFONE*) (SETQ Y X)))
              (T
               (PROGN
                (SETQ SIGN (|MINUS:| BFONE*))
                (SETQ Y (|DIVIDE:| BFONE* X K7)))))
        (COND ((|LESSP:| Y EEE) (PROGN (SETQ M 0) (SETQ Z Y)))
              (T
               (PROGN
                (COND
                 ((EQUAL (SETQ M (QUOTIENT (TIMES (|ORDER:| Y) 69) 100)) 0)
                  (SETQ Z Y))
                 (T
                  ((LAMBDA (|:BPREC:|)
                     (SETQ Z (|DIVIDE:| Y (|TEXPT:| EEE M) K7)))
                   K7)))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (|GREATERP:| Z EEE)) (RETURN NIL)))
                  (PROGN (SETQ M (PLUS M 1)) (SETQ Z (|DIVIDE:| Z EEE K7)))
                  (GO WHILELABEL)))))
        (SETQ L (CONS '|:RD:| (CONS M 0)))
        (SETQ Y |:BF-0.0625|)
        (PROG ()
         WHILELABEL
          (COND ((NOT (|GREATERP:| Z ES)) (RETURN NIL)))
          (PROGN (SETQ L (|PLUS:| L Y)) (SETQ Z (|DIVIDE:| Z ES K7)))
          (GO WHILELABEL))
        (SETQ Z (|DIFFERENCE:| Z BFONE*))
        (PROG (N DCUT TM ZI)
          (SETQ N 0)
          (SETQ Y (SETQ TM (SETQ ZI Z)))
          (SETQ Z (|MINUS:| Z))
          (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS K7))))
          (SETQ M 1)
          (PROG ()
           WHILELABEL
            (COND ((NOT (|GREATERP:| (|ABS:| TM) DCUT)) (RETURN NIL)))
            (PROGN
             (SETQ ZI (|CUT:EP| (|TIMES:| ZI Z) (MINUS K7)))
             (SETQ N (MAX 1 (PLUS K7 (|ORDER:| ZI))))
             (SETQ TM
                     (|DIVIDE:| ZI (CONS '|:RD:| (CONS (SETQ M (PLUS M 1)) 0))
                                N))
             (SETQ Y (|PLUS:| Y TM))
             (COND
              ((ZEROP (REMAINDER M 10)) (SETQ Y (|CUT:EP| Y (MINUS K7))))))
            (GO WHILELABEL)))
        (SETQ Y (|PLUS:| Y L))
        (RETURN (|ROUND:MT| (|TIMES:| SIGN Y) K)))))) 
(PUT '|SIN:| 'NUMBER-OF-ARGS 2) 
(PUT '|SIN:| 'DEFINED-ON-LINE '715) 
(PUT '|SIN:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|SIN:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SIN:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|SIN:|))
          ((|BFZEROP:| X) BFZ*)
          ((|MINUSP:| X) (|MINUS:| (|SIN:| (|MINUS:| X) K)))
          (T
           (PROG (K7 M PI4 SIGN Q R Y !SCLS)
             (SETQ K7 0)
             (SETQ M 0)
             (SETQ K7 (PLUS K 7))
             (SETQ M (|MSD:| (ABS (CADR X))))
             (SETQ PI4 (|TIMES:| (|:PI| (PLUS K7 M)) |:BF-0.25|))
             (COND ((|LESSP:| X PI4) (PROGN (SETQ M 0) (SETQ R X)))
                   (T
                    (PROGN
                     (SETQ M (|CONV:BF2I| (SETQ Q (|QUOTIENT:| X PI4))))
                     (SETQ R (|DIFFERENCE:| X (|TIMES:| Q PI4))))))
             (SETQ SIGN BFONE*)
             (COND ((GEQ M 8) (SETQ M (REMAINDER M 8))))
             (COND
              ((GEQ M 4)
               (PROGN (SETQ SIGN (|MINUS:| SIGN)) (SETQ M (DIFFERENCE M 4)))))
             (COND ((EQUAL M 0) (PROGN (SETQ !SCLS X) (GO SN)))
                   ((ONEP M) (GO M1)) ((EQUAL M 2) (GO M2)) (T (GO M3)))
            M1
             (SETQ R (|CUT:MT| (|DIFFERENCE:| PI4 R) K7))
             (RETURN (|TIMES:| SIGN (|COS:| R K)))
            M2
             (SETQ R (|CUT:MT| R K7))
             (RETURN (|TIMES:| SIGN (|COS:| R K)))
            M3
             (SETQ R (|CUT:MT| (|DIFFERENCE:| PI4 R) K7))
             (SETQ !SCLS X)
            SN
             (SETQ X (COND (!SCLC !SCLC) (T !SCLS)))
             (COND
              ((AND X
                    (|LESSP:| R
                              (|TIMES:| X
                                        (CONS '|:RD:|
                                              (CONS 1 (DIFFERENCE 3 K))))))
               (RETURN BFZ*))
              (T
               (PROG (J N NCUT DCUT FCTRIAL RI TM)
                 (SETQ J 0)
                 (SETQ N 0)
                 (SETQ NCUT 0)
                 (SETQ NCUT (DIFFERENCE K7 (MIN 0 (PLUS (|ORDER:| R) 1))))
                 (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS NCUT))))
                 (SETQ Y (SETQ RI (SETQ TM R)))
                 (SETQ R (|MINUS:| (|CUT:EP| (|TIMES:| R R) (MINUS NCUT))))
                 (SETQ M (SETQ J 1))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (|GREATERP:| (|ABS:| TM) DCUT)) (RETURN NIL)))
                   (PROGN
                    (SETQ J (PLUS J 2))
                    (SETQ FCTRIAL
                            (CONS '|:RD:|
                                  (CONS (SETQ M (TIMES M J (DIFFERENCE J 1)))
                                        0)))
                    (SETQ RI (|CUT:EP| (|TIMES:| RI R) (MINUS NCUT)))
                    (SETQ N
                            (MAX 1
                                 (PLUS (DIFFERENCE K7 (|ORDER:| FCTRIAL))
                                       (|ORDER:| RI))))
                    (SETQ TM (|DIVIDE:| RI FCTRIAL N))
                    (SETQ Y (|PLUS:| Y TM))
                    (COND
                     ((ZEROP (REMAINDER J 20))
                      (SETQ Y (|CUT:EP| Y (MINUS NCUT))))))
                   (GO WHILELABEL)))))
             (RETURN (|ROUND:MT| (|TIMES:| SIGN Y) K)))))) 
(PUT '|COS:| 'NUMBER-OF-ARGS 2) 
(PUT '|COS:| 'DEFINED-ON-LINE '765) 
(PUT '|COS:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|COS:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |COS:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|COS:|))
          ((|BFZEROP:| X) BFONE*) ((|MINUSP:| X) (|COS:| (|MINUS:| X) K))
          (T
           (PROG (K7 M PI4 SIGN Q R Y !SCLC)
             (SETQ K7 0)
             (SETQ M 0)
             (SETQ K7 (PLUS K 7))
             (SETQ M (|MSD:| (ABS (CADR X))))
             (SETQ PI4 (|TIMES:| (|:PI| (PLUS K7 M)) |:BF-0.25|))
             (COND ((|LESSP:| X PI4) (PROGN (SETQ M 0) (SETQ R X)))
                   (T
                    (PROGN
                     (SETQ M (|CONV:BF2I| (SETQ Q (|QUOTIENT:| X PI4))))
                     (SETQ R (|DIFFERENCE:| X (|TIMES:| Q PI4))))))
             (SETQ SIGN BFONE*)
             (COND ((GEQ M 8) (SETQ M (REMAINDER M 8))))
             (COND
              ((GEQ M 4)
               (PROGN (SETQ SIGN (|MINUS:| SIGN)) (SETQ M (DIFFERENCE M 4)))))
             (COND ((GEQ M 2) (SETQ SIGN (|MINUS:| SIGN))))
             (COND ((EQUAL M 0) (GO CS)) ((EQUAL M 1) (GO M1))
                   ((EQUAL M 2) (GO M2)) (T (GO M3)))
            M1
             (SETQ R (|CUT:MT| (|DIFFERENCE:| PI4 R) K7))
             (SETQ !SCLC X)
             (RETURN (|TIMES:| SIGN (|SIN:| R K)))
            M2
             (SETQ R (|CUT:MT| R K7))
             (SETQ !SCLC X)
             (RETURN (|TIMES:| SIGN (|SIN:| R K)))
            M3
             (SETQ R (|CUT:MT| (|DIFFERENCE:| PI4 R) K7))
            CS
             (PROG (J N DCUT FCTRIAL RI TM)
               (SETQ J 0)
               (SETQ N 0)
               (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS K7))))
               (SETQ Y (SETQ RI (SETQ TM BFONE*)))
               (SETQ R (|MINUS:| (|CUT:EP| (|TIMES:| R R) (MINUS K7))))
               (SETQ M 1)
               (SETQ J 0)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (|GREATERP:| (|ABS:| TM) DCUT)) (RETURN NIL)))
                 (PROGN
                  (SETQ J (PLUS J 2))
                  (SETQ FCTRIAL
                          (CONS '|:RD:|
                                (CONS (SETQ M (TIMES M J (DIFFERENCE J 1)))
                                      0)))
                  (SETQ RI (|CUT:EP| (|TIMES:| RI R) (MINUS K7)))
                  (SETQ N
                          (MAX 1
                               (PLUS (DIFFERENCE K7 (|ORDER:| FCTRIAL))
                                     (|ORDER:| RI))))
                  (SETQ TM (|DIVIDE:| RI FCTRIAL N))
                  (SETQ Y (|PLUS:| Y TM))
                  (COND
                   ((ZEROP (REMAINDER J 20))
                    (SETQ Y (|CUT:EP| Y (MINUS K7))))))
                 (GO WHILELABEL)))
             (RETURN (|ROUND:MT| (|TIMES:| SIGN Y) K)))))) 
(PUT '|TAN:| 'NUMBER-OF-ARGS 2) 
(PUT '|TAN:| 'DEFINED-ON-LINE '813) 
(PUT '|TAN:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|TAN:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |TAN:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|TAN:|))
          ((|BFZEROP:| X) BFZ*)
          ((|MINUSP:| X) (|MINUS:| (|TAN:| (|MINUS:| X) K)))
          (T
           (PROG (K7 M PI4 SIGN Q R)
             (SETQ K7 0)
             (SETQ M 0)
             (SETQ K7 (PLUS K 7))
             (SETQ M (|MSD:| (ABS (CADR X))))
             (SETQ PI4 (|TIMES:| (|:PI| (PLUS K7 M)) |:BF-0.25|))
             (COND ((|LESSP:| X PI4) (PROGN (SETQ M 0) (SETQ R X)))
                   (T
                    (PROGN
                     (SETQ M (|CONV:BF2I| (SETQ Q (|QUOTIENT:| X PI4))))
                     (SETQ R (|DIFFERENCE:| X (|TIMES:| Q PI4))))))
             (COND ((GEQ M 4) (SETQ M (REMAINDER M 4))))
             (COND ((GEQ M 2) (SETQ SIGN (|MINUS:| BFONE*)))
                   (T (SETQ SIGN BFONE*)))
             (COND
              ((OR (EQUAL M 1) (EQUAL M 3)) (SETQ R (|DIFFERENCE:| PI4 R))))
             (SETQ R (|CUT:MT| R K7))
             (COND ((OR (EQUAL M 0) (EQUAL M 3)) (GO M03)) (T (GO M12)))
            M03
             (SETQ R (|SIN:| R K7))
             (SETQ Q (|DIFFERENCE:| BFONE* (|TIMES:| R R)))
             (SETQ Q (|SQRT:| (|CUT:MT| Q K7) K7))
             (RETURN (|TIMES:| SIGN (|DIVIDE:| R Q K)))
            M12
             (SETQ R (|SIN:| R K7))
             (SETQ Q (|DIFFERENCE:| BFONE* (|TIMES:| R R)))
             (SETQ Q (|SQRT:| (|CUT:MT| Q K7) K7))
             (RETURN (|TIMES:| SIGN (|DIVIDE:| Q R K))))))) 
(PUT '|ASIN:| 'NUMBER-OF-ARGS 2) 
(PUT '|ASIN:| 'DEFINED-ON-LINE '845) 
(PUT '|ASIN:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|ASIN:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |ASIN:| (X K)
    (COND
     ((OR (|GREATERP:| (|ABS:| X) BFONE*) (NOT (FIXP K)) (LEQ K 0))
      (BFLERRMSG '|ASIN:|))
     ((|MINUSP:| X) (|MINUS:| (|ASIN:| (|MINUS:| X) K)))
     (T
      (PROG (K7 Y)
        (SETQ K7 0)
        (SETQ K7 (PLUS K 7))
        (COND
         ((|LESSP:| (|DIFFERENCE:| BFONE* X)
                    (CONS '|:RD:| (CONS 2 (MINUS K7))))
          (RETURN (|ROUND:MT| (|TIMES:| (|:PI| (ADD1 K)) BFHALF*) K))))
        (SETQ Y (|CUT:MT| (|DIFFERENCE:| BFONE* (|TIMES:| X X)) K7))
        (SETQ Y (|DIVIDE:| X (|SQRT:| Y K7) K7))
        (RETURN (|ATAN:| Y K)))))) 
(PUT '|ACOS:| 'NUMBER-OF-ARGS 2) 
(PUT '|ACOS:| 'DEFINED-ON-LINE '866) 
(PUT '|ACOS:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|ACOS:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |ACOS:| (X K)
    (COND
     ((OR (|GREATERP:| (|ABS:| X) BFONE*) (NOT (FIXP K)) (LEQ K 0))
      (BFLERRMSG '|ACOS:|))
     (T
      (PROG (K7 Y)
        (SETQ K7 0)
        (SETQ K7 (PLUS K 7))
        (COND
         ((|LESSP:| (|ABS:| X) (CONS '|:RD:| (CONS 2 (MINUS K7))))
          (RETURN (|ROUND:MT| (|TIMES:| (|:PI| (ADD1 K)) BFHALF*) K))))
        (SETQ Y (|DIFFERENCE:| BFONE* (|TIMES:| X X)))
        (SETQ Y (|CUT:MT| Y K7))
        (SETQ Y (|DIVIDE:| (|SQRT:| Y K7) (|ABS:| X) K7))
        (RETURN
         (COND
          ((|MINUSP:| X)
           (|ROUND:MT| (|DIFFERENCE:| (|:PI| (PLUS K 1)) (|ATAN:| Y K)) K))
          (T (|ATAN:| Y K)))))))) 
(PUT '|ATAN:| 'NUMBER-OF-ARGS 2) 
(PUT '|ATAN:| 'DEFINED-ON-LINE '890) 
(PUT '|ATAN:| 'DEFINED-IN-FILE 'ARITH/BFELEM.RED) 
(PUT '|ATAN:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |ATAN:| (X K)
    (COND ((OR (NOT (FIXP K)) (LEQ K 0)) (BFLERRMSG '|ATAN:|))
          ((|BFZEROP:| X) BFZ*)
          ((|MINUSP:| X) (|MINUS:| (|ATAN:| (|MINUS:| X) K)))
          (T
           (PROG (K7 PI4 Y Z)
             (SETQ K7 0)
             (SETQ K7 (PLUS K 7))
             (SETQ PI4 (|TIMES:| (|:PI| K7) |:BF-0.25|))
             (COND ((|EQUAL:| X BFONE*) (RETURN (|ROUND:MT| PI4 K))))
             (COND
              ((|GREATERP:| X BFONE*)
               (RETURN
                (|ROUND:MT|
                 (|DIFFERENCE:| (|PLUS:| PI4 PI4)
                                (|ATAN:| (|DIVIDE:| BFONE* X K7) (PLUS K 1)))
                 K))))
             (COND ((|LESSP:| X |:BF0.419921875|) (GO AT)))
             (SETQ Y (|PLUS:| BFONE* (|CUT:MT| (|TIMES:| X X) K7)))
             (SETQ Y (|PLUS:| BFONE* (|SQRT:| Y K7)))
             (SETQ Y (|ATAN:| (|DIVIDE:| X Y K7) (PLUS K 1)))
             (RETURN (|ROUND:MT| (|TIMES:| Y BFTWO*) K))
            AT
             (PROG (M N NCUT DCUT TM ZI)
               (SETQ M 0)
               (SETQ N 0)
               (SETQ NCUT 0)
               (SETQ NCUT (DIFFERENCE K7 (MIN 0 (PLUS (|ORDER:| X) 1))))
               (SETQ Y (SETQ TM (SETQ ZI X)))
               (SETQ Z (|MINUS:| (|CUT:EP| (|TIMES:| X X) (MINUS NCUT))))
               (SETQ DCUT (CONS '|:RD:| (CONS 2 (MINUS NCUT))))
               (SETQ M 1)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (|GREATERP:| (|ABS:| TM) DCUT)) (RETURN NIL)))
                 (PROGN
                  (SETQ ZI (|CUT:EP| (|TIMES:| ZI Z) (MINUS NCUT)))
                  (SETQ N (MAX 1 (PLUS K7 (|ORDER:| ZI))))
                  (SETQ TM
                          (|DIVIDE:| ZI
                                     (CONS '|:RD:|
                                           (CONS (SETQ M (PLUS M 2)) 0))
                                     N))
                  (SETQ Y (|PLUS:| Y TM))
                  (COND
                   ((ZEROP (REMAINDER M 20))
                    (SETQ Y (|CUT:EP| Y (MINUS NCUT))))))
                 (GO WHILELABEL)))
             (RETURN (|ROUND:MT| Y K)))))) 
(ENDMODULE) 
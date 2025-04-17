(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROESOLV)) 
(SETQ GROESOLVELEVEL* 0) 
(PUT 'GROESOLVEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROESOLVEEVAL 'DEFINED-ON-LINE '53) 
(PUT 'GROESOLVEEVAL 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROESOLVEEVAL (U)
    (PROG (*EZGCD GBLIST OLDTORDER RES *GROEBOPT *PRECISE Y FAIL PROBLEMS
           DENOMINATORS* VARIABLES* GMODULE AT *NOTESTPARAMETERS)
      (SETQ *NOTESTPARAMETERS T)
      (COND ((NULL DMODE*) (SETQ *EZGCD T)))
      (SETQ *GROEBOPT *VAROPT)
      (SETQ GVARSLAST (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(LIST)))
      (SETQ OLDTORDER (APPLY1 'TORDER '(LEX)))
      (SETQ GROESOLDMODE* (GET DMODE* 'DNAME))
      (SETQ *GROEBCOMPLEX *COMPLEX)
      (GROESETDMODE GROESOLDMODE* NIL)
      (SETQ PROBLEMS (LIST U))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PROBLEMS (NOT FAIL))) (RETURN NIL)))
        (PROGN
         (SETQ U (CAR PROBLEMS))
         (SETQ PROBLEMS (CDR PROBLEMS))
         (COND
          (*TRGROESOLV
           (PROGN
            (WRITEPRI "Next problem: " 'FIRST)
            (WRITEPRI (MKQUOTE (CAR U)) 'LAST))))
         (SETQ GBLIST (CDR (GROEBNERFEVAL U)))
         (SETQ AT (UNION AT (CDR GLTERMS)))
         (SETQ *GROEBOPT NIL)
         (GROESETDMODE GROESOLDMODE* T)
         (COND ((NULL VARIABLES*) (SETQ VARIABLES* GVARSLAST)))
         (COND
          ((NOT (EQUAL GBLIST '((LIST 1))))
           (PROG (GB)
             (SETQ GB GBLIST)
            LAB
             (COND ((NULL GB) (RETURN NIL)))
             ((LAMBDA (GB)
                (PROGN
                 (COND
                  (*TRGROESOLV
                   (PROGN
                    (WRITEPRI "starting from basis " 'FIRST)
                    (WRITEPRI (MKQUOTE GB) 'LAST))))
                 (PROG (R)
                   (SETQ R RES)
                  LAB
                   (COND ((NULL R) (RETURN NIL)))
                   ((LAMBDA (R)
                      (COND
                       (GB
                        (PROGN
                         (COND
                          (*TRGROESOLV
                           (PROGN
                            (WRITEPRI "Redundancy check: comparing basis "
                                      'FIRST)
                            (WRITEPRI (MKQUOTE GB) 'LAST)
                            (WRITEPRI "against already solved " 'FIRST)
                            (WRITEPRI (MKQUOTE (CAR R)) 'LAST))))
                         (COND
                          ((SUBSETP (CAR R) (CAR U))
                           (PROGN
                            (COND
                             (*TRGROESOLV
                              (PROGN
                               (WRITEPRI
                                "Skipping redundancy check: already solved is a subset of mother problem "
                                'FIRST)
                               (WRITEPRI (MKQUOTE (CAR U)) 'LAST))))))
                          ((GROESOLVIDSUBSET? GB (CAR R) VARIABLES*)
                           (PROGN
                            (COND
                             (*TRGROESOLV
                              (PROGN
                               (WRITEPRI "basis " 'FIRST)
                               (WRITEPRI (MKQUOTE GB) 'LAST)
                               (WRITEPRI "more general than already solved "
                                         'FIRST)
                               (WRITEPRI (MKQUOTE (CAR R)) 'LAST)
                               (WRITEPRI "deleting the latter" 'ONLY))))
                            (SETQ RES (DELETE R RES))))
                          ((GROESOLVIDSUBSET? (CAR R) GB VARIABLES*)
                           (PROGN
                            (COND
                             (*TRGROESOLV
                              (PROGN
                               (WRITEPRI "basis " 'FIRST)
                               (WRITEPRI (MKQUOTE GB) 'LAST)
                               (WRITEPRI "is special case of already solved "
                                         'FIRST)
                               (WRITEPRI (MKQUOTE (CAR R)) 'LAST)
                               (WRITEPRI "removing redundant basis " 'FIRST)
                               (WRITEPRI (MKQUOTE GB) 'LAST)
                               NIL)))
                            (SETQ GB NIL)
                            NIL))
                          (*TRGROESOLV
                           (PROGN
                            (WRITEPRI "Not redundant: keeping " 'FIRST)
                            (WRITEPRI (MKQUOTE GB) 'LAST)
                            NIL)))))))
                    (CAR R))
                   (SETQ R (CDR R))
                   (GO LAB))
                 (COND
                  (GB
                   (PROGN
                    (SETQ Y
                            (GROESOLVEARB (GROESOLVE0 GB VARIABLES*)
                             VARIABLES*))
                    (COND ((NEQ Y 'FAILED) (SETQ RES (CONS (CONS GB Y) RES)))
                          (T (SETQ FAIL T)))
                    (COND
                     (*TRGROESOLV
                      (PROGN
                       (WRITEPRI "partial result: " 'FIRST)
                       (WRITEPRI (MKQUOTE (CONS 'LIST (CDAR RES))) 'LAST))))
                    (PROG (D)
                      (SETQ D DENOMINATORS*)
                     LAB
                      (COND ((NULL D) (RETURN NIL)))
                      ((LAMBDA (D)
                         (PROGN
                          (COND
                           (*TRGROESOLV
                            (PROGN
                             (WRITEPRI "Denominator: " 'FIRST)
                             (WRITEPRI (MKQUOTE D) 'LAST)
                             (WRITEPRI " --> adding problem " 'FIRST)
                             (WRITEPRI (MKQUOTE (APPEND GB (LIST D))) 'LAST))))
                          (SETQ PROBLEMS
                                  (CONS (LIST (APPEND GB (LIST D)) VARIABLES*)
                                        PROBLEMS))))
                       (CAR D))
                      (SETQ D (CDR D))
                      (GO LAB))
                    (SETQ DENOMINATORS* NIL))))))
              (CAR GB))
             (SETQ GB (CDR GB))
             (GO LAB)))))
        (GO WHILELABEL))
      (APPLY1 'TORDER (LIST OLDTORDER))
      (SETQ PROBLEMS NIL)
      (COND (FAIL (SETQ RES NIL)))
      (COND ((NULL RES) (SETQ REQUIREMENTS (APPEND REQUIREMENTS AT)))
            (T (SETQ ASSUMPTIONS (APPEND ASSUMPTIONS AT))))
      (PROG (R)
        (SETQ R RES)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R) (SETQ PROBLEMS (UNION (CDR R) PROBLEMS))) (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN (CONS 'LIST (GROESOLVE-REDUN2 PROBLEMS))))) 
(PUT 'GROESOLVE-REDUN2 'NUMBER-OF-ARGS 1) 
(PUT 'GROESOLVE-REDUN2 'DEFINED-ON-LINE '133) 
(PUT 'GROESOLVE-REDUN2 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVE-REDUN2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROESOLVE-REDUN2 (SOL)
    (PROG (B)
      (PROG (S)
        (SETQ S SOL)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((MEMQ S SOL)
             (PROGN
              (SETQ B NIL)
              (PROG (R)
                (SETQ R SOL)
               LAB
                (COND ((NULL R) (RETURN NIL)))
                ((LAMBDA (R)
                   (COND
                    ((NOT (EQ R S))
                     (COND
                      ((AND (NOT B) (GROESOLVE-REDUN2A R S)) (SETQ B R))))))
                 (CAR R))
                (SETQ R (CDR R))
                (GO LAB))
              (COND (B (SETQ SOL (DELETE S SOL))))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RETURN SOL))) 
(PUT 'GROESOLVE-REDUN2A 'NUMBER-OF-ARGS 2) 
(PUT 'GROESOLVE-REDUN2A 'DEFINED-ON-LINE '145) 
(PUT 'GROESOLVE-REDUN2A 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVE-REDUN2A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESOLVE-REDUN2A (R S)
    (COND ((SMEMQ 'ROOT_OF S) NIL)
          (T
           (PROG (Q *EVALLHSEQP *PROTFG)
             (SETQ *EVALLHSEQP T)
             (SETQ *PROTFG T)
             (SETQ Q (ERRORSET (LIST 'SUBEVAL (MKQUOTE (LIST S R))) NIL NIL))
             (COND ((ERRORP Q) (PROGN (SETQ ERFG* NIL) (RETURN NIL))))
             (SETQ Q (CDAR Q))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND Q
                       (EQUAL 0
                              (REVAL1 (LIST 'DIFFERENCE (CADAR Q) (CADDAR Q))
                                      T))))
                 (RETURN NIL)))
               (SETQ Q (CDR Q))
               (GO WHILELABEL))
             (RETURN (NULL Q)))))) 
(PUT 'GROESOLVIDSUBSET? 'NUMBER-OF-ARGS 3) 
(PUT 'GROESOLVIDSUBSET? 'DEFINED-ON-LINE '157) 
(PUT 'GROESOLVIDSUBSET? 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVIDSUBSET? 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVIDSUBSET? (B1 B2 VARS)
    (OR (NULL B1)
        (AND
         (OR (EQUAL (CAR B1) 'LIST)
             (EQUAL 0 (PREDUCEEVAL (LIST (CAR B1) B2 VARS))))
         (GROESOLVIDSUBSET? (CDR B1) B2 VARS)))) 
(PUT 'GROESOLVEARB 'NUMBER-OF-ARGS 2) 
(PUT 'GROESOLVEARB 'DEFINED-ON-LINE '163) 
(PUT 'GROESOLVEARB 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVEARB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESOLVEARB (R VARS)
    (COND ((OR (ATOM R) (NOT *ARBVARS)) R)
          (T
           (PROG (S FORALL-RESULT FORALL-ENDPTR)
             (SETQ S R)
             (COND ((NULL S) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (S)
                                 (PROGN
                                  (PROG (X)
                                    (SETQ X (CDR VARS))
                                   LAB
                                    (COND ((NULL X) (RETURN NIL)))
                                    ((LAMBDA (X)
                                       (COND
                                        ((NOT (SMEMBER X S))
                                         (SETQ S
                                                 (APPEND S
                                                         (LIST
                                                          (LIST 'EQUAL X
                                                                (PREPF
                                                                 (MAKEARBCOMPLEX)))))))))
                                     (CAR X))
                                    (SETQ X (CDR X))
                                    (GO LAB))
                                  S))
                               (CAR S))
                              NIL)))
            LOOPLABEL
             (SETQ S (CDR S))
             (COND ((NULL S) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (S)
                         (PROGN
                          (PROG (X)
                            (SETQ X (CDR VARS))
                           LAB
                            (COND ((NULL X) (RETURN NIL)))
                            ((LAMBDA (X)
                               (COND
                                ((NOT (SMEMBER X S))
                                 (SETQ S
                                         (APPEND S
                                                 (LIST
                                                  (LIST 'EQUAL X
                                                        (PREPF
                                                         (MAKEARBCOMPLEX)))))))))
                             (CAR X))
                            (SETQ X (CDR X))
                            (GO LAB))
                          S))
                       (CAR S))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GROESOLVE0 'NUMBER-OF-ARGS 2) 
(PUT 'GROESOLVE0 'DEFINED-ON-LINE '174) 
(PUT 'GROESOLVE0 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVE0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESOLVE0 (A VARS)
    (PROG (R IDS NEWVARS NEWA)
      (COND ((SETQ R (GROEPOSTNUMSOLVE A VARS)) (RETURN R)))
      (COND ((SETQ R (GROEPOSTFASTSOLVE A VARS)) (RETURN R)))
      (COND ((SETQ R (GROEPOSTHILLEBRAND A VARS)) (RETURN R)))
      (SETQ R (GROEPOSTSOLVEEVAL (LIST A VARS)))
      (COND ((NEQ R 'FAILED) (RETURN (CDR R))))
      (SETQ IDS (CDR (GINDEPENDENT_SETEVAL (LIST A VARS))))
      (COND ((NULL IDS) (GO NULLERR)))
      (SETQ IDS (CAR IDS))
      (SETQ NEWVARS
              (CONS 'LIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (CDR VARS))
                     STARTOVER
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (X)
                                 (COND ((NOT (MEMQ X IDS)) (LIST X))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ X (CDR X))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (X)
                                 (COND ((NOT (MEMQ X IDS)) (LIST X))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ X (CDR X))
                      (GO LOOPLABEL))))
      (SETQ NEWA (GROEBNEREVAL (LIST A NEWVARS)))
      (SETQ DENOMINATORS* (CDR GLTERMS))
      (COND
       ((EQUAL NEWA '(LIST 1))
        (RERROR 'GROEBNER 24 "recomputation for dim=0 failed")))
      (SETQ R (GROEPOSTFASTSOLVE NEWA NEWVARS))
      (COND (R (RETURN R)))
      (SETQ R (GROEPOSTSOLVEEVAL (LIST A VARS)))
      (COND ((NEQ R 'FAILED) (RETURN (CDR R))))
     NULLERR
      (RERROR 'GROEBNER 23
              "Moeller ideal decomposition failed with 0 dim ideal."))) 
(PUT 'GROEPOSTNUMSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEPOSTNUMSOLVE 'DEFINED-ON-LINE '197) 
(PUT 'GROEPOSTNUMSOLVE 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTNUMSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEPOSTNUMSOLVE (GB VARS)
    (COND
     ((AND (NOT (ERRORP (ERRORSET '(LOAD-PACKAGE 'ROOTS2) NIL NIL)))
           (GETD 'MULTROOT0)
           (MEMBER (GET DMODE* 'DNAME) '(ROUNDED COMPLEX-ROUNDED))
           (EQUAL (LENGTH GB) (LENGTH VARS)) (GROEPOSTNUMSOLVE1 GB VARS))
      ((LAMBDA (*COMPXROOTS) (CDR (REVAL1 (MULTROOT0 (PRECISION 0) GB) T)))
       T)))) 
(PUT 'GROEPOSTNUMSOLVE1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEPOSTNUMSOLVE1 'DEFINED-ON-LINE '204) 
(PUT 'GROEPOSTNUMSOLVE1 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTNUMSOLVE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEPOSTNUMSOLVE1 (GB VARS)
    (COND ((NULL GB) T)
          (T
           ((LAMBDA (Q)
              (AND (GROEPOSTNUMSOLVE1 (CDR GB) (CDR VARS))
                   (PROGN
                    (PROG (X)
                      (SETQ X (KERNELS (CAR (SIMP (CAR GB)))))
                     LAB
                      (COND ((NULL X) (RETURN NIL)))
                      ((LAMBDA (X) (SETQ Q (AND Q (MEMBER X VARS)))) (CAR X))
                      (SETQ X (CDR X))
                      (GO LAB))
                    Q)))
            T)))) 
(PUT 'GROEPOSTFASTSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEPOSTFASTSOLVE 'DEFINED-ON-LINE '210) 
(PUT 'GROEPOSTFASTSOLVE 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTFASTSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEPOSTFASTSOLVE (GB VARS)
    (PROG (U P1 P2 FAIL KL RES)
      (COND (*TRGROESOLV (PRIN2T "fast solve attempt")))
      (SETQ GROESOLDMODE* (GET DMODE* 'DNAME))
      (SETQ *GROEBNUMVAL (MEMBER GROESOLDMODE* '(ROUNDED COMPLEX-ROUNDED)))
      (GROESETDMODE GROESOLDMODE* 'NIL)
      (SETQ U
              (SETQ KL
                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                        (SETQ P (CDR GB))
                        (COND ((NULL P) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (P)
                                            (PROGN
                                             (SETQ P (CAR (SIMP P)))
                                             (CONS
                                              (INTERSECTION VARS (KERNELS P))
                                              P)))
                                          (CAR P))
                                         NIL)))
                       LOOPLABEL
                        (SETQ P (CDR P))
                        (COND ((NULL P) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ P (CAR (SIMP P)))
                                     (CONS (INTERSECTION VARS (KERNELS P)) P)))
                                  (CAR P))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (COND ((EQUAL U '((NIL))) (GO TRIVIAL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (CDR U))) (RETURN NIL)))
        (PROGN
         (SETQ P1 (CAR U))
         (SETQ P2 (CADR U))
         (SETQ U (CDR U))
         (SETCAR P1 (SETDIFF (CAR P1) (CAR P2)))
         (SETQ FAIL (OR FAIL (NULL (CAR P1)))))
        (GO WHILELABEL))
      (COND (FAIL (GO EXIT)))
      (SETQ RES
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R (GROEPOSTFASTSOLVE1 (REVERSE KL) NIL 0))
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R) (CONS 'LIST (REVERSE R)))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (R) (CONS 'LIST (REVERSE R))) (CAR R))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GO EXIT)
     TRIVIAL
      (SETQ RES
              (LIST
               (CONS 'LIST
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CDR VARS))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X)
                                           (LIST 'EQUAL X
                                                 (CAAAR (MAKEARBCOMPLEX))))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (LIST 'EQUAL X (CAAAR (MAKEARBCOMPLEX))))
                                 (CAR X))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
     EXIT
      (GROESETDMODE GROESOLDMODE* T)
      (RETURN RES))) 
(FLUID '(F)) 
(PUT 'GROEPOSTFASTSOLVE1 'NUMBER-OF-ARGS 3) 
(PUT 'GROEPOSTFASTSOLVE1 'DEFINED-ON-LINE '236) 
(PUT 'GROEPOSTFASTSOLVE1 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTFASTSOLVE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEPOSTFASTSOLVE1 (FL SUB N)
    (COND ((NULL FL) '(NIL))
          (T
           (PROG (U F V SUB1)
             (SETQ N (IPLUS2 N 1))
             (SETQ F (CAR FL))
             (SETQ V (CAR F))
             (SETQ F (CAR (SUBF (CDR F) SUB)))
             (COND ((NULL F) (RETURN (GROEPOSTFASTSOLVE1 (CDR FL) SUB N))))
             (SETQ V (CAR V))
             ((LAMBDA (KORD*) (SETQ F (REORDER F))) (LIST V))
             (COND
              ((NOT ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CDAR F)))
               (GROEPOSTCOLLECTDEN (REORDER (CDAR F)))))
             (SETQ U (GROESOLVEPOLYV (PREPF F) V))
             (RETURN
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S U)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ SUB1
                                    (COND ((SMEMQ 'ROOT_OF S) SUB)
                                          (T (CONS (CONS V (CADDAR S)) SUB))))
                            (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Q (GROEPOSTFASTSOLVE1 (CDR FL) SUB1 N))
                              (COND ((NULL Q) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Q) (CONS (CAR S) Q))
                                                (CAR Q))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Q (CDR Q))
                              (COND ((NULL Q) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Q) (CONS (CAR S) Q)) (CAR Q))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ SUB1
                                    (COND ((SMEMQ 'ROOT_OF S) SUB)
                                          (T (CONS (CONS V (CADDAR S)) SUB))))
                            (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Q (GROEPOSTFASTSOLVE1 (CDR FL) SUB1 N))
                              (COND ((NULL Q) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Q) (CONS (CAR S) Q))
                                                (CAR Q))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Q (CDR Q))
                              (COND ((NULL Q) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Q) (CONS (CAR S) Q)) (CAR Q))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL))))))) 
(UNFLUID '(F)) 
(PUT 'GROEPOSTCOLLECTDEN 'NUMBER-OF-ARGS 1) 
(PUT 'GROEPOSTCOLLECTDEN 'DEFINED-ON-LINE '254) 
(PUT 'GROEPOSTCOLLECTDEN 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTCOLLECTDEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEPOSTCOLLECTDEN (D)
    (PROG (P)
      (SETQ P (CDR (FCTRF D)))
     LAB
      (COND ((NULL P) (RETURN NIL)))
      ((LAMBDA (P)
         (COND
          ((NOT (MEMBER (SETQ P (PREPF (CAR P))) DENOMINATORS*))
           (SETQ DENOMINATORS* (CONS P DENOMINATORS*)))))
       (CAR P))
      (SETQ P (CDR P))
      (GO LAB))) 
(PUT 'GROESOLVE 'PSOPFN 'GROESOLVEEVAL) 
(PUT 'GROEPOSTSOLVEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEPOSTSOLVEEVAL 'DEFINED-ON-LINE '263) 
(PUT 'GROEPOSTSOLVEEVAL 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEPOSTSOLVEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEPOSTSOLVEEVAL (U)
    (PROG (A B VARS OLDORDER GROESOLDB* *GROEBPREREDUCE *GROEBOPT
           *GROESOLGARBAGE)
      (SETQ GROESOLDMODE* (GET DMODE* 'DNAME))
      (GROESETDMODE GROESOLDMODE* 'NIL)
      (SETQ *GROEBNUMVAL (MEMBER GROESOLDMODE* '(ROUNDED COMPLEX-ROUNDED)))
      (COND ((EQUAL VDPSORTMODE* 'LEX) T)
            (T
             (RERROR 'GROEBNER 8
                     "groepostproc, illegal torder;(only lex allowed)")))
      (SETQ A (GROEREVLIST (REVAL1 (CAR U) T)))
      (SETQ VARS
              (OR (AND (CDR U) (GROEREVLIST (CADR U))) (GROEBNERVARS A NIL)))
      (SETQ OLDORDER (SETKORDER VARS))
      (SETQ B (GROESOLVE1 A A VARS))
      (SETQ A NIL)
      (COND ((EQ B 'FAILED) (SETQ A B))
            (T
             (PROGN
              (PROG (SOL)
                (SETQ SOL B)
               LAB
                (COND ((NULL SOL) (RETURN NIL)))
                ((LAMBDA (SOL)
                   (COND ((NOT (MEMBER SOL A)) (SETQ A (CONS SOL A)))))
                 (CAR SOL))
                (SETQ SOL (CDR SOL))
                (GO LAB))
              (SETQ A
                      (CONS 'LIST
                            (PROG (SOL FORALL-RESULT FORALL-ENDPTR)
                              (SETQ SOL A)
                              (COND ((NULL SOL) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (SOL) (CONS 'LIST SOL))
                                                (CAR SOL))
                                               NIL)))
                             LOOPLABEL
                              (SETQ SOL (CDR SOL))
                              (COND ((NULL SOL) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (SOL) (CONS 'LIST SOL))
                                        (CAR SOL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))))))
      (SETKORDER OLDORDER)
      (GROESETDMODE GROESOLDMODE* T)
      (RETURN A))) 
(PUT 'GROEPOSTPROC 'PSOPFN 'GROEPOSTSOLVEEVAL) 
(PUT 'GROESOLVE1 'NUMBER-OF-ARGS 3) 
(PUT 'GROESOLVE1 'DEFINED-ON-LINE '302) 
(PUT 'GROESOLVE1 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVE1 (A FULLA VARS)
    (COND ((OR (NULL A) (EQUAL A '(1))) NIL)
          (T
           ((LAMBDA (*GROESOLRECURS)
              (PROGN
               (PROG (F1 A1 RES Q GI NG1 NG2 NGQ QG MV TEST)
                 (SETQ RES (ASSOC A GROESOLDB*))
                 (COND (RES (RETURN (CDR RES))))
                 (SETQ GROESOLVELEVEL* (IPLUS2 GROESOLVELEVEL* 1))
                 (COND ((MEMBER A *GROESOLRECURS) (RETURN 'FAILED)))
                 (SETQ *GROESOLRECURS (CONS A *GROESOLRECURS))
                 (COND
                  ((EQUAL (LENGTH A) 1)
                   (PROGN (SETQ RES (GROESOLVEPOLY (CAR A))) (GO READY))))
                 (SETQ F1 (CAR A))
                 (SETQ A1 (CDR A))
                 (SETQ TEST NIL)
                 (SETQ MV (INTERSECTION VARS (LTERMVARIABLES F1)))
                 (PROG (P)
                   (SETQ P A1)
                  LAB
                   (COND ((NULL P) (RETURN NIL)))
                   ((LAMBDA (P)
                      (COND
                       ((INTERSECTION MV (LTERMVARIABLES P)) (SETQ TEST T))))
                    (CAR P))
                   (SETQ P (CDR P))
                   (GO LAB))
                 (COND
                  ((NOT TEST)
                   (PROGN
                    (SETQ NGQ (GROESOLVE1 A1 A1 VARS))
                    (COND
                     ((EQ NGQ 'FAILED) (PROGN (SETQ RES 'FAILED) (GO READY))))
                    (SETQ RES (ZEROSETINTERSECTION NGQ F1 VARS))
                    (GO READY))))
                 (SETQ Q (GROESOLVIDQ A1 F1 VARS))
                 (COND
                  ((EQUAL Q '(1))
                   (PROGN (SETQ RES (GROESOLVE1 A1 FULLA VARS)) (GO READY))))
                 (SETQ NGQ (GROESOLVE1 Q Q VARS))
                 (COND
                  ((EQ NGQ 'FAILED) (PROGN (SETQ RES 'FAILED) (GO READY))))
                 (SETQ NG1 (ZEROSETINTERSECTION NGQ F1 VARS))
                 (COND
                  ((GROESOLVIDQTEST A1 F1 VARS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT Q) (RETURN NIL)))
                      (PROGN
                       (SETQ GI (CAR Q))
                       (SETQ Q (CDR Q))
                       (SETQ GI
                               (PREDUCEEVAL
                                (LIST GI (CONS 'LIST A) (CONS 'LIST VARS))))
                       (COND ((EQUAL GI 0) (SETQ Q NIL))
                             (T
                              (SETQ A
                                      (CDR
                                       (GROEBIDQ (CONS 'LIST A) GI
                                        (CONS 'LIST VARS)))))))
                      (GO WHILELABEL))
                    (SETQ NG2 (GROESOLVE1 A A VARS))
                    (COND
                     ((EQ NG2 'FAILED)
                      (PROGN (SETQ RES 'FAILED) (GO READY))))))
                  (T
                   (PROGN
                    (SETQ NG2 NIL)
                    (COND
                     ((EQUAL (LENGTH Q) 1)
                      (PROGN
                       (SETQ GI
                               (PREDUCEEVAL
                                (LIST (CAR Q) (CONS 'LIST FULLA)
                                      (CONS 'LIST VARS))))
                       (COND
                        ((NEQ GI 0)
                         (PROGN
                          (SETQ QG
                                  (CDR
                                   (GROEBIDQ (CONS 'LIST FULLA) GI
                                    (CONS 'LIST VARS))))
                          (SETQ NG2 (GROESOLVE1 QG QG VARS))
                          (COND
                           ((EQ NG2 'FAILED)
                            (PROGN (SETQ RES 'FAILED) (GO READY)))))))))
                     (T
                      (PROGN
                       (SETQ NG2 (GROESOLVE2 A1 Q VARS))
                       (COND
                        ((EQ NG2 'FAILED)
                         (PROGN (SETQ RES 'FAILED) (GO READY))))))))))
                 (SETQ RES (ZEROSETUNION NG1 NG2))
                READY
                 (SETQ GROESOLVELEVEL* (IDIFFERENCE GROESOLVELEVEL* 1))
                 (SETQ GROESOLDB* (CONS (CONS A RES) GROESOLDB*))
                 (RETURN RES))))
            *GROESOLRECURS)))) 
(PUT 'GROESOLVIDQTEST 'NUMBER-OF-ARGS 3) 
(PUT 'GROESOLVIDQTEST 'DEFINED-ON-LINE '358) 
(PUT 'GROESOLVIDQTEST 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVIDQTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVIDQTEST (A1 F1 VARS)
    (NOT (EQ (DEG F1 (CAR VARS)) (DEG (CAR A1) (CAR VARS))))) 
(PUT 'GROESOLVIDQ 'NUMBER-OF-ARGS 3) 
(PUT 'GROESOLVIDQ 'DEFINED-ON-LINE '361) 
(PUT 'GROESOLVIDQ 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVIDQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVIDQ (A1 F1 VARS)
    (PROG (TEMP X)
      (SETQ X (CAR VARS))
      (COND
       ((NOT (GROESOLVIDQTEST A1 F1 VARS))
        (RETURN (CDR (GROEBIDQ (CONS 'LIST A1) F1 (CONS 'LIST VARS))))))
      (SETQ TEMP
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P A1)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (REVAL1
                                     (CAR (REVERSE (COEFFEVAL (LIST P X)))) T))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (REVAL1 (CAR (REVERSE (COEFFEVAL (LIST P X)))) T))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CDR (GROEBNEREVAL (LIST (CONS 'LIST TEMP) (CONS 'LIST VARS))))))) 
(PUT 'GROESOLVE2 'NUMBER-OF-ARGS 3) 
(PUT 'GROESOLVE2 'DEFINED-ON-LINE '371) 
(PUT 'GROESOLVE2 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVE2 (A Q VARS) (GROESOLVETREE A Q Q VARS)) 
(PUT 'GROESOLVETREE 'NUMBER-OF-ARGS 4) 
(PUT 'GROESOLVETREE 'DEFINED-ON-LINE '376) 
(PUT 'GROESOLVETREE 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVETREE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVETREE (A T1 PHI VARS)
    (PROG (Q NGTEMP NGALL T2 H G A2 PHI2)
      (COND ((NULL PHI) (RETURN NIL))
            ((NULL T1)
             (PROGN
              (SETQ Q
                      (CDR
                       (GROEBIDQ (CONS 'LIST A) (CONS 'TIMES PHI)
                        (CONS 'LIST VARS))))
              (RETURN
               (COND ((EQUAL (CAR Q) 1) NIL) (T (GROESOLVE1 Q Q VARS)))))))
      (PROG (G)
        (SETQ G T1)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (SETQ H (PREDUCEEVAL (LIST G (CONS 'LIST A) (CONS 'LIST VARS))))
            (SETQ PHI (DELETE G PHI))
            (COND
             ((NOT (EQUAL H 0))
              (PROGN (SETQ T2 (CONS H T2)) (SETQ PHI (CONS H PHI)))))))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (COND ((NULL PHI) (RETURN NIL)))
      (SETQ T1 T2)
      (SETQ Q
              (CDR
               (GROEBIDQ (CONS 'LIST A) (CONS 'TIMES PHI) (CONS 'LIST VARS))))
      (COND
       ((NOT (EQUAL (CAR Q) 1))
        (PROGN
         (SETQ NGALL (GROESOLVE1 Q Q VARS))
         (COND ((EQ NGALL 'FAILED) (RETURN 'FAILED))))))
      (COND (*GROESOLGARBAGE (RETURN (GROESOLVERESTRUCT Q PHI VARS NGALL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT T1) (RETURN NIL)))
        (PROGN
         (SETQ G (CAR T1))
         (SETQ T1 (CDR T1))
         (SETQ PHI2 (DELETE G PHI))
         (COND
          (PHI2
           (PROGN
            (SETQ A2
                    (CDR
                     (GROEBNEREVAL
                      (LIST (CONS 'LIST (CONS G A)) (CONS 'LIST VARS)))))
            (COND
             ((NOT (EQUAL (CAR A2) 1))
              (PROGN
               (SETQ NGTEMP (GROESOLVETREE A2 T1 PHI2 VARS))
               (COND
                ((EQ NGTEMP 'FAILED) (PROGN (SETQ NGALL 'FAILED) (GO READY)))
                (T (SETQ NGALL (ZEROSETUNION NGTEMP NGALL)))))))))))
        (GO WHILELABEL))
     READY
      (RETURN NGALL))) 
(PUT 'GROESOLVERESTRUCT 'NUMBER-OF-ARGS 4) 
(PUT 'GROESOLVERESTRUCT 'DEFINED-ON-LINE '405) 
(PUT 'GROESOLVERESTRUCT 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVERESTRUCT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROESOLVERESTRUCT (A PHI VARS NGALL)
    (PROG (NEWA NEWVARS MV OLDORDER SOLU)
      (SETQ MV (LTERMVARIABLES (CONS 'TIMES PHI)))
      (SETQ MV (CAR MV))
      (SETQ NEWVARS (DELETE MV VARS))
      (SETQ OLDORDER (SETKORDER NEWVARS))
      (SETQ NEWA
              (CDR (GROEBNEREVAL (LIST (CONS 'LIST A) (CONS 'LIST NEWVARS)))))
      (SETQ *GROESOLGARBAGE NIL)
      (SETQ SOLU (GROESOLVE1 NEWA NEWA NEWVARS))
      (SETKORDER OLDORDER)
      (RETURN (COND (*GROESOLGARBAGE NGALL) (T SOLU))))) 
(PUT 'LTERMVARIABLES 'NUMBER-OF-ARGS 1) 
(PUT 'LTERMVARIABLES 'DEFINED-ON-LINE '420) 
(PUT 'LTERMVARIABLES 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'LTERMVARIABLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LTERMVARIABLES (U)
    (PROG (V)
      (SETQ U (CAR (SIMP U)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM U) (ATOM (CAR U))))) (RETURN NIL)))
        (PROGN (SETQ V (CONS (CAAAR U) V)) (SETQ U (CDAR U)))
        (GO WHILELABEL))
      (RETURN (REVERSIP V)))) 
(PUT 'ZEROSETINTERSECTION 'NUMBER-OF-ARGS 3) 
(PUT 'ZEROSETINTERSECTION 'DEFINED-ON-LINE '427) 
(PUT 'ZEROSETINTERSECTION 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROSETINTERSECTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ZEROSETINTERSECTION (NG POLY VARS)
    (PROG (RES NS TESTPOLY PPOLY SOL S VAR DOM)
      (SETQ RES NIL)
      (SETQ POLY (SIMP POLY))
      (SETQ VAR
              (COND
               ((NOT (OR (ATOM (CAR POLY)) (ATOM (CAR (CAR POLY)))))
                (GROESOLMVAR (CAR POLY) VARS))
               (T 'CONSTANT)))
     LOOP
      (COND ((EQUAL NG NIL) (GO FINISH)))
      (SETQ NS (CAR NG))
      (SETQ NG (CDR NG))
      (SETQ TESTPOLY POLY)
      (SETQ DOM (OR GROESOLDMODE* 'RATIONAL))
      (GROESETDMODE DOM T)
      (SETQ TESTPOLY (SIMP (PREPSQ TESTPOLY)))
      (PROG (U)
        (SETQ U NS)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (COND
            ((AND (IDP (LHS U)) (NOT (SMEMQ 'ROOT_OF (RHS U))))
             (PROGN
              (SETQ S (RHS U))
              (SETQ TESTPOLY (SUBSQ TESTPOLY (LIST (CONS (LHS U) S))))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (GROESETDMODE DOM NIL)
      (SETQ PPOLY (PREPF (CAR TESTPOLY)))
      (SETQ SOL (GROESOLVEPOLYV PPOLY VAR))
      (SETQ RES
              (APPEND RES
                      (PROG (R FORALL-RESULT FORALL-ENDPTR)
                        (SETQ R SOL)
                        (COND ((NULL R) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (R) (APPEND R NS)) (CAR R))
                                         NIL)))
                       LOOPLABEL
                        (SETQ R (CDR R))
                        (COND ((NULL R) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (R) (APPEND R NS)) (CAR R))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (GO LOOP)
     FINISH
      (RETURN RES))) 
(PUT 'GROESOLMVAR 'NUMBER-OF-ARGS 2) 
(PUT 'GROESOLMVAR 'DEFINED-ON-LINE '459) 
(PUT 'GROESOLMVAR 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLMVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESOLMVAR (POLY VARS)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND ((NOT (AND VARS (NOT (SMEMBER (CAR VARS) POLY)))) (RETURN NIL)))
       (SETQ VARS (CDR VARS))
       (GO WHILELABEL))
     (COND ((NULL VARS) (RERROR 'GROEBNER 27 "illegal poly")))
     (CAR VARS))) 
(PUT 'GROESOLVEPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'GROESOLVEPOLY 'DEFINED-ON-LINE '467) 
(PUT 'GROESOLVEPOLY 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVEPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROESOLVEPOLY (P) (GROESOLVEPOLYV P (MAINVAR P))) 
(PUT 'GROESOLVEPOLYV 'NUMBER-OF-ARGS 2) 
(PUT 'GROESOLVEPOLYV 'DEFINED-ON-LINE '469) 
(PUT 'GROESOLVEPOLYV 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESOLVEPOLYV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESOLVEPOLYV (P VAR)
    ((LAMBDA (DEPL*)
       (PROG (RES U *CONVERT Y Z)
         (COND ((SETQ U (ASSOC VAR DEPL*)) (SETQ DEPL* (DELETE U DEPL*))))
         (COND
          (*TRGROESOLV
           (PROGN
            (WRITEPRI "   solving univariate with respect to " 'FIRST)
            (WRITEPRI (MKQUOTE VAR) 'LAST)
            (WRITEPRI (MKQUOTE P) 'ONLY))))
         (PROG (S)
           (SETQ S GROEBROOTS*)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((EQUAL 0 (REVAL1 (LIST 'DIFFERENCE P (CAR S)) T))
                (SETQ RES (CDR S)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (COND (RES (RETURN RES)))
         (GROESETDMODE GROESOLDMODE* T)
         (SETQ U (CAR (SIMP P)))
         (SETQ RES
                 (COND
                  ((AND *GROEBNUMVAL (UNIVARIATEPOLYNOMIAL? U))
                   (GROEROOTS P VAR))
                  (T
                   ((LAMBDA (KORD* ALGLIST*) (SOLVEEVAL (LIST P VAR))) NIL
                    (CONS NIL NIL)))))
         (SETQ RES (CDR RES))
         (PROG (X)
           (SETQ X RES)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ Y (PREPF (SETQ Z (REORDER (CDR (SIMP (CADDR X)))))))
               (COND ((DEPENDSL Y VARIABLES*) (GROEPOSTCOLLECTDEN Z)))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (SETQ RES
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X RES)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (LIST X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (GROESETDMODE GROESOLDMODE* NIL)
         (RETURN RES)))
     DEPL*)) 
(PUT 'UNIVARIATEPOLYNOMIAL? 'NUMBER-OF-ARGS 1) 
(PUT 'UNIVARIATEPOLYNOMIAL? 'DEFINED-ON-LINE '496) 
(PUT 'UNIVARIATEPOLYNOMIAL? 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'UNIVARIATEPOLYNOMIAL? 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIVARIATEPOLYNOMIAL? (FM)
    (OR (OR (ATOM FM) (ATOM (CAR FM))) (UNIVARIATEPOLYNOMIAL?1 FM (CAAAR FM)))) 
(PUT 'UNIVARIATEPOLYNOMIAL?1 'NUMBER-OF-ARGS 2) 
(PUT 'UNIVARIATEPOLYNOMIAL?1 'DEFINED-ON-LINE '499) 
(PUT 'UNIVARIATEPOLYNOMIAL?1 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'UNIVARIATEPOLYNOMIAL?1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNIVARIATEPOLYNOMIAL?1 (FM V)
    (OR (OR (ATOM FM) (ATOM (CAR FM)))
        (AND (OR (ATOM (CDAR FM)) (ATOM (CAR (CDAR FM)))) (EQUAL V (CAAAR FM))
             (UNIVARIATEPOLYNOMIAL?1 (CDR FM) V)))) 
(PUT 'PREDECESSOR 'NUMBER-OF-ARGS 2) 
(PUT 'PREDECESSOR 'DEFINED-ON-LINE '503) 
(PUT 'PREDECESSOR 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'PREDECESSOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREDECESSOR (R L)
    (COND
     ((OR (NOT (PAIRP L)) (NOT (PAIRP (CDR L))) (EQUAL R (CAR L)))
      (RERROR 'GROEBNER 9 "no predecessor available"))
     ((EQUAL R (CADR L)) (CAR L)) (T (PREDECESSOR R (CDR L))))) 
(PUT 'ZEROSETUNION 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROSETUNION 'DEFINED-ON-LINE '509) 
(PUT 'ZEROSETUNION 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROSETUNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROSETUNION (NG1 NG2) (PROGN (SETQ NG1 (ZEROSETUNION1 NG1 NG2)) NG1)) 
(PUT 'ZEROSETUNION1 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROSETUNION1 'DEFINED-ON-LINE '511) 
(PUT 'ZEROSETUNION1 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROSETUNION1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROSETUNION1 (NG1 NG2)
    (COND ((EQUAL NG1 NIL) NG2)
          ((ZEROSETMEMBER (CAR NG1) NG2) (ZEROSETUNION1 (CDR NG1) NG2))
          (T (CONS (CAR NG1) (ZEROSETUNION1 (CDR NG1) NG2))))) 
(PUT 'ZEROSETMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROSETMEMBER 'DEFINED-ON-LINE '517) 
(PUT 'ZEROSETMEMBER 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROSETMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROSETMEMBER (NS NG)
    (COND ((EQUAL NG NIL) NIL) ((ZEROEQUAL NS (CAR NG)) NG)
          (T (ZEROSETMEMBER NS (CDR NG))))) 
(PUT 'ZEROEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROEQUAL 'DEFINED-ON-LINE '521) 
(PUT 'ZEROEQUAL 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROEQUAL (NS1 NS2)
    (COND ((ZEROSUBSET NS1 NS2) (ZEROSUBSET NS2 NS1)) (T NIL))) 
(PUT 'ZEROSUBSET 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROSUBSET 'DEFINED-ON-LINE '524) 
(PUT 'ZEROSUBSET 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'ZEROSUBSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROSUBSET (NS1 NS2)
    (COND ((NULL NS1) T) ((MEMBER (CAR NS1) NS2) (ZEROSUBSET (CDR NS1) NS2))
          (T NIL))) 
(PUT 'GROESETDMODE 'NUMBER-OF-ARGS 2) 
(PUT 'GROESETDMODE 'DEFINED-ON-LINE '528) 
(PUT 'GROESETDMODE 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROESETDMODE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROESETDMODE (DMODE DIR)
    (COND ((NULL DMODE) NIL)
          (T
           (PROG (*MSG X Y)
             (COND
              ((NULL DIR)
               (PROGN
                (COND (*COMPLEX (SETQ Y (SETDMODE 'COMPLEX NIL))))
                (SETQ *COMPLEX NIL)
                (COND ((EQUAL DMODE* '|:RD:|) (SETQ *ROUNDED NIL)))
                (COND (DMODE* (SETQ Y (SETDMODE (GET DMODE* 'DNAME) NIL))))
                (COND
                 (*GROEBCOMPLEX
                  (PROGN (SETDMODE 'COMPLEX T) (SETQ *COMPLEX T))))))
              (T
               (PROGN
                (COND
                 ((MEMQ DMODE '(COMPLEX COMPLEX-ROUNDED COMPLEX-RATIONAL))
                  (PROGN
                   (SETQ *COMPLEX T)
                   (SETQ Y (SETDMODE 'COMPLEX T))
                   (COND
                    ((SETQ X
                             (ATSOC DMODE
                                    '((COMPLEX-ROUNDED . ROUNDED)
                                      (COMPLEX-RATIONAL . RATIONAL))))
                     (SETQ Y (SETDMODE (CDR X) T))))))
                 (T (SETQ Y (SETDMODE DMODE T))))
                (COND
                 ((MEMQ DMODE '(ROUNDED COMPLEX-ROUNDED))
                  (SETQ *ROUNDED T))))))
             (SETQ *EZGCD (NULL DMODE*))
             (RETURN Y))))) 
(PUT 'PREDUCEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'PREDUCEEVAL 'DEFINED-ON-LINE '549) 
(PUT 'PREDUCEEVAL 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'PREDUCEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREDUCEEVAL (PARS)
    (PROG (N VARS X U V W OLDORDER *FACTOR *EXP *GSUGAR *VDPINTEGER PCOUNT*)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (COND
       (*GROEBPROT
        (SETQ GROEBPROTFILE
                (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (LIST 'LIST)))))
      (SETQ N (LENGTH PARS))
      (SETQ X (REVAL1 (CAR PARS) T))
      (SETQ U (REVAL1 (CADR PARS) T))
      (SETQ V (COND ((IGREATERP N 2) (REVAL1 (CADDR PARS) T)) (T NIL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL W) (RERROR 'GROEBNR2 3 "empty list in preduce.")))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'PREDUCE)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (A2VDP J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (A2VDP J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (A2VDP X))
      (COND
       (*GROEBPROT
        (PROGN
         (SETQ W
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J W)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J) (VDPENUMERATE J)) (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (VDPENUMERATE J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (GROEBPROTSETQ 'CANDIDATE (DIP2A (CADR (CDDR X))))
         (PROG (J)
           (SETQ J W)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (GROEBPROTSETQ (MKID 'POLY (VDPGETPROP J 'NUMBER))
               (DIP2A (CADR (CDDR J)))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB)))))
      (SETQ W (GROEBNORMALFORM X W 'SORT))
      (SETQ W (DIP2A (CADR (CDDR W))))
      (SETKORDER OLDORDER)
      (RETURN (COND (W W) (T 0))))) 
(PUT 'GROEROOTS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEROOTS 'DEFINED-ON-LINE '585) 
(PUT 'GROEROOTS 'DEFINED-IN-FILE 'GROEBNER/GROESOLV.RED) 
(PUT 'GROEROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEROOTS (P X)
    (PROG (R)
      (SETQ X NIL)
      (SETQ R (REVAL1 (LIST 'ROOTS P) T))
      (SETQ R
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E (CDR R))
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E)
                                    (LIST 'EQUAL (CADR E)
                                          (REVAL1 (CADDR E) T)))
                                  (CAR E))
                                 NIL)))
               LOOPLABEL
                (SETQ E (CDR E))
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E)
                            (LIST 'EQUAL (CADR E) (REVAL1 (CADDR E) T)))
                          (CAR E))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST R)))) 
(ENDMODULE) 
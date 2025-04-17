(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'XREDUCT)) 
(FLUID '(*TRXMOD *TRXIDEAL XTRUNCATE*)) 
(INFIX (LIST 'XMOD)) 
(PRECEDENCE (LIST 'XMOD 'FREEOF)) 
(PUT 'XMOD 'RTYPEFN 'GETRTYPECAR) 
(PUT 'XMOD 'LISTFN 'XMODLIST) 
(PUT 'XMOD 'SIMPFN 'SIMPXMOD) 
(PUT 'SIMPXMOD 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPXMOD 'DEFINED-ON-LINE '43) 
(PUT 'SIMPXMOD 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'SIMPXMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPXMOD (U)
    (PROG (X)
      (COND
       ((NEQ (LENGTH U) 2)
        (RERROR 'XIDEAL 0 "Wrong number of arguments to xmod")))
      (SETQ X (GETRLIST (REVAL1 (CADR U) NIL)))
      (RETURN
       (*PF2SQ
        (REPARTIT
         (XREDUCE (XPARTITOP (CAR U))
          (PROG (G FORALL-RESULT FORALL-ENDPTR)
            (SETQ G X)
           STARTOVER
            (COND ((NULL G) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    ((LAMBDA (G) (COND ((SETQ G (XPARTITOP G)) (LIST G))))
                     (CAR G)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
            (SETQ G (CDR G))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND ((NULL G) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    ((LAMBDA (G) (COND ((SETQ G (XPARTITOP G)) (LIST G))))
                     (CAR G)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ G (CDR G))
            (GO LOOPLABEL)))))))) 
(PUT 'XMODLIST 'NUMBER-OF-ARGS 2) 
(PUT 'XMODLIST 'DEFINED-ON-LINE '55) 
(PUT 'XMODLIST 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XMODLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XMODLIST (U V)
    (PROG (X)
      (COND
       ((NEQ (LENGTH U) 2)
        (RERROR 'XIDEAL 0 "Wrong number of arguments to xmod")))
      (SETQ X (GETRLIST (REVAL1 (CADR U) NIL)))
      (SETQ U
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (REVAL1 (CAR U) NIL)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F X)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (RETURN
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F U)
              STARTOVER
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (F)
                          (COND
                           ((SETQ F (XREDUCE F X))
                            (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ F (CDR F))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (F)
                          (COND
                           ((SETQ F (XREDUCE F X))
                            (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ F (CDR F))
               (GO LOOPLABEL)))))) 
(INFIX (LIST 'XMODIDEAL)) 
(PRECEDENCE (LIST 'XMODIDEAL 'FREEOF)) 
(PUT 'XMODIDEAL 'RTYPEFN 'GETRTYPECAR) 
(PUT 'XMODIDEAL 'LISTFN 'XMODIDEALLIST) 
(PUT 'XMODIDEAL 'SIMPFN 'SIMPXMODIDEAL) 
(PUT 'SIMPXMODIDEAL 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPXMODIDEAL 'DEFINED-ON-LINE '76) 
(PUT 'SIMPXMODIDEAL 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'SIMPXMODIDEAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPXMODIDEAL (U)
    (PROG (X)
      (COND
       ((NEQ (LENGTH U) 2)
        (RERROR 'XIDEAL 0 "Wrong number of arguments to xmodideal")))
      (SETQ X (GETRLIST (REVAL1 (CADR U) NIL)))
      (SETQ U (XPARTITOP (CAR U)))
      (SETQ XTRUNCATE* (XMAXDEGREE U))
      (SETQ X
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F X)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (PROG (F)
        (SETQ F X)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (COND ((NOT (XHOMOGENEOUS F)) (SETQ XTRUNCATE* NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      ((LAMBDA (*TRXMOD) (SETQ X (XIDEALPF X))) NIL)
      (RETURN (*PF2SQ (REPARTIT (XREDUCE U X)))))) 
(PUT 'XMODIDEALLIST 'NUMBER-OF-ARGS 2) 
(PUT 'XMODIDEALLIST 'DEFINED-ON-LINE '91) 
(PUT 'XMODIDEALLIST 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XMODIDEALLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XMODIDEALLIST (U V)
    (PROG (X)
      (COND
       ((NEQ (LENGTH U) 2)
        (RERROR 'XIDEAL 0 "Wrong number of arguments to xmodideal")))
      (SETQ X (GETRLIST (REVAL1 (CADR U) NIL)))
      (SETQ U
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (REVAL1 (CAR U) NIL)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ XTRUNCATE*
              (EVAL
               (CONS 'MAX
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F U)
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (F) (XMAXDEGREE F)) (CAR F))
                                        NIL)))
                      LOOPLABEL
                       (SETQ F (CDR F))
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (F) (XMAXDEGREE F)) (CAR F))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ X
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F X)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (COND ((SETQ F (XPARTITOP F)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (PROG (F)
        (SETQ F X)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (COND ((NOT (XHOMOGENEOUS F)) (SETQ XTRUNCATE* NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      ((LAMBDA (*TRXMOD) (SETQ X (XIDEALPF X))) NIL)
      (RETURN
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F U)
              STARTOVER
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (F)
                          (COND
                           ((SETQ F (XREDUCE F X))
                            (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ F (CDR F))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (F)
                          (COND
                           ((SETQ F (XREDUCE F X))
                            (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ F (CDR F))
               (GO LOOPLABEL)))))) 
(PUT 'XAUTO 'RTYPEFN 'QUOTELIST) 
(PUT 'XAUTO 'LISTFN 'XAUTOLIST) 
(PUT 'XAUTOLIST 'NUMBER-OF-ARGS 2) 
(PUT 'XAUTOLIST 'DEFINED-ON-LINE '110) 
(PUT 'XAUTOLIST 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XAUTOLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XAUTOLIST (U V)
    (PROG (X)
      (COND
       ((NEQ (LENGTH U) 1)
        (RERROR 'XIDEAL 0 "Wrong number of arguments to xauto")))
      (SETQ U
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (REVAL1 (CAR U) NIL)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F (XAUTOREDUCE U))
              STARTOVER
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (F) (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ F (CDR F))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (F) (LIST (*Q2A1 (*PF2SQ (REPARTIT F)) V)))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ F (CDR F))
               (GO LOOPLABEL)))))) 
(PUT 'XREDUCE 'NUMBER-OF-ARGS 2) 
(PUT 'XREDUCE 'DEFINED-ON-LINE '121) 
(PUT 'XREDUCE 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XREDUCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XREDUCE (F P)
    (PROG (G L)
      (SETQ L (CONS NIL NIL))
      (COND
       (*TRXMOD
        (PROGN (WRITEPRI (MKQUOTE (PREPPF F)) 'NIL) (WRITEPRI " =" 'LAST))))
      (SETQ G (XREDUCE1 F P L))
      (COND
       (*TRXMOD
        (PROGN (WRITEPRI "   " 'FIRST) (WRITEPRI (MKQUOTE (PREPPF G)) 'LAST))))
      (RETURN G))) 
(PUT 'XREDUCE1 'NUMBER-OF-ARGS 3) 
(PUT 'XREDUCE1 'DEFINED-ON-LINE '138) 
(PUT 'XREDUCE1 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XREDUCE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XREDUCE1 (F P L)
    (COND
     ((SETQ F (WEAK_XREDUCE1 F P L)) (CONS (CAR F) (XREDUCE1 (CDR F) P L))))) 
(PUT 'WEAK_XREDUCE 'NUMBER-OF-ARGS 2) 
(PUT 'WEAK_XREDUCE 'DEFINED-ON-LINE '145) 
(PUT 'WEAK_XREDUCE 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'WEAK_XREDUCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WEAK_XREDUCE (F P)
    (PROG (G L)
      (SETQ L (CONS NIL NIL))
      (COND
       (*TRXMOD
        (PROGN (WRITEPRI (MKQUOTE (PREPPF F)) 'NIL) (WRITEPRI " =" 'LAST))))
      (SETQ G (WEAK_XREDUCE1 F P L))
      (COND
       (*TRXMOD
        (PROGN (WRITEPRI "   " 'FIRST) (WRITEPRI (MKQUOTE (PREPPF G)) 'LAST))))
      (RETURN G))) 
(PUT 'WEAK_XREDUCE1 'NUMBER-OF-ARGS 3) 
(PUT 'WEAK_XREDUCE1 'DEFINED-ON-LINE '162) 
(PUT 'WEAK_XREDUCE1 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'WEAK_XREDUCE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WEAK_XREDUCE1 (F P L)
    (PROG (Q G H C R)
      (SETQ Q P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND F Q)) (RETURN NIL)))
        (PROG ()
          (SETQ G (CAR Q))
          (SETQ Q (CDR Q))
          (COND
           ((SETQ R (XDIV (XVAL G) (XVAL F)))
            (PROG ()
              (SETQ R (CONS (CONS (MKNWEDGE R) (CONS 1 1)) NIL))
              (SETQ H (WEDGEPF R G))
              (SETQ C (MULTSQ (CDAR F) (INVSQ (CDAR H))))
              (SETQ F (SUBS2PF (ADDPF F (MULTPFSQ H (NEGSQ C)))))
              (COND
               (*TRXMOD (SETQ L (NCONC L (LIST (LIST (MULTPFSQ R C) G))))))
              (COND
               (*TRXMOD
                (PROGN
                 (WRITEPRI "   " 'FIRST)
                 (WRITEPRI
                  (MKQUOTE (LIST 'WEDGE (PREPPF (MULTPFSQ R C)) (PREPPF G)))
                  NIL)
                 (WRITEPRI " +" 'LAST)
                 NIL)))
              (SETQ Q P)))))
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'XAUTOREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'XAUTOREDUCE 'DEFINED-ON-LINE '191) 
(PUT 'XAUTOREDUCE 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XAUTOREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XAUTOREDUCE (F) (XAUTOREDUCE1 (WEAK_XAUTOREDUCE F))) 
(PUT 'XAUTOREDUCE1 'NUMBER-OF-ARGS 1) 
(PUT 'XAUTOREDUCE1 'DEFINED-ON-LINE '198) 
(PUT 'XAUTOREDUCE1 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'XAUTOREDUCE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XAUTOREDUCE1 (G)
    (PROG (H)
      (SETQ H (REVERSIP (SORT G 'PFORDP)))
      (SETQ G (LIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROG (K)
          (SETQ K (CAR H))
          (SETQ H (CDR H))
          (SETQ K (XREDUCE K G))
          (COND (K (SETQ G (CONS K G)))))
        (GO WHILELABEL))
      (RETURN (REVERSIP G)))) 
(PUT 'WEAK_XAUTOREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'WEAK_XAUTOREDUCE 'DEFINED-ON-LINE '214) 
(PUT 'WEAK_XAUTOREDUCE 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'WEAK_XAUTOREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WEAK_XAUTOREDUCE (F) (WEAK_XAUTOREDUCE1 F (LIST))) 
(PUT 'WEAK_XAUTOREDUCE1 'NUMBER-OF-ARGS 2) 
(PUT 'WEAK_XAUTOREDUCE1 'DEFINED-ON-LINE '220) 
(PUT 'WEAK_XAUTOREDUCE1 'DEFINED-IN-FILE 'XIDEAL/XREDUCT.RED) 
(PUT 'WEAK_XAUTOREDUCE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WEAK_XAUTOREDUCE1 (F G)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND ((NOT F) (RETURN NIL)))
        (PROG (K)
          (SETQ K (CAR F))
          (SETQ F (CDR F))
          (COND
           ((SETQ K (WEAK_XREDUCE K G))
            (PROG ()
              (SETQ K (XNORMALISE K))
              (PROG (H)
                (SETQ H G)
               LAB
                (COND ((NULL H) (RETURN NIL)))
                ((LAMBDA (H)
                   (COND
                    ((XDIV (XVAL K) (XVAL H))
                     (PROGN (SETQ F (CONS H F)) (SETQ G (DELETE H G))))))
                 (CAR H))
                (SETQ H (CDR H))
                (GO LAB))
              (SETQ G (APPEND G (LIST K)))))))
        (GO WHILELABEL))
      (RETURN G))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'XGROEB)) 
(FLUID
 '(*XFULLREDUCE *TRXIDEAL *TWOSIDED *TRXMOD XPOLYLIST* XVARLIST* ZERODIVS*
   XTRUNCATE* XDEGREELIST*)) 
(GLOBAL '(DIMEX*)) 
(PUT 'XIDEAL 'RTYPEFN 'QUOTELIST) 
(PUT 'XIDEAL 'LISTFN 'XIDEALLIST) 
(PUT 'XIDEALLIST 'NUMBER-OF-ARGS 2) 
(PUT 'XIDEALLIST 'DEFINED-ON-LINE '43) 
(PUT 'XIDEALLIST 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XIDEALLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XIDEALLIST (U V)
    (PROG (X Y)
      (SETQ XTRUNCATE* NIL)
      (COND
       ((ATOM U) (RERROR 'XIDEAL 0 "Wrong number of arguments to xideal")))
      (COND
       ((EQCAR (SETQ X (REVAL1 (CAR U) NIL)) 'LIST)
        (PROGN (SETQ X (CDR X)) (SETQ U (CDR U))))
       (T (TYPERR (CAR U) 'LIST)))
      (COND
       ((AND U (EQCAR (SETQ Y (REVAL1 (CAR U) T)) 'LIST))
        (PROGN (XVARS Y) (SETQ U (CDR U)))))
      (COND
       (U
        (COND
         ((FIXP (SETQ Y (REVAL1 (CAR U) T)))
          (PROGN (SETQ XTRUNCATE* Y) (SETQ U (CDR U))))
         (T (TYPERR Y "truncation degree")))))
      (COND (U (RERROR 'XIDEAL 0 "Wrong number of arguments to xideal")))
      (SETQ X
              (XIDEALPF
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
                 (GO LOOPLABEL))))
      (RETURN
       (CONS 'LIST
             (PROG (G FORALL-RESULT FORALL-ENDPTR)
               (SETQ G X)
               (COND ((NULL G) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (G) (*Q2A1 (*PF2SQ (REPARTIT G)) V))
                                 (CAR G))
                                NIL)))
              LOOPLABEL
               (SETQ G (CDR G))
               (COND ((NULL G) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (G) (*Q2A1 (*PF2SQ (REPARTIT G)) V)) (CAR G))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'XIDEALPF 'NUMBER-OF-ARGS 1) 
(PUT 'XIDEALPF 'DEFINED-ON-LINE '68) 
(PUT 'XIDEALPF 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XIDEALPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XIDEALPF (P)
    ((LAMBDA (XVARLIST* XDEGREELIST*) (XIDEAL0 (STOREXVARS P))) (LIST) (LIST))) 
(PUT 'STOREXVARS 'NUMBER-OF-ARGS 1) 
(PUT 'STOREXVARS 'DEFINED-ON-LINE '74) 
(PUT 'STOREXVARS 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'STOREXVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STOREXVARS (P)
    (PROG ()
      (SETQ XVARLIST* NIL)
      (PROG (F)
        (SETQ F P)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (COND
             ((AND XTRUNCATE* (NOT (XHOMOGENEOUS F)))
              (PROGN
               (LPRIM "inhomogeneous input - truncation not possible")
               (SETQ XTRUNCATE* NIL))))
            (SETQ XVARLIST* (UNION (ALLXVARS F) XVARLIST*))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ XVARLIST* (SORT XVARLIST* 'WORDERP))
      (SETQ XDEGREELIST*
              (CONS (CONS 1 0)
                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                      (SETQ K XVARLIST*)
                      (COND ((NULL K) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K) (CONS K (XDEGREE K)))
                                        (CAR K))
                                       NIL)))
                     LOOPLABEL
                      (SETQ K (CDR K))
                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (K) (CONS K (XDEGREE K))) (CAR K))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ ZERODIVS*
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V XVARLIST*)
               STARTOVER
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (V) (COND ((EXC_ODDP (XDEGREE V)) (LIST V))))
                         (CAR V)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ V (CDR V))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (V) (COND ((EXC_ODDP (XDEGREE V)) (LIST V))))
                         (CAR V)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ V (CDR V))
                (GO LOOPLABEL)))
      (COND
       ((AND (FIXP DIMEX*)
             (LESSP DIMEX*
                    (PROG (V FORALL-RESULT)
                      (SETQ V XVARLIST*)
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (PLUS ((LAMBDA (V) (XDEGREE V)) (CAR V))
                                    FORALL-RESULT))
                      (SETQ V (CDR V))
                      (GO LAB1))))
        (RERROR 'XIDEAL 0
                "too many independent p-forms in XIDEAL (check SPACEDIM)")))
      (RETURN P))) 
(PUT 'ALLXVARS 'NUMBER-OF-ARGS 1) 
(PUT 'ALLXVARS 'DEFINED-ON-LINE '97) 
(PUT 'ALLXVARS 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'ALLXVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLXVARS (F)
    (COND ((OR (NULL F) (EQUAL (CAAR F) 1)) NIL)
          (T (APPEND (WEDGEFAX (CAAR F)) (ALLXVARS (CDR F)))))) 
(PUT 'XIDEAL0 'NUMBER-OF-ARGS 1) 
(PUT 'XIDEAL0 'DEFINED-ON-LINE '103) 
(PUT 'XIDEAL0 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XIDEAL0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XIDEAL0 (F)
    (PROG (G F0 P)
      (COND (*TRXIDEAL (XPRINT_BASIS "Input Basis" F)))
      (COND (*XFULLREDUCE (SETQ F (WEAK_XAUTOREDUCE1 F (LIST)))))
      (COND
       ((AND *TRXIDEAL (NOT (XEQUIV F XPOLYLIST*)))
        (XPRINT_BASIS "New Basis" F)))
      (SETQ P (CRITICAL_PAIRS F (LIST) (CONS '*XSET* NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL (CDR P)))) (RETURN NIL)))
        (PROG (CP K)
          (SETQ CP (REMOVE_LEAST_ITEM P))
          (COND (*TRXIDEAL (XPRINT_PAIR CP)))
          (COND
           ((AND (NOT (XRITERION_1 CP F P)) (NOT (XRITERION_2 CP ZERODIVS* P)))
            (COND
             ((SETQ K (WEAK_XREDUCE (CRITICAL_ELEMENT CP) F))
              (COND
               ((EQUAL (CAAR K) 1)
                (PROGN
                 (SETQ P (CONS '*XSET* NIL))
                 (SETQ F
                         (LIST
                          (XREGISTER (CONS (CONS 1 (CONS 1 1)) NIL) CP)))))
               (T
                (PROGN
                 (SETQ K (XREGISTER (XNORMALISE K) CP))
                 (SETQ G
                         (COND (*XFULLREDUCE (WEAK_XAUTOREDUCE1 (LIST K) F))
                               (T (CONS K F))))
                 (SETQ F0 (INTERSECTION F G))
                 (SETQ P (REMOVE_CRITICAL_PAIRS (SETDIFF F F0) P))
                 (COND
                  ((AND *TRXIDEAL (NOT (XEQUIV G XPOLYLIST*)))
                   (XPRINT_BASIS "New Basis" G)))
                 (SETQ P (CRITICAL_PAIRS (SETDIFF G F0) F0 P))
                 (SETQ F G)))))
             ((AND *TRXIDEAL (NOT *TRXMOD)) (WRITEPRI 0 'LAST))))))
        (GO WHILELABEL))
      (RETURN
       (COND (*XFULLREDUCE (XAUTOREDUCE1 F)) (T (REVERSIP (SORT F 'PFORDP))))))) 
(PUT 'XRITERION_1 'NUMBER-OF-ARGS 3) 
(PUT 'XRITERION_1 'DEFINED-ON-LINE '138) 
(PUT 'XRITERION_1 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XRITERION_1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XRITERION_1 (CP G P)
    (COND ((NULL G) NIL) ((NEQ (CADR CP) 'SPOLY_PAIR) NIL)
          (T
           ((LAMBDA (X)
              (OR
               (AND (NEQ X (CADDR CP)) (NEQ X (CADDR (CDR CP)))
                    (XDIV (XVAL X) (CAR CP))
                    ((LAMBDA (PR) (OR (NULL PR) (NOT (FIND_ITEM PR P))))
                     (MAKE_SPOLY_PAIR X (CADDR CP)))
                    ((LAMBDA (PR) (OR (NULL PR) (NOT (FIND_ITEM PR P))))
                     (MAKE_SPOLY_PAIR X (CADDR (CDR CP))))
                    (PROGN
                     (COND (*TRXIDEAL (WRITEPRI "criterion 1 hit" 'LAST)))
                     T))
               (XRITERION_1 CP (CDR G) P)))
            (CAR G))))) 
(PUT 'XRITERION_2 'NUMBER-OF-ARGS 3) 
(PUT 'XRITERION_2 'DEFINED-ON-LINE '152) 
(PUT 'XRITERION_2 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XRITERION_2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XRITERION_2 (CP G P)
    (COND ((NULL G) NIL) ((NEQ (CADR CP) 'WEDGE_PAIR) NIL)
          (T
           ((LAMBDA (X)
              (OR
               (AND (NEQ (CONS (CONS X (CONS 1 1)) NIL) (CADDR CP))
                    (XDIV (LIST X X) (CAR CP))
                    ((LAMBDA (PR) (OR (NULL PR) (NOT (FIND_ITEM PR P))))
                     (MAKE_WEDGE_PAIR X (CADDR (CDR CP))))
                    (PROGN
                     (COND (*TRXIDEAL (WRITEPRI "criterion 2 hit" 'LAST)))
                     T))
               (XRITERION_2 CP (CDR G) P)))
            (CAR G))))) 
(PUT 'XEQUIV 'NUMBER-OF-ARGS 2) 
(PUT 'XEQUIV 'DEFINED-ON-LINE '168) 
(PUT 'XEQUIV 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XEQUIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XEQUIV (F G) (AND (EQUAL (LENGTH F) (LENGTH G)) (SUBLISTP F G))) 
(PUT 'XREGISTER 'NUMBER-OF-ARGS 2) 
(PUT 'XREGISTER 'DEFINED-ON-LINE '174) 
(PUT 'XREGISTER 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XREGISTER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XREGISTER (K PR)
    (PROG ()
      (EVAL (LIST (MKID 'XREGISTER_ (CADR PR))))
      (COND
       (*TRXIDEAL
        (PROGN
         (SETQ XPOLYLIST* (APPEND XPOLYLIST* (LIST K)))
         (WRITEPRI
          (MKQUOTE (LIST 'EQUAL (LIST 'XPOLY (XPOLYINDEX K)) (PREPPF K)))
          'LAST))))
      (RETURN K))) 
(PUT 'XREGISTER_SPOLY_PAIR 'NUMBER-OF-ARGS 0) 
(PUT 'XREGISTER_SPOLY_PAIR 'DEFINED-ON-LINE '188) 
(PUT 'XREGISTER_SPOLY_PAIR 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XREGISTER_SPOLY_PAIR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE XREGISTER_SPOLY_PAIR NIL NIL) 
(PUT 'XREGISTER_WEDGE_PAIR 'NUMBER-OF-ARGS 0) 
(PUT 'XREGISTER_WEDGE_PAIR 'DEFINED-ON-LINE '189) 
(PUT 'XREGISTER_WEDGE_PAIR 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XREGISTER_WEDGE_PAIR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE XREGISTER_WEDGE_PAIR NIL NIL) 
(PUT 'XREGISTER_XCOMM_PAIR 'NUMBER-OF-ARGS 0) 
(PUT 'XREGISTER_XCOMM_PAIR 'DEFINED-ON-LINE '190) 
(PUT 'XREGISTER_XCOMM_PAIR 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XREGISTER_XCOMM_PAIR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE XREGISTER_XCOMM_PAIR NIL NIL) 
(PUT 'XPRINT_BASIS 'NUMBER-OF-ARGS 2) 
(PUT 'XPRINT_BASIS 'DEFINED-ON-LINE '193) 
(PUT 'XPRINT_BASIS 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XPRINT_BASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XPRINT_BASIS (S P)
    (PROG ()
      (SETQ XPOLYLIST* P)
      (WRITEPRI S 'ONLY)
      (PROG (F)
        (SETQ F P)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (MATHPRINT (LIST 'EQUAL (LIST 'XPOLY (XPOLYINDEX F)) (PREPPF F))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB)))) 
(DE XPOLYINDEX (X) (LENGTH (MEMBER X (REVERSE XPOLYLIST*)))) 
(PUT 'XPOLYINDEX 'NUMBER-OF-ARGS 1) 
(PUT 'XPOLYINDEX 'DEFINED-ON-LINE '206) 
(PUT 'XPOLYINDEX 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XPOLYINDEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'XPOLYINDEX 'INLINE
      '(LAMBDA (X) (LENGTH (MEMBER X (REVERSE XPOLYLIST*))))) 
(PUT 'XPRINT_PAIR 'NUMBER-OF-ARGS 1) 
(PUT 'XPRINT_PAIR 'DEFINED-ON-LINE '210) 
(PUT 'XPRINT_PAIR 'DEFINED-IN-FILE 'XIDEAL/XGROEB.RED) 
(PUT 'XPRINT_PAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XPRINT_PAIR (CP)
    (PROG ()
      (WRITEPRI (MKQUOTE (CADR CP)) 'FIRST)
      (COND
       ((EQUAL (CADR CP) 'SPOLY_PAIR)
        (WRITEPRI
         (MKQUOTE
          (CONS 'LIST
                (LIST (LENGTH (MEMBER (CADDR CP) (REVERSE XPOLYLIST*)))
                      (LENGTH
                       (MEMBER (CADDR (CDR CP)) (REVERSE XPOLYLIST*))))))
         NIL))
       ((EQUAL (CADR CP) 'WEDGE_PAIR)
        (WRITEPRI
         (MKQUOTE
          (CONS 'LIST
                (LIST (CAAR (CADDR CP))
                      (LENGTH
                       (MEMBER (CADDR (CDR CP)) (REVERSE XPOLYLIST*))))))
         NIL))
       (T
        (WRITEPRI
         (MKQUOTE
          (CONS 'LIST
                (LIST (CAAR (CADDR CP))
                      (LENGTH
                       (MEMBER (CADDR (CDR CP)) (REVERSE XPOLYLIST*))))))
         NIL)))
      (WRITEPRI " -> " NIL))) 
(ENDMODULE) 
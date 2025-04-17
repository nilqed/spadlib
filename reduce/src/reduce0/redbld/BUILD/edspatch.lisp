(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSPATCH)) 
(FLUID
 '(*EDSVERBOSE *EDSDEBUG *ARBVARS *VAROPT *GROEBOPT *SOLVEINCONSISTENT DEPL*)) 
(PUT 'MAP-EVAL1 'NUMBER-OF-ARGS 4) 
(PUT 'MAP-EVAL1 'DEFINED-ON-LINE '39) 
(PUT 'MAP-EVAL1 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'MAP-EVAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAP-EVAL1 (O Q FCN1 FCN2) (MAP-APPLY (MAP-FUNCTION Q FCN1 FCN2) O)) 
(PUT 'MAP-FUNCTION 'NUMBER-OF-ARGS 3) 
(PUT 'MAP-FUNCTION 'DEFINED-ON-LINE '46) 
(PUT 'MAP-FUNCTION 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'MAP-FUNCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAP-FUNCTION (Q FCN1 FCN2)
    (PROG (V W)
      (SETQ V '&&X)
      (COND
       ((AND (IDP Q) (OR (GET Q 'SIMPFN) (EQUAL (GET Q 'NUMBER-OF-ARGS) 1)))
        (PROGN (SETQ W V) (SETQ Q (LIST Q V))))
       ((EQCAR Q 'REPLACEBY) (PROGN (SETQ W (CADR Q)) (SETQ Q (CADDR Q))))
       (T
        (PROGN
         (SETQ W (MAP-FRVARSOF Q NIL))
         (COND ((NULL W) (REDERR "map/select: no free variable found"))
               ((CDR W) (REDERR "map/select: free variable ambiguous")))
         (SETQ W (CAR W))
         NIL)))
      (COND ((EQCAR W '~) (SETQ W (CADR W))))
      (SETQ Q (SUBLIS (LIST (CONS W V) (CONS (LIST '~ W) V)) Q))
      (RETURN
       (LIST 'LAMBDA (LIST 'W)
             (LIST 'MAP-EVAL2 'W (MKQUOTE V) (MKQUOTE Q) (MKQUOTE FCN1)
                   (MKQUOTE FCN2)))))) 
(PUT 'MAP-APPLY 'NUMBER-OF-ARGS 2) 
(PUT 'MAP-APPLY 'DEFINED-ON-LINE '66) 
(PUT 'MAP-APPLY 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'MAP-APPLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAP-APPLY (F O)
    (COND ((ATOM O) (APPLY1 F O))
          (T
           ((LAMBDA (M)
              (COND (M (APPLY2 M F O))
                    (T
                     (CONS (CAR O)
                           (PROG (W FORALL-RESULT FORALL-ENDPTR)
                             (SETQ W (CDR O))
                             (COND ((NULL W) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (W) (APPLY1 F W))
                                               (CAR W))
                                              NIL)))
                            LOOPLABEL
                             (SETQ W (CDR W))
                             (COND ((NULL W) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (W) (APPLY1 F W)) (CAR W))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))))
            (GET (CAR O) 'MAPFN))))) 
(PUT 'MAPMAT 'NUMBER-OF-ARGS 2) 
(PUT 'MAPMAT 'DEFINED-ON-LINE '72) 
(PUT 'MAPMAT 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'MAPMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAPMAT (F O)
    (CONS 'MAT
          (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
            (SETQ ROW (CDR O))
            (COND ((NULL ROW) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ROW)
                                (PROG (W FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ W ROW)
                                  (COND ((NULL W) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (W) (APPLY1 F W))
                                                    (CAR W))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ W (CDR W))
                                  (COND ((NULL W) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (W) (APPLY1 F W)) (CAR W))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                              (CAR ROW))
                             NIL)))
           LOOPLABEL
            (SETQ ROW (CDR ROW))
            (COND ((NULL ROW) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (ROW)
                        (PROG (W FORALL-RESULT FORALL-ENDPTR)
                          (SETQ W ROW)
                          (COND ((NULL W) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (W) (APPLY1 F W)) (CAR W))
                                           NIL)))
                         LOOPLABEL
                          (SETQ W (CDR W))
                          (COND ((NULL W) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (W) (APPLY1 F W)) (CAR W))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR ROW))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'MAT 'MAPFN 'MAPMAT) 
(GLOBAL '(INDXL*)) 
(FLUID '(KORD*)) 
(COND ((NOT (GETD 'EXCALCPUTFORM)) (COPYD 'EXCALCPUTFORM 'PUTFORM))) 
(PUT 'PUTFORM 'NUMBER-OF-ARGS 2) 
(PUT 'PUTFORM 'DEFINED-ON-LINE '90) 
(PUT 'PUTFORM 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'PUTFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PUTFORM (U V)
    (PROG ()
      (EXCALCPUTFORM U V)
      (COND
       ((NOT (ATOM U))
        (PROGN
         (PUT (CAR U) 'SUBFUNC (FUNCTION ARG2OF2))
         (REMFLAG (LIST (CAR U)) 'COVARIANT)))))) 
(PUT 'NDEPENDS 'NUMBER-OF-ARGS 2) 
(PUT 'NDEPENDS 'DEFINED-ON-LINE '99) 
(PUT 'NDEPENDS 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'NDEPENDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NDEPENDS (U V)
    (COND ((OR (NULL U) (NUMBERP U) (NUMBERP V)) NIL) ((EQUAL U V) U)
          ((AND (ATOM U) (MEMQ U FRLIS*)) T)
          (((LAMBDA (X) (AND X (LNDEPENDS (CDR X) V))) (ASSOC U DEPL*)) T)
          ((AND (NOT (ATOM U)) (IDP (CAR U)) (GET (CAR U) 'DNAME))
           ((LAMBDA (DEPENDS-FN)
              (COND (DEPENDS-FN (APPLY2 DEPENDS-FN U V)) (T NIL)))
            (GET (CAR U) 'DOMAIN-DEPENDS-FN)))
          ((AND (NOT (ATOMF U))
                (OR (LNDEPENDS (CDR U) V) (NDEPENDS (CAR U) V)))
           T)
          ((OR (ATOMF V) (AND (IDP (CAR V)) (GET (CAR V) 'DNAME))) NIL)
          (T NIL))) 
(COPYD 'DEPENDS 'NDEPENDS) 
(PUT 'LNDEPENDS 'NUMBER-OF-ARGS 2) 
(PUT 'LNDEPENDS 'DEFINED-ON-LINE '120) 
(PUT 'LNDEPENDS 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'LNDEPENDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LNDEPENDS (U V)
    (COND ((NULL U) NIL) ((ATOM U) (NDEPENDS U V))
          (T (OR (NDEPENDS (CAR U) V) (LNDEPENDS (CDR U) V))))) 
(PUT 'NDEPENDSL 'NUMBER-OF-ARGS 2) 
(PUT 'NDEPENDSL 'DEFINED-ON-LINE '129) 
(PUT 'NDEPENDSL 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'NDEPENDSL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NDEPENDSL (U V) (AND V (OR (NDEPENDS U (CAR V)) (NDEPENDSL U (CDR V))))) 
(FLUID
 '(*EDSVERBOSE *EDSDEBUG *ARBVARS *VAROPT *GROEBOPT *SOLVEINCONSISTENT DEPL*)) 
(FLUID '(*EXPANDEXPT)) 
(FLUID
 '(SYSTEM* OSYSTEM* UV* IV* FV* KL* SUB* INV* DEPL* *SOLVEALGP SOLVEALGDB*
   LAST-VARS* CONST-VARS* ROOT-VARS* *EXPLI GROEBROOTS* *TEST_SOLVEALG
   *ARBVARS)) 
(FLUID '(*TRNONLNR)) 
(GLOBAL '(LOADED-PACKAGES* !ARBINT)) 
(PUT 'SOLVENONLNRSYS2 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVENONLNRSYS2 'DEFINED-ON-LINE '174) 
(PUT 'SOLVENONLNRSYS2 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'SOLVENONLNRSYS2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVENONLNRSYS2 NIL
    (COND ((NULL *SOLVEALGP) (SETQ SYSTEM* '(FAILED)))
          (T
           ((LAMBDA (DEPL*)
              (PROG (IV* KL* INV* FV* R W *SOLVEALGP SOLVEALGDB* SUB*
                     LAST-VARS* GROEBROOTS* CONST-VARS* ROOT-VARS*)
                (COND
                 ((NOT *VAROPT)
                  (SETQ DEPL*
                          (APPEND
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L UV*)
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR (CONS L NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR (CONS L NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))
                           DEPL*))))
                (PROG (F)
                  (SETQ F SYSTEM*)
                 LAB
                  (COND ((NULL F) (RETURN NIL)))
                  ((LAMBDA (F)
                     (SOLVEALGK0
                      (COND (DMODE* ((LAMBDA (DMODE*) (CAR (SUBF F NIL))) NIL))
                            (T F))))
                   (CAR F))
                  (SETQ F (CDR F))
                  (GO LAB))
                (COND (*TRNONLNR (PRINT (LIST "original system:" SYSTEM*))))
                (COND (*TRNONLNR (PRINT (LIST "original kernels:" KL*))))
                (COND
                 ((NULL (CDR SYSTEM*))
                  (COND
                   ((AND (OR (SMEMQ 'SIN SYSTEM*) (SMEMQ 'COS SYSTEM*))
                         (SETQ R
                                 (SOLVENONLNRTANSUB
                                  (PREPF (SETQ W (CAR SYSTEM*))) (CAR UV*)))
                         (CAR R))
                    (RETURN (SOLVENONLNRTANSOLVE R (CAR UV*) W)))
                   ((AND (OR (SMEMQ 'SINH SYSTEM*) (SMEMQ 'COSH SYSTEM*))
                         (SETQ R
                                 (SOLVENONLNRTANHSUB
                                  (PREPF (SETQ W (CAR SYSTEM*))) (CAR UV*)))
                         (CAR R))
                    (RETURN (SOLVENONLNRTANHSOLVE R (CAR UV*) W))))))
                (COND
                 (((LAMBDA (DMODE*)
                     (ATOM (ERRORSET '(SOLVEALGK1) *TRNONLNR NIL)))
                   NIL)
                  (RETURN (SETQ SYSTEM* '(FAILED)))))
                (SETQ SYSTEM*
                        (CONS 'LIST
                              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                (SETQ P SYSTEM*)
                                (COND ((NULL P) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (P) (PREPF P))
                                                  (CAR P))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ P (CDR P))
                                (COND ((NULL P) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS ((LAMBDA (P) (PREPF P)) (CAR P))
                                              NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))
                (COND
                 ((NOT (MEMQ 'GROEBNER LOADED-PACKAGES*))
                  (LOAD-PACKAGE 'GROEBNER)))
                (PROG (X)
                  (SETQ X IV*)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (COND
                      ((NOT (MEMBER X LAST-VARS*))
                       (PROG (Y)
                         (SETQ Y LAST-VARS*)
                        LAB
                         (COND ((NULL Y) (RETURN NIL)))
                         ((LAMBDA (Y) (DEPEND1 X Y T)) (CAR Y))
                         (SETQ Y (CDR Y))
                         (GO LAB)))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (SETQ IV* (SORT IV* (FUNCTION DEPENDS)))
                (COND
                 (*TRNONLNR
                  (PROGN
                   (PRIN2T "Entering Groebner for system")
                   (WRITEPRI (MKQUOTE SYSTEM*) 'ONLY)
                   (WRITEPRI (MKQUOTE (CONS 'LIST IV*)) 'ONLY)
                   NIL)))
                (SETQ R (LIST SYSTEM* (CONS 'LIST IV*)))
                (SETQ R (GROESOLVEEVAL R))
                (COND
                 (*TRNONLNR
                  (PROGN
                   (PRIN2T "leaving Groebner with intermediate result")
                   (WRITEPRI (MKQUOTE R) 'ONLY)
                   (TERPRI)
                   (TERPRI)
                   NIL)))
                (COND ((MEMQ 'SIN SOLVEALGDB*) (SETQ R (SOLVEALGTRIG2 R))))
                (COND ((MEMQ 'SINH SOLVEALGDB*) (SETQ R (SOLVEALGHYP2 R))))
                (SETQ R
                        (COND ((EQUAL R '(LIST)) '(INCONSISTENT))
                              (T (SOLVEALGINV R))))
                (SETQ SYSTEM* R)
                (RETURN R)))
            DEPL*)))) 
(FLUID '(*TRSPARSE)) 
(PUT 'SOLVESPARSECHECK 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVESPARSECHECK 'DEFINED-ON-LINE '234) 
(PUT 'SOLVESPARSECHECK 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'SOLVESPARSECHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVESPARSECHECK (SYS VL)
    (PROG (VL1 XL SYS1 Q X Y SP)
      (SETQ SP 0)
      (SETQ SP 0)
      (SETQ VL1
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X VL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (CONS X (CONS 0 NIL))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CONS X (CONS 0 NIL))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (Q)
        (SETQ Q SYS)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROG (X)
             (SETQ X (SETQ XL (INTERSECTION (TOPKERNS Q) VL)))
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (PROGN
                 (SETQ Y (ASSOC X VL1))
                 (SETCDR Y (CONS (PLUS (CADR Y) 1) (UNION XL (CDDR Y))))
                 (SETQ SP (PLUS SP 1))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB)))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (PROG (P)
        (SETQ P VL1)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETCDR (CDR P) (DIFFERENCE (LENGTH (CDDR P)) 1)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       ((GREATERP SP (TIMES (LENGTH SYS) (LENGTH VL) 0.8))
        (PROGN
         (COND (*TRSPARSE (PRIN2T "System is not very sparse")))
         (RETURN NIL))))
      (COND (*TRSPARSE (SOLVESPARSEPRINT "Original sparse system" SYS VL)))
      (COND
       (*VAROPT
        (PROGN
         (SETQ VL1 (SORT VL1 (FUNCTION SOLVEVARORDP)))
         (SETQ VL1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X VL1)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ VL1 (SOLVEVARADJUST VL1))
         (PROG (K)
           (SETQ K (REVERSE VL1))
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K) (UPDKORDER K)) (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (SETQ SYS
                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Q SYS)
                   (COND ((NULL Q) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (Q) (REORDER Q)) (CAR Q))
                                         NIL)))
                  LOOPLABEL
                   (SETQ Q (CDR Q))
                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (Q) (REORDER Q)) (CAR Q)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T
        (SETQ VL1
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X VL1)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ SYS1
              (CONS (CONS NIL NIL)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X VL1)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (X) (CONS X NIL)) (CAR X))
                                            NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS X NIL)) (CAR X)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (PROG (Q)
        (SETQ Q SYS)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (COND
             ((OR (OR (ATOM Q) (ATOM (CAR Q))) (NOT (MEMBER (CAAAR Q) VL1)))
              (SETQ Y (ASSOC NIL SYS1)))
             (T (SETQ Y (ASSOC (CAAAR Q) SYS1))))
            (SETCDR Y (CONS Q (CDR Y)))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (PROG (P)
        (SETQ P (CDR SYS1))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((CDR P) (SETCDR P (SORT (CDR P) (FUNCTION SOLVESPARSESORT))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ SYS
              (NCONC
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P SYS1)
                STARTOVER
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (P) (COND ((CDR P) (LIST (CADR P)))))
                          (CAR P)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ P (CDR P))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (P) (COND ((CDR P) (LIST (CADR P)))))
                          (CAR P)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ P (CDR P))
                 (GO LOOPLABEL))
               (REVERSIP
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P SYS1)
                 STARTOVER
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (P) (COND ((CDR P) (CDDR P)))) (CAR P)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ P (CDR P))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (P) (COND ((CDR P) (CDDR P)))) (CAR P)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ P (CDR P))
                  (GO LOOPLABEL)))))
      (COND
       (*TRSPARSE
        (SOLVESPARSEPRINT "Variables and/or equations rearranged" SYS VL1)))
      (RETURN (CONS SYS VL1)))) 
(PUT 'SOLVEVARORDP1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEVARORDP1 'DEFINED-ON-LINE '306) 
(PUT 'SOLVEVARORDP1 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'SOLVEVARORDP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEVARORDP1 (X Y)
    (OR (DEPENDS X Y) (AND (NOT (DEPENDS Y X)) (ORDOP X Y)))) 
(PUT 'EXPTEXPFLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'EXPTEXPFLISTP 'DEFINED-ON-LINE '324) 
(PUT 'EXPTEXPFLISTP 'DEFINED-IN-FILE 'EDS/EDSPATCH.RED) 
(PUT 'EXPTEXPFLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPTEXPFLISTP (U) NIL) 
(ENDMODULE) 
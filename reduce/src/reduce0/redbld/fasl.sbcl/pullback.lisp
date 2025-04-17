(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PULLBACK)) 
(FLUID '(XVARS* KORD* SUBFG*)) 
(GLOBAL '(*SQVAR*)) 
(FLUID '(CFRMCOB* CFRMCRD* CFRMDRV* CFRMRSX*)) 
(PUT '*A2MAP 'NUMBER-OF-ARGS 1) 
(PUT '*A2MAP 'DEFINED-ON-LINE '47) 
(PUT '*A2MAP 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT '*A2MAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2MAP (U)
    (UNROLLMAP
     (PROG (J FORALL-RESULT FORALL-ENDPTR)
       (SETQ J (GETRLIST U))
       (COND ((NULL J) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (J)
                           (COND ((EQEXPR J) (CONS (*A2K (LHS J)) (RHS J)))
                                 (T
                                  (RERROR 'EDS 0
                                          "Incorrectly formed pullback map"))))
                         (CAR J))
                        NIL)))
      LOOPLABEL
       (SETQ J (CDR J))
       (COND ((NULL J) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (J)
                   (COND ((EQEXPR J) (CONS (*A2K (LHS J)) (RHS J)))
                         (T
                          (RERROR 'EDS 0 "Incorrectly formed pullback map"))))
                 (CAR J))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT '*MAP2A 'NUMBER-OF-ARGS 1) 
(PUT '*MAP2A 'DEFINED-ON-LINE '55) 
(PUT '*MAP2A 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT '*MAP2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *MAP2A (U)
    (CONS 'LIST
          (PROG (P FORALL-RESULT FORALL-ENDPTR)
            (SETQ P U)
            (COND ((NULL P) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                              (CAR P))
                             NIL)))
           LOOPLABEL
            (SETQ P (CDR P))
            (COND ((NULL P) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P))) (CAR P))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'PULLBACK 'RTYPEFN 'GETRTYPECAR) 
(PUT 'PULLBACK 'CFRMFN 'PULLBACKCFRM) 
(PUT 'PULLBACK 'EDSFN 'PULLBACKEDS) 
(PUT 'PULLBACK 'LISTFN 'PULLBACKLIST) 
(PUT 'PULLBACK 'SIMPFN 'SIMPPULLBACK) 
(PUT 'PULLBACKCFRM 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKCFRM 'DEFINED-ON-LINE '70) 
(PUT 'PULLBACKCFRM 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKCFRM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKCFRM (M X)
    (PROG ()
      (SETQ X (*A2RMAP X))
      (SETQ M (CAR (PULLBACKCFRM1 M (CAR X))))
      (RETURN
       (COND
        ((CADR X) (CFRMPROTECT (LIST 'RESTRICTCFRM1 M (LIST (LIST) (CADR X)))))
        (T M))))) 
(PUT 'PULLBACKEDS 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKEDS 'DEFINED-ON-LINE '81) 
(PUT 'PULLBACKEDS 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKEDS (S X) (PULLBACK0 S (*A2RMAP X))) 
(PUT 'PULLBACKLIST 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKLIST 'DEFINED-ON-LINE '87) 
(PUT 'PULLBACKLIST 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKLIST (U V)
    (PROG (X)
      (SETQ X (CAR (*A2RMAP (REVAL1 (CADR U) T))))
      (SETQ U (REVAL1 (CAR U) T))
      (RETURN
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F (CDR U))
              STARTOVER
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (F)
                          (COND
                           ((NEQ
                             (SETQ F (*PF2A1 (PULLBACKPF (XPARTITOP F) X) V))
                             0)
                            (LIST F))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ F (CDR F))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (F)
                          (COND
                           ((NEQ
                             (SETQ F (*PF2A1 (PULLBACKPF (XPARTITOP F) X) V))
                             0)
                            (LIST F))))
                        (CAR F)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ F (CDR F))
               (GO LOOPLABEL)))))) 
(PUT 'SIMPPULLBACK 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPPULLBACK 'DEFINED-ON-LINE '99) 
(PUT 'SIMPPULLBACK 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'SIMPPULLBACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPPULLBACK (U)
    ((LAMBDA (F X)
       (COND
        ((LESSP (DEGREEPF F) 0) (RERROR 'EDS 0 "Cannot pull back vectors"))
        (T (*PF2SQ (REPARTIT (PULLBACKPF F X))))))
     (XPARTITOP (CAR U)) (CAR (*A2RMAP (REVAL1 (CADR U) T))))) 
(PUT 'PULLBACKCFRM1 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKCFRM1 'DEFINED-ON-LINE '107) 
(PUT 'PULLBACKCFRM1 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKCFRM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKCFRM1 (M X)
    (PROG (N CFRMCRD* CFRMRSX*)
      (COND ((NULL X) (RETURN M)))
      (SETQ M (COPYCFRM M))
      (SETQ X (UNROLLMAP X))
      (SETQ N (*MAP2SRCCFRM X))
      (SETCAR (CDDR M) (RIGHTUNION (CADDR M) (CADDR N)))
      (SETCAR (CDDR M)
              (SETDIFF (CADDR M)
                       (PROG (P FORALL-RESULT FORALL-ENDPTR)
                         (SETQ P X)
                         (COND ((NULL P) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (P) (CAR P)) (CAR P))
                                               NIL)))
                        LOOPLABEL
                         (SETQ P (CDR P))
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
      (SETCAR (CDDR (CDDR M)) (RIGHTUNION (CADDR (CDDR M)) (CADDR (CDDR N))))
      (SETCAR (CDDR (CDDR M)) (PULLBACKRSX (CADDR (CDDR M)) X))
      (COND
       ((MEMBER 0 (CADDR (CDDR M)))
        (RERROR 'EDS 0 "Map image not within target coframing in pullback")))
      (SETQ CFRMCRD* (CADDR M))
      ((LAMBDA (XVARS*)
         (SETQ CFRMRSX*
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (CADDR (CDDR M)))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (P) (XPARTITOP P)) (CAR P))
                                         NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (P) (XPARTITOP P)) (CAR P)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
       CFRMCRD*)
      (SETQ X (*MAP2COTANGENT X))
      (COND
       ((NOT (SUBSETP (CAR X) (CADR M)))
        (RERROR 'EDS 0 "Map image not within target coframing in pullback")))
      (SETCAR (CDR M) (RIGHTUNION (CADR M) (CADR N)))
      (SETCAR (CDR M) (SETDIFF (CADR M) (CAR X)))
      (SETCAR (CDDR (CDR M)) (RIGHTUNION (CADDR (CDR M)) (CADDR (CDR N))))
      (SETCAR (CDDR (CDR M)) (PULLBACKDRV (CADDR (CDR M)) (CADR X)))
      (RETURN (LIST (PURGECFRM M) X)))) 
(PUT 'UNROLLMAP 'NUMBER-OF-ARGS 1) 
(PUT 'UNROLLMAP 'DEFINED-ON-LINE '148) 
(PUT 'UNROLLMAP 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'UNROLLMAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNROLLMAP (X)
    (PROG (R Z CFRMCRD* C)
      (SETQ C 0)
      (SETQ CFRMCRD*
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P X)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND X (LESSP (SETQ C (PLUS C 1)) 20))) (RETURN NIL)))
        (PROG ()
          (PROG (P)
            (SETQ P X)
           LAB
            (COND ((NULL P) (RETURN NIL)))
            ((LAMBDA (P)
               (PROGN
                (SETQ R (SIMP* (CDR P)))
                (COND
                 ((AND (CFRMCONSTANT (CAR R)) (CFRMCONSTANT (CDR R)))
                  (SETQ Z (CONS P Z))))))
             (CAR P))
            (SETQ P (CDR P))
            (GO LAB))
          (SETQ X (PULLBACKMAP (SETDIFF X Z) (APPEND X Z))))
        (GO WHILELABEL))
      (COND (X (RERROR 'EDS 0 "Recursive map")))
      (RETURN Z))) 
(PUT '*MAP2SRCCFRM 'NUMBER-OF-ARGS 1) 
(PUT '*MAP2SRCCFRM 'DEFINED-ON-LINE '169) 
(PUT '*MAP2SRCCFRM 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT '*MAP2SRCCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *MAP2SRCCFRM (X)
    (*SYS2CFRM
     (PROG (P FORALL-RESULT FORALL-ENDPTR)
       (SETQ P X)
       (COND ((NULL P) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P) (CONS (CONS 1 (SIMP* (CDR P))) NIL))
                         (CAR P))
                        NIL)))
      LOOPLABEL
       (SETQ P (CDR P))
       (COND ((NULL P) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (P) (CONS (CONS 1 (SIMP* (CDR P))) NIL)) (CAR P))
                     NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT '*MAP2COTANGENT 'NUMBER-OF-ARGS 1) 
(PUT '*MAP2COTANGENT 'DEFINED-ON-LINE '177) 
(PUT '*MAP2COTANGENT 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT '*MAP2COTANGENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *MAP2COTANGENT (X)
    (PROG (F OLD XL)
      (PROG (P)
        (SETQ P X)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ F (XPOWS (EXDFK (CAR P))))
            (COND
             ((OR (GREATERP (LENGTH F) 1) (NEQ (CAR F) (LIST 'D (CAR P))))
              (SETQ XL (CONS P XL)))
             (T (SETQ OLD (CONS (CAR F) OLD))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (XL (SETQ X (EXDFMAP XL X))))
      (SETQ OLD
              (APPEND OLD
                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                        (SETQ P X)
                       STARTOVER
                        (COND ((NULL P) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (P)
                                   (COND
                                    ((EQUAL (XDEGREE (CAR P)) 1)
                                     (LIST (CAR P)))))
                                 (CAR P)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ P (CDR P))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL P) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (P)
                                   (COND
                                    ((EQUAL (XDEGREE (CAR P)) 1)
                                     (LIST (CAR P)))))
                                 (CAR P)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ P (CDR P))
                        (GO LOOPLABEL))))
      (EDSDEBUG "Cobasis elements eliminated" OLD 'COB)
      (RETURN (LIST OLD X)))) 
(PUT 'EXDFMAP 'NUMBER-OF-ARGS 2) 
(PUT 'EXDFMAP 'DEFINED-ON-LINE '195) 
(PUT 'EXDFMAP 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'EXDFMAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXDFMAP (XL X)
    (PROG (F OLD Y OK)
      (SETQ OK (UPDKORDL (LIST)))
      (PROG (P)
        (SETQ P XL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ F (EXDFK (CAR P)))
            (SETQ OLD (UNION (XPOWS F) OLD))
            (COND
             ((OR (CDR F) (NEQ (CAAR F) (LIST 'D (CAR P))))
              (SETQ F (PULLBACKPF F X))))
            (SETQ Y
                    (CONS
                     (ADDPF F
                            (MULTPFSQ
                             (PULLBACKPF (XPARTITOP (LIST 'D (CDR P))) X)
                             (CONS (MINUS 1) 1)))
                     Y))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (EDSDEBUG "Possibilities for elimination" OLD 'COB)
      (SETQ Y (SOLVEPFSYS1 Y OLD))
      (COND
       ((CADR Y)
        (RERROR 'EDS 0 "Cannot determine suitable coframe for pullback")))
      (SETKORDER OK)
      (RETURN
       (APPEND X
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F (CAR Y))
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (F)
                                     (CONS (CAAR F)
                                           (MK*SQ
                                            (*PF2SQ
                                             (MULTPFSQ (XREORDER* (CDR F))
                                                       (CONS (MINUS 1) 1))))))
                                   (CAR F))
                                  NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (F)
                             (CONS (CAAR F)
                                   (MK*SQ
                                    (*PF2SQ
                                     (MULTPFSQ (XREORDER* (CDR F))
                                               (CONS (MINUS 1) 1))))))
                           (CAR F))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))) 
(PUT 'PULLBACKDRV 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKDRV 'DEFINED-ON-LINE '219) 
(PUT 'PULLBACKDRV 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKDRV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKDRV (D X)
    (PROG (R FORALL-RESULT FORALL-ENDPTR)
      (SETQ R D)
      (COND ((NULL R) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (R)
                          (LIST (CAR R) (CADR R)
                                (MK*SQ (SUBSQ (SIMP* (CADDR R)) X))))
                        (CAR R))
                       NIL)))
     LOOPLABEL
      (SETQ R (CDR R))
      (COND ((NULL R) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (R)
                  (LIST (CAR R) (CADR R) (MK*SQ (SUBSQ (SIMP* (CADDR R)) X))))
                (CAR R))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PULLBACKMAP 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKMAP 'DEFINED-ON-LINE '226) 
(PUT 'PULLBACKMAP 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKMAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKMAP (P X)
    (PROG (S FORALL-RESULT FORALL-ENDPTR)
      (SETQ S P)
      (COND ((NULL S) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (S)
                          (CONS (CAR S) (MK*SQ (SUBSQ (SIMP* (CDR S)) X))))
                        (CAR S))
                       NIL)))
     LOOPLABEL
      (SETQ S (CDR S))
      (COND ((NULL S) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (S) (CONS (CAR S) (MK*SQ (SUBSQ (SIMP* (CDR S)) X))))
                (CAR S))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PULLBACK0 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACK0 'DEFINED-ON-LINE '232) 
(PUT 'PULLBACK0 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACK0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACK0 (S X)
    (COND ((EMPTYEDSP (SETQ S (PULLBACK S (CAR X)))) S)
          ((CADR X) (EDSPROTECT (LIST 'RESTRICT S (LIST (LIST) (CADR X)))))
          (T S))) 
(PUT 'PULLBACK 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACK 'DEFINED-ON-LINE '240) 
(PUT 'PULLBACK 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACK (S X)
    (PROG (PRL COB M)
      (COND ((NULL X) (RETURN S)))
      (SETQ PRL (PRLKRNS S))
      (SETQ COB (EDSCOB S))
      (SETQ M (PULLBACKCFRM1 (CADDR (CDR S)) X))
      (SETQ X (CADR M))
      (SETQ M (CAR M))
      (SETCFRM M)
      (SETQ S (PURGEEDS* S))
      (COND ((NOT (SUBSETP (CADR M) COB)) (REMPROPEDS S 'JET0)))
      (COND
       ((AND (SUBSETP (CAR X) PRL)
             (NULL
              (XNP PRL
                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                     (SETQ F (PFAFFPART (CADR S)))
                    STARTOVER
                     (COND ((NULL F) (RETURN NIL)))
                     (SETQ FORALL-RESULT ((LAMBDA (F) (XPOWS F)) (CAR F)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ F (CDR F))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR ((LAMBDA (F) (XPOWS F)) (CAR F)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ F (CDR F))
                     (GO LOOPLABEL))))
             (NULL
              (XNP PRL
                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                     (SETQ F (CADDR S))
                    STARTOVER
                     (COND ((NULL F) (RETURN NIL)))
                     (SETQ FORALL-RESULT ((LAMBDA (F) (XPOWS F)) (CAR F)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ F (CDR F))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR ((LAMBDA (F) (XPOWS F)) (CAR F)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ F (CDR F))
                     (GO LOOPLABEL)))))
        (REMTRUEEDS S 'REDUCED))
       (T (REMTRUEEDS S 'SOLVED)))
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'PFAFFIAN 'QUASILINEAR 'CLOSED))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (REMFALSEEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (REMPROPEDS S 'INVOLUTIVE)
      (SETCAR (CDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND ((SETQ F (PULLBACKPF F (CADR X))) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND ((SETQ F (PULLBACKPF F (CADR X))) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (SETCAR (CDDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND ((SETQ F (PULLBACKPF F (CADR X))) (LIST F))
                                 (T
                                  (PROGN
                                   (EDSVERBOSE
                                    "Pullback inconsistent with independence condition"
                                    NIL NIL)
                                   NIL))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND ((SETQ F (PULLBACKPF F (CADR X))) (LIST F))
                                 (T
                                  (PROGN
                                   (EDSVERBOSE
                                    "Pullback inconsistent with independence condition"
                                    NIL NIL)
                                   NIL))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      ((LAMBDA (I) (SETCAR (CDR M) (APPEND (SETDIFF (CADR M) I) I)))
       (INDKRNS S))
      (SETCAR (CDDR (CDR S)) M)
      (COND
       ((NOT (SUBSETP (CADR M) COB))
        (PROGN
         (SETCFRM M)
         (SETCAR (CDR S) (XREORDERSYS (CADR S)))
         (SETCAR (CDDR S) (XREORDERSYS (CADDR S)))
         NIL)))
      (REMKRNS S)
      (RETURN (NORMALEDS S)))) 
(PUT 'PULLBACKPF 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKPF 'DEFINED-ON-LINE '287) 
(PUT 'PULLBACKPF 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKPF (F X) (SUBS2PF (PULLBACKPF1 F X))) 
(PUT 'PULLBACKPF1 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKPF1 'DEFINED-ON-LINE '296) 
(PUT 'PULLBACKPF1 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKPF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKPF1 (F X)
    (COND
     (F
      (ADDPF (MULTPFSQ (PULLBACKK (CAAR F) X) (SUBSQ (CDAR F) X))
             (PULLBACKPF1 (CDR F) X))))) 
(PUT 'PULLBACKK 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKK 'DEFINED-ON-LINE '303) 
(PUT 'PULLBACKK 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKK (K X) (XREORDER (XPARTITSQ (SUBF (LIST (CONS (CONS K 1) 1)) X)))) 
(PUT 'PULLBACKSQ 'NUMBER-OF-ARGS 2) 
(PUT 'PULLBACKSQ 'DEFINED-ON-LINE '309) 
(PUT 'PULLBACKSQ 'DEFINED-IN-FILE 'EDS/PULLBACK.RED) 
(PUT 'PULLBACKSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PULLBACKSQ (Q X) (XPARTITSQ (SUBSQ Q X))) 
(ENDMODULE) 
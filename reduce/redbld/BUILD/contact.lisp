(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CONTACT)) 
(GLOBAL '(INDXL* *SQVAR*)) 
(PUT 'CONTACT 'RTYPEFN 'QUOTEEDS) 
(PUT 'CONTACT 'EDSFN 'CONTACT) 
(FLAG '(CONTACT) 'NOSPREAD) 
(PUT 'CONTACT 'NUMBER-OF-ARGS 1) 
(PUT 'CONTACT 'DEFINED-ON-LINE '40) 
(PUT 'CONTACT 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'CONTACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONTACT (U)
    (PROG (ORD BAS JET PROPS S M SYS)
      (COND
       ((OR (LESSP (LENGTH U) 3) (GREATERP (LENGTH U) 4))
        (RERROR 'EDS 0 "Wrong number of arguments to contact")))
      (SETQ ORD (CAR U))
      (COND
       ((OR (NOT (FIXP ORD)) (LESSP ORD 0))
        (TYPERR ORD "non-negative integer")))
      (SETQ BAS (*A2CFRM (LIST (CAR (SETQ U (CDR U))))))
      (SETQ JET (*A2CFRM (LIST (CAR (SETQ U (CDR U))))))
      (SETQ PROPS
              (COND
               ((CDR U)
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (GETRLIST (CADR U)))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (COND
                                       ((NOT (IDP (CADR X)))
                                        (RERROR 'EDS 0
                                                "Badly formed properties in EDS"))
                                       (T
                                        (CONS (CADR X)
                                              (COND
                                               ((EQCAR (CADDR X) 'LIST)
                                                (CDR
                                                 (INDEXEXPANDEVAL
                                                  (LIST (CADDR X)))))
                                               (T (CADDR X)))))))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (COND
                               ((NOT (IDP (CADR X)))
                                (RERROR 'EDS 0
                                        "Badly formed properties in EDS"))
                               (T
                                (CONS (CADR X)
                                      (COND
                                       ((EQCAR (CADDR X) 'LIST)
                                        (CDR
                                         (INDEXEXPANDEVAL (LIST (CADDR X)))))
                                       (T (CADDR X)))))))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ M (CFRMPROD2 BAS JET))
      (SETQ S
              (MKEDS
               (LIST (LIST)
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F (CADR BAS))
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (F)
                                           (CONS (CONS F (CONS 1 1)) NIL))
                                         (CAR F))
                                        NIL)))
                      LOOPLABEL
                       (SETQ F (CDR F))
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (F) (CONS (CONS F (CONS 1 1)) NIL))
                                 (CAR F))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     M PROPS)))
      (PUTEDS S 'JET0 (UNIQVARS (CADR JET)))
      (PUTEDS S 'SQVAR *SQVAR*)
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'REDUCED 'QUASILINEAR 'PFAFFIAN 'INVOLUTIVE))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (FLAGTRUEEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       ((ALLEXACT (CADR M))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE ORD I)) (RETURN NIL)))
          (PROGN
           (SETQ SYS (CADR S))
           (SETQ S (EDSPROTECT (LIST 'GBSYS S)))
           (SETCAR (CDR S) (APPEND SYS (CADR S))))
          (SETQ I (PLUS2 I 1))
          (GO LAB)))
       (T
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE ORD I)) (RETURN NIL)))
          (SETQ S (EDSPROTECT (LIST 'PROLONGEDS S)))
          (SETQ I (PLUS2 I 1))
          (GO LAB))))
      (RETURN S))) 
(PUT 'GBSYS 'NUMBER-OF-ARGS 1) 
(PUT 'GBSYS 'DEFINED-ON-LINE '80) 
(PUT 'GBSYS 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'GBSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBSYS (S)
    (PROG (PRL DEP IND JET JET0 SYS COB IDXS X CRD M)
      (COND
       ((NOT (NORMALEDSP S))
        (RERROR 'EDS 0 (LIST "System not in normal form"))))
      (SETQ IND (INDKRNS S))
      (SETQ IDXS (UNIQIDS IND))
      (SETQ PRL (PRLKRNS S))
      (SETQ JET (UNIQVARS PRL))
      (SETQ COB (EDSCOB S))
      (SETQ JET0 (OR (GETEDS S 'JET0) JET))
      (COND
       ((NOT (SUBSETP IDXS INDXL*))
        (APPLY1 'INDEXRANGE (LIST (LIST 'EQUAL (GENSYM) (CONS 'LIST IDXS))))))
      (SETQ JET (GBCOORDS JET IDXS JET0 (ALLEXACT COB)))
      (SETQ SYS
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR (PAIR PRL JET))
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (CONS (CAR PR) (CONS 1 1))
                                          (MULTPFSQ
                                           (ZIPPF (CADDR S)
                                            (PROG (C FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ C (CDR PR))
                                              (COND ((NULL C) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (C)
                                                                  (CONS
                                                                   (LIST
                                                                    (CONS
                                                                     (CONS C 1)
                                                                     1))
                                                                   1))
                                                                (CAR C))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ C (CDR C))
                                              (COND
                                               ((NULL C)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (C)
                                                          (CONS
                                                           (LIST
                                                            (CONS (CONS C 1)
                                                                  1))
                                                           1))
                                                        (CAR C))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))
                                           (CONS (MINUS 1) 1))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (CONS (CONS (CAR PR) (CONS 1 1))
                                  (MULTPFSQ
                                   (ZIPPF (CADDR S)
                                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ C (CDR PR))
                                      (COND ((NULL C) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (C)
                                                          (CONS
                                                           (LIST
                                                            (CONS (CONS C 1)
                                                                  1))
                                                           1))
                                                        (CAR C))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ C (CDR C))
                                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (C)
                                                  (CONS
                                                   (LIST (CONS (CONS C 1) 1))
                                                   1))
                                                (CAR C))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                   (CONS (MINUS 1) 1))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (J)
        (SETQ J JET)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (SETQ CRD (UNION J CRD))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ PRL
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C CRD)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C)
                                    (COND
                                     ((EQUAL (SETQ X (CAAR (EXDFK C)))
                                             (LIST 'D C))
                                      X)
                                     (T
                                      (ERRDHH
                                       (LIST "Bad differential" X "from"
                                             (LIST 'D C) "in gbsys")))))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (C)
                            (COND
                             ((EQUAL (SETQ X (CAAR (EXDFK C))) (LIST 'D C)) X)
                             (T
                              (ERRDHH
                               (LIST "Bad differential" X "from" (LIST 'D C)
                                     "in gbsys")))))
                          (CAR C))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PRL (REVERSIP (SETDIFF PRL COB)))
      (SETQ DEP (SETDIFF COB IND))
      (SETQ COB (APPEND DEP (APPEND PRL IND)))
      (SETQ CRD (REVERSIP (SETDIFF CRD (EDSCRD S))))
      (SETQ CRD (APPEND (EDSCRD S) CRD))
      (SETQ M (COPYCFRM (CADDR (CDR S))))
      (SETCAR (CDR M) COB)
      (SETCAR (CDDR M) CRD)
      (SETQ S (COPYEDS S))
      (SETCAR (CDR S) SYS)
      (SETCAR (CDDR (CDR S)) M)
      (PUTEDS S 'JET0 JET0)
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'REDUCED 'QUASILINEAR 'PFAFFIAN))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (FLAGTRUEEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (FLAGFALSEEDS S 'CLOSED)
      (REMPROPEDS S 'INVOLUTIVE)
      (SETQ S (PURGEEDS* S))
      (REMKRNS S)
      (RETURN S))) 
(PUT 'GBCOORDS 'NUMBER-OF-ARGS 4) 
(PUT 'GBCOORDS 'DEFINED-ON-LINE '130) 
(PUT 'GBCOORDS 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'GBCOORDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GBCOORDS (PRLVARS INDIDS JET0 FLG)
    (PROG (C FORALL-RESULT FORALL-ENDPTR)
      (SETQ C PRLVARS)
      (COND ((NULL C) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (C)
                          (PROG (X N)
                            (SETQ N 0)
                            (COND
                             ((EQ JET0 PRLVARS)
                              (SETQ C (LIST (SPLITOFFINDICES C C))))
                             (T
                              (SETQ C
                                      (PROG (C0 FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ C0 JET0)
                                       STARTOVER
                                        (COND ((NULL C0) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                ((LAMBDA (C0)
                                                   (COND
                                                    ((SETQ C0
                                                             (SPLITOFFINDICES
                                                              C0 C))
                                                     (LIST C0))))
                                                 (CAR C0)))
                                        (SETQ FORALL-ENDPTR
                                                (LASTPAIR FORALL-RESULT))
                                        (SETQ C0 (CDR C0))
                                        (COND
                                         ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                       LOOPLABEL
                                        (COND
                                         ((NULL C0) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                ((LAMBDA (C0)
                                                   (COND
                                                    ((SETQ C0
                                                             (SPLITOFFINDICES
                                                              C0 C))
                                                     (LIST C0))))
                                                 (CAR C0)))
                                        (SETQ FORALL-ENDPTR
                                                (LASTPAIR FORALL-ENDPTR))
                                        (SETQ C0 (CDR C0))
                                        (GO LOOPLABEL)))))
                            (COND
                             ((NEQ (LENGTH C) 1)
                              (ERRDHH
                               (LIST "Name conflict in gbcoords:" (LENGTH C)
                                     "matches")))
                             (T (SETQ C (CAR C))))
                            (SETQ N (PLUS (LENGTH (CAR C)) (LENGTH (CDR C))))
                            (COND
                             ((AND (SETQ X (GET (CAAR C) 'IFDEGREE))
                                   (SETQ X (ASSOC N X)) (CDR X))
                              (ERRDHH
                               (LIST "Degree conflict in gbcoords:"
                                     (APPEND (CAR C) (CONS NIL (CDR C))))))
                             (T
                              (MKFORM* (APPEND (CAR C) (CONS NIL (CDR C))) 0)))
                            (RETURN
                             (PROG (I FORALL-RESULT FORALL-ENDPTR)
                               (SETQ I INDIDS)
                               (COND ((NULL I) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (I)
                                                   (PROG (X)
                                                     (SETQ X
                                                             (COND
                                                              ((AND
                                                                (NEQ JET0
                                                                     PRLVARS)
                                                                FLG)
                                                               (PROG (J
                                                                      FORALL-RESULT
                                                                      FORALL-ENDPTR)
                                                                 (SETQ J
                                                                         (SORT
                                                                          (CONS
                                                                           I
                                                                           (FLATINDXL
                                                                            (CDR
                                                                             C)))
                                                                          'INDTORDP))
                                                                 (COND
                                                                  ((NULL J)
                                                                   (RETURN
                                                                    NIL)))
                                                                 (SETQ FORALL-RESULT
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CONS
                                                                                  ((LAMBDA
                                                                                       (
                                                                                        J)
                                                                                     (LIST
                                                                                      'MINUS
                                                                                      J))
                                                                                   (CAR
                                                                                    J))
                                                                                  NIL)))
                                                                LOOPLABEL
                                                                 (SETQ J
                                                                         (CDR
                                                                          J))
                                                                 (COND
                                                                  ((NULL J)
                                                                   (RETURN
                                                                    FORALL-RESULT)))
                                                                 (RPLACD
                                                                  FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (J)
                                                                      (LIST
                                                                       'MINUS
                                                                       J))
                                                                    (CAR J))
                                                                   NIL))
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CDR
                                                                          FORALL-ENDPTR))
                                                                 (GO
                                                                  LOOPLABEL)))
                                                              (T
                                                               (APPEND (CDR C)
                                                                       (LIST
                                                                        (LIST
                                                                         'MINUS
                                                                         I))))))
                                                     (SETQ X
                                                             (CAR
                                                              (FKERN
                                                               (APPEND (CAR C)
                                                                       X))))
                                                     (COND
                                                      ((NEQ (REVAL1 X T) X)
                                                       (TYPERR X
                                                               "free coordinate")))
                                                     (RETURN X)))
                                                 (CAR I))
                                                NIL)))
                              LOOPLABEL
                               (SETQ I (CDR I))
                               (COND ((NULL I) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (I)
                                           (PROG (X)
                                             (SETQ X
                                                     (COND
                                                      ((AND (NEQ JET0 PRLVARS)
                                                            FLG)
                                                       (PROG (J FORALL-RESULT
                                                              FORALL-ENDPTR)
                                                         (SETQ J
                                                                 (SORT
                                                                  (CONS I
                                                                        (FLATINDXL
                                                                         (CDR
                                                                          C)))
                                                                  'INDTORDP))
                                                         (COND
                                                          ((NULL J)
                                                           (RETURN NIL)))
                                                         (SETQ FORALL-RESULT
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CONS
                                                                          ((LAMBDA
                                                                               (
                                                                                J)
                                                                             (LIST
                                                                              'MINUS
                                                                              J))
                                                                           (CAR
                                                                            J))
                                                                          NIL)))
                                                        LOOPLABEL
                                                         (SETQ J (CDR J))
                                                         (COND
                                                          ((NULL J)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (RPLACD FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (J)
                                                                     (LIST
                                                                      'MINUS
                                                                      J))
                                                                   (CAR J))
                                                                  NIL))
                                                         (SETQ FORALL-ENDPTR
                                                                 (CDR
                                                                  FORALL-ENDPTR))
                                                         (GO LOOPLABEL)))
                                                      (T
                                                       (APPEND (CDR C)
                                                               (LIST
                                                                (LIST 'MINUS
                                                                      I))))))
                                             (SETQ X
                                                     (CAR
                                                      (FKERN
                                                       (APPEND (CAR C) X))))
                                             (COND
                                              ((NEQ (REVAL1 X T) X)
                                               (TYPERR X "free coordinate")))
                                             (RETURN X)))
                                         (CAR I))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
                        (CAR C))
                       NIL)))
     LOOPLABEL
      (SETQ C (CDR C))
      (COND ((NULL C) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (C)
                  (PROG (X N)
                    (SETQ N 0)
                    (COND
                     ((EQ JET0 PRLVARS) (SETQ C (LIST (SPLITOFFINDICES C C))))
                     (T
                      (SETQ C
                              (PROG (C0 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ C0 JET0)
                               STARTOVER
                                (COND ((NULL C0) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (C0)
                                           (COND
                                            ((SETQ C0 (SPLITOFFINDICES C0 C))
                                             (LIST C0))))
                                         (CAR C0)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ C0 (CDR C0))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL C0) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (C0)
                                           (COND
                                            ((SETQ C0 (SPLITOFFINDICES C0 C))
                                             (LIST C0))))
                                         (CAR C0)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ C0 (CDR C0))
                                (GO LOOPLABEL)))))
                    (COND
                     ((NEQ (LENGTH C) 1)
                      (ERRDHH
                       (LIST "Name conflict in gbcoords:" (LENGTH C)
                             "matches")))
                     (T (SETQ C (CAR C))))
                    (SETQ N (PLUS (LENGTH (CAR C)) (LENGTH (CDR C))))
                    (COND
                     ((AND (SETQ X (GET (CAAR C) 'IFDEGREE))
                           (SETQ X (ASSOC N X)) (CDR X))
                      (ERRDHH
                       (LIST "Degree conflict in gbcoords:"
                             (APPEND (CAR C) (CONS NIL (CDR C))))))
                     (T (MKFORM* (APPEND (CAR C) (CONS NIL (CDR C))) 0)))
                    (RETURN
                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                       (SETQ I INDIDS)
                       (COND ((NULL I) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (I)
                                           (PROG (X)
                                             (SETQ X
                                                     (COND
                                                      ((AND (NEQ JET0 PRLVARS)
                                                            FLG)
                                                       (PROG (J FORALL-RESULT
                                                              FORALL-ENDPTR)
                                                         (SETQ J
                                                                 (SORT
                                                                  (CONS I
                                                                        (FLATINDXL
                                                                         (CDR
                                                                          C)))
                                                                  'INDTORDP))
                                                         (COND
                                                          ((NULL J)
                                                           (RETURN NIL)))
                                                         (SETQ FORALL-RESULT
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CONS
                                                                          ((LAMBDA
                                                                               (
                                                                                J)
                                                                             (LIST
                                                                              'MINUS
                                                                              J))
                                                                           (CAR
                                                                            J))
                                                                          NIL)))
                                                        LOOPLABEL
                                                         (SETQ J (CDR J))
                                                         (COND
                                                          ((NULL J)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (RPLACD FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (J)
                                                                     (LIST
                                                                      'MINUS
                                                                      J))
                                                                   (CAR J))
                                                                  NIL))
                                                         (SETQ FORALL-ENDPTR
                                                                 (CDR
                                                                  FORALL-ENDPTR))
                                                         (GO LOOPLABEL)))
                                                      (T
                                                       (APPEND (CDR C)
                                                               (LIST
                                                                (LIST 'MINUS
                                                                      I))))))
                                             (SETQ X
                                                     (CAR
                                                      (FKERN
                                                       (APPEND (CAR C) X))))
                                             (COND
                                              ((NEQ (REVAL1 X T) X)
                                               (TYPERR X "free coordinate")))
                                             (RETURN X)))
                                         (CAR I))
                                        NIL)))
                      LOOPLABEL
                       (SETQ I (CDR I))
                       (COND ((NULL I) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (I)
                                   (PROG (X)
                                     (SETQ X
                                             (COND
                                              ((AND (NEQ JET0 PRLVARS) FLG)
                                               (PROG (J FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ J
                                                         (SORT
                                                          (CONS I
                                                                (FLATINDXL
                                                                 (CDR C)))
                                                          'INDTORDP))
                                                 (COND ((NULL J) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (J)
                                                                     (LIST
                                                                      'MINUS
                                                                      J))
                                                                   (CAR J))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ J (CDR J))
                                                 (COND
                                                  ((NULL J)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (J)
                                                             (LIST 'MINUS J))
                                                           (CAR J))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                              (T
                                               (APPEND (CDR C)
                                                       (LIST
                                                        (LIST 'MINUS I))))))
                                     (SETQ X (CAR (FKERN (APPEND (CAR C) X))))
                                     (COND
                                      ((NEQ (REVAL1 X T) X)
                                       (TYPERR X "free coordinate")))
                                     (RETURN X)))
                                 (CAR I))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
                (CAR C))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SPLITOFFINDICES 'NUMBER-OF-ARGS 2) 
(PUT 'SPLITOFFINDICES 'DEFINED-ON-LINE '165) 
(PUT 'SPLITOFFINDICES 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'SPLITOFFINDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPLITOFFINDICES (U V)
    (COND
     ((ATOM U)
      (COND ((EQUAL U V) (CONS (LIST U) (LIST)))
            ((AND (PAIRP V) (EQUAL (CAR V) U)) (CONS (LIST U) (CDR V)))
            (T NIL)))
     ((AND (PAIRP V) (EQUAL (CAR V) (CAR U)))
      (COND ((NULL (CDR U)) (CONS U (CDR V)))
            (T
             ((LAMBDA (X) (COND (X (CONS U (CDR X)))))
              (SPLITOFFINDICES (CDR U) (CDR V)))))))) 
(PUT 'INDTORDP 'NUMBER-OF-ARGS 2) 
(PUT 'INDTORDP 'DEFINED-ON-LINE '185) 
(PUT 'INDTORDP 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'INDTORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDTORDP (U V)
    (PROG (X)
      (SETQ X INDXL*)
     A
      (COND ((NULL X) (RETURN (ORDERP U V))) ((EQ U (CAR X)) (RETURN T))
            ((EQ V (CAR X)) (RETURN NIL)))
      (SETQ X (CDR X))
      (GO A))) 
(PUT 'UNIQIDS 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQIDS 'DEFINED-ON-LINE '197) 
(PUT 'UNIQIDS 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'UNIQIDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQIDS (U)
    (PROG (X)
      (SETQ X
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I U)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (I) (INDEXID I)) (CAR I)) NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (INDEXID I)) (CAR I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((OR (MEMQ NIL X)
            (NOT (ALLEQUAL (SUBLIS (PAIR X (NLIST NIL (LENGTH X))) U))))
        (SETQ X
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I U)
                  (COND ((NULL I) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (I) (PFORMID I)) (CAR I))
                                        NIL)))
                 LOOPLABEL
                  (SETQ I (CDR I))
                  (COND ((NULL I) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (I) (PFORMID I)) (CAR I)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND ((REPEATS X) (ERRDHH "Name conflict in uniqids")))
      (RETURN X))) 
(PUT 'INDEXID 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXID 'DEFINED-ON-LINE '214) 
(PUT 'INDEXID 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'INDEXID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXID (U)
    ((LAMBDA (X) (COND ((AND X (EQUAL (LENGTH X) 1)) (CAR X))))
     (FLATINDXL (INDEXLIST U)))) 
(PUT 'INDEXLIST 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXLIST 'DEFINED-ON-LINE '221) 
(PUT 'INDEXLIST 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'INDEXLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXLIST (U)
    (COND ((ATOM U) NIL)
          ((EQUAL (GET (CAR U) 'RTYPE) 'INDEXED-FORM)
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J (CDR U))
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (J) (REVALIND J)) (CAR J)) NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (J) (REVALIND J)) (CAR J)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          ((GET (CAR U) 'INDEXFUN)
           (INDEXLIST (APPLY1 (GET (CAR U) 'INDEXFUN) (CDR U))))
          ((EQ (CAR U) 'PARTDF)
           (COND
            ((NULL (CDDR U))
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J (INDEXLIST (CDR U)))
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J) (REVALIND (LIST 'MINUS J)))
                                 (CAR J))
                                NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (REVALIND (LIST 'MINUS J))) (CAR J))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
            (T
             (APPEND (INDEXLIST (CADR U))
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (INDEXLIST (CDDR U)))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J)
                                           (REVALIND (LIST 'MINUS J)))
                                         (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J) (REVALIND (LIST 'MINUS J)))
                                 (CAR J))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))
          (T (APPEND (INDEXLIST (CAR U)) (INDEXLIST (CDR U)))))) 
(PUT 'PFORMID 'NUMBER-OF-ARGS 1) 
(PUT 'PFORMID 'DEFINED-ON-LINE '239) 
(PUT 'PFORMID 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'PFORMID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFORMID (U)
    ((LAMBDA (X)
       (COND ((ATOM X) X)
             (T
              (INTERN
               (COMPRESS
                (PROG (A FORALL-RESULT FORALL-ENDPTR)
                  (SETQ A (FLATINDXL X))
                 STARTOVER
                  (COND ((NULL A) (RETURN NIL)))
                  (SETQ FORALL-RESULT ((LAMBDA (A) (EXPLODE A)) (CAR A)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ A (CDR A))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL A) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR ((LAMBDA (A) (EXPLODE A)) (CAR A)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ A (CDR A))
                  (GO LOOPLABEL)))))))
     (PFORMVAR U))) 
(PUT 'UNIQVARS 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQVARS 'DEFINED-ON-LINE '247) 
(PUT 'UNIQVARS 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'UNIQVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQVARS (U)
    (COND
     ((REPEATS
       (SETQ U
               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                 (SETQ K U)
                 (COND ((NULL K) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (K) (PFORMVAR K)) (CAR K))
                                       NIL)))
                LOOPLABEL
                 (SETQ K (CDR K))
                 (COND ((NULL K) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (K) (PFORMVAR K)) (CAR K)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (ERRDHH "Name conflict in uniqvars"))
     (T U))) 
(PUT 'PFORMVAR 'NUMBER-OF-ARGS 1) 
(PUT 'PFORMVAR 'DEFINED-ON-LINE '255) 
(PUT 'PFORMVAR 'DEFINED-IN-FILE 'EDS/CONTACT.RED) 
(PUT 'PFORMVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFORMVAR (U)
    (COND ((OR (ATOM U) (EQUAL (GET (CAR U) 'RTYPE) 'INDEXED-FORM)) U)
          ((AND (MEMQ (CAR U) '(D HODGE PARTDF)) (NULL (CDDR U)))
           (PFORMVAR (CADR U)))
          (T (ERRDHH (LIST "No unique variable in " U))))) 
(ENDMODULE) 
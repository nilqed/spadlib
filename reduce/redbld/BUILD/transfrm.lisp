(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRANSFRM)) 
(FLUID '(XVARS* KORD* SUBFG*)) 
(GLOBAL '(*SQVAR*)) 
(FLUID '(CFRMCOB* CFRMCRD* CFRMDRV* CFRMRSX* *EDSSLOPPY)) 
(PUT '*A2XFORM 'NUMBER-OF-ARGS 1) 
(PUT '*A2XFORM 'DEFINED-ON-LINE '48) 
(PUT '*A2XFORM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT '*A2XFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2XFORM (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J (GETRLIST U))
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND
                           ((EQEXPR J)
                            ((LAMBDA (SUBFG*)
                               (CONS (*A2K (LHS J)) (XPARTITOP (RHS J))))
                             NIL))
                           (T (RERROR 'EDS 0 "Incorrectly formed transform"))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND
                   ((EQEXPR J)
                    ((LAMBDA (SUBFG*)
                       (CONS (*A2K (LHS J)) (XPARTITOP (RHS J))))
                     NIL))
                   (T (RERROR 'EDS 0 "Incorrectly formed transform"))))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT '*XFORM2MAP 'NUMBER-OF-ARGS 1) 
(PUT '*XFORM2MAP 'DEFINED-ON-LINE '57) 
(PUT '*XFORM2MAP 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT '*XFORM2MAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *XFORM2MAP (X)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P X)
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (P) (CONS (CAR P) (MK*SQ (*PF2SQ (CDR P)))))
                        (CAR P))
                       NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (P) (CONS (CAR P) (MK*SQ (*PF2SQ (CDR P))))) (CAR P))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT '*MAP2XFORM 'NUMBER-OF-ARGS 1) 
(PUT '*MAP2XFORM 'DEFINED-ON-LINE '63) 
(PUT '*MAP2XFORM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT '*MAP2XFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *MAP2XFORM (U)
    ((LAMBDA (SUBFG*)
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X U)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (CONS (CAR U) (XPARTITOP (CADR U))))
                           (CAR X))
                          NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (X) (CONS (CAR U) (XPARTITOP (CADR U)))) (CAR X))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
     NIL)) 
(PUT '*XFORM2DRV 'NUMBER-OF-ARGS 1) 
(PUT '*XFORM2DRV 'DEFINED-ON-LINE '70) 
(PUT '*XFORM2DRV 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT '*XFORM2DRV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *XFORM2DRV (X)
    ((LAMBDA (SUBFG*)
       (PROG (P FORALL-RESULT FORALL-ENDPTR)
         (SETQ P X)
         (COND ((NULL P) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (P)
                             (LIST 'REPLACEBY (CAR P) (*PF2A (CDR P))))
                           (CAR P))
                          NIL)))
        LOOPLABEL
         (SETQ P (CDR P))
         (COND ((NULL P) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (P) (LIST 'REPLACEBY (CAR P) (*PF2A (CDR P))))
                   (CAR P))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
     NIL)) 
(PUT '*XFORM2SYS 'NUMBER-OF-ARGS 1) 
(PUT '*XFORM2SYS 'DEFINED-ON-LINE '77) 
(PUT '*XFORM2SYS 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT '*XFORM2SYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *XFORM2SYS (X)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P X)
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (P)
                          (ADDPF (CONS (CONS (CAR P) (CONS 1 1)) NIL)
                                 (MULTPFSQ (CDR P) (CONS (MINUS 1) 1))))
                        (CAR P))
                       NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (P)
                  (ADDPF (CONS (CONS (CAR P) (CONS 1 1)) NIL)
                         (MULTPFSQ (CDR P) (CONS (MINUS 1) 1))))
                (CAR P))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'TRANSFORM 'RTYPEFN 'GETRTYPECAR) 
(PUT 'TRANSFORM 'CFRMFN 'TRANSFORMCFRM) 
(PUT 'TRANSFORM 'EDSFN 'TRANSFORMEDS) 
(PUT 'TRANSFORMCFRM 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFORMCFRM 'DEFINED-ON-LINE '90) 
(PUT 'TRANSFORMCFRM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'TRANSFORMCFRM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFORMCFRM (M X) (CHECKCFRM (XFORMCFRM M (*A2XFORM X)))) 
(PUT 'TRANSFORMEDS 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFORMEDS 'DEFINED-ON-LINE '96) 
(PUT 'TRANSFORMEDS 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'TRANSFORMEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFORMEDS (S X) (XFORMEDS S (*A2XFORM X))) 
(PUT 'XFORMCFRM 'NUMBER-OF-ARGS 2) 
(PUT 'XFORMCFRM 'DEFINED-ON-LINE '102) 
(PUT 'XFORMCFRM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMCFRM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XFORMCFRM (M X)
    ((LAMBDA (U) (XFORMCFRM1 M (CAR U) (CADR U) (CADDR U)))
     (GETXFORM X (CADR M)))) 
(PUT 'XFORMCFRM1 'NUMBER-OF-ARGS 4) 
(PUT 'XFORMCFRM1 'DEFINED-ON-LINE '110) 
(PUT 'XFORMCFRM1 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMCFRM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE XFORMCFRM1 (M X Y NEW)
    (PROG (P Z)
      (SETQ M (COPYCFRM M))
      (SETQ Z
              (PAIR
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
                 (GO LOOPLABEL))
               NEW))
      (SETCAR (CDR M)
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (CADR M))
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (COND ((SETQ P (ATSOC K Z)) (CDR P))
                                          (T K)))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (COND ((SETQ P (ATSOC K Z)) (CDR P)) (T K)))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR M)
              (REVERSE
               (UNION
                (PROG (K FORALL-RESULT FORALL-ENDPTR)
                  (SETQ K NEW)
                 STARTOVER
                  (COND ((NULL K) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                           (CAR K)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ K (CDR K))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL K) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                           (CAR K)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ K (CDR K))
                  (GO LOOPLABEL))
                (REVERSE (CADDR M)))))
      (SETCAR (CDDR (CDR M))
              (APPEND (XFORMDRV (CADDR (CDR M)) X)
                      (APPEND
                       (*XFORM2DRV
                        (PROG (P FORALL-RESULT FORALL-ENDPTR)
                          (SETQ P X)
                         STARTOVER
                          (COND ((NULL P) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  ((LAMBDA (P)
                                     (COND ((EXACT (CAR P)) (LIST P))))
                                   (CAR P)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                          (SETQ P (CDR P))
                          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                         LOOPLABEL
                          (COND ((NULL P) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  ((LAMBDA (P)
                                     (COND ((EXACT (CAR P)) (LIST P))))
                                   (CAR P)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                          (SETQ P (CDR P))
                          (GO LOOPLABEL)))
                       (STRUCTEQNS Y X))))
      (COND (*EDSSLOPPY (SETQ M (UPDATERSX M))))
      (SETQ M (PURGECFRM M))
      (RETURN M))) 
(PUT 'XFORMCFRM0 'NUMBER-OF-ARGS 3) 
(PUT 'XFORMCFRM0 'DEFINED-ON-LINE '134) 
(PUT 'XFORMCFRM0 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMCFRM0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XFORMCFRM0 (M X NEW)
    (PROG (P Z)
      (SETQ M (COPYCFRM M))
      (SETQ Z
              (PAIR
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
                 (GO LOOPLABEL))
               NEW))
      (SETCAR (CDR M)
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (CADR M))
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (COND ((SETQ P (ATSOC K Z)) (CDR P))
                                          (T K)))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (COND ((SETQ P (ATSOC K Z)) (CDR P)) (T K)))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR M)
              (REVERSE
               (UNION
                (PROG (K FORALL-RESULT FORALL-ENDPTR)
                  (SETQ K NEW)
                 STARTOVER
                  (COND ((NULL K) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                           (CAR K)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ K (CDR K))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL K) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                           (CAR K)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ K (CDR K))
                  (GO LOOPLABEL))
                (REVERSE (CADDR M)))))
      (COND (*EDSSLOPPY (SETQ M (UPDATERSX M))))
      (SETQ M (PURGECFRM M))
      (RETURN M))) 
(PUT 'XFORMDRV 'NUMBER-OF-ARGS 2) 
(PUT 'XFORMDRV 'DEFINED-ON-LINE '154) 
(PUT 'XFORMDRV 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMDRV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XFORMDRV (D X) ((LAMBDA (SUBFG*) (PULLBACKDRV D (*XFORM2MAP X))) NIL)) 
(PUT 'UPDATERSX 'NUMBER-OF-ARGS 1) 
(PUT 'UPDATERSX 'DEFINED-ON-LINE '162) 
(PUT 'UPDATERSX 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'UPDATERSX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UPDATERSX (M)
    (PROG ()
      (SETQ M (COPYCFRM M))
      (SETCAR (CDDR (CDDR M))
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (PURGE CFRMRSX*))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (*PF2A F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (*PF2A F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN M))) 
(PUT 'XFORMEDS 'NUMBER-OF-ARGS 2) 
(PUT 'XFORMEDS 'DEFINED-ON-LINE '173) 
(PUT 'XFORMEDS 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XFORMEDS (S X)
    ((LAMBDA (U) (XFORMEDS1 S (CAR U) (CADR U) (CADDR U)))
     (GETXFORM X (EDSCOB S)))) 
(PUT 'XFORMEDS1 'NUMBER-OF-ARGS 4) 
(PUT 'XFORMEDS1 'DEFINED-ON-LINE '183) 
(PUT 'XFORMEDS1 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMEDS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE XFORMEDS1 (S X Y NEW)
    (PROG (K)
      (SETQ S (COPYEDS S))
      (SETCAR (CDDR (CDR S)) (XFORMCFRM1 (CADDR (CDR S)) X Y NEW))
      (SETQ K
              (UPDKORDL
               (APPEND
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
                  (GO LOOPLABEL))
                NEW)))
      (SETQ X (*XFORM2SYS X))
      (SETCAR (CDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (XREDUCE (XREORDER F) X))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREDUCE (XREORDER F) X)) (CAR F))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (XREDUCE (XREORDER F) X))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREDUCE (XREORDER F) X)) (CAR F))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (REMKRNS S)
      (SETQ S (PURGEEDS* S))
      (REMPROPEDS S 'JET0)
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'REDUCED))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (REMPROPEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETKORDER K)
      (SETQ S (NORMALEDS S))
      (SETCFRM (EDS_CFRM* S))
      (RETURN S))) 
(PUT 'XFORMEDS0 'NUMBER-OF-ARGS 3) 
(PUT 'XFORMEDS0 'DEFINED-ON-LINE '209) 
(PUT 'XFORMEDS0 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMEDS0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XFORMEDS0 (S X NEW)
    (PROG (K)
      (SETQ S (COPYEDS S))
      (SETCAR (CDDR (CDR S)) (XFORMCFRM0 (CADDR (CDR S)) X NEW))
      (SETQ K
              (UPDKORDL
               (APPEND
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
                  (GO LOOPLABEL))
                NEW)))
      (SETQ X (*XFORM2SYS X))
      (SETCAR (CDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (XREDUCE (XREORDER F) X))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREDUCE (XREORDER F) X)) (CAR F))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (XREDUCE (XREORDER F) X))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREDUCE (XREORDER F) X)) (CAR F))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (REMKRNS S)
      (SETQ S (PURGEEDS* S))
      (REMPROPEDS S 'JET0)
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'REDUCED))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (REMPROPEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETKORDER K)
      (SETQ S (NORMALEDS S))
      (SETCFRM (EDS_CFRM* S))
      (RETURN S))) 
(PUT 'GETXFORM 'NUMBER-OF-ARGS 2) 
(PUT 'GETXFORM 'DEFINED-ON-LINE '235) 
(PUT 'GETXFORM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'GETXFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETXFORM (X COB)
    (PROG (OLD NEW Y)
      (PROG (P)
        (SETQ P X)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ NEW (UNION (XPOWS (CDR P)) NEW))
            (SETQ OLD (CONS (CAR P) OLD))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       ((NOT (XNP OLD COB))
        (PROGN
         (SETQ Y X)
         (SETQ X (INVXFORM X))
         (SETQ NEW OLD)
         (SETQ OLD
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
                   (GO LOOPLABEL))))))
      (SETQ NEW (SORT (SETDIFF NEW COB) 'TERMORDP))
      (EDSDEBUG "New cobasis elements..." NEW 'COB)
      (EDSDEBUG "... replacing old cobasis elements" OLD 'COB)
      (COND
       ((OR (NEQ (LENGTH NEW) (LENGTH OLD)) (NOT (SUBSETP OLD COB)))
        (RERROR 'EDS 0 "Bad cobasis transformation")))
      (COND ((AND (NOT (ALLEXACT NEW)) (NULL Y)) (SETQ Y (INVXFORM X))))
      (RETURN (LIST X Y NEW)))) 
(PUT 'XFORMDRVEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'XFORMDRVEVAL 'DEFINED-ON-LINE '264) 
(PUT 'XFORMDRVEVAL 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMDRVEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XFORMDRVEVAL (U)
    (PROG (X Y K)
      (SETQ Y (*A2XFORM (CAR U)))
      (SETQ X (COND ((CDR U) (*A2XFORM (CADR U))) (T (INVXFORM Y))))
      (SETQ K
              (UPDKORDL
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
                 (GO LOOPLABEL))))
      (SETQ Y (STRUCTEQNS Y X))
      (SETKORDER K)
      (RETURN (CONS 'LIST Y)))) 
(PUT 'XFORMDRVEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'XFORMDRVEVAL 'DEFINED-ON-LINE '275) 
(PUT 'XFORMDRVEVAL 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'XFORMDRVEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XFORMDRVEVAL (U)
    (PROG (X Y XVARS*)
      (SETQ Y (*A2XFORM (CAR U)))
      (SETQ X (COND ((CDR U) (*A2XFORM (CADR U))) (T (INVXFORM Y))))
      (SETQ Y (STRUCTEQNS Y X))
      (RETURN (CONS 'LIST Y)))) 
(PUT 'STRUCTEQNS 'NUMBER-OF-ARGS 2) 
(PUT 'STRUCTEQNS 'DEFINED-ON-LINE '285) 
(PUT 'STRUCTEQNS 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'STRUCTEQNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STRUCTEQNS (Y X)
    (PROG (OK)
      (SETQ OK
              (UPDKORDL
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
                 (GO LOOPLABEL))))
      (SETQ X (*XFORM2SYS X))
      (SETQ Y
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P Y)
               STARTOVER
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (P)
                           (COND
                            ((NOT (EXACT (CAR P)))
                             (LIST
                              (LIST 'REPLACEBY (LIST 'D (CAR P))
                                    (*PF2A (XREDUCE (EXDFPF (CDR P)) X)))))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ P (CDR P))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (P)
                           (COND
                            ((NOT (EXACT (CAR P)))
                             (LIST
                              (LIST 'REPLACEBY (LIST 'D (CAR P))
                                    (*PF2A (XREDUCE (EXDFPF (CDR P)) X)))))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ P (CDR P))
                (GO LOOPLABEL)))
      (SETKORDER OK)
      (RETURN Y))) 
(PUT 'STRUCTEQNS 'NUMBER-OF-ARGS 2) 
(PUT 'STRUCTEQNS 'DEFINED-ON-LINE '301) 
(PUT 'STRUCTEQNS 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'STRUCTEQNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STRUCTEQNS (Y X)
    (PROG (OK)
      (SETQ OK
              (UPDKORDL
               (SORT
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
                  (GO LOOPLABEL))
                (FUNCTION ORDOP))))
      (SETQ X (*XFORM2SYS X))
      (SETQ Y
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P Y)
               STARTOVER
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (P)
                           (COND
                            ((NOT (EXACT (CAR P)))
                             (LIST
                              (LIST 'REPLACEBY (LIST 'D (CAR P))
                                    (*PF2A (XREDUCE (EXDFPF (CDR P)) X)))))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ P (CDR P))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (P)
                           (COND
                            ((NOT (EXACT (CAR P)))
                             (LIST
                              (LIST 'REPLACEBY (LIST 'D (CAR P))
                                    (*PF2A (XREDUCE (EXDFPF (CDR P)) X)))))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ P (CDR P))
                (GO LOOPLABEL)))
      (SETKORDER OK)
      (RETURN Y))) 
(PUT 'INVERT 'RTYPEFN 'QUOTELIST) 
(PUT 'INVERT 'LISTFN 'INVERTEVAL) 
(PUT 'INVERTEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'INVERTEVAL 'DEFINED-ON-LINE '323) 
(PUT 'INVERTEVAL 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'INVERTEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVERTEVAL (U V)
    (CONS 'LIST
          (PROG (P FORALL-RESULT FORALL-ENDPTR)
            (SETQ P (INVXFORM (*A2XFORM (SETQ U (REVAL1 (CAR U) T)))))
            (COND ((NULL P) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (P)
                                (LIST 'EQUAL (CAR P) (*PF2A1 (CDR P) V)))
                              (CAR P))
                             NIL)))
           LOOPLABEL
            (SETQ P (CDR P))
            (COND ((NULL P) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (P) (LIST 'EQUAL (CAR P) (*PF2A1 (CDR P) V)))
                      (CAR P))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'INVXFORM 'NUMBER-OF-ARGS 1) 
(PUT 'INVXFORM 'DEFINED-ON-LINE '330) 
(PUT 'INVXFORM 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'INVXFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVXFORM (X)
    (PROG (OLD Y K SUBFG*)
      (SETQ SUBFG* NIL)
      (PROG (P)
        (SETQ P X)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ OLD (UNION (XPOWS (CDR P)) OLD))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ OLD (SORT OLD 'TERMORDP))
      (SETQ K
              (UPDKORDL
               (APPEND OLD
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
                         (GO LOOPLABEL)))))
      (EDSDEBUG "Inverting transform" NIL NIL)
      (SETQ Y (SOLVEPFSYS1 (*XFORM2SYS X) OLD))
      (COND
       ((CADR Y) (RERROR 'EDS 0 "Cobasis transform could not be inverted")))
      (SETKORDER K)
      (RETURN
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F (CAR Y))
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (F)
                             (CONS (CAAR F)
                                   (MULTPFSQ (XREORDER (CDR F))
                                             (CONS (MINUS 1) 1))))
                           (CAR F))
                          NIL)))
        LOOPLABEL
         (SETQ F (CDR F))
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (F)
                     (CONS (CAAR F)
                           (MULTPFSQ (XREORDER (CDR F)) (CONS (MINUS 1) 1))))
                   (CAR F))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'TMPIND 'NUMBER-OF-ARGS 1) 
(PUT 'TMPIND 'DEFINED-ON-LINE '350) 
(PUT 'TMPIND 'DEFINED-IN-FILE 'EDS/TRANSFRM.RED) 
(PUT 'TMPIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TMPIND (S)
    (PROG (NEW X)
      (COND ((SINGLETERMS (CADDR S)) (RETURN (LIST S NIL))))
      (SETQ NEW
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (MKFORM* (INTERN (GENSYM)) 1))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F) (MKFORM* (INTERN (GENSYM)) 1)) (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (INVXFORM (PAIR NEW (CADDR S))))
      (UPDKORDL
       (PROG (P FORALL-RESULT FORALL-ENDPTR)
         (SETQ P X)
         (COND ((NULL P) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL)))
        LOOPLABEL
         (SETQ P (CDR P))
         (COND ((NULL P) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
      (RETURN (LIST (XFORMEDS0 S X NEW) X)))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'VARDF)) 
(FLUID '(DEPL* KORD*)) 
(GLOBAL '(KEEPL* BNDEQ*)) 
(PUT 'SIMPVARDF 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPVARDF 'DEFINED-ON-LINE '34) 
(PUT 'SIMPVARDF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'SIMPVARDF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPVARDF (U)
    (COND ((INDVARPF (CAR (SIMP0 (CADR U)))) (MKSQ (CONS 'VARDF U) 1))
          (T
           (PROG (B R S V W X Y Z)
             (SETQ V (*A2K (CADR U)))
             (COND
              ((NULL (CDDR U))
               (SETQ W
                       (INTERN
                        (COMPRESS
                         (APPEND (EXPLODE '|'|)
                                 (EXPLODE (COND ((ATOM V) V) (T (CAR V)))))))))
              (T (SETQ W (CADDR U))))
             (COND ((NULL (ATOM V)) (SETQ W (CONS W (CDR V)))))
             (PUTFORM W (PREPF (DEG*FORM V)))
             (SETQ S (MKSGNSQ (ADDF (DEG*FORM W) 1)))
             (SETQ KORD* (APPEND (LIST (SETQ W (*A2K W))) KORD*))
             (COND
              ((SETQ X (ASSOC V DEPL*))
               (PROG (J)
                 (SETQ J (CDR X))
                LAB
                 (COND ((NULL J) (RETURN NIL)))
                 ((LAMBDA (J) (DEPEND1 W J T)) (CAR J))
                 (SETQ J (CDR J))
                 (GO LAB))))
             (SETQ X (VARYSQ (SIMP* (CAR U)) V W))
             (SETQ B (SETQ Y (CONS NIL 1)))
             (PROG ()
              WHILELABEL
               (COND ((NOT X) (RETURN NIL)))
               (COND
                ((EQ (SETQ Z (CAAAR (CAAR X))) W)
                 (PROGN (SETQ Y (ADDSQ (CDAR X) Y)) (SETQ X (CDR X))))
                ((EQCAR Z 'WEDGE)
                 (COND
                  ((EQ (CADR Z) W)
                   (PROGN
                    (SETQ Y
                            (ADDSQ
                             (MULTSQ
                              (CONS
                               (LIST
                                (CONS
                                 (GETPOWER (FKERN (CONS 'WEDGE (CDDR Z))) 1)
                                 1))
                               1)
                              (CDAR X))
                             Y))
                    (SETQ X (CDR X))))
                  ((EQCAR (CADR Z) 'D)
                   (PROGN
                    (SETQ Y
                            (ADDSQ
                             (SIMP
                              (LIST 'WEDGE
                                    (LIST 'D
                                          (LIST 'TIMES (CONS 'WEDGE (CDDR Z))
                                                (PREPSQ
                                                 (MULTSQ (CDAR X) S))))))
                             Y))
                    (SETQ B
                            (ADDSQ
                             (MULTSQ
                              (CONS
                               (LIST
                                (CONS
                                 (GETPOWER
                                  (FKERN (CONS 'WEDGE (CONS W (CDDR Z)))) 1)
                                 1))
                               1)
                              (CDAR X))
                             B))
                    (SETQ X (CDR X))))
                  (T
                   (RERROR 'EXCALC 11
                           (LIST "Could not move" W "to the left in" Z)))))
                ((EQCAR Z 'PARTDF)
                 (PROGN
                  (SETQ R
                          (REVAL1
                           (LIST 'INNERPROD (LIST 'PARTDF (CADDR Z))
                                 (PREPSQ (CDAR X)))
                           T))
                  (SETQ X
                          (ADDPSF
                           (CONS
                            (CONS
                             (COND
                              ((CDDDR Z)
                               (LIST
                                (CONS
                                 (GETPOWER
                                  (FKERN (CONS 'PARTDF (CONS W (CDDDR Z)))) 1)
                                 1)))
                              (T (LIST (CONS (CONS W 1) 1))))
                             (NEGSQ (SIMP (LIST 'D R))))
                            NIL)
                           (CDR X)))
                  (SETQ B
                          (ADDSQ
                           (MULTSQ
                            (COND
                             ((CDDDR Z)
                              (CONS
                               (LIST
                                (CONS
                                 (GETPOWER
                                  (FKERN (CONS 'PARTDF (CONS W (CDDDR Z)))) 1)
                                 1))
                               1))
                             (T (CONS (LIST (CONS (CONS W 1) 1)) 1)))
                            (SIMP R))
                           B))))
                ((EQCAR Z 'D)
                 (PROGN
                  (SETQ Y
                          (ADDSQ (SIMP (LIST 'D (PREPSQ (MULTSQ (CDAR X) S))))
                                 Y))
                  (SETQ B
                          (ADDSQ
                           (MULTSQ (CONS (LIST (CONS (CONS W 1) 1)) 1)
                                   (CDAR X))
                           B))
                  (SETQ X (CDR X))))
                ((EQUAL Z 1)
                 (PROGN
                  (PROG (P)
                    (SETQ P (CAR (CDAR X)))
                   LAB
                    (COND ((NULL P) (RETURN NIL)))
                    (COND
                     ((EQ (CAAAR P) W)
                      (SETQ Y (ADDSQ (CONS (CDAR P) (CDR (CDAR X))) Y)))
                     ((EQCAR (CAAAR P) 'HODGE)
                      (COND
                       ((EQCAR (CADR (CAAAR P)) 'WEDGE)
                        (PROGN
                         (COND
                          ((AND (EQCAR (CADADR (CAAAR P)) 'D)
                                (EQ (CADR (CADADR (CAAAR P))) W))
                           (PROGN
                            (SETQ Y
                                    (ADDSQ
                                     (MULTSQ
                                      (SIMP
                                       (LIST 'D
                                             (LIST 'TIMES
                                                   (CONS 'WEDGE
                                                         (CDDADR (CAAAR P)))
                                                   (LIST 'HODGE
                                                         (PREPF (CDAR P))))))
                                      (MULTSQ S (CONS 1 (CDR (CDAR X)))))
                                     Y))
                            (SETQ B
                                    (ADDSQ
                                     (MULTSQ
                                      (SIMP
                                       (LIST 'TIMES
                                             (CONS 'WEDGE
                                                   (CONS W (CDDADR (CAAAR P))))
                                             (LIST 'HODGE (PREPF (CDAR P)))))
                                      (CONS 1 (CDR (CDAR X))))
                                     B))))
                          ((EQ (CADADR (CAAAR P)) W)
                           (SETQ Y
                                   (ADDSQ
                                    (MULTSQ
                                     (SIMP
                                      (LIST 'TIMES
                                            (CONS 'WEDGE (CDDADR (CAAAR P)))
                                            (LIST 'HODGE (PREPF (CDAR P)))))
                                     (CONS 1 (CDR (CDAR X))))
                                    Y)))
                          (T
                           (REDERR
                            (LIST "Unexpected expression in vardf" P)))))))))
                    (SETQ P (CDR P))
                    (GO LAB))
                  (SETQ X (CDR X)))))
               (GO WHILELABEL))
             (SETQ KORD* (CDR KORD*))
             (SETQ BNDEQ*
                     (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (MK*SQ (RESIMP B))))
             (RETURN Y))))) 
(PUT 'VARDF 'SIMPFN 'SIMPVARDF) 
(PUT 'VARDF 'RTYPEFN 'GETRTYPEOR) 
(PUT 'VARDF 'PARTITFN 'PARTITVARDF) 
(PUT 'PARTITVARDF 'NUMBER-OF-ARGS 1) 
(PUT 'PARTITVARDF 'DEFINED-ON-LINE '113) 
(PUT 'PARTITVARDF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'PARTITVARDF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARTITVARDF (U) (PARTITSQ* (SIMPVARDF U))) 
(PUT 'VARYSQ 'NUMBER-OF-ARGS 3) 
(PUT 'VARYSQ 'DEFINED-ON-LINE '116) 
(PUT 'VARYSQ 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYSQ (U V W)
    (MULTPSF
     (ADDPSF (VARYF (CAR U) V W)
      (MULTPSF (CONS (CONS 1 U) NIL) (VARYF (NEGF (CDR U)) V W)))
     (CONS (CONS 1 (CONS 1 (CDR U))) NIL))) 
(PUT 'VARYF 'NUMBER-OF-ARGS 3) 
(PUT 'VARYF 'DEFINED-ON-LINE '121) 
(PUT 'VARYF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYF (U V W)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (ADDPSF
            (ADDPSF
             (MULTPSF (CONS (CONS 1 (CONS (LIST (CONS (CAAR U) 1)) 1)) NIL)
              (VARYF (CDAR U) V W))
             (MULTPSF (VARYP (CAAR U) V W)
              (CONS (CONS 1 (CONS (CDAR U) 1)) NIL)))
            (VARYF (CDR U) V W))))) 
(PUT 'VARYP 'NUMBER-OF-ARGS 3) 
(PUT 'VARYP 'DEFINED-ON-LINE '129) 
(PUT 'VARYP 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYP (U V W)
    (PROG (X Z N)
      (SETQ N 0)
      (SETQ N (CDR U))
      (SETQ U (CAR U))
      (COND
       ((EQ U V)
        (SETQ Z (CONS (CONS (LIST (CONS (CONS W 1) 1)) (CONS 1 1)) NIL)))
       ((ATOMF U)
        (COND
         ((SETQ X (ASSOC U KEEPL*))
          (PROG (ALGLIST*)
            (SETQ ALGLIST* (CONS NIL NIL))
            (SETQ Z (VARYSQ (SIMP0 (CDR X)) V W))))
         ((AND (NULL (ATOM U)) (NULL (ATOM V)))
          (COND
           ((EQUAL U V)
            (CONS (CONS (LIST (CONS (CONS W 1) 1)) (CONS 1 1)) NIL))
           (T NIL)))
         ((NULL (ATOM V)) NIL)
         ((DEPENDS U V)
          (SETQ Z
                  (CONS
                   (CONS (LIST (CONS (CONS W 1) 1)) (SIMP (LIST 'PARTDF U V)))
                   NIL)))
         (T NIL)))
       ((SFP U) (SETQ Z (VARYF U V W)))
       ((EQ (CAR U) '*SQ) (SETQ Z (VARYSQ (CADR U) V W)))
       ((SETQ X (GET (CAR U) (DFN_PROP U)))
        (PROG (J)
          (SETQ J
                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                    (SETQ K (CDR U))
                    (COND ((NULL K) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (K) (VARYSQ (SIMP K) V W))
                                      (CAR K))
                                     NIL)))
                   LOOPLABEL
                    (SETQ K (CDR K))
                    (COND ((NULL K) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (K) (VARYSQ (SIMP K) V W)) (CAR K))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J)
             (PROGN
              (COND
               (J
                (SETQ Z
                        (ADDPSF
                         (MULTPSF J
                          (CONS
                           (CONS 1
                                 (SIMP
                                  (SUBLA (PAIR (CAAR X) (CDR U)) (CDAR X))))
                           NIL))
                         Z))))
              (SETQ X (CDR X))))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB)))
       ((SETQ X (GET (CAR U) 'VARYFN)) (SETQ Z (APPLY3 X (CDR U) V W)))
       ((NDEPENDS U V)
        (SETQ Z
                (CONS
                 (CONS (LIST (CONS (CONS W 1) 1)) (SIMP (LIST 'PARTDF U V)))
                 NIL)))
       (T NIL))
      (RETURN
       (COND ((EQUAL N 1) Z)
             (T
              (MULTPSF
               (CONS
                (CONS 1 (CONS (LIST (CONS (CONS U (DIFFERENCE N 1)) N)) 1))
                NIL)
               Z)))))) 
(PUT '*PF2PSF 'NUMBER-OF-ARGS 2) 
(PUT '*PF2PSF 'DEFINED-ON-LINE '164) 
(PUT '*PF2PSF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT '*PF2PSF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *PF2PSF (U V)
    (COND ((NULL U) NIL) ((OR (ATOM U) (ATOM (CAR U))) (MULTSQ (CONS U 1) V))
          (T
           (CONS (CONS (LIST (CONS (CONS (CAAR U) 1) 1)) (MULTSQ (CDAR U) V))
                 (*PF2PSF (CDR U) V))))) 
(PUT 'VARYWEDGE 'NUMBER-OF-ARGS 3) 
(PUT 'VARYWEDGE 'DEFINED-ON-LINE '169) 
(PUT 'VARYWEDGE 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYWEDGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYWEDGE (U V W)
    (PROG (X Y Z)
      (SETQ X (LIST 'WEDGE))
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (PROG ()
          (SETQ Y (VARYSQ (SIMP (CAR J)) V W))
         A
          (COND
           (Y
            (SETQ Z
                    (ADDPSF
                     (*PF2PSF
                      (PARTITOP (APPEND X (CONS (PREPF (CAAR Y)) (CDR J))))
                      (CDAR Y))
                     Z))))
          (COND ((AND Y (SETQ Y (CDR Y))) (GO A)))
          (SETQ X (APPEND X (LIST (CAR J)))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN Z))) 
(PUT 'WEDGE 'VARYFN 'VARYWEDGE) 
(PUT 'VARYEXDF 'NUMBER-OF-ARGS 3) 
(PUT 'VARYEXDF 'DEFINED-ON-LINE '191) 
(PUT 'VARYEXDF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYEXDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYEXDF (U V W)
    (PROG (X)
      (PROG (J)
        (SETQ J (VARYSQ (SIMP (CAR U)) V W))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         (J
          (SETQ X
                  (ADDPSF
                   (*PF2PSF (PARTITOP (LIST 'D (CAAAR (CAAR J)))) (CDAR J))
                   X))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN X))) 
(PUT 'D 'VARYFN 'VARYEXDF) 
(PUT 'VARYHODGE 'NUMBER-OF-ARGS 3) 
(PUT 'VARYHODGE 'DEFINED-ON-LINE '201) 
(PUT 'VARYHODGE 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYHODGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYHODGE (U V W)
    (PROG (X)
      (PROG (J)
        (SETQ J (VARYSQ (SIMP (CAR U)) V W))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         (J
          (SETQ X
                  (ADDPSF
                   (*PF2PSF (PARTITOP (LIST 'HODGE (CAAAR (CAAR J)))) (CDAR J))
                   X))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN X))) 
(PUT 'HODGE 'VARYFN 'VARYHODGE) 
(PUT 'VARYPARTDF 'NUMBER-OF-ARGS 3) 
(PUT 'VARYPARTDF 'DEFINED-ON-LINE '211) 
(PUT 'VARYPARTDF 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'VARYPARTDF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARYPARTDF (U V W)
    (PROG (X)
      (PROG (J)
        (SETQ J (VARYSQ (SIMP (CAR U)) V W))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         (J
          (SETQ X
                  (ADDPSF
                   (CONS
                    (CONS
                     (*Q2F
                      (SIMP* (CONS 'PARTDF (CONS (CAAAR (CAAR J)) (CDR U)))))
                     (CDAR J))
                    NIL)
                   X))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN X))) 
(PUT 'PARTDF 'VARYFN 'VARYPARTDF) 
(PUT 'SIMPNOETHER 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPNOETHER 'DEFINED-ON-LINE '222) 
(PUT 'SIMPNOETHER 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'SIMPNOETHER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPNOETHER (U)
    (COND ((INDVARPF (CAR (SIMP0 (CADDR U)))) (MKSQ (CONS 'NOETHER U) 1))
          (T
           (PROG (X Y)
             (SIMPVARDF (LIST (CAR U) (CADR U)))
             (SETQ X (SIMP* BNDEQ*))
             (SETQ Y
                     (INTERN
                      (COMPRESS
                       (APPEND (EXPLODE '|'|)
                               (EXPLODE
                                (COND ((ATOM (CADR U)) (CADR U))
                                      (T (CAADR U))))))))
             (COND ((NULL (ATOM (CADR U))) (SETQ Y (CONS Y (CDADR U)))))
             (SETQ Y (LIST (CONS Y (LIST 'LIEDF (CADDR U) (CADR U)))))
             (RETURN
              (ADDSQ (MULTSQ (SUBF (CAR X) Y) (CONS 1 (CDR X)))
                     (NEGSQ (SIMP (LIST 'INNERPROD (CADDR U) (CAR U)))))))))) 
(PUT 'NOETHER 'SIMPFN 'SIMPNOETHER) 
(PUT 'NOETHERIND 'NUMBER-OF-ARGS 1) 
(PUT 'NOETHERIND 'DEFINED-ON-LINE '239) 
(PUT 'NOETHERIND 'DEFINED-IN-FILE 'EXCALC/VARDF.RED) 
(PUT 'NOETHERIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOETHERIND (U) (CADDR U)) 
(PUT 'NOETHER 'INDEXFUN 'NOETHERIND) 
(PUT 'NOETHER 'RTYPEFN 'GETRTYPEOR) 
(ENDMODULE) 
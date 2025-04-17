(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'KRONF)) 
(PUT 'LINFACF 'NUMBER-OF-ARGS 1) 
(PUT 'LINFACF 'DEFINED-ON-LINE '40) 
(PUT 'LINFACF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'LINFACF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINFACF (U) (TRYKRF U '(0 1))) 
(PUT 'QUADFACF 'NUMBER-OF-ARGS 1) 
(PUT 'QUADFACF 'DEFINED-ON-LINE '42) 
(PUT 'QUADFACF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'QUADFACF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUADFACF (U) (TRYKRF U '(-1 0 1))) 
(PUT 'TRYKRF 'NUMBER-OF-ARGS 2) 
(PUT 'TRYKRF 'DEFINED-ON-LINE '44) 
(PUT 'TRYKRF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'TRYKRF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRYKRF (U POINTS)
    (PROG (ATTEMPT MV VALUES)
      (COND ((NULL U) (RETURN (CONS NIL NIL)))
            ((GREATERP (LENGTH POINTS) (CDAAR U)) (RETURN (CONS NIL U))))
      (SETQ MV (CAAAR U))
      (SETQ VALUES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J POINTS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SUBUF J U)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SUBUF J U)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((MEMBER 0 VALUES)
        (PROGN
         (SETQ ATTEMPT (CONS (CONS (CONS MV 1) 1) (MINUS 1)))
         (RETURN (CONS ATTEMPT ((LAMBDA (*EXP) (QUOTF1 U ATTEMPT)) T))))))
      (SETQ VALUES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J VALUES)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (DFACTORS J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (DFACTORS J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VALUES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J VALUES)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (APPEND J
                                            (PROG (K FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ K J)
                                              (COND ((NULL K) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (K)
                                                                  (|:MINUS| K))
                                                                (CAR K))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ K (CDR K))
                                              (COND
                                               ((NULL K)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (|:MINUS| K))
                                                        (CAR K))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (APPEND J
                                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ K J)
                                      (COND ((NULL K) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (|:MINUS| K))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K) (|:MINUS| K))
                                                (CAR K))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ATTEMPT (SEARCH4FACF U VALUES NIL))
      (COND ((NULL ATTEMPT) (SETQ ATTEMPT (CONS NIL U))))
      (RETURN ATTEMPT))) 
(PUT 'SUBUF 'NUMBER-OF-ARGS 2) 
(PUT 'SUBUF 'DEFINED-ON-LINE '65) 
(PUT 'SUBUF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'SUBUF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBUF (U V)
    (PROG (Z)
      (COND ((EQUAL U 0) (SETQ U NIL)))
      (SETQ Z NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT V) (RETURN NIL)))
        (COND
         ((OR (ATOM V) (ATOM (CAR V)))
          (PROGN (SETQ Z (ADDDM* V Z)) (SETQ V NIL)))
         (T
          (PROGN
           (COND (U (SETQ Z (ADDDM* (MULTDM* (EXPT U (CDAAR V)) (CDAR V)) Z))))
           (SETQ V (CDR V)))))
        (GO WHILELABEL))
      (RETURN (COND ((NULL Z) 0) (T Z))))) 
(PUT 'ADDDM* 'NUMBER-OF-ARGS 2) 
(PUT 'ADDDM* 'DEFINED-ON-LINE '79) 
(PUT 'ADDDM* 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'ADDDM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDDM* (U V) (COND ((NULL U) V) ((NULL V) U) (T (ADDDM U V)))) 
(PUT 'MULTDM* 'NUMBER-OF-ARGS 2) 
(PUT 'MULTDM* 'DEFINED-ON-LINE '83) 
(PUT 'MULTDM* 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'MULTDM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTDM* (U V) (COND ((OR (NULL U) (NULL V)) NIL) (T (MULTDM U V)))) 
(PUT 'DFACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'DFACTORS 'DEFINED-ON-LINE '87) 
(PUT 'DFACTORS 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'DFACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DFACTORS (N)
    (PROG (X)
      (COND ((EQUAL N 0) (RETURN (LIST 0))) ((EQUAL N 1) (RETURN (LIST 1)))
            ((|:MINUSP| N) (SETQ N (|:MINUS| N))))
      (RETURN
       (COND
        ((NOT (ATOM N))
         (COND
          ((SETQ X (GET (CAR N) 'FACTORFN)) (COMBINATIONTIMES (APPLY1 X N)))
          (T (LIST N))))
        (T (COMBINATIONTIMES (ZFACTOR N))))))) 
(PUT 'COMBINATIONTIMES 'NUMBER-OF-ARGS 1) 
(PUT 'COMBINATIONTIMES 'DEFINED-ON-LINE '100) 
(PUT 'COMBINATIONTIMES 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'COMBINATIONTIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMBINATIONTIMES (FL)
    (COND ((NULL FL) (LIST 1))
          (T
           (PROG (N C RES PR)
             (SETQ N (CAAR FL))
             (SETQ C (CDAR FL))
             (SETQ PR (COMBINATIONTIMES (CDR FL)))
             (PROG ()
              WHILELABEL
               (COND ((NOT (GEQ C 0)) (RETURN NIL)))
               (PROGN
                (SETQ RES (PUTIN (EXPT N C) PR RES))
                (SETQ C (DIFFERENCE C 1)))
               (GO WHILELABEL))
             (RETURN RES))))) 
(PUT 'PUTIN 'NUMBER-OF-ARGS 3) 
(PUT 'PUTIN 'DEFINED-ON-LINE '110) 
(PUT 'PUTIN 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'PUTIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PUTIN (N L W)
    (COND ((NULL L) W) (T (PUTIN N (CDR L) (CONS (TIMES N (CAR L)) W))))) 
(PUT 'SEARCH4FACF 'NUMBER-OF-ARGS 3) 
(PUT 'SEARCH4FACF 'DEFINED-ON-LINE '113) 
(PUT 'SEARCH4FACF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'SEARCH4FACF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEARCH4FACF (U VALUES CV)
    (COND ((NULL VALUES) (TRYFACTORF U CV))
          (T
           (PROG (Q W)
             (SETQ W (CAR VALUES))
            LOOP
             (COND ((NULL W) (RETURN NIL)))
             (SETQ Q (SEARCH4FACF U (CDR VALUES) (CONS (CAR W) CV)))
             (COND ((NULL Q) (PROGN (SETQ W (CDR W)) (GO LOOP))))
             (RETURN Q))))) 
(PUT 'TRYFACTORF 'NUMBER-OF-ARGS 2) 
(PUT 'TRYFACTORF 'DEFINED-ON-LINE '124) 
(PUT 'TRYFACTORF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'TRYFACTORF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRYFACTORF (U CV)
    (PROG (W)
      (COND ((NULL (ATOMLIS CV)) (RETURN NIL)))
      (COND
       ((NULL (CDDR CV)) (SETQ W (LINETHROUGHF (CADR CV) (CAR CV) (CAAAR U))))
       (T (SETQ W (QUADTHROUGHF (CADDR CV) (CADR CV) (CAR CV) (CAAAR U)))))
      (COND
       ((OR (EQ W 'FAILED) (NULL (SETQ U ((LAMBDA (*EXP) (QUOTF1 U W)) T))))
        (RETURN NIL))
       (T (RETURN (CONS W U)))))) 
(PUT 'LINETHROUGHF 'NUMBER-OF-ARGS 3) 
(PUT 'LINETHROUGHF 'DEFINED-ON-LINE '135) 
(PUT 'LINETHROUGHF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'LINETHROUGHF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LINETHROUGHF (Y0 Y1 MV)
    (PROG (X)
      (SETQ X (DIFFERENCE Y1 Y0))
      (COND ((EQUAL X 0) (RETURN 'FAILED))
            ((LESSP X 0) (PROGN (SETQ X (MINUS X)) (SETQ Y0 (MINUS Y0)))))
      (RETURN
       (COND ((OR (EQUAL Y0 0) (NEQ (GCDN X Y0) 1)) 'FAILED)
             (T (CONS (CONS (CONS MV 1) X) Y0)))))) 
(PUT 'QUADTHROUGHF 'NUMBER-OF-ARGS 4) 
(PUT 'QUADTHROUGHF 'DEFINED-ON-LINE '144) 
(PUT 'QUADTHROUGHF 'DEFINED-IN-FILE 'POLY/KRONF.RED) 
(PUT 'QUADTHROUGHF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUADTHROUGHF (YM1 Y0 Y1 MV)
    (PROG (X Y Z)
      (SETQ X (DIVIDE (PLUS YM1 Y1) 2))
      (COND ((EQUAL (CDR X) 0) (SETQ X (DIFFERENCE (CAR X) Y0)))
            (T (RETURN 'FAILED)))
      (COND ((EQUAL X 0) (RETURN 'FAILED)))
      (SETQ Z Y0)
      (SETQ Y (DIVIDE (DIFFERENCE Y1 YM1) 2))
      (COND ((EQUAL (CDR Y) 0) (SETQ Y (CAR Y))) (T (RETURN 'FAILED)))
      (COND ((NEQ (GCDN X (GCDN Y Z)) 1) (RETURN 'FAILED)))
      (COND
       ((LESSP X 0)
        (PROGN (SETQ X (MINUS X)) (SETQ Y (MINUS Y)) (SETQ Z (MINUS Z)))))
      (COND ((EQUAL Z 0) (RETURN 'FAILED))
            ((EQUAL Y 0) (RETURN (CONS (CONS (CONS MV 2) X) Z)))
            (T
             (RETURN
              (CONS (CONS (CONS MV 2) X) (CONS (CONS (CONS MV 1) Y) Z))))))) 
(ENDMODULE) 
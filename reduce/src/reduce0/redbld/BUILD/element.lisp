(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ELEMENT)) 
(PUT 'INTEGRAL_ELEMENT 'RTYPEFN 'QUOTELIST) 
(PUT 'INTEGRAL_ELEMENT 'LISTFN 'INTELTEVAL) 
(PUT 'INTELTEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'INTELTEVAL 'DEFINED-ON-LINE '42) 
(PUT 'INTELTEVAL 'DEFINED-IN-FILE 'EDS/ELEMENT.RED) 
(PUT 'INTELTEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTELTEVAL (U V)
    (COND
     ((NEQ (LENGTH U) 1)
      (RERROR 'EDS 0 "Wrong number of arguments to integral_element"))
     ((NOT (EDSP (SETQ U (REVAL1 (CAR U) T)))) (TYPERR U "EDS"))
     (T (*SYS2A1 (EDSPROTECT (LIST 'INTELT U)) V)))) 
(PUT 'INTELT 'NUMBER-OF-ARGS 1) 
(PUT 'INTELT 'DEFINED-ON-LINE '50) 
(PUT 'INTELT 'DEFINED-IN-FILE 'EDS/ELEMENT.RED) 
(PUT 'INTELT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTELT (S)
    (PROG (G V A H Z)
      (SETQ S (CLOSURE S))
      (SETQ G (GBSYS S))
      (SETQ V
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (NONPFAFFPART (CADR S)))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND ((SETQ F (XREDUCE F (CADR G))) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND ((SETQ F (XREDUCE F (CADR G))) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (SETQ H
              (REVERSIP
               (PROG (W FORALL-RESULT FORALL-ENDPTR)
                 (SETQ W (REVERSE (INDKRNS S)))
                 (COND ((NULL W) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ F V)
                                   STARTOVER
                                    (COND ((NULL F) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (F)
                                               (PROG (C FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ C
                                                         (ORDCOMB (CDR W)
                                                          (DIFFERENCE
                                                           (DEGREEPF F) 1)))
                                                STARTOVER
                                                 (COND ((NULL C) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST
                                                               (CDAR C)))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-RESULT))
                                                 (SETQ C (CDR C))
                                                 (COND
                                                  ((ATOM FORALL-ENDPTR)
                                                   (GO STARTOVER)))
                                                LOOPLABEL
                                                 (COND
                                                  ((NULL C)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST
                                                               (CDAR C)))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-ENDPTR))
                                                 (SETQ C (CDR C))
                                                 (GO LOOPLABEL)))
                                             (CAR F)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ F (CDR F))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            ((LAMBDA (F)
                                               (PROG (C FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ C
                                                         (ORDCOMB (CDR W)
                                                          (DIFFERENCE
                                                           (DEGREEPF F) 1)))
                                                STARTOVER
                                                 (COND ((NULL C) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST
                                                               (CDAR C)))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-RESULT))
                                                 (SETQ C (CDR C))
                                                 (COND
                                                  ((ATOM FORALL-ENDPTR)
                                                   (GO STARTOVER)))
                                                LOOPLABEL
                                                 (COND
                                                  ((NULL C)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST
                                                               (CDAR C)))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-ENDPTR))
                                                 (SETQ C (CDR C))
                                                 (GO LOOPLABEL)))
                                             (CAR F)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ F (CDR F))
                                    (GO LOOPLABEL))
                                  NIL)))
                LOOPLABEL
                 (SETQ W (CDR W))
                 (COND ((NULL W) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          (PROG (F FORALL-RESULT FORALL-ENDPTR)
                            (SETQ F V)
                           STARTOVER
                            (COND ((NULL F) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (F)
                                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ C
                                                 (ORDCOMB (CDR W)
                                                  (DIFFERENCE (DEGREEPF F) 1)))
                                        STARTOVER
                                         (COND ((NULL C) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST (CDAR C)))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-RESULT))
                                         (SETQ C (CDR C))
                                         (COND
                                          ((ATOM FORALL-ENDPTR)
                                           (GO STARTOVER)))
                                        LOOPLABEL
                                         (COND
                                          ((NULL C) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST (CDAR C)))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-ENDPTR))
                                         (SETQ C (CDR C))
                                         (GO LOOPLABEL)))
                                     (CAR F)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ F (CDR F))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL F) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (F)
                                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ C
                                                 (ORDCOMB (CDR W)
                                                  (DIFFERENCE (DEGREEPF F) 1)))
                                        STARTOVER
                                         (COND ((NULL C) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST (CDAR C)))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-RESULT))
                                         (SETQ C (CDR C))
                                         (COND
                                          ((ATOM FORALL-ENDPTR)
                                           (GO STARTOVER)))
                                        LOOPLABEL
                                         (COND
                                          ((NULL C) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST (CDAR C)))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-ENDPTR))
                                         (SETQ C (CDR C))
                                         (GO LOOPLABEL)))
                                     (CAR F)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ F (CDR F))
                            (GO LOOPLABEL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ A (SETQ V (LIST)))
      (PROG (W)
        (SETQ W (INDKRNS S))
       LAB
        (COND ((NULL W) (RETURN NIL)))
        ((LAMBDA (W)
           (PROGN
            (SETQ V
                    (CONS
                     (SETDIFF
                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                        (SETQ F (CADR G))
                        (COND ((NULL F) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (F)
                                            (CAAAR
                                             (CAR (CDAR (XCOEFF F (LIST W))))))
                                          (CAR F))
                                         NIL)))
                       LOOPLABEL
                        (SETQ F (CDR F))
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (CAAAR (CAR (CDAR (XCOEFF F (LIST W))))))
                                  (CAR F))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      A)
                     V))
            (SETQ A (APPEND (CAR V) A))))
         (CAR W))
        (SETQ W (CDR W))
        (GO LAB))
      (SETQ V (REVERSE V))
      (PROG (X)
        (SETQ X (PAIR H V))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ V (CDR X))
            (SETQ X
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F (CAR X))
                     STARTOVER
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (F)
                                 (COND ((CAR (SETQ F (SUBSQ F Z))) (LIST F))))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ F (CDR F))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (F)
                                 (COND ((CAR (SETQ F (SUBSQ F Z))) (LIST F))))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ F (CDR F))
                      (GO LOOPLABEL)))
            (EDSDEBUG "Polar system" X 'SQ)
            (SETQ Z (APPEND (EDSRANSOLVE X V) Z))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F (CADR G))
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (PULLBACKPF F Z)) (CAR F)) NIL)))
        LOOPLABEL
         (SETQ F (CDR F))
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (F) (PULLBACKPF F Z)) (CAR F)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'EDSRANSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'EDSRANSOLVE 'DEFINED-ON-LINE '82) 
(PUT 'EDSRANSOLVE 'DEFINED-IN-FILE 'EDS/ELEMENT.RED) 
(PUT 'EDSRANSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSRANSOLVE (X V)
    (PROG ()
      (SETQ X (EDSSOLVE X V))
      (COND ((NULL X) (RERROR 'EDS 0 "Singular system in integral_element")))
      (COND
       ((OR (GREATERP (LENGTH X) 1) (NULL (CAAR X)))
        (RERROR 'EDS 0 "Bad system in integral_element")))
      (SETQ X (CAR (CDR (CAR X))))
      (SETQ V
              (SETDIFF V
                       (PROG (M FORALL-RESULT FORALL-ENDPTR)
                         (SETQ M X)
                         (COND ((NULL M) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (M) (CAR M)) (CAR M))
                                               NIL)))
                        LOOPLABEL
                         (SETQ M (CDR M))
                         (COND ((NULL M) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (M) (CAR M)) (CAR M)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
      (EDSVERBOSE (LIST (LENGTH V) "free variables") NIL NIL)
      (SETQ V
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C V)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C) (CONS C (SPARSERANDOM 5)))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (C) (CONS C (SPARSERANDOM 5))) (CAR C))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (NCONC (PULLBACKMAP X V) V))
      (EDSDEBUG "Solution" X 'MAP)
      (RETURN X))) 
(PUT 'SPARSERANDOM 'NUMBER-OF-ARGS 1) 
(PUT 'SPARSERANDOM 'DEFINED-ON-LINE '100) 
(PUT 'SPARSERANDOM 'DEFINED-IN-FILE 'EDS/ELEMENT.RED) 
(PUT 'SPARSERANDOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPARSERANDOM (N)
    (COND ((LESSP (RANDOM 100) 0) 0)
          (T (DIFFERENCE (RANDOM (PLUS (TIMES 2 N) 1)) N)))) 
(ENDMODULE) 
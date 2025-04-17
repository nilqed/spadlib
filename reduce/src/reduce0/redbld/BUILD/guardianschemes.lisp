(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GUARDIANSCHEMES)) 
(PUT 'GDMKGUARDED 'PSOPFN 'GD_MKGUARDED) 
(PUT 'GD_MKGUARDED 'NUMBER-OF-ARGS 1) 
(PUT 'GD_MKGUARDED 'DEFINED-ON-LINE '35) 
(PUT 'GD_MKGUARDED 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANSCHEMES.RED) 
(PUT 'GD_MKGUARDED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_MKGUARDED (ARGL)
    (PROGN
     (PUT (CAR ARGL) 'RTYPEFN 'CQUOTEGEX)
     (PUT (CAR ARGL) 'GD_SCHEME (CADR ARGL)))) 
(AEVAL
 (LIST 'GDMKGUARDED 'ABS
       (LIST 'GE (LIST 'GER 'TRUE (LIST 'ABS 'A1))
             (LIST 'GEC (LIST 'GEQ 'A1 0) 'A1)
             (LIST 'GEC (LIST 'LESSP 'A1 0) (LIST 'MINUS 'A1))))) 
(AEVAL
 (LIST 'GDMKGUARDED 'QUOTIENT
       (LIST 'GE (LIST 'GEG (LIST 'NEQ 'A2 0) (LIST 'QUOTIENT 'A1 'A2))))) 
(AEVAL
 (LIST 'GDMKGUARDED 'SQRT
       (LIST 'GE (LIST 'GEG (LIST 'GEQ 'A1 0) (LIST 'SQRT 'A1))))) 
(AEVAL
 (LIST 'GDMKGUARDED 'SIGN
       (LIST 'GE (LIST 'GER 'TRUE (LIST 'SIGN 'A1))
             (LIST 'GEC (LIST 'GREATERP 'A1 0) 1)
             (LIST 'GEC (LIST 'EQUAL 'A1 0) 0)
             (LIST 'GEC (LIST 'LESSP 'A1 0) (MINUS 1))))) 
(PUT 'MIN 'RTYPEFN 'CQUOTEGEX) 
(PUT 'MIN 'GD_SCHEMEFN 'GD_SCHEME-MIN) 
(PUT 'MAX 'RTYPEFN 'CQUOTEGEX) 
(PUT 'MAX 'GD_SCHEMEFN 'GD_SCHEME-MAX) 
(PUT 'GD_SCHEME-MIN 'NUMBER-OF-ARGS 1) 
(PUT 'GD_SCHEME-MIN 'DEFINED-ON-LINE '53) 
(PUT 'GD_SCHEME-MIN 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANSCHEMES.RED) 
(PUT 'GD_SCHEME-MIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_SCHEME-MIN (N)
    (CONS 'GE
          (CONS
           (LIST 'GER 'TRUE
                 (CONS 'MIN
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR (CONS (MKID 'A I) NIL)))
                        LOOPLABEL
                         (SETQ I (PLUS2 I 1))
                         (COND
                          ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR (CONS (MKID 'A I) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I 1)
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              (LIST 'GEC
                                    (CONS 'AND
                                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ J 1)
                                           STARTOVER
                                            (COND
                                             ((MINUSP (DIFFERENCE N J))
                                              (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (COND
                                                     ((NEQ J I)
                                                      (LIST
                                                       (LIST 'LEQ (MKID 'A I)
                                                             (MKID 'A J))))))
                                            (SETQ FORALL-ENDPTR
                                                    (LASTPAIR FORALL-RESULT))
                                            (SETQ J (PLUS2 J 1))
                                            (COND
                                             ((ATOM FORALL-ENDPTR)
                                              (GO STARTOVER)))
                                           LOOPLABEL
                                            (COND
                                             ((MINUSP (DIFFERENCE N J))
                                              (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (COND
                                                     ((NEQ J I)
                                                      (LIST
                                                       (LIST 'LEQ (MKID 'A I)
                                                             (MKID 'A J))))))
                                            (SETQ FORALL-ENDPTR
                                                    (LASTPAIR FORALL-ENDPTR))
                                            (SETQ J (PLUS2 J 1))
                                            (GO LOOPLABEL)))
                                    (MKID 'A I))
                              NIL)))
            LOOPLABEL
             (SETQ I (PLUS2 I 1))
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      (LIST 'GEC
                            (CONS 'AND
                                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ J 1)
                                   STARTOVER
                                    (COND
                                     ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (COND
                                             ((NEQ J I)
                                              (LIST
                                               (LIST 'LEQ (MKID 'A I)
                                                     (MKID 'A J))))))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ J (PLUS2 J 1))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND
                                     ((MINUSP (DIFFERENCE N J))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (COND
                                             ((NEQ J I)
                                              (LIST
                                               (LIST 'LEQ (MKID 'A I)
                                                     (MKID 'A J))))))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ J (PLUS2 J 1))
                                    (GO LOOPLABEL)))
                            (MKID 'A I))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GD_SCHEME-MAX 'NUMBER-OF-ARGS 1) 
(PUT 'GD_SCHEME-MAX 'DEFINED-ON-LINE '59) 
(PUT 'GD_SCHEME-MAX 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANSCHEMES.RED) 
(PUT 'GD_SCHEME-MAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_SCHEME-MAX (N)
    (CONS 'GE
          (CONS
           (LIST 'GER 'TRUE
                 (CONS 'MAX
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR (CONS (MKID 'A I) NIL)))
                        LOOPLABEL
                         (SETQ I (PLUS2 I 1))
                         (COND
                          ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR (CONS (MKID 'A I) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I 1)
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              (LIST 'GEC
                                    (CONS 'AND
                                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ J 1)
                                           STARTOVER
                                            (COND
                                             ((MINUSP (DIFFERENCE N J))
                                              (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (COND
                                                     ((NEQ J I)
                                                      (LIST
                                                       (LIST 'GEQ (MKID 'A I)
                                                             (MKID 'A J))))))
                                            (SETQ FORALL-ENDPTR
                                                    (LASTPAIR FORALL-RESULT))
                                            (SETQ J (PLUS2 J 1))
                                            (COND
                                             ((ATOM FORALL-ENDPTR)
                                              (GO STARTOVER)))
                                           LOOPLABEL
                                            (COND
                                             ((MINUSP (DIFFERENCE N J))
                                              (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (COND
                                                     ((NEQ J I)
                                                      (LIST
                                                       (LIST 'GEQ (MKID 'A I)
                                                             (MKID 'A J))))))
                                            (SETQ FORALL-ENDPTR
                                                    (LASTPAIR FORALL-ENDPTR))
                                            (SETQ J (PLUS2 J 1))
                                            (GO LOOPLABEL)))
                                    (MKID 'A I))
                              NIL)))
            LOOPLABEL
             (SETQ I (PLUS2 I 1))
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      (LIST 'GEC
                            (CONS 'AND
                                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ J 1)
                                   STARTOVER
                                    (COND
                                     ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (COND
                                             ((NEQ J I)
                                              (LIST
                                               (LIST 'GEQ (MKID 'A I)
                                                     (MKID 'A J))))))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ J (PLUS2 J 1))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND
                                     ((MINUSP (DIFFERENCE N J))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (COND
                                             ((NEQ J I)
                                              (LIST
                                               (LIST 'GEQ (MKID 'A I)
                                                     (MKID 'A J))))))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ J (PLUS2 J 1))
                                    (GO LOOPLABEL)))
                            (MKID 'A I))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GD_GETSCHEME 'NUMBER-OF-ARGS 2) 
(PUT 'GD_GETSCHEME 'DEFINED-ON-LINE '65) 
(PUT 'GD_GETSCHEME 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANSCHEMES.RED) 
(PUT 'GD_GETSCHEME 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_GETSCHEME (OP N)
    (PROG (W)
      (COND ((SETQ W (GET OP 'GD_SCHEME)) (RETURN W)))
      (COND ((SETQ W (GET OP 'GD_SCHEMEFN)) (RETURN (APPLY W (LIST N)))))
      (RETURN
       (LIST 'GE
             (LIST 'GEG 'TRUE
                   (CONS OP
                         (PROG (I FORALL-RESULT FORALL-ENDPTR)
                           (SETQ I 1)
                           (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS (MKID 'A I) NIL)))
                          LOOPLABEL
                           (SETQ I (PLUS2 I 1))
                           (COND
                            ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS (MKID 'A I) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))))) 
(ENDMODULE) 
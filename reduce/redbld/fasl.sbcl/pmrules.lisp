(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PMRULES)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(DEFLIST '((~ ~)) 'UNARY) 
(MKOP '&) 
(INFIX (LIST '&)) 
(DEFLIST '((& &)) 'UNARY) 
(PRECEDENCE (LIST '& 'AND)) 
(REMPROP '& 'RTYPEFN) 
(MKOP '|\||) 
(INFIX (LIST '|\||)) 
(DEFLIST '((|\|| |\||)) 'UNARY) 
(PRECEDENCE (LIST '|\|| 'OR)) 
(FLAG '(& |\||) 'NARY) 
(FLAG '(& |\||) 'SYMMETRIC) 
(AEVAL (LIST 'RSET (LIST '& 'T) 'T)) 
(AEVAL (LIST 'RSET (LIST '& 0) 0)) 
(AEVAL (LIST 'RSET (LIST '& 0 (PROGN (PUT '??B 'MGEN T) '??B)) 0)) 
(AEVAL
 (LIST 'RSETD (LIST '& 'T (PROGN (PUT '??B 'MGEN T) '??B))
       (LIST '& (PROGN (PUT '??B 'MGEN T) '??B)))) 
(AEVAL
 (LIST 'RSETD
       (LIST '& (PROGN (PUT '?A 'GEN T) '?A) (PROGN (PUT '?A 'GEN T) '?A)
             (PROGN (PUT '??B 'MGEN T) '??B))
       (LIST '& (PROGN (PUT '?A 'GEN T) '?A) (PROGN (PUT '??B 'MGEN T) '??B)))) 
(AEVAL
 (LIST 'RSETD
       (LIST '& (PROGN (PUT '?A 'GEN T) '?A)
             (LIST '~ (PROGN (PUT '?A 'GEN T) '?A))
             (PROGN (PUT '??B 'MGEN T) '??B))
       0)) 
(AEVAL (LIST 'RSET (LIST '|\|| 'T) 'T)) 
(AEVAL (LIST 'RSET (LIST '|\|| 0) 0)) 
(AEVAL (LIST 'RSET (LIST '|\|| 'T (PROGN (PUT '??A 'MGEN T) '??A)) 'T)) 
(AEVAL
 (LIST 'RSETD (LIST '|\|| 0 (PROGN (PUT '??A 'MGEN T) '??A))
       (LIST '|\|| (PROGN (PUT '??A 'MGEN T) '??A)))) 
(AEVAL
 (LIST 'RSETD
       (LIST '|\|| (PROGN (PUT '?A 'GEN T) '?A) (PROGN (PUT '?A 'GEN T) '?A)
             (PROGN (PUT '??B 'MGEN T) '??B))
       (LIST '|\|| (PROGN (PUT '?A 'GEN T) '?A)
             (PROGN (PUT '??B 'MGEN T) '??B)))) 
(AEVAL
 (LIST 'RSET
       (LIST '|\|| (PROGN (PUT '?A 'GEN T) '?A)
             (LIST '~ (PROGN (PUT '?A 'GEN T) '?A)))
       'T)) 
(AEVAL
 (LIST 'RSETD
       (LIST '|\|| (PROGN (PUT '?A 'GEN T) '?A)
             (LIST '~ (PROGN (PUT '?A 'GEN T) '?A))
             (PROGN (PUT '??B 'MGEN T) '??B))
       (LIST '|\|| (PROGN (PUT '??B 'MGEN T) '??B)))) 
(AEVAL (LIST 'RSET (LIST '~ 'T) 0)) 
(AEVAL (LIST 'RSET (LIST '~ 0) 'T)) 
(PUT 'SIMPBOOL 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPBOOL 'DEFINED-ON-LINE '78) 
(PUT 'SIMPBOOL 'DEFINED-IN-FILE 'PM/PMRULES.RED) 
(PUT 'SIMPBOOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPBOOL (U)
    (PROG (X)
      (SETQ X (OR (GET (CAR U) 'BOOLFN) (CAR U)))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CDR U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (REVAL1 J T)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (REVAL1 J T)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (APPLY X U))
      (RETURN (CONS (COND (U (LIST (CONS (CONS T 1) 1))) (T 0)) 1)))) 
(FLAG '(NUMBERP FIXP) 'FULL) 
(PUT 'NUMBERP 'SIMPFN 'SIMPBOOL) 
(PUT 'FIXP 'SIMPFN 'SIMPBOOL) 
(OPERATOR (LIST 'NUMBP 'POSP 'INTP 'NATP 'ODDP 'EVNP 'COMPLEXP 'LISTP)) 
(AEVAL
 (LIST 'RSET
       (LIST 'NUMBP
             (LIST 'SUCH-THAT (PROGN (PUT '?N 'GEN T) '?N)
                   (LIST 'NUMBERP (PROGN (PUT '?N 'GEN T) '?N))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'NUMBP
             (LIST 'SUCH-THAT
                   (LIST 'QUOTIENT (PROGN (PUT '?N 'GEN T) '?N)
                         (PROGN (PUT '?M 'GEN T) '?M))
                   (LIST '& (LIST 'NUMBERP (PROGN (PUT '?N 'GEN T) '?N))
                         (LIST 'NUMBERP (PROGN (PUT '?M 'GEN T) '?M)))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'POSP
             (LIST 'SUCH-THAT (PROGN (PUT '?N 'GEN T) '?N)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?N 'GEN T) '?N))
                         (LIST 'GREATERP (PROGN (PUT '?N 'GEN T) '?N) 0))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'POSP
             (LIST 'SUCH-THAT (PROGN (PUT '?N 'GEN T) '?N)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?N 'GEN T) '?N))
                         (LIST '~
                               (LIST 'GREATERP (PROGN (PUT '?N 'GEN T) '?N)
                                     0)))))
       0)) 
(AEVAL
 (LIST 'RSET
       (LIST 'INTP
             (LIST 'SUCH-THAT (PROGN (PUT '?N 'GEN T) '?N)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?N 'GEN T) '?N))
                         (LIST 'FIXP (PROGN (PUT '?N 'GEN T) '?N)))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'INTP
             (LIST 'SUCH-THAT (PROGN (PUT '?N 'GEN T) '?N)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?N 'GEN T) '?N))
                         (LIST '~ (LIST 'FIXP (PROGN (PUT '?N 'GEN T) '?N))))))
       0)) 
(AEVAL
 (LIST 'RSET
       (LIST 'NATP
             (LIST 'SUCH-THAT (PROGN (PUT '?I 'GEN T) '?I)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?I 'GEN T) '?I))
                         (LIST 'INTP (PROGN (PUT '?I 'GEN T) '?I))
                         (LIST 'GREATERP (PROGN (PUT '?I 'GEN T) '?I) 0))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'NATP
             (LIST 'SUCH-THAT (PROGN (PUT '?I 'GEN T) '?I)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?I 'GEN T) '?I))
                         (LIST '~
                               (LIST '&
                                     (LIST 'INTP (PROGN (PUT '?I 'GEN T) '?I))
                                     (LIST 'GREATERP
                                           (PROGN (PUT '?I 'GEN T) '?I) 0))))))
       0)) 
(AEVAL
 (LIST 'RSET
       (LIST 'ODDP
             (LIST 'SUCH-THAT (PROGN (PUT '?X 'GEN T) '?X)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?X 'GEN T) '?X))
                         (LIST 'INTP
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (PROGN (PUT '?X 'GEN T) '?X)
                                           1)
                                     2)))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'ODDP
             (LIST 'SUCH-THAT (PROGN (PUT '?X 'GEN T) '?X)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?X 'GEN T) '?X))
                         (LIST '~
                               (LIST 'INTP
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS
                                                 (PROGN (PUT '?X 'GEN T) '?X)
                                                 1)
                                           2))))))
       0)) 
(AEVAL
 (LIST 'RSET
       (LIST 'EVNP
             (LIST 'SUCH-THAT (PROGN (PUT '?X 'GEN T) '?X)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?X 'GEN T) '?X))
                         (LIST 'INTP
                               (LIST 'QUOTIENT (PROGN (PUT '?X 'GEN T) '?X)
                                     2)))))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'EVNP
             (LIST 'SUCH-THAT (PROGN (PUT '?X 'GEN T) '?X)
                   (LIST '& (LIST 'NUMBP (PROGN (PUT '?X 'GEN T) '?X))
                         (LIST '~
                               (LIST 'INTP
                                     (LIST 'QUOTIENT
                                           (PROGN (PUT '?X 'GEN T) '?X) 2))))))
       0)) 
(AEVAL (LIST 'RSET (LIST 'COMPLEXP 'I) 'T)) 
(AEVAL
 (LIST 'RSET (LIST 'COMPLEXP (LIST 'TIMES (PROGN (PUT '??B 'MGEN T) '??B) 'I))
       'T)) 
(AEVAL
 (LIST 'RSET (LIST 'COMPLEXP (LIST 'PLUS (PROGN (PUT '??A 'MGEN T) '??A) 'I))
       'T)) 
(AEVAL
 (LIST 'RSET
       (LIST 'COMPLEXP
             (LIST 'PLUS (PROGN (PUT '??A 'MGEN T) '??A)
                   (LIST 'TIMES (PROGN (PUT '??B 'MGEN T) '??B) 'I)))
       'T)) 
(AEVAL
 (LIST 'RSET (LIST 'LISTP (LIST 'LIST (PROGN (PUT '??X 'MGEN T) '??X))) 'T)) 
(AEVAL (LIST 'RSET (LIST 'LISTP (PROGN (PUT '?X 'GEN T) '?X)) 'NIL)) 
(ENDMODULE) 
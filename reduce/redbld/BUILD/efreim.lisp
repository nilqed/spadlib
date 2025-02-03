(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFREIM)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(FLUID '(JACOBI-ASSOC-LIST)) 
(FLAG
 '(JACOBISN JACOBICN JACOBIDN JACOBISC JACOBISD JACOBICS JACOBICD JACOBIDS
   JACOBIDC JACOBINS JACOBINC JACOBIND)
 'REALVALUED) 
(SETQ JACOBI-ASSOC-LIST
        (LIST (LIST 'JACOBISN (LIST 'REIMSN1 'REIMSN1 'REIMSD1 (MINUS 1)))
              (LIST 'JACOBICN (LIST 'REIMCN1 'REIMDN1 'REIMND1 0))
              (LIST 'JACOBIDN (LIST 'REIMDN1 'REIMCN1 'REIMCD1 0))
              (LIST 'JACOBINS (LIST 'REIMNS1 'REIMNS1 'REIMDS1 1))
              (LIST 'JACOBINC (LIST 'REIMNC1 'REIMND1 'REIMDN1 0))
              (LIST 'JACOBIND (LIST 'REIMND1 'REIMNC1 'REIMDC1 0))
              (LIST 'JACOBISC (LIST 'REIMSC1 'REIMSD1 'REIMSN1 (MINUS 1)))
              (LIST 'JACOBISD (LIST 'REIMSD1 'REIMSC1 'REIMSC1 (MINUS 1)))
              (LIST 'JACOBICD (LIST 'REIMCD1 'REIMDC1 'REIMNC1 0))
              (LIST 'JACOBICS (LIST 'REIMCS1 'REIMDS1 'REIMNS1 1))
              (LIST 'JACOBIDC (LIST 'REIMDC1 'REIMCD1 'REIMCN1 0))
              (LIST 'JACOBIDS (LIST 'REIMDS1 'REIMCS1 'REIMCS1 1)))) 
(PUT 'REIMJAC 'NUMBER-OF-ARGS 1) 
(PUT 'REIMJAC 'DEFINED-ON-LINE '50) 
(PUT 'REIMJAC 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMJAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REIMJAC (U)
    (PROG (REARG IMARG KR KI OP FNLIST RES)
      (SETQ KR (PREPSQ (SIMPREPART (CDDR U))))
      (SETQ KI (PREPSQ (SIMPIMPART (CDDR U))))
      (COND
       ((AND (NEQ KR 0) (NEQ KI 0))
        (REDERR "reimjac: modulus must be real or purely imaginary")))
      (SETQ OP (CAR U))
      (SETQ REARG (PREPSQ (SIMPREPART (LIST (CADR U)))))
      (SETQ IMARG (PREPSQ (SIMPIMPART (LIST (CADR U)))))
      (SETQ FNLIST (CADR (ASSOC OP JACOBI-ASSOC-LIST)))
      (SETQ OP (CADDDR FNLIST))
      (COND
       ((NEQ KR 0)
        (COND
         (((LAMBDA (S1) (AND S1 (LESSP S1 0)))
           (SIGN-OF (REVAL1 (LIST 'PLUS (LIST 'TIMES KR KR) (MINUS 1)) T)))
          (RETURN (APPLY (CAR FNLIST) (LIST REARG IMARG KR))))
         (T
          (PROGN
           (SETQ RES
                   (APPLY (CADR FNLIST)
                          (LIST (REVAL1 (LIST 'TIMES KR REARG) T)
                                (REVAL1 (LIST 'TIMES KR IMARG) T)
                                (REVAL1 (LIST 'QUOTIENT 1 KR) T))))
           (RETURN
            (COND ((EQUAL OP 0) RES)
                  ((EQUAL OP (MINUS 1)) (MULTSQ RES (INVSQ (SIMP KR))))
                  (T (MULTSQ RES (SIMP KR)))))
           NIL))))
       (T
        (PROGN
         (SETQ KR (SIMP (LIST 'SQRT (LIST 'PLUS (LIST 'TIMES KI KI) 1))))
         (SETQ KI (PREPSQ KR))
         (SETQ RES
                 (APPLY (CADDR FNLIST)
                        (LIST (REVAL1 (LIST 'TIMES IMARG KI) T)
                              (REVAL1 (LIST 'MINUS (LIST 'TIMES REARG KI)) T)
                              (REVAL1 (LIST 'QUOTIENT 1 KI) T))))
         (RETURN
          (COND ((EQUAL OP 0) RES)
                ((EQUAL OP (MINUS 1))
                 (MULTSQ RES (MULTSQ (SIMP 'I) (INVSQ KR))))
                (T (MULTSQ RES (MULTSQ (NEGSQ (SIMP 'I)) KR)))))
         NIL))))) 
(PROG (FN)
  (SETQ FN
          '(JACOBISN JACOBICN JACOBIDN JACOBISC JACOBISD JACOBICS JACOBICD
            JACOBIDS JACOBIDC JACOBINS JACOBINC JACOBIND))
 LAB
  (COND ((NULL FN) (RETURN NIL)))
  ((LAMBDA (FN) (PUT FN 'CMPXSPLITFN 'REIMJAC)) (CAR FN))
  (SETQ FN (CDR FN))
  (GO LAB)) 
(PUT 'REIMSN1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMSN1 'DEFINED-ON-LINE '89) 
(PUT 'REIMSN1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMSN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMSN1 (RP IP K)
    (PROG (DENOM NUMER1 NUMER2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ DENOM
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ DENOM (ADDSQ (SIMP 1) (MULTSQ DENOM DENOM)))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (RETURN (MULTSQ (ADDSQ NUMER1 (MULTSQ (SIMP 'I) NUMER2)) (INVSQ DENOM))))) 
(PUT 'REIMCN1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMCN1 'DEFINED-ON-LINE '105) 
(PUT 'REIMCN1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMCN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMCN1 (RP IP K)
    (PROG (DENOM NUMER1 NUMER2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ DENOM
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ DENOM (ADDSQ (SIMP 1) (MULTSQ DENOM DENOM)))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (RETURN
       (MULTSQ (ADDSQ NUMER1 (NEGSQ (MULTSQ (SIMP 'I) NUMER2)))
               (INVSQ DENOM))))) 
(PUT 'REIMDN1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMDN1 'DEFINED-ON-LINE '121) 
(PUT 'REIMDN1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMDN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMDN1 (RP IP K)
    (PROG (DENOM NUMER1 NUMER2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ DENOM
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ DENOM (ADDSQ (SIMP 1) (MULTSQ DENOM DENOM)))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (RETURN
       (MULTSQ (ADDSQ NUMER1 (NEGSQ (MULTSQ (SIMP 'I) NUMER2)))
               (INVSQ DENOM))))) 
(PUT 'REIMNS1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMNS1 'DEFINED-ON-LINE '138) 
(PUT 'REIMNS1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMNS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMNS1 (RP IP K)
    (PROG (NUMER DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ NUMER (ADDSQ (SIMP 1) (MULTSQ NUMER NUMER)))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (SETQ NUMER
              (MULTSQ NUMER (ADDSQ DENOM1 (NEGSQ (MULTSQ (SIMP 'I) DENOM2)))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER (INVSQ DENOM1))))) 
(PUT 'REIMNC1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMNC1 'DEFINED-ON-LINE '156) 
(PUT 'REIMNC1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMNC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMNC1 (RP IP K)
    (PROG (NUMER DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ NUMER (ADDSQ (SIMP 1) (MULTSQ NUMER NUMER)))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (SETQ NUMER (MULTSQ NUMER (ADDSQ DENOM1 (MULTSQ (SIMP 'I) DENOM2))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER (INVSQ DENOM1))))) 
(PUT 'REIMND1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMND1 'DEFINED-ON-LINE '175) 
(PUT 'REIMND1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMND1 (RP IP K)
    (PROG (NUMER DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER
              (MULTSQ (SIMP K)
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBISN RP K)))))
      (SETQ NUMER (ADDSQ (SIMP 1) (MULTSQ NUMER NUMER)))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (SETQ NUMER (MULTSQ NUMER (ADDSQ DENOM1 (MULTSQ (SIMP 'I) DENOM2))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER (INVSQ DENOM1))))) 
(PUT 'REIMSC1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMSC1 'DEFINED-ON-LINE '195) 
(PUT 'REIMSC1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMSC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMSC1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (SETQ NUMER1
              (ADDSQ
               (ADDSQ (MULTSQ NUMER1 DENOM1) (NEGSQ (MULTSQ NUMER2 DENOM2)))
               (MULTSQ (SIMP 'I)
                       (ADDSQ (MULTSQ NUMER1 DENOM2) (MULTSQ NUMER2 DENOM1)))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(PUT 'REIMCS1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMCS1 'DEFINED-ON-LINE '219) 
(PUT 'REIMCS1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMCS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMCS1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (SETQ NUMER1
              (ADDSQ
               (ADDSQ (MULTSQ NUMER1 DENOM1) (NEGSQ (MULTSQ NUMER2 DENOM2)))
               (MULTSQ (SIMP 'I)
                       (NEGSQ
                        (ADDSQ (MULTSQ NUMER1 DENOM2)
                               (MULTSQ NUMER2 DENOM1))))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(PUT 'REIMSD1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMSD1 'DEFINED-ON-LINE '243) 
(PUT 'REIMSD1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMSD1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMSD1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (SETQ NUMER1
              (ADDSQ
               (ADDSQ (MULTSQ NUMER1 DENOM1) (NEGSQ (MULTSQ NUMER2 DENOM2)))
               (MULTSQ (SIMP 'I)
                       (ADDSQ (MULTSQ NUMER1 DENOM2) (MULTSQ NUMER2 DENOM1)))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(PUT 'REIMDS1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMDS1 'DEFINED-ON-LINE '268) 
(PUT 'REIMDS1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMDS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMDS1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                      (MULTSQ (SIMP (LIST 'JACOBIDC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ
               (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                       (SIMP (LIST 'JACOBIDN RP K)))
               (SIMP (LIST 'JACOBISC IP KP))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (SETQ NUMER1
              (ADDSQ
               (ADDSQ (MULTSQ NUMER1 DENOM1) (NEGSQ (MULTSQ NUMER2 DENOM2)))
               (MULTSQ (SIMP 'I)
                       (NEGSQ
                        (ADDSQ (MULTSQ NUMER1 DENOM2)
                               (MULTSQ NUMER2 DENOM1))))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(PUT 'REIMDC1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMDC1 'DEFINED-ON-LINE '294) 
(PUT 'REIMDC1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMDC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMDC1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (SETQ NUMER1
              (ADDSQ (ADDSQ (MULTSQ NUMER1 DENOM1) (MULTSQ NUMER2 DENOM2))
                     (MULTSQ (SIMP 'I)
                             (ADDSQ (MULTSQ NUMER1 DENOM2)
                                    (NEGSQ (MULTSQ NUMER2 DENOM1))))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(PUT 'REIMCD1 'NUMBER-OF-ARGS 3) 
(PUT 'REIMCD1 'DEFINED-ON-LINE '319) 
(PUT 'REIMCD1 'DEFINED-IN-FILE 'ELLIPFN/EFREIM.RED) 
(PUT 'REIMCD1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REIMCD1 (RP IP K)
    (PROG (NUMER1 NUMER2 DENOM1 DENOM2 KP)
      (SETQ KP
              (PREPSQ
               (SIMP (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'TIMES K K))))))
      (SETQ NUMER1
              (MULTSQ (SIMP (LIST 'JACOBICN RP K))
                      (SIMP (LIST 'JACOBINC IP KP))))
      (SETQ NUMER2
              (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ NUMER2
              (MULTSQ NUMER2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBIDN RP K)))))
      (SETQ DENOM1
              (MULTSQ (SIMP (LIST 'JACOBIDN RP K))
                      (SIMP (LIST 'JACOBIDC IP KP))))
      (SETQ DENOM2
              (MULTSQ (SIMP (LIST 'TIMES K K))
                      (MULTSQ (SIMP (LIST 'JACOBISC IP KP))
                              (SIMP (LIST 'JACOBINC IP KP)))))
      (SETQ DENOM2
              (MULTSQ DENOM2
                      (MULTSQ (SIMP (LIST 'JACOBISN RP K))
                              (SIMP (LIST 'JACOBICN RP K)))))
      (SETQ NUMER1
              (ADDSQ (ADDSQ (MULTSQ NUMER1 DENOM1) (MULTSQ NUMER2 DENOM2))
                     (MULTSQ (SIMP 'I)
                             (ADDSQ (MULTSQ NUMER1 DENOM2)
                                    (NEGSQ (MULTSQ NUMER2 DENOM1))))))
      (SETQ DENOM1 (ADDSQ (MULTSQ DENOM1 DENOM1) (MULTSQ DENOM2 DENOM2)))
      (RETURN (MULTSQ NUMER1 (INVSQ DENOM1))))) 
(ENDMODULE) 
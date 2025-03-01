(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INNERPRD)) 
(FLUID '(SUBFG*)) 
(GLOBAL '(BASISVECTORL* KEEPL*)) 
(NEWTOK '((_ |\||) INNERPROD)) 
(INFIX (LIST 'INNERPROD)) 
(PRECEDENCE (LIST 'INNERPROD 'TIMES)) 
(FLAG '(INNERPROD) 'SPACED) 
(PUT 'INNERPROD 'SIMPFN 'SIMPINNERPROD) 
(PUT 'INNERPROD 'RTYPEFN 'GETRTYPEOR) 
(PUT 'INNERPROD 'PARTITFN 'PARTITINNERPROD) 
(PUT 'PARTITINNERPROD 'NUMBER-OF-ARGS 1) 
(PUT 'PARTITINNERPROD 'DEFINED-ON-LINE '50) 
(PUT 'PARTITINNERPROD 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'PARTITINNERPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARTITINNERPROD (U) (INNERPRODPF (PARTITOP (CAR U)) (PARTITOP (CADR U)))) 
(PUT 'MKINNERPROD 'NUMBER-OF-ARGS 2) 
(PUT 'MKINNERPROD 'DEFINED-ON-LINE '54) 
(PUT 'MKINNERPROD 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'MKINNERPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKINNERPROD (U V)
    (PROG (X Y)
      (RETURN
       (COND ((SETQ X (OPMTCH (SETQ Y (LIST 'INNERPROD U V)))) (PARTITOP X))
             ((EQUAL (DEG*FORM V) 1)
              (COND ((CAR (SETQ X (MKSQ Y 1))) (CONS (CONS 1 X) NIL)) (T NIL)))
             (T (MKUPF Y)))))) 
(PUT 'SIMPINNERPROD 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINNERPROD 'DEFINED-ON-LINE '64) 
(PUT 'SIMPINNERPROD 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'SIMPINNERPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINNERPROD (U) (*PF2SQ (PARTITINNERPROD U))) 
(PUT 'INNERPRODPF 'NUMBER-OF-ARGS 2) 
(PUT 'INNERPRODPF 'DEFINED-ON-LINE '68) 
(PUT 'INNERPRODPF 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INNERPRODPF (U V)
    (COND ((OR (NULL U) (NULL V)) NIL) ((EQUAL (CAAR V) 1) NIL)
          (T
           (PROG (RES X)
             (PROG (J)
               (SETQ J U)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               (PROG (K)
                 (SETQ K V)
                LAB
                 (COND ((NULL K) (RETURN NIL)))
                 (COND
                  ((SETQ X (INNERPRODF (CAAR J) (CAAR K)))
                   (SETQ RES
                           (ADDPF (MULTPFSQ X (MULTSQ (CDAR J) (CDAR K)))
                            RES))))
                 (SETQ K (CDR K))
                 (GO LAB))
               (SETQ J (CDR J))
               (GO LAB))
             (RETURN RES))))) 
(PUT 'BASISVECTORP 'NUMBER-OF-ARGS 1) 
(PUT 'BASISVECTORP 'DEFINED-ON-LINE '80) 
(PUT 'BASISVECTORP 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'BASISVECTORP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BASISVECTORP (U) (AND (NULL (ATOM U)) (MEMQ U BASISVECTORL*))) 
(PUT 'TVECTORP 'NUMBER-OF-ARGS 1) 
(PUT 'TVECTORP 'DEFINED-ON-LINE '83) 
(PUT 'TVECTORP 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'TVECTORP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TVECTORP (U)
    ((LAMBDA (X) (AND (NUMBERP X) (LESSP X 0))) (DEG*FORM (CAAR U)))) 
(PUT 'INNERPRODF 'NUMBER-OF-ARGS 2) 
(PUT 'INNERPRODF 'DEFINED-ON-LINE '86) 
(PUT 'INNERPRODF 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INNERPRODF (U V)
    (COND
     ((NULL (TVECTORP (CONS (CONS U (CONS 1 1)) NIL)))
      (RERROR 'EXCALC 8 "First argument of inner product must be a vector"))
     ((EQUAL V 1) NIL) ((EQCAR V 'WEDGE) (INNERPRODWEDGE U (CDR V)))
     ((AND (EQCAR U 'PARTDF) (NULL (FREEINDP (CADR U)))) (INNERPRODNVEC U V))
     ((AND (BASISVECTORP U) (BASISFORMP V)) (INNERPRODBASIS U V))
     ((EQCAR V 'INNERPROD)
      (COND ((EQ U (CADR V)) NIL) ((ORDOP U (CADR V)) (MKINNERPROD U V))
            (T
             (MULTPFSQ
              (INNERPRODPF (CONS (CONS (CADR V) (CONS 1 1)) NIL)
               (INNERPRODF U (CADDR V)))
              (CONS (MINUS 1) 1)))))
     (T (MKINNERPROD U V)))) 
(PUT 'INNERPRODWEDGE 'NUMBER-OF-ARGS 2) 
(PUT 'INNERPRODWEDGE 'DEFINED-ON-LINE '105) 
(PUT 'INNERPRODWEDGE 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODWEDGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INNERPRODWEDGE (U V) (MKUNIQUEWEDGE (INNERPRODWEDGE1 U V NIL))) 
(PUT 'INNERPRODWEDGE1 'NUMBER-OF-ARGS 3) 
(PUT 'INNERPRODWEDGE1 'DEFINED-ON-LINE '108) 
(PUT 'INNERPRODWEDGE1 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODWEDGE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INNERPRODWEDGE1 (U V W)
    (COND
     ((NULL (CDR V))
      (MKUNARYWEDGE (MULTPFSQ (INNERPRODF U (CAR V)) (MKSGNSQ W))))
     (T
      (ADDPF
       (COND
        ((AND (NULL (CDR (CDR V))) (EQUAL (DEG*FORM (CAR (CDR V))) 1))
         (MULTPFSQ (CONS (CONS (LIST (CAR V)) (CONS 1 1)) NIL)
          (MULTSQ (MKSGNSQ (ADDF (DEG*FORM (CAR V)) W))
                  (*PF2SQ (INNERPRODF U (CAR (CDR V)))))))
        (T
         (WEDGEPF2 (CONS (CONS (CAR V) (CONS 1 1)) NIL)
          (INNERPRODWEDGE1 U (CDR V) (ADDF W (DEG*FORM (CAR V)))))))
       (COND
        ((EQUAL (DEG*FORM (CAR V)) 1)
         (MULTPFSQ (CONS (CONS (CDR V) (CONS 1 1)) NIL)
          (MULTSQ (*PF2SQ (INNERPRODF U (CAR V))) (MKSGNSQ W))))
        (T
         (WEDGEPF2 (INNERPRODF U (CAR V))
          (CONS (CONS (CDR V) (MKSGNSQ W)) NIL)))))))) 
(PUT 'INNERPRODNVEC 'NUMBER-OF-ARGS 2) 
(PUT 'INNERPRODNVEC 'DEFINED-ON-LINE '125) 
(PUT 'INNERPRODNVEC 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODNVEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INNERPRODNVEC (U V)
    (COND
     ((AND (EQCAR V 'D) (NULL (DEG*FORM (CADR V))) (NULL (FREEINDP (CADR V))))
      (COND ((EQ (CADR U) (CADR V)) (CONS (CONS 1 (CONS 1 1)) NIL)) (T NIL)))
     ((BASISFORMP V)
      (PROG (X OSUBFG)
        (SETQ OSUBFG SUBFG*)
        (SETQ SUBFG* NIL)
        (SETQ X
                (INNERPRODPF (CONS (CONS U (CONS 1 1)) NIL)
                 (PARTITOP (CDR (ASSOC V KEEPL*)))))
        (SETQ SUBFG* OSUBFG)
        (RETURN (REPARTIT X))))
     (T (MKINNERPROD U V)))) 
(PUT 'INNERPRODBASIS 'NUMBER-OF-ARGS 2) 
(PUT 'INNERPRODBASIS 'DEFINED-ON-LINE '141) 
(PUT 'INNERPRODBASIS 'DEFINED-IN-FILE 'EXCALC/INNERPRD.RED) 
(PUT 'INNERPRODBASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INNERPRODBASIS (U V)
    (COND ((OR (FREEINDP U) (FREEINDP V)) (MKINNERPROD U V))
          ((EQ (CADADR U) (CADR V)) (CONS (CONS 1 (CONS 1 1)) NIL)) (T NIL))) 
(ENDMODULE) 
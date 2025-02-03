(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRAVERSO)) 
(SETQ *GTRAVERSO-SLOPPY T) 
(PUT 'GTRAVERSO 'NUMBER-OF-ARGS 3) 
(PUT 'GTRAVERSO 'DEFINED-ON-LINE '34) 
(PUT 'GTRAVERSO 'DEFINED-IN-FILE 'GROEBNER/TRAVERSO.RED) 
(PUT 'GTRAVERSO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GTRAVERSO (G0 FACT ABORT1)
    (PROG (G D S H P *GSUGAR)
      (SETQ FACT NIL)
      (SETQ ABORT1 NIL)
      (SETQ *GSUGAR T)
      (SETQ G0
              (PROG (FJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FJ G0)
               STARTOVER
                (COND ((NULL FJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (FJ)
                           (COND
                            ((NOT (OR (NULL FJ) (NULL (CADR (CDDR FJ)))))
                             (PROGN
                              (GROEBSAVELTERM FJ)
                              (LIST
                               (GSETSUGAR (VDPENUMERATE (VDPSIMPCONT FJ))
                                          NIL))))))
                         (CAR FJ)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ FJ (CDR FJ))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL FJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (FJ)
                           (COND
                            ((NOT (OR (NULL FJ) (NULL (CADR (CDDR FJ)))))
                             (PROGN
                              (GROEBSAVELTERM FJ)
                              (LIST
                               (GSETSUGAR (VDPENUMERATE (VDPSIMPCONT FJ))
                                          NIL))))))
                         (CAR FJ)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ FJ (CDR FJ))
                (GO LOOPLABEL)))
     MAIN_LOOP
      (COND ((AND (NULL G0) (NULL D)) (RETURN (GTRAVERSOFINAL G))))
      (COND
       (G0
        (PROGN (SETQ H (CAR G0)) (SETQ G0 (CDR G0)) (SETQ P (LIST NIL H H))))
       (T
        (PROGN
         (SETQ P (CAR D))
         (SETQ D (CDR D))
         (SETQ S (GROEBSPOLYNOM (CADR P) (CADDR P)))
         (AND *TRGROEB (GROEBMESS3 P S))
         (SETQ H (GROEBSIMPCONTNORMALFORM (GROEBNORMALFORM S G 'LIST)))
         (COND
          ((OR (NULL H) (NULL (CADR (CDDR H))))
           (PROGN (AND *TRGROEB (GROEBMESS4 P D)) (GO MAIN_LOOP))))
         (COND
          ((OR (NULL (CADR H))
               (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
           (PROGN
            (AND *TRGROEB (GROEBMESS5 P H))
            (SETQ D (SETQ G (SETQ G0 NIL)))))))))
      (SETQ H (GROEBENUMERATE H))
      (AND *TRGROEB (GROEBMESS5 P H))
      (GROEBSAVELTERM H)
      (SETQ D (GTRAVERSOPAIRLIST H G D))
      (SETQ G (NCONC G (LIST H)))
      (GO MAIN_LOOP))) 
(PUT 'GTRAVERSOPAIRLIST 'NUMBER-OF-ARGS 3) 
(PUT 'GTRAVERSOPAIRLIST 'DEFINED-ON-LINE '58) 
(PUT 'GTRAVERSOPAIRLIST 'DEFINED-IN-FILE 'GROEBNER/TRAVERSO.RED) 
(PUT 'GTRAVERSOPAIRLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GTRAVERSOPAIRLIST (GK G D)
    (PROG (A EV R N NN Q)
      (SETQ D (GTRAVERSOPAIRSDISCARD1 GK D))
      (SETQ EV (CADR GK))
      (PROG (P)
        (SETQ P G)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((NOT (GROEBBUCHCRIT4T EV (SETQ A (CADR P))))
             (SETQ R (CONS (VEVLCM EV A) R)))
            (T
             (PROGN
              (COND
               ((OR (NULL GMODULE*) (GEVCOMPATIBLE1 A EV GMODULE*))
                (SETQ N (CONS (GROEBMAKEPAIR P GK) N))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (Q)
        (SETQ Q R)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROG (P)
             (SETQ P N)
            LAB
             (COND ((NULL P) (RETURN NIL)))
             ((LAMBDA (P) (COND ((EQUAL (CAR P) Q) (SETQ N (DELETE P N)))))
              (CAR P))
             (SETQ P (CDR P))
             (GO LAB)))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (COND (*GTRAVERSO-SLOPPY (SETQ *GSUGAR NIL)))
      (SETQ N (GROEBCPLISTSORT N))
      (SETQ *GSUGAR T)
      (SETQ NN N)
      (SETQ N NIL)
      (PROG (P)
        (SETQ P NN)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ Q NIL)
            (PROG (R)
              (SETQ R N)
             LAB
              (COND ((NULL R) (RETURN NIL)))
              ((LAMBDA (R) (SETQ Q (OR Q (VEVMTEST? (CAR P) (CAR R)))))
               (CAR R))
              (SETQ R (CDR R))
              (GO LAB))
            (COND ((NOT Q) (SETQ N (GROEBCPLISTSORTIN P N))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (GROEBCPLISTMERGE D (REVERSIP N))))) 
(PUT 'GTRAVERSOPAIRSDISCARD1 'NUMBER-OF-ARGS 2) 
(PUT 'GTRAVERSOPAIRSDISCARD1 'DEFINED-ON-LINE '82) 
(PUT 'GTRAVERSOPAIRSDISCARD1 'DEFINED-IN-FILE 'GROEBNER/TRAVERSO.RED) 
(PUT 'GTRAVERSOPAIRSDISCARD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GTRAVERSOPAIRSDISCARD1 (GK D)
    (PROG (GI GJ TIJ EVK)
      (SETQ EVK (CADR GK))
      (PROG (PIJ)
        (SETQ PIJ D)
       LAB
        (COND ((NULL PIJ) (RETURN NIL)))
        ((LAMBDA (PIJ)
           (PROGN
            (SETQ TIJ (CAR PIJ))
            (SETQ GI (CADR PIJ))
            (SETQ GJ (CADDR PIJ))
            (COND
             ((AND (VEVSTRICTLYDIVIDES? (VEVLCM (CADR GI) (CADR GK)) TIJ)
                   (VEVSTRICTLYDIVIDES? (VEVLCM (CADR GJ) (CADR GK)) TIJ))
              (SETQ D (DELETE PIJ D))))))
         (CAR PIJ))
        (SETQ PIJ (CDR PIJ))
        (GO LAB))
      (RETURN D))) 
(PUT 'VEVSTRICTLYDIVIDES? 'NUMBER-OF-ARGS 2) 
(PUT 'VEVSTRICTLYDIVIDES? 'DEFINED-ON-LINE '92) 
(PUT 'VEVSTRICTLYDIVIDES? 'DEFINED-IN-FILE 'GROEBNER/TRAVERSO.RED) 
(PUT 'VEVSTRICTLYDIVIDES? 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VEVSTRICTLYDIVIDES? (EV1 EV2)
    (AND (NOT (EQUAL EV1 EV2)) (VEVMTEST? EV2 EV1))) 
(PUT 'GTRAVERSOFINAL 'NUMBER-OF-ARGS 1) 
(PUT 'GTRAVERSOFINAL 'DEFINED-ON-LINE '95) 
(PUT 'GTRAVERSOFINAL 'DEFINED-IN-FILE 'GROEBNER/TRAVERSO.RED) 
(PUT 'GTRAVERSOFINAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GTRAVERSOFINAL (G)
    (PROG (R P *GSUGAR)
      (SETQ G (VDPLSORT G))
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR G))
         (SETQ G (CDR G))
         (COND
          ((NOT (GROEBSEARCHINLIST (CADR P) G))
           (SETQ R
                   (CONS (GROEBSIMPCONTNORMALFORM (GROEBNORMALFORM P G 'LIST))
                         R)))))
        (GO WHILELABEL))
      (RETURN (LIST (REVERSIP R))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTEGRATION)) 
(PUT 'LDLIST 'NUMBER-OF-ARGS 3) 
(PUT 'LDLIST 'DEFINED-ON-LINE '33) 
(PUT 'LDLIST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LDLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LDLIST (P F VL)
    (PROG (A)
      (COND
       ((NOT (ATOM P))
        (COND
         ((MEMBER (CAR P)
                  (LIST 'EXPT 'PLUS 'MINUS 'TIMES 'QUOTIENT 'DF 'EQUAL))
          (PROGN
           (COND
            ((OR (EQUAL (CAR P) 'PLUS) (EQUAL (CAR P) 'TIMES)
                 (EQUAL (CAR P) 'QUOTIENT) (EQUAL (CAR P) 'EQUAL))
             (PROGN
              (SETQ P (CDR P))
              (PROG ()
               WHILELABEL
                (COND ((NOT P) (RETURN NIL)))
                (PROGN
                 (SETQ A (DIFFINCL A (LDLIST (CAR P) F VL)))
                 (SETQ P (CDR P)))
                (GO WHILELABEL))))
            ((EQUAL (CAR P) 'MINUS) (SETQ A (LDLIST (CADR P) F VL)))
            ((EQUAL (CAR P) 'EXPT) (SETQ A (LDLIST (CADR P) F VL)))
            ((EQUAL (CAR P) 'DF)
             (COND
              ((EQUAL (CADR P) F)
               (PROGN
                (SETQ P (CDDR P))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT VL) (RETURN NIL)))
                  (PROGN
                   (SETQ A (CONS (DFDEG P (CAR VL)) A))
                   (SETQ VL (CDR VL)))
                  (GO WHILELABEL))
                (SETQ A (LIST A))))))))))))
      (RETURN A))) 
(PUT 'DIFFINCL 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFINCL 'DEFINED-ON-LINE '62) 
(PUT 'DIFFINCL 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DIFFINCL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFINCL (A B)
    (PROG (P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A B)) (RETURN NIL)))
        (PROGN
         (SETQ A (DDROPLOW A (CAR B)))
         (COND ((CAR A) (SETQ P (CONS (CAR A) P))))
         (SETQ A (CDR A))
         (SETQ B (CDR B)))
        (GO WHILELABEL))
      (RETURN
       (COND ((NULL A) (COND (P (NCONC P B)) (T B))) (P (SETQ A (NCONC P A)))
             (T A))))) 
(PUT 'DDROPLOW 'NUMBER-OF-ARGS 2) 
(PUT 'DDROPLOW 'DEFINED-ON-LINE '78) 
(PUT 'DDROPLOW 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DDROPLOW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DDROPLOW (A CB)
    (PROG (H)
      (RETURN
       (COND ((NULL A) (LIST CB))
             (T
              (PROGN
               (SETQ H (COMPDIFFER (CAR A) CB))
               (COND
                ((NUMBERP H)
                 (COND ((GREATERP H 0) (CONS NIL A))
                       (T (DDROPLOW (CDR A) CB))))
                (T
                 (PROGN
                  (SETQ H (DDROPLOW (CDR A) CB))
                  (CONS (CAR H) (CONS (CAR A) (CDR H)))))))))))) 
(PUT 'COMPDIFFER 'NUMBER-OF-ARGS 2) 
(PUT 'COMPDIFFER 'DEFINED-ON-LINE '94) 
(PUT 'COMPDIFFER 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'COMPDIFFER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPDIFFER (P Q)
    (PROG (P>Q Q>P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P (OR (NULL P>Q) (NULL Q>P)))) (RETURN NIL)))
        (PROGN
         (COND ((GREATERP (CAR P) (CAR Q)) (SETQ P>Q T))
               ((LESSP (CAR P) (CAR Q)) (SETQ Q>P T)))
         (SETQ P (CDR P))
         (SETQ Q (CDR Q)))
        (GO WHILELABEL))
      (RETURN (COND (Q>P (COND (P>Q NIL) (T 0))) (P>Q 2) (T 1))))) 
(PUT 'LDINTERSEC 'NUMBER-OF-ARGS 1) 
(PUT 'LDINTERSEC 'DEFINED-ON-LINE '111) 
(PUT 'LDINTERSEC 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LDINTERSEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LDINTERSEC (A)
    (PROG (B)
      (RETURN
       (COND ((NULL A) NIL)
             (T
              (PROGN
               (SETQ B (CAR A))
               (SETQ A (CDR A))
               (PROG ()
                WHILELABEL
                 (COND ((NOT A) (RETURN NIL)))
                 (PROGN (SETQ B (ISEC B (CAR A))) (SETQ A (CDR A)))
                 (GO WHILELABEL))
               B)))))) 
(PUT 'ISEC 'NUMBER-OF-ARGS 2) 
(PUT 'ISEC 'DEFINED-ON-LINE '127) 
(PUT 'ISEC 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ISEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ISEC (CA B)
    (PROG (NEWB)
      (PROG ()
       WHILELABEL
        (COND ((NOT CA) (RETURN NIL)))
        (PROGN
         (SETQ NEWB
                 (CONS (COND ((LESSP (CAR B) (CAR CA)) (CAR B)) (T (CAR CA)))
                       NEWB))
         (SETQ CA (CDR CA))
         (SETQ B (CDR B)))
        (GO WHILELABEL))
      (RETURN (REVERSE NEWB)))) 
(PUT 'DISJUN 'NUMBER-OF-ARGS 2) 
(PUT 'DISJUN 'DEFINED-ON-LINE '139) 
(PUT 'DISJUN 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DISJUN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DISJUN (A B)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND ((NOT A) (RETURN NIL)))
       (COND ((AND (NEQ (CAR A) 0) (NEQ (CAR B) 0)) (SETQ A NIL))
             (T (PROGN (SETQ A (CDR A)) (SETQ B (CDR B)))))
       (GO WHILELABEL))
     (COND (B NIL) (T T)))) 
(PUT 'DDROPHIGH 'NUMBER-OF-ARGS 2) 
(PUT 'DDROPHIGH 'DEFINED-ON-LINE '147) 
(PUT 'DDROPHIGH 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DDROPHIGH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DDROPHIGH (A CB)
    (PROG (H)
      (RETURN
       (COND ((NULL A) (LIST CB))
             (T
              (PROGN
               (SETQ H (COMPDIFFER (CAR A) CB))
               (COND
                ((NUMBERP H) (COND ((LESSP H 2) A) (T (DDROPHIGH (CDR A) CB))))
                (T (CONS (CAR A) (DDROPHIGH (CDR A) CB)))))))))) 
(PUT 'ELIBAR 'NUMBER-OF-ARGS 3) 
(PUT 'ELIBAR 'DEFINED-ON-LINE '164) 
(PUT 'ELIBAR 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ELIBAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELIBAR (L1 L2 LDS)
    (PROG (FOUND1 FOUND2 H)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LDS (NOT FOUND2))) (RETURN NIL)))
        (PROGN
         (COND ((EQUAL (CAR LDS) L1) (SETQ FOUND1 T))
               (T
                (PROGN
                 (SETQ H (COMPDIFFER L2 (CAR LDS)))
                 (COND ((OR (NULL H) (EQUAL H 2)) (SETQ FOUND2 T))))))
         (SETQ LDS (CDR LDS)))
        (GO WHILELABEL))
      (RETURN (AND FOUND1 (NOT FOUND2))))) 
(PUT 'INTMINDERIV 'NUMBER-OF-ARGS 5) 
(PUT 'INTMINDERIV 'DEFINED-ON-LINE '178) 
(PUT 'INTMINDERIV 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTMINDERIV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTMINDERIV (P FTEM VLREV MAXVANZ NFSUB)
    (PROG (L A LISTOFLDS)
      (PROG ()
       WHILELABEL
        (COND ((NOT FTEM) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (EQUAL MAXVANZ (FCTLENGTH (CAR FTEM))) (EQUAL NFSUB 0))
           (PROGN
            (SETQ L (LDLIST P (CAR FTEM) VLREV))
            (SETQ LISTOFLDS (CONS (CONS (CAR FTEM) L) LISTOFLDS))
            (SETQ A (COND (A (LDINTERSEC (CONS A L))) (T (LDINTERSEC L)))))))
         (SETQ FTEM (CDR FTEM)))
        (GO WHILELABEL))
      (RETURN (LIST A LISTOFLDS)))) 
(PUT 'POTINTEGRABLE 'NUMBER-OF-ARGS 1) 
(PUT 'POTINTEGRABLE 'DEFINED-ON-LINE '198) 
(PUT 'POTINTEGRABLE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'POTINTEGRABLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POTINTEGRABLE (LISTOFLDS)
    (PROG (H L1 L2 F N LDS F1 F2)
      (COND (TR_GENINT (PROGN (PRIN2 "Does a potential exist?") NIL)))
      (SETQ H LISTOFLDS)
      (SETQ L1 NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROGN
         (SETQ L2 (CDAR H))
         (PROG ()
          WHILELABEL
           (COND ((NOT L2) (RETURN NIL)))
           (PROGN (SETQ L1 (DDROPHIGH L1 (CAR L2))) (SETQ L2 (CDR L2)))
           (GO WHILELABEL))
         (SETQ H (CDR H)))
        (GO WHILELABEL))
      (RETURN
       (COND ((NEQ (LENGTH L1) 2) NIL) ((NOT (DISJUN (CAR L1) (CADR L1))) NIL)
             (T
              (PROGN
               (SETQ L2 (CADR L1))
               (SETQ L1 (CAR L1))
               (SETQ F NIL)
               (SETQ F1 NIL)
               (SETQ F2 NIL)
               (SETQ N 0)
               (SETQ H LISTOFLDS)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND H (OR (NOT F1) (NOT F2) (AND (NOT F) (NEQ N 2)))))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ LDS (CDAR H))
                  (COND
                   ((OR (NOT F1) (NOT F))
                    (COND
                     ((ELIBAR L1 L2 LDS)
                      (PROGN
                       (SETQ F1 (CONS (CAAR H) F1))
                       (COND ((EQUAL (LENGTH LDS) 1) (SETQ F (CAAR H)))
                             ((EQUAL (LENGTH LDS) 2)
                              (COND
                               ((OR (EQUAL (CAR LDS) L2) (EQUAL (CADR LDS) L2))
                                (SETQ N (PLUS N 1)))))))))))
                  (COND
                   ((OR (NOT F2) (NOT F))
                    (COND
                     ((ELIBAR L2 L1 LDS)
                      (PROGN
                       (SETQ F2 (CONS (CAAR H) F2))
                       (COND ((EQUAL (LENGTH LDS) 1) (SETQ F (CAAR H)))))))))
                  (SETQ H (CDR H)))
                 (GO WHILELABEL))
               (COND ((AND F1 (OR (GREATERP N 1) (AND F2 F))) (LIST L1 L2))
                     (T NIL)))))))) 
(PUT 'VLOFINTLIST 'NUMBER-OF-ARGS 2) 
(PUT 'VLOFINTLIST 'DEFINED-ON-LINE '250) 
(PUT 'VLOFINTLIST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'VLOFINTLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VLOFINTLIST (VL INTLIST)
    (PROG (A)
      (PROG ()
       WHILELABEL
        (COND ((NOT INTLIST) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (CAR INTLIST) (NOT (ZEROP (CAR INTLIST))))
           (SETQ A (CONS (CAR VL) A))))
         (SETQ VL (CDR VL))
         (SETQ INTLIST (CDR INTLIST)))
        (GO WHILELABEL))
      (RETURN A))) 
(PUT 'VLOFINTFACLIST 'NUMBER-OF-ARGS 2) 
(PUT 'VLOFINTFACLIST 'DEFINED-ON-LINE '262) 
(PUT 'VLOFINTFACLIST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'VLOFINTFACLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VLOFINTFACLIST (VL INTFACLIST)
    (PROG (E1 A)
      (PROG (E1)
        (SETQ E1 VL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND ((NOT (MY_FREEOF INTFACLIST E1)) (SETQ A (CONS E1 A)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (RETURN A))) 
(PUT 'MULTIPLEINT 'NUMBER-OF-ARGS 9) 
(PUT 'MULTIPLEINT 'DEFINED-ON-LINE '270) 
(PUT 'MULTIPLEINT 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'MULTIPLEINT 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE MULTIPLEINT (INTVAR FTEM Q VARI VL GENFLAG POTFLAG PARTIAL DONEINTVAR)
    (PROG (PRI VLCOP V NMAX GENI INTLIST IFLAG N NGES NEWCOND INTFACLIST PH PIH
           QH QIH INTFACDEPNEW INTFACDEP)
      (COND ((AND (NOT VARI) (ZEROP Q)) (RETURN NIL)))
      (SETQ NGES 0)
      (SETQ VLCOP VL)
      (SETQ PIH T)
      (SETQ Q (SPLITINHOM Q FTEM VL))
      (SETQ QH (CAR Q))
      (SETQ QIH (CDR Q))
      (SETQ Q NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (OR VARI VLCOP) (OR PIH (NOT POTFLAG)))) (RETURN NIL)))
        (PROGN
         (COND (VARI (PROGN (SETQ V VARI) (SETQ NMAX 1)))
               (T
                (PROGN
                 (SETQ V (CAR VLCOP))
                 (SETQ VLCOP (CDR VLCOP))
                 (SETQ NMAX (CAR INTVAR))
                 (SETQ INTVAR (CDR INTVAR)))))
         (COND ((ZEROP NMAX) (SETQ INTLIST (CONS NIL INTLIST)))
               (T
                (PROGN
                 (COND
                  (PRI
                   (PROGN
                    (PRIN2 "anf: intvar=")
                    (PRIN2 INTVAR)
                    (PRIN2 " vari=")
                    (PRIN2 VARI)
                    (PRIN2 " q=")
                    (PRIN2 Q)
                    (PRIN2 " v =")
                    (PRIN2 V)
                    (PRIN2 " vl =")
                    (PRIN2 VL)
                    NIL)))
                 (COND
                  ((AND VARI (NOT (MEMBER V VL)))
                   (PROGN
                    (SETQ QH (ERR_CATCH_INT QH V))
                    (COND ((NULL QH) (SETQ IFLAG NIL))
                          ((FREEOF QH 'INT)
                           (PROGN
                            (SETQ QIH (ERR_CATCH_INT QIH V))
                            (SETQ IFLAG
                                    (COND ((NULL QIH) NIL)
                                          ((AND FREEINT_
                                                (NULL (FREEOF QIH 'INT)))
                                           NIL)
                                          ((AND FREEABS_
                                                (NULL (FREEOF QIH 'ABS)))
                                           NIL)
                                          (T
                                           (PROGN
                                            (SETQ INTLIST
                                                    (CONS (LIST 1) INTLIST))
                                            'SUCCESS))))
                            (COND
                             (PRI
                              (PROGN
                               (PROGN (PRIN2 "232323 qh=") (PRIN2 QH) NIL)
                               (TERPRI)
                               (PROGN (PRIN2 "qih=") (PRIN2 QIH) NIL)
                               (TERPRI)))))))))
                  (T
                   (PROGN
                    (SETQ N 0)
                    (COND (PRI (PROGN (PRIN2 "333") NIL)))
                    (SETQ INTFACLIST NIL)
                    (COND
                     ((OR POTFLAG (MY_FREEOF INTFACDEP V))
                      (PROG ()
                       REPEATLABEL
                        (PROGN
                         (COND
                          (PRI
                           (PROGN
                            (PROGN (PRIN2 "444  vor intpde:") NIL)
                            (EQPRINT Q)
                            (TERPRI)
                            (PROGN
                             (PRIN2 "potflag=")
                             (PRIN2 POTFLAG)
                             (PRIN2 " v=")
                             (PRIN2 V)
                             (PRIN2 "  ftem=")
                             (PRIN2 FTEM)
                             NIL))))
                         (SETQ PH (INTPDE QH FTEM VL V PARTIAL))
                         (COND
                          (PRI
                           (PROGN
                            (PROGN (PRIN2 "nach intpde(qh):") NIL)
                            (DEPRINT PH))))
                         (SETQ INTFACDEPNEW INTFACDEP)
                         (COND
                          ((AND PH (OR PARTIAL (ZEROP (CADR PH))))
                           (PROGN
                            (SETQ INTFACLIST (CONS 1 INTFACLIST))
                            (SETQ PH (CAR PH))
                            (COND
                             (PRI
                              (PROGN
                               (PROGN (PRIN2 "565656 ph=") (PRIN2 PH) NIL)
                               (TERPRI))))
                            NIL))
                          (VARI (SETQ PH NIL))
                          (FACINT_
                           (PROGN
                            (SETQ PH
                                    (FINDINTFAC (LIST QH) FTEM VL V DONEINTVAR
                                     INTFACDEP (NOT (ZEROP N)) (NOT POTFLAG)))
                            (COND
                             (PH
                              (PROGN
                               (COND
                                (PRI
                                 (PROGN
                                  (PROGN (PRIN2 "of the homogeneous part") NIL)
                                  (TERPRI))))
                               (SETQ INTFACDEPNEW (CADDR PH))
                               (SETQ INTFACLIST (CONS (CAADR PH) INTFACLIST))
                               (SETQ QIH
                                       (REVAL1
                                        (REVAL1
                                         (REVAL1
                                          (LIST 'TIMES (CAR INTFACLIST) QIH) T)
                                         T)
                                        T))
                               (COND
                                (PRI
                                 (PROGN
                                  (PROGN (PRIN2 "454545 qih=") (PRIN2 QIH) NIL)
                                  (TERPRI))))
                               (SETQ PH (CAR PH))))))))
                         (COND ((NOT PH) (SETQ PIH NIL))
                               (T
                                (PROGN
                                 (COND ((ZEROP QIH) (SETQ PIH (LIST 0 0)))
                                       (T (SETQ PIH (INTPDE QIH FTEM VL V T))))
                                 (COND
                                  ((AND PRINT_ (NULL PIH))
                                   (PROGN
                                    (TERPRI)
                                    (PROGN (PRIN2 "Inhomogeneous part: ") NIL)
                                    (TYPE_PRE_EX QIH)
                                    (PROGN
                                     (PRIN2
                                      "can not be integrated explicitly wrt. ")
                                     (PRIN2 V)
                                     NIL)
                                    NIL)))
                                 (COND
                                  (PRI
                                   (PROGN
                                    (PROGN
                                     (PRIN2 "nach intpde(qih):")
                                     (PRIN2 PIH)
                                     NIL)
                                    (TERPRI)
                                    (PROGN
                                     (PRIN2 "genflag=")
                                     (PRIN2 GENFLAG)
                                     NIL)
                                    (TERPRI))))
                                 (COND
                                  (PIH
                                   (COND
                                    ((ZEROP (CADR PIH))
                                     (PROGN
                                      (SETQ QIH (CAR PIH))
                                      (SETQ N (ADD1 N))
                                      (SETQ IFLAG 'SUCCESS)
                                      (COND
                                       (PRI (PROGN (PRIN2 " success ") NIL)))
                                      NIL))
                                    ((NOT GENFLAG) (SETQ PIH NIL))
                                    (T
                                     (PROGN
                                      (COND (PRI (PROGN (PRIN2 "555") NIL)))
                                      (SETQ GENI
                                              (PARTINT (CADR PIH)
                                               (SMEMBERL FTEM (CADR PIH)) VL V
                                               GENFLAG))
                                      (COND
                                       (GENI
                                        (PROGN
                                         (SETQ QIH
                                                 (REVAL1
                                                  (LIST 'PLUS (CAR PIH)
                                                        (CAR GENI))
                                                  T))
                                         (SETQ N (ADD1 N))
                                         (SETQ FTEM (UNION FNEW_ FTEM))
                                         (SETQ NEWCOND
                                                 (APPEND (CDR GENI) NEWCOND))
                                         (COND
                                          (PRI
                                           (PROGN
                                            (TERPRI)
                                            (PROGN
                                             (PRIN2 "nach gen newcond:")
                                             (PRIN2 NEWCOND)
                                             NIL))))
                                         (SETQ IFLAG 'GENINT)))
                                       (PARTIAL (SETQ QIH (CAR PIH)))
                                       (T (SETQ PIH NIL))))))))
                                 (COND
                                  (PIH
                                   (PROGN
                                    (COND (PRI (PROGN (PRIN2 "AAA") NIL)))
                                    (SETQ QH PH)
                                    (COND
                                     ((AND (NOT POTFLAG) (NEQ N 1))
                                      (SETQ INTFACDEP INTFACDEPNEW)))))
                                  (T
                                   (PROGN
                                    (COND (PRI (PROGN (PRIN2 "BBB") NIL)))
                                    (COND
                                     ((NEQ (CAR INTFACLIST) 1)
                                      (SETQ QIH
                                              (REVAL1
                                               (LIST 'QUOTIENT QIH
                                                     (CAR INTFACLIST))
                                               T))))
                                    (SETQ INTFACLIST (CDR INTFACLIST)))))
                                 NIL)))
                         (COND
                          (PRI
                           (PROGN
                            (PRIN2 "n=")
                            (PRIN2 N)
                            (PRIN2 " nmax=")
                            (PRIN2 NMAX)
                            (PRIN2 " not pih=")
                            (PRIN2 (NOT PIH))
                            NIL)))
                         NIL)
                        (COND
                         ((NOT (OR (EQUAL N NMAX) (NOT PIH)))
                          (GO REPEATLABEL))))))
                    (COND
                     ((NOT (ZEROP N)) (SETQ DONEINTVAR (CONS V DONEINTVAR))))
                    (SETQ NGES (PLUS NGES N))
                    (SETQ INTLIST (CONS INTFACLIST INTLIST)))))
                 (COND (VARI (PROGN (SETQ VARI NIL) (SETQ VLCOP NIL))))
                 (COND
                  (PRI
                   (PROGN
                    (PRIN2 "ende: intvar=")
                    (PRIN2 INTVAR)
                    (PRIN2 " vari=")
                    (PRIN2 VARI)
                    (PRIN2 "    qh=")
                    (PRIN2 QH)
                    (PRIN2 "   qih=")
                    (PRIN2 QIH)
                    NIL)))
                 NIL))))
        (GO WHILELABEL))
      (SETQ Q (REVAL1 (LIST 'PLUS QH QIH) T))
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "\\\\\\  newcond:") NIL)
         (DEPRINT NEWCOND)
         (PROGN
          (PRIN2 "multiple result:")
          (PRIN2 (COND ((NULL IFLAG) NIL) (T (LIST Q INTLIST NEWCOND NGES))))
          NIL))))
      (RETURN (COND ((NULL IFLAG) NIL) (T (LIST Q INTLIST NEWCOND NGES)))))) 
(PUT 'UPLISTOFLDS 'NUMBER-OF-ARGS 2) 
(PUT 'UPLISTOFLDS 'DEFINED-ON-LINE '457) 
(PUT 'UPLISTOFLDS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'UPLISTOFLDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPLISTOFLDS (INTLIST LISTOFLDS)
    (PROG (F H1 H2 H3 H4 LDS ITL)
      (PROG ()
       WHILELABEL
        (COND ((NOT LISTOFLDS) (RETURN NIL)))
        (PROGN
         (SETQ F (CAAR LISTOFLDS))
         (SETQ LDS (CDAR LISTOFLDS))
         (SETQ LISTOFLDS (CDR LISTOFLDS))
         (SETQ H2 NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT LDS) (RETURN NIL)))
           (PROGN
            (SETQ H3 (CAR LDS))
            (SETQ LDS (CDR LDS))
            (SETQ ITL INTLIST)
            (SETQ H4 NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT H3) (RETURN NIL)))
              (PROGN
               (SETQ H4
                       (CONS
                        (DIFFERENCE (CAR H3)
                                    (COND ((NULL (CAR ITL)) 0)
                                          (T (LENGTH (CAR ITL)))))
                        H4))
               (SETQ H3 (CDR H3))
               (SETQ ITL (CDR ITL)))
              (GO WHILELABEL))
            (SETQ H2 (CONS (REVERSE H4) H2)))
           (GO WHILELABEL))
         (SETQ H1 (CONS (CONS F H2) H1)))
        (GO WHILELABEL))
      (RETURN H1))) 
(PUT 'PROPORTIONALITYCONDITIONS 'NUMBER-OF-ARGS 3) 
(PUT 'PROPORTIONALITYCONDITIONS 'DEFINED-ON-LINE '481) 
(PUT 'PROPORTIONALITYCONDITIONS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'PROPORTIONALITYCONDITIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROPORTIONALITYCONDITIONS (EX FL X)
    (COND
     (FL
      (PROG (FLO F1 F2 FLC S1 S2 C1 CONDI)
        (SETQ FLO FL)
        (PROG ()
         WHILELABEL
          (COND ((NOT (CDR FL)) (RETURN NIL)))
          (PROGN
           (SETQ F1 (CAR FL))
           (SETQ FL (CDR FL))
           (SETQ S1 (COEFFN EX F1 1))
           (COND
            ((NOT (ZEROP S1))
             (PROGN
              (SETQ FLC FL)
              (PROG ()
               WHILELABEL
                (COND ((NOT FLC) (RETURN NIL)))
                (PROGN
                 (SETQ F2 (CAR FLC))
                 (SETQ FLC (CDR FLC))
                 (SETQ S2 (COEFFN EX F2 1))
                 (COND
                  ((NOT (ZEROP S2))
                   (PROGN
                    (SETQ C1 (REVAL1 (LIST 'DF (LIST 'QUOTIENT S1 S2) X) T))
                    (SETQ C1 (SIMPLIFYSQ (SIMP C1) FTEM_ T NIL T))
                    (COND
                     ((AND (NEQ C1 (LIST (CONS 1 1)))
                           (NOT (FREEOFLIST C1 FTEM_)))
                      (SETQ CONDI (UNION C1 CONDI))))))))
                (GO WHILELABEL))))))
          (GO WHILELABEL))
        (RETURN CONDI))))) 
(PUT 'ADDINTCO 'NUMBER-OF-ARGS 5) 
(PUT 'ADDINTCO 'DEFINED-ON-LINE '510) 
(PUT 'ADDINTCO 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ADDINTCO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDINTCO (Q FTEM IFAC VL VARI)
    (PROG (V F L VL1 J FTEMCP FNEWCP)
      (SETQ FTEMCP FTEM)
      (COND ((ZEROP Q) (SETQ L 1))
            (T
             (PROGN
              (SETQ FTEM (FCTSORT FTEM))
              (PROG ()
               WHILELABEL
                (COND ((NOT FTEM) (RETURN NIL)))
                (COND
                 ((LESSP (FCTLENGTH (CAR FTEM)) (LENGTH VL)) (SETQ FTEM NIL))
                 ((FCTLINEAR Q F) (PROGN (SETQ F (CAR FTEM)) (SETQ FTEM NIL)))
                 (T (SETQ FTEM (CDR FTEM))))
                (GO WHILELABEL))
              (COND
               (F
                (PROGN
                 (SETQ L (LDERIV Q F (FCTARGS F)))
                 (SETQ L (REVAL1 (COEFFN Q (REVAL1 (CAR L) T) (CDR L)) T))
                 (COND ((NOT (FREEOFLIST L FTEM)) (SETQ L 1)))))
               (T (SETQ L 1))))))
      (COND (VARI (SETQ Q (LIST 'PLUS Q (INTCONST L VL VARI (LIST 1)))))
            (T
             (PROGN
              (SETQ VL1 VL)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND VL1 (NULL J))) (RETURN NIL)))
                (PROGN
                 (SETQ V (CAR VL1))
                 (SETQ VL1 (CDR VL1))
                 (SETQ FNEWCP FNEW_)
                 (COND
                  ((CAR IFAC)
                   (SETQ Q (LIST 'PLUS Q (INTCONST L VL V (CAR IFAC))))))
                 (SETQ J (ZERO_DEN Q FTEMCP))
                 (COND
                  ((NULL J)
                   (SETQ J
                           (PROPORTIONALITYCONDITIONS Q (SETDIFF FNEW_ FNEWCP)
                            V))))
                 (SETQ IFAC (CDR IFAC))
                 NIL)
                (GO WHILELABEL)))))
      (RETURN
       (COND ((NULL J) (REVAL1 Q T))
             (T
              (PROGN
               (COND
                ((FREEOFLIST J FNEW_)
                 (PROG (H)
                   (SETQ H J)
                  LAB
                   (COND ((NULL H) (RETURN NIL)))
                   ((LAMBDA (H)
                      (SETQ TO_DO_LIST
                              (CONS (LIST 'SPLIT_INTO_CASES H) TO_DO_LIST)))
                    (CAR H))
                   (SETQ H (CDR H))
                   (GO LAB))))
               NIL)))))) 
(PUT 'INTEGRATEPDE 'NUMBER-OF-ARGS 5) 
(PUT 'INTEGRATEPDE 'DEFINED-ON-LINE '576) 
(PUT 'INTEGRATEPDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRATEPDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATEPDE (P FTEM VARI GENFLAG POTFLAG)
    (PROG (VL VLREV V INTLIST ILI1A ILI2A MAXVANZ FSUB H HH NFSUB IFLAG NEWCOND
           N1 N2 POT1 POT2 P1 P2 LISTOFLDS SECND IFAC0 IFAC1A IFAC1B IFAC2A
           IFAC2B COP V1A V2A PRI AIC PNEW)
      (COND (PRI (PROGN (TERPRI) (PROGN (PRIN2 "Start Integratepde") NIL))))
      (SETQ VL (ARGSET FTEM))
      (SETQ VLREV (REVERSE VL))
      (COND
       (VARI
        (PROGN (SETQ POTFLAG NIL) (COND ((ZEROP P) (SETQ IFLAG 'SUCCESS)))))
       (T
        (PROGN
         (SETQ MAXVANZ (LENGTH VL))
         (SETQ FSUB NIL)
         (SETQ H FTEM)
         (PROG ()
          WHILELABEL
           (COND ((NOT H) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (FCTLENGTH (CAR H)) MAXVANZ)
              (SETQ FSUB (CONS (CAR H) FSUB))))
            (SETQ H (CDR H)))
           (GO WHILELABEL))
         (SETQ NFSUB (LENGTH FSUB))
         (SETQ H (INTMINDERIV P FTEM VLREV MAXVANZ NFSUB))
         (SETQ INTLIST (CAR H))
         (SETQ LISTOFLDS (CADR H))
         NIL)))
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "complete integrations:")
          (PRIN2 INTLIST)
          (PRIN2 " for:")
          (PRIN2 VL)
          NIL))))
      (SETQ N1
              (PROG (H FORALL-RESULT)
                (SETQ H INTLIST)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (H) H) (CAR H)) FORALL-RESULT))
                (SETQ H (CDR H))
                (GO LAB1)))
      (COND
       ((AND (NOT VARI) (ZEROP N1))
        (PROGN
         (SETQ N2 0)
         (COND
          (POTFLAG
           (PROG (H)
             (SETQ H 1)
            LAB
             (COND ((MINUSP (DIFFERENCE (LENGTH VL) H)) (RETURN NIL)))
             (SETQ IFAC0 (CONS NIL IFAC0))
             (SETQ H (PLUS2 H 1))
             (GO LAB))))))
       (T
        (PROGN
         (COND
          (TR_GENINT
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "integration of the expression : ") NIL)
            (EQPRINT P))))
         (COND
          (PRI
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "at first all multiple complete integration") NIL))))
         (SETQ H (MULTIPLEINT INTLIST FTEM P VARI VL GENFLAG NIL NIL NIL))
         (COND
          (H
           (PROGN
            (SETQ P (CAR H))
            (SETQ IFAC0 (CADR H))
            (SETQ NEWCOND (CADDR H))
            (SETQ N2 (CADDDR H))
            (SETQ H NIL)
            (SETQ IFLAG 'SUCCESS)
            NIL))
          (T (SETQ N2 0)))
         (SETQ FTEM (UNION FNEW_ FTEM))
         NIL)))
      (COND
       ((AND (EQUAL N1 N2) POTFLAG (GREATERP NFSUB 1))
        (PROGN
         (COND
          ((NOT (ZEROP N2))
           (SETQ LISTOFLDS (UPLISTOFLDS (REVERSE IFAC0) LISTOFLDS))))
         (COND
          (PRI
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "uplistoflds:") (PRIN2 LISTOFLDS) NIL))))
         (COND
          ((SETQ H (POTINTEGRABLE LISTOFLDS))
           (PROGN
            (SETQ ILI1A (CAR H))
            (SETQ ILI2A (CADR H))
            (COND
             (PRI
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "potintegrable:")
                (PRIN2 ILI1A)
                (PRIN2 "  ")
                (PRIN2 ILI2A)
                NIL))))
            (COND
             (PRI
              (PROGN
               (PROGN
                (PRIN2 "+++ intlist=")
                (PRIN2 INTLIST)
                (PRIN2 "    ili1a=")
                (PRIN2 ILI1A)
                (PRIN2 "    ili2a=")
                (PRIN2 ILI2A)
                NIL))))
            (SETQ V1A (VLOFINTLIST VL ILI1A))
            (SETQ V2A (VLOFINTLIST VL ILI2A))
            (SETQ HH T)
            (SETQ COP (REVERSE IFAC0))
            (SETQ IFAC1A ILI1A)
            (SETQ IFAC2A ILI2A)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND HH COP)) (RETURN NIL)))
              (PROGN
               (COND ((CAR COP) (SETQ H (VLOFINTFACLIST VL (CDAR COP))))
                     (T (SETQ H NIL)))
               (COND
                ((AND (FREEOFLIST H V2A) (EQUAL (CAR IFAC2A) 0))
                 (PROGN
                  (SETQ IFAC1B (CONS NIL IFAC1B))
                  (SETQ IFAC2B (CONS (REVERSE (CAR COP)) IFAC2B))))
                ((AND (FREEOFLIST H V1A) (EQUAL (CAR IFAC1A) 0))
                 (PROGN
                  (SETQ IFAC2B (CONS NIL IFAC2B))
                  (SETQ IFAC1B (CONS (REVERSE (CAR COP)) IFAC1B))))
                ((CAR COP) (SETQ HH NIL)))
               (SETQ IFAC1A (CDR IFAC1A))
               (SETQ IFAC2A (CDR IFAC2A))
               (SETQ COP (CDR COP))
               NIL)
              (GO WHILELABEL))
            (COND
             (PRI
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "ifac1a=")
                (PRIN2 IFAC1A)
                (PRIN2 "  ifac1b=")
                (PRIN2 IFAC1B)
                (PRIN2 "  ifac2a=")
                (PRIN2 IFAC2A)
                (PRIN2 "  ifac2b=")
                (PRIN2 IFAC2B)
                NIL))))
            (COND
             (HH
              (PROG ()
               REPEATLABEL
                (PROGN
                 (SETQ N1
                         (PROG (N1 FORALL-RESULT)
                           (SETQ N1 ILI1A)
                           (SETQ FORALL-RESULT 0)
                          LAB1
                           (COND ((NULL N1) (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (PLUS ((LAMBDA (N1) N1) (CAR N1))
                                         FORALL-RESULT))
                           (SETQ N1 (CDR N1))
                           (GO LAB1)))
                 (SETQ P1
                         (MULTIPLEINT ILI1A FTEM P NIL VL GENFLAG T T
                          (UNION (VLOFINTLIST VL ILI2A)
                                 (VLOFINTLIST VL IFAC1B))))
                 (SETQ FTEM (UNION FNEW_ FTEM))
                 (COND
                  (P1
                   (PROGN
                    (SETQ IFAC1A (CADR P1))
                    (COND (NEWCOND (SETQ NEWCOND (NCONC NEWCOND (CADDR P1))))
                          (T (SETQ NEWCOND (CADDR P1))))
                    (COND
                     (PRI
                      (PROGN
                       (TERPRI)
                       (PROGN (PRIN2 "mul2: newcond=") (PRIN2 NEWCOND) NIL))))
                    (SETQ N2 (CADDDR P1))
                    (SETQ P1 (CAR P1)))))
                 (COND
                  ((AND P1 (EQUAL N1 N2))
                   (PROGN
                    (SETQ N1
                            (PROG (N1 FORALL-RESULT)
                              (SETQ N1 ILI2A)
                              (SETQ FORALL-RESULT 0)
                             LAB1
                              (COND ((NULL N1) (RETURN FORALL-RESULT)))
                              (SETQ FORALL-RESULT
                                      (PLUS ((LAMBDA (N1) N1) (CAR N1))
                                            FORALL-RESULT))
                              (SETQ N1 (CDR N1))
                              (GO LAB1)))
                    (SETQ P2 P1)
                    (SETQ COP IFAC1A)
                    (SETQ HH VLREV)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT COP) (RETURN NIL)))
                      (PROGN
                       (SETQ H (CAR COP))
                       (SETQ COP (CDR COP))
                       (SETQ V (CAR HH))
                       (SETQ HH (CDR HH))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT H) (RETURN NIL)))
                         (PROGN
                          (SETQ P2
                                  (REVAL1
                                   (LIST 'QUOTIENT (LIST 'DF P2 V) (CAR H)) T))
                          (SETQ H (CDR H)))
                         (GO WHILELABEL)))
                      (GO WHILELABEL))
                    (SETQ P2
                            (REVAL1 (REVAL1 (LIST 'PLUS P (LIST 'MINUS P2)) T)
                                    T))
                    (SETQ P2
                            (MULTIPLEINT ILI2A FTEM P2 NIL VL GENFLAG T NIL
                             (UNION (VLOFINTLIST VL ILI1A)
                                    (VLOFINTLIST VL IFAC2B))))
                    (SETQ FTEM (UNION FNEW_ FTEM))
                    (COND
                     (P2
                      (PROGN
                       (SETQ IFAC2A (CADR P2))
                       (COND
                        (NEWCOND (SETQ NEWCOND (NCONC NEWCOND (CADDR P2))))
                        (T (SETQ NEWCOND (CADDR P2))))
                       (COND
                        (PRI
                         (PROGN
                          (TERPRI)
                          (PROGN
                           (PRIN2 "mul3: newcond=")
                           (PRIN2 NEWCOND)
                           NIL))))
                       (SETQ N2 (CADDDR P2))
                       (SETQ P2 (CAR P2)))))
                    (COND
                     ((AND P2 (EQUAL N1 N2))
                      (PROGN
                       (SETQ POT1 NIL)
                       (SETQ POT2 NIL)
                       (PROG (H)
                         (SETQ H FSUB)
                        LAB
                         (COND ((NULL H) (RETURN NIL)))
                         ((LAMBDA (H)
                            (PROGN
                             (COND
                              ((EQUAL (LD_DERIV_SEARCH P1 H VL) (CONS NIL 1))
                               (SETQ POT1 (CONS H POT1))))
                             (COND
                              ((EQUAL (LD_DERIV_SEARCH P2 H VL) (CONS NIL 1))
                               (SETQ POT2 (CONS H POT2))))
                             NIL))
                          (CAR H))
                         (SETQ H (CDR H))
                         (GO LAB))
                       (COND
                        ((OR (NULL (NOT_INCLUDED POT1 POT2))
                             (NULL (NOT_INCLUDED POT2 POT1)))
                         (SETQ P2 NIL))))))
                    (COND
                     ((AND P2 (EQUAL N1 N2))
                      (PROGN
                       (SETQ POT1 (NEWFCT FNAME_ VL NFCT_))
                       (SETQ POT2 POT1)
                       (SETQ NFCT_ (ADD1 NFCT_))
                       (SETQ FNEW_ (CONS POT1 FNEW_))
                       (SETQ FLIN_ (FCTINSERT POT1 FLIN_))
                       (SETQ V VLREV)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT V) (RETURN NIL)))
                         (PROGN
                          (SETQ COP (CAR IFAC1A))
                          (SETQ IFAC1A (CDR IFAC1A))
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT COP) (RETURN NIL)))
                            (PROGN
                             (SETQ POT1
                                     (REVAL1
                                      (LIST 'QUOTIENT (LIST 'DF POT1 (CAR V))
                                            (CAR COP))
                                      T))
                             (SETQ COP (CDR COP)))
                            (GO WHILELABEL))
                          (SETQ COP (CAR IFAC2A))
                          (SETQ IFAC2A (CDR IFAC2A))
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT COP) (RETURN NIL)))
                            (PROGN
                             (SETQ POT2
                                     (REVAL1
                                      (LIST 'QUOTIENT (LIST 'DF POT2 (CAR V))
                                            (CAR COP))
                                      T))
                             (SETQ COP (CDR COP)))
                            (GO WHILELABEL))
                          (SETQ V (CDR V))
                          NIL)
                         (GO WHILELABEL))
                       (SETQ PNEW
                               (ADDINTCO (LIST 'PLUS P1 (REVAL1 POT2 T)) FTEM
                                IFAC1B VLREV NIL))
                       (COND ((NULL PNEW) (SETQ SECND T))
                             (T
                              (PROGN
                               (SETQ AIC
                                       (ADDINTCO
                                        (LIST 'PLUS P2
                                              (LIST 'MINUS (REVAL1 POT1 T)))
                                        FTEM IFAC2B VLREV NIL))
                               (COND ((NULL AIC) (SETQ SECND T))
                                     (T
                                      (PROGN
                                       (SETQ NEWCOND (CONS AIC NEWCOND))
                                       (SETQ IFLAG 'POTINT))))))))))
                    (COND (PRI (PROGN (PRIN2 ":::") NIL)))
                    NIL)))
                 (SETQ SECND (NOT SECND))
                 (COND
                  ((AND (NEQ IFLAG 'POTINT) SECND)
                   (PROGN
                    (SETQ COP ILI1A)
                    (SETQ ILI1A ILI2A)
                    (SETQ ILI2A COP)))))
                (COND
                 ((NOT (OR (EQ IFLAG 'POTINT) (NOT SECND)))
                  (GO REPEATLABEL)))))))))
         NIL)))
      (RETURN
       (COND ((NULL IFLAG) NIL)
             (T
              (PROGN
               (COND ((EQUAL IFLAG 'POTINT) NIL)
                     (T
                      (SETQ PNEW
                              (ADDINTCO P FTEM
                               (PROGN
                                (SETQ H NIL)
                                (PROG ()
                                 WHILELABEL
                                  (COND ((NOT IFAC0) (RETURN NIL)))
                                  (PROGN
                                   (SETQ H (CONS (REVERSE (CAR IFAC0)) H))
                                   (SETQ IFAC0 (CDR IFAC0)))
                                  (GO WHILELABEL))
                                H)
                               VL VARI))))
               (COND ((NULL PNEW) NIL)
                     (T
                      (PROGN
                       (COND
                        (PRI
                         (PROGN
                          (TERPRI)
                          (PROGN (PRIN2 "ENDE INTEGRATEPDE") NIL)
                          (DEPRINT (CONS PNEW NEWCOND)))))
                       (CONS PNEW NEWCOND)))))))))) 
(PUT 'INTPDE 'NUMBER-OF-ARGS 5) 
(PUT 'INTPDE 'DEFINED-ON-LINE '850) 
(PUT 'INTPDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTPDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTPDE (P FTEM VL X POTINT)
    (PROG (FT IP H ITGL1 ITGL2)
      (COND
       ((OR POTINT (NULL LIN_PROBLEM)) (RETURN (INTPDE_ P FTEM VL X POTINT))))
      (PROG (H)
        (SETQ H FTEM)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND ((NOT (FREEOF (ASSOC H DEPL*) X)) (SETQ FT (CONS H FT)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ IP (INT_PARTITION P FT X))
      (COND ((NULL (CDR IP)) (RETURN (INTPDE_ P FTEM VL X POTINT))))
      (PROG ()
       WHILELABEL
        (COND ((NOT IP) (RETURN NIL)))
        (PROGN
         (SETQ H (INTPDE_ (CAR IP) FTEM VL X POTINT))
         (COND
          ((NULL H) (PROGN (SETQ IP NIL) (SETQ ITGL1 NIL) (SETQ ITGL2 NIL)))
          (T
           (PROGN
            (SETQ ITGL1 (NCONC (LIST (CAR H)) ITGL1))
            (SETQ ITGL2 (NCONC (LIST (CADR H)) ITGL2))
            (SETQ IP (CDR IP)))))
         NIL)
        (GO WHILELABEL))
      (COND
       (ITGL1
        (PROGN
         (SETQ ITGL1 (REVAL1 (CONS 'PLUS ITGL1) T))
         (SETQ ITGL2 (REVAL1 (CONS 'PLUS ITGL2) T)))))
      (RETURN (COND ((NULL ITGL1) NIL) (T (LIST ITGL1 ITGL2)))))) 
(PUT 'DROP_X_DIF 'NUMBER-OF-ARGS 2) 
(PUT 'DROP_X_DIF 'DEFINED-ON-LINE '886) 
(PUT 'DROP_X_DIF 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DROP_X_DIF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DROP_X_DIF (DER X)
    (PROG (DV NEWDV)
      (SETQ DV (CDDR DER))
      (PROG ()
       WHILELABEL
        (COND ((NOT DV) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (CAR DV) X)
           (COND ((AND (CDR DV) (FIXP (CADR DV))) (SETQ DV (CDR DV))) (T NIL)))
          (T (SETQ NEWDV (CONS (CAR DV) NEWDV))))
         (SETQ DV (CDR DV)))
        (GO WHILELABEL))
      (RETURN
       (COND (NEWDV (CONS 'DF (CONS (CADR DER) (REVERSE NEWDV))))
             (T (CADR DER)))))) 
(PUT 'STRIP_X 'NUMBER-OF-ARGS 3) 
(PUT 'STRIP_X 'DEFINED-ON-LINE '902) 
(PUT 'STRIP_X 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'STRIP_X 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STRIP_X (EX FT X)
    (PROG (TM CEX)
      (RETURN
       (COND ((FREEOFLIST EX FT) 1) ((NOT (PAIRP EX)) EX)
             ((EQUAL (CAR EX) 'MINUS) (STRIP_X (CADR EX) FT X))
             ((EQUAL (CAR EX) 'DF) (DROP_X_DIF EX X))
             ((EQUAL (CAR EX) 'EXPT)
              (COND ((NOT (PAIRP (CADR EX))) EX)
                    ((EQUAL (CAADR EX) 'DF)
                     (LIST 'EXPT (DROP_X_DIF (CADR EX) X) (CADDR EX)))
                    (T 1)))
             ((EQUAL (CAR EX) 'TIMES)
              (PROGN
               (SETQ EX (CDR EX))
               (PROG ()
                WHILELABEL
                 (COND ((NOT EX) (RETURN NIL)))
                 (PROGN
                  (SETQ CEX (CAR EX))
                  (SETQ EX (CDR EX))
                  (COND
                   ((NOT (FREEOFLIST CEX FT))
                    (COND ((NOT (PAIRP CEX)) (SETQ TM (CONS CEX TM)))
                          ((EQUAL (CAR CEX) 'DF)
                           (SETQ TM (CONS (DROP_X_DIF CEX X) TM)))
                          ((EQUAL (CAR CEX) 'EXPT)
                           (COND
                            ((NOT (PAIRP (CADR CEX))) (SETQ TM (CONS CEX TM)))
                            ((EQUAL (CAADR CEX) 'DF)
                             (SETQ TM
                                     (CONS
                                      (LIST 'EXPT (DROP_X_DIF (CADR CEX) X)
                                            (CADDR CEX))
                                      TM)))))))))
                 (GO WHILELABEL))
               (COND ((NULL TM) 1)
                     ((GREATERP (LENGTH TM) 1) (REVAL1 (CONS 'TIMES TM) T))
                     (T (CAR TM)))))
             (T 1))))) 
(PUT 'SORT_PARTITION 'NUMBER-OF-ARGS 4) 
(PUT 'SORT_PARTITION 'DEFINED-ON-LINE '934) 
(PUT 'SORT_PARTITION 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SORT_PARTITION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SORT_PARTITION (PNAME P FT X)
    (PROG (STCP PCOP PARTI)
      (COND
       (PNAME
        (COND ((GET PNAME 'PARTITIONED) (RETURN (GET PNAME 'PARTITIONED)))
              (T (PROGN (CP_SQ2P_VAL PNAME) (SETQ P (GET PNAME 'PVAL)))))))
      (COND ((OR (NOT (PAIRP P)) (NEQ (CAR P) 'PLUS)) (SETQ P (LIST P)))
            (T (SETQ P (CDR P))))
      (COND ((NULL FT) (SETQ PARTI (LIST (LIST 1 1 P))))
            (T
             (PROG ()
              WHILELABEL
               (COND ((NOT P) (RETURN NIL)))
               (PROGN
                (SETQ STCP (STRIP_X (CAR P) FT X))
                (SETQ PCOP PARTI)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND PCOP (NEQ (CAAR PCOP) STCP))) (RETURN NIL)))
                  (SETQ PCOP (CDR PCOP))
                  (GO WHILELABEL))
                (COND
                 ((NULL PCOP)
                  (SETQ PARTI (CONS (LIST STCP 1 (LIST (CAR P))) PARTI)))
                 (T
                  (RPLACA PCOP
                          (LIST STCP (ADD1 (CADAR PCOP))
                                (CONS (CAR P) (CADDAR PCOP))))))
                (SETQ P (CDR P)))
               (GO WHILELABEL))))
      (COND ((AND PNAME KEEP_PARTI) (PUT PNAME 'PARTITIONED PARTI)))
      (RETURN PARTI))) 
(PUT 'INT_PARTITION 'NUMBER-OF-ARGS 3) 
(PUT 'INT_PARTITION 'DEFINED-ON-LINE '961) 
(PUT 'INT_PARTITION 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INT_PARTITION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INT_PARTITION (P FT X)
    (PROG (PARTI PCOP)
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
        (RETURN
         (COND ((NOT (FREEOFLIST (CADDR P) FT)) (LIST P))
               (T
                (PROGN
                 (SETQ PCOP (INT_PARTITION (CADR P) FT X))
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H PCOP)
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (H) (LIST 'QUOTIENT H (CADDR P)))
                                     (CAR H))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (H) (LIST 'QUOTIENT H (CADDR P))) (CAR H))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))))
      (SETQ PARTI (SORT_PARTITION NIL P FT X))
      (SETQ PARTI
              (IDX_SORT
               (PROG (H FORALL-RESULT FORALL-ENDPTR)
                 (SETQ H PARTI)
                 (COND ((NULL H) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL)))
                LOOPLABEL
                 (SETQ H (CDR H))
                 (COND ((NULL H) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN
       (PROG (H FORALL-RESULT FORALL-ENDPTR)
         (SETQ H PARTI)
         (COND ((NULL H) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (H)
                             (COND ((EQUAL (CAR H) 1) (CAADR H))
                                   (T (CONS 'PLUS (CADR H)))))
                           (CAR H))
                          NIL)))
        LOOPLABEL
         (SETQ H (CDR H))
         (COND ((NULL H) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (H)
                     (COND ((EQUAL (CAR H) 1) (CAADR H))
                           (T (CONS 'PLUS (CADR H)))))
                   (CAR H))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'INTPDE_ 'NUMBER-OF-ARGS 5) 
(PUT 'INTPDE_ 'DEFINED-ON-LINE '979) 
(PUT 'INTPDE_ 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTPDE_ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTPDE_ (P FTEM VL X POTINT)
    (PROG (F FT L L1 L2 L3 L4 K S A IFLAG FLAG)
      (SETQ FT FTEM)
      (SETQ VL (CONS X (DELETE X VL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FTEM (NOT FLAG))) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FTEM))
         (COND
          ((MEMBER X (FCTARGS F))
           (PROGN
            (COND
             ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
              (PROGN
               (SETQ L1 (LDERIV (CADR P) F VL))
               (COND
                ((NEQ (CDR L1) 'INFINITY)
                 (PROGN
                  (SETQ L2 (LDERIV (CADDR P) F VL))
                  (COND ((EQUAL (CDR L2) 'INFINITY) (SETQ L1 L2))
                        (T
                         (PROGN
                          (COND
                           ((AND (CAR L1) (EQUAL (CAR L1) (CAR L2)))
                            (SETQ L1 (CONS (CAR L1) 2)))
                           (T
                            (PROGN
                             (SETQ L
                                     (LDERIV (LIST 'PLUS (CAR L1) (CAR L2)) F
                                      VL))
                             (COND
                              ((AND (CAR L2) (EQUAL (CAR L) (CAR L2)))
                               (SETQ L1 (CONS (CAR L2) (MINUS 1))))))))))))))
               (SETQ L L1)
               NIL))
             (T (SETQ L1 (SETQ L (LDERIV P F VL)))))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (NOT
                  (OR FLAG
                      (PROGN
                       (SETQ IFLAG (INTLINTEST L X))
                       (COND
                        ((OR (EQUAL IFLAG 'NOXDRV) (EQUAL IFLAG 'NODRV))
                         (PROGN
                          (SETQ L2 (START_LET_RULES))
                          (SETQ P (REVAL1 P T))
                          (STOP_LET_RULES L2)
                          (SETQ L (LDERIV P F VL))
                          (SETQ IFLAG (INTLINTEST L X))))
                        ((AND POTINT (EQUAL IFLAG 'NONLIN) (PAIRP P)
                              (EQUAL (CAR P) 'PLUS) (NULL L3)
                              (NEQ (CDR L) 'INFINITY))
                         (PROGN
                          (SETQ L3 T)
                          (SETQ L4 L)
                          (SETQ L (CONS (CAR L) 1))
                          (SETQ IFLAG (INTLINTEST L X))
                          (SETQ L L4))))
                       IFLAG))))
                (RETURN NIL)))
              (PROGN
               (COND
                ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
                 (SETQ K
                         (REVAL1
                          (LIST 'QUOTIENT (COEFFN (CADR P) (CAR L) (CDR L))
                                (CADDR P))
                          T)))
                (T (SETQ K (REVAL1 (COEFFN P (CAR L) (CDR L)) T))))
               (COND
                ((INTCOEFFTEST (CAR (LDERIV K F VL)) (CAR L) VL)
                 (PROGN
                  (SETQ A (DECDERIV (CAR L) X))
                  (SETQ K (ERR_CATCH_INT (SUBST 'V_A_R_ A K) 'V_A_R_))
                  (COND ((NULL K) (PROGN (SETQ K 0) (SETQ FLAG 'TOO_SLOW))))
                  (COND (LIN_PROBLEM (SETQ L4 NIL))
                        (T (SETQ L4 (ZERO_DEN K FT))))
                  (COND (L4 (PROGN (SETQ FLAG 'NEEDS_CASE_SPLIT)))
                        (T
                         (PROGN
                          (SETQ K (REVAL1 (SUBST A 'V_A_R_ K) T))
                          (SETQ S (CONS K S))
                          (SETQ P (LIST 'DIFFERENCE P (LIST 'DF K X)))
                          (SETQ P (ERR_CATCH_REVAL P))
                          (COND ((NULL P) (SETQ FLAG 'REVAL_TOO_SLOW))
                                ((DIFFRELP L1 (SETQ L (LDERIV P F VL)) VL)
                                 (SETQ FLAG 'NEVERENDING))
                                (T (SETQ L1 L))))))))
                (T (SETQ FLAG 'COEFFLD))))
              (GO WHILELABEL))
            (COND
             ((AND (NEQ FLAG 'REVAL_TOO_SLOW) (NEQ FLAG 'NEVERENDING))
              (COND ((EQUAL IFLAG 'NOFCT) (SETQ FTEM (SMEMBERL FTEM P)))
                    ((OR POTINT (LESSP (FCTLENGTH F) (LENGTH VL)))
                     (PROGN (SETQ FTEM (CDR FTEM)) (SETQ FLAG NIL)))
                    (T (SETQ FLAG (OR IFLAG FLAG))))))))
          (T (SETQ FTEM (CDR FTEM)))))
        (GO WHILELABEL))
      (RETURN
       (COND (FLAG NIL)
             (T
              (PROGN
               (SETQ L (EXPLICITPART P FT X))
               (SETQ L1 (ERR_CATCH_INT L X))
               (COND ((NULL L1) NIL)
                     (T
                      (PROGN
                       (SETQ S (REVAL1 (CONS 'PLUS (CONS L1 S)) T))
                       (COND (LIN_PROBLEM (SETQ L4 NIL))
                             (T (SETQ L4 (ZERO_DEN S FT))))
                       (COND
                        (L4
                         (PROGN
                          (PROG (L2)
                            (SETQ L2 L4)
                           LAB
                            (COND ((NULL L2) (RETURN NIL)))
                            ((LAMBDA (L2)
                               (SETQ TO_DO_LIST
                                       (UNION
                                        (LIST (LIST 'SPLIT_INTO_CASES L2))
                                        TO_DO_LIST)))
                             (CAR L2))
                            (SETQ L2 (CDR L2))
                            (GO LAB))
                          NIL))
                        ((AND FREEINT_ (NULL (FREEOF S 'INT))) NIL)
                        ((AND FREEABS_ (NULL (FREEOF S 'ABS))) NIL)
                        (T
                         (PROGN
                          (SETQ K (START_LET_RULES))
                          ((LAMBDA (*PRECISE)
                             (SETQ L2 (REVAL1 (LIST 'DF L1 X) T)))
                           NIL)
                          (COND
                           ((NEQ 0
                                 ((LAMBDA (*PRECISE)
                                    (REVAL1 (LIST 'DIFFERENCE L L2) T))
                                  NIL))
                            (PROGN
                             (PROGN (PRIN2 "REDUCE integrator error:") NIL)
                             (TERPRI)
                             (PROGN
                              (ASSGNPRI (AEVAL "int(") NIL 'FIRST)
                              (ASSGNPRI (AEVAL L) NIL NIL)
                              (ASSGNPRI (AEVAL ",") NIL NIL)
                              (ASSGNPRI X NIL NIL)
                              (ASSGNPRI (AEVAL ") neq ") NIL NIL)
                              (ASSGNPRI (AEVAL L1) NIL 'LAST))
                             (TERPRI)
                             (PROGN (PRIN2 "Result ignored.") NIL)
                             (TERPRI)
                             (STOP_LET_RULES K)
                             NIL))
                           (T
                            (PROGN
                             (SETQ P (REVAL1 (LIST 'DIFFERENCE P L2) T))
                             (STOP_LET_RULES K)
                             (COND
                              (POLY_ONLY
                               (COND ((RATEXP S FT) (LIST S P)) (T NIL)))
                              (T (LIST S P)))))))))))))))))) 
(PUT 'EXPLICITPART 'NUMBER-OF-ARGS 3) 
(PUT 'EXPLICITPART 'DEFINED-ON-LINE '1119) 
(PUT 'EXPLICITPART 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'EXPLICITPART 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPLICITPART (P FT X)
    (PROG (L)
      (COND ((NOT (MEMBER X (ARGSET (SMEMBERL FT P)))) (SETQ L P))
            ((PAIRP P)
             (PROGN
              (COND
               ((EQUAL (CAR P) 'MINUS)
                (SETQ L
                        (REVAL1 (LIST 'MINUS (EXPLICITPART (CADR P) FT X))
                                T))))
              (COND
               ((AND (EQUAL (CAR P) 'QUOTIENT)
                     (NOT (MEMBER X (ARGSET (SMEMBERL FT (CADDR P))))))
                (SETQ L
                        (REVAL1
                         (LIST 'QUOTIENT (EXPLICITPART (CADR P) FT X)
                               (CADDR P))
                         T))))
              (COND
               ((EQUAL (CAR P) 'PLUS)
                (PROGN
                 (PROG (A)
                   (SETQ A (CDR P))
                  LAB
                   (COND ((NULL A) (RETURN NIL)))
                   ((LAMBDA (A)
                      (COND
                       ((NOT (MEMBER X (ARGSET (SMEMBERL FT A))))
                        (SETQ L (CONS A L)))))
                    (CAR A))
                   (SETQ A (CDR A))
                   (GO LAB))
                 (COND ((PAIRP L) (SETQ L (REVAL1 (CONS 'PLUS L) T))))))))))
      (COND ((NOT L) (SETQ L 0)))
      (RETURN L))) 
(PUT 'INTCONST 'NUMBER-OF-ARGS 4) 
(PUT 'INTCONST 'DEFINED-ON-LINE '1135) 
(PUT 'INTCONST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTCONST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTCONST (CO VL X IFALIST)
    (COND ((NULL IFALIST) 0)
          (T
           (PROG (L L2 F COLI COTMP)
             (PROG ()
              WHILELABEL
               (COND ((NOT IFALIST) (RETURN NIL)))
               (PROGN
                (SETQ COTMP COLI)
                (SETQ COLI NIL)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT COTMP) (RETURN NIL)))
                  (PROGN
                   (SETQ COLI
                           (CONS
                            (LIST 'INT (LIST 'TIMES (CAR IFALIST) (CAR COTMP))
                                  X)
                            COLI))
                   (SETQ COTMP (CDR COTMP)))
                  (GO WHILELABEL))
                (SETQ COLI (CONS 1 COLI))
                (SETQ IFALIST (CDR IFALIST)))
               (GO WHILELABEL))
             (PROG ()
              WHILELABEL
               (COND ((NOT COLI) (RETURN NIL)))
               (PROGN
                (SETQ F (NEWFCT FNAME_ (DELETE X VL) NFCT_))
                (SETQ NFCT_ (ADD1 NFCT_))
                (SETQ FNEW_ (CONS F FNEW_))
                (SETQ FLIN_ (FCTINSERT F FLIN_))
                (SETQ L (CONS (LIST 'TIMES F (CAR COLI)) L))
                (SETQ COLI (CDR COLI)))
               (GO WHILELABEL))
             (COND ((GREATERP (LENGTH L) 1) (SETQ L (CONS 'PLUS L)))
                   ((PAIRP L) (SETQ L (CAR L))) (T (SETQ L 0)))
             (COND
              ((AND CO (NEQ CO 1))
               (COND
                ((PAIRP CO)
                 (PROGN
                  (COND ((EQUAL (CAR CO) 'TIMES) (SETQ CO (CDR CO)))
                        (T (SETQ CO (LIST CO))))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT CO) (RETURN NIL)))
                    (PROGN
                     (COND
                      ((MY_FREEOF (CAR CO) X) (SETQ L2 (CONS (CAR CO) L2))))
                     (SETQ CO (CDR CO)))
                    (GO WHILELABEL))))
                ((NEQ CO X) (SETQ L2 (LIST CO))))))
             (RETURN (REVAL1 (COND (L2 (CONS 'TIMES (CONS L L2))) (T L)) T)))))) 
(PUT 'INTCOEFFTEST 'NUMBER-OF-ARGS 3) 
(PUT 'INTCOEFFTEST 'DEFINED-ON-LINE '1173) 
(PUT 'INTCOEFFTEST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTCOEFFTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTCOEFFTEST (L L1 VL)
    (PROG (S)
      (RETURN
       (COND ((NOT (PAIRP L)) T)
             ((EQUAL (CAR L) 'DF)
              (PROGN
               (SETQ S (DECDERIV L1 (CAR VL)))
               (COND ((AND (PAIRP S) (PAIRP (CDR S))) (SETQ S (CDDR S)))
                     (T (SETQ S NIL)))
               (COND ((DIFFRELP (CONS (CDDR L) 1) (CONS S 1) VL) T) (T NIL))))
             (T T))))) 
(PUT 'FCTLINEAR 'NUMBER-OF-ARGS 2) 
(PUT 'FCTLINEAR 'DEFINED-ON-LINE '1185) 
(PUT 'FCTLINEAR 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'FCTLINEAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FCTLINEAR (P F)
    (PROGN (SETQ P (LDIFFP P F)) (AND (NULL (CAR P)) (EQUAL (CDR P) 1)))) 
(PUT 'INTLINTEST 'NUMBER-OF-ARGS 2) 
(PUT 'INTLINTEST 'DEFINED-ON-LINE '1189) 
(PUT 'INTLINTEST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTLINTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTLINTEST (L X)
    (COND ((NOT (CAR L)) (COND ((ZEROP (CDR L)) 'NOFCT) (T 'NONLIN)))
          ((AND (CAR L) (EQUAL (CDR L) 1))
           (COND
            ((PAIRP (CAR L))
             (COND
              ((EQUAL (CAAR L) 'DF)
               (COND ((MEMBER X (CDDAR L)) NIL)
                     ((MEMBER X (FCTARGS (CADAR L))) 'NOXDRV) (T 'NOXDEP)))
              (T 'NODRV)))
            (T 'NODRV)))
          (T 'NONLIN))) 
(PUT 'DECDERIV 'NUMBER-OF-ARGS 2) 
(PUT 'DECDERIV 'DEFINED-ON-LINE '1207) 
(PUT 'DECDERIV 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DECDERIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECDERIV (L X)
    (PROG (L1)
      (RETURN
       (COND
        (L
         (COND
          ((EQUAL (CAR L) 'DF)
           (PROGN
            (SETQ L1 (DECDERIV1 (CDDR L) X))
            (COND (L1 (CONS 'DF (CONS (CADR L) L1))) (T (CADR L)))))
          (T L)))
        (T NIL))))) 
(PUT 'DECDERIV1 'NUMBER-OF-ARGS 2) 
(PUT 'DECDERIV1 'DEFINED-ON-LINE '1218) 
(PUT 'DECDERIV1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DECDERIV1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECDERIV1 (L X)
    (COND ((NULL L) NIL)
          ((EQUAL (CAR L) X)
           (COND
            ((CDR L)
             (COND
              ((NUMBERP (CADR L))
               (COND
                ((GREATERP (CADR L) 2)
                 (CONS (CAR L) (CONS (SUB1 (CADR L)) (CDDR L))))
                (T (CONS (CAR L) (CDDR L)))))
              (T (CDR L))))
            (T NIL)))
          (T (CONS (CAR L) (DECDERIV1 (CDR L) X))))) 
(PUT 'INTEGRATEDE 'NUMBER-OF-ARGS 3) 
(PUT 'INTEGRATEDE 'DEFINED-ON-LINE '1230) 
(PUT 'INTEGRATEDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRATEDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATEDE (Q FTEM GENFLAG)
    (PROG (L L1 L2 FL LTDL)
      (SETQ FTEM (SMEMBERL FTEM Q))
      (SETQ LTDL (LENGTH TO_DO_LIST))
     AGAIN
      (COND
       ((SETQ L1 (INTEGRABLEODE Q FTEM))
        (COND
         ((SETQ L1 (INTEGRATEODE Q (CAR L1) (CADR L1) (CADDR L1) FTEM))
          (PROGN
           (SETQ L (APPEND (CDR L1) L))
           (SETQ Q
                   (PREPSQ
                    (CAR (SIMPLIFYPDESQ (SIMP (CAR L1)) FTEM NIL NIL NIL))))
           (SETQ FTEM (SMEMBERL (UNION FNEW_ FTEM) Q))
           (SETQ FL T)))
         ((AND (LESSP LTDL (LENGTH TO_DO_LIST))
               (EQUAL (CAAR TO_DO_LIST) 'SPLIT_INTO_CASES))
          (RETURN NIL)))))
      (COND
       ((SETQ L1 (INTEGRATEPDE Q FTEM NIL GENFLAG POTINT_))
        (PROGN
         (SETQ Q (CAR L1))
         (PROG (A)
           (SETQ A (CDR L1))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (PROGN
               (SETQ FTEM (UNION FNEW_ FTEM))
               (COND
                ((SETQ L2 (INTEGRATEDE A FTEM NIL)) (SETQ L (APPEND L2 L)))
                (T (SETQ L (CONS A L))))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ FL T)
         (COND ((NULL GENFLAG) (SETQ L1 NIL)))
         (SETQ FTEM (SMEMBERL (UNION FNEW_ FTEM) Q))
         (GO AGAIN))))
      (COND
       (FL
        (PROGN
         (SETQ L (CONS Q L))
         (SETQ L
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A L)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (REVAL1 A T)) (CAR A))
                                         NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (A) (REVAL1 A T)) (CAR A)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ L
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A L)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A)
                                       (COND
                                        ((AND (PAIRP A)
                                              (EQUAL (CAR A) 'QUOTIENT))
                                         (CADR A))
                                        (T A)))
                                     (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (A)
                               (COND
                                ((AND (PAIRP A) (EQUAL (CAR A) 'QUOTIENT))
                                 (CADR A))
                                (T A)))
                             (CAR A))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (RETURN L))) 
(PUT 'INTFLAGTEST 'NUMBER-OF-ARGS 2) 
(PUT 'INTFLAGTEST 'DEFINED-ON-LINE '1276) 
(PUT 'INTFLAGTEST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTFLAGTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTFLAGTEST (Q FULLINT)
    (COND
     ((FLAGP Q 'TO_INT)
      (COND
       (FULLINT
        (COND ((NULL (FLAGP Q 'TO_FULLINT)) NIL) ((GET Q 'STARDE) NIL)
              ((NULL (GET Q 'ALLVARFCTS)) NIL)
              (T
               (PROG (FL VL DL L N MI)
                 (SETQ N (GET Q 'NVARS))
                 (PROG (F)
                   (SETQ F (GET Q 'RATIONAL))
                  LAB
                   (COND ((NULL F) (RETURN NIL)))
                   ((LAMBDA (F)
                      (COND ((EQUAL (FCTLENGTH F) N) (SETQ FL (CONS F FL)))))
                    (CAR F))
                   (SETQ F (CDR F))
                   (GO LAB))
                 (COND ((NULL FL) (RETURN NIL)))
                 (SETQ VL (GET Q 'VARS))
                 (SETQ DL (GET Q 'DERIVS))
                 (PROG (D)
                   (SETQ D DL)
                  LAB
                   (COND ((NULL D) (RETURN NIL)))
                   ((LAMBDA (D)
                      (COND
                       ((MEMBER (CAAR D) FL)
                        (PUT (CAAR D) 'MAXDERIVS
                             (MAXDERIVS (GET (CAAR D) 'MAXDERIVS) (CDAR D)
                              VL)))))
                    (CAR D))
                   (SETQ D (CDR D))
                   (GO LAB))
                 (SETQ DL FL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT VL) (RETURN NIL)))
                   (PROGN
                    (SETQ MI (CAR (GET (CAR FL) 'MAXDERIVS)))
                    (SETQ L (LIST (CAR FL)))
                    (PUT (CAR FL) 'MAXDERIVS (CDR (GET (CAR FL) 'MAXDERIVS)))
                    (PROG (F)
                      (SETQ F (CDR FL))
                     LAB
                      (COND ((NULL F) (RETURN NIL)))
                      ((LAMBDA (F)
                         (PROGN
                          (COND
                           ((EQUAL (SETQ N (CAR (GET F 'MAXDERIVS))) MI)
                            (SETQ L (CONS F L)))
                           ((LESSP N MI)
                            (PROGN (SETQ L (LIST F)) (SETQ MI N))))
                          (PUT F 'MAXDERIVS (CDR (GET F 'MAXDERIVS)))))
                       (CAR F))
                      (SETQ F (CDR F))
                      (GO LAB))
                    (SETQ DL (INTERSECTION L DL))
                    (COND (DL (SETQ VL (CDR VL))) (T (SETQ VL NIL))))
                   (GO WHILELABEL))
                 (PROG (F)
                   (SETQ F FL)
                  LAB
                   (COND ((NULL F) (RETURN NIL)))
                   ((LAMBDA (F) (REMPROP F 'MAXDERIVS)) (CAR F))
                   (SETQ F (CDR F))
                   (GO LAB))
                 (COND
                  ((AND FULLINT (NULL DL)) (REMFLAG (LIST Q) 'TO_FULLINT)))
                 (RETURN DL)))))
       (T T))))) 
(PUT 'INTEGRATE 'NUMBER-OF-ARGS 4) 
(PUT 'INTEGRATE 'DEFINED-ON-LINE '1316) 
(PUT 'INTEGRATE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRATE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATE (Q GENINTFLAG FULLINT PDES)
    (PROG (L FLI FNEW_OLD H LOFTOLIST)
      (COND
       ((SETQ FLI (INTFLAGTEST Q FULLINT))
        (PROGN
         (COND (FULLINT (PROGN (SETQ FNEW_OLD FNEW_) (SETQ FNEW_ NIL))))
         (CP_SQ2P_VAL Q)
         (SETQ LOFTOLIST (LENGTH TO_DO_LIST))
         (COND
          ((SETQ L (INTEGRATEDE (GET Q 'PVAL) (GET Q 'FCTS) GENINTFLAG))
           (PROGN
            (COND
             (FULLINT
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND FLI
                        (OR
                         (NOT (NULL (CAR (SETQ H (LDIFFP (CAR L) (CAR FLI))))))
                         (NEQ (CDR H) 1)
                         (PROGN
                          (SETQ H (COEFFN (CAR L) (CAR FLI) 1))
                          (COND ((OR (ATOM H) (ATOM (CAR H))) NIL)
                                (T
                                 (PROGN
                                  (SETQ H
                                          (SIMPLIFYSQ (CADR H) (GET Q 'FCTS) T
                                           NIL T))
                                  (COND ((EQUAL H (LIST (CONS 1 1))) T)
                                        (T NIL)))))))))
                  (RETURN NIL)))
                (SETQ FLI (CDR FLI))
                (GO WHILELABEL))))
            (COND
             ((NULL FLI)
              (PROGN
               (REMFLAG (LIST Q) 'TO_FULLINT)
               (PROG (F)
                 (SETQ F FNEW_)
                LAB
                 (COND ((NULL F) (RETURN NIL)))
                 ((LAMBDA (F) (DROP_FCT F)) (CAR F))
                 (SETQ F (CDR F))
                 (GO LAB))
               (SETQ FNEW_ FNEW_OLD)
               (SETQ L NIL)
               (COND
                (PRINT_
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "Not enough integrations to solve for a function")
                   NIL)
                  (COND
                   ((NULL LIN_PROBLEM)
                    (PROGN
                     (PROGN (PRIN2 ", or,") NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 "substitution prevented through non-linearity.")
                      NIL)))
                   (T (PROGN (PRIN2 ".") NIL))))))))
             (T
              (PROGN
               (SETQ FNEW_ (UNION FNEW_OLD FNEW_))
               (PROG (F)
                 (SETQ F FNEW_)
                LAB
                 (COND ((NULL F) (RETURN NIL)))
                 ((LAMBDA (F)
                    (PROGN
                     (SETQ FTEM_ (FCTINSERT F FTEM_))
                     (SETQ FLIN_ (CONS F FLIN_))))
                  (CAR F))
                 (SETQ F (CDR F))
                 (GO LAB))
               (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
               (SETQ FNEW_ NIL)
               (SETQ H (CDR L))
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND H (NEQ (CAR H) (GET Q 'PVAL)))) (RETURN NIL)))
                 (SETQ H (CDR H))
                 (GO WHILELABEL))
               (COND
                (H
                 (PROGN
                  (SETQ L (DELETE (GET Q 'PVAL) L))
                  (SETQ L
                          (CONS Q
                                (MKEQSQLIST NIL NIL L FTEM_ (GET Q 'VARS)
                                 ALLFLAGS_ T (GET Q 'ORDERINGS) PDES)))
                  (COND
                   (PRINT_
                    (PROGN
                     (TERPRI)
                     (COND
                      ((AND L (CDR L) (CDDR L))
                       (PROGN
                        (PROGN
                         (PRIN2 "The equation ")
                         (PRIN2 Q)
                         (PRIN2 " is kept and an additional sufficient")
                         NIL)
                        (TERPRI)
                        (PROGN
                         (PRIN2 "integral with conditions ")
                         (PRIN2 (CDR L))
                         (PRIN2 " are added.")
                         NIL)))
                      (T
                       (PROGN
                        (PROGN
                         (PRIN2 "The equation ")
                         (PRIN2 Q)
                         (PRIN2 " is kept and an additional sufficient")
                         NIL)
                        (TERPRI)
                        (PROGN
                         (PRIN2 "integral ")
                         (PRIN2 (CADR L))
                         (PRIN2 " is added.")
                         NIL))))
                     (TERPRI))))))
                (T
                 (PROGN
                  (FLAG (LIST Q) 'TO_EVAL)
                  (UPDATESQ Q NIL NIL (CAR L) FTEM_ (GET Q 'VARS) T (LIST 0)
                   NIL)
                  (DROP_PDE_FROM_IDTIES Q PDES NIL)
                  (DROP_PDE_FROM_PROPERTIES Q PDES)
                  (SETQ L
                          (CONS Q
                                (MKEQSQLIST NIL NIL (CDR L) FTEM_ (GET Q 'VARS)
                                 ALLFLAGS_ T (GET Q 'ORDERINGS) PDES)))
                  (PUT Q 'DEC_WITH NIL)
                  (PUT Q 'DEC_WITH_RL NIL)
                  (COND
                   (PRINT_
                    (PROGN
                     (TERPRI)
                     (COND
                      ((CDR L)
                       (COND
                        ((EQUAL (GET Q 'NVARS) (GET (CADR L) 'NVARS))
                         (PROGN
                          (PRIN2 "Potential integration of ")
                          (PRIN2 Q)
                          (PRIN2 " yields ")
                          (PRIN2 L)
                          NIL))
                        (T
                         (PROGN
                          (PRIN2 "Partially potential integration of ")
                          (PRIN2 Q)
                          (PRIN2 " yields ")
                          (PRIN2 L)
                          NIL))))
                      (T (PROGN (PRIN2 "Integration of ") (PRIN2 Q) NIL)))
                     (TERPRI)))))))
               (COND
                ((EQUAL LOFTOLIST (LENGTH TO_DO_LIST))
                 (PROGN
                  (REMFLAG (LIST Q) 'TO_FULLINT)
                  (REMFLAG (LIST Q) 'TO_INT)))))))))
          (T
           (PROGN
            (REMFLAG (LIST Q) 'TO_FULLINT)
            (REMFLAG (LIST Q) 'TO_INT)))))))
      (RETURN L))) 
(PUT 'QUICK_INTEGRATE_ONE_PDE 'NUMBER-OF-ARGS 1) 
(PUT 'QUICK_INTEGRATE_ONE_PDE 'DEFINED-ON-LINE '1419) 
(PUT 'QUICK_INTEGRATE_ONE_PDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'QUICK_INTEGRATE_ONE_PDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUICK_INTEGRATE_ONE_PDE (PDES)
    (PROG (Q P R V NV NVC MINV MIORDR MINOFU MINODV ORDR NOFU NODV)
      (SETQ NV (NO_FNC_OF_V))
      (SETQ MIORDR 10000)
      (SETQ MINOFU 10000)
      (SETQ MINODV 10000)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND PDES (EQUAL (GET (CAR PDES) 'LENGTH) 1))) (RETURN NIL)))
        (PROGN
         (SETQ Q (GET (CAR PDES) 'DERIVS))
         (COND
          ((AND Q (CDAAR Q))
           (PROGN
            (SETQ Q (CAAR Q))
            (SETQ NODV 0)
            (SETQ ORDR 0)
            (SETQ R (CDR Q))
            (SETQ V (CADR Q))
            (PROG ()
             WHILELABEL
              (COND ((NOT R) (RETURN NIL)))
              (PROGN
               (COND
                ((FIXP (CAR R)) (SETQ ORDR (PLUS (DIFFERENCE ORDR 1) (CAR R))))
                (T (PROGN (SETQ ORDR (ADD1 ORDR)) (SETQ NODV (ADD1 NODV)))))
               (SETQ R (CDR R)))
              (GO WHILELABEL))
            (COND ((GREATERP NODV 1) (SETQ NOFU 10000))
                  (T
                   (PROGN
                    (SETQ NVC NV)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT (NEQ V (CAAR NVC))) (RETURN NIL)))
                      (SETQ NVC (CDR NVC))
                      (GO WHILELABEL))
                    (SETQ NOFU (CDAR NVC))
                    NIL)))
            (COND
             ((EQUAL NODV 1)
              (COND
               ((OR (AND (EQUAL ORDR 1) (GREATERP MIORDR 1))
                    (AND (A_BEFORE_B_ACCORDING_TO_C V MINV VL_)
                         (LEQ ORDR MIORDR))
                    (AND (EQUAL V MINV) (LESSP ORDR MIORDR))
                    (AND (EQUAL ORDR MIORDR) (LESSP NODV MINODV))
                    (AND (EQUAL NODV MINODV) (LESSP NOFU MINOFU)))
                (PROGN
                 (SETQ P (CAR PDES))
                 (SETQ MINV V)
                 (SETQ MINOFU NOFU)
                 (SETQ MIORDR ORDR)
                 (SETQ MINODV NODV))))))
            NIL)))
         (SETQ PDES (CDR PDES)))
        (GO WHILELABEL))
      (COND (P (SETQ P (INTEGRATE P NIL T PDES))))
      (RETURN P))) 
(PUT 'INTEGRATE_ONE_PDE 'NUMBER-OF-ARGS 3) 
(PUT 'INTEGRATE_ONE_PDE 'DEFINED-ON-LINE '1475) 
(PUT 'INTEGRATE_ONE_PDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRATE_ONE_PDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATE_ONE_PDE (PDES GENINTFLAG FULLINT)
    (PROG (L L1 M P PDESCP TDLCP)
      (SETQ TDLCP TO_DO_LIST)
      (SETQ M (MINUS 1))
      (SETQ PDESCP PDES)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDESCP) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (FLAGP (CAR PDESCP) 'TO_INT) (NOT (GET (CAR PDESCP) 'STARDE))
                (OR (NULL FULLINT) LIN_PROBLEM (GET (CAR PDESCP) 'LINEAR_)
                    (NOT (PAIRP (GET (CAR PDESCP) 'FAC)))))
           (PROGN
            (SETQ L (CONS (CAR PDESCP) L))
            (COND
             ((GREATERP (GET (CAR PDESCP) 'NVARS) M)
              (SETQ M (GET (CAR PDESCP) 'NVARS))))
            NIL)))
         (SETQ PDESCP (CDR PDESCP)))
        (GO WHILELABEL))
      (SETQ L (REVERSE L))
      (COND
       (MEM_EFF
        (PROG ()
         WHILELABEL
          (COND ((NOT L) (RETURN NIL)))
          (COND
           ((SETQ P (INTEGRATE (CAR L) GENINTFLAG FULLINT PDES)) (SETQ L NIL))
           (T (SETQ L (CDR L))))
          (GO WHILELABEL)))
       (T
        (PROG ()
         WHILELABEL
          (COND ((NOT (GEQ M 0)) (RETURN NIL)))
          (PROGN
           (SETQ L1 L)
           (PROG ()
            WHILELABEL
             (COND ((NOT L1) (RETURN NIL)))
             (COND
              ((AND (EQUAL (GET (CAR L1) 'NVARS) M)
                    (SETQ P (INTEGRATE (CAR L1) GENINTFLAG FULLINT PDES)))
               (PROGN (SETQ M (MINUS 1)) (SETQ L1 NIL)))
              (T (SETQ L1 (CDR L1))))
             (GO WHILELABEL))
           (SETQ M (SUB1 M)))
          (GO WHILELABEL))))
      (COND
       ((NULL P)
        (PROGN
         (SETQ TO_DO_LIST (CONS 1 TO_DO_LIST))
         (SETQ L TO_DO_LIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ (CDR L) TDLCP)) (RETURN NIL)))
           (COND ((FREEOFLIST (CADR L) FTEM_) (RPLACD L (CDDR L)))
                 (T (SETQ L (CDR L))))
           (GO WHILELABEL))
         (SETQ TO_DO_LIST (CDR TO_DO_LIST))
         NIL)))
      (RETURN P))) 
(ENDMODULE) 
(MODULE (LIST 'GENERALIZED_INTEGRATION)) 
(PUT 'GINTORDER 'NUMBER-OF-ARGS 3) 
(PUT 'GINTORDER 'DEFINED-ON-LINE '1548) 
(PUT 'GINTORDER 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'GINTORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GINTORDER (P VL X)
    (PROG (L L1 Q M B C Q1 Q2)
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
        (PROGN
         (SETQ Q (CADDR P))
         (SETQ P (CADR P))
         (SETQ L1 (GINTORDER1 Q X T))
         (SETQ Q1 (CAR L1))
         (SETQ Q2 (CADR L1))
         NIL)))
      (COND ((AND (PAIRP P) (EQUAL (CAR P) 'PLUS)) (SETQ P (CDR P)))
            (T (SETQ P (LIST P))))
      (PROG ()
       WHILELABEL
        (COND ((NOT P) (RETURN NIL)))
        (PROGN
         (SETQ L1 (GINTORDER1 (CAR P) X NIL))
         (COND
          ((DEPONALLVARS
            (COND ((EQUAL Q1 1) (CAR L1))
                  (T
                   (CONS 'TIMES
                         (APPEND
                          (COND
                           ((AND (PAIRP Q1) (EQUAL (CAR Q1) 'TIMES)) (CDR Q1))
                           (T (LIST Q1)))
                          (COND
                           ((AND (PAIRP (CAR L1)) (EQUAL (CAAR L1) 'TIMES))
                            (CDAR L1))
                           (T (LIST (CAR L1))))))))
            X VL)
           (SETQ L (SETQ P NIL)))
          (T (PROGN (SETQ L (TERMSORT L L1)) (SETQ P (CDR P))))))
        (GO WHILELABEL))
      (COND
       (L
        (PROGN
         (SETQ L
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A L)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A)
                                       (COND
                                        ((CDDR A)
                                         (PROGN
                                          (SETQ B (CAR A))
                                          (SETQ C
                                                  (CDR
                                                   (REVAL1
                                                    (COEFF1
                                                     (CONS 'PLUS (CDR A)) X
                                                     NIL)
                                                    T)))
                                          (SETQ M 0)
                                          (PROG ()
                                           WHILELABEL
                                            (COND
                                             ((NOT (AND C (EQUAL (CAR C) 0)))
                                              (RETURN NIL)))
                                            (PROGN
                                             (SETQ C (CDR C))
                                             (SETQ M (ADD1 M)))
                                            (GO WHILELABEL))
                                          (COND
                                           ((GREATERP M 0)
                                            (SETQ B
                                                    (LIST 'TIMES
                                                          (LIST 'EXPT X M)
                                                          B))))
                                          (CONS (REVAL1 B T) C)))
                                        (T
                                         (CONS (REVAL1 (CAR A) T)
                                               (CDR
                                                (REVAL1 (COEFF1 (CADR A) X NIL)
                                                        T))))))
                                     (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (A)
                               (COND
                                ((CDDR A)
                                 (PROGN
                                  (SETQ B (CAR A))
                                  (SETQ C
                                          (CDR
                                           (REVAL1
                                            (COEFF1 (CONS 'PLUS (CDR A)) X NIL)
                                            T)))
                                  (SETQ M 0)
                                  (PROG ()
                                   WHILELABEL
                                    (COND
                                     ((NOT (AND C (EQUAL (CAR C) 0)))
                                      (RETURN NIL)))
                                    (PROGN (SETQ C (CDR C)) (SETQ M (ADD1 M)))
                                    (GO WHILELABEL))
                                  (COND
                                   ((GREATERP M 0)
                                    (SETQ B (LIST 'TIMES (LIST 'EXPT X M) B))))
                                  (CONS (REVAL1 B T) C)))
                                (T
                                 (CONS (REVAL1 (CAR A) T)
                                       (CDR
                                        (REVAL1 (COEFF1 (CADR A) X NIL) T))))))
                             (CAR A))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          (Q
           (PROGN
            (SETQ L
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A L)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (A)
                                          (CONS (CAR A)
                                                (PROG (S FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ S (CDR A))
                                                  (COND
                                                   ((NULL S) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (S)
                                                                      (REVAL1
                                                                       (LIST
                                                                        'QUOTIENT
                                                                        S Q2)
                                                                       T))
                                                                    (CAR S))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ S (CDR S))
                                                  (COND
                                                   ((NULL S)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (S)
                                                              (REVAL1
                                                               (LIST 'QUOTIENT
                                                                     S Q2)
                                                               T))
                                                            (CAR S))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL))))
                                        (CAR A))
                                       NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (A)
                                  (CONS (CAR A)
                                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ S (CDR A))
                                          (COND ((NULL S) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (S)
                                                              (REVAL1
                                                               (LIST 'QUOTIENT
                                                                     S Q2)
                                                               T))
                                                            (CAR S))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ S (CDR S))
                                          (COND
                                           ((NULL S) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (S)
                                                      (REVAL1
                                                       (LIST 'QUOTIENT S Q2)
                                                       T))
                                                    (CAR S))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))
                                (CAR A))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ L
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A L)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (A)
                                          (CONS
                                           (REVAL1 (LIST 'QUOTIENT (CAR A) Q1)
                                                   T)
                                           (CDR A)))
                                        (CAR A))
                                       NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (A)
                                  (CONS (REVAL1 (LIST 'QUOTIENT (CAR A) Q1) T)
                                        (CDR A)))
                                (CAR A))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))
         NIL)))
      (RETURN L))) 
(PUT 'DEPONALLVARS 'NUMBER-OF-ARGS 3) 
(PUT 'DEPONALLVARS 'DEFINED-ON-LINE '1595) 
(PUT 'DEPONALLVARS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DEPONALLVARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEPONALLVARS (C X VL)
    (PROG (L)
      (COND ((AND (PAIRP C) (EQUAL (CAR C) 'TIMES)) (SETQ C (CDR C)))
            (T (SETQ C (LIST C))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C VL)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (MY_FREEOF (CAR C) X))
           (PROG (V)
             (SETQ V VL)
            LAB
             (COND ((NULL V) (RETURN NIL)))
             ((LAMBDA (V)
                (COND ((NOT (MY_FREEOF (CAR C) V)) (SETQ L (CONS V L)))))
              (CAR V))
             (SETQ V (CDR V))
             (GO LAB))))
         (SETQ VL (SETDIFF VL L))
         (SETQ C (CDR C)))
        (GO WHILELABEL))
      (RETURN (NULL VL)))) 
(PUT 'GINTORDER1 'NUMBER-OF-ARGS 3) 
(PUT 'GINTORDER1 'DEFINED-ON-LINE '1609) 
(PUT 'GINTORDER1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'GINTORDER1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GINTORDER1 (P X MODE2)
    (PROG (L1 L2 SIG)
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'MINUS))
        (PROGN (SETQ SIG T) (SETQ P (CADR P)))))
      (COND ((AND (PAIRP P) (EQUAL (CAR P) 'TIMES)) (SETQ P (CDR P)))
            (T (SETQ P (LIST P))))
      (PROG (A)
        (SETQ A P)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (COND ((MY_FREEOF A X) (SETQ L2 (CONS A L2)))
                  (MODE2 (SETQ L1 (CONS A L1)))
                  ((EQUAL A X) (SETQ L2 (CONS A L2)))
                  ((AND (PAIRP A) (EQUAL (CAR A) 'EXPT) (EQUAL (CADR A) X)
                        (FIXP (CADDR A)))
                   (SETQ L2 (CONS A L2)))
                  (T (SETQ L1 (CONS A L1))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (COND
       ((PAIRP L1)
        (COND ((CDR L1) (SETQ L1 (CONS 'TIMES L1))) (T (SETQ L1 (CAR L1))))))
      (COND
       ((PAIRP L2)
        (COND ((CDR L2) (SETQ L2 (CONS 'TIMES L2))) (T (SETQ L2 (CAR L2))))))
      (COND
       (SIG
        (COND (L2 (SETQ L2 (LIST 'MINUS L2))) (T (SETQ L2 (LIST 'MINUS 1))))))
      (RETURN (LIST (COND (L1 L1) (T 1)) (COND (L2 L2) (T 1)))))) 
(PUT 'PARTINT 'NUMBER-OF-ARGS 5) 
(PUT 'PARTINT 'DEFINED-ON-LINE '1655) 
(PUT 'PARTINT 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'PARTINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARTINT (P FTEM VL X GENINT)
    (PROG (F NEG L1 L2 N K L H)
      (COND
       (TR_GENINT
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "generalized integration of the unintegrated rest : ")
          NIL)
         (EQPRINT P))))
      (SETQ L (GINTORDER P VL X))
      (COND ((AND (PAIRP L) (GREATERP (LENGTH L) GENINT)) (RETURN NIL)))
      (SETQ L
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S L)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (PROGN
                                     (SETQ H (VARSLIST (CAR S) FTEM VL))
                                     (COND
                                      ((EQUAL H NIL)
                                       (PROGN
                                        (LIST 'TIMES X (CAR S)
                                              (CONS 'PLUS (CDR S)))))
                                      (T
                                       (PROGN
                                        (SETQ F (NEWFCT FNAME_ H NFCT_))
                                        (SETQ NFCT_ (ADD1 NFCT_))
                                        (SETQ FNEW_ (CONS F FNEW_))
                                        (SETQ FLIN_ (FCTINSERT F FLIN_))
                                        (SETQ NEG T)
                                        (SETQ N (SUB1 (LENGTH (CDR S))))
                                        (SETQ K (MINUS 1))
                                        (COND
                                         ((AND (PAIRP (CAR S))
                                               (EQUAL (CAAR S) 'DF))
                                          (PROGN
                                           (SETQ H
                                                   (REVAL1
                                                    (REVAL1
                                                     (LIST 'DIFFERENCE
                                                           (CADAR S)
                                                           (LIST 'DF F X
                                                                 (ADD1 N)))
                                                     T)
                                                    T))
                                           (COND
                                            ((NOT (ZEROP H))
                                             (SETQ L1 (CONS H L1))))
                                           (SETQ L2 (CDDAR S))))
                                         (T
                                          (PROGN
                                           (SETQ H
                                                   (SIGNCHANGE
                                                    (REVAL1
                                                     (REVAL1
                                                      (LIST 'DIFFERENCE (CAR S)
                                                            (LIST 'DF F X
                                                                  (ADD1 N)))
                                                      T)
                                                     T)))
                                           (COND
                                            ((NOT (ZEROP H))
                                             (SETQ L1 (CONS H L1))))
                                           (SETQ L2 NIL))))
                                        (REVAL1
                                         (CONS 'PLUS
                                               (PROG (SK FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ SK (CDR S))
                                                 (COND
                                                  ((NULL SK) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  (PROGN
                                                                   (SETQ NEG
                                                                           (NOT
                                                                            NEG))
                                                                   (SETQ K
                                                                           (ADD1
                                                                            K))
                                                                   (REVAL1
                                                                    (LIST
                                                                     'TIMES
                                                                     (COND
                                                                      (NEG
                                                                       (MINUS
                                                                        1))
                                                                      (T 1))
                                                                     (APPEND
                                                                      (LIST 'DF
                                                                            F X
                                                                            (DIFFERENCE
                                                                             N
                                                                             K))
                                                                      L2)
                                                                     (TAILSUM
                                                                      SK K X))
                                                                    T))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ SK (CDR SK))
                                                 (COND
                                                  ((NULL SK)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          (PROGN
                                                           (SETQ NEG (NOT NEG))
                                                           (SETQ K (ADD1 K))
                                                           (REVAL1
                                                            (LIST 'TIMES
                                                                  (COND
                                                                   (NEG
                                                                    (MINUS 1))
                                                                   (T 1))
                                                                  (APPEND
                                                                   (LIST 'DF F
                                                                         X
                                                                         (DIFFERENCE
                                                                          N K))
                                                                   L2)
                                                                  (TAILSUM SK K
                                                                   X))
                                                            T))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                         T))))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (PROGN
                             (SETQ H (VARSLIST (CAR S) FTEM VL))
                             (COND
                              ((EQUAL H NIL)
                               (PROGN
                                (LIST 'TIMES X (CAR S) (CONS 'PLUS (CDR S)))))
                              (T
                               (PROGN
                                (SETQ F (NEWFCT FNAME_ H NFCT_))
                                (SETQ NFCT_ (ADD1 NFCT_))
                                (SETQ FNEW_ (CONS F FNEW_))
                                (SETQ FLIN_ (FCTINSERT F FLIN_))
                                (SETQ NEG T)
                                (SETQ N (SUB1 (LENGTH (CDR S))))
                                (SETQ K (MINUS 1))
                                (COND
                                 ((AND (PAIRP (CAR S)) (EQUAL (CAAR S) 'DF))
                                  (PROGN
                                   (SETQ H
                                           (REVAL1
                                            (REVAL1
                                             (LIST 'DIFFERENCE (CADAR S)
                                                   (LIST 'DF F X (ADD1 N)))
                                             T)
                                            T))
                                   (COND
                                    ((NOT (ZEROP H)) (SETQ L1 (CONS H L1))))
                                   (SETQ L2 (CDDAR S))))
                                 (T
                                  (PROGN
                                   (SETQ H
                                           (SIGNCHANGE
                                            (REVAL1
                                             (REVAL1
                                              (LIST 'DIFFERENCE (CAR S)
                                                    (LIST 'DF F X (ADD1 N)))
                                              T)
                                             T)))
                                   (COND
                                    ((NOT (ZEROP H)) (SETQ L1 (CONS H L1))))
                                   (SETQ L2 NIL))))
                                (REVAL1
                                 (CONS 'PLUS
                                       (PROG (SK FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ SK (CDR S))
                                         (COND ((NULL SK) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          (PROGN
                                                           (SETQ NEG (NOT NEG))
                                                           (SETQ K (ADD1 K))
                                                           (REVAL1
                                                            (LIST 'TIMES
                                                                  (COND
                                                                   (NEG
                                                                    (MINUS 1))
                                                                   (T 1))
                                                                  (APPEND
                                                                   (LIST 'DF F
                                                                         X
                                                                         (DIFFERENCE
                                                                          N K))
                                                                   L2)
                                                                  (TAILSUM SK K
                                                                   X))
                                                            T))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ SK (CDR SK))
                                         (COND
                                          ((NULL SK) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  (PROGN
                                                   (SETQ NEG (NOT NEG))
                                                   (SETQ K (ADD1 K))
                                                   (REVAL1
                                                    (LIST 'TIMES
                                                          (COND (NEG (MINUS 1))
                                                                (T 1))
                                                          (APPEND
                                                           (LIST 'DF F X
                                                                 (DIFFERENCE N
                                                                             K))
                                                           L2)
                                                          (TAILSUM SK K X))
                                                    T))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                 T))))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (L (SETQ L (CONS (REVAL1 (CONS 'PLUS L) T) L1))))
      (COND
       (TR_GENINT
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "result (without constant or function of integration): ")
          NIL)
         (COND
          (L
           (PROGN
            (EQPRINT (CAR L))
            (PROGN (PRIN2 "additional equations : ") NIL)
            (DEPRINT (CDR L))))
          (T (PROGN (PRIN2 " nil ") NIL)))
         NIL)))
      (RETURN L))) 
(PUT 'TAILSUM 'NUMBER-OF-ARGS 3) 
(PUT 'TAILSUM 'DEFINED-ON-LINE '1711) 
(PUT 'TAILSUM 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'TAILSUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAILSUM (SK K X)
    (PROG (J)
      (SETQ J (MINUS 1))
      (RETURN
       (REVAL1
        (CONS 'PLUS
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A SK)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (PROGN
                                     (SETQ J (PLUS J 1))
                                     (LIST 'TIMES A
                                           (PROD (PLUS J 1) (PLUS J K))
                                           (LIST 'EXPT X J))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (PROGN
                             (SETQ J (PLUS J 1))
                             (LIST 'TIMES A (PROD (PLUS J 1) (PLUS J K))
                                   (LIST 'EXPT X J))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
        T)))) 
(PUT 'PROD 'NUMBER-OF-ARGS 2) 
(PUT 'PROD 'DEFINED-ON-LINE '1720) 
(PUT 'PROD 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'PROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROD (M N)
    (COND ((GREATERP M N) 1)
          (T
           (PROG (I FORALL-RESULT)
             (SETQ I M)
             (SETQ FORALL-RESULT 1)
            LAB1
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT (TIMES I FORALL-RESULT))
             (SETQ I (PLUS2 I 1))
             (GO LAB1))))) 
(ENDMODULE) 
(MODULE (LIST 'INTFACTOR)) 
(PUT 'FCTRS 'NUMBER-OF-ARGS 3) 
(PUT 'FCTRS 'DEFINED-ON-LINE '1762) 
(PUT 'FCTRS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'FCTRS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FCTRS (P INDEP V)
    (PROG (FL1 FL2)
      (SETQ P (CDR (REVAL1 (ERR_CATCH_FAC P) T)))
      (PROG (EL)
        (SETQ EL P)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((AND (FREEOFLIST EL INDEP)
                  (OR (EQUAL V NIL) (NOT (MY_FREEOF EL V))))
             (SETQ FL1 (CONS EL FL1)))
            (T (SETQ FL2 (CONS EL FL2)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (COND ((NULL FL1) (SETQ FL1 1)))
      (COND ((NULL FL2) (SETQ FL2 1)))
      (COND
       ((PAIRP FL1)
        (COND ((EQUAL (LENGTH FL1) 1) (SETQ FL1 (CAR FL1)))
              (T (SETQ FL1 (CONS 'TIMES FL1))))))
      (COND
       ((PAIRP FL2)
        (COND ((EQUAL (LENGTH FL2) 1) (SETQ FL2 (CAR FL2)))
              (T (SETQ FL2 (CONS 'TIMES FL2))))))
      (RETURN (LIST FL1 FL2)))) 
(PUT 'EXTRACTFAC 'NUMBER-OF-ARGS 3) 
(PUT 'EXTRACTFAC 'DEFINED-ON-LINE '1779) 
(PUT 'EXTRACTFAC 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'EXTRACTFAC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTRACTFAC (P INDEP V)
    (PROG (NU DE)
      (RETURN
       (COND
        ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
         (PROGN
          (SETQ NU (FCTRS (CADR P) INDEP V))
          (SETQ DE (FCTRS (CADDR P) INDEP V))
          (LIST
           (REVAL1
            (COND ((NEQ (CAR DE) 1) (LIST 'QUOTIENT (CAR NU) (CAR DE)))
                  (T (CAR NU)))
            T)
           (COND ((NEQ (CADR DE) 1) (LIST 'QUOTIENT (CADR NU) (CADR DE)))
                 (T (CADR NU))))))
        (T (FCTRS P INDEP V)))))) 
(PUT 'GET_KERNELS 'NUMBER-OF-ARGS 1) 
(PUT 'GET_KERNELS 'DEFINED-ON-LINE '1798) 
(PUT 'GET_KERNELS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'GET_KERNELS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_KERNELS (EX)
    (PROG (RES PRI)
      (SETQ EX (REVAL1 EX T))
      (COND (PRI (PROGN (TERPRI) (PROGN (PRIN2 "ex=") (PRIN2 EX) NIL))))
      (COND
       ((PAIRP EX)
        (COND
         ((OR (EQUAL (CAR EX) 'QUOTIENT) (EQUAL (CAR EX) 'PLUS)
              (EQUAL (CAR EX) 'TIMES))
          (PROG (S)
            (SETQ S (CDR EX))
           LAB
            (COND ((NULL S) (RETURN NIL)))
            ((LAMBDA (S) (SETQ RES (UNION (GET_KERNELS S) RES))) (CAR S))
            (SETQ S (CDR S))
            (GO LAB)))
         ((OR (EQUAL (CAR EX) 'MINUS)
              (AND (EQUAL (CAR EX) 'EXPT) (NEQ (CADR EX) 'E) (NEQ (CADR EX) 'E)
                   (NOT (FIXP (CADR EX)))))
          (SETQ RES (GET_KERNELS (CADR EX))))
         (T (SETQ RES (LIST EX)))))
       ((IDP EX) (SETQ RES (LIST EX))))
      (COND (PRI (PROGN (TERPRI) (PROGN (PRIN2 "res=") (PRIN2 RES) NIL))))
      (RETURN RES))) 
(PUT 'SPECIALSOL 'NUMBER-OF-ARGS 6) 
(PUT 'SPECIALSOL 'DEFINED-ON-LINE '1821) 
(PUT 'SPECIALSOL 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SPECIALSOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPECIALSOL (P VL FL X INDEP GK)
    (PROG (E1 E2 N NL H HH AI SUBLIST EQS STARTVAL PRI PRINTOLD PCOPY)
      (SETQ P (NUM P))
      (SETQ PCOPY P)
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "The equation for the integrating factor:") NIL)
         (TERPRI)
         (EQPRINT P)
         NIL)))
      (COND ((NULL GK) (SETQ GK (GET_KERNELS P))))
      (PROG (E1)
        (SETQ E1 FL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ H NIL)
            (COND
             (PRI
              (PROG (E2)
                (SETQ E2 GK)
               LAB
                (COND ((NULL E2) (RETURN NIL)))
                ((LAMBDA (E2)
                   (PROGN
                    (TERPRI)
                    (PROGN (PRIN2 "e2=") (PRIN2 E2) NIL)
                    (COND ((MY_FREEOF E2 X) (PROGN (PRIN2 " freeof1") NIL)))
                    (COND
                     ((NOT (FREEOFLIST E2 FL))
                      (PROGN (PRIN2 " not freeoflist") NIL)))
                    (COND
                     ((NOT (FREEOFLIST E2 INDEP))
                      (PROGN (PRIN2 " dependent on indep") NIL)))))
                 (CAR E2))
                (SETQ E2 (CDR E2))
                (GO LAB))))
            (PROG (E2)
              (SETQ E2 GK)
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (COND
                  ((AND (NOT (MY_FREEOF E2 X)) (FREEOFLIST E2 FL)
                        (FREEOFLIST E2 INDEP))
                   (PROGN
                    (SETQ N (GENSYM))
                    (SETQ NL (CONS N NL))
                    (SETQ H (CONS (LIST 'EXPT E2 N) H))
                    NIL))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (COND
             (H
              (PROGN
               (COND ((GREATERP (LENGTH H) 1) (SETQ H (CONS 'TIMES H)))
                     (T (SETQ H (CAR H))))
               (SETQ SUBLIST (CONS (CONS E1 H) SUBLIST))
               (COND
                (PRI
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "Ansatz: ")
                   (PRIN2 E1)
                   (PRIN2 " = ")
                   (PRIN2 H)
                   NIL))))
               (SETQ P (REVAL1 (NUM (REVAL1 (SUBST H E1 P) T)) T))
               (COND
                (PRI
                 (PROGN (TERPRI) (PROGN (PRIN2 "p=") NIL) (EQPRINT P)))))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND ((NULL H) (RETURN NIL)))
      (COND
       (NIL
        (PROGN
         (ON (LIST 'ROUNDED))
         (PRECISION 20)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "Before substituting numerical values:") NIL)
         (EQPRINT P)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "Constants to be calculated: ") NIL)
         (PROG (N)
           (SETQ N NL)
          LAB
           (COND ((NULL N) (RETURN NIL)))
           ((LAMBDA (N) (PROGN (PRIN2 N) (PRIN2 "  ") NIL)) (CAR N))
           (SETQ N (CDR N))
           (GO LAB))
         (PROG (E1)
           (SETQ E1 NL)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (SETQ H P)
               (PROG (E2)
                 (SETQ E2 GK)
                LAB
                 (COND ((NULL E2) (RETURN NIL)))
                 ((LAMBDA (E2)
                    (COND
                     ((FREEOFLIST E2 FL)
                      (COND
                       ((AND (PAIRP E2)
                             (OR (EQUAL (CAR E2) 'DF) (EQUAL (CAR E2) 'INT)))
                        (PROGN
                         (SETQ N
                                 (LIST 'QUOTIENT (PLUS 1 (RANDOM 30028))
                                       30029))
                         (TERPRI)
                         (PROGN
                          (PRIN2 "substitution done: ")
                          (PRIN2 E2)
                          (PRIN2 " = ")
                          (PRIN2 N)
                          NIL)
                         (SETQ H
                                 (SUBST
                                  (LIST 'QUOTIENT (PLUS 1 (RANDOM 30028))
                                        30029)
                                  E2 H))))))))
                  (CAR E2))
                 (SETQ E2 (CDR E2))
                 (GO LAB))
               (PROG (E2)
                 (SETQ E2 GK)
                LAB
                 (COND ((NULL E2) (RETURN NIL)))
                 ((LAMBDA (E2)
                    (COND
                     ((FREEOFLIST E2 FL)
                      (COND
                       ((NOT
                         (AND (PAIRP E2)
                              (OR (EQUAL (CAR E2) 'DF) (EQUAL (CAR E2) 'INT))))
                        (PROGN
                         (SETQ N
                                 (LIST 'QUOTIENT (PLUS 1 (RANDOM 30028))
                                       30029))
                         (TERPRI)
                         (PROGN
                          (PRIN2 "substitution done: ")
                          (PRIN2 E2)
                          (PRIN2 " = ")
                          (PRIN2 N)
                          NIL)
                         (SETQ H
                                 (SUBST
                                  (LIST 'QUOTIENT (PLUS 1 (RANDOM 30028))
                                        30029)
                                  E2 H))))))))
                  (CAR E2))
                 (SETQ E2 (CDR E2))
                 (GO LAB))
               (TERPRI)
               (TERPRI)
               (PROGN (PRIN2 "The equation after all substitutions: ") NIL)
               (TERPRI)
               (EQPRINT H)
               (SETQ EQS (CONS (REVAL1 H T) EQS))
               (SETQ STARTVAL (CONS (LIST 'EQUAL E1 1) STARTVAL))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (COND ((EQUAL (LENGTH EQS) 1) (SETQ EQS (CDR EQS)))
               (T (SETQ EQS (CONS 'LIST EQS))))
         (COND ((EQUAL (LENGTH STARTVAL) 1) (SETQ STARTVAL (CDR STARTVAL)))
               (T (SETQ STARTVAL (CONS 'LIST STARTVAL))))
         (TERPRI)
         (PROGN (PRIN2 "start rdsolveeval!") NIL)
         (TERPRI)
         (TERPRI)
         (SETQ H (RDSOLVEEVAL (LIST EQS STARTVAL)))
         (SETQ EQS NIL)
         (OFF (LIST 'ROUNDED))
         NIL)))
      (COND ((NULL PRI) (PROGN (SETQ PRINTOLD PRINT_) (SETQ PRINT_ NIL))))
      (COND
       ((AND P (NOT (ZEROP P)))
        (COND
         ((NOT
           (AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT)
                (INTERSECTION (ARGSET (SMEMBERL FL (CADR P))) VL)))
          (SETQ P (SEPAR2 P FL (LIST X))))
         (T (SETQ P NIL)))))
      (COND ((NULL PRI) (SETQ PRINT_ PRINTOLD)))
      (COND
       (P
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT P) (RETURN NIL)))
           (COND ((FREEOFLIST (CDAR P) NL) (PROGN (SETQ EQS NIL) (SETQ P NIL)))
                 (T (PROGN (SETQ EQS (CONS (CDAR P) EQS)) (SETQ P (CDR P)))))
           (GO WHILELABEL))
         NIL)))
      (COND (PRI (PROGN (TERPRI) (PROGN (PRIN2 "eqs1=") (PRIN2 EQS) NIL))))
      (COND ((OR (NULL EQS) (GREATERP (LENGTH EQS) MAXALGSYS_)) (RETURN NIL))
            (T
             (PROGN
              (COND
               (PRI
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "The algebraic system to solve for ")
                  (PRIN2 NL)
                  (PRIN2 " is:")
                  NIL)
                 (COND ((GREATERP (LENGTH EQS) 1) (DEPRINT EQS))
                       (T (EQPRINT (CAR EQS)))))))
              (COND ((GREATERP (LENGTH EQS) 1) (SETQ EQS (CONS 'LIST EQS)))
                    (T (SETQ EQS (CAR EQS))))
              (COND
               (PRI
                (PROGN
                 (TERPRI)
                 (PROGN (PRIN2 "eqs2=") (PRIN2 EQS) NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "nl=") (PRIN2 NL) NIL))))
              (SETQ HH (CONS 'LIST NL))
              (SETQ EQS (PROGN (SETQ AI !ARBINT) (ERR_CATCH_SOLVE EQS HH)))
              (COND
               (PRI
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "eqs3a=")
                  (PRIN2 EQS)
                  (PRIN2 "  ai=")
                  (PRIN2 AI)
                  (PRIN2 "  !!arbint=")
                  (PRIN2 !ARBINT)
                  NIL)
                 (TERPRI))))
              (COND
               ((NOT (FREEOF EQS 'ARBCOMPLEX))
                (PROGN
                 (SETQ EQS (REVAL1 (CAR EQS) T))
                 (PROG (H)
                   (SETQ H (PLUS AI 1))
                  LAB
                   (COND ((MINUSP (DIFFERENCE !ARBINT H)) (RETURN NIL)))
                   (SETQ EQS (SUBST 0 (LIST 'ARBCOMPLEX H) EQS))
                   (SETQ H (PLUS2 H 1))
                   (GO LAB))
                 (COND
                  (PRI
                   (PROGN
                    (TERPRI)
                    (PROGN (PRIN2 "eqs3b=") (PRIN2 EQS) NIL)
                    (TERPRI))))
                 (SETQ EQS (ERR_CATCH_SOLVE EQS HH)))))
              (COND
               (PRI
                (PROGN
                 (TERPRI)
                 (PROGN (PRIN2 "eqs3c=") (PRIN2 EQS) NIL)
                 (TERPRI))))
              (COND ((NULL EQS) (RETURN NIL)))
              (COND ((EQUAL (LENGTH NL) 1) (SETQ EQS (LIST EQS))))
              (COND
               (PRI
                (PROGN
                 (PROGN
                  (PRIN2 "nl=")
                  (PRIN2 NL)
                  (PRIN2 "  eqs4=")
                  (PRIN2 EQS)
                  NIL)
                 (TERPRI))))
              (PROG (E1)
                (SETQ E1 EQS)
               LAB
                (COND ((NULL E1) (RETURN NIL)))
                ((LAMBDA (E1)
                   (PROGN
                    (COND
                     (PRI
                      (PROGN
                       (TERPRI)
                       (PROGN (PRIN2 "e2=") (PRIN2 E1) NIL)
                       (TERPRI))))
                    (COND ((EQUAL (CAR E1) 'LIST) (SETQ E1 (CDR E1))))
                    (COND
                     (PRI
                      (PROGN
                       (TERPRI)
                       (PROGN (PRIN2 "e3=") (PRIN2 E1) NIL)
                       (TERPRI))))
                    (PROG (E2)
                      (SETQ E2 E1)
                     LAB
                      (COND ((NULL E2) (RETURN NIL)))
                      ((LAMBDA (E2)
                         (PROGN
                          (COND
                           (PRI
                            (PROGN
                             (ASSGNPRI (AEVAL "solution:") NIL 'FIRST)
                             (ASSGNPRI E2 NIL 'LAST))))
                          (SETQ SUBLIST (SUBST (CADDR E2) (CADR E2) SUBLIST))))
                       (CAR E2))
                      (SETQ E2 (CDR E2))
                      (GO LAB))
                    (COND
                     (PRI
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "The sublist is:")
                        (PRIN2 SUBLIST)
                        NIL))))))
                 (CAR E1))
                (SETQ E1 (CDR E1))
                (GO LAB))
              NIL)))
      (COND
       (PRI
        (PROGN (TERPRI) (PROGN (PRIN2 "pcopy1=") (PRIN2 PCOPY) NIL) (TERPRI))))
      (PROG (E1)
        (SETQ E1 SUBLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ PCOPY (SUBST (CDR E1) (CAR E1) PCOPY))
            (COND
             (PRI
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "e1=") (PRIN2 E1) NIL)
               (TERPRI)
               (PROGN (PRIN2 "pcopy2=") (PRIN2 PCOPY) NIL)
               (TERPRI))))
            NIL))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "pcopy3=") (PRIN2 (REVAL1 PCOPY T)) NIL)
         (TERPRI))))
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "pcopy4=") (PRIN2 (REVAL1 (REVAL1 PCOPY T) T)) NIL)
         (TERPRI))))
      (COND ((NOT (ZEROP (REVAL1 (REVAL1 PCOPY T) T))) (RETURN NIL))
            (T
             (RETURN
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 SUBLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (CONS (CAR E1) (REVAL1 (CDR E1) T)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1) (CONS (CAR E1) (REVAL1 (CDR E1) T)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'ADD_FACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_FACTORS 'DEFINED-ON-LINE '2001) 
(PUT 'ADD_FACTORS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ADD_FACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_FACTORS (GK P)
    (COND ((NULL P) GK)
          ((OR (NOT (PAIRP P)) (AND (PAIRP P) (NEQ (CAR P) 'TIMES)))
           (UNION
            (LIST
             (COND ((AND (PAIRP P) (EQUAL (CAR P) 'MINUS)) (CADR P)) (T P)))
            GK))
          (T
           (PROGN
            (PROG (H)
              (SETQ H (CDR P))
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (SETQ GK
                         (UNION
                          (LIST
                           (COND
                            ((AND (PAIRP H) (EQUAL (CAR H) 'MINUS)) (CADR H))
                            (T H)))
                          GK)))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            GK)))) 
(FLAG '(FINDINTFAC) 'OPFN) 
(PUT 'FINDINTFAC 'NUMBER-OF-ARGS 8) 
(PUT 'FINDINTFAC 'DEFINED-ON-LINE '2018) 
(PUT 'FINDINTFAC 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'FINDINTFAC 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE FINDINTFAC (PL FTEM VL X DONEINTVAR INTFACDEP FACTR VERBSE)
    (PROG (H NEWEQU TOZERO FL E1 PRI EXFACTORS FTR GK PRECISE_OLD CARFTR ZD)
      (SETQ PRECISE_OLD *PRECISE)
      (SETQ *PRECISE NIL)
      (SETQ FACTR T)
      (COND (PRI (PROGN (TERPRI) (PROGN (PRIN2 "START VON FINDINTFAC") NIL))))
      (PROG (E1)
        (SETQ E1 PL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ GK (UNION GK (GET_KERNELS E1)))
            (COND
             (FACTR
              (PROGN
               (SETQ FTR (EXTRACTFAC E1 (APPEND DONEINTVAR FTEM) X))
               (SETQ CARFTR (CAR FTR))
               (COND
                ((NOT (EVALNUMBERP CARFTR))
                 (COND
                  ((AND (PAIRP CARFTR) (EQUAL (CAR CARFTR) 'QUOTIENT))
                   (SETQ GK
                           (ADD_FACTORS (ADD_FACTORS GK (CADR CARFTR))
                            (CADDR CARFTR))))
                  (T (SETQ GK (ADD_FACTORS GK CARFTR))))))
               NIL))
             (T (SETQ FTR (LIST 1 NIL))))
            (SETQ EXFACTORS (CONS (CAR FTR) EXFACTORS))
            (COND
             ((NEQ (CAR FTR) 1)
              (PROGN
               (SETQ E1 (CADR FTR))
               (COND
                (PRI
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "extracted factor:")
                   (PRIN2 (EQPRINT (CAR FTR)))
                   NIL))))
               NIL)))
            (SETQ H (GENSYM))
            (SETQ DEPL* (CONS (LIST H X) DEPL*))
            (DEPEND (LIST H X))
            (SETQ FL (CONS H FL))
            (SETQ E1 (INTPDE (REVAL1 (LIST 'TIMES H E1) T) FTEM VL X T))
            (COND
             ((AND E1 (CAR E1))
              (PROGN
               (SETQ NEWEQU (CONS (CAR E1) NEWEQU))
               (SETQ TOZERO (CONS (CADR E1) TOZERO))
               (COND
                (PRI
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 " the main part of integration:") NIL)
                  (EQPRINT (CAR E1))
                  (TERPRI)
                  (PROGN (PRIN2 "car e1=") (PRIN2 (CAR E1)) NIL)
                  (TERPRI)
                  (PROGN (PRIN2 " the remainder of integration:") NIL)
                  (EQPRINT (CADR E1))
                  (TERPRI)
                  (PROGN (PRIN2 "cadr e1=") (PRIN2 (CADR E1)) NIL)
                  NIL))))))
            NIL))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND ((NULL TOZERO) (RETURN NIL)))
      (SETQ NEWEQU
              (COND ((GREATERP (LENGTH PL) 1) (CONS 'PLUS NEWEQU))
                    (T (CAR NEWEQU))))
      (SETQ TOZERO
              (REVAL1
               (COND ((GREATERP (LENGTH PL) 1) (CONS 'PLUS TOZERO))
                     (T (CAR TOZERO)))
               T))
      (COND
       ((AND (PAIRP TOZERO) (EQUAL (CAR TOZERO) 'QUOTIENT))
        (SETQ TOZERO (CADR TOZERO))))
      (COND
       (FACTR
        (PROGN
         (SETQ H (CDR (ERR_CATCH_FAC TOZERO)))
         (COND
          (H
           (PROGN
            (COND
             (PRI
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "The factors of tozero:") (PRIN2 H) NIL))))
            (SETQ TOZERO NIL)
            (PROG (E1)
              (SETQ E1 H)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (COND ((SMEMBERL FL E1) (SETQ TOZERO (CONS E1 TOZERO)))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (SETQ TOZERO
                    (REVAL1
                     (COND ((GREATERP (LENGTH TOZERO) 1) (CONS 'TIMES TOZERO))
                           (T (CAR TOZERO)))
                     T))))))))
      (COND
       ((AND NIL PRI) (PROGN (PROGN (PRIN2 "tozero =") NIL) (EQPRINT TOZERO))))
      (SETQ H NIL)
      (SETQ H (SPECIALSOL TOZERO VL FL X (APPEND FTEM DONEINTVAR) GK))
      (COND (PRI (PROGN (PROGN (PRIN2 "h=") (PRIN2 H) NIL) (TERPRI))))
      (COND
       (H
        (PROGN
         (PROG (E1)
           (SETQ E1 H)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (COND
                (PRI
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "e1=") (PRIN2 E1) NIL)
                  (TERPRI)
                  (PROGN (PRIN2 "newequ=") (PRIN2 NEWEQU) NIL)
                  (TERPRI))))
               (SETQ NEWEQU (REVAL1 (SUBST (CDR E1) (CAR E1) NEWEQU) T))
               (COND
                (PRI
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "newequ=") (PRIN2 NEWEQU) NIL))))
               NIL))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))))
       (PRI (PROGN (PRIN2 "no integrating factor") NIL)))
      (PROG (E1)
        (SETQ E1 FL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ DEPL* (DELETE (ASSOC E1 DEPL*) DEPL*))
            (SETQ DEPL* (DELETE (ASSOC (MKID E1 '_) DEPL*) DEPL*))
            NIL))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROG (E1)
        (SETQ E1 VL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((AND (NEQ E1 X) (MY_FREEOF INTFACDEP E1)
                  (OR (NOT (MY_FREEOF H E1)) (NOT (MY_FREEOF EXFACTORS E1))))
             (SETQ INTFACDEP (CONS E1 INTFACDEP)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       ((AND H PRINT_ VERBSE)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "The integrated equation: ") NIL)
         (EQPRINT NEWEQU)
         (TERPRI)
         (COND
          ((EQUAL (LENGTH PL) 1)
           (PROGN (PRIN2 "An integrating factor has been found:") NIL))
          (T (PROGN (PRIN2 "Integrating factors have been found: ") NIL)))
         NIL)))
      (SETQ *PRECISE PRECISE_OLD)
      (COND ((OR (NULL H) (ZEROP NEWEQU)) (RETURN NIL)))
      (SETQ ZD (ZERO_DEN H FTEM))
      (COND
       (ZD
        (RETURN
         (PROGN
          (PROG (E1)
            (SETQ E1 ZD)
           LAB
            (COND ((NULL E1) (RETURN NIL)))
            ((LAMBDA (E1)
               (SETQ TO_DO_LIST
                       (UNION (LIST (LIST 'SPLIT_INTO_CASES E1)) TO_DO_LIST)))
             (CAR E1))
            (SETQ E1 (CDR E1))
            (GO LAB))
          NIL))))
      (RETURN
       (LIST NEWEQU
             (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
               (SETQ E1 H)
               (COND ((NULL E1) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (E1)
                                   (PROGN
                                    (SETQ FTR (CAR EXFACTORS))
                                    (SETQ EXFACTORS (CDR EXFACTORS))
                                    (SETQ GK
                                            (COND ((EQUAL FTR 1) (CDR E1))
                                                  (T
                                                   (REVAL1
                                                    (LIST 'QUOTIENT (CDR E1)
                                                          FTR)
                                                    T))))
                                    (COND ((AND PRINT_ VERBSE) (MATHPRINT GK)))
                                    GK))
                                 (CAR E1))
                                NIL)))
              LOOPLABEL
               (SETQ E1 (CDR E1))
               (COND ((NULL E1) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (E1)
                           (PROGN
                            (SETQ FTR (CAR EXFACTORS))
                            (SETQ EXFACTORS (CDR EXFACTORS))
                            (SETQ GK
                                    (COND ((EQUAL FTR 1) (CDR E1))
                                          (T
                                           (REVAL1
                                            (LIST 'QUOTIENT (CDR E1) FTR) T))))
                            (COND ((AND PRINT_ VERBSE) (MATHPRINT GK)))
                            GK))
                         (CAR E1))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             INTFACDEP)))) 
(ENDMODULE) 
(MODULE (LIST 'ODEINTEGRATION)) 
(PUT 'INTEGRATEODE 'NUMBER-OF-ARGS 5) 
(PUT 'INTEGRATEODE 'DEFINED-ON-LINE '2181) 
(PUT 'INTEGRATEODE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRATEODE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATEODE (DE FOLD XNEW ORDR FTEM)
    (PROG (NEWDE NEWNEWDE L H NEWCOND)
      (SETQ H
              (COND
               ((NOT XNEW)
                (PROGN
                 (COND
                  ((SETQ L (INTEGRATEPDE DE FTEM NIL GENINT_ NIL))
                   (PROGN (SETQ NEWCOND (APPEND NEWCOND (CDR L))) (CAR L)))
                  (T NIL))))
               ((NOT (PAIRP FOLD))
                (COND
                 ((SETQ L (ODECONVERT DE FOLD XNEW ORDR FTEM))
                  (PROGN (SETQ NEWCOND (APPEND NEWCOND (CDR L))) (CAR L)))
                 (T NIL)))
               (T
                (PROGN
                 (SETQ NEWDE
                         (COND
                          ((SETQ L (ODECONVERT DE FOLD XNEW ORDR FTEM))
                           (PROGN
                            (SETQ NEWCOND (APPEND NEWCOND (CDR L)))
                            (CAR L)))
                          (T NIL)))
                 (COND ((NOT NEWDE) NIL)
                       (T
                        (PROGN
                         (SETQ FTEM (UNION FNEW_ FTEM))
                         (SETQ NEWNEWDE (INTEGRATEDE NEWDE FTEM NIL))
                         (COND
                          (NEWNEWDE
                           (PROGN
                            (SETQ NEWCOND (APPEND NEWCOND (CDR NEWNEWDE)))
                            (CAR NEWNEWDE)))
                          (T NEWDE)))))))))
      (RETURN (COND ((NOT H) NIL) (T (CONS H NEWCOND)))))) 
(PUT 'ODECHECK 'NUMBER-OF-ARGS 3) 
(PUT 'ODECHECK 'DEFINED-ON-LINE '2233) 
(PUT 'ODECHECK 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ODECHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODECHECK (EX FINT FTEM)
    (PROG (A B OP EX1)
      (COND
       ((EQUAL EX FINT)
        (PROGN
         (SETQ A (LIST EX))
         (SETQ EX (FCTARGS EX))
         (PROG ()
          WHILELABEL
           (COND ((NOT EX) (RETURN NIL)))
           (PROGN (SETQ A (APPEND (LIST 0 0) A)) (SETQ EX (CDR EX)))
           (GO WHILELABEL))
         (SETQ OP (REVERSE A))))
       ((PAIRP EX)
        (COND
         ((EQUAL (CAR EX) 'DF)
          (PROGN
           (SETQ A (ODECHECK (CADR EX) FINT FTEM))
           (COND ((NOT (PAIRP A)) (SETQ OP A))
                 (T
                  (PROGN
                   (SETQ OP (LIST (CAR A)))
                   (SETQ A (FCTARGS (CAR A)))
                   (SETQ EX (CDDR EX))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT A) (RETURN NIL)))
                     (PROGN
                      (SETQ EX1 EX)
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (AND EX1 (NEQ (CAR A) (CAR EX1))))
                          (RETURN NIL)))
                        (SETQ EX1 (CDR EX1))
                        (GO WHILELABEL))
                      (COND ((NULL EX1) (SETQ OP (CONS 0 (CONS 0 OP))))
                            (T
                             (PROGN
                              (COND ((NOT (CDR EX1)) (SETQ B 1))
                                    (T
                                     (PROGN
                                      (SETQ B (CADR EX1))
                                      (COND ((NOT (NUMBERP B)) (SETQ B 1))))))
                              (SETQ OP (CONS B (CONS B OP))))))
                      (SETQ A (CDR A)))
                     (GO WHILELABEL))
                   (SETQ OP (REVERSE OP)))))))
         (T
          (PROGN
           (SETQ A (CAR EX))
           (SETQ EX (CDR EX))
           (COND ((EQUAL A 'INT) (SETQ EX (LIST (REVAL1 (CAR EX) T)))))
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND (NEQ OP '_ABB) EX)) (RETURN NIL)))
             (PROGN
              (SETQ B (ODECHECK (CAR EX) FINT FTEM))
              (COND
               (B
                (COND ((EQ B '_ABB) (SETQ OP '_ABB))
                      (T (SETQ OP (ODETEST OP B))))))
              (SETQ EX (CDR EX)))
             (GO WHILELABEL)))))))
      (RETURN OP))) 
(PUT 'INTEGRABLEODE 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGRABLEODE 'DEFINED-ON-LINE '2281) 
(PUT 'INTEGRABLEODE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRABLEODE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGRABLEODE (P FTEM)
    (COND
     ((GREATERP (DELENGTH P) (COND (ODESOLVE_ ODESOLVE_) (T 0)))
      (COND
       (CONT_
        (COND
         ((YESP "expression to be integrated ? ") (INTEGRABLEODE1 P FTEM))))))
     (T (INTEGRABLEODE1 P FTEM)))) 
(PUT 'INTEGRABLEODE1 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGRABLEODE1 'DEFINED-ON-LINE '2290) 
(PUT 'INTEGRABLEODE1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTEGRABLEODE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGRABLEODE1 (P FTEM)
    (PROG (A B U VL LE UVAR FINT FIVAR FOLD XNEW ORDR1 ORDR2 INTLIST)
      (SETQ FTEM (SMEMBERL FTEM P))
      (SETQ VL (ARGSET FTEM))
      (SETQ A FTEM)
      (SETQ B NIL)
      (SETQ LE (LENGTH VL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A VL)) (RETURN NIL)))
        (PROGN
         (SETQ U (CAR A))
         (SETQ UVAR (FCTARGS U))
         (COND
          ((EQUAL (LENGTH UVAR) LE)
           (COND (B (PROGN (SETQ VL NIL) (SETQ A (LIST NIL))))
                 (T (PROGN (SETQ B T) (SETQ FINT U) (SETQ FIVAR UVAR)))))
          (T (SETQ VL (SETDIFF VL UVAR))))
         (SETQ A (CDR A)))
        (GO WHILELABEL))
      (COND ((NOT B) (SETQ VL NIL)))
      (SETQ LE (LENGTH P))
      (COND
       ((AND (LESSP 1 LE) VL)
        (PROGN
         (SETQ A (ODECHECK P FINT FTEM))
         (COND
          ((NOT (ATOM A))
           (PROGN
            (SETQ ORDR1 0)
            (SETQ ORDR2 0)
            (SETQ XNEW NIL)
            (SETQ A (CDR A))
            (SETQ B FIVAR)
            (PROG ()
             WHILELABEL
              (COND ((NOT B) (RETURN NIL)))
              (PROGN
               (COND
                ((NEQ (CAR A) 0)
                 (PROGN
                  (SETQ FOLD (CONS (CAR B) FOLD))
                  (COND
                   ((GREATERP (CAR A) 1) (SETQ FOLD (CONS (CAR A) FOLD)))))))
               (SETQ ORDR2 (PLUS ORDR2 (CAR A)))
               (COND
                ((NEQ (CAR A) (CADR A))
                 (PROGN
                  (SETQ XNEW (CAR B))
                  (COND
                   ((NOT (MEMBER XNEW VL))
                    (PROGN (SETQ B (LIST NIL)) (SETQ VL NIL))))
                  (SETQ ORDR1 (DIFFERENCE (CADR A) (CAR A))))))
               (SETQ B (CDR B))
               (SETQ A (CDDR A)))
              (GO WHILELABEL))
            (SETQ FOLD (REVERSE FOLD))
            (COND (FOLD (SETQ FOLD (CONS 'DF (CONS FINT FOLD))))
                  (T (SETQ FOLD FINT)))
            (COND
             ((AND VL (OR (NEQ ORDR1 0) (NEQ ORDR2 0)))
              (SETQ INTLIST (LIST FOLD XNEW ORDR1 ORDR2))))))))))
      (RETURN INTLIST))) 
(PUT 'ODETEST 'NUMBER-OF-ARGS 2) 
(PUT 'ODETEST 'DEFINED-ON-LINE '2352) 
(PUT 'ODETEST 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ODETEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODETEST (OP B)
    (COND ((NOT OP) B) ((NEQ (CAR OP) (CAR B)) '_ABB)
          (T
           (PROG (DIF A)
             (SETQ DIF NIL)
             (SETQ A (LIST (CAR OP)))
             (SETQ OP (CDR OP))
             (SETQ B (CDR B))
             (PROG ()
              WHILELABEL
               (COND ((NOT OP) (RETURN NIL)))
               (PROGN
                (SETQ A
                        (CONS (MAX (CADR OP) (CADR B))
                              (CONS (MIN (CAR OP) (CAR B)) A)))
                (COND
                 ((NEQ (CAR A) (CADR A))
                  (COND (DIF (PROGN (SETQ A '_ABB) (SETQ OP (LIST 1 1))))
                        (T (SETQ DIF T)))))
                (SETQ OP (CDDR OP))
                (SETQ B (CDDR B)))
               (GO WHILELABEL))
             (COND ((NOT (ATOM A)) (SETQ A (REVERSE A))))
             (RETURN A))))) 
(PUT 'ODECONVERT 'NUMBER-OF-ARGS 5) 
(PUT 'ODECONVERT 'DEFINED-ON-LINE '2374) 
(PUT 'ODECONVERT 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'ODECONVERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODECONVERT (DE FORD XNEW ORDR FTEM)
    (PROG (J H H1 H2 T2 FORD_ NEWCO OLDDE NEWDE NEWVL NULL_ RULI LD)
      (SETQ FORD_ (GENSYM))
      (SETQ DEPL* (DELETE (ASSOC FORD_ DEPL*) DEPL*))
      (DEPEND1 FORD_ XNEW T)
      (SETQ OLDDE (REVAL1 (SUBST FORD_ (REVAL1 FORD T) DE) T))
      (COND
       ((PAIRP FORD)
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE ORDR J)) (RETURN NIL)))
           (SETQ OLDDE
                   (REVAL1
                    (SUBST (REVAL1 (LIST 'DF FORD_ XNEW J) T)
                           (REVAL1 (LIST 'DF FORD XNEW J) T) OLDDE)
                    T))
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (SETK '!ARBCONST 0)
      (SETQ NEWDE (ERR_CATCH_ODESOLVE OLDDE FORD_ XNEW))
      (COND
       ((AND NEWDE (EQUAL (CAR NEWDE) 'LIST) (CDR NEWDE) (CDDR NEWDE))
        (RETURN
         (PROGN
          (COND
           (PRINT_
            (PROGN
             (TERPRI)
             (PROGN (PRIN2 "The ode has more than one solution.") NIL)
             (PROGN
              (ASSGNPRI (AEVAL "Equation is: ") NIL 'FIRST)
              (ASSGNPRI (AEVAL OLDDE) NIL 'LAST))
             (PROGN
              (ASSGNPRI (AEVAL "Solution is: ") NIL 'FIRST)
              (ASSGNPRI (AEVAL NEWDE) NIL 'LAST)))))
          NIL)))
       (T (SETQ NEWDE (CADR NEWDE))))
      (COND ((NULL NEWDE) (RETURN NIL)))
      (SETQ LD (REVAL1 (LIST 'DF FORD_ XNEW ORDR) T))
      (SETQ H (AEVAL (LIST 'COEFF OLDDE LD)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR H)) (RETURN NIL)))
        (SETQ H (CDR H))
        (GO WHILELABEL))
      (SETQ H (SIMP (CAR H)))
      (SETQ H (SIMPLIFYSQ H FTEM T NIL T))
      (COND ((NULL (CDR H)) (SETQ H (CAR H)))
            (T
             (PROGN
              (SETQ J (CAR H))
              (SETQ H (CDR H))
              (PROG ()
               WHILELABEL
                (COND ((NOT H) (RETURN NIL)))
                (PROGN (SETQ J (MULTSQ (CAR H) J)) (SETQ H (CDR H)))
                (GO WHILELABEL))
              (SETQ H J))))
      (COND
       ((AND (NOT (FREEOFLIST H FTEM)) (NOT (MEMBER H INEQ_)))
        (RETURN
         (PROGN
          (SETQ TO_DO_LIST (CONS (LIST 'SPLIT_INTO_CASES H) TO_DO_LIST))
          NIL))))
      (COND
       ((AND (PAIRP NEWDE) (EQUAL (CAR NEWDE) 'EQUAL) (PAIRP (CDR NEWDE))
             (EQUAL (CADR NEWDE) FORD_) (PAIRP (CDDR NEWDE))
             (PAIRP (CADDR NEWDE)) (EQUAL (CAADDR NEWDE) 'EQUAL))
        (COND ((FREEOF (CADDR NEWDE) FORD_) (SETQ NEWDE NIL))
              (T
               (PROGN
                (SETQ H
                        (SOLVEEVAL
                         (LIST
                          (LIST 'LIST
                                (LIST 'DIFFERENCE (CADR (CADDR NEWDE))
                                      (CADDR (CADDR NEWDE))))
                          (LIST 'LIST FORD_))))
                (COND
                 ((OR (NOT (FREEOF H 'I)) (NOT (FREEOF H '|:GI:|)))
                  (AEVAL (ON (LIST 'COMPLEX)))))
                (COND
                 ((AND (PAIRP H) (EQUAL (CAR H) 'LIST) (PAIRP (CADR H))
                       (EQUAL (CAADR H) 'EQUAL) (EQUAL (CADADR H) FORD_))
                  (SETQ NEWDE (CADR H)))))))))
      (COND
       ((AND (PAIRP NEWDE) (EQUAL (CAR NEWDE) 'LIST) (PAIRP (CDR NEWDE))
             (PAIRP (CADR NEWDE)) (EQUAL (CAADR NEWDE) 'EQUAL)
             (EQUAL (CADADR NEWDE) FORD_) (PAIRP (CDDR NEWDE))
             (PAIRP (CADDR NEWDE)) (EQUAL (CAADDR NEWDE) 'EQUAL)
             (EQUAL (CAR (CDADDR NEWDE)) XNEW) (PAIRP (CDDDR NEWDE))
             (NULL (CDDDDR NEWDE)))
        (PROGN
         (SETQ H
                 (SOLVEEVAL
                  (LIST
                   (LIST 'LIST
                         (LIST 'DIFFERENCE (CADR (CADDR NEWDE))
                               (CADDR (CADDR NEWDE))))
                   (LIST 'LIST (CADDDR NEWDE)))))
         (COND
          ((OR (NOT (FREEOF H 'I)) (NOT (FREEOF H '|:GI:|)))
           (AEVAL (ON (LIST 'COMPLEX)))))
         (COND
          ((AND (PAIRP H) (EQUAL (CAR H) 'LIST) (PAIRP (CADR H))
                (EQUAL (CAADR H) 'EQUAL) (EQUAL (CADADR H) (CADDDR NEWDE)))
           (SETQ NEWDE
                   (LIST 'EQUAL FORD_
                         (SUBST (CAR (CDDADR H)) (CADADR H)
                                (CAR (CDDADR NEWDE))))))))))
      (COND
       ((AND (PAIRP NEWDE) (EQUAL (CAR NEWDE) 'LIST) (PAIRP (CDR NEWDE))
             (PAIRP (CADR NEWDE)) (EQUAL (CAADR NEWDE) 'EQUAL)
             (PAIRP (CDDR NEWDE)) (PAIRP (CADDR NEWDE))
             (EQUAL (CAADDR NEWDE) 'EQUAL)
             (EQUAL (CAR (CDADDR NEWDE)) (CADADR NEWDE)))
        (RETURN
         (PROGN
          (COND
           (PRINT_
            (PROGN
             (TERPRI)
             (PROGN (PRIN2 "The ode has more than one solution.") NIL)
             (PROGN
              (ASSGNPRI (AEVAL "Equation is: ") NIL 'FIRST)
              (ASSGNPRI (AEVAL OLDDE) NIL 'LAST))
             (PROGN
              (ASSGNPRI (AEVAL "Solution is: ") NIL 'FIRST)
              (ASSGNPRI (AEVAL NEWDE) NIL 'LAST)))))
          NIL))))
      (SETQ RULI (START_LET_RULES))
      (COND
       (NEWDE
        (COND
         (*RATIONAL
          (PROGN
           (OFF (LIST 'RATIONAL))
           (SETQ NEWDE (REVAL1 NEWDE T))
           (ON (LIST 'RATIONAL))))
         (T (SETQ NEWDE (REVAL1 NEWDE T))))))
      (COND
       ((AND NEWDE (EQUAL (CAR NEWDE) 'EQUAL) (EQUAL (CADDR NEWDE) 0))
        (SETQ NEWDE (LIST 'EQUAL (REVAL1 (NUM (CADR NEWDE)) T) 0))))
      (COND
       ((AND FLIN_ NEWDE (EQUAL (CAR NEWDE) 'EQUAL))
        (COND
         ((NOT (FREEOFLIST (CADR NEWDE) FLIN_))
          (PROGN
           (SETQ H1 FLIN_)
           (PROG (H)
             (SETQ H H1)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (COND ((NOT (FREEOF NEWDE H)) (SETQ FLIN_ (DELETE H FLIN_)))))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))))
         (T
          (PROGN
           (SETQ J NIL)
           (SETQ H1 FLIN_)
           (PROG (H)
             (SETQ H H1)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H)
                (COND
                 ((NOT (FREEOF NEWDE H))
                  (COND
                   ((NULL (LIN_CHECK (CADDR NEWDE) (LIST H)))
                    (SETQ FLIN_ (DELETE H FLIN_)))
                   (T (SETQ J (CONS H J)))))))
              (CAR H))
             (SETQ H (CDR H))
             (GO LAB))
           (COND
            ((AND J (CDR J))
             (COND
              ((NULL (LIN_CHECK (CADDR NEWDE) J))
               (PROG (H)
                 (SETQ H J)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H) (SETQ FLIN_ (DELETE H FLIN_))) (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))))))
           NIL)))))
      (SETQ J (ZERO_DEN (SUBST FORD FORD_ NEWDE) (CONS FORD_ FTEM)))
      (COND
       ((AND (NULL J) (NOT (FREEOFLIST (CADDR NEWDE) FTEM_)))
        (SETQ J
                (PROPORTIONALITYCONDITIONS (CADDR NEWDE)
                 (PROG (H1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H1 1)
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) H1))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR (CONS (LIST 'ARBCONST H1) NIL)))
                  LOOPLABEL
                   (SETQ H1
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            H1))
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) H1))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS (LIST 'ARBCONST H1) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 XNEW))))
      (COND
       (J
        (RETURN
         (COND (T NIL)
               (T
                (PROGN
                 (SETQ NEWVL
                         (DELETE XNEW
                                 (COND
                                  ((AND (PAIRP FORD) (EQUAL (CAR FORD) 'DF))
                                   (FCTARGS (CADR FORD)))
                                  (T (FCTARGS FORD)))))
                 (PROG (H1)
                   (SETQ H1 1)
                  LAB
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) H1))
                     (RETURN NIL)))
                   (COND
                    ((NOT (FREEOF J (LIST 'ARBCONST H1)))
                     (PROGN
                      (SETQ NEWCO (NEWFCT FNAME_ NEWVL NFCT_))
                      (SETQ NFCT_ (ADD1 NFCT_))
                      (SETQ FNEW_ (CONS NEWCO FNEW_))
                      (SETQ J
                              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                                (SETQ H J)
                                (COND ((NULL H) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (H)
                                                    (SUBST NEWCO
                                                           (LIST 'ARBCONST H1)
                                                           H))
                                                  (CAR H))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ H (CDR H))
                                (COND ((NULL H) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (H)
                                            (SUBST NEWCO (LIST 'ARBCONST H1)
                                                   H))
                                          (CAR H))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))))
                   (SETQ H1
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            H1))
                   (GO LAB))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT J) (RETURN NIL)))
                   (PROGN
                    (COND
                     ((AND (FREEOFLIST (CAR J) FNEW_)
                           (NOT (MEMBER (CAR J) (CDR J))))
                      (SETQ TO_DO_LIST
                              (CONS (LIST 'SPLIT_INTO_CASES (CAR J))
                                    TO_DO_LIST))))
                    (SETQ J (CDR J)))
                   (GO WHILELABEL))
                 NIL))))))
      (COND ((AND FREEINT_ (NULL (FREEOF NEWDE 'INT))) (SETQ NEWDE NIL)))
      (COND ((AND FREEABS_ (NULL (FREEOF NEWDE 'ABS))) (SETQ NEWDE NIL)))
      (COND
       ((AND NEWDE (NEQ (CADR NEWDE) OLDDE))
        (PROGN
         (COND
          ((NEQ (CADR NEWDE) FORD_)
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "Solution of the ode is not explicitly given.")
                NIL)
               (PROGN
                (ASSGNPRI (AEVAL "Equation is: ") NIL 'FIRST)
                (ASSGNPRI (AEVAL OLDDE) NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL "Solution is: ") NIL 'FIRST)
                (ASSGNPRI (AEVAL NEWDE) NIL 'LAST)))))
            (COND
             (POLY_ONLY
              (COND ((NOT (RATIONALP NEWDE FORD_)) (SETQ NEWDE NIL))
                    (T
                     (PROGN
                      (SETQ J 1)
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT
                           (AND (LEQ J ORDR)
                                (RATIONALP
                                 (SUBST FORD_ (LIST 'ARBCONST J) NEWDE)
                                 FORD_)))
                          (RETURN NIL)))
                        (SETQ J (PLUS J 1))
                        (GO WHILELABEL))
                      (COND ((LEQ J ORDR) (SETQ NEWDE NIL))))))))
            (COND
             ((AND (PAIRP NEWDE) (EQUAL (CAR NEWDE) 'EQUAL))
              (COND
               ((AND (PAIRP (CADR NEWDE)) (EQUAL (CAADR NEWDE) 'QUOTIENT)
                     (ZEROP (CADDR NEWDE)))
                (SETQ NEWDE (LIST 'EQUAL (CADADR NEWDE) 0)))
               ((AND (PAIRP (CADDR NEWDE)) (EQUAL (CAADDR NEWDE) 'QUOTIENT)
                     (ZEROP (CADR NEWDE)))
                (SETQ NEWDE (LIST 'EQUAL 0 (CADR (CADDR NEWDE)))))
               (T
                (PROGN
                 (SETQ H1
                         (REVAL1 (LIST 'DIFFERENCE (CADR NEWDE) (CADDR NEWDE))
                                 T))
                 (SETQ J 1)
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND (LEQ J ORDR)
                           (OR (FREEOF H1 (LIST 'ARBCONST J))
                               (NOT
                                (LIN_CHECK H1 (LIST (LIST 'ARBCONST J)))))))
                     (RETURN NIL)))
                   (SETQ J (PLUS J 1))
                   (GO WHILELABEL))
                 (COND
                  ((LEQ J ORDR)
                   (PROGN
                    (SETQ H (COEFFN H1 (LIST 'ARBCONST J) 1))
                    (SETQ T2
                            (REVAL1
                             (LIST 'QUOTIENT
                                   (LIST 'DIFFERENCE
                                         (LIST 'TIMES (LIST 'ARBCONST J) H) H1)
                                   H)
                             T))
                    (SETQ H2
                            (LIST 'EQUAL
                                  (LIST 'PLUS (LIST 'ARBCONST J)
                                        (SIMPLIFIZIERE T2
                                         (CONS FORD_ (CONS XNEW FTEM)) NIL))
                                  0))
                    (COND
                     ((NEQ H2 NEWDE)
                      (PROGN
                       (SETQ NEWDE H2)
                       (COND
                        (PRINT_
                         (PROGN
                          (ASSGNPRI (AEVAL "The solution is modified to: ") NIL
                                    'FIRST)
                          (ASSGNPRI (AEVAL NEWDE) NIL 'LAST)))))))))))))))))
          (T
           (PROGN
            (SETQ NULL_ (SIMP* (REVAL1 (SUBST (CADDR NEWDE) FORD_ OLDDE) T)))
            (COND
             ((NOT (SQZEROP NULL_))
              (PROGN
               (COND
                (PRINT_
                 (PROGN
                  (PROGN (PRIN2 "odesolve solves :  ") NIL)
                  (DEPRINT (LIST OLDDE))
                  (PROGN (PRIN2 "to") NIL)
                  (EQPRINT NEWDE)
                  (PROGN (PRIN2 "which inserted in the equation yields ") NIL)
                  (EQPRINT (LIST '*SQ NULL_ T))))))))))))))
      (COND
       (NEWDE
        (PROGN
         (SETQ NEWDE (LIST 'PLUS (CADR NEWDE) (LIST 'MINUS (CADDR NEWDE))))
         (COND
          ((ZEROP (REVAL1 (LIST 'PLUS NEWDE (LIST 'MINUS OLDDE)) T))
           (SETQ NEWDE NIL)))
         NIL)))
      (SETQ DEPL* (DELETE (ASSOC FORD_ DEPL*) DEPL*))
      (STOP_LET_RULES RULI)
      (RETURN
       (COND ((NULL NEWDE) NIL)
             (T
              (PROGN
               (SETQ NEWDE (SUBST FORD FORD_ NEWDE))
               (SETQ NEWVL
                       (DELETE XNEW
                               (COND
                                ((AND (PAIRP FORD) (EQUAL (CAR FORD) 'DF))
                                 (FCTARGS (CADR FORD)))
                                (T (FCTARGS FORD)))))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) J))
                   (RETURN NIL)))
                 (COND
                  ((NOT (FREEOF NEWDE (LIST 'ARBCONST J)))
                   (PROGN
                    (SETQ NEWCO (NEWFCT FNAME_ NEWVL NFCT_))
                    (SETQ NFCT_ (ADD1 NFCT_))
                    (SETQ FNEW_ (CONS NEWCO FNEW_))
                    (SETQ NEWDE (SUBST NEWCO (LIST 'ARBCONST J) NEWDE))
                    (COND
                     ((LIN_CHECK NEWDE (LIST NEWCO))
                      (SETQ FLIN_ (FCTINSERT NEWCO FLIN_))))
                    NIL)))
                 (SETQ J
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          J))
                 (GO LAB))
               (LIST NEWDE))))))) 
(ENDMODULE) 
(MODULE (LIST 'JETDIFFERENTIATION)) 
(PUT 'COMPAREDIF1 'NUMBER-OF-ARGS 2) 
(PUT 'COMPAREDIF1 'DEFINED-ON-LINE '2687) 
(PUT 'COMPAREDIF1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'COMPAREDIF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPAREDIF1 (U1L U2L)
    (PROG (UL)
      (COND
       ((EQUAL U2L NIL) (COND ((NEQ U1L NIL) (RETURN NIL)) (T (RETURN 0))))
       ((EQUAL U1L NIL) (RETURN U2L))
       ((LESSP (CAR U1L) (CAR U2L)) (RETURN NIL))
       ((EQUAL (CAR U1L) (CAR U2L)) (RETURN (COMPAREDIF1 (CDR U1L) (CDR U2L))))
       (T
        (PROGN
         (SETQ UL (COMPAREDIF1 U1L (CDR U2L)))
         (RETURN
          (COND ((NOT UL) NIL) ((ZEROP UL) (LIST (CAR U2L)))
                (T (CONS (CAR U2L) UL))))))))) 
(PUT 'COMPAREDIF2 'NUMBER-OF-ARGS 3) 
(PUT 'COMPAREDIF2 'DEFINED-ON-LINE '2709) 
(PUT 'COMPAREDIF2 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'COMPAREDIF2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPAREDIF2 (U1 U1LIST DU2)
    (PROG (U2L)
      (SETQ U2L (COMBIDIF DU2))
      (COND ((NEQ (CAR U2L) U1) (RETURN NIL))
            (T (RETURN (COMPAREDIF1 U1LIST (CDR U2L))))))) 
(PUT 'LISTDIFDIF1 'NUMBER-OF-ARGS 2) 
(PUT 'LISTDIFDIF1 'DEFINED-ON-LINE '2721) 
(PUT 'LISTDIFDIF1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LISTDIFDIF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTDIFDIF1 (DU1 DEPLIST)
    (PROG (U1 U1LIST RES H)
      (SETQ H (COMBIDIF DU1))
      (SETQ U1 (CAR H))
      (SETQ U1LIST (CDR H))
      (PROG (H)
        (SETQ H DEPLIST)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND ((NOT (COMPAREDIF2 U1 U1LIST H)) (SETQ RES (CONS H RES)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN RES))) 
(PUT 'DIFFDEG 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFDEG 'DEFINED-ON-LINE '2736) 
(PUT 'DIFFDEG 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DIFFDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFDEG (P V)
    (COND ((NULL P) 0)
          ((EQUAL (CAR P) V)
           (COND ((CDR P) (COND ((NUMBERP (CADR P)) (CADR P)) (T 1))) (T 1)))
          (T (DIFFDEG (CDR P) V)))) 
(PUT 'LDIFF1 'NUMBER-OF-ARGS 2) 
(PUT 'LDIFF1 'DEFINED-ON-LINE '2749) 
(PUT 'LDIFF1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LDIFF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDIFF1 (L V)
    (COND ((NULL V) NIL) (T (CONS (DIFFDEG L (CAR V)) (LDIFF1 L (CDR V)))))) 
(PUT 'LDIFFTOT 'NUMBER-OF-ARGS 2) 
(PUT 'LDIFFTOT 'DEFINED-ON-LINE '2759) 
(PUT 'LDIFFTOT 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LDIFFTOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDIFFTOT (P F) (LDIFFTOT1 P F (FCTARGS F))) 
(PUT 'LDIFFTOT1 'NUMBER-OF-ARGS 3) 
(PUT 'LDIFFTOT1 'DEFINED-ON-LINE '2767) 
(PUT 'LDIFFTOT1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'LDIFFTOT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LDIFFTOT1 (P F VL)
    (PROG (A)
      (SETQ A (CONS NIL 0))
      (COND
       ((NOT (ATOM P))
        (COND
         ((MEMBER (CAR P) REDUCEFUNCTIONS_)
          (PROGN
           (COND
            ((OR (EQUAL (CAR P) 'PLUS) (EQUAL (CAR P) 'TIMES)
                 (EQUAL (CAR P) 'QUOTIENT) (EQUAL (CAR P) 'EQUAL))
             (PROGN
              (SETQ P (CDR P))
              (PROG ()
               WHILELABEL
                (COND ((NOT P) (RETURN NIL)))
                (PROGN
                 (SETQ A (DIFFRELTOT (LDIFFTOT1 (CAR P) F VL) A VL))
                 (SETQ P (CDR P)))
                (GO WHILELABEL))))
            ((EQUAL (CAR P) 'MINUS) (SETQ A (LDIFFTOT1 (CADR P) F VL)))
            ((EQUAL (CAR P) 'EXPT)
             (PROGN
              (SETQ A (LDIFFTOT1 (CADR P) F VL))
              (COND
               ((AND (NUMBERP (CADDR P)) (NUMBERP (CDR A)))
                (SETQ A (CONS (CAR A) (TIMES (CADDR P) (CDR A)))))
               (T (SETQ A (CONS (CAR A) 10000))))))
            ((EQUAL (CAR P) 'DF)
             (COND ((EQUAL (CADR P) F) (SETQ A (CONS (CDDR P) 1)))
                   (T (SETQ A (CONS NIL 0)))))
            (T
             (PROGN
              (SETQ P (CDR P))
              (PROG ()
               WHILELABEL
                (COND ((NOT P) (RETURN NIL)))
                (PROGN
                 (SETQ A (DIFFRELTOT (LDIFFTOT1 (CAR P) F VL) A VL))
                 (SETQ P (CDR P)))
                (GO WHILELABEL))
              (SETQ A (CONS (CAR A) 10000)))))))
         ((EQUAL P F) (SETQ A (CONS NIL 1))) (T (SETQ A (CONS NIL 0)))))
       ((EQUAL P F) (SETQ A (CONS NIL 1))))
      (RETURN A))) 
(PUT 'DIFFRELTOT 'NUMBER-OF-ARGS 3) 
(PUT 'DIFFRELTOT 'DEFINED-ON-LINE '2821) 
(PUT 'DIFFRELTOT 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DIFFRELTOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFFRELTOT (P Q V) (COND ((DIFFRELTOTP P Q V) Q) (T P))) 
(PUT 'DIFFRELTOTP 'NUMBER-OF-ARGS 3) 
(PUT 'DIFFRELTOTP 'DEFINED-ON-LINE '2828) 
(PUT 'DIFFRELTOTP 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DIFFRELTOTP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFFRELTOTP (P Q V)
    (PROG (N M)
      (SETQ M (EVAL (CONS 'PLUS (LDIFF1 (CAR P) V))))
      (SETQ N (EVAL (CONS 'PLUS (LDIFF1 (CAR Q) V))))
      (RETURN (COND ((LESSP M N) T) ((LESSP N M) NIL) (T (DIFFRELP P Q V)))))) 
(PUT 'SUBDIF1 'NUMBER-OF-ARGS 3) 
(FLAG '(SUBDIF1) 'OPFN) 
(PUT 'SUBDIF1 'DEFINED-ON-LINE '2844) 
(PUT 'SUBDIF1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SUBDIF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBDIF1 (XLIST YLIST ORDR)
    (PROG (ALLSUB REVX I EL OLDSUB NEWSUB)
      (SETQ REVX (AEVAL (LIST 'SQREVERSE XLIST)))
      (SETQ ALLSUB (AEVAL (LIST 'LIST)))
      (SETQ OLDSUB
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y (GETRLIST (AEVAL YLIST)))
                (COND ((NULL Y) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y) (AEVAL (LIST 'EQUAL Y Y)))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (AEVAL (LIST 'EQUAL Y Y))) (CAR Y))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDR) I)) (RETURN NIL)))
        (PROGN
         (SETQ OLDSUB
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL (GETRLIST (AEVAL* OLDSUB)))
                  STARTOVER
                   (COND ((NULL EL) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (EL) (AEVAL* (LIST 'NEXTDY REVX XLIST EL)))
                            (CAR EL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ EL (CDR EL))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (EL)
                               (AEVAL* (LIST 'NEXTDY REVX XLIST EL)))
                             (CAR EL))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ EL (CDR EL))
                   (GO LOOPLABEL)))
         (SETQ ALLSUB (AEVAL* (LIST 'SQCONS OLDSUB ALLSUB))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL ALLSUB)))) 
(PUT 'NEXTDY 'NUMBER-OF-ARGS 3) 
(FLAG '(NEXTDY) 'OPFN) 
(PUT 'NEXTDY 'DEFINED-ON-LINE '2860) 
(PUT 'NEXTDY 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'NEXTDY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEXTDY (REVX XLIST DY)
    (PROG (X N LDY RDY LDYX SUBLIST)
      (SETQ X (AEVAL (LIST 'SQFIRST REVX)))
      (SETQ REVX (AEVAL (LIST 'SQREST REVX)))
      (SETQ SUBLIST (AEVAL (LIST 'LIST)))
      (SETQ LDY (AEVAL (LIST 'LHS DY)))
      (SETQ RDY (AEVAL (LIST 'RHS DY)))
      (WHILE
       (AND
        (BOOLVALUE*
         (REVALX
          (NOT
           (MEMBER (PREPSQ (SIMP* (REVALX X)))
                   (PREPSQ (SIMP* (REVALX LDY)))))))
        (EVALNEQ (AEVAL* REVX) (AEVAL* (LIST 'LIST))))
       (PROGN
        (SETQ X (AEVAL* (LIST 'SQFIRST REVX)))
        (SETQ REVX (AEVAL* (LIST 'SQREST REVX)))))
      (SETQ N (AEVAL (LIST 'LENGTH XLIST)))
      (COND
       ((EVALNEQ (AEVAL REVX) (AEVAL (LIST 'LIST)))
        (WHILE (EVALNEQ (AEVAL* (LIST 'SQFIRST XLIST)) (AEVAL* X))
               (SETQ XLIST (AEVAL* (LIST 'SQREST XLIST))))))
      (SETQ XLIST (AEVAL (LIST 'REVERSE XLIST)))
      (WHILE (EVALNEQ (AEVAL* XLIST) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ X (AEVAL* (LIST 'SQFIRST XLIST)))
              (SETQ LDYX (AEVAL* (LIST 'DF LDY X)))
              (SETQ SUBLIST
                      (AEVAL*
                       (LIST 'CONS
                             (LIST 'EQUAL (REVAL1 (AEVAL* LDYX) T)
                                   (LIST 'MKID (LIST 'MKID RDY '|`|) N))
                             SUBLIST)))
              (SETQ N (AEVAL* (LIST 'DIFFERENCE N 1)))
              (SETQ XLIST (AEVAL* (LIST 'SQREST XLIST)))))
      (RETURN (AEVAL SUBLIST)))) 
(FLAG '(TOTDEG) 'OPFN) 
(PUT 'TOTDEG 'NUMBER-OF-ARGS 2) 
(PUT 'TOTDEG 'DEFINED-ON-LINE '2896) 
(PUT 'TOTDEG 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'TOTDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTDEG (P F)
    (EVAL
     (CONS 'PLUS
           (LDIFF1 (CAR (LDIFFTOT (REVAL1 P T) (REVAL1 F T)))
            (FCTARGS (REVAL1 F T)))))) 
(PUT 'COMBIDIF 'NUMBER-OF-ARGS 1) 
(PUT 'COMBIDIF 'DEFINED-ON-LINE '2902) 
(PUT 'COMBIDIF 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'COMBIDIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMBIDIF (S)
    (PROG (TEMP ANS NO N1)
      (SETQ S (REVAL1 S T))
      (SETQ TEMP (REVERSE (EXPLODE S)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL TEMP))) (RETURN NIL)))
        (PROGN
         (SETQ N1
                 (PROGN
                  (SETQ NO NIL)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT (AND (NOT (NULL TEMP)) (NOT (EQCAR TEMP '|`|))))
                      (RETURN NIL)))
                    (PROGN
                     (SETQ NO (CONS (CAR TEMP) NO))
                     (SETQ TEMP (CDR TEMP)))
                    (GO WHILELABEL))
                  (COMPRESS NO)))
         (COND ((NOT (FIXP N1)) (SETQ N1 (INTERN N1))))
         (SETQ ANS (CONS N1 ANS))
         (COND
          ((EQCAR TEMP '|`|)
           (PROGN (SETQ TEMP (CDR TEMP)) (SETQ TEMP (CDR TEMP)))))
         NIL)
        (GO WHILELABEL))
      (RETURN ANS))) 
(FLAG '(DIF) 'OPFN) 
(PUT 'DIF 'NUMBER-OF-ARGS 2) 
(PUT 'DIF 'DEFINED-ON-LINE '2924) 
(PUT 'DIF 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DIF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIF (S N)
    (PROG (TEMP ANS NO N1 N2 DONE)
      (SETQ S (REVAL1 S T))
      (SETQ TEMP (REVERSE (EXPLODE S)))
      (SETQ N2 (REVAL1 N T))
      (SETQ N2 (EXPLODE N2))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT (NULL TEMP)) (NOT DONE))) (RETURN NIL)))
        (PROGN
         (SETQ N1
                 (PROGN
                  (SETQ NO NIL)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT (AND (NOT (NULL TEMP)) (NOT (EQCAR TEMP '|`|))))
                      (RETURN NIL)))
                    (PROGN
                     (SETQ NO (CONS (CAR TEMP) NO))
                     (SETQ TEMP (CDR TEMP)))
                    (GO WHILELABEL))
                  (COMPRESS NO)))
         (COND
          ((OR (NOT (FIXP N1)) (AND (FIXP N1) (LEQ N1 N)))
           (PROGN
            (SETQ ANS (NCONC N2 ANS))
            (SETQ ANS (CONS '|`| ANS))
            (SETQ ANS (CONS '! ANS))
            (SETQ DONE T))))
         (SETQ ANS (NCONC NO ANS))
         (COND
          ((EQCAR TEMP '|`|)
           (PROGN
            (SETQ ANS (CONS '|`| ANS))
            (SETQ ANS (CONS '! ANS))
            (SETQ TEMP (CDR TEMP))
            (SETQ TEMP (CDR TEMP)))))
         NIL)
        (GO WHILELABEL))
      (RETURN (INTERN (COMPRESS (NCONC (REVERSE TEMP) ANS)))))) 
(PUT 'TOTDIF 'PSOPFN 'TOT*DIF) 
(PUT 'TOT*DIF 'NUMBER-OF-ARGS 1) 
(PUT 'TOT*DIF 'DEFINED-ON-LINE '2969) 
(PUT 'TOT*DIF 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'TOT*DIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOT*DIF (INP)
    (PROG (TDF EL1 EL2 S X N DYLIST)
      (SETQ S (REVAL1 (CAR INP) NIL))
      (SETQ S
              (COND ((AND (PAIRP S) (EQUAL (CAR S) '*SQ)) (CADR S))
                    (T (SIMP S))))
      (SETQ X (REVAL1 (CADR INP) T))
      (COND ((AND (PAIRP X) (EQUAL (CAR X) '*SQ)) (SETQ X (REVAL1 X T))))
      (SETQ N (REVAL1 (CADDR INP) T))
      (SETQ DYLIST (CDR (REVAL1 (CADDDR INP) T)))
      (SETQ TDF (DIFFSQ S X))
      (PROG ()
       WHILELABEL
        (COND ((NOT DYLIST) (RETURN NIL)))
        (PROGN
         (SETQ EL1 (CDAR DYLIST))
         (SETQ DYLIST (CDR DYLIST))
         (PROG ()
          WHILELABEL
           (COND ((NOT EL1) (RETURN NIL)))
           (PROGN
            (SETQ EL2 (CAR EL1))
            (SETQ EL1 (CDR EL1))
            (SETQ TDF (ADDSQ TDF (MULTSQ (SIMP* (DIF EL2 N)) (DIFFSQ S EL2)))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN (LIST '*SQ TDF T)))) 
(ENDMODULE) 
(MODULE (LIST 'DIVINTEGRATION)) 
(PUT 'COMBI 'NUMBER-OF-ARGS 1) 
(PUT 'COMBI 'DEFINED-ON-LINE '3001) 
(PUT 'COMBI 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'COMBI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMBI (ILIST)
    (PROG (N0 N1 N2 N3)
      (SETQ N0 0)
      (SETQ N1 0)
      (SETQ N2 0)
      (SETQ N3 0)
      (SETQ N1 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT ILIST) (RETURN NIL)))
        (PROGN
         (SETQ N0 (PLUS N0 1))
         (SETQ N1 (TIMES N1 N0))
         (COND
          ((EQUAL (CAR ILIST) N2)
           (PROGN (SETQ N3 (PLUS N3 1)) (SETQ N1 (QUOTIENT N1 N3))))
          (T (PROGN (SETQ N2 (CAR ILIST)) (SETQ N3 1))))
         (SETQ ILIST (CDR ILIST)))
        (GO WHILELABEL))
      (RETURN N1))) 
(PUT 'SORTLI 'NUMBER-OF-ARGS 1) 
(PUT 'SORTLI 'DEFINED-ON-LINE '3018) 
(PUT 'SORTLI 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SORTLI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORTLI (L)
    (PROG (L1 L2 L3 M N)
      (RETURN
       (COND ((NULL L) NIL)
             (T
              (PROGN
               (SETQ N (CAR L))
               (SETQ L2 (LIST (CAR L)))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (SETQ M (CAR L))
                  (COND ((GREATERP M N) (SETQ L1 (CONS (CAR L) L1)))
                        ((LESSP M N) (SETQ L3 (CONS (CAR L) L3)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (SORTLI L1) (APPEND L2 (SORTLI L3))))))))) 
(PUT 'DERILI 'NUMBER-OF-ARGS 1) 
(PUT 'DERILI 'DEFINED-ON-LINE '3040) 
(PUT 'DERILI 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DERILI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DERILI (IL)
    (COND ((NULL IL) NIL)
          (T
           (PROG (H1 H2 H3)
             (SETQ H1 (SORTLI IL))
             (PROG ()
              WHILELABEL
               (COND ((NOT H1) (RETURN NIL)))
               (PROGN
                (SETQ H2 (REVAL1 (AEVAL* (LIST 'MKID '|`| (CAR H1))) T))
                (SETQ H3 (COND (H3 (MKID H2 H3)) (T H2)))
                (SETQ H1 (CDR H1)))
               (GO WHILELABEL))
             (RETURN H3))))) 
(PUT 'NEWIL 'NUMBER-OF-ARGS 3) 
(PUT 'NEWIL 'DEFINED-ON-LINE '3056) 
(PUT 'NEWIL 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'NEWIL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWIL (IL MO NX)
    (COND ((OR (NULL IL) (LESSP (LENGTH IL) MO)) (CONS 1 IL))
          ((LESSP (CAR IL) NX) (CONS (ADD1 (CAR IL)) (CDR IL)))
          (T
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND IL (EQUAL (CAR IL) NX))) (RETURN NIL)))
              (SETQ IL (CDR IL))
              (GO WHILELABEL))
            (COND ((NULL IL) NIL) (T (CONS (ADD1 (CAR IL)) (CDR IL)))))))) 
(FLAG '(INTCURRENT1) 'OPFN) 
(PUT 'INTCURRENT1 'NUMBER-OF-ARGS 7) 
(PUT 'INTCURRENT1 'DEFINED-ON-LINE '3067) 
(PUT 'INTCURRENT1 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTCURRENT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE INTCURRENT1 (DIVP ULIST XLIST DULIST NX EQORD DENSORD)
    (PROG (II IL H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 CONTRACE U NEGA PII MO PL
           NU)
      (SETQ XLIST (CDR XLIST))
      (SETQ ULIST (CDR ULIST))
      (SETQ NU (LENGTH ULIST))
      (SETQ MO
              (COND ((GREATERP EQORD DENSORD) (DIFFERENCE EQORD 1))
                    (T (DIFFERENCE DENSORD 1))))
      (SETQ PL
              (PROG (II FORALL-RESULT FORALL-ENDPTR)
                (SETQ II 1)
                (COND ((MINUSP (DIFFERENCE NX II)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ IL NIL)
                                  (SETQ PII NIL)
                                  (PROG ()
                                   REPEATLABEL
                                    (PROGN
                                     (SETQ H11 (CONS II IL))
                                     (SETQ H1 (DERILI H11))
                                     (SETQ H11 (COMBI (SORTLI H11)))
                                     (COND
                                      (CONTRACE
                                       (PROGN
                                        (PROGN
                                         (PRIN2 "==== ii=")
                                         (PRIN2 II)
                                         (PRIN2 "  il=")
                                         (PRIN2 IL)
                                         (PRIN2 "  h1=")
                                         (PRIN2 H1)
                                         (PRIN2 "  h11=")
                                         (PRIN2 H11)
                                         NIL)
                                        (TERPRI))))
                                     (SETQ H2 IL)
                                     (SETQ H3 NIL)
                                     (COND
                                      ((NULL H2)
                                       (SETQ H4 (LIST (CONS NIL NIL))))
                                      (T
                                       (PROGN
                                        (SETQ H4 (LIST (CONS (CAR H2) NIL)))
                                        (PROG ()
                                         WHILELABEL
                                          (COND ((NOT H2) (RETURN NIL)))
                                          (PROGN
                                           (SETQ H3 (CONS (CAR H2) H3))
                                           (SETQ H2 (CDR H2))
                                           (SETQ H4
                                                   (CONS
                                                    (CONS
                                                     (COND (H2 (CAR H2))
                                                           (T NIL))
                                                     (DERILI H3))
                                                    H4))
                                           NIL)
                                          (GO WHILELABEL))
                                        NIL)))
                                     (COND
                                      (CONTRACE
                                       (PROGN
                                        (PROGN
                                         (PRIN2 "h3=")
                                         (PRIN2 H3)
                                         (PRIN2 "  h4=")
                                         (PRIN2 H4)
                                         NIL)
                                        (TERPRI))))
                                     (PROG (E1)
                                       (SETQ E1 1)
                                      LAB
                                       (COND
                                        ((MINUSP (DIFFERENCE NU E1))
                                         (RETURN NIL)))
                                       (PROGN
                                        (SETQ U (NTH ULIST E1))
                                        (SETQ H5 (MKID U H1))
                                        (COND
                                         (CONTRACE
                                          (PROGN
                                           (PROGN (PRIN2 "h5=") (PRIN2 H5) NIL)
                                           (TERPRI))))
                                        (COND
                                         (CONTRACE
                                          (PROGN
                                           (PROGN
                                            (PRIN2 "h6-1=")
                                            (PRIN2 (LIST 'DF DIVP H5))
                                            NIL)
                                           (TERPRI))))
                                        (SETQ H6
                                                (REVAL1
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DF DIVP H5) H11)
                                                 T))
                                        (COND
                                         (CONTRACE
                                          (PROGN
                                           (PROGN (PRIN2 "h6=") (PRIN2 H6) NIL)
                                           (TERPRI))))
                                        (SETQ NEGA NIL)
                                        (SETQ H7 H4)
                                        (PROG ()
                                         WHILELABEL
                                          (COND
                                           ((NOT (AND H7 (NOT (ZEROP H6))))
                                            (RETURN NIL)))
                                          (PROGN
                                           (SETQ H8 (CAR H7))
                                           (SETQ H7 (CDR H7))
                                           (SETQ H9 (CAR H8))
                                           (SETQ H8 (CDR H8))
                                           (COND
                                            (CONTRACE
                                             (PROGN
                                              (PROGN
                                               (PRIN2
                                                (COND (NEGA "--") (T "++")))
                                               (PRIN2 " h8=")
                                               (PRIN2 H8)
                                               (PRIN2 "   h9=")
                                               (PRIN2 H9)
                                               NIL)
                                              (TERPRI))))
                                           (COND
                                            (CONTRACE
                                             (PROGN
                                              (PROGN
                                               (PRIN2 "h9-2=")
                                               (PRIN2 H9)
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2 "h6=")
                                               (PRIN2 H6)
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2 "xlist=")
                                               (PRIN2 XLIST)
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2 "dulist=")
                                               (PRIN2 DULIST)
                                               NIL)
                                              (TERPRI))))
                                           (COND
                                            (H9
                                             (SETQ H6
                                                     (REVAL1
                                                      (TOT*DIF
                                                       (LIST (REVAL1 H6 NIL)
                                                             (NTH XLIST H9) H9
                                                             DULIST))
                                                      T))))
                                           (COND
                                            (CONTRACE
                                             (PROGN
                                              (PROGN
                                               (PRIN2 "h6-3=")
                                               (PRIN2 H6)
                                               NIL)
                                              (TERPRI))))
                                           (SETQ H10
                                                   (COND (H8 (MKID U H8))
                                                         (T U)))
                                           (COND
                                            (CONTRACE
                                             (PROGN
                                              (PROGN
                                               (PRIN2 "h10=")
                                               (PRIN2 H10)
                                               NIL)
                                              (TERPRI))))
                                           (SETQ H10 (LIST 'TIMES H6 H10))
                                           (COND
                                            (NEGA
                                             (SETQ H10 (LIST 'MINUS H10))))
                                           (COND
                                            (CONTRACE
                                             (PROGN
                                              (ASSGNPRI (AEVAL* ">>>> h10=")
                                                        NIL 'FIRST)
                                              (ASSGNPRI (AEVAL* H10) NIL
                                                        'LAST))))
                                           (SETQ PII (CONS H10 PII))
                                           (SETQ NEGA (NOT NEGA))
                                           NIL)
                                          (GO WHILELABEL)))
                                       (SETQ E1 (PLUS2 E1 1))
                                       (GO LAB))
                                     (SETQ IL (NEWIL IL MO NX))
                                     NIL)
                                    (COND ((NOT (NULL IL)) (GO REPEATLABEL))))
                                  (SETQ PII
                                          (REVAL1
                                           (COND ((NULL PII) 0)
                                                 ((EQUAL (LENGTH PII) 1)
                                                  (CAR PII))
                                                 (T (CONS 'PLUS PII)))
                                           T))
                                  (COND
                                   (CONTRACE
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "pii-end=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* PII) NIL 'LAST))))
                                  PII)
                                 NIL)))
               LOOPLABEL
                (SETQ II (PLUS2 II 1))
                (COND ((MINUSP (DIFFERENCE NX II)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ IL NIL)
                          (SETQ PII NIL)
                          (PROG ()
                           REPEATLABEL
                            (PROGN
                             (SETQ H11 (CONS II IL))
                             (SETQ H1 (DERILI H11))
                             (SETQ H11 (COMBI (SORTLI H11)))
                             (COND
                              (CONTRACE
                               (PROGN
                                (PROGN
                                 (PRIN2 "==== ii=")
                                 (PRIN2 II)
                                 (PRIN2 "  il=")
                                 (PRIN2 IL)
                                 (PRIN2 "  h1=")
                                 (PRIN2 H1)
                                 (PRIN2 "  h11=")
                                 (PRIN2 H11)
                                 NIL)
                                (TERPRI))))
                             (SETQ H2 IL)
                             (SETQ H3 NIL)
                             (COND ((NULL H2) (SETQ H4 (LIST (CONS NIL NIL))))
                                   (T
                                    (PROGN
                                     (SETQ H4 (LIST (CONS (CAR H2) NIL)))
                                     (PROG ()
                                      WHILELABEL
                                       (COND ((NOT H2) (RETURN NIL)))
                                       (PROGN
                                        (SETQ H3 (CONS (CAR H2) H3))
                                        (SETQ H2 (CDR H2))
                                        (SETQ H4
                                                (CONS
                                                 (CONS
                                                  (COND (H2 (CAR H2)) (T NIL))
                                                  (DERILI H3))
                                                 H4))
                                        NIL)
                                       (GO WHILELABEL))
                                     NIL)))
                             (COND
                              (CONTRACE
                               (PROGN
                                (PROGN
                                 (PRIN2 "h3=")
                                 (PRIN2 H3)
                                 (PRIN2 "  h4=")
                                 (PRIN2 H4)
                                 NIL)
                                (TERPRI))))
                             (PROG (E1)
                               (SETQ E1 1)
                              LAB
                               (COND
                                ((MINUSP (DIFFERENCE NU E1)) (RETURN NIL)))
                               (PROGN
                                (SETQ U (NTH ULIST E1))
                                (SETQ H5 (MKID U H1))
                                (COND
                                 (CONTRACE
                                  (PROGN
                                   (PROGN (PRIN2 "h5=") (PRIN2 H5) NIL)
                                   (TERPRI))))
                                (COND
                                 (CONTRACE
                                  (PROGN
                                   (PROGN
                                    (PRIN2 "h6-1=")
                                    (PRIN2 (LIST 'DF DIVP H5))
                                    NIL)
                                   (TERPRI))))
                                (SETQ H6
                                        (REVAL1
                                         (LIST 'QUOTIENT (LIST 'DF DIVP H5)
                                               H11)
                                         T))
                                (COND
                                 (CONTRACE
                                  (PROGN
                                   (PROGN (PRIN2 "h6=") (PRIN2 H6) NIL)
                                   (TERPRI))))
                                (SETQ NEGA NIL)
                                (SETQ H7 H4)
                                (PROG ()
                                 WHILELABEL
                                  (COND
                                   ((NOT (AND H7 (NOT (ZEROP H6))))
                                    (RETURN NIL)))
                                  (PROGN
                                   (SETQ H8 (CAR H7))
                                   (SETQ H7 (CDR H7))
                                   (SETQ H9 (CAR H8))
                                   (SETQ H8 (CDR H8))
                                   (COND
                                    (CONTRACE
                                     (PROGN
                                      (PROGN
                                       (PRIN2 (COND (NEGA "--") (T "++")))
                                       (PRIN2 " h8=")
                                       (PRIN2 H8)
                                       (PRIN2 "   h9=")
                                       (PRIN2 H9)
                                       NIL)
                                      (TERPRI))))
                                   (COND
                                    (CONTRACE
                                     (PROGN
                                      (PROGN (PRIN2 "h9-2=") (PRIN2 H9) NIL)
                                      (TERPRI)
                                      (PROGN (PRIN2 "h6=") (PRIN2 H6) NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2 "xlist=")
                                       (PRIN2 XLIST)
                                       NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2 "dulist=")
                                       (PRIN2 DULIST)
                                       NIL)
                                      (TERPRI))))
                                   (COND
                                    (H9
                                     (SETQ H6
                                             (REVAL1
                                              (TOT*DIF
                                               (LIST (REVAL1 H6 NIL)
                                                     (NTH XLIST H9) H9 DULIST))
                                              T))))
                                   (COND
                                    (CONTRACE
                                     (PROGN
                                      (PROGN (PRIN2 "h6-3=") (PRIN2 H6) NIL)
                                      (TERPRI))))
                                   (SETQ H10 (COND (H8 (MKID U H8)) (T U)))
                                   (COND
                                    (CONTRACE
                                     (PROGN
                                      (PROGN (PRIN2 "h10=") (PRIN2 H10) NIL)
                                      (TERPRI))))
                                   (SETQ H10 (LIST 'TIMES H6 H10))
                                   (COND (NEGA (SETQ H10 (LIST 'MINUS H10))))
                                   (COND
                                    (CONTRACE
                                     (PROGN
                                      (ASSGNPRI (AEVAL* ">>>> h10=") NIL
                                                'FIRST)
                                      (ASSGNPRI (AEVAL* H10) NIL 'LAST))))
                                   (SETQ PII (CONS H10 PII))
                                   (SETQ NEGA (NOT NEGA))
                                   NIL)
                                  (GO WHILELABEL)))
                               (SETQ E1 (PLUS2 E1 1))
                               (GO LAB))
                             (SETQ IL (NEWIL IL MO NX))
                             NIL)
                            (COND ((NOT (NULL IL)) (GO REPEATLABEL))))
                          (SETQ PII
                                  (REVAL1
                                   (COND ((NULL PII) 0)
                                         ((EQUAL (LENGTH PII) 1) (CAR PII))
                                         (T (CONS 'PLUS PII)))
                                   T))
                          (COND
                           (CONTRACE
                            (PROGN
                             (ASSGNPRI (AEVAL* "pii-end=") NIL 'FIRST)
                             (ASSGNPRI (AEVAL* PII) NIL 'LAST))))
                          PII)
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST PL)))) 
(FLAG '(INTCURRENT2) 'OPFN) 
(PUT 'INTCURRENT2 'NUMBER-OF-ARGS 3) 
(PUT 'INTCURRENT2 'DEFINED-ON-LINE '3157) 
(PUT 'INTCURRENT2 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTCURRENT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTCURRENT2 (DIVP ULIST XLIST)
    (PROG (H2 H3 H4 H5 H6 H7 H8 E2 E3 LIN_PROBLEM_BAK)
      (SETQ LIN_PROBLEM_BAK LIN_PROBLEM)
      (SETQ LIN_PROBLEM T)
      (SETQ ULIST (CDR (REVAL1 ULIST T)))
      (SETQ XLIST (CDR (REVAL1 XLIST T)))
      (SETQ H4 (LIST XLIST))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ E3 DIVP)
         (SETQ H3 (CAR H4))
         (SETQ H4 (CDR H4))
         (SETQ H5
                 (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E2 1)
                   (COND ((MINUSP (DIFFERENCE (LENGTH H3) E2)) (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                  LOOPLABEL
                   (SETQ E2 (PLUS2 E2 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH H3) E2))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ H8 0)
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ H8 (PLUS H8 1))
            (SETQ H2 H3)
            (SETQ H6 NIL)
            (SETQ H7 NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT (NEQ H2 NIL)) (RETURN NIL)))
              (PROGN
               (SETQ E2 (INTPDE E3 ULIST H3 (CAR H2) T))
               (COND ((NULL E2) (SETQ E2 (LIST NIL E3)))
                     (T (SETQ E3 (CADR E2))))
               (COND ((AND (CAR E2) (NOT (ZEROP (CAR E2)))) (SETQ H7 T)))
               (SETQ H6 (CONS (LIST 'PLUS (CAR E2) (CAR H5)) H6))
               (SETQ H5 (CDR H5))
               (SETQ H2 (CDR H2)))
              (GO WHILELABEL))
            (SETQ H5 (REVERSE H6))
            NIL)
           (COND
            ((NOT (OR (EQUAL H7 NIL) (EQUAL E3 0) (EQUAL H8 10)))
             (GO REPEATLABEL))))
         NIL)
        (COND ((NOT (OR (EQUAL E3 0) (EQUAL H4 NIL))) (GO REPEATLABEL))))
      (SETQ LIN_PROBLEM LIN_PROBLEM_BAK)
      (RETURN (LIST 'LIST (REVAL1 (CONS 'LIST H5) T) E3)))) 
(FLAG '(INTCURRENT3) 'OPFN) 
(PUT 'INTCURRENT3 'NUMBER-OF-ARGS 3) 
(PUT 'INTCURRENT3 'DEFINED-ON-LINE '3212) 
(PUT 'INTCURRENT3 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'INTCURRENT3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTCURRENT3 (DIVP ULIST XLIST)
    (PROG (XL H1 H2 H3 H4 H5 RESU1 RESU2 RESU3 SUCC LIN_PROBLEM_BAK)
      (SETQ LIN_PROBLEM_BAK LIN_PROBLEM)
      (SETQ LIN_PROBLEM T)
      (SETQ ULIST (CDR (REVAL1 ULIST T)))
      (SETQ XLIST (CDR (REVAL1 XLIST T)))
      (SETQ XL XLIST)
      (SETQ RESU1 NIL)
      (SETQ SUCC NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (CDR XL) (NOT SUCC))) (RETURN NIL)))
        (PROGN
         (SETQ H1 (INTPDE DIVP ULIST XLIST (CAR XL) T))
         (COND
          ((AND H1 (NOT (ZEROP (CAR H1))))
           (PROGN
            (SETQ RESU2 (CONS (CAR H1) RESU1))
            (SETQ H2 (CDR XL))
            (PROG ()
             REPEATLABEL
              (PROGN
               (SETQ H3 (INTPDE (CADR H1) ULIST XLIST (CAR H2) NIL))
               (COND
                ((AND H3 (ZEROP (CADR H3)))
                 (PROGN
                  (SETQ H4 (CONS (CAR H3) RESU2))
                  (PROG (H5)
                    (SETQ H5 (CDR H2))
                   LAB
                    (COND ((NULL H5) (RETURN NIL)))
                    ((LAMBDA (H5) (SETQ H4 (CONS 0 H4))) (CAR H5))
                    (SETQ H5 (CDR H5))
                    (GO LAB))
                  (SETQ SUCC T)
                  (SETQ RESU3 (LIST 'LIST (CONS 'LIST (REVERSE H4)) 0)))))
               (SETQ H2 (CDR H2))
               (SETQ RESU2 (CONS 0 RESU2)))
              (COND ((NOT (OR SUCC (NULL H2))) (GO REPEATLABEL))))
            NIL)))
         (SETQ RESU1 (CONS 0 RESU1))
         (SETQ XL (CDR XL)))
        (GO WHILELABEL))
      (SETQ LIN_PROBLEM LIN_PROBLEM_BAK)
      (RETURN
       (COND (SUCC RESU3) (T (LIST 'LIST (CONS 'LIST (CONS 0 RESU1)) DIVP)))))) 
(ENDMODULE) 
(MODULE (LIST 'QUASILINPDE_INTEGRATION)) 
(PUT 'SELECT_INDEP_VAR 'NUMBER-OF-ARGS 2) 
(FLAG '(SELECT_INDEP_VAR) 'OPFN) 
(PUT 'SELECT_INDEP_VAR 'DEFINED-ON-LINE '3263) 
(PUT 'SELECT_INDEP_VAR 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SELECT_INDEP_VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SELECT_INDEP_VAR (CLIST XLIST)
    (PROG (S SCAL NOTFOUND CQ X XX XANZ SANZ XCOP OK)
      (SETQ NOTFOUND (AEVAL 'T))
      (SETQ XCOP (AEVAL XLIST))
      (WHILE
       (AND (BOOLVALUE* NOTFOUND)
            (EVALNEQ (AEVAL* XCOP) (AEVAL* (LIST 'LIST))))
       (PROGN
        (SETQ X (AEVAL* (LIST 'FIRST XCOP)))
        (SETQ XCOP (AEVAL* (LIST 'REST XCOP)))
        (SETQ CQ (AEVAL* (LIST 'FIRST CLIST)))
        (SETQ CLIST (AEVAL* (LIST 'REST CLIST)))
        (COND
         ((EVALNEQ (AEVAL* CQ) 0)
          (PROGN
           (SETQ XANZ (AEVAL* 0))
           (PROG (XX)
             (SETQ XX (GETRLIST (AEVAL* XLIST)))
            LAB
             (COND ((NULL XX) (RETURN NIL)))
             ((LAMBDA (XX)
                (COND
                 ((NOT (FREEOF (REVALX CQ) (REVALX XX)))
                  (SETQ XANZ (AEVAL* (LIST 'PLUS XANZ 1))))))
              (CAR XX))
             (SETQ XX (CDR XX))
             (GO LAB))
           (SETQ OK (AEVAL* 'NIL))
           (COND ((NOT (BOOLVALUE* S)) (SETQ OK (AEVAL* 'T)))
                 ((EVALLESSP (AEVAL* XANZ) (AEVAL* SANZ))
                  (SETQ OK (AEVAL* 'T)))
                 ((EVALEQUAL (AEVAL* XANZ) (AEVAL* SANZ))
                  (COND
                   ((EVALEQUAL (AEVAL* (LIST 'LENGTH CQ)) 1)
                    (SETQ OK (AEVAL* 'T)))
                   ((EVALGREATERP (AEVAL* (LIST 'LENGTH SCAL)) 1)
                    (COND
                     ((EVALEQUAL (AEVAL* (LIST 'PART SCAL 0)) (AEVAL* 'PLUS))
                      (COND
                       ((OR (EVALNEQ (AEVAL* (LIST 'PART CQ 0)) (AEVAL* 'PLUS))
                            (EVALLESSP (AEVAL* (LIST 'LENGTH CQ))
                                       (AEVAL* (LIST 'LENGTH SCAL))))
                        (SETQ OK (AEVAL* 'T))))))))))
           (COND
            ((BOOLVALUE* OK)
             (PROGN
              (SETQ S (AEVAL* X))
              (SETQ SCAL (AEVAL* CQ))
              (SETQ SANZ (AEVAL* XANZ)))))
           (COND
            ((EVALEQUAL (AEVAL* SCAL) 1) (SETQ NOTFOUND (AEVAL* 'NIL)))))))))
      (RETURN (AEVAL (LIST 'LIST S SCAL))))) 
(PUT 'CHARSYSCRACK 'NUMBER-OF-ARGS 6) 
(FLAG '(CHARSYSCRACK) 'OPFN) 
(PUT 'CHARSYSCRACK 'DEFINED-ON-LINE '3297) 
(PUT 'CHARSYSCRACK 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'CHARSYSCRACK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHARSYSCRACK (XLIST CQLIST S SCAL U ODE)
    (PROG (LCOPY X CS FLIST SOLN PRINTOLD TIMEOLD FACINTOLD ADJUSTOLD
           SAFEINTOLD FREEINTOLD ODESOLVEOLD E1 E2 E3 E4 N NMAX DFF PROCLISTOLD
           FLIN_OLD LIN_PROBLEM_OLD SESSION_OLD LEVEL_OLD)
      (SETQ LCOPY (AEVAL CQLIST))
      (SETQ CS (AEVAL (LIST 'LIST)))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETK 'CQ (AEVAL (LIST 'FIRST LCOPY)))
            (SETQ LCOPY (AEVAL (LIST 'REST LCOPY)))
            (COND
             ((EVALNEQ (AEVAL X) (AEVAL S))
              (PROGN
               (AEVAL (DEPEND (LIST X S)))
               (SETQ CS
                       (AEVAL
                        (LIST 'CONS
                              (LIST 'DIFFERENCE
                                    (LIST 'TIMES SCAL (LIST 'DF X S)) 'CQ)
                              CS))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       ((EVALNEQ (AEVAL S) (AEVAL U))
        (PROGN
         (SETQ FLIST (AEVAL (LIST 'LIST U)))
         (AEVAL (DEPEND (LIST U S)))
         (SETQ CS
                 (AEVAL
                  (LIST 'CONS
                        (LIST 'PLUS (LIST 'TIMES SCAL (LIST 'DF U S)) ODE)
                        CS)))))
       (T (SETQ FLIST (AEVAL (LIST 'LIST)))))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((EVALNEQ (AEVAL S) (AEVAL X))
             (SETQ FLIST (AEVAL (LIST 'CONS X FLIST))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROGN
       (SETQ TIMEOLD TIME_)
       (SETQ TIME_ NIL)
       (SETQ FACINTOLD FACINT_)
       (SETQ FACINT_ 1000)
       (SETQ ADJUSTOLD ADJUST_FNC)
       (SETQ ADJUST_FNC T)
       (SETQ SAFEINTOLD SAFEINT_)
       (SETQ SAFEINT_ NIL)
       (SETQ FREEINTOLD FREEINT_)
       (SETQ FREEINT_ NIL)
       (SETQ ODESOLVEOLD ODESOLVE_)
       (SETQ ODESOLVE_ 50)
       (SETQ PROCLISTOLD PROC_LIST_)
       (SETQ PROC_LIST_
               (DELETE 'STOP_BATCH (DELETE 'ALG_SOLVE_SINGLE PROC_LIST_)))
       (SETQ FLIN_OLD FLIN_)
       (SETQ LIN_PROBLEM_OLD LIN_PROBLEM)
       (SETQ SESSION_OLD SESSION_)
       (SETQ LEVEL_ LEVEL_OLD)
       NIL)
      (COND
       ((BOOLVALUE* (REVALX (NEQ PRINT_ NIL)))
        (PROGN
         (PROGN (PRIN2 "The equivalent characteristic system:") NIL)
         (TERPRI)
         (DEPRINT (CDR (AEVAL CS)))
         (PROGN (PRIN2 "for the functions: ") NIL)
         (FCTPRINT (CDR (REVAL1 (AEVAL FLIST) T)))
         (PROGN (PRIN2 ".") NIL)
         (PROGN
          (PRIN2
           "A new CRACK computation will start now to solve a characteristic ODE system.")
          NIL)
         (TERPRI)
         NIL)))
      (SETQ SOLN (AEVAL (LIST 'CRACK CS FLIST FLIST (LIST 'LIST))))
      (SETQ LCOPY (AEVAL (LIST 'LIST)))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL SOLN)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ E1 (AEVAL (LIST 'FIRST X)))
            (COND
             ((AND (EVALNEQ (AEVAL E1) (AEVAL (LIST 'LIST)))
                   (EVALEQUAL
                    (AEVAL
                     (LIST 'PLUS (LIST 'LENGTH E1)
                           (LIST 'LENGTH (LIST 'SECOND X))))
                    (AEVAL (LIST 'LENGTH FLIST))))
              (PROGN
               (SETQ E2 (AEVAL (LIST 'LENGTH (LIST 'THIRD X))))
               (PROG (E3)
                 (SETQ E3 (GETRLIST (AEVAL FLIST)))
                LAB
                 (COND ((NULL E3) (RETURN NIL)))
                 ((LAMBDA (E3)
                    (COND
                     ((NOT (FREEOF (REVALX (LIST 'THIRD X)) (REVALX E3)))
                      (SETQ E2 (AEVAL (LIST 'DIFFERENCE E2 1))))))
                  (CAR E3))
                 (SETQ E3 (CDR E3))
                 (GO LAB))
               (COND
                ((EVALEQUAL (AEVAL E2) (AEVAL (LIST 'LENGTH CS)))
                 (PROGN
                  (SETQ E2 (AEVAL (LIST 'LIST)))
                  (SETQ E3 (AEVAL (LIST 'LIST)))
                  (PROG (E4)
                    (SETQ E4 (GETRLIST (AEVAL (LIST 'THIRD X))))
                   LAB
                    (COND ((NULL E4) (RETURN NIL)))
                    ((LAMBDA (E4)
                       (COND
                        ((FREEOF (REVALX FLIST) (REVALX E4))
                         (SETQ E3 (AEVAL (LIST 'CONS E4 E3))))
                        (T (SETQ E2 (AEVAL (LIST 'CONS E4 E2))))))
                     (CAR E4))
                    (SETQ E4 (CDR E4))
                    (GO LAB))
                  (COND
                   ((EVALEQUAL (AEVAL (LIST 'LENGTH CS))
                               (AEVAL (LIST 'LENGTH E3)))
                    (PROGN
                     (PROG (E4)
                       (SETQ E4 (GETRLIST (AEVAL E2)))
                      LAB
                       (COND ((NULL E4) (RETURN NIL)))
                       ((LAMBDA (E4)
                          (PROG (E5)
                            (SETQ E5 (GETRLIST (AEVAL E1)))
                           LAB
                            (COND ((NULL E5) (RETURN NIL)))
                            ((LAMBDA (E5)
                               (COND
                                ((BOOLVALUE*
                                  (REVALX
                                   (NOT
                                    (FREEOF
                                     (LDERIV (REVAL1 (REVALX E5) T)
                                      (REVAL1 (REVALX E4) T)
                                      (LIST (REVAL1 (REVALX S) T)))
                                     'DF))))
                                 (PROGN
                                  (SETQ E2 (AEVAL (LIST 'LIST)))
                                  (SETQ E1 (AEVAL (LIST 'LIST)))))))
                             (CAR E5))
                            (SETQ E5 (CDR E5))
                            (GO LAB)))
                        (CAR E4))
                       (SETQ E4 (CDR E4))
                       (GO LAB))
                     (COND
                      ((EVALNEQ (AEVAL E2) (AEVAL (LIST 'LIST)))
                       (PROGN
                        (SETQ NMAX (AEVAL 0))
                        (PROG (E4)
                          (SETQ E4 (GETRLIST (AEVAL E2)))
                         LAB
                          (COND ((NULL E4) (RETURN NIL)))
                          ((LAMBDA (E4)
                             (PROGN
                              (PROG (E5)
                                (SETQ E5 (GETRLIST (AEVAL (LIST 'SECOND X))))
                               LAB
                                (COND ((NULL E5) (RETURN NIL)))
                                ((LAMBDA (E5)
                                   (PROGN
                                    (PROGN
                                     (SETQ N
                                             (LDERIV
                                              (REVAL1 (AEVAL (LIST 'RHS E5)) T)
                                              (REVAL1 (AEVAL E4) T)
                                              (LIST (REVAL1 (AEVAL S) T))))
                                     (SETQ N
                                             (COND
                                              ((OR (NOT (PAIRP (CAR N)))
                                                   (NEQ (CAAR N) 'DF))
                                               0)
                                              ((EQUAL (LENGTH (CAR N)) 3) 1)
                                              (T (CADDDR (CAR N))))))
                                    (SETQ N (AEVAL N))
                                    (COND
                                     ((EVALGREATERP (AEVAL N) (AEVAL NMAX))
                                      (SETQ NMAX (AEVAL N))))
                                    (AEVAL 'NIL)))
                                 (CAR E5))
                                (SETQ E5 (CDR E5))
                                (GO LAB))
                              (COND
                               ((EVALGREATERP (AEVAL NMAX) 0)
                                (PROGN
                                 (SETK 'E5 (AEVAL E1))
                                 (WHILE
                                  (FREEOF (REVALX (LIST 'FIRST 'E5))
                                          (REVALX E4))
                                  (SETK 'E5 (AEVAL* (LIST 'REST 'E5))))
                                 (SETK 'E5 (AEVAL (LIST 'FIRST 'E5)))
                                 (SETQ DFF (AEVAL E4))
                                 (PROG (N)
                                   (SETQ N 1)
                                  LAB
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE (AEVAL* NMAX) N))
                                     (RETURN NIL)))
                                   (PROGN
                                    (SETK 'E5 (AEVAL* (LIST 'DF 'E5 S)))
                                    (SETQ E1 (AEVAL* (LIST 'CONS 'E5 E1)))
                                    (SETQ DFF (AEVAL* (LIST 'DF DFF S)))
                                    (SETQ E3 (AEVAL* (LIST 'CONS DFF E3))))
                                   (SETQ N
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            N))
                                   (GO LAB)))))))
                           (CAR E4))
                          (SETQ E4 (CDR E4))
                          (GO LAB))
                        (SETQ LCOPY
                                (AEVAL
                                 (LIST 'CONS
                                       (LIST 'LIST
                                             (LIST 'APPEND (LIST 'SECOND X) E1)
                                             E3)
                                       LCOPY)))
                        (AEVAL 'NIL))))))))))))
             ((AND (EVALEQUAL (AEVAL (LIST 'FIRST X)) (AEVAL (LIST 'LIST)))
                   (EVALEQUAL (AEVAL (LIST 'LENGTH CS))
                              (AEVAL (LIST 'LENGTH (LIST 'THIRD X)))))
              (SETQ LCOPY
                      (AEVAL
                       (LIST 'CONS
                             (LIST 'LIST (LIST 'SECOND X) (LIST 'THIRD X))
                             LCOPY)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROGN
       (SETQ TIME_ TIMEOLD)
       (SETQ FACINT_ FACINTOLD)
       (SETQ ADJUST_FNC ADJUSTOLD)
       (SETQ SAFEINT_ SAFEINTOLD)
       (SETQ FREEINT_ FREEINTOLD)
       (SETQ ODESOLVE_ ODESOLVEOLD)
       (SETQ PROC_LIST_ PROCLISTOLD)
       (SETQ FLIN_ FLIN_OLD)
       (SETQ LIN_PROBLEM LIN_PROBLEM_OLD)
       (SETQ SESSION_ SESSION_OLD)
       (SETQ LEVEL_ LEVEL_OLD)
       NIL)
      (RETURN
       (COND
        ((EVALEQUAL (AEVAL LCOPY) (AEVAL (LIST 'LIST)))
         (PROGN
          (PROG (X)
            (SETQ X (GETRLIST (AEVAL FLIST)))
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (COND
                ((NOT (BOOLVALUE* (REVALX (LIST 'MY_FREEOF X S))))
                 (AEVAL (NODEPEND (LIST X S))))))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (AEVAL (LIST 'LIST))))
        (T (AEVAL (LIST 'CONS S LCOPY))))))) 
(PUT 'STOREDEPEND 'NUMBER-OF-ARGS 1) 
(FLAG '(STOREDEPEND) 'OPFN) 
(PUT 'STOREDEPEND 'DEFINED-ON-LINE '3448) 
(PUT 'STOREDEPEND 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'STOREDEPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STOREDEPEND (XLIST)
    (PROG (Q E1 E2)
      (RETURN
       (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
         (SETQ E1 (GETRLIST (AEVAL XLIST)))
         (COND ((NULL E1) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (E1)
                             (PROGN
                              (SETQ Q (AEVAL (LIST 'FARGS E1)))
                              (PROG (E2)
                                (SETQ E2 (GETRLIST (AEVAL Q)))
                               LAB
                                (COND ((NULL E2) (RETURN NIL)))
                                ((LAMBDA (E2) (AEVAL (NODEPEND (LIST E1 E2))))
                                 (CAR E2))
                                (SETQ E2 (CDR E2))
                                (GO LAB))
                              (AEVAL (LIST 'CONS E1 Q))))
                           (CAR E1))
                          NIL)))
        LOOPLABEL
         (SETQ E1 (CDR E1))
         (COND ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (E1)
                     (PROGN
                      (SETQ Q (AEVAL (LIST 'FARGS E1)))
                      (PROG (E2)
                        (SETQ E2 (GETRLIST (AEVAL Q)))
                       LAB
                        (COND ((NULL E2) (RETURN NIL)))
                        ((LAMBDA (E2) (AEVAL (NODEPEND (LIST E1 E2))))
                         (CAR E2))
                        (SETQ E2 (CDR E2))
                        (GO LAB))
                      (AEVAL (LIST 'CONS E1 Q))))
                   (CAR E1))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'RESTOREDEPEND 'NUMBER-OF-ARGS 1) 
(FLAG '(RESTOREDEPEND) 'OPFN) 
(PUT 'RESTOREDEPEND 'DEFINED-ON-LINE '3461) 
(PUT 'RESTOREDEPEND 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'RESTOREDEPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESTOREDEPEND (PREV_DEPEND)
    (PROG (Q S X)
      (PROG (S)
        (SETQ S (GETRLIST (AEVAL PREV_DEPEND)))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ Q (AEVAL (LIST 'FIRST S)))
            (SETQ S (AEVAL (LIST 'REST S)))
            (PROG (X)
              (SETQ X (GETRLIST (AEVAL S)))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (AEVAL (DEPEND (LIST Q X)))) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB)))) 
(PUT 'DROPABLE 'NUMBER-OF-ARGS 3) 
(PUT 'DROPABLE 'DEFINED-ON-LINE '3471) 
(PUT 'DROPABLE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DROPABLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROPABLE (H FL ALLOWED_TO_DROP)
    (COND ((OR (NOT (SMEMBERL FL H)) (MEMBER H ALLOWED_TO_DROP)) T) (T NIL))) 
(PUT 'DROP_FACTORS 'NUMBER-OF-ARGS 3) 
(PUT 'DROP_FACTORS 'DEFINED-ON-LINE '3477) 
(PUT 'DROP_FACTORS 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'DROP_FACTORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROP_FACTORS (Q FL ALLOWED_TO_DROP)
    (COND
     ((OR (NOT (PAIRP Q)) (NEQ (CAR Q) 'TIMES))
      (COND ((DROPABLE Q FL ALLOWED_TO_DROP) 1) (T Q)))
     (T
      (REVAL1
       (CONS 'TIMES
             (PROG (H FORALL-RESULT FORALL-ENDPTR)
               (SETQ H (CDR Q))
               (COND ((NULL H) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (H)
                                   (COND ((DROPABLE H FL ALLOWED_TO_DROP) 1)
                                         (T H)))
                                 (CAR H))
                                NIL)))
              LOOPLABEL
               (SETQ H (CDR H))
               (COND ((NULL H) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (H)
                           (COND ((DROPABLE H FL ALLOWED_TO_DROP) 1) (T H)))
                         (CAR H))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
       T)))) 
(PUT 'SIMPLIFIZIERE 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFIZIERE 'DEFINED-ON-LINE '3486) 
(PUT 'SIMPLIFIZIERE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'SIMPLIFIZIERE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFIZIERE (Q FL ALLOWED_TO_DROP)
    (PROG (N NU DE)
      (RETURN
       (COND ((NOT (PAIRP Q)) Q)
             ((OR (MEMBER (CAR Q) ONE_ARGUMENT_FUNCTIONS_)
                  (AND (MEMBER (CAR Q) (LIST 'EXPT))
                       (DROPABLE (CADDR Q) FL ALLOWED_TO_DROP)))
              (SIMPLIFIZIERE (CADR Q) FL ALLOWED_TO_DROP))
             ((EQUAL (CAR Q) 'QUOTIENT)
              (PROGN
               (SETQ NU (DROP_FACTORS (CADR Q) FL ALLOWED_TO_DROP))
               (SETQ DE (DROP_FACTORS (CADDR Q) FL ALLOWED_TO_DROP))
               (COND ((EQUAL NU 1) (SIMPLIFIZIERE DE FL ALLOWED_TO_DROP))
                     ((EQUAL DE 1) (SIMPLIFIZIERE NU FL ALLOWED_TO_DROP))
                     (T (LIST 'QUOTIENT NU DE)))))
             ((EQUAL (CAR Q) 'TIMES)
              (PROGN
               (SETQ DE (DROP_FACTORS Q FL ALLOWED_TO_DROP))
               (COND
                ((OR (NOT (PAIRP DE)) (LESSP (LENGTH DE) (LENGTH Q)))
                 (SIMPLIFIZIERE DE FL ALLOWED_TO_DROP))
                (T DE))))
             (T
              (PROGN
               (SETQ Q (SIGNCHANGE Q))
               (SETQ N (NCONTENT Q))
               (COND ((EQUAL N 1) Q)
                     (T
                      (SIMPLIFIZIERE (REVAL1 (LIST 'QUOTIENT Q (REVAL1 N T)) T)
                       FL ALLOWED_TO_DROP))))))))) 
(PUT 'QUASILINPDE 'NUMBER-OF-ARGS 3) 
(FLAG '(QUASILINPDE) 'OPFN) 
(PUT 'QUASILINPDE 'DEFINED-ON-LINE '3517) 
(PUT 'QUASILINPDE 'DEFINED-IN-FILE 'CRACK/CRINT.RED) 
(PUT 'QUASILINPDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUASILINPDE (F U XLIST)
    (PROG (I Q QLIST CQ CQLIST ODE SOLN TRAN XCOP S S1 X XX H1 H2 SCAL QLIN
           PREV_DEPEND XLIST_CP1 XLIST_CP2)
      (PUT 'FF 'SIMPFN 'SIMPIDEN)
      (COND
       ((BOOLVALUE* (REVALX PRINT_MORE))
        (PROGN
         (ASSGNPRI (AEVAL "The quasilinear PDE:  0 = ") NIL 'FIRST)
         (ASSGNPRI (AEVAL F) NIL NIL)
         (ASSGNPRI (AEVAL ".") NIL 'LAST))))
      (SETQ I (AEVAL 0))
      (SETQ ODE (AEVAL F))
      (SETQ QLIST (AEVAL (LIST 'LIST)))
      (SETQ CQLIST (AEVAL (LIST 'LIST)))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ I (AEVAL (LIST 'PLUS I 1)))
            (SETQ Q (AEVAL (LIST 'MKID 'P_ I)))
            (SETQ QLIST (AEVAL (LIST 'CONS Q QLIST)))
            (SETQ F (AEVAL (LIST 'SUB (LIST 'EQUAL (LIST 'DF U X) Q) F)))
            (SETQ CQ (AEVAL (LIST 'DF F Q)))
            (SETQ CQLIST (AEVAL (LIST 'CONS (LIST 'DF F Q) CQLIST)))
            (SETQ ODE
                    (AEVAL
                     (LIST 'DIFFERENCE ODE
                           (LIST 'TIMES (LIST 'DF U X) (LIST 'DF F Q)))))
            (AEVAL 'NIL)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ DEPL* (DELETE (ASSOC U DEPL*) DEPL*))
      (SETQ DEPL* (DELETE (ASSOC (MKID U '_) DEPL*) DEPL*))
      (SETQ QLIST (AEVAL (LIST 'REVERSE QLIST)))
      (SETQ CQLIST (AEVAL (LIST 'REVERSE CQLIST)))
      (SETQ QLIN (AEVAL 'T))
      (PROG (CQ)
        (SETQ CQ (GETRLIST (AEVAL CQLIST)))
       LAB
        (COND ((NULL CQ) (RETURN NIL)))
        ((LAMBDA (CQ)
           (PROG (Q)
             (SETQ Q (GETRLIST (AEVAL QLIST)))
            LAB
             (COND ((NULL Q) (RETURN NIL)))
             ((LAMBDA (Q)
                (COND
                 ((EVALNEQ (AEVAL (LIST 'DF CQ Q)) 0)
                  (SETQ QLIN (AEVAL 'NIL)))))
              (CAR Q))
             (SETQ Q (CDR Q))
             (GO LAB)))
         (CAR CQ))
        (SETQ CQ (CDR CQ))
        (GO LAB))
      (COND ((NOT (BOOLVALUE* QLIN)) (RETURN (AEVAL (LIST 'LIST)))))
      (SETK 'PCOPY (AEVAL (LIST 'CONS (LIST 'MINUS ODE) CQLIST)))
      (SETQ XCOP (AEVAL (LIST 'CONS U XLIST)))
      (SETQ SCAL (AEVAL (LIST 'SELECT_INDEP_VAR 'PCOPY XCOP)))
      (SETQ S1 (AEVAL (LIST 'FIRST SCAL)))
      (SETQ PREV_DEPEND (AEVAL (LIST 'STOREDEPEND XLIST)))
      (SETQ SOLN
              (AEVAL
               (LIST 'CHARSYSCRACK XLIST CQLIST S1 (LIST 'SECOND SCAL) U ODE)))
      (COND
       ((EVALEQUAL (AEVAL SOLN) (AEVAL (LIST 'LIST)))
        (PROGN
         (REPEAT
          (PROGN
           (REPEAT
            (PROGN
             (SETQ S (AEVAL* (LIST 'FIRST XCOP)))
             (SETQ XCOP (AEVAL* (LIST 'REST XCOP)))
             (SETQ SCAL (AEVAL* (LIST 'FIRST 'PCOPY)))
             (SETK 'PCOPY (AEVAL* (LIST 'REST 'PCOPY))))
            (OR (EVALEQUAL (AEVAL* 'PCOPY) (AEVAL* (LIST 'LIST)))
                (AND (EVALNEQ (AEVAL* SCAL) 0)
                     (EVALNEQ (AEVAL* S) (AEVAL* S1)))))
           (COND
            ((AND (EVALNEQ (AEVAL* S) (AEVAL* S1)) (EVALNEQ (AEVAL* SCAL) 0))
             (PROGN
              (COND
               ((BOOLVALUE* (REVALX PRINT_))
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "New attempt with a different independent variable:")
                  NIL)
                 (TERPRI))))
              (SETQ SOLN
                      (AEVAL*
                       (LIST 'CHARSYSCRACK XLIST CQLIST S SCAL U ODE)))))))
          (OR (EVALNEQ (AEVAL* SOLN) (AEVAL* (LIST 'LIST)))
              (EVALEQUAL (AEVAL* XCOP) (AEVAL* (LIST 'LIST))))))))
      (SETQ TRAN (AEVAL (LIST 'LIST)))
      (COND
       ((EVALNEQ (AEVAL SOLN) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ S1 (AEVAL (LIST 'FIRST SOLN)))
         (PROG (S)
           (SETQ S (GETRLIST (AEVAL (LIST 'REST SOLN))))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (PROGN
               (SETQ CQ
                       (AEVAL
                        (LIST 'FIRST
                              (LIST 'SOLVE (LIST 'FIRST S) (LIST 'SECOND S)))))
               (SETQ X (AEVAL (LIST 'LIST)))
               (PROG (XX)
                 (SETQ XX (GETRLIST (AEVAL CQ)))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (COND
                     ((BOOLVALUE* (REVALX (ATOM (REVALX (LIST 'LHS XX)))))
                      (SETQ X
                              (AEVAL
                               (LIST 'CONS
                                     (PROGN
                                      (SETQ Q
                                              (REVAL1 (AEVAL (LIST 'RHS XX))
                                                      T))
                                      (SIMPLIFIZIERE Q
                                       (CONS (REVAL1 U T) (CDR XLIST)) NIL))
                                     X))))))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               (PROGN
                (SETQ X (CDR X))
                (PROG ()
                 REPEATLABEL
                  (PROGN
                   (SETQ XX X)
                   (SETQ X
                           (PROG (H1 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ H1 X)
                             (COND ((NULL H1) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (H1)
                                                 (SIMPLIFIZIERE H1
                                                  (CONS (REVAL1 U T)
                                                        (CDR XLIST))
                                                  (DELETE H1 X)))
                                               (CAR H1))
                                              NIL)))
                            LOOPLABEL
                             (SETQ H1 (CDR H1))
                             (COND ((NULL H1) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (H1)
                                         (SIMPLIFIZIERE H1
                                          (CONS (REVAL1 U T) (CDR XLIST))
                                          (DELETE H1 X)))
                                       (CAR H1))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                  (COND ((NOT (EQUAL X XX)) (GO REPEATLABEL))))
                (SETQ X (CONS 'LIST X))
                NIL)
               (SETQ XX (AEVAL TRAN))
               (WHILE
                (AND (EVALNEQ (AEVAL* XX) (AEVAL* (LIST 'LIST)))
                     (BOOLVALUE*
                      (PROGN
                       (SETQ H1 (REVALX X))
                       (SETQ H2 (REVALX (LIST 'FIRST XX)))
                       (WHILE
                        (AND (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                             (EVALNEQ (AEVAL* H2) (AEVAL* (LIST 'LIST)))
                             (EVALEQUAL (AEVAL* (LIST 'FIRST H1))
                                        (AEVAL* (LIST 'FIRST H2))))
                        (PROGN
                         (SETQ H1 (AEVAL* (LIST 'REST H1)))
                         (SETQ H2 (AEVAL* (LIST 'REST H2)))))
                       (COND
                        ((AND (EVALEQUAL (REVALX H1) (REVALX (LIST 'LIST)))
                              (EVALEQUAL (REVALX H2) (REVALX (LIST 'LIST))))
                         (REVALX 'NIL))
                        (T (REVALX 'T))))))
                (SETQ XX (AEVAL* (LIST 'REST XX))))
               (COND
                ((EVALEQUAL (AEVAL XX) (AEVAL (LIST 'LIST)))
                 (SETQ TRAN (AEVAL (LIST 'CONS X TRAN)))))
               (AEVAL 'NIL)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S (GETRLIST (AEVAL XLIST)))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((AND (EVALNEQ (AEVAL S) (AEVAL S1))
                     (NOT (BOOLVALUE* (REVALX (LIST 'MY_FREEOF S S1)))))
                (AEVAL (NODEPEND (LIST S S1))))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB)))))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (AEVAL (DEPEND (LIST U X)))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX PRINT_MORE))
        (COND
         ((EVALNEQ (AEVAL TRAN) (AEVAL (LIST 'LIST)))
          (PROGN
           (ASSGNPRI (AEVAL "The general solution of the PDE is given through")
                     NIL 'ONLY)
           (PROG (X)
             (SETQ X (GETRLIST (AEVAL TRAN)))
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (PROGN
                 (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
                 (ASSGNPRI (AEVAL (CONS 'FF (CDR (REVAL1 (AEVAL X) T)))) NIL
                           'LAST)))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (COND
            ((EVALGREATERP (AEVAL (LIST 'LENGTH TRAN)) 1)
             (ASSGNPRI (AEVAL "with arbitrary function(s) ff(..).") NIL 'ONLY))
            (T
             (ASSGNPRI (AEVAL "with arbitrary function ff(..).") NIL
                       'ONLY))))))))
      (AEVAL (LIST 'RESTOREDEPEND PREV_DEPEND))
      (RETURN (AEVAL TRAN)))) 
(ENDMODULE) 
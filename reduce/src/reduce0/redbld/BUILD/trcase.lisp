(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRCASE)) 
(FLUID
 '(*BACKTRACE *FAILHARD *NOWARNINGS *PURERISCH *REVERSE *TRINT BADPART CCOUNT
   CMAP CMATRIX CONTENT CVAL DENBAD DENOMINATOR* INDEXLIST LHS* LOGLIST LORDER
   ORDEROFELIM POWER-LIST* RHS* SILLIESLIST SQFR SQRTFLAG SQRTLIST TANLIST SVAR
   VARLIST XLOGS ZLIST)) 
(GLOBAL '(*NUMBER* *SEPLOGS *SPSIZE* *STATISTICS GENSYMCOUNT)) 
(SWITCH (LIST 'FAILHARD)) 
(EXPORTS (LIST 'TRANSCENDENTALCASE)) 
(IMPORTS
 (LIST 'BACKSUBST4CS 'COUNTZ 'CREATECMAP 'CREATEINDICES 'DF2Q 'DFNUMR 'DIFFLOGS
       'FSDF 'FACTORLISTLIST 'FINDSQRTS 'FINDTRIALDIVS 'GCDF 'MKVECT 'INTERR
       'LOGSTOSQ 'MERGIN 'MULTBYARBPOWERS '*MULTF 'PRINTDF 'PRINTSQ 'QUOTF
       'PUTV 'SIMPINT1 'SOLVE-FOR-U 'SQFREE 'SQMERGE 'SQRT2TOP 'SUBSTINULIST
       'TRIALDIV 'MERGEIN 'NEGSQ 'ADDSQ 'F2DF 'MKNILL 'PNTH 'INVSQ 'MULTSQ
       'DOMAINP 'MK*SQ 'MKSP 'PRETTYPRINT)) 
(PUT 'TRANSCENDENTALCASE 'NUMBER-OF-ARGS 5) 
(PUT 'TRANSCENDENTALCASE 'DEFINED-ON-LINE '87) 
(PUT 'TRANSCENDENTALCASE 'DEFINED-IN-FILE 'INT/TRCASE.RED) 
(PUT 'TRANSCENDENTALCASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANSCENDENTALCASE (INTEGRAND SVAR XLOGS ZLIST VARLIST)
    (PROG (DIVLIST JHD-CONTENT CONTENT PRIM SQFR DFU INDEXLIST SILLIESLIST
           ORIGINALORDER WRONGWAY POWER-LIST* SQRTLIST TANLIST LOGLIST DFLOGS
           EPRIM DFUN UNINTEGRAND SQRTFLAG BADPART RHS* LHS* GCDQ CMAP CVAL
           ORDEROFELIM CMATRIX CCOUNT DENOMINATOR* RESULT DENBAD TEMP)
      (SETQ GENSYMCOUNT 0)
      (SETQ INTEGRAND (SQRT2TOP INTEGRAND))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Extension variables z<i> are")
          (TERPRI)
          "Extension variables z<i> are")
         (PRINT ZLIST))))
      (PROG (W GG)
        (SETQ GG 1)
        (PROG (Z)
          (SETQ Z ZLIST)
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z)
             (PROGN
              (SETQ W (SUBS2 (DIFFSQ (SIMP Z) SVAR)))
              (SETQ GG (*MULTF GG (QUOTF-FAIL (CDR W) (GCDF (CDR W) GG))))))
           (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))
        (SETQ GG (QUOTF-FAIL GG (GCDF GG (CDR INTEGRAND))))
        (SETQ UNINTEGRAND
                (CONS (*MULTF GG (CAR INTEGRAND)) (*MULTF GG (CDR INTEGRAND))))
        (COND
         (*TRINT
          (PROGN
           (PROGN
            (PRIN2 "After unnormalization the integrand is ")
            (TERPRI)
            "After unnormalization the integrand is ")
           (PRINTSQ UNINTEGRAND)))))
      (SETQ DIVLIST (FINDTRIALDIVS ZLIST))
      (SETQ SQRTLIST (FINDSQRTS ZLIST))
      (SETQ DIVLIST (TRIALDIV (CDR UNINTEGRAND) DIVLIST))
      (SETQ PRIM (SQFREE (CDR DIVLIST) ZLIST))
      (SETQ JHD-CONTENT CONTENT)
      (PRINTFACTORS PRIM NIL)
      (SETQ EPRIM (SQMERGE (COUNTZ (CAR DIVLIST)) PRIM NIL))
      (PRINTFACTORS EPRIM T)
      (SETQ SQFR
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U EPRIM)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (MULTUP U)) (CAR U)) NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (MULTUP U)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*REVERSE (SETQ ZLIST (REVERSE ZLIST))))
      (SETQ INDEXLIST (CREATEINDICES ZLIST))
      (SETQ DFU (DFNUMR SVAR (CAR DIVLIST)))
      (SETQ LOGLIST (APPEND LOGLIST (FACTORLISTLIST PRIM)))
      (SETQ LOGLIST (MERGEIN XLOGS LOGLIST))
      (SETQ LOGLIST (MERGEIN TANLIST LOGLIST))
      (SETQ CMAP (CREATECMAP))
      (SETQ CCOUNT (LENGTH CMAP))
      (COND
       (*TRINT
        (PROGN
         (PROGN (PRIN2 "Loglist ") (TERPRI) "Loglist ")
         (PRINT LOGLIST))))
      (SETQ DFLOGS (DIFFLOGS LOGLIST (CDR UNINTEGRAND) SVAR))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "************ 'Derivative' of logs is:")
          (TERPRI)
          "************ 'Derivative' of logs is:")
         (PRINTSQ DFLOGS))))
      (SETQ DFLOGS (ADDSQ (CONS (CAR UNINTEGRAND) 1) (NEGSQ DFLOGS)))
      (SETQ GCDQ (GCDF (CDR DFLOGS) (CDR DFU)))
      (SETQ DFUN
              (*MULTF (CAR DFU) (SETQ DENBAD (QUOTF-FAIL (CDR DFLOGS) GCDQ))))
      (SETQ DENBAD (*MULTF (CDR DFU) DENBAD))
      (SETQ DENBAD (*MULTF (CDR UNINTEGRAND) DENBAD))
      (SETQ DFLOGS (*MULTF (CAR DFLOGS) (QUOTF-FAIL (CDR DFU) GCDQ)))
      (SETQ DFU DFUN)
      (SETQ RHS* (MULTBYARBPOWERS (F2DF DFU)))
      (COND
       ((CHECKDFFAIL RHS* SVAR)
        (PROGN
         (COND (*TRINT (PRINTSQ (CHECKDFFAIL RHS* SVAR))))
         (INTERR "Simplification fails on above expression"))))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Distributed Form of Numerator is:")
          (TERPRI)
          "Distributed Form of Numerator is:")
         (PRINTDF RHS*))))
      (SETQ LHS* (F2DF DFLOGS))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Distributed Form of integrand is:")
          (TERPRI)
          "Distributed Form of integrand is:")
         (PRINTDF LHS*)
         (TERPRI))))
      (SETQ CVAL (MKVECT CCOUNT))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE CCOUNT I)) (RETURN NIL)))
        (PUTV CVAL I (CONS NIL 1))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ POWER-LIST* (TANSFROM RHS* ZLIST INDEXLIST 0))
      (SETQ LORDER (MAXORDER POWER-LIST* ZLIST 0))
      (SETQ ORIGINALORDER
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X LORDER)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) X) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) X) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Maximum order for variables determined as ")
          (TERPRI)
          "Maximum order for variables determined as ")
         (PRINT LORDER))))
      (COND
       (*STATISTICS
        (PROGN
         (SETQ *NUMBER* 0)
         (SETQ *SPSIZE* 1)
         (PROG (XX)
           (SETQ XX LORDER)
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX) (SETQ *SPSIZE* (TIMES *SPSIZE* (PLUS XX 1))))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB)))))
      (SETQ DFUN (SOLVE-FOR-U RHS* LHS* NIL))
      (BACKSUBST4CS NIL ORDEROFELIM CMATRIX)
      (COND
       (*STATISTICS
        (PROGN
         (PRIN2 *NUMBER*)
         (PRIN2 " used out of ")
         ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) *SPSIZE*))))
      (SETQ BADPART (SUBSTINULIST BADPART))
      (SETQ DFUN (DF2Q (SUBSTINULIST DFUN)))
      (SETQ RESULT (*MULTSQ DFUN (*INVSQ (CONS DENOMINATOR* 1))))
      (SETQ RESULT (*MULTSQ RESULT (*INVSQ (CONS JHD-CONTENT 1))))
      (SETQ DFLOGS (LOGSTOSQ))
      (COND
       ((NOT (NULL (CAR DFLOGS)))
        (PROGN
         (COND
          ((AND *SEPLOGS
                (NOT (OR (ATOM (CAR RESULT)) (ATOM (CAR (CAR RESULT))))))
           (PROGN
            (SETQ RESULT (MK*SQ RESULT))
            (SETQ RESULT (CONS (CONS (GETPOWER (FKERN RESULT) 1) 1) NIL))
            (SETQ RESULT (CONS RESULT 1)))))
         (SETQ RESULT (SUBS2Q (ADDSQ RESULT DFLOGS))))))
      (COND
       (*TRINT
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "*****************************************************")
          (TERPRI)
          "*****************************************************")
         (PROGN
          (PRIN2 "************ THE INTEGRAL IS : **********************")
          (TERPRI)
          "************ THE INTEGRAL IS : **********************")
         (PROGN
          (PRIN2 "*****************************************************")
          (TERPRI)
          "*****************************************************")
         (TERPRI)
         (PRINTSQ RESULT)
         (TERPRI))))
      (COND
       (BADPART
        (PROG (N OORDER)
          (COND
           (*TRINT
            (PROGN
             (PRIN2 "plus a part which has not been integrated")
             (TERPRI)
             "plus a part which has not been integrated")))
          (SETQ LHS* BADPART)
          (SETQ LORDER (MAXORDER POWER-LIST* ZLIST 0))
          (SETQ OORDER ORIGINALORDER)
          (SETQ N (LENGTH LORDER))
          (PROG ()
           WHILELABEL
            (COND ((NOT LORDER) (RETURN NIL)))
            (PROGN
             (COND
              ((GREATERP (CAR LORDER) (CAR ORIGINALORDER)) (SETQ WRONGWAY T)))
             (COND
              ((EQUAL (CAR LORDER) (CAR ORIGINALORDER))
               (SETQ N (DIFFERENCE N 1))))
             (SETQ LORDER (CDR LORDER))
             (SETQ ORIGINALORDER (CDR ORIGINALORDER)))
            (GO WHILELABEL))
          (COND
           ((AND *TRINT WRONGWAY)
            (PROGN (PRIN2 "Went wrong way") (TERPRI) "Went wrong way")))
          (SETQ DFUN (DF2Q BADPART))
          (SETQ DFUN (*MULTSQ DFUN (INVSQ (CONS DENBAD 1))))
          (SETQ BADPART DFUN)
          (COND
           (WRONGWAY
            (PROGN
             (COND
              (*TRINT
               (PROGN (PRIN2 "Resetting....") (TERPRI) "Resetting....")))
             (SETQ RESULT (CONS NIL 1))
             (SETQ DFUN INTEGRAND)
             (SETQ BADPART DFUN))))
          (COND
           ((ROOTCHECKP UNINTEGRAND SVAR)
            (RETURN
             (CONS (SIMPINT1 (CONS INTEGRAND (CONS SVAR NIL))) (CONS NIL 1))))
           ((OR *PURERISCH (ALLOWEDFNS ZLIST))
            (PROGN (SETQ BADPART DFUN) (SETQ DFUN (CONS NIL 1))))
           (T
            (PROGN
             (SETQ *PURERISCH T)
             (COND
              (*TRINT
               (PROGN
                (PROGN
                 (PRIN2 "   Applying transformations ...")
                 (TERPRI)
                 "   Applying transformations ...")
                (PRINTSQ DFUN))))
             (SETQ TEMP (GET 'TAN 'OPMTCH))
             (REMPROP 'TAN 'OPMTCH)
             (SETQ DENBAD (TRANSFORM DFUN SVAR))
             (COND
              ((EQUAL DENBAD DFUN)
               (PROGN
                (SETQ DFUN (CONS NIL 1))
                (SETQ BADPART DENBAD)
                (PUT 'TAN 'OPMTCH TEMP)))
              (T
               (PROGN
                (SETQ DENBAD
                        (ERRORSET*
                         (LIST 'INTEGRATESQ (MKQUOTE DENBAD) (MKQUOTE SVAR)
                               (MKQUOTE XLOGS) NIL)
                         *BACKTRACE))
                (PUT 'TAN 'OPMTCH TEMP)
                (COND
                 ((NOT (ATOM DENBAD))
                  (PROGN
                   (SETQ DENBAD (CAR DENBAD))
                   (SETQ DFUN (UNTAN (CAR DENBAD)))
                   (COND
                    ((NEQ DFUN '(NIL . 1))
                     (SETQ BADPART (UNTAN (CDR DENBAD)))))
                   (COND
                    ((AND (CAR BADPART) (NOT (EQUAL BADPART DENBAD)))
                     (PROGN
                      (SETQ WRONGWAY NIL)
                      (SETQ LHS* (F2DF (CAR BADPART)))
                      (SETQ LORDER (MAXORDER POWER-LIST* ZLIST 0))
                      (SETQ N (LENGTH LORDER))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT LORDER) (RETURN NIL)))
                        (PROGN
                         (COND
                          ((GREATERP (CAR LORDER) (CAR OORDER))
                           (SETQ WRONGWAY T)))
                         (COND
                          ((EQUAL (CAR LORDER) (CAR OORDER))
                           (SETQ N (DIFFERENCE N 1))))
                         (SETQ LORDER (CDR LORDER))
                         (SETQ OORDER (CDR OORDER)))
                        (GO WHILELABEL))
                      (COND
                       ((OR WRONGWAY (EQUAL N 0))
                        (PROGN
                         (COND
                          (*TRINT
                           (PROGN
                            (PRIN2 "Still backwards")
                            (TERPRI)
                            "Still backwards")))
                         (SETQ RESULT (CONS NIL 1))
                         (SETQ DFUN (CONS NIL 1))
                         (SETQ BADPART INTEGRAND)))))))))
                 (T
                  (PROGN (SETQ BADPART DFUN) (SETQ DFUN (CONS NIL 1)))))))))))
          (COND (*FAILHARD (RERROR 'INT 9 "FAILHARD switch set")))
          (COND
           ((AND *SEPLOGS (NOT (OR (ATOM RESULT) (ATOM (CAR RESULT)))))
            (PROGN
             (SETQ RESULT (MK*SQ RESULT))
             (COND
              ((NOT (NUMBERP RESULT))
               (SETQ RESULT (CONS (CONS (GETPOWER (FKERN RESULT) 1) 1) NIL))))
             (SETQ RESULT (CONS RESULT 1)))))
          (SETQ RESULT (ADDSQ RESULT DFUN))))
       (T (SETQ BADPART (CONS NIL 1))))
      (RETURN (CONS (SQRT2TOP RESULT) BADPART)))) 
(PUT 'CHECKDFFAIL 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKDFFAIL 'DEFINED-ON-LINE '299) 
(PUT 'CHECKDFFAIL 'DEFINED-IN-FILE 'INT/TRCASE.RED) 
(PUT 'CHECKDFFAIL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKDFFAIL (U V)
    (COND ((NULL U) NIL) ((DEPENDS (CDAR U) V) (CDAR U))
          (T (CHECKDFFAIL (CDR U) V)))) 
(PUT 'PRINTFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'PRINTFACTORS 'DEFINED-ON-LINE '306) 
(PUT 'PRINTFACTORS 'DEFINED-IN-FILE 'INT/TRCASE.RED) 
(PUT 'PRINTFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINTFACTORS (W PRDENOM)
    (PROG (I WX)
      (SETQ I 1)
      (COND
       (PRDENOM
        (PROGN
         (SETQ DENOMINATOR* 1)
         (COND
          (*TRINT
           (PROGN
            (PRIN2 "Denominator of 1st part of answer is:")
            (TERPRI)
            "Denominator of 1st part of answer is:")))
         (COND ((NOT (NULL W)) (SETQ W (CDR W)))))))
     LOOPX
      (COND ((EQUAL W NIL) (RETURN NIL)))
      (COND
       (*TRINT
        (PROGN
         (PRIN2 "Factors of multiplicity ")
         (PRIN2 I)
         (PRIN2T ":")
         (TERPRI))))
      (SETQ WX (CAR W))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL WX))) (RETURN NIL)))
        (PROGN
         (COND (*TRINT (PRINTSF (CAR WX))))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE I J)) (RETURN NIL)))
           (SETQ DENOMINATOR* (*MULTF (CAR WX) DENOMINATOR*))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ WX (CDR WX)))
        (GO WHILELABEL))
      (SETQ I (PLUS I 1))
      (SETQ W (CDR W))
      (GO LOOPX))) 
(ENDMODULE) 
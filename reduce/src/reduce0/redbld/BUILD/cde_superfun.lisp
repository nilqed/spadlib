(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_SUPERFUN)) 
(PUT 'MK_SUPERFUN 'NUMBER-OF-ARGS 3) 
(PUT 'MK_SUPERFUN 'DEFINED-ON-LINE '34) 
(PUT 'MK_SUPERFUN 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'MK_SUPERFUN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_SUPERFUN (SUPERFUN NUM_ARG LEN_TARGET)
    (PROG ()
      (MKOP SUPERFUN)
      (PUT 'SFUN SUPERFUN T)
      (COND
       ((NOT (NUMBERP NUM_ARG)) (REDERR "Error in the number of arguments")))
      (PUT 'SFNARG SUPERFUN NUM_ARG)
      (COND
       ((NOT (FIXP NUM_ARG))
        (REDERR "Error: number of arguments must be an integer")))
      (COND
       ((NOT (FIXP LEN_TARGET))
        (REDERR "Error: the length of image vectors must be an integer")))
      (PUT 'SFTARGET SUPERFUN LEN_TARGET))) 
(FLAG '(MK_SUPERFUN) 'OPFN) 
(PUT 'SUPERFUNP 'NUMBER-OF-ARGS 1) 
(PUT 'SUPERFUNP 'DEFINED-ON-LINE '58) 
(PUT 'SUPERFUNP 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'SUPERFUNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUPERFUNP (SUPERFUN) (GET 'SFUN SUPERFUN)) 
(PUT 'CHECK_SUPERFUN_DEGONE 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_SUPERFUN_DEGONE 'DEFINED-ON-LINE '61) 
(PUT 'CHECK_SUPERFUN_DEGONE 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CHECK_SUPERFUN_DEGONE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_SUPERFUN_DEGONE (SUPERFUN)
    (PROG (NUM_ARG LEN_TARGET)
      (COND
       ((NOT (SUPERFUNP SUPERFUN))
        (REDERR "Error: the first argument must be a declared superfunction")))
      (COND
       ((NOT (EQN (SETQ NUM_ARG (GET 'SFNARG SUPERFUN)) 1))
        (REDERR "Error: the operator must have exactly one argument")))
      (COND
       ((NOT (FIXP (SETQ LEN_TARGET (GET 'SFTARGET SUPERFUN))))
        (REDERR "Error: the length of image vectors must be an integer"))))) 
(PUT 'CHECK_SUPERFUN_SCALAR 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_SUPERFUN_SCALAR 'DEFINED-ON-LINE '73) 
(PUT 'CHECK_SUPERFUN_SCALAR 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CHECK_SUPERFUN_SCALAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_SUPERFUN_SCALAR (SUPERFUN)
    (PROG (LEN_TARGET)
      (SETQ LEN_TARGET 0)
      (COND
       ((NOT (SUPERFUNP SUPERFUN))
        (REDERR "Error: the first argument must be a declared superfunction")))
      (COND
       ((NOT (FIXP (SETQ LEN_TARGET (GET 'SFTARGET SUPERFUN))))
        (REDERR
         "Error: the dimension of the target space must be an integer")))
      (COND
       ((NOT (EQN LEN_TARGET 1))
        (REDERR "Error: the dimension of the target space must be one"))))) 
(PUT 'CHECK_SUPERFUN_SAMETYPE 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_SUPERFUN_SAMETYPE 'DEFINED-ON-LINE '85) 
(PUT 'CHECK_SUPERFUN_SAMETYPE 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CHECK_SUPERFUN_SAMETYPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_SUPERFUN_SAMETYPE (SF1 SF2)
    (PROG (PARITY1 PARITY2 TARGET1 TARGET2)
      (COND
       ((NOT (SUPERFUNP SF1))
        (REDERR "The first argument must be a declared superfunction")))
      (COND
       ((NOT (SUPERFUNP SF2))
        (REDERR "The second argument must be a declared superfunction")))
      (SETQ PARITY1 (GET 'SFNARG SF1))
      (SETQ PARITY2 (GET 'SFNARG SF2))
      (COND
       ((NOT (EQN PARITY1 PARITY2))
        (REDERR
         "The number of arguments of the superfunctions must be the same")))
      (SETQ TARGET1 (GET 'SFTARGET SF1))
      (SETQ TARGET2 (GET 'SFTARGET SF2))
      (COND
       ((NOT (EQN TARGET1 TARGET2))
        (REDERR
         "The length of vectors in the target space must be the same"))))) 
(PUT 'CONV_GENFUN2VFORM 'NUMBER-OF-ARGS 2) 
(PUT 'CONV_GENFUN2VFORM 'DEFINED-ON-LINE '118) 
(PUT 'CONV_GENFUN2VFORM 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CONV_GENFUN2VFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONV_GENFUN2VFORM (GENFUN_ODD VFORM)
    (PROG (NUM_ARG LEN_TARGET)
      (CHECK_SUPERFUN_DEGONE GENFUN_ODD)
      (SETQ NUM_ARG (GET 'SFNARG GENFUN_ODD))
      (SETQ LEN_TARGET (GET 'SFTARGET GENFUN_ODD))
      (COND
       ((NOT (EQN LEN_TARGET (LENGTH ODD_VAR*)))
        (REDERR
         "Error: The number of components is not equal to the number of odd vars")))
      (MK_SUPERFUN VFORM (PLUS 1 NUM_ARG) 1)
      (SETK (LIST VFORM 1)
            (REPLACE_EXTODD
             (REVAL1
              (CONS 'PLUS
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND ((MINUSP (DIFFERENCE LEN_TARGET I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (ODD_PRODUCT (NTH ODD_VAR* I)
                                        (REVAL1 (LIST GENFUN_ODD I) NIL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE LEN_TARGET I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               (ODD_PRODUCT (NTH ODD_VAR* I)
                                (REVAL1 (LIST GENFUN_ODD I) NIL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
              NIL))))) 
(FLAG '(CONV_GENFUN2VFORM) 'OPFN) 
(PUT 'CONV_GENFUN2BIV 'NUMBER-OF-ARGS 2) 
(PUT 'CONV_GENFUN2BIV 'DEFINED-ON-LINE '137) 
(PUT 'CONV_GENFUN2BIV 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CONV_GENFUN2BIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONV_GENFUN2BIV (GENFUN_ODD BIV)
    (PROG (NUM_ARG LEN_TARGET)
      (CHECK_SUPERFUN_DEGONE GENFUN_ODD)
      (SETQ NUM_ARG (GET 'SFNARG GENFUN_ODD))
      (SETQ LEN_TARGET (GET 'SFTARGET GENFUN_ODD))
      (COND
       ((NOT (EQN LEN_TARGET (LENGTH ODD_VAR*)))
        (REDERR
         "Error: The number of components is not equal to the number of odd vars")))
      (MK_SUPERFUN BIV (PLUS 1 NUM_ARG) 1)
      (SETK (LIST BIV 1)
            (REPLACE_EXTODD
             (REVAL1
              (CONS 'PLUS
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND ((MINUSP (DIFFERENCE LEN_TARGET I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (ODD_PRODUCT
                                        (REVAL1 (LIST GENFUN_ODD I) NIL)
                                        (NTH ODD_VAR* I))
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE LEN_TARGET I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               (ODD_PRODUCT (REVAL1 (LIST GENFUN_ODD I) NIL)
                                (NTH ODD_VAR* I))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
              NIL))))) 
(FLAG '(CONV_GENFUN2BIV) 'OPFN) 
(PUT 'DEFINE_CDIFFOP 'NUMBER-OF-ARGS 2) 
(PUT 'DEFINE_CDIFFOP 'DEFINED-ON-LINE '157) 
(PUT 'DEFINE_CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'DEFINE_CDIFFOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEFINE_CDIFFOP (SUPERFUN_IJ CDIFF_OP_LIST)
    (PROG (CDIFFOPN CDIFFOPIND EXPROP PHI TEMPCOEFF TEMPODDVAR TEMPMIND
           CHANGE_EXPAND_TD TEMPIVARS)
      (SETQ CDIFFOPN (CAR CDIFF_OP_LIST))
      (SETQ CDIFFOPIND (CDR CDIFF_OP_LIST))
      (SETQ PHI (GENSYM))
      (SETQ EXPROP (CONS NIL 1))
      (SETQ CHANGE_EXPAND_TD 0)
      (COND
       ((EQ (GET 'TD 'SIMPFN) 'COMPUTE_TD)
        (PROGN (SETQ CHANGE_EXPAND_TD 1) (NOEXPAND_TD))))
      (PROG (EL)
        (SETQ EL SUPERFUN_IJ)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ TEMPCOEFF (CAR EL))
            (SETQ TEMPODDVAR (CADR EL))
            (SETQ TEMPMIND (CADR (IDTOMIND 1 TEMPODDVAR)))
            (SETQ TEMPIVARS
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                     STARTOVER
                      (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (LIST (NTH INDEP_VAR* I) (NTH TEMPMIND I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ I (PLUS2 I 1))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND
                       ((MINUSP (DIFFERENCE N_INDEP_VAR I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (LIST (NTH INDEP_VAR* I) (NTH TEMPMIND I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ I (PLUS2 I 1))
                      (GO LOOPLABEL)))
            (SETQ EXPROP
                    (ADDSQ
                     (SIMP
                      (REVAL1
                       (LIST 'TIMES TEMPCOEFF
                             (APPEND (LIST 'TD PHI) TEMPIVARS))
                       NIL))
                     EXPROP))
            NIL))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (CDE_EV_FORALL
       (CDE_FORALL_FORM CDIFFOPN CDIFFOPIND (LIST PHI) (MK*SQ EXPROP)))
      (RETURN (COND ((EQN CHANGE_EXPAND_TD 1) (EXPAND_TD)))))) 
(PUT 'SPLIT_SUPERFUN 'NUMBER-OF-ARGS 2) 
(PUT 'SPLIT_SUPERFUN 'DEFINED-ON-LINE '193) 
(PUT 'SPLIT_SUPERFUN 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'SPLIT_SUPERFUN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPLIT_SUPERFUN (SUPERFUN ODDVAR)
    (PROG (TEMPODDVARS SUPERFUN_SPLIT TEMPCOEFF)
      (SETQ TEMPODDVARS (SELECT_ALL_DERS 1 ODDVAR ALL_ODD_ID*))
      (SETQ SUPERFUN_SPLIT NIL)
      (PROG (EL)
        (SETQ EL TEMPODDVARS)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((NOT (EQN (SETQ TEMPCOEFF (COEFFN SUPERFUN EL 1)) 0))
             (SETQ SUPERFUN_SPLIT (CONS (LIST TEMPCOEFF EL) SUPERFUN_SPLIT)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN SUPERFUN_SPLIT))) 
(PUT 'CONV_SUPERFUN2CDIFF 'NUMBER-OF-ARGS 2) 
(PUT 'CONV_SUPERFUN2CDIFF 'DEFINED-ON-LINE '209) 
(PUT 'CONV_SUPERFUN2CDIFF 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CONV_SUPERFUN2CDIFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONV_SUPERFUN2CDIFF (SUPERFUN CDIFF_OP)
    (PROG (LEN_ODDVAR NUM_ARG LEN_ARG LEN_TARGET SUPERFUN_IJ)
      (CHECK_SUPERFUN_DEGONE SUPERFUN)
      (SETQ LEN_ODDVAR (LENGTH ODD_VAR*))
      (SETQ NUM_ARG (GET 'SFNARG SUPERFUN))
      (SETQ LEN_ARG (CONS 'LIST (LIST LEN_ODDVAR)))
      (SETQ LEN_TARGET (GET 'SFTARGET SUPERFUN))
      (MK_CDIFFOP CDIFF_OP 1 LEN_ARG LEN_TARGET)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN_ODDVAR J)) (RETURN NIL)))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE LEN_TARGET I)) (RETURN NIL)))
          (PROGN
           (SETQ SUPERFUN_IJ
                   (SPLIT_SUPERFUN (REVAL1 (LIST SUPERFUN I) NIL)
                    (NTH ODD_VAR* J)))
           (DEFINE_CDIFFOP SUPERFUN_IJ (LIST CDIFF_OP I J)))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ J (PLUS2 J 1))
        (GO LAB)))) 
(FLAG '(CONV_SUPERFUN2CDIFF) 'OPFN) 
(PUT 'MK_SUPERMAP 'NUMBER-OF-ARGS 4) 
(PUT 'MK_SUPERMAP 'DEFINED-ON-LINE '236) 
(PUT 'MK_SUPERMAP 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'MK_SUPERMAP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_SUPERMAP (SUPERMAP SM_PAR LEN_EVEN LEN_ODD)
    (PROG ()
      (MKOP SUPERMAP)
      (PUT 'SMAP SUPERMAP T)
      (COND
       ((NOT (FIXP SM_PAR))
        (REDERR "Error: the parity of the odd components must be an integer")))
      (PUT 'SMPAR SUPERMAP SM_PAR)
      (COND
       ((NOT (FIXP LEN_EVEN))
        (REDERR "Error in the number of even components")))
      (PUT 'SMAPECOMP SUPERMAP LEN_EVEN)
      (COND
       ((NOT (FIXP LEN_ODD)) (REDERR "Error in the number of odd components")))
      (PUT 'SMAPOCOMP SUPERMAP LEN_ODD))) 
(FLAG '(MK_SUPERMAP) 'OPFN) 
(PUT 'SUPERMAPP 'NUMBER-OF-ARGS 1) 
(PUT 'SUPERMAPP 'DEFINED-ON-LINE '264) 
(PUT 'SUPERMAPP 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'SUPERMAPP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUPERMAPP (SUPERMAP) (GET 'SMAP SUPERMAP)) 
(PUT 'CHECK_SUPERMAP_EVEN 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_SUPERMAP_EVEN 'DEFINED-ON-LINE '267) 
(PUT 'CHECK_SUPERMAP_EVEN 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CHECK_SUPERMAP_EVEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_SUPERMAP_EVEN (SUPERMAP)
    (PROG (PAR)
      (COND
       ((NOT (SUPERMAPP SUPERMAP))
        (REDERR "Error: the first argument must be a declared supermap")))
      (COND
       ((NOT (EQN (SETQ PAR (GET 'SMPAR SUPERMAP)) 0))
        (REDERR "Error: the supermap is not even"))))) 
(PUT 'CDE_SUPERFUN 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_SUPERFUN 'DEFINED-ON-LINE '277) 
(PUT 'CDE_SUPERFUN 'DEFINED-IN-FILE 'CDE/CDE_SUPERFUN.RED) 
(PUT 'CDE_SUPERFUN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_SUPERFUN NIL (PRIN2 "")) 
(ENDMODULE) 
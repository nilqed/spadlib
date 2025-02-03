(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPACES)) 
(REMFLAG (LIST 'MINUS) 'INTFN) 
(GLOBAL '(DIMEX* SGN* SIGNAT* SPACES* NUMINDXL* PAIR_ID_NUM*)) 
(SETQ PAIR_ID_NUM*
        '((|0| . 0) (|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
          (|7| . 7) (|8| . 8) (|9| . 9) (|10| . 10) (|11| . 11) (|12| . 12)
          (|13| . 13))) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON*)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(FLUID '(INDXL_TENS* DUMMY_ID* G_DVNAMES)) 
(SETQ DIMEX* (LIST (CONS (CONS 'DIM 1) 1))) 
(SETQ SGN* 1) 
(SETQ SIGNAT* 0) 
(FLUID '(ALGLIST*)) 
(DE GET_PROP_SPACE (U) (SUBLA SPACES* U)) 
(PUT 'GET_PROP_SPACE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_PROP_SPACE 'DEFINED-ON-LINE '65) 
(PUT 'GET_PROP_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'GET_PROP_SPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'GET_PROP_SPACE 'INLINE '(LAMBDA (U) (SUBLA SPACES* U))) 
(PUT '|CHARNUMP:| 'NUMBER-OF-ARGS 1) 
(PUT '|CHARNUMP:| 'DEFINED-ON-LINE '69) 
(PUT '|CHARNUMP:| 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT '|CHARNUMP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |CHARNUMP:| (X)
    (COND
     ((MEMQ X
            (LIST '|0| '|1| '|2| '|3| '|4| '|5| '|6| '|7| '|8| '|9| '|10| '|11|
                  '|12| '|13|))
      T))) 
(PUT 'GET_DIM_SPACE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_DIM_SPACE 'DEFINED-ON-LINE '75) 
(PUT 'GET_DIM_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'GET_DIM_SPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_DIM_SPACE (U)
    (COND ((NULL U) NIL)
          (T ((LAMBDA (X) (COND ((NOT (ATOM X)) (CAR X)))) (SUBLA SPACES* U))))) 
(PUT 'GET_SIGN_SPACE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_SIGN_SPACE 'DEFINED-ON-LINE '80) 
(PUT 'GET_SIGN_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'GET_SIGN_SPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_SIGN_SPACE (U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND
               ((AND (ATOM (CADR X)) (NULL (CDDR X)))
                (COND ((EQ (CADR X) 'EUCLIDIAN) 0) (T NIL)))
               (T (CADDR X))))
            (SUBLA SPACES* U))))) 
(PUT 'AFFINEP 'NUMBER-OF-ARGS 1) 
(PUT 'AFFINEP 'DEFINED-ON-LINE '89) 
(PUT 'AFFINEP 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'AFFINEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AFFINEP (U)
    ((LAMBDA (X) (COND (X (NULL (GET_SIGN_SPACE X)))))
     (GET (CAR U) 'BELONG_TO_SPACE))) 
(PUT 'GET_INDEXRANGE_SPACE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_INDEXRANGE_SPACE 'DEFINED-ON-LINE '94) 
(PUT 'GET_INDEXRANGE_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'GET_INDEXRANGE_SPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_INDEXRANGE_SPACE (U)
    (COND ((NULL SPACES*) NIL)
          (T
           ((LAMBDA (X)
              (COND
               (X
                (COND
                 ((AND (NOT (ATOM X)) (PAIRP (CDR X)) (PAIRP (CDDR X))
                       (PAIRP (CDDDR X)))
                  (CADDDR X))
                 ((AND (PAIRP (CDDR X)) (NOT (ATOM (CADDR X)))) (CADDR X))))))
            (COND (SPACES* (SUBLA SPACES* U))))))) 
(PUT 'ONESPACE 'NUMBER-OF-ARGS 1) 
(PUT 'ONESPACE 'DEFINED-ON-LINE '105) 
(PUT 'ONESPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'ONESPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ONESPACE (U)
    (COND
     ((EQ U '?) (COND (*ONESPACE (SYMB_TO_ALG 'YES)) (T (SYMB_TO_ALG 'NO))))
     (T NIL))) 
(PUT 'WHOLESPACE_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'WHOLESPACE_DIM 'DEFINED-ON-LINE '114) 
(PUT 'WHOLESPACE_DIM 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'WHOLESPACE_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WHOLESPACE_DIM (U)
    (PROG ()
      (COND ((EQ U '?) (RETURN (PREPSQ* (CONS DIMEX* 1))))
            ((NULL (GET 'WHOLESPACE 'SPACEDEF))
             (PROGN
              (SETQ DIMEX* (*Q2F (SIMP U)))
              (RETURN (PREPSQ* (CONS DIMEX* 1)))))))) 
(PUT 'GLOBAL_SIGN 'NUMBER-OF-ARGS 1) 
(PUT 'GLOBAL_SIGN 'DEFINED-ON-LINE '125) 
(PUT 'GLOBAL_SIGN 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'GLOBAL_SIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GLOBAL_SIGN (U)
    (PROG () (COND ((EQ U '?) (RETURN SGN*)) (T (RETURN (SETQ SGN* U)))))) 
(PUT 'SIGNATURE 'NUMBER-OF-ARGS 1) 
(PUT 'SIGNATURE 'DEFINED-ON-LINE '133) 
(PUT 'SIGNATURE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SIGNATURE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIGNATURE (U)
    (COND ((EQ U '?) SIGNAT*) ((AND *ONESPACE (FIXP U)) (SETQ SIGNAT* U))
          (T "non-active in OFF ONESPACE"))) 
(FLAG (LIST 'ONESPACE 'SHOW_SPACES 'WHOLESPACE_DIM 'GLOBAL_SIGN 'SIGNATURE)
      'OPFN) 
(PROG (*MSG) (NEWTOK '((|.| |.|) *INTERVAL*))) 
(COND
 ((NULL (GET '*INTERVAL* 'SIMPFN))
  (PROGN
   (PRECEDENCE (LIST '*INTERVAL* 'OR))
   (AEVAL (OPERATOR (LIST '*INTERVAL*)))
   (PUT '*INTERVAL* 'PRTCH '| .. |)
   NIL))) 
(PUT 'MKINTERVAL 'NUMBER-OF-ARGS 2) 
(PUT 'MKINTERVAL 'DEFINED-ON-LINE '161) 
(PUT 'MKINTERVAL 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MKINTERVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKINTERVAL (U V) (SYMB_TO_ALG (LIST '*INTERVAL* U V))) 
(PUT 'LST_BELONG_INTERVAL 'NUMBER-OF-ARGS 2) 
(PUT 'LST_BELONG_INTERVAL 'DEFINED-ON-LINE '166) 
(PUT 'LST_BELONG_INTERVAL 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'LST_BELONG_INTERVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LST_BELONG_INTERVAL (LST INT)
    (COND ((NULL LST) T)
          ((IDX_BELONG_INTERVAL (CAR LST) INT)
           (LST_BELONG_INTERVAL (CDR LST) INT))
          (T NIL))) 
(PUT 'IDX_BELONG_INTERVAL 'NUMBER-OF-ARGS 2) 
(PUT 'IDX_BELONG_INTERVAL 'DEFINED-ON-LINE '172) 
(PUT 'IDX_BELONG_INTERVAL 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'IDX_BELONG_INTERVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDX_BELONG_INTERVAL (IDX INT)
    (COND ((OR (NULL INT) (ATOM INT)) T)
          (T (AND (GEQ IDX (CAR INT)) (LEQ IDX (CADR INT)))))) 
(PUT 'NUMIDS2_BELONG_SAME_SPACE 'NUMBER-OF-ARGS 3) 
(PUT 'NUMIDS2_BELONG_SAME_SPACE 'DEFINED-ON-LINE '177) 
(PUT 'NUMIDS2_BELONG_SAME_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'NUMIDS2_BELONG_SAME_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUMIDS2_BELONG_SAME_SPACE (I1 I2 TENS)
    ((LAMBDA (X Y)
       (COND
        ((AND X Y)
         (PROG (IND SP)
           (COND ((NULL NUMINDXL*) (RETURN T)))
           (SETQ IND
                   (COND
                    ((SETQ SP (GET TENS 'BELONG_TO_SPACE))
                     (LIST (SUBLA NUMINDXL* SP)))
                    (T
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X NUMINDXL*)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (CDR X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
          LOOP
           (COND ((NULL IND) (RETURN NIL))
                 ((AND (IDX_BELONG_INTERVAL X (CAR IND))
                       (IDX_BELONG_INTERVAL Y (CAR IND)))
                  (RETURN T))
                 (T (SETQ IND (CDR IND))))
           (GO LOOP)))))
     (*ID2NUM I1) (*ID2NUM I2))) 
(PUT 'NUM_IDS_BELONG_SAME_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'NUM_IDS_BELONG_SAME_SPACE 'DEFINED-ON-LINE '196) 
(PUT 'NUM_IDS_BELONG_SAME_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'NUM_IDS_BELONG_SAME_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_IDS_BELONG_SAME_SPACE (U TENS)
    (PROGN
     (COND ((ODDP (LENGTH U)) (SETQ U (CONS (CAR U) U))))
     (PROG ()
      WHILELABEL
       (COND
        ((NOT (AND U (NUMIDS2_BELONG_SAME_SPACE (CAR U) (CADR U) TENS)))
         (RETURN NIL)))
       (SETQ U (CDDR U))
       (GO WHILELABEL))
     (COND ((NULL U) T) (T NIL)))) 
(PUT 'SYMB_IDS_BELONG_SAME_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'SYMB_IDS_BELONG_SAME_SPACE 'DEFINED-ON-LINE '203) 
(PUT 'SYMB_IDS_BELONG_SAME_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SYMB_IDS_BELONG_SAME_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMB_IDS_BELONG_SAME_SPACE (U V)
    (COND ((OR (NULL U) (EQUAL V 'WHOLESPACE)) T)
          ((OR (NULL (GET (CAR U) 'SPACE)) (EQUAL (GET (CAR U) 'SPACE) V))
           (SYMB_IDS_BELONG_SAME_SPACE (CDR U) V))
          ((NULL V) (SYMB_IDS_BELONG_SAME_SPACE (CDR U) (GET (CAR U) 'SPACE)))
          ((NEQ (GET (CAR U) 'SPACE) V) NIL))) 
(PUT '|SYMB_IDS_BELONG_SAME_SPACE:| 'NUMBER-OF-ARGS 2) 
(PUT '|SYMB_IDS_BELONG_SAME_SPACE:| 'DEFINED-ON-LINE '217) 
(PUT '|SYMB_IDS_BELONG_SAME_SPACE:| 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT '|SYMB_IDS_BELONG_SAME_SPACE:| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SYMB_IDS_BELONG_SAME_SPACE:| (U V)
    (COND ((NULL U) T)
          ((OR (NULL (GET (CAR U) 'SPACE)) (EQUAL (GET (CAR U) 'SPACE) V))
           (|SYMB_IDS_BELONG_SAME_SPACE:| (CDR U) V))
          ((NULL V)
           (|SYMB_IDS_BELONG_SAME_SPACE:| (CDR U) (GET (CAR U) 'SPACE)))
          ((NEQ (GET (CAR U) 'SPACE) V) NIL))) 
(PUT 'IND_SAME_SPACE_TENS 'NUMBER-OF-ARGS 2) 
(PUT 'IND_SAME_SPACE_TENS 'DEFINED-ON-LINE '236) 
(PUT 'IND_SAME_SPACE_TENS 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'IND_SAME_SPACE_TENS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IND_SAME_SPACE_TENS (U TENS)
    (PROG (LST LSTNUM)
      (SETQ LST (CLEAN_NUMID U))
      (SETQ LSTNUM (EXTRACT_NUM_ID U))
      (RETURN
       (COND
        ((AND (NUM_IDS_BELONG_SAME_SPACE LSTNUM TENS)
              (SYMB_IDS_BELONG_SAME_SPACE LST (GET TENS 'BELONG_TO_SPACE)))
         T)
        (T NIL))))) 
(RLISTAT '(DEFINE_SPACES REM_SPACES)) 
(PUT 'DEFINE_SPACES 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINE_SPACES 'DEFINED-ON-LINE '253) 
(PUT 'DEFINE_SPACES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'DEFINE_SPACES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINE_SPACES (U)
    (COND
     (*ONESPACE
      (REDERR
       "For the definition of subspaces the switch onespace needs to be set to off (see the documentation for details)."))
     ((NOT (FIXP SGN*)) (REDERR "set the global sign please"))
     (T
      (PROG (SP)
        (RMSUBS)
        (PROG (J)
          (SETQ J U)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J)
             (COND ((NOT (EQEXPR J)) (ERRPRI2 J 'HOLD))
                   ((OR (GET (SETQ SP (CADR J)) 'SPACEDEF) (FLAGP SP 'RESERVED)
                        (GETRTYPE SP) (GETTYPE SP))
                    (LPRI
                     (LIST "*** Warning:" SP
                           " cannot be (or is already) defined as space identifier")))
                   (T
                    (PROGN
                     ((LAMBDA (Y)
                        (PUT SP 'SPACEDEF
                             (COND
                              ((EQEXPR (CADDR Y))
                               (CONS SP (CONS (CADR Y) (WHOLE_SPACE SP Y))))
                              (T (CONS SP (WHOLE_EUCLID_SPACE SP Y))))))
                      (CADDR J))
                     (SETQ SPACES*
                             (COND
                              ((NULL (ASSOC SP SPACES*))
                               (UNION (LIST (GET SP 'SPACEDEF)) SPACES*))))
                     (SETQ NUMINDXL*
                             (COND
                              ((SPACE_INDEX_RANGE SP)
                               (UNION (LIST (CONS SP (SPACE_INDEX_RANGE SP)))
                                      NUMINDXL*))))
                     NIL))))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB))
        (RETURN T))))) 
(PUT 'WHOLE_EUCLID_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'WHOLE_EUCLID_SPACE 'DEFINED-ON-LINE '285) 
(PUT 'WHOLE_EUCLID_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'WHOLE_EUCLID_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WHOLE_EUCLID_SPACE (SP U)
    ((LAMBDA (W)
       (COND
        ((EQ SP 'WHOLESPACE)
         (PROGN
          (SETQ DIMEX* (LIST (CONS (CONS (CAR W) 1) 1)))
          (SETQ SIGNAT* 0)
          W))
        (T W)))
     (CDR U))) 
(PUT 'WHOLE_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'WHOLE_SPACE 'DEFINED-ON-LINE '292) 
(PUT 'WHOLE_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'WHOLE_SPACE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WHOLE_SPACE (SP U)
    ((LAMBDA (W)
       (COND
        ((EQ SP 'WHOLESPACE)
         (PROGN
          (SETQ DIMEX* (LIST (CONS (CONS (CAR W) 1) 1)))
          (SETQ SIGNAT* (CADDR (CADR W)))
          (COND
           ((CDDR W)
            (CONS (CADADR W) (CONS (CADR (CDADR W)) (LIST (CADDR W)))))
           (T (CDADR W)))))
        ((CDDR W) (CONS (CADADR W) (CONS (CADR (CDADR W)) (LIST (CADDR W)))))
        (T (CDADR W))))
     (CDR U))) 
(PUT 'SPACE_INDEX_RANGE 'NUMBER-OF-ARGS 1) 
(PUT 'SPACE_INDEX_RANGE 'DEFINED-ON-LINE '315) 
(PUT 'SPACE_INDEX_RANGE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SPACE_INDEX_RANGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPACE_INDEX_RANGE (U)
    (PROG (X)
      (SETQ X (GET_INDEXRANGE_SPACE U))
      (RETURN
       (COND ((NULL X) NIL)
             ((NOT (EQCAR X '*INTERVAL*))
              (RERROR 'CANTENSOR 4
                      (LIST "Invalid indexrange " X
                            " (Forgot space around .. operator?)")))
             (T (BUBBLESORT1 (CONS (CADDR (CADR X)) (CONS (CADDR X) NIL)))))))) 
(PUT 'REM_SPACES 'NUMBER-OF-ARGS 1) 
(PUT 'REM_SPACES 'DEFINED-ON-LINE '327) 
(PUT 'REM_SPACES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'REM_SPACES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_SPACES (U)
    (PROGN
     (PROG (J)
       (SETQ J U)
      LAB
       (COND ((NULL J) (RETURN NIL)))
       ((LAMBDA (J)
          (PROGN
           (REMPROP J 'SPACEDEF)
           (SETQ SPACES* (DELETE (ASSOC J SPACES*) SPACES*))
           (SETQ NUMINDXL* (DELETE (ASSOC J NUMINDXL*) NUMINDXL*))
           (REMFLAG (LIST J) 'RESERVED)
           (COND
            ((EQ J 'WHOLESPACE)
             (PROGN
              (SETQ DIMEX* (LIST (CONS (CONS 'DIM 1) 1)))
              (SETQ SIGNAT* 0)
              NIL)))))
        (CAR J))
       (SETQ J (CDR J))
       (GO LAB))
     T)) 
(PUT 'MKEQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'MKEQUAL 'DEFINED-ON-LINE '338) 
(PUT 'MKEQUAL 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MKEQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKEQUAL (U) (LIST 'EQUAL 'SIGNATURE (CADR U))) 
(PUT 'INSERT_SIGN_EQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'INSERT_SIGN_EQUAL 'DEFINED-ON-LINE '342) 
(PUT 'INSERT_SIGN_EQUAL 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'INSERT_SIGN_EQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INSERT_SIGN_EQUAL (U)
    (PROG (L)
     LOOP
      (COND ((NULL U) (RETURN (REVERSE L))))
      (COND
       ((NEQ (CAR U) 'SIGNATURE)
        (PROGN (SETQ L (CONS (CAR U) L)) (SETQ U (CDR U))))
       (T (PROGN (SETQ L (CONS (MKEQUAL U) L)) (SETQ U (CDDR U)))))
      (GO LOOP))) 
(PUT 'SHOW_SPACES 'NUMBER-OF-ARGS 0) 
(PUT 'SHOW_SPACES 'DEFINED-ON-LINE '351) 
(PUT 'SHOW_SPACES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SHOW_SPACES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SHOW_SPACES NIL
    (PROG (X)
      (SETQ X
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I SPACES*)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I) (INSERT_SIGN_EQUAL I)) (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (INSERT_SIGN_EQUAL I)) (CAR I))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y X)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y)
                                    (CONS 'LIST
                                          (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ Z Y)
                                            (COND ((NULL Z) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (Z)
                                                                (COND
                                                                 ((PAIRP Z) Z)
                                                                 (T
                                                                  (MK*SQ
                                                                   (CONS
                                                                    (LIST
                                                                     (CONS
                                                                      (CONS Z
                                                                            1)
                                                                      1))
                                                                    1)))))
                                                              (CAR Z))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ Z (CDR Z))
                                            (COND
                                             ((NULL Z) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (Z)
                                                        (COND ((PAIRP Z) Z)
                                                              (T
                                                               (MK*SQ
                                                                (CONS
                                                                 (LIST
                                                                  (CONS
                                                                   (CONS Z 1)
                                                                   1))
                                                                 1)))))
                                                      (CAR Z))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Y)
                            (CONS 'LIST
                                  (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ Z Y)
                                    (COND ((NULL Z) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (Z)
                                                        (COND ((PAIRP Z) Z)
                                                              (T
                                                               (MK*SQ
                                                                (CONS
                                                                 (LIST
                                                                  (CONS
                                                                   (CONS Z 1)
                                                                   1))
                                                                 1)))))
                                                      (CAR Z))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ Z (CDR Z))
                                    (COND ((NULL Z) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (Z)
                                                (COND ((PAIRP Z) Z)
                                                      (T
                                                       (MK*SQ
                                                        (CONS
                                                         (LIST
                                                          (CONS (CONS Z 1) 1))
                                                         1)))))
                                              (CAR Z))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                          (CAR Y))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST (REVERSE X))))) 
(FLAG (LIST 'MK_IDS_BELONG_SPACE) 'OPFN) 
(PUT 'MK_IDS_BELONG_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'MK_IDS_BELONG_SPACE 'DEFINED-ON-LINE '363) 
(PUT 'MK_IDS_BELONG_SPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MK_IDS_BELONG_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_IDS_BELONG_SPACE (U V)
    (COND (*ONESPACE NIL) ((IDP U) (PROGN (PUT U 'SPACE V) T))
          (T
           (PROGN
            (PROG (X)
              (SETQ X U)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (PUT X 'SPACE V)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            T)))) 
(RLISTAT '(MK_IDS_BELONG_ANYSPACE)) 
(PUT 'MK_IDS_BELONG_ANYSPACE 'NUMBER-OF-ARGS 1) 
(PUT 'MK_IDS_BELONG_ANYSPACE 'DEFINED-ON-LINE '375) 
(PUT 'MK_IDS_BELONG_ANYSPACE 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MK_IDS_BELONG_ANYSPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_IDS_BELONG_ANYSPACE (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (REMPROP X 'SPACE)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'SPACE_OF_IDX 'NUMBER-OF-ARGS 1) 
(PUT 'SPACE_OF_IDX 'DEFINED-ON-LINE '379) 
(PUT 'SPACE_OF_IDX 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SPACE_OF_IDX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPACE_OF_IDX (U)
    (PROG (SP)
      (RETURN
       (COND ((SETQ SP (GET U 'SPACE)) SP)
             ((ASSOC 'WHOLESPACE SPACES*) 'WHOLESPACE)
             ((EQUAL (LENGTH SPACES*) 1)
              (COND
               ((YESP (LIST "Does " U " belong to " (CAAR SPACES*) "?"))
                (PUT U 'SPACE (CAAR SPACES*)))
               (T
                (RERROR 'CANTENSOR 4 (LIST "Space of index " U " unknown")))))
             (T (MSGPRI NIL NIL U "MUST belong to a (sub)space" T)))))) 
(PUT 'SPACE_DIM_OF_IDX 'NUMBER-OF-ARGS 1) 
(PUT 'SPACE_DIM_OF_IDX 'DEFINED-ON-LINE '395) 
(PUT 'SPACE_DIM_OF_IDX 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'SPACE_DIM_OF_IDX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPACE_DIM_OF_IDX (U)
    (COND
     ((NULL *ONESPACE)
      (PROG (SP)
        (SETQ SP (GET U 'SPACE))
        (COND ((NULL SP) (RETURN (CAAAR DIMEX*)))
              (T (RETURN (GET_DIM_SPACE SP)))))))) 
(PUT 'EXTRACT_DUMMY_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'EXTRACT_DUMMY_IDS 'DEFINED-ON-LINE '406) 
(PUT 'EXTRACT_DUMMY_IDS 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'EXTRACT_DUMMY_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRACT_DUMMY_IDS (U)
    (COND ((NULL U) NIL)
          ((MEMQ (CAR U) DUMMY_ID*) (CONS (CAR U) (EXTRACT_DUMMY_IDS (CDR U))))
          (T (EXTRACT_DUMMY_IDS (CDR U))))) 
(RLISTAT '(REM_DUMMY_INDICES)) 
(PUT 'REM_DUMMY_INDICES 'NUMBER-OF-ARGS 1) 
(PUT 'REM_DUMMY_INDICES 'DEFINED-ON-LINE '415) 
(PUT 'REM_DUMMY_INDICES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'REM_DUMMY_INDICES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_DUMMY_INDICES (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (PROGN
           (SETQ DUMMY_ID* (DELETE X DUMMY_ID*))
           (REMPROP X 'SPACE)
           (REMFLAG (LIST X) 'DUMMY)
           (REMFLAG (LIST X) 'RESERVED)))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (DUMMY_NAM DUMMY_ID*)
     T)) 
(PUT 'DUMMY_INDICES 'NUMBER-OF-ARGS 0) 
(PUT 'DUMMY_INDICES 'DEFINED-ON-LINE '425) 
(PUT 'DUMMY_INDICES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'DUMMY_INDICES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DUMMY_INDICES NIL (SYMB_TO_ALG DUMMY_ID*)) 
(FLAG (LIST 'DUMMY_INDICES) 'OPFN) 
(PUT 'MK_DUMMY_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'MK_DUMMY_IDS 'DEFINED-ON-LINE '430) 
(PUT 'MK_DUMMY_IDS 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MK_DUMMY_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_DUMMY_IDS (U)
    (PROG (Y)
      (SETQ Y (CLEAN_NUMID (INTERSECTION (CAR U) (CADR U))))
      (FLAG Y 'DUMMY)
      (FLAG Y 'RESERVED)
      (SETQ DUMMY_ID* (UNION Y DUMMY_ID*)))) 
(PUT 'MK_LST_FOR_DUMMY 'NUMBER-OF-ARGS 1) 
(PUT 'MK_LST_FOR_DUMMY 'DEFINED-ON-LINE '442) 
(PUT 'MK_LST_FOR_DUMMY 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MK_LST_FOR_DUMMY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_LST_FOR_DUMMY (U)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X U)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (COND ((ATOM X) X)
                                ((MEMQ (CADR X) DUMMY_ID*) (CADR X)) (T X)))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (COND ((ATOM X) X) ((MEMQ (CADR X) DUMMY_ID*) (CADR X))
                        (T X)))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MULTIPLICITY_ELT 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIPLICITY_ELT 'DEFINED-ON-LINE '451) 
(PUT 'MULTIPLICITY_ELT 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MULTIPLICITY_ELT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIPLICITY_ELT (OB L)
    (PROG (N)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ L (MEMQ OB L))) (RETURN NIL)))
        (PROGN (SETQ L (CDR L)) (SETQ N (PLUS N 1)))
        (GO WHILELABEL))
      (RETURN N))) 
(PUT 'MULT_LEQ_ONEP 'NUMBER-OF-ARGS 1) 
(PUT 'MULT_LEQ_ONEP 'DEFINED-ON-LINE '459) 
(PUT 'MULT_LEQ_ONEP 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'MULT_LEQ_ONEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULT_LEQ_ONEP (U)
    (COND ((NULL U) T)
          ((LEQ (MULTIPLICITY_ELT (CAR U) U) 1) (MULT_LEQ_ONEP (CDR U))))) 
(PUT 'EQN_INDICES 'NUMBER-OF-ARGS 2) 
(PUT 'EQN_INDICES 'DEFINED-ON-LINE '466) 
(PUT 'EQN_INDICES 'DEFINED-IN-FILE 'ASSIST/SPACES.RED) 
(PUT 'EQN_INDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQN_INDICES (U V)
    ((LAMBDA (X Y) (AND X Y (EQN X Y))) (*ID2NUM U) (*ID2NUM V))) 
(ENDMODULE) 
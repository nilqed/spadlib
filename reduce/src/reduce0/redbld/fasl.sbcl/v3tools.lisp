(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(TERPRI) 
(PRIN2 "********************************************************************") 
NIL 
(TERPRI) 
(PRIN2 "* Please note the following conventions to use these routines:     *") 
NIL 
(TERPRI) 
(PRIN2 "* - Vectors are denoted by a single character and products         *") 
NIL 
(TERPRI) 
(PRIN2 "*   (A,Bx(Cx(Dx...))) by single identifiers ABCD... . For example, *") 
NIL 
(TERPRI) 
(PRIN2 "*   scalar products (A,B) by AB, triple products (A,BxC) by ABC,   *") 
NIL 
(TERPRI) 
(PRIN2 "*   (A,Bx(CxD)) by ABCD and so on. For scalar and triple products  *") 
NIL 
(TERPRI) 
(PRIN2 "*   only those versions are used for which letters are sorted      *") 
NIL 
(TERPRI) 
(PRIN2 "*   alphabetically, for example, AB, ABU but not BA, AUB or BUA.   *") 
NIL 
(TERPRI) 
(PRIN2 "* - Any non-vectorial variable starts with the two characters !&.  *") 
NIL 
(TERPRI) 
(PRIN2 "* - For further comments see the beginning of this file and the    *") 
NIL 
(TERPRI) 
(PRIN2 "*   files v3tools.tex and v3tools.tst.                             *") 
NIL 
(TERPRI) 
(PRIN2 "********************************************************************") 
NIL 
(TERPRI) 
(COND ((GETD 'SET_BNDSTK_SIZE) (SET_BNDSTK_SIZE 50000))) 
(LOAD_PACKAGE (LIST 'GROEBNER)) 
(LOAD_PACKAGE (LIST 'CRACK)) 
(SETCRACKFLAGS) 
(FLUID
 '(PRINT_MORE RECORD_HIST MAX_GC_SHORT SIZE_WATCH MAX_GC_FAC PRINT_ OLD_HISTORY
   TR_SHORT TR_VEC)) 
(SETQ TR_VEC NIL) 
(AEVAL (OPERATOR (LIST 'POI_))) 
(FLUID '(WGTHS_ FINO_ NFCT_)) 
(PUT 'GEN 'NUMBER-OF-ARGS 1) 
(PUT 'GEN 'DEFINED-ON-LINE '142) 
(PUT 'GEN 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GEN (ALL)
    (COND
     ((NULL ALL)
      (LIST
       (CONS NIL
             (PROG (H FORALL-RESULT FORALL-ENDPTR)
               (SETQ H WGTHS_)
               (COND ((NULL H) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (H) 0) (CAR H)) NIL)))
              LOOPLABEL
               (SETQ H (CDR H))
               (COND ((NULL H) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (H) 0) (CAR H)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
     (T
      (PROG (H V VW OLDLI NEWLI VWCP WGCP FOUND_LESS FOUND_MORE B C D)
        (SETQ H (CAR ALL))
        (SETQ ALL (CDR ALL))
        (SETQ V (CAR H))
        (SETQ VW (CDR H))
        (SETQ OLDLI (GEN ALL))
        (PROG ()
         WHILELABEL
          (COND ((NOT OLDLI) (RETURN NIL)))
          (PROGN
           (SETQ H (CAR OLDLI))
           (SETQ OLDLI (CDR OLDLI))
           (SETQ NEWLI (CONS H NEWLI))
           (PROG ()
            REPEATLABEL
             (PROGN
              (SETQ VWCP VW)
              (SETQ WGCP WGTHS_)
              (SETQ FOUND_LESS NIL)
              (SETQ FOUND_MORE NIL)
              (SETQ H
                      (CONS (CONS V (CAR H))
                            (PROG (B FORALL-RESULT FORALL-ENDPTR)
                              (SETQ B (CDR H))
                              (COND ((NULL B) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (B)
                                                  (PROGN
                                                   (SETQ D
                                                           (PLUS B
                                                                 (PROGN
                                                                  (SETQ C
                                                                          (CAR
                                                                           VWCP))
                                                                  (SETQ VWCP
                                                                          (CDR
                                                                           VWCP))
                                                                  C)))
                                                   (COND
                                                    ((LESSP D (CAR WGCP))
                                                     (SETQ FOUND_LESS T))
                                                    ((GREATERP D (CAR WGCP))
                                                     (SETQ FOUND_MORE T)))
                                                   (SETQ WGCP (CDR WGCP))
                                                   D))
                                                (CAR B))
                                               NIL)))
                             LOOPLABEL
                              (SETQ B (CDR B))
                              (COND ((NULL B) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (B)
                                          (PROGN
                                           (SETQ D
                                                   (PLUS B
                                                         (PROGN
                                                          (SETQ C (CAR VWCP))
                                                          (SETQ VWCP
                                                                  (CDR VWCP))
                                                          C)))
                                           (COND
                                            ((LESSP D (CAR WGCP))
                                             (SETQ FOUND_LESS T))
                                            ((GREATERP D (CAR WGCP))
                                             (SETQ FOUND_MORE T)))
                                           (SETQ WGCP (CDR WGCP))
                                           D))
                                        (CAR B))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
              NIL)
             (COND
              ((NOT
                (COND (FOUND_MORE T)
                      ((EQUAL FOUND_LESS NIL)
                       (PROGN
                        (SETQ FINO_
                                (CONS
                                 (COND ((CDAR H) (CONS 'TIMES (CAR H)))
                                       (T (CAAR H)))
                                 FINO_))
                        T))
                      (T (PROGN (SETQ NEWLI (CONS H NEWLI)) NIL))))
               (GO REPEATLABEL)))))
          (GO WHILELABEL))
        (RETURN NEWLI))))) 
(PUT 'IS_REDUCEABLE 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_REDUCEABLE) 'OPFN) 
(PUT 'IS_REDUCEABLE 'DEFINED-ON-LINE '190) 
(PUT 'IS_REDUCEABLE 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'IS_REDUCEABLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_REDUCEABLE (TRM HS)
    (PROG ()
      (WHILE
       (AND (EVALNEQ (AEVAL* HS) (AEVAL* (LIST 'LIST)))
            (EVALNEQ (AEVAL* (LIST 'DEN (LIST 'QUOTIENT TRM (LIST 'FIRST HS))))
                     1))
       (SETQ HS (AEVAL* (LIST 'REST HS))))
      (RETURN
       (COND ((EVALNEQ (AEVAL HS) (AEVAL (LIST 'LIST))) (AEVAL 'T))
             (T (AEVAL 'NIL)))))) 
(PUT 'L2S 'NUMBER-OF-ARGS 1) 
(PUT 'L2S 'DEFINED-ON-LINE '199) 
(PUT 'L2S 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'L2S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE L2S (LI)
    (COND
     ((EQUAL (LENGTH LI) 2)
      (COND ((ORDP (CAR LI) (CADR LI)) (MKID (CAR LI) (CADR LI)))
            (T (MKID (CADR LI) (CAR LI)))))
     ((EQUAL (LENGTH LI) 3)
      (COND
       ((OR (EQUAL (CAR LI) (CADR LI)) (EQUAL (CAR LI) (CADDR LI))
            (EQUAL (CADR LI) (CADDR LI)))
        0)
       ((AND (ORDP (CAR LI) (CADR LI)) (ORDP (CADR LI) (CADDR LI)))
        (MKID (CAR LI) (MKID (CADR LI) (CADDR LI))))
       ((AND (ORDP (CADR LI) (CADDR LI)) (ORDP (CADDR LI) (CAR LI)))
        (MKID (CADR LI) (MKID (CADDR LI) (CAR LI))))
       ((AND (ORDP (CADDR LI) (CAR LI)) (ORDP (CAR LI) (CADR LI)))
        (MKID (CADDR LI) (MKID (CAR LI) (CADR LI))))
       ((AND (ORDP (CAR LI) (CADDR LI)) (ORDP (CADDR LI) (CADR LI)))
        (LIST 'MINUS (MKID (CAR LI) (MKID (CADDR LI) (CADR LI)))))
       ((AND (ORDP (CADR LI) (CAR LI)) (ORDP (CAR LI) (CADDR LI)))
        (LIST 'MINUS (MKID (CADR LI) (MKID (CAR LI) (CADDR LI)))))
       ((AND (ORDP (CADDR LI) (CADR LI)) (ORDP (CADR LI) (CAR LI)))
        (LIST 'MINUS (MKID (CADDR LI) (MKID (CADR LI) (CAR LI)))))
       (T (PROGN (PRIN2 "error in ord!") NIL))))
     (T
      (LIST 'DIFFERENCE
            (LIST 'TIMES
                  (COND ((ORDP (CAR LI) (CADDR LI)) (MKID (CAR LI) (CADDR LI)))
                        (T (MKID (CADDR LI) (CAR LI))))
                  (L2S (CONS (CADR LI) (CDDDR LI))))
            (LIST 'TIMES
                  (COND
                   ((ORDP (CADR LI) (CADDR LI)) (MKID (CADR LI) (CADDR LI)))
                   (T (MKID (CADDR LI) (CADR LI))))
                  (L2S (CONS (CAR LI) (CDDDR LI)))))))) 
(FLUID '(PERMU_REPI_CACHE)) 
(PUT 'PERMU_REPI 'NUMBER-OF-ARGS 1) 
(PUT 'PERMU_REPI 'DEFINED-ON-LINE '271) 
(PUT 'PERMU_REPI 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'PERMU_REPI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PERMU_REPI (LI) (PROG (PERMU_REPI_CACHE) (RETURN (PERMU_REPI_1 LI)))) 
(PUT 'PERMU_REPI_1 'NUMBER-OF-ARGS 1) 
(PUT 'PERMU_REPI_1 'DEFINED-ON-LINE '280) 
(PUT 'PERMU_REPI_1 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'PERMU_REPI_1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PERMU_REPI_1 (LI)
    (PROG (R P PERM RED_PERM LIX XLIX PERMS_SEEN)
      (COND ((SETQ R (ASSOC LI PERMU_REPI_CACHE)) (RETURN (CDR R))))
      (SETQ PERM
              (COND ((EQUAL 1 (LENGTH LI)) (LIST LI))
                    (T
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X LI)
                      STARTOVER
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (X)
                                  (PROGN
                                   (SETQ LIX (DELETE X LI))
                                   (SETQ XLIX (CONS X LIX))
                                   (COND
                                    ((NOT (MEMBER XLIX PERMS_SEEN))
                                     (PROGN
                                      (SETQ PERMS_SEEN (CONS XLIX PERMS_SEEN))
                                      (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ Y (PERMU_REPI_1 LIX))
                                        (COND ((NULL Y) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (Y)
                                                            (CONS X Y))
                                                          (CAR Y))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ Y (CDR Y))
                                        (COND
                                         ((NULL Y) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Y) (CONS X Y))
                                                  (CAR Y))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))))))
                                (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ X (CDR X))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (X)
                                  (PROGN
                                   (SETQ LIX (DELETE X LI))
                                   (SETQ XLIX (CONS X LIX))
                                   (COND
                                    ((NOT (MEMBER XLIX PERMS_SEEN))
                                     (PROGN
                                      (SETQ PERMS_SEEN (CONS XLIX PERMS_SEEN))
                                      (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ Y (PERMU_REPI_1 LIX))
                                        (COND ((NULL Y) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (Y)
                                                            (CONS X Y))
                                                          (CAR Y))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ Y (CDR Y))
                                        (COND
                                         ((NULL Y) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Y) (CONS X Y))
                                                  (CAR Y))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))))))
                                (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ X (CDR X))
                       (GO LOOPLABEL)))))
      (COND
       ((AND PERM (EQUAL (LENGTH (CAR PERM)) 3))
        (PROGN
         (PROG (P)
           (SETQ P PERM)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((NEQ (CADR P) (CADDR P)) (SETQ RED_PERM (CONS P RED_PERM)))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (SETQ PERM RED_PERM)
         NIL)))
      (SETQ PERM (REVERSIP PERM))
      (SETQ PERMU_REPI_CACHE (CONS (CONS LI PERM) PERMU_REPI_CACHE))
      (RETURN PERM))) 
(PUT 'GENID 'NUMBER-OF-ARGS 2) 
(PUT 'GENID 'DEFINED-ON-LINE '308) 
(PUT 'GENID 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GENID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENID (VLI WGLI)
    (PROG (H HM P PERM ALLPERM IDLI IDLICP)
      (SETQ FINO_ NIL)
      (SETQ WGTHS_ WGLI)
      (GEN VLI)
      (PROG (H)
        (SETQ H FINO_)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (PROGN
             (PRIN2
              "Generating specific permutations of products of the vectors ")
             (PRIN2 (COMPRESS (CDR H)))
             (PRIN2 ".")
             NIL)
            (TERPRI)
            (SETQ ALLPERM (PERMU_REPI (CDR H)))
            (PROGN
             (PRIN2 (LENGTH ALLPERM))
             (PRIN2 " permutations generated.")
             NIL)
            (TERPRI)
            (PROG (P)
              (SETQ P ALLPERM)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (COND ((NEQ (CAR P) (CADR P)) (SETQ PERM (CONS P PERM)))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ ALLPERM NIL)
            (PROGN
             (PRIN2 "Applying another filter, ")
             (PRIN2 (LENGTH PERM))
             (PRIN2 " are left.")
             NIL)
            (TERPRI)
            (SETQ IDLI NIL)
            (PROG (P)
              (SETQ P PERM)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (PROGN
                  (SETQ H (REVAL1 (L2S P) T))
                  (COND
                   ((NOT (ZEROP H))
                    (PROGN
                     (SETQ HM (REVAL1 (LIST 'MINUS H) T))
                     (SETQ IDLICP IDLI)
                     (PROG ()
                      WHILELABEL
                       (COND
                        ((NOT
                          (AND IDLICP (NEQ (CAAR IDLICP) H)
                               (NEQ (CAAR IDLICP) HM)))
                         (RETURN NIL)))
                       (SETQ IDLICP (CDR IDLICP))
                       (GO WHILELABEL))
                     (COND
                      ((NULL IDLICP)
                       (SETQ IDLI
                               (CONS (CONS H (INTERN (COMPRESS P))) IDLI)))))))
                  NIL))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (PROGN
             (PRIN2 "At a first look ")
             (PRIN2 (LENGTH IDLI))
             (PRIN2 " seem to be non-equivalent.")
             NIL)
            (TERPRI)
            NIL))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ FINO_ NIL)
      (RETURN
       (PROG (H FORALL-RESULT FORALL-ENDPTR)
         (SETQ H IDLI)
         (COND ((NULL H) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (H) (LIST 'DIFFERENCE (CAR H) (CDR H)))
                           (CAR H))
                          NIL)))
        LOOPLABEL
         (SETQ H (CDR H))
         (COND ((NULL H) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (H) (LIST 'DIFFERENCE (CAR H) (CDR H))) (CAR H))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(FLUID '(ALGEBRAIC_REDUCE_FUNCTIONS)) 
(SETQ ALGEBRAIC_REDUCE_FUNCTIONS
        '(PLUS MINUS DIFFERENCE TIMES QUOTIENT EXPT ARBCOMPLEX LIST)) 
(PUT 'EXPRO 'NUMBER-OF-ARGS 1) 
(PUT 'EXPRO 'DEFINED-ON-LINE '361) 
(PUT 'EXPRO 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'EXPRO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPRO (A)
    (COND ((NULL A) NIL)
          ((ATOM A)
           (COND ((OR (NUMBERP A) (MEMBER A ALGEBRAIC_REDUCE_FUNCTIONS)) NIL)
                 (T (LIST A))))
          (T (UNION (EXPRO (CAR A)) (EXPRO (CDR A)))))) 
(PUT 'EXTRACT_PROD 'NUMBER-OF-ARGS 1) 
(PUT 'EXTRACT_PROD 'DEFINED-ON-LINE '377) 
(PUT 'EXTRACT_PROD 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'EXTRACT_PROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRACT_PROD (A)
    (PROG (EP EPL H RET)
      (SETQ EP (EXPRO (REVAL1 A T)))
      (PROG (H)
        (SETQ H EP)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ EPL (EXPLODE H))
            (COND
             ((AND (NEQ (CAR EPL) '&) (NEQ (CAR EPL) '&) (NEQ (CAR EPL) '!))
              (SETQ RET (CONS H RET))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN RET))) 
(FLAG '(E2S) 'OPFN) 
(PUT 'E2S 'NUMBER-OF-ARGS 1) 
(PUT 'E2S 'DEFINED-ON-LINE '392) 
(PUT 'E2S 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'E2S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE E2S (A)
    (PROG (P PL PX)
      (SETQ PL (EXTRACT_PROD A))
      (PROG (P)
        (SETQ P PL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ PX (L2S (EXPLODE P)))
            (COND ((NEQ P PX) (SETQ A (SUBST PX P A))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN A))) 
(PUT 'VC 'NUMBER-OF-ARGS 1) 
(PUT 'VC 'DEFINED-ON-LINE '406) 
(PUT 'VC 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'VC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VC (V) (LIST (MKID V 1) (MKID V 2) (MKID V 3))) 
(PUT 'DOT 'NUMBER-OF-ARGS 2) 
(PUT 'DOT 'DEFINED-ON-LINE '411) 
(PUT 'DOT 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'DOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOT (F G)
    (REVAL1
     (LIST 'PLUS (LIST 'TIMES (CAR F) (CAR G)) (LIST 'TIMES (CADR F) (CADR G))
           (LIST 'TIMES (CADDR F) (CADDR G)))
     T)) 
(PUT 'CROSS 'NUMBER-OF-ARGS 2) 
(PUT 'CROSS 'DEFINED-ON-LINE '419) 
(PUT 'CROSS 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'CROSS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CROSS (F G)
    (LIST
     (LIST 'DIFFERENCE (LIST 'TIMES (CADR F) (CADDR G))
           (LIST 'TIMES (CADDR F) (CADR G)))
     (LIST 'DIFFERENCE (LIST 'TIMES (CADDR F) (CAR G))
           (LIST 'TIMES (CAR F) (CADDR G)))
     (LIST 'DIFFERENCE (LIST 'TIMES (CAR F) (CADR G))
           (LIST 'TIMES (CADR F) (CAR G))))) 
(PUT 'SPAT 'NUMBER-OF-ARGS 3) 
(PUT 'SPAT 'DEFINED-ON-LINE '427) 
(PUT 'SPAT 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'SPAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPAT (F G H) (DOT F (CROSS G H))) 
(FLAG '(V2C) 'OPFN) 
(PUT 'V2C 'NUMBER-OF-ARGS 1) 
(PUT 'V2C 'DEFINED-ON-LINE '434) 
(PUT 'V2C 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'V2C 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE V2C (A)
    (PROG (P PL PX)
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "v2c start") NIL) (TERPRI))))
      (SETQ A (E2S A))
      (SETQ PL (EXTRACT_PROD A))
      (PROG (P)
        (SETQ P PL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ PX (EXPLODE P))
            (SETQ A
                    (SUBST
                     (COND
                      ((EQUAL (LENGTH PX) 2)
                       (DOT (VC (CAR PX)) (VC (CADR PX))))
                      (T (SPAT (VC (CAR PX)) (VC (CADR PX)) (VC (CADDR PX)))))
                     P A))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "v2c end") NIL) (TERPRI))))
      (RETURN (REVAL1 A T)))) 
(PUT 'ADDVEC 'NUMBER-OF-ARGS 2) 
(PUT 'ADDVEC 'DEFINED-ON-LINE '449) 
(PUT 'ADDVEC 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'ADDVEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDVEC (V VL)
    (PROG (VE)
      (SETQ VE (ASSOC V VL))
      (RETURN
       (COND ((NULL VE) (CONS (CONS V 1) VL))
             (T (CONS (CONS V (ADD1 (CDR VE))) (DELETE VE VL))))))) 
(PUT 'LETTERS_OF 'NUMBER-OF-ARGS 1) 
(PUT 'LETTERS_OF 'DEFINED-ON-LINE '461) 
(PUT 'LETTERS_OF 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'LETTERS_OF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LETTERS_OF (A)
    (PROG (L H EA)
      (SETQ EA (EXPLODE A))
      (COND
       ((AND (NEQ (CAR EA) '&) (NEQ (CAR EA) '&) (NEQ (CAR EA) '!))
        (PROG (H)
          (SETQ H EA)
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (COND
              ((FREEOF '(|0| |1| |2| |3| |4| |5| |6| |7| |8| |9|) H)
               (SETQ L (CONS H L)))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))))
      (RETURN L))) 
(PUT 'GET_VLIST_OF_TERM 'NUMBER-OF-ARGS 1) 
(PUT 'GET_VLIST_OF_TERM 'DEFINED-ON-LINE '476) 
(PUT 'GET_VLIST_OF_TERM 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GET_VLIST_OF_TERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_VLIST_OF_TERM (AT)
    (PROG (F1 V VL VLC H)
      (COND ((AND (PAIRP AT) (EQUAL (CAR AT) 'QUOTIENT)) (SETQ AT (CADR AT))))
      (COND
       ((AND (PAIRP AT) (EQUAL (CAR AT) 'MINUS))
        (PROGN
         (SETQ AT (CADR AT))
         (COND
          ((AND (PAIRP AT) (EQUAL (CAR AT) 'QUOTIENT)) (SETQ AT (CADR AT)))))))
      (COND
       ((AND (PAIRP AT)
             (OR (EQUAL (CAR AT) 'TIMES) (EQUAL (CAR AT) 'LIST)
                 (EQUAL (CAR AT) 'EQUAL)))
        (SETQ AT (CDR AT)))
       (T (SETQ AT (LIST AT))))
      (PROG ()
       WHILELABEL
        (COND ((NOT AT) (RETURN NIL)))
        (PROGN
         (SETQ F1 (CAR AT))
         (SETQ AT (CDR AT))
         (COND
          ((NOT
            (OR (NUMBERP F1)
                (AND (PAIRP F1) (EQUAL (CAR F1) 'QUOTIENT) (NUMBERP (CADR F1))
                     (NUMBERP (CADDR F1)))))
           (COND
            ((ATOM F1)
             (PROG (V)
               (SETQ V (LETTERS_OF F1))
              LAB
               (COND ((NULL V) (RETURN NIL)))
               ((LAMBDA (V) (SETQ VL (ADDVEC V VL))) (CAR V))
               (SETQ V (CDR V))
               (GO LAB)))
            ((NEQ (CAR F1) 'ARBCOMPLEX)
             (COND
              ((NEQ (CAR F1) 'EXPT)
               (PROGN (PRIN2 "****** car not EXPT ******") NIL))
              (T
               (PROG (V)
                 (SETQ V (LETTERS_OF (CADR F1)))
                LAB
                 (COND ((NULL V) (RETURN NIL)))
                 ((LAMBDA (V)
                    (PROG (H)
                      (SETQ H 1)
                     LAB
                      (COND ((MINUSP (DIFFERENCE (CADDR F1) H)) (RETURN NIL)))
                      (SETQ VL (ADDVEC V VL))
                      (SETQ H (PLUS2 H 1))
                      (GO LAB)))
                  (CAR V))
                 (SETQ V (CDR V))
                 (GO LAB)))))))))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT VL) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR VL))
         (PROG (U)
           (SETQ U VL)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U) (COND ((ORDP (CAR V) (CAR U)) (SETQ V U)))) (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (SETQ VLC (CONS V VLC))
         (SETQ VL (DELETE V VL))
         NIL)
        (GO WHILELABEL))
      (RETURN VLC))) 
(PUT 'FILTER_HOM 'NUMBER-OF-ARGS 2) 
(PUT 'FILTER_HOM 'DEFINED-ON-LINE '515) 
(PUT 'FILTER_HOM 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'FILTER_HOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FILTER_HOM (A AT)
    (PROG (VL V H GS)
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "filter_hom start") NIL) (TERPRI))))
      (SETQ VL (GET_VLIST_OF_TERM AT))
      (SETQ GS (GENSYM))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (PROG (H)
              (SETQ H (VC (CAR V)))
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H) (SETQ A (SUBST (LIST 'TIMES H GS) H A))) (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (SETQ A (REVAL1 A T))
            (SETQ A (COEFFN A GS (CDR V)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "filter_hom end") NIL) (TERPRI))))
      (RETURN (CONS A VL)))) 
(PUT 'T1 'NUMBER-OF-ARGS 1) 
(PUT 'T1 'DEFINED-ON-LINE '537) 
(PUT 'T1 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'T1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE T1 (A)
    (COND ((NULL A) 0) ((ATOM A) A)
          ((EQUAL (CAR A) 'QUOTIENT)
           (REVAL1 (LIST 'QUOTIENT (T1 (CADR A)) (CADDR A)) T))
          ((OR (EQUAL (CAR A) 'PLUS) (EQUAL (CAR A) 'DIFFERENCE)) (CADR A))
          (T A))) 
(FLAG '(GENPRO_WG) 'OPFN) 
(PUT 'GENPRO_WG 'NUMBER-OF-ARGS 1) 
(PUT 'GENPRO_WG 'DEFINED-ON-LINE '549) 
(PUT 'GENPRO_WG 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GENPRO_WG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENPRO_WG (VL)
    (CONS 'LIST
          (PROG (G FORALL-RESULT FORALL-ENDPTR)
            (SETQ G
                    (GENPRO_WG_L
                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                       (SETQ H (CDR VL))
                       (COND ((NULL H) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (H) (CDR H)) (CAR H))
                                             NIL)))
                      LOOPLABEL
                       (SETQ H (CDR H))
                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (COND ((NULL G) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (G) (CONS 'LIST G)) (CAR G)) NIL)))
           LOOPLABEL
            (SETQ G (CDR G))
            (COND ((NULL G) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (G) (CONS 'LIST G)) (CAR G)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'ADDVECTOWG 'NUMBER-OF-ARGS 2) 
(PUT 'ADDVECTOWG 'DEFINED-ON-LINE '554) 
(PUT 'ADDVECTOWG 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'ADDVECTOWG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDVECTOWG (WG V)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J 1)
      (COND ((MINUSP (DIFFERENCE (LENGTH WG) J)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS (REVAL1 (LIST 'PLUS (NTH WG J) (NTH V J)) T) NIL)))
     LOOPLABEL
      (SETQ J (PLUS2 J 1))
      (COND ((MINUSP (DIFFERENCE (LENGTH WG) J)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS (REVAL1 (LIST 'PLUS (NTH WG J) (NTH V J)) T) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'GENPRO_WG_L 'NUMBER-OF-ARGS 1) 
(PUT 'GENPRO_WG_L 'DEFINED-ON-LINE '559) 
(PUT 'GENPRO_WG_L 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GENPRO_WG_L 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENPRO_WG_L (VL)
    (COND ((NULL VL) (LIST 'LIST))
          (T
           (PROG (N J K L P2 P3 WG)
             (SETQ WG
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                       (COND
                        ((MINUSP (DIFFERENCE (LENGTH (CDAR VL)) J))
                         (RETURN NIL)))
                       (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                      LOOPLABEL
                       (SETQ J (PLUS2 J 1))
                       (COND
                        ((MINUSP (DIFFERENCE (LENGTH (CDAR VL)) J))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ N (LENGTH VL))
             (SETQ P2
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                      STARTOVER
                       (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K J)
                                 (COND
                                  ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (CONS
                                                   (MKID (CAR (NTH VL J))
                                                         (CAR (NTH VL K)))
                                                   (ADDVECTOWG
                                                    (ADDVECTOWG WG
                                                     (CDR (NTH VL J)))
                                                    (CDR (NTH VL K))))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ K (PLUS2 K 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE N K))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (CONS
                                           (MKID (CAR (NTH VL J))
                                                 (CAR (NTH VL K)))
                                           (ADDVECTOWG
                                            (ADDVECTOWG WG (CDR (NTH VL J)))
                                            (CDR (NTH VL K))))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ J (PLUS2 J 1))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND
                        ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K J)
                                 (COND
                                  ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (CONS
                                                   (MKID (CAR (NTH VL J))
                                                         (CAR (NTH VL K)))
                                                   (ADDVECTOWG
                                                    (ADDVECTOWG WG
                                                     (CDR (NTH VL J)))
                                                    (CDR (NTH VL K))))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ K (PLUS2 K 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE N K))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (CONS
                                           (MKID (CAR (NTH VL J))
                                                 (CAR (NTH VL K)))
                                           (ADDVECTOWG
                                            (ADDVECTOWG WG (CDR (NTH VL J)))
                                            (CDR (NTH VL K))))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ J (PLUS2 J 1))
                       (GO LOOPLABEL)))
             (SETQ P3
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                      STARTOVER
                       (COND
                        ((MINUSP (DIFFERENCE (DIFFERENCE N 2) J))
                         (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K (PLUS J 1))
                                STARTOVER
                                 (COND
                                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K))
                                   (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ L (PLUS K 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (CONS
                                                             (MKID
                                                              (CAR (NTH VL J))
                                                              (MKID
                                                               (CAR (NTH VL K))
                                                               (CAR
                                                                (NTH VL L))))
                                                             (ADDVECTOWG
                                                              (ADDVECTOWG
                                                               (ADDVECTOWG WG
                                                                (CDR
                                                                 (NTH VL J)))
                                                               (CDR
                                                                (NTH VL K)))
                                                              (CDR
                                                               (NTH VL L))))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ L (PLUS2 L 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (CONS
                                                     (MKID (CAR (NTH VL J))
                                                           (MKID
                                                            (CAR (NTH VL K))
                                                            (CAR (NTH VL L))))
                                                     (ADDVECTOWG
                                                      (ADDVECTOWG
                                                       (ADDVECTOWG WG
                                                        (CDR (NTH VL J)))
                                                       (CDR (NTH VL K)))
                                                      (CDR (NTH VL L))))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                 (SETQ K (PLUS2 K 1))
                                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                LOOPLABEL
                                 (COND
                                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ L (PLUS K 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (CONS
                                                             (MKID
                                                              (CAR (NTH VL J))
                                                              (MKID
                                                               (CAR (NTH VL K))
                                                               (CAR
                                                                (NTH VL L))))
                                                             (ADDVECTOWG
                                                              (ADDVECTOWG
                                                               (ADDVECTOWG WG
                                                                (CDR
                                                                 (NTH VL J)))
                                                               (CDR
                                                                (NTH VL K)))
                                                              (CDR
                                                               (NTH VL L))))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ L (PLUS2 L 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (CONS
                                                     (MKID (CAR (NTH VL J))
                                                           (MKID
                                                            (CAR (NTH VL K))
                                                            (CAR (NTH VL L))))
                                                     (ADDVECTOWG
                                                      (ADDVECTOWG
                                                       (ADDVECTOWG WG
                                                        (CDR (NTH VL J)))
                                                       (CDR (NTH VL K)))
                                                      (CDR (NTH VL L))))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                 (SETQ K (PLUS2 K 1))
                                 (GO LOOPLABEL)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ J (PLUS2 J 1))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND
                        ((MINUSP (DIFFERENCE (DIFFERENCE N 2) J))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K (PLUS J 1))
                                STARTOVER
                                 (COND
                                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K))
                                   (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ L (PLUS K 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (CONS
                                                             (MKID
                                                              (CAR (NTH VL J))
                                                              (MKID
                                                               (CAR (NTH VL K))
                                                               (CAR
                                                                (NTH VL L))))
                                                             (ADDVECTOWG
                                                              (ADDVECTOWG
                                                               (ADDVECTOWG WG
                                                                (CDR
                                                                 (NTH VL J)))
                                                               (CDR
                                                                (NTH VL K)))
                                                              (CDR
                                                               (NTH VL L))))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ L (PLUS2 L 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (CONS
                                                     (MKID (CAR (NTH VL J))
                                                           (MKID
                                                            (CAR (NTH VL K))
                                                            (CAR (NTH VL L))))
                                                     (ADDVECTOWG
                                                      (ADDVECTOWG
                                                       (ADDVECTOWG WG
                                                        (CDR (NTH VL J)))
                                                       (CDR (NTH VL K)))
                                                      (CDR (NTH VL L))))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                 (SETQ K (PLUS2 K 1))
                                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                LOOPLABEL
                                 (COND
                                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ L (PLUS K 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (CONS
                                                             (MKID
                                                              (CAR (NTH VL J))
                                                              (MKID
                                                               (CAR (NTH VL K))
                                                               (CAR
                                                                (NTH VL L))))
                                                             (ADDVECTOWG
                                                              (ADDVECTOWG
                                                               (ADDVECTOWG WG
                                                                (CDR
                                                                 (NTH VL J)))
                                                               (CDR
                                                                (NTH VL K)))
                                                              (CDR
                                                               (NTH VL L))))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ L (PLUS2 L 1))
                                           (COND
                                            ((MINUSP (DIFFERENCE N L))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (CONS
                                                     (MKID (CAR (NTH VL J))
                                                           (MKID
                                                            (CAR (NTH VL K))
                                                            (CAR (NTH VL L))))
                                                     (ADDVECTOWG
                                                      (ADDVECTOWG
                                                       (ADDVECTOWG WG
                                                        (CDR (NTH VL J)))
                                                       (CDR (NTH VL K)))
                                                      (CDR (NTH VL L))))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                 (SETQ K (PLUS2 K 1))
                                 (GO LOOPLABEL)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ J (PLUS2 J 1))
                       (GO LOOPLABEL)))
             (RETURN (APPEND P2 P3)))))) 
(PUT 'STD_WG 'NUMBER-OF-ARGS 1) 
(PUT 'STD_WG 'DEFINED-ON-LINE '586) 
(PUT 'STD_WG 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'STD_WG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STD_WG (VL)
    (PROG (H FORALL-RESULT FORALL-ENDPTR)
      (SETQ H 1)
      (COND ((MINUSP (DIFFERENCE (LENGTH VL) H)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       (CONS (NTH VL H)
                             (PROG (K FORALL-RESULT FORALL-ENDPTR)
                               (SETQ K 1)
                               (COND
                                ((MINUSP (DIFFERENCE (LENGTH VL) K))
                                 (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                (COND ((EQUAL H K) 1) (T 0))
                                                NIL)))
                              LOOPLABEL
                               (SETQ K (PLUS2 K 1))
                               (COND
                                ((MINUSP (DIFFERENCE (LENGTH VL) K))
                                 (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS (COND ((EQUAL H K) 1) (T 0)) NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                       NIL)))
     LOOPLABEL
      (SETQ H (PLUS2 H 1))
      (COND ((MINUSP (DIFFERENCE (LENGTH VL) H)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               (CONS (NTH VL H)
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K 1)
                       (COND
                        ((MINUSP (DIFFERENCE (LENGTH VL) K)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS (COND ((EQUAL H K) 1) (T 0))
                                             NIL)))
                      LOOPLABEL
                       (SETQ K (PLUS2 K 1))
                       (COND
                        ((MINUSP (DIFFERENCE (LENGTH VL) K))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS (COND ((EQUAL H K) 1) (T 0)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(FLAG '(GENPRO) 'OPFN) 
(PUT 'GENPRO 'NUMBER-OF-ARGS 1) 
(PUT 'GENPRO 'DEFINED-ON-LINE '594) 
(PUT 'GENPRO 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GENPRO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENPRO (VL)
    (CONS 'LIST
          (PROG (H FORALL-RESULT FORALL-ENDPTR)
            (SETQ H (GENPRO_WG_L (STD_WG (CDR VL))))
            (COND ((NULL H) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (H) (CAR H)) (CAR H)) NIL)))
           LOOPLABEL
            (SETQ H (CDR H))
            (COND ((NULL H) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (H) (CAR H)) (CAR H)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'HC2S 'NUMBER-OF-ARGS 1) 
(PUT 'HC2S 'DEFINED-ON-LINE '600) 
(PUT 'HC2S 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'HC2S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HC2S (AHC)
    (PROG (V VL NF F FL ZRO H SOL ANSATZ)
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "hc2s start") NIL) (TERPRI))))
      (SETQ FINO_ NIL)
      (SETQ WGTHS_
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (CDR AHC))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (CDR V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CDR V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "gen start") NIL) (TERPRI))))
      (GEN
       (GENPRO_WG_L
        (STD_WG
         (PROG (V FORALL-RESULT FORALL-ENDPTR)
           (SETQ V (CDR AHC))
           (COND ((NULL V) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL)))
          LOOPLABEL
           (SETQ V (CDR V))
           (COND ((NULL V) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "gen end") NIL) (TERPRI))))
      (SETQ NF 1)
      (SETQ ANSATZ
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V FINO_)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (PROGN
                                     (SETQ F (MKID '&C NF))
                                     (SETQ NF (ADD1 NF))
                                     (SETQ FL (CONS F FL))
                                     (LIST 'TIMES F V)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ F (MKID '&C NF))
                             (SETQ NF (ADD1 NF))
                             (SETQ FL (CONS F FL))
                             (LIST 'TIMES F V)))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FINO_ NIL)
      (COND
       (TR_VEC
        (PROGN
         (PROGN
          (PRIN2 "The vector ansatz has ")
          (PRIN2 (LENGTH FL))
          (PRIN2 " unknown coefficients.")
          NIL)
         (TERPRI))))
      (COND ((NULL (CDR ANSATZ)) (SETQ ANSATZ (REVAL1 (CAR ANSATZ) T)))
            (T (SETQ ANSATZ (REVAL1 (CONS 'PLUS ANSATZ) T))))
      (SETQ VL
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (CDR AHC))
               STARTOVER
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (V) (VC (CAR V))) (CAR V)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ V (CDR V))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (V) (VC (CAR V))) (CAR V)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ V (CDR V))
                (GO LOOPLABEL)))
      (SETQ ZRO
              (LIST 'LIST
                    (REVAL1 (LIST 'DIFFERENCE (CAR AHC) (V2C ANSATZ)) T)))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (COND
             (TR_VEC
              (PROGN (PROGN (PRIN2 "splitting wrt ") (PRIN2 V) NIL) (TERPRI))))
            (SETQ SOL
                    (CDR
                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                       (SETQ H (GETRLIST (AEVAL ZRO)))
                      STARTOVER
                       (COND ((NULL H) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (H) (AEVAL (LIST 'COEFF H V)))
                                (CAR H)))
                       (SETQ FORALL-ENDPTR
                               (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                       (SETQ H (CDR H))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (GETRLIST
                                ((LAMBDA (H) (AEVAL (LIST 'COEFF H V)))
                                 (CAR H))))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ H (CDR H))
                       (GO LOOPLABEL))))
            (SETQ ZRO NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT SOL) (RETURN NIL)))
              (PROGN
               (COND ((NEQ (CAR SOL) 0) (SETQ ZRO (CONS (CAR SOL) ZRO))))
               (SETQ SOL (CDR SOL))
               NIL)
              (GO WHILELABEL))
            (COND
             (TR_VEC
              (PROGN
               (PROGN
                (PRIN2 "zro has ")
                (PRIN2 (LENGTH ZRO))
                (PRIN2 " conditions.")
                NIL)
               (TERPRI))))
            (SETQ ZRO (CONS 'LIST ZRO))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND
       (TR_VEC
        (PROGN
         (PROGN
          (PRIN2 (DIFFERENCE (LENGTH ZRO) 1))
          (PRIN2 " conditions for ")
          (PRIN2 (LENGTH FL))
          (PRIN2 " unknowns.")
          NIL)
         (TERPRI))))
      (SETQ !ARBINT 0)
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "solveeval start") NIL) (TERPRI))))
      (SETQ SOL (SOLVEEVAL (LIST ZRO (CONS 'LIST FL))))
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "solveeval end") NIL) (TERPRI))))
      (RETURN
       (COND ((NULL (CDR SOL)) NIL)
             (T
              (PROGN
               (SETQ ANSATZ (AEVAL (LIST 'SUB (LIST 'FIRST SOL) ANSATZ)))
               (SETQ ZRO (AEVAL 0))
               (SETQ SOL
                       (PROG (H FORALL-RESULT FORALL-ENDPTR)
                         (SETQ H 1)
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) H))
                           (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          (PROGN
                                           (SETQ V
                                                   (AEVAL*
                                                    (LIST 'COEFFN ANSATZ
                                                          (LIST 'ARBCOMPLEX H)
                                                          1)))
                                           (SETQ ZRO
                                                   (AEVAL*
                                                    (LIST 'PLUS ZRO
                                                          (LIST 'TIMES
                                                                (LIST
                                                                 'ARBCOMPLEX H)
                                                                V))))
                                           (AEVAL* (LIST 'NUM V)))
                                          NIL)))
                        LOOPLABEL
                         (SETQ H
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  H))
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) H))
                           (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  (PROGN
                                   (SETQ V
                                           (AEVAL*
                                            (LIST 'COEFFN ANSATZ
                                                  (LIST 'ARBCOMPLEX H) 1)))
                                   (SETQ ZRO
                                           (AEVAL*
                                            (LIST 'PLUS ZRO
                                                  (LIST 'TIMES
                                                        (LIST 'ARBCOMPLEX H)
                                                        V))))
                                   (AEVAL* (LIST 'NUM V)))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ ANSATZ (AEVAL (LIST 'DIFFERENCE ANSATZ ZRO)))
               (COND
                (T
                 (PROGN
                  (AEVAL (OFF (LIST 'BATCH_MODE)))
                  (PROGN
                   (SETQ PRINT_MORE NIL)
                   (SETQ RECORD_HIST T)
                   (SETQ MAX_GC_SHORT 15)
                   (SETQ SIZE_WATCH T)
                   (SETQ MAX_GC_FAC 4)
                   (SETQ PRINT_ NIL)
                   (SETQ OLD_HISTORY '(L 11 |;| Q 0))
                   NIL)
                  (SETQ SOL
                          (AEVAL
                           (LIST 'CRACK SOL (LIST 'LIST)
                                 (CONS 'LIST (EXTRACT_PROD SOL))
                                 (LIST 'LIST))))
                  (AEVAL 'NIL))))
               (COND (TR_VEC (PROGN (PROGN (PRIN2 "hc2s end") NIL) (TERPRI))))
               (AEVAL (LIST 'CONS ANSATZ (LIST 'FIRST (LIST 'FIRST SOL)))))))))) 
(PUT 'C2S 'NUMBER-OF-ARGS 1) 
(FLAG '(C2S) 'OPFN) 
(PUT 'C2S 'DEFINED-ON-LINE '696) 
(PUT 'C2S 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'C2S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE C2S (A)
    (PROG (N)
      (RETURN
       (PROGN
        (SETQ N (AEVAL 0))
        (PROG (H FORALL-RESULT)
          (SETQ H (GETRLIST (AEVAL (LIST 'C2SL A))))
          (SETQ FORALL-RESULT 0)
         LAB1
          (COND ((NULL H) (RETURN FORALL-RESULT)))
          (SETQ FORALL-RESULT
                  (AEVAL*
                   (LIST 'PLUS
                         ((LAMBDA (H)
                            (AEVAL
                             (LIST 'PLUS (LIST 'FIRST H)
                                   (PROG (G FORALL-RESULT)
                                     (SETQ G (GETRLIST (AEVAL (LIST 'REST H))))
                                     (SETQ FORALL-RESULT 0)
                                    LAB1
                                     (COND ((NULL G) (RETURN FORALL-RESULT)))
                                     (SETQ FORALL-RESULT
                                             (AEVAL*
                                              (LIST 'PLUS
                                                    ((LAMBDA (G)
                                                       (PROGN
                                                        (SETQ N
                                                                (AEVAL
                                                                 (LIST 'PLUS N
                                                                       1)))
                                                        (AEVAL
                                                         (LIST 'TIMES
                                                               (MKID '&C
                                                                     (REVAL1
                                                                      (AEVAL N)
                                                                      T))
                                                               G))))
                                                     (CAR G))
                                                    FORALL-RESULT)))
                                     (SETQ G (CDR G))
                                     (GO LAB1)))))
                          (CAR H))
                         FORALL-RESULT)))
          (SETQ H (CDR H))
          (GO LAB1)))))) 
(FLAG '(C2SL) 'OPFN) 
(PUT 'C2SL 'NUMBER-OF-ARGS 1) 
(PUT 'C2SL 'DEFINED-ON-LINE '710) 
(PUT 'C2SL 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'C2SL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE C2SL (A)
    (PROG (AV AHV AHC AT1 TR_VEC)
      (COND (TR_VEC (PROGN (PROGN (PRIN2 "c2sl start") NIL) (TERPRI))))
      (SETQ TR_VEC NIL)
      (SETQ A (REVAL1 A T))
      (SETQ AV (LIST))
      (SETQ AHV 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NEQ A 0) AHV)) (RETURN NIL)))
        (PROGN
         (SETQ AT1 (T1 A))
         (COND
          (TR_VEC
           (PROGN
            (PROGN (PRIN2 (LENGTH A)) (PRIN2 " terms to vectorize.") NIL)
            (TERPRI))))
         (SETQ AHC (FILTER_HOM A AT1))
         (SETQ AHV (HC2S AHC))
         (COND
          (AHV
           (PROGN
            (SETQ AV (CONS AHV AV))
            (SETQ A (REVAL1 (LIST 'DIFFERENCE A (CAR AHC)) T))))
          (T
           (PROGN
            (PROGN
             (PRIN2 "All the terms with the same homogeneity as the")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "following terms can not be vectorized: ") NIL)
            (MATHPRINT AT1)))))
        (GO WHILELABEL))
      (RETURN
       (COND ((NULL AHV) NIL)
             (T
              (PROGN
               (COND
                (TR_VEC
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "The input expression in component form has been partitioned. Each of")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "the partitions Pi has been converted into vector form and comes with")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "identities Iij of the same homogeneity type to be used to shorten Pi.")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "Everything is returned in the form {{P1,I11,I12,..},{P2,I21,..},..}.")
                   NIL)
                  (TERPRI))))
               (COND (TR_VEC (PROGN (PROGN (PRIN2 "c2sl stop") NIL) (TERPRI))))
               (REVAL1 (CONS 'LIST AV) T))))))) 
(PUT 'ADD_HOM_TERM 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_HOM_TERM 'DEFINED-ON-LINE '755) 
(PUT 'ADD_HOM_TERM 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'ADD_HOM_TERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_HOM_TERM (V AT PL)
    (PROG (PLC)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PL (NEQ (CAAR PL) V))) (RETURN NIL)))
        (PROGN (SETQ PLC (CONS (CAR PL) PLC)) (SETQ PL (CDR PL)))
        (GO WHILELABEL))
      (COND ((NULL PL) (SETQ PLC (CONS (CONS V AT) PLC)))
            (T
             (PROGN
              (SETQ PLC (CONS (CONS V (LIST 'PLUS (CDAR PL) AT)) PLC))
              (SETQ PL (CDR PL))
              (PROG ()
               WHILELABEL
                (COND ((NOT PL) (RETURN NIL)))
                (PROGN (SETQ PLC (CONS (CAR PL) PLC)) (SETQ PL (CDR PL)))
                (GO WHILELABEL)))))
      (RETURN PLC))) 
(PUT 'SHORTVEX 'NUMBER-OF-ARGS 2) 
(PUT 'SHORTVEX 'DEFINED-ON-LINE '772) 
(PUT 'SHORTVEX 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'SHORTVEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SHORTVEX (A D)
    (PROG (AT1 PL SH N WG J K VLI WLI VLC IDTY GS)
      (SETQ A (REVAL1 A T))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ A 0)) (RETURN NIL)))
        (PROGN
         (SETQ AT1 (T1 A))
         (SETQ PL (ADD_HOM_TERM (GET_VLIST_OF_TERM AT1) AT1 PL))
         (SETQ A (REVAL1 (LIST 'DIFFERENCE A AT1) T)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT PL) (RETURN NIL)))
        (PROGN
         (SETQ VLC (CAAR PL))
         (SETQ SH (CDAR PL))
         (SETQ PL (CDR PL))
         (COND
          (D
           (PROGN
            (SETQ N (LENGTH VLC))
            (SETQ K 0)
            (SETQ VLI NIL)
            (SETQ WLI NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT VLC) (RETURN NIL)))
              (PROGN
               (SETQ K (ADD1 K))
               (SETQ WG
                       (APPEND
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J 1)
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE K 1) J))
                            (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                         LOOPLABEL
                          (SETQ J (PLUS2 J 1))
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE K 1) J))
                            (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        (CONS 1
                              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                (SETQ J (PLUS K 1))
                                (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                               LOOPLABEL
                                (SETQ J (PLUS2 J 1))
                                (COND
                                 ((MINUSP (DIFFERENCE N J))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))
               (SETQ VLI (CONS (CONS (CAAR VLC) WG) VLI))
               (SETQ WLI (CONS (CDAR VLC) WLI))
               (SETQ VLC (CDR VLC)))
              (GO WHILELABEL))
            NIL)))
         (ASSGNPRI
          (AEVAL* "==========================================================")
          NIL 'ONLY)
         (PROGN
          (ASSGNPRI (AEVAL* "One partition of input: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL* SH) NIL 'LAST))
         (SETQ IDTY
                 (AEVAL*
                  (LIST 'REST (LIST 'FIRST (LIST 'C2SL (LIST 'V2C (T1 SH)))))))
         (SETQ GS (GENSYM))
         (SETQ IDTY
                 (CONS 'LIST
                       (CONS (REVAL1 (LIST 'DIFFERENCE SH GS) T)
                             (COND
                              (D
                               (APPEND (CDR IDTY)
                                       (GENID (REVERSE VLI) (REVERSE WLI))))
                              (T (CDR IDTY))))))
         (AEVAL* (OFF (LIST 'BATCH_MODE)))
         (SETQ PRINT_MORE NIL)
         (SETQ RECORD_HIST T)
         (SETQ MAX_GC_SHORT 15)
         (SETQ SIZE_WATCH T)
         (SETQ MAX_GC_FAC 4)
         (SETQ PRINT_ NIL)
         (SETQ TR_SHORT T)
         (SETQ OLD_HISTORY '(67 E_1 NIL Q 0))
         (SETQ IDTY
                 (AEVAL*
                  (LIST 'CRACK IDTY (LIST 'LIST)
                        (CONS 'LIST (EXTRACT_PROD IDTY)) (LIST 'LIST))))
         (COND
          ((EQUAL IDTY (LIST 'LIST))
           (PROGN (PROGN (PRIN2 "ERROR 1 in CRACK!") NIL) (TERPRI)))
          (T
           (PROGN
            (SETQ IDTY (CADR (CADR IDTY)))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND IDTY (FREEOF (CAR IDTY) GS))) (RETURN NIL)))
              (SETQ IDTY (CDR IDTY))
              (GO WHILELABEL))
            (COND
             ((NULL IDTY)
              (PROGN (PROGN (PRIN2 "ERROR 2 in CRACK!") NIL) (TERPRI)))
             (T
              (PROGN
               (SETQ IDTY (SOLVEEVAL (LIST (CAR IDTY) (LIST 'LIST GS))))
               (COND
                ((OR (NULL IDTY) (EQUAL IDTY (LIST 'LIST)))
                 (PROGN (PROGN (PRIN2 "ERROR 3 in SOLVE!") NIL) (TERPRI)))
                (T
                 (PROGN
                  (SETQ IDTY (CADDR (CADR IDTY)))
                  (COND
                   ((EQUAL 0 (REVAL1 (REVAL1 (LIST 'DIFFERENCE SH IDTY) T) T))
                    (PROGN (PRIN2 "Partition is unchanged.") NIL))
                   (T
                    (PROGN
                     (PROGN (PRIN2 "shortened expression:") NIL)
                     (MATHPRINT IDTY)))))))))))))
         (SETQ A (AEVAL* (LIST 'PLUS A IDTY)))
         (SETQ IDTY NIL))
        (GO WHILELABEL))
      (RETURN A))) 
(FLAG '(S2S) 'OPFN) 
(PUT 'S2S 'NUMBER-OF-ARGS 1) 
(PUT 'S2S 'DEFINED-ON-LINE '869) 
(PUT 'S2S 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'S2S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE S2S (A) (SHORTVEX A NIL)) 
(FLAG '(S2E) 'OPFN) 
(PUT 'S2E 'NUMBER-OF-ARGS 1) 
(PUT 'S2E 'DEFINED-ON-LINE '874) 
(PUT 'S2E 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'S2E 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE S2E (A) (SHORTVEX A T)) 
(PUT 'POISSON_C 'NUMBER-OF-ARGS 3) 
(FLAG '(POISSON_C) 'OPFN) 
(PUT 'POISSON_C 'DEFINED-ON-LINE '888) 
(PUT 'POISSON_C 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'POISSON_C 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POISSON_C (F G POI_STRUC_MAT)
    (PROG (H FORALL-RESULT)
      (SETQ H (GETRLIST (AEVAL POI_STRUC_MAT)))
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL H) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (AEVAL*
               (LIST 'PLUS
                     ((LAMBDA (H)
                        (AEVAL
                         (LIST 'TIMES
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'DF F (LIST 'FIRST H))
                                           (LIST 'DF G (LIST 'SECOND H)))
                                     (LIST 'TIMES (LIST 'DF G (LIST 'FIRST H))
                                           (LIST 'DF F (LIST 'SECOND H))))
                               (LIST 'THIRD H))))
                      (CAR H))
                     FORALL-RESULT)))
      (SETQ H (CDR H))
      (GO LAB1))) 
(PUT 'POISSON_V 'NUMBER-OF-ARGS 3) 
(FLAG '(POISSON_V) 'OPFN) 
(PUT 'POISSON_V 'DEFINED-ON-LINE '895) 
(PUT 'POISSON_V 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'POISSON_V 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POISSON_V (F G POI_STRUC_MAT)
    (PROG (H R S ALLE)
      (SETQ ALLE (AEVAL (LIST 'REVERSE (LIST 'GENPRO 'V_))))
      (AEVAL (LIST 'TORDER ALLE 'LEX))
      (SETQ !ARBINT 0)
      (SETQ H
              (PROG (R FORALL-RESULT)
                (SETQ R (GETRLIST (AEVAL ALLE)))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               ((LAMBDA (R)
                                  (PROG (S FORALL-RESULT)
                                    (SETQ S (GETRLIST (AEVAL ALLE)))
                                    (SETQ FORALL-RESULT 0)
                                   LAB1
                                    (COND ((NULL S) (RETURN FORALL-RESULT)))
                                    (SETQ FORALL-RESULT
                                            (AEVAL*
                                             (LIST 'PLUS
                                                   ((LAMBDA (S)
                                                      (COND
                                                       ((EVALEQUAL (AEVAL R)
                                                                   (AEVAL S))
                                                        (PROGN
                                                         (SETK (LIST 'POI_ R S)
                                                               (AEVAL 0))
                                                         0))
                                                       (T
                                                        (PROGN
                                                         (COND
                                                          ((AND
                                                            (EVALEQUAL
                                                             (AEVAL
                                                              (LIST 'ARGLENGTH
                                                                    (LIST 'POI_
                                                                          R
                                                                          S)))
                                                             2)
                                                            (EVALEQUAL
                                                             (AEVAL
                                                              (LIST 'PART
                                                                    (LIST 'POI_
                                                                          R S)
                                                                    0))
                                                             (AEVAL 'POI_)))
                                                           (SETK
                                                            (LIST 'POI_ R S)
                                                            (PROG (H
                                                                   FORALL-RESULT)
                                                              (SETQ H
                                                                      (GETRLIST
                                                                       (AEVAL
                                                                        (LIST
                                                                         'C2SL
                                                                         (LIST
                                                                          'POISSON_C
                                                                          (LIST
                                                                           'V2C
                                                                           R)
                                                                          (LIST
                                                                           'V2C
                                                                           S)
                                                                          POI_STRUC_MAT)))))
                                                              (SETQ FORALL-RESULT
                                                                      0)
                                                             LAB1
                                                              (COND
                                                               ((NULL H)
                                                                (RETURN
                                                                 FORALL-RESULT)))
                                                              (SETQ FORALL-RESULT
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'PLUS
                                                                        ((LAMBDA
                                                                             (
                                                                              H)
                                                                           (AEVAL
                                                                            (LIST
                                                                             'FIRST
                                                                             H)))
                                                                         (CAR
                                                                          H))
                                                                        FORALL-RESULT)))
                                                              (SETQ H (CDR H))
                                                              (GO LAB1)))))
                                                         (AEVAL
                                                          (LIST 'TIMES
                                                                (LIST 'POI_ R
                                                                      S)
                                                                (LIST 'DF F R)
                                                                (LIST 'DF G
                                                                      S)))))))
                                                    (CAR S))
                                                   FORALL-RESULT)))
                                    (SETQ S (CDR S))
                                    (GO LAB1)))
                                (CAR R))
                               FORALL-RESULT)))
                (SETQ R (CDR R))
                (GO LAB1)))
      (SETQ R (AEVAL (LIST 'PREDUCE (LIST 'NUM H) 'GBASE_)))
      (RETURN (AEVAL R)))) 
(PUT 'SYMADDWEIGHTS 'NUMBER-OF-ARGS 2) 
(PUT 'SYMADDWEIGHTS 'DEFINED-ON-LINE '942) 
(PUT 'SYMADDWEIGHTS 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'SYMADDWEIGHTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMADDWEIGHTS (VWGHTS EXL)
    (PROG (H G K WG P V N)
      (RETURN
       (PROG (H FORALL-RESULT FORALL-ENDPTR)
         (SETQ H (CDR EXL))
         (COND ((NULL H) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (H)
                             (PROGN
                              (SETQ H (REVAL1 H T))
                              (SETQ G (T1 H))
                              (SETQ K (GET_VLIST_OF_TERM G))
                              (SETQ WG
                                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ P (CDDADR VWGHTS))
                                        (COND ((NULL P) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (P) 0)
                                                          (CAR P))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ P (CDR P))
                                        (COND
                                         ((NULL P) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS ((LAMBDA (P) 0) (CAR P))
                                                      NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                              (PROG ()
                               WHILELABEL
                                (COND ((NOT K) (RETURN NIL)))
                                (PROGN
                                 (SETQ P (CAR K))
                                 (SETQ K (CDR K))
                                 (SETQ V (CDR VWGHTS))
                                 (PROG ()
                                  WHILELABEL
                                   (COND
                                    ((NOT (AND V (NEQ (CADAR V) (CAR P))))
                                     (RETURN NIL)))
                                   (SETQ V (CDR V))
                                   (GO WHILELABEL))
                                 (SETQ V (CDDAR V))
                                 (PROG (N)
                                   (SETQ N 1)
                                  LAB
                                   (COND
                                    ((MINUSP (DIFFERENCE (CDR P) N))
                                     (RETURN NIL)))
                                   (SETQ WG (ADDVECTOWG WG V))
                                   (SETQ N (PLUS2 N 1))
                                   (GO LAB)))
                                (GO WHILELABEL))
                              (CONS H WG)))
                           (CAR H))
                          NIL)))
        LOOPLABEL
         (SETQ H (CDR H))
         (COND ((NULL H) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (H)
                     (PROGN
                      (SETQ H (REVAL1 H T))
                      (SETQ G (T1 H))
                      (SETQ K (GET_VLIST_OF_TERM G))
                      (SETQ WG
                              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                (SETQ P (CDDADR VWGHTS))
                                (COND ((NULL P) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS ((LAMBDA (P) 0) (CAR P))
                                                      NIL)))
                               LOOPLABEL
                                (SETQ P (CDR P))
                                (COND ((NULL P) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS ((LAMBDA (P) 0) (CAR P)) NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT K) (RETURN NIL)))
                        (PROGN
                         (SETQ P (CAR K))
                         (SETQ K (CDR K))
                         (SETQ V (CDR VWGHTS))
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT (AND V (NEQ (CADAR V) (CAR P))))
                             (RETURN NIL)))
                           (SETQ V (CDR V))
                           (GO WHILELABEL))
                         (SETQ V (CDDAR V))
                         (PROG (N)
                           (SETQ N 1)
                          LAB
                           (COND
                            ((MINUSP (DIFFERENCE (CDR P) N)) (RETURN NIL)))
                           (SETQ WG (ADDVECTOWG WG V))
                           (SETQ N (PLUS2 N 1))
                           (GO LAB)))
                        (GO WHILELABEL))
                      (CONS H WG)))
                   (CAR H))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(FLAG '(ADDWEIGHTS) 'OPFN) 
(PUT 'ADDWEIGHTS 'NUMBER-OF-ARGS 2) 
(PUT 'ADDWEIGHTS 'DEFINED-ON-LINE '971) 
(PUT 'ADDWEIGHTS 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'ADDWEIGHTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDWEIGHTS (VWGHTS EXL)
    (CONS 'LIST
          (PROG (H FORALL-RESULT FORALL-ENDPTR)
            (SETQ H (SYMADDWEIGHTS VWGHTS EXL))
            (COND ((NULL H) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (H) (CONS 'LIST H)) (CAR H)) NIL)))
           LOOPLABEL
            (SETQ H (CDR H))
            (COND ((NULL H) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (H) (CONS 'LIST H)) (CAR H)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(SETQ NFCT_ 1) 
(FLAG '(GFI) 'OPFN) 
(PUT 'GFI 'NUMBER-OF-ARGS 4) 
(PUT 'GFI 'DEFINED-ON-LINE '979) 
(PUT 'GFI 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GFI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFI (WGCP ALLE FDEP HEADS)
    (PROG (K G H P RTN DROPPED1 DROPPED2 F FL GT1)
      (SETQ WGTHS_ (CDR WGCP))
      (SETQ FINO_ NIL)
      (GEN
       (PROG (H FORALL-RESULT FORALL-ENDPTR)
         (SETQ H (CDR (REVAL1 (AEVAL (LIST 'GENPRO_WG ALLE)) T)))
         (COND ((NULL H) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL)))
        LOOPLABEL
         (SETQ H (CDR H))
         (COND ((NULL H) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
      (SETQ DROPPED2 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT FINO_) (RETURN NIL)))
        (PROGN
         (SETQ H (REVAL1 (CAR FINO_) T))
         (SETQ FINO_ (CDR FINO_))
         (COND
          ((AEVAL* (LIST 'IS_REDUCEABLE H HEADS))
           (SETQ DROPPED2 (ADD1 DROPPED2)))
          (T (SETQ RTN (CONS H RTN)))))
        (GO WHILELABEL))
      (SETQ FDEP (SYMADDWEIGHTS ALLE FDEP))
      (SETQ DROPPED1 0)
      (SETQ FINO_ NIL)
      (GEN FDEP)
      (PROG (H)
        (SETQ H FINO_)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ H (REVAL1 H T))
            (SETQ H
                    (COND ((NOT (PAIRP H)) (LIST H))
                          ((OR (EQUAL (CAR H) 'PLUS)
                               (EQUAL (CAR H) 'DIFFERENCE))
                           (CDR H))
                          (T (LIST H))))
            (SETQ GT1 NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT H) (RETURN NIL)))
              (PROGN
               (SETQ G (CAR H))
               (SETQ H (CDR H))
               (COND
                ((AND (PAIRP G) (EQUAL (CAR G) 'MINUS)) (SETQ G (CADR G))))
               (COND
                ((AND (PAIRP G) (EQUAL (CAR G) 'QUOTIENT) (NUMBERP (CADDR G)))
                 (SETQ G (CADR G))))
               (COND
                ((AND (PAIRP G) (EQUAL (CAR G) 'TIMES))
                 (PROGN
                  (SETQ P NIL)
                  (PROG (K)
                    (SETQ K (CDR G))
                   LAB
                    (COND ((NULL K) (RETURN NIL)))
                    ((LAMBDA (K)
                       (COND
                        ((OR (NUMBERP K)
                             (AND (PAIRP K) (EQUAL (CAR K) 'QUOTIENT)
                                  (NUMBERP (CADR K)) (NUMBERP (CADDR K))))
                         (SETQ P (CONS K P)))))
                     (CAR K))
                    (SETQ K (CDR K))
                    (GO LAB))
                  (COND
                   (P
                    (PROGN
                     (COND ((CDR P) (SETQ P (CONS 'TIMES P)))
                           (T (SETQ P (CAR P))))
                     (SETQ G (REVAL1 (LIST 'QUOTIENT G P) T))))))))
               (COND
                ((NULL (MEMBER G RTN)) (PROGN (SETQ GT1 NIL) (SETQ H NIL)))
                ((NULL GT1) (SETQ GT1 G))))
              (GO WHILELABEL))
            (COND
             (GT1
              (PROGN
               (SETQ DROPPED1 (ADD1 DROPPED1))
               (SETQ RTN (DELETE GT1 RTN)))))
            NIL))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ FINO_ RTN)
      (SETQ RTN NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT FINO_) (RETURN NIL)))
        (PROGN
         (SETQ H (REVAL1 (CAR FINO_) T))
         (SETQ FINO_ (CDR FINO_))
         (SETQ F (MKID '&R NFCT_))
         (SETQ NFCT_ (ADD1 NFCT_))
         (SETQ FL (CONS F FL))
         (SETQ RTN (CONS (LIST 'TIMES F H) RTN)))
        (GO WHILELABEL))
      (COND
       (TR_VEC
        (PROGN
         (PRIN2 "dropped: ")
         (PRIN2 DROPPED1)
         (PRIN2 "+")
         (PRIN2 DROPPED2)
         (PRIN2 " kept: ")
         (PRIN2 (LENGTH RTN))
         NIL)))
      (RETURN
       (REVAL1
        (CONS 'LIST
              (COND ((NULL RTN) NIL)
                    (T
                     (CONS (COND ((CDR RTN) (CONS 'PLUS RTN)) (T (CAR RTN)))
                           FL))))
        T)))) 
(PUT 'GPI 'NUMBER-OF-ARGS 3) 
(FLAG '(GPI) 'OPFN) 
(PUT 'GPI 'DEFINED-ON-LINE '1070) 
(PUT 'GPI 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'GPI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GPI (WGL VL GBASE)
    (PROG (HH VCL ID FL G H)
      (SETQ HH
              (AEVAL
               (CONS 'LIST
                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                       (SETQ H (STD_WG (CDR VL)))
                       (COND ((NULL H) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (H) (CONS 'LIST H)) (CAR H))
                                        NIL)))
                      LOOPLABEL
                       (SETQ H (CDR H))
                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (H) (CONS 'LIST H)) (CAR H))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ HH (AEVAL (LIST 'GFI WGL HH (LIST 'LIST) (LIST 'LIST))))
      (COND
       ((EVALNEQ (AEVAL HH) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ ID (AEVAL (LIST 'FIRST HH)))
         (SETQ FL (AEVAL (LIST 'REST HH)))
         (SETQ HH (AEVAL (LIST 'LIST (LIST 'V2C ID))))
         (SETQ VCL
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H (GETRLIST (AEVAL VL)))
                  STARTOVER
                   (COND ((NULL H) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (H) (AEVAL (CONS 'LIST (VC (REVAL1 H T)))))
                            (CAR H)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ H (CDR H))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (H)
                               (AEVAL (CONS 'LIST (VC (REVAL1 H T)))))
                             (CAR H))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ H (CDR H))
                   (GO LOOPLABEL)))
         (PROG (G)
           (SETQ G (GETRLIST (AEVAL VCL)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (SETQ HH
                      (PROG (H FORALL-RESULT FORALL-ENDPTR)
                        (SETQ H (GETRLIST (AEVAL HH)))
                       STARTOVER
                        (COND ((NULL H) (RETURN (MAKELIST NIL))))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (H) (AEVAL (LIST 'COEFF H G)))
                                 (CAR H)))
                        (SETQ FORALL-ENDPTR
                                (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                        (SETQ H (CDR H))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL H) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (GETRLIST
                                 ((LAMBDA (H) (AEVAL (LIST 'COEFF H G)))
                                  (CAR H))))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ H (CDR H))
                        (GO LOOPLABEL))))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (SETQ !ARBINT 0)
         (SETQ HH (AEVAL (LIST 'SOLVE HH FL)))
         (SETQ ID (AEVAL (LIST 'SUB (LIST 'FIRST HH) ID)))
         (SETQ ID
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H 1)
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) H))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (AEVAL*
                                     (LIST 'NUM
                                           (LIST 'COEFFN ID
                                                 (LIST 'ARBCOMPLEX H) 1)))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            H))
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* !ARBINT) H))
                     (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (AEVAL*
                             (LIST 'NUM
                                   (LIST 'COEFFN ID (LIST 'ARBCOMPLEX H) 1)))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (H)
           (SETQ H (GETRLIST (AEVAL ID)))
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND
               ((NOT (FIXP (REVALX H)))
                (COND
                 ((EVALEQUAL (AEVAL GBASE) (AEVAL (LIST 'LIST)))
                  (SETQ GBASE (AEVAL (LIST 'LIST H))))
                 (T
                  (PROGN
                   (SETQ H (AEVAL (LIST 'PREDUCE H GBASE)))
                   (COND
                    ((EVALNEQ (AEVAL H) 0)
                     (SETQ GBASE
                             (AEVAL
                              (LIST 'GROEBNER (LIST 'CONS H GBASE))))))))))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB)))))
      (RETURN (AEVAL GBASE)))) 
(PUT 'VINIT 'NUMBER-OF-ARGS 1) 
(FLAG '(VINIT) 'OPFN) 
(PUT 'VINIT 'DEFINED-ON-LINE '1101) 
(PUT 'VINIT 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'VINIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VINIT (ALLE)
    (PROG (I J K L NATBAK)
      (SETK 'V_ (AEVAL ALLE))
      (AEVAL (LIST 'TORDER (LIST 'REVERSE (LIST 'GENPRO 'V_)) 'LEX))
      (SETK 'GBASE_ (AEVAL (LIST 'LIST)))
      (AEVAL (ON (LIST 'GLTBASIS)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 0)
         LAB
          (COND ((MINUSP (DIFFERENCE 3 J)) (RETURN NIL)))
          (PROG (K)
            (SETQ K 0)
           LAB
            (COND ((MINUSP (DIFFERENCE 3 K)) (RETURN NIL)))
            (PROG (L)
              (SETQ L 0)
             LAB
              (COND ((MINUSP (DIFFERENCE 3 L)) (RETURN NIL)))
              (COND
               ((AND (EVALGREATERP (AEVAL* (LIST 'PLUS I J K L)) 0)
                     (EVALLESSP (AEVAL* (LIST 'PLUS I J K L)) 11))
                (SETK 'GBASE_
                      (AEVAL* (LIST 'GPI (LIST 'LIST I J K L) 'V_ 'GBASE_)))))
              (SETQ L (PLUS2 L 1))
              (GO LAB))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETK 'HEADS_ (AEVAL 'GLTP))
      (AEVAL (ON (LIST 'NAT))))) 
(PUT 'WG_LI 'NUMBER-OF-ARGS 2) 
(PUT 'WG_LI 'DEFINED-ON-LINE '1120) 
(PUT 'WG_LI 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'WG_LI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WG_LI (A VLW)
    (PROG (WG AT1 H WL N VLWCP ERRORCD M)
      (SETQ A (REVAL1 A T))
      (COND ((AND (PAIRP A) (EQUAL (CAR A) 'QUOTIENT)) (SETQ A (CADR A))))
      (SETQ A
              (COND ((AND (PAIRP A) (EQUAL (CAR A) 'PLUS)) (CDR A))
                    (T (LIST A))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ AT1 (CAR A))
         (SETQ A (CDR A))
         (SETQ H (GET_VLIST_OF_TERM AT1))
         (SETQ WL
                 (PROG (N FORALL-RESULT FORALL-ENDPTR)
                   (SETQ N 1)
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH (CDAR VLW)) N)) (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                  LOOPLABEL
                   (SETQ N (PLUS2 N 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH (CDAR VLW)) N))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT H) (RETURN NIL)))
           (PROGN
            (SETQ VLWCP VLW)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND VLWCP (NEQ (CAAR VLWCP) (CAAR H)))) (RETURN NIL)))
              (SETQ VLWCP (CDR VLWCP))
              (GO WHILELABEL))
            (COND
             ((NULL VLWCP)
              (PROGN
               (SETQ ERRORCD 1)
               (PROGN
                (PRIN2 "Unspecified vector ")
                (PRIN2 (CAAR H))
                (PRIN2 " found!")
                NIL)
               (TERPRI)))
             (T
              (PROG (M)
                (SETQ M 1)
               LAB
                (COND ((MINUSP (DIFFERENCE (CDAR H) M)) (RETURN NIL)))
                (SETQ WL (ADDVECTOWG WL (CDAR VLWCP)))
                (SETQ M (PLUS2 M 1))
                (GO LAB))))
            (SETQ H (CDR H)))
           (GO WHILELABEL))
         (COND ((NULL WG) (SETQ WG WL))
               ((NEQ WG WL)
                (PROGN
                 (SETQ ERRORCD 2)
                 (PROGN (PRIN2 "Expression is inhomogeneous!") NIL)
                 (TERPRI)))))
        (COND ((NOT (OR (NULL A) ERRORCD)) (GO REPEATLABEL))))
      (RETURN (COND (ERRORCD NIL) (T WG))))) 
(FLAG '(FNC_DEP) 'OPFN) 
(PUT 'FNC_DEP 'NUMBER-OF-ARGS 3) 
(PUT 'FNC_DEP 'DEFINED-ON-LINE '1161) 
(PUT 'FNC_DEP 'DEFINED-IN-FILE 'CRACK/V3TOOLS.RED) 
(PUT 'FNC_DEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FNC_DEP (A LI VLW)
    (PROG (EL H SUBLI1 SUBLI2 G PARA F CND FL ANSATZ PL N)
      (SETQ VLW
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL (CDR VLW))
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (CDR EL)) (CAR EL)) NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (CDR EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ WGTHS_ (WG_LI A VLW))
      (SETQ H
              (PROG (G FORALL-RESULT)
                (SETQ G WGTHS_)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL G) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (G) G) (CAR G)) FORALL-RESULT))
                (SETQ G (CDR G))
                (GO LAB1)))
      (COND ((ZEROP H) (RETURN NIL)))
      (SETQ N 0)
      (PROG (EL)
        (SETQ EL (CDR LI))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ H (GENSYM))
            (SETQ SUBLI1 (CONS (LIST 'EQUAL H EL) SUBLI1))
            (SETQ N (ADD1 N))
            (SETQ SUBLI2 (CONS (LIST 'EQUAL H (MKID 'P_ N)) SUBLI2))
            (SETQ G (WG_LI EL VLW))
            (SETQ PARA (CONS (CONS H G) PARA))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (SETQ SUBLI1 (CONS 'LIST SUBLI1))
      (SETQ SUBLI2 (CONS 'LIST SUBLI2))
      (SETQ FINO_ NIL)
      (GEN PARA)
      (COND ((NULL FINO_) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FINO_) (RETURN NIL)))
        (PROGN
         (SETQ H (REVAL1 (CAR FINO_) T))
         (SETQ FINO_ (CDR FINO_))
         (SETQ F (MKID '&R NFCT_))
         (SETQ NFCT_ (ADD1 NFCT_))
         (SETQ FL (CONS F FL))
         (SETQ ANSATZ (CONS (LIST 'TIMES F H) ANSATZ)))
        (GO WHILELABEL))
      (SETQ FL (CONS 'LIST FL))
      (SETQ ANSATZ (CONS 'PLUS ANSATZ))
      (SETQ CND
              (REVAL1 (LIST 'DIFFERENCE A (AEVAL (LIST 'SUB SUBLI1 ANSATZ)))
                      T))
      (RETURN
       (PROGN
        (SETQ PL (AEVAL (LIST 'REVERSE (LIST 'GENPRO 'V_))))
        (AEVAL (LIST 'TORDER PL 'LEX))
        (SETQ CND (AEVAL (LIST 'LIST (LIST 'PREDUCE CND 'GBASE_))))
        (PROG (G)
          (SETQ G (GETRLIST (AEVAL PL)))
         LAB
          (COND ((NULL G) (RETURN NIL)))
          ((LAMBDA (G)
             (SETQ CND
                     (PROG (H FORALL-RESULT FORALL-ENDPTR)
                       (SETQ H (GETRLIST (AEVAL CND)))
                      STARTOVER
                       (COND ((NULL H) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (H) (AEVAL (LIST 'COEFF H G)))
                                (CAR H)))
                       (SETQ FORALL-ENDPTR
                               (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                       (SETQ H (CDR H))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL H) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (GETRLIST
                                ((LAMBDA (H) (AEVAL (LIST 'COEFF H G)))
                                 (CAR H))))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ H (CDR H))
                       (GO LOOPLABEL))))
           (CAR G))
          (SETQ G (CDR G))
          (GO LAB))
        (SETQ H (AEVAL (LIST 'SOLVE CND FL)))
        (COND
         ((EVALNEQ (AEVAL H) (AEVAL (LIST 'LIST)))
          (PROGN
           (PROGN
            (PROGN
             (PRIN2 "The expression in question is functionally dependent on")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2
              "the list of expressions {p_1,p_2,...} in the following way:")
             NIL)
            NIL)
           (ASSGNPRI
            (AEVAL (LIST 'SUB SUBLI2 (LIST 'SUB (LIST 'FIRST H) ANSATZ))) NIL
            'ONLY)
           (AEVAL 'T)))
         (T (AEVAL 'NIL))))))) 
(SETK 'V_ (AEVAL (LIST 'LIST 'A 'B 'U 'V))) 
(AEVAL (LIST 'TORDER (LIST 'REVERSE (LIST 'GENPRO 'V_)) 'LEX)) 
(SETK 'GBASE_
      (AEVAL
       (LIST 'LIST
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'BB 'UU 'VV))
                   (LIST 'TIMES 'BB (LIST 'EXPT 'UV 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'EXPT 'BU 2) 'VV)
                         (LIST 'TIMES 2 'BU 'BV 'UV))
                   (LIST 'EXPT 'BUV 2) (LIST 'TIMES (LIST 'EXPT 'BV 2) 'UU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AB 'UU 'VV))
                   (LIST 'TIMES 'AB (LIST 'EXPT 'UV 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AU 'BU 'VV)
                         (LIST 'TIMES 'AU 'BV 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AUV 'BUV)
                         (LIST 'TIMES 'AV 'BU 'UV))
                   (LIST 'TIMES 'AV 'BV 'UU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AB 'BU 'VV))
                   (LIST 'TIMES 'AB 'BV 'UV) (LIST 'TIMES 'ABV 'BUV)
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AU 'BB 'VV)
                               (LIST 'TIMES 'AU (LIST 'EXPT 'BV 2)))
                         (LIST 'TIMES 'AV 'BB 'UV))
                   (LIST 'TIMES 'AV 'BU 'BV))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AB 'BU 'UV))
                   (LIST 'TIMES 'AB 'BV 'UU) (LIST 'TIMES 'ABU 'BUV)
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AU 'BB 'UV)
                               (LIST 'TIMES 'AU 'BU 'BV))
                         (LIST 'TIMES 'AV 'BB 'UU))
                   (LIST 'TIMES 'AV (LIST 'EXPT 'BU 2)))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'ABU 'VV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'UV)
                         (LIST 'TIMES 'AUV 'BV))
                   (LIST 'TIMES 'AV 'BUV))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'ABU 'UV))
                   (LIST 'TIMES 'ABV 'UU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AU 'BUV)
                         (LIST 'TIMES 'AUV 'BU)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'BUV)
                         (LIST 'TIMES 'ABU 'BV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'BU)
                         (LIST 'TIMES 'AUV 'BB)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'BUV)
                               (LIST 'TIMES 'AB 'AUV))
                         (LIST 'TIMES 'ABU 'AV))
                   (LIST 'TIMES 'ABV 'AU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'UU 'VV))
                   (LIST 'TIMES 'AA (LIST 'EXPT 'UV 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'EXPT 'AU 2) 'VV)
                         (LIST 'TIMES 2 'AU 'AV 'UV))
                   (LIST 'EXPT 'AUV 2) (LIST 'TIMES (LIST 'EXPT 'AV 2) 'UU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'BU 'VV))
                   (LIST 'TIMES 'AA 'BV 'UV)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'AU 'VV)
                         (LIST 'TIMES 'AB 'AV 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'AUV)
                         (LIST 'TIMES 'AU 'AV 'BV))
                   (LIST 'TIMES (LIST 'EXPT 'AV 2) 'BU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'BU 'UV))
                   (LIST 'TIMES 'AA 'BV 'UU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'AU 'UV)
                         (LIST 'TIMES 'AB 'AV 'UU))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABU 'AUV)
                         (LIST 'TIMES (LIST 'EXPT 'AU 2) 'BV))
                   (LIST 'TIMES 'AU 'AV 'BU))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'ABU 'AU 'VV)
                               (LIST 'TIMES 'ABU 'AV 'UV))
                         (LIST 'TIMES 'ABV 'AU 'UV))
                   (LIST 'TIMES 'ABV 'AV 'UU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AU 'AUV 'BV)
                         (LIST 'TIMES 'AUV 'AV 'BU)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'ABU 'VV)
                         (LIST 'TIMES 'AB 'ABV 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'AUV 'BV)
                         (LIST 'TIMES 'ABU 'AV 'BV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'AV 'BU)
                         (LIST 'TIMES 'AUV 'AV 'BB)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABU 'VV)
                         (LIST 'TIMES 'AA 'ABV 'UV))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'AUV 'BV)
                               (LIST 'TIMES 'AB 'AUV 'AV))
                         (LIST 'TIMES 'ABU (LIST 'EXPT 'AV 2)))
                   (LIST 'TIMES 'ABV 'AU 'AV))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'ABU 'UV)
                         (LIST 'TIMES 'AB 'ABV 'UU))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'AUV 'BU)
                         (LIST 'TIMES 'ABU 'AU 'BV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'AU 'BU)
                         (LIST 'TIMES 'AU 'AUV 'BB)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABU 'UV)
                         (LIST 'TIMES 'AA 'ABV 'UU))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'AUV 'BU)
                               (LIST 'TIMES 'AB 'AU 'AUV))
                         (LIST 'TIMES 'ABU 'AU 'AV))
                   (LIST 'TIMES 'ABV (LIST 'EXPT 'AU 2)))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABU 'BV)
                         (LIST 'TIMES 'AA 'ABV 'BU))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'AUV 'BB)
                               (LIST 'TIMES (LIST 'EXPT 'AB 2) 'AUV))
                         (LIST 'TIMES 'AB 'ABU 'AV))
                   (LIST 'TIMES 'AB 'ABV 'AU))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'BB 'VV))
                   (LIST 'TIMES 'AA (LIST 'EXPT 'BV 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'EXPT 'AB 2) 'VV)
                         (LIST 'TIMES 2 'AB 'AV 'BV))
                   (LIST 'EXPT 'ABV 2) (LIST 'TIMES (LIST 'EXPT 'AV 2) 'BB))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'BB 'UV))
                   (LIST 'TIMES 'AA 'BU 'BV)
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'EXPT 'AB 2) 'UV)
                               (LIST 'TIMES 'AB 'AU 'BV))
                         (LIST 'TIMES 'AB 'AV 'BU))
                   (LIST 'TIMES 'ABU 'ABV) (LIST 'TIMES 'AU 'AV 'BB))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AB 'ABU 'BU 'VV))
                   (LIST 'TIMES 'AB 'ABU 'BV 'UV)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'ABV 'BU 'UV)
                         (LIST 'TIMES 'AB 'ABV 'BV 'UU))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'ABU 'AU 'BB 'VV)
                               (LIST 'TIMES 'ABU 'AU (LIST 'EXPT 'BV 2)))
                         (LIST 'TIMES 'ABU 'AV 'BB 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABU 'AV 'BU 'BV)
                         (LIST 'TIMES 'ABV 'AU 'BB 'UV))
                   (LIST 'TIMES 'ABV 'AU 'BU 'BV)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV 'AV 'BB 'UU)
                         (LIST 'TIMES 'ABV 'AV (LIST 'EXPT 'BU 2))))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'ABU 'BU 'VV))
                   (LIST 'TIMES 'AA 'ABU 'BV 'UV)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABV 'BU 'UV)
                         (LIST 'TIMES 'AA 'ABV 'BV 'UU))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'ABU 'AU 'VV)
                               (LIST 'TIMES 'AB 'ABU 'AV 'UV))
                         (LIST 'TIMES 'AB 'ABV 'AU 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AB 'ABV 'AV 'UU)
                         (LIST 'TIMES 'ABU 'AU 'AV 'BV))
                   (LIST 'TIMES 'ABU (LIST 'EXPT 'AV 2) 'BU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABV (LIST 'EXPT 'AU 2) 'BV)
                         (LIST 'TIMES 'ABV 'AU 'AV 'BU)))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'ABU 'BB 'VV))
                   (LIST 'TIMES 'AA 'ABU (LIST 'EXPT 'BV 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABV 'BB 'UV)
                         (LIST 'TIMES 'AA 'ABV 'BU 'BV))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'EXPT 'AB 2) 'ABU 'VV)
                               (LIST 'TIMES (LIST 'EXPT 'AB 2) 'ABV 'UV))
                         (LIST 'TIMES 2 'AB 'ABU 'AV 'BV))
                   (LIST 'TIMES 'AB 'ABV 'AU 'BV)
                   (LIST 'TIMES 'AB 'ABV 'AV 'BU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABU (LIST 'EXPT 'AV 2) 'BB)
                         (LIST 'TIMES 'ABV 'AU 'AV 'BB)))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'ABU 'BB 'UV))
                   (LIST 'TIMES 'AA 'ABU 'BU 'BV)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'ABV 'BB 'UU)
                         (LIST 'TIMES 'AA 'ABV (LIST 'EXPT 'BU 2)))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'EXPT 'AB 2) 'ABU 'UV)
                                     (LIST 'TIMES (LIST 'EXPT 'AB 2) 'ABV 'UU))
                               (LIST 'TIMES 'AB 'ABU 'AU 'BV))
                         (LIST 'TIMES 'AB 'ABU 'AV 'BU))
                   (LIST 'TIMES 2 'AB 'ABV 'AU 'BU)
                   (LIST 'DIFFERENCE (LIST 'TIMES 'ABU 'AU 'AV 'BB)
                         (LIST 'TIMES 'ABV (LIST 'EXPT 'AU 2) 'BB)))
             (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 'AA 'BB 'UU))
                   (LIST 'TIMES 'AA (LIST 'EXPT 'BU 2))
                   (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'EXPT 'AB 2) 'UU)
                         (LIST 'TIMES 2 'AB 'AU 'BU))
                   (LIST 'EXPT 'ABU 2) (LIST 'TIMES (LIST 'EXPT 'AU 2) 'BB))
             (LIST 'PLUS
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 'AA 'BB 'UU 'VV)
                               (LIST 'TIMES 'AA 'BB (LIST 'EXPT 'UV 2)))
                         (LIST 'TIMES 'AA (LIST 'EXPT 'BU 2) 'VV))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 'AA 'BU 'BV 'UV)
                               (LIST 'TIMES 'AA (LIST 'EXPT 'BV 2) 'UU))
                         (LIST 'TIMES (LIST 'EXPT 'AB 2) 'UU 'VV))
                   (LIST 'TIMES (LIST 'EXPT 'AB 2) (LIST 'EXPT 'UV 2))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 'AB 'AU 'BU 'VV)
                               (LIST 'TIMES 2 'AB 'AU 'BV 'UV))
                         (LIST 'TIMES 2 'AB 'AV 'BU 'UV))
                   (LIST 'DIFFERENCE (LIST 'TIMES 2 'AB 'AV 'BV 'UU)
                         (LIST 'TIMES (LIST 'EXPT 'AU 2) 'BB 'VV))
                   (LIST 'TIMES (LIST 'EXPT 'AU 2) (LIST 'EXPT 'BV 2))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 'AU 'AV 'BB 'UV)
                               (LIST 'TIMES 2 'AU 'AV 'BU 'BV))
                         (LIST 'TIMES (LIST 'EXPT 'AV 2) 'BB 'UU))
                   (LIST 'TIMES (LIST 'EXPT 'AV 2) (LIST 'EXPT 'BU 2)))))) 
(SETK 'HEADS_
      (AEVAL
       (LIST 'LIST (LIST 'EXPT 'BUV 2) (LIST 'TIMES 'AUV 'BUV)
             (LIST 'TIMES 'ABV 'BUV) (LIST 'TIMES 'ABU 'BUV)
             (LIST 'TIMES 'AV 'BUV) (LIST 'TIMES 'AU 'BUV)
             (LIST 'TIMES 'AB 'BUV) (LIST 'TIMES 'AA 'BUV) (LIST 'EXPT 'AUV 2)
             (LIST 'TIMES 'ABV 'AUV) (LIST 'TIMES 'ABU 'AUV)
             (LIST 'TIMES 'AU 'AUV 'BV) (LIST 'TIMES 'AB 'AUV 'BV)
             (LIST 'TIMES 'AA 'AUV 'BV) (LIST 'TIMES 'AB 'AUV 'BU)
             (LIST 'TIMES 'AA 'AUV 'BU) (LIST 'TIMES 'AA 'AUV 'BB)
             (LIST 'EXPT 'ABV 2) (LIST 'TIMES 'ABU 'ABV)
             (LIST 'TIMES 'AB 'ABV 'BU 'UV) (LIST 'TIMES 'AA 'ABV 'BU 'UV)
             (LIST 'TIMES 'AA 'ABV 'BB 'UV) (LIST 'TIMES 'AA 'ABV 'BB 'UU)
             (LIST 'EXPT 'ABU 2) (LIST 'TIMES 'AA 'BB 'UU 'VV)))) 
(AEVAL 'NIL) 
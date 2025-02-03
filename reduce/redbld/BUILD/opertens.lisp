(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OPERTENS)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(FLUID '(OPERTENSNEWIDS*)) 
(PUT 'RESTOREALLDFS 'NUMBER-OF-ARGS 1) 
(PUT 'RESTOREALLDFS 'DEFINED-ON-LINE '44) 
(PUT 'RESTOREALLDFS 'DEFINED-IN-FILE 'ASSIST/OPERTENS.RED) 
(PUT 'RESTOREALLDFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESTOREALLDFS (U)
    (PROG (Y Z W)
      (SETQ Z (FULLCOPY U))
      (SETQ W Z)
     L
      (COND ((OR (ATOM Z) (ATOM (CAR Z))) (RETURN W))
            ((AND (NOT (ATOM (CAAAR Z)))
                  (SETQ Y (GET (CAR (CAAAR Z)) 'TRANSLATE2)))
             (SETCAR (CAAR Z) (*A2K (APPLY1 (CAR Y) (CAAAR Z)))))
            (T (SETCAR (CAAR Z) (*A2K (CAAAR Z)))))
      (SETQ Z (CDAR Z))
      (GO L))) 
(PUT 'CLEARALLNEWIDS 'NUMBER-OF-ARGS 0) 
(PUT 'CLEARALLNEWIDS 'DEFINED-ON-LINE '66) 
(PUT 'CLEARALLNEWIDS 'DEFINED-IN-FILE 'ASSIST/OPERTENS.RED) 
(PUT 'CLEARALLNEWIDS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEARALLNEWIDS NIL
    (PROGN
     (PROG (X)
       (SETQ X OPERTENSNEWIDS*)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (PROGN
           (COND ((FLAGP X 'TENSOR) (REM_TENSOR1 X)) (T (CLEAR (LIST X))))
           (REMPROP X 'TRANSLATE2)))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (SETQ OPERTENSNEWIDS* NIL))) 
(PUT 'DFTYPETOOPER 'NUMBER-OF-ARGS 1) 
(PUT 'DFTYPETOOPER 'DEFINED-ON-LINE '77) 
(PUT 'DFTYPETOOPER 'DEFINED-IN-FILE 'ASSIST/OPERTENS.RED) 
(PUT 'DFTYPETOOPER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DFTYPETOOPER (U)
    (PROG (NAME PROPLIST ARGLIST VARLIST SWITCHID ISTENS SPACEL Z)
      (SETQ NAME (LIST (CAR U)))
      (SETQ PROPLIST NAME)
      (PROG (Y)
        (SETQ Y (CDR U))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (COND
             ((LISTP Y)
              (PROGN
               (SETQ NAME (CONS (CAR Y) (CONS '_ NAME)))
               (COND
                ((FLAGP (CAR Y) 'TENSOR)
                 (PROGN
                  (SETQ ISTENS T)
                  (COND
                   ((AND (NULL *ONESPACE)
                         (NULL
                          (MEMQ (SETQ Z (GET (CAR Y) 'BELONG_TO_SPACE))
                                SPACEL)))
                    (SETQ SPACEL (CONS Z SPACEL))))
                  (COND
                   ((AND (LISTP (CADR Y)) (EQ (CAADR Y) 'LIST))
                    (PROGN
                     (SETQ PROPLIST
                             (CONS
                              (LIST (CAR Y) (DIFFERENCE (LENGTH (CDR Y)) 1)
                                    (DIFFERENCE (LENGTH (CADR Y)) 1))
                              PROPLIST))
                     (SETQ VARLIST (APPEND VARLIST (CDADR Y)))
                     (PROG (Z)
                       (SETQ Z (CDDR Y))
                      LAB
                       (COND ((NULL Z) (RETURN NIL)))
                       ((LAMBDA (Z)
                          (SETQ ARGLIST
                                  (CONS
                                   (PROGN
                                    (COND
                                     (SWITCHID
                                      (COND ((EQCAR Z 'MINUS) (CADR Z))
                                            (T (LIST 'MINUS Z))))
                                     (T Z)))
                                   ARGLIST)))
                        (CAR Z))
                       (SETQ Z (CDR Z))
                       (GO LAB))
                     NIL))
                   (T
                    (PROGN
                     (SETQ PROPLIST
                             (CONS (LIST (CAR Y) (LENGTH (CDR Y))) PROPLIST))
                     (PROG (Z)
                       (SETQ Z (CDR Y))
                      LAB
                       (COND ((NULL Z) (RETURN NIL)))
                       ((LAMBDA (Z)
                          (SETQ ARGLIST
                                  (CONS
                                   (PROGN
                                    (COND
                                     (SWITCHID
                                      (COND ((EQCAR Z 'MINUS) (CADR Z))
                                            (T (LIST 'MINUS Z))))
                                     (T Z)))
                                   ARGLIST)))
                        (CAR Z))
                       (SETQ Z (CDR Z))
                       (GO LAB))
                     NIL)))
                  NIL))
                (T
                 (PROGN
                  (SETQ PROPLIST
                          (CONS (LIST (CAR Y) (LENGTH (CDR Y))) PROPLIST))
                  (SETQ VARLIST (APPEND VARLIST (CDR Y)))
                  NIL)))
               NIL))
             (T
              (PROGN
               (SETQ NAME (CONS Y (CONS '_ NAME)))
               (SETQ PROPLIST (CONS Y PROPLIST))
               NIL)))
            (SETQ SWITCHID T)
            NIL))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ ARGLIST (REVERSE ARGLIST))
      (SETQ PROPLIST (REVERSE PROPLIST))
      (SETQ NAME (|LIST_TO_IDS:| (REVERSE NAME)))
      (COND
       (ISTENS
        (PROGN
         (COND
          ((FLAGP NAME 'TENSOR)
           (PROGN
            (COND
             ((AND (GET NAME 'TRANSLATE2)
                   (NEQ (CDR (GET NAME 'TRANSLATE2)) PROPLIST))
              (RERROR 'CANTENS 13 "problem in number of arg")))))
          (T
           (PROGN
            (MAKE_TENSOR NAME T)
            (INTERN NAME)
            (COND
             ((AND (NULL *ONESPACE) (EQUAL (LENGTH SPACEL) 1))
              (PUT NAME 'BELONG_TO_SPACE (CAR SPACEL))))
            (SETQ OPERTENSNEWIDS* (CONS NAME OPERTENSNEWIDS*))
            (PUT NAME 'TRANSLATE2 (CONS 'OPERTODFTYPE PROPLIST)))))
         (COND (VARLIST (SETQ ARGLIST (CONS (CONS 'LIST VARLIST) ARGLIST))))))
       (T
        (PROGN
         (COND
          ((AND (GET NAME 'TRANSLATE2)
                (NEQ (CDR (GET NAME 'TRANSLATE2)) PROPLIST))
           (RERROR 'CANTENS 13 "problem in number of arg"))
          (T
           (PROGN
            (COND
             ((NULL (EQUAL (GETTYPE NAME) 'OPERATOR))
              (PROGN
               (MKOP NAME)
               (SETQ OPERTENSNEWIDS* (CONS NAME OPERTENSNEWIDS*))
               (INTERN NAME))))
            (PUT NAME 'TRANSLATE2 (CONS 'OPERTODFTYPE PROPLIST))
            (SETQ ARGLIST VARLIST)))))))
      (RETURN (CONS NAME ARGLIST)))) 
(PUT 'OPERTODFTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'OPERTODFTYPE 'DEFINED-ON-LINE '141) 
(PUT 'OPERTODFTYPE 'DEFINED-IN-FILE 'ASSIST/OPERTENS.RED) 
(PUT 'OPERTODFTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPERTODFTYPE (U)
    (PROG (PROPLIST IDSLIST VARLIST ARGRES NAME I SWITCHID Y IDSL VARL)
      (SETQ PROPLIST (CDR (GET (CAR U) 'TRANSLATE2)))
      (SETQ NAME (CAR PROPLIST))
      (SETQ PROPLIST (CDR PROPLIST))
      (SETQ IDSLIST (CDR U))
      (COND
       ((AND (LISTP (CAR IDSLIST)) (EQ (CAAR IDSLIST) 'LIST))
        (PROGN (SETQ VARLIST (CDAR IDSLIST)) (SETQ IDSLIST (CDR IDSLIST)))))
      (COND
       ((FLAGP (CAR U) 'TENSOR)
        (PROG (Y)
          (SETQ Y PROPLIST)
         LAB
          (COND ((NULL Y) (RETURN NIL)))
          ((LAMBDA (Y)
             (PROGN
              (COND
               ((LISTP Y)
                (COND
                 ((FLAGP (CAR Y) 'TENSOR)
                  (PROGN
                   (SETQ IDSL NIL)
                   (PROG (I)
                     (SETQ I 1)
                    LAB
                     (COND ((MINUSP (DIFFERENCE (CADR Y) I)) (RETURN NIL)))
                     (PROGN
                      (SETQ IDSL
                              (CONS
                               (COND
                                (SWITCHID
                                 (COND
                                  ((EQCAR (CAR IDSLIST) 'MINUS)
                                   (CADR (CAR IDSLIST)))
                                  (T (LIST 'MINUS (CAR IDSLIST)))))
                                (T (CAR IDSLIST)))
                               IDSL))
                      (SETQ IDSLIST (CDR IDSLIST))
                      NIL)
                     (SETQ I (PLUS2 I 1))
                     (GO LAB))
                   (SETQ IDSL (REVERSE IDSL))
                   (COND
                    ((CDDR Y)
                     (PROGN
                      (SETQ VARL NIL)
                      (PROG (I)
                        (SETQ I 1)
                       LAB
                        (COND ((MINUSP (DIFFERENCE (CADDR Y) I)) (RETURN NIL)))
                        (PROGN
                         (SETQ VARL (CONS (CAR VARLIST) VARL))
                         (SETQ VARLIST (CDR VARLIST)))
                        (SETQ I (PLUS2 I 1))
                        (GO LAB))
                      (SETQ VARL (REVERSE VARL))
                      (SETQ ARGRES
                              (CONS
                               (CONS (CAR Y) (CONS (CONS 'LIST VARL) IDSL))
                               ARGRES))))
                    (T (SETQ ARGRES (CONS (CONS (CAR Y) IDSL) ARGRES))))
                   NIL))
                 (T
                  (PROGN
                   (SETQ VARL NIL)
                   (PROG (I)
                     (SETQ I 1)
                    LAB
                     (COND ((MINUSP (DIFFERENCE (CADR Y) I)) (RETURN NIL)))
                     (PROGN
                      (SETQ VARL (CONS (CAR VARLIST) VARL))
                      (SETQ VARLIST (CDR VARLIST)))
                     (SETQ I (PLUS2 I 1))
                     (GO LAB))
                   (SETQ VARL (REVERSE VARL))
                   (SETQ ARGRES (CONS (CONS (CAR Y) VARL) ARGRES))))))
               (T (SETQ ARGRES (CONS Y ARGRES))))
              (SETQ SWITCHID T)
              NIL))
           (CAR Y))
          (SETQ Y (CDR Y))
          (GO LAB)))
       (T
        (PROGN
         (PROG (Y)
           (SETQ Y PROPLIST)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (COND
               ((LISTP Y)
                (PROGN
                 (SETQ VARL NIL)
                 (PROG (I)
                   (SETQ I 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE (CADR Y) I)) (RETURN NIL)))
                   (PROGN
                    (SETQ VARL (CONS (CAR IDSLIST) VARL))
                    (SETQ IDSLIST (CDR IDSLIST)))
                   (SETQ I (PLUS2 I 1))
                   (GO LAB))
                 (SETQ VARL (REVERSE VARL))
                 (SETQ ARGRES (CONS (CONS (CAR Y) VARL) ARGRES))))
               (T (SETQ ARGRES (CONS Y ARGRES)))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         NIL)))
      (RETURN (CONS NAME (REVERSE ARGRES))))) 
(PUT 'MAKEDFPERM 'NUMBER-OF-ARGS 0) 
(PUT 'MAKEDFPERM 'DEFINED-ON-LINE '193) 
(PUT 'MAKEDFPERM 'DEFINED-IN-FILE 'ASSIST/OPERTENS.RED) 
(PUT 'MAKEDFPERM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MAKEDFPERM NIL (PUT 'DF 'TRANSLATE1 'DFTYPETOOPER)) 
(FLAG '(MAKEDFPERM) 'OPFN) 
(DEFLIST '((MAKEDFPERM ENDSTAT)) 'STAT) 
(MAKEDFPERM) 
(ENDMODULE) 
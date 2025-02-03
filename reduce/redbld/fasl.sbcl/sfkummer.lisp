(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFKUMMER)) 
(IMPORTS
 (LIST 'COMPLEX*ON*SWITCH 'COMPLEX*OFF*SWITCH 'COMPLEX*RESTORE*SWITCH 'SQ2BF*)) 
(EXPORTS (LIST 'KUMMERM*CALC)) 
(AEVAL (OPERATOR (LIST 'KUMMERM 'KUMMERU))) 
(FLAG '(KUMMERM*CALC) 'OPFN) 
(FLAG '(KUMMERM KUMMERU) 'SPECFN) 
(DEFLIST '((KUMMERM 3) (KUMMERU 3)) 'NUMBER-OF-ARGS) 
(SETK 'KUMMER*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'KUMMERU (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 'PI
                                     (LIST 'SIN (LIST 'TIMES 'PI 'B)))
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT (LIST 'KUMMERM 'A 'B 'Z)
                                           (LIST 'TIMES
                                                 (LIST 'GAMMA
                                                       (LIST 'PLUS 1
                                                             (LIST 'DIFFERENCE
                                                                   'A 'B)))
                                                 (LIST 'GAMMA 'B)))
                                     (LIST 'TIMES
                                           (LIST 'EXPT 'Z
                                                 (LIST 'DIFFERENCE 1 'B))
                                           (LIST 'QUOTIENT
                                                 (LIST 'KUMMERM
                                                       (LIST 'PLUS 1
                                                             (LIST 'DIFFERENCE
                                                                   'A 'B))
                                                       (LIST 'DIFFERENCE 2 'B)
                                                       'Z)
                                                 (LIST 'TIMES (LIST 'GAMMA 'A)
                                                       (LIST 'GAMMA
                                                             (LIST 'DIFFERENCE
                                                                   2 'B)))))))
                         (LIST 'AND (LIST 'NUMBERP 'B)
                               (LIST 'OR (LIST 'NEQ (LIST 'IMPART 'B) 0)
                                     (LIST 'NEQ 'B (LIST 'FLOOR 'B)))
                               (LIST 'NUMBERP 'A)
                               (LIST 'OR (LIST 'NEQ (LIST 'IMPART 'A) 0)
                                     (LIST 'NEQ 'A (LIST 'FLOOR 'A))
                                     (LIST 'GREATERP 'A 0))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'Z 0)
                                           (LIST 'LESSP
                                                 (LIST 'REPART
                                                       (LIST 'DIFFERENCE 1 'B))
                                                 0)))
                               (LIST 'OR
                                     (LIST 'NEQ (LIST 'DIFFERENCE 'A 'B)
                                           (LIST 'FLOOR
                                                 (LIST 'REPART
                                                       (LIST 'DIFFERENCE 'A
                                                             'B))))
                                     (LIST 'GREATERP (LIST 'DIFFERENCE 'A 'B)
                                           (LIST 'MINUS 1))))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERU (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 'PI
                                     (LIST 'SIN (LIST 'TIMES 'PI 'B)))
                               (LIST 'MINUS
                                     (LIST 'TIMES
                                           (LIST 'EXPT 'Z
                                                 (LIST 'DIFFERENCE 1 'B))
                                           (LIST 'QUOTIENT
                                                 (LIST 'KUMMERM
                                                       (LIST 'PLUS 1
                                                             (LIST 'DIFFERENCE
                                                                   'A 'B))
                                                       (LIST 'DIFFERENCE 2 'B)
                                                       'Z)
                                                 (LIST 'TIMES (LIST 'GAMMA 'A)
                                                       (LIST 'GAMMA
                                                             (LIST 'DIFFERENCE
                                                                   2 'B)))))))
                         (LIST 'AND (LIST 'NUMBERP 'B)
                               (LIST 'OR (LIST 'NEQ (LIST 'IMPART 'B) 0)
                                     (LIST 'NEQ 'B (LIST 'FLOOR 'B)))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'Z 0)
                                           (LIST 'LESSP
                                                 (LIST 'REPART
                                                       (LIST 'DIFFERENCE 1 'B))
                                                 0)))
                               (LIST 'NUMBERP 'A)
                               (LIST 'OR (LIST 'NEQ (LIST 'IMPART 'A) 0)
                                     (LIST 'NEQ 'A (LIST 'FLOOR 'A))
                                     (LIST 'GREATERP 'A 0)))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'A) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'EXP 'Z)
                         (LIST 'NOT
                               (LIST 'AND (LIST 'FIXP 'A) (LIST 'LEQ 'A 0)))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES 2
                                           (LIST 'EXP (LIST 'QUOTIENT 'Z 2)))
                                     'Z)
                               (LIST 'SINH (LIST 'QUOTIENT 'Z 2)))
                         (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'NUMBERP 'Z) (LIST 'EQUAL 'A 1)
                               (LIST 'EQUAL 'B 2)
                               (LIST 'EQUAL (LIST 'IMPART 'Z) 0)
                               (LIST 'NEQ 'Z 0))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'MINUS
                                           (LIST 'TIMES 2 'I
                                                 (LIST 'EXP
                                                       (LIST 'QUOTIENT 'Z 2))))
                                     'Z)
                               (LIST 'SIN
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT 'Z
                                                 (LIST 'TIMES 2 'I)))))
                         (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'NUMBERP 'Z) (LIST 'EQUAL 'A 1)
                               (LIST 'EQUAL 'B 2)
                               (LIST 'EQUAL (LIST 'REPART 'Z) 0)
                               (LIST 'NEQ 'Z 0))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN 'INFINITY
                         (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'EQUAL (LIST 'IMPART 'B) 0)
                               (LIST 'LESSP 'B 0)
                               (LIST 'EQUAL 'B (LIST 'FLOOR 'B))
                               (LIST 'NOT
                                     (LIST 'AND
                                           (LIST 'EQUAL (LIST 'IMPART 'A) 0)
                                           (LIST 'LESSP 'A 0)
                                           (LIST 'EQUAL 'A (LIST 'FLOOR 'A))
                                           (LIST 'GEQ 'A 'B))))))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'DO*KUMMERM 'A 'B 'Z)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'NUMBERP 'Z) (LIST 'NEQ 'B 0)
                               (LIST 'EQUAL (LIST 'IMPART 'A) 0)
                               (LIST 'EQUAL (LIST 'IMPART 'B) 0)
                               (LIST 'EQUAL (LIST 'IMPART 'Z) 0)
                               (LIST 'NOT
                                     (LIST 'AND
                                           (LIST 'EQUAL (LIST 'REPART 'B)
                                                 (LIST 'FLOOR
                                                       (LIST 'REPART 'B)))
                                           (LIST 'EQUAL (LIST 'REPART 'A)
                                                 (LIST 'FLOOR
                                                       (LIST 'REPART 'A)))
                                           (LIST 'LESSP (LIST 'REPART 'A) 0)
                                           (LIST 'LESSP (LIST 'REPART 'B) 0)
                                           (LIST 'GEQ (LIST 'REPART 'A)
                                                 (LIST 'REPART 'B)))))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                         'Z)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 'Z)
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'DIFFERENCE 'B 'A)
                                     (LIST 'KUMMERM (LIST 'DIFFERENCE 'A 1) 'B
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE (LIST 'DIFFERENCE 'B 'A)
                                           'Z)
                                     (LIST 'KUMMERM 'A 'B 'Z)))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'KUMMERU (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                         'Z)
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'KUMMERU (LIST 'DIFFERENCE 'A 1) 'B
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'PLUS (LIST 'DIFFERENCE 'A 'B) 'Z)
                                     (LIST 'KUMMERU 'A 'B 'Z)))
                         'Z))))) 
(AEVAL (LET '(KUMMER*RULES))) 
(PUT 'DO*KUMMERM 'NUMBER-OF-ARGS 3) 
(FLAG '(DO*KUMMERM) 'OPFN) 
(PUT 'DO*KUMMERM 'DEFINED-ON-LINE '115) 
(PUT 'DO*KUMMERM 'DEFINED-IN-FILE 'SPECFN/SFKUMMER.RED) 
(PUT 'DO*KUMMERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DO*KUMMERM (A B Z) (SF*EVAL 'KUMMERM*CALC (LIST 'LIST A B Z))) 
(PUT 'KUMMERM*CALC 'NUMBER-OF-ARGS 3) 
(FLAG '(KUMMERM*CALC) 'OPFN) 
(PUT 'KUMMERM*CALC 'DEFINED-ON-LINE '119) 
(PUT 'KUMMERM*CALC 'DEFINED-IN-FILE 'SPECFN/SFKUMMER.RED) 
(PUT 'KUMMERM*CALC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE KUMMERM*CALC (A B Z)
    (PROG (A0 B0 Z0 RESULT ALGLIST* PREPRE PRECOM)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PRECOM 0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 1))))
       (T (AEVAL (LIST 'PRECISION (PLUS PREPRE 2)))))
      (SETQ A0 (AEVAL A))
      (SETQ B0 (AEVAL B))
      (SETQ Z0 (AEVAL Z))
      (SETQ RESULT (AEVAL (KUMMERM*CALC*SUB A0 B0 Z0)))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'KUMMERM*CALC*SUB 'NUMBER-OF-ARGS 3) 
(PUT 'KUMMERM*CALC*SUB 'DEFINED-ON-LINE '135) 
(PUT 'KUMMERM*CALC*SUB 'DEFINED-IN-FILE 'SPECFN/SFKUMMER.RED) 
(PUT 'KUMMERM*CALC*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE KUMMERM*CALC*SUB (A B Z)
    (PROG (RESULT THIS ADMISSABLE PAMOD PBMOD RP ORDA K)
      (SETQ RP 0)
      (SETQ ORDA 0)
      (SETQ K 0)
      (SETQ A
              (COND ((FIXP A) (CONS '|:RD:| (CONS A 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* A))))))
      (SETQ B
              (COND ((FIXP B) (CONS '|:RD:| (CONS B 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* B))))))
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ RESULT BFONE*)
      (SETQ K 1)
      (SETQ PAMOD (NORMBF (|ROUND:MT| (|TIMES:| A Z) |:BPREC:|)))
      (SETQ PBMOD B)
      (SETQ ADMISSABLE
              (NORMBF
               (|DIVIDE:| BFONE*
                          (CONS '|:RD:| (CONS (EXPT 2 (PLUS 5 |:BPREC:|)) 0))
                          |:BPREC:|)))
      (SETQ ORDA (DIFFERENCE (|ORDER:| ADMISSABLE) 5))
      (SETQ THIS BFONE*)
      (SETQ RP |:BPREC:|)
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ THIS
                 (|DIVIDE:| (|TIMES:| THIS PAMOD)
                            (|TIMES:| PBMOD (CONS '|:RD:| (CONS K 0))) RP))
         (SETQ RP (DIFFERENCE (|ORDER:| THIS) ORDA))
         (SETQ RESULT (|PLUS:| RESULT THIS))
         (SETQ K (PLUS K 1))
         (SETQ PAMOD (|PLUS:| PAMOD Z))
         (SETQ PBMOD (|PLUS:| PBMOD BFONE*))
         NIL)
        (GO WHILELABEL))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(ENDMODULE) 
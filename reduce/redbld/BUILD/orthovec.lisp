(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ORTHOVEC)) 
(CREATE-PACKAGE '(ORTHOVEC) '(CONTRIB AVECTOR)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'VSTART0 'NUMBER-OF-ARGS 0) 
(FLAG '(VSTART0) 'OPFN) 
(PUT 'VSTART0 'DEFINED-ON-LINE '88) 
(PUT 'VSTART0 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VSTART0 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE VSTART0 NIL
    (PROG (CTYPE)
      (ASSGNPRI (AEVAL "Select Coordinate System by number") NIL 'ONLY)
      (ASSGNPRI (AEVAL "1] cartesian") NIL 'ONLY)
      (ASSGNPRI (AEVAL "2] cylindrical") NIL 'ONLY)
      (ASSGNPRI (AEVAL "3] spherical") NIL 'ONLY)
      (ASSGNPRI (AEVAL "4] general") NIL 'ONLY)
      (ASSGNPRI (AEVAL "5] others") NIL 'ONLY)
      (AEVAL (CLEAR (LIST 'U1 'U2 'U3 'H1 'H2 'H3)))
      (AEVAL (DEPEND (LIST 'H1 'U1 'U2 'U3)))
      (AEVAL (DEPEND (LIST 'H2 'U1 'U2 'U3)))
      (AEVAL (DEPEND (LIST 'H3 'U1 'U2 'U3)))
      (AEVAL (NODEPEND (LIST 'H1 'U1 'U2 'U3)))
      (AEVAL (NODEPEND (LIST 'H2 'U1 'U2 'U3)))
      (AEVAL (NODEPEND (LIST 'H3 'U1 'U2 'U3)))
      (SETQ CTYPE (AEVAL (READ)))
      (COND
       ((EVALEQUAL (AEVAL CTYPE) 1)
        (PROGN
         (SETK 'U1 (AEVAL 'X))
         (SETK 'U2 (AEVAL 'Y))
         (SETK 'U3 (AEVAL 'Z))
         (SETK 'H1 (AEVAL 1))
         (SETK 'H2 (AEVAL 1))
         (SETK 'H3 (AEVAL 1))))
       ((EVALEQUAL (AEVAL CTYPE) 2)
        (PROGN
         (SETK 'U1 (AEVAL 'R))
         (SETK 'U2 (AEVAL 'TH))
         (SETK 'U3 (AEVAL 'Z))
         (SETK 'H1 (AEVAL 1))
         (SETK 'H2 (AEVAL 'R))
         (SETK 'H3 (AEVAL 1))))
       ((EVALEQUAL (AEVAL CTYPE) 3)
        (PROGN
         (SETK 'U1 (AEVAL 'R))
         (SETK 'U2 (AEVAL 'TH))
         (SETK 'U3 (AEVAL 'PH))
         (SETK 'H1 (AEVAL 1))
         (SETK 'H2 (AEVAL 'R))
         (SETK 'H3 (AEVAL (LIST 'TIMES 'R (LIST 'SIN 'TH))))))
       ((EVALEQUAL (AEVAL CTYPE) 4)
        (PROGN
         (AEVAL (DEPEND (LIST 'H1 'U1 'U2 'U3)))
         (AEVAL (DEPEND (LIST 'H2 'U1 'U2 'U3)))
         (AEVAL (DEPEND (LIST 'H3 'U1 'U2 'U3)))))
       (T
        (PROGN
         (ASSGNPRI (AEVAL "To define another coordinate system, give values ")
                   NIL 'ONLY)
         (ASSGNPRI (AEVAL "to components u1,u2,u3 and give functional form or")
                   NIL 'ONLY)
         (ASSGNPRI
          (AEVAL "DEPEND for scale factors h1,h2 and h3. For example,") NIL
          'ONLY)
         (ASSGNPRI (AEVAL "to set up paraboloidal coords u,v,w type in:-") NIL
                   'ONLY)
         (ASSGNPRI
          (AEVAL "u1:=u;u2:=v;u3:=w;h1:=sqrt(u**2+v**2);h2:=h1;h3:=u*v;") NIL
          'ONLY))))
      (PROGN
       (ASSGNPRI (AEVAL "coordinate type = ") NIL 'FIRST)
       (ASSGNPRI (AEVAL CTYPE) NIL 'LAST))
      (PROGN
       (ASSGNPRI (AEVAL "coordinates = ") NIL 'FIRST)
       (ASSGNPRI (AEVAL 'U1) NIL NIL)
       (ASSGNPRI (AEVAL ",") NIL NIL)
       (ASSGNPRI (AEVAL 'U2) NIL NIL)
       (ASSGNPRI (AEVAL ",") NIL NIL)
       (ASSGNPRI (AEVAL 'U3) NIL 'LAST))
      (PROGN
       (ASSGNPRI (AEVAL "scale factors = ") NIL 'FIRST)
       (ASSGNPRI (AEVAL 'H1) NIL NIL)
       (ASSGNPRI (AEVAL ",") NIL NIL)
       (ASSGNPRI (AEVAL 'H2) NIL NIL)
       (ASSGNPRI (AEVAL ",") NIL NIL)
       (ASSGNPRI (AEVAL 'H3) NIL 'LAST))
      (RETURN (AEVAL 'NIL)))) 
(LET '((EQUAL VSTART (VSTART0)))) 
(FLAG '(GETV) 'OPFN) 
(FLAG '(VECTORP) 'DIRECT) 
(FLAG '(VECTORP) 'BOOLEAN) 
(PUT 'SVEC 'NUMBER-OF-ARGS 3) 
(PUT 'SVEC 'DEFINED-ON-LINE '141) 
(PUT 'SVEC 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'SVEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SVEC (C1 C2 C3)
    (PROG (A)
      (SETQ A (MKVECT 2))
      (PUTV A 0 C1)
      (PUTV A 1 C2)
      (PUTV A 2 C3)
      (RETURN (REVAL1 A NIL)))) 
(FLAG '(SVEC) 'OPFN) 
(PUT 'VOUT 'NUMBER-OF-ARGS 1) 
(FLAG '(VOUT) 'OPFN) 
(PUT 'VOUT 'DEFINED-ON-LINE '167) 
(PUT 'VOUT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VOUT (V)
    (PROG ()
      (COND
       ((VECTORP (REVALX V))
        (PROG (J)
          (SETQ J 0)
         LAB
          (COND ((MINUSP (DIFFERENCE 2 J)) (RETURN NIL)))
          (PROGN
           (ASSGNPRI (AEVAL* "[") NIL 'FIRST)
           (ASSGNPRI (PLUS J 1) NIL NIL)
           (ASSGNPRI (AEVAL* "] ") NIL NIL)
           (ASSGNPRI (AEVAL* (LIST 'GETV V J)) NIL 'LAST))
          (SETQ J (PLUS2 J 1))
          (GO LAB)))
       (T (ASSGNPRI (AEVAL V) NIL 'ONLY)))
      (RETURN (AEVAL V)))) 
(REMFLAG '(DEPEND NODEPEND) 'LOSE) 
(PUT 'DEPEND 'NUMBER-OF-ARGS 1) 
(PUT 'DEPEND 'DEFINED-ON-LINE '182) 
(PUT 'DEPEND 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DEPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEPEND (U)
    (PROG (V W)
      (SETQ V (*A2K (CAR U)))
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((VECTORP V)
             (PROG (IC)
               (SETQ IC 0)
              LAB
               (COND ((MINUSP (DIFFERENCE (UPBV V) IC)) (RETURN NIL)))
               (PROGN
                (COND
                 ((AND (ATOM (SETQ W (GETV V IC))) (NOT (NUMBERP W)))
                  (DEPEND1 W X T))))
               (SETQ IC (PLUS2 IC 1))
               (GO LAB)))
            (T (DEPEND1 (CAR U) X T))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'NODEPEND 'NUMBER-OF-ARGS 1) 
(PUT 'NODEPEND 'DEFINED-ON-LINE '194) 
(PUT 'NODEPEND 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'NODEPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NODEPEND (U)
    (PROG (V W)
      (RMSUBS)
      (SETQ V (*A2K (CAR U)))
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((VECTORP V)
             (PROG (IC)
               (SETQ IC 0)
              LAB
               (COND ((MINUSP (DIFFERENCE (UPBV V) IC)) (RETURN NIL)))
               (PROGN
                (COND
                 ((AND (ATOM (SETQ W (GETV V IC))) (NOT (NUMBERP W)))
                  (DEPEND1 W X NIL))))
               (SETQ IC (PLUS2 IC 1))
               (GO LAB)))
            (T (DEPEND1 (CAR U) X NIL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(NEWTOK '((+) VECTORADD)) 
(NEWTOK '((-) VECTORDIFFERENCE)) 
(NEWTOK '((> <) VECTORCROSS)) 
(NEWTOK '((*) VECTORTIMES)) 
(NEWTOK '((/) VECTORQUOTIENT)) 
(NEWTOK '((_) VECTORCOMPONENT)) 
(NEWTOK '((^) VECTOREXPT)) 
(OPERATOR (LIST 'VECTORMINUS 'VECTORPLUS 'VECTORRECIP)) 
(INFIX
 (LIST 'VECTORADD 'VECTORDIFFERENCE 'VECTORCROSS 'VECTOREXPT 'VECTORCOMPONENT
       'VECTORTIMES 'VECTORQUOTIENT 'DOTGRAD)) 
(PRECEDENCE (LIST 'VECTORADD 'LESSP)) 
(PRECEDENCE (LIST 'VECTORDIFFERENCE 'VECTORADD)) 
(PRECEDENCE (LIST 'DOTGRAD 'VECTORDIFFERENCE)) 
(PRECEDENCE (LIST 'VECTORTIMES 'DOTGRAD)) 
(PRECEDENCE (LIST 'VECTORCROSS 'VECTORTIMES)) 
(PRECEDENCE (LIST 'VECTORQUOTIENT 'VECTORCROSS)) 
(PRECEDENCE (LIST 'VECTOREXPT 'VECTORQUOTIENT)) 
(PRECEDENCE (LIST 'VECTORCOMPONENT 'VECTOREXPT)) 
(DEFLIST
 '((VECTORDIFFERENCE VECTORMINUS) (VECTORADD VECTORPLUS)
   (VECTORQUOTIENT VECTORRECIP) (VECTORRECIP VECTORRECIP))
 'UNARY) 
(DEFLIST '((VECTORMINUS VECTORPLUS) (VECTORRECIP VECTORTIMES)) 'ALT) 
(PUT 'VECTORCOMPONENT 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORCOMPONENT) 'OPFN) 
(PUT 'VECTORCOMPONENT 'DEFINED-ON-LINE '247) 
(PUT 'VECTORCOMPONENT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORCOMPONENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORCOMPONENT (V IC)
    (COND
     ((VECTORP (REVALX V))
      (COND
       ((OR (EVALEQUAL (AEVAL IC) 1) (EVALEQUAL (AEVAL IC) 2)
            (EVALEQUAL (AEVAL IC) 3))
        (AEVAL (LIST 'GETV V (LIST 'VECTORDIFFERENCE IC 1))))
       (T (AEVAL (RERROR 'ORTHOVEC 1 (REVALX "Incorrect component number"))))))
     (T (AEVAL (RERROR 'ORTHOVEC 2 (REVALX "Not a vector")))))) 
(PUT 'VECTORADD 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORADD) 'OPFN) 
(PUT 'VECTORADD 'DEFINED-ON-LINE '256) 
(PUT 'VECTORADD 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORADD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORADD (V1 V2)
    (COND
     ((AND (VECTORP (REVALX V1)) (VECTORP (REVALX V2)))
      (AEVAL
       (LIST 'SVEC (LIST 'PLUS (LIST 'GETV V1 0) (LIST 'GETV V2 0))
             (LIST 'PLUS (LIST 'GETV V1 1) (LIST 'GETV V2 1))
             (LIST 'PLUS (LIST 'GETV V1 2) (LIST 'GETV V2 2)))))
     ((AND (NOT (VECTORP (REVALX V1))) (NOT (VECTORP (REVALX V2))))
      (AEVAL (LIST 'PLUS V1 V2)))
     (T (AEVAL (RERROR 'ORTHOVEC 3 (REVALX "Incorrect args to vector add")))))) 
(PUT 'VECTORPLUS 'NUMBER-OF-ARGS 1) 
(FLAG '(VECTORPLUS) 'OPFN) 
(PUT 'VECTORPLUS 'DEFINED-ON-LINE '269) 
(PUT 'VECTORPLUS 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORPLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORPLUS (V) V) 
(PUT 'VECTORMINUS 'NUMBER-OF-ARGS 1) 
(FLAG '(VECTORMINUS) 'OPFN) 
(PUT 'VECTORMINUS 'DEFINED-ON-LINE '273) 
(PUT 'VECTORMINUS 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORMINUS (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'SVEC (LIST 'MINUS (LIST 'GETV V 0))
             (LIST 'MINUS (LIST 'GETV V 1)) (LIST 'MINUS (LIST 'GETV V 2)))))
     (T (AEVAL (LIST 'MINUS V))))) 
(PUT 'VECTORDIFFERENCE 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORDIFFERENCE) 'OPFN) 
(PUT 'VECTORDIFFERENCE 'DEFINED-ON-LINE '280) 
(PUT 'VECTORDIFFERENCE 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORDIFFERENCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORDIFFERENCE (V1 V2) (LIST 'VECTORADD V1 (LIST 'VECTORMINUS V2))) 
(PUT 'VECTORTIMES 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORTIMES) 'OPFN) 
(PUT 'VECTORTIMES 'DEFINED-ON-LINE '285) 
(PUT 'VECTORTIMES 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORTIMES (V1 V2)
    (COND
     ((AND (VECTORP (REVALX V1)) (VECTORP (REVALX V2)))
      (PROG (IC FORALL-RESULT)
        (SETQ IC 0)
        (SETQ FORALL-RESULT 0)
       LAB1
        (COND ((MINUSP (DIFFERENCE 2 IC)) (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT
                (AEVAL*
                 (LIST 'PLUS
                       (AEVAL*
                        (LIST 'TIMES (LIST 'GETV V1 IC) (LIST 'GETV V2 IC)))
                       FORALL-RESULT)))
        (SETQ IC (PLUS2 IC 1))
        (GO LAB1)))
     ((AND (NOT (VECTORP (REVALX V1))) (NOT (VECTORP (REVALX V2))))
      (AEVAL (LIST 'TIMES V1 V2)))
     ((AND (VECTORP (REVALX V1)) (NOT (VECTORP (REVALX V2))))
      (AEVAL
       (LIST 'SVEC (LIST 'TIMES (LIST 'GETV V1 0) V2)
             (LIST 'TIMES (LIST 'GETV V1 1) V2)
             (LIST 'TIMES (LIST 'GETV V1 2) V2))))
     (T
      (AEVAL
       (LIST 'SVEC (LIST 'TIMES (LIST 'GETV V2 0) V1)
             (LIST 'TIMES (LIST 'GETV V2 1) V1)
             (LIST 'TIMES (LIST 'GETV V2 2) V1)))))) 
(PUT 'VECTORCROSS 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORCROSS) 'OPFN) 
(PUT 'VECTORCROSS 'DEFINED-ON-LINE '296) 
(PUT 'VECTORCROSS 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORCROSS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORCROSS (V1 V2)
    (COND
     ((AND (VECTORP (REVALX V1)) (VECTORP (REVALX V2)))
      (AEVAL
       (LIST 'SVEC
             (LIST 'PLUS (LIST 'TIMES (LIST 'GETV V1 1) (LIST 'GETV V2 2))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'GETV V1 2) (LIST 'GETV V2 1))))
             (LIST 'PLUS (LIST 'TIMES (LIST 'GETV V1 2) (LIST 'GETV V2 0))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'GETV V1 0) (LIST 'GETV V2 2))))
             (LIST 'PLUS (LIST 'TIMES (LIST 'GETV V1 0) (LIST 'GETV V2 1))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'GETV V1 1) (LIST 'GETV V2 0)))))))
     (T
      (AEVAL
       (RERROR 'ORTHOVEC 4
               (REVALX "Incorrect args to vector cross product")))))) 
(PUT 'VECTORQUOTIENT 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTORQUOTIENT) 'OPFN) 
(PUT 'VECTORQUOTIENT 'DEFINED-ON-LINE '305) 
(PUT 'VECTORQUOTIENT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTORQUOTIENT (V1 V2)
    (COND
     ((VECTORP (REVALX V1))
      (COND
       ((VECTORP (REVALX V2))
        (AEVAL
         (LIST 'QUOTIENT (LIST 'VECTORTIMES V1 V2) (LIST 'VECTORTIMES V2 V2))))
       (T (AEVAL (LIST 'VECTORTIMES V1 (LIST 'RECIP V2))))))
     ((VECTORP (REVALX V2))
      (AEVAL
       (LIST 'VECTORTIMES V1
             (LIST 'VECTORTIMES V2 (LIST 'RECIP (LIST 'TIMES V2 V2))))))
     (T (AEVAL (LIST 'QUOTIENT V1 V2))))) 
(PUT 'VECTORRECIP 'NUMBER-OF-ARGS 1) 
(FLAG '(VECTORRECIP) 'OPFN) 
(PUT 'VECTORRECIP 'DEFINED-ON-LINE '312) 
(PUT 'VECTORRECIP 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTORRECIP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORRECIP (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL (LIST 'VECTORTIMES V (LIST 'RECIP (LIST 'VECTORTIMES V V)))))
     (T (AEVAL (LIST 'RECIP V))))) 
(PUT 'VMOD 'NUMBER-OF-ARGS 1) 
(FLAG '(VMOD) 'OPFN) 
(PUT 'VMOD 'DEFINED-ON-LINE '318) 
(PUT 'VMOD 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VMOD (V) (LIST 'SQRT (LIST 'VECTORTIMES V V))) 
(PUT 'VECTOREXPT 'NUMBER-OF-ARGS 2) 
(FLAG '(VECTOREXPT) 'OPFN) 
(PUT 'VECTOREXPT 'DEFINED-ON-LINE '323) 
(PUT 'VECTOREXPT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VECTOREXPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VECTOREXPT (V N)
    (COND ((VECTORP (REVALX V)) (AEVAL (LIST 'EXPT (LIST 'VMOD V) N)))
          (T (AEVAL (LIST 'EXPT V N))))) 
(PUT 'DIV 'NUMBER-OF-ARGS 1) 
(FLAG '(DIV) 'OPFN) 
(PUT 'DIV 'DEFINED-ON-LINE '333) 
(PUT 'DIV 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DIV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIV (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'VECTORQUOTIENT
             (LIST 'VECTORQUOTIENT
                   (LIST 'VECTORQUOTIENT
                         (LIST 'VECTORADD
                               (LIST 'DF (LIST 'TIMES 'H2 'H3 (LIST 'GETV V 0))
                                     'U1)
                               (LIST 'VECTORADD
                                     (LIST 'DF
                                           (LIST 'TIMES 'H3 'H1
                                                 (LIST 'GETV V 1))
                                           'U2)
                                     (LIST 'DF
                                           (LIST 'TIMES 'H1 'H2
                                                 (LIST 'GETV V 2))
                                           'U3)))
                         'H1)
                   'H2)
             'H3)))
     (T (AEVAL (RERROR 'ORTHOVEC 5 (REVALX "Incorrect arguments to div")))))) 
(PUT 'GRAD 'NUMBER-OF-ARGS 1) 
(FLAG '(GRAD) 'OPFN) 
(PUT 'GRAD 'DEFINED-ON-LINE '345) 
(PUT 'GRAD 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'GRAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRAD (S)
    (COND
     ((NOT (VECTORP (REVALX S)))
      (AEVAL
       (LIST 'SVEC (LIST 'QUOTIENT (LIST 'DF S 'U1) 'H1)
             (LIST 'QUOTIENT (LIST 'DF S 'U2) 'H2)
             (LIST 'QUOTIENT (LIST 'DF S 'U3) 'H3))))
     (T (AEVAL (RERROR 'ORTHOVEC 6 (REVALX "Incorrect argument to grad")))))) 
(PUT 'CURL 'NUMBER-OF-ARGS 1) 
(FLAG '(CURL) 'OPFN) 
(PUT 'CURL 'DEFINED-ON-LINE '354) 
(PUT 'CURL 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'CURL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CURL (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'SVEC
             (LIST 'QUOTIENT
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'DF (LIST 'TIMES 'H3 (LIST 'GETV V 2))
                                     'U2)
                               (LIST 'MINUS
                                     (LIST 'DF
                                           (LIST 'TIMES 'H2 (LIST 'GETV V 1))
                                           'U3)))
                         'H2)
                   'H3)
             (LIST 'QUOTIENT
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'DF (LIST 'TIMES 'H1 (LIST 'GETV V 0))
                                     'U3)
                               (LIST 'MINUS
                                     (LIST 'DF
                                           (LIST 'TIMES 'H3 (LIST 'GETV V 2))
                                           'U1)))
                         'H3)
                   'H1)
             (LIST 'QUOTIENT
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'DF (LIST 'TIMES 'H2 (LIST 'GETV V 1))
                                     'U1)
                               (LIST 'MINUS
                                     (LIST 'DF
                                           (LIST 'TIMES 'H1 (LIST 'GETV V 0))
                                           'U2)))
                         'H1)
                   'H2))))
     (T (AEVAL (RERROR 'ORTHOVEC 7 (REVALX "Incorrect argument to curl")))))) 
(PUT 'DELSQ 'NUMBER-OF-ARGS 1) 
(FLAG '(DELSQ) 'OPFN) 
(PUT 'DELSQ 'DEFINED-ON-LINE '366) 
(PUT 'DELSQ 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DELSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELSQ (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'VECTORADD (LIST 'GRAD (LIST 'DIV V))
             (LIST 'VECTORMINUS (LIST 'CURL (LIST 'CURL V))))))
     (T (AEVAL (LIST 'DIV (LIST 'GRAD V)))))) 
(PUT 'VDF 'NUMBER-OF-ARGS 2) 
(FLAG '(VDF) 'OPFN) 
(PUT 'VDF 'DEFINED-ON-LINE '373) 
(PUT 'VDF 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VDF (V X)
    (COND
     ((VECTORP (REVALX X))
      (AEVAL
       (RERROR 'ORTHOVEC 8 (REVALX "Second argument to VDF must be scalar"))))
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'SVEC (LIST 'VDF (LIST 'GETV V 0) X)
             (LIST 'VDF (LIST 'GETV V 1) X) (LIST 'VDF (LIST 'GETV V 2) X))))
     (T (AEVAL (LIST 'DF V X))))) 
(PUT 'DOTGRAD 'NUMBER-OF-ARGS 2) 
(FLAG '(DOTGRAD) 'OPFN) 
(PUT 'DOTGRAD 'DEFINED-ON-LINE '382) 
(PUT 'DOTGRAD 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DOTGRAD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOTGRAD (V1 V2)
    (COND
     ((VECTORP (REVALX V1))
      (COND
       ((VECTORP (REVALX V2))
        (AEVAL
         (LIST 'VECTORTIMES (LIST 'QUOTIENT 1 2)
               (LIST 'VECTORADD (LIST 'GRAD (LIST 'VECTORTIMES V1 V2))
                     (LIST 'VECTORADD (LIST 'VECTORTIMES V1 (LIST 'DIV V2))
                           (LIST 'VECTORADD
                                 (LIST 'VECTORMINUS
                                       (LIST 'VECTORTIMES (LIST 'DIV V1) V2))
                                 (LIST 'VECTORMINUS
                                       (LIST 'VECTORADD
                                             (LIST 'CURL
                                                   (LIST 'VECTORCROSS V1 V2))
                                             (LIST 'VECTORADD
                                                   (LIST 'VECTORCROSS V1
                                                         (LIST 'CURL V2))
                                                   (LIST 'VECTORMINUS
                                                         (LIST 'VECTORCROSS
                                                               (LIST 'CURL V1)
                                                               V2)))))))))))
       (T (AEVAL (LIST 'TIMES V1 (LIST 'GRAD V2))))))
     (T
      (AEVAL (RERROR 'ORTHOVEC 9 (REVALX "Incorrect arguments to dotgrad")))))) 
(PUT 'VTAYLOR 'NUMBER-OF-ARGS 4) 
(FLAG '(VTAYLOR) 'OPFN) 
(PUT 'VTAYLOR 'DEFINED-ON-LINE '399) 
(PUT 'VTAYLOR 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VTAYLOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VTAYLOR (VEX VX VPT VORDER)
    (COND
     ((VECTORP (REVALX VEX))
      (AEVAL
       (LIST 'SVEC (LIST 'VPTAYLOR (LIST 'GETV VEX 0) VX VPT VORDER)
             (LIST 'VPTAYLOR (LIST 'GETV VEX 1) VX VPT VORDER)
             (LIST 'VPTAYLOR (LIST 'GETV VEX 2) VX VPT VORDER))))
     (T (AEVAL (LIST 'VPTAYLOR VEX VX VPT VORDER))))) 
(PUT 'VPTAYLOR 'NUMBER-OF-ARGS 4) 
(FLAG '(VPTAYLOR) 'OPFN) 
(PUT 'VPTAYLOR 'DEFINED-ON-LINE '410) 
(PUT 'VPTAYLOR 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VPTAYLOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VPTAYLOR (SEX VX VPT VORDER)
    (COND
     ((VECTORP (REVALX VX))
      (COND
       ((VECTORP (REVALX VPT))
        (COND
         ((VECTORP (REVALX VORDER))
          (AEVAL
           (LIST 'OV-TAYLOR
                 (LIST 'OV-TAYLOR
                       (LIST 'OV-TAYLOR SEX (LIST 'GETV VX 0)
                             (LIST 'GETV VPT 0) (LIST 'GETV VORDER 0))
                       (LIST 'GETV VX 1) (LIST 'GETV VPT 1)
                       (LIST 'GETV VORDER 1))
                 (LIST 'GETV VX 2) (LIST 'GETV VPT 2) (LIST 'GETV VORDER 2))))
         (T
          (AEVAL
           (LIST 'OV-TAYLOR
                 (LIST 'OV-TAYLOR
                       (LIST 'OV-TAYLOR SEX (LIST 'GETV VX 0)
                             (LIST 'GETV VPT 0) VORDER)
                       (LIST 'GETV VX 1) (LIST 'GETV VPT 1) VORDER)
                 (LIST 'GETV VX 2) (LIST 'GETV VPT 2) VORDER)))))
       (T
        (AEVAL
         (RERROR 'ORTHOVEC 10
                 (REVALX "VTAYLOR: vector VX mismatches scalar VPT"))))))
     ((VECTORP (REVALX VPT))
      (AEVAL
       (RERROR 'ORTHOVEC 11
               (REVALX "VTAYLOR: scalar VX mismatches vector VPT"))))
     ((VECTORP (REVALX VORDER))
      (AEVAL
       (RERROR 'ORTHOVEC 12
               (REVALX "VTAYLOR: scalar VX mismatches vector VORDER"))))
     (T (AEVAL (LIST 'OV-TAYLOR SEX VX VPT VORDER))))) 
(PUT 'OV-TAYLOR 'NUMBER-OF-ARGS 4) 
(FLAG '(OV-TAYLOR) 'OPFN) 
(PUT 'OV-TAYLOR 'DEFINED-ON-LINE '437) 
(PUT 'OV-TAYLOR 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'OV-TAYLOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OV-TAYLOR (EX X PT N)
    (PROG (TERM SERIES DX MFAC)
      (COND
       ((EVALNUMBERP (AEVAL N))
        (PROGN
         (SETQ MFAC (AEVAL 1))
         (SETQ DX (AEVAL (LIST 'VECTORDIFFERENCE X PT)))
         (SETQ TERM (AEVAL EX))
         (SETQ SERIES
                 (AEVAL
                  (LIST 'PLUS (LIST 'OV_LIMIT EX X PT)
                        (PROG (K FORALL-RESULT)
                          (SETQ K 1)
                          (SETQ FORALL-RESULT 0)
                         LAB1
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                            (RETURN FORALL-RESULT)))
                          (SETQ FORALL-RESULT
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (AEVAL*
                                          (LIST 'TIMES
                                                (LIST 'OV_LIMIT
                                                      (SETQ TERM
                                                              (AEVAL*
                                                               (LIST 'DF TERM
                                                                     X)))
                                                      X PT)
                                                (SETQ MFAC
                                                        (AEVAL*
                                                         (LIST 'QUOTIENT
                                                               (LIST 'TIMES
                                                                     MFAC DX)
                                                               K)))))
                                         FORALL-RESULT)))
                          (SETQ K
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   K))
                          (GO LAB1)))))))
       (T
        (AEVAL
         (RERROR 'ORTHOVEC 13
                 (REVALX
                  "Truncation orders of Taylor series must be integers")))))
      (RETURN (AEVAL SERIES)))) 
(PUT 'OV_LIMIT 'NUMBER-OF-ARGS 3) 
(FLAG '(OV_LIMIT) 'OPFN) 
(PUT 'OV_LIMIT 'DEFINED-ON-LINE '455) 
(PUT 'OV_LIMIT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'OV_LIMIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OV_LIMIT (EX X PT)
    (PROG (DENEX NUMEX)
      (COND
       ((EVALEQUAL (SETQ DENEX (AEVAL (LIST 'DEN EX))) 1)
        (RETURN (AEVAL (LIST 'SUB (LIST 'EQUAL X PT) EX))))
       ((EVALEQUAL (AEVAL (LIST 'SUB (LIST 'EQUAL X PT) DENEX)) 0)
        (PROGN
         (COND
          ((EVALEQUAL
            (AEVAL
             (LIST 'SUB (LIST 'EQUAL X PT)
                   (SETQ NUMEX (AEVAL (LIST 'NUM EX)))))
            0)
           (RETURN
            (AEVAL
             (LIST 'OV_LIMIT
                   (LIST 'QUOTIENT (LIST 'DF NUMEX X) (LIST 'DF DENEX X)) X
                   PT))))
          (T
           (AEVAL
            (RERROR 'ORTHOVEC 14
                    (REVALX "Singular coefficient found by LIMIT")))))))
       (T (RETURN (AEVAL (LIST 'SUB (LIST 'EQUAL X PT) EX))))))) 
(PUT 'VINT 'NUMBER-OF-ARGS 2) 
(FLAG '(VINT) 'OPFN) 
(PUT 'VINT 'DEFINED-ON-LINE '476) 
(PUT 'VINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VINT (V X)
    (COND
     ((VECTORP (REVALX X))
      (AEVAL
       (RERROR 'ORTHOVEC 15
               (REVALX "Second argument to VINT must be scalar"))))
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'SVEC (LIST 'INT (LIST 'GETV V 0) X)
             (LIST 'INT (LIST 'GETV V 1) X) (LIST 'INT (LIST 'GETV V 2) X))))
     (T (AEVAL (LIST 'INT V X))))) 
(PUT 'DVINT 'NUMBER-OF-ARGS 4) 
(FLAG '(DVINT) 'OPFN) 
(PUT 'DVINT 'DEFINED-ON-LINE '487) 
(PUT 'DVINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DVINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DVINT (V X XLB XUB)
    (COND
     ((OR (VECTORP (REVALX XLB)) (VECTORP (REVALX XUB)))
      (AEVAL (RERROR 'ORTHOVEC 16 (REVALX "Limits to DVINT must be scalar"))))
     ((VECTORP (REVALX V))
      (PROG (I0 I1 I2)
        (SETQ I0 (AEVAL (LIST 'INT (LIST 'GETV V 0) X)))
        (SETQ I1 (AEVAL (LIST 'INT (LIST 'GETV V 1) X)))
        (SETQ I2 (AEVAL (LIST 'INT (LIST 'GETV V 2) X)))
        (RETURN
         (AEVAL
          (LIST 'SVEC
                (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL X XUB) I0)
                      (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL X XLB) I0)))
                (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL X XUB) I1)
                      (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL X XLB) I1)))
                (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL X XUB) I2)
                      (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL X XLB) I2))))))))
     (T
      (PROG (II)
        (SETQ II (AEVAL (LIST 'INT V X)))
        (RETURN
         (AEVAL
          (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL X XUB) II)
                (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL X XLB) II))))))))) 
(PUT 'VOLINT 'NUMBER-OF-ARGS 1) 
(FLAG '(VOLINT) 'OPFN) 
(PUT 'VOLINT 'DEFINED-ON-LINE '507) 
(PUT 'VOLINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'VOLINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VOLINT (V)
    (COND
     ((VECTORP (REVALX V))
      (AEVAL
       (LIST 'SVEC (LIST 'VOLINT (LIST 'GETV V 0))
             (LIST 'VOLINT (LIST 'GETV V 1)) (LIST 'VOLINT (LIST 'GETV V 2)))))
     (T
      (AEVAL
       (LIST 'INT (LIST 'INT (LIST 'INT (LIST 'TIMES V 'H1 'H2 'H3) 'U1) 'U2)
             'U3))))) 
(PUT 'DVOLINT 'NUMBER-OF-ARGS 4) 
(FLAG '(DVOLINT) 'OPFN) 
(PUT 'DVOLINT 'DEFINED-ON-LINE '516) 
(PUT 'DVOLINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DVOLINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DVOLINT (V VLB VUB N)
    (COND
     ((AND (VECTORP (REVALX VLB)) (VECTORP (REVALX VUB)))
      (PROG (II)
        (SETQ II (AEVAL (LIST 'TIMES (LIST 'TIMES 'H1 'H2 'H3) V)))
        (COND
         ((EVALEQUAL (AEVAL N) 1)
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U1 (LIST 'GETV VLB 0)
                              (LIST 'GETV VUB 0))
                        'U2 (LIST 'GETV VLB 1) (LIST 'GETV VUB 1))
                  'U3 (LIST 'GETV VLB 2) (LIST 'GETV VUB 2)))))
         ((EVALEQUAL (AEVAL N) 2)
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U3 (LIST 'GETV VLB 2)
                              (LIST 'GETV VUB 2))
                        'U1 (LIST 'GETV VLB 0) (LIST 'GETV VUB 0))
                  'U2 (LIST 'GETV VLB 1) (LIST 'GETV VUB 1)))))
         ((EVALEQUAL (AEVAL N) 3)
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U2 (LIST 'GETV VLB 1)
                              (LIST 'GETV VUB 1))
                        'U3 (LIST 'GETV VLB 2) (LIST 'GETV VUB 2))
                  'U1 (LIST 'GETV VLB 0) (LIST 'GETV VUB 0)))))
         ((EVALEQUAL (AEVAL N) 4)
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U1 (LIST 'GETV VLB 0)
                              (LIST 'GETV VUB 0))
                        'U3 (LIST 'GETV VLB 2) (LIST 'GETV VUB 2))
                  'U2 (LIST 'GETV VLB 1) (LIST 'GETV VUB 1)))))
         ((EVALEQUAL (AEVAL N) 5)
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U2 (LIST 'GETV VLB 1)
                              (LIST 'GETV VUB 1))
                        'U1 (LIST 'GETV VLB 0) (LIST 'GETV VUB 0))
                  'U3 (LIST 'GETV VLB 2) (LIST 'GETV VUB 2)))))
         (T
          (RETURN
           (AEVAL
            (LIST 'DVINT
                  (LIST 'DVINT
                        (LIST 'DVINT II 'U3 (LIST 'GETV VLB 2)
                              (LIST 'GETV VUB 2))
                        'U2 (LIST 'GETV VLB 1) (LIST 'GETV VUB 1))
                  'U1 (LIST 'GETV VLB 0) (LIST 'GETV VUB 0))))))))
     (T
      (AEVAL
       (RERROR 'ORTHOVEC 17 (REVALX "Bounds to DVOLINT must be vectors")))))) 
(PUT 'LINEINT 'NUMBER-OF-ARGS 3) 
(FLAG '(LINEINT) 'OPFN) 
(PUT 'LINEINT 'DEFINED-ON-LINE '556) 
(PUT 'LINEINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'LINEINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LINEINT (V VLINE TT)
    (COND
     ((AND (VECTORP (REVALX V)) (VECTORP (REVALX VLINE))
           (NOT (VECTORP (REVALX TT))))
      (AEVAL
       (LIST 'INT
             (LIST 'SUB (LIST 'EQUAL 'U1 (LIST 'GETV VLINE 0))
                   (LIST 'EQUAL 'U2 (LIST 'GETV VLINE 1))
                   (LIST 'EQUAL 'U3 (LIST 'GETV VLINE 2))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'GETV V 0)
                               (LIST 'DF (LIST 'GETV VLINE 0) TT) 'H1)
                         (LIST 'TIMES (LIST 'GETV V 1)
                               (LIST 'DF (LIST 'GETV VLINE 1) TT) 'H2)
                         (LIST 'TIMES (LIST 'GETV V 2)
                               (LIST 'DF (LIST 'GETV VLINE 2) TT) 'H3)))
             TT)))
     (T
      (AEVAL (RERROR 'ORTHOVEC 18 (REVALX "Incorrect arguments to LINEINT")))))) 
(PUT 'DLINEINT 'NUMBER-OF-ARGS 5) 
(FLAG '(DLINEINT) 'OPFN) 
(PUT 'DLINEINT 'DEFINED-ON-LINE '566) 
(PUT 'DLINEINT 'DEFINED-IN-FILE 'ORTHOVEC/ORTHOVEC.RED) 
(PUT 'DLINEINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DLINEINT (V VLINE TT TLB TUB)
    (COND
     ((OR (VECTORP (REVALX TLB)) (VECTORP (REVALX TUB)))
      (AEVAL
       (RERROR 'ORTHOVEC 19 (REVALX "Limits to DLINEINT must be scalar"))))
     (T
      (PROG (II)
        (SETQ II (AEVAL (LIST 'LINEINT V VLINE TT)))
        (RETURN
         (AEVAL
          (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL TT TUB) II)
                (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL TT TLB) II))))))))) 
(SETK 'CTYPE (AEVAL 1)) 
(SETK 'U1 (AEVAL 'X)) 
(SETK 'U2 (AEVAL 'Y)) 
(SETK 'U3 (AEVAL 'Z)) 
(SETK 'H1 (AEVAL 1)) 
(SETK 'H2 (AEVAL 1)) 
(SETK 'H3 (AEVAL 1)) 
(ENDMODULE) 
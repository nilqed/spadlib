(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INDXPRIN)) 
(FLUID
 '(*NAT *NERO *REVPRI OBRKP* ORIG* PLINE* POSN* YCOORD* YMAX* YMIN* FANCY-POS*
   FANCY-LINE*)) 
(GLOBAL '(*ERAISE SPARE*)) 
(PUT 'INDVARPRT 'NUMBER-OF-ARGS 1) 
(PUT 'INDVARPRT 'DEFINED-ON-LINE '35) 
(PUT 'INDVARPRT 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'INDVARPRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDVARPRT (U)
    (COND
     ((NULL *NAT)
      (PROGN
       (PRIN2* (CAR U))
       (PRIN2* "(")
       (COND ((CDDR U) (INPRINT '*COMMA* 0 (CDR U))) (T (MAPRIN (CADR U))))
       (PRIN2* ")")))
     (T
      (PROG (Y L)
        (SETQ L 0)
        (SETQ L
                (PLUS (FLATSIZEC (FLATINDXL U))
                      (DIFFERENCE (LENGTH (CDR U)) 1)))
        (COND
         ((GREATERP L (DIFFERENCE (DIFFERENCE (LINELENGTH NIL) SPARE*) POSN*))
          (TERPRI* T)))
        (SETQ Y YCOORD*)
        (PRIN2* (CAR U))
        (PROG (J)
          (SETQ J (CDR U))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          (PROGN
           (SETQ YCOORD* (PLUS Y (COND ((ATOM (CAR J)) 1) (T (MINUS 1)))))
           (COND ((GREATERP YCOORD* YMAX*) (SETQ YMAX* YCOORD*)))
           (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
           (PRIN2* (COND ((ATOM (CAR J)) (CAR J)) (T (CADAR J))))
           (COND ((CDR J) (PRIN2* " "))))
          (SETQ J (CDR J))
          (GO LAB))
        (SETQ YCOORD* Y))))) 
(PUT 'REMBRAS 'NUMBER-OF-ARGS 1) 
(PUT 'REMBRAS 'DEFINED-ON-LINE '56) 
(PUT 'REMBRAS 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'REMBRAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMBRAS (U)
    (COND
     ((AND *NAT (OR (ATOM U) (NULL (GET (CAR U) 'INFIX))))
      (PROGN (PRIN2* " ") (MAPRIN U)))
     (T (PROGN (PRIN2* "(") (MAPRIN U) (PRIN2* ")"))))) 
(PUT 'FORM-WITH-FREE-INDICES 'TAG 'FORM-WITH-FREE-INDICES) 
(PUT 'FORM-WITH-FREE-INDICES 'PRIFN 'INDXPRI1) 
(PUT 'FORM-WITH-FREE-INDICES 'FANCY-SETPRIFN 'INDXPRI) 
(PUT 'FORM-WITH-FREE-INDICES 'FANCY-ASSGNPRI 'ASSGN_INDXPRI) 
(PUT 'FORM-WITH-FREE-INDICES 'ASSGNPRI 'ASSGN_INDXPRI) 
(FLAG '(FORM-WITH-FREE-INDICES) 'SPRIFN) 
(PUT 'INDVARPRT 'EXPT 'INBRACKETS) 
(PUT 'ASSGN_INDXPRI 'NUMBER-OF-ARGS 1) 
(PUT 'ASSGN_INDXPRI 'DEFINED-ON-LINE '78) 
(PUT 'ASSGN_INDXPRI 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'ASSGN_INDXPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASSGN_INDXPRI (U)
    (PROGN
     (COND
      ((MEMQ (CADDR U) '(FIRST ONLY))
       (COND (*FANCY (FANCY-TERPRI* T)) (T (TERPRI* T)))))
     (COND
      ((AND (NULL (CADR U)) (NULL (ATOM (CAR U)))
            (NULL (FLAGP (CAAR U) 'INDEXVAR)))
       (INDXPRI1 (CAR U)))
      (T (INDXPRI (COND ((NULL (CADR U)) (CAR U)) (T (CAADR U))) (CAR U)))))) 
(FLUID '(FANCY-TEXPOS)) 
(PUT 'XINDVARPRT 'NUMBER-OF-ARGS 2) 
(PUT 'XINDVARPRT 'DEFINED-ON-LINE '87) 
(PUT 'XINDVARPRT 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'XINDVARPRT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XINDVARPRT (L P)
    (PROG (POS TPOS FL W)
      (SETQ POS FANCY-POS*)
      (SETQ TPOS FANCY-TEXPOS)
      (SETQ FL FANCY-LINE*)
      (SETQ W
              (COND
               ((NOT (GREATERP (GET 'EXPT 'INFIX) P))
                (FANCY-IN-BRACKETS (LIST 'XINDVARPRT (MKQUOTE L) 0) '|(| '|)|))
               (T
                (PROG (W X B S)
                  (SETQ W (FANCY-PREFIX-OPERATOR (CAR L)))
                  (COND ((EQ W 'FAILED) (RETURN W)))
                  (SETQ L (XINDXLFIX (CDR L)))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND L (NEQ W 'FAILED))) (RETURN NIL)))
                    (PROGN
                     (COND (B (FANCY-PRIN2* "{}" 0)))
                     (SETQ B T)
                     (COND
                      ((ATOM (CAR L))
                       (COND ((EQ S '^) (SETQ X (CONS (CAR L) X)))
                             (T
                              (PROGN
                               (COND
                                (S
                                 (PROGN
                                  (SETQ W
                                          (FANCY-PRINT-INDEXLIST1 (REVERSIP X)
                                           S NIL))
                                  (FANCY-PRIN2* "{}" 0))))
                               (SETQ X (LIST (CAR L)))
                               (SETQ S '^)))))
                      (T
                       (COND ((EQ S '_) (SETQ X (CONS (CADAR L) X)))
                             (T
                              (PROGN
                               (COND
                                (S
                                 (PROGN
                                  (SETQ W
                                          (FANCY-PRINT-INDEXLIST1 (REVERSIP X)
                                           S NIL))
                                  (FANCY-PRIN2* "{}" 0))))
                               (SETQ X (LIST (CADAR L)))
                               (SETQ S '_))))))
                     (SETQ L (CDR L)))
                    (GO WHILELABEL))
                  (SETQ W (FANCY-PRINT-INDEXLIST1 (REVERSIP X) S NIL))
                  (RETURN W)))))
      (COND
       ((EQ W 'FAILED) (SETQ FANCY-LINE* FL) (SETQ FANCY-TEXPOS TPOS)
        (SETQ FANCY-POS* POS)))
      (RETURN W))) 
(PUT 'XINDXLFIX 'NUMBER-OF-ARGS 1) 
(PUT 'XINDXLFIX 'DEFINED-ON-LINE '121) 
(PUT 'XINDXLFIX 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'XINDXLFIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XINDXLFIX (U)
    (COND ((NULL U) NIL)
          ((ATOM (CAR U)) (CONS (XINDXFIX (CAR U)) (XINDXLFIX (CDR U))))
          (T (CONS (LIST 'MINUS (XINDXFIX (CADAR U))) (XINDXLFIX (CDR U)))))) 
(PUT 'XINDXFIX 'NUMBER-OF-ARGS 1) 
(PUT 'XINDXFIX 'DEFINED-ON-LINE '126) 
(PUT 'XINDXFIX 'DEFINED-IN-FILE 'EXCALC/INDXPRIN.RED) 
(PUT 'XINDXFIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XINDXFIX (X)
    (PROG (XX)
      (SETQ XX (EXPLODE X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND XX (EQUAL (CAR XX) '!))) (RETURN NIL)))
        (SETQ XX (CDR XX))
        (GO WHILELABEL))
      (RETURN (COND ((AND XX (NUMBERP (SETQ XX (COMPRESS XX)))) XX) (T X))))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TEMPLT)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL '(GENTRANLANG* |$#|)) 
(FLUID '(*GENDECS)) 
(SHARE (LIST 'GENTRANLANG* '|$#|)) 
(SETQ GENTRANLANG* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'FORTRAN)) 
(SETQ |$#| (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 0)) 
(SWITCH (LIST 'GENDECS)) 
(GLOBAL '(*SPACE* *STDOUT* $EOF$ $EOL$)) 
(SETQ *SPACE* BLANK) 
(FLUID '(*MODE)) 
(PUT 'PROCACTIVE 'NUMBER-OF-ARGS 0) 
(PUT 'PROCACTIVE 'DEFINED-ON-LINE '62) 
(PUT 'PROCACTIVE 'DEFINED-IN-FILE 'GENTRAN/TEMPLT.RED) 
(PUT 'PROCACTIVE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PROCACTIVE NIL
    (PROG (C BUF MODE OCH *INT *ERRCONT)
      (SETQ *ERRCONT 'T)
      (SETQ C (READCH))
      (COND
       ((EQ C 'E)
        (COND
         ((EQ (SETQ C (READCH)) 'N)
          (COND
           ((EQ (SETQ C (READCH)) 'D)
            (COND ((EQ (SETQ C (READCH)) '|;|) (RETURN $EOF$))
                  (T (SETQ BUF '|;END|))))
           (T (SETQ BUF '|;EN|))))
         (T (SETQ BUF '|;E|))))
       ((EQ C 'B)
        (COND
         ((EQ (SETQ C (READCH)) 'E)
          (COND
           ((EQ (SETQ C (READCH)) 'G)
            (COND
             ((EQ (SETQ C (READCH)) 'I)
              (COND
               ((EQ (SETQ C (READCH)) 'N)
                (COND
                 ((EQ (SETQ C (READCH)) '|;|)
                  (PROGN
                   (SETQ MODE *MODE)
                   (SETQ *MODE
                           (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'ALGEBRAIC))
                   (SETQ OCH (WRS (CDR *STDOUT*)))
                   (BEGIN1)
                   (WRS OCH)
                   (SETQ *MODE (PROGN (SETQ ALGLIST* (CONS NIL NIL)) MODE))
                   (LINELENGTH 150)
                   (RETURN
                    (COND ((EQ (SETQ C (READCH)) $EOL$) (READCH)) (T C)))))
                 (T (SETQ BUF '|;BEGIN|))))
               (T (SETQ BUF '|;BEGI|))))
             (T (SETQ BUF '|;BEG|))))
           (T (SETQ BUF '|;BE|))))
         (T (SETQ BUF '|;B|))))
       (T (SETQ BUF '|;|)))
      (PPRIN2 BUF)
      (RETURN C))) 
(ENDMODULE) 
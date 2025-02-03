(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(GLOBAL '(DATE* VERSION*)) 
(CREATE-PACKAGE
 '(RLISP MODULE NEWTOK RSUPPORT SLFNS SUPERV TOK XREAD LPRI PARSER BLOCK FORM
   PROC FORSTAT LOOPS STATMISC SMACRO IO INFIX SWITCH WHERE LIST ARRAY INTER
   CHARNAME NEWTOK1)
 NIL) 
(FLAG '(RLISP) 'CORE_PACKAGE) 
(SETQ DATE* (DATE)) 
(FLUID '(REVISION*)) 
(COND
 ((NULL VERSION*)
  (PROGN
   (COND ((NULL REVISION*) (SETQ VERSION* "REDUCE"))
         (T
          (SETQ VERSION*
                  (COMPRESS
                   (CONS '|"|
                         (APPEND (EXPLODE2 "REDUCE (")
                                 (APPEND (EXPLODE2 REVISION*)
                                         '(|)| |"|))))))))))) 
(PUT 'RLISP88 'SIMPFG '((T (LOAD-PACKAGE 'RLISP88) (RLISP88_ON)))) 
(FLAG '(RLISP88) 'SWITCH) 
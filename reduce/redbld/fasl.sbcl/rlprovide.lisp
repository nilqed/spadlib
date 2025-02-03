(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLPROVIDE)) 
(REVISION 'RLPROVIDE
          "$Id: rlprovide.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'RLPROVIDE "(c) 2017 T. Sturm") 
(PUT 'RL_PROVIDESERVICE 'STAT 'RL_PROVIDESERVICESTAT) 
(PUT 'RL_PROVIDESERVICESTAT 'NUMBER-OF-ARGS 0) 
(DE RL_PROVIDESERVICESTAT NIL
    (PROG (RLS PS BBL)
      (SCAN)
      (COND
       ((NOT (IDP CURSYM*))
        (REDERR
         (LIST "expecting identifier in rl_provideService but found"
               CURSYM*))))
      (SETQ RLS CURSYM*)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* 'EQUAL)
         (REDERR
          (LIST "expecting '=' in" "rl_provideService" "but found" CURSYM*)))))
      (SCAN)
      (COND
       ((NOT (IDP CURSYM*))
        (REDERR
         (LIST "expecting identifier in rl_provideService" RLS "but found"
               CURSYM*))))
      (SETQ PS CURSYM*)
      (SCAN)
      (COND
       ((EQ CURSYM* '*SEMICOL*)
        (RETURN (LIST 'RL_PROVIDESERVICE (MKQUOTE RLS) (MKQUOTE PS) NIL))))
      (COND
       ((NEQ CURSYM* 'USING)
        (REDERR
         (LIST "expecting 'using' or delimiter in rl_provideService" RLS "=" PS
               "but found" CURSYM*))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SCAN)
         (COND
          ((NOT (IDP CURSYM*))
           (REDERR
            (LIST "expecting identifier in rl_provideService" RLS "=" PS
                  "but found" CURSYM*))))
         (SETQ BBL (LTO_INSERTQ CURSYM* BBL))
         (SCAN)
         (COND
          ((NOT (MEMQ CURSYM* '(*COMMA* *SEMICOL*)))
           (REDERR
            (LIST "expecting ',' in rl_provideService" RLS "=" PS "but found"
                  CURSYM*)))))
        (COND ((NOT (EQ CURSYM* '*SEMICOL*)) (GO REPEATLABEL))))
      (RETURN
       (LIST 'RL_PROVIDESERVICE (MKQUOTE RLS) (MKQUOTE PS) (MKQUOTE BBL))))) 
(FLAG '(RL_PROVIDESERVICE) 'NOFORM) 
(PUT 'RL_PROVIDESERVICE 'NUMBER-OF-ARGS 3) 
(DE RL_PROVIDESERVICE (RLSERVICE PSERVICE BLACKBOXES)
    (PROGN
     (PUT RLSERVICE 'RL_KNOWNIMPLEMENTATIONS
          (LTO_INSERTQ PSERVICE (GET RLSERVICE 'RL_KNOWNIMPLEMENTATIONS)))
     (PUT PSERVICE 'RL_PROVIDEDSERVICE RLSERVICE)
     (PUT PSERVICE 'RL_REGISTEREDBLACKBOXES
          (UNION (GET PSERVICE 'RL_REGISTEREDBLACKBOXES) BLACKBOXES)))) 
(PUT 'RL_PROVIDEDSERVICEP 'NUMBER-OF-ARGS 1) 
(DE RL_PROVIDEDSERVICEP (X) (AND (IDP X) (FLAGP X 'RL_PROVIDEDSERVICE))) 
(PUT 'RL_REGISTEREDBLACKBOXES 'NUMBER-OF-ARGS 1) 
(DE RL_REGISTEREDBLACKBOXES (X) (GET X 'RL_REGISTEREDBLACKBOXES)) 
(ENDMODULE) 
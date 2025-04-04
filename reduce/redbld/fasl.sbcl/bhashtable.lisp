(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BHASHTABLE)) 
(REVISION 'BHASHTABLE
          "$Id: bhashtable.red 6087 2021-10-08 16:02:24Z thomas-sturm $") 
(COPYRIGHT 'BHASHTABLE "(c) 2021 T. Sturm") 
(PUT 'BHASHTABLE_NEW 'NUMBER-OF-ARGS 3) 
(DE BHASHTABLE_NEW (SIZE GROWTH BOUND)
    (PROG (BHASHTABLE)
      (SETQ BHASHTABLE (MKVECT 5))
      (PUTV BHASHTABLE 0 'BHASHTABLE)
      (PUTV BHASHTABLE 1 (MKHASH SIZE 'EQUAL GROWTH))
      (PUTV BHASHTABLE 2 NIL)
      (PUTV BHASHTABLE 3 NIL)
      (PUTV BHASHTABLE 4 0)
      (PUTV BHASHTABLE 5 BOUND)
      (RETURN BHASHTABLE))) 
(PUT 'BHASHTABLE_GETHASH 'NUMBER-OF-ARGS 2) 
(DE BHASHTABLE_GETHASH (KEY BHASHTABLE) (GETHASH KEY (GETV BHASHTABLE 1))) 
(PUT 'BHASHTABLE_PUTHASH 'NUMBER-OF-ARGS 3) 
(DE BHASHTABLE_PUTHASH (KEY BHASHTABLE ENTRY)
    (PROG (HASHTABLE QUEUE LASTNODE NUMBEROFELEMENTS OLDESTENTRY)
      (SETQ HASHTABLE (GETV BHASHTABLE 1))
      (SETQ QUEUE (GETV BHASHTABLE 2))
      (SETQ LASTNODE (GETV BHASHTABLE 3))
      (SETQ NUMBEROFELEMENTS (GETV BHASHTABLE 4))
      (COND
       ((EQN NUMBEROFELEMENTS (GETV BHASHTABLE 5))
        (PROGN
         (COND (NIL NIL))
         (SETQ OLDESTENTRY (PROG1 (CAR QUEUE) (SETQ QUEUE (CDR QUEUE))))
         (COND ((NULL QUEUE) (SETQ LASTNODE NIL)))
         (REMHASH OLDESTENTRY HASHTABLE)))
       (T (SETQ NUMBEROFELEMENTS (PLUS NUMBEROFELEMENTS 1))))
      (COND
       ((NULL QUEUE) (PROGN (SETQ QUEUE (LIST KEY)) (SETQ LASTNODE QUEUE)))
       (T
        (PROGN
         (SETCDR LASTNODE (CONS KEY NIL))
         (SETQ LASTNODE (CDR LASTNODE)))))
      (PUTHASH KEY HASHTABLE ENTRY)
      (PUTV BHASHTABLE 2 QUEUE)
      (PUTV BHASHTABLE 3 LASTNODE)
      (PUTV BHASHTABLE 4 NUMBEROFELEMENTS))) 
(ENDMODULE) 
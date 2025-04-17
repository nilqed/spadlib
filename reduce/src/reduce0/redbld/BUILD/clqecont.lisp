(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLQECONT)) 
(REVISION 'CLQECONT
          "$Id: clqecont.red 6072 2021-09-28 13:07:18Z thomas-sturm $") 
(COPYRIGHT 'CLQECONT "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'QECONT_NEW 'NUMBER-OF-ARGS 2) 
(DE QECONT_NEW (TRAVERSALMODE DYNAMICPROGRAMMING)
    (PROG (CO H)
      (SETQ CO (MKVECT 7))
      (PUTV CO 0 'QECONT)
      (PUTV CO 1 NIL)
      (PUTV CO 3 TRAVERSALMODE)
      (PUTV CO 4 DYNAMICPROGRAMMING)
      (COND
       ((EQ TRAVERSALMODE 'BFS) (PROGN (PUTV CO 2 NIL) (PUTV CO 5 'UNUSED)))
       ((AND (EQ TRAVERSALMODE 'DFS) DYNAMICPROGRAMMING)
        (PROGN
         (SETQ H (MKHASH 65536 'EQUAL 1.5))
         (PUTV CO 5 H)
         (PUTV CO 2 'UNUSED)))
       (T (PROGN (COND (NIL NIL)) (PUTV CO 2 'UNUSED) (PUTV CO 5 'UNUSED))))
      (PUTV CO 6 0)
      (PUTV CO 7 0)
      (RETURN CO))) 
(PUT 'QECONT_PRINT 'NUMBER-OF-ARGS 2) 
(DE QECONT_PRINT (CO EXTERNALINDENT)
    (PROG (*NAT INDENT NEXTINDENT)
      (SETQ INDENT "  ")
      (SETQ NEXTINDENT (LTO_SCONCAT2 EXTERNALINDENT INDENT))
      (IOTO_TPRIN2T (LIST EXTERNALINDENT "{"))
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "Class:              " (GETV CO 0)))
      (IOTO_TPRIN2 (LIST EXTERNALINDENT INDENT "Nodes:              "))
      (QENODE_PRINTLIST (GETV CO 1) NEXTINDENT)
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "LastNode:           "
             (COND ((EQ (GETV CO 2) 'UNUSED) 'UNUSED) (T "(hidden)"))))
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "TraversalMode:      " (GETV CO 3)))
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "HashTable:          "
             (COND ((EQ (GETV CO 5) 'UNUSED) 'UNUSED) (T "(hidden)"))))
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "RequestedAdditions: " (GETV CO 6)))
      (IOTO_TPRIN2T
       (LIST EXTERNALINDENT INDENT "EffectiveAdditions: " (GETV CO 7)))
      (IOTO_TPRIN2T (LIST EXTERNALINDENT "}"))
      (RETURN CO))) 
(PUT 'QECONT_ADD 'NUMBER-OF-ARGS 2) 
(DE QECONT_ADD (CO NEWNODES)
    (PROG (NODES LASTNODE HASHTABLE)
      (COND ((NULL NEWNODES) (RETURN CO)))
      (COND
       ((EQ (GETV CO 3) 'BFS)
        (PROGN
         (SETQ NODES (GETV CO 1))
         (SETQ LASTNODE (GETV CO 2))
         (COND
          ((NULL LASTNODE)
           (PROGN
            (SETQ LASTNODE
                    (SETQ NODES
                            (LIST
                             (PROG1 (CAR NEWNODES)
                               (SETQ NEWNODES (CDR NEWNODES))))))
            (PUTV CO 6 (PLUS (GETV CO 6) 1))
            (PUTV CO 7 (PLUS (GETV CO 7) 1)))))
         (PROG (NODE)
           (SETQ NODE NEWNODES)
          LAB
           (COND ((NULL NODE) (RETURN NIL)))
           ((LAMBDA (NODE)
              (PROGN
               (PUTV CO 6 (PLUS (GETV CO 6) 1))
               (COND
                ((NOT (QECONT_MEMBER NODE NODES))
                 (PROGN
                  (SETCDR LASTNODE (CONS NODE NIL))
                  (SETQ LASTNODE (CDR LASTNODE))
                  (PUTV CO 7 (PLUS (GETV CO 7) 1)))))))
            (CAR NODE))
           (SETQ NODE (CDR NODE))
           (GO LAB))
         (PUTV CO 1 NODES)
         (PUTV CO 2 LASTNODE)
         (RETURN CO))))
      (COND (NIL NIL))
      (COND
       ((GETV CO 4)
        (PROGN
         (SETQ NODES (GETV CO 1))
         (PROG (NODE)
           (SETQ NODE NEWNODES)
          LAB
           (COND ((NULL NODE) (RETURN NIL)))
           ((LAMBDA (NODE)
              (PROGN
               (PUTV CO 6 (PLUS (GETV CO 6) 1))
               (SETQ HASHTABLE (GETV CO 5))
               (COND
                ((NULL
                  (GETHASH
                   (CONS (QENODE_GETVARIABLES NODE) (QENODE_GETFORMULA NODE))
                   HASHTABLE))
                 (PROGN
                  (PUTHASH
                   (CONS (QENODE_GETVARIABLES NODE) (QENODE_GETFORMULA NODE))
                   HASHTABLE T)
                  (PUTV CO 5 HASHTABLE)
                  (SETQ NODES (CONS NODE NODES))
                  (PUTV CO 7 (PLUS (GETV CO 7) 1)))))))
            (CAR NODE))
           (SETQ NODE (CDR NODE))
           (GO LAB))
         (PUTV CO 1 NODES)
         (RETURN CO))))
      (COND (NIL NIL))
      (SETQ NODES (GETV CO 1))
      (PROG (NODE)
        (SETQ NODE NEWNODES)
       LAB
        (COND ((NULL NODE) (RETURN NIL)))
        ((LAMBDA (NODE)
           (PROGN
            (PUTV CO 6 (PLUS (GETV CO 6) 1))
            (SETQ NODES (CONS NODE NODES))
            (PUTV CO 7 (PLUS (GETV CO 7) 1))))
         (CAR NODE))
        (SETQ NODE (CDR NODE))
        (GO LAB))
      (PUTV CO 1 NODES)
      (RETURN CO))) 
(PUT 'QECONT_MEMBER 'NUMBER-OF-ARGS 2) 
(DE QECONT_MEMBER (NODE NODES)
    (PROG (FOUND VARIABLES FORMULA CURRENTNODE)
      (SETQ FOUND NIL)
      (SETQ VARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ FORMULA (QENODE_GETFORMULA NODE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT (NULL NODES)) (NOT FOUND))) (RETURN NIL)))
        (PROGN
         (SETQ CURRENTNODE (PROG1 (CAR NODES) (SETQ NODES (CDR NODES))))
         (SETQ FOUND
                 (AND (EQUAL (QENODE_GETVARIABLES CURRENTNODE) VARIABLES)
                      (EQUAL (QENODE_GETFORMULA CURRENTNODE) FORMULA))))
        (GO WHILELABEL))
      (RETURN FOUND))) 
(PUT 'QECONT_FIRSTNODE 'NUMBER-OF-ARGS 1) 
(DE QECONT_FIRSTNODE (CO) (CAR (GETV CO 1))) 
(PUT 'QECONT_FETCH 'NUMBER-OF-ARGS 1) 
(DE QECONT_FETCH (CO)
    (PROG (NODES NODE)
      (SETQ NODES (GETV CO 1))
      (COND (NIL NIL))
      (SETQ NODE (PROG1 (CAR NODES) (SETQ NODES (CDR NODES))))
      (COND ((AND (NULL NODES) (EQ (GETV CO 3) 'BFS)) (PUTV CO 2 NIL)))
      (PUTV CO 1 NODES)
      (RETURN NODE))) 
(PUT 'QECONT_ISEMPTY 'NUMBER-OF-ARGS 1) 
(DE QECONT_ISEMPTY (CO) (NULL (GETV CO 1))) 
(PUT 'QECONT_LENGTH 'NUMBER-OF-ARGS 1) 
(DE QECONT_LENGTH (CO) (LENGTH (GETV CO 1))) 
(PUT 'QECONT_STATISTICS 'NUMBER-OF-ARGS 1) 
(DE QECONT_STATISTICS (CO)
    (PROG (STATISTICS STATISTICSENTRY THISLENGTH MAXLENGTH)
      (SETQ THISLENGTH 0)
      (SETQ MAXLENGTH 0)
      (PROG (NODE)
        (SETQ NODE (GETV CO 1))
       LAB
        (COND ((NULL NODE) (RETURN NIL)))
        ((LAMBDA (NODE)
           (PROGN
            (SETQ THISLENGTH (LENGTH (QENODE_GETVARIABLES NODE)))
            (COND
             ((GREATERP THISLENGTH MAXLENGTH) (SETQ MAXLENGTH THISLENGTH)))
            (SETQ STATISTICSENTRY (ASSOC THISLENGTH STATISTICS))
            (COND
             (STATISTICSENTRY
              (SETCDR STATISTICSENTRY (PLUS (CDR STATISTICSENTRY) 1)))
             (T
              (PROG (W1)
                (SETQ W1 (CONS THISLENGTH 1))
                (SETQ STATISTICS (CONS W1 STATISTICS))
                (RETURN W1))))))
         (CAR NODE))
        (SETQ NODE (CDR NODE))
        (GO LAB))
      (PROG (KEY)
        (SETQ KEY 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MAXLENGTH KEY)) (RETURN NIL)))
        (COND
         ((NOT (ASSOC KEY STATISTICS))
          (PROG (W1)
            (SETQ W1 (CONS KEY 0))
            (SETQ STATISTICS (CONS W1 STATISTICS))
            (RETURN W1))))
        (SETQ KEY (PLUS2 KEY 1))
        (GO LAB))
      (SETQ STATISTICS
              (SORT STATISTICS
                    (FUNCTION (LAMBDA (P1 P2) (GREATERP (CAR P1) (CAR P2))))))
      (RETURN (MAPCAR STATISTICS 'CDR)))) 
(PUT 'QECONT_GETEFFECTIVEADDITIONS 'NUMBER-OF-ARGS 1) 
(DE QECONT_GETEFFECTIVEADDITIONS (CO) (GETV CO 7)) 
(PUT 'QECONT_SETEFFECTIVEADDITIONS 'NUMBER-OF-ARGS 2) 
(DE QECONT_SETEFFECTIVEADDITIONS (CO N) (PROGN (PUTV CO 7 N) CO)) 
(PUT 'QECONT_GETREQUESTEDADDITIONS 'NUMBER-OF-ARGS 1) 
(DE QECONT_GETREQUESTEDADDITIONS (CO) (GETV CO 6)) 
(PUT 'QECONT_SETREQUESTEDADDITIONS 'NUMBER-OF-ARGS 2) 
(DE QECONT_SETREQUESTEDADDITIONS (CO N) (PROGN (PUTV CO 6 N) CO)) 
(ENDMODULE) 
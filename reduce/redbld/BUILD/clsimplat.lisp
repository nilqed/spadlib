(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLSIMPLAT)) 
(REVISION 'CLSIMPLAT
          "$Id: clsimplat.red 6088 2021-10-08 16:03:01Z thomas-sturm $") 
(COPYRIGHT 'CLSIMPLAT "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'CL_INITIALIZEIDENTIFYAT 'NUMBER-OF-ARGS 0) 
(DE CL_INITIALIZEIDENTIFYAT NIL
    (PROG (HASHTABLE)
      (SETQ HASHTABLE (MKHASH 65536 'EQUAL 1))
      (PUT 'CL_IDENTIFYAT 'HASHTABLE HASHTABLE))) 
(DE CL_IDENTIFYATHASHKEY (ATF) (CONS (CADR ATF) (CONS (CAR ATF) (CDDR ATF)))) 
(PUT 'CL_IDENTIFYATHASHKEY 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_IDENTIFYATHASHKEY 'INLINE
      '(LAMBDA (ATF) (CONS (CADR ATF) (CONS (CAR ATF) (CDDR ATF))))) 
(DE CL_IDENTIFYATS (F)
    (COND (*RLIDENTIFY (CL_APPLY2ATS F 'CL_IDENTIFYAT)) (T F))) 
(PUT 'CL_IDENTIFYATS 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_IDENTIFYATS 'INLINE
      '(LAMBDA (F) (COND (*RLIDENTIFY (CL_APPLY2ATS F 'CL_IDENTIFYAT)) (T F)))) 
(PUT 'CL_IDENTIFYAT 'NUMBER-OF-ARGS 1) 
(DE CL_IDENTIFYAT (ATF)
    (PROG (HASHTABLE ENTRY)
      (SETQ HASHTABLE (GET 'CL_IDENTIFYAT 'HASHTABLE))
      (COND
       ((SETQ ENTRY
                (GETHASH (CONS (CADR ATF) (CONS (CAR ATF) (CDDR ATF)))
                         HASHTABLE))
        (RETURN ENTRY)))
      (PUTHASH (CONS (CADR ATF) (CONS (CAR ATF) (CDDR ATF))) HASHTABLE ATF)
      (RETURN ATF))) 
(PUT 'CL_SIMPLAT 'REMEMBER (BHASHTABLE_NEW 65536 1 65536)) 
(DE CL_SIMPLATHASHKEY (ATF SOP)
    (CONS (CADR ATF)
          (CONS (CAR ATF)
                (CONS (CDDR ATF)
                      (CONS SOP
                            (CONS RL_CID*
                                  (CONS RL_ARGL* (SIATENV_ASINTEGER)))))))) 
(PUT 'CL_SIMPLATHASHKEY 'NUMBER-OF-ARGS 2) 
(PUTC 'CL_SIMPLATHASHKEY 'INLINE
      '(LAMBDA (ATF SOP)
         (CONS (CADR ATF)
               (CONS (CAR ATF)
                     (CONS (CDDR ATF)
                           (CONS SOP
                                 (CONS RL_CID*
                                       (CONS RL_ARGL*
                                             (SIATENV_ASINTEGER))))))))) 
(PUT 'CL_SIMPLAT 'NUMBER-OF-ARGS 2) 
(DE CL_SIMPLAT (ATF SOP)
    (PROG (BHASHTABLE KEY ENTRY SIMPLIFICATIONRESULT)
      (COND
       ((LEQ 65536 0)
        (RETURN
         ((LAMBDA (F)
            (COND (*RLIDENTIFY (CL_APPLY2ATS F 'CL_IDENTIFYAT)) (T F)))
          (RL_SIMPLAT1 ATF SOP)))))
      (SETQ BHASHTABLE (GET 'CL_SIMPLAT 'REMEMBER))
      (SETQ KEY
              (CONS (CADR ATF)
                    (CONS (CAR ATF)
                          (CONS (CDDR ATF)
                                (CONS SOP
                                      (CONS RL_CID*
                                            (CONS RL_ARGL*
                                                  (SIATENV_ASINTEGER))))))))
      (COND ((SETQ ENTRY (BHASHTABLE_GETHASH KEY BHASHTABLE)) (RETURN ENTRY)))
      (SETQ SIMPLIFICATIONRESULT
              ((LAMBDA (F)
                 (COND (*RLIDENTIFY (CL_APPLY2ATS F 'CL_IDENTIFYAT)) (T F)))
               (RL_SIMPLAT1 ATF SOP)))
      (BHASHTABLE_PUTHASH KEY BHASHTABLE SIMPLIFICATIONRESULT)
      (COND
       ((CL_ATFP SIMPLIFICATIONRESULT)
        (PROGN
         (SETQ KEY
                 (CONS (CADR SIMPLIFICATIONRESULT)
                       (CONS (CAR SIMPLIFICATIONRESULT)
                             (CONS (CDDR SIMPLIFICATIONRESULT)
                                   (CONS SOP
                                         (CONS RL_CID*
                                               (CONS RL_ARGL*
                                                     (SIATENV_ASINTEGER))))))))
         (BHASHTABLE_PUTHASH KEY BHASHTABLE SIMPLIFICATIONRESULT))))
      (RETURN SIMPLIFICATIONRESULT))) 
(ENDMODULE) 
(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLSIMPLATENV)) 
(REVISION 'CLSIMPLATENV
          "$Id: clsimplatenv.red 6088 2021-10-08 16:03:01Z thomas-sturm $") 
(COPYRIGHT 'CLSIMPLATENV "(c) 2021 A. Dolzmann, T. Sturm") 
(SWITCH (LIST 'RLIDENTIFY)) 
(SWITCH (LIST 'RLSIATADV)) 
(ON1 'RLSIATADV) 
(SWITCH (LIST 'RLSIEXPL)) 
(ON1 'RLSIEXPL) 
(SWITCH (LIST 'RLSIEXPLA)) 
(ON1 'RLSIEXPLA) 
(SWITCH (LIST 'RLSIFAC)) 
(ON1 'RLSIFAC) 
(SWITCH (LIST 'RLSIFACO)) 
(OFF1 'RLSIFACO) 
(SWITCH (LIST 'RLSIPD)) 
(ON1 'RLSIPD) 
(SWITCH (LIST 'RLSITSQSPL)) 
(ON1 'RLSITSQSPL) 
(PUT 'RLIDENTIFY 'SIMPFG
     '((T (CL_INITIALIZEIDENTIFYAT)) (NIL (REMPROP 'CL_IDENTIFYAT 'HASHTABLE)))) 
(PUT 'SIATENV_NEW 'NUMBER-OF-ARGS 0) 
(DE SIATENV_NEW NIL
    (PROG (ENV)
      (SETQ ENV (MKVECT 7))
      (PUTV ENV 0 'SIATENV)
      (PUTV ENV 1 *RLSIATADV)
      (PUTV ENV 2 *RLSIEXPL)
      (PUTV ENV 3 *RLSIEXPLA)
      (PUTV ENV 4 *RLSIFAC)
      (PUTV ENV 5 *RLSIFACO)
      (PUTV ENV 7 *RLSITSQSPL)
      (RETURN ENV))) 
(PUT 'SIATENV_ASLIST 'NUMBER-OF-ARGS 0) 
(DE SIATENV_ASLIST NIL
    (LIST *RLSIATADV *RLSIEXPL *RLSIEXPLA *RLSIFAC *RLSIFACO *RLSITSQSPL)) 
(PUT 'SIATENV_BINARYENCODING 'DEFINED-ON-LINE '70) 
(PUT 'SIATENV_BINARYENCODING 'DEFINED-IN-FILE 'REDLOG/CL/CLSIMPLATENV.RED) 
(PUT 'SIATENV_BINARYENCODING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM SIATENV_BINARYENCODING (ARGL)
    (PROG (BIT ENCODING)
      (SETQ ARGL (CDR ARGL))
      (COND
       ((NULL ARGL)
        (REDERR "SiAtEnv_binaryEncoding requires at least one argument")))
      (SETQ ENCODING (LIST 'COND (LIST (CAR ARGL) 1) (LIST T 0)))
      (SETQ ARGL (CDR ARGL))
      (PROG ()
       WHILELABEL
        (COND ((NOT ARGL) (RETURN NIL)))
        (PROGN
         (SETQ BIT (LIST 'COND (LIST (CAR ARGL) 1) (LIST T 0)))
         (SETQ ENCODING (LIST 'IPLUS2 BIT (LIST 'ITIMES2 2 ENCODING)))
         (SETQ ARGL (CDR ARGL)))
        (GO WHILELABEL))
      (RETURN ENCODING))) 
(PUT 'SIATENV_ASINTEGER 'NUMBER-OF-ARGS 0) 
(DE SIATENV_ASINTEGER NIL
    (IPLUS2 (COND (*RLSITSQSPL 1) (T 0))
            (ITIMES2 2
                     (IPLUS2 (COND (*RLSIFACO 1) (T 0))
                             (ITIMES2 2
                                      (IPLUS2 (COND (*RLSIFAC 1) (T 0))
                                              (ITIMES2 2
                                                       (IPLUS2
                                                        (COND (*RLSIEXPLA 1)
                                                              (T 0))
                                                        (ITIMES2 2
                                                                 (IPLUS2
                                                                  (COND
                                                                   (*RLSIEXPL
                                                                    1)
                                                                   (T 0))
                                                                  (ITIMES2 2
                                                                           (COND
                                                                            (*RLSIATADV
                                                                             1)
                                                                            (T
                                                                             0))))))))))))) 
(ENDMODULE) 
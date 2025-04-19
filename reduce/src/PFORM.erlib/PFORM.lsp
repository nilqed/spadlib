
(SDEFUN |PFORM;mkqsym;SS;1| ((|s| (|Symbol|)) (% (|String|)))
        (SPROG ((|ss| (|List| (|OutputForm|))) (|ns| (|Symbol|)))
               (SEQ
                (COND
                 ((SPADCALL |s| (QREFELT % 9))
                  (SEQ (LETT |ns| (SPADCALL |s| (QREFELT % 10)))
                       (LETT |ss| (QVELT (SPADCALL |s| (QREFELT % 12)) 0))
                       (EXIT
                        (SPADCALL (FORMAT 'NIL "(list '~A ~{'~A ~})" |ns| |ss|)
                                  (QREFELT % 14)))))
                 ('T (SPADCALL (FORMAT 'NIL "'~A" |s|) (QREFELT % 14))))))) 

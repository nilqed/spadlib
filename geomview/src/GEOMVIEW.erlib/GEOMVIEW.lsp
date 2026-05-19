
(SDEFUN |GEOMVIEW;sys| ((|c| (|String|)) (% (|Void|)))
        (SPADCALL (SPADCALL "system " |c| (QREFELT % 9)) (QREFELT % 12))) 

(SDEFUN |GEOMVIEW;newID| ((% (|String|)))
        (SPROG
         ((|fmt|
           (#1="(format 'nil \"fricas.~A.~A\" (get-universal-time) (random 1000))")))
         (SEQ (LETT |fmt| #1#)
              (EXIT
               (SPADCALL (SPADCALL |fmt| (QREFELT % 14)) (QREFELT % 15)))))) 

(SDEFUN |GEOMVIEW;pipeExists?;%B;3| ((|p| (%)) (% (|Boolean|)))
        (SPROG ((|b| (|SExpression|)))
               (NULL
                (SPADCALL (LETT |b| (PROBE-FILE (QCDR |p|))) (QREFELT % 17))))) 

(SDEFUN |GEOMVIEW;pipeAlive?;%B;4| ((|p| (%)) (% (|Boolean|)))
        (SPROG ((|cmd| (|String|)) (|bs| (|String|)))
               (SEQ
                (LETT |cmd| (STRINGIMAGE (FORMAT 'NIL "fuser ~A" (QCDR |p|))))
                (LETT |bs| (SPADCALL |cmd| (QREFELT % 19)))
                (EXIT (NULL (EQUAL |bs| "")))))) 

(SDEFUN |GEOMVIEW;makePipe;2S;5| ((|s| (|String|)) (% (|String|)))
        (SPROG ((|cmd| (|String|)) (|b| (|SExpression|)) (#1=#:G33 NIL))
               (SEQ
                (EXIT
                 (SEQ
                  (LETT |cmd|
                        (STRINGIMAGE
                         (FORMAT 'NIL "togeomview ~A </dev/null" |s|)))
                  (|GEOMVIEW;sys| |cmd| %)
                  (LETT |b|
                        (PROBE-FILE
                         (SPADCALL (QREFELT % 7) |s| (QREFELT % 9))))
                  (EXIT
                   (COND
                    ((SPADCALL |b| (QREFELT % 17))
                     (|error|
                      (STRINGIMAGE
                       (FORMAT 'NIL "Could not create pipe ~A" |s|))))
                    ('T (PROGN (LETT #1# |s|) (GO #2=#:G32)))))))
                #2# (EXIT #1#)))) 

(SDEFUN |GEOMVIEW;writePipe;%SV;6| ((|p| (%)) (|c| (|String|)) (% (|Void|)))
        (SPROG ((|cmd| (|String|)))
               (SEQ
                (LETT |cmd|
                      (STRINGIMAGE
                       (FORMAT 'NIL "printf '~A' > ~A" |c| (QCDR |p|))))
                (EXIT (|GEOMVIEW;sys| |cmd| %))))) 

(SDEFUN |GEOMVIEW;new;%;7| ((% (%)))
        (SPROG ((|pipeID| (|String|)) (|p| (|String|)))
               (SEQ (LETT |pipeID| (|GEOMVIEW;newID| %))
                    (LETT |p| (SPADCALL |pipeID| (QREFELT % 21)))
                    (EXIT
                     (CONS |p| (SPADCALL (QREFELT % 7) |p| (QREFELT % 9))))))) 

(SDEFUN |GEOMVIEW;close;%B;8| ((|g| (%)) (% (|Boolean|)))
        (SEQ (SPADCALL |g| "(quit)" (QREFELT % 22))
             (|GEOMVIEW;sys| (STRINGIMAGE (FORMAT 'NIL "rm ~A" (QCDR |g|))) %)
             (EXIT (NULL (SPADCALL |g| (QREFELT % 18)))))) 

(SDEFUN |GEOMVIEW;coerce;%Of;9| ((|g| (%)) (% (|OutputForm|)))
        (SPADCALL (QCAR |g|) (QREFELT % 26))) 

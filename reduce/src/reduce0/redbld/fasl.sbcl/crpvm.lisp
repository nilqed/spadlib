(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CRACKPVM)) 
(FLUID '(LOADDIRECTORIES* LOADEXTENSIONS* *LOWER UNIXARGS* OPTIONS*)) 
(PUT 'PROCZAEHLER 'NUMBER-OF-ARGS 2) 
(PUT 'PROCZAEHLER 'DEFINED-ON-LINE '86) 
(PUT 'PROCZAEHLER 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'PROCZAEHLER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROCZAEHLER (FILE ACTION)
    (PROG (FL FPID *ECHO SEMIC* A SAVE OFL*BAK)
      (SETQ FPID (BLDMSG_INTERNAL "%s.%w" (LIST FILE (GETPID))))
      (COND ((EQUAL ACTION 'INIT) (SETQ BACKUP_ 0))
            (T
             (PROGN
              (SETQ FL 1)
              (PROG ()
               REPEATLABEL
                (PROGN
                 (SETQ FL (RENAME-FILE FILE FPID))
                 (COND ((NULL FL) (SYSTEM "sleep 1"))))
                (COND ((NOT FL) (GO REPEATLABEL))))
              (SETQ SEMIC* '$)
              (IN (LIST FPID)))))
      (COND
       ((AND (NEQ ACTION 'INIT) (NOT (NUMBERP BACKUP_)))
        (PROGN
         (PROGN (PRIN2 "pwd = ") (PRIN2 (PWD)) NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "***** ERROR: file ")
          (PRIN2 PROCESS_COUNTER)
          (PRIN2 " does not contain a number: ")
          NIL)
         (PROGN (PRIN2 BACKUP_) NIL)
         (TERPRI)
         (ERROR 1000 "We stop now!"))))
      (COND
       ((LESSP BACKUP_ 0)
        (PROGN
         (PROGN (PRIN2 "pwd = ") (PRIN2 (PWD)) NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "***** ERROR: file ")
          (PRIN2 PROCESS_COUNTER)
          (PRIN2 " contains a number <0 : ")
          NIL)
         (PROGN (PRIN2 BACKUP_) NIL)
         (TERPRI)
         (ERROR 1001 "We stop now!"))))
      (SETQ A (OPEN FPID 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* FPID)
      (SETQ SAVE (WRS A))
      (COND
       ((EQUAL ACTION 'PLUS)
        (PROGN
         (PRIN2 "lisp (backup_ := ")
         (PRIN2 (PLUS BACKUP_ 1))
         (PRIN2 ")$ end$")
         NIL)))
      (COND
       ((EQUAL ACTION 'MINUS)
        (PROGN
         (PRIN2 "lisp (backup_ := ")
         (PRIN2 (DIFFERENCE BACKUP_ 1))
         (PRIN2 ")$ end$")
         NIL)))
      (COND
       ((EQUAL ACTION 'INIT)
        (PROGN (PRIN2 "lisp (backup_ := ") (PRIN2 0) (PRIN2 ")$ end$") NIL)))
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (SETQ FL 1)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ FL (RENAME-FILE FPID FILE))
         (COND ((NULL FL) (SYSTEM "sleep 1"))))
        (COND ((NOT FL) (GO REPEATLABEL)))))) 
(PUT 'INI_CHECK_OF_PARALLEL_CRACK 'NUMBER-OF-ARGS 0) 
(PUT 'INI_CHECK_OF_PARALLEL_CRACK 'DEFINED-ON-LINE '149) 
(PUT 'INI_CHECK_OF_PARALLEL_CRACK 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'INI_CHECK_OF_PARALLEL_CRACK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INI_CHECK_OF_PARALLEL_CRACK NIL
    (COND (PARACRACK_INITIALIZED T)
          (T
           (PROG (OS)
             (COND
              ((NEQ REDUCERC_INITIALIZED 13112006)
               (RETURN
                (PROGN
                 (PROGN
                  (PRIN2
                   "##### To run this parallelization, a certain file .reducerc has to be")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "##### placed in your home directory! This file comes with all crack")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "##### files. To see it under unix do:  ls -a  . If it is not in your")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "##### CRACK directory you can download it together with the latest")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2 "##### version of CRACK from ")
                  (PRIN2 CRACK_DOWNLOAD_ADD)
                  (PRIN2 "crack.tar.gz ")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2 "##### or as a single file ")
                  (PRIN2 CRACK_DOWNLOAD_ADD)
                  (PRIN2 ".reducerc ")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "##### and afterwards copy it into your home directory and restart REDUCE.")
                  NIL)
                 (TERPRI)
                 NIL))))
             (COND
              ((OR (MEMQ 'LINUX LISPSYSTEM*) (MEMQ 'UNIX LISPSYSTEM*)
                   (MEMQ 'DARWIN13.4.0 LISPSYSTEM*)
                   (MEMQ 'LINUX-GNU LISPSYSTEM*))
               (SETQ OS 'LINUX)))
             (COND
              ((NEQ OS 'LINUX)
               (PROGN
                (PROGN (PRIN2 "### WARNING:") NIL)
                (TERPRI)
                (PROGN
                 (PRIN2
                  "Currently only the operating system linux is supported.")
                 NIL)
                (PROGN
                 (PRIN2 "Parallel execution on your ")
                 (PRIN2 OS)
                 (PRIN2 " system may not be possible.")
                 NIL)
                (TERPRI)
                NIL)))
             (COND
              ((NOT (FILEP PROCESS_COUNTER))
               (PROCZAEHLER PROCESS_COUNTER 'INIT)))
             (CHANGE_PROMPT_TO "")
             (PROGN
              (PRIN2
               "Shall each parallel process ask for verification at the end? (y/n) ")
              NIL)
             (PROG ()
              REPEATLABEL
               (SETQ OS (TERMREAD))
               (COND
                ((NOT (OR (EQUAL OS 'Y) (EQUAL OS 'N))) (GO REPEATLABEL))))
             (SETQ VERIFY_END_OF_PARALLEL_RUN (COND ((EQUAL OS 'Y) T) (T NIL)))
             (RESTORE_INTERACTIVE_PROMPT)
             (START_SOL_LIST_FILE)
             (RETURN (SETQ PARACRACK_INITIALIZED T)))))) 
(PUT 'PVM_ACTIVATE 'NUMBER-OF-ARGS 0) 
(PUT 'PVM_ACTIVATE 'DEFINED-ON-LINE '198) 
(PUT 'PVM_ACTIVATE 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'PVM_ACTIVATE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PVM_ACTIVATE NIL
    (COND ((GETD 'PVM_MYTID) (SETQ PVM_ABLE T)) (T (SETQ PVM_ABLE NIL)))) 
(PUT 'PVM_ACTIVE 'NUMBER-OF-ARGS 0) 
(PUT 'PVM_ACTIVE 'DEFINED-ON-LINE '202) 
(PUT 'PVM_ACTIVE 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'PVM_ACTIVE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PVM_ACTIVE NIL
    (COND (PVM_ABLE T)
          (T
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "PVM is either not active on this computer") NIL)
            (TERPRI)
            (PROGN (PRIN2 "or interactively switched off. Try 'vp'.") NIL)
            (TERPRI)
            NIL)))) 
(PUT 'CRLOAD 'NUMBER-OF-ARGS 0) 
(PUT 'CRLOAD 'DEFINED-ON-LINE '214) 
(PUT 'CRLOAD 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'CRLOAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CRLOAD NIL
    (PROG (FOUND U LD LE)
      (SETQ LD LOADDIRECTORIES*)
      (SETQ LE LOADEXTENSIONS*)
      (SETQ U 'CRACK)
      ((LAMBDA (*LOWER)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND LD (NOT FOUND))) (RETURN NIL)))
           (PROGN
            (SETQ LE LOADEXTENSIONS*)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND LE (NOT FOUND))) (RETURN NIL)))
              (PROGN
               (COND
                ((FILEP
                  (BLDMSG_INTERNAL "%w%w%w"
                                   (LIST (FIRST LD) U (CAR (FIRST LE)))))
                 (SETQ FOUND (BLDMSG_INTERNAL "%w%w" (LIST (FIRST LD) U))))
                (T (SETQ LE (REST LE))))
               NIL)
              (GO WHILELABEL))
            (SETQ LD (REST LD))
            NIL)
           (GO WHILELABEL)))
       T)
      (RETURN FOUND))) 
(PUT 'CRACK_LOAD_CMD 'NUMBER-OF-ARGS 0) 
(PUT 'CRACK_LOAD_CMD 'DEFINED-ON-LINE '233) 
(PUT 'CRACK_LOAD_CMD 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'CRACK_LOAD_CMD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CRACK_LOAD_CMD NIL
    (COND
     ((NULL CRACK_LOAD_COMMAND)
      (PROG (H)
        (PROGN
         (PRIN2
          "If this session was started by loading \"crack\" then enter  1  else ")
         NIL)
        (TERPRI)
        (PROGN
         (PRIN2
          "If this session was started by loading \"crall\" then enter  2  else ")
         NIL)
        (TERPRI)
        (PROGN (PRIN2 "enter the load command in \" \".") NIL)
        (TERPRI)
        (CHANGE_PROMPT_TO " ")
        (SETQ H (TERMREAD))
        (SETQ CRACK_LOAD_COMMAND
                (COND ((EQUAL H 1) "crack") ((EQUAL H 2) "crall")
                      (T (BLDMSG_INTERNAL "%w" (LIST H)))))
        (RESTORE_INTERACTIVE_PROMPT))))) 
(PUT 'REDUCE_CALL_CMD 'NUMBER-OF-ARGS 0) 
(PUT 'REDUCE_CALL_CMD 'DEFINED-ON-LINE '290) 
(PUT 'REDUCE_CALL_CMD 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'REDUCE_CALL_CMD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REDUCE_CALL_CMD NIL
    (COND
     ((NULL REDUCE_CALL)
      (PROGN
       (COND
        ((NULL REDUCE_CALL)
         (PROGN
          (PROGN
           (PRIN2
            "For calling REDUCE the program needs to know the calling command.")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "Please input this call. Examples:") NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on Eee: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "\"/home/thomas/reduce-algebra/trunk/bin/redpsl\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on Eee: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/home/thomas/reduce-algebra/trunk/bin/redcsl -w -i /home/thomas/reduce-algebra/trunk/cslbuild/i686-unknown-ubuntu10.04-nogui/csl/reduce.img -o /home/thomas/red/src6/cr32.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on sente: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "\"/homes/reduce/reduce-algebra/trunk/bin/redpsl -td 200\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on sente: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/homes/reduce/reduce-algebra/trunk/bin/redcsl -w -i /homes/reduce/reduce-algebra/trunk/cslbuild/i686-unknown-suse11.4/csl/reduce.img -o ~/red/src6/cr32.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on lie: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/homes/reduce/reduce-algebra/trunk/bin/redcsl -w -i /homes/reduce/reduce-algebra/trunk/cslbuild/x86_64-unknown-rh6.3-nogui/csl/reduce.img -o ~/red/src6/cr64.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on goedel: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/home/eschruefer/Reduce-Algebra/trunk/bin/redpsl -td 8000\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on goedel: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/home/reduce/Reduce-Algebra/trunk/bin/redcsl -w -i /home/reduce/Reduce-Algebra/trunk/cslbuild/x86_64-unknown-suse13.1/csl/reduce.img -o ~/moyo/red/src6/crgd.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on sharc-198: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "\"/home/twolf/reduce-algebra/trunk/bin/redpsl -td 8000\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on sharc-198: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/home/twolf/reduce-algebra/trunk/bin/redcsl -w -i /home/twolf/reduce-algebra/trunk/cslbuild/x86_64-unknown-linux-gnu/csl/reduce.img -o ~/red/src6/cr.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on ProMac: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/Users/eberhardschruefer/Reduce-Algebra/trunk/bin/redpsl -td 8000\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on ProMac: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/Users/eberhardschruefer/Reduce-Algebra/trunk/bin/redcsl -w -i /Users/eberhardschruefer/Reduce-Algebra/trunk/cslbuild/x86_64-mac_10.9_mavericks-darwin13.4.0-nogui/csl/reduce.img -o ~/red/src6/cr.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "PSL-REDUCE on Sharcnet (with 160 GB): ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/work/neun/hound/reduce-algebra/trunk/bin/redpsl -td 160000\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (PROGN (PRIN2 "CSL-REDUCE on Sharcnet: ") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "\"/work/schrufer/hound/Reduce-Algebra/trunk/bin/redcsl -w -i /work/schrufer/hound/Reduce-Algebra/trunk/cslbuild/x86_64-unknown-linux-gnu/csl/reduce.img -o ~/red/src6/cr64.img\"")
           NIL)
          (TERPRI)
          (TERPRI)
          (CHANGE_PROMPT_TO "")
          (SETQ REDUCE_CALL (TERMREAD))
          (RESTORE_INTERACTIVE_PROMPT)
          (SETQ REDUCE_CALL (BLDMSG_INTERNAL "%w" (LIST REDUCE_CALL)))
          NIL))))))) 
(PUT 'READ_PROCZAEHLER 'NUMBER-OF-ARGS 0) 
(PUT 'READ_PROCZAEHLER 'DEFINED-ON-LINE '361) 
(PUT 'READ_PROCZAEHLER 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'READ_PROCZAEHLER 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_PROCZAEHLER NIL
    (PROG (FL FPID *ECHO SEMIC*)
      (SETQ FPID (BLDMSG_INTERNAL "%s.%w" (LIST PROCESS_COUNTER (GETPID))))
      (SETQ FL 1)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ FL (COPY-FILE PROCESS_COUNTER FPID))
         (COND ((NULL FL) (SLEEP 1))))
        (COND ((NOT FL) (GO REPEATLABEL))))
      (SETQ SEMIC* '$)
      (IN (LIST FPID))
      (SYSTEM (BLDMSG_INTERNAL "rm %s" (LIST FPID)))
      (COND ((NOT (FIXP BACKUP_)) (SETQ BACKUP_ 1000000)))
      (RETURN BACKUP_))) 
(PUT 'ADD_SESSION 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_SESSION 'DEFINED-ON-LINE '432) 
(PUT 'ADD_SESSION 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'ADD_SESSION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_SESSION (PDES FORG PARA_MODE)
    (PROG (S SS H CURRENT_DIR STARTUP P ID A SAVE OFL*BAK)
      (SETQ SS (LEVEL_STRING SESSION_))
      (SETQ CURRENT_DIR
              (COND ((NEQ PARA_CASE_DIR "") PARA_CASE_DIR) (T (PWD))))
      (SETQ H (BLDMSG_INTERNAL "%w%w%w" (LIST CURRENT_DIR SS "tmp")))
      (SETQ S EQN_INPUT)
      (COND
       ((EQUAL EQN_INPUT 'DONE)
        (PROGN (SETQ EQUATIONS_FILE "") (SETQ EQN_NO 0))))
      (SETQ EQN_INPUT NIL)
      (CRACK_LOAD_CMD)
      (COND
       ((EQUAL CRACK_LOAD_COMMAND "not found")
        (RETURN
         (PROGN
          (SHUT (LIST STARTUP))
          (SETQ S (BLDMSG_INTERNAL "rm %w" (LIST STARTUP)))
          (SYSTEM S)
          (PROGN
           (PRIN2
            "##### This parallelization could not be started as the call of crack")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### could not be found. Please have at the start of your computation")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### a line     load \"path/crack\"$         with suitable path, if")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### necessary  load \"./crack\"$ when loaded from active directory.")
           NIL)
          (TERPRI)
          NIL))))
      (REDUCE_CALL_CMD)
      (BACKUP_TO_FILE PDES FORG H)
      (SETQ EQN_INPUT S)
      (SETQ STARTUP (BLDMSG_INTERNAL "%w%w%w" (LIST CURRENT_DIR SS "sup")))
      (SETQ A (OPEN STARTUP 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* STARTUP)
      (SETQ SAVE (WRS A))
      (PROGN (PRIN2 "lisp$") NIL)
      (TERPRI)
      (COND
       (NIL
        (COND
         ((EQUAL PARA_MODE 2)
          (PROGN
           (PROGN (PRIN2 "if null getenv \"reduce\" then <<") NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             " write\"##### To start REDUCE automatically in a new screen window\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             " write\"##### the environment variable   reduce   must be assigned\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             " write\"##### the name of the REDUCE directory. For a bash shell this\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 " write\"##### could be done by adding once a line like\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 " write\"##### export reduce=/home/twolf/newreduce\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             " write\"##### For a c shell the line would be like this\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 " write\"##### setenv reduce /home/twolf/newreduce\"$")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 " terpri()$") NIL)
           (TERPRI)
           (PROGN (PRIN2 ">>$") NIL)
           (TERPRI)
           NIL)))))
      (PROGN
       (PRIN2 "write\" Session-level: ")
       (PRIN2 SS)
       (PRIN2 " \"$terpri()$")
       NIL)
      (TERPRI)
      (COND ((NULL *INT) (PROGN (PROGN (PRIN2 "off int$") NIL) (TERPRI))))
      (COND (*GC (PROGN (PROGN (PRIN2 "on gc$") NIL) (TERPRI))))
      (PROGN
       (PRIN2 "load_package \"")
       (PRIN2 CRACK_LOAD_COMMAND)
       (PRIN2 "\" $")
       NIL)
      (TERPRI)
      (COND
       (CRACK_INI_FILE
        (PROGN
         (PROGN (PRIN2 "load \"") (PRIN2 CRACK_INI_FILE) (PRIN2 "\"$") NIL)
         (TERPRI)
         NIL)))
      (SETQ P (LINELENGTH 500))
      (PROGN (PRIN2 "old_history:='(rb ") NIL)
      (TERPRI)
      (COND (*BATCH_MODE (PROGN (PRIN2 "\"") (PRIN2 H) (PRIN2 "\")$") NIL))
            (T (PROGN (PRIN2 "\"") (PRIN2 H) (PRIN2 "\" s)$") NIL)))
      (TERPRI)
      (LINELENGTH P)
      (PROGN (PRIN2 "off batch_mode$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "crackshell()$") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "proczaehler(\"")
       (PRIN2 PROCESS_COUNTER)
       (PRIN2 "\",'minus)$")
       NIL)
      (TERPRI)
      (COND
       (VERIFY_END_OF_PARALLEL_RUN
        (PROGN
         (PROGN
          (PRIN2
           "write\"Is the computation ok and can the input files be deleted?\"$")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "terpri()$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "write\"Please input Y/N : \"$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "change_prompt_to \"\"$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "if 'y=termread() then <<") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  system \"rm ") (PRIN2 H) (PRIN2 "\"$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  system \"rm ") (PRIN2 STARTUP) (PRIN2 "\"$") NIL)
         (TERPRI)
         (PROGN (PRIN2 ">> else <<") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "  write\"Files ")
          (PRIN2 H)
          (PRIN2 ", ")
          (PRIN2 STARTUP)
          (PRIN2 " will not be deleted.\"$")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "  system\"sleep 5\"$ ") NIL)
         (TERPRI)
         (PROGN (PRIN2 ">>$") NIL)
         NIL))
       (T
        (PROGN
         (PROGN (PRIN2 "system \"rm ") (PRIN2 H) (PRIN2 "\"$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "system \"rm ") (PRIN2 STARTUP) (PRIN2 "\"$") NIL)
         (TERPRI)
         NIL)))
      (PROGN (PRIN2 "eval '(bye)$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (COND
       ((EQUAL PARA_MODE 1)
        (COND
         (*ICONIC
          (SETQ S
                  (BLDMSG_INTERNAL
                   "cd %w;xterm -iconic -e \"%w -d pre_read_=%w\" &"
                   (LIST CURRENT_DIR REDUCE_CALL STARTUP))))
         (T
          (SETQ S
                  (BLDMSG_INTERNAL "cd %w;xterm -e \"%w -d pre_read_=%w\" &"
                                   (LIST CURRENT_DIR REDUCE_CALL STARTUP))))))
       ((EQUAL PARA_MODE 2)
        (SETQ S
                (BLDMSG_INTERNAL "screen -h 10000 %w -d pre_read_=%w"
                                 (LIST REDUCE_CALL STARTUP))))
       ((EQUAL PARA_MODE 3)
        (SETQ S
                (BLDMSG_INTERNAL
                 "sqsub -o %w%wout -e %w%werr -t %w -d pre_read_=%w"
                 (LIST CURRENT_DIR SS CURRENT_DIR SS REDUCE_CALL STARTUP)))))
      (PROCZAEHLER PROCESS_COUNTER 'PLUS)
      (COND
       ((EQUAL PARA_MODE 3)
        (PROGN
         (SETQ P (PIPE-OPEN S 'INPUT))
         (SETQ ID "")
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ H (CHANNELREADCHAR P))
            (COND
             ((AND (GREATERP H 47) (LESSP H 58))
              (SETQ ID (BLDMSG_INTERNAL "%w%w" (LIST ID (INT2ID H)))))))
           (COND ((NOT (EQUAL H 4)) (GO REPEATLABEL))))
         (CLOSE P)
         NIL))
       (T (SYSTEM S)))
      (PROGN
       (PRIN2 " A process for case ")
       (PRIN2 SS)
       (PRIN2 " has been started.")
       NIL)
      (TERPRI))) 
(PUT 'ADD_PROCESS 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_PROCESS 'DEFINED-ON-LINE '605) 
(PUT 'ADD_PROCESS 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'ADD_PROCESS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_PROCESS (PROCESSES PDES FORG)
    (PROG (S SS H CURRENT_DIR)
      (SYSTEM "pvm")
      (SETQ SS (LEVEL_STRING SESSION_))
      (SETQ CURRENT_DIR
              (COND ((NEQ PARA_CASE_DIR "") PARA_CASE_DIR) (T (PWD))))
      (SETQ H (BLDMSG_INTERNAL "%w%w%w" (LIST CURRENT_DIR SS "tmp")))
      (SETQ S EQN_INPUT)
      (SETQ EQN_INPUT NIL)
      (BACKUP_TO_FILE PDES FORG H)
      (SETQ EQN_INPUT S)
      (SETQ S (REMOTE_PROCESS ""))
      (SETQ PROCESSES (CONS S PROCESSES))
      (PROCZAEHLER PROCESS_COUNTER 'PLUS)
      (REMOTE_WRITE S (LIST 'LIST "Process id: " S " level: " SS))
      (TERPRI)
      (PROGN
       (PRIN2 "A process with id no ")
       (PRIN2 S)
       (PRIN2 " has been started.")
       NIL)
      (REMOTE_CALL* S 'CD (LIST CURRENT_DIR) 0)
      (COND ((NULL *INT) (REMOTE_CALL* S 'ONOFF (LIST 'INT NIL) 0)))
      (COND (*GC (REMOTE_CALL* S 'ONOFF (LIST 'GC T) 0)))
      (CRACK_LOAD_CMD)
      (COND
       ((EQUAL CRACK_LOAD_COMMAND "not found")
        (RETURN
         (PROGN
          (PROGN
           (PRIN2
            "##### This parallelization could not be started as the call of crack")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### could not be found. Please have at the start of your computation")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### a line     load \"path/crack\"$         with suitable path, if")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "##### necessary  load \"./crack\"$ when loaded from active directory.")
           NIL)
          (TERPRI)
          (REMOTE_CALL* S 'EXITLISP (LIST 'LIST) 0)
          NIL))))
      (REMOTE_CALL* S 'LOAD_PACKAGE (LIST (LIST CRACK_LOAD_COMMAND)) 0)
      (COND
       (CRACK_INI_FILE
        (REMOTE_CALL* S 'ERR_CATCH_READIN (LIST CRACK_INI_FILE 'ALGEBRAIC) 0)))
      (REMOTE_CALL* S 'SET (LIST 'OLD_HISTORY (LIST 'RB H)) 0)
      (REMOTE_CALL S 'CRACKSHELL (LIST 'LIST) 0)
      (COND ((NULL *ICONIC) (REMOTE_CALL* S 'SYSTEM (LIST "sleep 1000") 0)))
      (PROCZAEHLER PROCESS_COUNTER 'MINUS)
      (REMOTE_CALL* S 'SYSTEM (LIST (BLDMSG_INTERNAL "%w%w" (LIST "rm " H))) 0)
      (REMOTE_CALL* S 'EXITLISP (LIST 'LIST) 0)
      (RETURN PROCESSES))) 
(PUT 'DROP_PROCESS 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_PROCESS 'DEFINED-ON-LINE '698) 
(PUT 'DROP_PROCESS 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'DROP_PROCESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_PROCESS (PROCESSES)
    (PROG (S)
      (TERPRI)
      (PROGN
       (PRIN2 "The following processes had been activated in this order: ")
       NIL)
      (LISTPRINT (REVERSE PROCESSES))
      (TERPRI)
      (PROGN (PRIN2 "To kill ALL remote processes that have been") NIL)
      (TERPRI)
      (PROGN (PRIN2 "generated by this process enter -1,") NIL)
      (TERPRI)
      (PROGN (PRIN2 "to kill a single process enter its process id: ") NIL)
      (CHANGE_PROMPT_TO "")
      (SETQ S (TERMREAD))
      (RESTORE_INTERACTIVE_PROMPT)
      (COND
       ((EQUAL S (MINUS 1))
        (PROGN (SETQ PROCESSES NIL) (PROCZAEHLER PROCESS_COUNTER 'INIT) NIL))
       (T
        (PROGN
         (SETQ PROCESSES (DELETE S PROCESSES))
         (PROCZAEHLER PROCESS_COUNTER 'MINUS)
         NIL)))
      (REMOTE_KILL S)
      (RETURN PROCESSES))) 
(PUT 'CRACKMAIN_IF_POSSIBLE_REMOTE 'NUMBER-OF-ARGS 2) 
(PUT 'CRACKMAIN_IF_POSSIBLE_REMOTE 'DEFINED-ON-LINE '720) 
(PUT 'CRACKMAIN_IF_POSSIBLE_REMOTE 'DEFINED-IN-FILE 'CRACK/CRPVM.RED) 
(PUT 'CRACKMAIN_IF_POSSIBLE_REMOTE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CRACKMAIN_IF_POSSIBLE_REMOTE (PDES FORG)
    (COND
     ((OR COLLECT_SOL (NULL AUTO_PARA_MODE)
          (GEQ (READ_PROCZAEHLER) MAX_PROC_NO))
      (PROGN
       (COND
        ((AND COLLECT_SOL AUTO_PARA_MODE)
         (PROGN
          (PROGN
           (PRIN2 "Currently is collect_sol=t. Therefore parallel case")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "solving is not enabled because solutions would not")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "be collected even though auto_para_mode<>nil.") NIL)
          (TERPRI)
          (PROGN (PRIN2 "Therefore, either set collect_sol to nil using") NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "'as {collect_sol,nil};' or disable parallelism with 'dp' .")
           NIL)
          (TERPRI)
          NIL)))
       (COND ((NULL BATCH_MODE_SUB) (SETQ *BATCH_MODE NIL)))
       (CRACKMAIN PDES FORG)))
     (T
      (PROG (PROCESSES S)
        (COND ((NULL BATCH_MODE_SUB) (SETQ *BATCH_MODE NIL)))
        (COND ((LESSP AUTO_PARA_MODE 4) (ADD_SESSION PDES FORG AUTO_PARA_MODE))
              (T (SETQ PROCESSES (ADD_PROCESS PROCESSES PDES FORG))))
        (FINISH_LEVEL 0)
        (DROP_ALL_PDES PDES)
        (PROG (S)
          (SETQ S FORG)
         LAB
          (COND ((NULL S) (RETURN NIL)))
          ((LAMBDA (S)
             (COND ((PAIRP S) (SETPROP (CADR S) NIL)) (T (SETPROP S NIL))))
           (CAR S))
          (SETQ S (CDR S))
          (GO LAB)))))) 
(ENDMODULE) 
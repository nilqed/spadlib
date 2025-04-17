(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HELPASST)) 
(PUT 'ASSIST 'NUMBER-OF-ARGS 0) 
(FLAG '(ASSIST) 'OPFN) 
(PUT 'ASSIST 'DEFINED-ON-LINE '28) 
(PUT 'ASSIST 'DEFINED-IN-FILE 'ASSIST/HELPASST.RED) 
(PUT 'ASSIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ASSIST NIL
    (PROGN
     (ASSGNPRI
      (AEVAL " Argument of ASSISTHELP must be an integer between 3 and 14. ")
      NIL 'ONLY)
     (ASSGNPRI
      (AEVAL
       " Each integer corresponds to a section number in the documentation:")
      NIL 'ONLY)
     (PROGN
      (ASSGNPRI (AEVAL " 3: switches ") NIL 'FIRST)
      (ASSGNPRI (AEVAL "   4: lists      ") NIL NIL)
      (ASSGNPRI (AEVAL "  5: bags    ") NIL NIL)
      (ASSGNPRI (AEVAL "  6: sets ") NIL 'LAST))
     (PROGN
      (ASSGNPRI (AEVAL " 7: utilities ") NIL 'FIRST)
      (ASSGNPRI (AEVAL "  8: properties and flags ") NIL NIL)
      (ASSGNPRI (AEVAL " 9: control functions ") NIL 'LAST))
     (ASSGNPRI (AEVAL " 10: handling of polynomials ") NIL 'ONLY)
     (ASSGNPRI (AEVAL " 11: handling of transcendental functions") NIL 'ONLY)
     (ASSGNPRI (AEVAL " 12: handling of n-dimensional vectors ") NIL 'ONLY)
     (PROGN
      (ASSGNPRI (AEVAL " 13: grassmann variables ") NIL 'FIRST)
      (ASSGNPRI (AEVAL "          14: matrices") NIL 'LAST))
     (AEVAL 'NIL))) 
(PUT 'ASSISTHELP 'NUMBER-OF-ARGS 1) 
(FLAG '(ASSISTHELP) 'OPFN) 
(PUT 'ASSISTHELP 'DEFINED-ON-LINE '40) 
(PUT 'ASSISTHELP 'DEFINED-IN-FILE 'ASSIST/HELPASST.RED) 
(PUT 'ASSISTHELP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASSISTHELP (N)
    (COND ((EVALEQUAL (AEVAL N) (AEVAL 'ASSIST)) (AEVAL (LIST 'ASSIST)))
          ((NOT (FIXP (REVALX N)))
           (AEVAL (REDERR (REVALX "Argument must be an integer"))))
          ((EVALGEQ (AEVAL N) 15) (AEVAL "Argument must be less then 15"))
          ((EVALLESSP (AEVAL N) 3)
           (AEVAL "Argument must be greater or equal to 3"))
          (T
           (PROG (XX)
             (SETQ XX (AEVAL (LIST 'ASFLIST N 'ASSIST_FUNC)))
             (RETURN
              (COND
               ((EVALEQUAL (AEVAL (LIST 'LENGTH XX)) 1)
                (AEVAL (LIST 'REST (LIST 'FIRST XX))))
               (T
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I (GETRLIST (AEVAL XX)))
                  (COND ((NULL I) (RETURN (MAKELIST NIL))))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (I) (AEVAL (LIST 'REST I)))
                                    (CAR I))
                                   NIL)))
                 LOOPLABEL
                  (SETQ I (CDR I))
                  (COND ((NULL I) (RETURN (CONS 'LIST FORALL-RESULT))))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (I) (AEVAL (LIST 'REST I))) (CAR I))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))))))) 
(SETK 'ASSIST_FUNC
      (AEVAL
       (LIST 'LIST (LIST 'LIST 3 "switches" "switchorg")
             (LIST 'LIST 4 "dot" "mklist" "algnlist" "frequency" "sequences"
                   "split" "kernlist")
             (LIST 'LIST 4 "delete" "delete_all" "remove")
             (LIST 'LIST 4 "elmult" "insert" "insert_keep_order" "merge_list")
             (LIST 'LIST 4 "last" "belast" "position" "depth" "mkdepth_one"
                   "pair" "delpair" "appendn")
             (LIST 'LIST 4 "repfirst" "represt" "asfirst" "aslast" "asrest"
                   "restaslist" "asflist" "asslist")
             (LIST 'LIST 5 "putbag" "clearbag" "bagp" "baglistp" "alistp"
                   "abaglistp" "listbag")
             (LIST 'LIST 6 "union" "setp" "mkset" "diffset" "symdiff")
             (LIST 'LIST 7 "mkidnew" "list_to_ids" "oddp" "followline"
                   "detidnum" "dellastdigit" "==")
             (LIST 'LIST 7 "randomlist" "mkrandtabl")
             (LIST 'LIST 7 "permutations" "perm_to_num" "num_to_perm" "combnum"
                   "combinations" "cyclicpermlist" "symmetrize" "remsym")
             (LIST 'LIST 7 "extremum" "sortnumlist" "sortlist" "algsort")
             (LIST 'LIST 7 "funcvar" "implicit" "depatom" "explicit" "simplify"
                   "korderlist" "remcom")
             (LIST 'LIST 7 "checkproplist" "extractlist" "array_to_list"
                   "list_to_array")
             (LIST 'LIST 7 "remvector" "remindex" "mkgam")
             (LIST 'LIST 8 "putflag" "putprop" "displayprop" "displayflag"
                   "clearflag" "clearprop")
             (LIST 'LIST 9 "nordp" "depvarp" "alatomp" "alkernp" "precp")
             (LIST 'LIST 9 "show" "suppress" "clearop" "clearfunctions")
             (LIST 'LIST 10 "alg_to_symb" "symb_to_alg")
             (LIST 'LIST 10 "gcdnl" "distribute" "leadterm" "redexpr" "monom"
                   "lowestdeg" "splitterms" "splitplusminus" "norm_mon"
                   "norm_pol" "list_coeff_pol")
             (LIST 'LIST 11 "trigexpand" "hypexpand" "trigreduce" "hypreduce")
             (LIST 'LIST 12 "sumvect" "minvect" "sscalvect" "crossvect"
                   "mpvect")
             (LIST 'LIST 13 "putgrass" "remgrass" "grassp" "grassparity"
                   "ghostfactor")
             (LIST 'LIST 14 "mkidm" "baglmat" "coercemat" "unitmat" "submat"
                   "matsubr" "matsubc" "matextr" "matextc")
             (LIST 'LIST 14 "hconcmat" "vconcmat" "tpmat" "hermat" "seteltmat"
                   "geteltmat")))) 
(ENDMODULE) 
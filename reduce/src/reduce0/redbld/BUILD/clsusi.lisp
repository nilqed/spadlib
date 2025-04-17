(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLSUSI)) 
(REVISION 'CLSUSI "$Id: clsusi.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'CLSUSI "(c) 1995-2021 A. Dolzmann, T. Sturm") 
(PUT 'CL_SUSIMKATL 'NUMBER-OF-ARGS 4) 
(DE CL_SUSIMKATL (OP KNOWL NEWKNOWL N)
    (PROG (RES)
      (SETQ RES
              (PROG (PAIR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PAIR NEWKNOWL)
               STARTOVER
                (COND ((NULL PAIR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PAIR)
                           (COND ((EQUAL (CDR PAIR) N) (LIST (CAR PAIR)))))
                         (CAR PAIR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PAIR (CDR PAIR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PAIR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PAIR)
                           (COND ((EQUAL (CDR PAIR) N) (LIST (CAR PAIR)))))
                         (CAR PAIR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PAIR (CDR PAIR))
                (GO LOOPLABEL)))
      (COND ((NULL RES) (RETURN NIL)))
      (SETQ RES (RL_SUSIPOST RES KNOWL))
      (COND
       ((OR (EQ RES 'TRUE) (EQ RES 'FALSE))
        (RETURN (LIST (CL_CFLIP RES (EQ OP 'AND))))))
      (COND ((EQ RES 'INCTHEO) (RETURN (CL_CFLIP 'FALSE (EQ OP 'AND)))))
      (COND
       ((EQ OP 'OR)
        (SETQ RES
                (PROG (AT FORALL-RESULT FORALL-ENDPTR)
                  (SETQ AT RES)
                  (COND ((NULL AT) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (AT) (RL_NEGATEAT AT)) (CAR AT))
                                   NIL)))
                 LOOPLABEL
                  (SETQ AT (CDR AT))
                  (COND ((NULL AT) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (AT) (RL_NEGATEAT AT)) (CAR AT)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ RES
              (PROG (AT FORALL-RESULT FORALL-ENDPTR)
                (SETQ AT RES)
                (COND ((NULL AT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (AT) (RL_SUSITF AT KNOWL)) (CAR AT))
                                 NIL)))
               LOOPLABEL
                (SETQ AT (CDR AT))
                (COND ((NULL AT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (AT) (RL_SUSITF AT KNOWL)) (CAR AT))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'CL_SUSICPKNOWL 'NUMBER-OF-ARGS 1) 
(DE CL_SUSICPKNOWL (KNOWL)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P KNOWL)
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (P) (CONS (CAR P) (CDR P))) (CAR P))
                            NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (P) (CONS (CAR P) (CDR P))) (CAR P)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_SUSIUPDKNOWL 'NUMBER-OF-ARGS 4) 
(DE CL_SUSIUPDKNOWL (OP ATL KNOWL N)
    (PROG (AT)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ AT (PROG1 (CAR ATL) (SETQ ATL (CDR ATL))))
         (SETQ KNOWL (CL_SUSIUPDKNOWL1 OP AT KNOWL N))
         (COND ((EQ KNOWL 'FALSE) (PROGN (SETQ ATL NIL) (SETQ AT 'BREAK)))))
        (GO WHILELABEL))
      (COND ((EQ AT 'BREAK) (RETURN 'FALSE)) (T (RETURN KNOWL))))) 
(PUT 'CL_SUSIUPDKNOWL1 'NUMBER-OF-ARGS 4) 
(DE CL_SUSIUPDKNOWL1 (OP AT KNOWL N)
    (COND ((EQ OP 'AND) (CL_SUSIUPDKNOWL2 (CONS AT N) KNOWL N))
          (T
           (PROGN
            (COND (NIL NIL))
            (CL_SUSIUPDKNOWL2 (CONS (RL_NEGATEAT AT) N) KNOWL N))))) 
(PUT 'CL_SUSIUPDKNOWL2 'NUMBER-OF-ARGS 3) 
(DE CL_SUSIUPDKNOWL2 (LAT KNOWL N)
    (PROG (A W SCK IGNFLG DELFLG ADDL TERM)
      (SETQ SCK KNOWL)
      (PROG ()
       WHILELABEL
        (COND ((NOT SCK) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR SCK) (SETQ SCK (CDR SCK))))
         (SETQ W (RL_SUSIBIN A LAT))
         (COND ((EQ W 'FALSE) (SETQ SCK NIL))
               (T
                (PROGN
                 (COND (NIL NIL))
                 (SETQ W (CL_SUSIINTER W KNOWL A))
                 (SETQ ADDL (NCONC ADDL (CADR W)))
                 (SETQ KNOWL (CAR W))
                 (COND ((CADDR W) (SETQ IGNFLG T)))
                 (COND ((CADDDR W) (PROGN (SETQ DELFLG T) (SETQ SCK NIL))))))))
        (GO WHILELABEL))
      (COND ((EQ W 'FALSE) (RETURN 'FALSE)))
      (COND ((NULL DELFLG) (PROGN (SETQ KNOWL (CONS LAT KNOWL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT ADDL) (RETURN NIL)))
        (PROGN
         (SETQ KNOWL (CL_SUSIUPDKNOWL2 (CAR ADDL) KNOWL N))
         (COND ((EQ KNOWL 'FALSE) (SETQ ADDL NIL)) (T (SETQ ADDL (CDR ADDL)))))
        (GO WHILELABEL))
      (RETURN KNOWL))) 
(PUT 'CL_SUSIINTER 'NUMBER-OF-ARGS 3) 
(DE CL_SUSIINTER (PRG KNOWL A)
    (PROG (ADDL IGNFLG DELFLG)
      (PROG (P)
        (SETQ P PRG)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((OR (EQ (CAR P) 'DELETE) (EQ (CAR P) 'IGNORE))
             (COND ((CDR P) (SETQ DELFLG T))
                   (T (SETQ KNOWL (LTO_DELQIP A KNOWL)))))
            ((EQ (CAR P) 'ADD) (SETQ ADDL (CONS (CDR P) ADDL)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (LIST KNOWL ADDL IGNFLG DELFLG)))) 
(PUT 'CL_SUSIMINLEVEL 'NUMBER-OF-ARGS 2) 
(DE CL_SUSIMINLEVEL (L1 L2)
    (COND ((EQ L1 'IGNORE) L2) ((EQ L2 'IGNORE) L1) (T (MIN L1 L2)))) 
(ENDMODULE) 
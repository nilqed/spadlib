(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'XIDEAL)) 
(LOAD_PACKAGE (LIST 'EXCALC)) 
(CREATE-PACKAGE '(XIDEAL XGROEB XREDUCT XCRIT XPOWERS XSTORAGE XAUX XEXCALC)
                '(CONTRIB XIDEAL)) 
(FLUID '(*XFULLREDUCE *TRXIDEAL *TWOSIDED *TRXMOD)) 
(SWITCH (LIST 'XFULLREDUCE 'TRXIDEAL 'TWOSIDED 'TRXMOD)) 
(SETQ *XFULLREDUCE T) 
(SETQ *TRXIDEAL NIL) 
(SETQ *TWOSIDED NIL) 
(SETQ *TRXMOD NIL) 
(FLUID '(XVARS* XTRUNCATE* XVARLIST* XDEGREELIST* ZERODIVS* XPOLYLIST*)) 
(SETQ XVARS* T) 
(SETQ XTRUNCATE* NIL) 
(SETQ XVARLIST* (LIST)) 
(SETQ XDEGREELIST* (LIST)) 
(SETQ ZERODIVS* (LIST)) 
(SETQ XPOLYLIST* (LIST)) 
(PUT 'PR_RHS 'NUMBER-OF-ARGS 1) 
(PUTC 'PR_RHS 'INLINE '(LAMBDA (U) (CADDR (CDR U)))) 
(DE PR_RHS (U) (CADDR (CDR U))) 
(PUT 'PR_RHS 'SETQFN '(LAMBDA (U V) (SETCAR (CDDDR U) V))) 
(PUT 'SET_PR_RHS 'NUMBER-OF-ARGS 2) 
(PUTC 'SET_PR_RHS 'INLINE '(LAMBDA (U V) (SETCAR (CDDDR U) V))) 
(DE SET_PR_RHS (U V) (SETCAR (CDDDR U) V)) 
(PUT 'PR_LHS 'NUMBER-OF-ARGS 1) 
(PUTC 'PR_LHS 'INLINE '(LAMBDA (U) (CADDR U))) 
(DE PR_LHS (U) (CADDR U)) 
(PUT 'PR_LHS 'SETQFN '(LAMBDA (U V) (SETCAR (CDDR U) V))) 
(PUT 'SET_PR_LHS 'NUMBER-OF-ARGS 2) 
(PUTC 'SET_PR_LHS 'INLINE '(LAMBDA (U V) (SETCAR (CDDR U) V))) 
(DE SET_PR_LHS (U V) (SETCAR (CDDR U) V)) 
(PUT 'PR_TYPE 'NUMBER-OF-ARGS 1) 
(PUTC 'PR_TYPE 'INLINE '(LAMBDA (U) (CADR U))) 
(DE PR_TYPE (U) (CADR U)) 
(PUT 'PR_TYPE 'SETQFN '(LAMBDA (U V) (SETCAR (CDR U) V))) 
(PUT 'SET_PR_TYPE 'NUMBER-OF-ARGS 2) 
(PUTC 'SET_PR_TYPE 'INLINE '(LAMBDA (U V) (SETCAR (CDR U) V))) 
(DE SET_PR_TYPE (U V) (SETCAR (CDR U) V)) 
(PUT 'XKEY 'NUMBER-OF-ARGS 1) 
(PUTC 'XKEY 'INLINE '(LAMBDA (U) (CAR U))) 
(DE XKEY (U) (CAR U)) 
(PUT 'XKEY 'SETQFN '(LAMBDA (U V) (SETCAR U V))) 
(PUT 'SET_XKEY 'NUMBER-OF-ARGS 2) 
(PUTC 'SET_XKEY 'INLINE '(LAMBDA (U V) (SETCAR U V))) 
(DE SET_XKEY (U V) (SETCAR U V)) 
NIL 
(DE EMPTY_XSET NIL (CONS '*XSET* NIL)) 
(PUT 'EMPTY_XSET 'NUMBER-OF-ARGS 0) 
(PUT 'EMPTY_XSET 'DEFINED-ON-LINE '172) 
(PUT 'EMPTY_XSET 'DEFINED-IN-FILE 'XIDEAL/XIDEAL.RED) 
(PUT 'EMPTY_XSET 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'EMPTY_XSET 'INLINE '(LAMBDA () (CONS '*XSET* NIL))) 
(DE EMPTY_XSETP (C) (NULL (CDR C))) 
(PUT 'EMPTY_XSETP 'NUMBER-OF-ARGS 1) 
(PUT 'EMPTY_XSETP 'DEFINED-ON-LINE '175) 
(PUT 'EMPTY_XSETP 'DEFINED-IN-FILE 'XIDEAL/XIDEAL.RED) 
(PUT 'EMPTY_XSETP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'EMPTY_XSETP 'INLINE '(LAMBDA (C) (NULL (CDR C)))) 
(DE XSET_ITEM (C) (CAR C)) 
(PUT 'XSET_ITEM 'NUMBER-OF-ARGS 1) 
(PUT 'XSET_ITEM 'DEFINED-ON-LINE '178) 
(PUT 'XSET_ITEM 'DEFINED-IN-FILE 'XIDEAL/XIDEAL.RED) 
(PUT 'XSET_ITEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'XSET_ITEM 'INLINE '(LAMBDA (C) (CAR C))) 
(PUT 'LPDF 'NUMBER-OF-ARGS 1) 
(PUTC 'LPDF 'INLINE '(LAMBDA (U) (CAAR U))) 
(DE LPDF (U) (CAAR U)) 
(PUT 'LPDF 'SETQFN '(LAMBDA (U V) (SETCAR (CAR U) V))) 
(PUT 'SET_LPDF 'NUMBER-OF-ARGS 2) 
(PUTC 'SET_LPDF 'INLINE '(LAMBDA (U V) (SETCAR (CAR U) V))) 
(DE SET_LPDF (U V) (SETCAR (CAR U) V)) 
NIL 
(ENDMODULE) 
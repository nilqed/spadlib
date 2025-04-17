(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)

(load "sl-on-cl")
(load "clprolo")
(load "revision")

;;;;;;;;;;
;;; RLISP
;;;;;;;;;;
(load "rlisp")
(load "module")
(load "newtok")
(load "rsupport")
(load "slfns")
(load "superv")
(load "tok")
(load "xread")
(load "lpri")
(load "parser")
(load "block")
(load "form")
(load "proc")
(load "forstat")
(load "loops")
(load "statmisc")
(load "smacro")
(load "io")
(load "infix")
(load "switch")
(load "where")
(load "list")
(load "array")
(load "inter")
(load "charname")
(load "newtok1")

;;; clrend
(load "clrend")

;;;;;;;;;;
;;; POLY
;;;;;;;;;;
(load "poly")
(load "polrep")
(load "polydiv")
(load "quotf")
(load "gcd")
(load "exptf")
(load "kernel")
(load "mksp")
(load "reord")
(load "dmode")
(load "dmodeop")
(load "rational")
(load "rnelem")
(load "gint")
(load "cpxrn")
(load "compopr")
(load "modular")
(load "facform")
(load "homog")
(load "tdconv")
(load "primfac")
(load "specfac")
(load "kronf")
(load "conj")
(load "diff")
(load "polyop")
(load "decompos")
(load "interpol")
(load "subs2q")
(load "subs3q")
(load "subs4q")
(load "horner")
(load "heugcd")


;;;;;;;;;
;;; ALG
;;;;;;;;;
(load "alg")
(load "alg-form")
(load "intro")
(load "lifted")
(load "farith")
(load "numsup")
(load "zfactor")
(load "reval")
(load "algbool")
(load "simp")
(load "exptchk")
(load "simplog")
(load "logsort")
(load "sub")
(load "order")
(load "forall")
(load "eqn")
(load "rmsubs")
(load "algdcl")
(load "opmtch")
(load "prep")
(load "extout")
(load "depend")
(load "str")
(load "coeff")
(load "weight")
(load "linop")
(load "elem")
(load "showrule")
(load "nestrad")
(load "maxmin")
(load "nssimp")
(load "part")
(load "map")
(load "spcfnint")


;;;;;;;;;;;;
;;; RTOOLS
;;;;;;;;;;;;
(load "rtools")
(load "general")
(load "rprintf")
(load "random")
(load "genmod")
(load "smallmod")
(load "sort")
(load "simplertrace")

;;;;;;;;;;;
;;; ARITH
;;;;;;;;;;;
(load "arith")
(load "smlbflot")
(load "bfauxil")
(load "paraset")
(load "math")
(load "rounded")
(load "comprd")
(load "rdelem")
(load "crelem")
(load "bfelem")

;;; entry/remake
(load "entry")
(load "remake")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END CORE PACKAGES  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXCALC
(load "excalc")
(load "exintro")
(load "exaux")
(load "degform")
(load "exdf")
(load "forder")
(load "frames")
(load "hodge")
(load "idexf")
(load "indices")
(load "indsymm")
(load "indxprin")
(load "innerprd")
(load "killing_vector")
(load "liedf")
(load "lievalfm")
(load "partdf")
(load "partitsf")
(load "vardf")
(load "vecanlys")
(load "exlists")
(load "wedge")
;;; contrib excalc ??









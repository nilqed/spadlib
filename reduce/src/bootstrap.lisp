;; Lisp code to build a REDUCE image for bootstrapping on Common Lisp
;; FJW, April 2022
;; modified: SBCL only, kfp, October 2024


(load "sl-on-cl")


#-DEBUG (declaim (optimize speed))
#+DEBUG (declaim (optimize debug safety))
#+SBCL (declaim (sb-ext:muffle-conditions sb-ext:compiler-note style-warning))

;;; added sl:: for fricas 
(sl::standard-lisp)   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STANDARD LISP SYNTAX FROM NOW ON! %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setq !*verboseload t)
(setq !*redefmsg nil)           % Just duplicates CL warnings!

(cl:defparameter !*init!-time!* (time))

(cl:defvar !*argnochk t)
(cl:defvar !*int nil)  % Prevents input buffer being saved.
(cl:defvar !*msg nil)

% Do not use fasl version of "boot.sl": the CL compiler may optimize
% away (i.e. discard) uses of fluid variables that are needed later in
% the build process!

(cond ((filep "boot.sl") (load "boot.sl"))
      ((filep "../psl/boot.sl") (load "../psl/boot.sl"))
      (t (error 0 "Cannot find boot file.") (exit 1)))

(setq !*comp t)  % It's faster in some lisps if we compile.

%%%%%%%%%%%%%%%%%%%%%%%
%% Start of build.sl %%
%%%%%%%%%%%%%%%%%%%%%%%

% The following code is essentially a Standard Lisp version
% of "packages/support/build.red".  It primarily defines the function
% load!-package!-sources, which is required for bootstrapping.

(global '(loaded!-packages!*))

% Since some of the early modules may have tabs in them, we must redefine
% seprp. Note that there is a TAB in this definition and that may not be
% readily visible when merely editing the file.

(de seprp (u) (or (eq u '! ) (eq u '!	) (eq u !$eol!$)))

(de mkfil (u)
   (cond
      ((stringp u) u)
      ((not (idp u)) (typerr u "file name"))
      (t (string!-downcase u))))

% Convert the module u in package directory v, or the current
% directory if v is nil, to a (lower-case) file name relative to the
% directory containing packages.  Also defined in remake.red!

(de module2!-to!-file (u v)
   (progn
      (setq u (concat2 (mkfil u) ".red"))
      (cond
         (v
            (concat2
               "packages/"
               (concat2 (mkfil v) (concat2 "/" u))))
         (t u))))

(de inmodule (u v)
   (prog (file)
      (terpri)
      (terpri)
      (prin2 "+++ Reading file: ")
      (prin2 (setq file (module2!-to!-file u v)))
      (terpri)
      (setq u (open file 'input))
      (setq v (rds u))
      (setq cursym!* '!*semicol!*)
      (prog nil
   whilelabel
         (cond ((not (not (eq cursym!* 'end))) (return nil)))
         (progn (prin2 (eval (form (xread nil)))) (prin2 " "))
         (go whilelabel))
      (rds v)
      (close u)))

(de load!-package!-sources (u v)
   (prog (!*int !*echo w)
      (inmodule u v)
      (cond ((setq w (get u 'package)) (setq w (cdr w))))
      (prog nil
   whilelabel
         (cond ((not w) (return nil)))
         (progn (inmodule (car w) v) (setq w (cdr w)))
         (go whilelabel))
      (setq loaded!-packages!* (cons u loaded!-packages!*))))

%%%%%%%%%%%%%%%%%%%%%
%% End of build.sl %%
%%%%%%%%%%%%%%%%%%%%%

(load!-package!-sources 'clprolo nil)
(load!-package!-sources 'revision 'support)
(load!-package!-sources 'rlisp 'rlisp)
(load!-package!-sources 'smacros 'support)
(load!-package!-sources 'clrend nil)
(load!-package!-sources 'poly 'poly)
(load!-package!-sources 'alg 'alg)
(load!-package!-sources 'rtools 'rtools)  
(load!-package!-sources 'arith 'arith)
(load!-package!-sources 'entry 'support)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% kfp ++++ more reduce packages below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(load!-package!-sources 'excalc 'excalc)  
(load!-package!-sources 'int 'int)  
(load!-package!-sources 'matrix 'matrix)  
(load!-package!-sources 'solve 'solve)  
(load!-package!-sources 'ineq 'solve)  
(load!-package!-sources 'tps 'tps)  
(load!-package!-sources 'limits 'limit)
(load!-package!-sources 'sum 'sum)    
(load!-package!-sources 'specfn  'specfn) 
(load!-package!-sources 'specfn2 'specfn)
(load!-package!-sources 'sfgamma 'specfn)
(load!-package!-sources 'specbess 'specfn)
(load!-package!-sources 'misc 'misc)
(load!-package!-sources 'defint 'defint)  
(load!-package!-sources 'rprint 'rprint)
(load!-package!-sources 'ezgcd 'factor)
(load!-package!-sources 'factor 'factor)  
(load!-package!-sources 'dipoly 'dipoly) 
(load!-package!-sources 'groebner 'groebner) 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%(load!-package!-sources 'remake nil)
%
%%% Notice: more packages must be placed before this !!!
%%%kfp/build in .red as sl above
%(load!-package!-sources 'build 'support)     
% not necessary -- use e.g. slEval "(load-package-sources 'groebner 'groebner)"

%% Package map, see 
%% github.com/nilqed/spadlib/blob/master/reduce/src/packages/package.map
%% https://sourceforge.net/p/reduce-algebra/code/5845/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



(setq !*comp nil)

% (load "compiler")

(prog nil
   (terpri)
   (prin2 "Time to build bootstrap REDUCE: ")
   (prin2 (quotient (difference (time) !*init!-time!*) 1000.0))
   (prin2t " secs")
   (prin2 "Heap left: ")
   (prin2 (gtheap))
   (prin2t " bytes")
)

(initreduce)
(setq date!* (date))
(setq version!* "Bootstrap REDUCE")
%%%%%%%%%%%% don't (save!-reduce!-image "bootstrap")

;;;(load "bootstrap.lisp")
;(sl::reduce-init-function)
;symbolic; 
;!*forcecompile := t;
;off redefmsg;
(load "trace")
(standard-lisp)

(cl:defparameter !*init!-stats!* (list (time) (gtheap)))

(setq !*verboseload t)
(setq !*redefmsg nil)
(cl:defvar !*argnochk t)        % check argument count

(load "module")                 % for definition of load-package
(load "clprolo")                % initial CL specific code

(cond ((equal "$revision" "") (load!-package 'revision))
      (t (setq revision!* "$revision")))
(load!-package 'rlisp)
(load!-package 'clrend)
(load!-package 'smacros)
(load!-package 'poly)
(load!-package 'arith)
(load!-package 'alg)
(load!-package 'rtools)
(load!-package 'mathpr)
(load!-package 'entry)

(cl:fmakunbound 'prettyprint)   % otherwise defautoload has no effect!
(defautoload prettyprint pretty)  % since only in entry file for PSL!

(setq date!* (date))
(setq version!* (cl:format nil "REDUCE (Free ~a version, revision ~a)"
      (cond ((memq 'sbcl lispsystem!*) "SBCL")
            ((memq 'clisp lispsystem!*) "CLISP")
            ((memq 'ccl lispsystem!*) "CCL"))
      revision!*))

(initreduce)

(setq !*verboseload nil)        % inhibit loading messages
(setq !*redefmsg t)             % display redefinition messages

(cond ((memq 'sbcl lispsystem!*)
       (setq !*muffled-warnings!* 'warning))) % exported from sb-ext


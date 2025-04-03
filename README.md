# SPADLIB V0.4 :rocket:


## Required

>  QuickLisp https://www.quicklisp.org/beta/
>
>  FriCAS    https://fricas.github.io/ 
>  
>  Optional: jfricas (Jupyter) https://pypi.org/project/jfricas/
  
## Install[^1]

    $ cd ~/quicklisp/local-projects
    $ git clone https://github.com/nilqed/spadlib.git
  
  
#### Note 
Add/merge the contents of `spadlib/system-index.txt` to 
`local-projects/system-index.txt` (or only those you like), so that
`quicklisp` can find the `ASDF` files (`*.asd`). This is not necessary in case the
packages are moved to ``local-project/<item>``, i.e without the ``spadlib``
folder.  
  
		  
## Usage

Add the following macros to your `~/.fricas.input` start file:

    )set message type off

    macro quickLoad(s) ==
      syscmd(s) ==> systemCommand(s)$MoreSystemCommands
      lisp2(f,a) ==> syscmd string(FORMAT('NIL,"lisp (~A _"~A_")",f,a)$Lisp)
      syscmd "lisp (defvar |$inclAssertions| NIL)" 
      lisp2("load", "~/quicklisp/setup")
      lisp2("ql:quickload",s)

    macro mjaxOn == 
      systemCommand("set output algebra off")
      setFormat!(FormatMathJax)$JFriCASSupport

    macro mjaxOff ==
      systemCommand("set output algebra on")
      systemCommand("set output formatted off")

    )set message type on


Then
  
    quickLoad "packageName"
    
should compile/load your package/domain/category.

  
#### Note 
If `quickLoad` is unknown after `FriCAS` start, you have to enter

        )frame next 

under certain circumstances (depends on start parameters).
        
**BTW** nothing will be changed to your `FriCAS` installation,
i.e. the packages are available at runtime only.    
  
## Create a package
The simple *shell script* `fripac.sh` may help to create a package, that is - in a nutshell - 
a certain directory structure, an **ASDF** file, a **Lisp** file and your sources (see the example
below).

In the `spadlib` folder do:

    $ ./fripac.sh "packageName" 
  
Type 

    ./fripac.sh -h 
    
for more options.

### Example package ``mypkg``

Skeleton:

    $ ls -R  ~/quicklisp/local-projects/spadlib/mypkg/
  
    docs/  lib/  src/  test/  mypkg.asd

    docs:
      README
      ...

    lib:
      README
      MYPKG.NRLIB (after installing)

    src:
      mypkg.lisp  
      mypkg.spad (your source file/s)

    test:
      README
      mypkg.input (your test file, optional)

   
The structure of `mypkg.asd`:

    (in-package :common-lisp-user)
    (asdf:defsystem #:mypkg
    :serial t
    :description "Short description"
    :version "1.0.0"
    :author "Your Name <your.name@nowhere.xyz>"
    :license "License"
    :depends-on ("trivial-shell", "whatever")
    :pathname "src/"
    :components ((:file "mypkg")))
    
  
The Lisp file `mypkg.lisp` has the general form (just the package name changes):  
  
    ;;;
    ;;; ASDF/QuickLisp
    ;;;
    (defparameter *mypkg* (asdf:system-source-directory :mypkg))

    (defun |compile_mypkg| ()
    (progn
    (|doSystemCommand| (format nil "cd ~Alib" *mypkg*))
    (|doSystemCommand| (format nil "compile ../src/mypkg.spad )quiet"))))

    (defun |load_mypkg| ()
    (if (probe-file (format nil "~Alib/mypkg.NRLIB/mypkg.lsp" *mypkg*))
       (|doSystemCommand| (format nil "lib )dir ~Alib/" *mypkg*))
       (|compile_mypkg|)))

    (defun |test_mypkg| ()
    (if (probe-file (format nil "~Atest/test_mypkg.input" *mypkg*))
      (|doSystemCommand| (format nil "read ~Atest/test_mypkg )quiet" *mypkg*))
      (print "Test file not found ...")))

    (catch 'spad_reader (|load_mypkg|))

 
 Therefore you can build a package manually as well.

---

### NOTES 

Before creating a new package, check if `packageName` is not  already in use.

Either by  

    )lisp (ql:system-apropos "packageName")
    
 or  
      
    quickLoad "quicklsp"
    qlApropos "packageName".
	  
Also notice the name ``quicklsp``, because `quicklisp` has already been taken,
of course. By the way, `qlApropos` will also search the QuickLisp online
repository.
  
After a package has been loaded with ``quickLoad"``, the following functions are
available in the **Lisp** ``BOOT`` package, i.e. in `FriCAS`:

    compile_packageName()$Lisp
    load_packageName()$Lisp 
    test_packageName()$Lisp
 
where ``packageName`` is the *name* of the package (case insensitive), for 
instance "compile_pipe()$Lisp" will recompile the package `pipe`. 
 
These functions may be used to recompile/reload the package if necessary
(e.g after FriCAS upgrades or package updates).   

---


[^1]: Instead one may also symlink the folder or
    `(push "~/your/local/package-folder/" ql:*local-project-directories*)`.
    However, the latter holds only once, unless added to the rc-file, 
    e.g. `~/.sbclrc`.


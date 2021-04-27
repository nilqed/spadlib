# SPADLIB V0.2


## Required

>  QuickLisp https://www.quicklisp.org/beta/
>
>  FriCAS    https://fricas.github.io/ 
>  
>  Optional: jfricas (Jupyter) https://pypi.org/project/jfricas/
  
## Install  

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
      lisp2("defvar |$inclAssertions|","nil")  
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
  
    () -> quickLoad "packageName"
  
#### Note 
If `quickLoad` is unknown after `FriCAS` start, you have to enter

        )frame next 

under certain circumstances (depends on start parameters).
        
**BTW** nothing will be changed to your `FriCAS` installation,
i.e. the packages are available at runtime only.    
  
## Create a package

    $ fripac.sh "packageName" 
  
type 

    fripac.sh -h 
    
for more options.


---

### NOTES 

Before creating a new package, check if "packageName" is not  already in use:
  Either by  )lisp (ql:system-apropos "packageName") or
  by  
      quickLoad "quicklsp"
	  qlApropos "packageName".
	  
  Also notice the name "quicklsp", because "quicklisp" is already taken,
  of course.
  
After a package has been loaded by "quickLoad", the following functions are
available in the Lisp "BOOT" package, i.e. in FriCAS:

  compile_packageName()$Lisp
  load_packageName()$Lisp 
  test_packageName()$Lisp
 
where "packageName" is the name of the package (case insensitive), for 
instance "compile_pipe()$Lisp" will recompile the package "pipe". 
 
These functions may be used to recompile/reload the package if necessary
(e.g after FriCAS upgrades or package updates).   

---

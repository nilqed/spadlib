<TeXmacs|2.1.4>

<style|generic>

<\body>
  <\session|fricas|default>
    <\output>
      Checking for foreign routines

      FRICAS="/usr/local/lib/fricas/target/x86_64-linux-gnu"

      spad-lib="/usr/local/lib/fricas/target/x86_64-linux-gnu//lib/libspad.so"

      foreign routines found

      openServer result -2

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ FriCAS Computer Algebra
      System\ 

      \ \ \ \ \ \ \ \ \ \ \ Version: FriCAS 2024-04-15 built with sbcl
      2.2.9.debian

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Timestamp: Di 28 Mai 2024
      21:49:04 CEST

      -----------------------------------------------------------------------------

      \ \ \ Issue )copyright to view copyright notices.

      \ \ \ Issue )summary for a summary of useful system commands.

      \ \ \ Issue )quit to leave FriCAS and return to shell.

      -----------------------------------------------------------------------------

      \ 

      \ \ \ Function declaration sixel : TexFormat -\<gtr\> Void has been
      added to\ 

      \ \ \ \ \ \ workspace.

      Value = #\<less\>INTERPRETED-FUNCTION NIL {10020A026B}\<gtr\>
    </output>

    <\unfolded-io>
      (6) -\<gtr\>\ 
    <|unfolded-io>
      quickLoad tmspt
    <|unfolded-io>
      \;

      Value = \|$inclAssertions\|

      Value = T

      To load "tmspt":

      \ \ Load 1 ASDF system:

      \ \ \ \ tmspt

      ; Loading "tmspt"

      \ \ \ The current FriCAS default directory is\ 

      \ \ \ \ \ \ /home/kfp/quicklisp/local-projects/spadlib/tmspt/lib\ 

      \ \ \ Compiling FriCAS source code from file\ 

      \ \ \ \ \ \ /home/kfp/quicklisp/local-projects/spadlib/tmspt/lib/../src/tmspt.spad

      \ \ \ \ \ \ using old system compiler.

      \ \ \ TMSPT abbreviates package TexmacsSupport\ 
    </unfolded-io>

    <\unfolded-io>
      (6) -\<gtr\>\ 
    <|unfolded-io>
      a/b \ -- normal mode
    <|unfolded-io>
      \;

      <with|mode|math|<frac|a|b>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Fraction(Polynomial(Integer))
    </unfolded-io>

    <\unfolded-io-math>
      (8) -\<gtr\>\ 
    <|unfolded-io-math>
      <frac|a|b>
    <|unfolded-io-math>
      \;

      <with|mode|math|<frac|a|b>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Fraction(Polynomial(Integer))
    </unfolded-io-math>

    <\unfolded-io-math>
      (10) -\<gtr\>\ 
    <|unfolded-io-math>
      tmMathOn<around*|(||)>
    <|unfolded-io-math>
      \ \ \ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Void
    </unfolded-io-math>

    <\unfolded-io-math>
      (12) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <frac|a|b>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? (a/b)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (14) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <matrix|<tformat|<table|<row|<cell|q >>|<row|<cell|a>>>>>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? matrix([[q ], [a]])

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (16) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <matrix|<tformat|<table|<row|<cell|1,2>>|<row|<cell|3,4>>>>>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? matrix([[1,2], [3,4]])

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (19) -\<gtr\>\ 
    <|unfolded-io-math>
      \ a\<assign\>b/z
    <|unfolded-io-math>
      \;

      <with|mode|math|<frac|b|z>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Fraction(Polynomial(Integer))
    </unfolded-io-math>

    <\unfolded-io-math>
      (18) -\<gtr\>\ 
    <|unfolded-io-math>
      tmMathOff<around*|(||)>
    <|unfolded-io-math>
      \ \ \ 

      \;

      <with|mode|math|<text|"<math|()>">>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Void
    </unfolded-io-math>

    <\unfolded-io-math>
      (20) -\<gtr\>\ 
    <|unfolded-io-math>
      tmMathOn<around*|(||)>
    <|unfolded-io-math>
      \ \ \ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Void
    </unfolded-io-math>

    <\unfolded-io-math>
      (21) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <big|int><rsub|a><rsup|b>f<around*|(|x,y|)> d x
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? integrate(f(x,y) d x=a..b)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (26) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <big|int><rsub|a><rsup|b>f<around*|(|x,y|)> dx
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? integrate(f(x,y) dx=a..b)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (27) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <frac|d f<around*|(|x|)>|d x>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? ((d f(x))/(d x))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (28) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <frac|\<partial\> f<around*|(|x,y|)>|\<partial\> y>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? ((partial f(x,y))/(partial y))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (30) -\<gtr\>\ 
    <|unfolded-io-math>
      ? binomial<around*|(|n,k|)>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ? binomial(n,k)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (31) -\<gtr\>\ 
    <|unfolded-io-math>
      ? <binom|n|k>
    <|unfolded-io-math>
      \;

      \ LISP output:

      ?\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\unfolded-io-math>
      (32) -\<gtr\>\ 
    <|unfolded-io-math>
      ?!n
    <|unfolded-io-math>
      \;

      \ LISP output:

      ?!n

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Any
    </unfolded-io-math>

    <\input-math>
      (33) -\<gtr\>\ 
    <|input-math>
      \;
    </input-math>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>
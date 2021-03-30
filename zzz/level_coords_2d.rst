Level set coordinates for quasi-convex functions in :math:`\mathbb{R}^2`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: spad

    )version


.. parsed-literal::

    Value = "FriCAS 2020-04-23 compiled at Do Apr 30 00:15:41 CEST 2020"


.. code:: spad

    R ==> Expression Integer




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Void.html" target="_blank" style="color:blue;text-decoration:none;">Void</a></sub></div>



``simpeq`` simplifies the right hand side of an equation.

.. code:: spad

    simpeq(x:Equation R):Equation R == lhs x = simplify rhs x 


.. parsed-literal::

    Function declaration simpeq : Equation(Expression(Integer)) -> Equation(
    Expression(Integer)) has been added to workspace.




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Void.html" target="_blank" style="color:blue;text-decoration:none;">Void</a></sub></div>



Define basic operators for the functions :math:`f(x,y)` and
:math:`h(t,p)`

.. code:: spad

    [f,h]:=map(operator,['f,'h])




.. parsed-literal::

    [f, h]





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/BasicOperator.html" target="_blank" style="color:blue;text-decoration:none;">BasicOperator</a>)</sub></div>


.. code:: spad

    )set output tex on
    )set output algebra off

Equations for the support function :math:`h(t,p)` and its derivative
w.r.t :math:`p`

.. code:: spad

    eq1 := h(t,p) = x * cos(p) + y * sin(p)
    eq2 := D(eq1,p)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {h 
    \left(
    {t, \: p} 
    \right)}={{y
    \  {\sin 
    \left(
    {p} 
    \right)}}+{x
    \  {\cos 
    \left(
    {p} 
    \right)}}}
    \leqno(5)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}={-{x
    \  {\sin 
    \left(
    {p} 
    \right)}}+{y
    \  {\cos 
    \left(
    {p} 
    \right)}}}
    \leqno(6)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


Solve for :math:`x` and :math:`y`.

.. code:: spad

    Sxy := solve([eq1,eq2],[x,y]).1
    eqx := lhs Sxy.1 = simplify rhs Sxy.1
    eqy := lhs Sxy.2 = simplify rhs Sxy.2




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    \left[
    {x={{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}}
    \over {{{{\sin 
    \left(
    {p} 
    \right)}}
    \sp {2}}+{{{\cos 
    \left(
    {p} 
    \right)}}
    \sp {2}}}}}, \: {y={{{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}
    \over {{{{\sin 
    \left(
    {p} 
    \right)}}
    \sp {2}}+{{{\cos 
    \left(
    {p} 
    \right)}}
    \sp {2}}}}} 
    \right]
    \leqno(7)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    x={-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}}
    \leqno(8)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    y={{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}
    \leqno(9)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


Jacobian :math:`[D x,D y]`

.. code:: spad

    Jxy := matrix [[D(rhs eqx,t), D(rhs eqx,p)],[D(rhs eqy,t), D(rhs eqy,p)]]
    detJxy := simplify determinant Jxy




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    \left[
    \begin{array}{cc}
    {-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{\cos
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}}
    & {-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}
    -{{h 
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}
    \\ 
    {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{\sin
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}}
    & {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}}
    \end{array}
    \right]
    \leqno(10)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Matrix.html" target="_blank" style="color:blue;text-decoration:none;">Matrix</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \leqno(11)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


Equation for level curve: :math:`f(x,y)=t`

.. code:: spad

    eqf := eval(f(x,y),[eqx,eqy]) = t




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {f 
    \left(
    {{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}},
    \: {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}}
    \right)}=t
    \leqno(12)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


Differentiate the equation aboce w.r.t. :math:`t` and :math:`p`.

.. code:: spad

    eqft := D(eqf,t)
    eqfp := D(eqf,p)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{\left( -{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{\cos
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \right)}
    \  {{f \sb {{,1}}} 
    \left(
    {{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}},
    \: {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}}
    \right)}}+{{\left(
    {{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{\sin
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \right)}
    \  {{f \sb {{,2}}} 
    \left(
    {{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}},
    \: {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}}
    \right)}}}=1
    \leqno(13)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{\left( -{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}
    -{{h 
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}
    \right)}
    \  {{f \sb {{,1}}} 
    \left(
    {{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}},
    \: {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}}
    \right)}}+{{\left(
    {{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}
    \right)}
    \  {{f \sb {{,2}}} 
    \left(
    {{-{{\sin 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}},
    \: {{{\cos 
    \left(
    {p} 
    \right)}
    \  {{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}}}
    \right)}}}=0
    \leqno(14)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


.. code:: spad

    tmp1:=[eval(D(f(x,y),x),[eqx,eqy])=ft, eval(D(f(x,y),y),[eqx,eqy])=fp];
    eq3:=eval(eqft, tmp1)
    eq4:=eval(eqfp, tmp1)




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>





.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{\left( -{ft \  {\sin 
    \left(
    {p} 
    \right)}}+{fp
    \  {\cos 
    \left(
    {p} 
    \right)}}
    \right)}
    \  {{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{\left(
    {fp \  {\sin 
    \left(
    {p} 
    \right)}}+{ft
    \  {\cos 
    \left(
    {p} 
    \right)}}
    \right)}
    \  {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}}=1
    \leqno(54)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{\left( -{ft \  {\sin 
    \left(
    {p} 
    \right)}}+{fp
    \  {\cos 
    \left(
    {p} 
    \right)}}
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}
    -{ft \  {h 
    \left(
    {t, \: p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}+{fp
    \  {h 
    \left(
    {t, \: p} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}}}=0
    \leqno(55)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


Solve for :math:`f_t` amd :math:`f_p`.

.. code:: spad

    S1:=map(simpeq, solve([eq3,eq4],[ft,fp]).1)


.. parsed-literal::

    Compiling function simpeq with type Equation(Expression(Integer)) -> Equation
    (Expression(Integer)) 




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    \left[
    {ft={{\cos 
    \left(
    {p} 
    \right)}
    \over {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}},
    \: {fp={{\sin 
    \left(
    {p} 
    \right)}
    \over {{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}}
    \right]
    \leqno(18)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>


Now same for second derivatives.

.. code:: spad

    eqftt := D(eqf,t,2);
    eqfpp := D(eqf,p,2);
    eqftp := D(eqft,p);




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>



.. code:: spad

    tmp2:=[eval(D(f(x,y),x,2),[eqx,eqy])=ftt, eval(D(f(x,y),y,2),[eqx,eqy])=fpp, _
           eval(D(f(x,y),[x,y]),[eqx,eqy])=ftp];
    tmp3:=concat(tmp1,tmp2);
    eq5 := eval(eval(eqftt, tmp3), S1);
    eq6 := eval(eval(eqfpp, tmp3), S1);
    eq7 := eval(eval(eqftp, tmp3), S1);




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>



Solve for :math:`f_{tt}, f_{pp}` and :math:`f_{tp}`.

.. code:: spad

    S2:=map(simpeq, solve([eq5,eq6,eq7],[ftt,fpp,ftp]).1);




.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>



.. code:: spad

    simpeq(S2.1+S2.2)
    simpeq(S2.1*S2.2-S2.3^2)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {ftt+fpp}={{-{{{h \sb {{{,1}{,1}}}} 
    \left(
    {t, \: p} 
    \right)}
    \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}
    -{{h 
    \left(
    {t, \: p} 
    \right)}
    \  {{h \sb {{{,1}{,1}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{{{h
    \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {2}}+{{{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {2}}} \over {{{{{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {3}} \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {{{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {3}}}}} 
    \leqno(28)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{fpp \  ftt} -{{ftp} \sp {2}}}=-{{{h \sb {{{,1}{,1}}}} 
    \left(
    {t, \: p} 
    \right)}
    \over {{{{{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {4}} \  {{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}}+{{h
    \left(
    {t, \: p} 
    \right)}
    \  {{{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}}
    \sp {4}}}}} 
    \leqno(29)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


For better readability define a substitution list. We abbreviate
:math:`h(t,p)` as :math:`H` and so on.

.. code:: spad

    SL:=[h(t,p)=H, D(h(t,p),t)=H[t], D(h(t,p),p)=H[p], D(h(t,p),t,2)=H[tt],_
          D(h(t,p),p,2)=H[pp], D(h(t,p),[t,p])=H[tp]]




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    \left[
    {{h 
    \left(
    {t, \: p} 
    \right)}=H},
    \: {{{h \sb {{,1}}} 
    \left(
    {t, \: p} 
    \right)}={H
    \sb {t}}}, \: {{{h \sb {{,2}}} 
    \left(
    {t, \: p} 
    \right)}={H
    \sb {p}}}, \: {{{h \sb {{{,1}{,1}}}} 
    \left(
    {t, \: p} 
    \right)}={H
    \sb {tt}}}, \: {{{h \sb {{{,2}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}={H
    \sb {pp}}}, \: {{{h \sb {{{,1}{,2}}}} 
    \left(
    {t, \: p} 
    \right)}={H
    \sb {tp}}} 
    \right]
    \leqno(30)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)))</sub></div>


We now use the letter :math:`u` for the function :math:`f`, i.e.
:math:`u(x,y)=f(x,y)`:

.. code:: spad

    equx := u[x] = subst(rhs S1.1,SL)
    equy := u[y] = subst(rhs S1.2,SL)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {u \sb {x}}={{\cos 
    \left(
    {p} 
    \right)}
    \over {H \sb {t}}} 
    \leqno(31)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {u \sb {y}}={{\sin 
    \left(
    {p} 
    \right)}
    \over {H \sb {t}}} 
    \leqno(32)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


.. code:: spad

    equxx := u[xx] = subst(rhs S2.1,SL)
    equyy := u[yy] = subst(rhs S2.2,SL)
    equxy := u[xy] = subst(rhs S2.3,SL)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {u \sb {xx}}={{{2 \  {H \sb {t}} \  {H \sb {tp}} \  {\cos 
    \left(
    {p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}+{{\left(
    -{{H \sb {tt}} \  H} -{{H \sb {pp}} \  {H \sb {tt}}}+{{{H \sb {tp}}} \sp {2}} 
    -{{{H \sb {t}}} \sp {2}} 
    \right)}
    \  {{{\cos 
    \left(
    {p} 
    \right)}}
    \sp {2}}}+{{{H \sb {t}}} \sp {2}}} \over {{{{{H \sb {t}}} \sp {3}} \  H}+{{H 
    \sb {pp}} \  {{{H \sb {t}}} \sp {3}}}}} 
    \leqno(33)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {u \sb {yy}}={{-{2 \  {H \sb {t}} \  {H \sb {tp}} \  {\cos 
    \left(
    {p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}+{{\left(
    {{H \sb {tt}} \  H}+{{H \sb {pp}} \  {H \sb {tt}}} -{{{H \sb {tp}}} \sp 
    {2}}+{{{H \sb {t}}} \sp {2}} 
    \right)}
    \  {{{\cos 
    \left(
    {p} 
    \right)}}
    \sp {2}}} -{{H \sb {tt}} \  H} -{{H \sb {pp}} \  {H \sb {tt}}}+{{{H \sb 
    {tp}}} \sp {2}}} \over {{{{{H \sb {t}}} \sp {3}} \  H}+{{H \sb {pp}} \  {{{H 
    \sb {t}}} \sp {3}}}}} 
    \leqno(34)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {u \sb {xy}}={{{{\left( -{{H \sb {tt}} \  H} -{{H \sb {pp}} \  {H \sb 
    {tt}}}+{{{H \sb {tp}}} \sp {2}} -{{{H \sb {t}}} \sp {2}} 
    \right)}
    \  {\cos 
    \left(
    {p} 
    \right)}
    \  {\sin 
    \left(
    {p} 
    \right)}}
    -{2 \  {H \sb {t}} \  {H \sb {tp}} \  {{{\cos 
    \left(
    {p} 
    \right)}}
    \sp {2}}}+{{H \sb {t}} \  {H \sb {tp}}}} \over {{{{{H \sb {t}}} \sp {3}} \  
    H}+{{H \sb {pp}} \  {{{H \sb {t}}} \sp {3}}}}} 
    \leqno(35)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


.. code:: spad

     eqdet:=simpeq(equxx*equyy-equxy^2)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{{u \sb {xx}} \  {u \sb {yy}}} -{{{u \sb {xy}}} \sp {2}}}=-{{H \sb {tt}} 
    \over {{{{{H \sb {t}}} \sp {4}} \  H}+{{H \sb {pp}} \  {{{H \sb {t}}} \sp 
    {4}}}}} 
    \leqno(36)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>


.. code:: spad

    eqlap:=simpeq(equxx+equyy)




.. math::

    \def\sp{^}\def\sb{_}\def\leqno(#1){}\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}
    {{u \sb {yy}}+{u \sb {xx}}}={{-{{H \sb {tt}} \  H} -{{H \sb {pp}} \  {H \sb 
    {tt}}}+{{{H \sb {tp}}} \sp {2}}+{{{H \sb {t}}} \sp {2}}} \over {{{{{H \sb 
    {t}}} \sp {3}} \  H}+{{H \sb {pp}} \  {{{H \sb {t}}} \sp {3}}}}} 
    \leqno(37)
    $$





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Equation.html" target="_blank" style="color:blue;text-decoration:none;">Equation</a>(<a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>))</sub></div>



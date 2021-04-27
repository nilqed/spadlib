.. code:: spad

    )version


.. parsed-literal::

    Value = "FriCAS 2021-03-06 compiled at Mi Mär 31 16:01:42 CEST 2021"


.. code:: spad

    )cd /tmp


.. parsed-literal::

    The current FriCAS default directory is /tmp 


.. code:: spad

    )! ls /tmp/FMTMJAX*


.. parsed-literal::

    FMTMJAX.fasl
    FMTMJAX.lsp
    index.KAF


.. code:: spad

    )lib FMTMJAX


.. parsed-literal::

     FormatMathJax is now explicitly exposed in frame initial 
    FormatMathJax will be automatically loaded when needed from 
    /tmp/FMTMJAX.NRLIB/FMTMJAX


.. code:: spad

    )set message type off
    )set output algebra off
    setFormat!(FormatMathJax)$JFriCASSupport
    )set message type on

.. code:: spad

    a%




.. math::

    \[                        
    \alpha 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Variable.html" target="_blank" style="color:blue;text-decoration:none;">Variable</a>(a%)</sub></div>


.. code:: spad

    [a%,b%,g%,d%,e%,z%,ee%,th%,i%,k%,l%,m%,n%,x%,o%,p%,r%,s%,t%,y%,ph%,ch%,_
     ps%,oo%,G%,D%,Th%,L%,X%,P%,S%,Ph%,Ps%,OO%,hbar%]




.. math::

    \[                        
    \left[\alpha , \beta , \gamma , \delta , \epsilon , \zeta , \eta , \theta , \iota , \kappa , \lambda , \mu , \nu , \xi , \omicron , \pi , \rho , \sigma , \tau , \upsilon , \phi , \chi , \psi , \omega , \Gamma , \Delta , \Theta , \Lambda , \Xi , \Pi , \Sigma , \Phi , \Psi , \Omega , \hbar \right]
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/OrderedVariableList.html" target="_blank" style="color:blue;text-decoration:none;">OrderedVariableList</a>([a%,b%,g%,d%,e%,z%,ee%,th%,i%,k%,l%,m%,n%,x%,o%,p%,r%,s%,t%,y%,ph%,ch%,ps%,oo%,<a href="https://fricas.github.io/api/G.html" target="_blank" style="color:blue;text-decoration:none;">G</a>%,<a href="https://fricas.github.io/api/D.html" target="_blank" style="color:blue;text-decoration:none;">D</a>%,<a href="https://fricas.github.io/api/Th.html" target="_blank" style="color:blue;text-decoration:none;">Th</a>%,<a href="https://fricas.github.io/api/L.html" target="_blank" style="color:blue;text-decoration:none;">L</a>%,<a href="https://fricas.github.io/api/X.html" target="_blank" style="color:blue;text-decoration:none;">X</a>%,<a href="https://fricas.github.io/api/P.html" target="_blank" style="color:blue;text-decoration:none;">P</a>%,<a href="https://fricas.github.io/api/S.html" target="_blank" style="color:blue;text-decoration:none;">S</a>%,<a href="https://fricas.github.io/api/Ph.html" target="_blank" style="color:blue;text-decoration:none;">Ph</a>%,<a href="https://fricas.github.io/api/Ps.html" target="_blank" style="color:blue;text-decoration:none;">Ps</a>%,<a href="https://fricas.github.io/api/OO.html" target="_blank" style="color:blue;text-decoration:none;">OO</a>%,hbar%]))</sub></div>


.. code:: spad

    alpha%




.. math::

    \[                        
    \alpha 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Variable.html" target="_blank" style="color:blue;text-decoration:none;">Variable</a>(alpha%)</sub></div>


.. code:: spad

    [alpha%, beta%, chi%, delta%, epsilon%, eta%, gamma%, iota%, kappa%, lambda%, mu%, nu%, omega%, phi%,_ 
     pi%, psi%, rho%, sigma%, tau%, theta%, upsilon%, xi%, zeta%, digamma%, varepsilon%, varkappa%, varphi%,_ 
     varpi%, varrho%, varsigma%, vartheta%, Delta%, Gamma%, Lambda%, Omega%, Phi%, Pi%, Psi%, Sigma%, Theta%,_ 
     Upsilon%, Xi%, aleph%, beth%, daleth%, gimel%, pm%, mp%, times%, divide%, cdot%, ast%, star%, dagger%,_ 
     ddagger%, amalg%, cap%, cup%, uplus%, sqcap%, sqcup%, vee%, wedge%, oplus%, ominus%, otimes%, circ%,_ 
     bullet%, diamond%, lhd%, rhd%, unlhd%, unrhd%, oslash%, odot%, bigcirc%, triangleleft%, Diamond%,_ 
     bigtriangleup%, bigtriangledown%, Box%, triangleright%, setminus%, wr%, le%, ge%, neq%, sim%, ll%,_ 
     gg%, doteq%, simeq%, subset%, supset%, approx%, asymp%, subseteq%, supseteq%, cong%, smile%, sqsubset%,_ 
     sqsupset%, equiv%, frown%, sqsubseteq%, sqsupseteq%, propto%, in%, ni%, prec%, succ%, vdash%, dashv%,_ 
     preceq%, succeq%, models%, perp%, parallel%, mid%, nmid%, nleq%, ngeq%, nsim%, ncong%, nparallel%,_ 
     nless%, ngtr%, lneq%, gneq%, lnsim%, lneqq%, gneqq%, gets%, to%, leftarrow%, Leftarrow%, rightarrow%,_ 
     Rightarrow%, leftrightarrow%, Leftrightarrow%, mapsto%, hookleftarrow%, leftharpoonup%, leftharpoondown%,_ 
     rightleftharpoons%, longleftarrow%, Longleftarrow%, longrightarrow%, Longrightarrow%, longleftrightarrow%,_ 
     Longleftrightarrow%, longmapsto%, hookrightarrow%, rightharpoonup%, rightharpoondown%, leadsto%,_ 
     uparrow%, Uparrow%, downarrow%, Downarrow%, updownarrow%, Updownarrow%, nearrow%, searrow%, swarrow%,_ 
     nwarrow%, ldots%, vdots%, cdots%, ddots%, infty%, triangle%, angle%, hbar%, imath%, jmath%, ell%, wp%,_ 
     Re%, Im%, mho%, prime%, emptyset%, nabla%, partial%, top%, bot%, forall%, exists%, neg%, flat%, natural%,_ 
     sharp%, backslash%, clubsuit%, diamondsuit%, heartsuit%, spadesuit%, blacksquare%, pounds%, sum%, int%,_ 
     oint%, prod%, coprod%, bigcap%, bigcup%, bigsqcup%, bigvee%, bigwedge%, bigodot%, bigotimes%, bigoplus%]




.. math::

    \[                        
    \left[\alpha , \beta , \chi , \delta , \epsilon , \eta , \gamma , \iota , \kappa , \lambda , \mu , \nu , \omega , \phi , \pi , \psi , \rho , \sigma , \tau , \theta , \upsilon , \xi , \zeta , \digamma , \varepsilon , \varkappa , \varphi , \varpi , \varrho , \varsigma , \vartheta , \Delta , \Gamma , \Lambda , \Omega , \Phi , \Pi , \Psi , \Sigma , \Theta , \Upsilon , \Xi , \aleph , \beth , \daleth , \gimel , \pm , \mp , \times , \div , \cdot , \ast , \star , \dagger , \ddagger , \amalg , \cap , \cup , \uplus , \sqcap , \sqcup , \vee , \wedge , \oplus , \ominus , \otimes , \circ , \bullet , \diamond , \lhd , \rhd , \unlhd , \unrhd , \oslash , \odot , \bigcirc , \triangleleft , \Diamond , \bigtriangleup , \bigtriangledown , \Box , \triangleright , \setminus , \wr , \le , \ge , \neq , \sim , \ll , \gg , \doteq , \simeq , \subset , \supset , \approx , \asymp , \subseteq , \supseteq , \cong , \smile , \sqsubset , \sqsupset , \equiv , \frown , \sqsubseteq , \sqsupseteq , \propto , \in , \ni , \prec , \succ , \vdash , \dashv , \preceq , \succeq , \models , \perp , \parallel , \mid , \nmid , \nleq , \ngeq , \nsim , \ncong , \nparallel , \nless , \ngtr , \lneq , \gneq , \lnsim , \lneqq , \gneqq , \gets , \to , \leftarrow , \Leftarrow , \rightarrow , \Rightarrow , \leftrightarrow , \Leftrightarrow , \mapsto , \hookleftarrow , \leftharpoonup , \leftharpoondown , \rightleftharpoons , \longleftarrow , \Longleftarrow , \longrightarrow , \Longrightarrow , \longleftrightarrow , \Longleftrightarrow , \longmapsto , \hookrightarrow , \rightharpoonup , \rightharpoondown , \leadsto , \uparrow , \Uparrow , \downarrow , \Downarrow , \updownarrow , \Updownarrow , \nearrow , \searrow , \swarrow , \nwarrow , \ldots , \vdots , \cdots  , \ddots , \infty , \triangle , \angle , \hbar , \imath , \jmath , \ell , \wp , \Re , \Im , \mho , \prime , \emptyset , \nabla , \partial , \top , \bot , \forall , \exists , \neg , \flat , \natural , \sharp , \backslash , \clubsuit , \diamondsuit , \heartsuit , \spadesuit , \blacksquare , pounds\%, \sum , \int , \oint , \prod , \coprod , \bigcap , \bigcup , \bigsqcup , \bigvee , \bigwedge , \bigodot , \bigotimes , \bigoplus \right]
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/List.html" target="_blank" style="color:blue;text-decoration:none;">List</a>(<a href="https://fricas.github.io/api/OrderedVariableList.html" target="_blank" style="color:blue;text-decoration:none;">OrderedVariableList</a>([alpha%,beta%,chi%,delta%,epsilon%,eta%,gamma%,iota%,kappa%,lambda%,mu%,nu%,omega%,phi%,pi%,psi%,rho%,sigma%,tau%,theta%,upsilon%,xi%,zeta%,digamma%,varepsilon%,varkappa%,varphi%,varpi%,varrho%,varsigma%,vartheta%,<a href="https://fricas.github.io/api/Delta.html" target="_blank" style="color:blue;text-decoration:none;">Delta</a>%,<a href="https://fricas.github.io/api/Gamma.html" target="_blank" style="color:blue;text-decoration:none;">Gamma</a>%,<a href="https://fricas.github.io/api/Lambda.html" target="_blank" style="color:blue;text-decoration:none;">Lambda</a>%,<a href="https://fricas.github.io/api/Omega.html" target="_blank" style="color:blue;text-decoration:none;">Omega</a>%,<a href="https://fricas.github.io/api/Phi.html" target="_blank" style="color:blue;text-decoration:none;">Phi</a>%,<a href="https://fricas.github.io/api/Pi.html" target="_blank" style="color:blue;text-decoration:none;">Pi</a>%,<a href="https://fricas.github.io/api/Psi.html" target="_blank" style="color:blue;text-decoration:none;">Psi</a>%,<a href="https://fricas.github.io/api/Sigma.html" target="_blank" style="color:blue;text-decoration:none;">Sigma</a>%,<a href="https://fricas.github.io/api/Theta.html" target="_blank" style="color:blue;text-decoration:none;">Theta</a>%,<a href="https://fricas.github.io/api/Upsilon.html" target="_blank" style="color:blue;text-decoration:none;">Upsilon</a>%,<a href="https://fricas.github.io/api/Xi.html" target="_blank" style="color:blue;text-decoration:none;">Xi</a>%,aleph%,beth%,daleth%,gimel%,pm%,mp%,times%,divide%,cdot%,ast%,star%,dagger%,ddagger%,amalg%,cap%,cup%,uplus%,sqcap%,sqcup%,vee%,wedge%,oplus%,ominus%,otimes%,circ%,bullet%,diamond%,lhd%,rhd%,unlhd%,unrhd%,oslash%,odot%,bigcirc%,triangleleft%,<a href="https://fricas.github.io/api/Diamond.html" target="_blank" style="color:blue;text-decoration:none;">Diamond</a>%,bigtriangleup%,bigtriangledown%,<a href="https://fricas.github.io/api/Box.html" target="_blank" style="color:blue;text-decoration:none;">Box</a>%,triangleright%,setminus%,wr%,le%,ge%,neq%,sim%,ll%,gg%,doteq%,simeq%,subset%,supset%,approx%,asymp%,subseteq%,supseteq%,cong%,smile%,sqsubset%,sqsupset%,equiv%,frown%,sqsubseteq%,sqsupseteq%,propto%,in%,ni%,prec%,succ%,vdash%,dashv%,preceq%,succeq%,models%,perp%,parallel%,mid%,nmid%,nleq%,ngeq%,nsim%,ncong%,nparallel%,nless%,ngtr%,lneq%,gneq%,lnsim%,lneqq%,gneqq%,gets%,to%,leftarrow%,<a href="https://fricas.github.io/api/Leftarrow.html" target="_blank" style="color:blue;text-decoration:none;">Leftarrow</a>%,rightarrow%,<a href="https://fricas.github.io/api/Rightarrow.html" target="_blank" style="color:blue;text-decoration:none;">Rightarrow</a>%,leftrightarrow%,<a href="https://fricas.github.io/api/Leftrightarrow.html" target="_blank" style="color:blue;text-decoration:none;">Leftrightarrow</a>%,mapsto%,hookleftarrow%,leftharpoonup%,leftharpoondown%,rightleftharpoons%,longleftarrow%,<a href="https://fricas.github.io/api/Longleftarrow.html" target="_blank" style="color:blue;text-decoration:none;">Longleftarrow</a>%,longrightarrow%,<a href="https://fricas.github.io/api/Longrightarrow.html" target="_blank" style="color:blue;text-decoration:none;">Longrightarrow</a>%,longleftrightarrow%,<a href="https://fricas.github.io/api/Longleftrightarrow.html" target="_blank" style="color:blue;text-decoration:none;">Longleftrightarrow</a>%,longmapsto%,hookrightarrow%,rightharpoonup%,rightharpoondown%,leadsto%,uparrow%,<a href="https://fricas.github.io/api/Uparrow.html" target="_blank" style="color:blue;text-decoration:none;">Uparrow</a>%,downarrow%,<a href="https://fricas.github.io/api/Downarrow.html" target="_blank" style="color:blue;text-decoration:none;">Downarrow</a>%,updownarrow%,<a href="https://fricas.github.io/api/Updownarrow.html" target="_blank" style="color:blue;text-decoration:none;">Updownarrow</a>%,nearrow%,searrow%,swarrow%,nwarrow%,ldots%,vdots%,cdots%,ddots%,infty%,triangle%,angle%,hbar%,imath%,jmath%,ell%,wp%,<a href="https://fricas.github.io/api/Re.html" target="_blank" style="color:blue;text-decoration:none;">Re</a>%,<a href="https://fricas.github.io/api/Im.html" target="_blank" style="color:blue;text-decoration:none;">Im</a>%,mho%,prime%,emptyset%,nabla%,partial%,top%,bot%,forall%,exists%,neg%,flat%,natural%,sharp%,backslash%,clubsuit%,diamondsuit%,heartsuit%,spadesuit%,blacksquare%,pounds%,sum%,int%,oint%,prod%,coprod%,bigcap%,bigcup%,bigsqcup%,bigvee%,bigwedge%,bigodot%,bigotimes%,bigoplus%]))</sub></div>


.. code:: spad

    bigcup%




.. math::

    \[                        
    \bigcup 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Variable.html" target="_blank" style="color:blue;text-decoration:none;">Variable</a>(bigcup%)</sub></div>


.. code:: spad

    f:=operator 'f




.. math::

    \[                        
    f
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/BasicOperator.html" target="_blank" style="color:blue;text-decoration:none;">BasicOperator</a></sub></div>


.. code:: spad

    display(f,o+->message "\alpha")




.. math::

    \[                        
    f
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/BasicOperator.html" target="_blank" style="color:blue;text-decoration:none;">BasicOperator</a></sub></div>


.. code:: spad

    f(x)




.. math::

    \[                        
    \texttt{\\alpha}
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


.. code:: spad

    )lisp (princ "$\alpha$")


.. parsed-literal::

    $alpha$
    Value = "$alpha$"


.. code:: spad

    a%+b%




.. math::

    \[                        
    \beta +\alpha 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Polynomial.html" target="_blank" style="color:blue;text-decoration:none;">Polynomial</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


.. code:: spad

    u:=phi%; v:=psi%




.. math::

    \[                        
    \psi 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Variable.html" target="_blank" style="color:blue;text-decoration:none;">Variable</a>(psi%)</sub></div>


.. code:: spad

    u^2+v




.. math::

    \[                        
    \psi +{\phi }^{2}
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Polynomial.html" target="_blank" style="color:blue;text-decoration:none;">Polynomial</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


.. code:: spad

    a%^b%




.. math::

    \[                        
    {\alpha }^{\beta }
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


.. code:: spad

    rightarrow%




.. math::

    \[                        
    \rightarrow 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Variable.html" target="_blank" style="color:blue;text-decoration:none;">Variable</a>(rightarrow%)</sub></div>


.. code:: spad

    g := operator 'g%




.. math::

    \[                        
    \gamma 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/BasicOperator.html" target="_blank" style="color:blue;text-decoration:none;">BasicOperator</a></sub></div>


.. code:: spad

    g(x,y)




.. math::

    \[                        
    \operatorname{g\%}\left(x, y\right)
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>


.. code:: spad

    display(g,o+->g%)




.. math::

    \[                        
    \gamma 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/BasicOperator.html" target="_blank" style="color:blue;text-decoration:none;">BasicOperator</a></sub></div>


.. code:: spad

    g(x)




.. math::

    \[                        
    \gamma 
    \]                        





.. raw:: html

    <div style="text-align:right;"><sub><a href="https://fricas.github.io/api/Expression.html" target="_blank" style="color:blue;text-decoration:none;">Expression</a>(<a href="https://fricas.github.io/api/Integer.html" target="_blank" style="color:blue;text-decoration:none;">Integer</a>)</sub></div>



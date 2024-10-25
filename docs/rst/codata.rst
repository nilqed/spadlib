=====================================================
CODATA - values of the fundamental physical constants
=====================================================

Reference: `CODATA@NIST <https://www.nist.gov/programs-projects/codata-values-fundamental-physical-constants>`_


Summary
-------
Every four years, the Committee on Data for Science and Technology 
(`CODATA <https://codata.org/initiatives/strategic-programme/fundamental-physical-constants/>`_) 
issues recommended values of the fundamental physical constants. The values 
are determined by a least-squares adjustment, based on all the available 
theoretical and experimental information. The selection and assessment of 
data is done under the auspices of the CODATA Task Group on Fundamental 
Constants.

Loading
-------
:Requires: ``PHYSQTY`` (automatic when using quickLoad).

Using ``quickLoad`` ::

    > quickLoad "codata"
       Codata will be automatically loaded when needed from 
          /Users/kfp/quicklisp/local-projects/spadlib/codata/lib/CODATA.NRLIB/CODATA
    
    Value = ("codata")
                                                                    Type: Void
    > )sh CODATA
     Codata is a package constructor
     Abbreviation for Codata is CODATA 
     This constructor is exposed in this frame.
    ----------------------------------------------------- Operations -------
     F : () -> PhysQty(SIunit)                                   G : () -> PhysQty(SIunit)
     G_0 : () -> PhysQty(SIunit)                                 K_J : () -> PhysQty(SIunit)
     L : () -> PhysQty(SIunit)                                   Phi_0 : () -> PhysQty(SIunit)
     R : () -> PhysQty(SIunit)                                   R_K : () -> PhysQty(SIunit)
     R_inf : () -> PhysQty(SIunit)                               alpha : () -> PhysQty(SIunit)
     c : () -> PhysQty(SIunit)                                   c0 : () -> PhysQty(SIunit)
     codata : () -> Table(Symbol,PhysQty(SIunit))                e : () -> PhysQty(SIunit)
     eV : () -> PhysQty(SIunit)                                  eps_0 : () -> PhysQty(SIunit)
     h : () -> PhysQty(SIunit)                                   hbar : () -> PhysQty(SIunit)
     k : () -> PhysQty(SIunit)                                   m_e : () -> PhysQty(SIunit)
     m_n : () -> PhysQty(SIunit)                                 m_p : () -> PhysQty(SIunit)
     mu_0 : () -> PhysQty(SIunit)                                sigma : () -> PhysQty(SIunit)
     u : () -> PhysQty(SIunit)                                  
    
    > 
    

The general form to get the value of a constant is ::
    
    <symbol>()@CODATA
    
whereby the package selector ``@CODATA`` may usually be neglected ::
    
    
    > F()
                               - 1
       (4)  96485.33212 s A mol
                                                        Type: PhysQty(SIunit)
    > eV
    
       (5)  eV
                                                        Type: Variable(eV)
    > eV()
    
                                2    - 2
       (6)  0.1602176634 E -18 m kg s
                                                        Type: PhysQty(SIunit)
    > m_e()  * c()^2
    
                                           2    - 2
       (7)  0.8187105776_8238859678 E -13 m kg s
                                                        Type: PhysQty(SIunit)
    




A table with all avialable constants can be called by ::
    
        > CT:=codata()
    
       (8)
       table
                             2    - 3
          R_K = 25812.80745 m kg s   A
      ,
                             - 2  - 1 2
          K_J = 483597.8484 m   kg   s A
      ,
                                        - 3 - 4
          sigma = 0.5670374419 E -7 kg s   K
      ,
                              - 1
          c0 = 299792458.0 m s
      ,
                             - 1
          c = 299792458.0 m s
      , ....
      
                          Type: Table(Symbol,PhysQty(SIunit))
                          

Then ::
    
    
    > CT.hbar

                             2    - 1
   (19)  0.1054571817 E -33 m kg s
                                                 Type: PhysQty(SIunit)
    > CT.k

                          2    - 2 - 1
   (20)  0.1380649 E -22 m kg s   K
                                                 Type: PhysQty(SIunit)
    > 

    
For output with ``uncertainty``, we have to use options in ``PQTY``::
    
    > setOutputMode(1)$PhysQty(SIunit)

   (9)  1
                                                                                                   Type: PositiveInteger
   > F()

                                        - 1
   (10)  96485.33212 +- 0.4 E -15 s A mol
                                              
                               Type: PhysQty(SIunit)
                               
        
Definitions 
-----------
(see source code) ::
    
    
      -- atomic mass constant
      -- $m_{\rm u}$
      -- Value 	 1.660 539 066 60 x 10-27 kg
      -- Standard uncertainty 	 0.000 000 000 50 x 10-27 kg
      -- Relative standard uncertainty 	  1.2 x 10-8
      -- Concise form 	 1.660 539 066 60(50) x 10-27 kg 
      ct.'m_u := CT(1.66053906660,0.00000000050, %kg(1))
      
      -- Avogadro constant
      -- $N_{\rm A},L$
      -- Value 	 6.022 140 76 x 10+23 mol-1
      -- Standard uncertainty 	 0.0 x 10+23 mol-1
      -- Relative standard uncertainty 	  exact
      -- Concise form 	 6.022 140 76 x 10+23 mol-1 
      ct.'L  := CT(6.02214076E23, 0.0, %mol(-1))
      
      -- Boltzmann constant
      -- $k$
      -- Value 	 1.380 649 x 10+23 J K-1
      -- Standard uncertainty 	 0.0 x 10-23 J K-1
      -- Relative standard uncertainty 	  exact
      -- Concise form 	 1.380 649 x 10-23 J K-1  
      ct.'k  := CT(1.380649E-23, 0.0E-23, %m(2)*%kg(1)*%s(-2)*%K(-1))
      
      -- conductance quantum
      -- $G_0$
      -- Value 	 7.748 091 7310 x 10-5 S
      -- Standard uncertainty 	 0.0 x 10-5 S
      -- Relative standard uncertainty 	  exact
      -- Concise form 	 7.748 091 729 x 10-5 S   
      ct.'G_0 := CT(7.748091729E-5, 0.0E-5, %m(-2)*%kg(-1)*%s(3)*%A(2))
      
      -- electric constant
      -- $\varepsilon_0$
      -- Value 	 8.854 187 8128 x 10-12 F m-1
      -- Standard uncertainty 0.000 000 0013 x 10-12 F m-1
      -- Relative standard uncertainty 	1.5 x 10-10
      -- Concise form 8.854 187 8128(13) x 10-12 F m-1       
      ct.'eps_0 := CT(8.8541878128E-12,0.0000000013E-12 , %m(-3)*%kg(-1)*%s(4)*%A(2))
      
      -- electron mass
      -- $m_{\rm e}$
      -- Value 	 9.109 383 7015 x 10-31 kg
      -- Standard uncertainty 	 0.000 000 0028 x 10-31 kg
      -- Relative standard uncertainty 	  3.0 x 10-10
      -- Concise form 	9.109 383 7015(28) x 10-31 kg   
      ct.'m_e := CT(9.1093837015E-31, 0.0000000028E-31, %kg(1))
      
      -- electron volt
      -- eV
      -- Value 	 1.602 176 634 x 10-19 J
      -- Standard uncertainty 	 0.0 x 10-19 J
      -- Relative standard uncertainty 	  exact
      -- Concise form 	1.602 176 634 x 10-19 J    
      ct.'eV := CT(1.602176634E-19, 0.0E-19, %m(2)*%kg(1)*%s(-2))
      
      -- elementary charge
      -- $e$
      -- Value 	  1.602 176 634 x 10-19  C
      -- Standard uncertainty 	 0.0 x 10-19 C
      -- Relative standard uncertainty 	  exact
      -- Concise form 	  1.602 176 634  x 10-19 C  
      ct.'e  := CT(1.602176634E-19, 0.0E-19, %A(1)*%s(1))
      
      -- Faraday constant
      -- $F$
      -- Value 	 96 485.332 12 C mol-1
      -- Standard uncertainty 	      0.0 C mol-1
      -- Relative standard uncertainty 	  exact
      -- Concise form 	96 485.332 12 C mol-1 
      ct.'F := CT(96485.33212, 0.0, %s(1)*%A(1)*%mol(-1))
      
      -- fine-structure constant
      -- $\alpha$
      -- Value 	 7.297 352 5693 x 10-3
      -- Standard uncertainty 	 0.000 000 0011 x 10-3
      -- Relative standard uncertainty 	  1.5 x 10-10
      -- Concise form 	 7.297 352 5693(11) x 10-3 
      ct.'alpha := CT(7.2973525693E-3, 0.0000000011E-3,%m(0)$SI)
      
      -- magnetic constant
      -- $\mu_0$
      -- Value 	 1.256 637 062 12 x 10-6 N A-2
      -- Standard uncertainty 0.000 000 000 19 x 10-6 N A-2
      -- Relative standard uncertainty 	1.5 x 10-10
      -- Concise form  1.256 637 062 12(19) x 10-6 N A-2   
      ct.'mu_0 := CT(1.25663706212E-6, 0.00000000019E-6, %m(1)*%kg(1)*%s(-2)*%A(-2)) 
      
      -- magnetic flux quantum
      -- ${\it \Phi}_0$
      -- Value 	 2.067 833 848 x 10-15 Wb
      -- Standard uncertainty exact
      -- Relative standard uncertainty 	exact
      -- Concise form 	 2.067 833 848 x 10-15 Wb 
      ct.'Phi_0 := CT(2.067833848E-15, 0.0E-15, %m(2)*%kg(1)*%s(-2)*%A(-1))
      
      -- molar gas constant
      -- $R$
      -- Value 	 8.314 462 618 J mol-1 K-1
      -- Standard uncertainty 	exact
      -- Relative standard uncertainty 	  exact
      -- Concise form 	8.314 462 618... J mol-1 K-1   
      ct.'R  := CT(8.314462618, 0.0, %m(2)*%kg(1)*%s(-2)*%mol(-1)*%K(-1))
      
      -- neutron mass CODATA2014
      -- $m_{\rm n}$
      -- Value 	 1.674 927 351 x 10-27 kg
      -- Standard uncertainty 	 0.000 000 074 x 10-27 kg
      -- Relative standard uncertainty 	  4.4 x 10-8
      -- Concise form 	 1.674 927 351(74) x 10-27 kg  
      ct.'m_n := CT(1.674927351E-27, 0.000000074E-27, %kg(1)) 
      
      -- Newtonian constant of gravitation
      -- $ G $
      -- Value 	 6.674 30 x 10-11 m3 kg-1 s-2
      -- Standard uncertainty 	 0.000 15 x 10-11 m3 kg-1 s-2
      -- Relative standard uncertainty 	  2.2 x 10-5
      -- Concise form 	 6.674 30(31) x 10-11 m3 kg-1 s-2    
      ct.'G  := CT(6.67430E-11, 0.00015E-11, %m(3)*%kg(-1)*%s(-2))
      
      -- Planck constant
      -- $ h $
      -- Value 	 6.626 070 15 x 10-34 J s
      -- Standard uncertainty 	 exact
      -- Relative standard uncertainty 	exact
      -- Concise form 	 6.626 070 15 x 10-34 J s
      ct.'h  := CT(6.62607015E-34, 0.0E-34, %m(2)*%kg(1)*%s(-1))
      
      -- Planck constant over 2 pi 
      -- $\hbar$
      -- Value 	 1.054 571 817 x 10-34 J s
      -- Standard uncertainty exact	 
      -- Relative standard  exact
      -- Concise form 	 1.054 571 817... x 10-34 J s    
      ct.'hbar := CT(1.054571817E-34, 0.0E-34,%m(2)*%kg(1)*%s(-1))
      
      -- proton mass
      -- $m_{\rm p}$
      -- Value 	 1.672 621 923 69 x 10-27 kg
      -- Standard uncertainty 	0.000 000 000 51 x 10-27 kg
      -- Relative standard uncertainty 	  3.1 x 10-8
      -- Concise form 1.672 621 923 69(51) x 10-27 kg       
      ct.'m_p := CT(1.67262192369E-27, 0.00000000051E-27, %kg(1))
      
      -- Rydberg constant
      -- $R_\infty$
      -- Value 	 10 973 731.568 508 m-1
      -- Standard uncertainty 	          0.000 065 m-1
      -- Relative standard uncertainty 	  5.9 x 10-12
      -- Concise form 	 10 973 731.568 539(55) m-1    
      ct.'R_inf := CT(10973731.568508, 0.000065, %m(-1))
      
      -- speed of light in vacuum
      -- $ c,c_0 $
      -- Value 	 299 792 458 m s-1
      -- Standard uncertainty 	 (exact)
      -- Relative standard uncertainty 	 (exact)
      -- Concise form 	 299 792 458 m s-1    
      ct.'c  := CT(299792458.0, 0.0, %m(1)*%s(-1))
      ct.'c0 := CT(299792458.0, 0.0, %m(1)*%s(-1))
      
      -- Stefan-Boltzmann constant
      -- $\sigma$
      -- Value 	 5.670 374 419 x 10-8 W m-2 K-4
      -- Standard uncertainty 	exact
      -- Relative standard uncertainty 	  exact
      -- Concise form 	5.670 374 419... x 10-8 W m-2 K-4  
      ct.'sigma := CT(5.670374419E-8,0.000013E-8,%kg(1)*%s(-3)*%K(-4)) 
      
      -- Josephson constant
      -- $K_J$
      -- Numerical value 	 483 597.848 4...  x 109 Hz V-1 
      -- Standard uncertainty 	 (exact)
      -- Relative standard uncertainty 	 (exact)
      --  Concise form 	 483 597.848 4... x 109 Hz V-1    
      ct.'K_J := CT(483597.8484,0.0,%m(-2)*%kg(-1)*%s(2)*%A(1))
      
      -- von Klitzing constant
      -- $R_K$
      -- Numerical value 	 25 812.807 45... Ohm
      -- Standard uncertainty 	 (exact)
      -- Relative standard uncertainty 	 (exact)
      -- Concise form 	 25 812.807 45... Ohm
      ct.'R_K := CT(25812.80745,0.0,%m(2)*%kg(1)*%s(-3)*%A(1))





===============================================
AWAIC - Atomic Weights and Isotope Compositions
===============================================

Developers and Contributors:
----------------------------

J. S. Coursey, D. J. Schwab, J. J. Tsai, and R. A. Dragoset
NIST Physical Measurement Laboratory

The atomic weights are available for elements 1 through 118 and isotopic 
compositions or abundances are given when appropriate. The atomic weights 
data were published by J. Meija et al in Atomic Weights of the Elements 2013, 
and the isotopic compositions data were published by M. Berglund and 
M.E. Wieser in Isotopic Compositions of the Elements 2009. The relative 
atomic masses of the isotopes data were published by M. Wang, G. Audi, 
A.H. Wapstra, F.G. Kondev, M. MacCormick, X. Xu1, and B. Pfeiffer in 
The AME2012 Atomic Mass Evaluation.

These data have been compiled from the above sources for the user's 
convenience and does not represent a critical evaluation by the 
NIST Physical Measurement Laboratory.

Get up-to-date data file from NIST
----------------------------------
Goto `NIST <https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses>`_
and select (Search the Database)::
    
     Output Type: Linearized ASCII Output
     Isotopes: All isotopes
     --> Get Data
     
Then ``save`` the output as a text file and remove the heading and trailing
lines. ::
    
    Atomic Number = 1
    Atomic Symbol = H
    Mass Number = 1
    Relative Atomic Mass = 1.00782503223(9)
    Isotopic Composition = 0.999885(70)
    Standard Atomic Weight = [1.00784,1.00811]
    Notes = m
    
    ...
    
    Atomic Number = 118
    Atomic Symbol = Og
    Mass Number = 295
    Relative Atomic Mass = 295.21624(69#)
    Isotopic Composition = 
    Standard Atomic Weight =  
    Notes =  
    

There is already a file ``awaic.txt`` in ``awaic/src/``, however, it is 
recommended to fetch the latest version if th``last update`` on the NIST
page is newer.


Load the AWAIC package
----------------------
In FriCAS do ::
        
        (3) -> quickLoad "awaic"
       Compiling function quickLoad with type String -> Void
    Value = T
    To load "awaic":
      Load 1 ASDF system:
        awaic
    ; Loading "awaic"
       AtomicWeights is now explicitly exposed in frame initial
       AtomicWeights will be automatically loaded when needed from
          /Users/kfp/quicklisp/local-projects/spadlib/awaic/lib/AWAIC.NRLIB/AWAIC
    
    Value = ("awaic")
                                                                       Type: Void
    (4) -> )sh AWAIC
     AtomicWeights is a package constructor
     Abbreviation for AtomicWeights is AWAIC
     This constructor is exposed in this frame.
    ------------------------------- Operations --------------------------------
    
     readDataAsInput : String -> Void      str2float : String -> Float
     atomicMass : (String, StringTable(List(String))) -> Float
     atomicMassUnc : (String, StringTable(List(String))) -> Float
     atomicNumber : (String, StringTable(List(String))) -> Integer
     atomicSymbol : (String, StringTable(List(String))) -> Symbol
     elements : StringTable(List(String)) -> List(String)
     isotopeSymbol : (String, StringTable(List(String))) -> Symbol
     massNumber : (String, StringTable(List(String))) -> Integer
     readDefault : () -> StringTable(List(String))
     readRawDataFile : String -> StringTable(List(String))
     str2floatUncertainty : String -> Float
    


Read the data file into FriCAS
------------------------------
First of all tell FriCAS where your data file can be found (e.g.): ::
    
    data:= "C:/Users/kfp/quicklisp/local-projects/spadlib/awaic/src/awaic.txt"
    
Then we can read it by::
     
    (2) -> aw:=readRawDataFile data;

                                              Type: StringTable(List(String))
                                              
Now ``aw`` is a string table::
    
    (14) -> )sh StringTable
     StringTable(Entry: Type) is a domain constructor
     Abbreviation for StringTable is STRTBL
     This constructor is not exposed in this frame.
    ------------------------------- Operations --------------
     copy : % -> %                         dictionary : () -> %
     elt : (%, String, Entry) -> Entry     ?.? : (%, String) -> Entry
     empty : () -> %                       empty? : % -> Boolean
     entries : % -> List(Entry)   
     ...
     
The size is (for instance) ::
    
    (15) -> #aw

    (15)  3352
                                                        Type: PositiveInteger

                                                        
To get the keys: ::
    
    (16) -> keys aw

   (16)
   ["Og295", "Og294", "Og293", "Uus294", "Ts293", "Ts292", "Ts291", "Lv293",
    "Lv292", "Lv291", "Lv290", "Lv289", "Uup291", "Mc290", "Mc289", "Mc288",
    "Mc287", "Fl289", "Fl288", "Fl287", "Fl286", "Fl285", "Nh287", "Nh286",
    "Nh285", "Nh284", "Nh283", "Nh282", "Nh281", "Nh280", "Nh279", "Nh278",
    "Cn285", "Cn284", "Cn283", "Cn282", "Cn281", "Cn280", "Cn279", "Cn278",
    "Cn277", "Cn276", "Rg283", "Rg282", "Rg281", "Rg280", "Rg279", "Rg278",
    "Rg277", "Rg276", "Rg275", "Rg274", "Rg273", "Rg272", "Ds281", "Ds280",
    "Ds279", "Ds278", "Ds277", "Ds276", "Ds275", "Ds274", "Ds273", "Ds272",
    "Ds271", "Ds270", "Ds269", "Ds268", "Ds267", "Mt279", "Mt278", "Mt277",
    "Mt276", "Mt275", "Mt274", "Mt273", "Mt272", "Mt271", "Mt270", "Mt269",
    "Mt268", ....
    
    
To obtain a record to a key (Symbol): ::
    
    (17) -> aw "Og295"

    (17)  ["118", "Og", "295", "295.21624(69#)", "", "", ""]
                                                           Type: List(String)


One may use the specific accessor functions, e.g. ::
    
    (18) -> atomicNumber("Og295",aw)

    (18)  118
                                                        Type: PositiveInteger
    (19) -> atomicSymbol("Og295",aw)

    (19)  Og
                                                                 Type: Symbol
    (20) -> atomicMass("Og295",aw)

    (20)  295.21624
                                                                  Type: Float

    (21) -> atomicMassUnc("H1",aw)

    (21)  0.9 E -10
                                                                  Type: Float
    (22) -> isotopeSymbol("He4",aw)

         4
    (22)  He
         2
                                                                 Type: Symbol
                                                                  
                                                                                                                                                                                             
and so on.  

The correspondence is as follows: ::
           
    Atomic Number = 1 ......................... atomicNumber -> 1
    Atomic Symbol = H ......................... atomicSymbol -> H
    Mass Number = 1 ........................... massNumber -> 1
    Relative Atomic Mass 1.00782503223(9) ..... atomicMass -> 1.00782503223
                                          ..... atomicMassUnc ->  0.9 E -10 
   
..


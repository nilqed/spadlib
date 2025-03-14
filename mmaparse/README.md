# MMA Parser for FriCAS (CL)[^1]

## Compile

    )compile mmaparse
    
in the same directory where `mma2spad.lisp` lives.

    (1) -> )sh MMA
    MmaParser is a package constructor
    Abbreviation for MmaParser is MMA 
    This constructor is exposed in this frame.
    ------------------------------- Operations --------------------------------

    lisp : String -> SExpression          mmaOnce : () -> SExpression
    mmaParse : String -> SExpression      mmaREPL : () -> Void
     

## Examples

`mmaParse` parses a MMA string:

    (2) -> mmaParse "f[x_,y_]=x^2-y^2"

    (2)  (Set (f x y) (Plus (Power x 2) (Times - 1 (Power y 2))))
                                                            Type: SExpression

    (3) -> mmaParse "f[t_]={Cos[t], Sin[t], t} "

    (3)  (Set (f t) (List (Cos t) (Sin t) t))
                                                            Type: SExpression


    (3) -> mmaParse "If[ x < 0, 0, If[ x > 1, 1, x] ]"

    (3)  (If (Comparison x Less 0) 0 (If (Comparison x Greater 1) 1 x))
                                                            Type: SExpression











[^1]:https://people.eecs.berkeley.edu/~fateman/lisp/mma4max/

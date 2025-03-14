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

### `mmaParse` parses a MMA string:

    (2) -> mmaParse "f[x_,y_]=x^2-y^2"

    (2)  (Set (f x y) (Plus (Power x 2) (Times - 1 (Power y 2))))
                                                            Type: SExpression

    (3) -> mmaParse "f[t_]={Cos[t], Sin[t], t} "

    (3)  (Set (f t) (List (Cos t) (Sin t) t))
                                                            Type: SExpression


    (3) -> mmaParse "If[ x < 0, 0, If[ x > 1, 1, x] ]"

    (3)  (If (Comparison x Less 0) 0 (If (Comparison x Greater 1) 1 x))
                                                            Type: SExpression



### `mmaOnce` parses once interactively. 
The result is also stored in `mma_result$Lisp`:

    (4) -> mmaOnce()
     Sin[X]*D[Cos[X],X]+1234567890

    (4)  (Plus (Times (Sin X) (D (Cos X) X)) 1234567890)
                                                            Type: SExpression

    (5) -> mma_result$Lisp

    (5)  (Plus (Times (Sin X) (D (Cos X) X)) 1234567890)
                                                            Type: SExpression

    (6) -> r:=mmaOnce()
     X=Y+2

    (6)  (Set X (Plus Y 2))
                                                            Type: SExpression

### `mmaREPL()` is ... a REPL
where the first line (===) returns an SExpression and the second output line a `unparsed` version of the former.


    (7) -> mmaREPL()

       *** Type 'quit' to stop.

    f[x_,y_]=x^2-y^2
    ===    (Set  (f (Pattern x (Blank)) (Pattern y (Blank))) (Plus (Power x 2) (Times - 1 (Power y 2))))
    +++   Set(f(Pattern(x,Blank()),Pattern(y,Blank())),Plus(Power(x,2),Times(-1,Power(y,2))))

    f[t_]={Cos[t], Sin[t], t} 
    ===    (Set (f (Pattern t (Blank))) (List (Cos t) (Sin t) t))
    +++    Set(f(Pattern(t,Blank())),List(Cos(t),Sin(t),t))

    Sin[X]*D[Cos[X],X]+1234567890
    ===    (Plus (Times (Sin X) (D (Cos X) X)) 1234567890)
    +++    Plus(Times(Sin(X),D(Cos(X),X)),1234567890)

    X=Y+2
    ===    (Set X (Plus Y 2))
    +++    Set(X,Plus(Y,2))

    quit
    ===    quit
    +++    quit

                                                                   Type: Void
    (8) -> 


                                                            







[^1]:https://people.eecs.berkeley.edu/~fateman/lisp/mma4max/

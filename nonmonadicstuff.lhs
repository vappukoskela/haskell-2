COMPPROG: translates program into code, passing a label around as an extra parameter

> compprog :: Prog -> Label -> (Code, Label)
> compprog (Assign v e) n = ((compexpr e) ++ [POP v], n)
> compprog (If e p1 p2) n = compif e p1 p2 n
> compprog (While e p) n = compwhile e p n
> compprog (Seqn []) n = ([], n)
> compprog (Seqn (p:ps)) n = ((c++cs), n'')
>                               where
>                                   (c, n') = compprog p n
>                                   (cs, n'') = compprog (Seqn ps) n'

COMPIF: compiles 'if' statement
        if the expression is true (i.e. not zero) then run the first program
        else run the second program

> compif :: Expr -> Prog -> Prog -> Label -> (Code, Label)
> compif e p1 p2 n = ((ce ++ [JUMPZ n] ++ c1 ++ 
>                      [JUMP (n+1), LABEL n] ++ 
>                      c2 ++ [LABEL (n+1)]), n'')
>                          where
>                              ce = compexpr e
>                              (c1, n') = compprog p1 (n+2)
>                              (c2, n'') = compprog p2 n'

COMPWHILE: compiles while loop

> compwhile :: Expr -> Prog -> Label -> (Code, Label)
> compwhile e p n = ([LABEL n] ++ ce ++ 
>                    [JUMPZ (n+1)] ++ cp ++ 
>                    [JUMP n, LABEL (n+1)], n')
>                       where
>                           ce = compexpr e
>                           (cp, n') = compprog p (n+2)
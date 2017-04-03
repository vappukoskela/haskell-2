G52AFP Coursework 2 - Monadic Compiler

Rebecca Tickle and Vappu Koskela
psyrlt@nottingham.ac.uk, psyvk@nottingham.ac.uk

--------------------------------------------------------------------------------

> import Data.Char

--------------------------------------------------------------------------------

Imperative language:

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seqn [Prog]
>             deriving Show
>
> data Expr = Val Int | Var Name | App Op Expr Expr
>             deriving Show
>
> type Name = Char
>
> data Op   = Add | Sub | Mul | Div
>             deriving Show

Factorial example:

> fac :: Int -> Prog
> fac n = Seqn [Assign 'A' (Val 1),
>               Assign 'B' (Val n),
>               While (Var 'B') (Seqn
>                  [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                   Assign 'B' (App Sub (Var 'B') (Val (1)))])]

Virtual machine:

> type Stack = [Int]
>
> type Mem   = [(Name,Int)]
>
> type Code  = [Inst]
> 
> data Inst  = PUSH Int
>            | PUSHV Name
>            | POP Name
>            | DO Op
>            | JUMP Label
>            | JUMPZ Label
>            | LABEL Label
>              deriving Show
> 
> type Label = Int

State monad:

> type State = Label
>
> newtype ST a = S (State -> (a, State))
>
> app :: ST a -> State -> (a,State)
> app (S st) x     =  st x
>
> instance Functor ST where
>    -- fmap :: (a -> b) -> ST a -> ST b
>    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
>
> instance Applicative ST where
>    -- pure :: a -> ST a
>    pure x = S (\s -> (x,s))
>
>    -- (<*>) :: ST (a -> b) -> ST a -> ST b
>    stf <*> stx = S (\s ->
>       let (f,s')  = app stf s
>           (x,s'') = app stx s' in (f x, s''))
>
> instance Monad ST where
>    -- return :: a -> ST a
>    return x = S (\s -> (x,s))
>
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

--------------------------------------------------------------------------------

EXAMPLE CODE: for factorial example

> exCode :: Code
> exCode = [PUSH 1, POP 'A',
>           PUSH 10, POP 'B',
>               LABEL 0,
>               PUSHV 'B', JUMPZ 1,
>               PUSHV 'A', PUSHV 'B', DO Mul, POP 'A',
>               PUSHV 'B', PUSH 1, DO Sub, POP 'B',
>               JUMP 0,
>           LABEL 1]

--------------------------------------------------------------------------------

EXEC: executes code and returns final contents of memory

PUSH value n -> put n on front of list
POP -> remove first value of list

can we change the type of this so that it takes stack and memory as parameters?

> exec :: Code -> Mem
> exec [] = []
> exec c = myExec c [] [] c

MYEXEC: Takes the code to execute, the current stack, and the current memory, then returns the new memory
        If the list of instructions is empty, then return the current memory
        
        j is the first argument supplied to an operator, i is the second argument

        TRY TO REMOVE: currently has oc parameter which is the whole code it is executing
                       need to maintain to enable us to find labels
                       probably a better way of doing this

        NEEDS CHECKING

> myExec :: Code -> Stack -> Mem -> Code -> Mem
> myExec [] _ m oc              = m
> myExec (PUSH i:c) s m oc      = myExec c (i:s) m oc
> myExec (PUSHV n:c) s m oc     = myExec c ((getVal n m):s) m oc
> myExec (POP n:c) (x:s) m oc   = myExec c s (putVal n m x) oc
> myExec (DO x:c) (i:j:s) m oc  = myExec c ((eval x j i):s) m oc
> myExec (JUMP l:c) s m oc      = myExec (findLabel oc l) s m oc 
> myExec (JUMPZ l:c) (x:s) m oc | x == 0    = myExec (findLabel oc l) s m oc
>                               | otherwise = myExec c s m oc
> myExec (LABEL l:c) s m oc     = myExec c s m oc

GETVAL: Takes current memory and variable name as parameters
        Returns the value stored in that variable
        Assumes the name provided exists in the memory

> getVal :: Name -> Mem -> Int
> getVal x ((n,v):ns) | n == x     = v
>                     | otherwise  = getVal x ns

PUTVAL: Takes current memory, variable name, and a value as parameters
        Returns new memory with the value stored under the given variable name
        If the variable already exists, the value is replaced
        If it does not already exist, a new variable is created, with the value attached

> putVal :: Name -> Mem -> Int -> Mem
> putVal n [] i         = [(n,i)]
> putVal n ((m,v):ms) i | n == m    = (m,i) : ms
>                       | otherwise = (m,v) : putVal n ms i

EVAL: Takes an operator and two integers as parameters
      Returns the result of applying the operator to the integers

> eval :: Op -> Int -> Int -> Int
> eval Add x y = x + y
> eval Sub x y = x - y
> eval Mul x y = x * y
> eval Div x y = x `div` y

FINDLABEL: Takes some code and a label
           Returns the code from the point of the label onwards
           Assumes the label provided exists in the code

> findLabel :: Code -> Label -> Code
> findLabel (LABEL l:cs) x | l == x    = (LABEL l):cs
>                          | otherwise = findLabel cs x
> findLabel (_:cs) x       = findLabel cs x

--------------------------------------------------------------------------------

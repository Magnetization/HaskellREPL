-- 1 Expression Trees
import Data.Char
import Data.List
import Parsing
import System.IO
-- Expression Trees
--data Maybe a = Just a | Nothing
data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)
data Expr 
    = Bin Binop Expr Expr
    | Val Int
    | Var String
    deriving (Eq, Show)
type Env = [(String, Int)]
-- a = [("x",1),("y",2),("z",3),("c",4)] :: Env -- test case
-- look up a certain variable in environment
lookUp :: Env -> String -> Maybe Int
lookUp xs s = case [ y | (x,y) <- xs , x == s] of
    [] -> Nothing  -- not found return nothing
    (y:_) -> Just y -- found

-- evaluate expressions
eval :: Env -> Expr -> Maybe Int
eval env (Val n) = Just n
-- if val, return int
eval env (Var str) = lookUp env str
-- if variable, lookup that and return 

-- for all the other operations, have to examine whether the  result exists before doing the operations
-- Add operation
eval env (Bin Add x y) = case eval env x of
    Nothing -> Nothing
    Just x -> case eval env y of
        Nothing -> Nothing
        Just y -> Just (x + y)

-- Sub operation
eval env (Bin Sub x y) = case eval env x of
    Nothing -> Nothing
    Just x -> case eval env y of
        Nothing -> Nothing
        Just y -> Just (x - y)

-- Mul operation
eval env (Bin Mul x y) = case eval env x of
    Nothing -> Nothing
    Just x -> case eval env y of
        Nothing -> Nothing
        Just y -> Just (x * y)

-- Div operation
eval env (Bin Div x y) = case eval env x of
    Nothing -> Nothing
    Just x -> case eval env y of
        Nothing -> Nothing
        Just y -> if y == 0
            then Nothing
            else Just ( x `div` y)

-- Mod operation
eval env (Bin Mod x y) = case eval env x of
    Nothing -> Nothing
    Just x -> case eval env y of
        Nothing -> Nothing
        Just y -> if y == 0
            then Nothing
            else Just (x `mod` y)
        
-- 2 Parsing Expressions
-- pExpr :: Parser Expr
-- pExpr = do f <- pTerm
--            ( (do token $ char '+'
--                  t <- pExpr
--                  return $ Bin Add f t) +++ (do token $ char '-'
--                                                t <- pExpr
--                                                return $ Bin Sub f t) ) +++ return f

-- pTerm :: Parser Expr
-- pTerm = do f <- pFactor
--            ((do token $ char '*' 
--                 t <- pTerm
--                 return $ Bin Mul f t) +++ ( do token $ char '/'
--                                                t <- pTerm
--                                                return $ Bin Div f t) +++ ( do token $ char '%'
--                                                                               t <- pTerm
--                                                                               return $ Bin Mod f t ) )+++ return f

-- the code above is not correct, because the previous code do the operation from right to left(tail recursion), so we have to use foldl to do head recursion
pExpr :: Parser Expr
pExpr = do t <- pTerm
           ts <- many pOpTerm 
           return $ foldl addOrSub t ts
           where addOrSub t1 (op, t2) = (if op == '+' then Bin Add else Bin Sub) t1 t2
                 pOpTerm = do
                           op <- (token $ char '+') +++ (token $ char '-')
                           tm <- pTerm
                           return (op, tm)
-- the same as pExpr
pTerm :: Parser Expr
pTerm = do f <- pFactor
           fs <- many pOpFactor
           return $ foldl mulOrDM f fs
           where mulOrDM f1 (op, f2) = (if op == '*' then Bin Mul else if op == '/' then Bin Div else Bin Mod) f1 f2
                 pOpFactor = do
                    op <- (token $ char '*') +++ (token $ char '/') +++ (token $ char '%')
                    fc <- pFactor
                    return (op, fc)

-- for a factor, test if it is p is expr or interger or identifier
pFactor :: Parser Expr 
pFactor = pExprr +++ pInt  +++ pIden
  where pExprr = do token $ char '('
                    e <- pExpr
                    token $ char ')' 
                    return e
        pInt = do x <- integer
                  return $ Val x
        pIden = do x <- ident -- ident is declared in parsing.hs
                   return $ Var x

runParser :: Parser a -> String -> Maybe a
runParser parser str =
    case parse parser str of
        []       -> Nothing
        (x, y):_ -> if null y then Just x else Nothing

-- 3 Compilation

data Instr = IVal Int | IBin Binop | IVar String deriving (Eq, Show)
type Stack = [Int]
type Prog = [Instr]
-- below are basic operations on stack
-- push an int into stack
push :: Int -> Stack -> Stack
push num xs = num:xs

-- pop an int from stack 
pop :: Stack -> (Maybe Int, Stack)
pop [] = (Nothing,[])
pop (x:xs) = (Just x, xs)

-- the top element of the stack, if the stack is empty return Nothing
top :: Stack -> Maybe Int
top [] = Nothing
top (x:_) = Just x

-- the size of the stack (the length of the list)
size :: Stack -> Int
size [] = 0
size xs = length(xs)

-- test if the stack is empty or not, return True or False
isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty xs = False

-- change the type of int from maybe int to int(make it easy for us to do calculation)
toJust :: Maybe Int -> Int
toJust (Just n) = n
toJust Nothing = error "nothing!"

-- interpreter for the operators
operator :: Binop -> Int -> Int -> Int
operator (Add) = (+)
operator (Sub) = (-)
operator (Mul) = (*)
operator (Div) = div
operator (Mod) = mod

-- push the element to stack, if there comes an operation, pop two elements and push the result back to stack
pushStack :: Prog -> Env -> Stack -> Stack
pushStack [] env stack = stack
pushStack (IVal n: rest) env stack = pushStack rest env (push n stack)
pushStack (IVar str: rest) env stack 
    | lookUp env str /= Nothing = pushStack rest env (push (toJust (lookUp env str)) stack)
    | otherwise = [0,0]
pushStack (IBin op: rest) env stack
    | size stack >= 2 = pushStack rest env ( push (operator op (stack !! 0) (stack !!1)) (drop 2 stack))
    | otherwise = [0,0]

-- evaluate the result, if there is only one result, means the calculation is correct, otherwise must be wrong
evalStack :: Stack -> Maybe Int
evalStack xs
    | size xs == 1 = Just (head xs)
    | otherwise = Nothing
runProg :: Prog -> Env -> Maybe Int
runProg prog env = evalStack ( pushStack prog env [])

compile :: Expr -> Prog
compile (Val n) = [IVal n]
compile (Var x) = [IVar x]
compile (Bin op x y) = compile y ++ compile x ++ [IBin op]

-- 4 Optimization
-- use pattern matching to do the optimization
-- after optimizing the children, we have to see whether the parent can be optimized,
-- if so, optimize the parent
convertMaybe :: Maybe Expr -> Expr
convertMaybe (Just ( Val n)) = (Val n)
convertMaybe (Just ( Var x)) = (Var x)
convertMaybe (Just (Bin Add x y)) = Bin Add (convertMaybe (Just x))  (convertMaybe (Just y))
convertMaybe (Just (Bin Mul x y)) = Bin Mul (convertMaybe (Just x))  (convertMaybe (Just y))
convertMaybe (Just (Bin Mod x y)) = Bin Mod (convertMaybe (Just x))  (convertMaybe (Just y))
convertMaybe (Just (Bin Div x y)) = Bin Div (convertMaybe (Just x))  (convertMaybe (Just y))
optimize :: Expr -> Maybe Expr
optimize (Val n) = Just (Val n)
optimize (Var x) = Just (Var x)
optimize (Bin Sub x (Val 0)) = optimize x
optimize (Bin Sub (Val a) (Val b)) = Just (Val (a-b))
optimize (Bin Sub (Var x) (Val a)) = Just (Bin Sub (Var x) (Val a))
optimize (Bin Sub (Val a) (Var x)) = Just (Bin Sub (Val a) (Var x))
optimize (Bin Sub (Var x) (Var y)) = Just (Bin Sub (Var x) (Var y))
optimize (Bin Sub x y) 
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Sub (convertMaybe (optimize x)) (convertMaybe(optimize y))) /= (Bin Sub x y) = optimize (Bin Sub (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Sub (convertMaybe (optimize x)) (convertMaybe(optimize y))) == (Bin Sub x y) = Just(Bin Sub (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | otherwise = Nothing

optimize (Bin Add x (Val 0)) = optimize x
optimize (Bin Add (Val 0) x) = optimize x
optimize (Bin Add (Val a) (Val b)) = Just (Val (a+b))
optimize (Bin Add (Var x) (Val a)) = Just (Bin Add (Var x) (Val a))
optimize (Bin Add (Val a) (Var x)) = Just (Bin Add (Val a) (Var x))
optimize (Bin Add (Var x) (Var y)) = Just (Bin Add (Var x) (Var y))
optimize (Bin Add x y) 
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Add (convertMaybe (optimize x)) (convertMaybe(optimize y))) /= (Bin Add x y) = optimize (Bin Add (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Add (convertMaybe (optimize x)) (convertMaybe(optimize y))) == (Bin Add x y) = Just(Bin Add (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | otherwise = Nothing

optimize (Bin Mul (Val 0) y) = Just (Val 0)
optimize (Bin Mul x (Val 0)) = Just (Val 0)
optimize (Bin Mul (Var x) (Val a)) = Just (Bin Mul (Var x) (Val a))
optimize (Bin Mul (Val a) (Var x)) = Just (Bin Mul (Val a) (Var x))
optimize (Bin Mul (Var x) (Var y)) = Just (Bin Mul (Var x) (Var y))
optimize (Bin Mul x y) 
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Mul (convertMaybe (optimize x)) (convertMaybe(optimize y))) /= (Bin Mul x y) = optimize (Bin Mul (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Mul (convertMaybe (optimize x)) (convertMaybe(optimize y))) == (Bin Mul x y) = Just(Bin Mul (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | otherwise = Nothing

optimize (Bin Div x (Val 0)) = Nothing
optimize (Bin Div (Val 0) x) = Just (Val 0)
optimize (Bin Div (Var x) (Val a)) = Just (Bin Div (Var x) (Val a))
optimize (Bin Div (Val a) (Var x)) = Just (Bin Div (Val a) (Var x))
optimize (Bin Div (Var x) (Var y)) = Just (Bin Div (Var x) (Var y))
optimize (Bin Div x y) 
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Div (convertMaybe (optimize x)) (convertMaybe(optimize y))) /= (Bin Div x y) = optimize (Bin Div (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Div (convertMaybe (optimize x)) (convertMaybe(optimize y))) == (Bin Div x y) = Just(Bin Div (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | otherwise = Nothing

optimize (Bin Mod x (Val 0)) = Nothing
optimize (Bin Mod (Val 0) x) = Just (Val 0)
optimize (Bin Mod (Var x) (Val a)) = Just (Bin Mod (Var x) (Val a))
optimize (Bin Mod (Val a) (Var x)) = Just (Bin Mod (Val a) (Var x))
optimize (Bin Mod (Var x) (Var y)) = Just (Bin Mod (Var x) (Var y))
optimize (Bin Mod x y) 
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Mod (convertMaybe (optimize x)) (convertMaybe(optimize y))) /= (Bin Mod x y) = optimize (Bin Mod (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | ((optimize x) /= Nothing && (optimize y) /= Nothing) && (Bin Mod (convertMaybe (optimize x)) (convertMaybe(optimize y))) == (Bin Mod x y) = Just(Bin Mod (convertMaybe (optimize x)) (convertMaybe(optimize y)))
    | otherwise = Nothing


-- 5 REPL
-- create a new data type to interpreter the command
data Command
    = Quit
    | Delete String
    | Variable String
    | PrintEnv
    | LetBe String Expr   deriving(Show)

-- interpreter  test the command, use the data Command as return value
interpreter :: Parser Command
interpreter = pQuit +++ pDelete +++ pVariable +++ pPrintEnv +++ pLetBe
    where
        pQuit = do space
                   string "quit"
                   return Quit
        pDelete = do space
                     string "del"
                     space
                     name <- ident
                     return $ Delete name
        pVariable = do space
                       string "var"
                       space
                       name <- ident
                       return $ Variable name
        pPrintEnv = do space
                       string "env"
                       return PrintEnv
        pLetBe = do space
                    string "let"
                    space
                    name <- ident
                    token $ char '='
                    e <- pExpr
                    return $ LetBe name e

-- main 
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    putStrLn "Enter your command"
    repl []

repl :: Env -> IO ()
repl env = do
    putStr "\n> "
    line <- getLine
    dispatch env line

-- print something and do it again
loop :: String -> Env -> IO ()
loop str next = do
    putStrLn str
    repl next

-- convert an int to string, make it easy for us to print it out
intToString :: Int ->  String
intToString n 
    | n < 10 = [intToDigit n]
    | otherwise = (intToString (n `div` 10)) ++ [(intToDigit (n `mod` 10))]

printLine :: String -> Int -> String
printLine x y = x ++ " = " ++ (intToString y) ++ "\n"
-- sort environment and print it all
sortAndPrint :: Env -> String
sortAndPrint [] = "\n"
sortAndPrint ((x,y):xs) = (printLine x y) ++ (sortAndPrint xs)
 
-- test if the input is all space
allSpace :: String -> Bool
allSpace "" = False
allSpace [x] = if x == ' ' then True else False
allSpace (x:xs) =  (allSpace [x]) && (allSpace xs)

dispatch :: Env -> String -> IO ()
--dispatch = error "!!!"
dispatch env line = 
    case runParser interpreter line of
        (Just command) -> exec command
        Nothing -> case runParser pExpr line of
            Just expr -> case eval env (convertMaybe(runParser pExpr line)) of
                Just int -> loop ("ans = " ++ intToString(toJust(eval env (convertMaybe(runParser pExpr line))))) env
                Nothing -> loop "Error" env
            Nothing -> if (allSpace line) || (line == "") then loop "" env else loop "Error" env
    where
        exec :: Command -> IO()
        exec Quit = return()
        exec PrintEnv = loop (sortAndPrint (sort(env))) env
        exec (Variable str) = 
             case lookUp env str of
                  Nothing -> loop "Error" env
                  Just int -> loop (str ++ " = " ++ intToString(toJust(lookUp env str)))  env
        exec (Delete str)   = 
            case lookUp env str of
                Nothing -> loop "Error" env
                Just int -> let newEnv = [(x,y) | (x,y) <- env, x /= str] in loop ("deleted " ++ str) newEnv
        exec (LetBe str expr) = 
            case eval env expr of
                Nothing -> loop "Error" env
                Just int -> case lookUp env str of
                    Nothing ->  let newEnv = env ++ [(str,int)] in loop (str ++ " = " ++ intToString(int) ) newEnv
                    Just int -> let newEnv = tempEnv ++ [(str,newInt)] in loop (str ++ " = " ++ intToString(newInt) ) newEnv
                                    where tempEnv = [(x,y) | (x,y) <- env , x /= str]
                                    where newInt = toJust(eval env expr)
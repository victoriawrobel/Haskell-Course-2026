module Solution where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Control.Monad.Writer

-- Task 1
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    options <- Map.lookup pos maze
    Map.lookup dir options

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = return pos
followPath maze pos (d:ds) = do
    nextPos <- move maze pos d
    followPath maze nextPos ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = return [pos]
safePath maze pos (d:ds) = do
    nextPos <- move maze pos d
    remPath <- safePath maze nextPos ds
    return (pos : remPath)


-- Task 2
type Key = Map Char Char
decrypt :: Key -> String -> Maybe String
decrypt key str = traverse (\c -> Map.lookup c key) str

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key words = traverse (decrypt key) words


-- Task 3
type Guest = String
type Conflict = (Guest, Guest)

-- Assumption: Each guest has a unique name
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    first <- guests
    let remaining = filter (/= first) guests
    rest <- permutations first remaining
    guard (not (haveConflict (head rest) first))
    return (reverse rest)

    where
    haveConflict :: Guest -> Guest -> Bool
    haveConflict g1 g2 =
        (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts

    permutations :: Guest -> [Guest] -> [[Guest]]
    permutations lastGuest [] = return [lastGuest]
    permutations lastGuest remaining = do
        nextGuest <- remaining
        guard (not (haveConflict lastGuest nextGuest))
        let newRemaining = filter (/= nextGuest) remaining
        rest <- permutations nextGuest newRemaining
        return (lastGuest : rest)


-- Task 4
data Result a = Failure String | Success a [String] deriving (Show, Eq)

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success val warnings) = Success (f val) warnings

instance Applicative Result where
    pure val = Success val []
    (Failure msg) <*> _ = Failure msg
    _ <*> (Failure msg) = Failure msg
    (Success f warnings1) <*> (Success val warnings2) = Success (f val) (warnings1 ++ warnings2)

instance Monad Result where
    (Failure msg) >>= _ = Failure msg
    (Success val warnings) >>= f = case f val of
        Failure msg -> Failure msg
        Success newVal newWarnings -> Success newVal (warnings ++ newWarnings)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure msg = Failure msg

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = warn "No human ever lived that long" >> return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages


-- Task 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Add expr1 expr2) = do
    e1 <- simplify expr1
    e2 <- simplify expr2
    case (e1, e2) of
        (Lit 0, e) -> do
            tell ["Add identity:  0 + " ++ show e ++ " -> " ++ show e]
            return e
        (e, Lit 0) -> do
            tell ["Add identity: " ++ show e ++ " + 0 -> " ++ show e]
            return e
        (Lit a, Lit b) -> do
            tell ["Performing addition:  " ++ show a ++ " + " ++ show b]
            return (Lit(a+b))
        (e1, e2)   -> return (Add e1 e2)

simplify (Mul expr1 expr2) = do
    e1 <- simplify expr1
    e2 <- simplify expr2
    case (e1, e2) of
        (Lit 0, _) -> do
            tell ["Multiplication by 0 -> 0"]
            return (Lit 0)
        (_, Lit 0) -> do
            tell ["Multiplication by 0 -> 0"]
            return (Lit 0)
        (Lit 1, e) -> do
            tell ["Mul identity: 1 * " ++ show e ++ " -> " ++ show e]
            return e
        (e, Lit 1) -> do
            tell ["Mul identity: " ++ show e ++ " * 1 -> " ++ show e]
            return e
        (Lit a, Lit b) -> do
            tell ["Performing multiplication:  " ++ show a ++ " * " ++ show b]
            return (Lit(a*b))
        (e1, e2)   -> return (Mul e1 e2)

simplify (Neg expr) = do
    e <- simplify expr
    case e of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            return inner
        _ -> return (Neg e)


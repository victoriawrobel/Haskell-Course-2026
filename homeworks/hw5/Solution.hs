module Solution where
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (unless)
import System.IO (hFlush, stdout)

-- Task 1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr instr = do
    stack <- get
    case (instr, stack) of
        (PUSH n, _) -> put (n : stack)
        (POP, _ : xs) -> put xs
        (DUP, x : xs) -> put (x : x : xs)
        (SWAP, x : y : xs) -> put (y : x : xs)
        (ADD, x : y : xs) -> put ((x + y) : xs)
        (MUL, x : y : xs) -> put ((x * y) : xs)
        (NEG, x : xs) -> put ((-x) : xs)
        _ -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []


-- Task 2
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right
  deriving (Show, Eq)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n

eval (Assign name e) = do
    val <- eval e
    modify (Map.insert name val)
    return val

eval (Var x) = do
    env <- get
    return (env Map.! x)

eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 + v2)

eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 * v2)

eval (Neg e) = do
    v <- eval e
    return (-v)

eval (Seq e1 e2) = do
    _ <- eval e1
    eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty


-- Task 3
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just result -> return result
        Nothing -> do
            result <- compute
            modify (Map.insert (i, j) result)
            return result
    where
    compute :: State (Map (Int, Int) Int) Int
    compute
        | i == 0 = return j
        | j == 0 = return i
        | otherwise = do
            let x = xs !! (i - 1)
                y = ys !! (j - 1)
            if x == y
                then editDistM xs ys (i - 1) (j - 1)
                else do
                    deletion <- editDistM xs ys (i - 1) j
                    insertion <- editDistM xs ys i (j - 1)
                    substitution <- editDistM xs ys (i - 1) (j - 1)
                    return (1 + min deletion (min insertion substitution))

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- ----------------------------------------

data Tile = Normal | Obstacle | Treasure | Trap | Decision [String] | Goal
  deriving (Show, Eq)

data GameState = GameState
  { path :: String,
  pos :: Int,
  energy :: Int,
  points :: Int
  } deriving (Show, Eq)

type AdventureGame a = StateT GameState IO a

getBoard :: String -> [Tile]
getBoard "start" = [Normal, Obstacle, Decision ["left", "right"]]
getBoard "left" = [Treasure, Trap, Goal]
getBoard "right" = [Obstacle, Trap, Goal]
getBoard _ = []

-- Task 4
movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    st <- get
    let tiles = getBoard (path st)
        maxMove = min roll (energy st)
        newPos = min (pos st + maxMove) (length tiles - 1)
        moved = newPos - pos st
    modify (\s -> s { pos = newPos, energy = energy s - moved })
    return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift $ getPlayerChoice options
    modify (\s -> s { path = choice, pos = 0 })
    return choice


-- Task 5
handleLocation :: AdventureGame Bool
handleLocation = do
    st <- get
    let currentTile = getBoard (path st) !! pos st
    case currentTile of
        Goal -> lift (putStrLn "Reached the main goal :))") >> return True
        Obstacle -> lift (putStrLn "Encountered an obstacle :( -5 energy. ") >> modify (\s -> s { energy = max 0 (energy s - 5) }) >> return False
        Treasure -> lift (putStrLn "Reached a treasure :) + 10 pts.") >> modify (\s -> s { points = points s + 10 }) >> return False
        Trap -> lift (putStrLn "Encountered a trap :( -10 pts. ") >> modify (\s -> s { points = max 0 (points s - 10) }) >> return False
        Decision opts -> lift (putStrLn "Choose your path. ") >> makeDecision opts >> return False
        Normal -> lift (putStrLn "Literally nothing here.") >> return False

playTurn :: AdventureGame Bool
playTurn = do
    st <- get
    if energy st <= 0
        then lift (putStrLn "You ran out of energy...") >> return True
        else do
            roll <- lift getDiceRoll
            _ <- movePlayer roll
            st' <- get
            lift $ displayGameState st'
            isWin <- handleLocation
            st'' <- get
            return (isWin || energy st'' <= 0)

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    unless ended playGame


-- Task 6
getDiceRoll :: IO Int
getDiceRoll = do
    putStr "Enter dice roll (1-6): "
    hFlush stdout
    input <- getLine
    case reads input of
        [(val, "")] | val >= 1 && val <= 6 -> return val
        _ -> putStrLn "Invalid number." >> getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = putStrLn $
    "\n Path: " ++ path st ++ " (Tile " ++ show (pos st) ++
    ") | Energy: " ++ show (energy st) ++ " | Points: " ++ show (points st)

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn $ "Options: " ++ show options
    putStr "Enter choice: "
    hFlush stdout
    choice <- getLine
    if choice `elem` options
        then return choice
        else putStrLn "Invalid selection." >> getPlayerChoice options

main :: IO ()
main = do
    let startState = GameState { path = "start", pos = 0, energy = 20, points = 0 }
    displayGameState startState
    _ <- runStateT playGame startState
    putStrLn "Game over."
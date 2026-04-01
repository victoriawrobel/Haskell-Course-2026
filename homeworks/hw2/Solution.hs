module Solution where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving Show

-- Task 1
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append seq1 seq2) = Append (fmap f seq1) (fmap f seq2)


-- Task 2
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append seq1 seq2) = foldMap f seq1 <> foldMap f seq2

seqToList :: Sequence a -> [a]
seqToList Empty = []
seqToList (Single x) = [x]
seqToList (Append seq1 seq2) = seqToList seq1 ++ seqToList seq2

seqLength :: Sequence a -> Int
seqLength Empty = 0
seqLength (Single _) = 1
seqLength (Append seq1 seq2) = seqLength seq1 + seqLength seq2


-- Task 3
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    Empty <> Empty = Empty
    Empty <> seq = seq
    seq <> Empty = seq
    seq1 <> seq2 = Append seq1 seq2


instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty


-- Task 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go [seq]
    where
    go [] = False
    go (Empty:stack) = go stack
    go (Single x:stack)
        | x == target = True
        | otherwise = go stack
    go (Append seq1 seq2:stack) = go (seq1:seq2:stack)


-- Task 5
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
    where
    go [] list = reverse list
    go (Empty:stack) list = go stack list
    go (Single x:stack) list = go stack (x:list)
    go (Append seq1 seq2:stack) list = go (seq1 : seq2 : stack) list


-- Task 6
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
    where
    go [] [result] = Just result
    go [] _ = Nothing
    go (TNum n : ts) stack = go ts (n : stack)
    go (t : ts) stack = case (t, stack) of
        (TAdd, x:y:rest) -> go ts ((y + x) : rest)
        (TSub, x:y:rest) -> go ts ((y - x) : rest)
        (TMul, x:y:rest) -> go ts ((y * x) : rest)
        (TDiv, x:y:rest) -> if x /= 0 then go ts ((y `div` x) : rest) else Nothing
        _ -> Nothing


-- Task 7
myReverse :: [a] -> [a]
myReverse = foldl (\reversed x -> x : reversed) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile predicate = foldr (\x seq -> if predicate x then x : seq else []) []

decimal :: [Int] -> Int
decimal = foldl (\num digit -> num * 10 + digit) 0


-- Task 8
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr go []
    where
    go x [] = [(x, 1)]
    go x ((y, count):rest)
        | x == y = (y, count + 1) : rest
        | otherwise = (x, 1) : (y, count) : rest

decode :: [(a, Int)] -> [a]
decode = foldr go []
    where
    go (x, count) res = replicate count x ++ res
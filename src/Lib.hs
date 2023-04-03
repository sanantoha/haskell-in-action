module Lib where

import Data.List (group, intersperse)
import qualified Data.Map as M    


someFunc :: IO ()
someFunc = putStrLn "someFunc"


length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs


sumFold :: (Num a) => [a] -> a
sumFold = foldl (+) 0


last' :: [a] -> a
last' [] = error "empty list"
last' [a] = a
last' (_:xs) = last' xs


lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [_] = error "list has one element"
lastButOne [x, _] = x
lastButOne (_:xs) = lastButOne xs


tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs


init' :: [a] -> [a]
init' [] = error "empty list"
init' [_] = []
init' (x:xs) = x : init' xs


emptyChars :: String -> Int
emptyChars [] = 0
emptyChars (' ':xs) = 1 + emptyChars xs
emptyChars (_:xs) = emptyChars xs


stringReverse :: String -> String
stringReverse = foldl (\acc c -> c:acc) []


stringReverse' :: String -> String
stringReverse' s = rev s ""
        where rev :: String -> String -> String
              rev [] target = target
              rev (x:xs) target = rev xs (x:target)



maxElem :: (Ord a) => [a] -> Maybe a
maxElem [] = Nothing
maxElem [x] = Just x
maxElem (x:xs) = max <$> Just x <*> maxElem xs


maxElemFold :: (Ord a) => [a] -> Maybe a
maxElemFold [] = Nothing
maxElemFold (x:xs) = Just (foldl max x xs)


averageFold :: [Double] -> Double
averageFold = fst . avgFold
        where avgFold = foldl helper (0, 0)
              helper (res, idx) d = do
                    let nidx = idx + 1
                        avg = res * idx

                    ((avg + d) / nidx, nidx)


product' :: [Int] -> Int
product' = foldl (*) 1


mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x acc -> f(x) : acc) []


filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr helper []
      where helper x acc = if f x then
                              x : acc
                           else
                              acc


flatMapFold :: (a -> [b]) -> [a] -> [b]
flatMapFold f = foldr (\x acc -> f x ++ acc) []


scanLeft :: (b -> a -> b) -> b -> [a] -> [b]
scanLeft _ ini [] = [ini]
scanLeft f ini (x:xs) = ini : scanLeft f (f ini x) xs


scanRight :: (a -> b -> b) -> b -> [a] -> [b]
scanRight _ ini [] = [ini]
scanRight f ini (x:xs) = f x y : ys
      where ys@(y:_) = scanRight f ini xs


compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)


digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum x = do
      let m = x `mod` 10
          b = x `div` 10

      m + digitSum b


digitSum' :: (Integral a) => a -> a
digitSum' = ds 0 
      where ds res n
                  | b == 0 && m == 0 = res
                  | otherwise = ds (m + res) b
                        where (b, m) = n `quotRem` 10


reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []


reverseList' :: [a] -> [a]
reverseList' = helper []
      where helper acc [] = acc
            helper acc (x:xs) = helper (x:acc) xs


charOccurs :: String -> M.Map Char Int
charOccurs = M.fromListWith (+) . fmap (\xs -> (head xs, length xs)) . group


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (less xs) ++ [x] ++ quickSort (greater xs)
      where less = filter (\y -> y <= x) 
            greater = filter (\y -> y > x)



mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = do
      let l = length xs
          n = l `div` 2
      if n == 0 then 
            xs
      else  do 
            let (left, right) = splitAt n xs
            merge (mergeSort left) (mergeSort right)
      where merge [] right = right
            merge left [] = left
            merge l@(x:xss) r@(y:yss) = if x <= y 
                                          then x : merge xss r
                                          else y : merge l yss


mergeSortTailRec :: (Ord a) => [a] -> [a]
mergeSortTailRec xs = do
      let n = length xs `div` 2

      if n == 0 then xs
      else do
            let (l, r) = splitAt n xs
            merge [] (mergeSortTailRec l) (mergeSortTailRec r)

      where merge res [] right = (reverse res) ++ right
            merge res left [] = (reverse res) ++ left
            merge res l@(x:xss) r@(y:yss) = if x <= y then merge (x:res) xss r
                                          else merge (y:res) l yss



insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
      where insert z [] = [z]
            insert k (y:ys) = if k <= y then 
                                    k : insert y ys
                              else
                                    y : insert k ys



insertSortTailRec :: (Ord a) => [a] -> [a]
insertSortTailRec = foldr (\x acc -> insert [] x acc) []
      where insert res x [] = reverse (x:res)
            insert res x (y:ys) = if x <= y then
                                    insert (x:res) y ys
                                  else 
                                    insert (y:res) x ys  



insertSortFold :: (Ord a) => [a] -> [a]
insertSortFold = foldl helper []
      where helper acc x = do
                  let (first, rest) = span (<=x) acc
                  first ++ x : rest
            


setHead :: a -> [a] -> [a]
setHead _ [] = error "setHead on empty list"
setHead x (_:xs) = x:xs


arrayMultiplication :: [Int] -> [Int]
arrayMultiplication [] = []
arrayMultiplication xs = helper xs
      where helper ys = do 
                  let lst = reverse . snd . foldl leftMulti (1, []) $ ys
                  snd . foldr rightMulti (1, []) . zip ys $ lst
            leftMulti (prev, acc) x = (prev * x, prev : acc)
            rightMulti (x, sr) (prev, acc) = (prev * x, prev * sr : acc)


removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n + 1) xs


squareListMap :: (Num a) => [a] -> [a]
squareListMap = fmap (\x -> x * x)


pack :: (Ord a) => [a] -> [[a]]
pack [] = []
pack l@(x:_) = do
      let (first, rest) = span ( == x) l
      first : pack rest


packTailRec :: (Ord a) => [a] -> [[a]]
packTailRec = helper []
      where helper res [] = reverse res
            helper res l@(x:_) = do
                  let (first, rest) = span ( == x) l
                  helper (first:res) rest



encode :: (Ord a) => [a] -> [(a, Int)]
encode [] = []
encode l@(x:_) = do
      let (first, rest) = span (==x) l
      (x, length first) : encode rest




encodeTailRec :: (Ord a) => [a] -> [(a, Int)]
encodeTailRec = helper []
      where helper res [] = reverse res
            helper res l@(x:_) = do
                  let (first, rest) = span (x==) l
                  helper ((x, length first) : res) rest



foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ ini [] = ini
foldLeft f ini (x:xs) = foldLeft f (f ini x) xs


foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ ini [] = ini
foldRight f ini (x:xs) = f x . foldRight f ini $ xs


unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f ini = helper (f ini)
      where helper Nothing = []
            helper (Just (x, y)) = x : helper (f y)


countChanges :: Int -> [Int] -> Int
countChanges money xs | money < 0 = 0
                      | money == 0 = 1
                      | null xs = 0
                      | otherwise = countChanges (money - head xs) xs + countChanges money (tail xs)


balanceParenthess :: String -> Bool
balanceParenthess = helper 0
      where helper :: Integer -> String -> Bool
            helper cnt [] = cnt == 0
            helper cnt (x:xs) | cnt < 0 = False
                              | x == '(' = helper (cnt + 1) xs
                              | x == ')' = helper (cnt - 1) xs
                              | otherwise = helper cnt xs


factorial :: Integer -> Integer
factorial = helper 1
      where helper res x | x < 0 = error "negative input value"
                         | x == 0 = res
                         | otherwise = helper (x * res) (x - 1)


fibonacci :: Integer -> Integer
fibonacci = helper 0 1
      where helper x y n | n <= 0 = x
                         | otherwise = helper y (x + y) (n - 1)


drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n l@(_:xs) | n <= 0 = l
                 | otherwise = drop' (n - 1) xs


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f l@(x:xs) = if f x 
                        then dropWhile' f xs
                        else l


take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
      | n <= 0 = []
      | otherwise = x : take' (n - 1) xs


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else []


lengthFold :: Foldable t => t a -> Int
lengthFold = foldl (\acc _ -> 1 + acc) 0


listReverseFold :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
listReverseFold = foldl (\acc x -> mappend (pure x) acc) mempty


foldLeftInTermFoldRight :: (b -> a -> b) -> b -> [a] -> b
foldLeftInTermFoldRight f ini xs = (foldr helper id xs) ini
      where helper x g y = g . f y $ x


appendList :: [a] -> [a] -> [a]
appendList = flip $ foldr (:)


concatListOfList :: [[a]] -> [a]
concatListOfList = foldr (++) []


mapTailRec :: (a -> b) -> [a] -> [b]
mapTailRec f = helper []
      where helper res [] = reverse res
            helper res (x:xs) = helper (f x : res) xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs


filterTailRec :: (a -> Bool) -> [a] -> [a]
filterTailRec f = helper []
      where helper res [] = reverse res
            helper res (x:xs) | f x = helper (x:res) xs
                              | otherwise = helper res xs


flatMap :: (a -> [b]) -> [a] -> [b]
flatMap _ [] = []
flatMap f (x:xs) = f x ++ flatMap f xs


flatMap' :: (a -> [b]) -> [a] -> [b]
flatMap' f = foldr (\x acc -> f x ++ acc) []


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


zipWithTailRec :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithTailRec f = helper [] 
      where helper res [] _ = reverse res
            helper res _ [] = reverse res
            helper res (x:xs) (y:ys) = helper ((f x y) : res) xs ys


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xys) = let (xs, ys) = unzip' xys
                      in ((x:xs), (y:ys))



zipTailRec :: [a] -> [b] -> [(a, b)]
zipTailRec = helper []
      where helper res [] _ = reverse res
            helper res _ [] = reverse res
            helper res (x:xs) (y:ys) = helper ((x, y) : res) xs ys


isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted f (x:y:xys) = f x y && isSorted f (y:xys)


hasSubsequence :: (Eq a) => [a] -> [a] -> Bool
hasSubsequence xs ys = any anyF rng
      where rng = [0 .. length xs]
            drp i = drop i xs
            anyF x = do
                  let xss = drp x
                  if length xss < length ys then False
                  else all (\(i, j) -> i == j) $ zip xss ys



buyMinSellMax :: [Int] -> Int
buyMinSellMax [] = 0
buyMinSellMax xs = helper 0 (head xs) (tail xs)
      where helper res _ [] = res
            helper res minVal (y:ys) = do
                  let maxRes = max res (y - minVal)
                      newMinVal = min y minVal
                  helper maxRes newMinVal ys



intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [y] = [y]
intersperse' x (y:ys) = [y, x] ++ intersperse' x ys


intercalate2 :: [a] -> [[a]] -> [a]
intercalate2 xs xss = concat (intersperse xs xss)


sumFirstN :: (Foldable f, Monoid a) => Int -> f a -> a
sumFirstN n fa = exec n
      where exec = foldr helper (\_ -> mempty) fa
            helper x f y = if y <= 0 then mempty
                           else x <> f (y - 1)


sumFirstN' :: (Foldable f, Monoid a) => Int -> f a -> a
sumFirstN' = flip $ foldr helper mempty
      where helper :: Monoid a => a -> (Int -> a) -> Int -> a
            helper x f n | n <= 0 = mempty
                         | otherwise = x <> f (n - 1)

-- maxSeqOfIntegersLessThanOne
-- maxSeqOfIntegersLessThanOne'
-- sumFirstN







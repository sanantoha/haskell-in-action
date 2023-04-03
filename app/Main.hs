module Main (main) where

import Lib
import Data.Monoid


main :: IO ()
main = do    
    -- print $ length' ['a', 'b', 'c']
    -- print $ length' ([1, 2, 3, 4] :: [Int])
    -- print $ sumFold [1,2,3,4,5]
    -- print $ last' ([1,2,3] :: [Int])

    -- print $ lastButOne ([1,2,3] :: [Int])

    -- print $ tail' ([1,2,3,4] :: [Int])

    -- print $ init' ([1,2,3,4] :: [Int])

    -- print $ emptyChars "hello world how are you doing"

    -- print $ stringReverse "hello"
    -- print $ stringReverse' "hello"

    -- print $ maxElem ([1,2,3,4,5,6] :: [Int])

    -- print $ maxElemFold ([10,2,6,4,5,6] :: [Int])

    -- print $ averageFold ([1, 2, 3, 4] :: [Double])

    -- print $ product' ([1,2,3,4] :: [Int])

    -- print $ mapFold (\x -> "This is " <> show x <> " !") ([1,2,3,4] :: [Int])

    -- print $ filterFold (\x -> x `mod` 2 == 0) ([1,2,3,4,5] :: [Int])


    -- print $ flatMapFold (\x -> [x, x]) ([1,2,3,4] :: [Int])

    -- print $ scanLeft (\s x -> s <> show x) "!" ([1,2,3,4,5] :: [Int])

    -- print $ scanl (\s x -> s <> show x) "!" ([1,2,3,4,5] :: [Int])

    -- print $ scanRight (\x s -> show x <> s) "!" ([1,2,3,4,5] :: [Int])

    -- print $ scanr (\x s -> show x <> s) "!" ([1,2,3,4,5] :: [Int])

    -- let f = (\x -> "This is: " <> show x)

    -- let g = (\x -> x + 1)

    -- let f' = compose f g

    -- print $ f' 1


    -- print $ digitSum (100000123 :: Int)

    -- print $ digitSum' (1000000123 :: Int)

    -- print $ reverseList ([1,2,3,4,5,6] :: [Int])

    -- print $ reverseList' ([1,2,3,4,5,6] :: [Int])

    -- print $ charOccurs "hello world"

    -- print $ quickSort ([9,4,7,8,0,2,3,6,7,4] :: [Int])

    -- print $ mergeSort ([9,4,7,8,0,2,3,6,7,4] :: [Int])
    -- putStr "mergeSortTailRec="
    -- print $ mergeSortTailRec ([9,4,7,8,0,2,3,6,7,4] :: [Int])


    -- print $ insertSort ([9,1,8,2,3,7,5,6,4] :: [Int])
    -- print $ insertSortTailRec ([9,1,8,2,3,7,5,6,4] :: [Int])
    -- print $ insertSortFold ([9,9,9,1,8,2,3,7,5,6,4] :: [Int])


    -- print $ setHead 0 ([1,2,3,4] :: [Integer])

    -- print $ arrayMultiplication ([1,2,3,4] :: [Int])


    -- print $ removeAt 3 ([1,2,3,4,5,6,7] :: [Int])

    -- print $ squareListMap ([1,2,3] :: [Int])

    -- print $ pack ['a', 'a', 'a', 'b', 'b', 'c', 'c', 'c']

    -- print $ packTailRec ['a', 'a', 'a', 'b', 'b', 'c', 'c', 'c']

    -- print $ encode  ['a', 'a', 'a', 'b', 'b', 'c', 'c', 'c']
    -- print $ encodeTailRec  ['a', 'a', 'a', 'b', 'b', 'c', 'c', 'c']

    -- print . balanceParenthess $ "()() ()"

    -- print . balanceParenthess $ ")()"

    -- print $ take' 30 ([1,2,3,4,5,6] :: [Int])
    -- print $ take 30 ([1,2,3,4,5,6] :: [Int])
    -- print $ take' 0 ([1,2,3,4,5,6] :: [Int])
    -- print $ take 0 ([1,2,3,4,5,6] :: [Int])

    -- print $ lengthFold ([1,2,3,4,5,6] :: [Int])

    -- print $ listReverseFold ([1,2,3,4,5,6] :: [Int])

    -- print $ foldLeftInTermFoldRight (\acc x -> acc ++ show x) "!" ([1,2,3,4,5] :: [Int])
    -- print $ foldl (\acc x -> acc ++ show x) "!" ([1,2,3,4,5] :: [Int])

    -- print $ mapTailRec (*1000) ([1,2,3,4] :: [Int])

    -- print $ flatMap (\x -> [x, x]) ([1,2,3,4,5,6,7,8] :: [Int])
    -- print $ flatMap' (\x -> [x, x]) ([1,2,3,4,5,6,7,8] :: [Int])
    -- print $ zipWith' (\x y -> (x, y)) [1,2,3,4,5,6] ['a', 'b', 'c', 'd']
    -- print $ zipWithTailRec (\x y -> (x, y)) [1,2,3,4,5,6] ['a', 'b', 'c', 'd']

    -- print $ unzip' [('a', 1), ('b', 2), ('c', 3)]

    -- print $ hasSubsequence ([1,2,3,4,5,6,7,8] :: [Int]) ([2,3,4] :: [Int])
    -- print $ hasSubsequence ([1,2,3,4,5,6,7,8] :: [Int]) ([2,3,5] :: [Int])


    -- print $ buyMinSellMax ([8,7,6,5,4,4,1,2,3,4,5,6,7,6,5,4,3,2,1] :: [Int])


    -- print $ intersperse' 0 ([1,2,3,4,5,6,7] :: [Int])


    print $ getSum $ sumFirstN 5 (Sum <$> ([1,1..] :: [Int]))

    print $ getSum $ sumFirstN' 10 (Sum <$> ([1,1..] :: [Int]))

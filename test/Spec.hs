import Test.Hspec
import Test.QuickCheck
import Control.Exception
import Safe.Foldable
import Data.AEq ((~==))
import Data.List (unfoldr, sort, intersperse, intercalate)
import qualified Data.Map as M

import Lib

main :: IO ()
main = hspec spec



spec :: Spec
spec = do

    describe "length'" $ do
        it "should elements count" $ property $
            \xs -> length' xs `shouldBe` (toInteger . length $ (xs :: [Int]))


    describe "foldSum" $ do
        it "should sum input list" $ property $
            \xs -> sumFold xs `shouldBe` sum (xs :: [Integer])


    describe "last'" $ do
        it "should return last value" $ property $
            \xs -> (not . null $ xs) ==> last' xs `shouldBe` last (xs :: [Int])
        it "empty list should throw exception" $ do
            evaluate (last' []) `shouldThrow` errorCall "empty list"


    describe "lastButOne" $ do
        it "should return penultimate value" $ property $
            \xs -> (length xs > 1) ==> lastButOne xs `shouldBe` (last . init $ (xs :: [Int]))
        it "empty list" $ do
            evaluate (lastButOne []) `shouldThrow` errorCall "empty list"
        it "one element" $ property $
            \x -> evaluate (lastButOne [x :: Int]) `shouldThrow` errorCall "list has one element"


    describe "tail'" $ do
        it "should return all elements of the list except first one" $ property $
            \xs -> (not . null $ xs) ==>  tail' xs `shouldBe` tail (xs :: [Int])
        it "should throw exception on empty list" $ do
            evaluate (tail' []) `shouldThrow` errorCall "empty list"


    describe "init'" $ do
        it "should return all elements of the list except last one" $ property $
            \xs -> (not . null $ xs) ==> init' xs `shouldBe` init (xs :: [Int])
        it "should thrwo exception on empty list" $ do
            evaluate (init' []) `shouldThrow` errorCall "empty list"


    describe "emptyChars" $ do
        it "should return number of empty chars" $ do
            let str = "hello world where is my car dude?"
            emptyChars str === 6


    describe "stringReverse" $ do
        it "should reverse string" $ property $ 
            \s -> stringReverse s `shouldBe` reverse s


    describe "stringReverse'" $ do
        it "should reverse string" $ property $ 
            \s -> stringReverse' s `shouldBe` reverse s


    describe "maxElem" $ do
        it "should return Nothing for empty list" $ do
            maxElem [] `shouldBe` (Nothing :: Maybe Int)
        it "should return max element from list" $ property $
            \xs -> (not . null $ xs) ==> maxElem xs `shouldBe` maximumMay (xs :: [Int])


    describe "maxElemFold" $ do
        it "should return Nothing for empty list" $ do
            maxElemFold [] `shouldBe` (Nothing :: Maybe Int)
        it "should return max element from list" $ property $
            \xs -> (not . null $ xs) ==> maxElemFold xs `shouldBe` maximumMay (xs :: [Int])


    describe "averageFold" $ do
        it "should return average value" $ property $
            \xs -> (not . null $ xs) ==> averageFold xs ~== (sum xs / (fromIntegral . length $ (xs :: [Double])))        


    describe "product'" $ do
        it "should return list of product" $ property $
            \xs -> product' xs === product (xs :: [Int])


    describe "mapFold" $ do
        it "should be the same as fmap" $ property $
            \xs -> do
                let f x = x * 100
                mapFold f xs `shouldBe` fmap f (xs :: [Int])

    
    describe "filterFold" $ do
        it "should be the same as filter" $ property $
            \xs x -> filterFold (< x) xs `shouldBe` filter (< (x :: Int)) (xs :: [Int])                

    
    describe "flatMapFold" $ do
        it "should be the same as >>=" $ property $
            \xs -> do
                let f x = [x,x]
                flatMapFold f xs `shouldBe` ((xs :: [Int]) >>= f)


    describe "scanLeft" $ do
        it "should be the same as scanl" $ property $
            \xs -> do
                let f acc x = show x ++ acc
                scanLeft f "!" xs `shouldBe` scanl f "!" (xs :: [Int])


    describe "scanRight" $ do
        it "should fold right the list and keep temporary result in list" $ property $
            \xs -> do
                let f x acc = show x ++ acc
                scanRight f "!" xs `shouldBe` scanr f "!" (xs :: [Int])


    describe "compose" $ do
        it "should be the same as dot function" $ property $
            \x y z -> do
                let f k = k + y
                    g k = k - z
                (compose g f) x `shouldBe` ((x :: Int) + (y :: Int) - (z :: Int))


    describe "digitSum" $ do
        it "should sum all digits in number" $ do
            digitSum 10000001 `shouldBe` (2 :: Int)
        it "should sum all digits in number 100111001 == 5" $ do
            digitSum 100111001 `shouldBe` (5 :: Int)
        it "should sum all digits in number 123 == 6" $ do
            digitSum 123 `shouldBe` (6 :: Int)
        

    describe "digitSum'" $ do
        it "should sum all digits in number" $ do
            digitSum' 10000001 `shouldBe` (2 :: Int)
        it "should sum all digits in number 100111001 == 5" $ do
            digitSum' 100111001 `shouldBe` (5 :: Int)        
        it "should sum all digits in number 123 == 6" $ do
            digitSum' 123 `shouldBe` (6 :: Int)


    describe "reverseList" $ do
        it "should be the same as revert" $ property $
            \xs -> reverseList xs `shouldBe` reverse (xs :: [Int])


    describe "reverseList'" $ do
        it "should be the same as revert" $ property $
            \xs -> reverseList' xs `shouldBe` reverse (xs :: [Int])



    describe "charOccurs" $ do
        it "should count chars in word" $ do
            charOccurs "hello" `shouldBe` M.fromList [('h', 1), ('e', 1), ('l', 2), ('o', 1)]


    describe "quickSort" $ do
        it "should sort list" $ property $
            \xs -> quickSort xs `shouldBe` sort (xs :: [Int])


    describe "mergeSort" $ do
        it "should sort list" $ property $
            \xs -> mergeSort xs `shouldBe` sort (xs :: [Int])

    
    describe "mergeSortTailRec" $ do
        it "should sort list" $ property $
            \xs -> mergeSortTailRec xs `shouldBe` sort (xs :: [Int])


    describe "insertSort" $ do
        it "should sort list" $ property $
            \xs -> insertSort xs `shouldBe` sort (xs :: [Int])


    describe "insertSortTailRec" $ do
        it "should sort list" $ property $
            \xs -> insertSortTailRec xs `shouldBe` sort (xs :: [Int])

    
    describe "insertSortFold" $ do
        it "should sort list" $ property $
            \xs -> insertSortFold xs `shouldBe` sort (xs :: [Int])

    describe "setHead" $ do
        it "should change head" $ property $
            \xs x -> (not . null $ xs) ==> setHead x xs `shouldBe` ((x :: Int) : tail (xs :: [Int]))


    describe "arrayMultiplication" $ do
        it "should multiply all elements except current" $ do
            arrayMultiplication [1,2,3,4] `shouldBe` [24,12,8,6]
    -- --


    describe "encode" $ do
        it "should return char with count" $ do
            encode ['a', 'a', 'a', 'b', 'b', 'c'] `shouldBe` [('a', 3), ('b', 2), ('c', 1)]
        it "should return empty list" $ do
            encode [] `shouldBe` ([] :: [(Int, Int)])


    describe "encodeTailRec" $ do
        it "should return char with count" $ do
            encodeTailRec ['a', 'a', 'a', 'b', 'b', 'c'] `shouldBe` [('a', 3), ('b', 2), ('c', 1)]
        it "should return empty list" $ do
            encodeTailRec [] `shouldBe` ([] :: [(Int, Int)])


    describe "foldLeft" $ do
        it "should fold seq" $ property $            
            \xs -> do
                let f acc x = show x ++ acc
                foldLeft f "!" xs `shouldBe` foldl f "!" (xs :: [Int])


    describe "foldRight" $ do
        it "should fold seq from right" $ property $
            \xs -> do
                let f x acc = show x ++ acc
                foldRight f "!" xs `shouldBe` foldr f "!" (xs :: [Int])


    describe "unfoldr'" $ do
        it "should unfold right" $ property $
            \x y -> do
                let f z = if z < y 
                                then Just (z, z + 1)
                                else Nothing
                unfoldr' f x `shouldBe` unfoldr f (x :: Int)


    describe "countChange" $ do
        it "money = 0 and [1,2] should be 1" $ do
            countChanges 0 [1,2] === 1
        it "money = 0 and [] should be 1" $ do
            countChanges 0 [] === 1
        it "money = 4 and [] should be 0" $ do
            countChanges 4 [] === 0
        it "should count changes" $ do
            countChanges 3 [1,2] === 2


    describe "balanceParenthess" $ do
        it "`()()()j()` should be True" $ do
            balanceParenthess "()()()j()" `shouldBe` True
        it "empty string should be True" $ do
            balanceParenthess "" `shouldBe` True
        it "unbalanced string should be False" $
            balanceParenthess ")()" `shouldBe` False
        it "(hello world) should be True" $ do
            balanceParenthess "(hello world)" `shouldBe` True
        
    
    describe "factorial" $ do
        it "-3 should throw error" $ do
            evaluate (factorial (-3)) `shouldThrow` errorCall "negative input value"
        it "0! should be 1" $ do
            factorial 0 `shouldBe` 1
        it "5! should be 120" $ do
            factorial 5 `shouldBe` 120


    describe "fibonacci" $ do
        it "0 should return 0" $ do
            fibonacci 0 `shouldBe` 0
        it "1 should return 1" $ do
            fibonacci 1 `shouldBe` 1
        it "2 should return 1" $ do
            fibonacci 2 `shouldBe` 1
        it "3 should return 2" $ do
            fibonacci 3 `shouldBe` 2
        it "4 should return 3" $ do
            fibonacci 4 `shouldBe` 3
        it "5 should return 5" $ do
            fibonacci 5 `shouldBe` 5
        it "6 should return 8" $ do
            fibonacci 6 `shouldBe` 8
        it "7 should return 13" $ do
            fibonacci 7 `shouldBe` 13
        it "8 should return 21" $ do
            fibonacci 8 `shouldBe` 21

    
    describe "drop'" $ do
        it "should drop elements from beginning until n equal 0" $ property $
            \n xs -> drop' n xs `shouldBe` drop (n :: Int) (xs :: [Int])

    
    describe "dropWhile'" $ do
        it "should drop elements from beginning if element satisfy given function" $ property $
            \xs -> do                
                let f x | x <= 0 = False
                        | otherwise = True
                dropWhile' f xs `shouldBe` dropWhile f (xs :: [Int])


    describe "take'" $ do
        it "should take n elements from list" $ property $
            \n xs -> (n > 0) ==> take' n xs `shouldBe` take (n :: Int) (xs :: [Int])


    describe "takeWhile'" $ do
        it "should take element until sitisfy condition by given function" $ property $
            \xs -> do
                let f x | x < 5 = True
                        | otherwise = False
                takeWhile' f xs `shouldBe` takeWhile f (xs :: [Int])

    
    describe "lengthFold" $ do
        it "should return length of container" $ property $
            \xs -> lengthFold xs `shouldBe` length (xs :: [Int])


    describe "listReverseFold" $ do
        it "should reverse list" $ property $
            \xs -> listReverseFold xs `shouldBe` listReverseFold (xs :: [Int])
                    
    
    describe "foldLeftInTermFoldRight" $ do
        it "should be the same as foldl" $ property $
            \xs -> do
                let f acc x = acc <> show x
                foldLeftInTermFoldRight f "!" xs `shouldBe` foldl f "!" (xs :: [Int])

    
    describe "appendList" $ do
        it "should be append one list to another" $ do
            appendList [1,2,3,4,5] [6,7,8,9,10] `shouldBe` ([1,2,3,4,5,6,7,8,9,10] :: [Int])

    
    describe "concatListOfList" $ do
        it "should concat all list in one" $ do
            concatListOfList [[1,2,3], [4,5,6], [7,8,9, 10]] `shouldBe` ([1,2,3,4,5,6,7,8,9,10] :: [Int])


    describe "mapTailRec" $ do
        it "should be the same as fmap" $ property $
            \xs -> do
                let f x = "This is " <> show x
                mapTailRec f xs `shouldBe` fmap f (xs :: [Int])


    describe "filter'" $ do
        it "should be the same as filter" $ property $
            \xs -> do
                let f x = x `mod` 2 == 0
                filter' f xs `shouldBe` filter f (xs :: [Int])


    describe "filterTailRec" $ do
        it "should be the same as filter" $ property $
            \xs -> do
                let f x = x `mod` 2 == 0
                filterTailRec f xs `shouldBe` filter f (xs :: [Int])


    describe "flatMap" $ do
        it "should be the same as >>=" $ property $
            \xs -> do
                let f x = [x, x, x]
                flatMap f xs `shouldBe` ((xs :: [Int]) >>= f)


    describe "flatMap'" $ do
        it "should be the same as >>=" $ property $
            \xs -> do
                let f x = [x - 1, x, x  + 2]
                flatMap' f xs `shouldBe` ((xs :: [Int]) >>= f)


    describe "zipWith'" $ do
        it "should be the same as zipWith" $ property $
            \xs ys -> do
                let f x y = (x, y)
                zipWith' f xs ys `shouldBe` zipWith f (xs :: [Int]) (ys :: [Char])


    describe "zipWithTailRec" $ do
        it "should be the same as zipWith" $ property $
            \xs ys -> do
                let f x y = (x, y)
                zipWithTailRec f xs ys `shouldBe` zipWith f (xs :: [Int]) (ys :: [Char])

    
    describe "zip'" $ do
        it "should be the same as zip" $ property $
            \xs ys -> zip' xs ys `shouldBe` zip (xs :: [Int]) (ys :: [Char])


    describe "unzip'" $ do
        it "should be the same as unzip" $ property $
            \xs -> unzip xs `shouldBe` unzip (xs :: [(Char, Int)])

    
    describe "zipTailRec" $ do
        it "should be the same as zip" $ property $
            \xs ys -> zipTailRec xs ys `shouldBe` zip (xs :: [Int]) (ys :: [Char])

    
    describe "isSorted" $ do
        it "should be True if list is sorted" $
            isSorted (<) ([1,2,3,4,5,6,7,8,9] :: [Int])
        it "sould be False if not sorted" $ 
            not (isSorted (<) ([1,1,1,1,3,4,5,6,7] :: [Int]))
        it "property based test" $ property $
            \xs -> isSorted (<=) $ sort (xs :: [Int])


    describe "hasSubsequence" $ do
        it "should return True if second list is subsequence of first one" $ do
            hasSubsequence ([1,2,3,4,5,6,7] :: [Int]) ([3,4,5] :: [Int])
        it "should hasSubsequence [3,2,1] [1,2] return False" $ do
            not . hasSubsequence ([3,2,1] :: [Int]) $ ([1,2] :: [Int])
        it "should hasSubsequence [] [1,2] return False" $ do
            not . hasSubsequence ([] :: [Int]) $ ([1,2] :: [Int])


    describe "buyMinSellMax" $ do
        it "buyMinSellMax [1,2,3,4,5,6]" $ do
            buyMinSellMax [1,2,3,4,5,6] `shouldBe` 5
        it "buyMinSellMax []" $ do
            buyMinSellMax [] `shouldBe` 0
        it "buyMinSellMax [5,4,3,2,1]" $ do
            buyMinSellMax [5,4,3,2,1] `shouldBe` 0

    
    describe "intersperse'" $ do
        it "should be the same as intersperse" $ property $
            \x xs -> intersperse' x xs `shouldBe` intersperse (x :: Int) (xs :: [Int])

    
    describe "intercalate2" $ do
        it "should be the same as intercalate" $ property $
            \xs xss -> intercalate2 xs xss `shouldBe` intercalate (xs :: [Int]) (xss :: [[Int]])

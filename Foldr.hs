module Foldr where

import Test.HUnit (Test (TestList), runTestTT, (~:), (~=?), (~?=))
import Prelude hiding (all, filter, foldl, foldl1, last, length, map, reverse)

length1 :: [a] -> Int
length1 [] = 0
length1 (_ : xs) = 1 + length1 xs

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

testLength :: Test
testLength =
  "length"
    ~: TestList
      [ length "abcd" ~?= 4,
        length "" ~?= 0
      ]

all1 :: (a -> Bool) -> [a] -> Bool
all1 _ [] = True
all1 p (x : xs) = p x && all1 p xs

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x xs -> p x && xs) True

testAll :: Test
testAll =
  "all"
    ~: TestList
      [ all (> 10) ([1 .. 20] :: [Int]) ~?= False,
        all (> 0) ([1 .. 20] :: [Int]) ~?= True
      ]

last1 :: [a] -> Maybe a
last1 [] = Nothing
last1 (x : xs) = case xs of
  [] -> Just x
  _ -> last1 xs

last :: [a] -> Maybe a
last =
  foldr
    ( \x xs -> case xs of
        Nothing -> Just x
        _ -> xs
    )
    Nothing

testLast :: Test
testLast =
  "last"
    ~: TestList
      [ last "abcd" ~?= Just 'd',
        last "" ~?= Nothing
      ]

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x : xs else xs) []

testFilter :: Test
testFilter =
  "filter"
    ~: TestList
      [ filter (> 10) [1 .. 20] ~?= ([11 .. 20] :: [Int]),
        filter (\l -> sum l <= 42) [[10, 20], [50, 50], [1 .. 5]] ~?= ([[10, 20], [1 .. 5]] :: [[Int]])
      ]

reverse1 :: [a] -> [a]
reverse1 l = aux l []
  where
    aux [] = id
    aux (x : xs) = \ys -> aux xs (x : ys)

reverse :: [a] -> [a]
reverse l = foldr (\x f acc -> f (x : acc)) id l []

testReverse :: Test
testReverse =
  "reverse"
    ~: TestList
      [ reverse "abcd" ~?= "dcba",
        reverse "" ~?= ""
      ]

-- >>> intersperse ',' "abcde"
-- "a,b,c,d,e"

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 a (x : xs) = case xs of
  [] -> [x]
  _ -> x : a : intersperse1 a xs

intersperse :: a -> [a] -> [a]
intersperse a = foldr (\x acc -> if length acc == 0 then x : acc else x : a : acc) []

testIntersperse :: Test
testIntersperse =
  "intersperse"
    ~: TestList
      [ "intersperse0" ~: intersperse ',' "abcde" ~=? "a,b,c,d,e",
        "intersperse1" ~: intersperse ',' "" ~=? ""
      ]

foldl1 :: (b -> a -> b) -> b -> [a] -> b
foldl1 _ z [] = z
foldl1 f z (x : xs) = foldl1 f (z `f` x) xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = foldr (\x g a -> g (f a x)) id xs z

testFoldl :: Test
testFoldl = foldl (++) "x" ["1", "2", "3"] ~=? "x123"

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [ testLength,
          testAll,
          testLast,
          testFilter,
          testReverse,
          testIntersperse,
          testFoldl
        ]
  return ()

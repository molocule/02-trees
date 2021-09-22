module Kata20 where

import Data.Char as Char ()
import Data.List as List (transpose)
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (TestList), runTestTT, (@?=), (~:))
import qualified Text.Read as Read

readInt :: String -> Maybe Int
readInt = Read.readMaybe

-----------------------------------
-- SAMPLE A --

getMinSpread2 :: (Int, Int, Int) -> Int -> String -> String
getMinSpread2 (nI, maxI, minI) d p =
  gms (drop d $ lines p) ("genericName", maxBound :: Int)
  where
    gms (x : xs) t@(n, ms) =
      let w = words x
       in if length w > maximum [nI, maxI, minI]
            then case (readInt (w !! maxI), readInt (w !! minI)) of
              (Just max, Just min) ->
                let currSpread = abs (max - min)
                 in if currSpread < ms
                      then gms xs (w !! nI, currSpread)
                      else gms xs t
              (_, _) -> gms xs t
            else gms xs t
    gms _ (n, _) = n

weather2_A :: String -> String
weather2_A = getMinSpread2 (0, 1, 2) 18

soccer2_A :: String -> String
soccer2_A = getMinSpread2 (1, 6, 8) 1

-----------------------------------

-----------------------------------
-- SAMPLE B --

-- | find the element with smallest difference
smallestSpread :: [[String]] -> String
smallestSpread ((x0 : x1 : x2 : []) : (y0 : y1 : y2 : []) : ys) =
  if compareSpread (readInt x1) (readInt x2) (readInt y1) (readInt y2)
    then smallestSpread ((x0 : x1 : x2 : []) : ys)
    else smallestSpread ((y0 : y1 : y2 : []) : ys)
smallestSpread [x] = head x

-- | compare difference between two pairs of values
compareSpread :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Bool
compareSpread (Just a) (Just b) (Just c) (Just d) = abs (a - b) < abs (c - d)
compareSpread _ _ _ _ = False

weather2_B :: String -> String
weather2_B str = smallestSpread2 $ map (take 3 . words) (take 31 $ drop 18 $ lines str)

soccer2_B :: String -> String
soccer2_B str =
  smallestSpread2 $ List.transpose ((table !! 1) : (table !! 6) : (table !! 8) : [])
  where
    table = List.transpose $ map (words) (take 20 $ drop 1 $ lines str)

-- | find the element with smallest difference
smallestSpread2 :: [[String]] -> String
smallestSpread2 ((x0 : x1 : x2 : []) : (y0 : y1 : y2 : []) : ys) =
  if compareSpread (readInt x1) (readInt x2) (readInt y1) (readInt y2)
    then smallestSpread ((x0 : x1 : x2 : []) : ys)
    else smallestSpread ((y0 : y1 : y2 : []) : ys)
smallestSpread2 [x] = head x

-- | compare difference between two pairs of values
compareSpread2 :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Bool
compareSpread2 (Just a) (Just b) (Just c) (Just d) = abs (a - b) < abs (c - d)
compareSpread2 _ _ _ _ = False

-----------------------------------

-------------------------------------------------
-- SAMPLE C --

readLine :: String -> (String, String)
readLine ('\n' : xs) = ("", xs)
readLine (x : xs) = let (rd, rest) = readLine xs in (x : rd, rest)
readLine "" = ("", "")

jumpLine :: String -> String
jumpLine = snd . readLine

numsParse :: String -> Int -> Int -> (Int, Int)
numsParse line i j =
  ( Maybe.fromJust . readInt $ [line !! i, line !! (i + 1)],
    Maybe.fromJust . readInt $ [line !! j, line !! (j + 1)]
  )

nameParse2 :: String -> Int -> String
nameParse2 line addr = _nameParse2 0 "" line
  where
    _nameParse2 i name (c : cs) =
      if i < addr
        then _nameParse2 (i + 1) name cs
        else
          ( if (c == ' ') && (name /= "")
              then reverse name
              else
                _nameParse2
                  (i + 1)
                  (if c /= ' ' then c : name else name)
                  cs
          )
    _nameParse2 _ _ "" = ""

getMin ::
  String ->
  Int ->
  Int ->
  Int ->
  Int ->
  (String -> Bool) ->
  (String -> Bool) ->
  String
getMin text 0 nameAddr data1Addr data2Addr skipPred endPred =
  getMinHelp "" (-1) text
  where
    getMinHelp prevName prevSpread str =
      if endPred str
        then prevName
        else
          if skipPred str
            then getMinHelp prevName prevSpread (jumpLine str)
            else
              let (line, rest) = readLine str
               in let (data1, data2) = numsParse line data1Addr data2Addr
                   in let name = nameParse2 line nameAddr
                       in let update = (prevSpread < 0 || abs (data2 - data1) < prevSpread)
                           in getMinHelp
                                (if update then name else prevName)
                                (if update then abs (data2 - data1) else prevSpread)
                                rest
getMin text i nameAddr data1Addr data2Addr skipPred endPred =
  getMin
    (jumpLine text)
    (i -1)
    nameAddr
    data1Addr
    data2Addr
    skipPred
    endPred

weather2_C :: String -> String
weather2_C str =
  getMin
    str
    18
    0
    4
    8
    (const False)
    (\s -> s /= [] && (s !! 0) == '=')

soccer2_C :: String -> String
soccer2_C str = getMin str 1 7 43 50 (\str -> (str !! 3) == '-') null

-------------------------------------------

-----------------------------------
-- SAMPLE D --

-- Apply function f to the value x, y, z if they exist.
maybeMap :: (t1 -> t2 -> t3 -> a) -> t1 -> Maybe t2 -> Maybe t3 -> Maybe a
maybeMap f x (Just y) (Just z) = Just $ f x y z
maybeMap _ _ _ _ = Nothing

-- Compute the difference between num1 and num2 and return with its name.
getDataDiff :: Maybe (String, String, String) -> Maybe (Int, String)
getDataDiff (Just (name, num1, num2)) =
  maybeMap (\x y z -> (abs (y - z), x)) name (readInt num1) (readInt num2)
getDataDiff _ = Nothing

-- Extract weather data from input String.
getWeatherData :: [String] -> Maybe (String, String, String)
getWeatherData (date : max : min : _) = Just (date, max, min)
getWeatherData _ = Nothing

-- Extract soccer data from input String.
getSoccerData :: [String] -> Maybe (String, String, String)
getSoccerData (num : team : p : w : l : d : f : dash : a : pts : _) =
  Just (team, f, a)
getSoccerData _ = Nothing

weather2_D :: String -> String
weather2_D =
  snd . minimum
    . Maybe.mapMaybe (getDataDiff . getWeatherData . words)
    . drop 18
    . lines

soccer2_D :: String -> String
soccer2_D =
  snd . minimum
    . Maybe.mapMaybe (getDataDiff . getSoccerData . words)
    . drop 1
    . lines

-----------------------------------

testWeather :: (String -> String) -> Test
testWeather weather =
  "weather" ~: do
    str <- readFile "jul20.dat"
    weather str @?= "10"

testSoccer :: (String -> String) -> Test
testSoccer soccer =
  "soccer" ~: do
    str <- readFile "soccer.dat"
    soccer str @?= "Aston_Villa"

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ "A" ~: testWeather weather2_A,
          "B" ~: testWeather weather2_B,
          "C" ~: testWeather weather2_C,
          "D" ~: testWeather weather2_D,
          "A" ~: testSoccer soccer2_A,
          "B" ~: testSoccer soccer2_B,
          "C" ~: testSoccer soccer2_C,
          "D" ~: testSoccer soccer2_D
        ]
  return ()

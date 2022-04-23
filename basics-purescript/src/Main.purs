module Main where

import Prelude

import Data.String.CodePoints (length)
import Data.String.Common (toLower, toUpper)
import Effect (Effect)
import Effect.Console (log)

add :: Int -> Int -> Int
add x y = x + y

isSmall :: String -> Boolean
isSmall s = length s < 10

isOddLenght :: String -> Boolean
isOddLenght s = length s `mod` 2 /= 0

myToString :: Int -> String
myToString = show

myToArray :: String -> Array String
myToArray s = [s]

intToStringArray :: Int -> Array String
intToStringArray = myToArray <<< myToString 

appendIf :: (String -> Boolean) -> String -> String -> String
appendIf pred s append = if pred s then s <> append else s
  

upperLower :: Int -> (String -> String)
upperLower n = if isEven n then toUpper else toLower

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

negate :: Int -> Int
negate num = (-1) * num

show3 :: Int -> (Int -> (Int -> String))
show3 x y z = show x <> "," <> show y <> "," <> show z 

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z


main :: Effect Unit
main = do
  log "🍝"
  log $ appendIf isSmall "Hello World" "!!!"
  log $ appendIf isOddLenght "Hello World" "!!!"
  log $ upperLower 2 "Hello World"
  log $ myToString 2
  log $ show3 23 54 26
module PatternMatching where

import Prelude

import Data.Maybe (Maybe(..))
import Prim.RowList (Nil)

-- If it's not Nothing return False, so we use _ as a dont-care variable.
isNothing :: ∀ a. Maybe a -> Boolean
isNothing m = case m of
    Nothing -> true
    _       -> false

----------------------- String Patterns ----------------------- 
toString :: Boolean -> String
toString true   = "true"
toString _      = "false"

fromString :: String -> Boolean
fromString "true"           = true
fromString _                = false

----------------------- Array Patterns -----------------------

isEmpty :: ∀ a. Array a -> Boolean
isEmpty []  = true
isEmpty _   = false

multiplyTwo :: Array Int -> Maybe Int
multiplyTwo [x, y]  = Just (x * y)
multiplyTwo _       = Nothing

-- Array vs List
addThree :: Array Int -> Maybe Int
addThree [x, y, z]  = Just (x + y + z)
addThree _          = Nothing

data List a = Cons a (List a) | Nil
infixr 6  Cons as :

sum :: List Int -> Int
sum Nil = 0
sum (x : xs) = x + sum xs

-- Record Patterns
type Address = 
    { street :: String
    , city :: String
    , state :: String
    , zip :: String
    }

type Employee =
    { name :: String
    , jobTitle :: String
    , yearsAtCompany :: Int
    , address :: Address
    }

type Company =
    { name :: String
    , yearsInBusiness :: Int
    , address :: Address
    }

isCalifornia :: Company -> Boolean
isCalifornia { address : { state } } = state == "CA"
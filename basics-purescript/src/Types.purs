module Types where

import Data.String.CodeUnits
import Prelude

import Data.Array.NonEmpty (head)
import Data.Array.ST (splice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Class.Console (error)
import Prim.Boolean (True)
import Prim.Row (class Cons)
import Prim.RowList (Nil)


----------------------- Boolean -----------------------
t :: Boolean
t = true

f :: Boolean
f = false

----------------------- Char -----------------------
c :: Char
c = 'c'

unicodeCh :: Char
unicodeCh = '\x00E9'

----------------------- String -----------------------
s :: String
s = "stringa"

s2 :: String
s2 = "Test stringa che\
      \ continua senza "


----------------------- Number -----------------------
n :: Number
n = 1.0

smallestNumber :: Number
smallestNumber = (-5e-324)

largestNumber :: Number
largestNumber = 1.7676931348623157e+308

----------------------- Int -----------------------
i :: Int
i = 42

i2 :: Int
i2 = 3 + 4

smallestInt :: Int
smallestInt = (-2147483648) -- -2^31

largestInt :: Int
largestInt = 2147483647 -- 2^31 - 1

----------------------- Array -----------------------
emptyArray :: Array Int
emptyArray = []

a :: Array Int
a = [1,2,4]

a2 :: Array String
a2 = ["prima", "seconda", "terza"]

aa :: Array (Array Int)
aa = [ [12,45,6], [3,6,7], [87,6] ]

----------------------- Record -----------------------
r :: { firstName :: String, lastName :: String }
r = { firstName: "Mastro", lastName: "Andrea" }

type Person = 
    {   name :: String
    ,   age :: Int
    }

r2 :: Person
r2 = { name: "Mastro Adrea", age: 33 }

type Nested = 
    {   val :: Int
    ,   rec :: 
            { val2 :: Int
            , name :: String
            }
    }


----------------------- Type Alias -----------------------
type Id = String

type Message = { id :: Id, payload :: String }

----------------------- Data Type -----------------------
data MyType = MyType

----------------------- Sum Type | Unions -----------------------
-- Unions because a union in Set Theory is an OR operation
data Bool = True | False

-- Other ↓
    -- implied Type ↓
        -- Other :: String -> FailureReason
    -- Other is a Function, other Data Constructors for FailureReason are values.
-- InvalidSyntax :: FailureReason
-- InvalidInput :: FailureReason
-- AlreadyExists :: FailureReason
-- NotFound :: FailureReason

-- FailureReason is Monomorphic
data FailureReason
    = InvalidSyntax
    | InvalidInput
    | AlreadyExists
    | NotFound
    | Other String

-- Now FailureReason can take anything, also a String
-- tht's why is Polymorphic 
data FailureReason' a
    = InvalidSyntax'
    | InvalidInput'
    | AlreadyExists'
    | NotFound'
    | Other' a

----------------------- Product Type -----------------------
data Triplet a b c = Triplet a b c

type StringStats = Triplet String Int Int

add :: Int -> Int -> Int
add x y = x + y

useAdd :: Maybe Int -> Maybe Int
useAdd y' = 
    let x :: Int
        x = 10
    in
    case y' of
        Just y -> Just (x + y)
        Nothing -> Nothing

myHush :: ∀ a b. Either a b -> Maybe b
myHush (Left _) = Nothing
myHush (Right x) = Just x

myNote :: ∀ a b. a -> Maybe b -> Either a b
myNote err Nothing = Left err
myNote _ (Just x)  = Right x

----------------------- Tuple -----------------------
-- Tuple is a Product Type
data Tuple a b = Tuple a b

-- use case: coordinates --> it can be an alias
type Point = Tuple Int Int

-- use case: when you want to return back two values from Function:
-- splitPosAndNeg :: Array Int -> Tuple (Array Int) (Array Int)

-- Either vs Tuple
    -- Either is a Sum Type
        -- Either: = a + b
    -- Tuple is a Product Type
        -- Tuple: = a * b

-- Either and Tuple are Isomorphic when a and b are equal 2 or 0
    -- a + b = a * b
    -- 0 + 0 = 0 * 0 (a, b = 0)
    -- 2 + 2 = 2 * 2 (a, b = 2)

-- Tuple Void Void ≅ Either Void Void
-- Tuple Boolean Boolean ≅ Either Boolean Boolean

----------------------- List -----------------------
data List a = Cons a (List a) | Nil -- Cons put a  single Value onto the head of a List
infixr 6  Cons as : -- The : is an infix operator for the Cons Data Constructor
                    -- The : is righ (r) associative infix(r)

nums :: List Int
nums = 1 : 2 : 3 : Nil

-- right associative can be written as ↓
    -- nums :: List Int
    -- nums = (1 : (2 : (3 : Nil)))
-- with Cons
    -- nums :: List Int
    -- nums = Cons 1 (Cons 2 (Cons 3 Nil))

head :: ∀ a. List a -> Maybe a
head Nil        = Nothing
head (x : _)    = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil        = Nothing
tail (_ : xs)   = Just xs
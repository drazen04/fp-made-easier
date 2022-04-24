module LogicalControl where

import Prelude

----------------------- if-then-else expression -----------------------
keepPositive :: Int -> Int
keepPositive x = if x < 0 then 0 else x

----------------------- case expression -----------------------
data ContactMethod
    = Phone
    | Email
    | Fax

keepModern :: ContactMethod -> ContactMethod
keepModern prefContactMethod =
    case prefContactMethod of
        Phone   -> Phone
        Email   -> Email
        Fax     -> Email

keepPositive' :: Int -> Int
keepPositive' x = case x < 0 of
    true    -> 0
    false   -> x

----------------------- Guards -----------------------
-- A more concise if logic
keepModernIfYoung :: ContactMethod -> Int -> ContactMethod
keepModernIfYoung prefContactMethod age =
    case prefContactMethod of
        Phone -> Phone
        Email -> Email
        Fax | age < 40  -> Fax
            | otherwise -> Email

data List a = Cons a (List a) | Nil
infixr 6  Cons as :

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile p (x : xs) | p x = x : takeWhile p xs
takeWhile _ _ = Nil
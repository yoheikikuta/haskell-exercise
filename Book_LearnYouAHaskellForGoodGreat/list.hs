infixr 5 :-:
infixr 5 ^++
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
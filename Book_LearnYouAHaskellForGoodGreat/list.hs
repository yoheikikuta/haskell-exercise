infixr 5 :-:
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

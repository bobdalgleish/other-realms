module Browser.Predicate where

data Predicate a = Test (a -> Bool)
                 | AllOf [Predicate a]
                 | AnyOf [Predicate a]
                 | NoneOf [Predicate a]
                 | Success
                 | Fail

testPredicate :: (a -> Bool) -> Predicate a
testPredicate predicate = Test predicate

-- |Apply a predicate, return True if success
applyPredicate :: Predicate a -> a -> Bool
applyPredicate Success  _     = True
applyPredicate Fail     _     = False
applyPredicate (Test f) log   = f log
applyPredicate (AllOf l) log  = all (\f -> applyPredicate f log) l
applyPredicate (AnyOf l) log  = any (\f -> applyPredicate f log) l
applyPredicate (NoneOf l) log = not $ any (\f -> applyPredicate f log) l

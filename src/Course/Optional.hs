{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b)
  -> Optional a
  -> Optional b
mapOptional _ Empty = Empty -- if a is empty, then no need to turn it into b, just return empty
mapOptional function (Full a) = Full (function a) -- run the function over a
  {-mapOptional function optional =
  case optional of
    Empty -> Empty
    Full a -> Full (function a)
-}

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
--
-- a doesn't have to be an integer, it can be a function
-- >>> (Empty ?? (*2)) 99
-- 198
(??) ::
  Optional a
  -> a
  -> a
(??) Empty p1 = p1  
(??) (Full p1) _ = p1

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- if (x != null)
--   return x
-- else
--   return y
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >> Empty <+> Full 9
-- Full 9
--
-- >> Full 8 <+> Full 9 <+> Full 10
-- Full 8
--
-- >> Full 8 <+> Full 9 <+> Full 10 <+> Full 11
-- Full 8
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a
  -> Optional a
  -> Optional a
--(<+>) Empty (Full p2) = (Full p2)  
--(<+>) Empty Empty = Empty 
-- condense because we only care about the 2nd parameter, the 1st is empty
(<+>) Empty p2 = p2  
--(<+>) (Full p1) Empty = (Full p1) 
--(<+>) (Full p1) (Full p2) = (Full p1)
-- condense because we don't care about the 2nd parameter, the 1st has the value we want
(<+>) (Full p1) _ = (Full p1)
-- Can remove code repetition by assigning a value to the expression
--(<+>) q@(Full _) _ = q
--(<+>) q@(Full{}) _ = q

-- BUT WHY? Because it's not built into the language! 

-- bindOptional ::
--   (a -> Optional b)
--   -> Optional a
--   -> Optional b
-- applyOptional :: 
--   Optional (a -> b) 
--   -> Optional a 
--   -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: 
  (a -> b -> c) 
  -> Optional a 
  -> Optional b 
  -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: 
  Eq a => a 
  -> Optional a 
  -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full

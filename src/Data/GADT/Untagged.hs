{-# LANGUAGE ExistentialQuantification, Rank2Types, PolyKinds #-}

module Data.GADT.Untagged where

-- | Existential type, representing GADT, abstracted from typelevel tag (first type parameter).
data Untagged con = forall a. Tagged (con a)

-- | Function to untag values.
--
-- > f :: [Term A] -> [Term B] -> [Untagged Term]
-- > f xs ys = map untag xs ++ map untag ys
untag :: con a -> Untagged con
untag = Tagged

-- | Processes untagged value by unpacking it from existential wrapper and feeding result to rank2-typed funarg. 
--
-- >  f :: Untagged Term -> Integer
-- >  f term = match term $ \case
-- >    Var ... -> ...
-- >    Lam ... -> ...
match :: Untagged con -> (forall a. con a -> r) -> r
match (Tagged x) f = f x

-- | Existential type, representing GADT, abstracted from two typelevel tags (first two type parameters).
data Untagged2 con = forall a b. Tagged2 (con a b)

untag2 :: con a b -> Untagged2 con
untag2 = Tagged2

match2 :: Untagged2 con -> (forall a b. con a b -> r) -> r
match2 (Tagged2 x) f = f x

-- | Existential type, representing GADT, abstracted from three typelevel tags (first three type parameters).
data Untagged3 con = forall a b c. Tagged3 (con a b c)

untag3 :: con a b c -> Untagged3 con
untag3 = Tagged3

match3 :: Untagged3 con -> (forall a b c. con a b c -> r) -> r
match3 (Tagged3 x) f = f x
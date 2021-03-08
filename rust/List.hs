{--
  Goal of this exercise is to show most common typeclasses and their usages.
  Requires base >= 4.11
--}



data List a 
  = MkEmpty
  | MkCons a (List a)

instance Semigroup (List a) where
  -- :: List a -> List a -> List a
  MkEmpty <> ys = ys
  (MkCons x xs) <> ys = MkCons x (xs `mappend` ys)

instance Monoid (List a) where
  mempty = MkEmpty
    
instance Functor List where
  -- :: (a -> b) -> List a -> List b
  fmap _ MkEmpty = MkEmpty
  fmap fn (MkCons x xs) = fn x `MkCons` fmap fn xs

instance Applicative List where
  -- :: a -> List a
  pure x = MkCons x MkEmpty
  -- :: f (a -> b) -> f a -> f b
  fs <*> xs = joinL $ fmap (\f -> fmap f xs) fs

instance Monad List where
  --
  return = pure
  --
  xs >>= fn = foldr joinL $ fmap fn xs
  
instance Foldable List where
  -- :: (a -> b -> b) -> b -> t a -> b
  foldr _  init MkEmpty = init
  foldr fn init (MkCons x xs) = fn x $ foldr fn init xs

instance Traversable List where
  -- :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse _  MkEmpty = pure MkEmpty
  traverse fn (MkCons x xs) = pure MkCons <*> fn x <*> traverse fn xs 

instance (Show a) => Show (List a) where
  show = ("list"++) . show . toLs

joinL :: List (List a) -> List a
joinL = foldr (<>) mempty

{--
--}
toLs :: List a -> [a]
toLs = foldr (:) []

fromLs :: [a] -> List a
fromLs = foldr MkCons MkEmpty 

main = do
  print $ fromLs [1,2,3]
  print $ toLs $ fromLs [1,2,3]
  print $ traverse (\x -> Just x) $ fromLs [1,2,3]
  print $ fromLs [(+1), (+2)] <*> fromLs [10, 20]
  print $ do
    x <- fromLs [1, 2]
    y <- fromLs "abc"
    return (x, y) 


class Cat k where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

instance Cat (->) where
  id x = x
  f . g = \x -> f (g x)



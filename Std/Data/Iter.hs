
newtype Iter a = Iter (forall acc. (acc -> a -> Int -> m (IPair a)) -> a -> m (IPair a))



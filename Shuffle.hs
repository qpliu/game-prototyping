module Shuffle(shuffle,shuffleIO) where

import System.Random(RandomGen,getStdGen,randomR)

shuffleIO :: [a] -> IO [a]
shuffleIO list = getStdGen >>= return . snd . flip shuffle list

shuffle :: (RandomGen g) => g -> [a] -> (g,[a])
shuffle gen list = iterate reshuffle1 (gen,list) !! len
  where
    len = length list

    reshuffle1 (gen,list) =
      let (index,gen') = randomR (1,len) gen
      in  (gen',shuffle1 list index)

    shuffle1 list index = uncurry weave $ splitAt index list

    weave (a:as) (b:bs) = b:a:weave as bs
    weave [] bs = bs
    weave as [] = as

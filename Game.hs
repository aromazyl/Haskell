module Game where
  import Control.Monad
  import System.Random

  data Item = Scissor | Stock | Paper deriving (Show, Eq)

  class (Eq a) => Comp a where
    (<<<) :: a -> a -> Bool
    (>>>) :: a -> a -> Bool
    (>>>) a b = not (a <<< b)
    
  instance Comp Item where
    Scissor <<< Stock = True
    Scissor <<< Paper = False
    Paper <<< Stock = True
    a <<< b = not $ b <<< a

  map2Item :: Int -> Item
  map2Item num = case (num `mod` 3) of
                    0 -> Scissor
                    1 -> Stock
                    2 -> Paper

  game = 
    forever $ do
      gen <- newStdGen
      let x = map map2Item $ take 2 (randomRs (0, 99) gen)
      if ((x!!0) /= (x!!1)) then 
        do
          print x
          putStrLn . show $ (x !! 0) <<< (x !! 1)
          game
      else
          return ()

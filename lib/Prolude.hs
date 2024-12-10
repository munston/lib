{-#Language MultiParamTypeClasses,AllowAmbiguousTypes,RankNTypes,ScopedTypeVariables,TypeApplications#-}
module Lib.Prolude where
import Data.List

type V = [Double]

type S = (Double,V)

type State s i o = s -> i -> (s,o)

type Vec = V

type Sample = S

(+++) :: Vec -> Vec -> Vec
(+++) = zipWith (+)

zeroVector n = replicate n 0 

type Loss = Vec -> Double

{-
data Sample = Sample (Double,Vec) deriving Show

instance Eq Sample where
 (==) (Sample (x,_)) (Sample (y,_)) = x == y

instance Ord Sample where
 (<) (Sample (x,_)) (Sample (y,_)) = x < y
 (>) (Sample (x,_)) (Sample (y,_)) = x > y
 (>=) (Sample (x,_)) (Sample (y,_)) = x >= y
 (<=) (Sample (x,_)) (Sample (y,_)) = x <= y
-}
feedback :: Int -> State s a a -> State s a [a]
feedback 1 sf s a = let (s' ,b) = sf s a in (s',[b])
feedback n sf s a = let (s' ,b) = sf s a 
                        (s'',c) = feedback (n-1) sf s' b 
                     in (s'',b:c)

feedbackLast :: Int -> State s a a -> State s a a
feedbackLast 1 sf s a = let (s' ,b) = sf s a in (s',b)
feedbackLast n sf s a = let (s' ,b) = sf s a 
                            (s'',c) = feedbackLast (n-1) sf s' b 
                         in (s'',c)
                    
scanner :: State s a b -> State s [a] [b]                 
scanner sf s    []  = (s,[])
scanner sf s (x:xs) = (s,y:ys)
 where
  (s' ,y ) = sf s x
  (s'',ys) = scanner sf s' xs
  
scannerLast :: State s a b -> State s [a] b
scannerLast sf s [] = error "scannerLast given empty list"
scannerLast sf s [x   ] = sf s x 
scannerLast sf s (x:xs) = (s,o)
 where
  (s' ,y) = sf s x
  (s'',o) = scannerLast sf s' xs
  
consume :: State s a b -> s -> [a] -> b
consume sf s xs = snd $ scannerLast sf s xs

--
  
bestOf :: [(Double,a)] -> (Double,a)
bestOf [] = error "bestOf given empty list"
bestOf [x] = x
bestOf (x:xs) = consume f x xs
 where
  f :: State (Double,a) (Double,a) (Double,a)
  f acc@(minVal, _) cur@(curVal, _) 
   | curVal < minVal = (cur,cur)
   | otherwise       = (acc,acc)

bestOfAverage :: [(Double,a)] -> (Double,a)
bestOfAverage xs = let (e1,(e2,a)) = bestOfAverage' xs in (e2{-e1/(fromIntegral (length xs))-},a)

bestOfAverage' :: [(Double,a)] -> (Double,(Double,a))
bestOfAverage' [] = error "bestOfAverage given empty list"
bestOfAverage' [x] = (fst x,x)
bestOfAverage' (x:xs) = consume f (0,x) xs
 where
  f :: State (Double,(Double,a)) (Double,a) (Double,(Double,a))
  f (carry, acc@(minVal,x)) cur@(curVal, y) 
   | curVal < minVal = let x = (carry+curVal,cur) in (x,x)
   | otherwise       = let x = (carry+curVal,acc) in (x,x)

bestOfMovingAverage :: Double -> [(Double,a)] -> (Double,a)
bestOfMovingAverage n xs = let (e1,(e2,a)) = bestOfMovingAverage' n xs in (e1+e2,a)

bestOfMovingAverage' :: Double -> [(Double,a)] -> (Double,(Double,a))
bestOfMovingAverage' n [] = error "bestOfAverage given empty list"
bestOfMovingAverage' n [x] = (fst x,x)
bestOfMovingAverage' n (x:xs) = consume f (0,x) xs
 where
  f :: State (Double,(Double,a)) (Double,a) (Double,(Double,a))
  f (carry, acc@(minVal,x)) cur@(curVal, y) 
   | curVal < minVal = let x = (carry+curVal * n,cur) in (x,x)
   | otherwise       = let x = (carry+curVal * n,acc) in (x,x)   

--

class Lens a b where
 view :: a -> b
 edit :: (b->b) -> a->a
 
inheritView :: forall a b c. (Lens a b,Lens b c) => a -> c
inheritView a = view @b @c (view @a @b a)

inheritEdit :: forall a b c.(Lens a b,Lens b c) => (c->c) -> a->a
inheritEdit f a = edit @a @b (edit @b @c f) a


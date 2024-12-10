{-# Language GADTs,UndecidableInstances,ApplicativeDo ,FlexibleInstances, DefaultSignatures , FunctionalDependencies , AllowAmbiguousTypes , RankNTypes,ScopedTypeVariables,TypeApplications#-}
module Lib.Parametric where
import Data.List
import Data.Foldable
import Data.List.Split


reparametrizer :: Traversable f => String -> [Double] -> f Double -> f Double
reparametrizer caller xs f | length xs /= length (toList f) = error $ caller ++ ". reparametrize (lengthProvided,lengthRequired) "++ (show (length xs , length (toList f))) -- ++ "\n"++(show xs)
                    | otherwise = ((snd .). mapAccumR (\(x:xs) _ -> (xs,x))) xs f

class Parametric p init | p -> init  where
 shape :: p -> init
 parameters :: p -> [Double]
 reparametrize :: [Double] -> p -> p
 blank :: init -> p
 parameterLength :: p -> Int
 
zero :: Parametric p init => init -> (Int,p)
zero init = (l,reparametrize xs a)
 where 
  l = parameterLength a
  a = blank init
  xs = replicate l 0
  
make :: Parametric p init => init -> (Int -> [Double]) -> p
make init f = reparametrize xs a
 where 
  l = parameterLength a
  a = blank init
  xs = f l
  
new :: Parametric p init => init -> [Double] -> p
new init xs = reparametrize xs $ blank init

lengthFromInit :: forall p init. Parametric p init => init -> Int
lengthFromInit = parameterLength . blank @p @init

instance Parametric a i => Parametric [a] (Int,i) where
 shape xs = (length xs,shape $ head xs)
 parameters = concatMap parameters
 reparametrize xs ys = zipWith reparametrize (chunksOf l xs) ys where l = parameterLength (head ys)
 blank (l,i) = replicate l (blank i)
 parameterLength xs = length xs * parameterLength (head xs)

instance (Parametric a i,Parametric b j) => Parametric (a,b) (i,j) where
 shape (a,b) = (shape a,shape b)
 parameters (a,b) = parameters a ++ parameters b
 reparametrize xs (a,b) = let (l1,l2) = (parameterLength a,parameterLength b) in 
                          let (x,y) = (take l1 xs,take l2 $ drop l1 xs) in (reparametrize x a,reparametrize y b)
 blank (i,j) = (blank i,blank j)
 parameterLength (a,b) = parameterLength a + parameterLength b	
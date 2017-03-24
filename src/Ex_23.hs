module Ex_23 where

import Graphics.Gnuplot.Simple
import Data.List
import Test.QuickCheck
import System.Random
import qualified Data.Vector as V

-- Trivial:

nDigitosFactTrivial :: Integer -> Integer
nDigitosFactTrivial n = (genericLength . show . product) [1..n]

-- Primera función:

nDigitosFact :: Integer -> Integer
nDigitosFact 0 = 1
nDigitosFact 1 = 1
nDigitosFact n = (ceiling . sum) (logBase 10.fromInteger <$> [1..n])

-- Vemos que perdemos tiempo al repetir el cálculo de logaritmos. Definimos una función, de orden linea,O(2n), que nos devuelve un vector(lo cual nos permitirá ahorrarnos tiempo de acceso) con el número de dígitos de los primeros n factoriales.

logBase10 :: [Double]
logBase10 = logBase 10.fromInteger <$> [1..]

digitosFact3 :: [Integer]
digitosFact3 = nDigitosFact <$> [1..]

digitosFact :: Int -> V.Vector Integer
digitosFact n = V.fromList $ take n (((1:) . tail . aux logBase10) 0)
            where
             aux (x:xs) s = (ceiling (s + x)) : aux xs (s+x)
            
digitosFact2 :: [Integer]
digitosFact2 = (tail.tail) (ceiling <$> scanl' (+) 0 logBase10)

-- Este función, aunque rápida, no cumple los tiempos pedidos
-- *Ex_23>  V.sum $ digitosFact (10^6)
-- 2674285603295
-- (2.85 secs, 1,109,908,312 bytes)

-- Utilizamos la fórmula de Kamenetsky 

nDigitosFactKamenetsky :: Integer -> Integer
nDigitosFactKamenetsky 0 = 1
nDigitosFactKamenetsky 1 = 1
nDigitosFactKamenetsky n = floor ((logBase 10 . sqrt . (2*) . (pi*)) x + ((x*) . logBase 10 . (x/) . exp) 1) + 1
                         where x = fromInteger n

prop_nDigitosFact :: Positive Integer -> Bool
prop_nDigitosFact (Positive n) = nDigitosFact n == nDigitosFactKamenetsky n

graficas     :: [Integer] -> IO ()
graficas xs = plotLists [Key Nothing] [nDigitosFactKamenetsky <$> xs]

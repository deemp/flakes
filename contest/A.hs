module Main(main, readInts, readString, toDoubles, toDouble, ceil, floor) where
import Data.Functor ((<&>))
import GHC.Float.RealFracMethods (ceilingDoubleInt, floorDoubleInt)
import Prelude hiding (floor)

readInts :: IO [Int]
readInts = getLine <&> words <&> fmap read

readString :: IO String
readString = getLine

toDoubles :: (Functor f, Integral a) => f a -> f Double
toDoubles xs = (\x -> fromIntegral x :: Double) <$> xs

toDouble :: (Integral a) => a -> Double
toDouble = fromIntegral

ceil :: Double -> Int
ceil = ceilingDoubleInt

floor :: Double -> Int
floor = floorDoubleInt

main :: IO ()
main = print "hello, world!"
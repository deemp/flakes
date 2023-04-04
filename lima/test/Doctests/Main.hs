import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "lima" =<< getArgs
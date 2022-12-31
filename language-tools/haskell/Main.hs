import System.Process
main = putStrLn =<< readProcess "gen-hie" ["--help"] ""
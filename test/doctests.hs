import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Prelude (IO, (=<<))

main :: IO ()
main = mainFromCabal "cabal-fix" =<< getArgs

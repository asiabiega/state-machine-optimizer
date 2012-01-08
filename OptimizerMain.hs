import System.Environment
import System.IO
import Control.Concurrent.MVar
import Control.Monad.State

import AST
import Optimizer
import FileUtils
import MainCommon

--Author: Aleksander Balicki
--Binary for optimization, ./smopt -a does summary, cat machinefile | ./smopt 10, does optimization with 10 second time limit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> case arg of
            "-a" -> do
                files <- listFiles "inputs/"
                contentlist <- mapM readFile files

                astlist <- mapM (oldNewAstWithSize 10 optimize) contentlist
                let (oldSizeSum, newSizeSum) = foldl (\(x,y) ((_,x2),(_,y2)) -> (x+x2, y+y2)) (0,0) astlist

                putStrLn $ "old size sum: " ++ show oldSizeSum
                putStrLn $ "new size sum: " ++ show newSizeSum
                putStrLn $ "sum delta: " ++ show (oldSizeSum - newSizeSum) ++ " (" ++
                     show (100.0 - (fromIntegral newSizeSum / fromIntegral oldSizeSum * 100.0) :: Double) ++ "% reduction)"

            _ -> do
                let time = read arg :: Int
                cont <- getContents
                ((_, oldSize), (newAst, newSize)) <- oldNewAstWithSize time optimize cont
    --          case ast of
    --              Nothing -> hPutStrLn stderr "timed out"
                putStrLn $ pp newAst
                putStrLn $ "old size: " ++ show oldSize --(takeMVar bestSolution)
                putStrLn $ "new size: " ++ show newSize
                putStrLn $ "delta:" ++ show (oldSize - newSize) ++ " (" ++
                    show (100.0 - (fromIntegral newSize / fromIntegral oldSize * 100.0) :: Double) ++ "% reduction)"
        _ -> putStrLn "Use -a for the summary run or give an Integer as an argument and pipe a machine into it"


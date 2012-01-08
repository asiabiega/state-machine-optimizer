import System.Environment
import System.IO
import Control.Concurrent.MVar
import Control.Monad.State

import AST
import Optimizer
import FileUtils
import MainCommon

main :: IO ()
main = do
    [arg] <- getArgs
    case arg of
        "-a" -> do
            files <- listFiles "inputs/"
            contentlist <- mapM readFile files

            astlist <- mapM (oldNewAstWithSize optimize) contentlist
            let (oldSizeSum, newSizeSum) = foldl (\(x,y) ((_,x2),(_,y2)) -> (x+x2, y+y2)) (0,0) astlist

            putStrLn $ "old size sum: " ++ show oldSizeSum
            putStrLn $ "new size sum: " ++ show newSizeSum
            putStrLn $ "sum delta: " ++ show (oldSizeSum - newSizeSum) ++ " (" ++
                 show (100.0 - (fromIntegral newSizeSum / fromIntegral oldSizeSum * 100.0) :: Double) ++ "% reduction)"

        _ -> do
--            let time = read arg
            cont <- getContents
            ((_, oldSize), (newAst, newSize)) <- oldNewAstWithSize optimize cont
--            bestSolution <- newMVar (msize oldAst, oldAst)
--            ast <- timeout (time*1000000) (return $ fst $ runState (optimize $ oldAst) state)
--            case ast of
--                Nothing -> hPutStrLn stderr "timed out"
            putStrLn $ pp newAst
            putStrLn $ "old size: " ++ show oldSize --(takeMVar bestSolution)
            putStrLn $ "new size: " ++ show newSize
            putStrLn $ "delta:" ++ show (oldSize - newSize) ++ " (" ++
                show (100.0 - (fromIntegral newSize / fromIntegral oldSize * 100.0) :: Double) ++ "% reduction)"


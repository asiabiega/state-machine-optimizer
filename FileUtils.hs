module FileUtils where

import System.FilePath.Posix
import System.Directory
import Data.List

-- function inspired by python's string.split().  A list is split
-- on a separator which is itself a list (not a single element).
split :: Eq a => [a] -> [a] -> [[a]]
split tok = unfoldr (sp1 tok)
    where sp1 _ [] = Nothing
          sp1 t s = case find (t `isSuffixOf`) $ inits s of
                      Nothing -> Just (s, [])
                      Just p -> Just (take (length p - length t) p,
                                      drop (length p) s)

-- | Lists all filenames in a given directory
listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    allfiles <- getDirectoryContents path
    let files = sort $ filter (\s -> last (split "/" s) `notElem` [".", ".."]) allfiles
    return $ map (path </>) files


module Main (main) where

import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), filterDir, readDirectoryWith)
import System.Environment
import System.Exit

import System.FilePath (takeExtension)

import ParseModule

-- | Loop recursivly through the given directory
main :: IO ()
main = do
  r <- getArgs >>= parse
  _:/tree <- readDirectoryWith return r
  traverse print $ filterDir myPred tree
  return ()
    where myPred (Dir ('.':_) _) = False
          myPred (File n _) = takeExtension n == ".hs"
          myPred _ = True

-- | Parse command line arguments
parse :: [String] -> IO (String)
parse [] = putStrLn "No root dir given" >> exitWith (ExitFailure 1)
parse [r] = return r
parse (a:as) = putStrLn "Too many arguments" >> exitWith (ExitFailure 1)

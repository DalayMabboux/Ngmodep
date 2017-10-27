module Main (main) where

import Data.Foldable (traverse_, toList)
import Data.Set as S
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), filterDir, readDirectoryWith)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

import System.FilePath (takeExtensions)

import ParseModule (parseModule)
import DrawGraph (merge, pushOut, morphToDot)

-- | Loop recursivly through the given directory
main :: IO ()
main = do
  r <- getArgs >>= parse
  _:/tree <- readDirectoryWith return r
  -- Parse every file (JS module)
  --i <- mapM parseModule (filterDir myPred tree)
  ---mapM_ (putStrLn . show) i
  let res = foldl' merge S.empty (filterDir myPred tree)
  pushOut $ morphToDot res
  -- Create a graph out of the [ImpExports]
  return ()
    where myPred (Dir ('.':_) _) = False
          myPred (File n _) = takeExtensions n == ".module.js"
          myPred _ = True

-- | Parse command line arguments
parse :: [String] -> IO (String)
parse [] = putStrLn "No root dir given" >> exitWith (ExitFailure 1)
parse [r] = return r
parse (a:as) = putStrLn "Too many arguments" >> exitWith (ExitFailure 1)


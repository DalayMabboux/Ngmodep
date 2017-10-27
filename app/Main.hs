module Main (main) where

import Data.Foldable (foldl')
import Data.Set as S
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), filterDir, readDirectoryWith)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

import System.FilePath (takeExtensions)

import ParseModule (parseModule)
import DrawGraph (merge, pushOut, morphToDot)

main :: IO ()
main = do
  r <- getArgs >>= parse
  _:/tree <- readDirectoryWith return r
  let ft = filterDir myPred tree
  res <- Data.Foldable.foldl' f (return (S.empty, S.empty)) ft
  pushOut $ morphToDot res
  return ()
    where myPred (Dir ('.':_) _) = False
          myPred (File n _) = takeExtensions n == ".module.js"
          myPred _ = True
          f :: IO (S.Set String, S.Set (String, String)) -> FilePath -> IO (S.Set String, S.Set (String, String))
          f g f = do
                    (vs, es) <- g
                    n <- parseModule f
                    return $ merge n (vs, es)

-- | Parse command line arguments
parse :: [String] -> IO (String)
parse [] = putStrLn "No root dir given" >> exitWith (ExitFailure 1)
parse [r] = return r
parse (a:as) = putStrLn "Too many arguments" >> exitWith (ExitFailure 1)


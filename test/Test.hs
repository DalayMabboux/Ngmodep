module Main where

import ParseModuleTest (mainParseModuleTest)
import DrawGraphTest (mainDrawGraphTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    parseTests <- mainParseModuleTest
    defaultMain $ testGroup "Main" [parseTests, mainDrawGraphTest]
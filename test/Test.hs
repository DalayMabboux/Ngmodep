module Main where

import ParseModuleTest (mainParseModuleTest)
import DrawGraphTest (mainDrawGraphTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    tt <- mainParseModuleTest
    defaultMain $ testGroup "Main" [tt, mainDrawGraphTest]
module Main (main) where

import Test.Tasty

import Test.EurekaPROM.IO.Input qualified as Input

main :: IO ()
main = defaultMain $ testGroup "test-eurekaprom-io" [
      Input.tests
    ]
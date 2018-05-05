{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Tasty
import           Property.Vector
import           Property.Text
import           Unit.LowResTimer

main :: IO ()
main = defaultMain $ testGroup "stdio tests" [
        propertyVector
    ,   propertyText
    ,   unitLowResTimer


    ]


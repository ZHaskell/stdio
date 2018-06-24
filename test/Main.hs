{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Tasty
import           Property.Vector
import           Property.Text
import           Unit.LowResTimer
import           Unit.Resource
import           Unit.TCP

main :: IO ()
main = defaultMain $ testGroup "stdio tests" [
        propertyVector
    ,   propertyText
    ,   unitLowResTimer
    ,   unitResource
    ,   unitTCP

    ]


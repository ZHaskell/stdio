{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Tasty
import           Property.Vector
import           Property.Text
import           Unit.FileSystem
import           Unit.LowResTimer
import           Unit.Resource

main :: IO ()
main = defaultMain $ testGroup "stdio tests" [
        propertyVector
    ,   propertyText
    ,   unitLowResTimer
    ,   unitResource
    ,   unitFileSystem
    ]


{-# LANGUAGE InstanceSigs #-}

module Game (Player (O, B, X), Grid, initialGame) where

import Constants (size)
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    arbitrary,
    elements,
  )

data Player = O | B | X
  deriving (Eq, Ord, Show)

instance Arbitrary Player where
  arbitrary :: Gen Player
  arbitrary = elements [O, B, X]

type Grid = [[Player]]

initialGame :: Grid
initialGame = replicate size (replicate size B)

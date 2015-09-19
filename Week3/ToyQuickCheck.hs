{-# OPTIONS_GHC -Wall #-}

module ToyQuickCheck where
import Test.QuickCheck

data Subject = Mathematics | Philosophy | Anthropology deriving (Eq, Show, Enum)
instance Arbitrary Subject where arbitrary = elements[Mathematics, Philosophy, Anthropology]

compatible :: Subject -> Subject -> Bool
compatible x y
    | x == y                  = True
    | Philosophy `elem` [x,y] = True
    | otherwise               = False

badCompatible :: Subject -> Subject -> Bool
badCompatible x y
    | x == y                  = True
    | x == Philosophy         = True
    | otherwise               = False

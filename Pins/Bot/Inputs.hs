{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Pins.Bot.Inputs where

data InputResult = InputString String
                 | InputInt Int
                 | InputFailed

data Input = ConstantString String

class Constantable a where
    constant :: a -> Input

instance Constantable String where
    constant = ConstantString

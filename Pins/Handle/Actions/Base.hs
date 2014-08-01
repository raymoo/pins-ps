module Pins.Handle.Actions.Base where

data Action = Send String
            | Login Int String
            | Print String
            | Arbitrary (IO ())

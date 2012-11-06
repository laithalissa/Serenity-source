
module Serenity.Game.Model.Common
( Location
, Direction
, Size
, Resources(..)
) where


type Location = (Float, Float)
type Direction = (Float, Float)
type Size = (Float, Float)

--data Resource = Metal { amount :: Float } | Fuel { amount :: Float } | AntiMatter { amount :: Float }
data Resources = Resources { fuel :: Float, antiMatter :: Float, metal :: Float } deriving(Show, Eq)

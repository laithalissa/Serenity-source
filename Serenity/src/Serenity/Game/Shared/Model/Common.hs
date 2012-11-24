
module Serenity.Game.Shared.Model.Common where

type EntityId = Int
type TimeDuration = Float -- milliseconds
type Location = (Float, Float)
type Path = [Location]
type Polygon = [Location]
type Direction = (Float, Float)
type Orientation = (Float, Float, Float, Float)
type Size = (Float, Float)

--data Resource = Metal { amount :: Float } | Fuel { amount :: Float } | AntiMatter { amount :: Float }
data Resources = Resources { fuel :: Float, antiMatter :: Float, metal :: Float } deriving(Show, Eq)


-- helper functions --
toList2 (a1, a2) = [a1, a2]
toList3 (a1, a2, a3) = [a1, a2, a3]
toList4 (a1, a2, a3, a4) = [a1, a2, a3, a4]

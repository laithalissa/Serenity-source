
module Serenity.Game.Model.Common where


type Location = (Float, Float)
type Path = [Location]
type Polygon = [Location]
type Direction = (Float, Float)
type Orientation = (Float, Float, Float, Float)
type Size = (Float, Float)

--data Resource = Metal { amount :: Float } | Fuel { amount :: Float } | AntiMatter { amount :: Float }
data Resources = Resources { fuel :: Float, antiMatter :: Float, metal :: Float } deriving(Show, Eq)


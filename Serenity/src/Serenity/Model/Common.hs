
module Serenity.Model.Common where

type EntityID = Int
type OwnerID = Int

type Location = (Double, Double)
type Direction = (Double, Double)
type Position = (Location, Direction)
type Speed = Double
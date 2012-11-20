
module Serenity.Game.Model.ShipClass where

import Graphics.Gloss.Data.Picture(Picture)
import Serenity.Game.Model.Common(Location, Polygon, Direction)


data WeaponSlotType = SideWeaponType | TurretWeaponType | SpecialWeaponType deriving(Show, Eq)

data ShipClass = 
  ShipClass 
  {         shipClassName :: String,
            collisionPolygon :: Polygon,
            image :: Picture,
            centreOfRotation :: Location,
            systemSlotLocations :: [(Location, Direction)],
            weaponSlotLocations :: [(Location, Direction, WeaponSlotType)] 
  } deriving(Show, Eq)

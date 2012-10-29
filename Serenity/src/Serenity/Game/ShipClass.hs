
module Serenity.Game.ShipClass where

import qualified Graphics.Gloss.Data.Picture as GlossPicture

import qualified Serenity.Game.ShipModel as ShipModel

type DirectionVector = (Float, Float)
type PositionVector = (Float, Float)
type Polygon = [PositionVector]

data ShipClass = ShipClass 
{ shipClassName :: String,
  collisionPolygon :: Polygon,
  image :: GlossPicture.Picture,
  centreOfRotation :: PositionVector,
  systemSlotLocations :: [(PositionVector, DirectionVector)],
  weaponSlotLocations :: [(PositionVector, 
                           DirectionVecotor, 
                           ShipMode.WeaponSlotType)] 
}





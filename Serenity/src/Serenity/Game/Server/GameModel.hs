
module Serenity.Game.Server.GameModel
(
) where




type PositionVector = (Int, Int)
type DirectionVector = (Int, Int)
type OrientationVector :: (PositionVector, DirectionVector)






data Planet = Planet {
      planetId :: Int,
      planetPosition :: PositionVector,
      fuelGenerated :: Int,
      antiMatterGenerated :: Int,
      metalGenerated :: Int
}

data JumpLane = JumpLane {
      jumpLaneId :: Int,
      jumpLane
}



data ShipClass = ShipClass {
      shipClassId :: Int,
      shipClassName :: String,
      shipClassBaseHull :: Int,
      shipClassBaseShield :: Int,
      shipClassWeaponSlots :: [WeaponSlot],
      shipClassSystemSlots :: [SystemSlot],
      shipClassShape :: ShipShape
}


data ShipShape = ShipShape = {
      polygon :: [(Int, Int)],
      centre :: (Int, Int)
}


data WeaponSlot = WeaponSlot {
      weaponSlotId :: Int,
      weaponSlotType :: WeaponSlotType,
      weaponSlotShipOrientation :: OrientaionVector,
      weaponSlotFireAngle :: Int
}

data SystemSlot = SystemSlot {
      systemSlotId :: Int,
      systemSlotOrientation :: OrientationVector
}


data WeaponSlotType = Side | Turret | Special






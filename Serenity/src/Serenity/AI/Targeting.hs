module Serenity.AI.Targeting where

import Serenity.Model.Entity
import Serenity.Model.Game

import Control.Lens
import Data.List (findIndex)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.VectorSpace

-- | Quadrants, relative to the ship, that a weapon can fire upon
data Quadrant = LeftQuad | FrontQuad | RightQuad | AllQuads deriving Show

type WeaponInfo = (Int, (Maybe String, Quadrant))

-- | Build a list of enemy ships to fire upon for each weapon of the
-- given ship
findShipTargets :: Entity Ship -> Game -> Map Int [Int]
findShipTargets entity@Entity{_entityData=ship} game = M.fromList $ map (weaponTargets entity enemies) weaponInfo
	where
		enemies = partition3 (target2Quadrant entity) (M.elems $ game^.gameShips)

		weaponInfo = zip [0..] $ expandSlots weaponsAndSlots
		weaponsAndSlots = zip (ship^.shipConfiguration.shipConfigurationWeapons) (ship^.shipWeaponSlots (game^.gameBuilder))

		shipAngle = (uncurry . flip) atan2 $ ship^.shipDirection

		quadrantAngles = map (normalizeAngles . (quad2Angle shipAngle)) [LeftQuad, FrontQuad, RightQuad]
		quad2Angle sA quad = case quad of
			LeftQuad -> (sA + 3.0 * pi / 4.0, sA + pi / 4.0)
			FrontQuad -> (sA + pi / 4.0, sA - pi / 4.0)
			RightQuad -> (sA - pi / 4.0, sA - 3.0 * pi / 4.0)
			AllQuads -> error "AllQuads passed to quad2Angle. Should never happen."
		normalizeAngles (l, r) = (l', r')
			where
				r' = if r < -pi
					then r + (2 * pi)
					else r
				l' = if l > pi
					then l - (2 * pi)
					else l

		inFiringArc angle2Enemy (l, r) = if l < r
			then angle2Enemy >= r || angle2Enemy <= l
			else angle2Enemy >= r && angle2Enemy <= l

		angle2Enemy e t = (uncurry . flip) atan2 $ (t^.entityData.shipLocation) ^-^ (e^.entityData.shipLocation)

		target2Quadrant e t = if (e^.ownerID) == (t^.ownerID)
			then Nothing
			else case findIndex (inFiringArc $ angle2Enemy e t) quadrantAngles of
				Just 0 -> Just LeftQuad
				Just 1 -> Just FrontQuad
				Just 2 -> Just RightQuad
				_ -> Nothing

-- | Build a list of enemy ships in range of the given weapon
weaponTargets :: Entity Ship -> ([Entity Ship], [Entity Ship], [Entity Ship]) -> WeaponInfo -> (Int, [EntityID])
weaponTargets entity (l, f, r) (id, (weapon, quadrant)) = if isJust weapon
	then case quadrant of
		LeftQuad -> (id, (idOfInRange entity) l)
		FrontQuad -> (id, (idOfInRange entity) f)
		RightQuad -> (id, (idOfInRange entity) r)
		AllQuads -> (id, (idOfInRange entity) (l ++ f ++ r))
	else (id, [])
	where
		idOfInRange e ts = map (_entityID) $ filter (inRange e) ts
		inRange e t = magnitude ((e^.entityData.shipLocation) ^-^ (t^.entityData.shipLocation)) < 25

expandSlots :: [(Maybe String, WeaponSlot)] -> [(Maybe String, Quadrant)]
expandSlots slots = concatMap expandSlot slots

expandSlot :: (Maybe String, WeaponSlot) -> [(Maybe String, Quadrant)]
expandSlot (weapon, slot) = case slot^.weaponSlotType of
	Front -> [(weapon, FrontQuad)]
	Side -> [(weapon, LeftQuad), (weapon, RightQuad)]
	Turret -> [(weapon, AllQuads)]

partition3 p xs = foldl (select p) ([], [], []) xs
select :: (a -> Maybe Quadrant) -> ([a], [a], [a]) -> a -> ([a], [a], [a]) 
select p (l, f, r) x = case p x of
	Just LeftQuad -> (x:l, f, r)
	Just FrontQuad -> (l, x:f, r)
	Just RightQuad -> (l, f, x:r)
	_ -> (l, f, r)
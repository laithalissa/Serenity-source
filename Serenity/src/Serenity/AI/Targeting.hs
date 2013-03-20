module Serenity.AI.Targeting where

import Serenity.Model.Entity
import Serenity.Model.Game

import Control.Lens
import Data.List (findIndex, group, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.VectorSpace

-- | Quadrants, relative to the ship, that a weapon can fire upon
data Quadrant = LeftQuad | FrontQuad | RightQuad | AllQuads deriving Show

type PartitionedShips = ([Entity Ship], [Entity Ship], [Entity Ship])
type WeaponInfo = (Int, (Maybe Weapon, Quadrant))

-- | Build a list of enemy ships to fire upon for each weapon of the
-- given ship
findShipTargets :: Entity Ship -> Game -> Map Int [EntityID]
findShipTargets entity@Entity{_entityData=ship} game = M.fromList targets
	where
		targets = map (\(id, ts) -> (id, take 1 $ sortBy compareTFreq ts)) potentialTargets

		allTargets = map (\xs -> (head xs, length xs)) $ group $ sort (concatMap snd potentialTargets)
		potentialTargets = map (weaponTargets (ship^.shipLocation) enemies) weaponInfo
		enemies = partition3 (target2Quadrant entity) (M.elems $ game^.gameShips)
		compareTFreq a b = compare (fromMaybe 0 $ lookup a allTargets) (fromMaybe 0 $ lookup b allTargets)

		weaponInfo = zip [0..] $ expandSlots (ship^.shipWeapons (game^.gameBuilder))

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
weaponTargets
	:: (Double, Double) -- ^ Location of the ship
	-> PartitionedShips -- ^ List of all enemy ships
	-> WeaponInfo       -- ^ Info about the weapon to select targets for
  -> (Int, [EntityID])
weaponTargets loc (l, f, r) (id, (weapon, quadrant)) = if isJust weapon
	then case quadrant of
		LeftQuad -> (id, getPotentialTargets l)
		FrontQuad -> (id, getPotentialTargets f)
		RightQuad -> (id, getPotentialTargets r)
		AllQuads -> (id, getPotentialTargets (l ++ f ++ r))
	else (id, [])
	where
		getPotentialTargets enemies = map (_entityID) $ filter inRange enemies
		inRange t = (magnitude $ loc ^-^ (t^.entityData.shipLocation)) < (fromIntegral $ weapon^.(to fromJust).weaponRange)

expandSlots :: [(Maybe Weapon, WeaponSlot)] -> [(Maybe Weapon, Quadrant)]
expandSlots slots = concatMap expandSlot slots

expandSlot :: (Maybe Weapon, WeaponSlot) -> [(Maybe Weapon, Quadrant)]
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

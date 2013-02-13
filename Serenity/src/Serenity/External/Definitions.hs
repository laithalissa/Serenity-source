
module Serenity.External.Definitions where

import Serenity.External.Addons

---------- Ship Class ----------



shipClassMaker :: Yaml -> (ShipClass, String, FilePath)
shipClassMaker node = (ShipClass cor' strength' weapons' systems', name', imageName')
	where
		name' = yamlLookupString "shipName" node
		imageName' = yamlLookupString "fileName" node
		cor' = read $ yamlLookupString "centerOfRotation" node
		strength' = damageStrengthMaker $ yamlLookup "damageStrength" node
		weapons' = map weaponSlotMaker $ yamlList $ yamlLookup "weaponSlots" node
		systems' = map systemSlotMaker $  yamlList $ yamlLookup "systemSlots" node

damageStrengthMaker :: Yaml -> Damage
damageStrengthMaker node = (Damage hull' shields')
	where
	hull' = read $ yamlLookupString "hull" node
	shields' = read $ yamlLookupString "shields" node

weaponSlotMaker :: Yaml -> WeaponSlot
weaponSlotMaker node = WeaponSlot location' direction' type'
	where
		location' = read $ yamlLookupString "location" node
		direction' = read $ yamlLookupString "direction" node
		type' = read $ yamlLookupString "type" node

systemSlotMaker :: Yaml -> SystemSlot
systemSlotMaker node = SystemSlot location' direction'
	where
		location' = read $ yamlLookupString "location" node
		direction' = read $ yamlLookupString "direction" node

---------- Weapon ----------
weaponMaker 
	:: Yaml -- ^ yaml node 
	-> ( Weapon -- ^ weapon extracted from node 
	   , String -- ^ weapon name
	   , FilePath -- ^ weapon image file
	   )
weaponMaker node = (weapon, name, fileName)
	where
		weapon = Weapon range' effect' reloadTime' accuracy' cost'
		name = yamlLookupString "weaponName" node
		fileName = yamlLookupString "fileName" node
		range' = read $ yamlLookupString "range" node
		reloadTime' = read $ yamlLookupString "reloadTime" node
		accuracy' = read $ yamlLookupString "accuracy" node
		effect' = weaponDamageMaker $ yamlLookup "damage" node
		cost' = weaponUseCostMaker $ yamlLookup "useCost" node
		

weaponDamageMaker 
	:: Yaml -- ^ yaml node
	-> WeaponEffect -- ^ weapon damage extracted from node
weaponDamageMaker node = WeaponEffect shield' hull' penetration'
	where
		shield' = read $ yamlLookupString "shield" node
		hull' = read $ yamlLookupString "hull" node
		penetration' = read $ yamlLookupString "penetration" node


weaponUseCostMaker 
	:: Yaml -- ^ yaml node
	-> Resources -- ^ resource cost extracted from node
weaponUseCostMaker node = Resources fuel' metal' antimatter'
	where
		fuel' = read $ yamlLookupString "fuel" node
		metal' = read $ yamlLookupString "metal" node
		antimatter' = read $ yamlLookupString "antiMatter" node


---------- Systems ----------

systemMaker
	:: Yaml -- ^ yaml node
	-> ( System -- ^ ship system extracted from node
	   , String -- ^ system name
	   , FilePath -- ^ filename of system image
	   ) 
systemMaker node = (System shield' hull' engine', name', fileName')
	where
		name' = yamlLookupString "name" node
		fileName' = yamlLookupString "fileName" node
		shield' = read $ yamlLookupString "shield" node
		hull' = read $ yamlLookupString "hull" node
		engine' = read $ yamlLookupString "speed" node



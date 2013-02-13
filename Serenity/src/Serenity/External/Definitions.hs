
module Serenity.External.Definitions where

import Serenity.External.Addons


---------- Ship Class ----------

shipClassYamlForm :: YamlForm ShipClass
shipClassYamlForm = YamlForm toYaml' fromYaml' getName' getAssetName' "ships"
	where
	toYaml' (a, name, asset) = YamlNull
	fromYaml' node = (ShipClass cor' strength' weapons' systems', name', imageName')
		where
		cor' = read $ yamlLookupString "centerOfRotation" node
		strength' = damageStrengthMaker $ yamlLookup "damageStrength" node
		weapons' = map weaponSlotMaker $ yamlList $ yamlLookup "weaponSlots" node
		systems' = map systemSlotMaker $  yamlList $ yamlLookup "systemSlots" node	

	getName' node = yamlLookupString "shipName" node
	getAssetName' node = yamlLookupString "fileName" node


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

weaponYamlForm :: YamlForm Weapon
weaponYamlForm = YamlForm toYaml' fromYaml' getName' getAssetName' "weapons"
	where
	toYaml' a = YamlNull
	fromYaml' node = Weapon range' effect' reloadTime' accuracy' cost'
		where
		range' = read $ yamlLookupString "range" node
		reloadTime' = read $ yamlLookupString "reloadTime" node
		accuracy' = read $ yamlLookupString "accuracy" node
		effect' = weaponDamageMaker $ yamlLookup "damage" node
		cost' = weaponUseCostMaker $ yamlLookup "useCost" node
	getName' node = yamlLookupString "weaponName" node
	getAssetName' node = yamlLookupString "fileName" node


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

systemYamlForm :: YamlForm System
systemYamlForm = YamlForm toYaml' fromYaml' getName' getAssetName' "systems"
	where
	toYaml' a = YamlNull
	fromYaml' node = System shield' hull' engine'
		where
		shield' = read $ yamlLookupString "shield" node
		hull' = read $ yamlLookupString "hull" node
		engine' = read $ yamlLookupString "speed" node
	getName' node = yamlLookupString "name" node
	getAssetName' = yamlLookupString "fileName" node


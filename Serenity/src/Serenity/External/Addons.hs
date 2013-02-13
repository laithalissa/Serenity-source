
module Serenity.External.Addons
(	module Serenity.External.Common
,	initAddons
) where

import Serenity.External.Common 
import Serenity.External.Definitions
import Serenity.Game.Model

makeGameBuidler :: Sector -> Map OwnerID Fleet -> IO GameBuilder
makeGameBuilder sector fleets = do
	shipClasses <- initAddons shipClassYamlForm
	weapons <- initAddons weaponsYamlForm
	systems <- initAddons systemsYamlForm
	return $ GameBuidler sector shipClasses weapons systems fleets


initAddons :: YamlForm a -> IO (Map String a)
initAddons yamlForm = do
	yamlNodes <- loadYamlForm yamlForm
	let as = map (yamlForm^.yamlFormFromYaml) yamlNodes
	let names = map (yamlForm^.yamlFormName) yamlNodes
	return $ Map.fromList $ zip names as
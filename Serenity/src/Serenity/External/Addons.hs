
module Serenity.External.Addons
(	module Serenity.External.Common
,	initAddons
) where

import Serenity.External.Common 



initAddons :: YamlForm a -> IO (Map String a)
initAddons yamlForm = do
	yamlNodes <- loadYamlForm yamlForm
	let as = map (yamlForm^.yamlFormFromYaml) yamlNodes
	let names = map (yamlForm^.yamlFormName) yamlNodes
	return $ Map.fromList $ zip names as
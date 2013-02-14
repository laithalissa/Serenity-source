
module Serenity.External
(	module Serenity.External.Addons
,	module Serenity.External.Assets
,	module Serenity.External.Common
,	module Serenity.External.Definitions
,	makeGameBuilder
) where

import Serenity.External.Addons
import Serenity.External.Assets
import Serenity.External.Common
import Serenity.External.Definitions

makeGameBuilder :: Sector -> Map OwnerID Fleet -> IO GameBuilder
makeGameBuilder sector fleets = do
	shipClasses <- initAddons shipClassYamlForm
	weapons <- initAddons weaponsYamlForm
	systems <- initAddons systemsYamlForm
	return $ GameBuidler sector shipClasses weapons systems fleets
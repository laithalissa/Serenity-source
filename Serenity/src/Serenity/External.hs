
module Serenity.External
(	module Serenity.External.Addons
,	module Serenity.External.Assets
,	module Serenity.External.Common
,	module Serenity.External.Definitions
,	makeGameBuilder
,	makeDemoGameBuilder
) where

import Data.Map(Map)
import qualified Data.Map as Map

import Serenity.External.Addons
import Serenity.External.Assets
import Serenity.External.Common
import Serenity.External.Definitions

import Serenity.Model

makeGameBuilder :: Sector -> Map OwnerID Fleet -> IO GameBuilder
makeGameBuilder sector fleets = do
	shipClasses <- initAddons shipClassYamlForm
	weapons <- initAddons weaponYamlForm
	systems <- initAddons systemYamlForm
	return $ GameBuilder sector shipClasses weapons systems fleets

makeDemoGameBuilder :: IO GameBuilder
makeDemoGameBuilder = makeGameBuilder sectorOne fleet'
	where
	fleet' = Map.fromList 
		[	(0, demoFleet)
		,	(1, demoFleet)
		,	(2, demoFleet)
		,	(3, demoFleet)
		]
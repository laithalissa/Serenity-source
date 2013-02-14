
module Serenity.External
(	module Serenity.External.Addons
,	module Serenity.External.Assets
,	module Serenity.External.Common
,	module Serenity.External.Definitions
,	makeGameBuilder
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
	return $ GameBuidler sector shipClasses weapons systems fleets
{-# LANGUAGE TemplateHaskell #-}

module Serenity.External.Addons
(	module Serenity.External.Common
,	initAddons
) where

import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map

import Serenity.External.Common 
import Serenity.Model


initAddons :: YamlForm a -> IO (Map String a)
initAddons yamlForm = do
	yamlNodes <- loadYamlForm yamlForm
	let as = map (yamlForm^.yamlFormFromYaml) yamlNodes
	let names = map (yamlForm^.yamlFormName) yamlNodes
	return $ Map.fromList $ zip names as
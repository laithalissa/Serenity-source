
module Serenity.Game.Server.GameRunner where

import qualified Data.Set as Set

import qualified Serenity.Game.Server.World as W
import Serenity.Game.Model.ClientMessage
import Serenity.Game.Model.Common
import qualified Serenity.Game.Model.Entity as E
import Serenity.Game.Model.GameMap
import qualified Serenity.Game.Server.Demo as D

main :: IO ()
main = D.main


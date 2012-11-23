{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Serenity.Game.Server.GameState
() where

import Graphics.Gloss.Interface.Pure.Game(play)
import Graphics.Gloss.Data.Picture(Picture(..), loadBMP, text, color, pictures, scale, translate, line, rotate)
import Graphics.Gloss.Data.Color(red)
import Graphics.Gloss.Interface.Pure.Game(SpecialKey(..), Key(..), Event(..), KeyState(..), MouseButton(..))

import Serenity.Game.Model.ClientMessage(ClientMessage(..), GraphicsMessage(..), WorldMessage(..))
import Serenity.Game.Model.WorldDelta(WorldDelta)
import Serenity.Game.Model.GameMap(GameMap(..), Planet(..), SpaceLane(..))
import Serenity.Game.Model.Common(TimeDuration, ViewPort)
import Serenity.Game.Model.ShipClass(ShipClass(..))
import Serenity.Game.Model.Common(TimeDuration, toList4)
import Serenity.Game.Model.Entity(Entity(..))
import Serenity.Game.Server.InputFilter(InputFilter, initialize, handleInput)
import qualified Data.Map as Map

type WindowSize = (Int, Int)

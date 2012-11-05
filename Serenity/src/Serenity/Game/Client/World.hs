
module Serenity.Game.Client.World where

import Serenity.Game.World.World

class World a where
    updateFromWorldDelta :: WorldDelta -> a -> a
    




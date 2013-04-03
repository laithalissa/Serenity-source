module Serenity.Debug {-# WARNING "Please do not use this module in a production environment." #-}
(	module Debug.Trace
,	trace'
) where

import Debug.Trace

trace' x = traceShow x x

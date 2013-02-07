module Serenity.Debug
(	module Debug.Trace
,	trace'
) where

import Debug.Trace

trace' x = traceShow x x

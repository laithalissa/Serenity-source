module Serenity.External.Fleets
(	saveFleet
,	loadFleet
,	listFleets
)
where

import Serenity.External.Common
import Serenity.Model.Fleet (Fleet)
import Paths_Serenity

import Data.Binary (encodeFile, decodeFile)
import System.EasyFile (getDirectoryContents, dropExtensions, takeExtensions, pathSeparator)

fleetsDir :: String
fleetsDir = "fleets"

fleetExt :: String
fleetExt = ".fleet"

-- | Save a fleet to disk
saveFleet :: String -> Fleet -> IO ()
saveFleet name fleet = do
	dataDir <- getDataDir
	let path = (subdir dataDir fleetsDir) ++ (pathSeparator : name) ++ fleetExt
	encodeFile path fleet

-- | Load a saved fleet from disk
loadFleet :: String -> IO Fleet
loadFleet name = do
	dataDir <- getDataDir
	let path = (subdir dataDir fleetsDir) ++ (pathSeparator : name) ++ fleetExt
	decodeFile path

-- | Retrieve a list of all the saved fleets
listFleets :: IO [String]
listFleets = do
	dataDir <- getDataDir
	contents <- getDirectoryContents (subdir dataDir fleetsDir)
	return $ map dropExtensions $ filter ((== fleetExt) . takeExtensions) contents

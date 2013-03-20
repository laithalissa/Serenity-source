module Serenity.Misc.GameNameGenerator
(	generateName
) where

import Serenity.Misc.VideoGameNames (names)

import Control.Monad
import Data.Random
import Data.Random.Extras hiding (sample)
import Data.List
import Data.List.Split

generateName :: IO String
generateName = do
	terms <- return $ splitOn ["----"] names
	game  <- mapM (foldM pickwords ("", [""])) (inits terms)
	return $ foldr1 (\x n -> if x == "" then n else x ++ " " ++ n) (map fst game)

pickwords :: (String, [String]) -> [String] -> IO (String, [String])
pickwords (lastWord, lastBad) terms = nextWord (lastWord:lastBad) terms

nextWord :: [String] -> [String] -> IO (String, [String])
nextWord badTerms terms = do
	string <- sample $ choice terms
	(word : bad) <- return $ splitOn "^" string
	if not $ elem word badTerms 
		then return (word, badTerms++(splitOn "|" (concat bad))) 
		else nextWord badTerms terms

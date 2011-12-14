module Customizable (process, combine, addHead) where

process :: [Block] -> [Block]
process (Header 2 _:xs) = ruleify xs where
	ruleify :: [Block] -> [Block]
	ruleify (Header 2 _:xs) = HorizontalRule : ruleify xs
	ruleify (x:xs) = x:ruleify xs
	ruleify [] = []
process (x:xs) = x:process xs
process [] = []

combine :: Int -> [Block] -> [Block]

-- Levels:
-- 1 : a ministrand (if leaf) or part
-- 2 : a strand
-- 3 : a chapter
addHead :: Int -> [Block] -> Contextual IO [Block]
addHead 1 blocks = do
	miniStrand <- asks isFile
	if miniStrand
	then addHeadToMiniStrand blocks
	else addHeadToPart blocks
addHead 2 = addHeadToStrand
addHead 3 = addHeadToChapter
addHeadToMiniStrand :: Int -> [Block] -> Contextual IO [Block]
addHeadToPart :: [Block] -> Contextual IO [Block]
addHeadToStrand :: [Block] -> Contextual IO [Block]
addHeadToChapter :: [Block] -> Contextual IO [Block]

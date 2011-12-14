module Process (process) where

h2ToHr :: [Block] -> [Block]
h2ToHr (Header 2 _:xs) = HorizontalRule : h2ToHr xs
h2ToHr (x:xs) = x:h2ToHr xs
h2ToHr [] = []

process :: [Block] -> [Block]
process (Header 2 _:xs) = h2ToHr xs
process (x:xs) = x:process xs
process [] = []

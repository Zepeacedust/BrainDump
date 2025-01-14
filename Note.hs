module Note where

import System.IO

import System.Environment

data Note = Note String String [Tag] deriving (Show, Eq)

data Tag = Tag String deriving (Show, Eq)


consumeAll :: ([a] -> (b, [a])) -> [a] -> [b]
consumeAll f [] = []
consumeAll f xs = b : consumeAll f rest where
    (b, rest) = f xs 

parseNote ("":xs) = parseNote xs
parseNote (x:xs) = (length x, xs)
parseNote [] = (0,[])
hGetLines file = do
    line <- hGetLine file
    done <- hIsEOF file
    if done 
        then return [line]
        else do  
            rest <- hGetLines file
            return (line : rest)

-- readNotesFromFile :: String -> IO [Note]
readNotesFromFile fileName = do
    file <- openFile fileName ReadWriteMode
    lines <- hGetLines file 
    return (consumeAll parseNote lines)



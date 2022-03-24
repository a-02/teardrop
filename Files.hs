{-# LANGUAGE OverloadedStrings #-}

module Files where

import Control.Applicative hiding (many)
import Data.ByteString as B hiding (getLine, putStr)
import Data.Either
import Data.Functor
import Data.Serialize
import Data.Tuple (swap) -- oops!!! ooops!!!!! FUCK
import Data.Vector
import Data.Vector.Serialize
import Data.Void
import Graphics
import System.IO hiding (readFile, writeFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Types
import Prelude as P

type Parser = Parsec Void String -- remember how you can partially apply type constructors?

parseKeyCommand :: Parser KeyCommand
parseKeyCommand = P.foldl1 (<|>) $ kcTypes <*> (string' <$> kcStrings)
  where
    kcTypes =
      (<$)
        <$> [ UpLeft, -- die
              UpRight,
              DownLeft,
              DownRight,
              Up,
              Down,
              KLeft,
              KRight,
              PrevFG,
              NextFG,
              PrevBG,
              NextBG,
              PrevTX,
              NextTX,
              PrevPG,
              NextPG,
              Save,
              Load,
              Select
            ]
    kcStrings =
      [ "upleft",
        "upright",
        "downleft",
        "downright",
        "up",
        "down",
        "left",
        "right",
        "prevfg",
        "nextfg",
        "prevbg",
        "nextbg",
        "prevtx",
        "nexttx",
        "prevpg",
        "nextpg",
        "save",
        "load",
        "select"
      ]

parseLetterOrPunct :: Parser Char
parseLetterOrPunct = letterChar <|> punctuationChar

parseKCLine :: Parser (Char, KeyCommand)
parseKCLine = do
  kc <- parseKeyCommand
  void $ char '='
  ch <- parseLetterOrPunct
  void $ newline
  return (ch, kc)

-- maybe make this so a user can call a keyconfig from command line
grabby :: IO [(Char, KeyCommand)]
grabby = do
  conf <- P.readFile "keys.conf"
  return $ case runParser (many parseKCLine <* eof) "keys.conf" conf of
    Left _ -> undefined
    Right x -> x

lookupU :: (Eq a, Unital b) => [(a, b)] -> a -> b -- fuck Maybe. all my homies hate Maybe
lookupU [] _ = unit
lookupU ((x, y) : rest) key
  | key == x = y
  | otherwise = lookupU rest key

saveImage :: Image -> IO Image
saveImage img = do
  hSetEcho stdin True
  hSetBuffering stdout NoBuffering
  putStr "filepath to save to: "
  filepath <- getLine
  putStrLn $ "saving to " P.++ filepath P.++ ".td ..."
  B.writeFile (filepath P.++ ".td") (encode img)
  putStrLn "done."
  hSetEcho stdin False
  hSetBuffering stdout LineBuffering
  return img

loadImage :: IO Image
loadImage = do
  hSetEcho stdin True
  hSetBuffering stdout NoBuffering
  putStr "filepath to load from:"
  filepath <- getLine
  putStrLn $ "loading " P.++ filepath P.++ " ..."
  file <- B.readFile (filepath P.++ ".td")
  let image = decode file :: Either String Image
   in do
        putStrLn "done, probably."
        hSetEcho stdin False
        hSetBuffering stdout LineBuffering
        return $ either (const $ blankImage 30 30) id image

diskOp :: Char -> Image -> IO Image
diskOp input image =
  case input of
    'S' -> saveImage image
    'L' -> loadImage
    _ -> return image

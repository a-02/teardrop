{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Files where

import Control.Applicative hiding (many)
import Control.Monad (join)
import Data.ByteString as B hiding (getLine, putStr)
import Data.Either
import Data.Functor
import Data.IORef
import Data.Serialize
import Data.Vector
import Data.Vector.Serialize
import Data.Void
import Graphics
import System.IO hiding (readFile, writeFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error -- debugging!
import Types
import Prelude as P

type Parser = Parsec Void String -- remember how you can partially apply type constructors?

parseKeyCommandByHand :: Parser KeyCommand
parseKeyCommandByHand =
  choice
    [ UpLeft <$ string "upleft",
      UpRight <$ string "upright",
      DownLeft <$ string "downleft",
      DownRight <$ string "downright",
      Up <$ string "up",
      Down <$ string "down",
      KLeft <$ string "left",
      KRight <$ string "right",
      PrevFG <$ string "prevfg",
      NextFG <$ string "nextfg",
      PrevBG <$ string "prevbg",
      NextBG <$ string "nextbg",
      PrevTX <$ string "prevtx",
      NextTX <$ string "nexttx",
      PrevPG <$ string "prevpg",
      NextPG <$ string "nextpg",
      Save <$ string "save",
      Load <$ string "load",
      Select <$ string "select"
    ]

parseLetterOrPunct :: Parser Char
parseLetterOrPunct = letterChar <|> punctuationChar

parseKCLine :: Parser (Char, KeyCommand)
parseKCLine = do
  kc <- parseKeyCommandByHand
  void $ char '='
  ch <- parseLetterOrPunct
  void $ newline
  return (ch, kc)

-- maybe make this so a user can call a keyconfig from command line
grabby :: IO (Char -> KeyCommand)
grabby =
  lookupU <$> do
    conf <- P.readFile "keys.conf"
    return $ case runParser (many parseKCLine <* eof) "keys.conf" conf of
      Left x -> error (errorBundlePretty x) -- Fuck You
      Right x -> x

parseTest =
  join $ do
    conf <- P.readFile "keys.conf"
    return $ case runParser (many parseKCLine <* eof) "keys.conf" conf of
      Left x -> putStr (errorBundlePretty x) -- Fuck You
      Right x -> print x

char2kcRef :: IO Conf
char2kcRef = do
  grab <- grabby
  conf <- newIORef grab
  return conf

char2kcRef' :: IO Conf
char2kcRef' = grabby >>= newIORef

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

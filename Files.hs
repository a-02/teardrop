module Files where

import Prelude as P

import System.IO hiding (readFile, writeFile)

import Data.Either
import Data.Vector
import Data.Vector.Serialize
import Data.Serialize
import Data.ByteString as B hiding (getLine)

import Types
import Graphics

saveImage :: Image -> IO Image
saveImage img = do
  hSetEcho stdin True
  print "filepath to save to: "
  filepath <- getLine
  putStrLn $ "saving to" P.++ filepath P.++ ".td ..."
  B.writeFile (filepath P.++ ".td") (encode img)
  putStrLn "done."
  hSetEcho stdin False
  return img

loadImage :: IO Image
loadImage = do
  hSetEcho stdin True
  print "filepath to load from:"
  filepath <- getLine
  putStrLn $ "loading" P.++ filepath P.++ " ..."
  file <- B.readFile filepath 
  let image = decode file :: Either String Image
   in do putStrLn "done, probably."
         hSetEcho stdin False
         return $ either (const $ blankImage 30 30) id image
  
diskOp :: Char -> Image -> IO Image
diskOp input image = 
  case input of
    'S' -> saveImage image
    'L' -> loadImage 
    _   -> return image

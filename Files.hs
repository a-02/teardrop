module Files where

import Prelude as P

import System.IO hiding (readFile, writeFile)

import Data.Either
import Data.Vector
import Data.Vector.Serialize
import Data.Serialize
import Data.ByteString as B hiding (getLine, putStr)
import Data.Tini -- >:)

import Types
import Graphics



-- dont look over here


saveImage :: Image -> IO Image
saveImage img = do
  hSetEcho stdin True; hSetBuffering stdout NoBuffering
  putStr "filepath to save to: "
  filepath <- getLine
  putStrLn $ "saving to " P.++ filepath P.++ ".td ..."
  B.writeFile (filepath P.++ ".td") (encode img)
  putStrLn "done."
  hSetEcho stdin False; hSetBuffering stdout LineBuffering
  return img

loadImage :: IO Image
loadImage = do
  hSetEcho stdin True; hSetBuffering stdout NoBuffering
  putStr "filepath to load from:"
  filepath <- getLine
  putStrLn $ "loading " P.++ filepath P.++ " ..."
  file <- B.readFile (filepath P.++ ".td")
  let image = decode file :: Either String Image
   in do putStrLn "done, probably."
         hSetEcho stdin False; hSetBuffering stdout LineBuffering
         return $ either (const $ blankImage 30 30) id image
  
diskOp :: Char -> Image -> IO Image
diskOp input image = 
  case input of
    'S' -> saveImage image
    'L' -> loadImage 
    _   -> return image

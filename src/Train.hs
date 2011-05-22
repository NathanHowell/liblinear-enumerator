module Main (main) where

import qualified Data.ByteString as B
import Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import Data.LibLinear
import qualified Data.List as L
import Data.Text as T
import qualified Data.Text.Read as TR
import System.IO

countLines :: Iteratee B.ByteString IO (Int, Int)
countLines = EB.fold step (0, 0)
  where step acc@(e, f) x | x == 0x0A = (e+1, f)
                          | x == 0x3A = (e, f+1)
                          | otherwise = acc

lineToExample :: Text -> Example
lineToExample a = Example target features
  where Right (target, _) = TR.double t
        features = L.map tokenToFeature f
        t:f = T.words a

tokenToFeature :: Text -> Feature
tokenToFeature t = Feature i' v'
  where Right (i', _) = TR.decimal i
        Right (v', _) = TR.double v
        i:v:_ = T.split (==':') t

xform :: Enumeratee Text Example IO b
xform (Continue k) = continue go
  where go (Chunks []) = continue go
        go (Chunks xs) = (k $ Chunks (L.map lineToExample xs)) >>== xform
        go EOF = k EOF >>== (\ step -> yield step EOF)
  
main :: IO ()
main = withFile "heart_scale" ReadMode go where
  go h = do
    Right (rows, features) <- E.run $ EB.enumHandle 4096 h E.$$ countLines
    hSeek h AbsoluteSeek 0
    let params = TrainParams {trainSolver = L2R_LR, trainExamples = rows, trainFeatureSum = features}
    print params
    Right model <- E.run $ ET.enumHandle h E.$$ xform E.=$ train params
    print rows


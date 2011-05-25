{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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

import Data.Data
import System.Console.CmdArgs

data TrainOpts = TrainOpts
  { optSolver      :: Solver
  , optCost        :: Double
  , optEpsilon     :: Maybe Double
  , optBias        :: Maybe Double
  , optWeights     :: [Double]
  , optXValidation :: Maybe Int
  , optQuiet       :: Bool
  , optInput       :: String
  , optOutput      :: String
  } deriving (Show, Data, Typeable)

argz :: TrainOpts
argz = TrainOpts
  { optSolver = L2R_L2LOSS_SVC_DUAL
                         &= explicit &= name "s" &= name "solver"
                         &= help "set type of solver"
  , optCost = 1.0        &= explicit &= name "c" &= name "cost"
                         &= help "set the parameter C"
  , optEpsilon = def     &= explicit &= name "e" &= name "espilon"
                         &= help "set tolerance of termination criterion"
  , optBias = def        &= explicit &= name "B" &= name "bias"
                         &= help "instance x becomes [x; bias]"
  , optWeights = def     &= explicit &= name "wi"
                         &= help "weights adjust the parameter C of different classes"
  , optXValidation = def &= explicit &= name "v"
                         &= help "n-fold cross validation mode"
  , optQuiet = False     &= explicit &= name "q" &= name "quiet"
                         &= help "quiet mode (no outputs)"
  , optInput = def       &= argPos 0
                         &= typ "training_set_file"
  , optOutput = def      &= argPos 1 &= opt "" 
                         &= typ "model_file"
  }

setEpsDefault :: TrainOpts -> TrainOpts
setEpsDefault to@TrainOpts{optSolver, optEpsilon = Nothing} =
  let eps = case optSolver of
        L2R_LR              -> case1
        L2R_L2LOSS_SVC      -> case1
        L2R_L2LOSS_SVC_DUAL -> case2
        L2R_L1LOSS_SVC_DUAL -> case2
        MCSVM_CS            -> case2
        L2R_LR_DUAL         -> case2
        L1R_L2LOSS_SVC      -> case3
        L1R_LR              -> case3
  in to{optEpsilon = Just $! eps}
  where case1 = 0.01
        case2 = 0.1
        case3 = 0.01

setEpsDefault to = to

countLines :: Iteratee B.ByteString IO (Int, Int)
countLines = EB.fold step (0, 0)
  where step acc@(e, f) x | x == 0x0A = (e+1, f)
                          | x == 0x3A = (e, f+1)
                          | otherwise = acc

lineToExample :: Text -> Example
lineToExample a = Example (Target target) features
  where Right (target, _) = TR.signed TR.decimal t
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
main = do
  to@TrainOpts{..} <- cmdArgs argz
  let to' = setEpsDefault to
      output' = if optOutput == "" then optInput ++ ".model" else optOutput
  withFile optInput ReadMode $ \ hIn -> do
  withFile output' WriteMode $ \ hOut -> do
    Right (rows, features) <- E.run $ EB.enumHandle 4096 hIn E.$$ countLines
    hSeek hIn AbsoluteSeek 0
    let params = TrainParams {trainSolver = optSolver, trainExamples = rows, trainFeatureSum = features}
    print params
    Right model <- E.run $ ET.enumHandle hIn E.$$ xform E.=$ train params
    hPrint hOut model


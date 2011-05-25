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
  -- XXX/TODO: estimate values at runtime
  where case1 = 0.01
        case2 = 0.1
        case3 = 0.01

setEpsDefault to = to

{--
	"-e epsilon : set tolerance of termination criterion\n"
	"	-s 0 and 2\n"
	"		|f'(w)|_2 <= eps*min(pos,neg)/l*|f'(w0)|_2,\n"
	"		where f is the primal function and pos/neg are # of\n"
	"		positive/negative data (default 0.01)\n"
	"	-s 1, 3, 4 and 7\n"
	"		Dual maximal violation <= eps; similar to libsvm (default 0.1)\n"
	"	-s 5 and 6\n"
	"		|f'(w)|_1 <= eps*min(pos,neg)/l*|f'(w0)|_1,\n"
	"		where f is the primal function (default 0.01)\n"
--}

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
  to <- cmdArgs argz
  let to' = setEpsDefault to
  withFile (optInput to') ReadMode $ \ h -> do
    Right (rows, features) <- E.run $ EB.enumHandle 4096 h E.$$ countLines
    hSeek h AbsoluteSeek 0
    let params = TrainParams {trainSolver = optSolver to', trainExamples = rows, trainFeatureSum = features}
    print params
    Right model <- E.run $ ET.enumHandle h E.$$ xform E.=$ train params
    print model


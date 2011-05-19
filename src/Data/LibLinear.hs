{-# LANGUAGE ScopedTypeVariables #-}

module Data.LibLinear
  ( Model(..)
  , Feature(..)
  , Example(..)
  , Solver(..)
  , train
  ) where

import Bindings.Linear
import Control.Monad.Trans (liftIO)
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.List as L
import qualified Data.Vector.Storable.Mutable as MVec
import Foreign as F
import Foreign.C.Types

data Model = Model deriving (Show)
data Feature = Feature !Int !Double deriving (Show)
data Example = Example !Double [Feature] deriving (Show)

data Solver
  = L2R_LR
  | L2R_L2LOSS_SVC_DUAL
  | L2R_L2LOSS_SVC
  | L2R_L1LOSS_SVC_DUAL
  | MCSVM_CS
  | L1R_L2LOSS_SVC
  | L1R_LR
  | L2R_LR_DUAL
    deriving (Show, Eq)

instance Bounded Solver where
  minBound = L2R_LR
  maxBound = L2R_LR_DUAL

instance Enum Solver where
  fromEnum L2R_LR              = c'L2R_LR
  fromEnum L2R_L2LOSS_SVC_DUAL = c'L2R_L2LOSS_SVC_DUAL
  fromEnum L2R_L2LOSS_SVC      = c'L2R_L2LOSS_SVC
  fromEnum L2R_L1LOSS_SVC_DUAL = c'L2R_L1LOSS_SVC_DUAL
  fromEnum MCSVM_CS            = c'MCSVM_CS
  fromEnum L1R_L2LOSS_SVC      = c'L1R_L2LOSS_SVC
  fromEnum L1R_LR              = c'L1R_LR
  fromEnum L2R_LR_DUAL         = c'L2R_LR_DUAL
  toEnum v | v <= c'L2R_LR              = L2R_LR
           | v == c'L2R_L2LOSS_SVC_DUAL = L2R_L2LOSS_SVC_DUAL
           | v == c'L2R_L2LOSS_SVC      = L2R_L2LOSS_SVC
           | v == c'L2R_L1LOSS_SVC_DUAL = L2R_L1LOSS_SVC_DUAL
           | v == c'MCSVM_CS            = MCSVM_CS
           | v == c'L1R_L2LOSS_SVC      = L1R_L2LOSS_SVC
           | v == c'L1R_LR              = L1R_LR
           | v == c'L2R_LR_DUAL         = L2R_LR_DUAL
           | otherwise                  = maxBound

featuresToNodeList :: [Feature] -> IO (Ptr C'feature_node)
featuresToNodeList features = F.newArray $ L.map mapper features ++ [sentintel]
  where mapper (Feature i v) = C'feature_node
          { c'feature_node'index = fromIntegral i
          , c'feature_node'value = realToFrac v }
        sentintel = mapper $ Feature (-1) 0.0

newParameter :: Solver -> C'parameter
newParameter solver = C'parameter
  { c'parameter'solver_type = fromIntegral $ fromEnum solver
  , c'parameter'eps = 0.1
  , c'parameter'C = 1.0
  , c'parameter'nr_weight = 0
  , c'parameter'weight_label = nullPtr
  , c'parameter'weight = nullPtr
  }

writeByIndex :: MVec.IOVector CDouble
             -> MVec.IOVector (Ptr C'feature_node)
             -> (Int, Int)
             -> Example
             -> IO (Int, Int)
writeByIndex targets features = \ (i,fm) e@(Example t f) -> do
  let nfm = L.maximum $ L.map (\ (Feature i _) -> i) f
  MVec.write features i =<< featuresToNodeList f 
  MVec.write targets i $! realToFrac t
  return $! (i+1, max fm nfm)

convertModel :: C'model -> Model
convertModel model = undefined

train :: Int -> Solver -> Iteratee Example IO Model 
train exampleCount solver = do
  targets  <- liftIO $ MVec.new exampleCount
  features <- liftIO $ MVec.new exampleCount
  (countedTargets, featureMax) <- EL.foldM (writeByIndex targets features) (0,0)
  if (exampleCount /= countedTargets)
    then fail $! "target mismatch: " ++ show exampleCount ++ " != " ++ show countedTargets
    else liftIO $
    MVec.unsafeWith targets  $ \ targets'  -> do
    MVec.unsafeWith features $ \ features' -> do
      let problem = C'problem
            { c'problem'l = fromIntegral exampleCount
            , c'problem'n = fromIntegral featureMax
            , c'problem'y = targets'
            , c'problem'x = features'
            , c'problem'bias = (-1.0)
            }
      model <- with problem $ \ problem' ->
        with (newParameter solver) $ \ param' ->
          c'train problem' param'
      return . convertModel =<< F.peek model


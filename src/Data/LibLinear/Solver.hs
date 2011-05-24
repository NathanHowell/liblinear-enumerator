{-# LANGUAGE DeriveDataTypeable #-}

module Data.LibLinear.Solver (Solver(..)) where

import Bindings.LibLinear
import Data.Data

data Solver
  = L2R_LR
  | L2R_L2LOSS_SVC_DUAL
  | L2R_L2LOSS_SVC
  | L2R_L1LOSS_SVC_DUAL
  | MCSVM_CS
  | L1R_L2LOSS_SVC
  | L1R_LR
  | L2R_LR_DUAL
    deriving (Show, Eq, Data, Typeable)

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


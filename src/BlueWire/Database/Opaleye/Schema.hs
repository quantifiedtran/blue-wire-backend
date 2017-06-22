{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BlueWire.Database.Opaleye.Schema where

import Opaleye
import Data.Typeable
import Data.Generics
import Data.Time
import Data.Profunctor.Product.TH
import Data.Profunctor.Product.Default
import Data.Profunctor (lmap)
import Data.Aeson.TH

instance Default Constant NominalDiffTime (Column PGFloat8) where
    def = lmap (fromRational . toRational) (def :: Constant Double (Column PGFloat8))

data Recovery r h mx = Recovery {
    rate :: r,
    hurdle :: h,
    maxTime :: mx
} deriving (Eq, Ord, Show, Data, Typeable)

$(makeAdaptorAndInstance "pRecovery" ''Recovery)
deriveJSON defaultOptions ''Recovery
type Recovery' = Recovery NominalDiffTime NominalDiffTime NominalDiffTime
type RecoveryCol = Recovery (Column PGFloat8) (Column PGFloat8) (Column PGFloat8)

data Kick dur ctdn repct rec = Kick {
    duration :: dur,
    countdown :: ctdn,
    repeatCountdown :: repct,
    recoveryProfile :: rec
} deriving (Eq, Ord, Show, Data, Typeable)

$(makeAdaptorAndInstance "pKick" ''Kick)
deriveJSON defaultOptions ''Kick
type Kick' = Kick NominalDiffTime NominalDiffTime (Maybe NominalDiffTime) (Maybe Recovery')
type KickCol
    = Kick (Column PGFloat8)
           (Column PGFloat8)
           (Column (Nullable PGFloat8))
           (Column (Nullable RecoveryCol))

data Profile nm actkck lsthb kckends cnsk = Profile {
    name :: nm,
    activeKicks :: actkck,
    lastHeartbeat :: lsthb,
    kickEnds :: kckends,
    canNextSetKicks :: cnsk
} deriving (Eq, Ord, Show, Data, Typeable)

$(makeAdaptorAndInstance "pProfile" ''Profile)
deriveJSON defaultOptions ''Profile
type Profile' = Profile String [Kick'] UTCTime (Maybe UTCTime) UTCTime
type ProfileCol
    = Profile (Column PGText)
              (Column (PGArray KickCol))
              (Column PGTimestamptz)
              (Column (Nullable PGTimestamptz))
              (Column PGTimestamptz)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Examples.Aggregation (
  testAggregation
) where

import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           GHC.Generics (Generic)

import           Cardano.Logging

data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)


instance LogFormatting BaseStats where
  forMachine _dtal BaseStats{..} =
      HM.fromList
        [ "kind" AE..= AE.String "BaseStats"
        , "bsMeasure" AE..= AE.String (pack $ show bsMeasure)
        , "bsMin" AE..= AE.String (pack $ show bsMin)
        , "bsMax" AE..= AE.String (pack $ show bsMax)
        , "bsCount" AE..= AE.String (pack $ show bsCount)
        , "bsSum" AE..= AE.String (pack $ show bsSum)
        ]
  asMetrics BaseStats {..} =
    [ DoubleM "measure" bsMeasure
    , DoubleM "sum" bsSum]

baseStatsDocumented :: Documented Double
baseStatsDocumented = Documented
  [
    DocMsg
      ["BaseStats"]
      [ ("measure", "This is the value of a single measurment")
      , ("sum", "This is the sum of all measurments")
      ]
      "Basic statistics"
  ]

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

calculate :: BaseStats -> LoggingContext -> Double -> BaseStats
calculate BaseStats{..} _ val =
    BaseStats
      val
      (min bsMin val)
      (max bsMax val)
      (1 + bsCount)
      (bsSum + val)

testAggregation :: IO ()
testAggregation = do
    simpleTracer <- standardTracer
    formTracer <- fmap (appendName "BaseTrace")
                      (humanFormatter True "cardano" simpleTracer)
    tracer <- foldTraceM calculate emptyStats formTracer
    configureTracers emptyTraceConfig baseStatsDocumented [tracer]
    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5

module Stat where

import Prelude
import Lib

type Row = (Double, Double)
type Data = [Row]

-- | We'll use the first column to predict the second.
testData :: Data
-- testData :: [(Double, Double)]
-- ^ Might as well written this, but we want to reuse the types defined above
testData =
  [ (3, 2)
  , (2, 2)
  , (6, 4)
  , (8, 7)
  ]

{- | Model is a representation of a linear equation with an intercept (theta0)
     and slope (theta1), i.e y = theta0 + theta1 * x. -}
type Model = (Double, Double)
{- | ModelF is is an actual function generated from the Model. We have Model
     simply because we can inspect it's content, while the function is a black
     box: given an argument it evaluates to a value. -}
type ModelF = Double -> Double

-- | Convert a model representation to a regular function.
toModelF :: Model -> ModelF
toModelF (theta0, theta1) x = theta0 + theta1 * x

{- | Calculate cost of some model at a single data point by taking the power of
     the diff. This is the common way do to it. -}
pointCost :: ModelF -> Row -> Double
pointCost mf (x, y) = diff^2
  where diff = mf x - y

{- | Calculate cost of some model over some data. -}
cost :: Model -> Data -> Double
cost m d = sum (map pointCost' d) / (2 * length' d)
  where
    mf = toModelF m
    pointCost' = pointCost mf

{- | Try to find a model with minimal cost over the data. It's implicit that we
     only look within linear equations. The probable first algorithm will be the
     gradient descent. -}
minimize :: Data -> Model
minimize d = undefined

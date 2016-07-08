module Stat where

import Prelude
import Data.Maybe (fromJust)
import Data.List (find)
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
     only look within linear equations. -}
minimize :: Data -> Model
minimize data_ = snd $ fromJust $ getConverged $ gradientDescent data_ alpha start
  where
    alpha = 0.001   -- what's a good alpha?
    start = (1, 1)  -- what's a good starting point?
    isConverged = (< 0.0001) -- what's a good distance to stop?
    getConverged :: [Model] -> Maybe (Double, Model)
    getConverged models = let
        distModel :: Model -> Model -> (Double, Model)
        distModel m1 m2 = (dist m1 m2, m2)
        dists :: [(Double, Model)]
        dists = zipWith distModel models (tail models)
      in find (isConverged . fst) dists
    dist :: Model -> Model -> Double
    dist (t0, t1) (t0', t1') = sqrt $ (t0 - t0')^2 + (t1 - t1')^2

{- | Gradient descent for linear equation. -}
gradientDescent
  :: Data     -- ^ Data to calculate cost from
  -> Double   -- ^ Learning rate
  -> Model    -- ^ Initial model/coordinates
  -> [Model]  -- ^ Infinite list iteration steps
gradientDescent data_ alpha model = gradientDescent' model
  where
    gradientDescent' :: Model -> [Model]
    gradientDescent' m@ (theta0, theta1) = let
        mf = toModelF m
        theta0' = theta0 - alpha * sum' (linearCost mf)
        theta1' = theta1 - alpha * sum' (\t@ (x0, x1) -> x1 * linearCost mf t)
      in m : gradientDescent' (theta0', theta1')
    sum' :: (Row -> Double) -> Double
    sum' f = sum (map f data_) / length' data_
    linearCost :: ModelF -> Row -> Double
    linearCost mf (x, y) = mf x - y

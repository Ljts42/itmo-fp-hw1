module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay = undefined

afterDays :: Natural -> Day -> Day
afterDays = undefined

isWeekend :: Day -> Bool
isWeekend = undefined

daysToParty :: Day -> Natural
daysToParty = undefined

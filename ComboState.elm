module ComboState where

import Keys exposing (..)

type alias Combo = List Key

type ComboState a = Null | Partial (Key -> ComboState a) | Completed a


--handleKey : Key -> ComboState a -> ComboState a

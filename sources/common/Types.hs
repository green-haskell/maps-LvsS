module Types where

import qualified Data.Text.Lazy as DTL (
    Text ( .. )
    )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
type MapId = DTL.Text

type BenchmarkId = DTL.Text
type BenchmarkOutput = DTL.Text

type Key = Int
type Datum = Int



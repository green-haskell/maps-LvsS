module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified Data.List as DL (
      group
    , length
    , sort
    )

import qualified System.Directory as SD (
    copyFile
    )

import qualified System.Process as SP (
      CreateProcess ( .. )
    , StdStream ( .. )
    , callCommand
    , callProcess
    , createProcess
    , proc
    , readProcess
    , shell
    , showCommandForUser
    )

import qualified Data.List as DL (
      intersperse
    , isPrefixOf
    , stripPrefix
    )

import qualified Data.Text.Lazy as DTL (
      Text ( .. )
    , all
    , append
    , breakOn
    , dropWhile
    , concat
    --, intersperse
    , isPrefixOf
    , lines
    , null
    , pack
    , replace
    , split
    , splitOn
    , strip
    , stripStart
    , takeWhile
    , unpack
    )

import qualified Data.Text.Lazy.IO as DTLI (
      hGetContents
    , readFile
    , writeFile
    )

{-import qualified Text.CSV as TC (
    printCSV )
-}


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import Paths

import CommonFunctions

import Types

import UnitConv


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do
    SP.callCommand "cabal configure"
    
    SP.callCommand "cabal build runForAllMapImplementations"

    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc "./dist/build/runForAllMapImplementations/runForAllMapImplementations" [] ){ SP.std_out = SP.CreatePipe } )
    timeBenchmarkOutput <- DTLI.hGetContents hout
    DTLI.writeFile "delete-output.csv" timeBenchmarkOutput

    --print timeBenchmarkOutput


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



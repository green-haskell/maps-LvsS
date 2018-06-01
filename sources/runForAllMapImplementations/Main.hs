{-{-# LANGUAGE ScopedTypeVariables #-}-}


module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified Data.List as DL (
      group
    , length
    , sort
    )

import Data.Time

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
    , fromChunks
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

import qualified Data.Text.IO as DTI (
      hGetContents
    , readFile
    , writeFile
    )

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
import System.IO
-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import Paths

import CommonFunctions

import Types

import UnitConv


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do

    mapImplementations <- getMapImplementationsFromFile mapImplementationsFilePath
        :: IO [ MapId ]

    benchmarksGroupedByMapImplementation <- mapM getBenchmarksForMapImplementation mapImplementations
        :: IO [ [ BenchmarkId ] ]

    let
        benchmarksGroupedByBenchmark = groupHeads benchmarksGroupedByMapImplementation
            :: [ [ BenchmarkId ] ]

    benchmarksOutputsForAllMapImplementations <- mapM ( flip getBenchmarkOutputForAllMapImplementations mapImplementations ) benchmarksGroupedByBenchmark
        :: IO [ [ BenchmarkOutput ] ]
    --print benchmarksOutputsForAllMapImplementations


    -- Times

    timesOutputCSV <- processFeature "time" benchmarksOutputsForAllMapImplementations
    DTLI.writeFile "time-Time.csv" timesOutputCSV :: IO ()


    -- Energy

    packageEnergyOutputCSV <- processFeature "packageEnergy" benchmarksOutputsForAllMapImplementations
    DTLI.writeFile "packageEnergy-Energy.csv" packageEnergyOutputCSV :: IO ()

    dramEnergyOutputCSV <- processFeature "dramEnergy" benchmarksOutputsForAllMapImplementations
    DTLI.writeFile "dramEnergy-Energy.csv" dramEnergyOutputCSV :: IO ()


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

processFeature :: String -> [ [ BenchmarkOutput ] ] -> IO ( DTL.Text )
processFeature feature benchmarksOutputsForAllMapImplementations = do
    let
        values = case feature of
            "time" -> map ( map getTime ) benchmarksOutputsForAllMapImplementations
            --"energy" -> map ( map getEnergy ) benchmarksOutputsForAllMapImplementations
            "packageEnergy" -> map ( map getPackageEnergy ) benchmarksOutputsForAllMapImplementations
            "dramEnergy" -> map ( map getDRAMEnergy ) benchmarksOutputsForAllMapImplementations
                :: [ [ ( DTL.Text , Double , DTL.Text ) ] ]
    --print values
    
    let
        output = map ( map (\ (x , y ,z) -> [ x , DTL.pack ( show y ) , z] ) ) values
            :: [ [ [ DTL.Text ] ] ]

        outputCSV = DTL.pack . concat
            . (:) ( case feature of
                "time" -> ",Time,Units\n"
                --"energy" -> ",Energy,Units\n"
                "packageEnergy" -> ",PackageEnergy,Units\n"
                "dramEnergy" -> ",DRAMEnergy,Units\n"
                )
            . DL.intersperse "\n" . map ( concat . DL.intersperse "\n" . map ( concat . DL.intersperse "," . map show ) ) $ output
            :: DTL.Text
    return outputCSV


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getTime' function extracts the benchmark name, execution time and this time's units from the benchmark output.
        It's parameters are:
            a benchmark output
        It's output is:
            a triple ( benchmark identifier , time , time units )

-}

getTime :: BenchmarkOutput -> ( DTL.Text , Double , DTL.Text )
getTime bmOutput =
    let
        ls = take 2 . DTL.lines $ bmOutput
        bmName = DTL.append ( DTL.pack "Time/" ) . snd . DTL.breakOn ( DTL.pack "Map/" ) . head . take 1 $ ls
        tl = filter ( not . DTL.null ) . DTL.split ( DC.isSpace ) . head . drop 1 $ ls

        -- Here, 0 should probably be 1!
        snd_tl = if length tl > 0 then ( DTL.unpack ( tl !! 1 ) ) else error "tl, in getTime should have at least 2 elements: " ++ ( show tl )
        third_tl = if length tl > 2 then ( DTL.unpack ( tl !! 2 ) ) else error "tl, in getTime should have at least 3 elements: " ++ show tl
    in
        ( bmName , read ( snd_tl ) :: Double , DTL.pack third_tl )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getPackageEnergy' function extracts the benchmark name, energy consumption for the package and this value's units from the benchmark output.
        It's parameters are:
            a benchmark output
        It's output is:
            a triple ( benchmark identifier , energy , energy units )

-}

getPackageEnergy :: BenchmarkOutput -> ( DTL.Text , Double , DTL.Text )
getPackageEnergy bmOutput =
    let
        ls = take 7 . DTL.lines $ bmOutput
        bmName = DTL.append ( DTL.pack "PackageEnergyConsumption/" ) . snd . DTL.breakOn ( DTL.pack "Map/" ) . head . take 1 $ ls
        tl = filter ( not . DTL.null ) . DTL.split ( DC.isSpace ) . head . drop 6 $ ls

        -- Here, 0 should probably be 1!
        snd_tl = if length tl > 0 then ( DTL.unpack ( tl !! 1 ) ) else error "tl, in getPackageEnergy should have at least 2 elements: " ++ ( show tl )

    in
        ( bmName , read ( snd_tl ) :: Double , DTL.pack "j" ) -- NOTE: Use "secs" from Criterion?


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getDRAMEnergy' function extracts the benchmark name, DRAM energy consumption and this value's units from the benchmark output.
        It's parameters are:
            a benchmark output
        It's output is:
            a triple ( benchmark identifier , energy , energy units )

-}

getDRAMEnergy :: BenchmarkOutput -> ( DTL.Text , Double , DTL.Text )
getDRAMEnergy bmOutput =
    let
        ls = take 10 . DTL.lines $ bmOutput
        bmName = DTL.append ( DTL.pack "DRAMEnergyConsumption/" ) . snd . DTL.breakOn ( DTL.pack "Map/" ) . head . take 1 $ ls
        tl = filter ( not . DTL.null ) . DTL.split ( DC.isSpace ) . head . drop 9 $ ls

        -- Here, 0 should probably be 1!
        snd_tl = if length tl > 0 then ( DTL.unpack ( tl !! 1 ) ) else error "tl, in getDRAMEnergy should have at least 2 elements: " ++ ( show tl )

    in
        ( bmName , read ( snd_tl ) :: Double , DTL.pack "j" ) -- NOTE: Use "secs" from Criterion?


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The function 'getBenchmarksForMapImplementation' runs an (Criterion generated) executable for (the benchmarks of) a associative collection implementation, to list the benchmarks included in that executable.
        It's parameters are:
            a associative collection implementation identifier
        It's output is:
            a list of benchmark identifiers included in the executable

-}

getBenchmarksForMapImplementation :: MapId -> IO [ BenchmarkId ]
getBenchmarksForMapImplementation s = do
    let 
        benchmarkCommand = benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s
    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc benchmarkCommand [ "--list" ] ){ SP.std_out = SP.CreatePipe } )
    benchmarkCommandOutput <- DTLI.hGetContents hout

    return . DTL.lines $ benchmarkCommandOutput


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getBenchmarkOutputForAllMapImplementations' runs a specific benchmark for a list of associative collection implementations.
        It's parameters are:
            a list of benchmark identifiers (all being the same benchmark, but for different associative collection implementations)
            a list of associative collection implementations
        It's output is:
            a list of the output of the benchmarks

-}

getBenchmarkOutputForAllMapImplementations :: [ BenchmarkId ] -> [ MapId ] -> IO [ BenchmarkOutput ]
getBenchmarkOutputForAllMapImplementations bl sil =
    mapM (uncurry getBenchmarkOutputForMapImplementation ) $ zip bl sil


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |

        The 'getBenchmarkOutputForMapImplementation' function, runs a specific benchmark for one associative collection implementation.
        It's parameters are:
            a benchmark identifier;
            an associative collection implementation identifier.
        It's output is:
            the textual output from the benchmark.

-}

getBenchmarkOutputForMapImplementation :: BenchmarkId -> MapId -> IO BenchmarkOutput
getBenchmarkOutputForMapImplementation b s = do
    let
        benchmarkCommand = benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s
        benchmarkId = "" ++ DTL.unpack b ++ "" -- Note: nothing around!

    hPutStrLn stderr $ SP.showCommandForUser benchmarkCommand [ "--regress", "packageEnergy:iters" , "--regress", "dramEnergy:iters" , benchmarkId ]

    it <- getCurrentTime
    hPutStrLn stderr . (++) "Started: " . show $ it

    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc benchmarkCommand [ "--regress",  "packageEnergy:iters" , "--regress",  "dramEnergy:iters" , benchmarkId ] ){ SP.std_out = SP.CreatePipe } )
    benchmarkOutput <- DTI.hGetContents hout

    hPutStrLn stderr . DTL.unpack $ DTL.fromChunks [ benchmarkOutput ]

    ft <- getCurrentTime
    hPutStrLn stderr . (++) "Finished: " . show $ ft

    return ( DTL.fromChunks [ benchmarkOutput ])


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



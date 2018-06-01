module Main where


import qualified Control.Monad as CM (
    forM
    )

import qualified Data.List as DL (
      group
    , sort
    )

import qualified Data.Text.Lazy as DTL (
      Text ( .. )
    , all
    , append
    , concat
    , dropWhile
    , intercalate
    , isPrefixOf
    , lines
    , null
    , pack
    , replace
    , span
    , strip
    , stripStart
    , tail
    , unlines
    , unpack
    )

import qualified Data.Text.Lazy.IO as DTLI (
      readFile
    , writeFile
    )

import qualified System.Environment as SE (
    getArgs
    )

import Text.CSV

import Text.Parsec.Error


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import CommonFunctions

import Paths

import UnitConv


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

-- HARDWIRED

mapImplementations :: [ String ]
mapImplementations = [
      "Data.HashMap.Lazy"
    , "Data.HashMap.Strict"
    , "Data.IntMap.Lazy"
    , "Data.IntMap.Strict"
    , "Data.Map.Lazy"
    , "Data.Map.Strict"
    ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

convertUnitsToMostObservedUnit :: [ [ String ] ] -> String -> [ [ String ] ]
convertUnitsToMostObservedUnit valuesList unit =
    let
        lvup = zip valuesList ( repeat unit )
    in
        map (uncurry convUnits') lvup


convUnits' :: [ String ] -> String -> [ String ]
convUnits' [ si_bm , v , iu ] ou = [ si_bm , cv , ou ]
    where
        cv = show $ cvu ( ( read v ) ) iu ou

cvu :: Double -> String -> String -> Double
cvu v iuStr ouStr = convert v ( uStrToUnit iuStr ) ( uStrToUnit ouStr ) 


uStrToUnit "s" = Unit
uStrToUnit "ms" = Milli
--uStrToUnit "µs" = Micro
uStrToUnit "\956s" = Micro

-- NOTE:
uStrToUnit "\\956s" = Micro

uStrToUnit "ns" = Nano
uStrToUnit "ps" = Pico


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

readFiles :: String -> IO [ DTL.Text ]
readFiles feature = do
    let
        -- NOTE: Associative Collection Implementations HARDWIRED
        filenames = map ( (++) csvFileSetsFilesPath . flip (++) ( '.' : ( feature ++ ".csv" ) ) ) mapImplementations
            :: [ FilePath ]
    --print filenames
    
    filesContents <- mapM ( DTLI.readFile ) filenames
        :: IO ( [ DTL.Text ] )

    --print filesContents
    return filesContents


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

postProcessFeature feature = do
    filesContents <- readFiles feature
    let
        simplifiedFilesContentsAsLines = map (
            DTL.lines .
            DTL.replace ( DTL.pack (
                case feature of
                    "time" -> "Time/Map/Data."
                    --"energy" -> "EnergyConsumption/Map/Data.Edison.Assoc."
                    "packageEnergy" -> "PackageEnergyConsumption/Map/Data."
                    "dramEnergy" -> "DRAMEnergyConsumption/Map/Data."
                        ) ) ( DTL.pack "" ) ) $ filesContents
            :: [ [ DTL.Text ] ]

    --print simplifiedFilesContentsAsLines
    let
        ( headings : resultsGroupedByBenchmark ) = groupHeads simplifiedFilesContentsAsLines
            :: [ [ DTL.Text ] ]

        output = DTL.intercalate ( DTL.pack "\n\n" ) . map ( postProcessOperation feature . DTL.unpack . DTL.unlines ) $ resultsGroupedByBenchmark

    return output


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

postProcessOperation feature opCSVStr = --do
    let
        csv = filter ( (/=) [""] ) . ( \ ( Right rl ) -> rl ) . parseCSV "si" $ opCSVStr
            :: CSV -- [ Record ]
    --print csv

    --let
        mostObservedUnit = snd . head . reverse . DL.sort . rle . map ( flip (!!) 2 ) $ csv
            :: String
    --print mostObservedUnit

        ir1 = case feature of
                  "time" -> printCSV $ convertUnitsToMostObservedUnit csv mostObservedUnit
                  --"energy" -> printCSV csv
                  "packageEnergy" -> printCSV csv
                  "dramEnergy" -> printCSV csv

        ir2 = finishResults feature . DTL.lines . DTL.pack $ ir1

    in
        ir2

finishResults feature ir1 =
    ggg ( DTL.pack ( case feature of
                         "time" -> ",Time,Unit"
                         "packageEnergy" -> ",PackageEnergy,Unit"
                         "dramEnergy" -> ",DRAMEnergy,Unit" )
                         --"energy" -> ",Energy,Unit" )
                    )
    . fff
    . unzip
    . map (
        --
        ( \ ( s , ( b , r ) ) -> ( b , DTL.append s r ) )
        -- mantain the mapID and separate the benchmarkId from the results
        . (><) id ( DTL.span ( (/=) '\"' ) . DTL.tail )
        -- separate the mapID from the rest
        . DTL.span ( (/=) '/' )
        )
        $ ir1


fff :: ( [ b ], [ DTL.Text ] ) -> ( b , DTL.Text )
fff = (><) head ( DTL.intercalate ( DTL.pack "\n" ) )


ggg :: DTL.Text -> ( DTL.Text , DTL.Text ) -> DTL.Text
ggg h ( b , ss ) = DTL.intercalate ( DTL.pack "\n" ) $ [ b , h , ss ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do
    --args <- SE.getArgs

    timesResults <- postProcessFeature "time"
    DTLI.writeFile ( "times.csv" ) timesResults

    {-
    energyResults <- postProcessFeature "energy"
    DTLI.writeFile ( "energy.csv" ) energyResults
    -}

    packageEnergyResults <- postProcessFeature "packageEnergy"
    DTLI.writeFile ( "packageEnergy.csv" ) packageEnergyResults

    dramEnergyResults <- postProcessFeature "dramEnergy"
    DTLI.writeFile ( "dramEnergy.csv" ) dramEnergyResults

    return ()


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        Sort Of: Run-length encoding (It's sorts elements first!)

-}

rle :: ( Eq a , Ord a ) => [ a ] -> [ ( Int , a ) ]
rle = map ( \ ul -> ( length ul , head ul ) ) . DL.group . DL.sort


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



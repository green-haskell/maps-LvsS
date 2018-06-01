module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified System.Directory as SD (
    copyFile
    )

import qualified System.Process as SP (
    callCommand
    )

import qualified Data.List as DL (
      isPrefixOf
    , isSuffixOf
    , stripPrefix
    )
{-
import qualified Data.Text as DT (
      Text ( .. )
    , pack
    , replace
    , unpack
    )

import qualified Data.Text.IO as DTI (
      readFile
    , writeFile
    )
-}
import qualified Data.Text.Lazy as DTL (
      Text ( .. )
    , all
    , dropWhile
    , isPrefixOf
    , lines
    , null
    , pack
    , replace
    , strip
    , stripStart
    , unpack
    )

import qualified Data.Text.Lazy.IO as DTLI (
      readFile
    , writeFile
    )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

--import Defs

import CommonFunctions

import Paths



-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do
    mapImplementations <- getMapImplementationsFromFile mapImplementationsFilePath
    mapM_ ( \ a -> sequence_ [
            --patchAllFilesForMapImplementation a ( sourceFilesToPatchForBenchmark ++ commonSourceFilesToPatch )
-- TODO: Check if it works!
            {-, -}SP.callCommand "cabal configure"

            , SP.callCommand "cabal build benchmarkForOneMapImpl"
            , SD.copyFile benchmarkExecutableFilePath ( benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack a )

            , SP.callCommand "cabal clean"
            ]
        ) mapImplementations

-- Atenção: Sem o "cabal clean" existe um erro aleatório (não compila um, ou seja produz 2 executáveis iguais)
-- Junto com o Professor chegámos à conclusão que o problema tem a ver com a "resolução do relógio" usado pela cabal


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
{-
patchFileForMapImplementation :: DTL.Text -> FilePath -> IO ()
patchFileForMapImplementation a f = do
    fileContents <- DTLI.readFile f
    let
        iOutput1 = DTL.replace ( DTL.pack "<MapImplementation>" ) a fileContents

        -- Ugly Hack!
        searchStr = "instance NFData a => NFData ( A.FM k a )"
        output = if ( ( a == DTL.pack "Data.Edison.Assoc.StandardMap" ) && ( DL.isSuffixOf "Ops.hs.template" f ) ) then DTL.replace ( DTL.pack searchStr ) ( DTL.pack ( "--" ++ searchStr ) ) iOutput1 else iOutput1


        outputFile = reverse . maybe [] id . DL.stripPrefix ( reverse ".template" ) . reverse $ f
    DTLI.writeFile outputFile output

-}
-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
{-
patchAllFilesForMapImplementation :: DTL.Text -> [ FilePath ] -> IO ()
patchAllFilesForMapImplementation a l = mapM_ ( patchFileForMapImplementation a ) l
-}

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



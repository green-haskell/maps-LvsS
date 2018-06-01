module Paths where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

mapImplementationsFilePath :: String
mapImplementationsFilePath = "mapImplementations.txt"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

--benchmarkSourceFilesPath = "sources/benchmarkForOneMapImpl/"

--sourceFilesToPatchForBenchmark = [] -- map ( (++) benchmarkSourceFilesPath) [ "Main.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkExecutableFilePath :: String
benchmarkExecutableFilePath = "dist/build/benchmarkForOneMapImpl/benchmarkForOneMapImpl"

benchmarkExecutableBaseName :: String
benchmarkExecutableBaseName = "benchmarkForOneMapImpl-"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commonSourceFilesPath :: FilePath
commonSourceFilesPath = "sources/common/"

--commonSourceFilesToPatch :: [ FilePath ]
--commonSourceFilesToPatch = [] -- map ( (++) commonSourceFilesPath ) [ "Environments.hs.template", "Ops.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarksExecutablesDestinationPath :: FilePath
benchmarksExecutablesDestinationPath = "tmp/executables/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

runForAllMapImplementationsSourceFilesPath :: FilePath
runForAllMapImplementationsSourceFilesPath ="sources/runForAllMapImplementations/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

csvFileSetsFilesPath :: FilePath
csvFileSetsFilesPath = "csvFileSets/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



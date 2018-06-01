module Defs where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

-- Benchmarks operations configurations


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        Number of elements to populate a data structure with, as a base for the benchmark operations


    -}

baseNElems :: Int
baseNElems = 100000


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        addToNElems - number of elements in a map, to add to

        addFromNElems - number of elements to add to a map for the add benchmark operation


    -}

addToNElems :: Int
addToNElems = baseNElems

addFromNElems :: Int
addFromNElems = 100000

addOpDesc   :: String
addOpDesc   = "add_" ++ show addFromNElems ++ "_elements_to_" ++ show addToNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        addAllFromNElems - number of elements in a map, to add to another map, for the addAll operation

        addAllNRepeats - number of repetitions for the addAll operation


    -}

addAllToNElems :: Int
addAllToNElems = baseNElems

addAllFromNElems :: Int
addAllFromNElems  = 1000 -- Default

addAllNRepeats :: Int
addAllNRepeats = 1000  -- Default

addAllOpDesc :: String
addAllOpDesc    = "addAll_" ++ show addAllNRepeats ++ "_times_" ++ show addAllFromNElems ++ "_elements_to_" ++ show addAllToNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        clearNElems - number of elements in a data structure, which will be cleared
            20000 - takes 8 minutes


    -}

clearNElems :: Int
--clearNElems = baseNElems -- 10h20min (server)
--clearNElems = 25000 -- 15 Minutes -- TODO: Increase? Yes.
--clearNElems = 37500 -- 35 minutes
--clearNElems = 43750 -- ~1h
clearNElems = 50000 -- 2h10min (server)

clearOpDesc :: String
clearOpDesc = "clear_" ++ show clearNElems ++ "_elements"


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        containsElement - element to search for, in the contains operation

        containsNRepeats - number of repetitions for the contains operation


    -}

containsNRepeats  :: Int
containsNRepeats = 1000 -- Default

containsElement :: Int
containsElement = 9999999

containsOpDesc :: String
containsOpDesc    = "contains_" ++ show containsElement ++ "_" ++ show containsNRepeats ++ "_times"


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        containsAllSearchInNElems - the number of elements in the data structure to search in

        containsAllSearchForNElems - the number of elements in the data structure; we'll check if all these are contained in another data structure

        containsAllNRepeats - number of repetitions for the containsAll operation

        containsAllOpDesc - textual description of the benchmark operation

        With 100000 in base: 20 Minutes


    -}

containsAllSearchInNElems :: Int
containsAllSearchInNElems = baseNElems

containsAllSearchForNElems :: Int
containsAllSearchForNElems = 1000 -- TODO: Not specified!

containsAllNRepeats :: Int
containsAllNRepeats = 5000 -- Default

containsAllOpDesc :: String
containsAllOpDesc = "containsAll_" ++ show containsAllNRepeats ++ "_times_for_" ++ show containsAllSearchForNElems ++ "_in_" ++ show containsAllSearchInNElems ++ ""


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        iterate through the (base) data structure


    -}

--iteratorForNElems :: Int
--iteratorForNElems = baseNElems

iteratorOpDesc :: String
iteratorOpDesc = "iterator_" ++ show baseNElems ++ "_elements"


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        removeNElems - number of elements to remove From, for the remove operation

        removeFromNElems - number of elements in the data structure to remove from


    -}

removeNRepeats :: Int
--removeNRepeats = 10000 -- Default!
--removeNRepeats = 1000
removeNRepeats = 2500

removeFromNElems :: Int
--removeFromNElems = baseNElems -- >6h30min (server)
removeFromNElems = 3125

removeOpDesc :: String
removeOpDesc = "remove_" ++ show removeNRepeats ++ "_times_from_" ++ show removeFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        removeAllNElems - number of elements to remove from a map, for the removeAll operation

            40000 -- Works! 8-10 minutes

        removeAllNRepeats - number of repetitions for the removeAll operation

        With 100000 base, 30 Minutes

    -}


removeAllFromNElems :: Int
removeAllFromNElems = baseNElems

removeAllNElems :: Int
removeAllNElems  = 1000 -- Default

removeAllNRepeats :: Int
removeAllNRepeats = 10 -- Default

removeAllOpDesc :: String
removeAllOpDesc = "removeAll_" ++ show removeAllNRepeats ++ "_times_" ++ show removeAllNElems ++ "_elements_from_" ++ show removeAllFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        retainAllFromNElems - number of elements of the data structure to remove from
            10000 (and default values for the rest) completes in 6 minutes

        retainAllToNElems - number of elements on the data structure to retain

        retainAllNRepeats - number of repetitions of the retainAll operation


    -}

retainAllFromNElems :: Int
--retainAllFromNElems = baseNElems
retainAllFromNElems = 25000 -- ~ 1h30m

retainAllToNElems :: Int
retainAllToNElems = 1000 -- TODO: Not specified!

retainAllNRepeats :: Int
retainAllNRepeats = 10 -- Default

retainAllOpDesc :: String
retainAllOpDesc    = "retainAll_" ++ show retainAllNRepeats ++ "_times_" ++ show retainAllToNElems ++ "_elems_from_" ++ show retainAllFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        toListFromNElems - number of elements present in the data structure

        toListNRepeats - number of repetitions for the toList operation


    -}

toListFromNElems :: Int
--toListFromNElems = baseNElems
toListFromNElems = 3125 -- ~ 2h30m

toListNRepeats :: Int
--toListNRepeats = 5000 -- Default
toListNRepeats = 2500

toListOpDesc :: String
toListOpDesc    = "toList_" ++ show toListNRepeats ++ "_times_from_" ++ show toListFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



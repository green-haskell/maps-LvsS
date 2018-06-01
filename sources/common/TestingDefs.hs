module Defs where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

-- Benchmarks operations configurations


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        Number of elements to populate a data structure with, as a base for the benchmark operations


    -}

baseNElems :: Int
baseNElems = 10


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        addToNElems - number of elements in a map, to add to

        addFromNElems - number of elements to add to a map for the add benchmark operation


    -}

addToNElems :: Int
addToNElems = baseNElems

addFromNElems :: Int
addFromNElems = 1

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
addAllFromNElems  = 1

addAllNRepeats :: Int
addAllNRepeats = 1

addAllOpDesc :: String
addAllOpDesc    = "addAll_" ++ show addAllNRepeats ++ "_times_" ++ show addAllFromNElems ++ "_elements_to_" ++ show addAllToNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        clearNElems - number of elements in a data structure, which will be cleared
            20000 - takes 8 minutes


    -}

clearNElems :: Int
clearNElems = baseNElems

clearOpDesc :: String
clearOpDesc = "clear_" ++ show clearNElems ++ "_elements"


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        containsElement - element to search for, in the contains operation

        containsNRepeats - number of repetitions for the contains operation


    -}

containsNRepeats  :: Int
containsNRepeats = 1

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


    -}

containsAllSearchInNElems :: Int
containsAllSearchInNElems = baseNElems

containsAllSearchForNElems :: Int
containsAllSearchForNElems = 1

containsAllNRepeats :: Int
containsAllNRepeats = 4

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
removeNRepeats = 1

removeFromNElems :: Int
removeFromNElems = baseNElems

removeOpDesc :: String
removeOpDesc = "remove_" ++ show removeNRepeats ++ "_times_from_" ++ show removeFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        removeAllNElems - number of elements to remove from a map, for the removeAll operation

            40000 -- Works! 8-10 minutes

        removeAllNRepeats - number of repetitions for the removeAll operation


    -}


removeAllFromNElems :: Int
removeAllFromNElems = baseNElems

removeAllNElems :: Int
removeAllNElems  = 1

removeAllNRepeats :: Int
removeAllNRepeats = 1 -- Default

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
retainAllFromNElems = baseNElems

retainAllToNElems :: Int
retainAllToNElems = 1

retainAllNRepeats :: Int
retainAllNRepeats = 1 -- Default

retainAllOpDesc :: String
retainAllOpDesc    = "retainAll_" ++ show retainAllNRepeats ++ "_times_" ++ show retainAllToNElems ++ "_elems_from_" ++ show retainAllFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------

    {-

        toListFromNElems - number of elements present in the data structure

        toListNRepeats - number of repetitions for the toList operation


    -}

toListFromNElems :: Int
toListFromNElems = baseNElems

toListNRepeats :: Int
toListNRepeats = 5

toListOpDesc :: String
toListOpDesc    = "toList_" ++ show toListNRepeats ++ "_times_from_" ++ show toListFromNElems


    -- ---------- ---------- ---------- ---------- ---------- ----------


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------



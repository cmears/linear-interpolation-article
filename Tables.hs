module Tables where

import qualified Data.Map as M
import Template
import Text.Printf

-- Switch the comments to select the small or big table.
--sinTable = [ (x, sin x) | x <- [0.0, 0.1 .. 10.0 ] ]
sinTable = [ (x, sin x) | x <- [0.0, 0.5 .. 10.0 ] ]

sinMap = M.fromList sinTable
sinTree = makeTree sinTable

targetFront = 0.0005
targetMiddle = 5.1
targetBack = 9.95

printTable :: [(Double,Double)] -> IO ()
printTable = mapM_ (\(a,b) -> printf "(%.2f, %.2f)\n" a b)

littleTable :: [(Double,Double)]
littleTable = [ (1,1), (2,4), (3,9) ]

module Generator where

import Setup
import Presentation


createEmptyGrid :: Grid
createEmptyGrid = [ map numberToEmptyValue columns | r <- rows ]

-- normalBlocks :: [[Int]]
-- normalBlocks = [[1..3],[4..6],[7..9]]

-- convertGridToBlocks :: Grid -> [[Value]]
-- convertGridToBlocks _ =

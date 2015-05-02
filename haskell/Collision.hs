module Collision where

cellCollision :: KissCell -> KissCell -> Bool
cellCollision cell1 cell2 
    | cell1.mapped || cell1.visible 
    | cell1.x >= cell2.x + cell2.width  = False
    | cell1.x + cell2.width <= cell2.x  = False
    | cell1.y >= cell2.y + cell2.height = False
    | cell1.y + cell1.height <= cell2.y = False
    | otherwise                         = True
    

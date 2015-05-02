module Kiss where

-- this will be structs and stuff

type SetCoords = [ (Int, Int) ] 

getx :: Int -> SetCoords -> Int
getx set sets = fst (sets !! set)

gety :: Int -> SetCoords -> Int
getx set sets = snd (sets !! set)


data KissCell = KissCell { filename :: Filename,
                           buffers  :: ImageBuffer,
                           width    :: Int,
                           height   :: Int,
                           coords   :: SetCoords,
                           offx     :: Int,
                           offy     ::  

-- Haskell Inverse Kinematics Test
--
--References:
--
--Coding Math: Episode 43 - Kinematics Part I   -> https://www.youtube.com/watch?v=WHn2ouKKSfs
--Coding Math: Episode 44 - Kinematics Part II  -> https://www.youtube.com/watch?v=4oCo1j8xGew&t=1s
--Coding Math: Episode 45 - Kinematics Part III -> https://www.youtube.com/watch?v=sEKNoWyKUA0
--Coding Math: Episode 46 - Kinematics Part IV  -> https://www.youtube.com/watch?v=7t54saw9I8k&t=35s
--
--http://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Data-Vector.html
--http://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Data-Point-Arithmetic.html
--http://hackage.haskell.org/package/gloss-1.13.0.1/docs/src/Graphics.Gloss.Data.Point.Arithmetic.html
--
-- written by Leonardo Ono (ono.leo@gmail.com)
-- April 22, 2020
--
-- GHC version 8.6.5
-- gloss-1.13.1.1

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA

ik :: Float -> [Point] -> Point -> [Point]
ik _ [_] target = [target]
ik fixedSegmentSize (x:xs) target = target : ik fixedSegmentSize xs newTarget
    where   secondPoint = head xs
            direction = fixedSegmentSize PA.* (normalizeV $ secondPoint PA.- target)
            newTarget = target PA.+ direction

main :: IO ()
main = play window background fps initialState render handleInput update
    where   window = InWindow "Haskell Inverse Kinematics Test" (800, 600) (100, 100)
            background = white
            fps = 60

            segmentSize = 40
            initialState = ((0, 0), replicate 7 (1, 1))

            render (_, points) = line points

            handleInput (EventMotion m) (_, p) = (m, p)
            handleInput _ state = state

            --update seconds (target, points) = (target, 
            --    reverse $ ik segmentSize (reverse $ ik segmentSize points target) (0, 0))

            update seconds (target, points) = (target, ik segmentSize points target)

            
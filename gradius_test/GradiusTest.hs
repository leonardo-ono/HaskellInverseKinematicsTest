import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA

ik :: [Point] -> Point -> [Point]
ik [_] target = [target]
ik (x:xs) target = target : ik xs newTarget
    where   secondPoint = head xs
            distance = magV $ x PA.- secondPoint
            direction = distance PA.* (normalizeV $ secondPoint PA.- target)
            newTarget = target PA.+ direction

main :: IO ()
main = do
    background <- loadBMP "background.bmp"
    ship <- loadBMP "ship.bmp"
    optionPowerUp <- loadBMP "option_power_up.bmp"

    play window backgroundColor fps initialState (render background ship optionPowerUp) handleInput update
    where   window = InWindow "Haskell Inverse Kinematics - Gradius Option Power-Up Test" (640, 480) (100, 100)
            backgroundColor = black
            fps = 60
            initialState = ((0, 0), [(x * 4, x * 4) | x <- [0..30]])

            render background ship optionPowerUp (target, points) = pictures 
                [ background
                , color white $ line points
                , translate x3 y3 $ scale 2 2 $ optionPowerUp
                , translate x2 y2 $ scale 2 2 $ optionPowerUp
                , translate x1 y1 $ scale 2 2 $ optionPowerUp
                , translate x0 y0 $ scale 2 2 $ ship]
                where   (x0, y0) = points !! 0
                        (x1, y1) = points !! 10
                        (x2, y2) = points !! 20
                        (x3, y3) = points !! 30

            handleInput (EventMotion (x, y)) (_, p) = ((x + 20, y + 20), p)
            handleInput _ x = x

            update seconds (target, points) = (target, ik points target)

            
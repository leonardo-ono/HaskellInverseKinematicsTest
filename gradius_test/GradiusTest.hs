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

            render background ship optionPowerUp (target, points) = pictures $
                [ background, color black $ line points] ++
                [ uncurry translate (points !! index) $ scale 2 2 $ sprite
                    | (index, sprite) <- [  (30, optionPowerUp), 
                                            (20, optionPowerUp), 
                                            (10, optionPowerUp), 
                                            (0, ship) ] ]

            handleInput (EventMotion (x, y)) (_, p) = ((x + 20, y + 20), p)
            handleInput _ x = x

            update seconds (target, points) = (target, ik points target)

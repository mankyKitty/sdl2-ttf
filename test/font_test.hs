import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL     as SDL

main =
    SDL.withInit [SDL.InitEverything] $
    TTF.withInit $
    SDL.withWindow "test" (SDL.Position 100 100) (SDL.Size 640 480) [SDL.WindowShown] $ \window ->
    SDL.withRenderer window (SDL.Device (-1)) [SDL.Accelerated] $ \renderer -> do
      font <- TTF.openFont "/usr/share/fonts/truetype/DroidSans.ttf" 13
      textSurface <- TTF.renderTextSolid font "some text" (SDL.Color 255 255 255 0)
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      loop window renderer textTexture

loop window renderer textTexture = do
    SDL.renderClear renderer
    SDL.renderCopy renderer textTexture (Just $ SDL.Rect 0 0 640 480) (Just $ SDL.Rect 0 0 640 480)
    SDL.renderPresent renderer

    handleEvents window renderer textTexture

handleEvents window renderer textTexture = do
    mbEvent <- SDL.pollEvent
    case fmap SDL.eventData mbEvent of
      Just SDL.Quit -> return ()
      _ -> loop window renderer textTexture



import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL     as SDL

main =
    SDL.withInit [SDL.InitEverything] $
    TTF.withInit $
    SDL.withWindow "test" (SDL.Position 100 100) (SDL.Size 640 480) [SDL.WindowShown] $ \window ->
    SDL.withRenderer window (SDL.Device (-1)) [SDL.Software] $ \renderer -> do
      font <- TTF.openFont "/usr/share/fonts/truetype/DroidSans.ttf" 13
      textSurface <- TTF.renderTextSolid font "some text" (SDL.Color 255 255 255 0)
      loop window renderer textSurface

loop window renderer textSurface = do
    SDL.renderClear renderer
    winSurface <- SDL.getWindowSurface window
    SDL.blitSurface textSurface (SDL.Rect 0 0 500 500) winSurface (SDL.Rect 0 0 500 500)
    SDL.renderPresent renderer

    handleEvents window renderer textSurface

handleEvents window renderer textSurface = do
    mbEvent <- SDL.pollEvent
    case fmap SDL.eventData mbEvent of
      Just SDL.Quit -> return ()
      _ -> loop window renderer textSurface



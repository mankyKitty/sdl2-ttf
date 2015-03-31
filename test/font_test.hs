module Main where

import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL     as SDL

import Foreign.C.String (withCAString)
import Foreign (peek,alloca,with,maybePeek,nullPtr)

arial :: String
arial = "./test/ARIAL.TTF"

main :: IO ()
main = do
    _ <- SDL.init SDL.SDL_INIT_VIDEO
    window <- createWindow
    renderer <- createRenderer window
    
    TTF.withInit $ do
      font <- TTF.openFont arial 150 -- Pt size for retina screen. :<
      textSurface <- TTF.renderUTF8Solid font "some text" (SDL.Color 255 255 255 0)
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      SDL.freeSurface textSurface
      loop window renderer textTexture
      
      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit

createRenderer :: SDL.Window -> IO (SDL.Renderer)
createRenderer w = SDL.createRenderer w (-1) 0

createWindow :: IO (SDL.Window)
createWindow = withCAString "test" $ \t ->
      SDL.createWindow t 
        SDL.SDL_WINDOWPOS_UNDEFINED 
        SDL.SDL_WINDOWPOS_UNDEFINED 
        640 480 
        SDL.SDL_WINDOW_SHOWN

loop :: t -> SDL.Renderer -> SDL.Texture -> IO ()
loop window renderer textTexture = do
    let loc = SDL.Rect 320 240 150 100
    _ <- SDL.renderClear renderer
    _ <- with loc $ \loc' ->
             SDL.renderCopy renderer textTexture nullPtr loc'
    SDL.renderPresent renderer

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \ptr -> do
  status <- SDL.pollEvent ptr
  if status == 1
    then maybePeek peek ptr
    else return Nothing


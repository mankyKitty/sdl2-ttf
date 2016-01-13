{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL.Raw             as Raw
import qualified SDL as SDL

--import Foreign.C.String (withCAString)
--import Foreign (peek,alloca,with,maybePeek,nullPtr)

import Linear.V2
import Linear.Affine (Point(..))

arial :: String
arial = "./examples/ARIAL.TTF"

main :: IO ()
main = do
    _ <- SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Test" SDL.defaultWindow
    renderer <- SDL.createRenderer window 0 SDL.defaultRenderer

    TTF.withInit $ do
      inited <- TTF.wasInit
      if not inited then error "[Bug] Font system not initialized" else return ()
      font <- TTF.openFont arial 150 -- Pt size for retina screen. :<
      textSurface <- TTF.renderUTF8Solid font "some text" (Raw.Color 255 255 255 0)
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      SDL.freeSurface textSurface
      loop window renderer textTexture

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit

--createRenderer :: SDL.Window -> IO (SDL.Renderer)
--createRenderer w = SDL.createRenderer w (-1) 0

{-
createWindow :: IO (SDL.Window)
createWindow = withCAString "test" $ \t ->
      SDL.createWindow t
        SDL.SDL_WINDOWPOS_UNDEFINED
        SDL.SDL_WINDOWPOS_UNDEFINED
        640 480
        SDL.SDL_WINDOW_SHOWN
-}

loop :: t -> SDL.Renderer -> SDL.Texture -> IO ()
loop window renderer textTexture = do
    let loc = SDL.Rectangle (P $ V2 320 240) (V2 150 100)
    SDL.clear renderer
    SDL.copy renderer textTexture Nothing (Just loc)
    SDL.present renderer
    handleEvents window renderer textTexture

{-
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \ptr -> do
  status <- SDL.pollEvent ptr
  if status == 1
    then maybePeek peek ptr
    else return Nothing
-}

handleEvents :: t -> SDL.Renderer -> SDL.Texture -> IO ()
handleEvents window renderer textTexture = do
  mbEvent <- SDL.pollEvent
  case mbEvent of
    Just (SDL.Event _ SDL.QuitEvent) -> return ()
    _ -> loop window renderer textTexture

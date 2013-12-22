{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Exit
import Control.Monad.State.Strict

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.Types as TTF


data Text = Text String Color !Int

screenSize = Size 640 480
screenPosition = Position 100 100
screenTitle = "Print events"

main =
    SDL.withInit [InitEverything] $
    Image.withInit [InitPNG] $
    TTF.withInit $
    withWindow screenTitle screenPosition screenSize [WindowShown] $ \win ->
    withRenderer win (Device (-1)) [Accelerated] $ \renderer -> do
      font <- TTF.openFont "/usr/share/fonts/truetype/DroidSans.ttf" 13
      TTF.setFontStyle font TTF.TTFBold
      time <- getTicks
      evalStateT (mainLoop win renderer time font) []

handleEvents = do
    mbEvent <- liftIO $ pollEvent
    case fmap eventData mbEvent of
      Nothing -> return ()
      Just Quit ->
        liftIO exitSuccess
      Just Keyboard{keyMovement=KeyDown, keyRepeat, keySym} ->
        keypressed keyRepeat keySym >> handleEvents
      Just Keyboard{keyMovement=KeyUp, keyRepeat, keySym} ->
        keyreleased keyRepeat keySym >> handleEvents
      Just MouseButton{mouseButton, mouseButtonState=Pressed, mouseButtonAt} ->
        mousepressed mouseButton mouseButtonAt >> handleEvents
      Just MouseButton{mouseButton, mouseButtonState=Released, mouseButtonAt} ->
        mousereleased mouseButton mouseButtonAt >> handleEvents
      _ ->
        handleEvents

mainLoop win renderer time font = do
    handleEvents

    time' <- liftIO getTicks
    let dt = time' - time
    update dt

    texts <- get
    liftIO $ do
      renderClear renderer
      draw font renderer texts
      renderPresent renderer

    mainLoop win renderer time' font

keypressed :: Bool -> Keysym -> StateT [Text] IO ()
keypressed repeat keysym = when repeat (modify (Text ("Key pressed: " ++ show keysym) (Color 255 255 255 0) 2000 :))

keyreleased :: Bool -> Keysym -> StateT [Text] IO ()
keyreleased _ _ = return ()

mousepressed :: MouseButton -> Position -> StateT [Text] IO ()
mousepressed _ _ = return ()

mousereleased :: MouseButton -> Position -> StateT [Text] IO ()
mousereleased _ _ = return ()

update dt =
    modify $ \s ->
      filter (\(Text _ _ t) -> t > 0) $
        map (\(Text str color timeLeft) -> Text str color (timeLeft - fromIntegral dt)) s

draw font renderer ts = iter ts 50 50
  where
    iter [] _ _ = return ()
    iter (Text str color _ : ts) x y = do
      textSurface <- TTF.renderUTF8Solid font str color
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      (width, height) <- TTF.sizeText font str
      renderCopy renderer textTexture Nothing (Just $ Rect x y width height)
      iter ts x (y + height + 10)

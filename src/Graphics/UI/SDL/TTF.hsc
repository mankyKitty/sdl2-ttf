module Graphics.UI.SDL.TTF where

import Foreign.C.String
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Control.Monad
import Data.Int

import Graphics.UI.SDL.TTF.FFI (TTFFont)

import qualified Graphics.UI.SDL.TTF.FFI as FFI
import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.Types

import Prelude hiding (init)

init :: IO CInt
init = FFI.init

quit :: IO ()
quit = FFI.quit

withInit :: IO a -> IO a
withInit a = do
  ret <- init >> a
  quit
  return ret

openFont :: String -> Int -> IO TTFFont
openFont file ptsize = withCString file $ \cstr ->
    FFI.openFont cstr (fromIntegral ptsize)
    
closeFont :: TTFFont -> IO ()
closeFont fontPtr = FFI.closeFont fontPtr

openFontIndex :: String -> Int -> Int -> IO TTFFont
openFontIndex file ptsize index = withCString file $ \cstr -> 
  FFI.openFontIndex cstr (fromIntegral ptsize) (fromIntegral index)

getFontStyle :: TTFFont -> IO TTFStyle
getFontStyle fontPtr = liftM (toEnum . fromIntegral) $ FFI.getFontStyle fontPtr

setFontStyle :: TTFFont -> TTFStyle -> IO ()
setFontStyle fontPtr style = FFI.setFontStyle fontPtr (fromIntegral $ fromEnum style)

getFontHinting :: TTFFont -> IO TTFHinting
getFontHinting fontPtr = liftM (toEnum . fromIntegral) $ FFI.getFontHinting fontPtr

setFontHinting :: TTFFont -> TTFHinting -> IO ()
setFontHinting fontPtr hinting = FFI.setFontHinting fontPtr (fromIntegral $ fromEnum hinting)

getFontHeight :: TTFFont -> IO Int
getFontHeight fontPtr = liftM fromIntegral $ FFI.getFontHeight fontPtr

getFontAscent :: TTFFont -> IO Int
getFontAscent fontPtr = liftM fromIntegral $ FFI.getFontAscent fontPtr

getFontDescent :: TTFFont -> IO Int
getFontDescent fontPtr = liftM fromIntegral $ FFI.getFontDescent fontPtr

getFontKerning :: TTFFont -> IO Int
getFontKerning fontPtr = liftM fromIntegral $ FFI.getFontKerning fontPtr

setFontKerning :: TTFFont -> Int -> IO ()
setFontKerning fontPtr i = FFI.setFontKerning fontPtr (fromIntegral i)

fontFaces :: TTFFont -> IO Int64
fontFaces fontPtr = liftM fromIntegral $ FFI.fontFaces fontPtr

fontFaceIsFixedWidth :: TTFFont -> IO Bool
fontFaceIsFixedWidth fontPtr = liftM (== 0) $ FFI.fontFaceIsFixedWidth fontPtr

fontFaceFamilyName :: TTFFont -> IO String
fontFaceFamilyName fontPtr = FFI.fontFaceFamilyName fontPtr >>= peekCString

fontFaceStyleName :: TTFFont -> IO String
fontFaceStyleName fontPtr = FFI.fontFaceStyleName fontPtr >>= peekCString

peekInts 
  :: (FFI.TTFFont -> CString -> Ptr CInt -> Ptr CInt -> IO CInt)
  -> TTFFont
  -> String
  -> IO (Int,Int)
peekInts fn fontPtr text = do
    alloca $ \wPtr ->
      alloca $ \hPtr -> do
        -- TODO: handle errors
        void $ withCString text $ \cstr -> fn fontPtr cstr wPtr hPtr
        w <- peek wPtr
        h <- peek hPtr
        return (fromIntegral w, fromIntegral h)

sizeText :: TTFFont -> String -> IO (Int, Int)
sizeText = peekInts FFI.sizeText

sizeUTF8 :: TTFFont -> String -> IO (Int, Int)
sizeUTF8 = peekInts FFI.sizeUTF8

sizeUNICODE :: TTFFont -> String -> IO (Int, Int)
sizeUNICODE = peekInts FFI.sizeUNICODE

renderTextSolid :: TTFFont -> String -> Color -> IO (Ptr Surface)
renderTextSolid fontPtr text fg = withCString text $ \cstr -> do
    with fg $ \colorPtr -> FFI.renderTextSolid fontPtr cstr colorPtr

renderTextShaded :: TTFFont -> String -> Color -> Color -> IO (Ptr Surface)
renderTextShaded fontPtr text fg bg = withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        FFI.renderTextShaded fontPtr cstr fgColorPtr bgColorPtr

renderTextBlended :: TTFFont -> String -> Color -> IO (Ptr Surface)
renderTextBlended fontPtr text color = withCString text $ \cstr ->
    with color $ \colorPtr -> FFI.renderTextBlended fontPtr cstr colorPtr

renderUTF8Solid :: TTFFont -> String -> Color -> IO (Ptr Surface)
renderUTF8Solid fontPtr text fg = withCString text $ \cstr -> do
    with fg $ \colorPtr -> FFI.renderUTF8Solid fontPtr cstr colorPtr

renderUTF8Shaded :: TTFFont -> String -> Color -> Color -> IO (Ptr Surface)
renderUTF8Shaded fontPtr text fg bg = withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        FFI.renderUTF8Shaded fontPtr cstr fgColorPtr bgColorPtr

renderUTF8Blended :: TTFFont -> String -> Color -> IO (Ptr Surface)
renderUTF8Blended fontPtr text color = withCString text $ \cstr ->
    with color $ \colorPtr -> FFI.renderUTF8Blended fontPtr cstr colorPtr


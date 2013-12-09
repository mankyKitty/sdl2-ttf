module Graphics.UI.SDL.TTF where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad
import Data.Int

import Graphics.UI.SDL.TTF.FFI as FFI
import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Video (mkFinalizedSurface)

openFont :: String -> Int -> IO TTFFont
openFont file ptsize = withCString file $ \cstr -> liftM TTFFont $ FFI.openFont cstr (fromIntegral ptsize)

openFontIndex :: String -> Int -> Int -> IO TTFFont
openFontIndex file ptsize index = withCString file $ \cstr -> liftM TTFFont $ FFI.openFontIndex cstr (fromIntegral ptsize) (fromIntegral index)

getFontStyle :: TTFFont -> IO TTFStyle
getFontStyle (TTFFont fontPtr) = liftM (toEnum . fromIntegral) $ FFI.getFontStyle fontPtr

setFontStyle :: TTFFont -> TTFStyle -> IO ()
setFontStyle (TTFFont fontPtr) style = FFI.setFontStyle fontPtr (fromIntegral $ fromEnum style)

getFontHinting :: TTFFont -> IO TTFHinting
getFontHinting (TTFFont fontPtr) = liftM (toEnum . fromIntegral) $ FFI.getFontHinting fontPtr

setFontHinting :: TTFFont -> TTFHinting -> IO ()
setFontHinting (TTFFont fontPtr) hinting = FFI.setFontHinting fontPtr (fromIntegral $ fromEnum hinting)

getFontHeight :: TTFFont -> IO Int
getFontHeight (TTFFont fontPtr) = liftM fromIntegral $ FFI.getFontHeight fontPtr

getFontAscent :: TTFFont -> IO Int
getFontAscent (TTFFont fontPtr) = liftM fromIntegral $ FFI.getFontAscent fontPtr

getFontDescent :: TTFFont -> IO Int
getFontDescent (TTFFont fontPtr) = liftM fromIntegral $ FFI.getFontDescent fontPtr

getFontKerning :: TTFFont -> IO Int
getFontKerning (TTFFont fontPtr) = liftM fromIntegral $ FFI.getFontKerning fontPtr

setFontKerning :: TTFFont -> Int -> IO ()
setFontKerning (TTFFont fontPtr) i = FFI.setFontKerning fontPtr (fromIntegral i)

fontFaces :: TTFFont -> IO Int64
fontFaces (TTFFont fontPtr) = liftM fromIntegral $ FFI.fontFaces fontPtr

fontFaceIsFixedWidth :: TTFFont -> IO Bool
fontFaceIsFixedWidth (TTFFont fontPtr) = liftM (== 0) $ FFI.fontFaceIsFixedWidth fontPtr

fontFaceFamilyName :: TTFFont -> IO String
fontFaceFamilyName (TTFFont fontPtr) = FFI.fontFaceFamilyName fontPtr >>= peekCString

fontFaceStyleName :: TTFFont -> IO String
fontFaceStyleName (TTFFont fontPtr) = FFI.fontFaceStyleName fontPtr >>= peekCString

peekInts fn (TTFFont fontPtr) text = do
    wPtr <- malloc
    hPtr <- malloc
    -- TODO: handle errors
    void $ withCString text $ \cstr -> fn fontPtr cstr wPtr hPtr
    w <- peek wPtr
    h <- peek hPtr
    free wPtr
    free hPtr
    return (fromIntegral w, fromIntegral h)

sizeText :: TTFFont -> String -> IO (Int, Int)
sizeText = peekInts FFI.sizeText

sizeUTF8 :: TTFFont -> String -> IO (Int, Int)
sizeUTF8 = peekInts FFI.sizeUTF8

sizeUNICODE :: TTFFont -> String -> IO (Int, Int)
sizeUNICODE = peekInts FFI.sizeUNICODE

renderTextSolid :: TTFFont -> String -> Color -> IO Surface
renderTextSolid (TTFFont fontPtr) text fg = withCString text $ \cstr -> do
    colorPtr <- malloc
    poke colorPtr fg
    ret <- mkFinalizedSurface =<< FFI.renderTextSolid fontPtr cstr colorPtr
    free colorPtr
    return ret

renderUTF8Solid :: TTFFont -> String -> Color -> IO Surface
renderUTF8Solid (TTFFont fontPtr) text fg = withCString text $ \cstr -> do
    colorPtr <- malloc
    poke colorPtr fg
    ret <- mkFinalizedSurface =<< FFI.renderUTF8Solid fontPtr cstr colorPtr
    free colorPtr
    return ret

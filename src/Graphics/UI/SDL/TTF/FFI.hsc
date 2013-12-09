#include "SDL_ttf.h"
module Graphics.UI.SDL.TTF.FFI where

import Foreign.C
import Foreign.Ptr

import qualified Graphics.UI.SDL.Types as SDL

newtype TTFFontPtr = TTFFontPtr (Ptr ())

foreign import ccall unsafe "TTF_OpenFont"
  openFont :: CString -> CInt -> IO TTFFontPtr

foreign import ccall unsafe "TTF_OpenFontIndex"
  openFontIndex :: CString -> CInt -> CInt -> IO TTFFontPtr

foreign import ccall unsafe "TTF_GetFontStyle"
  getFontStyle :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_SetFontStyle"
  setFontStyle :: TTFFontPtr -> CInt -> IO ()

foreign import ccall unsafe "TTF_GetFontHinting"
  getFontHinting :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_SetFontHinting"
  setFontHinting :: TTFFontPtr -> CInt -> IO ()

foreign import ccall unsafe "TTF_FontHeight"
  getFontHeight :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_FontAscent"
  getFontAscent :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_FontDescent"
  getFontDescent :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_GetFontKerning"
  getFontKerning :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_SetFontKerning"
  setFontKerning :: TTFFontPtr -> CInt -> IO ()

foreign import ccall unsafe "TTF_FontFaces"
  fontFaces :: TTFFontPtr -> IO CLong

foreign import ccall unsafe "TTF_FontFaceIsFixedWidth"
  fontFaceIsFixedWidth :: TTFFontPtr -> IO CInt

foreign import ccall unsafe "TTF_FontFaceFamilyName"
  fontFaceFamilyName :: TTFFontPtr -> IO CString

foreign import ccall unsafe "TTF_FontFaceStyleName"
  fontFaceStyleName :: TTFFontPtr -> IO CString

foreign import ccall unsafe "TTF_SizeText"
  sizeText :: TTFFontPtr -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "TTF_SizeUTF8"
  sizeUTF8 :: TTFFontPtr -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "TTF_SizeUNICODE"
  sizeUNICODE :: TTFFontPtr -> CString -> Ptr CInt -> Ptr CInt -> IO CInt


-- TODO: third argument of TTF_Render* functions are actually a struct,
-- not a struct pointer. I have no idea how to implement that using Haskell
-- FFI because apparently there's no way to easily do that.
--
-- Instead, I'm doing this: SDL_Color struct has 4 Uint8 members and I'm
-- assuming that takes 32bits, so I'm using 32bit integers instead.
foreign import ccall unsafe "TTF_RenderText_Solid"
  renderTextSolid :: TTFFontPtr -> CString -> CInt -> IO (Ptr SDL.SurfaceStruct)

foreign import ccall unsafe "TTF_RenderUTF8_Solid"
  renderUTF8Solid :: TTFFontPtr -> CString -> CInt -> IO (Ptr SDL.SurfaceStruct)

foreign import ccall unsafe "TTF_RenderUNICODE_Solid"
  renderUNICODESolid :: TTFFontPtr -> CString -> CInt -> IO (Ptr SDL.SurfaceStruct)

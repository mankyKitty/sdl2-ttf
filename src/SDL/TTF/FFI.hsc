#include "SDL2/SDL_ttf.h"
module SDL.TTF.FFI where

import Foreign.C
import Foreign.Ptr

import qualified SDL.Raw.Types as SDL
import SDL.Raw.Types (Color)

type TTFFont = Ptr ()

foreign import ccall unsafe "TTF_Init"
  init :: IO CInt

foreign import ccall unsafe "TTF_WasInit"
  wasInit :: IO CInt

foreign import ccall unsafe "TTF_Quit"
  quit :: IO ()

foreign import ccall unsafe "TTF_OpenFont"
  openFont :: CString -> CInt -> IO TTFFont

foreign import ccall unsafe "TTF_CloseFont"
  closeFont :: TTFFont -> IO ()
 
foreign import ccall unsafe "TTF_OpenFontIndex"
  openFontIndex :: CString -> CInt -> CInt -> IO TTFFont

foreign import ccall unsafe "TTF_GetFontStyle"
  getFontStyle :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_SetFontStyle"
  setFontStyle :: TTFFont -> CInt -> IO ()

foreign import ccall unsafe "TTF_GetFontHinting"
  getFontHinting :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_SetFontHinting"
  setFontHinting :: TTFFont -> CInt -> IO ()

foreign import ccall unsafe "TTF_FontHeight"
  getFontHeight :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_FontAscent"
  getFontAscent :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_FontDescent"
  getFontDescent :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_GetFontKerning"
  getFontKerning :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_SetFontKerning"
  setFontKerning :: TTFFont -> CInt -> IO ()

foreign import ccall unsafe "TTF_FontFaces"
  fontFaces :: TTFFont -> IO CLong

foreign import ccall unsafe "TTF_FontFaceIsFixedWidth"
  fontFaceIsFixedWidth :: TTFFont -> IO CInt

foreign import ccall unsafe "TTF_FontFaceFamilyName"
  fontFaceFamilyName :: TTFFont -> IO CString

foreign import ccall unsafe "TTF_FontFaceStyleName"
  fontFaceStyleName :: TTFFont -> IO CString

foreign import ccall unsafe "TTF_SizeText"
  sizeText :: TTFFont -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "TTF_SizeUTF8"
  sizeUTF8 :: TTFFont -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "TTF_SizeUNICODE"
  sizeUNICODE :: TTFFont -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "TTF_RenderText_Solid1"
  renderTextSolid :: TTFFont -> CString -> Ptr Color -> IO (Ptr SDL.Surface)

foreign import ccall unsafe "TTF_RenderText_Shaded1"
  renderTextShaded :: TTFFont -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SDL.Surface)

foreign import ccall unsafe "TTF_RenderText_Blended1"
  renderTextBlended :: TTFFont -> CString -> Ptr Color -> IO (Ptr SDL.Surface)

foreign import ccall unsafe "TTF_RenderUTF8_Solid1"
  renderUTF8Solid :: TTFFont -> CString -> Ptr Color -> IO (Ptr SDL.Surface)

foreign import ccall unsafe "TTF_RenderUTF8_Shaded1"
  renderUTF8Shaded :: TTFFont -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SDL.Surface)

foreign import ccall unsafe "TTF_RenderUTF8_Blended1"
  renderUTF8Blended :: TTFFont -> CString -> Ptr Color -> IO (Ptr SDL.Surface)

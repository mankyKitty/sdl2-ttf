-----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.TTF
--
-- Introduction from SDL_ttf documentation at:
-- <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html>
--
-- This library is a wrapper around the excellent FreeType 1.2 library,
-- available at: Freetype Homepage
--
-- WARNING: There may be patent issues with using the FreeType library.
-- Check the FreeType website for up-to-date details.
-- 
-- This library allows you to use TrueType fonts to render text in SDL
-- applications.
-- 
-- Be careful when including fonts with your application, as many of them
-- are copyrighted. The Microsoft fonts, for example, are not freely
-- redistributable and even the free "web" fonts they provide are only
-- redistributable in their special executable installer form (May 1998).
-- There are plenty of freeware and shareware fonts available on the
-- Internet though, which may suit your purposes.	
-- 
-- Enjoy! -Sam Lantinga slouken@devolution.com (5/1/98)
-----------------------------------------------------------------------	
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

-- | Initialize the truetype font API.
-- This must be called before using other functions in this library, 
-- except TTF_WasInit.
--
-- SDL does not have to be initialized before this call.
-- Returns: 0 on success, -1 on any error.
init :: IO CInt
init = FFI.init

-- | Query if TTF API has been initialised.
-- Query the initilization status of the truetype font API.
-- You may, of course, use this before TTF_Init to avoid
-- initializing twice in a row. Or use this to determine if you
-- need to call TTF_Quit.
wasInit :: IO Bool
wasInit = FFI.init >>= return . (==1)

-- | Shut down the TTF system.
-- Shutdown and cleanup the truetype font API.
-- After calling this the SDL_ttf functions should not be used,
-- excepting TTF_WasInit. You may, of course, use TTF_Init to use
-- the functionality again.
quit :: IO ()
quit = FFI.quit

-- | Initialise the TTF system, run the given action(s), then quit TTF.
-- This function handles the initialisation and shut down of the TTF system
-- however if the initialisation fails an exception will be thrown and 
-- your program will crash.
withInit :: IO a -> IO a
withInit a = do
  ret <- init >> a
  quit
  return ret
  
-- | Load file for use as a font, at ptsize size.
--
-- This basically translates to pixel height. Equivalent to TTF_OpenFontIndex(file, ptsize, 0).
-- It can also can load TTF and FON files.
-- 
openFont :: String     -- ^ File name to load font from.
         -> Int        -- ^ Point size (based on 72DPI) to load font as.
         -> IO TTFFont -- ^ Pointer to the font as a TTF_Font. NULL is returned on errors.
openFont file ptsize = withCString file $ \cstr ->
    FFI.openFont cstr (fromIntegral ptsize)

-- | Free the memory used by font, and free font itself as well.
--
-- @Do not use font after this without loading a new font to it.@
closeFont :: TTFFont -- ^ Font to be freed.
          -> IO ()
closeFont fontPtr = FFI.closeFont fontPtr

-- | Load file, face index, for use as a font, at ptsize size.
--
-- This is equivalent to TTF_OpenFontIndexRW(SDL_RWFromFile(file), ptsize, index),
-- but checks that the RWops it creates is not NULL. This can load TTF and FON files.
--
openFontIndex :: String      -- ^ File name to load font from.
              -> Int         -- ^ Point size (based on 72DPI) to load font as.
              -> Int         -- ^ Font face index. The first face is always index 0.
              -> IO TTFFont  -- ^ Pointer to the font as a TTF_Font.
openFontIndex file ptsize index = withCString file $ \cstr -> 
  FFI.openFontIndex cstr (fromIntegral ptsize) (fromIntegral index)

-- | Get the rendering style of the loaded font.
--
-- If no style is set then TTF_STYLE_NORMAL is returned.
--
-- @Extra pointers: <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC21>@
getFontStyle :: TTFFont     -- ^ Font
             -> IO TTFStyle -- ^ Current style bitmask of the font.
getFontStyle fontPtr = liftM (toEnum . fromIntegral) $ FFI.getFontStyle fontPtr

-- | Set the rendering style of the loaded font.
--
-- This will flush the internal cache of previously rendered glyphs,
-- even if there is no change in style, so it may be best to check
-- the current style using TTF_GetFontStyle first.
--
-- @Extra pointers: <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC22>@
setFontStyle :: TTFFont     -- ^ Font 
             -> TTFStyle    -- ^ The style as a bitmask
             -> IO ()
setFontStyle fontPtr style = FFI.setFontStyle fontPtr (fromIntegral $ fromEnum style)

-- | Get the current hinting setting of the loaded font.
--
-- If no hinting is set then TTF_HINTING_NORMAL is returned.
getFontHinting :: TTFFont       -- ^ Font 
               -> IO TTFHinting -- ^ The current hinting setting of the loaded font.
getFontHinting fontPtr = liftM (toEnum . fromIntegral) $ FFI.getFontHinting fontPtr

-- | Set the hinting of the loaded font.
--
-- You should experiment with this setting if you know which font you
-- are using beforehand, especially when using smaller sized fonts.
-- If the user is selecting a font, you may wish to let them select the
-- hinting mode for that font as well.
setFontHinting :: TTFFont    -- ^ Font 
               -> TTFHinting -- ^ The hinting setting desired.
               -> IO ()
setFontHinting fontPtr hinting = FFI.setFontHinting fontPtr (fromIntegral $ fromEnum hinting)

-- | Get the maximum pixel height of all glyphs of the loaded font.
--
-- You may use this height for rendering text as close together vertically
-- as possible, though adding at least one pixel height to it will space it
-- so they can't touch. Remember that SDL_ttf doesn't handle multiline
-- printing, so you are responsible for line spacing, see the TTF_FontLineSkip as well.
getFontHeight :: TTFFont  -- ^ Font 
              -> IO Int   -- ^ Maximum pixel height of all glyphs in the font.
getFontHeight fontPtr = liftM fromIntegral $ FFI.getFontHeight fontPtr

-- | Get the maximum pixel ascent of all glyphs of the loaded font. 
--
-- This can also be interpreted as the distance from the top of the font
-- to the baseline. It could be used when drawing an individual glyph
-- relative to a top point, by combining it with the glyph's maxy metric
-- to resolve the top of the rectangle used when blitting the glyph on the screen.
--
-- Warning: C code (ewwwww)
--
-- @rect.y = top + TTF_FontAscent(font) - glyph_metric.maxy;@
getFontAscent :: TTFFont  -- ^ Font 
              -> IO Int   -- ^ Maximum pixel ascent of all glyphs in the font.
getFontAscent fontPtr = liftM fromIntegral $ FFI.getFontAscent fontPtr

-- | Get the maximum pixel descent of all glyphs of the loaded font. 
--
-- This can also be interpreted as the distance from the baseline to the bottom of the font.
-- It could be used when drawing an individual glyph relative to a bottom point,
-- by combining it with the glyph's maxy metric to resolve the top of the rectangle
-- used when blitting the glyph on the screen.
getFontDescent :: TTFFont  -- ^ Font  
               -> IO Int   -- ^ Maximum pixel height of all glyphs in the font.
getFontDescent fontPtr = liftM fromIntegral $ FFI.getFontDescent fontPtr

-- | Get the current kerning setting of the loaded font.
--
-- The default for a newly loaded font is enabled(True).
fontKerningEnabled :: TTFFont           -- ^ Font
                   -> IO KerningStatus  -- ^ Current Kerning status.
fontKerningEnabled fontPtr = FFI.getFontKerning fontPtr >>= return . toKS
  where
    toKS 1 = KerningOn
    toKS _ = KerningOff

-- | Set whther to use kerning when rendering the loaded font.
--
-- This has no effect on individual glyphs, but rather when rendering whole
-- strings of characters, at least a word at a time. Perhaps the only time
-- to disable this is when kerning is not working for a specific font,
-- resulting in overlapping glyphs or abnormal spacing within words.
setFontKerning :: TTFFont       -- ^ Font
               -> KerningStatus -- ^ Desired Kerning status.
               -> IO ()
setFontKerning fontPtr KerningOn  = FFI.setFontKerning fontPtr 1
setFontKerning fontPtr KerningOff = FFI.setFontKerning fontPtr 0

-- | Get the number of faces ("sub-fonts") available in the loaded font.
--
-- This is a count of the number of specific fonts (based on size and
-- style and other typographical features perhaps) contained in the
-- font itself. It seems to be a useless fact to know, since it can't
-- be applied in any other SDL_ttf functions. 
fontFaces :: TTFFont   -- ^ Font
          -> IO Int64  -- ^ The number of faces in the font.
fontFaces fontPtr = liftM fromIntegral $ FFI.fontFaces fontPtr

-- | Test if the current font face of the loaded font is a fixed width font.
--
-- Fixed width fonts are monospace, meaning every character that exists in
-- the font is the same width, thus you can assume that a rendered string's
-- width is going to be the result of a simple calculation:
--
-- @glyph_width * string_length@
--
fontFaceIsFixedWidth :: TTFFont       -- ^ Font
                     -> IO FixedWidth -- ^ If font is a fixed width font.
fontFaceIsFixedWidth fontPtr =
  FFI.fontFaceIsFixedWidth fontPtr >>= return . toFW
  where
    toFW 1 = IsFixedW
    toFW _ = NotFixedW

-- | Get the current font face family name from the loaded font.
--
-- This function may return a NULL pointer, in which case the information
-- is not available. 
fontFaceFamilyName :: TTFFont   -- ^ Font
                   -> IO String -- ^ The current family name of of the face of the font, or NULL perhaps.
fontFaceFamilyName fontPtr = FFI.fontFaceFamilyName fontPtr >>= peekCString

-- | Get the current font face style name from the loaded font.
--
-- This function may return a NULL pointer, in which case the information
-- is not available. 
fontFaceStyleName :: TTFFont   -- ^ Font
                  -> IO String -- ^ The current style name of of the face of the font, or NULL perhaps.
fontFaceStyleName fontPtr = FFI.fontFaceStyleName fontPtr >>= peekCString

-- | Calculate the resulting surface size of the LATIN1 encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual
-- width. The height returned in h is the same as you can get using @getFontHeight@. 
sizeText :: TTFFont       -- ^ Font
         -> String        -- ^ The LATIN1 null terminated string to size up.
         -> IO (Int, Int) -- ^ (Width,Height)
sizeText = peekInts FFI.sizeText

-- | Calculate the resulting surface size of the UTF8 encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual width.
-- The height returned in h is the same as you can get using @getFontHeight@. 
sizeUTF8 :: TTFFont       -- ^ Font
         -> String        -- ^ The UTF8 null terminated string to size up.
         -> IO (Int, Int) -- ^ (Width,Height)
sizeUTF8 = peekInts FFI.sizeUTF8

-- | Calculate the resulting surface size of the UNICODE encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual width.
-- The height returned in h is the same as you can get using @getFontHeight@. 
sizeUNICODE :: TTFFont       -- ^ Font
            -> String        -- ^ The UNICODE null terminated string to size up.
            -> IO (Int, Int) -- ^ (Width,Height)
sizeUNICODE = peekInts FFI.sizeUNICODE

-- | Render the LATIN1 encoded text, using the Solid mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Solid mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextSolid :: TTFFont          -- ^ Font 
                -> String           -- ^ The LATIN1 null terminated string to render.
                -> Color            -- ^ The color to render the text in. Colormap index 1.
                -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderTextSolid fontPtr text fg = withCString text $ \cstr -> do
    with fg $ \colorPtr -> FFI.renderTextSolid fontPtr cstr colorPtr
    
-- | Render the LATIN1 encoded text, using the Shaded mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Shaded mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextShaded :: TTFFont          -- ^ Font 
                 -> String           -- ^ The LATIN1 null terminated string to render.
                 -> Color            -- ^ The color to render the text in. Colormap index 1.
                 -> Color            -- ^ The color to render the background box in. Colormap index 0.
                 -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderTextShaded fontPtr text fg bg = withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        FFI.renderTextShaded fontPtr cstr fgColorPtr bgColorPtr

-- | Render the LATIN1 encoded text, using the Blended mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Blended mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextBlended :: TTFFont          -- ^ Font 
                  -> String           -- ^ The LATIN1 null terminated string to render.
                  -> Color            -- ^ The color to render the text in. Colormap index 1.
                  -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderTextBlended fontPtr text color = withCString text $ \cstr ->
    with color $ \colorPtr -> FFI.renderTextBlended fontPtr cstr colorPtr

-- | Render the UTF8 encoded text, using the Solid mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Solid mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Solid :: TTFFont            -- ^ Font 
                  -> String           -- ^ The UTF8 null terminated string to render.
                  -> Color            -- ^ The color to render the text in. Colormap index 1.
                  -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderUTF8Solid fontPtr text fg = withCString text $ \cstr -> do
    with fg $ \colorPtr -> FFI.renderUTF8Solid fontPtr cstr colorPtr
    
-- | Render the UTF8 encoded text, using the Shaded mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Shaded mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Shaded :: TTFFont          -- ^ Font 
                 -> String           -- ^ The UTF8 null terminated string to render.
                 -> Color            -- ^ The color to render the text in. Colormap index 1.
                 -> Color            -- ^ The color to render the background box in. Colormap index 0.
                 -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderUTF8Shaded fontPtr text fg bg = withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        FFI.renderUTF8Shaded fontPtr cstr fgColorPtr bgColorPtr

-- | Render the UTF8 encoded text, using the Blended mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Blended mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Blended :: TTFFont          -- ^ Font 
                  -> String           -- ^ The UTF8 null terminated string to render.
                  -> Color            -- ^ The color to render the text in. Colormap index 1.
                  -> IO (Ptr Surface) -- ^ Pointer to a new SDL_Surface. NULL is returned on errors.
renderUTF8Blended fontPtr text color = withCString text $ \cstr ->
    with color $ \colorPtr -> FFI.renderUTF8Blended fontPtr cstr colorPtr

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

-----------------------------------------------------------------------
-- |
-- Module      :  SDL.TTF
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
module SDL.TTF where

import Foreign.C.String
import Foreign.C.Types (CInt)
--import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
--import Foreign.Storable
--import Foreign.Ptr
import Control.Monad
import Data.Int

import qualified SDL as SDL
import qualified SDL.Raw as Raw

import SDL.TTF.FFI (TTFFont)

import qualified SDL.TTF.FFI as FFI
import SDL.TTF.Types
import SDL.TTF.Internals

import Control.Monad.IO.Class
import Prelude hiding (init)

-- | Initialize the truetype font API.
-- This must be called before using other functions in this library,
-- except TTF_WasInit.
--
-- SDL does not have to be initialized before this call.
-- Returns: 0 on success, -1 on any error.
init :: MonadIO m => m CInt
init = liftIO $ FFI.init

-- | Query if TTF API has been initialised.
-- Query the initilization status of the truetype font API.
-- You may, of course, use this before TTF_Init to avoid
-- initializing twice in a row. Or use this to determine if you
-- need to call TTF_Quit.
wasInit :: MonadIO m => m Bool
wasInit = liftIO $ FFI.wasInit >>= return . (/=0)

-- | Shut down the TTF system.
-- Shutdown and cleanup the truetype font API.
-- After calling this the SDL_ttf functions should not be used,
-- excepting TTF_WasInit. You may, of course, use TTF_Init to use
-- the functionality again.
quit :: MonadIO m => m ()
quit = liftIO $ FFI.quit

-- | Initialise the TTF system, run the given action(s), then quit TTF.
-- This function handles the initialisation and shut down of the TTF system
-- however if the initialisation fails an exception will be thrown and
-- your program will crash.
withInit :: MonadIO m => m a -> m a
withInit a = do
  ret <- init >> a
  quit
  return ret

-- | Load file for use as a font, at ptsize size.
--
-- This basically translates to pixel height. Equivalent to TTF_OpenFontIndex(file, ptsize, 0).
-- It can also can load TTF and FON files.
--
openFont :: MonadIO m => String -- ^ File name to load font from.
         -> Int                 -- ^ Point size (based on 72DPI) to load font as.
         -> m TTFFont           -- ^ Pointer to the font as a TTF_Font. NULL is returned on errors.
openFont file ptsize = liftIO $ withCString file $ \cstr ->
    FFI.openFont cstr (fromIntegral ptsize)

-- | Free the memory used by font, and free font itself as well.
--
-- @Do not use font after this without loading a new font to it.@
closeFont :: MonadIO m => TTFFont -- ^ Font to be freed.
          -> m ()
closeFont fontPtr = liftIO $ FFI.closeFont fontPtr

-- | Load file, face index, for use as a font, at ptsize size.
--
-- This is equivalent to TTF_OpenFontIndexRW(SDL_RWFromFile(file), ptsize, index),
-- but checks that the RWops it creates is not NULL. This can load TTF and FON files.
--
openFontIndex :: MonadIO m => String -- ^ File name to load font from.
              -> Int                 -- ^ Point size (based on 72DPI) to load font as.
              -> Int                 -- ^ Font face index. The first face is always index 0.
              -> m TTFFont           -- ^ Pointer to the font as a TTF_Font.
openFontIndex file ptsize index = liftIO $ withCString file $ \cstr ->
  FFI.openFontIndex cstr (fromIntegral ptsize) (fromIntegral index)

-- | Get the rendering style of the loaded font.
--
-- If no style is set then TTF_STYLE_NORMAL is returned.
--
-- @Extra pointers: <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC21>@
getFontStyle :: MonadIO m => TTFFont -- ^ Font
             -> m TTFStyle           -- ^ Current style bitmask of the font.
getFontStyle fontPtr = liftIO $ liftM (toEnum . fromIntegral) $ FFI.getFontStyle fontPtr

-- | Set the rendering style of the loaded font.
--
-- This will flush the internal cache of previously rendered glyphs,
-- even if there is no change in style, so it may be best to check
-- the current style using TTF_GetFontStyle first.
--
-- @Extra pointers: <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC22>@
setFontStyle :: MonadIO m => TTFFont -- ^ Font
             -> TTFStyle             -- ^ The style as a bitmask
             -> m ()
setFontStyle fontPtr style = liftIO $ FFI.setFontStyle fontPtr (fromIntegral $ fromEnum style)

-- | Get the current hinting setting of the loaded font.
--
-- If no hinting is set then TTF_HINTING_NORMAL is returned.
getFontHinting :: MonadIO m => TTFFont -- ^ Font
               -> m TTFHinting         -- ^ The current hinting setting of the loaded font.
getFontHinting fontPtr = liftIO $ liftM (toEnum . fromIntegral) $ FFI.getFontHinting fontPtr

-- | Set the hinting of the loaded font.
--
-- You should experiment with this setting if you know which font you
-- are using beforehand, especially when using smaller sized fonts.
-- If the user is selecting a font, you may wish to let them select the
-- hinting mode for that font as well.
setFontHinting :: MonadIO m => TTFFont -- ^ Font
               -> TTFHinting           -- ^ The hinting setting desired.
               -> m ()
setFontHinting fontPtr hinting = liftIO $ FFI.setFontHinting fontPtr (fromIntegral $ fromEnum hinting)

-- | Get the maximum pixel height of all glyphs of the loaded font.
--
-- You may use this height for rendering text as close together vertically
-- as possible, though adding at least one pixel height to it will space it
-- so they can't touch. Remember that SDL_ttf doesn't handle multiline
-- printing, so you are responsible for line spacing, see the TTF_FontLineSkip as well.
getFontHeight :: MonadIO m => TTFFont -- ^ Font
              -> m Int                -- ^ Maximum pixel height of all glyphs in the font.
getFontHeight fontPtr = liftIO $ liftM fromIntegral $ FFI.getFontHeight fontPtr

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
getFontAscent :: MonadIO m => TTFFont -- ^ Font
              -> m Int                -- ^ Maximum pixel ascent of all glyphs in the font.
getFontAscent fontPtr = liftIO $ liftM fromIntegral $ FFI.getFontAscent fontPtr

-- | Get the maximum pixel descent of all glyphs of the loaded font.
--
-- This can also be interpreted as the distance from the baseline to the bottom of the font.
-- It could be used when drawing an individual glyph relative to a bottom point,
-- by combining it with the glyph's maxy metric to resolve the top of the rectangle
-- used when blitting the glyph on the screen.
getFontDescent :: MonadIO m => TTFFont -- ^ Font
               -> m Int                -- ^ Maximum pixel height of all glyphs in the font.
getFontDescent fontPtr = liftIO $ liftM fromIntegral $ FFI.getFontDescent fontPtr

-- | Get the current kerning setting of the loaded font.
--
-- The default for a newly loaded font is enabled(True).
fontKerningEnabled :: MonadIO m => TTFFont -- ^ Font
                   -> m KerningStatus      -- ^ Current Kerning status.
fontKerningEnabled fontPtr = liftIO $ FFI.getFontKerning fontPtr >>= return . toKS
  where
    toKS 1 = KerningOn
    toKS _ = KerningOff

-- | Set whther to use kerning when rendering the loaded font.
--
-- This has no effect on individual glyphs, but rather when rendering whole
-- strings of characters, at least a word at a time. Perhaps the only time
-- to disable this is when kerning is not working for a specific font,
-- resulting in overlapping glyphs or abnormal spacing within words.
setFontKerning :: MonadIO m => TTFFont -- ^ Font
               -> KerningStatus        -- ^ Desired Kerning status.
               -> m ()
setFontKerning fontPtr KerningOn  = liftIO $ FFI.setFontKerning fontPtr 1
setFontKerning fontPtr KerningOff = liftIO $ FFI.setFontKerning fontPtr 0

-- | Get the number of faces ("sub-fonts") available in the loaded font.
--
-- This is a count of the number of specific fonts (based on size and
-- style and other typographical features perhaps) contained in the
-- font itself. It seems to be a useless fact to know, since it can't
-- be applied in any other SDL_ttf functions.
fontFaces :: MonadIO m => TTFFont -- ^ Font
          -> m Int64              -- ^ The number of faces in the font.
fontFaces fontPtr = liftIO $ liftM fromIntegral $ FFI.fontFaces fontPtr

-- | Test if the current font face of the loaded font is a fixed width font.
--
-- Fixed width fonts are monospace, meaning every character that exists in
-- the font is the same width, thus you can assume that a rendered string's
-- width is going to be the result of a simple calculation:
--
-- @glyph_width * string_length@
--
fontFaceIsFixedWidth :: MonadIO m => TTFFont -- ^ Font
                     -> m FixedWidth         -- ^ If font is a fixed width font.
fontFaceIsFixedWidth fontPtr =
  liftIO $ FFI.fontFaceIsFixedWidth fontPtr >>= return . toFW
  where
    toFW 1 = IsFixedW
    toFW _ = NotFixedW

-- | Get the current font face family name from the loaded font.
--
-- This function may return a NULL pointer, in which case the information
-- is not available.
fontFaceFamilyName :: MonadIO m => TTFFont -- ^ Font
                   -> m String             -- ^ The current family name of of the face of the font, or NULL perhaps.
fontFaceFamilyName fontPtr = liftIO $ FFI.fontFaceFamilyName fontPtr >>= peekCString

-- | Get the current font face style name from the loaded font.
--
-- This function may return a NULL pointer, in which case the information
-- is not available.
fontFaceStyleName :: MonadIO m => TTFFont -- ^ Font
                  -> m String             -- ^ The current style name of of the face of the font, or NULL perhaps.
fontFaceStyleName fontPtr = liftIO $ FFI.fontFaceStyleName fontPtr >>= peekCString

-- | Calculate the resulting surface size of the LATIN1 encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual
-- width. The height returned in h is the same as you can get using @getFontHeight@.
sizeText :: MonadIO m => TTFFont -- ^ Font
         -> String               -- ^ The LATIN1 null terminated string to size up.
         -> m (Int, Int)         -- ^ (Width,Height)
sizeText = peekInts FFI.sizeText

-- | Calculate the resulting surface size of the UTF8 encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual width.
-- The height returned in h is the same as you can get using @getFontHeight@.
sizeUTF8 :: MonadIO m => TTFFont -- ^ Font
         -> String               -- ^ The UTF8 null terminated string to size up.
         -> m (Int, Int)         -- ^ (Width,Height)
sizeUTF8 = peekInts FFI.sizeUTF8

-- | Calculate the resulting surface size of the UNICODE encoded text rendered using font.
--
-- No actual rendering is done, however correct kerning is done to get the actual width.
-- The height returned in h is the same as you can get using @getFontHeight@.
sizeUNICODE :: MonadIO m => TTFFont -- ^ Font
            -> String               -- ^ The UNICODE null terminated string to size up.
            -> m (Int, Int)         -- ^ (Width,Height)
sizeUNICODE = peekInts FFI.sizeUNICODE

-- | Render the LATIN1 encoded text, using the Solid mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Solid mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextSolid :: MonadIO m => TTFFont -- ^ Font
                -> String               -- ^ The LATIN1 null terminated string to render.
                -> Raw.Color            -- ^ The color to render the text in. Colormap index 1.
                -> m SDL.Surface        -- ^ The returned high level SDL.Surface
renderTextSolid fontPtr text fg = liftIO $ withCString text $ \cstr -> do
    --with fg $ \colorPtr -> FFI.renderTextSolid fontPtr cstr colorPtr
    with fg $ \colorPtr -> unmanagedSurface <$> FFI.renderTextSolid fontPtr cstr colorPtr

-- | Render the LATIN1 encoded text, using the Shaded mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Shaded mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextShaded :: MonadIO m => TTFFont -- ^ Font
                 -> String               -- ^ The LATIN1 null terminated string to render.
                 -> Raw.Color            -- ^ The color to render the text in. Colormap index 1.
                 -> Raw.Color            -- ^ The color to render the background box in. Colormap index 0.
                 -> m SDL.Surface        -- ^ Pointer to a new SDL_SDL.Surface. NULL is returned on errors.
renderTextShaded fontPtr text fg bg = liftIO $ withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        unmanagedSurface <$> FFI.renderTextShaded fontPtr cstr fgColorPtr bgColorPtr

-- | Render the LATIN1 encoded text, using the Blended mode
--
-- Render the LATIN1 encoded text using font with fg color onto a new surface,
-- using the Blended mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderTextBlended :: MonadIO m => TTFFont -- ^ Font
                  -> String               -- ^ The LATIN1 null terminated string to render.
                  -> Raw.Color            -- ^ The color to render the text in. Colormap index 1.
                  -> m SDL.Surface        -- ^ Pointer to a new SDL_SDL.Surface. NULL is returned on errors.
renderTextBlended fontPtr text color = liftIO $ withCString text $ \cstr ->
    with color $ \colorPtr -> unmanagedSurface <$> FFI.renderTextBlended fontPtr cstr colorPtr

-- | Render the UTF8 encoded text, using the Solid mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Solid mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Solid :: MonadIO m => TTFFont -- ^ Font
                  -> String             -- ^ The UTF8 null terminated string to render.
                  -> Raw.Color          -- ^ The color to render the text in. Colormap index 1.
                  -> m SDL.Surface      -- ^ Pointer to a new SDL_SDL.Surface. NULL is returned on errors.
renderUTF8Solid fontPtr text fg = liftIO $ withCString text $ \cstr -> do
    with fg $ \colorPtr -> unmanagedSurface <$> FFI.renderUTF8Solid fontPtr cstr colorPtr

-- | Render the UTF8 encoded text, using the Shaded mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Shaded mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Shaded :: MonadIO m => TTFFont -- ^ Font
                 -> String               -- ^ The UTF8 null terminated string to render.
                 -> Raw.Color            -- ^ The color to render the text in. Colormap index 1.
                 -> Raw.Color            -- ^ The color to render the background box in. Colormap index 0.
                 -> m SDL.Surface        -- ^ Pointer to a new SDL_SDL.Surface. NULL is returned on errors.
renderUTF8Shaded fontPtr text fg bg = liftIO $ withCString text $ \cstr ->
    with fg $ \fgColorPtr ->
      with bg $ \bgColorPtr ->
        unmanagedSurface <$> FFI.renderUTF8Shaded fontPtr cstr fgColorPtr bgColorPtr

-- | Render the UTF8 encoded text, using the Blended mode
--
-- Render the UTF8 encoded text using font with fg color onto a new surface,
-- using the Blended mode.
--
-- @The caller (you!) is responsible for freeing any returned surface.@
renderUTF8Blended :: MonadIO m => TTFFont -- ^ Font
                  -> String               -- ^ The UTF8 null terminated string to render.
                  -> Raw.Color            -- ^ The color to render the text in. Colormap index 1.
                  -> m SDL.Surface        -- ^ Pointer to a new SDL_SDL.Surface. NULL is returned on errors.
renderUTF8Blended fontPtr text color = liftIO $ withCString text $ \cstr ->
    with color $ \colorPtr -> unmanagedSurface <$> FFI.renderUTF8Blended fontPtr cstr colorPtr

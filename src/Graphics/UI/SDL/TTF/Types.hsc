#include "SDL2/SDL_ttf.h"
{-# LANGUAGE EmptyDataDecls #-}
module Graphics.UI.SDL.TTF.Types where

import Graphics.UI.SDL.TTF.FFI as FFI

newtype TTFFont = TTFFont FFI.TTFFontPtr

data TTFError
  = RenderUTF8Blended
  | RenderUTF8Shaded
  | RenderUTF8Solid
  | RenderTextBlended
  | RenderTextShaded
  | RenderTextSolid
  | OpenFont
  deriving Show

data TTFStyle = TTFNormal | TTFBold | TTFItalic | TTFUnderline | TTFStrikethrough
  deriving ( Eq, Ord, Show, Read )

instance Enum TTFStyle where
    fromEnum TTFNormal = #{const TTF_STYLE_NORMAL}
    fromEnum TTFBold = #{const TTF_STYLE_BOLD}
    fromEnum TTFItalic = #{const TTF_STYLE_ITALIC}
    fromEnum TTFUnderline = #{const TTF_STYLE_UNDERLINE}
    fromEnum TTFStrikethrough = #{const TTF_STYLE_STRIKETHROUGH}

    toEnum #{const TTF_STYLE_NORMAL} = TTFNormal
    toEnum #{const TTF_STYLE_BOLD} = TTFBold
    toEnum #{const TTF_STYLE_ITALIC} = TTFItalic
    toEnum #{const TTF_STYLE_UNDERLINE} = TTFUnderline
    toEnum #{const TTF_STYLE_STRIKETHROUGH} = TTFStrikethrough
    toEnum _ = error "TTFStyle.toEnum: Invalid argument."

data TTFHinting = TTFHNormal | TTFHLight | TTFHMono | TTFHNone
  deriving ( Eq, Ord, Show, Read )

instance Enum TTFHinting where
    fromEnum TTFHNormal = #{const TTF_HINTING_NORMAL}
    fromEnum TTFHLight = #{const TTF_HINTING_LIGHT}
    fromEnum TTFHMono = #{const TTF_HINTING_MONO}
    fromEnum TTFHNone = #{const TTF_HINTING_NONE}

    toEnum #{const TTF_HINTING_NORMAL} = TTFHNormal
    toEnum #{const TTF_HINTING_LIGHT} = TTFHLight
    toEnum #{const TTF_HINTING_MONO} = TTFHMono
    toEnum #{const TTF_HINTING_NONE} = TTFHNone
    toEnum _ = error "TTFHinting.toEnum: Invalid argument."

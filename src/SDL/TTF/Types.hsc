#include "SDL2/SDL_ttf.h"
{-# LANGUAGE EmptyDataDecls #-}
module SDL.TTF.Types where

data KerningStatus
  = KerningOn
  | KerningOff
  deriving (Show,Eq)

data FixedWidth
  = IsFixedW
  | NotFixedW
  deriving (Show,Eq)

data TTFError
  = RenderUTF8Blended
  | RenderUTF8Shaded
  | RenderUTF8Solid
  | RenderTextBlended
  | RenderTextShaded
  | RenderTextSolid
  | OpenFont
  deriving Show

data TTFStyle
  = TTFNormal
  | TTFBold
  | TTFItalic
  | TTFUnderline
  | TTFStrikethrough
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
    
-- | Hinting
--
-- Font hinting is the use of mathematical instructions to adjust
-- the display of an outline font so that it lines up with a rasterized grid.
-- At small screen sizes, with or without antialiasing, hinting is critical
-- for producing a clear, legible text for human readers.
data TTFHinting
  = TTFHNormal
  | TTFHLight
  | TTFHMono
  | TTFHNone
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

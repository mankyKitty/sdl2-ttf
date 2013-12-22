#ifndef _RENDERING_H
#define _RENDERING_H

#include "SDL2/SDL.h"
#include "SDL2/SDL_ttf.h"

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg);
extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Shaded1(TTF_Font *font,
                const char *text, SDL_Color *fg, SDL_Color *bg);
extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Blended1(TTF_Font *font,
                const char *text, SDL_Color *fg);
extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg);
extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Shaded1(TTF_Font *font,
                const char *text, SDL_Color *fg, SDL_Color *bg);
extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Blended1(TTF_Font *font,
                const char *text, SDL_Color *fg);

#endif

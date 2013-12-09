#include "rendering.h"

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderText_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderUTF8_Solid(font, text, *fg);
}

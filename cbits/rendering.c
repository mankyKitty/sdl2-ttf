#include "rendering.h"

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderText_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Shaded1(TTF_Font *font,
                const char *text, SDL_Color *fg, SDL_Color *bg)
{
    return TTF_RenderText_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Blended1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderText_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Solid1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderUTF8_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Shaded1(TTF_Font *font,
                const char *text, SDL_Color *fg, SDL_Color *bg)
{
    return TTF_RenderUTF8_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Blended1(TTF_Font *font,
                const char *text, SDL_Color *fg)
{
    return TTF_RenderUTF8_Blended(font, text, *fg);
}

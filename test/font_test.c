#include "SDL2/SDL.h"
#include "SDL2/SDL_video.h"
#include "SDL2/SDL_ttf.h"

int handle_event()
{
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_QUIT) {
      return -1;
    }
  }
  return 0;
}

int main()
{
  SDL_Init(SDL_INIT_EVERYTHING);
  TTF_Init();

  SDL_Window *window = SDL_CreateWindow("test", 100, 100, 640, 480, SDL_WINDOW_SHOWN);
  SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);

  TTF_Font *font = TTF_OpenFont("/usr/share/fonts/truetype/DroidSans.ttf", 13);
  if (font == NULL)
    printf("null font\n");

  SDL_Color color = {255, 255, 255};
  SDL_Surface *textSurface = TTF_RenderText_Solid(font, "some text", color);
  if (textSurface == NULL)
    printf("null text surface\n");

  while (handle_event() == 0) {
    SDL_RenderClear(renderer);
    SDL_Surface *windowSurface = SDL_GetWindowSurface(window);
    SDL_BlitSurface(textSurface, NULL, windowSurface, NULL);
    SDL_RenderPresent(renderer);
  }

  return 0;
}

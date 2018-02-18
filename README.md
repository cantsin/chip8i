
# chip8i

A [Chip8](https://en.wikipedia.org/wiki/CHIP-8) emulator, written in Idris.

## Requirements

- [Idris 1.2.0](https://www.idris-lang.org/)

- Uses [SDL Effects library](https://github.com/edwinb/SDL-idris) (requires `SDL` and `SDL_gfx`).

    git clone https://github.com/edwinb/SDL-idris
    idris --install sdl.ipkg

- A Makefile is provided. `make run` will run this emulator with the default ROM (currently `maze.rom`). Note that ROMs are not provided in this repository.

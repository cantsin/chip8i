.PHONY: chip8i

chip8i:
	idris Main.idr -o chip8i

run: chip8i
	./chip8i

all:
	chip8i

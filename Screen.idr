module Screen

import Data.Buffer
import Data.Vect

import Constants
import Utilities

-- a bit represents a monochrome pixel
data Pixel =
  On
  | Off

xorOnePixel : (p1 : Pixel) -> (p2 : Pixel) -> Pixel
xorOnePixel On Off = On
xorOnePixel Off On = On
xorOnePixel _ _ = Off

-- track if any pixel was erased (so we can set VF in the CPU)
pixelWasErased : (p1 : Pixel) -> (p2 : Pixel) -> Bool
pixelWasErased On Off = True
pixelWasErased _ _ = False

-- 64x32 monochrome pixel display


partial
writePixelToScreen : (s : ?screen) -> (p : Pixel) -> (x : Int) -> (y : Int) -> IO ?screen
writePixelToScreen s p x y =
  let realX = x `mod` ScreenWidth in
  let realY = y `mod` ScreenHeight in
  -- get pixel from screen
  -- xor
  -- check for VF?
  -- write
  ?writePixelTo?Screen

-- sprites may be up to 15 bytes (8x15)
-- Sprite : Type
-- Sprite = List Bits8
Sprite : Type
Sprite = Vect 16 Bits8

loadSprite : (b : Buffer) -> Sprite
loadSprite = ?loadSprite

writeSpriteToScreen : (s : ?screen) -> (sprite : ?sprite) -> (x : Int) -> (y : Int) -> IO ?screen
writeSpriteToScreen = ?writeSpriteToScreen
  -- ignore empty sprite

-- the address where we will store sprite data. this address is
-- arbitrarily chosen, but must fit in the range 0x000 to 0x1fff.
spriteDataAddress : Int
spriteDataAddress = 0x100

spriteDataLength : Int
spriteDataLength = 5

spriteStartingAddress : Fin 16 -> Bits16
spriteStartingAddress x =
  let spriteOffset = cast $ finToNat x in
  let offsetAddress = spriteDataLength * spriteOffset in
  cast $ spriteDataAddress + offsetAddress

sprite0x0 : List Bits8
sprite0x0 = [0xf0, 0x90, 0x90, 0x90, 0xf0]
sprite0x1 : List Bits8
sprite0x1 = [0x20, 0x60, 0x20, 0x20, 0x70]
sprite0x2 : List Bits8
sprite0x2 = [0xf0, 0x10, 0xf0, 0x80, 0xf0]
sprite0x3 : List Bits8
sprite0x3 = [0xf0, 0x10, 0xf0, 0x10, 0xf0]
sprite0x4 : List Bits8
sprite0x4 = [0x90, 0x90, 0xf0, 0x10, 0x10]
sprite0x5 : List Bits8
sprite0x5 = [0xf0, 0x80, 0xf0, 0x10, 0xf0]
sprite0x6 : List Bits8
sprite0x6 = [0xf0, 0x80, 0xf0, 0x90, 0xf0]
sprite0x7 : List Bits8
sprite0x7 = [0xf0, 0x10, 0x20, 0x40, 0x40]
sprite0x8 : List Bits8
sprite0x8 = [0xf0, 0x90, 0xf0, 0x90, 0xf0]
sprite0x9 : List Bits8
sprite0x9 = [0xf0, 0x90, 0xf0, 0x10, 0xf0]
sprite0xa : List Bits8
sprite0xa = [0xf0, 0x90, 0xf0, 0x90, 0x90]
sprite0xb : List Bits8
sprite0xb = [0xe0, 0x90, 0xe0, 0x90, 0xe0]
sprite0xc : List Bits8
sprite0xc = [0xf0, 0x80, 0x80, 0x80, 0xf0]
sprite0xd : List Bits8
sprite0xd = [0xe0, 0x90, 0x90, 0x90, 0xe0]
sprite0xe : List Bits8
sprite0xe = [0xf0, 0x80, 0xf0, 0x80, 0xf0]
sprite0xf : List Bits8
sprite0xf = [0xf0, 0x80, 0xf0, 0x80, 0x80]

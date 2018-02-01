module Screen

import Data.Bits
import Data.Vect

import Constants
import Utilities

-- a bit represents a monochrome pixel
data Pixel = On | Off

Show Pixel where
  show On = "▓"
  show Off = " "

xorOnePixel : (p1 : Pixel) -> (p2 : Pixel) -> Pixel
xorOnePixel On Off = On
xorOnePixel Off On = On
xorOnePixel _ _ = Off

-- track if any pixel was erased (so we can set VF in the CPU)
pixelWasErased : (p1 : Pixel) -> (p2 : Pixel) -> Bool
pixelWasErased On Off = True
pixelWasErased _ _ = False

-- for convenience
ScreenWidth : Nat
ScreenWidth = 64

ScreenHeight : Nat
ScreenHeight = 32

-- 64x32 monochrome pixel display
export
record Screen where
  constructor MkScreen
  Picture : Vect ScreenWidth (Vect ScreenHeight Pixel)
  WasErased : Bool -- track for VF

export
newScreen : Screen
newScreen =
  let picture = Vect.replicate ScreenWidth $ Vect.replicate ScreenHeight Off in
  MkScreen picture False

readPixelFromPicture : (s : Screen) -> (x : Fin ScreenWidth) -> (y : Fin ScreenHeight) -> Pixel
readPixelFromPicture s x y =
  let picture = Picture s in
  let column = Vect.index x picture in
  Vect.index y column

writePixelToPicture : (s : Screen) -> (p : Pixel) -> (x : Fin ScreenWidth) -> (y : Fin ScreenHeight) -> Screen
writePixelToPicture s p x y =
  let picture = Picture s in
  let column = Vect.index x picture in
  let newColumn = replaceAt y p column in
  let newPicture = replaceAt x newColumn picture in
  record { Picture = newPicture } s

writePixelToScreen : (s : Screen) -> (p : Pixel) -> (x : Int) -> (y : Int) -> Screen
writePixelToScreen s p x y =
  let realX = restrict (pred ScreenWidth) $ cast x in
  let realY = restrict (pred ScreenHeight) $ cast y in
  let pixel = readPixelFromPicture s realX realY in
  let newPixel = xorOnePixel pixel p in
  let newScreen = writePixelToPicture s newPixel realX realY in
  let wasErased = pixelWasErased pixel p || WasErased s in
  record { WasErased = wasErased } newScreen

-- {len: Nat} -> (Fin len) -> {auto p: len < 16 = True} -> Vect len Bits8

-- a sprite may be up to 8x15 pixels
-- data Sprite : Vect Nat Bits8 -> Type where
--   MkSprite : (len : Nat) -> {auto p: len < 16 = True} -> Sprite (Vect len Bits8)

export
writeSpriteToScreen : (s : Screen) -> (sprite : ?sprite) -> (x : Int) -> (y : Int) -> Screen
writeSpriteToScreen = ?writeSpriteToScreen
  -- ignore empty sprite

export
defaultSpriteStartingAddress : Fin 16 -> Bits16
defaultSpriteStartingAddress x =
  let spriteOffset = cast $ finToNat x in
  let offsetAddress = DefaultSpriteDataLength * spriteOffset in
  cast $ DefaultSpriteDataAddress + offsetAddress

-- 16 8x5 sprites for hexadecimal values
export
defaultSpriteData : List Bits8
defaultSpriteData = [
    0xf0, 0x90, 0x90, 0x90, 0xf0,
    0x20, 0x60, 0x20, 0x20, 0x70,
    0xf0, 0x10, 0xf0, 0x80, 0xf0,
    0xf0, 0x10, 0xf0, 0x10, 0xf0,
    0x90, 0x90, 0xf0, 0x10, 0x10,
    0xf0, 0x80, 0xf0, 0x10, 0xf0,
    0xf0, 0x80, 0xf0, 0x90, 0xf0,
    0xf0, 0x10, 0x20, 0x40, 0x40,
    0xf0, 0x90, 0xf0, 0x90, 0xf0,
    0xf0, 0x90, 0xf0, 0x10, 0xf0,
    0xf0, 0x90, 0xf0, 0x90, 0x90,
    0xe0, 0x90, 0xe0, 0x90, 0xe0,
    0xf0, 0x80, 0x80, 0x80, 0xf0,
    0xe0, 0x90, 0x90, 0x90, 0xe0,
    0xf0, 0x80, 0xf0, 0x80, 0xf0,
    0xf0, 0x80, 0xf0, 0x80, 0x80
  ]

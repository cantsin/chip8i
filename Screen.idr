module Screen

import System
import Data.Bits
import Data.Buffer
import Data.Vect

import Constants
import Utilities

-- a bit represents a monochrome pixel
data Pixel = On | Off

xorOnePixel : (p1 : Pixel) -> (p2 : Pixel) -> Pixel
xorOnePixel On Off = On
xorOnePixel Off On = On
xorOnePixel _ _ = Off

-- track if any pixel was erased (so we can set VF in the CPU)
pixelWasErased : (p1 : Pixel) -> (p2 : Pixel) -> Bool
pixelWasErased On Off = True
pixelWasErased _ _ = False

-- for convenience
Width : Nat
Width = cast ScreenWidth

Height : Nat
Height = cast ScreenHeight

-- 64x32 monochrome pixel display
record Screen where
  constructor MkScreen
  Picture : Vect Width (Vect Height Pixel)
  WasErased : Bool -- track for VF

readPixelFromPicture : (s : Screen) -> (x : Fin Width) -> (y : Fin Height) -> Pixel
readPixelFromPicture s x y =
  let picture = Picture s in
  let column = Vect.index x picture in
  Vect.index y column

writePixelToPicture : (s : Screen) -> (p : Pixel) -> (x : Fin Width) -> (y : Fin Height) -> Screen
writePixelToPicture s p x y =
  let picture = Picture s in
  let column = Vect.index x picture in
  let newColumn = replaceAt y p column in
  let newPicture = replaceAt x newColumn picture in
  record { Picture = newPicture } s

writePixelToScreen : (s : Screen) -> (p : Pixel) -> (x : Int) -> (y : Int) -> Screen
writePixelToScreen s p x y =
  let adjustedX : Integer = cast $ x `mod` ScreenWidth in
  let adjustedY : Integer = cast $ y `mod` ScreenHeight in
  let realX = integerToFin adjustedX Width in
  let realY = integerToFin adjustedY Height in
  -- TODO we know the pixels are in bounds because they are modulo the
  -- screen size. prove this.
  case (realX, realY) of
    (Just x, Just y) =>
      let pixel = readPixelFromPicture s x y in
      let newPixel = xorOnePixel pixel p in
      let newScreen = writePixelToPicture s newPixel x y in
      let wasErased = pixelWasErased pixel p || WasErased s in
      record { WasErased = wasErased } newScreen
    _ =>
      idris_crash "could not write pixel to screen"

-- {len: Nat} -> (Fin len) -> {auto p: len < 16 = True} -> Vect len Bits8

-- a sprite may be up to 8x15 pixels
-- data Sprite : Vect Nat Bits8 -> Type where
--   MkSprite : (len : Nat) -> {auto p: len < 16 = True} -> Sprite (Vect len Bits8)

-- load a sprite from RAM
loadSpriteFromMemory : (ram : Buffer) -> (address : Bits16) -> ?sprite
loadSpriteFromMemory = ?loadSpriteFromMemory

writeSpriteToScreen : (s : Screen) -> (sprite : ?sprite) -> (x : Int) -> (y : Int) -> Screen
writeSpriteToScreen = ?writeSpriteToScreen
  -- ignore empty sprite

defaultSpriteStartingAddress : Fin 16 -> Bits16
defaultSpriteStartingAddress x =
  let spriteOffset = cast $ finToNat x in
  let offsetAddress = DefaultSpriteDataLength * spriteOffset in
  cast $ DefaultSpriteDataAddress + offsetAddress

-- load into interpreter space
copyOverDefaultSpriteData : (ram : Buffer) -> IO ()
copyOverDefaultSpriteData ram =
  do
    buf <- Buffer.newBuffer (DefaultSpriteDataLength * 0xf)
    case buf of
      Just emptyData =>
        ?copyOverDefaultSpriteData
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

-- 16 8x5 sprites for hexadecimal values
defaultSpriteData : Vect 0x50 Bits8
defaultSpriteData = fromList [
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

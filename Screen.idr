module Screen

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
  Picture: Vect Width (Vect Height Pixel)
  WasErased : Bool -- track for VF

readPixelFromPicture : (s : Screen) -> (x : Fin Width) -> (y : Fin Height) -> Pixel
readPixelFromPicture s x y =
  Vect.index y $ Vect.index x $ Picture s

writePixelToPicture : (s : Screen) -> (p : Pixel) -> (x : Fin Width) -> (y : Fin Height) -> Screen
writePixelToPicture s p x y =
  ?updateScreenPixel

writePixelToScreen : (s : Screen) -> (p : Pixel) -> (x : Int) -> (y : Int) -> Screen
writePixelToScreen s p x y =
  let adjustedX : Integer = cast $ x `mod` ScreenWidth in
  let adjustedY : Integer = cast $ y `mod` ScreenHeight in
  let realX = integerToFin adjustedX Width in
  let realY = integerToFin adjustedY Height in
  -- we know the pixels are in bounds because they are modulo the
  -- screen size. but...
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
-- createSprite : (b : Buffer) -> Sprite
-- createSprite = ?loadSprite

writeSpriteToScreen : (s : Screen) -> (sprite : ?sprite) -> (x : Int) -> (y : Int) -> IO Screen
writeSpriteToScreen = ?writeSpriteToScreen
  -- ignore empty sprite

-- we store default sprites in the interpreter space. this address is
-- arbitrarily chosen, but must fit in the range 0x000 to 0x1fff.

defaultSpriteDataAddress : Int
defaultSpriteDataAddress = 0x100

defaultSpriteDataLength : Int
defaultSpriteDataLength = 5

defaultSpriteStartingAddress : Fin 16 -> Bits16
defaultSpriteStartingAddress x =
  let spriteOffset = cast $ finToNat x in
  let offsetAddress = defaultSpriteDataLength * spriteOffset in
  cast $ defaultSpriteDataAddress + offsetAddress

-- load into interpreter space
copyOverDefaultSpriteData : ?copyOverDefaultSpriteData

defaultSprite0x0 : Vect 5 Bits8
defaultSprite0x0 = fromList [0xf0, 0x90, 0x90, 0x90, 0xf0]
defaultSprite0x1 : Vect 5 Bits8
defaultSprite0x1 = fromList [0x20, 0x60, 0x20, 0x20, 0x70]
defaultSprite0x2 : Vect 5 Bits8
defaultSprite0x2 = fromList [0xf0, 0x10, 0xf0, 0x80, 0xf0]
defaultSprite0x3 : Vect 5 Bits8
defaultSprite0x3 = fromList [0xf0, 0x10, 0xf0, 0x10, 0xf0]
defaultSprite0x4 : Vect 5 Bits8
defaultSprite0x4 = fromList [0x90, 0x90, 0xf0, 0x10, 0x10]
defaultSprite0x5 : Vect 5 Bits8
defaultSprite0x5 = fromList [0xf0, 0x80, 0xf0, 0x10, 0xf0]
defaultSprite0x6 : Vect 5 Bits8
defaultSprite0x6 = fromList [0xf0, 0x80, 0xf0, 0x90, 0xf0]
defaultSprite0x7 : Vect 5 Bits8
defaultSprite0x7 = fromList [0xf0, 0x10, 0x20, 0x40, 0x40]
defaultSprite0x8 : Vect 5 Bits8
defaultSprite0x8 = fromList [0xf0, 0x90, 0xf0, 0x90, 0xf0]
defaultSprite0x9 : Vect 5 Bits8
defaultSprite0x9 = fromList [0xf0, 0x90, 0xf0, 0x10, 0xf0]
defaultSprite0xa : Vect 5 Bits8
defaultSprite0xa = fromList [0xf0, 0x90, 0xf0, 0x90, 0x90]
defaultSprite0xb : Vect 5 Bits8
defaultSprite0xb = fromList [0xe0, 0x90, 0xe0, 0x90, 0xe0]
defaultSprite0xc : Vect 5 Bits8
defaultSprite0xc = fromList [0xf0, 0x80, 0x80, 0x80, 0xf0]
defaultSprite0xd : Vect 5 Bits8
defaultSprite0xd = fromList [0xe0, 0x90, 0x90, 0x90, 0xe0]
defaultSprite0xe : Vect 5 Bits8
defaultSprite0xe = fromList [0xf0, 0x80, 0xf0, 0x80, 0xf0]
defaultSprite0xf : Vect 5 Bits8
defaultSprite0xf = fromList [0xf0, 0x80, 0xf0, 0x80, 0x80]

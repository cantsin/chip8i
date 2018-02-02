module Screen

import Data.Bits
import Data.Vect

import Constants
import Utilities

-- a bit represents a monochrome pixel
data Pixel = On | Off

Show Pixel where
  show On = "▓"
  show Off = "░"

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
  Picture : Vect ScreenHeight (Vect ScreenWidth Pixel)
  WasErased : Bool -- track for VF

export
Show Screen where
  show s = foldl drawRow "" (Picture s)
  where
    concatenate : (accum : String) -> (p : Pixel) -> String
    concatenate accum val = accum ++ show val
    drawRow : (accum : String) -> (row : Vect ScreenWidth Pixel) -> String
    drawRow accum row = accum ++ (foldl concatenate "" row) ++ "\n"

export
newScreen : Screen
newScreen =
  let picture = Vect.replicate ScreenHeight $ Vect.replicate ScreenWidth Off in
  MkScreen picture False

readPixelFromPicture : (s : Screen) -> (x : Fin ScreenWidth) -> (y : Fin ScreenHeight) -> Pixel
readPixelFromPicture s x y =
  let picture = Picture s in
  let row = Vect.index y picture in
  Vect.index x row

writePixelToPicture : (s : Screen) -> (p : Pixel) -> (x : Fin ScreenWidth) -> (y : Fin ScreenHeight) -> Screen
writePixelToPicture s p x y =
  let picture = Picture s in
  let row = Vect.index y picture in
  let newRow = replaceAt x p row in
  let newPicture = replaceAt y newRow picture in
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

explodeByte : (val : Bits8) -> Vect 8 Pixel
explodeByte val =
  let bits : Bits 8 = cast val in
  let booleans = map (flip getBit bits) [0, 1, 2, 3, 4, 5, 6, 7] in
  map convertPixel booleans
  where
    convertPixel : Bool -> Pixel
    convertPixel True = On
    convertPixel False = Off

writeSpriteLineToScreen : (screen : Screen) -> (val : Bits8) -> (x : Int) -> (y : Int) -> Screen
writeSpriteLineToScreen screen val x y =
  let pixels = explodeByte val in
  -- foldl (drawPixel y) screen pixels
  -- where
  --   drawPixel : (pixel : Pixel) -> (screen : Screen) -> Screen
  --   drawPixel p s =
  ?foo

export
writeSpriteToScreen : (screen : Screen) -> (sprite : Vect len Bits8) -> (x : Int) -> (y : Int) -> Screen
writeSpriteToScreen screen sprite x y =
  -- for each index, decrement y
  -- writePixelToScreen screen On x y
  -- ignore empty sprite
  ?writeSpriteToScreen

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

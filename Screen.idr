module Screen

import Data.Bits
import Data.Vect
import Effects
import Effect.SDL

import Constants
import Utilities

%default total

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

data Coord = MkCoord Int Int Pixel

export
renderScreen : (screen : Screen) -> { [SDL_ON] } Eff ()
renderScreen screen =
  let (coords, _) = foldl pixelCoordsForRow ([], 0) (Picture screen) in
  do
    mapE renderPixel coords
    pure ()
  where
    pixelCoords : (accum : (List Coord, (Int, Int))) -> (pixel : Pixel) -> (List Coord, (Int, Int))
    pixelCoords (rects, (x, y)) pixel =
      MkPair (MkCoord x y pixel :: rects) (x + 1, y)
    pixelCoordsForRow : (accum : (List Coord, Int)) -> Vect ScreenWidth Pixel -> (List Coord, Int)
    pixelCoordsForRow (coords, y) row =
      let (newCoords, _) = foldl pixelCoords ([], (MkPair 0 y)) row in
      MkPair (coords ++ newCoords) (y + 1)
    renderPixel : Coord -> EffM m () [SDL_ON] (\_ => [SDL_ON])
    renderPixel (MkCoord x y pixel) =
      let color = case pixel of On => white; Off => black in
      rectangle color (x * Scale) (y * Scale) Scale Scale

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
  let booleans = map (flip getBit bits) [7, 6, 5, 4, 3, 2, 1, 0] in
  map convertPixel booleans
  where
    convertPixel : Bool -> Pixel
    convertPixel True = On
    convertPixel False = Off

writeSpriteLineToScreen : (screen : Screen) -> (val : Bits8) -> (x : Int) -> (y : Int) -> Screen
writeSpriteLineToScreen screen val x y =
  let pixels = explodeByte val in
  fst $ foldl (drawPixel y) (MkPair screen x) pixels
  where
    drawPixel : (y : Int) -> (accum : (Screen, Int)) -> (pixel : Pixel) -> (Screen, Int)
    drawPixel y (screen, x) p =
      let newScreen = writePixelToScreen screen p x y in
      MkPair newScreen (x + 1)

export
writeSpriteToScreen : (screen : Screen) -> (sprite : Vect len Bits8) -> (x : Int) -> (y : Int) -> Screen
writeSpriteToScreen screen sprite x y =
  fst $ foldl (drawLine x) (MkPair screen y) sprite
  where
    drawLine : (x : Int) -> (accum : (Screen, Int)) -> (val : Bits8) -> (Screen, Int)
    drawLine x (screen, y) val =
      let newScreen = writeSpriteLineToScreen screen val x y in
      MkPair newScreen (y + 1)

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

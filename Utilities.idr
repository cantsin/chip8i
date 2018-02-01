module Utilities

import Data.Bits
import Data.Fin
import Effects
import Effect.Random

-- not sure why these casts are not included already. goes without
-- saying that some conversions are potentially lossy.

export
Cast Int Bits16 where
  cast = prim__zextInt_B16

export
Cast Int Bits8 where
  cast = prim__truncInt_B8

export
Cast Integer Bits8 where
  cast = fromInteger

export
Cast Bits16 Int where
  cast = prim__zextB16_Int

export
Cast Bits8 Int where
  cast = prim__zextB8_Int

export
Cast Bits8 Bits16 where
  cast = prim__zextB8_B16

export
Cast Bits8 (Bits 8) where
  cast x =
    let asInt: Int = cast x in
    intToBits $ the Integer $ cast asInt

export
Cast (Bits 8) Bits8 where
  cast x =
    let asInteger: Integer = bitsToInt x in
    cast $ the Int $ fromInteger asInteger

export
Cast Bits8 (Fin 16) where
  cast x =
    let reg: Int = cast x in
    let fin = fromIntegerNat $ cast reg in
    case natToFin fin 16 of
      Just f => f
      Nothing => idris_crash "value exceeded bounds" -- sad face

Cast Bits16 (Bits 16) where
  cast x =
    let asInt: Int = cast x in
    intToBits $ the Integer $ cast asInt

Cast (Bits 16) Bits16 where
  cast x =
    let asInteger: Integer = bitsToInt x in
    cast $ the Int $ fromInteger asInteger

extractNibble : (value : Bits16) -> (n : Integer) -> Bits8
extractNibble value n =
  let v: Bits 16 = cast value in
  let shift = intToBits $ 4 * n in
  let mask = intToBits 0xf `shiftLeft` shift in
  let result = (and v mask) `shiftRightLogical` shift in
  cast $ the Int $ fromInteger $ bitsToInt result

export
extractFirstNibble : (value : Bits16) -> Bits8
extractFirstNibble value = extractNibble value 3

export
extractSecondNibble : (value : Bits16) -> Bits8
extractSecondNibble value = extractNibble value 2

export
extractThirdNibble : (value : Bits16) -> Bits8
extractThirdNibble value = extractNibble value 1

export
extractFourthNibble : (value : Bits16) -> Bits8
extractFourthNibble value = extractNibble value 0

export
extractFirstByte : (value : Bits16) -> Bits8
extractFirstByte value =
  let v: Bits 16 = cast value in
  let mask = intToBits 0xff00 in
  let shift = intToBits 8 in
  let result = (and v mask) `shiftRightLogical` shift in
  cast $ the Int $ fromInteger $ bitsToInt result

export
extractSecondByte : (value : Bits16) -> Bits8
extractSecondByte value =
  let v: Bits 16 = cast value in
  let mask = intToBits 0x00ff in
  let result = and v mask in
  cast $ the Int $ fromInteger $ bitsToInt result

export
extractMask0xfff : (value : Bits16) -> Bits16
extractMask0xfff value =
  let v: Bits 16 = cast value in
  let mask = intToBits 0x0fff in
  let result = and v mask in
  cast result

export
partial
getRandomByte : () -> Eff Integer [RND]
getRandomByte () =
  rndInt 0x00 0xff

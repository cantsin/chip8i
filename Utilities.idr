module Utilities

import Data.Bits

-- not sure why these casts are not included already. goes without
-- saying that some conversions are potentially lossy.
export
Cast Int Bits16 where
  cast = prim__zextInt_B16

export
Cast Bits16 Int where
  cast = prim__zextB16_Int

export
Cast Bits8 Bits16 where
  cast = prim__zextB8_B16

export
Cast Bits16 (Bits 16) where
  cast x =
    let asInt: Int = cast x in
    intToBits $ the Integer $ cast asInt

export
Cast (Bits 16) Bits16 where
  cast x =
    let asInteger: Integer = bitsToInt x in
    cast $ the Int $ fromInteger asInteger

export
extractAddress : (value : Bits16) -> Bits16
extractAddress value =
  let mask = intToBits 0x0fff in
  let v : Bits 16 = cast value in
  let result = and v mask in
  cast result

export
extractNibble : (value : Bits16) -> (n : Integer) -> Integer
extractNibble value n =
  let shift = intToBits $ 4 * n in
  let mask = intToBits 0xf `shiftLeft` shift in
  let v : Bits 16 = cast value in
  let result = (and v mask) `shiftRightLogical` shift in
  bitsToInt result

-- extract byte n

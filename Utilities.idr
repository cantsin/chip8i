module Utilities

-- not sure why these casts are not included already. goes without
-- saying that some conversions are potentially lossy.
public export
Cast Int Bits16 where
  cast = prim__zextInt_B16

public export
Cast Bits16 Int where
  cast = prim__zextB16_Int

public export
Cast Bits8 Bits16 where
  cast = prim__zextB8_B16

module Keypad

import Effects
import Effect.SDL
import Data.Vect

export
record Keypad where
  constructor MkKeypad
  -- 16 key hexadecimal keypad
  K : Vect 16 Bool

export
isKeyPressed : (keypad : Keypad) -> (n : Fin 16) -> Bool
isKeyPressed keypad n = Vect.index n (K keypad)


-- TODO map layout to
-- 1	2	3	C
-- 4	5	6	D
-- 7	8	9	E
-- A	0	B	F
-- qwer
-- asdf
-- zxcv

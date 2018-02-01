module Keypad

import Data.Vect

export
record Keypad where
  constructor MkKeypad
  -- 16 key hexadecimal keypad
  K : Vect 16 Bits8

-- layout

-- 1	2	3	C
-- 4	5	6	D
-- 7	8	9	E
-- A	0	B	F

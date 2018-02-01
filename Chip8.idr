module Chip8

import Utilities
import Constants
import Screen
import Cpu

export
record Chip8 where
  constructor MkChip8
  cpu : Cpu
  screen : Screen
  -- keys : Keys
  counter : Integer
  halted : Bool
  waiting : Bool
  -- keep track of the next random byte to use
  randomN : Bits8
  reseed : Bool

-- TODO should have RAM?

-- loop:
--   increment PC
--   increment counter (Hz)
--   tick down DT/ST
--   if reseed, generate new random #
--   if halted, wait for user to press esc before exiting
--   if waiting, wait for user to press key

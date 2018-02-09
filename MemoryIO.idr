module MemoryIO

import Effects
import Effect.State
import Data.Bits
import Data.Buffer

import Chip8
import Cpu
import Ram
import Utilities

export
getOpcode : { [Chip8 ::: STATE Chip8, RAM] } Effects.DepEff.Eff (Bits16)
getOpcode =
  let chip = !(Chip8 :- get) in
  let memory = Memory chip in
  let cpu = Computer chip in
  let pc : Int = cast $ getPC cpu in
  let b1 = !(readByte memory pc) in
  let b2 = !(readByte memory (pc + 1)) in
  pure $ (cast b1) * 0x100 + (cast b2)

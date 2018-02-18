module MemoryIO

import Effects
import Effect.State
import Data.Bits
import Data.Buffer
import Data.Vect

import Chip8
import Cpu
import Ram
import Utilities
import Screen

%default total

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

export
dumpBlock : (address : Int) -> (Vect len Bits8) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
dumpBlock address values =
  let chip = !(Chip8 :- get) in
  let memory = Memory chip in
  -- not aware of a faster way, this seems inefficient
  let _ = foldl (writeByteAt memory) (pure address) values in
    pure ()
  where
    writeByteAt : (buffer : Buffer) -> (addr : { [RAM] } Eff Int) -> (value : Bits8) -> { [RAM] } Eff Int
    writeByteAt buffer addr value =
      do
        offset <- addr
        writeByte buffer offset value
        pure $ offset + 1

-- quite inefficient as well.
build : {n : Nat} -> (f : Fin n -> { [t] } Eff a) -> { [t] } Eff (Vect n a)
build {n = Z} f = pure []
build {n = S _} f =
  do
    first <- f 0
    rest <- build (f . FS)
    pure $ first :: rest

export
loadBlock : (address : Int) -> (n : Fin len) -> { [Chip8 ::: STATE Chip8, RAM] } Eff (Vect (finToNat n) Bits8)
loadBlock address n =
  let chip = !(Chip8 :- get) in
  let memory = Memory chip in
  build {n = finToNat n} (readByteAt memory address)
  where
    readByteAt : (buffer : Buffer) -> (address : Int) -> (count : Fin index) -> { [RAM] } Eff Bits8
    readByteAt buffer address count =
      let offset = cast $ finToNat count in
      let location = address + offset in
      readByte buffer location

export
loadDefaultSpriteDataAt : (address : Int) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
loadDefaultSpriteDataAt address =
  dumpBlock address $ fromList defaultSpriteData

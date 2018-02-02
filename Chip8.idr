module Chip8

import System
import Data.Bits
import Data.Buffer
import Data.Vect

import Utilities
import Constants
import Screen
import Keypad
import Cpu

export
record Chip8 where
  constructor MkChip8
  Computer : Cpu
  Display : Screen
  Ram : Buffer -- 4kb RAM
  Keys : Keypad
  Counter : Integer
  Halted : Bool
  Waiting : Bool
  Error : String

export
newChip8 : IO Chip8
newChip8 =
  do
    buf <- Buffer.newBuffer RamSize
    case buf of
      Just ram =>
        let keys = MkKeypad $ Vect.replicate 16 0 in
        pure $ MkChip8 newCpu newScreen ram keys 0 False False "No error"
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

export
getComputer : (chip : Chip8) -> Cpu
getComputer = Computer

export
getDisplay : (chip : Chip8) -> Screen
getDisplay = Display

export
getCounter : (chip : Chip8) -> Integer
getCounter = Counter

export
isHalted : (chip : Chip8) -> Bool
isHalted = Halted

export
errorMessage : (chip : Chip8) -> String
errorMessage = Error

export
isWaiting : (chip : Chip8) -> Bool
isWaiting = Waiting

export
loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt chip rom address =
  Buffer.copyData rom 0 (Buffer.size rom) (Ram chip) address

export
dumpBlock : (chip : Chip8) -> (address : Int) -> (Vect len Bits8) -> IO Int
dumpBlock chip address values =
  let ram = Ram chip in
  -- not aware of a faster way, this seems inefficient
  foldl (writeByte ram) (pure address) values
  where
    writeByte : (buffer : Buffer) -> (addr : IO Int) -> (value : Bits8) -> IO Int
    writeByte buffer addr value =
      do
        offset <- addr
        setByte buffer offset value
        pure $ offset + 1

-- quite inefficient as well.
build : {n : Nat} -> (f : Fin n -> IO a) -> IO (Vect n a)
build {n = Z} f = pure []
build {n = S _} f = do
  first <- f 0
  rest <- build (f . FS)
  pure $ first :: rest

export
loadBlock : (chip : Chip8) -> (address : Int) -> (n : Fin len) -> IO (Vect (finToNat n) Bits8)
loadBlock chip address n =
  let ram = Ram chip in
  build {n = finToNat n} (readByte ram address)
  where
    readByte : (buffer : Buffer) -> (address : Int) -> (count : Fin index) -> IO Bits8
    readByte buffer address count =
      let offset = cast $ finToNat count in
      getByte buffer (address + offset)

export
loadDefaultSpriteDataAt : (chip : Chip8) -> (address : Int) -> IO Int
loadDefaultSpriteDataAt chip address =
  dumpBlock chip address $ fromList defaultSpriteData

export
getOpcode : (chip : Chip8) -> IO Bits16
getOpcode chip =
  let ram = Ram chip in
  let cpu = Computer chip in
  let pc : Int = cast $ getPC cpu in
  do
    b1 <- Buffer.getByte ram pc
    b2 <- Buffer.getByte ram (pc + 1)
    pure $ (cast b1) * 0x100 + (cast b2)

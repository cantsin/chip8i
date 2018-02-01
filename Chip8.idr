module Chip8

import System
import Data.Bits
import Data.Buffer
import Data.Vect

import Utilities
import Constants
import Screen
import Cpu

public
export
record Chip8 where
  constructor MkChip8
  Computer : Cpu
  Display : Screen
  -- 4kb RAM: 0x000 to 0x1ff are reserved, start instruction at 0x200
  Ram : Buffer
  -- keys : Keys
  Counter : Integer
  Halted : Bool
  Waiting : Bool
  -- keep track of the next random byte to use
  RandomNumber : Bits8
  Reseed : Bool

export
newChip8 : IO Chip8
newChip8 =
  do
    buf <- Buffer.newBuffer RamSize
    case buf of
      Just ram =>
        -- TODO initialize random number
        pure $ MkChip8 newCpu newScreen ram 0 False False 0x00 False
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

export
loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt chip rom address =
  Buffer.copyData rom 0 (Buffer.size rom) (Ram chip) address

export
loadDefaultSpriteDataAt : (chip : Chip8) -> (address : Int) -> IO Int
loadDefaultSpriteDataAt chip address =
  let ram = Ram chip in
  -- not aware of a faster way, this seems inefficient
  foldl (writeByte ram) (pure address) defaultSpriteData
  where
    writeByte : (buffer : Buffer) -> (addr : IO Int) -> (value : Bits8) -> IO Int
    writeByte buffer addr value =
      do
        offset <- addr
        setByte buffer offset value
        pure $ offset + 1

export
loadSpriteAt : (chip : Chip8) -> (address : Int) -> (n : Fin len) -> Vect len Bits8
loadSpriteAt chip address n =
  let ram = Ram chip in
  -- read n bytes
  -- getByte ram (address + offset)
  -- can do either list or directly as a vect
  -- ... but need IO.
  ?loadSpriteFromMemory

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

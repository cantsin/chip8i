module Chip8

import System
import Data.Bits
import Data.Buffer

import Utilities
import Constants
import Opcodes
import Screen
import Cpu

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
  RandomN : Bits8
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

getOpcode : (chip : Chip8) -> IO Bits16
getOpcode chip =
  let ram = Ram chip in
  let cpu = Computer chip in
  let pc : Int = cast $ getPC cpu in
  do
    b1 <- Buffer.getByte ram pc
    b2 <- Buffer.getByte ram (pc + 1)
    pure $ (cast b1) * 0x100 + (cast b2)

-- TODO when to draw screen?

export
partial
runChip8 : (chip : Chip8) -> IO ()
runChip8 chip =
  let cpu = Computer chip in
  let counter = Counter chip in
  -- The CPU runs at roughly 500Hz, however, we want to tick down the
  -- CPU DT/ST at a rate of 60Hz. As a first approximation, let's say
  -- we tick down DT/ST every 8 CPU cycles.
  let tick = counter `mod` 8 == 1 in
  do
    instruction <- getOpcode chip
    modifiedCpu <- runOneCycle cpu $ opcode instruction
    computer <- pure $ updateCPUState modifiedCpu tick
    -- TODO: if Reseed then generate new random #
    -- TODO: if Halted then wait for user to press esc before exiting
    -- TODO: if Waiting then wait for user to press key
    runChip8 $ record { Computer = computer, Counter $= (+ 1) } chip

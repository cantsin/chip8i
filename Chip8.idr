module Chip8

import System
import Data.Bits
import Data.Buffer

import Utilities
import Constants
import Screen
import Opcodes
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
getOpcode : (chip : Chip8) -> IO Bits16
getOpcode c =
  let cpu = Computer c in
  let ram = Ram c in
  let pc : Int = cast $ PC cpu in
  do
    b1 <- Buffer.getByte ram pc
    b2 <- Buffer.getByte ram (pc + 1)
    pure $ (cast b1) * 0x100 + (cast b2)

-- loop:
--   run CPU
--   increment PC
--   increment counter (Hz)
--   tick down DT/ST
--   if reseed, generate new random #
--   if halted, wait for user to press esc before exiting
--   if waiting, wait for user to press key

-- TODO when to draw screen?

export
partial
runChip8 : (chip : Chip8) -> IO ()
runChip8 c =
  do
    op <- getOpcode c
    instruction <- pure $ opcode op
    putStrLn $ (show $ Computer c) ++ " => " ++ (show instruction)
    case instruction of
      Invalid _ =>
        do
          putStrLn $ "terminating unexpectedly"
      _ =>
        do
          cpuState <- pure $ dispatch (incrementPC $ Computer c) instruction
          runChip8 $ record { Computer = cpuState } c

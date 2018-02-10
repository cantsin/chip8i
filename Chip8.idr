module Chip8

import System
import Data.Bits
import Data.Fin
import Data.Buffer
import Data.Vect

import Utilities
import Constants
import Screen
import Keypad
import Cpu

public
export
data Chip8State =
  Active
  | WaitingForKey (Fin 16)
  | Halted String

public
export
record Chip8 where
  constructor MkChip8
  Computer : Cpu
  Display : Screen
  Memory : Buffer -- 4kb RAM
  Keys : Keypad
  State : Chip8State

export
newChip8 : IO Chip8
newChip8 =
  do
    buf <- Buffer.newBuffer RamSize
    case buf of
      Just ram =>
        let keys = MkKeypad $ Vect.replicate 16 False in
        pure $ MkChip8 newCpu newScreen ram keys Active
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

export
loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt chip rom address =
  let memory = Memory chip in
  let length = Buffer.size rom in
  do
    Buffer.copyData rom 0 length memory address

export
isHalted : (chip : Chip8) -> Maybe String
isHalted chip =
  case State chip of
    Halted error => Just error
    _ => Nothing

export
isKeyPressed : (chip : Chip8) -> (n : Fin 16) -> Bool
isKeyPressed chip n = isKeyPressed (Keys chip) n

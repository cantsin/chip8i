module Main

import Data.Vect
import Data.Buffer
import System

RamSize : Int
RamSize = 4096

record Chip8 where
  constructor MkChip8
  -- 16 general purpose 8-bit registers, V0 to Vf
  V : Vect 16 Bits8
  -- 16-bit register called I
  I : Bits16
  -- pseudo PC (16-bit)
  PC : Bits16
  -- stack is an array of 16 16-bit values
  Stack : Vect 16 Bits16
  -- pseudo SP
  SP : Fin 16
  -- 4kb RAM: 0x000 to 0x1ff are reserved, start instruction at 0x200
  Ram : Buffer
  -- delay time register DT
  DT : Bits8
  -- sound timer register ST
  ST : Bits8

newChip : IO Chip8
newChip =
  do
    buf <- Buffer.newBuffer RamSize
    case buf of
      Just ram =>
        let v = Vect.replicate 16 0 in
        let stack = Vect.replicate 16 0 in
        pure $ MkChip8 v 0 0 stack 0 ram 0 0
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

getRegister : (chip : Chip8) -> (index : Fin 16) -> Bits8
getRegister c i =
  Vect.index i (V c)

setRegister : (chip : Chip8) -> (index : Fin 16) -> (value : Bits8) -> Chip8
setRegister c i v =
  let newV = replaceAt i v (V c) in
  record { V = newV } c

loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt c rom address =
  Buffer.copyData rom 0 (Buffer.size rom) (Ram c) 0x200

readROMFromFile : (filename : String) -> IO Buffer
readROMFromFile filename =
  let maxLength : Int = 0x1000 - 0x200 in
  do
    handle <- openFile filename Read
    case handle of
      Right file => do
        buffer <- Buffer.newBuffer maxLength
        case buffer of
          Just mem =>
            Buffer.readBufferFromFile file mem maxLength
          Nothing =>
            do
              putStrLn "Not enough memory"
              System.exitFailure
      Left _ =>
        do
          putStrLn "Could not read ROM"
          System.exitFailure

main : IO ()
main = do
  chip <- newChip
  rom <- readROMFromFile "./roms/maze.rom"
  loadROMAt chip rom 0x200
  putStrLn "Fin."

-- TODO
-- implement basic structure of interpreter

-- first opcode:

-- 6xkk - LD Vx, byte
-- Set Vx = kk.

-- The interpreter puts the value kk into register Vx.

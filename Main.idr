module Main

import Data.Vect
import Data.Buffer

record Chip8 where
  constructor MkChip8
  -- 16 general purpose 8-bit registers, V0 to Vf
  V : Vect 16 Bits8
  -- 16-bit register called I
  I : Bits16
  -- pseudo PC (16-bit)
  PC : Bits16
  -- pseudo SP (8-bit)
  SP : Bits8
  -- stack is an array of 16 16-bit values
  Stack : Vect 16 Bits16
  -- 4kb RAM
  -- 0x000 to 0x1ff are reserved, start instruction at 0x200
  Ram : IO (Maybe Buffer)
  -- delay time register DT
  DT : Bits8
  -- sound timer register ST
  ST : Bits8


main : IO ()
main = putStrLn "Hello world"

newChip : Chip8
newChip =
  let v = Vect.replicate 16 0 in
  let stack = Vect.replicate 16 0 in
  let ram = Buffer.newBuffer 4096 in
  MkChip8 v 0 0 0 stack ram 0 0

getRegister : (chip : Chip8) -> (index : Fin 16) -> Bits8
getRegister c i =
  Vect.index i (V c)

setRegister : (chip : Chip8) -> (index : Fin 16) -> (value : Bits8) -> Chip8
setRegister c i v =
  let newV = replaceAt i v (V c) in
  record { V = newV } c

-- TODO
-- implement basic structure of interpreter
-- load file into RAM (appears to be little endian)
-- start with maze.rom

-- first opcode:

-- 6xkk - LD Vx, byte
-- Set Vx = kk.

-- The interpreter puts the value kk into register Vx.

module Main

import Data.Vect
import Data.Buffer
import Data.Bits
import System

RamSize : Int
RamSize = 0x1000

StartingAddress : Int
StartingAddress = 0x200

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

-- not sure why these casts are not included already. goes without
-- saying that some conversions are potentially lossy.
Cast Int Bits16 where
  cast = prim__zextInt_B16

Cast Bits16 Int where
  cast = prim__zextB16_Int

Cast Bits8 Bits16 where
  cast = prim__zextB8_B16

newChip : IO Chip8
newChip =
  do
    buf <- Buffer.newBuffer RamSize
    case buf of
      Just ram =>
        let v = Vect.replicate 16 0 in
        let stack = Vect.replicate 16 0 in
        let pc : Bits16 = cast StartingAddress in
        pure $ MkChip8 v 0 pc stack 0 ram 0 0
      Nothing =>
        do
          putStrLn "Not enough memory"
          System.exitFailure

getOpcode : (chip : Chip8) -> IO Bits16
getOpcode c =
  let pc : Int = cast $ PC c in
  let ram = Ram c in
  do
    b1 <- Buffer.getByte ram pc
    b2 <- Buffer.getByte ram (pc + 1)
    pure $ (cast b1) * 0x100 + (cast b2)

incrementPC : (chip : Chip8) -> Chip8
incrementPC c =
  let newPC = (PC c) + 2 in
  record { PC = newPC } c

getRegister : (chip : Chip8) -> (index : Fin 16) -> Bits8
getRegister c i =
  Vect.index i (V c)

setRegister : (chip : Chip8) -> (index : Fin 16) -> (value : Bits8) -> Chip8
setRegister c i v =
  let newV = replaceAt i v (V c) in
  record { V = newV } c

loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt c rom address =
  Buffer.copyData rom 0 (Buffer.size rom) (Ram c) address

readROMFromFile : (filename : String) -> IO Buffer
readROMFromFile filename =
  let maxLength : Int = RamSize - StartingAddress in
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

dispatch : (chip : Chip8) -> (opcode : Bits16) -> IO Chip8
dispatch c op =
  do
    putStrLn (show $ the Bits16 op)
    ?test

-- TODO
-- implement basic structure of interpreter

-- first opcode:

-- 6xkk - LD Vx, byte
-- Set Vx = kk.

-- The interpreter puts the value kk into register Vx.

runCPU : (chip : Chip8) -> IO ()
runCPU c =
  do
    op <- getOpcode c
    modifiedC <- dispatch (incrementPC c) op
    runCPU modifiedC

main : IO ()
main = do
  chip <- newChip
  rom <- readROMFromFile "./roms/maze.rom"
  loadROMAt chip rom StartingAddress
  runCPU chip
  putStrLn "Fin."

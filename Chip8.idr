module Chip8

import System
import Data.Buffer
import Data.Vect

import Utilities
import Constants

export
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

export
Show Chip8 where
  show c = show (PC c) ++ " " ++ show (V c) ++ " " ++ show (I c)

export
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

export
loadROMAt : (chip : Chip8) -> (rom : Buffer) -> (address : Int) -> IO ()
loadROMAt c rom address =
  Buffer.copyData rom 0 (Buffer.size rom) (Ram c) address

export
getOpcode : (chip : Chip8) -> IO Bits16
getOpcode c =
  let pc : Int = cast $ PC c in
  let ram = Ram c in
  do
    b1 <- Buffer.getByte ram pc
    b2 <- Buffer.getByte ram (pc + 1)
    pure $ (cast b1) * 0x100 + (cast b2)

export
incrementPC : (chip : Chip8) -> Chip8
incrementPC c =
  record { PC $= (+ 2) } c

export
setPC : (chip : Chip8) -> (value : Bits16) -> Chip8
setPC c v =
  record { PC = v } c

export
getPC : (chip : Chip8) -> Bits16
getPC c = PC c

export
getRegister : (chip : Chip8) -> (index : Fin 16) -> Bits8
getRegister c i =
  Vect.index i (V c)

export
setRegister : (chip : Chip8) -> (index : Fin 16) -> (value : Bits8) -> Chip8
setRegister c i v =
  let newV = replaceAt i v (V c) in
  record { V = newV } c

export
setRegisterFlag : (chip : Chip8) -> (value : Bits8) -> Chip8
setRegisterFlag c v =
  let newV = replaceAt 0xf v (V c) in
  record { V = newV } c

export
setRegisterI : (chip : Chip8) -> (value : Bits16) -> Chip8
setRegisterI c v =
  record { I = v } c

export
pushStack : (chip : Chip8) -> Chip8
pushStack c =
  let index = SP c in
  case toIntegerNat $ finToNat index of
    15 => idris_crash "Chip8: tried to push to full stack"
    n =>
      let newIndex = restrict 15 (n + 1) in
      let newStack = replaceAt newIndex (PC c) (Stack c) in
      record { SP = newIndex, Stack = newStack } c

export
popStack : (chip : Chip8) -> Chip8
popStack c =
  let index = SP c in
  case index of
    FZ => idris_crash "Chip8: tried to pop from empty stack"
    FS n =>
      let newIndex = restrict 15 $ cast n in
      let newStack = replaceAt index 0 (Stack c) in
      record { SP = newIndex, Stack = newStack } c

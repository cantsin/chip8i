module Main

import Data.Vect
import Data.Buffer
import Data.Bits
import System

%default total

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
  SP : Bits8
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
  record { PC $= (+ 2) } c

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

data Opcode =
  -- no parameter
    ClearScreen
  | Return
  -- address
  | Jump Bits16
  | Call Bits16
  -- register and value
  | SkipIfEq     Bits8 Bits16
  | SkipIfNeq    Bits8 Bits16
  | LoadRegister Bits8 Bits16
  | AddRegister  Bits8 Bits16
  | Random       Bits8 Bits16
  -- register to register
  | SkipIfRegisterEq   Bits8 Bits8
  | SkipIfRegisterNeq  Bits8 Bits8
  | CopyRegister       Bits8 Bits8
  | OrRegister         Bits8 Bits8
  | AndRegister        Bits8 Bits8
  | XorRegister        Bits8 Bits8
  | AddRegisterCarry   Bits8 Bits8
  | SubRegister        Bits8 Bits8
  | SubRegisterInverse Bits8 Bits8
  | ShiftRightRegister Bits8 Bits8
  | ShiftLeftRegister  Bits8 Bits8
  -- value
  | LoadRegisterI Bits16
  | JumpRegister0 Bits16
  -- registers and value
  | Display Bits8 Bits8 Bits8
  -- key
  | SkipIfKeyPressed     Bits8
  | SkipIfKeyNotPressed  Bits8
  | LoadRegisterIFromKey Bits8
  -- register
  | LoadRegisterDelay    Bits8
  | WaitForKeyPress      Bits8
  | SetDelayFromRegister Bits8
  | SetSoundFromRegister Bits8
  | AddRegisterI         Bits8
  | StoreBCD             Bits8
  | DumpRegisters        Bits8
  | LoadRegisters        Bits8

opcode : (value : Bits16) -> Opcode
opcode v =
  case v of
    0x00e0 => ClearScreen
    n => Jump n

dispatch : (chip : Chip8) -> (opcode : Bits16) -> IO Chip8
dispatch c op =
  do
    putStrLn (show $ the Bits16 op)
    ?test

-- TODO
-- implement basic structure of interpreter

-- 00E0 - CLS
clearScreen : (chip : Chip8) -> Chip8
clearScreen =
  ?clear

-- 00EE - RET
-- subroutineReturn : (chip : Chip8) -> Chip8
-- subroutineReturn c =
--   let newStack = (Stack c) ++ [PC c] in
--   record { SP $= (+ 1), Stack = newStack } c

-- 1nnn - JP addr
jumpDirect : (chip : Chip8) -> (address : Bits16) -> Chip8
jumpDirect c address =
  record { PC = address } c

-- 6xkk - LD Vx, byte
loadRegisterDirect : (chip : Chip8) -> (index : Fin 16) -> (value : Bits8) -> Chip8
loadRegisterDirect = setRegister

partial
runCPU : (chip : Chip8) -> IO ()
runCPU c =
  do
    op <- getOpcode c
    modifiedC <- dispatch (incrementPC c) op
    runCPU modifiedC

partial
main : IO ()
main = do
  chip <- newChip
  rom <- readROMFromFile "./roms/maze.rom"
  loadROMAt chip rom StartingAddress
  runCPU chip
  putStrLn "Fin."

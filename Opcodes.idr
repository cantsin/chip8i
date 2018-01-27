module Opcodes

import Data.Bits
import Data.Fin

import Chip8
import Utilities

Register : Type
Register = Bits8 -- TODO 4 bit value

Key : Type
Key = Bits8 -- TODO 4 bit value

Sprite : Type
Sprite = Bits8 -- TODO 4 bit value

Address : Type
Address = Bits16 -- TODO 12 bit value

Value : Type
Value = Bits8

export
data Opcode =
  Invalid Bits16
  -- no parameter
  | ClearScreen
  | Return
  -- address
  | Jump Address
  | Call Address
  -- register and value
  | SkipIfEq     Register Value
  | SkipIfNeq    Register Value
  | LoadRegister Register Value
  | AddRegister  Register Value
  | Random       Register Value
  -- register to register
  | SkipIfRegisterEq   Register Register
  | SkipIfRegisterNeq  Register Register
  | CopyRegister       Register Register
  | OrRegister         Register Register
  | AndRegister        Register Register
  | XorRegister        Register Register
  | AddRegisterCarry   Register Register
  | SubRegister        Register Register
  | SubRegisterInverse Register Register
  | ShiftRightRegister Register Register
  | ShiftLeftRegister  Register Register
  -- value
  | LoadRegisterI Bits16 -- TODO spec is ambiguous, I is 8 bit but we're loading a 12 bit value?
  | JumpRegister0 Address
  -- registers and value
  | Display Register Register Sprite
  -- key
  | SkipIfKeyPressed     Key
  | SkipIfKeyNotPressed  Key
  -- register
  | LoadRegisterWithSprite Register
  | LoadRegisterDelay      Register
  | WaitForKeyPress        Register
  | SetDelayFromRegister   Register
  | SetSoundFromRegister   Register
  | AddRegisterI           Register
  | StoreBCD               Register
  | DumpRegisters          Register
  | LoadRegisters          Register

export
Show Opcode where
  show (Invalid op) = show op ++ " ???"
  show ClearScreen = "CLS"
  show Return = "RET"
  show (Jump a) = "JP " ++ show a
  show (Call a) = "CALL " ++ show a
  show (SkipIfEq r v) = "SE V" ++ show r ++ ", " ++ show v
  show (SkipIfNeq r v) = "SNE V" ++ show r ++ ", " ++ show v
  show (SkipIfRegisterEq r1 r2) = "SE V" ++ show r1 ++ ", V" ++ show r2
  show (LoadRegister r v) = "LD V" ++ show r ++ ", " ++ show v
  show (AddRegister r v) = "ADD V" ++ show r ++ ", " ++ show v
  show (CopyRegister r1 r2) = "LD V" ++ show r1 ++ ", V" ++ show r2
  show (OrRegister r1 r2) = "OR V" ++ show r1 ++ ", V" ++ show r2
  show (AndRegister r1 r2) = "AND" ++ show r1 ++ ", V" ++ show r2
  show (XorRegister r1 r2) = "XOR" ++ show r1 ++ ", V" ++ show r2
  show (AddRegisterCarry r1 r2) = "ADD V" ++ show r1 ++ ", V" ++ show r2
  show (SubRegister r1 r2) = "SUB V" ++ show r1 ++ ", V" ++ show r2
  show (ShiftRightRegister r1 r2) = "SHR V" ++ show r1 ++ ", V" ++ show r2
  show (SubRegisterInverse r1 r2) = "SUBN V" ++ show r1 ++ ", V" ++ show r2
  show (ShiftLeftRegister r1 r2) = "SHL V" ++ show r1 ++ ", V" ++ show r2
  show (SkipIfRegisterNeq r1 r2) = "SNE V" ++ show r1 ++ ", V" ++ show r2
  show (LoadRegisterI a) = "LD I, " ++ show a
  show (JumpRegister0 a) = "JP V0, " ++ show a
  show (Random r v) = "RND V" ++ show r ++ ", " ++ show v
  show (Display r1 r2 s) = "DRW V" ++ show r1 ++ ", V" ++ show r2 ++ ", " ++ show s
  show (SkipIfKeyPressed k) = "SKP V" ++ show k
  show (SkipIfKeyNotPressed k) = "SKNP V" ++ show k
  show (LoadRegisterDelay r) = "LD V" ++ show r ++ ", DT"
  show (WaitForKeyPress r) = "LD V" ++ show r ++ ", K"
  show (SetDelayFromRegister r) = "LD DT, V" ++ show r
  show (SetSoundFromRegister r) = "LD ST, V" ++ show r
  show (AddRegisterI r) = "ADD I, V" ++ show r
  show (LoadRegisterWithSprite r) = "LD F, V" ++ show r
  show (StoreBCD r) = "LD B, V" ++ show r
  show (DumpRegisters r) = "LD [I], V" ++ show r
  show (LoadRegisters r) = "LD V" ++ show r ++ ", [I]"

opcodeFamily : (n : Int) -> (op : Bits16) -> Opcode
opcodeFamily 0 op =
  case op of
    0x00e0 => ClearScreen
    0x00ee => Return
    _ => Invalid op
opcodeFamily 1 op = Jump (extractAddress op)
opcodeFamily 2 op = Call (extractAddress op)
opcodeFamily 3 op = SkipIfEq (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 4 op = SkipIfNeq (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 5 op =
  let r1 = extractSecondNibble op in
  let r2 = extractThirdNibble op in
  let valid = extractFourthNibble op in
    if valid == 0 then
      SkipIfRegisterEq r1 r2
    else
      Invalid op
opcodeFamily 6 op = LoadRegister (extractSecondNibble op) (extractSecondByte op)
opcodeFamily _ op = Invalid op

-- 00E0 - CLS
clearScreen : (chip : Chip8) -> Chip8
clearScreen c =
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

opcode : (value : Bits16) -> Opcode
opcode op =
  let family: Int = cast $ extractFirstNibble op in
  opcodeFamily family op

dispatch : (chip : Chip8) -> (opcode : Opcode) -> IO Chip8
dispatch c ClearScreen = pure $ clearScreen c
dispatch c opcode =
  do
    putStrLn $ "unhandled " ++ (show $ opcode)
    pure $ c

export
partial
runCPU : (chip : Chip8) -> IO ()
runCPU c =
  do
    op <- getOpcode c
    case opcode op of
      Invalid op =>
        do
          putStrLn $ "hit invalid opcode " ++ (show op)
      instruction =>
        do
          modifiedC <- dispatch (incrementPC c) instruction
          runCPU modifiedC

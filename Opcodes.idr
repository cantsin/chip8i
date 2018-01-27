module Opcodes

import Data.Bits
import Data.Fin

import Chip8
import Utilities

Register : Type
Register = Bits8 -- TODO 4 bit value

Cast Register (Fin 16) where
  cast r =
    let reg: Int = cast $ the Bits8 r in
    let fin = fromIntegerNat $ cast reg in
    case natToFin fin 16 of
      Just f => f
      Nothing => idris_crash "register value exceeded bounds" -- sad face

Key : Type
Key = Bits8 -- TODO 4 bit value

Sprite : Type
Sprite = Bits8 -- TODO 4 bit value

Address : Type
Address = Bits16 -- TODO 12 bit value

Value : Type
Value = Bits8

LargeValue : Type
LargeValue = Bits16 -- TODO 12 bit value

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
  | LoadRegisterI LargeValue
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
opcodeFamily 0x1 op = Jump (extractAddress op)
opcodeFamily 0x2 op = Call (extractAddress op)
opcodeFamily 0x3 op = SkipIfEq (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 0x4 op = SkipIfNeq (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 0x5 op =
  let r1 = extractSecondNibble op in
  let r2 = extractThirdNibble op in
  let valid = extractFourthNibble op in
    if valid == 0 then
      SkipIfRegisterEq r1 r2
    else
      Invalid op
opcodeFamily 0x6 op = LoadRegister (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 0x7 op = AddRegister (extractSecondNibble op) (extractSecondByte op)
-- opcodeFamily 0x8
-- opcodeFamily 0x9
opcodeFamily 0xa op = LoadRegisterI (extractAddress op)
-- opcodeFamily 0xb
opcodeFamily 0xc op = Random (extractSecondNibble op) (extractSecondByte op)
opcodeFamily 0xd op = Display (extractSecondNibble op) (extractThirdNibble op) (extractFourthNibble op)
-- opcodeFamily 0xe
-- opcodeFamily 0xf
opcodeFamily _ op = Invalid op

opcode : (value : Bits16) -> Opcode
opcode op =
  let family: Int = cast $ extractFirstNibble op in
  opcodeFamily family op

clearScreen : (chip : Chip8) -> Chip8
clearScreen c =
  ?clear

-- subroutineReturn : (chip : Chip8) -> Chip8
-- subroutineReturn c =
--   let newStack = (Stack c) ++ [PC c] in
--   record { SP $= (+ 1), Stack = newStack } c

jumpDirect : (chip : Chip8) -> (address : Address) -> Chip8
jumpDirect = setPC

skipIfRegisterEqual : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
skipIfRegisterEqual c r v =
  ?test

loadRegisterDirect : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
loadRegisterDirect c r v = setRegister c (cast r) v

loadRegisterI : (chip : Chip8) -> (value : LargeValue) -> Chip8
loadRegisterI = setRegisterI

dispatch : (chip : Chip8) -> (opcode : Opcode) -> IO Chip8
dispatch c ClearScreen = pure $ clearScreen c
-- dispatch c Return = pure $ subroutineReturn c
dispatch c (Jump addr) = pure $ jumpDirect c addr
dispatch c (SkipIfEq r v) = pure $ skipIfRegisterEqual c r v
dispatch c (LoadRegister r v) = pure $ loadRegisterDirect c r v
dispatch c (LoadRegisterI r) = pure $ loadRegisterI c r
dispatch c opcode =
  do
    putStrLn "(unhandled)"
    pure c

export
partial
runCPU : (chip : Chip8) -> IO ()
runCPU c =
  do
    op <- getOpcode c
    instruction <- pure $ opcode op
    putStrLn $ (show c) ++ " => " ++ (show instruction)
    case instruction of
      Invalid _ =>
        do
          putStrLn $ "terminating unexpectedly"
      _ =>
        do
          modifiedC <- dispatch (incrementPC c) instruction
          runCPU modifiedC

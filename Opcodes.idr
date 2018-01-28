module Opcodes

import Data.Bits
import Data.Fin
import Effects
import Effect.StdIO
import Effect.Random
import Effect.State

import Chip8
import Utilities

Register : Type
Register = Fin 16

-- always in position 0x0R00
extractFirstRegister : (value : Bits16) -> Register
extractFirstRegister v = cast $ extractSecondNibble v

-- always in position 0x00R0
extractSecondRegister : (value : Bits16) -> Register
extractSecondRegister v = cast $ extractThirdNibble v

SpriteIndex : Type
SpriteIndex = Fin 16

-- always in position 0x000S
extractSpriteIndex : (value : Bits16) -> SpriteIndex
extractSpriteIndex v = cast $ extractFourthNibble v

Address : Type
Address = Bits16 -- really a 12 bit value

-- always in position 0x0AAA
extractAddress : (value : Bits16) -> Address
extractAddress = extractMask0xfff

Value : Type
Value = Bits8

-- always in position 0x00VV
extractValue : (value : Bits16) -> Value
extractValue = extractSecondByte

export
data Opcode =
  Invalid Bits16
  | ClearScreen
  | Return
  | Jump                   Address
  | Call                   Address
  | SkipIfEq               Register Value
  | SkipIfNeq              Register Value
  | SkipIfRegisterEq       Register Register
  | LoadRegister           Register Value
  | AddRegister            Register Value
  | CopyRegister           Register Register
  | OrRegister             Register Register
  | AndRegister            Register Register
  | XorRegister            Register Register
  | AddRegisterCarry       Register Register
  | SubRegister            Register Register
  | ShiftRightRegister     Register Register
  | SubRegisterInverse     Register Register
  | ShiftLeftRegister      Register Register
  | SkipIfRegisterNeq      Register Register
  | LoadRegisterI          Address
  | JumpRegister0          Address
  | Random                 Register Value
  | Display                Register Register SpriteIndex
  | SkipIfKeyPressed       Register
  | SkipIfKeyNotPressed    Register
  | LoadRegisterDelay      Register
  | WaitForKeyPress        Register
  | SetDelayFromRegister   Register
  | SetSoundFromRegister   Register
  | AddRegisterI           Register
  | LoadRegisterWithSprite Register
  | StoreBCD               Register
  | DumpRegisters          Register
  | LoadRegisters          Register

Show Register where
  show r = "V" ++ (show $ finToNat r)

export
Show Opcode where
  show (Invalid op)               = "??? "     ++ show op
  show ClearScreen                = "CLS"
  show Return                     = "RET"
  show (Jump a)                   = "JP "      ++ show a
  show (Call a)                   = "CALL "    ++ show a
  show (SkipIfEq r v)             = "SE "      ++ show r  ++ ", " ++ show v
  show (SkipIfNeq r v)            = "SNE "     ++ show r  ++ ", " ++ show v
  show (SkipIfRegisterEq r1 r2)   = "SE "      ++ show r1 ++ ", " ++ show r2
  show (LoadRegister r v)         = "LD "      ++ show r  ++ ", " ++ show v
  show (AddRegister r v)          = "ADD "     ++ show r  ++ ", " ++ show v
  show (CopyRegister r1 r2)       = "LD "      ++ show r1 ++ ", " ++ show r2
  show (OrRegister r1 r2)         = "OR "      ++ show r1 ++ ", " ++ show r2
  show (AndRegister r1 r2)        = "AND"      ++ show r1 ++ ", " ++ show r2
  show (XorRegister r1 r2)        = "XOR"      ++ show r1 ++ ", " ++ show r2
  show (AddRegisterCarry r1 r2)   = "ADD "     ++ show r1 ++ ", " ++ show r2
  show (SubRegister r1 r2)        = "SUB "     ++ show r1 ++ ", " ++ show r2
  show (ShiftRightRegister r1 r2) = "SHR "     ++ show r1 ++ ", " ++ show r2
  show (SubRegisterInverse r1 r2) = "SUBN "    ++ show r1 ++ ", " ++ show r2
  show (ShiftLeftRegister r1 r2)  = "SHL "     ++ show r1 ++ ", " ++ show r2
  show (SkipIfRegisterNeq r1 r2)  = "SNE "     ++ show r1 ++ ", " ++ show r2
  show (LoadRegisterI a)          = "LD I, "   ++ show a
  show (JumpRegister0 a)          = "JP V0, "  ++ show a
  show (Random r v)               = "RND "     ++ show r  ++ ", " ++ show v
  show (Display r1 r2 s)          = "DRW "     ++ show r1 ++ ", " ++ show r2 ++ ", S" ++ show (finToNat s)
  show (SkipIfKeyPressed r)       = "SKP "     ++ show r
  show (SkipIfKeyNotPressed r)    = "SKNP "    ++ show r
  show (LoadRegisterDelay r)      = "LD "      ++ show r  ++ ", DT"
  show (WaitForKeyPress r)        = "LD "      ++ show r  ++ ", K"
  show (SetDelayFromRegister r)   = "LD DT, "  ++ show r
  show (SetSoundFromRegister r)   = "LD ST, "  ++ show r
  show (AddRegisterI r)           = "ADD I, "  ++ show r
  show (LoadRegisterWithSprite r) = "LD F, "   ++ show r
  show (StoreBCD r)               = "LD B, "   ++ show r
  show (DumpRegisters r)          = "LD [I], " ++ show r
  show (LoadRegisters r)          = "LD "      ++ show r  ++ ", [I]"

opcodeDispatch : (n : Int) -> (op : Bits16) -> Opcode
opcodeDispatch 0 op =
  case op of
    0x00e0 => ClearScreen
    0x00ee => Return
    _ => Invalid op
opcodeDispatch 0x1 op = Jump          (extractAddress op)
opcodeDispatch 0x2 op = Call          (extractAddress op)
opcodeDispatch 0x3 op = SkipIfEq      (extractFirstRegister op) (extractValue op)
opcodeDispatch 0x4 op = SkipIfNeq     (extractFirstRegister op) (extractValue op)
opcodeDispatch 0x5 op =
  case extractFourthNibble op of
    0x0 => SkipIfRegisterEq           (extractFirstRegister op) (extractSecondRegister op)
    _ => Invalid op
opcodeDispatch 0x6 op = LoadRegister  (extractFirstRegister op) (extractValue op)
opcodeDispatch 0x7 op = AddRegister   (extractFirstRegister op) (extractValue op)
opcodeDispatch 0x8 op =
  case extractFourthNibble op of
    0x0 => CopyRegister               (extractFirstRegister op) (extractSecondRegister op)
    0x1 => OrRegister                 (extractFirstRegister op) (extractSecondRegister op)
    0x2 => AndRegister                (extractFirstRegister op) (extractSecondRegister op)
    0x3 => XorRegister                (extractFirstRegister op) (extractSecondRegister op)
    0x4 => AddRegisterCarry           (extractFirstRegister op) (extractSecondRegister op)
    0x5 => SubRegister                (extractFirstRegister op) (extractSecondRegister op)
    0x6 => ShiftRightRegister         (extractFirstRegister op) (extractSecondRegister op)
    0x7 => SubRegisterInverse         (extractFirstRegister op) (extractSecondRegister op)
    0xe => ShiftLeftRegister          (extractFirstRegister op) (extractSecondRegister op)
    _ => Invalid op
opcodeDispatch 0x9 op =
  case extractFourthNibble op of
    0x0 => SkipIfRegisterNeq          (extractFirstRegister op) (extractSecondRegister op)
    _ => Invalid op
opcodeDispatch 0xa op = LoadRegisterI (extractAddress op)
opcodeDispatch 0xb op = JumpRegister0 (extractAddress op)
opcodeDispatch 0xc op = Random        (extractFirstRegister op) (extractValue op)
opcodeDispatch 0xd op = Display       (extractFirstRegister op) (extractSecondRegister op) (extractSpriteIndex op)
opcodeDispatch 0xe op =
  case extractSecondByte op of
    0x9e => SkipIfKeyPressed          (extractFirstRegister op)
    0xa1 => SkipIfKeyNotPressed       (extractFirstRegister op)
    _ => Invalid op
opcodeDispatch 0xf op =
  case extractSecondByte op of
    0x07 => LoadRegisterDelay         (extractFirstRegister op)
    0x0a => WaitForKeyPress           (extractFirstRegister op)
    0x15 => SetDelayFromRegister      (extractFirstRegister op)
    0x18 => SetSoundFromRegister      (extractFirstRegister op)
    0x1e => AddRegisterI              (extractFirstRegister op)
    0x29 => LoadRegisterWithSprite    (extractFirstRegister op)
    0x33 => StoreBCD                  (extractFirstRegister op)
    0x55 => DumpRegisters             (extractFirstRegister op)
    0x65 => LoadRegisters             (extractFirstRegister op)
    _ => Invalid op
opcodeDispatch _ op = Invalid op

opcode : (value : Bits16) -> Opcode
opcode op =
  let family: Int = cast $ extractFirstNibble op in
  opcodeDispatch family op

-- opcode implementations

clearScreen : (chip : Chip8) -> Chip8
clearScreen c =
  ?clear

jumpDirect : (chip : Chip8) -> (address : Address) -> Chip8
jumpDirect c addr =
  if (getPC c == (addr + 2)) then
    ?infiniteLoop
  else
    setPC c addr

skipIfRegisterEqual : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
skipIfRegisterEqual c r v =
  let toCompare = getRegister c r in
  if v == toCompare then
    incrementPC c
  else
    c

addRegisterDirect : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
addRegisterDirect c r v =
  let value = getRegister c r in
  setRegister c r (value + v)

andRandomValue : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
andRandomValue c r v =
  -- let value: Bits 16 = 0x1 in -- getRandomByte in
  -- let mask = value `and` intToBits v in
  setRegister c r 0x1 --(cast mask)

dispatch : (chip : Chip8) -> (opcode : Opcode) -> IO Chip8
dispatch c ClearScreen = pure $ clearScreen c
dispatch c (Jump addr) = pure $ jumpDirect c addr
dispatch c (SkipIfEq r v) = pure $ skipIfRegisterEqual c r v
dispatch c (LoadRegister r v) = pure $ setRegister c r v
dispatch c (AddRegister r v) = pure $ addRegisterDirect c r v
dispatch c (LoadRegisterI r) = pure $ setRegisterI c r
dispatch c (Random r v) = pure $ andRandomValue c r v
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

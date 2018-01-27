module Opcodes

import Data.Bits
import Data.Fin

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

Key : Type
Key = Fin 16

Sprite : Type
Sprite = Fin 16

-- always in position 0x000S
extractSprite : (value : Bits16) -> Sprite
extractSprite v = cast $ extractFourthNibble v

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
  show (Display r1 r2 s)          = "DRW "     ++ show r1 ++ ", " ++ show r2 ++ ", " ++ show s
  show (SkipIfKeyPressed k)       = "SKP "     ++ show k
  show (SkipIfKeyNotPressed k)    = "SKNP "    ++ show k
  show (LoadRegisterDelay r)      = "LD "      ++ show r  ++ ", DT"
  show (WaitForKeyPress r)        = "LD "      ++ show r  ++ ", K"
  show (SetDelayFromRegister r)   = "LD DT, "  ++ show r
  show (SetSoundFromRegister r)   = "LD ST, "  ++ show r
  show (AddRegisterI r)           = "ADD I, "  ++ show r
  show (LoadRegisterWithSprite r) = "LD F, "   ++ show r
  show (StoreBCD r)               = "LD B, "   ++ show r
  show (DumpRegisters r)          = "LD [I], " ++ show r
  show (LoadRegisters r)          = "LD "      ++ show r  ++ ", [I]"

opcodeFamily : (n : Int) -> (op : Bits16) -> Opcode
opcodeFamily 0 op =
  case op of
    0x00e0 => ClearScreen
    0x00ee => Return
    _ => Invalid op
opcodeFamily 0x1 op = Jump (extractAddress op)
opcodeFamily 0x2 op = Call (extractAddress op)
opcodeFamily 0x3 op = SkipIfEq (extractFirstRegister op) (extractSecondByte op)
opcodeFamily 0x4 op = SkipIfNeq (extractFirstRegister op) (extractSecondByte op)
opcodeFamily 0x5 op =
  let r1 = extractFirstRegister op in
  let r2 = extractSecondRegister op in
  let valid = extractFourthNibble op in
    if valid == 0 then
      SkipIfRegisterEq r1 r2
    else
      Invalid op
opcodeFamily 0x6 op = LoadRegister (extractFirstRegister op) (extractSecondByte op)
opcodeFamily 0x7 op = AddRegister (extractFirstRegister op) (extractSecondByte op)
-- opcodeFamily 0x8
-- opcodeFamily 0x9
opcodeFamily 0xa op = LoadRegisterI (extractAddress op)
-- opcodeFamily 0xb
opcodeFamily 0xc op = Random (extractFirstRegister op) (extractSecondByte op)
opcodeFamily 0xd op = Display (extractFirstRegister op) (extractSecondRegister op) (extractSprite op)
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

loadRegisterDirect : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
loadRegisterDirect c r v = setRegister c r v

addRegisterDirect : (chip : Chip8) -> (register : Register) -> (value : Value) -> Chip8
addRegisterDirect c r v =
  let value = getRegister c r in
  setRegister c r (value + v)

loadRegisterI : (chip : Chip8) -> (value : LargeValue) -> Chip8
loadRegisterI = setRegisterI

dispatch : (chip : Chip8) -> (opcode : Opcode) -> IO Chip8
dispatch c ClearScreen = pure $ clearScreen c
dispatch c (Jump addr) = pure $ jumpDirect c addr
dispatch c (SkipIfEq r v) = pure $ skipIfRegisterEqual c r v
dispatch c (LoadRegister r v) = pure $ loadRegisterDirect c r v
dispatch c (AddRegister r v) = pure $ addRegisterDirect c r v
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

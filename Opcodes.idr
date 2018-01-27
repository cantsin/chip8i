module Opcodes

import Data.Bits
import Data.Fin

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
  -- | AddRegister  Register Value
  -- | Random       Register Value
  -- register to register
  | SkipIfRegisterEq   Register Register
  -- | SkipIfRegisterNeq  Register Register
  -- | CopyRegister       Register Register
  -- | OrRegister         Register Register
  -- | AndRegister        Register Register
  -- | XorRegister        Register Register
  -- | AddRegisterCarry   Register Register
  -- | SubRegister        Register Register
  -- | SubRegisterInverse Register Register
  -- | ShiftRightRegister Register Register
  -- | ShiftLeftRegister  Register Register
  -- -- value
  -- | LoadRegisterI Bits16 -- TODO spec is ambiguous, I is 8 bit but we're loading a 12 bit value?
  -- | JumpRegister0 Address
  -- -- registers and value
  -- | Display Register Register Sprite
  -- -- key
  -- | SkipIfKeyPressed     Key
  -- | SkipIfKeyNotPressed  Key
  -- | LoadRegisterIFromKey Key
  -- -- register
  -- | LoadRegisterDelay    Register
  -- | WaitForKeyPress      Register
  -- | SetDelayFromRegister Register
  -- | SetSoundFromRegister Register
  -- | AddRegisterI         Register
  -- | StoreBCD             Register
  -- | DumpRegisters        Register
  -- | LoadRegisters        Register

export
Show Opcode where
  show (Invalid op) = "INVALID " ++ show op
  show ClearScreen = "CLS"
  show Return = "RET"
  show (Jump a) = "JP " ++ show a
  show (Call a) = "CALL " ++ show a
  show (SkipIfEq r v) = "SE V" ++ show r ++ ", " ++ show v
  show (SkipIfNeq r v) = "SNE V" ++ show r ++ ", " ++ show v
  show (SkipIfRegisterEq r1 r2) = "SE V" ++ show r1 ++ ", V" ++ show r2
  show (LoadRegister r v) = "LD V" ++ show r ++ ", " ++ show v

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

export
opcode : (value : Bits16) -> Opcode
opcode op =
  let family: Int = cast $ extractFirstNibble op in
  opcodeFamily family op

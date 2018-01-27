module Opcodes

import Data.Bits

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
  -- no parameter
    ClearScreen
  -- | Return
  -- address
  | Jump Address
  -- | Call Address
  -- -- register and value
  -- | SkipIfEq     Register Value
  -- | SkipIfNeq    Register Value
  -- | LoadRegister Register Value
  -- | AddRegister  Register Value
  -- | Random       Register Value
  -- -- register to register
  -- | SkipIfRegisterEq   Register Register
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
  show ClearScreen = "CLS"
  show (Jump a) = "JMP " ++ show a

export
opcode : (value : Bits16) -> Opcode
opcode v =
  let shift = intToBits 12 in
  let mask = intToBits 0xf000 in
  let opInt : Int = (cast v) in
  let op: Bits 16 = intToBits (cast opInt) in
  let family = (and op mask) `shiftRightLogical` shift in
  case bitsToInt family of
    0 => ClearScreen
    n => Jump (the Bits16 $ fromInteger n)

  -- case v of
  --   0x00e0 => ClearScreen
  --   0x00ee => Return
  --   n => Jump n

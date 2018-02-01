module Opcodes

import Data.Buffer
import Data.Bits
import Data.Fin
import Effects
import Effect.StdIO
import Effect.Random
import Effect.State

import Cpu
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

SpriteLength : Type
SpriteLength = Fin 16

-- always in position 0x000S
extractSpriteLength : (value : Bits16) -> SpriteLength
extractSpriteLength v = cast $ extractFourthNibble v

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
  | Display                Register Register SpriteLength
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
  show (ShiftRightRegister r1 r2) = "SHR "     ++ show r1
  show (SubRegisterInverse r1 r2) = "SUBN "    ++ show r1 ++ ", " ++ show r2
  show (ShiftLeftRegister r1 r2)  = "SHL "     ++ show r1
  show (SkipIfRegisterNeq r1 r2)  = "SNE "     ++ show r1 ++ ", " ++ show r2
  show (LoadRegisterI a)          = "LD I, "   ++ show a
  show (JumpRegister0 a)          = "JP V0, "  ++ show a
  show (Random r v)               = "RND "     ++ show r  ++ ", " ++ show v
  show (Display r1 r2 s)          = "DRW "     ++ show r1 ++ ", " ++ show r2 ++ ", I[" ++ show (finToNat s) ++ "]"
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

export
opcode : (value : Bits16) -> Opcode
opcode op =
  let family: Int = cast $ extractFirstNibble op in
  opcodeDispatch family op
  where
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
    opcodeDispatch 0xd op = Display       (extractFirstRegister op) (extractSecondRegister op) (extractSpriteLength op)
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

-- opcode implementations

clearScreen : (chip8 : Chip8) -> Chip8
clearScreen chip =
  record { Display = newScreen } chip

jumpDirect : (cpu : Cpu) -> (address : Address) -> Cpu
jumpDirect c addr =
  if (getPC c == (addr + 2)) then
    ?infiniteLoop
  else
    setPC c addr

skipIfRegisterEqual : (cpu : Cpu) -> (register : Register) -> (value : Value) -> Cpu
skipIfRegisterEqual c r v =
  let toCompare = getRegister c r in
  if v == toCompare then
    incrementPC c
  else
    c

skipIfRegisterNotEqual : (cpu : Cpu) -> (register : Register) -> (value : Value) -> Cpu
skipIfRegisterNotEqual c r v =
  let toCompare = getRegister c r in
  if v /= toCompare then
    incrementPC c
  else
    c

skipIfRegistersEqual : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
skipIfRegistersEqual c r1 r2 =
  let v1 = getRegister c r1 in
  let v2 = getRegister c r2 in
  if v1 == v2 then
    incrementPC c
  else
    c

copyRegisters : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
copyRegisters c r1 r2 =
  let value = getRegister c r2 in
  setRegister c r1 value

orRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
orRegister c r1 r2 =
  let v1 = cast $ getRegister c r1 in
  let v2 = cast $ getRegister c r2 in
  let result = cast $ bitsToInt (v1 `or` v2) in
  setRegister c r1 result

andRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
andRegister c r1 r2 =
  let v1 = cast $ getRegister c r1 in
  let v2 = cast $ getRegister c r2 in
  let result = cast $ bitsToInt (v1 `and` v2) in
  setRegister c r1 result

xorRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
xorRegister c r1 r2 =
  let v1 = cast $ getRegister c r1 in
  let v2 = cast $ getRegister c r2 in
  let result = cast $ bitsToInt (v1 `xor` v2) in
  setRegister c r1 result

addRegisterCarry : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
addRegisterCarry c r1 r2 =
  ?addregistercarry

subtractRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
subtractRegister c r1 r2 =
  let v1 : Int = cast $ getRegister c r1 in
  let v2 : Int = cast $ getRegister c r2 in
  let result = cast (v1 - v2) in
  let flag = if v1 > v2 then 1 else 0 in
  let newChip = setRegister c r1 result in
  setRegisterFlag newChip flag

shiftRightRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
shiftRightRegister c r _ =
  let v = cast $ getRegister c r in
  let one = intToBits 0x1 in
  let result = cast $ bitsToInt (v `shiftRightLogical` one) in
  let flag = cast $ bitsToInt (v `and` one) in
  let newChip = setRegister c r result in
  setRegisterFlag newChip flag

subtractRegisterInverse : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
subtractRegisterInverse c r1 r2 =
  let v1 : Int = cast $ getRegister c r1 in
  let v2 : Int = cast $ getRegister c r2 in
  let result = cast (v2 - v1) in
  let flag = if v2 > v1 then 1 else 0 in
  let newChip = setRegister c r1 result in
  setRegisterFlag newChip flag

shiftLeftRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
shiftLeftRegister c r _ =
  let v = getRegister c r in
  let one = intToBits 0x1 in
  let result = cast $ bitsToInt ((cast v) `shiftLeft` one) in
  let flag = if v > 0x7f then 1 else 0 in
  let newChip = setRegister c r result in
  setRegisterFlag newChip flag

skipIfRegistersNotEqual : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
skipIfRegistersNotEqual c r1 r2 =
  let v1 = getRegister c r1 in
  let v2 = getRegister c r2 in
  if v1 /= v2 then
    incrementPC c
  else
    c

addRegisterDirect : (cpu : Cpu) -> (register : Register) -> (value : Value) -> Cpu
addRegisterDirect c r v =
  let value = getRegister c r in
  setRegister c r (value + v)

jumpRegister0 : (cpu : Cpu) -> (address : Address) -> Cpu
jumpRegister0 c addr =
  let value : Bits16 = cast $ getRegister c 0 in
  let newAddress = addr + value in
  jumpDirect c newAddress

andRandomValue : (chip : Chip8) -> (cpu : Cpu) -> (register : Register) -> (value : Value) -> Chip8
andRandomValue chip c r v =
  let value = RandomNumber chip in
  let mask = value `and` intToBits v in
  let cpu = setRegister c r $ cast mask in
  record { Reseed = True, Computer = cpu } chip

display : (cpu : Cpu) -> (register : Register) -> (register : Register) -> (sprite: SpriteLength) -> Cpu
display c r1 r2 s =
  ?display

skipIfKeyPressed : (cpu : Cpu) -> (register : Register) -> Cpu
skipIfKeyPressed c r =
  ?skipIfKeyPressed

skipIfKeyNotPressed : (cpu : Cpu) -> (register : Register) -> Cpu
skipIfKeyNotPressed c r =
  ?skipIfKeyNotPressed

loadRegisterDelay : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisterDelay c r =
  ?loadRegisterDelay

waitForKeyPress : (cpu : Cpu) -> (register : Register) -> Cpu
waitForKeyPress c r =
  ?waitForKeyPress

setDelayFromRegister : (cpu : Cpu) -> (register : Register) -> Cpu
setDelayFromRegister c r =
  ?setDelayFromRegisteregister

setSoundFromRegister : (cpu : Cpu) -> (register : Register) -> Cpu
setSoundFromRegister c r =
  ?setSoundFromRegisteregister

addRegisterI : (cpu : Cpu) -> (register : Register) -> Cpu
addRegisterI c r =
  ?addRegisterI

loadRegisterWithSprite : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisterWithSprite c r =
  ?loadRegisterWithSprite

storeBCD : (cpu : Cpu) -> (register : Register) -> Cpu
storeBCD c r =
  ?storeBCD

dumpRegisters : (cpu : Cpu) -> (register : Register) -> Cpu
dumpRegisters c r =
  ?dumpRegisters

loadRegisters : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisters c r =
  ?loadRegisters

dispatch : (cpu : Cpu) -> (opcode : Opcode) -> Cpu
dispatch c ClearScreen                = clearScreen c
dispatch c Return                     = popStack c
dispatch c (Jump addr)                = jumpDirect c addr
dispatch c (Call addr)                = pushStack c
dispatch c (SkipIfEq r v)             = skipIfRegisterEqual c r v
dispatch c (SkipIfNeq r v)            = skipIfRegisterNotEqual c r v
dispatch c (SkipIfRegisterEq r1 r2)   = skipIfRegistersEqual c r1 r2
dispatch c (LoadRegister r v)         = setRegister c r v
dispatch c (AddRegister r v)          = addRegisterDirect c r v
dispatch c (CopyRegister r1 r2)       = copyRegisters c r1 r2
dispatch c (OrRegister r1 r2)         = orRegister c r1 r2
dispatch c (AndRegister r1 r2)        = andRegister c r1 r2
dispatch c (XorRegister r1 r2)        = xorRegister c r1 r2
dispatch c (AddRegisterCarry r1 r2)   = addRegisterCarry c r1 r2
dispatch c (SubRegister r1 r2)        = subtractRegister c r1 r2
dispatch c (ShiftRightRegister r1 r2) = shiftRightRegister c r1 r2
dispatch c (SubRegisterInverse r1 r2) = subtractRegisterInverse c r1 r2
dispatch c (ShiftLeftRegister r1 r2)  = shiftLeftRegister c r1 r2
dispatch c (SkipIfRegisterNeq r1 r2)  = skipIfRegistersNotEqual c r1 r2
dispatch c (LoadRegisterI r)          = setRegisterI c r
dispatch c (JumpRegister0 addr)       = jumpRegister0 c addr
dispatch c (Random r v)               = andRandomValue c r v
dispatch c (Display r1 r2 s)          = display c r1 r2 s
dispatch c (SkipIfKeyPressed r)       = skipIfKeyPressed c r
dispatch c (SkipIfKeyNotPressed r)    = skipIfKeyNotPressed c r
dispatch c (LoadRegisterDelay r)      = loadRegisterDelay c r
dispatch c (WaitForKeyPress r)        = waitForKeyPress c r
dispatch c (SetDelayFromRegister r)   = setDelayFromRegister c r
dispatch c (SetSoundFromRegister r)   = setSoundFromRegister c r
dispatch c (AddRegisterI r)           = addRegisterI c r
dispatch c (LoadRegisterWithSprite r) = loadRegisterWithSprite c r
dispatch c (StoreBCD r)               = storeBCD c r
dispatch c (DumpRegisters r)          = dumpRegisters c r
dispatch c (LoadRegisters r)          = loadRegisters c r

export
runOneCycle : (cpu : Cpu) -> (opcode : Opcode) -> IO Cpu
runOneCycle cpu opcode =
  do
    putStrLn $ (show cpu) ++ " => " ++ (show opcode)
    case opcode of
      Invalid _ =>
        do
          putStrLn $ "terminating unexpectedly"
          pure $ cpu
      _ =>
        do
          pure $ dispatch cpu opcode

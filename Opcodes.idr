module Opcodes

import Effects
import Effect.Random
import Effect.State
import Data.Bits
import Data.Fin
import Data.Vect

import Cpu
import Screen
import Chip8
import Utilities
import Ram
import MemoryIO

%default total

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

Show Opcode where
  show (Invalid op)               = "INVALID " ++ show op
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

extractOpcode : (value : Bits16) -> Opcode
extractOpcode op =
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

clearScreen : (chip : Chip8) -> Chip8
clearScreen chip =
  let cpu = Computer chip in
  record { Display = newScreen, Computer = incrementPC cpu } chip

jumpDirect : (chip : Chip8) -> (address : Address) -> Chip8
jumpDirect chip addr =
  let cpu = Computer chip in
  if (getPC cpu == addr) then
    record { State = Halted "Infinite loop" } chip
  else
    record { Computer = setPC cpu addr } chip

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
  let v1 : Bits16 = cast $ getRegister c r1 in
  let v2 : Bits16 = cast $ getRegister c r2 in
  let result : Bits16 = v1 + v2 in
  let flag = if result > 0xff then 1 else 0 in
  let masked = extractSecondByte result in
  let cpu = setRegister c r1 masked in
  setRegisterFlag cpu flag

subtractRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
subtractRegister c r1 r2 =
  let v1 : Int = cast $ getRegister c r1 in
  let v2 : Int = cast $ getRegister c r2 in
  let result = cast (v1 - v2) in
  let flag = if v1 > v2 then 1 else 0 in
  let cpu = setRegister c r1 result in
  setRegisterFlag cpu flag

shiftRightRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
shiftRightRegister c r _ =
  let v = cast $ getRegister c r in
  let one = intToBits 0x1 in
  let result = cast $ bitsToInt (v `shiftRightLogical` one) in
  let flag = cast $ bitsToInt (v `and` one) in
  let cpu = setRegister c r result in
  setRegisterFlag cpu flag

subtractRegisterInverse : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
subtractRegisterInverse c r1 r2 =
  let v1 : Int = cast $ getRegister c r1 in
  let v2 : Int = cast $ getRegister c r2 in
  let result = cast (v2 - v1) in
  let flag = if v2 > v1 then 1 else 0 in
  let cpu = setRegister c r1 result in
  setRegisterFlag cpu flag

shiftLeftRegister : (cpu : Cpu) -> (register : Register) -> (register : Register) -> Cpu
shiftLeftRegister c r _ =
  let v = getRegister c r in
  let one = intToBits 0x1 in
  let result = cast $ bitsToInt ((cast v) `shiftLeft` one) in
  let flag = if v > 0x7f then 1 else 0 in
  let cpu = setRegister c r result in
  setRegisterFlag cpu flag

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

jumpRegister0 : (chip : Chip8) -> (address : Address) -> Chip8
jumpRegister0 chip addr =
  let cpu = Computer chip in
  let value : Bits16 = cast $ getRegister cpu 0 in
  let newAddress = addr + value in
  jumpDirect chip newAddress

partial
andRandomValue : (register : Register) -> (value : Value) -> { [Chip8 ::: STATE Chip8, RND] } Eff ()
andRandomValue r v =
  let chip = !(Chip8 :- get) in
  let cpu = Computer chip in
  let mask = cast v in
  let rand = !(rndInt 0x00 0xff) in
  let value = cast rand `and` mask in
  let newCpu = setRegister cpu r $ cast value in
  let newChip = record { Computer = incrementPC newCpu } chip in
  Chip8 :- put newChip

display : (register : Register) -> (register : Register) -> (sprite: SpriteLength) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
display r1 r2 s =
  let chip = !(Chip8 :- get) in
  let cpu = Computer chip in
  let screen = Display chip in
  let x = cast $ getRegister cpu r1 in
  let y = cast $ getRegister cpu r2 in
  let loadAddress = cast $ getRegisterI cpu in
  let sprite = !(loadBlock loadAddress s) in
  let newDisplay = writeSpriteToScreen screen sprite x y in
  let newChip = record { Display = newDisplay, Computer = incrementPC cpu } chip in
  Chip8 :- put newChip

skipIfKeyPressed : (chip : Chip8) -> (register : Register) -> Chip8
skipIfKeyPressed chip r =
  let cpu = Computer chip in
  let k = cast $ getRegister cpu r in
  let pressed = isKeyPressed chip k in
  if pressed then
    record { Computer = incrementPC $ incrementPC cpu } chip
  else
    record { Computer = incrementPC cpu } chip

skipIfKeyNotPressed : (chip : Chip8) -> (register : Register) -> Chip8
skipIfKeyNotPressed chip r =
  let cpu = Computer chip in
  let k = cast $ getRegister cpu r in
  let pressed = isKeyPressed chip k in
  if pressed then
    record { Computer = incrementPC cpu } chip
  else
    record { Computer = incrementPC $ incrementPC cpu } chip

loadRegisterDelay : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisterDelay c r =
  let countdown = getDelayTimer c in
  setRegister c r countdown

waitForKeyPress : (chip : Chip8) -> (register : Register) -> Chip8
waitForKeyPress chip r =
  record { State = WaitingForKey r } chip

setDelayFromRegister : (cpu : Cpu) -> (register : Register) -> Cpu
setDelayFromRegister c r =
  let countdown = getRegister c r in
  setDelayTimer c countdown

setSoundFromRegister : (cpu : Cpu) -> (register : Register) -> Cpu
setSoundFromRegister c r =
  let countdown = getRegister c r in
  setSoundTimer c countdown

addRegisterI : (cpu : Cpu) -> (register : Register) -> Cpu
addRegisterI c r =
  let value = getRegister c r in
  let currentI = getRegisterI c in
  let newI = currentI + (cast value) in
  setRegisterI c newI

loadRegisterWithSprite : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisterWithSprite c r =
  let v : Int = cast $ getRegister c r in
  let index = restrict 15 $ cast v in
  let address = defaultSpriteStartingAddress index in
  setRegisterI c address

partial
storeBCD : (register : Register) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
storeBCD r =
  let chip = !(Chip8 :- get) in
  let cpu = Computer chip in
  let value = getRegister cpu r in
  let dumpAddress = cast $ getRegisterI cpu in
  let first = value `div` 100 in
  let second = (value `mod` 100) `div` 10 in
  let third = value `mod` 10 in
  let digits = [first, second, third] in
  do
    dumpBlock dumpAddress digits
    Chip8 :- put (record { Computer = incrementPC cpu } chip)

partial
dumpRegisters : (register : Register) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
dumpRegisters r =
  let chip = !(Chip8 :- get) in
  let cpu = Computer chip in
  let dumpAddress = cast $ getRegisterI cpu in
  let value : Int = cast $ getRegister cpu r in
  let len = restrict 15 $ cast value in
  let registers = getRegisters cpu len in
  do
    dumpBlock dumpAddress registers
    Chip8 :- put (record { Computer = incrementPC cpu } chip)

partial
loadRegisters : (register : Register) -> { [Chip8 ::: STATE Chip8, RAM] } Eff ()
loadRegisters r =
  let chip = !(Chip8 :- get) in
  let cpu = Computer chip in
  let loadAddress = cast $ getRegisterI cpu in
  let value : Int = cast $ getRegister cpu r in
  let len = restrict 15 $ cast value in
  let registers = !(loadBlock loadAddress len) in
  let newCpu = setRegisters cpu len registers in
  Chip8 :- put (record { Computer = incrementPC cpu } chip)

-- convenience function for pure opcode functions that operate on the CPU only
updateCPU : (cpu : Cpu) -> { [Chip8 ::: STATE Chip8] } Eff ()
updateCPU cpu =
  let chip = !(Chip8 :- get) in
  let newComputer = incrementPC cpu in
  Chip8 :- put (record { Computer = newComputer } chip)

-- convenience function for pure opcode functions that operate on the chip
updateChip : (chip : Chip8) -> { [Chip8 ::: STATE Chip8] } Eff ()
updateChip newChip = Chip8 :- put newChip

partial
dispatch : (opcode : Opcode) -> { [Chip8 ::: STATE Chip8, RAM, RND] } Eff ()
dispatch ClearScreen                = updateChip $ clearScreen                       !(Chip8 :- get)
dispatch Return                     = updateCPU  $ popStack                (Computer !(Chip8 :- get))
dispatch (Jump addr)                = updateChip $ jumpDirect                        !(Chip8 :- get)  addr
dispatch (Call addr)                = updateCPU  $ pushStack               (Computer !(Chip8 :- get)) addr
dispatch (SkipIfEq r v)             = updateCPU  $ skipIfRegisterEqual     (Computer !(Chip8 :- get)) r  v
dispatch (SkipIfNeq r v)            = updateCPU  $ skipIfRegisterNotEqual  (Computer !(Chip8 :- get)) r  v
dispatch (SkipIfRegisterEq r1 r2)   = updateCPU  $ skipIfRegistersEqual    (Computer !(Chip8 :- get)) r1 r2
dispatch (LoadRegister r v)         = updateCPU  $ setRegister             (Computer !(Chip8 :- get)) r  v
dispatch (AddRegister r v)          = updateCPU  $ addRegisterDirect       (Computer !(Chip8 :- get)) r  v
dispatch (CopyRegister r1 r2)       = updateCPU  $ copyRegisters           (Computer !(Chip8 :- get)) r1 r2
dispatch (OrRegister r1 r2)         = updateCPU  $ orRegister              (Computer !(Chip8 :- get)) r1 r2
dispatch (AndRegister r1 r2)        = updateCPU  $ andRegister             (Computer !(Chip8 :- get)) r1 r2
dispatch (XorRegister r1 r2)        = updateCPU  $ xorRegister             (Computer !(Chip8 :- get)) r1 r2
dispatch (AddRegisterCarry r1 r2)   = updateCPU  $ addRegisterCarry        (Computer !(Chip8 :- get)) r1 r2
dispatch (SubRegister r1 r2)        = updateCPU  $ subtractRegister        (Computer !(Chip8 :- get)) r1 r2
dispatch (ShiftRightRegister r1 r2) = updateCPU  $ shiftRightRegister      (Computer !(Chip8 :- get)) r1 r2
dispatch (SubRegisterInverse r1 r2) = updateCPU  $ subtractRegisterInverse (Computer !(Chip8 :- get)) r1 r2
dispatch (ShiftLeftRegister r1 r2)  = updateCPU  $ shiftLeftRegister       (Computer !(Chip8 :- get)) r1 r2
dispatch (SkipIfRegisterNeq r1 r2)  = updateCPU  $ skipIfRegistersNotEqual (Computer !(Chip8 :- get)) r1 r2
dispatch (LoadRegisterI r)          = updateCPU  $ setRegisterI            (Computer !(Chip8 :- get)) r
dispatch (JumpRegister0 addr)       = updateChip $ jumpRegister0                     !(Chip8 :- get)  addr
dispatch (Random r v)               =              andRandomValue                                     r  v
dispatch (Display r1 r2 s)          =              display                                            r1 r2 s
dispatch (SkipIfKeyPressed r)       = updateChip $ skipIfKeyPressed                  !(Chip8 :- get)  r
dispatch (SkipIfKeyNotPressed r)    = updateChip $ skipIfKeyNotPressed               !(Chip8 :- get)  r
dispatch (LoadRegisterDelay r)      = updateCPU  $ loadRegisterDelay       (Computer !(Chip8 :- get)) r
dispatch (WaitForKeyPress r)        = updateChip $ waitForKeyPress                   !(Chip8 :- get)  r
dispatch (SetDelayFromRegister r)   = updateCPU  $ setDelayFromRegister    (Computer !(Chip8 :- get)) r
dispatch (SetSoundFromRegister r)   = updateCPU  $ setSoundFromRegister    (Computer !(Chip8 :- get)) r
dispatch (AddRegisterI r)           = updateCPU  $ addRegisterI            (Computer !(Chip8 :- get)) r
dispatch (LoadRegisterWithSprite r) = updateCPU  $ loadRegisterWithSprite  (Computer !(Chip8 :- get)) r
dispatch (StoreBCD r)               =              storeBCD                                           r
dispatch (DumpRegisters r)          =              dumpRegisters                                      r
dispatch (LoadRegisters r)          =              loadRegisters                                      r

export
partial
runOneCycle : (tick : Bool) -> { [Chip8 ::: STATE Chip8, RAM, RND] } Eff ()
runOneCycle tick =
  let instruction = extractOpcode !getOpcode in
  case instruction of
    Invalid _ =>
      let errorMessage = "Unknown opcode: " ++ (show instruction) in
      let chip = !(Chip8 :- get) in
      Chip8 :- put (record { State = Halted errorMessage } chip)
    _ =>
      do
        dispatch instruction
        chip <- Chip8 :- get
        newComputer <- pure $ updateCPUTimers (Computer chip) tick
        Chip8 :- put (record { Computer = newComputer } chip)

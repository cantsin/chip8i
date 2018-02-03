module Opcodes

import Effects
import Effect.Random
import Effect.System
import Data.Buffer
import Data.Bits
import Data.Fin
import Data.Vect

import Cpu
import Screen
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

-- special: modifies external state
clearScreen : (chip8 : Chip8) -> Chip8
clearScreen chip =
  let c = getComputer chip in
  record { Display = newScreen, Computer = incrementPC c } chip

return : (chip8 : Chip8) -> Chip8
return chip =
  let c = getComputer chip in
  record { Computer = popStack c } chip

jumpDirect : (chip : Chip8) -> (address : Address) -> Chip8
jumpDirect chip addr =
  let c = getComputer chip in
  if (getPC c == addr) then
    record { Halted = True, Error = "Infinite loop" } chip
  else
    record { Computer = setPC c addr } chip

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
  let newChip = setRegister c r1 masked in
  setRegisterFlag newChip flag

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

jumpRegister0 : (chip : Chip8) -> (address : Address) -> Chip8
jumpRegister0 chip addr =
  let c = getComputer chip in
  let value : Bits16 = cast $ getRegister c 0 in
  let newAddress = addr + value in
  jumpDirect chip newAddress

-- this is a hack. we do not run effects in one block, so fake
-- randomness by seeding with the CPU counter multiplied by the
-- current time. Idris effects seem deprecated, anyhow.
partial
getRandomByte : (counter : Integer) -> Eff Bits8 [RND, SYSTEM]
getRandomByte counter =
  do
    t <- time
    srand $ t * counter
    val <- rndInt 0x00 0xff
    pure $ cast val

-- special: modifies external state
partial
andRandomValue : (chip : Chip8) -> (register : Register) -> (value : Value) -> IO Chip8
andRandomValue chip r v =
  let c = getComputer chip in
  let mask = cast v in
  do
    rand <- run $ getRandomByte (getCounter chip)
    value <- pure $ cast rand `and` mask
    cpu <- pure $ setRegister c r $ cast value
    pure $ record { Computer = incrementPC cpu } chip

-- special: modifies external state
display : (chip : Chip8) -> (register : Register) -> (register : Register) -> (sprite: SpriteLength) -> IO Chip8
display chip r1 r2 s =
  let c = getComputer chip in
  let screen = getDisplay chip in
  let x = cast $ getRegister c r1 in
  let y = cast $ getRegister c r2 in
  let loadAddress = cast $ getRegisterI c in
  do
    sprite <- loadBlock chip loadAddress s
    newDisplay <- pure $ writeSpriteToScreen screen sprite x y
    pure $ record { Display = newDisplay, Computer = incrementPC c } chip

-- special: accesses external state
skipIfKeyPressed : (chip : Chip8) -> (register : Register) -> Chip8
skipIfKeyPressed chip r =
  let c = getComputer chip in
  let k = cast $ getRegister c r in
  let pressed = isKeyPressed chip k in
  if pressed then
    record { Computer = incrementPC $ incrementPC c } chip
  else
    record { Computer = incrementPC c } chip

-- special: accesses external state
skipIfKeyNotPressed : (chip8 : Chip8) -> (register : Register) -> Chip8
skipIfKeyNotPressed chip r =
  let c = getComputer chip in
  let k = cast $ getRegister c r in
  let pressed = isKeyPressed chip k in
  if pressed then
    record { Computer = incrementPC c } chip
  else
    record { Computer = incrementPC $ incrementPC c } chip

loadRegisterDelay : (cpu : Cpu) -> (register : Register) -> Cpu
loadRegisterDelay c r =
  let countdown = getDelayTimer c in
  setRegister c r countdown

-- special: modifies external state
waitForKeyPress : (chip : Chip8) -> (register : Register) -> IO Chip8
waitForKeyPress chip r =
  let c = getComputer chip in
  let k = ?waitForKeyPress chip in
  let cpu = setRegister c r k in
  do
    pure $ record { Computer = incrementPC cpu } chip

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
storeBCD : (chip : Chip8) -> (register : Register) -> IO Chip8
storeBCD chip r =
  let c = getComputer chip in
  let value = getRegister c r in
  let dumpAddress = cast $ getRegisterI c in
  let first = value `div` 100 in
  let second = (value `mod` 100) `div` 10 in
  let third = value `mod` 10 in
  let digits = [first, second, third] in
  do
    dumpBlock chip dumpAddress digits
    pure $ record { Computer = incrementPC c } chip

dumpRegisters : (chip : Chip8) -> (register : Register) -> IO Chip8
dumpRegisters chip r =
  let c = getComputer chip in
  let dumpAddress = cast $ getRegisterI c in
  let value : Int = cast $ getRegister c r in
  let len = restrict 15 $ cast value in
  let registers = getRegisters c len in
  do
    dumpBlock chip dumpAddress registers
    pure $ record { Computer = incrementPC c } chip

loadRegisters : (chip : Chip8) -> (register : Register) -> IO Chip8
loadRegisters chip r =
  let c = getComputer chip in
  let loadAddress = cast $ getRegisterI c in
  let value : Int = cast $ getRegister c r in
  let len = restrict 15 $ cast value in
  do
    registers <- loadBlock chip loadAddress len
    cpu <- pure $ setRegisters c len registers
    pure $ record { Computer = incrementPC cpu } chip

-- convenience function for most opcodes
updateCPU : (chip : Chip8) -> (cpu : Cpu) -> IO Chip8
updateCPU chip c =
  pure $ record { Computer = incrementPC c } chip

partial
dispatch : (chip : Chip8) -> (opcode : Opcode) -> IO Chip8
dispatch chip ClearScreen                = pure $ clearScreen chip
dispatch chip Return                     = pure $ return chip
dispatch chip (Jump addr)                = pure $ jumpDirect chip addr
dispatch chip (Call addr)                = updateCPU chip $ pushStack (getComputer chip)
dispatch chip (SkipIfEq r v)             = updateCPU chip $ skipIfRegisterEqual (getComputer chip) r v
dispatch chip (SkipIfNeq r v)            = updateCPU chip $ skipIfRegisterNotEqual (getComputer chip) r v
dispatch chip (SkipIfRegisterEq r1 r2)   = updateCPU chip $ skipIfRegistersEqual (getComputer chip) r1 r2
dispatch chip (LoadRegister r v)         = updateCPU chip $ setRegister (getComputer chip) r v
dispatch chip (AddRegister r v)          = updateCPU chip $ addRegisterDirect (getComputer chip) r v
dispatch chip (CopyRegister r1 r2)       = updateCPU chip $ copyRegisters (getComputer chip) r1 r2
dispatch chip (OrRegister r1 r2)         = updateCPU chip $ orRegister (getComputer chip) r1 r2
dispatch chip (AndRegister r1 r2)        = updateCPU chip $ andRegister (getComputer chip) r1 r2
dispatch chip (XorRegister r1 r2)        = updateCPU chip $ xorRegister (getComputer chip) r1 r2
dispatch chip (AddRegisterCarry r1 r2)   = updateCPU chip $ addRegisterCarry (getComputer chip) r1 r2
dispatch chip (SubRegister r1 r2)        = updateCPU chip $ subtractRegister (getComputer chip) r1 r2
dispatch chip (ShiftRightRegister r1 r2) = updateCPU chip $ shiftRightRegister (getComputer chip) r1 r2
dispatch chip (SubRegisterInverse r1 r2) = updateCPU chip $ subtractRegisterInverse (getComputer chip) r1 r2
dispatch chip (ShiftLeftRegister r1 r2)  = updateCPU chip $ shiftLeftRegister (getComputer chip) r1 r2
dispatch chip (SkipIfRegisterNeq r1 r2)  = updateCPU chip $ skipIfRegistersNotEqual (getComputer chip) r1 r2
dispatch chip (LoadRegisterI r)          = updateCPU chip $ setRegisterI (getComputer chip) r
dispatch chip (JumpRegister0 addr)       = pure $ jumpRegister0 chip addr
dispatch chip (Random r v)               = andRandomValue chip r v
dispatch chip (Display r1 r2 s)          = display chip r1 r2 s
dispatch chip (SkipIfKeyPressed r)       = pure $ skipIfKeyPressed chip r
dispatch chip (SkipIfKeyNotPressed r)    = pure $ skipIfKeyNotPressed chip r
dispatch chip (LoadRegisterDelay r)      = updateCPU chip $ loadRegisterDelay (getComputer chip) r
dispatch chip (WaitForKeyPress r)        = waitForKeyPress chip r
dispatch chip (SetDelayFromRegister r)   = updateCPU chip $ setDelayFromRegister (getComputer chip) r
dispatch chip (SetSoundFromRegister r)   = updateCPU chip $ setSoundFromRegister (getComputer chip) r
dispatch chip (AddRegisterI r)           = updateCPU chip $ addRegisterI (getComputer chip) r
dispatch chip (LoadRegisterWithSprite r) = updateCPU chip $ loadRegisterWithSprite (getComputer chip) r
dispatch chip (StoreBCD r)               = storeBCD chip r
dispatch chip (DumpRegisters r)          = dumpRegisters chip r
dispatch chip (LoadRegisters r)          = loadRegisters chip r

export
partial
runOneCycle : (chip : Chip8) -> (tick : Bool) -> IO Chip8
runOneCycle chip tick =
  do
    opcodeValue <- getOpcode chip
    instruction <- pure $ extractOpcode opcodeValue
    case instruction of
      Invalid _ =>
        let errorMessage = "Unknown opcode: " ++ (show instruction) in
        pure $ record { Halted = True, Error = errorMessage } chip
      _ =>
        do
          -- debugging
          putStrLn $ (show $ getDisplay chip)
          putStrLn $ (show $ getComputer chip) ++ " => " ++ (show instruction)
          modifiedChip <- dispatch chip instruction
          modifiedComputer <- pure $ updateCPUTimers (getComputer modifiedChip) tick
          pure $ record { Computer = modifiedComputer } modifiedChip

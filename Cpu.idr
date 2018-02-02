module Cpu

import System
import Data.Buffer
import Data.Vect

import Utilities
import Constants

export
record Cpu where
  constructor MkCpu
  -- 16 general purpose 8-bit registers, V0 to Vf
  V : Vect 16 Bits8
  -- 16-bit register called I
  I : Bits16
  -- pseudo PC (16-bit)
  PC : Bits16
  -- stack is an array of 16 16-bit values
  Stack : Vect 16 Bits16
  -- pseudo SP
  SP : Fin 16
  -- delay time register DT
  DT : Bits8
  -- sound timer register ST
  ST : Bits8

export
Show Cpu where
  show c = show (PC c) ++ " " ++ show (V c) ++ " " ++ show (I c)

export
newCpu : Cpu
newCpu =
  let v = Vect.replicate 16 0 in
  let stack = Vect.replicate 16 0 in
  let pc : Bits16 = cast StartingAddress in
  MkCpu v 0 pc stack 0 0 0

export
incrementPC : (cpu : Cpu) -> Cpu
incrementPC c =
  record { PC $= (+ 2) } c

export
setPC : (cpu : Cpu) -> (value : Bits16) -> Cpu
setPC c v =
  record { PC = v } c

export
getPC : (cpu : Cpu) -> Bits16
getPC = PC

export
getRegister : (cpu : Cpu) -> (index : Fin 16) -> Bits8
getRegister c i =
  Vect.index i (V c)

export
setRegister : (cpu : Cpu) -> (index : Fin 16) -> (value : Bits8) -> Cpu
setRegister c i v =
  let newV = replaceAt i v (V c) in
  record { V = newV } c

export
setRegisterFlag : (cpu : Cpu) -> (value : Bits8) -> Cpu
setRegisterFlag c v =
  let newV = replaceAt 0xf v (V c) in
  record { V = newV } c

export
getRegisterI : (cpu : Cpu) -> Bits16
getRegisterI = I

export
setRegisterI : (cpu : Cpu) -> (value : Bits16) -> Cpu
setRegisterI c v =
  record { I = v } c

export
pushStack : (cpu : Cpu) -> Cpu
pushStack c =
  let index = SP c in
  case toIntegerNat $ finToNat index of
    15 => idris_crash "Cpu: tried to push to full stack"
    n =>
      let newIndex = restrict 15 (n + 1) in
      let newStack = replaceAt newIndex (PC c) (Stack c) in
      record { SP = newIndex, Stack = newStack } c

export
popStack : (cpu : Cpu) -> Cpu
popStack c =
  let index = SP c in
  case index of
    FZ => idris_crash "Cpu: tried to pop from empty stack"
    FS n =>
      let stack = Stack c in
      let newIndex = restrict 15 $ cast n in
      let newPC = Vect.index index stack in
      let newStack = replaceAt index 0 stack in
      record { SP = newIndex, Stack = newStack, PC = newPC } c

export
getDelayTimer : (cpu : Cpu) -> Bits8
getDelayTimer = DT

export
setDelayTimer : (cpu : Cpu) -> (countdown : Bits8) -> Cpu
setDelayTimer c v =
  record { DT = v } c

export
setSoundTimer : (cpu : Cpu) -> (countdown : Bits8) -> Cpu
setSoundTimer c v =
  record { ST = v } c

export
delayTimerIsActive : (cpu : Cpu) -> Bool
delayTimerIsActive c = DT c > 0

export
soundTimerIsActive : (cpu : Cpu) -> Bool
soundTimerIsActive c = ST c > 0

export
updateCPUTimers : (cpu : Cpu) -> (tick : Bool) -> Cpu
updateCPUTimers c tick =
  let currentDT = DT c in
  let currentST = ST c in
  let newDT = if tick && delayTimerIsActive c then decrement currentDT else currentDT in
  let newST = if tick && soundTimerIsActive c then decrement currentST else currentST in
  record { ST = newST, DT = newDT } c
  where
    decrement : (x : Bits8) -> Bits8
    decrement x =
      let xAsInt : Int = cast x in
      case xAsInt of
        0 => 0
        n => cast $ n - 1

module Keybindings

import Effects
import Effect.SDL
import Effect.StdIO
import Effect.State
import Data.Fin

import Chip8
import Cpu
import Keypad
import Utilities

updateKeydown : (pressedKey : Fin 16) -> { [Chip8 ::: STATE Chip8] } Effects.DepEff.Eff Bool
updateKeydown pressedKey =
  do
    chip <- Chip8 :- get
    case getState chip of
      Active =>
        let keypad = setKeyPress (getKeypad chip) pressedKey in
        do
          Chip8 :- put (record { Keys = keypad } chip)
          pure True
      WaitingForKey r =>
        let keypad = setKeyPress (getKeypad chip) pressedKey in
        let cpu = setRegister (getComputer chip) r $ cast pressedKey in
        do
          Chip8 :- put (record { State = Active, Keys = keypad, Computer = incrementPC cpu } chip)
          pure True
      Halted =>
        pure False

updateKeyup : (releasedKey : Fin 16) -> { [Chip8 ::: STATE Chip8] } Effects.DepEff.Eff Bool
updateKeyup releasedKey =
  do
    chip <- Chip8 :- get
    case getState chip of
      Active =>
        let keypad = clearKeyPress (getKeypad chip) releasedKey in
        do
          Chip8 :- put (record { Keys = keypad } chip)
          pure True
      WaitingForKey _ =>
        pure True
      Halted =>
        pure False

export
processKeys : Maybe Event -> { [Chip8 ::: STATE Chip8, STDIO] } Effects.DepEff.Eff Bool
processKeys (Just (KeyDown $ KeyAny '1')) = updateKeydown 0x1
processKeys (Just (KeyUp   $ KeyAny '1')) = updateKeyup   0x1
processKeys (Just (KeyDown $ KeyAny '2')) = updateKeydown 0x2
processKeys (Just (KeyUp   $ KeyAny '2')) = updateKeyup   0x2
processKeys (Just (KeyDown $ KeyAny '3')) = updateKeydown 0x3
processKeys (Just (KeyUp   $ KeyAny '3')) = updateKeyup   0x3
processKeys (Just (KeyDown $ KeyAny '4')) = updateKeydown 0xc
processKeys (Just (KeyUp   $ KeyAny '4')) = updateKeyup   0xc
processKeys (Just (KeyDown $ KeyAny 'q')) = updateKeydown 0x4
processKeys (Just (KeyUp   $ KeyAny 'q')) = updateKeyup   0x4
processKeys (Just (KeyDown $ KeyAny 'w')) = updateKeydown 0x5
processKeys (Just (KeyUp   $ KeyAny 'w')) = updateKeyup   0x5
processKeys (Just (KeyDown $ KeyAny 'e')) = updateKeydown 0x6
processKeys (Just (KeyUp   $ KeyAny 'e')) = updateKeyup   0x6
processKeys (Just (KeyDown $ KeyAny 'r')) = updateKeydown 0xd
processKeys (Just (KeyUp   $ KeyAny 'r')) = updateKeyup   0xd
processKeys (Just (KeyDown $ KeyAny 'a')) = updateKeydown 0x7
processKeys (Just (KeyUp   $ KeyAny 'a')) = updateKeyup   0x7
processKeys (Just (KeyDown $ KeyAny 's')) = updateKeydown 0x8
processKeys (Just (KeyUp   $ KeyAny 's')) = updateKeyup   0x8
processKeys (Just (KeyDown $ KeyAny 'd')) = updateKeydown 0x9
processKeys (Just (KeyUp   $ KeyAny 'd')) = updateKeyup   0x9
processKeys (Just (KeyDown $ KeyAny 'f')) = updateKeydown 0xe
processKeys (Just (KeyUp   $ KeyAny 'f')) = updateKeyup   0xe
processKeys (Just (KeyDown $ KeyAny 'z')) = updateKeydown 0xa
processKeys (Just (KeyUp   $ KeyAny 'z')) = updateKeyup   0xa
processKeys (Just (KeyDown $ KeyAny 'x')) = updateKeydown 0x0
processKeys (Just (KeyUp   $ KeyAny 'x')) = updateKeyup   0x0
processKeys (Just (KeyDown $ KeyAny 'c')) = updateKeydown 0xb
processKeys (Just (KeyUp   $ KeyAny 'c')) = updateKeyup   0xb
processKeys (Just (KeyDown $ KeyAny 'v')) = updateKeydown 0xf
processKeys (Just (KeyUp   $ KeyAny 'v')) = updateKeyup   0xf
processKeys (Just (KeyDown KeyEsc)) = pure False
processKeys (Just AppQuit) =
  do
    chip <- Chip8 :- get
    case isHalted chip of
      Just error => do
        putStrLn $ "Chip8 halted. Reason: " ++ error
        pure False
      _ =>
        pure False
processKeys _ = pure True

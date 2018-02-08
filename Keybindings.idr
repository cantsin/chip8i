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

updateChipKeypad : (pressedKey : Fin 16) -> { [Chip8 ::: STATE Chip8] } Effects.DepEff.Eff Bool
updateChipKeypad pressedKey =
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
          Chip8 :- put (record { Keys = keypad, Computer = incrementPC cpu } chip)
          pure True
      Halted =>
        pure False

-- TODO keyup

export
processKeys : Maybe Event -> { [Chip8 ::: STATE Chip8, STDIO] } Effects.DepEff.Eff Bool
processKeys (Just (KeyDown KeyEsc)) = pure False
processKeys (Just (KeyDown $ KeyAny '1')) = updateChipKeypad 0x1
processKeys (Just (KeyDown $ KeyAny '2')) = updateChipKeypad 0x2
processKeys (Just (KeyDown $ KeyAny '3')) = updateChipKeypad 0x3
processKeys (Just (KeyDown $ KeyAny '4')) = updateChipKeypad 0xc
processKeys (Just (KeyDown $ KeyAny 'q')) = updateChipKeypad 0x4
processKeys (Just (KeyDown $ KeyAny 'w')) = updateChipKeypad 0x5
processKeys (Just (KeyDown $ KeyAny 'e')) = updateChipKeypad 0x6
processKeys (Just (KeyDown $ KeyAny 'r')) = updateChipKeypad 0xd
processKeys (Just (KeyDown $ KeyAny 'a')) = updateChipKeypad 0x7
processKeys (Just (KeyDown $ KeyAny 's')) = updateChipKeypad 0x8
processKeys (Just (KeyDown $ KeyAny 'd')) = updateChipKeypad 0x9
processKeys (Just (KeyDown $ KeyAny 'f')) = updateChipKeypad 0xe
processKeys (Just (KeyDown $ KeyAny 'z')) = updateChipKeypad 0xa
processKeys (Just (KeyDown $ KeyAny 'x')) = updateChipKeypad 0x0
processKeys (Just (KeyDown $ KeyAny 'c')) = updateChipKeypad 0xb
processKeys (Just (KeyDown $ KeyAny 'v')) = updateChipKeypad 0xf
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

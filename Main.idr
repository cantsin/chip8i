module Main

import System
import Data.Buffer

import Constants
import Opcodes
import Chip8

%default total

defaultROM : String
defaultROM = "./roms/maze.rom"

readROMFromFile : (filename : String) -> IO Buffer
readROMFromFile filename =
  let maxLength : Int = RamSize - StartingAddress in
  do
    handle <- openFile filename Read
    case handle of
      Right file => do
        buffer <- Buffer.newBuffer maxLength
        case buffer of
          Just mem =>
            Buffer.readBufferFromFile file mem maxLength
          Nothing =>
            do
              putStrLn "Not enough memory"
              System.exitFailure
      Left _ =>
        do
          putStrLn "Could not read ROM"
          System.exitFailure

getROMPath : List String -> String
getROMPath args =
  case index' 1 args of
    Nothing => defaultROM
    Just path => path

partial
runChip8 : (chip : Chip8) -> IO ()
runChip8 chip =
  if (Halted chip) then
    -- TODO: wait for user to press esc before exiting
    pure ()
  else
    let mustWait = Waiting chip in
    -- The CPU runs at roughly 500Hz, however, we want to tick down the
    -- CPU DT/ST at a rate of 60Hz. As a first approximation, let's say
    -- we tick down DT/ST every 8 CPU cycles.
    let counter = Counter chip in
    let tick = counter `mod` 8 == 1 in
    do
      modifiedChip <- runOneCycle chip tick
      -- TODO: if Waiting then wait for user to press key
      runChip8 $ record { Counter $= (+ 1) } modifiedChip
      -- TODO when to draw screen?

partial
main : IO ()
main =
  do
    args <- getArgs
    chip8 <- newChip8
    rom <- readROMFromFile $ getROMPath args
    loadDefaultSpriteDataAt chip8 DefaultSpriteDataAddress
    loadROMAt chip8 rom StartingAddress
    runChip8 chip8
    putStrLn "Fin."

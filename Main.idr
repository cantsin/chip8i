module Main

import System
import Data.Buffer
import Effects
import Effect.StdIO
import Effect.Random
import Effect.State

import Constants
import Chip8

%default total

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

-- testing purposes
execute : Chip8 -> Eff () [RND, STDIO]
execute chip =
  do
    srand 12345
    putStrLn "Fin."

partial
main : IO ()
main =
  do
    chip8 <- newChip8
    rom <- readROMFromFile "./roms/maze.rom"
    loadROMAt chip8 rom StartingAddress
    runChip8 chip8

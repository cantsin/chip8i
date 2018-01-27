module Main

import System
import Data.Buffer

import Chip8
import Opcodes
import Constants

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

partial
main : IO ()
main = do
  chip <- newChip
  rom <- readROMFromFile "./roms/maze.rom"
  loadROMAt chip rom StartingAddress
  runCPU chip
  putStrLn "Fin."

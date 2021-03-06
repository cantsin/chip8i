module Main

import System
import Data.Buffer
import Data.Fin
import Effects
import Effect.SDL
import Effect.State
import Effect.StdIO
import Effect.System
import Effect.Random

import Constants
import Opcodes
import Screen
import Keybindings
import Keypad
import Chip8
import Ram
import Cpu
import MemoryIO

%default total

data Counter : Type where -- empty type

-- Effects: support SDL, chip8 state, counter, RAM (buffer I/O),
-- random number generation, console I/O, and system (for `usleep`).
State : Type -> Type -> Type
State i t = { [SDL i, Chip8 ::: STATE Chip8, Counter ::: STATE Integer, RAM, RND, STDIO, SYSTEM] } Effects.DepEff.Eff t

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
runChip8 : { [SDL_ON, Chip8 ::: STATE Chip8, Counter ::: STATE Integer, RAM, RND] } Eff ()
runChip8 =
  do
    chip <- Chip8 :- get
    case State chip of
      Halted _ => pure ()
      WaitingForKey _ => pure ()
      Active =>
        -- The CPU runs at roughly 500Hz, however, we want to tick down the
        -- CPU DT/ST at a rate of 60Hz. As a first approximation, let's say
        -- we tick down DT/ST every 8 CPU cycles.
        let counter = !(Counter :- get) + 1 in
        let newCounter = counter `mod` 8 in
        let tick = newCounter == 7 in
        do
          runOneCycle tick
          Counter :- put newCounter

drawScreen : { [SDL_ON, Chip8 ::: STATE Chip8, STDIO] } Eff ()
drawScreen =
  let c : Chip8 = !(Chip8 :- get) in
  let cpu : Cpu = Computer c in
  let screen : Screen = Display c in
  let keys : Keypad = Keys c in
  do
    -- debugging
    -- putStrLn $ show cpu
    -- putStrLn $ show screen
    -- putStrLn $ show keys
    renderScreen screen
    flip

partial
runChip8Loop : State SDLSurface ()
runChip8Loop =
  do
    runChip8
    drawScreen
    usleep 2083 -- update ~480 times per second
    when !(processKeys !poll) runChip8Loop

partial
kickoff : Buffer -> State () ()
kickoff rom =
  do
    initialise (Width * Scale) (Height * Scale)
    loadDefaultSpriteDataAt DefaultSpriteDataAddress
    runChip8Loop
    quit
    putStrLn "Fin."

partial
main : IO ()
main =
  do
    args <- getArgs
    rom <- readROMFromFile $ getROMPath args
    chip8 <- newChip8
    loadROMAt chip8 rom StartingAddress
    runInit [(), Chip8 := chip8, Counter := 0, (), RandomSeed, (), ()] $ kickoff rom

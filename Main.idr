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
import Keypad
import Chip8

%default total

-- Effects: support SDL, chip8 state, random number generation,
-- console I/O, and system interaction.
State : Type -> Type -> Type
State i t = { [ SDL i, Chip8 ::: STATE Chip8, RND, STDIO, SYSTEM] } Effects.DepEff.Eff t

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
runChip8 : (chip : Chip8) -> { [SDL_ON, Chip8 ::: STATE Chip8, STDIO] } Eff ()
runChip8 chip =
  case getState chip of
    Halted _ => pure ()
    WaitingForKey _ => pure ()
    Active =>
      -- The CPU runs at roughly 500Hz, however, we want to tick down the
      -- CPU DT/ST at a rate of 60Hz. As a first approximation, let's say
      -- we tick down DT/ST every 8 CPU cycles.
      let counter = getCounter chip + 1 in
      let tick = counter `mod` 8 == 1 in
      do
        -- modifiedChip <- runOneCycle chip tick
        -- Chip8 :- put $ record { Counter = counter } modifiedChip
        pure ()

Chip8Effect : Type -> Type
Chip8Effect t = { [Chip8 ::: STATE Chip8] } Eff t

-- SDL
process : Maybe Event -> Chip8Effect Bool
process (Just AppQuit) = pure False
-- TODO if halted, putStrLn $ "Chip8 halted. Reason: " ++ errorMessage chip
process (Just (KeyDown KeyEsc)) = pure False
-- process (Just (KeyDown KeyLeftArrow))  = do xmove (-2); pure True
-- process (Just (KeyUp KeyLeftArrow))    = do xmove 0; pure True
-- process (Just (KeyDown KeyRightArrow)) = do xmove 2; pure True
-- process (Just (KeyUp KeyRightArrow))   = do xmove 0; pure True
-- process (Just (KeyDown KeyUpArrow))    = do ymove (-2); pure True
-- process (Just (KeyUp KeyUpArrow))      = do ymove 0; pure True
-- process (Just (KeyDown KeyDownArrow))  = do ymove 2; pure True
-- process (Just (KeyUp KeyDownArrow))    = do ymove 0; pure True
-- process (Just (KeyDown KeySpace))      = do addBullet; pure True
process _ = pure True

Running : Type -> Type
Running t = State SDLSurface t

partial
runChip8Loop : Running ()
runChip8Loop =
  do
    chip <- Chip8 :- get
    runChip8 chip
    -- drawScreen (getDisplay chip)
    usleep 2083 -- update ~480 times per second
    when !(process !poll) runChip8Loop

partial
kickoff : State () ()
kickoff =
  do
    initialise (64 * Scale) (32 * Scale)
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
    loadDefaultSpriteDataAt chip8 DefaultSpriteDataAddress
    loadROMAt chip8 rom StartingAddress
    runInit [(), Chip8 := chip8, RandomSeed, (), ()] kickoff

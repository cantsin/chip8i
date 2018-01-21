module Main

import Data.Vect

main : IO ()
main = putStrLn "Hello world"

-- 16 general purpose 8-bit registers, V0 to Vf
V : Vect 16 Bits8
-- 16-bit register called I
I : Bits16

-- pseudo PC (16-bit)
PC : Bits16
-- pseudo SP (8-bit)
SP : Bits8

-- stack is an array of 16 16-bit values
stack : Vect 16 Bits16

-- 4kb RAM
-- 0x200 start instruction
-- 0x000 to 0x1ff are reserved for interpreter
ram : Vect 0xfff Bits8

-- delay time register DT
DT : Bits8
-- sound timer register ST
ST : Bits8

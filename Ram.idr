module Ram

import Effects
import Data.Bits
import Data.Buffer

export
data Ram : Effect where
  Allocate : Int -> Ram (Maybe Buffer) a (\v => ())
  ReadByte : Buffer -> Int -> Ram Bits8 a (\v => a)
  WriteByte : Buffer -> Int -> Bits8 -> Ram () a (\v => ())
  Copy : Buffer -> Int -> Int -> Buffer -> Int -> Ram () a (\v => ())

Handler Ram IO where
  handle ram (Allocate len) k =
    do
      buf <- Buffer.newBuffer len
      k buf ()
  handle ram (ReadByte buf addr) k =
    do
      val <- Buffer.getByte buf addr
      k val ram
  handle ram (WriteByte buf addr val) k =
    do
      Buffer.setByte buf addr val
      k () ()
  handle ram (Copy buf start len dest loc) k =
    do
      Buffer.copyData buf start len dest loc
      k () ()

export
RAM : Type -> EFFECT
RAM t = MkEff t Ram

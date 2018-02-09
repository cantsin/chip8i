module Ram

import Effects
import Data.Bits
import Data.Buffer

public
export
data Ram : Effect where
  Allocate : Int -> Ram (Maybe Buffer) a (\v => ())
  ReadByte : Buffer -> Int -> Ram Bits8 a (\v => a)
  WriteByte : Buffer -> Int -> Bits8 -> Ram () a (\v => ())
  Copy : Buffer -> Int -> Int -> Buffer -> Int -> Ram () a (\v => ())

public
export
Handler Ram IO where
  handle () (Allocate len) k =
    do
      buf <- Buffer.newBuffer len
      k buf ()
  handle () (ReadByte buf addr) k =
    do
      val <- Buffer.getByte buf addr
      k val ()
  handle () (WriteByte buf addr val) k =
    do
      Buffer.setByte buf addr val
      k () ()
  handle () (Copy buf start len dest loc) k =
    do
      Buffer.copyData buf start len dest loc
      k () ()

public
export
RAM : EFFECT
RAM = MkEff () Ram

public
export
readByte : Buffer -> Int -> Eff Bits8 [RAM]
readByte buf addr = call (ReadByte buf addr)

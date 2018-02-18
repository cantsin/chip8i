module Ram

import Effects
import Data.Bits
import Data.Buffer

%default total

public
export
data Ram : Effect where
  ReadByte : Buffer -> Int -> Ram Bits8 a (\v => a)
  WriteByte : Buffer -> Int -> Bits8 -> Ram () a (\v => ())

public
export
Handler Ram IO where
  handle () (ReadByte buf addr) k =
    do
      val <- Buffer.getByte buf addr
      k val ()
  handle () (WriteByte buf addr val) k =
    do
      Buffer.setByte buf addr val
      k () ()

public
export
RAM : EFFECT
RAM = MkEff () Ram

public
export
readByte : Buffer -> Int -> Eff Bits8 [RAM]
readByte buf addr = call (ReadByte buf addr)

public
export
writeByte : Buffer -> Int -> Bits8 -> Eff () [RAM]
writeByte buf addr val = call (WriteByte buf addr val)

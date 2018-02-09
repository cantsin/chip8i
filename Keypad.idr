module Keypad

import Data.Vect

export
record Keypad where
  constructor MkKeypad
  -- 16 key hexadecimal keypad
  K : Vect 16 Bool

export
Show Keypad where
  show k = show (K k)

export
isKeyPressed : (keypad : Keypad) -> (n : Fin 16) -> Bool
isKeyPressed keypad n = Vect.index n (K keypad)

export
setKeyPress : (keypad : Keypad) -> (n : Fin 16) -> Keypad
setKeyPress keypad n =
  let newK = Vect.replaceAt n True (K keypad) in
  record { K = newK } keypad

export
clearKeyPress : (keypad : Keypad) -> (n : Fin 16) -> Keypad
clearKeyPress keypad n =
  let newK = Vect.replaceAt n False (K keypad) in
  record { K = newK } keypad

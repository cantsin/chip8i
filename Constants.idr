module Constants

export
RamSize : Int
RamSize = 0x1000

export
StartingAddress : Int
StartingAddress = 0x200

-- we store default sprites in the interpreter space. this address is
-- arbitrarily chosen, but must fit in the range 0x000 to 0x1fff.
export
DefaultSpriteDataAddress : Int
DefaultSpriteDataAddress = 0x100

export
DefaultSpriteDataLength : Int
DefaultSpriteDataLength = 5

export
scale : Integer
scale = 4

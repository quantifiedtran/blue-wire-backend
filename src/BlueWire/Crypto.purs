module BlueWire.Crypto where

import Data.Maybe
import Data.String.Yarn

newtype Key = Key Number
newtype Encrypted = Encrypted String
newtype Decrypted = Decrypted String

-- | Encrypt a string with a given key. bear in mind this is for inconvenience, not real security
-- TODO: Use real encryption, not rot13
encrypt :: Key -> String -> Encrypted
encrypt (Key k) str = Encrypted (rot13 str)

-- | Decrypt an encrypted bit of data with the given key
-- TODO: Use real encryption, not rot13
decrypt :: Key -> Encrypted -> Maybe Decrypted
decrypt (Key k) (Encrypted en) = Just (Decrypted (rot13 en))

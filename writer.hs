-- The Writer Monad

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Text.Read (readMaybe)

type MyWriter = Writer [String]

zero :: MyWriter Int
zero = next (-1)

next :: Int -> MyWriter Int
next a = let a' = a + 1 in
  do
    tell [(show a')]
    return a'

example = runWriterT (zero >>= next >>= next >>  zero >>= next)

example' = runWriter $ zero >>= next >>= next >> (censor (const ["[REDACTED]"]) zero) >>= next

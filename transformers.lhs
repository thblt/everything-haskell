> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Reader
> import Control.Monad.Writer

Let's write a *real* monad transformer stack. Inspired by XMonad,
we'll make a three-layer cake.

 - A Reader to hold the configuration;
 - A State to hold the mutable state;
 - IO, to obtain state change.

To keep things simple, we'll implement a simple guess the number game
using this stack.  The Reader will hold a few global parameters, like
the phrases used to report on attempts, the range of values and the
max number of attempts.  The State will store the number of attempts,
and the guessed bounds (that is, the smallest value that is greater
than the secret, and the largest value that is smallest, etc).
Last but not least, we'll use IO to talk to the user.

Let's begin by the config type.

> data GConf = GConf
>   { min :: Int
>   , max :: Int
>   , tries :: Int
>   , msgTooLow :: String
>   , msgTooHigh :: String
>   , msgTooManyAttempts :: String
>   , msgVictory :: String }

Very stupid, but useful as an example.  This will be set once before
the game, then left as is.

Now for a runtime state.

> data GState = GState
>   { guessMin :: Int
>   , guessMax :: Int
>   , triesCount :: Int }

And our monadic stack:

> type Game = ReaderT GConf (StateT GState IO)

Now we can implement a single guess:

> guess :: Game GState
> guess = do
>   msg <- get
>   return msg

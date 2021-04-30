Guessing game with monad transformers

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Data.Default
> import Data.Maybe
> import Text.Read (readMaybe)

To keep things simple, we'll implement a simple guess the number game
using this stack.  The Reader will hold a few global parameters, like
the phrases used to report on attempts, the range of values and the
max number of attempts.  The State will store the number of attempts,
and the guessed bounds (that is, the smallest value that is greater
than the secret, and the largest value that is smallest, etc).
Last but not least, we'll use IO to talk to the user.

Let's write a *real* monad transformer stack. Inspired by XMonad,
we'll make a three-layer cake.

 - A Reader to hold the configuration;
 - A State to hold the mutable state;
 - IO, to actually interact with the user.

Let's begin by the config type.

> data GConf = GConf
>   { minValue :: Int
>   , maxValue :: Int
>   , maxTries :: Int
>   , msgTooLow :: String
>   , msgTooHigh :: String
>   , msgTooManyAttempts :: String
>   , msgVictory :: String }
>   deriving (Eq, Ord, Read, Show)


> instance Default GConf where
>   def = GConf
>     { minValue = 0
>     , maxValue = 16384
>     , maxTries = 16
>     , msgTooLow = "Not enough! Why so modest?"
>     , msgTooHigh = "Too much! Quiet down!"
>     , msgTooManyAttempts = "Sorry, you don't have any more tries!"
>     , msgVictory = "*Trumpets* YOU WIN!" }

Now for a runtime state.

> data GState = GState
>   { secret :: Int
>   , guessMin :: Maybe Int
>   , guessMax :: Maybe Int
>   , triesCount :: Int }
>   deriving (Eq, Ord, Read, Show)

> mkState secret = GState secret Nothing Nothing 0

And our monadic stack:

> type Game = ReaderT GConf (StateT GState IO)

A (very-)pseudo random number generator, that actually just picks the
number in the middle of the range:

> dice :: Game Int
> dice = do
>   a <- asks minValue
>   b <- asks maxValue
>   return $ a + (b - a) `div` 1

`getInt` reads a line from input until it gets something that parses
as an Int.

> getInt :: IO Int
> getInt = do
>   putStr "> "
>   line <- getLine
>   cand <- return $ (readMaybe line :: Maybe Int)
>   maybe getInt return cand

`loop` is the main function of the game.  It runs iteratively until
victory or defeat.

> loop :: Game ()
> loop = do
>   tmax <- asks maxTries
>   t <- gets triesCount
>   if tmax > t then do
>     success <- step
>     if success then return () else loop
>   else do
>     -- msg <- gets msgTooManyAttempts
>     liftIO $ putStrLn "Too late!"
>     return ()
>   return ()

`step` runs a single step of the game.  It prompts for a number, tests
it, updates the state, and returns a bool.

> step :: Game Bool
> step = do
>   secret <- gets secret
>   guess <- liftIO getInt
>   feedback secret guess
>   return $ secret == guess

> feedback :: Int -> Int -> Game ()
> feedback s g = do
>   m <- asks (pick s g)
>   liftIO $ putStrLn m
>     where pick s g | s < g = msgTooHigh
>                    | s > g = msgTooLow
>                    | otherwise = msgVictory

> play :: Game ()
> play = do
>   liftIO $ putStrLn "Welcome to Guess-A-Number!"
>   liftIO $ putStrLn $ "I chose a number, try and guess it?"
>   loop
>   liftIO $ putStrLn $ "Byiiie"

> main :: IO ()
> main = do
>   runStateT (runReaderT play def)  (mkState 33)
>   return ()

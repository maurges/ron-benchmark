{-# LANGUAGE LambdaCase, BangPatterns #-}

-- | A sanity check for my haskell profiler, to see if it's really my
-- deserializer or it's the best it's possible in haskell. Well, they say aeson
-- is not the best, but it's the standard

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (readFile)
import Data.Fixed (Pico, Fixed (..))
import Data.HashMap.Strict (toList)
import Data.Aeson.Parser (json)
import Data.Aeson.Types (Value (..))
import Data.Time.Clock (getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import System.Environment (getArgs, getProgName)

import Prelude hiding (readFile)

usage = do
    name <- getProgName
    putStrLn $ "Usage: " <> name <> " FILENAME.ron\n\tParse RON file and measure time"

main = getArgs >>= \case
    "--help":_ -> usage
    [name] -> run name =<< readFile name
    _ -> usage

-- | Prints a csv line: (name, size, time in microseconds)
run name content = do
    start <- getCurrentTime

    !r <- case parseOnly json content of
            Right val -> pure $! computeDepth val
            Left err -> putStrLn ("Parse error: " <> err) *> pure 0

    end <- getCurrentTime
    let delta = end `diffUTCTime` start
    let picoseconds = case nominalDiffTimeToSeconds delta :: Pico of
            MkFixed x -> x
    let microseconds = picoseconds `div` 1000000
    putStrLn $ name <> "," <> show r <> "," <> show microseconds

-- | A way to force the whole value tree which makes some real-life sense.
-- Since the constructors are strict, the primitive values will be forces as
-- well
computeDepth :: Value -> Int
computeDepth = go 1 where
    go !n (Number _) = n
    go !n (Bool _) = n
    go !n Null = n
    go !n (String _) = n
    -- this two are not very real-lifey since the values are supposed to be all
    -- the same type, but we go into them separately
    go !n (Array xs) = maximum . fmap (go (1 + n)) $ xs
    go !n (Object xs) = maximum . fmap (go (1 + n) . snd) . toList $ xs

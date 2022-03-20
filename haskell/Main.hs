{-# LANGUAGE LambdaCase, BangPatterns #-}

import Data.ByteString (readFile)
import Data.Fixed (Pico, Fixed (..))
import Data.Map.Strict (toAscList)
import Data.Ron.Value (Value (..))
import Data.Ron.Deserialize (loads)
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

    !r <- case loads content of
            Right !val -> pure 8
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
    go !n (Integral _) = n
    go !n (Floating _) = n
    go !n (Char _) = n
    go !n (String _) = n
    -- this two are not very real-lifey since the values are supposed to be all
    -- the same type, but we go into them separately
    go !n (List xs) = maximum . fmap (go (1 + n)) $ xs
    go !n (Map xs) = maximum . fmap (go (1 + n) . snd) . toAscList $ xs
    go !n (Unit _) = n
    go !n (Tuple _ xs) = maximum . fmap (go (1 + n)) $ xs
    go !n (Record _ xs) = maximum . fmap (go (1 + n) . snd) . toAscList $ xs

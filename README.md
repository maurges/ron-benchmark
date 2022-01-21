# RON deseriazlizers benchmark

Even on one example it's already really not great for haskell

### list of ints

| engine | time (milliseconds) |
|--------|---------------------|
| ron-rs | 93612 |
| ron-hs | 1147095 |
| aeson  | 820941 |

Including aeson because the input is compatible. So it seems haskell is 10
times worse than rust even if it's not me writing the code.. And my code is 1/3
worse than that...

data Config = Config { base :: Int, multiplier :: Int } deriving Show

defaultConfig :: Config
defaultConfig = Config { base = 0, multiplier = 2 }

compute :: Config -> Int
compute cfg = base cfg * multiplier cfg

-- Usage
result = compute defaultConfig { base = 5 } -- Only override `base`, use default `multiplier`
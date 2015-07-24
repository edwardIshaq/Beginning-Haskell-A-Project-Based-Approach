module Chapter2.Section2.Example where
  
  data TimeMachineR = TimeMachineR {  manufacturer :: String,
                                      model :: Integer,
                                      name :: String,
                                      past :: Bool,
                                      future :: Bool,
                                      price :: Float }
                                      deriving(Show, Read)

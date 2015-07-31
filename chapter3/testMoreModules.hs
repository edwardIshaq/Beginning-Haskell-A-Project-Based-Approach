import Chapter3.DataTypes

prettyRange :: Range -> String
prettyRange rng = case rng of
                    ( r -> R a b ) -> "[" ++ show a ++ "," ++ show b ++ "]"

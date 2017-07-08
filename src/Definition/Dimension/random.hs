{-# LANGUAGE TemplateHaskell, CPP #-}



import Definition.TH
import Language.Haskell.TH


defineDimension "Length"

pika = runQ [d| data instance Data Length Int = DLength Int |]
{-}

data family Key b a
runQ [d| data instance Dim Char Bool = Nc Char|]
[DataInstD [] Ghci1.Key 
           [ConT GHC.Types.Char,ConT GHC.Types.Bool]
           Nothing 
           [NormalC Nc_0 
                [(Bang NoSourceUnpackedness NoSourceStrictness,
                  ConT GHC.Types.Char)
                ]
           ] 
           []
]

-}
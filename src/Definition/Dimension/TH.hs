-- Must be enabled in order to allow TH functions defined here.
{-# LANGUAGE TemplateHaskell, CPP #-}

-- Data families and a few things to make life easier.
{-# LANGUAGE FlexibleContexts, TypeFamilies, FlexibleInstances #-}

-- standalone derivation is illegal without this declaration
{-# LANGUAGE StandaloneDeriving #-}


module Definition.Dimension.TH (BaseUnit(..)
                               ,declareBase
                               ) where


-- Template Haskell Imports
import Language.Haskell.TH
import Language.Haskell.TH.Quote


import Prelude

{-

GOAL:

data Length' = Length'
instance BaseUnit Length' where
  data Base Length' 
    = Length 
      deriving (Show,Eq)
-}

--- Calling it Base instead of dimension because it's called base unit.
--- This is the type of base unit that everything is based on.
class Eq (Base c) => BaseUnit c where
    data Base c :: *

-- defines a Base.
-- eg defineBase "Length" well return
-- data Length' = Length' deriving (Show,Eq)
-- 
-- The ' added to the end of the phrase is because Length will eventually be used in the definition of another data type.
-- Also, this definition isn't used by the end user.
declareBase :: String -> Q [Dec]
declareBase stringName = do 
  create_alt_dataConstructor <- return (defineEmptyDataD str)
  show_instance      <- defineShowInstance str str
  eq_instance        <- defineEqInstance str
  base_instance      <- declarePartOfBaseUnit str
  return $ (defineEmptyDataD str) 
           : show_instance ++ eq_instance ++ base_instance
  where
    -- string manipulations
    str = (++) stringName "\'"

    -- Helper Functions
    defineEmptyDataD :: String -> Dec
    defineEmptyDataD name
      = DataD [] nameTH [] Nothing [con] []
      where
        con = NormalC nameTH []    
        nameTH = mkName name

    defineShowInstance :: String -> String -> Q [Dec]
    defineShowInstance name abbrev =
      [d| 
      instance Show $(return $ ConT nameTH) where
        show _ = abbrev
        |]
      where nameTH = mkName name

    defineEqInstance :: String -> Q [Dec]
    defineEqInstance name =
      [d| 
      instance Eq $(return $ ConT nameTH) where 
        (==) name1 name2 = True

        |]
      where nameTH = mkName name
            name1 = nameTH
            name2 = nameTH

    declarePartOfBaseUnit :: String -> Q [Dec]
    declarePartOfBaseUnit givenName = return (helper givenName) :: Q [Dec]
      where 
            helper name = 
                          [InstanceD Nothing 
                                     [] 
                                     (AppT (ConT $ mkName "BaseUnit" ) 
                                           (ConT str'TH  )
                                     ) 
                                     [DataInstD [] 
                                                (mkName "Base")
                                                [ConT $ str'TH] 
                                                Nothing 
                                                [NormalC strTH
                                                         []
                                                ] 
                                                [ConT $ mkName "Show"
                                                ,ConT $ mkName "Eq"
                                                ]
                                     ]
                          ]
              where 
                strTH      = mkName str
                str'TH     = mkName str'
                str'       = (++) str "\'"
                str        = name



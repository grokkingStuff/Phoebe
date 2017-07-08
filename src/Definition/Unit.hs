{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_HADDOCK prune #-}


import Definition.Dimension (DimPower(..),
                             DimensionSignature(..)
                             )


-- the Symbol used for a name. For example meter's symbol if m.
type Symbol = String
-- String representation of a unit. Examples: "meter", "foot".
type UnitName = String
-- the prefix of a unit.
type Prefix = String
-- the power to which something is raised.
type Power = DimPower Int



data UnitRepresentation = URep { unitname :: UnitName, 
                                 symbol :: Symbol, 
                                 prefix :: Prefix, 
                                 power  :: Power} deriving (Eq, Ord)


instance Show UnitRepresentation where
  show (URep unitname symbol prefix (D power))
    | power == 1        = sym
    | otherwise             = sym ++ "^" ++ (show power)
    where sym = prefix ++ symbol


{-
-- | Data type to hold compound units, which are simple units multiplied
-- together.
data CompoundUnit = CompoundUnit { defs   :: Definitions
                                   -- ^ Definitions used to create the units.
                                 , sUnits :: [SimpleUnit]
                                   -- ^ List of SimpleUnits that is interpreted
                                   -- as the units being multiplied together.
                                 } deriving (Eq, Ord)

instance Show CompoundUnit where
  show (CompoundUnit _ us) = unwords . map showCompUnit' $ showSort us
-}
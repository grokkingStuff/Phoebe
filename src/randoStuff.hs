-- Shows how to implement an `instance Eq` for a GADT wrapped in an existential type.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}




import Prelude (($), (+), (++), (-), (.), Num, (&&), Int, String, Show, Eq(..), Ord(..), Maybe(..), Bool(..), otherwise) 
import qualified Prelude as Pre 

import Data.Monoid
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Typeable
import Data.Kind
import Data.Type.Equality


class Dimensional a where
    (*) :: a -> a -> a
    reciprocal :: a -> a
    {-# MINIMAL (*), reciprocal #-}
    -- Why isn't (*) and (/) not the minimal? well, (*),(/) and reciprocal are dependant such that I only need two to define all three.
    -- But more importantly, reciprocal is the main concept I wish to impart to Dimensional
    -- reciprocal is so key to the concept of Dimensional that I honestly considered not implementing (\) which could be implemented by manually using (*) (reciprocal) instead.
    (/) :: a -> a -> a
    (^) :: (Pre.Integral b) => a -> b -> a

    (/) a b = (*) a $reciprocal b
    (^) a 1 = a 
    (^) a 0 = a/a
    (^) a b | b < 0 = (a^0) / ((^) a (-b))
            | b > 0 = (*) a $ (^) a (b-1)


data Base =     Length                                     --- metre        - m        - length
                | Mass                                     --- kilogram     - kg       - mass
                | Time                                     --- second       - s        - time
                | ElectricCurrent                          --- ampere       - A        - electric Current
                | ThermodynamicTemperature                 --- Kelvin       - K        - thermodynamic temperature
                | AmountOfSubstance                        --- mole         - mol      - amount of substance
                | LuminousIntensity                        --- candela      - cd       - luminous intensity
--- Calling it Base instead of dimension because it's called base unit.
--- This is the type of base unit that everything is based on.
deriving instance Show (Base)
deriving instance Eq   (Base)
deriving instance Ord  (Base)

data DimPower a = D a deriving (Show,Eq,Ord)
--- Created to prevent Int-like type mix and becasue dimensional numbers are similar to exponents
--- Why is Int so popular? Dimensions can be any non-decimal number. 
------Thus, Int should be the only option.
--- Why are we not constraining DimPower a to DimPower Int? 
------Too annoying to deal with Haskell's need to generalize contexts.

-- Applying typical algebras to DimPowers.
instance Num a => Dimensional (DimPower a) where
  (*) (D x) (D y) = D (x+y)
  reciprocal (D x) = D (-x)
instance Num n => Monoid (DimPower n) where
  mempty = D 0
  mappend = (*)
instance Functor DimPower where  
 fmap func (D value) = D (func value)
instance Applicative DimPower where
  pure x = (D x)
  (<*>) (D func) (D x) = D (func x)
instance Monad DimPower where
  return = pure
  (>>=) (D x) (func) = (func x) 

data Dim b a where
  DLength :: a -> Dim Length a
  DMass :: a -> Dim Mass a
  DTime :: a -> Dim Time a
  DElectricCurrent :: a -> Dim ElectricCurrent a
  DThermodynamicTemperature :: a -> Dim ThermodynamicTemperature a
  DAmountOfSubstance :: a -> Dim AmountOfSubstance a
  DLuminousIntensity :: a -> Dim LuminousIntensity a

deriving instance Show a => Show (Dim b a)
--deriving instance Eq a => Eq (Dim b a)

(=~=) :: (Eq a) => (Dim b1 a) -> (Dim b a) -> Bool
(=~=) (DLength a)                   (DLength b)                   = a == b
(=~=) (DLength _)                   _                             = False
(=~=) (DMass a)                     (DMass b)                     = a == b
(=~=) (DMass _)                     _                             = False
(=~=) (DTime a)                     (DTime b)                     = a == b
(=~=) (DTime _)                     _                             = False
(=~=) (DElectricCurrent a)          (DElectricCurrent b)          = a == b
(=~=) (DElectricCurrent _)          _                             = False
(=~=) (DThermodynamicTemperature a) (DThermodynamicTemperature b) = a == b
(=~=) (DThermodynamicTemperature _) _                             = False
(=~=) (DAmountOfSubstance a)        (DAmountOfSubstance b)        = a == b
(=~=) (DAmountOfSubstance _)        _                             = False
(=~=) (DLuminousIntensity a)        (DLuminousIntensity b)        = a == b
(=~=) (DLuminousIntensity _)        _                             = False


instance (Dimensional power) => Dimensional (Dim base power) where
  (*) (DLength x)                   (DLength y)                   = DLength                   (x*y)
  (*) (DMass x)                     (DMass y)                     = DMass                     (x*y)
  (*) (DTime x)                     (DTime y)                     = DTime                     (x*y)
  (*) (DElectricCurrent  x)         (DElectricCurrent  y)         = DElectricCurrent          (x*y)
  (*) (DThermodynamicTemperature x) (DThermodynamicTemperature y) = DThermodynamicTemperature (x*y)
  (*) (DAmountOfSubstance x)        (DAmountOfSubstance y)        = DAmountOfSubstance        (x*y)
  (*) (DLuminousIntensity x)        (DLuminousIntensity y)        = DLuminousIntensity        (x*y)
--  (*) _                             _                             = Pre.error "You are trying to multiply dimensions of different types. This is illogical."
  
  reciprocal (DLength x)                   = DLength                   (reciprocal x)
  reciprocal (DMass x)                     = DMass                     (reciprocal x)
  reciprocal (DTime x)                     = DTime                     (reciprocal x)
  reciprocal (DElectricCurrent  x)         = DElectricCurrent          (reciprocal x)
  reciprocal (DThermodynamicTemperature x) = DThermodynamicTemperature (reciprocal x)
  reciprocal (DAmountOfSubstance x)        = DAmountOfSubstance        (reciprocal x)
  reciprocal (DLuminousIntensity x)        = DLuminousIntensity        (reciprocal x)

instance Dimensional power => Semigroup (Dim base power) where
  (<>) = (*)

instance Functor (Dim base) where
  fmap func (DLength x) = DLength (func x)
  fmap func (DMass x) = DMass (func x)
  fmap func (DTime x) = DTime (func x)
  fmap func (DElectricCurrent x) = DElectricCurrent (func x)
  fmap func (DThermodynamicTemperature x) = DThermodynamicTemperature (func x)
  fmap func (DAmountOfSubstance x) = DAmountOfSubstance (func x)
  fmap func (DLuminousIntensity x) = DLuminousIntensity (func x)


data DimensionSignature = DimSig 
                          { dLength                   :: (Dim Length                   (DimPower Int)), 
                            dMass                     :: (Dim Mass                     (DimPower Int)), 
                            dTime                     :: (Dim Time                     (DimPower Int)), 
                            dElectricCurrent          :: (Dim ElectricCurrent          (DimPower Int)), 
                            dThermodynamicTemperature :: (Dim ThermodynamicTemperature (DimPower Int)), 
                            dAmountOfSubstance        :: (Dim AmountOfSubstance        (DimPower Int)), 
                            dLuminousIntensity        :: (Dim LuminousIntensity        (DimPower Int)) 
                          }

instance Show (DimensionSignature) where
 show (DimSig ( DLength                   (D a) ) 
              ( DMass                     (D b) ) 
              ( DTime                     (D c) ) 
              ( DElectricCurrent          (D d) ) 
              ( DThermodynamicTemperature (D e) ) 
              ( DAmountOfSubstance        (D f) ) 
              ( DLuminousIntensity        (D g) ) 
      ) = 
        "Length                    : " ++ (Pre.show a) ++ "\n"
     ++ "Mass                      : " ++ (Pre.show b) ++ "\n"
     ++ "Time                      : " ++ (Pre.show c) ++ "\n"
     ++ "Electric Current          : " ++ (Pre.show d) ++ "\n"
     ++ "Thermodynamic Temperature : " ++ (Pre.show e) ++ "\n"
     ++ "Amount of Substance       : " ++ (Pre.show f) ++ "\n"
     ++ "Luminous Intensity        : " ++ (Pre.show g)



-- Why are we defining our own version of Eq? Because the compoenents of DimSig aren't instances of Eq themselves.
-- This would be the perfect case for a --TODO but it's too annoying.  
instance Eq (DimensionSignature) where
 (==) (DimSig a1 b1 c1 d1 e1 f1 g1) (DimSig a2 b2 c2 d2 e2 f2 g2) = Pre.all (==True)    [a1=~=a2,
                                                                                         b1=~=b2,
                                                                                         c1=~=c2,
                                                                                         d1=~=d2,
                                                                                         e1=~=e2,
                                                                                         f1=~=f2,
                                                                                         g1=~=g2]
instance Dimensional (DimensionSignature) where
  (*) (DimSig a1 b1 c1 d1 e1 f1 g1) (DimSig a2 b2 c2 d2 e2 f2 g2) = DimSig (a1*a2) 
                                                                           (b1*b2) 
                                                                           (c1*c2) 
                                                                           (d1*d2) 
                                                                           (e1*e2) 
                                                                           (f1*f2) 
                                                                           (g1*g2)
  reciprocal (DimSig a1 b1 c1 d1 e1 f1 g1) =  DimSig (reciprocal a1) 
                                                     (reciprocal b1)
                                                     (reciprocal c1)
                                                     (reciprocal d1)
                                                     (reciprocal e1)
                                                     (reciprocal f1)
                                                     (reciprocal g1)
dimSigGen (a1:a2:a3:a4:a5:a6:a7:[])    =    DimSig (DLength                    ( D a1 ) )                                       
                                                   (DMass                      ( D a2 ) )
                                                   (DTime                      ( D a3 ) )
                                                   (DElectricCurrent           ( D a4 ) )
                                                   (DThermodynamicTemperature  ( D a5 ) )
                                                   (DAmountOfSubstance         ( D a6 ) )
                                                   (DLuminousIntensity         ( D a7 ) )
dimSigGen _                            =    Pre.error "This list is invalid. Probably due to incorrect number of entries."

dimSig1 = DimSig (DLength (D 1))                                       --- Valid instance
                        (DMass (D 2)) 
                        (DTime (D 3)) 
                        (DElectricCurrent (D 4)) 
                        (DThermodynamicTemperature (D 5)) 
                        (DAmountOfSubstance (D 6)) 
                        (DLuminousIntensity (D 7))

dimSig2 = DimSig (DLength (D 7))                                       --- Valid Instance. Differs in Dimension Power
                        (DMass (D 6))
                        (DTime (D 5)) 
                        (DElectricCurrent (D 4)) 
                        (DThermodynamicTemperature (D 3)) 
                        (DAmountOfSubstance (D 2)) 
                        (DLuminousIntensity (D 1))

{- Invalid Instance that will give an error if used
dimSig3 = DimSig (DLength (D 7))                                       
                        (DLength (D 6)) 
                        (DTime (D 5)) 
                        (DElectricCurrent (D 4)) 
                        (DThermodynamicTemperature (D 3)) 
                        (DAmountOfSubstance (D 2)) 
                        (DLuminousIntensity (D 1))
-}




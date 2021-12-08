module AOC.Day.Three

import Data.Vect
import Data.String
import Decidable.Equality


%default total

data Bit : Type where
  Low : Bit
  High : Bit

Show Bit where
  show Low = "0"
  show High = "1"

Byte : (n : Nat) -> Type
Byte n = Vect n Bit

-- The only problem with this is that I now need to prove that all inputs
-- provided are of the same length. What fun.
data Binary : Nat -> Type where
  BE : Binary Z
  B0 : Binary n -> Binary (S n)
  B1 : Binary n -> Binary (S n)

export
Show (Binary n) where
  show BE = ""
  show (B0 b) = "0" ++ show b
  show (B1 b) = "1" ++ show b

----------------------------------------------------------------------------
-- Code I did not need hooray ----------------------------------------------
----------------------------------------------------------------------------

toBinary' : (s : List Char) -> Maybe (Binary (length s))
toBinary' [] = Just BE
toBinary' ('0' :: str) = (toBinary' str) >>= (\bin => Just $ B0 bin)
toBinary' ('1' :: str) = (toBinary' str) >>= (\bin => Just $ B1 bin)
toBinary' _ = Nothing

-- This works only if the type is unpacked in the type. Otherwise there is a
-- conflict in the length function used.
toBinary : (s : String) -> Maybe (Binary (length (unpack s)))
toBinary s = toBinary' (unpack s)

public export
Eq (Binary n) where
  (==) BE BE = False
  (==) (B0 x) (B0 y) = x == y
  (==) (B0 x) (B1 y) = False
  (==) (B1 x) (B0 y) = False
  (==) (B1 x) (B1 y) = x == y

-- The new Control.Function defines the `Injective` interface, which would make
-- these definitions unnecessary.
B0Injective : (0 left, right : Binary n) -> B0 left = B0 right -> left = right
B0Injective _ _ Refl = Refl

B1Injective : (0 left, right : Binary n) -> B1 left = B1 right -> left = right
B1Injective _ _ Refl = Refl

-- Sweet, I don't actually need this for the exactLength function
public export
DecEq (Binary n) where
  decEq BE BE = Yes Refl
  decEq (B0 x) (B0 y) with (decEq x y)
    decEq (B0 x) (B0 y) | (Yes prf) = Yes (cong B0 prf)
    decEq (B0 x) (B0 y) | (No contra) = No $ \h : (B0 x = B0 y) => contra $ B0Injective x y h
  decEq (B1 x) (B1 y) with (decEq x y)
    decEq (B1 x) (B1 y) | (Yes prf) = Yes (cong B1 prf)
    decEq (B1 x) (B1 y) | (No contra) = No $ \h : (B1 x = B1 y) => contra $ B1Injective x y h
  decEq (B0 x) (B1 y) = No B0B1impossible
    where
      B0B1impossible : (B0 x = B1 y) -> Void
      B0B1impossible Refl impossible
  decEq (B1 x) (B0 y) = No B1B0impossible
    where
      B1B0impossible : (B1 x = B0 y) -> Void
      B1B0impossible Refl impossible

----------------------------------------------------------------------------
-- Proofs required for parsing ---------------------------------------------
----------------------------------------------------------------------------

exactLength : {m : Nat} -> -- expected at run-time
              (len : Nat) -> (xs : Binary m) -> Maybe (Binary len)
exactLength {m} len xs with (decEq m len)
  exactLength {m = m} m xs | (Yes Refl) = Just xs
  exactLength {m = m} len xs | (No contra) = Nothing

-- Stolen from https://github.com/jumper149/AoC2021/blob/main/03/src/Main.idr
exactLengths : (len : Nat) -> Traversable t => t (n ** Binary n) -> Maybe (t (Binary len))
exactLengths len xs = traverse (\ x => exactLength len x.snd) xs

----------------------------------------------------------------------------
-- Actual parsing code -----------------------------------------------------
----------------------------------------------------------------------------

toBinaryDep' : (s : List Char) -> Maybe (n ** Binary n)
toBinaryDep' [] = Just (0 ** BE)
toBinaryDep' ('0' :: str) = (toBinaryDep' str) >>= (\(n ** bin) => Just $ (S n ** B0 bin))
toBinaryDep' ('1' :: str) = (toBinaryDep' str) >>= (\(n ** bin) => Just $ (S n ** B1 bin))
toBinaryDep' _ = Nothing

export
toBinaryDep : (s : String) -> Maybe (n ** Binary n)
toBinaryDep s = toBinaryDep' (unpack s)

export
parseBinary : List (n ** Binary n) -> Maybe (m ** List (Binary m))
parseBinary [] = Just (0 ** [])
parseBinary ((len ** bin) :: ys) = do
    list <- exactLengths len ys
    pure (len ** bin :: list)

  -- Ok, this does not work for some reason or another
  -- Prove that the size of the binary numbers in ys is the same as the size of
  -- bin.

  -- (ys_len ** ys) <- parseBinary ys
  -- bin <- exactLength ys_len bin

  -- -- With the sizes resolved, reconstruct with the dependent pair on the outside
  -- -- of the list.
  -- pure (ys_len ** bin :: ys)

----------------------------------------------------------------------------
-- Implementing Vect like interfaces for Binary ----------------------------
----------------------------------------------------------------------------

-- Most of these functions are just to make it easier to answer the AOC questions, since they involve transforming the individual bits into a

binaryToVect : Binary n -> Vect n Bool
binaryToVect BE = []
binaryToVect (B0 x) = False :: (binaryToVect x)
binaryToVect (B1 x) = True :: (binaryToVect x)

mapBinary : (Bool -> a) -> Binary n -> Vect n a
mapBinary f = map f . binaryToVect


-- day3 : List String -> (Integer, Integer)
-- day3 binary = ?something

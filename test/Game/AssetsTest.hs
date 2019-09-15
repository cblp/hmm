{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Game.AssetsTest where

import Prelude hiding (Product)

import qualified Data.List as List
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- this module exists solely for getting us ready & comfortable
-- to write many-many tests using the awesome hedgehog package

-- 0

xxhprop_success :: Property
xxhprop_success = property success

xxhprop_discard :: Property
xxhprop_discard = property discard

xhprop_failure :: Property
xhprop_failure = property failure

-- 1

xhprop_test_limit :: Property
xhprop_test_limit = withTests 100 . property $ success

xhprop_discard_limit :: Property
xhprop_discard_limit = withDiscards 10 . property $ discard

xhprop_shrink_limit :: Property
xhprop_shrink_limit =
  withShrinks 1 . property $ do
    x <- forAll $ Gen.enum 'a' 'z'
    assert $ x == 'a'

-- 2

xhprop_foo :: Property
xhprop_foo =
  property $ do
  x <- forAll $ Gen.enum 'a' 'z'
  y <- forAll $ Gen.choice
    [ Gen.integral $ Range.linear 0 1000
    , Gen.integral $ Range.linear 0 1000
    ]
  guard (y `mod` 2 == (1 :: Int))
  assert $ y < 87 && x <= 'r'

-- 3

newtype Product = Product String
  deriving (Eq, Ord, Show)

newtype USD = USD Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data Item = Item Product USD
  deriving (Eq, Ord, Show)

newtype Order = Order [Item]
  deriving (Eq, Ord, Show)

merge :: Order -> Order -> Order
merge (Order xs) (Order ys) =
  Order $ xs ++ ys ++
    if any ((< 50) . price) xs &&
       any ((> 50) . price) xs &&
       any ((> 1050) . price) ys then
      [Item (Product "processing") (USD 1)]
    else
      []

price :: Item -> USD
price (Item _ x) = x

total :: Order -> USD
total (Order xs) = sum $ price <$> xs

cheap :: Gen Item
cheap = Item
  <$> (Product <$> Gen.element ["foo", "bar"])
  <*> (USD <$> Gen.integral (Range.constant 5 10))

expensive :: Gen Item
expensive = Item
  <$> (Product <$> Gen.element ["baz", "qux"])
  <*> (USD <$> Gen.integral (Range.linear 1000 2000))

order :: Gen Item -> Gen Order
order gen = Order <$> Gen.list (Range.linear 0 50) gen

xhprop_total :: Property
xhprop_total = property $ do
  x <- forAll (order $ Gen.choice [cheap, expensive])
  y <- forAll (order expensive)
  total (merge x y) === total x + total y

-- 4

data Exp
  = Lit !Int
  | Add !Exp !Exp
  deriving (Eq, Ord, Show)

evalExp :: Exp -> Int
evalExp = \case
  Lit x -> x
  Add x y -> evalExp x + evalExp y

genExp1 :: Gen Exp
genExp1 = Gen.recursive Gen.choice
  [ Lit <$> Gen.int (Range.linear 0 1000) ]
  [ Gen.subterm2 genExp1 genExp1 Add ]

xhprop_hutton_1 :: Property
xhprop_hutton_1 = property $ do
  x <- forAll genExp1
  case x of
    Add (Add _ _) _ -> assert (evalExp x < 100)
    _               -> success

-- | replaces the expression with
-- the literal it evaluates to
shrinkExp2 :: Exp -> [Exp]
shrinkExp2 = \case
  Lit _   -> []
  Add x y -> [Lit (evalExp (Add x y))]

genExp2 :: Gen Exp
genExp2 =
  Gen.shrink shrinkExp2 $
  Gen.recursive Gen.choice
  [Lit <$> Gen.int (Range.linear 0 10000)]
  [Gen.subterm2 genExp2 genExp2 Add]

xhprop_hutton_2 :: Property
xhprop_hutton_2 = property $ do
  x <- forAll genExp2
  case x of
    Add (Add _ _) _ -> assert (evalExp x < 100)
    _               -> success

-- 5

data SomeRecord = SomeRecord
  { someInt    :: Int
  , someBool   :: Bool
  , someDouble :: Double
  , someList   :: [(Int, String)]
  } deriving (Eq, Show)

genRecord :: Gen SomeRecord
genRecord = SomeRecord
  <$> Gen.int (Range.linearFrom 0 (-1000) 1000)
  <*> Gen.bool
  <*> Gen.double (Range.linearFrac 7.2 15.9)
  <*> Gen.list (Range.linear 5 100)
        ( (,)
          <$> Gen.int (Range.constant 0 10)
          <*> Gen.string (Range.constant 2 4) Gen.alpha
        )

xhprop_record :: Property
xhprop_record = property $ do
  x <- forAll genRecord
  y <- forAll genRecord
  x === y

xhprop_different_record :: Property
xhprop_different_record = property $ do
  x <- forAll genRecord
  x /== x

-- 6

xhprop_takeEnd :: Property
xhprop_takeEnd = property $ do
  xs <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
  n  <- forAll $ Gen.int (Range.linear 0 100)
  let string
        = List.reverse
        . List.take n
        $ List.reverse xs
      text
        = Text.unpack
        . Text.takeEnd n
        $ Text.pack xs
  string === text

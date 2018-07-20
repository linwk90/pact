{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Pact.Analyze.Types.Languages
  ( (:<:)(inject, project)
  , EInvariant
  , EProp
  , Invariant(..)
  , Prop(..)
  , PropSpecific(..)
  , Core(..)

  , mkDecimal

  , pattern ILiteral
  , pattern ILogicalOp
  , pattern Inj
  , pattern PAnd
  , pattern PAt
  , pattern PDecAddTime
  , pattern PIntAddTime
  , pattern PKeySetEqNeq
  , pattern PLit
  , pattern PLogical
  , pattern PNot
  , pattern PNumerical
  , pattern PObjectEqNeq
  , pattern POr
  , pattern PStrConcat
  , pattern PStrLength
  , pattern PVar
  ) where

import qualified Data.Decimal                 as Decimal
import           Data.Map.Strict              (Map)
import           Data.SBV                     (Boolean (bnot, false, true, (&&&), (|||)),
                                               (%))
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import           Data.Typeable                ((:~:) (Refl))
import           Prelude                      hiding (Float)

import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Util

#define EQ_EXISTENTIAL(tm)                                \
instance Eq (Existential tm) where                        \
  ESimple ta ia == ESimple tb ib = case typeEq ta tb of { \
    Just Refl -> ia == ib;                                \
    Nothing   -> False};                                  \
  EObject sa pa == EObject sb pb = sa == sb && pa == pb;  \
  _ == _ = False;

#define SHOW_EXISTENTIAL(tm)                                         \
instance Show (Existential tm) where                                 \
  show (ESimple ty inv) = "(" ++ show inv ++ ": " ++ show ty ++ ")"; \
  show (EObject ty obj) = "(" ++ show obj ++ ": " ++ show ty ++ ")";


-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class sub :<: sup where
  inject  :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inject  = id
  project = Just

pattern Inj :: sub :<: sup => sub a -> sup a
pattern Inj a <- (project -> Just a) where
  Inj a = inject a


-- | Core terms.
--
-- These are the expressions shared by all three languages ('Prop',
-- 'Invariant', and 'Term'). Another way of thinking about this type is the
-- pure subset of any of then languages. This happens to coincide with all of
-- the invariant language, but properties and terms have more constructions.
--
-- This consists of:
--
-- * comparisons
--   - { <, >, <=, >= } apply to { integer, decimal, string, time }
--   - { =, != } apply to { integer, decimal, string, time, bool, keyset }
-- * literals
-- * variables
-- * logical operations
-- * string length and concatenation
-- * 'add-time'
-- * 'at'
data Core t a where
  Lit :: a -> Core t a
  -- | Injects a symbolic value into the language
  Sym :: S a -> Core t a

  -- | Refers to a function argument, universally/existentially-quantified
  -- variable, or column
  Var :: VarId -> Text -> Core t a

  -- string ops
  -- | The concatenation of two 'String' expressions
  StrConcat :: t String -> t String -> Core t String
  -- | The length of a 'String' expression
  StrLength :: t String                     -> Core t Integer

  -- numeric ops
  Numerical :: Numerical t a -> Core t a

  -- Time
  -- | Adds an 'Integer' expression to a 'Time' expression
  IntAddTime :: t Time -> t Integer -> Core t Time
  -- | Adds a 'Decimal' expression to a 'Time' expression
  DecAddTime :: t Time -> t Decimal -> Core t Time

  -- comparison. Note that while it's cumbersome to define five different
  -- monomorphized comparisons, the alternative is implementing Eq by hand
  -- here.

  -- | A 'ComparisonOp' expression over two 'Integer' expressions
  IntegerComparison :: ComparisonOp -> t Integer -> t Integer -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Decimal' expressions
  DecimalComparison :: ComparisonOp -> t Decimal -> t Decimal -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Time' expressions
  TimeComparison    :: ComparisonOp -> t Time    -> t Time    -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'String' expressions
  StringComparison  :: ComparisonOp -> t String  -> t String  -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Bool' expressions
  BoolComparison    :: ComparisonOp -> t Bool    -> t Bool    -> Core t Bool

  KeySetEqNeq   :: EqNeq -> t KeySet -> t KeySet -> Core t Bool
  ObjectEqNeq   :: EqNeq -> t Object -> t Object -> Core t Bool

  At            :: Schema -> t String -> t Object -> EType -> Core t a

  LiteralObject :: Map Text (Existential t) -> Core t Object

  -- boolean ops
  -- | A 'Logical' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.
  Logical :: LogicalOp -> [t Bool] -> Core t Bool

deriving instance Eq a   => Eq   (Core Prop a)
deriving instance Show a => Show (Core Prop a)


-- | Property-specific constructions.
--
-- This encompasses every construction that can appear in a 'Prop' that's not
-- in 'Core'.
data PropSpecific a where

  -- TX success/failure

  --
  -- TODO: remove either Success Or Abort.
  --

  -- | Whether a transaction aborts (does not succeed)
  Abort   :: PropSpecific Bool
  -- | Whether a transaction succeeds (does not abort)
  Success :: PropSpecific Bool
  -- | The return value of the function under examination
  Result  :: PropSpecific a

  -- Abstraction

  -- | Introduces a universally-quantified variable over another property
  Forall :: VarId -> Text -> QType -> Prop Bool -> PropSpecific Bool
  -- | Introduces an existentially-quantified variable over another property
  Exists :: VarId -> Text -> QType -> Prop Bool -> PropSpecific Bool

  -- DB properties

  -- | True when anything in the table is written
  TableWrite :: Prop TableName  ->                PropSpecific Bool
  -- | True when anything in the table is read
  TableRead  :: Prop TableName  ->                PropSpecific Bool

  --
  -- NOTE: it's possible that in a standard library we could implement these in
  --       terms of "CellRead"/"CellWrite" and existential quantification.
  --
  -- | Whether a column is written
  ColumnWrite :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool
  -- | Whether a column is read
  ColumnRead  :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool

  --
  -- TODO: rewrite these in terms of CellBefore, CellAfter, ColumnSumBefore,
  --       ColumnSumAfter:
  --
  -- | The difference (@after-before@) in a cell's integer value across a transaction
  IntCellDelta   :: Prop TableName  -> Prop ColumnName  -> Prop RowKey -> PropSpecific Integer
  -- | The difference (@after-before@) in a cell's decimal value across a transaction
  DecCellDelta   :: Prop TableName  -> Prop ColumnName  -> Prop RowKey -> PropSpecific Decimal
  -- | The difference (@after-before@) in a column's integer sum across a transaction
  IntColumnDelta :: Prop TableName  -> Prop ColumnName                 -> PropSpecific Integer
  -- | The difference (@after-before@) in a column's decimal sum across a transaction
  DecColumnDelta :: Prop TableName  -> Prop ColumnName                 -> PropSpecific Decimal

  -- | Whether a row is read
  RowRead       :: Prop TableName  -> Prop RowKey -> PropSpecific Bool
  -- | Number of times a row is read
  RowReadCount  :: Prop TableName  -> Prop RowKey -> PropSpecific Integer
  -- | Whether a row is written
  RowWrite      :: Prop TableName  -> Prop RowKey -> PropSpecific Bool
  -- | Number of times a row is written
  RowWriteCount :: Prop TableName  -> Prop RowKey -> PropSpecific Integer

  --
  -- TODO: StaleRead?
  --

  -- Authorization

  -- | Whether a transaction contains a signature that satisfied the named key set
  KsNameAuthorized :: KeySetName      ->                                   PropSpecific Bool
  -- | Whether a row has its keyset @enforce@d in a transaction
  RowEnforced      :: Prop TableName  -> Prop ColumnName -> Prop RowKey -> PropSpecific Bool

deriving instance Eq a   => Eq   (PropSpecific a)
deriving instance Show a => Show (PropSpecific a)


data Prop a
  = PropSpecific (PropSpecific a)
  | PureProp     (Core Prop a)
  deriving (Show, Eq)

instance S :<: Prop where
  inject = PureProp . Sym
  project = \case
    PureProp (Sym a) -> Just a
    _                -> Nothing

instance PropSpecific :<: Prop where
  inject = PropSpecific
  project = \case
    PropSpecific a -> Just a
    _              -> Nothing

instance Core Prop :<: Prop where
  inject = PureProp
  project = \case
    PureProp a -> Just a
    _          -> Nothing

instance Numerical Prop :<: Prop where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance IsString (Prop TableName) where
  fromString = PLit . fromString

instance IsString (Prop ColumnName) where
  fromString = PLit . fromString

instance Boolean (Prop Bool) where
  true      = PLit True
  false     = PLit False
  bnot p    = PureProp $ Logical NotOp [p]
  p1 &&& p2 = PAnd p1 p2
  p1 ||| p2 = POr  p1 p2

instance Num (Prop Integer) where
  fromInteger = PLit . fromInteger
  (+)         = inject ... IntArithOp Add
  (*)         = inject ... IntArithOp Mul
  abs         = inject .   IntUnaryArithOp Abs
  signum      = inject .   IntUnaryArithOp Signum
  negate      = inject .   IntUnaryArithOp Negate

instance Num (Prop Decimal) where
  fromInteger = PLit . mkDecimal . fromInteger
  (+)         = inject ... DecArithOp Add
  (*)         = inject ... DecArithOp Mul
  abs         = inject .   DecUnaryArithOp Abs
  signum      = inject .   DecUnaryArithOp Signum
  negate      = inject .   DecUnaryArithOp Negate

type EProp = Existential Prop
EQ_EXISTENTIAL(Prop)
SHOW_EXISTENTIAL(Prop)

pattern PLit :: a -> Prop a
pattern PLit a = PureProp (Lit a)

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = PureProp (Var vid name)

pattern PNumerical :: Numerical Prop t -> Prop t
pattern PNumerical x = PureProp (Numerical x)

pattern PStrConcat :: Prop String -> Prop String -> Prop String
pattern PStrConcat x y = PureProp (StrConcat x y)

pattern PIntAddTime :: Prop Time -> Prop Integer -> Prop Time
pattern PIntAddTime x y = PureProp (IntAddTime x y)

pattern PDecAddTime :: Prop Time -> Prop Decimal -> Prop Time
pattern PDecAddTime x y = PureProp (DecAddTime x y)

pattern PAt :: Schema -> Prop String -> Prop Object -> EType -> Prop t
pattern PAt a b c d = PureProp (At a b c d)

pattern PKeySetEqNeq :: EqNeq -> Prop KeySet -> Prop KeySet -> Prop Bool
pattern PKeySetEqNeq op x y = PureProp (KeySetEqNeq op x y)

pattern PObjectEqNeq :: EqNeq -> Prop Object -> Prop Object -> Prop Bool
pattern PObjectEqNeq op x y = PureProp (ObjectEqNeq op x y)

pattern PLogical :: LogicalOp -> [Prop Bool] -> Prop Bool
pattern PLogical op args = PureProp (Logical op args)

pattern PStrLength :: Prop String -> Prop Integer
pattern PStrLength str = PureProp (StrLength str)

pattern PAnd :: Prop Bool -> Prop Bool -> Prop Bool
pattern PAnd a b = PureProp (Logical AndOp [a, b])

pattern POr :: Prop Bool -> Prop Bool -> Prop Bool
pattern POr a b = PureProp (Logical OrOp [a, b])

pattern PNot :: Prop Bool -> Prop Bool
pattern PNot a = PureProp (Logical NotOp [a])

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = fromRational $
  mantissa % 10 ^ places


-- | The schema invariant language.
--
-- This language is pure / stateless. It includes exactly the same
-- constructions as 'Core'.
newtype Invariant a = PureInvariant (Core Invariant a)
  deriving (Show, Eq)

deriving instance Eq a   => Eq   (Core Invariant a)
deriving instance Show a => Show (Core Invariant a)

instance Core Invariant :<: Invariant where
  inject = PureInvariant
  project = \case
    PureInvariant a -> Just a
    _               -> Nothing

instance Numerical Invariant :<: Invariant where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance S :<: Invariant where
  inject = PureInvariant . Sym
  project = \case
    PureInvariant (Sym a) -> Just a
    _                     -> Nothing

type EInvariant = Existential Invariant
EQ_EXISTENTIAL(Invariant)
SHOW_EXISTENTIAL(Invariant)

pattern ILiteral :: a -> Invariant a
pattern ILiteral a = PureInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant Bool] -> Invariant Bool
pattern ILogicalOp op args = PureInvariant (Logical op args)
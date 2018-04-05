{-# language DeriveAnyClass      #-}
{-# language DeriveDataTypeable  #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language FlexibleInstances   #-}
{-# language GADTs               #-}
{-# language MultiWayIf          #-}
{-# language OverloadedStrings   #-}
{-# language PatternSynonyms     #-}
{-# language Rank2Types          #-}
{-# language RecordWildCards     #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving  #-}
{-# language TemplateHaskell     #-}
{-# language TypeApplications    #-}
{-# language ViewPatterns        #-}
{-# language TupleSections       #-}
{-# language TypeOperators       #-}
{-# language TypeFamilies        #-}
{-# language TypeFamilyDependencies        #-}

module Pact.Analyze.Types where

import Control.Monad.Except (ExceptT(..), Except, runExcept)
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Map.Strict (Map)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV.Internals as SBVI
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Thyme
import GHC.Generics
import Pact.Types.Lang hiding (Term, TableName, Type, TObject)
import qualified Pact.Types.Lang as Pact
-- import Pact.Types.Runtime hiding (Term, WriteType(..), TableName, Type)
import Pact.Types.Typecheck hiding (Var, UserType)
import qualified Pact.Types.Typecheck as TC
import qualified Pact.Types.Typecheck as Pact

type family SymbolicRep a = r | r -> a where
  SymbolicRep Object = Object
  SymbolicRep a      = SBV a

type FieldType = EType
  -- = () -- TODO

newtype Object
  = Object (Map String (FieldType, AVal))
  deriving (Eq, Show)

newtype Schema = Schema (Map String FieldType)
  deriving Show

-- | Untyped symbolic value.
data AVal
  = AVal SBVI.SVal
  | AnObj Object
  deriving (Eq, Show)

--
-- TODO: will this work?
--
-- unsafeCastAVal :: AVal -> SymbolicRep a
-- unsafeCastAVal (AVal sval) = SBVI.SBV sval
-- unsafeCastAVal (AnObj obj) = obj

mkSBV :: SBVI.SVal -> SBV a
mkSBV = SBVI.SBV

mkAVal :: SBV a -> AVal
mkAVal (SBVI.SBV sval) = AVal sval

coerceSBV :: SBV a -> SBV b
coerceSBV = SBVI.SBV . SBVI.unSBV

data AnalyzeEnv = AnalyzeEnv
  { _scope     :: Map Text AVal      -- used with 'local' in a stack fashion
  , _nameAuths :: SArray String Bool -- read-only
  } deriving Show

newtype AnalyzeLog
  = AnalyzeLog ()

instance Monoid AnalyzeLog where
  mempty = AnalyzeLog ()
  mappend _ _ = AnalyzeLog ()

instance Mergeable AnalyzeLog where
  --
  -- NOTE: If we change the underlying representation of AnalyzeLog to a list,
  -- the default Mergeable instance for this will have the wrong semantics, as
  -- it requires that lists have the same length. We more likely want to use
  -- monoidal semantics for anything we log:
  --
  symbolicMerge _f _t = mappend

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SBVI.SVal KString
                 . Left
                 . SBVI.CW KString
                 . SBVI.CWString

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

newtype TableName
  = TableName String
  deriving (Eq, Ord, Show)

instance SymWord TableName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (TableName s) = mkConcreteString s
  fromCW = wrappedStringFromCW TableName

instance HasKind TableName where
  kindOf _ = KString

instance IsString TableName where
  fromString = TableName

newtype ColumnName
  = ColumnName String
  deriving (Eq, Ord, Show)

instance SymWord ColumnName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (ColumnName s) = mkConcreteString s
  fromCW = wrappedStringFromCW ColumnName

instance HasKind ColumnName where
  kindOf _ = KString

instance IsString ColumnName where
  fromString = ColumnName

newtype RowKey
  = RowKey String
  deriving (Eq, Ord, Show)

instance SymWord RowKey where
  mkSymWord = SBVI.genMkSymVar KString
  literal (RowKey s) = mkConcreteString s
  fromCW = wrappedStringFromCW RowKey

instance HasKind RowKey where
  kindOf _ = KString

instance IsString RowKey where
  fromString = RowKey

-- a unique column, comprised of table name and column name
-- e.g. accounts__balance
newtype ColumnId
  = ColumnId String
  deriving (Eq, Ord)

instance SymWord ColumnId where
  mkSymWord = SBVI.genMkSymVar KString
  literal (ColumnId cid) = mkConcreteString cid
  fromCW = wrappedStringFromCW ColumnId

instance HasKind ColumnId where
  kindOf _ = KString

instance IsString ColumnId where
  fromString = ColumnId

sColId :: TableName -> SBV ColumnName -> SBV ColumnId
sColId (TableName tn) sCn = coerceSBV $
  (literal $ tn ++ "__") .++ coerceSBV sCn

-- a unique cell, from table and column names, and a row key
-- e.g. accounts__balance__25
newtype CellId
  = CellId String
  deriving (Eq, Ord)

instance SymWord CellId where
  mkSymWord = SBVI.genMkSymVar KString
  literal (CellId cid) = mkConcreteString cid
  fromCW = wrappedStringFromCW CellId

instance HasKind CellId where
  kindOf _ = KString

instance IsString CellId where
  fromString = CellId

--
-- TODO: set up more hygenic munging. we don't want SBV to find a solution with
-- a column name or row key containing "__". perhaps we accumulate symbolic
-- column name and row key variables (in a Set, in GlobalState) and then assert
-- contraints that these variables must take names from a whitelist, or never
-- contain "__".
--
-- Another solution might be using indirection in the form of multiple layers
-- of arrays, without the need for munging.
--

sCellId :: TableName -> SBV ColumnName -> SBV RowKey -> SBV CellId
sCellId (TableName tn) sCn sRk = coerceSBV $
  (literal $ tn ++ "__") .++ coerceSBV sCn .++ "__" .++ coerceSBV sRk

data SymbolicCells
  = SymbolicCells
    { _scIntValues    :: SArray CellId Integer
    , _scBoolValues   :: SArray CellId Bool
    , _scStringValues :: SArray CellId String
    -- TODO: decimal
    -- TODO: time
    -- TODO: opaque blobs
    }
    deriving (Show, Generic, Mergeable)

mkSymbolicCells :: Symbolic SymbolicCells
mkSymbolicCells = SymbolicCells
  <$> newArray "intCells"
  <*> newArray "boolCells"
  <*> newArray "stringCells"

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds      :: SBool
    , _lasTablesRead    :: SFunArray TableName Bool
    , _lasTablesWritten :: SFunArray TableName Bool
    , _lasColumnDeltas  :: SFunArray ColumnId Integer
    , _lasCells         :: SymbolicCells
    }
  deriving (Show, Generic, Mergeable)

-- Checking state that is transferred through every computation, in-order.
newtype GlobalAnalyzeState
  = GlobalAnalyzeState ()
  deriving (Show, Eq)

data AnalyzeState
  = AnalyzeState
    { _latticeState :: LatticeAnalyzeState
    , _globalState  :: GlobalAnalyzeState
    }
  deriving (Show)

instance Mergeable AnalyzeState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (AnalyzeState lls _) (AnalyzeState rls rgs) =
    AnalyzeState (symbolicMerge force test lls rls) rgs

initialAnalyzeState :: SymbolicCells -> AnalyzeState
initialAnalyzeState symCells = AnalyzeState
  { _latticeState = LatticeAnalyzeState
      { _lasSucceeds      = true
      , _lasTablesRead    = mkSFunArray $ const false
      , _lasTablesWritten = mkSFunArray $ const false
      , _lasColumnDeltas  = mkSFunArray $ const 0
      , _lasCells         = symCells
      }
  , _globalState  = GlobalAnalyzeState ()
  }

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind UserType
deriving instance SymWord UserType

symArrayAt
  :: forall array k v
   . (SymWord k, SymWord v, SymArray array)
  => SBV k -> Lens' (array k v) (SBV v)
symArrayAt symKey = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

data AnalyzeFailure
  = MalformedArithOpExec ArithOp [Term Integer]
  | UnsupportedArithOp ArithOp
  | MalformedComparison Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  | MalformedLogicalOpExec LogicalOp [Term Bool]
  | MalformedArithOp Text [AST Node]
  -- | Some translator received a node it didn't expect
  | UnexpectedNode String (AST Node)
  -- | 'translateBody' expects at least one node in a function body.
  | EmptyBody
  -- | A node we have a good reason not to handle
  | UnhandledTerm String ETerm
  | TypesDontMatch EType EType
  | BranchesDifferentTypes EType EType
  | KeyNotPresent String Object
  | NotConvertibleToSchema (Pact.Type Pact.UserType)
  deriving Show

type AnalyzeM = RWST AnalyzeEnv AnalyzeLog AnalyzeState (Except AnalyzeFailure)

data ArithOp
  = Sub -- "-" Integer / Decimal
  | Add -- "+"
  | Mul -- "*"
  | Div -- "/"

  | Pow -- "^"
  | Sqrt -- "sqrt"
  | Mod -- "mod"
  | Log -- "log"
  | Ln -- "ln"

  | Exp -- "exp"
  | Abs -- "abs"
  | Round -- "round"
  | Ceiling -- "ceiling"
  | Floor -- "floor"

  -- Extras not necessarily in pact
  -- @Signum@ because we want (Term Integer) to be a Num instance
  | Signum
  -- @Negate@ because @-@ is overloaded in pact to mean "minus" or "negate"
  | Negate
  deriving (Show, Eq, Ord)

unsupportedArithOps :: Set ArithOp
unsupportedArithOps = Set.fromList
  -- TODO(joel) support Exp with svExp?
  [Pow, Sqrt, Log, Ln, Exp, Round, Ceiling, Floor]

data LogicalOp = AndOp | OrOp | NotOp
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

data Type a where
  TInt     :: Type Integer
  TBool    :: Type Bool
  TStr     :: Type String
  TTime    :: Type Time
  TDecimal :: Type Decimal
  TObject  :: Type Object

data EType where
  -- TODO: parametrize over constraint
  EType :: (Show a, SymWord a) => Type a -> EType

typeEq :: Type a -> Type b -> Maybe (a :~: b)
typeEq TInt  TInt  = Just Refl
typeEq TBool TBool = Just Refl
typeEq TStr TStr = Just Refl
typeEq TTime TTime = Just Refl
typeEq TDecimal TDecimal = Just Refl
-- TODO: this should probably compare types of fields
typeEq TObject TObject = Just Refl
typeEq _     _     = Nothing

instance Eq EType where
  EType a == EType b = case typeEq a b of
    Just _refl -> True
    Nothing    -> False

data ETerm where
  -- TODO: remove Show (add constraint c?)
  ETerm :: (Show a, SymWord a) => Term a -> Type a -> ETerm
  EObject :: Term Object -> Type Object -> ETerm

data Term ret where
  IfThenElse     ::                        Term Bool    -> Term a         -> Term a -> Term a
  Enforce        ::                        Term Bool    ->                             Term Bool
  -- TODO: do we need a noop to handle a sequence of one expression?
  Sequence       :: (Show b, SymWord b) => Term b       -> Term a         ->           Term a
  Literal        ::                        SBV a        ->                             Term a

  LiteralObject  ::                        Map String (FieldType, ETerm)       ->                             Term Object
  At             ::                        String       -> Term Object    ->           Term a
  Read           ::                        TableName   -> Schema -> Term String    ->           Term Object
  -- NOTE: pact really does return a string here:
  Write          ::                        TableName -> Schema -> Term String -> Term Object -> Term String

  --
  -- TODO: retire:
  --
  WithRead       :: (Show a) => TableName -> Term String -> [Text] -> Term a -> Term a

  Let            :: (Show a, SymWord a) => Text         -> Term a         -> Term b -> Term b
  -- TODO: not sure if we need a separate `Bind` ctor for object binding. try
  --       just using Let+At first.
  Var            ::                        Text         ->                             Term a
  Arith          ::                        ArithOp      -> [Term Integer] ->           Term Integer
  Comparison     :: (Show a, SymWord a) => ComparisonOp -> Term a         -> Term a -> Term Bool
  Logical        ::                        LogicalOp    -> [Term Bool]    ->           Term Bool
  AddTimeInt     ::                        Term Time    -> Term Integer   ->           Term Time
  AddTimeDec     ::                        Term Time    -> Term Decimal   ->           Term Time
  NameAuthorized ::                        Term String  ->                             Term Bool
  Concat         ::                        Term String  -> Term String    ->           Term String
  PactVersion    ::                                                                    Term String

  --
  -- TODO: figure out the object representation we use here:
  --
  -- ObjAuthorized  ::                     Term Obj     ->                     Term Bool
  --
  -- TODO: we will also want to handle cases where load a keyset object by its
  -- name, and then use the object: e.g.:
  --
  --   (defconst ADMIN_KEYSET (read-keyset "accounts-admin-keyset"))
  --
  --  and then ADMIN_KEYSET is used in the code
  --

deriving instance Show a => Show (Term a)
deriving instance Show ETerm

deriving instance Show (Type a)
deriving instance Eq (Type a)
deriving instance Show EType
-- deriving instance Eq EType

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  x + y = Arith Add [x, y]
  x * y = Arith Mul [x, y]
  abs x = Arith Abs [x]
  signum x = Arith Signum [x]
  negate x = Arith Negate [x]

data DomainProperty where
  TableWrite       :: TableName  ->             DomainProperty -- anything in table is written
  TableRead        :: TableName  ->             DomainProperty -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnId -> DomainProperty -- particular column is written
  --
  CellIncrease     :: TableName  -> ColumnId -> DomainProperty -- any cell at all in col increases
  ColumnConserves  :: TableName  -> ColumnId -> DomainProperty -- sum of all changes in col is 0
  --
  KsNameAuthorized :: KeySetName ->             DomainProperty -- keyset authorized by name
  Abort            ::                           DomainProperty
  Success          ::                           DomainProperty
  --
  -- TODO: row-level keyset enforcement seems like it needs some form of
  --       unification, so that using a variable we can connect >1 domain
  --       property?
  --
  --       e.g.: forall row. RowWrite("balances", r) `Implies` RowKsEnforced(r)
  --
  --       RowKsEnforced  :: RowUid    ->            DomainProperty
  --       RowWrite       :: TableName -> RowUid  -> DomainProperty
  --
  -- TODO: Add DomainProperty/ies for constraining function arguments
  --       - e.g.: x > 10 `Implies` table_write(t0)
  --
  -- TODO: possibly allow use of input as parameter to domain properties
  --       - e.g.: column_increases_by(t0, x)     [where x is function input]
  --

data Property where
  Implies :: Property       -> Property -> Property
  Not     :: Property       ->             Property
  And     :: Property       -> Property -> Property
  Or      :: Property       -> Property -> Property
  Occurs  :: DomainProperty ->             Property

data Check where
  Satisfiable :: Property -> Check
  Valid       :: Property -> Check

--
-- TODO: this should probably have its own TranslateFailure type?
--
type TranslateM = ReaderT (Map Node Text) (Except AnalyzeFailure)

type Time = Int64

mkTime :: UTCTime -> Time
mkTime utct = utct ^. _utctDayTime . microseconds

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa

data CheckFailure
  = Invalid SBVI.SMTModel
  | Unsatisfiable
  | Unknown String -- reason
  | SatExtensionField SBVI.SMTModel
  | ProofError [String]
  | TypecheckFailure (Set TC.Failure)
  | AnalyzeFailure AnalyzeFailure
  --
  -- TODO: maybe remove this constructor from from CheckFailure.
  --
  | CodeCompilationFailed String
  deriving (Show)

data CheckSuccess
  = SatisfiedProperty SBVI.SMTModel
  | ProvedTheorem
  deriving (Show)

type CheckResult
  = Either CheckFailure CheckSuccess

makeLenses ''AnalyzeEnv
makeLenses ''AnalyzeState
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells

succeeds :: Lens' AnalyzeState SBool
succeeds = latticeState.lasSucceeds

tableRead :: TableName -> Lens' AnalyzeState SBool
tableRead tn = latticeState.lasTablesRead.symArrayAt (literal tn)

tableWritten :: TableName -> Lens' AnalyzeState SBool
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literal tn)

columnDelta :: TableName -> SBV ColumnName -> Lens' AnalyzeState SInteger
columnDelta tn sCn = latticeState.lasColumnDeltas.symArrayAt (sColId tn sCn)

intCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SInteger
intCell tn sCn sRk =
  latticeState.lasCells.scIntValues.symArrayAt (sCellId tn sCn sRk)

boolCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SBool
boolCell tn sCn sRk =
  latticeState.lasCells.scBoolValues.symArrayAt (sCellId tn sCn sRk)

stringCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SString
stringCell tn sCn sRk =
  latticeState.lasCells.scStringValues.symArrayAt (sCellId tn sCn sRk)

instance (Mergeable a) => Mergeable (AnalyzeM a) where
  symbolicMerge force test left right = RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per AnalyzeState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST act r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

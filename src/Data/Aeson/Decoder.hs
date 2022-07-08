-- |
-- Stability: experimental
--
-- Functions and combinators for decoding JSON 'Value's into Haskell values.
--
-- It\'s recommended to import this module qualified,
-- to avoid name clashes with "Prelude" functions.
--
-- Here we will use the following import scheme:
--
-- @
-- {-# LANGUAGE ImportQualifiedPost #-}
-- import Data.Aeson.Decoder (Decoder)
-- import Data.Aeson.Decoder qualified as Decoder
-- @
--
-- Consider the following data type:
--
-- >>> :{
-- data Person = Person
--   { name :: String
--   , age :: Int
--   , email :: Maybe String
--   }
--   deriving (Show)
-- :}
--
-- We could write a 'Decoder' for this type like this:
--
-- >>> :{
-- personDecoder :: Decoder Person
-- personDecoder = Person
--   <$> Decoder.field "name" Decoder.string
--   <*> Decoder.field "age" Decoder.int
--   <*> Decoder.optionalField "email" Decoder.string
-- :}
--
-- And we could use this 'Decoder' to decode a 'ByteString':
--
-- >>> Decoder.decodeByteString personDecoder "{\"name\":\"John Doe\",\"age\":42,\"email\":\"foo@bar.baz\"}"
-- Success (Person {name = "John Doe", age = 42, email = Just "foo@bar.baz"})
module Data.Aeson.Decoder
  ( -- * Decoder
    Decoder (..),

    -- * Result

    -- | 'Result' is reexported from "Data.Aeson".
    -- In this module it represents the result of applying a 'Decoder' to a 'Value'.
    Result (..),

    -- * Construction
    decoder,
    auto,
    generic,
    genericWith,

    -- * Runners
    decode,
    decodeByteString,

    -- * Primitives
    null,
    bool,

    -- ** Numbers
    int,
    integer,
    word,
    natural,
    float,
    double,
    scientific,

    -- ** Strings
    char,
    string,
    text,

    -- ** JSON 'Value's
    value,
    object,
    array,

    -- * Combinators

    -- ** Objects
    field,
    optionalField,

    -- ** Arrays
    vector,
    list,
    nonEmpty,

    -- ** Alternatives
    maybe,
    nullable,
    either,
    oneOf,
  )
where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Internal qualified as Aeson
import Data.Aeson.Parser.Internal qualified as Aeson
import Data.Aeson.Types
  ( Array,
    JSONPathElement (Index),
    Key,
    Object,
    Parser,
    Result (..),
    Value (..),
    (<?>),
  )
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic (Rep))
import Numeric.Natural (Natural)
import Prelude hiding (either, maybe, null)
import Prelude qualified

-- $setup
-- >>> :m -Data.Aeson.Decoder
-- >>> import Data.Aeson.Decoder (Decoder)
-- >>> import Data.Aeson.Decoder qualified as Decoder

-- | A value which can decode JSON 'Value's into Haskell values of type @a@.
newtype Decoder a = Decoder {parseJSON :: Value -> Parser a}
  deriving
    (Functor, Applicative, Monad, Alternative, MonadFail)
    via (ReaderT Value Parser)

-- | Construct a 'Decoder' from a function from a 'Value' to 'Result'.
--
-- Consider using primitives and combinators to build a 'Decoder' instead.
decoder :: (Value -> Result a) -> Decoder a
decoder f = Decoder $ \v -> case f v of
  Error s -> fail s
  Success a -> pure a

-- | Construct a 'Decoder' using the 'Aeson.FromJSON' instance.
auto :: Aeson.FromJSON a => Decoder a
auto = Decoder Aeson.parseJSON

-- | Construct a 'Decoder' with 'Aeson.genericParseJSON' and 'Aeson.defaultOptions' via the 'Generic' class.
--
-- @
-- 'generic' = 'genericWith' 'Aeson.defaultOptions'
-- @
generic :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Decoder a
generic = genericWith Aeson.defaultOptions

-- | Construct a 'Decoder' with 'Aeson.genericParseJSON' via the 'Generic' class.
genericWith :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.Options -> Decoder a
genericWith = Decoder . Aeson.genericParseJSON

-- | Run a 'Decoder' on a JSON 'Value'.
decode :: Decoder a -> Value -> Result a
decode (Decoder d) = Aeson.parse d

-- | Run a 'Decoder on a 'ByteString'.
decodeByteString :: Decoder a -> ByteString -> Result a
decodeByteString (Decoder d) =
  Prelude.either (Error . uncurry Aeson.formatError) Success
    . Aeson.eitherDecodeWith Aeson.jsonEOF (Aeson.iparse d)

-- | Decode a 'Null' value.
null :: Decoder ()
null = Decoder $ \case
  Null -> pure ()
  v -> Aeson.typeMismatch "Null" v

-- | Decode a 'Bool' value.
bool :: Decoder Bool
bool = auto

-- | Decode an 'Int' value.
int :: Decoder Int
int = auto

-- | Decode an 'Integer' value.
integer :: Decoder Integer
integer = auto

-- | Decode a 'Word' value.
word :: Decoder Word
word = auto

-- | Decode a 'Natural' value.
natural :: Decoder Natural
natural = auto

-- | Decode a 'Float' value.
float :: Decoder Float
float = auto

-- | Decode a 'Double' value.
double :: Decoder Double
double = auto

-- | Decode a 'Scientific' number.
scientific :: Decoder Scientific
scientific = auto

-- | Decode a single 'Char'.
char :: Decoder Char
char = auto

-- | Decode a 'String'.
string :: Decoder String
string = auto

-- | Decode 'Text'.
text :: Decoder Text
text = auto

-- | Decode a JSON 'Value'. This decoder will always succeed.
value :: Decoder Value
value = Decoder pure

-- | Decode a JSON 'Object'.
object :: Decoder Object
object = auto

-- | Decode a JSON 'Array'.
array :: Decoder Array
array = auto

-- | Decode the value at the given 'Key'.
--
-- >>> Decoder.decodeByteString (Decoder.field "foo" Decoder.int) "{\"foo\": 42}"
-- Success 42
field :: Key -> Decoder a -> Decoder a
field key (Decoder d) = Decoder $ \case
  Object obj -> Aeson.explicitParseField d obj key
  v -> Aeson.typeMismatch "Object" v

-- | Decode a optional value at the given 'Key'.
-- This will return 'Nothing' if the key is not present or if its value is 'Null.
-- See '(Aeson..:?)' for more details.
--
-- >>> let foo = Decoder.optionalField "foo" Decoder.int
--
-- >>> Decoder.decodeByteString foo "{\"foo\": 42}"
-- Success (Just 42)
--
-- >>> Decoder.decodeByteString foo "{\"foo\": null}"
-- Success Nothing
--
-- >>> Decoder.decodeByteString foo "{\"bar\": false}"
-- Success Nothing
optionalField :: Key -> Decoder a -> Decoder (Maybe a)
optionalField key (Decoder d) = Decoder $ \case
  Object obj -> Aeson.explicitParseFieldMaybe d obj key
  v -> Aeson.typeMismatch "Object" v

-- | Decode a 'Vector' using the given 'Decoder'.
--
-- >>> Decoder.decodeByteString (Decoder.vector Decoder.int) "[1, 2, 3]"
-- Success [1,2,3]
vector :: Decoder a -> Decoder (Vector a)
vector (Decoder d) = Decoder $ \case
  Array vs -> Vector.imapM (\i v -> d v <?> Index i) vs
  v -> Aeson.typeMismatch "Array" v

-- | Decode a List using the given 'Decoder'.
list :: Decoder a -> Decoder [a]
list d = Vector.toList <$> vector d

-- | Decode a 'NonEmpty' list using the given 'Decoder'.
nonEmpty :: Decoder a -> Decoder (NonEmpty a)
nonEmpty d =
  list d
    >>= Prelude.maybe (fail "parsing NonEmpty failed, unexpected empty list") pure
      . NonEmpty.nonEmpty

-- | Try to decode a value, or return 'Nothing' if the 'Decoder' fails.
-- This will always succeed.
maybe :: Decoder a -> Decoder (Maybe a)
maybe = optional

-- | Try to decode a value, or return 'Nothing' on 'Null'.
--
-- Compared to 'maybe' this 'Decoder' may fail. For example:
--
-- >>> Decoder.decodeByteString (Decoder.nullable Decoder.int) "null"
-- Success Nothing
--
-- >>> Decoder.decodeByteString (Decoder.nullable Decoder.int) "true"
-- Error "Error in $: parsing Int failed, expected Number, but encountered Boolean"
nullable :: Decoder a -> Decoder (Maybe a)
nullable d = Decoder $ \v -> case decode d v of
  Error s
    | v == Null -> pure Nothing
    | otherwise -> fail s
  Success a -> pure $ Just a

-- | Decoder either an @a@ or @b@. The @b@ 'Decoder' is attempted first.
either :: Decoder a -> Decoder b -> Decoder (Either a b)
either a b = Right <$> b <|> Left <$> a

-- | Try a list of 'Decoder's in order.
-- Returns the first success or fails if none of them succeed.
-- @
-- oneOf = 'asum'
-- @
oneOf :: [Decoder a] -> Decoder a
oneOf = asum

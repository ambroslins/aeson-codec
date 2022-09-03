-- |
-- Stability: experimental
--
-- A @'Codec' a@ describes a @'Decoder' a@ and an @'Encoder' a@ in a single data type.
--
-- It\'s recommended to import this module qualified,
-- to avoid name clashes with "Prelude" functions.
--
-- Here we will use the following import scheme:
--
-- @
-- {-# LANGUAGE ImportQualifiedPost #-}
-- import Data.Aeson.Codec (Codec)
-- import Data.Aeson.Codec qualified as Codec
-- import Data.Aeson.Decoder (Decoder)
-- import Data.Aeson.Decoder qualified as Decoder
-- import Data.Aeson.Encoder (Encoder)
-- import Data.Aeson.Encoder qualified as Encoder
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
-- We could write a 'Codec' for this type like this:
--
-- >>> :{
-- personCodec :: Codec Person
-- personCodec =
--   Codec.object $ Person
--     <$> Codec.field "name" Codec.string name
--     <*> Codec.field "age" Codec.int age
--     <*> Codec.optionalField "email" Codec.string email
-- :}
--
-- We can define the 'Decoder' and 'Encoder' for @Person@ in terms of this 'Codec'.
-- >>> personDecoder = Codec.decoder personCodec
-- >>> personEncoder = Codec.encoder personCodec
--
--
-- >>> Encoder.encodeByteString personEncoder (Person "John Doe" 42 (Just "foo@bar.baz"))
-- "{\"name\":\"John Doe\",\"age\":42,\"email\":\"foo@bar.baz\"}"
-- >>> Decoder.decodeByteString personDecoder "{\"name\":\"John Doe\",\"age\":42,\"email\":\"foo@bar.baz\"}"
-- Success (Person {name = "John Doe", age = 42, email = Just "foo@bar.baz"})
module Data.Aeson.Codec
  ( -- * Codec
    Codec (..),

    -- * Construction
    codec,
    auto,
    generic,
    genericWith,

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

    -- * Combinators

    -- ** Objects
    ObjectCodec,
    object,
    field,
    optionalField,

    -- ** Arrays
    vector,
    list,
    nonEmpty,

    -- ** Alternatives
    maybe,
    nullable,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Compat(Key)
import Data.Aeson.Decoder (Decoder (..))
import Data.Aeson.Decoder qualified as Decoder
import Data.Aeson.Encoder (Encoder)
import Data.Aeson.Encoder qualified as Encoder
import Data.Aeson.Types (Object, Parser, Value (Object))
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.List.NonEmpty (NonEmpty)
import Data.Profunctor (Profunctor (dimap))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic, Rep)
import Numeric.Natural (Natural)
import Prelude hiding (map, maybe, null)

-- $setup
-- >>> :m -Data.Aeson.Codec
-- >>> import Data.Aeson.Codec (Codec)
-- >>> import Data.Aeson.Codec qualified as Codec
-- >>> import Data.Aeson.Decoder (Decoder)
-- >>> import Data.Aeson.Decoder qualified as Decoder
-- >>> import Data.Aeson.Encoder (Encoder)
-- >>> import Data.Aeson.Encoder qualified as Encoder

-- | The 'Codec' type which contains a 'Decoder' and an 'Encoder'.
data Codec a = Codec
  { encoder :: Encoder a,
    decoder :: Decoder a
  }

-- | Construct a 'Codec' from a given 'Decoder' and an 'Encoder'.
--
-- Consider using primitives and combinators to build a 'Codec' instead.
codec :: Encoder a -> Decoder a -> Codec a
codec = Codec

-- | Construct a 'Codec' using the 'Aeson.FromJSON' and 'Aeson.ToJson' instances.
auto :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
auto = Codec {encoder = Encoder.auto, decoder = Decoder.auto}

-- | Construct a 'Codec' using the 'Generic' typeclass and 'Aeson.defaultOptions'.
--
-- @
-- 'generic' = 'genericWith' 'Aeson.defaultOptions'
-- @
generic ::
  ( Generic a,
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a),
    Aeson.GToJSON' Value Aeson.Zero (Rep a),
    Aeson.GFromJSON Aeson.Zero (Rep a)
  ) =>
  Codec a
generic = genericWith Aeson.defaultOptions

-- | Construct a 'Codec' using the 'Generic' typeclass.
genericWith ::
  ( Generic a,
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a),
    Aeson.GToJSON' Value Aeson.Zero (Rep a),
    Aeson.GFromJSON Aeson.Zero (Rep a)
  ) =>
  Aeson.Options ->
  Codec a
genericWith options =
  Codec
    { encoder = Encoder.genericWith options,
      decoder = Decoder.genericWith options
    }

-- | 'Codec' for 'Null' values.
null :: Codec ()
null =
  Codec
    { encoder = Encoder.null,
      decoder = Decoder.null
    }

-- | 'Codec' for 'Bool' values.
bool :: Codec Bool
bool = auto

-- | 'Codec' for 'Int' values.
int :: Codec Int
int = auto

-- | 'Codec' for 'Integer' values.
integer :: Codec Integer
integer = auto

-- | 'Codec' for 'Word' values.
word :: Codec Word
word = auto

-- | 'Codec' for 'Natural' values.
natural :: Codec Natural
natural = auto

-- | 'Codec' for 'Float' values.
float :: Codec Float
float = auto

-- | 'Codec' for 'Double' values.
double :: Codec Double
double = auto

-- | 'Codec' for 'Scientific' numbers.
scientific :: Codec Scientific
scientific = auto

-- | 'Codec' for 'Char'.
char :: Codec Char
char = auto

-- | 'Codec' for 'String':
string :: Codec String
string = auto

-- | 'Codec' for 'Text'.
text :: Codec Text
text = auto

-- | The 'ObjectCodec' object type which is used for bidirectional object en-/decoding.
--
-- Use 'object' to convert a 'ObjectCodec' to a 'Codec'.
data ObjectCodec a b = ObjectCodec
  { en :: [Encoder.KeyValuePair a],
    de :: Object -> Parser b
  }
  deriving (Functor)

instance Profunctor ObjectCodec where
  dimap f g ObjectCodec {en, de} =
    ObjectCodec
      { en = contramap f <$> en,
        de = fmap g <$> de
      }

instance Applicative (ObjectCodec a) where
  pure x = ObjectCodec {en = [], de = const (pure x)}
  f <*> k =
    ObjectCodec
      { en = en f <> en k,
        de = \o -> de f o <*> de k o
      }

-- | Convert a 'ObjectCodec' to a 'Codec'.
object :: ObjectCodec a a -> Codec a
object ObjectCodec {en, de} =
  Codec
    { encoder = Encoder.object en,
      decoder = Decoder $ \case
        Object o -> de o
        v -> Aeson.typeMismatch "Object" v
    }

-- | 'Codec' for a field with the given 'Key'.
field :: Key -> Codec b -> (a -> b) -> ObjectCodec a b
field key Codec {encoder, decoder} f =
  ObjectCodec
    { en = [Encoder.field key encoder f],
      de = \o -> Aeson.explicitParseField (Decoder.parseJSON decoder) o key
    }

-- | 'Codec' for a optional field with the given 'Key'.
optionalField :: Key -> Codec b -> (a -> Maybe b) -> ObjectCodec a (Maybe b)
optionalField key Codec {encoder, decoder} f =
  ObjectCodec
    { en = [Encoder.optionalField key encoder f],
      de = \o -> Aeson.explicitParseFieldMaybe (Decoder.parseJSON decoder) o key
    }

map :: (Encoder a -> Encoder b) -> (Decoder a -> Decoder b) -> Codec a -> Codec b
map f g Codec {encoder, decoder} =
  Codec
    { encoder = f encoder,
      decoder = g decoder
    }

-- | 'Codec' for a 'Vector' using the given 'Codec'.
vector :: Codec a -> Codec (Vector a)
vector = map Encoder.vector Decoder.vector

-- | 'Codec' for a list using the given 'Codec'.
list :: Codec a -> Codec [a]
list = map Encoder.list Decoder.list

-- | 'Codec' for a 'NonEmpty' list using the given 'Codec'.
nonEmpty :: Codec a -> Codec (NonEmpty a)
nonEmpty = map Encoder.nonEmpty Decoder.nonEmpty

-- | 'Codec' for a 'Maybe' value using the given 'Codec'.
--
-- See 'Decoder.maybe'.
maybe :: Codec a -> Codec (Maybe a)
maybe = map Encoder.nullable Decoder.maybe

-- | 'Codec' for a 'Maybe' value using the given 'Codec'.
--
-- See 'Decoder.nullable'.
nullable :: Codec a -> Codec (Maybe a)
nullable = map Encoder.nullable Decoder.nullable

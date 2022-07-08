-- |
-- Stability: experimental
--
-- Functions and combinators for encoding Haskell values int JSON 'Value's.
--
-- It\'s recommended to import this module qualified,
-- to avoid name clashes with "Prelude" functions.
--
-- Here we will use the following import scheme:
--
-- @
-- {-# LANGUAGE ImportQualifiedPost #-}
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
-- :}
--
-- We could write an 'Encoder' for this type like this:
--
-- >>> :{
-- personEncoder :: Encoder Person
-- personEncoder = Encoder.object
--   [ Encoder.field "name" Encoder.string name
--   , Encoder.field "age" Encoder.int age
--   , Encoder.optionalField "email" Encoder.string email
--   ]
-- :}
--
-- And we could use this 'Encoder' to encode some Person to a 'ByteString':
--
-- >>> Encoder.encodeByteString personEncoder (Person "John Doe" 42 (Just "foo@bar.baz"))
-- "{\"name\":\"John Doe\",\"age\":42,\"email\":\"foo@bar.baz\"}"
module Data.Aeson.Encoder
  ( -- * Encoder
    Encoder (..),

    -- * Construction
    encoder,
    auto,
    generic,
    genericWith,

    -- * Runners
    encode,
    encodeByteString,

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
    array,

    -- * Combinators

    -- ** Objects
    KeyValuePair,
    object,
    field,
    optionalField,
    constField,

    -- ** Arrays
    list,
    vector,

    -- ** Alternatives
    maybe,
    nullable,
    either,
  )
where

import Data.Aeson (Array, Encoding, Key, Value (Array, Null))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic (Rep))
import Numeric.Natural (Natural)
import Prelude hiding (either, maybe, null)
import Prelude qualified

-- $setup
-- >>> :m -Data.Aeson.Encoder
-- >>> import Data.Aeson.Encoder (Encoder)
-- >>> import Data.Aeson.Encoder qualified as Encoder

data Encoder a = Encoder
  { toValue :: a -> Value,
    toEncoding :: a -> Encoding
  }

instance Contravariant Encoder where
  contramap f e =
    Encoder
      { toValue = toValue e . f,
        toEncoding = toEncoding e . f
      }

data KeyValuePair a = forall b. KeyValuePair Key (Encoder b) (a -> Maybe b)

instance Contravariant KeyValuePair where
  contramap f (KeyValuePair key e g) = KeyValuePair key e (g . f)

-- | Construct a 'Encoder' from a function from 'a' to 'Value'.
encoder :: (a -> Value) -> Encoder a
encoder f = Encoder {toValue = f, toEncoding = Aeson.toEncoding . f}

-- | Construct a 'Encoder' using the 'Aeson.ToJSON' instance.
auto :: Aeson.ToJSON a => Encoder a
auto =
  Encoder
    { toValue = Aeson.toJSON,
      toEncoding = Aeson.toEncoding
    }

-- | Construct a 'Decoder' with 'Aeson.genericToJSON' and
-- 'Aeson.genericToEncoding' with 'Aeson.defaultOpetions' via the 'Generic' class.
--
-- @ 'generic' = 'genericWith' 'Aeson.defaultOptions' @
generic ::
  ( Generic a,
    Aeson.GToJSON Aeson.Zero (Rep a),
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a)
  ) =>
  Encoder a
generic = genericWith Aeson.defaultOptions

-- | Construct a 'Decoder' with 'Aeson.genericToJSON' and
-- 'Aeson.genericToEncoding' via the 'Generic' class.
genericWith ::
  ( Generic a,
    Aeson.GToJSON Aeson.Zero (Rep a),
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a)
  ) =>
  Aeson.Options ->
  Encoder a
genericWith options =
  Encoder
    { toValue = Aeson.genericToJSON options,
      toEncoding = Aeson.genericToEncoding options
    }

-- | Run a 'Encoder' mapping some @a@ to a JSON 'Value'.
--
-- Alias for 'toValue'
encode :: Encoder a -> a -> Value
encode = toValue

-- | Run a 'Encoder' mapping some @a@ to a lazy 'ByteString'.
encodeByteString :: Encoder a -> a -> ByteString
encodeByteString =
  fmap Aeson.encodingToLazyByteString
    . toEncoding

-- | Encode any value as 'Null'.
null :: Encoder a
null = Encoder {toValue = const Null, toEncoding = const Aeson.null_}

-- | Encode a 'Bool' value.
bool :: Encoder Bool
bool = auto

-- | Encode an 'Int' value.
int :: Encoder Int
int = auto

-- | Encode an 'Integer' value.
integer :: Encoder Integer
integer = auto

-- | Encode a 'Word' value.
word :: Encoder Word
word = auto

-- | Encode a 'Natural' value.
natural :: Encoder Natural
natural = auto

-- | Encode a 'Float' value.
float :: Encoder Float
float = auto

-- | Encode a 'Double' value.
double :: Encoder Double
double = auto

-- | Encode a 'Scientific' number.
scientific :: Encoder Scientific
scientific = auto

-- | Encode a single 'Char'.
char :: Encoder Char
char = auto

-- | Encode a 'String'.
string :: Encoder String
string = auto

-- | Encode 'Text'.
text :: Encoder Text
text = auto

-- | Encode a JSON 'Value'. This decoder will always succeed.
value :: Encoder Value
value = auto

-- | Encode a JSON 'Array'.
array :: Encoder Array
array = auto

-- | Encode a List of 'KeyValuePair's as an 'Object'.
object :: forall a. [KeyValuePair a] -> Encoder a
object kvs =
  Encoder
    { toValue =
        Aeson.object
          . go toValue,
      toEncoding =
        Aeson.pairs
          . foldMap (uncurry Aeson.pair)
          . go toEncoding
    }
  where
    go :: (forall b. Encoder b -> b -> e) -> a -> [(Key, e)]
    go f v =
      mapMaybe
        ( \(KeyValuePair k e g) -> case g v of
            Nothing -> Nothing
            Just a -> Just (k, f e a)
        )
        kvs

-- | Encode some @a@ using the projection @a -> b@ and a @'Encoder' b@ at the
-- given 'Key'.
field :: Key -> Encoder b -> (a -> b) -> KeyValuePair a
field k e f = KeyValuePair k e (Just . f)

-- | Encode some @a@ using the optional projection @a -> 'Maybe' b@ and a
-- @'Encoder' b@ at the given 'Key'.
-- If the projection returns 'Nothing' the 'Key' will be absent from the 'Object'.
--
-- >>> :{
-- let maybePair =
--       Encoder.object
--         [ Encoder.optionalField "fst" Encoder.bool fst
--         , Encoder.optionalField "snd" Encoder.int snd
--         ]
-- :}
--
-- >>> Encoder.encodeByteString maybePair (Just True, Just 42)
-- "{\"fst\":true,\"snd\":42}"
--
-- >>> Encoder.encodeByteString maybePair (Just True, Nothing)
-- "{\"fst\":true}"
optionalField :: Key -> Encoder b -> (a -> Maybe b) -> KeyValuePair a
optionalField = KeyValuePair

-- | Encode some value with an 'Encoder' at the given 'Key'.
--
-- >>> let foo = Encoder.object [Encoder.constField "foo" Encoder.int 42]
-- >>> Encoder.encodeByteString foo ()
-- "{\"foo\":42}"
constField :: Key -> Encoder b -> b -> KeyValuePair a
constField k e x = field k e (const x)

-- | Enocde a list using the given 'Encoder.
list :: Encoder a -> Encoder [a]
list e =
  Encoder
    { toValue = Array . Vector.fromList . map (toValue e),
      toEncoding = Aeson.list (toEncoding e)
    }

-- | Enocde a 'Vector' using the given 'Encoder.
vector :: Encoder a -> Encoder (Vector a)
vector e =
  Encoder
    { toValue = Array . Vector.map (toValue e),
      toEncoding = Aeson.list (toEncoding e) . Vector.toList
    }

-- | Encode a 'Maybe' value.
maybe :: Encoder () -> Encoder a -> Encoder (Maybe a)
maybe eNothing eJust =
  Encoder
    { toValue = Prelude.maybe (toValue eNothing ()) (toValue eJust),
      toEncoding = Prelude.maybe (toEncoding eNothing ()) (toEncoding eJust)
    }

-- | Encode a 'Maybe' value.
-- This will encode 'Nothing' as 'Null'.
-- @ nullable = maybe null @
nullable :: Encoder a -> Encoder (Maybe a)
nullable = maybe null

-- | Encode an 'Either' value.
either :: Encoder a -> Encoder b -> Encoder (Either a b)
either l r =
  Encoder
    { toValue = Prelude.either (toValue l) (toValue r),
      toEncoding = Prelude.either (toEncoding l) (toEncoding r)
    }

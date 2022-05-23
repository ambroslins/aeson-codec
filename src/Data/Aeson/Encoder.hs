-- |
-- Stability: experimental
--
-- Functions and combinators for encoding Haskell values int JSON 'Value's.
module Data.Aeson.Encoder
  ( -- * Encoder
    Encoder,

    -- * Construction
    encoder,
    auto,
    generic,
    genericWith,

    -- * Runners
    encode,
    encodeByteString,
    toValue,
    toEncoding,

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
  )
where

import Data.Aeson (Array, Key, Value (Array, Null))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoder.Internal (Encoder (..), KeyValuePair (..))
import Data.Aeson.Encoding qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic (Rep))
import Numeric.Natural (Natural)
import Prelude hiding (either, maybe, null)
import Prelude qualified

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
--
-- Consider the following data type and encoder:
--
-- >>> :{ data Person = Person
--    { name :: String
--    , age :: Int
--    , email :: Maybe String
--    }
-- :}
--
-- > person = object [ field "name" string name, field "age" int age, optionalField "email" string email ]
--
-- >>> encodeByteString person (Person "Naomi" 42 (Just "foo@bar.baz"))
-- "{\"name\":\"Naomi\",\"age\":42,\"email\":\"foo@bar.baz\"}"
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
--
-- See 'object' for more information.
field :: Key -> Encoder b -> (a -> b) -> KeyValuePair a
field k e f = KeyValuePair k e (Just . f)

-- | Encode some @a@ using the optional projection @a -> 'Maybe' b@ and a
-- @'Encoder' b@ at the given 'Key'.
-- If the projection returns 'Nothing' the 'Key' will be absent from the 'Object'.
--
-- See 'object' for more information.
optionalField :: Key -> Encoder b -> (a -> Maybe b) -> KeyValuePair a
optionalField = KeyValuePair

-- | Enocde a 'Vector' using the given 'Encoder.
vector :: Encoder a -> Encoder (Vector a)
vector e =
  Encoder
    { toValue = Array . Vector.map (toValue e),
      toEncoding = Aeson.list (toEncoding e) . Vector.toList
    }

-- | Enocde a list using the given 'Encoder.
list :: Encoder a -> Encoder [a]
list e =
  Encoder
    { toValue = Array . Vector.fromList . map (toValue e),
      toEncoding = Aeson.list (toEncoding e)
    }

-- | Encode an 'Either' value.
either :: Encoder a -> Encoder b -> Encoder (Either a b)
either l r =
  Encoder
    { toValue = Prelude.either (toValue l) (toValue r),
      toEncoding = Prelude.either (toEncoding l) (toEncoding r)
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
-- @ maybeOrNull = maybe null @
maybeOrNull :: Encoder a -> Encoder (Maybe a)
maybeOrNull = maybe null

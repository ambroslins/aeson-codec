module Data.Aeson.Encoder.Internal
  ( Encoder,
    KeyValuePair,
    toValue,
    toEncoding,
    encoder,
    auto,
    array,
    text,
    string,
    scientific,
    int,
    double,
    float,
    bool,
    null,
    vector,
    list,
    either,
    maybe,
    maybeOrNull,
    object,
    field,
    optionalField,
    generic,
  )
where

import Data.Aeson (Array, Encoding, Key, Value (Array, Null))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic (Rep))
import Prelude hiding (either, maybe, null)
import Prelude qualified

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

encoder :: (a -> Value) -> Encoder a
encoder f = Encoder {toValue = f, toEncoding = Aeson.toEncoding . f}

auto :: Aeson.ToJSON a => Encoder a
auto =
  Encoder
    { toValue = Aeson.toJSON,
      toEncoding = Aeson.toEncoding
    }

array :: Encoder Array
array = auto

text :: Encoder Text
text = auto

string :: Encoder String
string = auto

scientific :: Encoder Scientific
scientific = auto

-- | Encode an int
-- >>> toValue int 42
-- Number 42.0
int :: Encoder Int
int = auto

double :: Encoder Double
double = auto

float :: Encoder Float
float = auto

bool :: Encoder Bool
bool = auto

null :: Encoder a
null = Encoder {toValue = const Null, toEncoding = const Aeson.null_}

vector :: Encoder a -> Encoder (Vector a)
vector e =
  Encoder
    { toValue = Array . Vector.map (toValue e),
      toEncoding = Aeson.list (toEncoding e) . Vector.toList
    }

list :: Encoder a -> Encoder [a]
list e =
  Encoder
    { toValue = Array . Vector.fromList . map (toValue e),
      toEncoding = Aeson.list (toEncoding e)
    }

either :: Encoder a -> Encoder b -> Encoder (Either a b)
either eLeft eRight =
  Encoder
    { toValue = Prelude.either (toValue eLeft) (toValue eRight),
      toEncoding = Prelude.either (toEncoding eLeft) (toEncoding eRight)
    }

maybe :: Encoder () -> Encoder a -> Encoder (Maybe a)
maybe eNothing eJust =
  Encoder
    { toValue = Prelude.maybe (toValue eNothing ()) (toValue eJust),
      toEncoding = Prelude.maybe (toEncoding eNothing ()) (toEncoding eJust)
    }

maybeOrNull :: Encoder a -> Encoder (Maybe a)
maybeOrNull = maybe null

data KeyValuePair a = forall b. KeyValuePair Key (Encoder b) (a -> Maybe b)

instance Contravariant KeyValuePair where
  contramap f (KeyValuePair key e g) = KeyValuePair key e (g . f)

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

field :: Key -> Encoder b -> (a -> b) -> KeyValuePair a
field k e f = KeyValuePair k e (Just . f)

optionalField :: Key -> Encoder b -> (a -> Maybe b) -> KeyValuePair a
optionalField = KeyValuePair

generic ::
  ( Generic a,
    Aeson.GToJSON Aeson.Zero (Rep a),
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a)
  ) =>
  Aeson.Options ->
  Encoder a
generic options =
  Encoder
    { toValue = Aeson.genericToJSON options,
      toEncoding = Aeson.genericToEncoding options
    }

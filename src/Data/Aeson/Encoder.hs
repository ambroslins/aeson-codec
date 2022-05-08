module Data.Aeson.Encoder
  ( Encoder,
    KeyValuePair,
    toValue,
    toEncoding,
    encoder,
    viaToJSON,
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

viaToJSON :: Aeson.ToJSON a => Encoder a
viaToJSON =
  Encoder
    { toValue = Aeson.toJSON,
      toEncoding = Aeson.toEncoding
    }

array :: Encoder Array
array = viaToJSON

text :: Encoder Text
text = viaToJSON

string :: Encoder String
string = viaToJSON

scientific :: Encoder Scientific
scientific = viaToJSON

int :: Encoder Int
int = viaToJSON

double :: Encoder Double
double = viaToJSON

float :: Encoder Float
float = viaToJSON

bool :: Encoder Bool
bool = viaToJSON

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
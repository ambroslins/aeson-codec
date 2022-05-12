module Data.Aeson.Codec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Decoder (Decoder)
import Data.Aeson.Decoder qualified as Decoder
import Data.Aeson.Encoder (Encoder)
import Data.Aeson.Encoder qualified as Encoder
import Data.Aeson.Types (Key, Object, Parser, Value (Object))
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Profunctor (Profunctor (dimap))

data Codec a = Codec
  { encoder :: Encoder a,
    decoder :: Decoder a
  }

auto :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
auto = Codec {encoder = Encoder.auto, decoder = Decoder.auto}

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
        de = ((<*>) . de f) <*> de k
      }

object :: ObjectCodec a a -> Codec a
object ObjectCodec {en, de} =
  Codec
    { encoder = Encoder.object en,
      decoder = Decoder.Decoder $ \case
        Object o -> de o
        v -> Aeson.typeMismatch "Object" v
    }

field :: Key -> Codec b -> (a -> b) -> ObjectCodec a b
field key Codec {encoder, decoder} f =
  ObjectCodec
    { en = [Encoder.field key encoder f],
      de = \o -> Aeson.explicitParseField (Decoder.parseJSON decoder) o key
    }

optionalField :: Key -> Codec b -> (a -> Maybe b) -> ObjectCodec a (Maybe b)
optionalField key Codec {encoder, decoder} f =
  ObjectCodec
    { en = [Encoder.optionalField key encoder f],
      de = \o -> Aeson.explicitParseFieldMaybe (Decoder.parseJSON decoder) o key
    }

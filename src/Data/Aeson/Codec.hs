module Data.Aeson.Codec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Decoder (Decoder)
import Data.Aeson.Decoder qualified as Decoder
import Data.Aeson.Encoder (Encoder)
import Data.Aeson.Encoder qualified as Encoder
import Data.Aeson.Types (Object, Parser, Value (Object))
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Profunctor (Profunctor (lmap, rmap))

data Codec a = Codec
  { decoder :: Decoder a,
    encoder :: Encoder a
  }

instanced :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
instanced = Codec {decoder = Decoder.viaFromJSON, encoder = Encoder.viaToJSON}

data ObjectCodec a b = ObjectCodec
  { objectDecoder :: Object -> Parser b,
    objectEncoders :: [Encoder.KeyValuePair a]
  }
  deriving (Functor)

instance Profunctor ObjectCodec where
  lmap f oc = oc {objectEncoders = contramap f <$> objectEncoders oc}
  rmap f oc = oc {objectDecoder = fmap f <$> objectDecoder oc}

instance Applicative (ObjectCodec a) where
  pure x =
    ObjectCodec
      { objectDecoder = const (pure x),
        objectEncoders = []
      }

  f <*> k =
    ObjectCodec
      { objectDecoder = ((<*>) . objectDecoder f) <*> objectDecoder k,
        objectEncoders = objectEncoders f <> objectEncoders k
      }

object :: ObjectCodec a a -> Codec a
object oc =
  Codec
    { decoder = Decoder.fromParseJSON $ \case
        Object o -> objectDecoder oc o
        v -> Aeson.typeMismatch "Object" v,
      encoder = Encoder.object $ objectEncoders oc
    }

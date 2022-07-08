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
module Data.Aeson.Codec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Decoder (Decoder (..))
import Data.Aeson.Decoder qualified as Decoder
import Data.Aeson.Encoder (Encoder)
import Data.Aeson.Encoder qualified as Encoder
import Data.Aeson.Types (Key, Object, Parser, Value (Object))
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Profunctor (Profunctor (dimap))
import GHC.Generics (Generic, Rep)

-- $setup
-- >>> :m -Data.Aeson.Codec
-- >>> import Data.Aeson.Codec (Codec)
-- >>> import Data.Aeson.Codec qualified as Codec
-- >>> import Data.Aeson.Decoder (Decoder)
-- >>> import Data.Aeson.Decoder qualified as Decoder
-- >>> import Data.Aeson.Encoder (Encoder)
-- >>> import Data.Aeson.Encoder qualified as Encoder

data Codec a = Codec
  { encoder :: Encoder a,
    decoder :: Decoder a
  }

auto :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
auto = Codec {encoder = Encoder.auto, decoder = Decoder.auto}

codec :: Encoder a -> Decoder a -> Codec a
codec = Codec

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
      decoder = Decoder $ \case
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

generic ::
  ( Generic a,
    Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a),
    Aeson.GToJSON' Value Aeson.Zero (Rep a),
    Aeson.GFromJSON Aeson.Zero (Rep a)
  ) =>
  Aeson.Options ->
  Codec a
generic options =
  Codec
    { encoder = Encoder.genericWith options,
      decoder = Decoder.genericWith options
    }

int :: Codec Int
int = auto

string :: Codec String
string = auto

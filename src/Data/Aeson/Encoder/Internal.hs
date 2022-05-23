module Data.Aeson.Encoder.Internal where

import Data.Aeson (Encoding, Key, Value)
import Data.Functor.Contravariant (Contravariant (contramap))

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

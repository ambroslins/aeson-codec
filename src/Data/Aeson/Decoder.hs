module Data.Aeson.Decoder
  ( Decoder (..),
    decoder,
    auto,
    decode,
    object,
    array,
    text,
    string,
    scientific,
    int,
    bool,
    null,
    optional,
    value,
    field,
    optionalField,
    vector,
    list,
    generic,
  )
where

import Control.Applicative (Alternative, optional)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Internal (formatError, iparse)
import Data.Aeson.Parser qualified as Aeson
import Data.Aeson.Types
  ( Array,
    Key,
    Object,
    Parser,
    Result (..),
    Value (..),
  )
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic (Rep))
import Prelude hiding (null)

newtype Decoder a = Decoder {parseJSON :: Value -> Parser a}
  deriving
    (Functor, Applicative, Monad, Alternative, MonadFail)
    via (ReaderT Value Parser)

decode :: Decoder a -> ByteString -> Either String a
decode (Decoder d) = first (uncurry formatError) . Aeson.eitherDecodeWith Aeson.json (iparse d)

decoder :: (Value -> Result a) -> Decoder a
decoder f = Decoder $ \v -> case f v of
  Error s -> fail s
  Success a -> pure a

auto :: Aeson.FromJSON a => Decoder a
auto = Decoder Aeson.parseJSON

object :: Decoder Object
object = auto

array :: Decoder Array
array = auto

text :: Decoder Text
text = auto

string :: Decoder String
string = auto

scientific :: Decoder Scientific
scientific = auto

int :: Decoder Int
int = auto

bool :: Decoder Bool
bool = auto

null :: Decoder ()
null = Decoder $ \case
  Null -> pure ()
  v -> Aeson.typeMismatch "Null" v

value :: Decoder Value
value = Decoder pure

field :: Key -> Decoder a -> Decoder a
field key (Decoder d) = Decoder $ \case
  Object obj -> Aeson.explicitParseField d obj key
  v -> Aeson.typeMismatch "Object" v

optionalField :: Key -> Decoder a -> Decoder (Maybe a)
optionalField key (Decoder d) = Decoder $ \case
  Object obj -> Aeson.explicitParseFieldMaybe d obj key
  v -> Aeson.typeMismatch "Object" v

vector :: Decoder a -> Decoder (Vector a)
vector (Decoder d) = Decoder $ \case
  Array vs -> Vector.mapM d vs
  v -> Aeson.typeMismatch "Array" v

list :: Decoder a -> Decoder [a]
list d = Vector.toList <$> vector d

generic :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.Options -> Decoder a
generic = Decoder . Aeson.genericParseJSON
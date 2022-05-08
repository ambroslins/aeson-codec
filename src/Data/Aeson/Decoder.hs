module Data.Aeson.Decoder
  ( Decoder,
    decoder,
    toParseJSON,
    viaFromJSON,
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
import Prelude hiding (null)

newtype Decoder a = Decoder {toParseJSON :: Value -> Parser a}
  deriving
    (Functor, Applicative, Monad, Alternative, MonadFail)
    via (ReaderT Value Parser)

decode :: Decoder a -> ByteString -> Either String a
decode (Decoder d) = first (uncurry formatError) . Aeson.eitherDecodeWith Aeson.json (iparse d)

decoder :: (Value -> Result a) -> Decoder a
decoder f = Decoder $ \v -> case f v of
  Error s -> fail s
  Success a -> pure a

viaFromJSON :: Aeson.FromJSON a => Decoder a
viaFromJSON = Decoder Aeson.parseJSON

object :: Decoder Object
object = viaFromJSON

array :: Decoder Array
array = viaFromJSON

text :: Decoder Text
text = viaFromJSON

string :: Decoder String
string = viaFromJSON

scientific :: Decoder Scientific
scientific = viaFromJSON

int :: Decoder Int
int = viaFromJSON

bool :: Decoder Bool
bool = viaFromJSON

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
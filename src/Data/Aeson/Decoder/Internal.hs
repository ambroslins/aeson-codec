module Data.Aeson.Decoder.Internal where

import Control.Applicative (Alternative)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson.Types (Parser, Value (..))

-- | A value which can decode JSON 'Value's into Haskell values of type @a@.
newtype Decoder a = Decoder {parseJSON :: Value -> Parser a}
  deriving
    (Functor, Applicative, Monad, Alternative, MonadFail)
    via (ReaderT Value Parser)

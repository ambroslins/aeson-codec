{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
  ( Key,
  )
where

#if (MIN_VERSION_aeson(2,0,0))

import Data.Aeson.Key(Key)

#else

import Data.Text (Text)
type Key = Text

#endif

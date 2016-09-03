module Data.Argonaut.Generic.Util where

import Prelude
import Data.Array (reverse, fromFoldable, null, length)
import Data.Foldable (all)
import Data.Generic (DataConstructor)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, lastIndexOf, drop)

allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary = all (null <<< _.sigValues)


-- | Needed for applying unwrapUnaryRecords.
isUnaryRecord :: Array DataConstructor -> Boolean
isUnaryRecord constrSigns = length constrSigns == 1 -- Only one constructor


stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr

type History = List String
newtype DecodeError = DecodeError
  { history:: History
  , message:: String
  }

decodeError:: History -> String -> DecodeError
decodeError h s = DecodeError { history: h, message: s}

instance eqDecodeError :: Eq DecodeError where
  eq (DecodeError a) (DecodeError b) = a.history == b.history && a.message == b.message

instance showDecodeError :: Show DecodeError where
  show decodeError' = "DecodeError: " <> asMessage decodeError'

asMessage :: DecodeError -> String
asMessage (DecodeError d) = "Failed to decode Json '" <> d.message <> "' at " <> asString d.history
  where
    asString :: History -> String
    asString history = joinWith "." $ reverse (fromFoldable history)

module Data.Foreign.Helpers (
  readPropUnsafe,
  decodeJsonRespondable,
  exceptDecodeJsonRespondable,
  fromResponseDecodeJson
) where



import Control.Monad.Except.Trans (ExceptT, except)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, ForeignError(JSONError), fail, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.List.Types (NonEmptyList(..))
import Data.Identity (Identity)
import Data.NonEmpty (singleton)
import Prelude



-- readPropUnsafe :: forall t. String -> Foreign -> ExceptT (NonEmptyList ForeignError) Identity t
readPropUnsafe :: forall t4. String -> Foreign -> ExceptT (NonEmptyList ForeignError) Identity t4
readPropUnsafe x y = unsafeFromForeign <$> readProp x y



decodeJsonRespondable k =
  case decodeJson k of
       Left e -> Left $ NonEmptyList $ singleton $ JSONError e
       Right v -> Right v



exceptDecodeJsonRespondable = except <<< decodeJsonRespondable
-- (except $ decodeJson x0)



fromResponseDecodeJson json = either (\a -> fail $ JSONError "parse error") (\a -> pure a) (decodeJson (encodeJson (unsafeFromForeign json :: String)))

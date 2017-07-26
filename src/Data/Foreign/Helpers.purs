module Data.Foreign.Helpers (
  readPropUnsafe,
  decodeJsonRespondable,
  exceptDecodeJsonRespondable,
  fromResponseDecodeJson
) where



import Control.Monad.Except.Trans (ExceptT, except)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, ForeignError(JSONError), fail, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.List.Types (NonEmptyList(..))
import Data.Identity (Identity)
import Data.NonEmpty (singleton)
import Prelude (class Applicative, pure, (<$>), ($), (<<<))



readPropUnsafe :: forall a. String -> Foreign -> ExceptT (NonEmptyList ForeignError) Identity a
readPropUnsafe x y = unsafeFromForeign <$> readProp x y



decodeJsonRespondable :: forall a. DecodeJson a => Json -> Either (NonEmptyList ForeignError) a
decodeJsonRespondable k =
  case decodeJson k of
       Left e -> Left $ NonEmptyList $ singleton $ JSONError e
       Right v -> Right v



exceptDecodeJsonRespondable :: forall a b. Applicative b => DecodeJson a => Json -> ExceptT (NonEmptyList ForeignError) b a
exceptDecodeJsonRespondable = except <<< decodeJsonRespondable
-- (except $ decodeJson x0)



fromResponseDecodeJson :: forall a. DecodeJson a => Foreign -> ExceptT (NonEmptyList ForeignError) Identity a
fromResponseDecodeJson json = either (\a -> fail $ JSONError "parse error") (\a -> pure a) (decodeJson (encodeJson (unsafeFromForeign json :: String)))

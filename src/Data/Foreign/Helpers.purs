module Data.Foreign.Helpers (
  readPropUnsafe
) where



import Prelude ((<$>))
import Data.Foreign (Foreign(..), unsafeFromForeign)
import Data.Foreign.Index (readProp)



-- readPropUnsafe :: forall t. String -> Foreign -> ExceptT (NonEmptyList ForeignError) Identity t
readPropUnsafe x y = unsafeFromForeign <$> readProp x y

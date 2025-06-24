module ValidationParseT where

import Control.Selective (Selective)
import Data.Error.Tree (ErrorTree)
import Data.Functor.Compose (Compose (..))
import MyPrelude

-- | A simple way to create an Applicative parser that parses from some environment.
--
-- Use with DerivingVia. Grep codebase for examples.
newtype ValidationParseT env m a = ValidationParseT {unValidationParseT :: env -> m (Validation (NonEmpty Error) a)}
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ((->) env)
            (Compose m (Validation (NonEmpty Error)))
        )

-- | A simple way to create an Applicative parser that parses from some environment. Version that uses ErrorTree instead of Error
--
-- Use with DerivingVia. Grep codebase for examples.
newtype ValidationParseTreeT env m a = ValidationParseTreeT {unValidationParseTreeT :: env -> m (Validation (NonEmpty ErrorTree) a)}
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ((->) env)
            (Compose m (Validation (NonEmpty ErrorTree)))
        )

module EurekaPROM.IO.Util (
    -- * Enums
    IrregularEnum(..)
  , AsIrregularEnum(..)
  ) where

import Data.Typeable

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

-- | Enumerations with an irregular (non-consecutive) domain
class IrregularEnum a where
  -- | Equivalent of 'fromEnum'
  fromIrregularEnum :: a -> Int

  -- | Equivalent of 'toEnum', but total rather than partial
  toIrregularEnum :: Int -> Maybe a

  -- | Used to define 'succ'
  --
  -- This only needs to specify the /irregular/ cases. The other cases will
  -- be defined using
  --
  -- > toIrregularEnum' . succ . fromIrregularEnum
  irregularSucc :: a -> Maybe a
  irregularSucc _ = Nothing

-- | Partial variant of 'irregularSucc'
toIrregularEnum' :: forall a. (Typeable a, IrregularEnum a) => Int -> a
toIrregularEnum' x =
    case toIrregularEnum x of
      Just y  -> y
      Nothing -> error $ concat [
          show x
        , " out of range of "
        , show (typeRep (Proxy @a))
        ]

newtype AsIrregularEnum a = WrapIrregularEnum {
      unwrapIrregularEnum :: a
    }
  deriving stock (Eq)

instance (Typeable a, IrregularEnum a, Eq a) => Enum (AsIrregularEnum a) where
  fromEnum   = fromIrregularEnum . unwrapIrregularEnum
  toEnum     = WrapIrregularEnum . toIrregularEnum'
  enumFromTo = enumFromToUsingSucc

  succ (WrapIrregularEnum x) = WrapIrregularEnum $
      case irregularSucc x of
        Just y  -> y
        Nothing -> toIrregularEnum' . succ . fromIrregularEnum $ x

  pred           = error "'pred' not defined for irregular enums"
  enumFrom       = error "'enumFrom' not defined for irregular enums"
  enumFromThen   = error "'enumFromThen' not defined for irregular enums"
  enumFromThenTo = error "'enumFromThenTo' not defined for irregular enums"

enumFromToUsingSucc :: (Eq a, Enum a) => a -> a -> [a]
enumFromToUsingSucc x y =
    case break (== y) (iterate succ x) of
      (as, b:_) -> as ++ [b]
      _         -> error "impossible"

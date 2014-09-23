{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MagicHash      #-}
{-# LANGUAGE MultiParamTypeClasses, PolyKinds, QuasiQuotes, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies, TypeOperators                               #-}
module Messaging.Core ((:>)(..), cast, Object(..), withTypeableSymbol,
                       Selector(..), send, (#), (#.), (===), unsafeDownCast) where
import Data.Typeable.Internal (Proxy (..), Typeable (..), mkTyCon3, mkTyConApp)
import Foreign.ForeignPtr     (ForeignPtr)
import GHC.Base               (Proxy#)
import GHC.TypeLits           (Symbol)
import GHC.TypeLits           (symbolVal)
import GHC.TypeLits           (KnownSymbol)
import Unsafe.Coerce          (unsafeCoerce)

newtype Object (a :: k) = Object (ForeignPtr ())
                        deriving (Typeable, Show, Eq, Ord)

-- | @a ':>' b@ indicates that @a@ is a super-class of @b@.
class (a :: k) :> (b :: k2)

newtype Typing sym a = Typing (Typeable (sym :: Symbol) => a)

unsafeDownCast :: (a :> b) => Object a -> Object b
unsafeDownCast (Object a) = Object a

withTypeableSymbol :: forall sym a proxy. KnownSymbol sym
                   => proxy sym -> (Typeable sym => a) ->  a
withTypeableSymbol _ f = unsafeCoerce (Typing f :: Typing sym a) inst
  where
    inst pxy =
      let _ = pxy :: Proxy# sym
      in mkTyConApp (mkTyCon3 "base" "GHC.TypeLits" ('"': symbolVal (Proxy :: Proxy sym) ++ "\"")) []

instance (a :> b, c :> d) => (b -> c) :> (a -> d)
instance a :> a
instance (a :> b) => [a] :> [b]

class Selector cls msg | msg -> cls where
  data Message (msg :: k') :: *
  type Returns msg :: *
  send' :: Object cls -> Message msg -> Returns msg

cast :: (a :> b) => Object b -> Object a
cast (Object ptr) = Object ptr

send :: (a :> b, Selector a msg) => Object b -> Message msg -> Returns msg
send = send' . cast

infixl 4 #
(#) :: (a :> b, Selector a msg) => Object b -> Message msg -> Returns msg
(#) = send

infixl 4 #.
(#.) :: (a :> b, Selector a msg, Returns msg ~ IO c) => IO (Object b) -> Message msg -> IO c
recv #. sel = recv >>= flip send sel

infix 4 ===
(===) :: Object t -> Object t1 -> Bool
Object fptr === Object fptr' = fptr == fptr'

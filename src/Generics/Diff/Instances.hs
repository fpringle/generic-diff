{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Here we define orphan instances for as many @base@ types as we can. These fall into a few categories:

- Primitive types e.g. 'Char' and 'Int' - these typically don't have Generic instances, so we use 'eqDiff'
- Opaque types that don't expose constructors, so we couldn't write hand-rolled instances if we want to -
  all we have is an 'Eq' instance, so again we use 'eqDiff'.
- Compound types e.g. 'Maybe' and 'Either' - these have 'Generic' instances so we can just use 'gdiff'.
- List-like types such as @[a]@ and @'NE.NonEmpty' a@ - these we give slightly special treatment, since they're
  so ubiquitous and 'gdiff' would produce very hard-to-read output.
-}
module Generics.Diff.Instances where

import Control.Applicative
import Control.Concurrent
import Control.Exception
#if MIN_VERSION_base(4,17,0)
import Data.Array.Byte
#endif
import Data.Char
import Data.Complex
import Data.Data
import Data.Fixed
import qualified Data.Functor.Compose as F
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.IORef
import Data.Int
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as M
import Data.Ord
import Data.Ratio
import Data.SOP
import Data.STRef
import qualified Data.Semigroup as S
import qualified Data.Text as T
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Type.Coercion
import Generics.Diff.Special.List ()
#if MIN_VERSION_base(4,16,0)
import Data.Type.Ord
#endif
import Data.Unique
import Data.Version
import Data.Void
import Data.Word
import Numeric.Natural
#if MIN_VERSION_base(4,18,0)
import Foreign.C.ConstPtr
#endif
import Foreign.C.Error
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Arr
import GHC.Base
import GHC.ByteOrder
import GHC.Conc
import GHC.Event
import GHC.Fingerprint
import qualified GHC.Generics as G
import GHC.IO.Buffer
import GHC.IO.Device
import GHC.IOArray
#if MIN_VERSION_base(4,18,0)
import GHC.InfoProv
#endif
import GHC.StableName
import GHC.Stack
#if MIN_VERSION_base(4,17,0)
import GHC.Stack.CloneStack
#endif
import GHC.TypeLits
import Generics.Diff.Class
import Generics.Diff.Type
import Generics.SOP
import System.Exit
import System.IO
import System.IO.Error
#if MIN_VERSION_base(4,14,0)
import System.Timeout
#endif
import Text.Read (Lexeme)
import qualified Type.Reflection as TR

-- primitive(ish) types

{- FOURMOLU_DISABLE -}

instance Diff Void where diff = \case {}
instance Diff () where diff () () = Equal
instance Diff (Proxy a) where diff Proxy Proxy = Equal
instance Diff Bool where diff = eqDiff
instance Diff Ordering where diff = eqDiff
instance Diff Double where diff = eqDiff
instance Diff Float where diff = eqDiff
instance Diff Int where diff = eqDiff
instance Diff Int8 where diff = eqDiff
instance Diff Int16 where diff = eqDiff
instance Diff Int32 where diff = eqDiff
instance Diff Int64 where diff = eqDiff
instance Diff Word where diff = eqDiff
instance Diff Word8 where diff = eqDiff
instance Diff Word16 where diff = eqDiff
instance Diff Word32 where diff = eqDiff
instance Diff Word64 where diff = eqDiff
instance Diff Version where diff = eqDiff

instance Diff Char where
  diff = eqDiff
  diffList = eqDiff

instance (Eq a) => Diff (Ratio a) where diff = eqDiff
instance Diff Integer where diff = eqDiff
instance Diff ThreadId where diff = eqDiff
instance Diff (Chan a) where diff = eqDiff
instance Diff (MVar a) where diff = eqDiff
instance Diff (IORef a) where diff = eqDiff
instance Diff TypeRep where diff = eqDiff
instance Diff (TR.TypeRep a) where diff = eqDiff
instance Diff TyCon where diff = eqDiff
instance Diff (STRef s a) where diff = eqDiff
instance Diff Unique where diff = eqDiff
instance Diff (ForeignPtr a) where diff = eqDiff
instance Diff (Ptr a) where diff = eqDiff
instance Diff (FunPtr a) where diff = eqDiff
instance Diff IntPtr where diff = eqDiff
instance Diff WordPtr where diff = eqDiff
instance Diff (StablePtr a) where diff = eqDiff
instance Diff Handle where diff = eqDiff
instance Diff HandlePosn where diff = eqDiff
instance Diff (StableName a) where diff = eqDiff
instance Diff (TVar a) where diff = eqDiff
instance Diff Natural where diff = eqDiff
instance Diff Event where diff = eqDiff

instance Diff CChar where diff = eqDiff
instance Diff CSChar where diff = eqDiff
instance Diff CUChar where diff = eqDiff
instance Diff CShort where diff = eqDiff
instance Diff CUShort where diff = eqDiff
instance Diff CInt where diff = eqDiff
instance Diff CUInt where diff = eqDiff
instance Diff CLong where diff = eqDiff
instance Diff CULong where diff = eqDiff
instance Diff CPtrdiff where diff = eqDiff
instance Diff CSize where diff = eqDiff
instance Diff CWchar where diff = eqDiff
instance Diff CSigAtomic where diff = eqDiff
instance Diff CLLong where diff = eqDiff
instance Diff CULLong where diff = eqDiff
instance Diff CIntPtr where diff = eqDiff
instance Diff CUIntPtr where diff = eqDiff
instance Diff CIntMax where diff = eqDiff
instance Diff CUIntMax where diff = eqDiff
instance Diff CClock where diff = eqDiff
instance Diff CTime where diff = eqDiff
instance Diff CUSeconds where diff = eqDiff
instance Diff CSUSeconds where diff = eqDiff
instance Diff CFloat where diff = eqDiff
instance Diff CDouble where diff = eqDiff

instance Diff E0
instance Diff E1
instance Diff E2
instance Diff E3
instance Diff E6
instance Diff E9
instance Diff E12

instance Diff T.Text where diff = eqDiff
instance Diff TL.Text where diff = eqDiff
instance Diff TLB.Builder where diff = eqDiff
instance Diff UnicodeException where diff = eqDiff

instance Diff ArithException where diff = eqDiff
instance Diff ArrayException where diff = eqDiff
instance Diff Associativity where diff = eqDiff
instance Diff AsyncException where diff = eqDiff
instance Diff BlockReason where diff = eqDiff
instance Diff BufferMode where diff = eqDiff
instance Diff BufferState where diff = eqDiff
#if MIN_VERSION_base(4,17,0)
instance Diff ByteArray where diff = eqDiff
#endif
instance Diff ByteOrder where diff = eqDiff
instance Diff CBool where diff = eqDiff
instance Diff (Coercion a b) where diff = eqDiff
#if MIN_VERSION_base(4,18,0)
instance Diff (ConstPtr a) where diff = eqDiff
#endif
instance Diff DataRep where diff = eqDiff
instance Diff G.DecidedStrictness where diff = eqDiff
instance Diff Errno where diff = eqDiff
instance Diff ErrorCall where diff = eqDiff
instance Diff ExitCode where diff = eqDiff
instance Diff FdKey where diff = eqDiff
instance Diff Fingerprint where diff = eqDiff
instance Diff G.Fixity where diff = eqDiff
instance Diff GeneralCategory where diff = eqDiff
#if MIN_VERSION_base(4,18,0)
instance Diff InfoProv where diff = eqDiff
#endif
instance Diff (IOArray i e) where diff = eqDiff
instance Diff IODeviceType where diff = eqDiff
instance Diff IOErrorType where diff = eqDiff
instance Diff IOException where diff = eqDiff
instance Diff IOMode where diff = eqDiff
instance Diff Lexeme where diff = eqDiff
instance Diff Lifetime where diff = eqDiff
instance Diff MaskingState where diff = eqDiff
#if MIN_VERSION_base(4,17,0)
instance Diff (MutableByteArray a) where diff = eqDiff
#endif
instance Diff NewlineMode where diff = eqDiff
instance Diff Newline where diff = eqDiff
#if MIN_VERSION_base(4,16,0)
instance Diff (OrderingI a b) where diff = eqDiff
#endif
instance Diff SeekMode where diff = eqDiff
#if MIN_VERSION_base(4,16,0)
instance Diff SomeChar where diff = eqDiff
#endif
instance Diff SomeNat where diff = eqDiff
instance Diff SomeSymbol where diff = eqDiff
instance Diff SrcLoc where diff = eqDiff
#if MIN_VERSION_base(4,17,0)
instance Diff StackEntry where diff = eqDiff
#endif
instance Diff (STArray s i a) where diff = eqDiff
instance Diff ThreadStatus where diff = eqDiff
instance Diff TimeoutKey where diff = eqDiff
#if MIN_VERSION_base(4,14,0)
instance Diff Timeout where diff = eqDiff
#endif
instance Diff TrName where diff = eqDiff

{- FOURMOLU_ENABLE -}

-- list-like types

instance (Diff a) => Diff [a] where
  {-# SPECIALIZE instance Diff [Char] #-}
  diff = diffList

instance (Diff a) => Diff (NE.NonEmpty a) where
  diff = diffWithSpecial

-- combinators - typically we'll use gdiff

{- FOURMOLU_DISABLE -}

instance (Diff a) => Diff (Identity a)
instance (Diff a) => Diff (I a)
instance (Diff a) => Diff (Maybe a)
instance (Diff a, Diff b) => Diff (Either a b)
instance (Diff a) => Diff (Complex a)
instance (Diff a) => Diff (M.Dual a)
instance Diff M.All
instance Diff M.Any
instance (Diff a) => Diff (M.Sum a)
instance (Diff a) => Diff (M.Product a)
instance (Diff a) => Diff (M.First a)
instance (Diff a) => Diff (M.Last a)

instance
  (HasDatatypeInfo (M.Alt f a), All2 Diff (Code (M.Alt f a))) =>
  Diff (M.Alt f a)

instance
  (HasDatatypeInfo (M.Ap f a), All2 Diff (Code (M.Ap f a))) =>
  Diff (M.Ap f a)

instance (Diff a) => Diff (Down a)
instance (Diff a) => Diff (S.Min a)
instance (Diff a) => Diff (S.Max a)
instance (Diff a) => Diff (S.First a)
instance (Diff a) => Diff (S.Last a)
instance (Diff a) => Diff (S.WrappedMonoid a)
instance (Diff a, Diff b) => Diff (S.Arg a b)

#if !MIN_VERSION_base(4,16,0)
instance (Diff a) => Diff (S.Option a)
#endif

{- FOURMOLU_ENABLE -}

-- with phantom types

instance
  (HasDatatypeInfo (Const a b), All2 Diff (Code (Const a b))) =>
  Diff (Const a b)

instance
  (HasDatatypeInfo (K a b), All2 Diff (Code (K a b))) =>
  Diff (K a b)

instance
  (HasDatatypeInfo (Fixed a), All2 Diff (Code (Fixed a))) =>
  Diff (Fixed a)

instance
  (HasDatatypeInfo ((:.:) f g a), All2 Diff (Code ((:.:) f g a))) =>
  Diff ((:.:) f g a)

instance
  (HasDatatypeInfo (F.Compose f g a), All2 Diff (Code (F.Compose f g a))) =>
  Diff (F.Compose f g a)

instance
  (HasDatatypeInfo (Product f g a), All2 Diff (Code (Product f g a))) =>
  Diff (Product f g a)

instance
  (HasDatatypeInfo (Sum f g a), All2 Diff (Code (Sum f g a))) =>
  Diff (Sum f g a)

-- tuples

{- FOURMOLU_DISABLE -}

instance (Diff a, Diff b) => Diff (a, b)
instance (Diff a, Diff b, Diff c) => Diff (a, b, c)
instance (Diff a, Diff b, Diff c, Diff d) => Diff (a, b, c, d)
instance (Diff a, Diff b, Diff c, Diff d, Diff e) => Diff (a, b, c, d, e)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f) => Diff (a, b, c, d, e, f)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g) => Diff (a, b, c, d, e, f, g)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h) => Diff (a, b, c, d, e, f, g, h)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i) => Diff (a, b, c, d, e, f, g, h, i)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j) => Diff (a, b, c, d, e, f, g, h, i, j)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k) => Diff (a, b, c, d, e, f, g, h, i, j, k)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l) => Diff (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u, Diff v) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u, Diff v, Diff w) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u, Diff v, Diff w, Diff x) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u, Diff v, Diff w, Diff x, Diff y) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
instance (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f, Diff g, Diff h, Diff i, Diff j, Diff k, Diff l, Diff m, Diff n, Diff o, Diff p, Diff q, Diff r, Diff s, Diff t, Diff u, Diff v, Diff w, Diff x, Diff y, Diff z) => Diff (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
{- FOURMOLU_ENABLE -}

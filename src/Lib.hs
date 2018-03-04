{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Data.Array.Accelerate
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits
import GHC.Types (Type)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

-- $(singletons [d|
--   data Blah = Blah

--   data MyMaybe a = MyNothing | MyJust a

--   data MyTuple a b = MyTuple a b
--   |])

----------
-- Blah --
----------

-- data Blah = Blan_constr

-- data instance Sing (a :: Blah) where
--   SBlah :: Sing 'Blan_constr
--   -- a ~ Blan_constr => SBlah

-- type SBlah = (Sing :: Blah -> Type)

-- instance SingKind Blah where
--   type Demote Blah = Blah

--   fromSing :: Sing (a :: Blah) -> Blah
--   fromSing SBlah = Blan_constr

--   toSing :: Blah -> SomeSing Blah
--   toSing Blan_constr = SomeSing SBlah

-- instance SingI Blan_constr where
--   sing = SBlah

-- lalaBlah :: Blah -> IO ()
-- lalaBlah blah = withSomeSing blah $ \case
--   SBlah -> undefined


-------------
-- MyMaybe --
-------------

-- data MyMaybe a = MyNothing | MyJust a

-- type MyNothingSym = MyNothing

-- type MyJustSym1 (a :: k) = MyJust a

-- instance SuppressUnusedWarnings MyJustSym0 where
--   suppressUnusedWarnings _ =
--     Prelude.snd
--         ((ghc-prim-0.5.1.1:GHC.Tuple.(,) MyJustSym0KindInference)
--            ghc-prim-0.5.1.1:GHC.Tuple.())

-- data MyJustSym0 (u :: TyFun k (MyMaybe k))
--   = forall a. SameKind (Apply MyJustSym0 a) (MyJustSym1 a) => MyJustSym0KindInference

-- type instance Apply MyJustSym0 u = MyJust u

-- data instance Sing (z :: MyMaybe a) where
--   SMyNothing :: Sing 'MyNothing
--   SMyJust :: forall (n :: a). Sing (n :: a) -> Sing ('MyJust n)

-- type SMyMaybe = (Sing :: MyMaybe a -> Type)

-- instance SingKind a => SingKind (MyMaybe a) where
--   type Demote (MyMaybe a) = MyMaybe (Demote a)

--   fromSing :: Sing (x :: MyMaybe a) -> MyMaybe (Demote a)
--   fromSing SMyNothing = MyNothing
--   fromSing (SMyJust b) = MyJust (fromSing b)

--   toSing :: MyMaybe (Demote a) -> SomeSing (MyMaybe a)
--   toSing MyNothing = SomeSing SMyNothing
--   toSing (MyJust b)
--     = case toSing b :: SomeSing a of
--         SomeSing c -> SomeSing (SMyJust c)

-- instance SingI MyNothing where
--   sing = SMyNothing

-- instance SingI n => SingI (MyJust (n :: a)) where
--   sing = SMyJust sing

-- lalaMyMaybe :: SingKind a => MyMaybe (Demote a) -> IO ()
-- lalaMyMaybe myMaybe = withSomeSing myMaybe $ \case
--   SMyNothing -> undefined
--   SMyJust b -> undefined

-------------
-- MyTuple --
-------------

-- data MyTuple a b = MyTuple_constr a b

-- type MyTupleSym2 (a :: k1) (b :: k2) = MyTuple_constr a b

-- instance SuppressUnusedWarnings MyTupleSym1 where
--   suppressUnusedWarnings _
--     = Prelude.snd
--         ((ghc-prim-0.5.1.1:GHC.Tuple.(,) MyTupleSym1KindInference)
--            ghc-prim-0.5.1.1:GHC.Tuple.())

-- data MyTupleSym1 (l_aioe :: a6989586621679080475) (l_aiod :: TyFun b6989586621679080476 (MyTuple a6989586621679080475 b6989586621679080476))
--   = forall arg_aiof. SameKind (Apply (MyTupleSym1 l_aioe) arg_aiof) (MyTupleSym2 l_aioe arg_aiof) =>
--     MyTupleSym1KindInference
-- type instance Apply (MyTupleSym1 l_aioe) l_aiod = MyTuple_constr l_aioe l_aiod
-- instance SuppressUnusedWarnings MyTupleSym0 where
--   suppressUnusedWarnings _
--     = Prelude.snd
--         ((ghc-prim-0.5.1.1:GHC.Tuple.(,) MyTupleSym0KindInference)
--            ghc-prim-0.5.1.1:GHC.Tuple.())
-- data MyTupleSym0 (l_aiog :: TyFun a6989586621679080475 (TyFun b6989586621679080476 (MyTuple a6989586621679080475 b6989586621679080476)
--                                                         -> ghc-prim-0.5.1.1:GHC.Types.Type))
--   = forall arg_aioh. SameKind (Apply MyTupleSym0 arg_aioh) (MyTupleSym1 arg_aioh) =>
--     MyTupleSym0KindInference
-- type instance Apply MyTupleSym0 l_aiog = MyTupleSym1 l_aiog


-- data instance Sing (z_aioo :: MyTuple a_aio3 b_aio4)
--   = forall (n_aiop :: a_aio3)
--            (n_aioq :: b_aio4). z_aioo ~ MyTuple_constr n_aiop n_aioq =>
--     SMyTuple (Sing (n_aiop :: a_aio3)) (Sing (n_aioq :: b_aio4))

-- type SMyTuple =
--     (Sing :: MyTuple a_aio3 b_aio4
--              -> ghc-prim-0.5.1.1:GHC.Types.Type)

-- instance (SingKind a_aio3, SingKind b_aio4) =>
--          SingKind (MyTuple a_aio3 b_aio4) where
--   type Demote (MyTuple a_aio3 b_aio4) = MyTuple (Demote a_aio3) (Demote b_aio4)
--   fromSing (SMyTuple b_aior b_aios)
--     = (MyTuple_constr (fromSing b_aior)) (fromSing b_aios)
--   toSing (MyTuple_constr b_aiot b_aiou)
--     = case
--           (ghc-prim-0.5.1.1:GHC.Tuple.(,) (toSing b_aiot :: SomeSing a_aio3))
--             (toSing b_aiou :: SomeSing b_aio4)
--       of {
--         ghc-prim-0.5.1.1:GHC.Tuple.(,) (SomeSing c_aiov) (SomeSing c_aiow)
--           -> SomeSing ((SMyTuple c_aiov) c_aiow) }

-- instance (SingI n_aiop, SingI n_aioq) =>
--          SingI (MyTuple_constr (n_aiop :: a_aio3) (n_aioq :: b_aio4)) where
--   sing = (SMyTuple sing) sing

-- data instance Sing (a :: Z) where
--   SZ :: Sing 'Z

-- deriving instance Show (Sing (a :: Z))

-- instance SingI ('Z :: Z) where
--   sing :: Sing 'Z
--   sing = SZ

-- instance SingKind Z where
--   type Demote Z = Z

--   fromSing :: Sing (a :: Z) -> Z
--   fromSing SZ = Z

--   toSing :: Z -> SomeSing Z
--   toSing Z = SomeSing SZ

-- data instance Sing (a :: (head :. tail)) where
--   SLOL :: forall head tail. Sing head -> Sing tail -> Sing (head ':. tail)

-- instance SingI (':.) :: (head :. tail)) where
--   sing = undefined

data MyNat where
  Zero :: MyNat
  Succ :: MyNat -> MyNat

$(genSingletons [''Z, ''(Data.Array.Accelerate.:.), ''MyNat])

-- type family ShapeOfType (n :: Nat) where
--   ShapeOfType 0 = Z
--   ShapeOfType n = ShapeOfType (n :- 1) :. Int

-- class ShapeOf (n :: Nat) where
--   shapeOf :: Proxy n -> ShapeOfType n

-- instance {-# OVERLAPPING #-} ShapeOf 0 where
--   shapeOf :: Proxy 0 -> Z
--   shapeOf _ = Z

-- instance {-# OVERLAPPABLE #-} ShapeOf n where
--   shapeOf :: forall n. Proxy n -> ShapeOfType n
--   shapeOf _ = unsafeCoerce (shapeOf (Proxy :: Proxy (n :- 1)) :. 1)

-- shapeOf :: forall n. Proxy n -> ShapeOfType n
-- shapeOf _ = case sing :: Sing n of
--               SNat -> unsafeCoerce (shapeOf (Proxy :: Proxy (n :- 1)) :. 1)

-- natToShape :: forall n. (KnownNat (n :- 1), KnownNat n) => SNat n -> ShapeOfType n
-- natToShape snat =
--   let nat = natVal snat
--   in
--   if nat Prelude.== 0
--     then unsafeCoerce Z
--     else unsafeCoerce (natToShape (SNat :: SNat (n :- 1)) :. 1)

type family ShapeOfType (n :: MyNat) where
  ShapeOfType Zero = Z
  ShapeOfType (Succ n) = ShapeOfType n :. Int

natToShape :: forall n. SMyNat n -> ShapeOfType n
natToShape SZero = Z
natToShape (SSucc n) = natToShape n :. 1

type family NatToSMyNat (n :: Nat) where
  NatToSMyNat 0 = 'Zero
  NatToSMyNat n = 'Succ (NatToSMyNat (n :- 1))

snatToMyNat :: KnownNat n => proxy n -> SMyNat (NatToSMyNat n)
snatToMyNat snat =
  let n = natVal snat
  in case n of
    0 -> unsafeCoerce SZero
    n ->
      case someNatVal (n - 1) of
        Nothing -> error "error"
        Just (SomeNat (Proxy :: Proxy m)) ->
          unsafeCoerce (SSucc (snatToMyNat (Proxy :: Proxy m)))

papapa :: Integer -> IO ()
papapa int =
  withSomeSing int $ \case
    snat@SNat -> natToShape (snatToMyNat snat)


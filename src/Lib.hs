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
import Data.Singletons.Prelude
import Data.Singletons.TH
import GHC.Types (Type)

-- $(singletons [d|
--   data Blah = Blah

--   data MyMaybe a = MyNothing | MyJust a

--   data MyTuple a b = MyTuple a b
--   |])

----------
-- Blah --
----------

data Blah = Blan_constr

type BlahSym = Blan_constr

data instance Sing (a :: Blah) =
  a ~ Blan_constr => SBlah

type SBlah = (Sing :: Blah -> Type)

instance SingKind Blah where
  type Demote Blah = Blah

  fromSing SBlah = Blan_constr

  toSing Blan_constr = SomeSing SBlah

instance SingI Blan_constr where
  sing = SBlah

-------------
-- MyMaybe --
-------------

data MyMaybe a = MyNothing | MyJust a

type MyNothingSym = MyNothing

type MyJustSym1 (a :: k) = MyJust a

-- instance SuppressUnusedWarnings MyJustSym0 where
--   suppressUnusedWarnings _ =
--     Prelude.snd
--         ((ghc-prim-0.5.1.1:GHC.Tuple.(,) MyJustSym0KindInference)
--            ghc-prim-0.5.1.1:GHC.Tuple.())

data MyJustSym0 (u :: TyFun k (MyMaybe k))
  = forall a. SameKind (Apply MyJustSym0 a) (MyJustSym1 a) => MyJustSym0KindInference

type instance Apply MyJustSym0 u = MyJust u

data instance Sing (z :: MyMaybe a)
  = z ~ MyNothing => SMyNothing
  | forall (n :: a). z ~ MyJust n => SMyJust (Sing (n :: a))

type SMyMaybe = (Sing :: MyMaybe a -> Type)

instance SingKind a => SingKind (MyMaybe a) where
  type Demote (MyMaybe a) = MyMaybe (Demote a)

  fromSing SMyNothing = MyNothing
  fromSing (SMyJust b) = MyJust (fromSing b)

  toSing MyNothing = SomeSing SMyNothing
  toSing (MyJust b)
    = case toSing b :: SomeSing a of
        SomeSing c -> SomeSing (SMyJust c)

instance SingI MyNothing where
  sing = SMyNothing

instance SingI n => SingI (MyJust (n :: a)) where
  sing = SMyJust sing

-------------
-- MyTuple --
-------------

data MyTuple a b = MyTuple_constr a b

type MyTupleSym2 (a :: k1) (b :: k2) = MyTuple_constr a b

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




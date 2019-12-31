{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler.ABT where

import Data.Functor.Classes (Show1 (..), showsPrec1)
import qualified Data.Set as Set
import Relude ((++), (<>), (==))
import qualified Relude as R
import Text.Show.Deriving (deriveShow1)
import qualified Prelude (show)

type V = R.String

data ABT f a
  = Var V
  | Abs V a
  | Tm (f a)
  deriving (R.Functor, R.Foldable, R.Show)

$(deriveShow1 ''ABT)

data Term f a
  = Term
      { _termFreevars :: Set.Set V,
        _termAnnotation :: a,
        _termOut :: ABT f (Term f a)
        }

$(deriveShow1 ''Term)

instance (Show1 f, R.Functor f, R.Show a) => R.Show (Term f a) where
  show (Term _ a x) = R.show a <> "@" <> showsPrec1 0 x ""

annotatedVar :: a -> V -> Term f a
annotatedVar a v = Term (Set.singleton v) a (Var v)

var :: V -> Term f ()
var = annotatedVar ()

abs :: V -> Term f () -> Term f ()
abs = abs' ()

abs' :: a -> V -> Term f a -> Term f a
abs' a v body = Term (Set.delete v (_termFreevars body)) a (Abs v body)

tm' :: (R.Foldable f, R.Functor f) => a -> f (Term f a) -> Term f a
tm' a t =
  Term (Set.unions (R.toList (R.fmap _termFreevars t)))
    a
    (Tm t)

tm :: (R.Foldable f, R.Functor f) => f (Term f ()) -> Term f ()
tm = tm' ()

into' :: (R.Foldable f, R.Functor f) => a -> ABT f (Term f a) -> Term f a
into' a abt = case abt of
  Var x -> annotatedVar a x
  Abs v r -> abs' a v r
  Tm t -> tm' a t

into :: (R.Foldable f, R.Functor f) => ABT f (Term f ()) -> Term f ()
into = into' ()

fresh :: (V -> R.Bool) -> V -> V
fresh used v | used v = fresh used ("'" ++ v)
fresh _ v = v

fresh' :: R.Set V -> V -> V
fresh' vs = fresh (`Set.member` vs)

rename :: (R.Foldable f, R.Functor f) => V -> V -> Term f a -> Term f a
rename old new t0@(Term _fvs a t) = case t of
  Var v -> if v == old then annotatedVar a new else t0
  Abs v body ->
    if v == old
      then abs' a v body
      else abs' a v (rename old new body)
  Tm v -> tm' a (R.fmap (rename old new) v)

-- Strategies (and skeletons) in the Par monad
--
-- Author: Patrick Maier
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}  -- for type annotations in Static decl

module Control.Parallel.HdpH.Strategies
  ( -- * Strategy type
    Strategy,
    using,

    -- * Basic sequential strategies
    r0,
    rseq,
    rdeepseq,

    -- * Proto-strategies for generating parallelism
    ProtoStrategy,
    sparkClosure,

    forceCC,

    -- * Strategies for lists
    evalList,
    parClosureList
  ) where

import Prelude
import Control.DeepSeq (NFData, deepseq)
import Control.Monad (zipWithM, zipWithM_)
import Data.Functor ((<$>))
import Data.List (transpose)
import Data.Monoid (mconcat)
import System.Random (randomRIO)

import Control.Parallel.HdpH
       (Par, io, fork, pushTo, {- spark, -} new, get, glob, rput, equiDist,
        Node, IVar, GIVar)
import qualified Control.Parallel.HdpH as HdpH (spark)
import Control.Parallel.HdpH.Dist (Dist, one, div2)

import Control.Parallel.HdpH.SerialUtil
import GHC.Packing.Core
import GHC.Packing.Type


-- auxiliary definition to get 'Strategies' to compiles
spark :: Serialized (Par ()) -> Par ()
spark = HdpH.spark one


-----------------------------------------------------------------------------
-- Strategy type

-- | A @'Strategy'@ for type @a@ is a (semantic) identity in the @'Par'@ monad.
-- For an elaboration of this concept (in the context of the @Eval@ monad)
-- see the paper:
--   Marlow et al.
--   /Seq no more: Better Strategies for parallel Haskell./
--   Haskell 2010.
type Strategy a = a -> Par a

-- | Strategy application is actual application (in the @'Par'@ monad).
using :: a -> Strategy a -> Par a
using = flip ($)


-----------------------------------------------------------------------------
-- Basic sequential strategies (polymorphic);
-- these are exactly as in the "Seq no more" paper.

-- | /Do Nothing/ strategy.
r0 :: Strategy a
r0 = return

-- | /Evaluate head-strict/ strategy; probably not very useful in HdpH.
rseq :: Strategy a
rseq x = x `seq` return x -- Order of eval irrelevant due to 2nd arg converging

-- | /Evaluate fully/ strategy.
rdeepseq :: (NFData a) => Strategy a
rdeepseq x = x `deepseq` return x  -- Order of eval irrelevant (2nd arg conv)


-----------------------------------------------------------------------------
-- proto-strategies for generating parallelism

-- | A @'ProtoStrategy'@ is almost a @'Strategy'@.
-- More precisely, a @'ProtoStrategy'@ for type @a@ is a /delayed/ (semantic)
-- identity function in the @'Par'@ monad, ie. it returns an @'IVar'@ (rather
-- than a term) of type @a@.
type ProtoStrategy a = a -> Par (IVar a)


-- | @sparkClosure clo_strat@ is a @'ProtoStrategy'@ that sparks a @'Closure'@;
-- evaluation of the sparked @'Closure'@ is governed by the strategy
-- @'unClosure' clo_strat@.
sparkClosure :: Serialized (Strategy (Serialized a)) ->
                  ProtoStrategy (Serialized a)
sparkClosure clo_strat clo = do
  v <- new
  gv <- glob v
  spark $ serial $ sparkClosure_abs (clo, clo_strat, gv)
  return v

sparkClosure_abs :: (Serialized a,
                     Serialized (Strategy (Serialized a)),
                     GIVar (Serialized a))
                 -> Par ()
sparkClosure_abs (clo, clo_strat, gv) =
  (clo `using` deserial clo_strat) >>= rput gv


forceCC :: (NFData a) => Serialized (Strategy (Serialized a))
forceCC = serial $ fn
            where fn serialized = x `deepseq` return (serial x)
                    where x = deserial serialized

------------------------------------------------------------------------------
-- strategies for lists

-- 'evalList' is a (type-restricted) monadic map; should be suitably
-- generalisable for all data structures that support mapping over
-- | Evaluate each element of a list according to the given strategy.
evalList :: Strategy a -> Strategy [a]
evalList _strat []     = return []
evalList  strat (x:xs) = do x' <- strat x
                            xs' <- evalList strat xs
                            return (x':xs')

-- | Evaluate each element of a list of Closures in parallel according to
-- the given strategy (wrapped in a Closure). Work is distributed by
-- lazy work stealing.
parClosureList :: Serialized (Strategy (Serialized a)) -> Strategy [Serialized a]
parClosureList clo_strat xs = mapM (sparkClosure clo_strat) xs >>=
                              mapM get
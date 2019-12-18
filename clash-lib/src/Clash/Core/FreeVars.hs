{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Free variable calculations
-}

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}

module Clash.Core.FreeVars
  (-- * Free variable calculation
    typeFreeVars
  , freeIds
  , freeLocalVars
  , freeLocalIds
  , globalIds
  , termFreeTyVars
  , tyFVsOfTypes
  , localFVsOfTerms
  , hasLocalFreeVars
  -- * Fast
  , noFreeVarsOfType
  -- * occurrence check
  , localIdOccursIn
  , globalIdOccursIn
  , localIdDoesNotOccurIn
  , localIdsDoNotOccurIn
  , localVarsDoNotOccurIn
  -- * Internal
  , typeFreeVars'
  , termFreeVars'
  )
where

import qualified Control.Lens           as Lens
import Control.Lens.Fold                (Fold)
import Control.Lens.Getter              (Contravariant)
import Data.Coerce
import qualified Data.IntSet            as IntSet
import Data.Monoid                      (All (..), Any (..))

import Clash.Core.Term                  (Pat (..), Term (..), TickInfo (..))
import Clash.Core.Type                  (Type (..))
import Clash.Core.Var
  (Id, IdScope (..), TyVar, Var (..), isLocalId)
import Clash.Core.VarEnv                (VarSet, unitVarSet)

-- | Gives the free type-variables in a Type, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of its variables, so:
--
-- @
-- foldMapOf typeFreeVars unitVarSet ((a:* -> k) Int) = {a, k}
-- @
typeFreeVars :: Fold Type TyVar
typeFreeVars = typeFreeVars' (const True) IntSet.empty

-- | Gives the "interesting" free variables in a Type, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf (typeFreeVars' (const True) IntSet.empty) unitVarSet ((a:* -> k) Int) = {a, k}
-- @
--
-- Note [Closing over kind variables]
--
-- Consider the type
--
-- > forall k . b -> k
--
-- where
--
-- > b :: k -> Type
--
-- When we close over the free variables of @forall k . b -> k@, i.e. @b@, then
-- the @k@ in @b :: k -> Type@ is most definitely /not/ the @k@ in
-- @forall k . b -> k@. So when a type variable is free, i.e. not in the inScope
-- set, its kind variables also aren´t; so in order to prevent collisions due to
-- shadowing we close using an empty inScope set.
--
-- See also: https://git.haskell.org/ghc.git/commitdiff/503514b94f8dc7bd9eab5392206649aee45f140b
typeFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> IntSet.IntSet
  -- ^ Uniques of the variables in scope, used by 'termFreeVars''
  -> (Var a -> f (Var a))
  -> Type
  -> f Type
typeFreeVars' interesting is f = go is where
  go inScope = \case
    VarTy tv -> tv1 <* go inScope1 (varType tv)
      where
        isInteresting = interesting tv
        tvInScope     = varUniq tv `IntSet.member` inScope
        inScope1
          | tvInScope = inScope
          | otherwise = IntSet.empty -- See Note [Closing over type variables]

        tv1 | isInteresting
            , not tvInScope
            = VarTy . coerce <$> f (coerce tv)
            | otherwise
            = pure (VarTy tv)

    ForAllTy tv ty -> ForAllTy <$> goBndr inScope tv
                               <*> go (IntSet.insert (varUniq tv) inScope) ty
    AppTy l r -> AppTy <$> go inScope l <*> go inScope r
    ty -> pure ty

  goBndr inScope tv = (\t -> tv {varType = t}) <$> go inScope (varType tv)

-- | Check whether an identifier does not occur free in a term
localIdDoesNotOccurIn
  :: Id
  -> Term
  -> Bool
localIdDoesNotOccurIn v e = getAll (Lens.foldMapOf freeLocalIds (All . (/= v)) e)

-- | Check whether a set of identifiers does not occur free in a term
localIdsDoNotOccurIn
  :: [Id]
  -> Term
  -> Bool
localIdsDoNotOccurIn vs e =
  getAll (Lens.foldMapOf freeLocalIds (All . (`notElem` vs)) e)

-- | Check whether a set of variables does not occur free in a term
localVarsDoNotOccurIn
  :: [Var a]
  -> Term
  -> Bool
localVarsDoNotOccurIn vs e =
  getAll (Lens.foldMapOf freeLocalVars (All . (`notElem` vs)) e)

-- | Check whether a local identifier occurs free in a term
localIdOccursIn
  :: Id
  -> Term
  -> Bool
localIdOccursIn v e = getAny (Lens.foldMapOf freeLocalIds (Any . (== v)) e)

-- | Check whether a local identifier occurs free in a term
globalIdOccursIn
  :: Id
  -> Term
  -> Bool
globalIdOccursIn v e = getAny (Lens.foldMapOf globalIds (Any . (== v)) e)

-- | Calculate the /local/ free variable of an expression: the free type
-- variables and the free identifiers that are not bound in the global
-- environment.
freeLocalVars :: Fold Term (Var a)
freeLocalVars = termFreeVars' isLocalVar where
  isLocalVar (Id {idScope = GlobalId}) = False
  isLocalVar _ = True

-- | Gives the free identifiers of a Term, implemented as a 'Fold'
freeIds :: Fold Term Id
freeIds = termFreeVars' isId where
  isId (Id {}) = True
  isId _       = False

-- | Calculate the /local/ free identifiers of an expression: the free
-- identifiers that are not bound in the global environment.
freeLocalIds :: Fold Term Id
freeLocalIds = termFreeVars' isLocalId

-- | Calculate the /global/ free identifiers of an expression: the free
-- identifiers that are bound in the global environment.
globalIds :: Fold Term Id
globalIds = termFreeVars' isGlobalId where
  isGlobalId (Id {idScope = GlobalId}) = True
  isGlobalId _ = False

-- | Determines if term has any locally free variables. That is, the free type
-- variables and the free identifiers that are not bound in the global
-- scope.
hasLocalFreeVars :: Term -> Bool
hasLocalFreeVars = Lens.notNullOf freeLocalVars

-- | Gives the free type-variables of a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf termFreeTyVars unitVarSet (case (x : (a:* -> k) Int)) of {}) = {a, k}
-- @
termFreeTyVars :: Fold Term TyVar
termFreeTyVars = termFreeVars' isTV where
  isTV (TyVar {}) = True
  isTV _          = False

-- | Gives the "interesting" free variables in a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf (termFreeVars' (const True)) unitVarSet (case (x : (a:* -> k) Int)) of {}) = {x, a, k}
-- @
--
-- Note [Closing over type variables]
--
-- Consider the term
--
-- > /\(k :: Type) -> \(b :: k) -> a
--
-- where
--
-- > a :: k
--
-- When we close over the free variables of @/\k -> \(b :: k) -> (a :: k)@, i.e.
-- @a@, then the @k@ in @a :: k@ is most definitely /not/ the @k@ in introduced
-- by the @/\k ->@. So when a term variable is free, i.e. not in the inScope
-- set, its type variables also aren´t; so in order to prevent collisions due to
-- shadowing we close using an empty inScope set.
--
-- See also: https://git.haskell.org/ghc.git/commitdiff/503514b94f8dc7bd9eab5392206649aee45f140b
termFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> (Var a -> f (Var a))
  -> Term
  -> f Term
termFreeVars' interesting f = go IntSet.empty where
  go inLocalScope = \case
    Var v -> v1 <* typeFreeVars' interesting inLocalScope1 f (varType v)
      where
        isInteresting = interesting v
        vInScope      = isLocalId v && varUniq v `IntSet.member` inLocalScope
        inLocalScope1
          | vInScope  = inLocalScope
          | otherwise = IntSet.empty -- See Note [Closing over type variables]

        v1 | isInteresting
           , not vInScope
           = Var . coerce <$> f (coerce v)
           | otherwise
           = pure (Var v)

    Lam id_ tm ->
      Lam <$> goBndr inLocalScope id_
          <*> go (IntSet.insert (varUniq id_) inLocalScope) tm
    TyLam tv tm ->
      TyLam <$> goBndr inLocalScope tv
            <*> go (IntSet.insert (varUniq tv) inLocalScope) tm

    App l r ->
      App <$> go inLocalScope l <*> go inLocalScope r

    TyApp l r ->
      TyApp <$> go inLocalScope l
            <*> typeFreeVars' interesting inLocalScope f r

    Letrec bs e ->
      Letrec <$> traverse (goBind inLocalScope') bs
             <*> go inLocalScope' e
      where
        inLocalScope' = foldr IntSet.insert inLocalScope (map (varUniq.fst) bs)

    Case subj ty alts ->
      Case <$> go inLocalScope subj
           <*> typeFreeVars' interesting inLocalScope f ty
           <*> traverse (goAlt inLocalScope) alts

    Cast tm t1 t2 ->
      Cast <$> go inLocalScope tm
           <*> typeFreeVars' interesting inLocalScope f t1
           <*> typeFreeVars' interesting inLocalScope f t2

    Tick tick tm ->
      Tick <$> goTick inLocalScope tick
      <*> go inLocalScope tm

    tm -> pure tm

  goBndr inLocalScope v =
    (\t -> v  {varType = t}) <$> typeFreeVars' interesting inLocalScope f (varType v)

  goBind inLocalScope (l,r) = (,) <$> goBndr inLocalScope l <*> go inLocalScope r

  goAlt inLocalScope (pat,alt) = case pat of
    DataPat dc tvs ids -> (,) <$> (DataPat <$> pure dc
                                           <*> traverse (goBndr inLocalScope') tvs
                                           <*> traverse (goBndr inLocalScope') ids)
                              <*> go inLocalScope' alt
      where
        inLocalScope' = foldr IntSet.insert
                         (foldr IntSet.insert inLocalScope (map varUniq tvs))
                         (map varUniq ids)
    _ -> (,) <$> pure pat <*> go inLocalScope alt

  goTick inLocalScope = \case
    NameMod m ty -> NameMod m <$> typeFreeVars' interesting inLocalScope f ty
    tick         -> pure tick

-- | Determine whether a type has no free type variables.
noFreeVarsOfType
  :: Type
  -> Bool
noFreeVarsOfType ty = case ty of
  VarTy {}    -> False
  ForAllTy {} -> getAll (Lens.foldMapOf typeFreeVars (const (All False)) ty)
  AppTy l r   -> noFreeVarsOfType l && noFreeVarsOfType r
  _           -> True

-- | Collect the free variables of a collection of type into a set
tyFVsOfTypes
  :: Foldable f
  => f Type
  -> VarSet
tyFVsOfTypes = foldMap go
 where
  go = Lens.foldMapOf typeFreeVars unitVarSet

-- | Collect the free variables of a collection of terms into a set
localFVsOfTerms
  :: Foldable f
  => f Term
  -> VarSet
localFVsOfTerms = foldMap go
 where
  go = Lens.foldMapOf freeLocalVars unitVarSet

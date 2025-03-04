{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeFoilTypecheck.SystemF.Syntax.Pattern where

import qualified Control.Monad.Foil as Foil
import Data.Map (Map)
import qualified Data.Map as Map
import qualified FreeFoilTypecheck.SystemF.Parser.Abs as Raw

-- ** Scope-safe patterns

-- mkFoilPattern ''Raw.Ident ''Raw.Pattern
data FoilPattern ty (o :: Foil.S) (i :: Foil.S) where
  FoilPatternVar :: Foil.NameBinder o i -> FoilPattern ty o i
  FoilPatternAsc :: Foil.NameBinder o i -> ty o -> FoilPattern ty o i

-- deriveCoSinkable ''Raw.Ident ''Raw.Pattern
instance (Foil.Sinkable ty) => Foil.CoSinkable (FoilPattern ty) where
  -- coSinkabilityProof ::
  --      (Name n -> Name n')
  --   -> pattern n l
  --   -> ((forall (l' :: S). (Name l -> Name l') -> pattern n' l' -> r)	-> r)
  --      (                   ___________________ -> _____________ -> r)  -> r)
  --   -> (l' :: Foil.S, rename' :: Name l -> Name l', pattern' :: pattern n' l')
  coSinkabilityProof rename pat cont =
    case pat of
      FoilPatternVar binder ->
        Foil.coSinkabilityProof rename binder $ \rename' binder' ->
          let pat' = FoilPatternVar binder'
           in cont rename' pat'
      FoilPatternAsc binder type_ ->
        Foil.coSinkabilityProof rename binder $ \rename' binder' ->
          let type_' = Foil.sinkabilityProof rename type_
              pat' = FoilPatternAsc binder' type_'
           in cont rename' pat'
  
  withPattern withBinder _unit _comp scope pat cont =
    case pat of
      FoilPatternVar binder -> withBinder scope binder $ \result binder' ->
        cont result (FoilPatternVar binder')
      FoilPatternAsc binder type_ -> withBinder scope binder $ \result binder' ->
        let rename = undefined  -- FIXME
         in cont result (FoilPatternAsc binder' (rename type_))

-- mkToFoilPattern ''Raw.Ident ''Raw.Pattern
toFoilPattern ::
  (Foil.Distinct n) =>
  (Foil.Scope n -> Map Raw.Ident (Foil.Name n) -> Raw.Term -> ty n) -> -- toType
  Foil.Scope n ->
  Map Raw.Ident (Foil.Name n) ->
  Raw.Pattern ->
  (forall l. (Foil.DExt n l) => FoilPattern ty n l -> Map Raw.Ident (Foil.Name l) -> r) ->
  r
toFoilPattern toType scope env pat cont =
  case pat of
    Raw.PatternVar x ->
      Foil.withFresh scope $ \binder ->
        let pat' = FoilPatternVar binder
            env' = Map.insert x (Foil.nameOf binder) (Foil.sink <$> env)
         in cont pat' env'
    Raw.PatternAsc x rawType ->
      Foil.withFresh scope $ \binder ->
        let type_' = toType scope env rawType
            pat' = FoilPatternAsc binder type_'
            env' = Map.insert x (Foil.nameOf binder) (Foil.sink <$> env)
         in cont pat' env'

-- mkFromFoilPattern ''Raw.Ident ''Raw.Pattern
fromFoilPattern ::
  ((Int -> Raw.Ident) -> ty n -> Raw.Term) -> -- fromType
  (Int -> Raw.Ident) -> -- default naming
  FoilPattern ty n l -> -- type-safe pattern
  Raw.Pattern
fromFoilPattern fromType mkIdent pat =
  case pat of
    FoilPatternVar x -> Raw.PatternVar (mkIdent (Foil.nameId (Foil.nameOf x)))
    FoilPatternAsc x type_ ->
      let x' = mkIdent (Foil.nameId (Foil.nameOf x))
          type_' = fromType mkIdent type_
       in Raw.PatternAsc x' type_'

-- deriveUnifiablePattern ''Raw.Ident ''Raw.Pattern
instance (Foil.Sinkable ty) => Foil.UnifiablePattern (FoilPattern ty) where
  unifyPatterns (FoilPatternVar x) (FoilPatternVar y) = Foil.unifyNameBinders x y
  unifyPatterns (FoilPatternAsc x _) (FoilPatternAsc y _) = Foil.unifyNameBinders x y
  unifyPatterns (FoilPatternAsc x _) (FoilPatternVar y) = Foil.unifyNameBinders x y
  unifyPatterns (FoilPatternVar x) (FoilPatternAsc y _) = Foil.unifyNameBinders x y
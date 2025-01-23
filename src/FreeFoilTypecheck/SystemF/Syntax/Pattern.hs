{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module FreeFoilTypecheck.SystemF.Syntax.Pattern where


import qualified FreeFoilTypecheck.SystemF.Parser.Abs as Raw
import           Control.Monad.Foil.TH
import           Control.Monad.Free.Foil.TH


-- ** Scope-safe patterns

mkFoilPattern ''Raw.Ident ''Raw.Pattern
deriveCoSinkable ''Raw.Ident ''Raw.Pattern
mkToFoilPattern ''Raw.Ident ''Raw.Pattern
mkFromFoilPattern ''Raw.Ident ''Raw.Pattern
deriveUnifiablePattern ''Raw.Ident ''Raw.Pattern

mkGetPatternBinder ''Raw.Ident ''Raw.Pattern

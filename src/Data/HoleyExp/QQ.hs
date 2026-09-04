{-|
Module      : QQ
Description : Quasi-Quoter for Holey Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

TODO
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeAbstractions             #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
module Data.HoleyExp.QQ where

import GHC.Natural                        (Natural)
import Data.Text                          qualified as DT
import Data.Text                          (Text)
import Language.Haskell.TH.Quote          (QuasiQuoter(..))
import Language.Haskell.TH                qualified as TH
import Language.Haskell.TH                (Q
                                          ,Exp
                                          ,Name)
import Data.HoleyExp.HExpInternal         (Hole
                                          ,HoleProps
                                          ,HoleFilling(..)
                                          ,HExp(..)
                                          ,Proxy(..)
                                          ,pattern EmptyHole
                                          ,pattern FilledHole
                                          ,pattern UndefHole)
import Data.HoleyExp.Text                 (parseHExp)

class ToQExp a where
    toQExp :: a -> Q Exp

class (ToQExp text,ToQExp filling,HoleFilling text filling) => HExpQExp text filling

instance ToQExp Int where
    toQExp :: Int -> Q Exp
    toQExp = TH.litE . TH.IntegerL . toInteger

instance ToQExp Double where
    toQExp :: Double -> Q Exp
    toQExp = TH.litE . TH.RationalL . toRational

instance ToQExp Text where
    toQExp :: Text -> Q Exp
    toQExp = TH.litE . TH.StringL . DT.unpack

instance ToQExp String where
    toQExp :: String -> Q Exp
    toQExp = TH.litE . TH.StringL

instance HExpQExp Text   Int
instance HExpQExp Text   Double
instance HExpQExp Text   String
instance HExpQExp String Text

{-
hole2QExp :: (HExpQExp text filling) => Proxy text (Hole filling) -> Q Exp
hole2QExp (Proxy (EmptyHole  i   _)) = appCombinator1 (TH.mkName "hole") (mkNaturalLit i)
hole2QExp (Proxy (FilledHole i f _)) = 
    appCombinator2 (TH.mkName "filled") (mkNaturalLit i) $ toQExp f
hole2QExp (Proxy (UndefHole i _)) = error $ "QQ error: hole index "
                                  <> (show i)
                                  <> " present in internal hExp, but not defined in the templates hole properties."

iHExp2QExp :: (HExpQExp text filling) 
               => ITemplate text 
               -> HoleProps filling 
               -> Q Exp
iHExp2QExp (IChunk chk) _ = do
    let chunk = TH.mkName "chunk"
    appCombinator1 chunk $ toQExp chk
iHExp2QExp @text @filling (ICompose p h r) hlsProps = do
    -- ICompose p h r = (chunk p) +> (hole h) +> r
    let pExp      = iHExp2QExp (IChunk p) hlsProps
    let hExp      = hole2QExp (Proxy @text @(Hole filling) (h, hlsProps))
    let rExp      = iHExp2QExp r hlsProps
    let compose   = appInfixCombinator (TH.mkName "+>")
    (pExp `compose` hExp) `compose` rExp

appInfixCombinator :: TH.Quote m 
                   => Name  -- ^ Name of the combinator
                   -> m Exp -- ^ First argument expression
                   -> m Exp -- ^ Second argument expression
                   -> m Exp 
appInfixCombinator constName e1 e2 = TH.infixE (Just e1) (TH.varE constName) (Just e2)

-- | Convert a type that can be converted into a hExp into a Template
-- Haskell expression. Use this to create new quasi-quoters for types that
-- convert to hExp.
template2QExp :: (HExpQExp text filling) => Template text filling -> Q Exp
template2QExp (Template it hls) = iHExp2QExp it hls

-- * Helpful Template Haskell combinators.

-- | Apply a combinator to a single argument.
appCombinator1 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ Argument expression
               -> m Exp 
appCombinator1 constName = TH.appE (TH.varE constName) 

-- | Apply a combinator to two arguments.
appCombinator2 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp 
appCombinator2 constName a1 a2 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 

-- | Apply a combinator to three arguments.
appCombinator3 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp -- ^ Third argument expression
               -> m Exp 
appCombinator3 constName a1 a2 a3 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 `TH.appE` a3

-- | Convert a `Text` into a Template Haskell literal.
mkTextLit :: TH.Quote m 
          => DT.Text -- ^ Text to convert
          -> m Exp
mkTextLit = TH.litE . TH.StringL . DT.unpack

-- | Convert a `Natural` to a Template Haskell literal.
mkNaturalLit :: TH.Quote m 
            => Natural -- ^ Natural to convert
            -> m Exp
mkNaturalLit n | n >= 0 = TH.litE . TH.IntegerL . toInteger $ n
               | otherwise = error "QQ error: hole indices must be natural numbers"
-}
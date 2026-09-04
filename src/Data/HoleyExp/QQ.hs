{-|
Module      : QQ
Description : Quasi-Quoter for Holey Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

TODO
-}
{- HLINT ignore "Redundant bracket" -}
module Data.HoleyExp.QQ 
    (ToQExp(..)
    ,HExpQExp
    ,hole2QExp
    ,hExp2QExp) where

import GHC.Natural                 (Natural)
import Data.Text                   qualified as DT
import Data.Text                   (Text)
import Language.Haskell.TH         qualified as TH
import Language.Haskell.TH         (Q
                                   ,Exp
                                   ,Name)
import Data.HoleyExp.HExpInternal  (Hole
                                   ,HoleProps
                                   ,HoleFilling(..)
                                   ,HExp(..)
                                   ,IHExp(..)
                                   ,Proxy(..)
                                   ,pattern EmptyHole
                                   ,pattern FilledHole
                                   ,pattern UndefHole)
import Data.HoleyExp.Text          ()

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

instance ToQExp Natural where
        toQExp :: Natural -> Q Exp
        toQExp = TH.litE . TH.IntegerL . toInteger 

instance ToQExp () where
    toQExp :: () -> Q Exp
    toQExp () = TH.conE . TH.mkName $ "()"

instance HExpQExp Text   Int
instance HExpQExp Text   Double
instance HExpQExp Text   String
instance HExpQExp String Text
instance HExpQExp Text   ()
instance HExpQExp Text   Text

hole2QExp :: (HExpQExp text filling) 
          => Proxy text (Hole filling) 
          -> Q Exp
hole2QExp (Proxy (EmptyHole  i   _)) = appCombinator1 (TH.mkName "empty") (toQExp i)
hole2QExp (Proxy (FilledHole i f _)) = 
    appCombinator2 (TH.mkName "filled") (toQExp i) $ toQExp f
hole2QExp (Proxy (UndefHole i _)) 
    = error $ "QQ error: hole index "                                  
           <> (show i)                                  
           <> " present in internal hExp, but not defined in the templates hole properties."

iHExp2QExp :: (HExpQExp text filling) 
           => IHExp text 
           -> HoleProps filling 
           -> Q Exp
iHExp2QExp (IChunk chk) _ = do
    let chunk = TH.mkName "chunk"
    appCombinator1 chunk $ toQExp chk
iHExp2QExp (ICompose @text p h r) hlsProps = do
    -- ICompose p h r = (chunk p) +> (hole h) +> r
    let pExp      = iHExp2QExp (IChunk p) hlsProps
    let hExp      = hole2QExp (Proxy @text (h, hlsProps))
    let rExp      = iHExp2QExp r hlsProps
    let compose   = appInfixCombinator (TH.mkName "+>")
    (pExp `compose` hExp) `compose` rExp

hExp2QExp :: (HExpQExp text filling) 
          => HExp text filling 
          -> Q Exp
hExp2QExp (HExp it hls) = iHExp2QExp it hls

-- * Helpful Template Haskell combinators.

appInfixCombinator :: TH.Quote m 
                   => Name  -- ^ Name of the combinator
                   -> m Exp -- ^ First argument expression
                   -> m Exp -- ^ Second argument expression
                   -> m Exp 
appInfixCombinator constName e1 e2 = TH.infixE (Just e1) (TH.varE constName) (Just e2)

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



{-|
Module      : TextTemplateQQ
Description : Quasi-quoters for Text Templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

This is the quasi-quoter for "Data.TextTemplate"; see its documentation for an
introduction.

Using the quasi-quoter we can write templates as their own expressions.

For example, 

>>> let textDoubleTemplate = runProxy @Double textTemplate
>>> let t = [textDoubleTemplate|Today Temperature: $1{} high/$2{} low|] :: Template Text Double
>>> t
Today Temperature: $1{} high/$2{} low

Here is an example with a filled hole:

>>> let t' = [textDoubleTemplate|Today\'s Temperature: $1{} high/$2{77.3} low|] :: Template Text Double
>>> t'
Today's Temperature: $1{} high/$2{77.3} low

-}
module Data.HoleyExp.TextQQ 
    (hExp
    ,he
    ,unitHExp
    ,uhe) where

import Data.HoleyExp.HExpInternal (HExp
                                  ,Proxy(..) )
import Data.HoleyExp.Text         (parseHExp)
import Data.HoleyExp.QQ           (HExpQExp
                                  ,hExp2QExp ) 
import Language.Haskell.TH.Quote  (QuasiQuoter (..))
import Language.Haskell.TH        (Q
                                  ,Exp)
import Data.Text                  (Text)
import Data.Text                  qualified as DT

uhe :: QuasiQuoter
uhe = unitHExp

unitHExp :: QuasiQuoter
unitHExp = textHExpPQQ $ textHExp2QExp . Proxy @()

he :: QuasiQuoter
he = hExp

hExp :: QuasiQuoter
hExp = textHExpPQQ $ textHExp2QExp . Proxy @Text

textHExpPQQ :: (Text -> Q Exp)
            -> QuasiQuoter            
textHExpPQQ l = QuasiQuoter {
    quoteExp  = l . DT.pack
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

textHExp2QExp :: (HExpQExp Text filling)
                  => Proxy filling Text  -- ^ String to parse as a hExp
                  -> Q Exp
textHExp2QExp = flip (.) proxy $ \case {
         Right t  -> hExp2QExp t
        ;Left err -> fail $ DT.unpack err
    }
    where
        proxy :: (HExpQExp Text filling) 
              => Proxy filling Text 
              -> Either Text (HExp Text filling)
        proxy p@(Proxy @filling _) = parseHExp @filling . runProxy $ p

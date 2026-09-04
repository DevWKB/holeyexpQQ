{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeAbstractions       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
module Data.HoleyExp.TextQQ () where

import Data.HoleyExp.HExpInternal
import Data.HoleyExp.Text (parseHExp)
import Data.HoleyExp.QQ 

import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH       (Q, Exp)
import Language.Haskell.TH       qualified as TH
import Data.Text                 (Text)
import Data.Text                 qualified as DT


textHExpPQQ :: (HExpQExp Text filling) => Proxy filling QuasiQuoter
textHExpPQQ = Proxy $ QuasiQuoter {
    quoteExp  = textHExp2QExp . Proxy . DT.pack
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

textHExp2QExp :: (HExpQExp Text filling)
                  => Proxy filling Text  -- ^ String to parse as a hExp
                  -> Q Exp
textHExp2QExp = (flip (.) proxy $ \case {
         Right t  -> hExp2QExp t
        ;Left err -> fail $ DT.unpack err
    })
    where
        proxy :: (HExpQExp Text filling) 
              => Proxy filling Text 
              -> Either Text (HExp Text filling)
        proxy p@(Proxy @filling _) = parseHExp @filling . runProxy $ p

unitHExpQQ :: QuasiQuoter
unitHExpQQ = runProxy @() textHExpPQQ

textHExpQQ :: QuasiQuoter
textHExpQQ = runProxy @Text textHExpPQQ

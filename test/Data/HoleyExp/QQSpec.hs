{-# LANGUAGE QuasiQuotes #-}
module Data.HoleyExp.QQSpec (spec) where

import Test.Hspec                 (describe,it,shouldBe,Spec)
import Data.HoleyExp.HExpInternal (IHExp(..),HExp(..))
import Data.HoleyExp.Text         ((+>),chunk,empty,filled,Text)
import Data.HoleyExp.TextQQ       (he
                                  ,uhe)
import Data.IntMap                qualified as M

spec :: Spec 
spec = do
    describe "quasi-quoting tests:" $ do
        it "chunk test 1" $ do
            (fst testChunk1) `shouldBe` (snd testChunk1)
        it "chunk test 2" $ do
            (fst testChunk2) `shouldBe` (snd testChunk2)
        it "chunk test 3" $ do
            (fst testChunk3) `shouldBe` (snd testChunk3)
        it "hole test 1" $ do
            (fst testHole1) `shouldBe` (snd testHole1)
        it "hole test 2" $ do
            (fst testHole2) `shouldBe` (snd testHole2)
        it "hole test 3" $ do
            (fst testHole3) `shouldBe` (snd testHole3)
        it "hole test 4" $ do
            (fst testHole4) `shouldBe` (snd testHole4)
        it "hole test 5" $ do
            (fst testHole5) `shouldBe` (snd testHole5)          
        it "filled hole test 1" $ 
            (fst testFilledHole1) `shouldBe` (snd testFilledHole1)

testChunk1 :: (HExp Text (),HExp Text ())
testChunk1 = ([uhe|this is a chunk|],chunk "this is a chunk")

testChunk2 :: (HExp Text (),HExp Text ())
testChunk2 = ([uhe| |],chunk " ")

testChunk3 :: (HExp Text (),HExp Text ())
testChunk3 = ([uhe|😩|],chunk "😩")

testHole1 :: (HExp Text (),HExp Text ())
testHole1 = ([uhe|$1()$2()$3()|],HExp (ICompose "" 1 (ICompose "" 2 (ICompose "" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole2 :: (HExp Text (),HExp Text ())
testHole2 = ([uhe|$1()$2()-$3()|],HExp (ICompose "" 1 (ICompose "" 2 (ICompose "-" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole3 :: (HExp Text (),HExp Text ())
testHole3 = ([uhe|this $1() and $2() is $1()|],HExp (ICompose "this " 1 (ICompose " and " 2 (ICompose " is " 1 (IChunk "")))) ([1,2],M.empty))

testHole4 :: (HExp Text (),HExp Text ())
testHole4 = ([uhe|Hi $1()!|], HExp (ICompose "Hi " 1 (IChunk "!")) ([1],M.empty))

testHole5 :: (HExp Text (),HExp Text ())
testHole5 = ([uhe|Hi ❤️, $1() ‼|], HExp (ICompose "Hi ❤️, " 1 (IChunk " ‼")) ([1],M.empty))

testFilledHole1 :: (HExp Text Text,HExp Text Text)
testFilledHole1 = ([he|before-$1(example filling)-and-after|], HExp (ICompose "before-" 1 (IChunk "-and-after")) ([],M.fromList [(1,"example filling")]))

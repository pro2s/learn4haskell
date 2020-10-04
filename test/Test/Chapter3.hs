module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3

arthur :: Knight
arthur = MkKnight "Arthur" 0 100 20 50
boo :: Monster
boo = MkMonster "Boo" "booooo...boo" 70 30 25
deathclaw :: Monster
deathclaw = MkMonster "Deathclaw" "ARGH..." 150 50 1000

dobrynya :: FighterKnight
dobrynya = FighterKnight "Dobrynya Nikitich" (MkAttack 50) 100 50 [AttackAction, DrinkAction (MkHealth 25)]
gorynych :: FighterMonster
gorynych = FighterMonster "Zmey Gorynych" (MkAttack 25) 200 [AttackAction, RunAction]

chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ do
        describe "Task 2" $ do
            it "Arthur against Boo" $ fight arthur boo `shouldBe` 75
            it "Arthur against Deathclaw" $ fight arthur deathclaw `shouldBe` -1
    describe "Chapter3Advanced" $ do
        describe "Task9*" $ do
            it "Dobrynya against Gorynych" $ superFight dobrynya gorynych `shouldBe` "Dobrynya Nikitich"

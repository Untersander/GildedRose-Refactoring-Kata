module GildedRoseSpec (spec) where

import GildedRose
import Test.Hspec


spec :: Spec
spec = do
  describe "update Quality" $ do
    it "Correct Inventory after 1 day" $
      let 
        inventory =
              [ Item "+5 Dexterity Vest" 10 20,
                Item "Aged Brie" 2 0,
                Item "Elixir of the Mongoose" 5 7,
                Item "Sulfuras, Hand of Ragnaros" 0 80,
                Item "Sulfuras, Hand of Ragnaros" (-1) 80,
                Item "Backstage passes to a TAFKAL80ETC concert" 15 20,
                Item "Backstage passes to a TAFKAL80ETC concert" 10 49,
                Item "Backstage passes to a TAFKAL80ETC concert" 5 49,
                -- this conjured item does not work properly yet
                Item "Conjured Mana Cake" 3 6
              ]
        actual = updateQuality inventory
        expected = 
          [ Item "+5 Dexterity Vest" 9 19,
            Item "Aged Brie" 1 1,
            Item "Elixir of the Mongoose" 4 6,
            Item "Sulfuras, Hand of Ragnaros" 0 80,
            Item "Sulfuras, Hand of Ragnaros" (-1) 80,
            Item "Backstage passes to a TAFKAL80ETC concert" 14 21,
            Item "Backstage passes to a TAFKAL80ETC concert" 9 50,
            Item "Backstage passes to a TAFKAL80ETC concert" 4 50,
            -- this conjured item does not work properly yet
            Item "Conjured Mana Cake" 2 5
          ]
      in actual `shouldBe` expected
    it "Correct Inventory on day 10" $
      let
        inventory =
              [ Item "+5 Dexterity Vest" 10 20,
                Item "Aged Brie" 2 0,
                Item "Elixir of the Mongoose" 5 7,
                Item "Sulfuras, Hand of Ragnaros" 0 80,
                Item "Sulfuras, Hand of Ragnaros" (-1) 80,
                Item "Backstage passes to a TAFKAL80ETC concert" 15 20,
                Item "Backstage passes to a TAFKAL80ETC concert" 10 49,
                Item "Backstage passes to a TAFKAL80ETC concert" 5 49,
                -- this conjured item does not work properly yet
                Item "Conjured Mana Cake" 3 6
              ]
        actual = iterate updateQuality inventory !! 10
        expected =
          [ Item "+5 Dexterity Vest" 0 10,
            Item "Aged Brie" (-8) 18,
            Item "Elixir of the Mongoose" (-5) 0,
            Item "Sulfuras, Hand of Ragnaros" 0 80,
            Item "Sulfuras, Hand of Ragnaros" (-1) 80,
            Item "Backstage passes to a TAFKAL80ETC concert" 5 35,
            Item "Backstage passes to a TAFKAL80ETC concert" 0 50,
            Item "Backstage passes to a TAFKAL80ETC concert" (-5) 0,
            -- this conjured item does not work properly yet
            Item "Conjured Mana Cake" (-7) 0
          ]
        in actual `shouldBe` expected
    it "Correct Inventory on day 19" $
      let
        inventory =
              [ Item "+5 Dexterity Vest" 10 20,
                Item "Aged Brie" 2 0,
                Item "Elixir of the Mongoose" 5 7,
                Item "Sulfuras, Hand of Ragnaros" 0 80,
                Item "Sulfuras, Hand of Ragnaros" (-1) 80,
                Item "Backstage passes to a TAFKAL80ETC concert" 15 20,
                Item "Backstage passes to a TAFKAL80ETC concert" 10 49,
                Item "Backstage passes to a TAFKAL80ETC concert" 5 49,
                -- this conjured item does not work properly yet
                Item "Conjured Mana Cake" 3 6
              ]
        actual = iterate updateQuality inventory !! 19
        expected =
          [ Item "+5 Dexterity Vest" (-9) 0,
            Item "Aged Brie" (-17) 36,
            Item "Elixir of the Mongoose" (-14) 0,
            Item "Sulfuras, Hand of Ragnaros" 0 80,
            Item "Sulfuras, Hand of Ragnaros" (-1) 80,
            Item "Backstage passes to a TAFKAL80ETC concert" (-4) 0,
            Item "Backstage passes to a TAFKAL80ETC concert" (-9) 0,
            Item "Backstage passes to a TAFKAL80ETC concert" (-14) 0,
            -- this conjured item does not work properly yet
            Item "Conjured Mana Cake" (-16) 0
          ]
        in actual `shouldBe` expected
{-   describe "update Quality 20 days" $ do
    it "Quality and sellIn of Legendary Item " $
      let inventory = [Item "Sulfuras, Hand of Ragnaros" 0 80]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Sulfuras, Hand of Ragnaros" 0 80]
      in actual `shouldBe` expected
    it "Quality and sellIn of Legendary Item (SellIn Negative)" $
      let inventory = [Item "Sulfuras, Hand of Ragnaros" (-1) 80]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Sulfuras, Hand of Ragnaros" (-1) 80]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of regular decreasing Item" $
      let inventory = [Item "+5 Dexterity Vest" 10 20]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "+5 Dexterity Vest" (-9) 0]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of regular increasing Item" $
      let inventory = [Item "Aged Brie" 2 0]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Aged Brie" (-17) 36]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case1" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 15 20]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-4) 0]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case2" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 49]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-9) 0]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case3" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 49]
          actual = iterate updateQuality inventory !! 19
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-14) 0]
      in actual `shouldBe` expected
  describe "updateQualityOnce" $ do
    it "Quality and sellIn of Legendary Item " $
      let inventory = [Item "Sulfuras, Hand of Ragnaros" 0 80]
          actual = updateQuality inventory
          expected = [Item "Sulfuras, Hand of Ragnaros" 0 80]
      in actual `shouldBe` expected
    it "Quality and sellIn of Legendary Item (SellIn Negative)" $
      let inventory = [Item "Sulfuras, Hand of Ragnaros" (-1) 80]
          actual = updateQuality inventory
          expected = [Item "Sulfuras, Hand of Ragnaros" (-1) 80]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of regular decreasing Item" $
      let inventory = [Item "+5 Dexterity Vest" 10 20]
          actual = updateQuality inventory
          expected = [Item "+5 Dexterity Vest" 9 19]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of regular increasing Item" $
      let inventory = [Item "Aged Brie" 2 0]
          actual = updateQuality inventory
          expected = [Item "Aged Brie" 1 1]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case1" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 15 20]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 14 21]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case2" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 49]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 9 50]
      in actual `shouldBe` expected
    it "Correct Quality and sellIn of ConcertTicketIncrease case3" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 49]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 4 50]
      in actual `shouldBe` expected -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (
    main
) where

import Test.Hspec
import Data.Monoid
import SolarWallpaper
import SolarWallpaper.Types
import SolarWallpaper.XML
import Data.Time.Solar
import Data.Time
import Text.XML.Light

blockAt :: ZonedTime -> ZonedTime -> [ImageBlock] -> FilePath
blockAt at start =
    snd .
    head .
    dropWhile ((< dt) . fst) .
    scanl1 (\(a, _) (c, d) -> (a + c, d)) .
    map (\x -> (blockTime x, blockImage x))
  where
    dt = zonedTimeToLocalTime at `diffLocalTime` zonedTimeToLocalTime start

main :: IO ()
main =
    hspec $ do
        let imgs = Images "sunrise" "noon" "sunset" "evening" "midnight"
            loc = Location 50 20
            now =
                ZonedTime (LocalTime (fromGregorian 2019 03 03) midday) utc
            start = sunrise now loc
        describe "imageSequence" $ do
            let blks = imageSequence imgs now loc Nothing
                blksB = imageSequence imgs now loc (Just 0.2)
            it "starts at sunrise" $
                zonedTimeToLocalTime (fst blks) `shouldBe`
                zonedTimeToLocalTime start
            it "sums up to 24h with bias 0.5" $ do
                let blockSum = getSum . foldMap (Sum . blockTime)
                (blockSum . snd $ blks) `shouldBe` 86400
            it "sums up to 24h with bias 0.2" $ do
                let blockSum = getSum . foldMap (Sum . blockTime)
                (blockSum . snd $ blksB) `shouldBe` 86400
            it "displays sunrise image at sunrise" $
                blockAt (sunrise now loc) start (snd blks) `shouldBe` "sunrise"
            it "displays noon image at noon" $
                blockAt (solarNoon now loc) start (snd blks) `shouldBe` "noon"
            it "displays sunset image at sunset" $
                blockAt (sunset now loc) start (snd blks) `shouldBe` "sunset"
            it "displays midnight image at midnight" $
                blockAt (solarMidnight now loc) start (snd blks) `shouldBe`
                "midnight"
        context "XML conversion" $ do
            describe "imageBlock" $ do
                it "renders static" $
                    ppElement (imageBlock (StaticImage "foobar" 300)) `shouldBe`
                        "<static>\n  <duration>300.0</duration>\n  <file>foobar</file>\n</static>"
                it "renders transition" $
                    ppElement (imageBlock (Transition Overlay "foo" "bar" 300)) `shouldBe`
                        "<transition type=\"overlay\">\n  <duration>300.0</duration>\n\
                        \  <from>foo</from>\n  <to>bar</to>\n</transition>"
            describe "startTime" $ do
                it "renders the start time" $ do
                    ppElement (startTime now) `shouldBe`
                        "<starttime>\n  <year>2019</year>\n  <month>03</month>\n  <day>03</day>\n\
                        \  <hour>12</hour>\n  <minute>00</minute>\n  <second>00</second>\n</starttime>"

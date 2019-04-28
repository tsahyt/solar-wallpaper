module SolarWallpaper.Pretty
    ( prettyOut
    ) where

import Data.Time
import SolarWallpaper.Types
import Text.PrettyPrint.Leijen

import Prelude hiding ((<$>), (<>))

prettyOut :: ZonedTime -> [ImageBlock] -> String
prettyOut start blks = ($ "") . displayS . renderPretty 0.6 120 $ go
  where
    go = vcat (map (uncurry block) . accumulateBlocks start $ blks)

accumulateBlocks :: ZonedTime -> [ImageBlock] -> [(ZonedTime, ImageBlock)]
accumulateBlocks _ [] = []
accumulateBlocks start (x:xs) =
    (start, x) : accumulateBlocks (blockTime x `addZonedTime` start) xs

zonedTime :: ZonedTime -> Doc
zonedTime = text . show

diffTime :: NominalDiffTime -> Doc
diffTime = text . show

block :: ZonedTime -> ImageBlock -> Doc
block at (StaticImage path duration) =
    zonedTime at <> char ':' <$>
    indent 4 (text "static" <+> text path <+> text "for" <+> diffTime duration)
block at (Transition Overlay from to duration) =
    zonedTime at <> char ':' <$>
    indent
        4
        (text "overlay" <+>
         text from <+>
         text "to" <+> text to <+> text "for" <+> diffTime duration)

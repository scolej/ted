module Ted.Fancy.Colours where

import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace

todCos :: Float -> Float
todCos tod = cos (2 * pi * tod)

-- | Calculate the foreground lightness as a function of the time of day fraction.
bgLightness :: Float -> Float
bgLightness tod = 0.5 + (-0.5) * todCos tod

-- | Calculate the background lightness as a function of the time of day fraction.
fgLightness :: Float -> Float
fgLightness tod = 0.25 + (0.25) * todCos tod

slidingHue :: Float -> Float
slidingHue tod = 300 + 60 * todCos tod

slidingSaturation :: Float -> Float
slidingSaturation tod = 0.5 + (-0.5) * todCos tod

slidingFg :: Float -> RGB Float
slidingFg tod = hsl (slidingHue tod) (slidingSaturation tod) (fgLightness tod)

slidingBg :: Float -> RGB Float
slidingBg tod = hsl (slidingHue tod) (slidingSaturation tod) (bgLightness tod)

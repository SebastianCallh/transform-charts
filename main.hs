import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Array.IArray
import Numeric.Transform.Fourier.FFT
import Data.Complex
import GHC.Float

type Signal = Array Integer (Complex Double)

signalSize :: (Integer, Integer)
signalSize = (0, 1000)

rect t = array signalSize [(x, if x < t then 1 else 0) | x <- [start..end]] where
  start = fst signalSize
  end = snd signalSize

makeRenderable :: Signal -> [(Double, Double)]
makeRenderable signal = take half . normalize $ map f $ assocs signal where
  f (i, e) = (fromIntegral i, magnitude e)
  half = length signal `div` 2

normalize :: (Fractional t, Integral a) => [(a, t)] -> [(t, t)]
normalize x = map (\(i, e) -> (fromIntegral i / l, e)) x where
  l = fromIntegral $ length x

main = do
  let trans_x = fft $ rect 20
  toFile def "rect_transform.png" $ do
    layout_title .= "Rect fourier transform"
    layout_x_axis . laxis_generate .= scaledAxis def (0, 0.5)
    setColors [opaque blue]
    plot $ line "transform" $ [makeRenderable trans_x]

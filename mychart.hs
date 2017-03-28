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













    --plot (line "am" [signal2 [0,(0.1)..2*pi]])
    --plot (points "am points" (signal2 [0,(0.1)..2*pi]))
    --plot $ line "signal" $ [makeRenderable x]
        --    plot (line "third" [signal [0, (0.5)..400]])
      
    --plot (line "fourth" [signal3 [0, 3]])
--xAxis :: LayoutAxis
--xAxis = LayoutAxis 

{--
chart = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5)^2)

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_x_axis .~ xAxis  -- _laxis_title ""
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def

main = renderableToFile def "example1_big.png" chart


main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [makeRenderable $ rect 20])
    --pplot (points "am points" (signal [0,7..400]))

main = do
  let x = rect 20
  let trans_x = fft x 
  toFile def "mychart.svg" $ do
    layout_title .= "Rect fourier transform"

    scaledAxis (0, 1)
    setColors [opaque blue, opaque red]
    --plot (line "am" [signal2 [0,(0.1)..2*pi]])
    --plot (points "am points" (signal2 [0,(0.1)..2*pi]))
    plot $ line "signal" $ [makeRenderable x]
    plot $ line "transform" $ [makeRenderable trans_x]
        --    plot (line "third" [signal [0, (0.5)..400]])
    --plot (line "fourth" [signal3 [0, 3]])
--}

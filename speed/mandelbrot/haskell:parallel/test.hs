module Main(main) where
import Control.Parallel.Strategies
max_iterations :: Int
max_iterations = 99888
iterate :: Double -> Double -> Char
iterate ci cr = loop 0.0 0.0 1
    where
      loop zi zr i
          | i > max_iterations = '*'
          | zi2 + zr2 > 4.0    = ' '
          | otherwise          = loop zi' zr' (i + 1)
          where temp = zr * zi
                zr2 = zr * zr
                zi2 = zi * zi
                zi' = temp + temp + ci
                zr' = zr2 - zi2 + cr
mandelbrot :: String
mandelbrot = unlines image
    where image     = parMap rnf line [-39..38] -- (*)
          line y    = map (pixel y)   [-39..38]
          pixel y x = Main.iterate (x / 40.0) (y/40.0-0.5)
main :: IO ()
main = putStrLn mandelbrot

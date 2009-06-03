module Main(main) where
import Control.Parallel.Strategies
import Data.Array.Vector
max_iterations :: Int
max_iterations = 99888
pixel :: Int -> Int -> Char
pixel x y = loop 0.0 0.0 1
    where
      ci, cr :: Float
      ci = fromIntegral y / 40.0
      cr = fromIntegral x / 40.0 - 0.5
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
mandelbrot = unlines $ map fromU image
    where image     = parMap (`seq` ()) line [-39..38]
          line y    = mapU (pixel y) $ enumFromToU (-39) 38
main :: IO ()
main = putStrLn mandelbrot

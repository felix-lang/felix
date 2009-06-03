module Main
where
import System.Time
runs :: Int
runs = 1
max_iterations :: Int
max_iterations = 99888
iterate :: Double -> Double -> Int
iterate ci cr =
   let bailout = 4.0
       loop zi zr i =
             if i > max_iterations then
               0
             else
               let temp = zr * zi
                   zr2 = zr * zr
                   zi2 = zi * zi  in
                       if zi2 + zr2 > bailout then
                         i
                       else
                         loop (temp + temp + ci) (zr2 - zi2 + cr) (i + 1)
       in
   loop 0.0 0.0 1
mandelbrot n = do
     let y = [-39..38]
         x = [-39..38]
         iter y x = do
             let res = Main.iterate ((fromIntegral x) / 40.0)
                                ((fromIntegral y) / 40.0 - 0.5)
             if n == 1 then
                 if res == 0 then putChar '*' else putChar ' '
                 else return ()
         inner y = do
             mapM_ (iter y) x
             if n == 1 then putChar '\n' else return ()
         outer = mapM_ (\i -> inner i) y
     outer
main = do
     let iter = [1..runs]
     startTime <- getClockTime
     mapM_ mandelbrot iter
     endTime <- getClockTime
     let diff = show (diffClockTimes endTime startTime)
     putStrLn $ "Time: " ++ diff


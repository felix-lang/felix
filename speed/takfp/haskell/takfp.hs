import System(getArgs)

main = do n <- getArgs >>= readIO . head
          putStrLn (show (tak (3*n) (2*n) n))

tak :: Float -> Float -> Float -> Float
tak x y z | y>=x      = z
          | otherwise = tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)


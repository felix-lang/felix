import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . haqify . head

haqify s = "Hello, " ++ s

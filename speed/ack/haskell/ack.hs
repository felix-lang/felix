import System.Environment (getArgs)
import System.IO
import Text.Printf (hPrintf)
import GHC.Exts

main = do (x1:xs) <- getArgs
          (I# n#) <- return (read x1)
          hPrintf stdout "Ack(3,%d): %d\n" (I# n#) (I# (ack  3# n#))

ack :: Int# -> Int# -> Int#
ack 0# n = n +# 1#
ack m 0# = ack (m -# 1#) 1#
ack m n = ack (m -# 1#) (ack m (n -# 1#))


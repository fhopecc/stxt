import Foreign.C -- get the C types
import Foreign.C.String (castCUCharToChar)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr


-- pure function
foreign import ccall sumTwo :: Int -> Int -> Int

main = do
    print $ sumTwo 2 3


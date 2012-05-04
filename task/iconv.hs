 import Codec.Text.IConv as IConv
 import Data.ByteString.Lazy as ByteString

 main = ByteString.interact (convert "LATIN1" "UTF-8")

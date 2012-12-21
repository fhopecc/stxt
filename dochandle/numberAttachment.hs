import Text.Regex.Base
import Text.Regex.Posix
import 

main = do
    print (unsafeAt result!1)

result ::  MatchArray

result = "no match a mat" =~ "mat"

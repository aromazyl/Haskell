import Control.Monad


main = do
        y <- return "kill"
        return "hello world" ++ y

module Main where
import JOHN

main :: IO ()
main = putStrLn $ show $ parseJohn "hello \"world!\""
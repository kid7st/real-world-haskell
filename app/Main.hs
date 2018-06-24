module Main where

import Ch5.SimpleJSON
-- import Ch5.PutJSON
import Ch5.PrettyJSON
import Ch5.Prettify

main :: IO ()
main = do
  print (JObject [("foo", JNumber 1), ("boo", JBool False)])
  putStrLn ((replicate 10 '-') ++ "compact")

  let value = renderJValue (JObject [("foo", JNumber 1), ("boo", JObject [("test", JBool True)])])
  putStrLn (compact value)
  putStrLn ((replicate 10 '-') ++ "pretty")
  putStrLn (pretty 30 value)
  putStrLn ((replicate 10 '-') ++ "fill")
  putStrLn (compact (fill 20 value))
  putStrLn ((replicate 10 '-') ++ "nest")
  putStrLn (compact (nest 1 value))
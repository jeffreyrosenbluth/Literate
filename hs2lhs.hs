{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Control.Applicative ((<$>), (<|>))
import            Data.Maybe          (fromMaybe)
import            Data.Text           (Text, stripStart, stripPrefix)
import qualified  Data.Text           as T
import qualified  Data.Text.IO        as T
import            System.Environment

data Current = Comment | Code

stripPre pre w t = g <$> stripPrefix pre t
  where g x = (w, stripStart x)

lhsLine :: Current -> Text -> (Current, Text)
lhsLine w t = fromMaybe d c
  where
    c = stripPre "---" Comment t <|> stripPre "-- |" Comment t <|> stripPre "--" Comment t
    d = if t == T.empty then (Code, "") else (Code, s)
    s = p `T.append` t
    p = case w of {Comment -> "\n> "; Code -> "> "}

lhs :: Current -> [Text] -> [Text]
lhs _ [] = []
lhs c (t:ts) = t' : (lhs c' ts)
  where (c', t') = lhsLine c t

main = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
  mapM_ T.putStrLn (lhs Code p)

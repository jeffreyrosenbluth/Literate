{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Control.Applicative ((<$>), (<|>))
import            Data.Maybe          (fromJust, fromMaybe)
import            Data.Text           (Text, stripStart, stripPrefix)
import qualified  Data.Text           as T
import qualified  Data.Text.IO        as T
import            System.Environment

data Current = Comment | Haddock | CHeader | Code

stripPre pre w t = g <$> stripPrefix pre t
  where g x = (w, stripStart x)

lhsLine :: Current -> Text -> (Current, Text)
lhsLine w t = case w of
    Code -> handleCode t 
    _    -> handleComments w t

handleCode :: Text -> (Current, Text)
handleCode t = fromMaybe d c
  where
    c = stripPre "---" CHeader t <|> stripPre "-- |" Haddock t <|> stripPre "--" Comment t
    d = if t == T.empty then (Code, "") else (Code, "> " `T.append` t)

handleComments :: Current ->Text -> (Current, Text)
handleComments w t = fromMaybe d c
  where
    c = stripPre "---" w t <|> stripPre "-- |" w t <|> stripPre "--" w t
    d = if t == T.empty then (Code, "") else (Code, "\n> " `T.append` t)

lhs :: Current -> [Text] -> [Text]
lhs _ [] = []
lhs c (t:ts) = t' : (lhs c' ts)
  where (c', t') = lhsLine c t

main = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
  mapM_ T.putStrLn (lhs Code p)

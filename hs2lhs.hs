{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main where

import            Control.Applicative ((<$>), (<|>))
import            Data.Maybe          (fromMaybe)
import            Data.Text           (Text, stripStart, stripPrefix,
                                       isPrefixOf, isSuffixOf)
import qualified  Data.Text           as T
import qualified  Data.Text.IO        as T
import            System.Environment

data Current = Comment | Code

stripPre :: Text -> Current -> Text -> Maybe (Current, Text)
stripPre pre w t = g <$> stripPrefix pre t
  where g x = (w, stripStart x)

lhsLine :: Current -> Text -> (Current, Text)
lhsLine w t = fromMaybe d c
  where
    d = if | t == T.empty -> (Code, "") 
           | isPrefixOf "{-#" t && isSuffixOf "#-}" t -> (Code, t)
           | otherwise -> (Code, s `T.append` t)
    s = case w of {Comment -> "\n> "; Code -> "> "}
    c = stripPre "---" Comment t <|> stripPre "-- |" Comment t 
                                 <|> stripPre "--"   Comment t

lhs :: Current -> [Text] -> [Text]
lhs _ [] = []
lhs c (t:ts) = t' : (lhs c' ts)
  where (c', t') = lhsLine c t

main = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
  mapM_ T.putStrLn (lhs Code p)

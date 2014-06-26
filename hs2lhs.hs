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

data Tag = Comment | Code

lhsLine :: Tag -> Text -> (Tag, Text)
lhsLine w t = fromMaybe d c
  where
    d = if | t == T.empty -> (Code, "") 
           | isPrefixOf "{-#" t && isSuffixOf "#-}" t -> (Code, t)
           | otherwise -> (Code, s `T.append` t)
    s = case w of {Comment -> "\n> "; Code -> "> "}
    c = stripC "-- |" t <|> stripC "--"  t
    stripC p t = (\x -> (Comment, stripStart x)) <$> stripPrefix p t

lhs :: Tag -> [Text] -> [Text]
lhs _ [] = []
lhs c (t:ts) = t' : (lhs c' ts)
  where (c', t') = lhsLine c t

main = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
  mapM_ T.putStrLn (lhs Code p)

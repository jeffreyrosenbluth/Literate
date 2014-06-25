{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Control.Monad.State
import            Data.Maybe          (fromJust)
import            Data.Text           (Text, isPrefixOf, stripStart, stripPrefix)
import qualified  Data.Text           as T
import qualified  Data.Text.IO        as T
import            System.Environment

data WIP = Comment | Haddock | MComment | CHeader | Code

type WIPState = State WIP [Text]

-- Warning partial function
strip :: Text -> Text -> Text
strip s = stripStart . fromJust . stripPrefix s

toCode :: Text -> Text
toCode s = T.append "> " s

lhsLine :: WIP -> Text -> (WIP, Text)
lhsLine w t = case w of
  Haddock -> handleHaddock t
  CHeader -> handleCHeader t
  Comment -> handleComment t
  Code    -> handleCode t

handleMComment :: Text -> (WIP, Text)
handleMComment s
  | isPrefixOf "{-#" s  = (Code, T.cons 'n' (toCode s))
  | isPrefixOf "---" s  = (Comment, s)
  | isPrefixOf "-- |" s = (Haddock, strip "-- |" $ s)
  | isPrefixOf "--" s   = (Comment, strip "--" $ s)
  | s == T.empty        = (Code, "")
  | otherwise           = (Code, T.cons '\n' (toCode s))

handleComment :: Text -> (WIP, Text)
handleComment s
  | isPrefixOf "{-#" s  = (Code, T.cons 'n' (toCode s))
  | isPrefixOf "---" s  = (Comment, s)
  | isPrefixOf "---" s  = (Comment, s)
  | isPrefixOf "-- |" s = (Haddock, strip "-- |" $ s)
  | isPrefixOf "--" s   = (Comment, strip "--" $ s)
  | s == T.empty        = (Code, "")
  | otherwise           = (Code, T.cons '\n' (toCode s))

handleCode :: Text -> (WIP, Text)
handleCode s
  | isPrefixOf "{-#" s  = (Code, toCode s)
  | isPrefixOf "---" s  = (Comment, s)
  | isPrefixOf "---" s  = (CHeader, s)
  | isPrefixOf "-- |" s = (Haddock, strip "-- |" $ s)
  | isPrefixOf "--" s   = (Comment, strip "--" $ s)
  | s == T.empty        = (Code, "")
  | otherwise           = (Code, "> " `T.append` s)

handleHaddock :: Text -> (WIP, Text)
handleHaddock s
  | isPrefixOf "{-#" s  = (Code, T.cons 'n' (toCode s))
  | isPrefixOf "---" s  = (Haddock, s)
  | isPrefixOf "-- |" s = (Haddock, strip "-- |" $ s)
  | isPrefixOf "--" s   = (Haddock, strip "--" $ s)
  | s == T.empty        = (Code, "")
  | otherwise           = (Code, T.cons '\n' (toCode s))

handleCHeader :: Text -> (WIP, Text)
handleCHeader s
  | isPrefixOf "{-#" s  = (Code, T.cons 'n' (toCode s))
  | isPrefixOf "---" s  = (CHeader, s)
  | isPrefixOf "-- |" s = (CHeader, s)
  | isPrefixOf "--" s   = (CHeader, s)
  | s == T.empty        = (Code, "")
  | otherwise           = (Code, T.cons '\n' (toCode s))

lhs :: [Text] -> WIPState
lhs [] = return []
lhs (t:ts) = do
  w <- get
  let (w', t') = lhsLine w t
  put w'
  liftM (t':) (lhs ts)

main = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
      q = evalState (lhs p) Code
  mapM_ T.putStrLn q

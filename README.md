Literate
========
hs2hs
-----
hs2lhs is a script to convert haskell source files (.hs) to bird style literate haskell files (.lhs).

Usage: 
``` bash
./hs2lhs myprog.hs >> myprog.lhs
```

Converting hs to lhs requires making some fairly arbitrary decision about how to handle comments.
hslhs makes any comment begininng with `--` (including haddock comments) into plain lhs text leaves
any other comments (includinge `{- ... -}`) embedded in the code.
Of course there are many examples that wont convert as you expect, since coding styles vary. Consider
hs2lhs generated files as the first step in converting to literate haskell.

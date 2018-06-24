module Ch5.Prettify where

import Ch5.SimpleJSON

data Doc  = Empty
          | Char Char
          | Text String
          | Line
          | Concat Doc Doc
          | Union Doc Doc
  deriving (Show, Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line            = Char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

concat :: [[a]] -> [a]
concat = foldr (++) []

compact :: Doc -> String
compact x = transform [x]
  where transform []      = ""
        transform (d:ds)  =
          case d of
            Empty         -> transform ds
            Char c        -> c : transform ds
            Text s        -> s ++ transform ds
            Line          -> '\n' : transform ds
            a `Concat` b  -> transform (a:b:ds)
            _ `Union` b   -> transform (b:ds)

fits :: Int -> String -> Bool
w `fits` _ | w < 0  = False
w `fits` ""         = True
w `fits` ('\n':_)   = True
w `fits` (c:cs)     = (w - 1) `fits` cs

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d:ds) =
      case d of
        Empty         -> best col ds
        Char c        -> c : best (col + 1) ds
        Text s        -> s ++ best (col + length s) ds
        Line          -> '\n' : best 0 ds
        a `Concat` b  -> best col (a:b:ds)
        a `Union` b   -> nicest col (best col (a:ds)) (best col (b:ds))
    best _ _ = ""
    nicest col a b | (width - least) `fits` a = a
                   | otherwise                = b
      where least = min width col

fill :: Int -> Doc -> Doc
fill width doc = best 0 [doc]
  where
    best col (d:ds) =
      case d of
        Empty -> Empty <> best col ds
        Char c -> Char c <> best (col + 1) ds
        Text s -> Text s <> best (col + length s) ds
        Line -> Text (replicate (width - col) ' ') <> Line <> best 0 ds
        a `Concat` b -> best col (a:b:ds)
        a `Union` b -> best col (a:ds) `Union` best col (b:ds)
    best col [] = Text (replicate (width - col) ' ')

nest :: Int -> Doc -> Doc
nest indent doc = best 0 [0] [doc]
  where
    best col nestStack (d:ds) =
      case d of
        Empty -> Empty <> best col nestStack ds
        Char c -> Char c <> best (col + 1) newNestStack ds
          where newNestStack  = if c == '{' || c == '['
                                then (col + indent):nestStack
                                else if c == '}' || c == ']' then tail nestStack
                                else nestStack
        Text s -> Text s <> best (col + length s) nestStack ds
        Line -> Line <> Text (replicate (head nestStack) ' ') <> best 0 nestStack ds
        a `Concat` b -> best col nestStack (a:b:ds)
        a `Union` b -> best col nestStack (a:ds) `Union` best col nestStack (b:ds)
    best col nestStack [] = Empty


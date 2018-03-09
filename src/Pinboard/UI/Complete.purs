module Pinboard.UI.Complete
  ( split
  , match
  , corpus
  , Result(..)
  , Score
  , Span(..)
  , Split
  , Part(..)
  )
  where

import Prelude
import Partial.Unsafe           (unsafePartial)
import Data.Maybe               (Maybe(..), fromMaybe, fromJust)
import Data.Array               ((:), drop, uncons, catMaybes, zipWith, length, unsafeIndex)
import Data.Char                (toUpper)
import Data.String              as S
import Data.Either              (Either, fromRight)
import Data.Traversable         (traverse, sequence)
import Data.String.Regex        as R
import Data.String.Regex.Flags  as R
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)

-- a:b matches f[a]rm:co[b]b
-- ab  matches f[a]rm:co[b]b
-- ab  matches t[ab]s
-- ab  matches c[a]r[b]s
-- ab  matches r[a]t:[b]at

complete :: forall m. Applicative m => Array String -> String -> m (Array Result)
complete xs x = pure []

match :: Split String -> Split String -> Boolean
match xs ys = aux 0 0
  where
    lx = length xs
    ly = length ys

    -- match up xs[ix] with ys, but abort at the end of either
    aux :: Int -> Int -> Boolean
    aux ix iy
      | ix >= lx  = true
      | iy >= ly  = false
      | otherwise = cmp ix iy (xs `at` ix) (ys `at` iy)

    -- check if xs[ix] matches ys[iy], otherwise skip to next b
    cmp :: Int -> Int -> Part String -> Part String -> Boolean
    cmp ix iy (Symbols _) (Letters _) = aux ix (iy+1)
    cmp ix iy (Letters _) (Symbols _) = aux ix (iy+1)
    cmp ix iy (Symbols a) (Symbols b) = aux ix (iy+2) || aux (ix+1) (iy+1)
    cmp ix iy (Letters a) (Letters b) = aux ix (iy+2) || tok ix iy 0 0 a b (S.length a) (S.length b)

    -- compare chars in two strings, (a=xs[ix])[ia] and (b=ys[iy])[ib]
    tok :: Int -> Int -> Int -> Int -> String -> String -> Int -> Int -> Boolean
    tok ix iy ia ib a b la lb
      | la - ia <= 0 = if ix+1 >= lx
                          then true
                          else cmp (ix+1) iy (xs `at` (ix+1)) (suffix b (ib+1))
      | lb - ib <= 0 = if iy+2 >= ly
                          then false
                          else cmp ix (iy+2) (suffix a ia) (ys `at` (iy+2))
      | (a `char` ia) `eq` (b `char` ib)
                  = tok ix iy ia (ib+1) a b la lb || tok ix iy (ia+1) (ib+1) a b la lb
      | otherwise = tok ix iy ia (ib+1) a b la lb

    at xs k  = unsafePartial (unsafeIndex xs k)
    eq a b   = toUpper a == toUpper b
    char s k = unsafePartial (fromJust (S.charAt k s))
    suffix s n = Letters (S.drop n s)

-------------------------------------------------------------------------------

type Score  = Int
data Result = Result Score (Array Span)

data Span
  = Matched String
  | Unmatch String

type Split a = Array (Part a)
data Part a
  = Symbols a
  | Letters a

derive instance eqSpan :: Eq Span
derive instance eqPart :: Eq a => Eq (Part a)
derive instance functorPart :: Functor Part
derive instance genericSpan :: Generic Span _
derive instance genericPart :: Generic (Part a) _
derive instance genericResult :: Generic Result _

instance showSpan :: Show Span where show = genericShow
instance showPart :: Show a => Show (Part a) where show = genericShow
instance showResult :: Show Result where show = genericShow

split :: String -> Split String
split "" = []
split s  =
  case uncons letters of
       Nothing -> letters
       Just {head,tail} ->
         let ps = join (zipWith (\d t -> [d,t]) symbols tail)
          in head : ps
  where
    regex :: R.Regex
    regex = unsafePartial (fromRight (R.regex "[^a-zA-Z0-9]+" R.global))

    letters :: Array (Part String)
    letters = map Letters (R.split regex s)

    symbols :: Array (Part String)
    symbols = map Symbols (fromMaybe [] (map catMaybes (R.match regex s)))

corpus :: Array String
corpus =
  ["algorithms"
  ,"audio"
  ,"ayn"
  ,"bad"
  ,"bayes"
  ,"type:blog-post"
  ,"book"
  ,"books"
  ,"bug"
  ,"c"
  ,"career"
  ,"cf"
  ,"conferences"
  ,"cooking"
  ,"course"
  ,"cv"
  ,"data"
  ,"db"
  ,"diy"
  ,"dog"
  ,"dsp"
  ,"dst"
  ,"economics"
  ,"embedded"
  ,"essay"
  ,"evil"
  ,"fail"
  ,"finance"
  ,"food"
  ,"fp"
  ,"furniture"
  ,"garden"
  ,"gear"
  ,"golf"
  ,"good"
  ,"gpu"
  ,"haskell"
  ,"health"
  ,"hosting"
  ,"house"
  ,"humor"
  ,"ios"
  ,"java"
  ,"journal"
  ,"kaggle"
  ,"kc"
  ,"logic"
  ,"math"
  ,"ml"
  ,"music"
  ,"nlp"
  ,"ocaml"
  ,"philosophy"
  ,"pl"
  ,"politics"
  ,"practice"
  ,"privacy"
  ,"project"
  ,"psc"
  ,"psychology"
  ,"purescript"
  ,"python"
  ,"queue"
  ,"recipe"
  ,"recording"
  ,"reddit"
  ,"reference"
  ,"rpi"
  ,"scala"
  ,"science"
  ,"shuchi"
  ,"society"
  ,"spark"
  ,"sqlite"
  ,"type:article"
  ,"type:blog"
  ,"type:book"
  ,"type:course"
  ,"type:essay"
  ,"type:forum"
  ,"type:interview"
  ,"type:manual"
  ,"type:photo"
  ,"type:podcast"
  ,"type:presentation"
  ,"type:repo"
  ,"type:video"
  ,"type:wiki"
  ,"vim"
  ,"wedding"
  ,"wishlist"
  ,"writing"
  ,"yelp"
  ]

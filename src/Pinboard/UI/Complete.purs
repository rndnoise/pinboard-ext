module Pinboard.UI.Complete
  ( Span(..)
  , Part(..)
  , parse
  , matches
  , iron
  ) where

import Prelude
import Control.Alt              ((<|>))
import Control.Plus             (empty)
import Data.List                (List(..), (:), fromFoldable, uncons, zipWith)
import Data.Array               (catMaybes)
import Data.Either              (fromRight)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Char                (toUpper)
import Data.String              (singleton, drop)
import Data.String.Unsafe       (charAt)
import Data.String.Regex        (Regex, regex, match, split)
import Data.String.Regex.Flags  (global)
import Partial.Unsafe           (unsafePartial)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)

import Pinboard.UI.Search       (Search, tell)


-- | Concise tuple
data T a = T a a


-- |
type Result = List Span
data Span
  = M String  -- matched
  | U String  -- unmatched
derive instance gSpan :: Generic Span _
instance showSpan :: Show Span where show = genericShow


-- |
type Split = List Part
data Part
  = S String  -- symbols
  | L String  -- letters
derive instance gPart :: Generic Part _
instance showPart :: Show Part where show = genericShow


-- |
iron :: Result -> Result
iron Nil                           = Nil
iron (Cons (M a) (Cons (M b) cs))  = iron (M (a <> b) : cs)
iron (Cons (U a) (Cons (U b) cs))  = iron (U (a <> b) : cs)
iron (Cons (U a) (Cons (M "") cs)) = iron (U a : cs)
iron (Cons (M a) (Cons (U "") cs)) = iron (M a : cs)
iron (Cons (M "") bs)              = iron bs
iron (Cons (U "") bs)              = iron bs
iron (Cons a bs)                   = a : iron bs

-- |
matches :: Split -> Split -> Search Result Unit
matches x y = case T x y of
  T Nil bs                            -> consume bs
  T (Cons a as) Nil                   -> empty
  T as@(Cons (S _) _) (Cons (L b) bs) ->  log (U b) *> matches as bs
  T as@(Cons (L _) _) (Cons (S b) bs) ->  log (U b) *> matches as bs
  T z@(Cons (S a) as) (Cons (S b) bs) -> (log (U b) *> matches z bs) <|> (log (M b) *> matches as bs)
  T z@(Cons (L a) as) (Cons (L b) bs) -> (log (U b) *> matches z bs) <|> letters a b (T as bs)
  where
    log :: Span -> Search (List Span) Unit
    log = tell <<< (_ : Nil)

    consume :: Split -> Search Result Unit
    consume Nil             = pure unit
    consume (Cons (S b) bs) = log (U b) *> consume bs
    consume (Cons (S b) bs) = log (U b) *> consume bs
    consume (Cons (L b) bs) = log (U b) *> consume bs

    letters :: String -> String -> T Split -> Search (List Span) Unit
    letters "" b (T as bs) = matches as (L b : bs)
    letters a "" (T as bs) = matches (L a : as) bs
    letters a b xx =
      let a0 = charAt 0 a
          b0 = charAt 0 b
          as = drop 1 a
          bs = drop 1 b
          no = log (U (singleton b0)) *> letters a bs xx
       in no <|> if toUpper a0 == toUpper b0
                    then log (M (singleton b0)) *> letters as bs xx
                    else empty

-- |
parse :: String -> Split
parse "" = Nil
parse s  =
  case uncons letters of
       Nothing          -> letters
       Just {head,tail} -> head : join (zipWith (\d t -> d : t : Nil) symbols tail)
  where
    pattern :: Regex
    pattern = unsafePartial (fromRight (regex "[^a-zA-Z0-9]+" global))

    letters :: List Part
    letters = map L (fromFoldable (split pattern s))

    symbols :: List Part
    symbols = map S (fromFoldable (fromMaybe [] (map catMaybes (match pattern s))))


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

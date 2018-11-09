module Pinboard.UI.Component.TagComplete
  ( Span(..)
  , Part(..)
  , Result
  , matches
  ) where

import Prelude
import Control.Alt                ((<|>))
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Control.Plus               (empty)
import Data.Array                 (catMaybes)
import Data.Array.NonEmpty        (toArray)
import Data.Char.Unicode          (toUpper)
import Data.Either                (fromRight)
import Data.List                  (List(..), (:), fromFoldable, head, uncons, zipWith)
import Data.Maybe                 (Maybe(..), fromMaybe)
import Data.String.CodeUnits      (singleton, drop)
import Data.String.Regex          (regex, match, split)
import Data.String.Regex.Flags    (global)
import Data.String.Unsafe         (charAt)
import Data.Tuple                 (Tuple(..))
import Data.Traversable           (sequence)
import Partial.Unsafe             (unsafePartial)

-------------------------------------------------------------------------------

type Search w a = WriterT w List a

-- | Concise tuple
data T a = T a a

data Part
  = S String
    -- ^ Sequence of symbols (e.g., #%@!)
  | L String
    -- ^ Sequence of non-symbols (e.g., abc123)

data Span
  = M String
    -- ^ Sequence of characters that match the query
  | U String
    -- ^ Sequence of characters that do not match the query

type Split  = List Part
type Result = List Span

derive instance eqPart :: Eq Part
derive instance eqSpan :: Eq Span

-------------------------------------------------------------------------------

-- | Find all the strings in `haystack` that the given `needle` matches
matches :: Array String -> String -> Array (Tuple String Result)
matches haystack =
  let haystack_ = map (\x -> Tuple x (parse x)) haystack
   in \needle ->
     -- Due to the let binding, haystack_ won't be recomputed
     -- each time a search is performed (so, should be faster)
     let needle_ = parse needle
      in catMaybes (map (sequence <<< map (try needle_)) haystack_)
  where
    -- Pick the first way `this` matches `that`
    try this that = map cleanup (head (execWriterT (sub this that)))

    -- Match `x` with `y`; this is not a commutative operator
    -- since "abc" will match "[a]l[b]um [c]over" but not the
    -- other way around. All sequences of symbols (S) are equal
    -- with any other sequence of symbols
    sub :: Split -> Split -> Search Result Unit
    sub x y = case T x y of
      T Nil bs                  -> consume bs
      T (a:as) Nil              -> empty
      T as@(S _ : _) (L b : bs) ->  log (U b) *> sub as bs
      T as@(L _ : _) (S b : bs) ->  log (U b) *> sub as bs
      T z@(S a : as) (S b : bs) -> (log (U b) *> sub z bs) <|> (log (M b) *> sub as bs)
      T z@(L a : as) (L b : bs) -> (log (U b) *> sub z bs) <|> letters a b (T as bs)
      where
        log :: Span -> Search (List Span) Unit
        log = tell <<< (_ : Nil)

        consume :: Split -> Search Result Unit
        consume Nil             = pure unit
        consume (S b : bs) = log (U b) *> consume bs
        consume (L b : bs) = log (U b) *> consume bs

        letters :: String -> String -> T Split -> Search (List Span) Unit
        letters "" b (T as bs) = sub as (L b : bs)
        letters a "" (T as bs) = sub (L a : as) bs
        letters a b xx =
          let a0 = charAt 0 a
              b0 = charAt 0 b
              as = drop 1 a
              bs = drop 1 b
              no = log (U (singleton b0)) *> letters a bs xx
           in no <|> if toUpper a0 == toUpper b0
                        then log (M (singleton b0)) *> letters as bs xx
                        else empty

    -- | Merge consecutive spans of the same type and remove
    -- | spans that only have an empty string
    cleanup :: Result -> Result
    cleanup xs = op xs
      where
        op Nil                           = Nil
        op (Cons (M a) (Cons (M b) cs))  = op (M (a <> b) : cs)
        op (Cons (U a) (Cons (U b) cs))  = op (U (a <> b) : cs)
        op (Cons (U a) (Cons (M "") cs)) = op (U a : cs)
        op (Cons (M a) (Cons (U "") cs)) = op (M a : cs)
        op (Cons (M "") bs)              = op bs
        op (Cons (U "") bs)              = op bs
        op (Cons a bs)                   = a : op bs


-- | Split a string into groups of consecutive symbols (S)
-- | and consecutive letters (L)
parse :: String -> Split
parse "" = Nil
parse s  =
  case uncons letters of
       Nothing          -> letters
       Just {head,tail} -> head : join (zipWith (\d t -> d : t : Nil) symbols tail)
  where
    pattern = unsafePartial (fromRight (regex "[^a-zA-Z0-9]+" global))
    letters = map L (fromFoldable (split pattern s))
    symbols = map S (fromFoldable (fromMaybe [] (map (catMaybes <<< toArray) (match pattern s))))

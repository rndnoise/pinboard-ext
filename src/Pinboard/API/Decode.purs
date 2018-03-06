module Pinboard.API.Decode where

import Prelude

import Data.Either              (Either(..), note)
import Data.Maybe               (Maybe(..))
import Data.StrMap              (StrMap)
import Data.StrMap              as StrMap
import Data.JSDate              (parse, toDateTime)
import Data.String              (Pattern(..), split)
import Data.DateTime            (DateTime)
import Data.Argonaut.Core       (Json, toString, toObject, toNumber, toArray)
import Data.Traversable         (traverse)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Pinboard.API.Types       (Error(..), Post)

type Name = Maybe String

decodePosts :: Name -> Json -> Either Error (Array Post)
decodePosts name x = traverse decodePost =<< decodeArray name x
  where
    decodePost z = do
      o <- decodeObject name z
      { href: _, description: _, extended: _, hash: _, meta: _, others: _, tags: _, time: _, toRead: _, shared: _ } <$>
        decodePropWith  decodeString "href" o         <*>
        decodePropWith  decodeString "description" o  <*>
        decodePropWith  decodeString "extended" o     <*>
        decodePropWith  decodeString "hash" o         <*>
        decodePropWith  decodeString "meta" o         <*>
        decodePropWithM decodeNumber "others" o       <*>
        decodePropWith  decodeTags   "tags" o         <*>
        decodePropWith  decodeDate   "time" o         <*>
        decodePropWith  decodeBoolean "toread" o      <*>
        decodePropWith  decodeBoolean "shared" o
    decodeTags name' z = split (Pattern " ") <$> decodeString name' z

decodeObject :: Name -> Json -> Either Error (StrMap Json)
decodeObject name x = explain name "not an object" (toObject x)

decodePropWith :: forall a. (Name -> Json -> Either Error a) -> String -> (StrMap Json) -> Either Error a
decodePropWith f name x = do
  o <- explain (Just name) "property is missing" (StrMap.lookup name x)
  f (Just name) o

decodePropWithM :: forall a. (Name -> Json -> Either Error a) -> String -> (StrMap Json) -> Either Error (Maybe a)
decodePropWithM f name x =
  case StrMap.lookup name x of
       Nothing -> Right Nothing
       Just o  -> Just <$> f (Just name) o

decodeString :: Name -> Json -> Either Error String
decodeString name x = explain name "not a string" (toString x)

decodeNumber :: Name -> Json -> Either Error Number
decodeNumber name x = explain name "not a number" (toNumber x)

decodeArray :: Name -> Json -> Either Error (Array Json)
decodeArray name x = explain name "not an array" (toArray x)

decodeBoolean :: Name -> Json -> Either Error Boolean
decodeBoolean name x = do
  s <- explain name "not a boolean" (toString x)
  case s of
       "yes"  -> Right true
       "no"   -> Right false
       _      -> explain name "not a boolean" Nothing

decodeDate :: Name -> Json -> Either Error DateTime
decodeDate name x = do
  s <- explain name "not a date" (toString x)
  explain name "not a date" (date' s)
  where date' s = toDateTime (unsafePerformEff (parse s))

explain :: forall a. Name -> String -> Maybe a -> Either Error a
explain name msg x = case name of
  Nothing -> note (DecodeError msg) x
  Just k  -> note (DecodeError (k <> ": " <> msg)) x

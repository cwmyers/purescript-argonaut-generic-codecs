module Test.Main where

import Data.Argonaut.Generic.Aeson as Aeson
import Data.Argonaut.Generic.Argonaut as Argonaut
import Data.StrMap as SM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Generic.Decode (genericDecodeJson, genericDecodeJson')
import Data.Argonaut.Generic.Encode (genericEncodeJson, genericEncodeJson')
import Data.Argonaut.Generic.Options (Options(..))
import Data.Argonaut.Generic.Util (decodeError, DecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic (gShow, class Generic, isValidSpine, gEq)
import Data.Int (toNumber)
import Data.List ((:), List(Nil, Cons))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (class Show, class Eq, Unit, show, bind, (<<<), ($), (==), (&&), (<$>))
import Test.Assert (ASSERT, assert')
import Test.StrongCheck (quickCheck)
import Test.StrongCheck.Generic (GenericValue, runGenericValue)
import Data.Argonaut.Core hiding (toNumber)


newtype MyRecord = MyRecord { foo :: String, bar :: Int}
derive instance genericMyRecord :: Generic MyRecord
instance eqMyRecord :: Eq MyRecord where
  eq = gEq

instance showMyRecord :: Show MyRecord where
  show = gShow


newtype DeepRecord = DeepRecord { outer :: String
                                , inner :: MyRecord}
derive instance genericDeepRecord :: Generic DeepRecord
instance eqDeepRecord :: Eq DeepRecord where
  eq = gEq

instance showDeepRecord :: Show DeepRecord where
  show = gShow



data User = Anonymous
          | Guest String
          | Registered { name :: String
                       , age :: Int
                       , balance :: Number
                       , banned :: Boolean
                       , tweets :: Array String
                       , followers :: Array User
                       }
derive instance genericUser :: Generic User

instance eqGenericUser :: Eq User where
  eq = gEq

instance showUser :: Show User where
  show = gShow

data AllNullary = Nullary1 | Nullary2 | Nullary3
derive instance genericAllNullary :: Generic AllNullary
instance genericEqAllNullary :: Eq AllNullary where
  eq = gEq

data MultipleArgs = MArgs Int Int String | NArgs
derive instance genericMultipleArgs :: Generic MultipleArgs
instance genericEqMArgs :: Eq MultipleArgs where
  eq = gEq

newtype NewTypeWrapper1 = NewTypeWrapper1 { test :: String }
derive instance genericNewTypeWrapper1 :: Generic NewTypeWrapper1
instance eqNewTypeWrapper1 :: Eq NewTypeWrapper1 where
  eq = gEq

data NewTypeWrapper2 = NewTypeWrapper2 {test :: Int}
derive instance genericNewTypeWrapper2 :: Generic NewTypeWrapper2
instance eqNewTypeWrapper2 :: Eq NewTypeWrapper2 where
  eq = gEq

data UnwrapTestMult = UnwrapTestMult Int String
derive instance genericUnwrapTestMult :: Generic UnwrapTestMult
instance eqUnwrapTestMult :: Eq UnwrapTestMult where
  eq = gEq

data UnwrapTestSingle = UnwrapTestSingle Int
derive instance genericUnwrapTestSingle :: Generic UnwrapTestSingle
instance eqUnwrapTestSingle :: Eq UnwrapTestSingle where
  eq = gEq

prop_iso_generic :: Options -> GenericValue -> Boolean
prop_iso_generic opts genericValue =
  Right val.spine == genericDecodeJson' opts Nil val.signature (genericEncodeJson' opts val.signature val.spine)
  where val = runGenericValue genericValue

prop_decoded_spine_valid :: Options -> GenericValue -> Boolean
prop_decoded_spine_valid opts genericValue =
  Right true == (isValidSpine val.signature <$> genericDecodeJson' opts Nil val.signature (genericEncodeJson' opts val.signature val.spine))
  where val = runGenericValue genericValue

checkAesonCompat :: Boolean
checkAesonCompat =
  let
    myTuple = Tuple (Tuple 1 2) "Hello"
    myJust = Just "Test"
    myNothing = Nothing :: Maybe Int
    myLeft = Left "Foo" :: Either String String
    myRight = Right "Bar" :: Either Int String
    unwrapMult = UnwrapTestMult 8 "haha"
    unwrapSingle = UnwrapTestSingle 8
  in
        Aeson.encodeJson myTuple      == fromArray [fromNumber $ toNumber 1, fromNumber $ toNumber 2, fromString "Hello"]
    &&  Aeson.encodeJson myJust       == fromString "Test"
    &&  Aeson.encodeJson myNothing    == jsonNull
    &&  Aeson.encodeJson myLeft       == fromObject (SM.fromList (Tuple "Left" (fromString "Foo") `Cons` Nil))
    &&  Aeson.encodeJson myRight      == fromObject (SM.fromList (Tuple "Right" (fromString "Bar") `Cons` Nil))
    &&  Aeson.encodeJson unwrapMult   == fromArray [fromNumber $ toNumber 8, fromString "haha"]
    &&  Aeson.encodeJson unwrapSingle == (fromNumber $ toNumber 8)


genericsCheck :: forall e. Options -> Eff ( err :: EXCEPTION , random :: RANDOM , console :: CONSOLE, assert :: ASSERT | e) Unit
genericsCheck opts = do
  let vNullary = Nullary2
  let mArgs = MArgs 9 20 "Hello"
  let ntw1 = NewTypeWrapper1 { test : "hello" }
  let ntw2 = NewTypeWrapper2 { test : 9 }
  let mJust = Just "Test"
  let mNothing = Nothing :: Maybe Int
  let mRight = Right 9 :: Either String Int
  let mLeft = Right (Left 2) :: Either String (Either Int Int)
  let mTuple = Tuple (Tuple (Tuple 2 3) "haha") "test"
  let unwrapMult = UnwrapTestMult 8 "haha"
  let unwrapSingle = UnwrapTestSingle 8
  log "Check that decodeJson' and encodeJson' form an isomorphism .."
  assert' " Check all nullary:" (valEncodeDecode opts vNullary)
  assert' " Check multiple args:" (valEncodeDecode opts mArgs)
  assert' " Check new type wrapper (1) encoding:" (valEncodeDecode opts ntw1)
  assert' " Check new type wrapper (2) encoding:" (valEncodeDecode opts ntw2)
  assert' " Check Just" (valEncodeDecode opts mJust)
  assert' " Check Nothing" (valEncodeDecode opts mNothing)
  assert' " Check Right" (valEncodeDecode opts mRight)
  assert' " Check Left" (valEncodeDecode opts mLeft)
  assert' " Check tuple" (valEncodeDecode opts mTuple)
  assert' " Check unwrapMult" (valEncodeDecode opts unwrapMult)
  assert' " Check unwrapSingle" (valEncodeDecode opts unwrapSingle)

  quickCheck (prop_iso_generic opts)
  log "Check that decodeJson' returns a valid spine"
  quickCheck (prop_decoded_spine_valid opts)
  log "Print samples of values encoded with genericEncodeJson"
  print $ genericEncodeJson opts 5
  print $ genericEncodeJson opts [1, 2, 3, 5]
  print $ genericEncodeJson opts (Just "foo")
  print $ genericEncodeJson opts (Right "foo" :: Either String String)
  print $ genericEncodeJson opts $ MyRecord { foo: "foo", bar: 2}
  print $ genericEncodeJson opts "foo"
  print $ genericEncodeJson opts Anonymous
  print $ genericEncodeJson opts $ Guest "guest's handle"
  print $ genericEncodeJson opts $ Registered { name: "user1"
                                   , age: 5
                                   , balance: 26.6
                                   , banned: false
                                   , tweets: ["Hello", "What's up"]
                                   , followers: [ Anonymous
                                                , Guest "someGuest"
                                                , Registered { name: "user2"
                                                             , age: 6
                                                             , balance: 32.1
                                                             , banned: false
                                                             , tweets: ["Hi"]
                                                             , followers: []
                                                             }]}
  print $ genericEncodeJson opts Nullary1
  print $ genericEncodeJson opts Nullary2
  print $ genericEncodeJson opts $ MArgs 9 22 "Test"
  print $ genericEncodeJson opts NArgs
  print $ genericEncodeJson opts ntw1
  print $ genericEncodeJson opts ntw2

  where
    valEncodeDecode :: forall a. (Eq a, Generic a) => Options -> a -> Boolean
    valEncodeDecode opts val = ((Right val) == _) <<< genericDecodeJson opts <<< genericEncodeJson opts $ val

checkErrors :: forall eff.
  Eff ( console :: CONSOLE, err :: EXCEPTION, random :: RANDOM, assert :: ASSERT| eff) Unit
checkErrors = do
  log "checking for Guest"
  checkErrorHistory (DeepRecord { outer: " " , inner : MyRecord {foo : "Test", bar : 42}})
    "{\"outer\" : \"123\", \"inner\": {\"foo\" : \"hi\" , \"bar\" : \"32\"} }"

checkErrorHistory :: forall e a. (Eq a, Generic a, Show a) => a -> String -> Eff ( err :: EXCEPTION , console :: CONSOLE, assert :: ASSERT | e) Unit
checkErrorHistory a input = do
  let result = decodeListing input :: Either DecodeError a
  print result
  assert' "blah"  (result == Left (decodeError ("bar" : "inner" : Nil) "Expected an integer number"))

decodeListing :: forall a. (Generic a) => String -> Either DecodeError a
decodeListing listingString = do
  json <- lmap (decodeError Nil) $ jsonParser listingString
  decodeJson json


main:: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE, assert :: ASSERT | e ) Unit
main = do
  log "check errors"
  checkErrors
  assert' "aesonCompatcheck: " checkAesonCompat
  log "genericsCheck check for argonautOptions"
  genericsCheck Argonaut.options
  log "genericsCheck check for aesonOptions"
  genericsCheck Aeson.options
  log "genericsCheck check for encodeSingleOptions"
  let unwrapOpts = case Aeson.options of Options a -> a
  let encodeSingleOptions = Options $ unwrapOpts { encodeSingleConstructors = false }
  genericsCheck encodeSingleOptions

print :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
print = log <<< show

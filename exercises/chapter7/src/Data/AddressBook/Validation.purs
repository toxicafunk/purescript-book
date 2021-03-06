module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.Maybe
import Data.String (length)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

import Debug.Trace (trace, traceShow, traceAny, spy, traceA, traceShowA, traceAnyA, traceShowM, traceAnyM)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^\\D{2}$" noFlags of
      Right r -> r

nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial
    case regex "^(?=\\s*\\S).*$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value =
  do
    trace ("Called matches with " <> show value) \_ ->
    pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (matches "Street" nonEmptyRegex o.street *> pure o.street)
          -- <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (matches "City" nonEmptyRegex o.city *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)
          -- <*> (lengthIs "State" 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)


validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> traverse validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p

module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntryByStreet
  where
  filterEntryByStreet :: Entry -> Boolean
  filterEntryByStreet entry = entry.address.street == street

addressBookContains :: String -> AddressBook -> Boolean
--addressBookContains name = null <<< filter \e -> e.firstName == name
addressBookContains name = not null <<< filter findName
  where
  findName :: Entry -> Boolean
  findName entry = entry.firstName == name

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy \x y -> x.firstName == y.firstName && x.lastName == y.lastName

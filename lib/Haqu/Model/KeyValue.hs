-- Module: Haqu.Model.KeyValue Datenstruktur für Key-Value Paare
module Haqu.Model.KeyValue (
    KeyValue(..)
) where

-- Datenstruktur für Key-Value Paare
data KeyValue = KeyValue {
    kv_key :: String,
    kv_value :: String
} deriving Show
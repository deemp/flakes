{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MessageTH (options) where

import Data.Aeson.TH
  ( Options (..),
    SumEncoding (..),
    defaultOptions,
  )

options :: Options
options = defaultOptions {tagSingleConstructors = True, unwrapUnaryRecords = True, sumEncoding = TaggedObject {tagFieldName = "(tag)", contentsFieldName = "(contents)"}}